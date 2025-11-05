// runtime.cpp
// Runtime de consola (PDCurses) para JSON de BrickScript (Tetris/Snake)
// Contrato de JSON compatible con tu runtime.py
// Compilar: g++ -std=c++17 runtime.cpp -lpdcurses -o runtime.exe

#include "librerias/pdcurses/curses.h"
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <optional>
#include <random>
#include <chrono>
#include "json.hpp"

using json = nlohmann::json;
using namespace std;

struct Juego {
    // ---- Estado base ----
    string tipo = "TETRIS";
    int W = 10, H = 20;
    vector<vector<int>> grid;
    int score = 0;
    bool game_over = false;

    // tiempo
    double tick_sec = 0.30;
    double acc = 0.0;

    // RNG
    mt19937 rng{ static_cast<unsigned>(chrono::high_resolution_clock::now().time_since_epoch().count()) };

    // events: nombre -> lista de acciones
    // acción: {accion, objeto (opcional), params (opcional)}
    map<string, vector<json>> events;

    // ---- Tetris ----
    // shapes: nombre -> [ rotaciones ][ fila ][ col ] (0/1)
    map<string, vector<vector<vector<int>>>> shapes;
    vector<vector<vector<int>>> pieza;  // rotaciones de la pieza activa
    int px=0, py=0, prot=0;             // pos pieza activa y rot
    bool have_piece=false;

    // ---- Snake ----
    vector<pair<int,int>> snake;        // cabeza = front()
    pair<int,int> sdir{1,0};
    optional<pair<int,int>> food;

    // ---------- Carga ----------
    void cargar(const json& j) {
        if (j.contains("tipo_juego") && j["tipo_juego"].is_string())
            tipo = j["tipo_juego"].get<string>();

        if (j.contains("config") && j["config"].contains("grid_size")) {
            auto gs = j["config"]["grid_size"];
            if (gs.is_array() && gs.size()==2) {
                W = (int)gs[0]; H = (int)gs[1];
            }
        }
        grid.assign(H, vector<int>(W, 0));

        if (j.contains("events") && j["events"].is_object()) {
            for (auto& [k, arr] : j["events"].items()) {
                if (arr.is_array()) events[k] = arr.get<vector<json>>();
            }
        }

        if (tipo == "TETRIS") {
            tick_sec = 0.35;
            if (j.contains("shapes") && j["shapes"].is_object()) {
                for (auto& [name, rots] : j["shapes"].items()) {
                    vector<vector<vector<int>>> R;
                    for (auto& r : rots) {
                        vector<vector<int>> M;
                        for (auto& row : r) {
                            vector<int> mr;
                            for (auto& c : row) mr.push_back((int)c);
                            M.push_back(move(mr));
                        }
                        R.push_back(move(M));
                    }
                    shapes[name] = move(R);
                }
            }
        } else {
            tick_sec = 0.12;
        }
    }

    // ---------- Loop ----------
    void run() {
        // Init curses
        initscr(); cbreak(); noecho(); keypad(stdscr, TRUE); nodelay(stdscr, TRUE); curs_set(0);

        auto t_prev = chrono::high_resolution_clock::now();
        ejecutar("ON_START");

        while (!game_over) {
            // dt
            auto t_now = chrono::high_resolution_clock::now();
            double dt = chrono::duration<double>(t_now - t_prev).count();
            t_prev = t_now;
            acc += dt;

            // input
            handle_input();

            // tick
            if (acc >= tick_sec) { acc = 0.0; ejecutar("ON_TICK"); }

            // draw
            render();
            napms(40);
        }

        // Game over screen
        clear();
        mvprintw(2, 10, "=================");
        mvprintw(3, 10, "  JUEGO TERMINADO");
        mvprintw(4, 10, "=================");
        mvprintw(6, 10, "Puntuacion: %d", score);
        mvprintw(8, 10, "Pulsa una tecla...");
        nodelay(stdscr, FALSE); getch();
        endwin();
    }

    // ---------- Entrada ----------
    void handle_input() {
        int k = getch();
        if (k == ERR) return;
        if (k=='q' || k=='Q') { game_over = true; return; }

        if (tipo=="TETRIS") {
            if (k==KEY_LEFT)  ejecutar("ON_KEY_LEFT");
            if (k==KEY_RIGHT) ejecutar("ON_KEY_RIGHT");
            if (k==KEY_DOWN)  ejecutar("ON_KEY_DOWN");
            if (k==KEY_UP)    ejecutar("ON_KEY_UP");
        } else {
            if (k==KEY_LEFT)  snake_dir(-1,0);
            if (k==KEY_RIGHT) snake_dir(1,0);
            if (k==KEY_UP)    snake_dir(0,-1);
            if (k==KEY_DOWN)  snake_dir(0,1);
        }
    }

    // ---------- Render ----------
    void render() {
        clear();

        // copiar grid
        auto disp = grid;

        // pintar pieza tetris (2)
        if (tipo=="TETRIS" && have_piece) {
            const auto& M = pieza[prot];
            for (int y=0; y<(int)M.size(); ++y) {
                for (int x=0; x<(int)M[y].size(); ++x) {
                    if (M[y][x]==1) {
                        int gx = px + x, gy = py + y;
                        if (gy>=0 && gy<H && gx>=0 && gx<W) disp[gy][gx] = 2;
                    }
                }
            }
        }

        // snake: cabeza 3, cuerpo 2, comida 4
        if (tipo=="SNAKE") {
            for (size_t i=0;i<snake.size();++i) {
                auto [x,y] = snake[i];
                if (y>=0 && y<H && x>=0 && x<W) disp[y][x] = (i==0?3:2);
            }
            if (food) {
                auto [fx_,fy_] = *food;
                if (fy_>=0 && fy_<H && fx_>=0 && fx_<W) disp[fy_][fx_] = 4;
            }
        }

        // borde + buffer estilo ASCII
        string top(W*2 + 4, '#');
        mvprintw(0, 0, "%s", top.c_str());
        int row=1;

        for (int y=0;y<H;++y,++row) {
            string line = "# ";
            for (int x=0;x<W;++x) {
                int c = disp[y][x];
                if (c==0)      line += "  ";
                else if (c==1) line += "[]";
                else if (c==2) line += "[]";
                else if (c==3) line += "OO";
                else if (c==4) line += "@@";
            }
            line += " #";
            if (y==2) line += "    PUNTUACION: " + to_string(score);
            if (y==4) line += "    CONTROLES:";
            if (y==5) line += "     Flechas";
            if (y==6) line += "     'q': Salir";
            mvprintw(row, 0, "%s", line.c_str());
        }
        mvprintw(row, 0, "%s", top.c_str());

        refresh();
    }

    // ---------- Eventos ----------
    void ejecutar(const string& nombre) {
        auto it = events.find(nombre);
        if (it == events.end()) return;
        for (const auto& act : it->second) {
            const string verbo  = act.value("accion", "");
            const string objeto = act.value("objeto", "");

            if (verbo=="INCREASE_SCORE") {
                // objeto puede ser número o string
                int val = 0;
                if (act.contains("objeto") && act["objeto"].is_number_integer())
                    val = act["objeto"].get<int>();
                else if (act.contains("objeto") && act["objeto"].is_string())
                    val = stoi(act["objeto"].get<string>());
                score += val;
            } else if (verbo=="GAME_OVER") {
                game_over = true;
            }

            if (tipo=="TETRIS") {
                if (verbo=="SPAWN") t_spawn();
                else if (verbo=="MOVE") {
                    string dir = "DOWN";
                    if (act.contains("params") && act["params"].is_array() && !act["params"].empty())
                        dir = act["params"][0].get<string>();
                    t_move(dir);
                } else if (verbo=="ROTATE") t_rotate();
            } else { // SNAKE
                if (verbo=="SPAWN" && objeto=="PLAYER") s_spawn_player(act);
                else if (verbo=="SPAWN" && objeto=="FOOD") s_spawn_food();
                else if (verbo=="MOVE"  && objeto=="PLAYER") s_move();
                else if (verbo=="GROW") {/* implícito al comer */}
            }
        }
    }

    // ---------- TETRIS ----------
    void t_spawn() {
        if (shapes.empty()) return;
        // elegir pieza aleatoria
        vector<string> ids; ids.reserve(shapes.size());
        for (auto& kv : shapes) ids.push_back(kv.first);
        uniform_int_distribution<int> dist(0, (int)ids.size()-1);
        pieza = shapes[ids[dist(rng)]];
        prot = 0; have_piece = true;
        px = max(0, W/2 - 2); py = 0;
        if (t_collide(px, py, prot)) game_over = true;
    }

    bool t_collide(int x, int y, int r) const {
        if (!have_piece) return false;
        const auto& M = pieza[r];
        for (int yy=0; yy<(int)M.size(); ++yy) {
            for (int xx=0; xx<(int)M[yy].size(); ++xx) {
                if (M[yy][xx]==1) {
                    int gx = x+xx, gy=y+yy;
                    if (gx<0 || gx>=W || gy<0 || gy>=H) return true;
                    if (grid[gy][gx]!=0) return true;
                }
            }
        }
        return false;
    }

    void t_move(const string& dir) {
        if (!have_piece) return;
        int dx=0, dy=0;
        if (dir=="LEFT") dx=-1;
        else if (dir=="RIGHT") dx=1;
        else if (dir=="DOWN") dy=1;

        if (!t_collide(px+dx, py+dy, prot)) {
            px += dx; py += dy;
        } else if (dy>0) {
            t_lock();
        }
    }

    void t_rotate() {
        if (!have_piece) return;
        int nr = (prot + 1) % (int)pieza.size();
        if (!t_collide(px, py, nr)) prot = nr;
    }

    void t_lock() {
        const auto& M = pieza[prot];
        for (int yy=0; yy<(int)M.size(); ++yy) {
            for (int xx=0; xx<(int)M[yy].size(); ++xx) {
                if (M[yy][xx]==1) {
                    int gx=px+xx, gy=py+yy;
                    if (gy>=0 && gy<H && gx>=0 && gx<W) grid[gy][gx]=1;
                }
            }
        }
        have_piece=false;
        t_clear_lines();
        ejecutar("ON_START"); // spawnea siguiente
    }

    void t_clear_lines() {
        vector<vector<int>> keep;
        for (auto& row : grid) {
            bool full=true;
            for (int c: row) if (c==0){ full=false; break; }
            if (!full) keep.push_back(row);
        }
        int cleared = H - (int)keep.size();
        if (cleared>0) {
            vector<vector<int>> top(cleared, vector<int>(W,0));
            grid = move(top);
            grid.insert(grid.end(), keep.begin(), keep.end());
            // notificar evento por cada línea
            for (int i=0;i<cleared;++i) ejecutar("ON_LINE_CLEAR");
        }
    }

    // ---------- SNAKE ----------
    void s_spawn_player(const json& act) {
        int sx = W/2, sy = H/2;
        if (act.contains("params") && act["params"].is_array() && !act["params"].empty()) {
            auto p = act["params"][0];
            if (p.is_array() && p.size()==2) { sx = (int)p[0]; sy = (int)p[1]; }
        }
        snake.clear();
        snake.emplace_back(sx, sy);
        sdir = {1, 0};
    }

    void s_spawn_food() {
        if (W<=0 || H<=0) return;
        uniform_int_distribution<int> dx(0, W-1), dy(0, H-1);
        for (int t=0;t<5000;++t) {
            int x = dx(rng), y = dy(rng);
            bool ocup = false;
            for (auto& s: snake) if (s.first==x && s.second==y){ ocup=true; break; }
            if (!ocup) { food = pair{x,y}; return; }
        }
        food.reset(); // muy lleno
    }

    void s_move() {
        if (snake.empty()) return;
        auto [hx,hy] = snake.front();
        pair<int,int> nh{hx + sdir.first, hy + sdir.second};

        // paredes
        if (nh.first<0 || nh.first>=W || nh.second<0 || nh.second>=H) {
            ejecutar("ON_COLLISION_WALL"); return;
        }
        // cuerpo (excepto la última celda si va a mover)
        for (size_t i=0;i+1<snake.size();++i) {
            if (snake[i]==nh) { ejecutar("ON_COLLISION_SELF"); return; }
        }

        snake.insert(snake.begin(), nh);

        if (food && nh==*food) {
            ejecutar("ON_EAT_FOOD"); // puede aumentar score y re-spawnear comida
        } else {
            if (!snake.empty()) snake.pop_back();
        }
    }

    void snake_dir(int x, int y) {
        // evita reversa inmediata
        if (x!=0 && sdir.first == -x) return;
        if (y!=0 && sdir.second== -y) return;
        sdir = {x,y};
    }
};

// ---------- main ----------
int main(int argc, char** argv){
    if (argc != 2) {
        std::cout << "Uso: " << argv[0] << " <archivo_juego.json>\n";
        return 1;
    }

    try{
        // leer JSON
        std::ifstream in(argv[1]);
        if (!in) { std::cout << "Error: no se pudo abrir " << argv[1] << "\n"; return 1; }
        json j; in >> j;

        // crear juego
        Juego G;
        G.cargar(j);
        G.run();
        return 0;
    } catch (const std::exception& e){
        endwin(); // por si alcanzó a iniciar curses
        std::cout << "Error: " << e.what() << "\n";
        return 1;
    }
}
