// runtime.cpp
// Runtime de consola (PDCurses) para JSON de BrickScript (Tetris/Snake)
// Contrato de JSON compatible con tu runtime.py
// Compilar: g++ -std=c++17 runtime.cpp -lpdcurses -o runtime.exe

#include <pdcurses.h>
#include "json.hpp"
#include <algorithm>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <optional>
#include <random>
#include <chrono>


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
    string cur_shape;
    int cur_color = 1; // color pair id for current tetris piece

    // ---- Snake ----
    vector<pair<int,int>> snake;        // cabeza = front()
    pair<int,int> sdir{1,0};
    optional<pair<int,int>> food;
    int snake_grow_pending = 0;

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
        snake.clear();
        food.reset();
        snake_grow_pending = 0;
        have_piece = false;
        pieza.clear();

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
        if (has_colors()) {
            start_color();
            init_pair(1, COLOR_CYAN,   COLOR_BLACK);  // I
            init_pair(2, COLOR_BLUE,   COLOR_BLACK);  // J
            init_pair(3, COLOR_YELLOW, COLOR_BLACK);  // O (and L if wanted)
            init_pair(4, COLOR_WHITE,  COLOR_BLACK);  // fallback
            init_pair(5, COLOR_GREEN,  COLOR_BLACK);  // S
            init_pair(6, COLOR_MAGENTA,COLOR_BLACK);  // T
            init_pair(7, COLOR_RED,    COLOR_BLACK);  // Z
        }

        auto t_prev = chrono::high_resolution_clock::now();
        ejecutar("ON_START");
        if (tipo=="TETRIS" && !have_piece) t_spawn();
        if (tipo=="SNAKE") {
            if (snake.empty()) s_spawn_player(json::object());
            if (!food) s_spawn_food();
        }

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
    }

    // ---------- Input ----------
    void handle_input() {
        int ch = getch();
        if (ch == ERR) return;
        if (ch == 'q' || ch == 'Q') { game_over = true; return; }

        if (tipo=="SNAKE") {
            if (ch == KEY_UP)       snake_dir(0,-1);
            else if (ch == KEY_DOWN)  snake_dir(0, 1);
            else if (ch == KEY_LEFT)  snake_dir(-1,0);
            else if (ch == KEY_RIGHT) snake_dir(1, 0);
        } else if (tipo=="TETRIS") {
            if (ch == KEY_LEFT)      t_move("LEFT");
            else if (ch == KEY_RIGHT) t_move("RIGHT");
            else if (ch == KEY_DOWN)  t_move("DOWN");
            else if (ch == KEY_UP)    t_rotate();
        }
    }

    // --- Tetris movement/rotation ---
    void t_move(const string& dir) {
        if (!have_piece) { t_spawn(); return; }
        if (dir == "LEFT") {
            if (!t_collide(px-1, py, prot)) px -= 1;
        } else if (dir == "RIGHT") {
            if (!t_collide(px+1, py, prot)) px += 1;
        } else if (dir == "DOWN") {
            if (!t_collide(px, py+1, prot)) {
                py += 1;
            } else {
                // lock piece into grid
                const auto& M = pieza[prot];
                for (int y=0;y<(int)M.size();++y)
                    for (int x=0;x<(int)M[y].size();++x)
                        if (M[y][x]==1) {
                            int gx = px + x, gy = py + y;
                            if (gy>=0 && gy<H && gx>=0 && gx<W) grid[gy][gx] = cur_color;
                        }
                have_piece = false;
                t_clear_lines();
                t_spawn();
            }
        }
    }

    void t_rotate() {
        if (!have_piece) return;
        int next = (prot + 1) % (int)pieza.size();
        if (!t_collide(px, py, next)) { prot = next; return; }
        // simple wall kicks
        if (!t_collide(px-1, py, next)) { px -= 1; prot = next; return; }
        if (!t_collide(px+1, py, next)) { px += 1; prot = next; return; }
        // otherwise keep current orientation
    }

    // ---------- Render ----------
    void render() {
        clear();

        // copiar grid
        auto disp = grid;

        // pintar pieza tetris (usa codigos 100+color para pieza activa)
        if (tipo=="TETRIS" && have_piece) {
            const auto& M = pieza[prot];
            for (int y=0; y<(int)M.size(); ++y) {
                for (int x=0; x<(int)M[y].size(); ++x) {
                    if (M[y][x]==1) {
                        int gx = px + x, gy = py + y;
                        if (gy>=0 && gy<H && gx>=0 && gx<W) disp[gy][gx] = 100 + cur_color;
                    }
                }
            }
        }

        // snake: cabeza 3, cuerpo 2, comida 4
        if (tipo=="SNAKE") {
            const int WALL = 5;
            if (W>0 && H>0) {
                for (int x=0; x<W; ++x) {
                    disp[0][x] = WALL;
                    disp[H-1][x] = WALL;
                }
                for (int y=0; y<H; ++y) {
                    disp[y][0] = WALL;
                    disp[y][W-1] = WALL;
                }
            }
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

        if (tipo=="TETRIS") {
            for (int y=0;y<H;++y,++row) {
                mvprintw(row, 0, "# ");
                for (int x=0;x<W;++x) {
                    int c = disp[y][x];
                    if (c==0) {
                        mvprintw(row, 2 + x*2, "  ");
                    } else {
                        int cp = (c>=100)? (c-100) : c;
                        if (cp < 1 || cp > 7) cp = 4;
                        attron(COLOR_PAIR(cp) | A_BOLD);
                        mvprintw(row, 2 + x*2, "[]");
                        attroff(COLOR_PAIR(cp) | A_BOLD);
                    }
                }
                mvprintw(row, 2 + W*2, " #");
                if (y==2) mvprintw(row, 2 + W*2 + 4, "PUNTUACION: %d", score);
                if (y==4) mvprintw(row, 2 + W*2 + 4, "CONTROLES:");
                if (y==5) mvprintw(row, 2 + W*2 + 4, "Flechas");
                if (y==6) mvprintw(row, 2 + W*2 + 4, "'q': Salir");
            }
        } else { // SNAKE render with colors
            for (int y=0;y<H;++y,++row) {
                mvprintw(row, 0, "# ");
                for (int x=0;x<W;++x) {
                    int c = disp[y][x];
                    if (c==0) {
                        mvprintw(row, 2 + x*2, "  ");
                    } else if (c==2 || c==3) { // snake body/head in green
                        int cp = 5; // green
                        attron(COLOR_PAIR(cp) | A_BOLD);
                        mvprintw(row, 2 + x*2, "OO");
                        attroff(COLOR_PAIR(cp) | A_BOLD);
                    } else if (c==4) { // food in red
                        int cp = 7; // red
                        attron(COLOR_PAIR(cp) | A_BOLD);
                        mvprintw(row, 2 + x*2, "@@");
                        attroff(COLOR_PAIR(cp) | A_BOLD);
                    } else if (c==5) { // walls
                        mvprintw(row, 2 + x*2, "##");
                    } else {
                        mvprintw(row, 2 + x*2, "  ");
                    }
                }
                mvprintw(row, 2 + W*2, " #");
                if (y==2) mvprintw(row, 2 + W*2 + 4, "PUNTUACION: %d", score);
                if (y==4) mvprintw(row, 2 + W*2 + 4, "CONTROLES:");
                if (y==5) mvprintw(row, 2 + W*2 + 4, "Flechas");
                if (y==6) mvprintw(row, 2 + W*2 + 4, "'q': Salir");
            }
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
            const string objeto = (act.contains("objeto") && act["objeto"].is_string())
                                  ? act["objeto"].get<string>()
                                  : string("");

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
                else if (verbo=="GROW") s_grow(act);
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
        int pick = dist(rng);
        cur_shape = ids[pick];
        pieza = shapes[cur_shape];
        // color por pieza
        auto shape_color = [&](const string& s)->int{
            if (s=="I") return 1; // cyan
            if (s=="J") return 2; // blue
            if (s=="O") return 3; // yellow
            if (s=="S") return 5; // green
            if (s=="T") return 6; // magenta
            if (s=="Z") return 7; // red
            if (s=="L") return 3; // use yellow as fallback
            return 4; // white
        };
        cur_color = shape_color(cur_shape);
        // If shape has only one rotation, synthesize the remaining rotations (up to 4)
        if (!pieza.empty() && pieza.size()==1) {
            auto rot90 = [](const vector<vector<int>>& M){
                int h = (int)M.size();
                int w = h? (int)M[0].size() : 0;
                vector<vector<int>> R(w, vector<int>(h, 0));
                for (int y=0;y<h;++y)
                    for (int x=0;x<w;++x)
                        R[x][h-1-y] = M[y][x];
                return R;
            };
            vector<vector<int>> r1 = rot90(pieza[0]);
            vector<vector<int>> r2 = rot90(r1);
            vector<vector<int>> r3 = rot90(r2);
            pieza.push_back(r1);
            pieza.push_back(r2);
            pieza.push_back(r3);
        }
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
                    if (grid[gy][gx] != 0) return true;
                }
            }
        }
        return false;
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
        auto [min_x, max_x] = s_inner_bounds_x();
        auto [min_y, max_y] = s_inner_bounds_y();
        if (min_x > max_x || min_y > max_y) {
            snake.clear();
            snake.emplace_back(0, 0);
        } else {
            sx = std::clamp(sx, min_x, max_x);
            sy = std::clamp(sy, min_y, max_y);
            snake.clear();
            snake.emplace_back(sx, sy);
        }
        sdir = {1, 0};
        snake_grow_pending = 0;
    }

    void s_spawn_food() {
        if (W<=0 || H<=0) return;
        auto [min_x, max_x] = s_inner_bounds_x();
        auto [min_y, max_y] = s_inner_bounds_y();
        if (min_x > max_x || min_y > max_y) { food.reset(); return; }
        uniform_int_distribution<int> dx(min_x, max_x), dy(min_y, max_y);
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

        auto [min_x, max_x] = s_inner_bounds_x();
        auto [min_y, max_y] = s_inner_bounds_y();
        if (min_x > max_x || min_y > max_y) {
            ejecutar("ON_COLLISION_WALL");
            return;
        }
        // paredes dentro del área jugable
        if (nh.first < min_x || nh.first > max_x || nh.second < min_y || nh.second > max_y) {
            ejecutar("ON_COLLISION_WALL");
            return;
        }
        // cuerpo (excepto la última celda si va a mover)
        
        bool will_keep_tail = (food && nh==*food) || snake_grow_pending > 0;
        size_t check_len = snake.size();
        if (!will_keep_tail && check_len>0) --check_len;
        for (size_t i=0;i<check_len;++i) {
            if (snake[i]==nh) { ejecutar("ON_COLLISION_SELF"); return; }
        }

        snake.insert(snake.begin(), nh);

        bool ate = food && nh==*food;
        if (ate) {
            food.reset();
            ejecutar("ON_EAT_FOOD"); // puede aumentar score y re-spawnear comida
            // crecer al comer
            snake_grow_pending += 1;
        }

        if (!ate) {
            if (snake_grow_pending > 0) {
                --snake_grow_pending;
            } else {
                if (!snake.empty()) snake.pop_back();
            }
        }
    }

    void snake_dir(int x, int y) {
        // evita reversa inmediata
        if (x!=0 && sdir.first == -x) return;
        if (y!=0 && sdir.second== -y) return;
        sdir = {x,y};
    }

    void s_grow(const json& act) {
        int amount = 1;
        if (act.contains("params") && act["params"].is_array() && !act["params"].empty()) {
            const auto& p = act["params"][0];
            if (p.is_number_integer()) amount = p.get<int>();
            else if (p.is_string()) {
                try {
                    amount = stoi(p.get<string>());
                } catch (...) {}
            }
        } else if (act.contains("objeto") && act["objeto"].is_number_integer()) {
            amount = act["objeto"].get<int>();
        }
        if (amount > 0) snake_grow_pending += amount;
    }

    pair<int,int> s_inner_bounds_x() const {
        if (W <= 0) return {0, -1};
        if (W <= 2) return {0, W-1};
        return {1, W-2};
    }

    pair<int,int> s_inner_bounds_y() const {
        if (H <= 0) return {0, -1};
        if (H <= 2) return {0, H-1};
        return {1, H-2};
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
