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

    //======================
    //      DATOS BASE
    //======================

    string tipo = "TETRIS";
    int W = 10, H = 20;
    vector<vector<int>> grid;
    int score = 0;
    bool game_over = false;

    double tick_sec = 0.30;
    double acc = 0.0;

    // rng
    mt19937 rng{ static_cast<unsigned>(chrono::high_resolution_clock::now().time_since_epoch().count()) };
    // events mapea nombres a una lista de acciones
    map<string, vector<json>> events;

    //======================
    //      TETRIS
    //======================
    map<string, vector<vector<vector<int>>>> shapes;
    vector<vector<vector<int>>> pieza; // Aquí guardamos las rotaciones de la pieza activa
    int px=0, py=0, prot=0; // Esta es la posición incial de la pieza activa             
    bool have_piece = false;
    string cur_shape;
    int cur_color = 1;

    //======================
    //      SNAKE
    //======================
    vector<pair<int,int>> snake;
    pair<int,int> sdir{1,0};
    optional<pair<int,int>> food;
    int snake_grow_pending = 0;

    //=============================
    //      CARGAR EL JUEGO
    //=============================

    void cargar(const json& game_json) {
        if (game_json.contains("tipo_juego") && game_json["tipo_juego"].is_string())
            tipo = game_json["tipo_juego"].get<string>();

        if (game_json.contains("config") && game_json["config"].contains("grid_size")) {
            auto gs = game_json["config"]["grid_size"];
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

        if (game_json.contains("events") && game_json["events"].is_object()) {
            for (auto& [event_key, arr] : game_json["events"].items()) {
                if (arr.is_array()) events[event_key] = arr.get<vector<json>>();
            }
        }

        if (tipo == "TETRIS") {
            tick_sec = 0.35;
            if (game_json.contains("shapes") && game_json["shapes"].is_object()) {
                for (auto& [name, rots] : game_json["shapes"].items()) {
                    vector<vector<vector<int>>> rotationsVec;
                    for (auto& rotation_item : rots) {
                        vector<vector<int>> piece_matrix;
                        for (auto& row : rotation_item) {
                            vector<int> mapped_row;
                            for (auto& cell : row) mapped_row.push_back((int)cell);
                            piece_matrix.push_back(move(mapped_row));
                        }
                        rotationsVec.push_back(move(piece_matrix));
                    }
                    shapes[name] = move(rotationsVec);
                }
            }
        } else {
            tick_sec = 0.12;
        }
    }

    //=================================================================================
    //                           BUCLE PRINCIPAL
    //=================================================================================
    void run() {
        initscr();
        cbreak();
        noecho();
        keypad(stdscr, TRUE); 
        nodelay(stdscr, TRUE); 
        curs_set(0);
        
        // COLORES FIGURAS
        if (has_colors()) {
            start_color();
            init_pair(1, COLOR_CYAN, COLOR_BLACK); // I
            init_pair(2, COLOR_BLUE, COLOR_BLACK); // J
            init_pair(3, COLOR_YELLOW, COLOR_BLACK); // O & L
            init_pair(4, COLOR_WHITE, COLOR_BLACK); // fallback
            init_pair(5, COLOR_GREEN, COLOR_BLACK); // S
            init_pair(6, COLOR_MAGENTA,COLOR_BLACK); // T
            init_pair(7, COLOR_RED, COLOR_BLACK); // Z
        }

        auto t_prev = chrono::high_resolution_clock::now();
        ejecutar("ON_START");
        if (tipo=="TETRIS" && !have_piece) t_spawn();
        if (tipo=="SNAKE") {
            if (snake.empty()) s_spawn_player(json::object());
            if (!food) s_spawn_food();
        }

        while (!game_over) {

            auto t_now = chrono::high_resolution_clock::now();
            double dt = chrono::duration<double>(t_now - t_prev).count();
            t_prev = t_now;
            acc += dt;

            handle_input();

            if (acc >= tick_sec) { acc = 0.0; ejecutar("ON_TICK"); }

            render();
            napms(40);
        }

        // Pantalla para el game over (aún está en desarrollo)
        clear();
        mvprintw(2, 10, "===================");
        mvprintw(3, 10, "  JUEGO TERMINADO");
        mvprintw(4, 10, "===================");
        mvprintw(6, 10, "Puntuacion: %d", score);
        mvprintw(8, 10, "Presiona cualquier tecla para salir...");
        refresh();
        nodelay(stdscr, FALSE);
        getch();
        endwin();
    }

    //=================================================================================
    //                           LECTURA DE TECLADO
    //=================================================================================
    void handle_input() {
        int key_code = getch();
        if (key_code == ERR) return;
        if (key_code == 'q' || key_code == 'Q') { game_over = true; return; }

        if (tipo=="SNAKE") {
            if (key_code == KEY_UP) snake_dir(0,-1);
            else if (key_code == KEY_DOWN) snake_dir(0, 1);
            else if (key_code == KEY_LEFT) snake_dir(-1,0);
            else if (key_code == KEY_RIGHT) snake_dir(1, 0);
        } else if (tipo=="TETRIS") {
            if (key_code == KEY_LEFT) t_move("LEFT");
            else if (key_code == KEY_RIGHT) t_move("RIGHT");
            else if (key_code == KEY_DOWN) t_move("DOWN");
            else if (key_code == KEY_UP) t_rotate();
        }
    }

    // rOTACION TETRIS
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
                
                // bloquear pieza en el area jugable
                const auto& pieceMatrix = pieza[prot];
                for (int cell_y=0; cell_y<(int)pieceMatrix.size(); ++cell_y)
                    for (int cell_x=0; cell_x<(int)pieceMatrix[cell_y].size(); ++cell_x)
                        if (pieceMatrix[cell_y][cell_x] == 1) {
                            int gx = px + cell_x, gy = py + cell_y;
                            if (gy >= 0 && gy < H && gx >= 0 && gx < W) grid[gy][gx] = cur_color;
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
        // wall kicks
        if (!t_collide(px-1, py, next)) { px -= 1; prot = next; return; }
        if (!t_collide(px+1, py, next)) { px += 1; prot = next; return; }
        
    }

    //=================================================================================
    //                           RENDERIZADO O DIBUJADO
    //=================================================================================
    void render() {
        clear();
        
        auto disp = grid;

        // pintar pieza tetris (usa codigos 100+color para pieza activa)
        if (tipo=="TETRIS" && have_piece) {
            const auto& pieceMatrix = pieza[prot];
            for (int cell_y=0; cell_y<(int)pieceMatrix.size(); ++cell_y) {
                for (int cell_x=0; cell_x<(int)pieceMatrix[cell_y].size(); ++cell_x) {
                    if (pieceMatrix[cell_y][cell_x]==1) {
                        int gx = px + cell_x, gy = py + cell_y;
                        if (gy>=0 && gy<H && gx>=0 && gx<W) disp[gy][gx] = 100 + cur_color;
                    }
                }
            }
        }

        // snake: cabeza 3, cuerpo 2, comida 4
        if (tipo=="SNAKE") {
            const int WALL = 5;
            if (W>0 && H>0) {
                for (int cell_x=0; cell_x<W; ++cell_x) {
                    disp[0][cell_x] = WALL;
                    disp[H-1][cell_x] = WALL;
                }
                for (int cell_y=0; cell_y<H; ++cell_y) {
                    disp[cell_y][0] = WALL;
                    disp[cell_y][W-1] = WALL;
                }
            }
            for (size_t i=0;i<snake.size();++i) {
                auto [sx,sy] = snake[i];
                if (sy>=0 && sy<H && sx>=0 && sx<W) disp[sy][sx] = (i==0?3:2);
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
            for (int cell_y=0;cell_y<H;++cell_y,++row) {
                mvprintw(row, 0, "# ");
                for (int cell_x=0;cell_x<W;++cell_x) {
                    int cell_val = disp[cell_y][cell_x];
                    if (cell_val==0) {
                        mvprintw(row, 2 + cell_x*2, "  ");
                    } else {
                        int cp = (cell_val>=100)? (cell_val-100) : cell_val;
                        if (cp < 1 || cp > 7) cp = 4;
                        attron(COLOR_PAIR(cp) | A_BOLD);
                        mvprintw(row, 2 + cell_x*2, "██");
                        attroff(COLOR_PAIR(cp) | A_BOLD);
                    }
                }
                mvprintw(row, 2 + W*2, " #");
                if (cell_y==2) mvprintw(row, 2 + W*2 + 4, "PUNTUACION: %d", score);
                if (cell_y==4) mvprintw(row, 2 + W*2 + 4, "CONTROLES:");
                if (cell_y==5) mvprintw(row, 2 + W*2 + 4, "Flechas");
                if (cell_y==6) mvprintw(row, 2 + W*2 + 4, "'q': Salir");
            }
        } else { // SNAKE render 
            for (int cell_y=0;cell_y<H;++cell_y,++row) {
                mvprintw(row, 0, "# ");
                for (int cell_x=0;cell_x<W;++cell_x) {
                    int cell_val = disp[cell_y][cell_x];
                    if (cell_val==0) {
                        mvprintw(row, 2 + cell_x*2, "  ");
                    } else if (cell_val==2 || cell_val==3) { 
                        int cp = 5;
                        attron(COLOR_PAIR(cp) | A_BOLD);
                        mvprintw(row, 2 + cell_x*2, "██");
                        attroff(COLOR_PAIR(cp) | A_BOLD);
                    } else if (cell_val==4) { 
                        int cp = 7; 
                        attron(COLOR_PAIR(cp) | A_BOLD);
                        mvprintw(row, 2 + cell_x*2, "██");
                        attroff(COLOR_PAIR(cp) | A_BOLD);
                    } else if (cell_val==5) { // walls
                        mvprintw(row, 2 + cell_x*2, "##");
                    } else {
                        mvprintw(row, 2 + cell_x*2, "  ");
                    }
                }
                mvprintw(row, 2 + W*2, " #");
                if (cell_y==2) mvprintw(row, 2 + W*2 + 4, "PUNTUACION: %d", score);
                if (cell_y==4) mvprintw(row, 2 + W*2 + 4, "CONTROLES:");
                if (cell_y==5) mvprintw(row, 2 + W*2 + 4, "Flechas");
                if (cell_y==6) mvprintw(row, 2 + W*2 + 4, "'q': Salir");
            }
        }
        mvprintw(row, 0, "%s", top.c_str());

        refresh();
    }

    //=================================================================================
    //                           MANEJO DE EVENTOS
    //=================================================================================
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
                else if (verbo=="GROW") s_grow(act);
            }
        }
    }

    //TETRIS 
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
        auto shape_color = [&](const string& shape_name)->int{
            if (shape_name=="I") return 1; // cyan
            if (shape_name=="J") return 6; // pink
            if (shape_name=="O") return 3; // yellow
            if (shape_name=="S") return 5; // green
            if (shape_name=="T") return 2; // J
            if (shape_name=="Z") return 7; // red
            if (shape_name=="L") return 3; // use yellow as fallback
            return 4; // white
        };
        cur_color = shape_color(cur_shape);
        if (!pieza.empty() && pieza.size()==1) {
            
            auto rot90 = [](const vector<vector<int>>& inputMatrix){
                int height = (int)inputMatrix.size();
                int width = height? (int)inputMatrix[0].size() : 0;
                vector<vector<int>> rotated(width, vector<int>(height, 0));
                for (int row=0;row<height;++row)
                    for (int col=0;col<width;++col)
                        rotated[col][height-1-row] = inputMatrix[row][col];
                return rotated;
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

    bool t_collide(int pos_x, int pos_y, int rot_index) const {
        if (!have_piece) return false;
        const auto& pieceMatrix = pieza[rot_index];
        for (int yy=0; yy<(int)pieceMatrix.size(); ++yy) {
            for (int xx=0; xx<(int)pieceMatrix[yy].size(); ++xx) {
                if (pieceMatrix[yy][xx]==1) {
                    int gx = pos_x+xx, gy=pos_y+yy;
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
            for (int cell: row) if (cell==0){ full=false; break; }
            if (!full) keep.push_back(row);
        }
        int cleared = H - (int)keep.size();
        if (cleared>0) {
            vector<vector<int>> top(cleared, vector<int>(W,0));
            grid = move(top);
            grid.insert(grid.end(), keep.begin(), keep.end());
            for (int i=0;i<cleared;++i) ejecutar("ON_LINE_CLEAR");
        }
    }

    //SNAKE
    void s_spawn_player(const json& act) {
        int sx = W/2, sy = H/2;
        if (act.contains("params") && act["params"].is_array() && !act["params"].empty()) {
            auto param0 = act["params"][0];
            if (param0.is_array() && param0.size()==2) { sx = (int)param0[0]; sy = (int)param0[1]; }
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
        for (int attempt=0;attempt<5000;++attempt) {
            int x = dx(rng), y = dy(rng);
            bool ocup = false;
            for (auto& segment: snake) if (segment.first==x && segment.second==y){ ocup=true; break; }
            if (!ocup) { food = pair{x,y}; return; }
        }
        food.reset();
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
            ejecutar("ON_EAT_FOOD"); //aumentar score y re-spawnear comida
            snake_grow_pending += 1; // crecer al comer
        }

        if (!ate) {
            if (snake_grow_pending > 0) {
                --snake_grow_pending;
            } else {
                if (!snake.empty()) snake.pop_back();
            }
        }
    }

    void snake_dir(int dx, int dy) {
        // evita reversa inmediata
        if (dx!=0 && sdir.first == -dx) return;
        if (dy!=0 && sdir.second== -dy) return;
        sdir = {dx,dy};
    }

    void s_grow(const json& act) {
        int amount = 1;
        if (act.contains("params") && act["params"].is_array() && !act["params"].empty()) {
            const auto& param0 = act["params"][0];
            if (param0.is_number_integer()) amount = param0.get<int>();
            else if (param0.is_string()) {
                try {
                    amount = stoi(param0.get<string>());
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

    //=================================================================================
    //                                    MAIN
    //=================================================================================
int main(int argc, char** argv){
    if (argc != 2) {
        std::cout << "Uso: " << argv[0] << " <archivo_juego.json>\n";
        return 1;
    }

    try{
        // leer JSON
        std::ifstream in(argv[1]);
        if (!in) { std::cout << "Error: no se pudo abrir " << argv[1] << "\n"; return 1; }
        json game_json; in >> game_json;

        // crear juego
        Juego G;
        G.cargar(game_json);
        G.run();
        return 0;
    } catch (const std::exception& e){
        endwin(); // por si alcanzó a iniciar curses
        std::cout << "Error: " << e.what() << "\n";
        return 1;
    }
}