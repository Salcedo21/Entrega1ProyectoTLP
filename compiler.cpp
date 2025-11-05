// compiler.cpp
// De .brik -> JSON compatible con runtime.py
// C++17 + nlohmann::json (header-only: json.hpp)
// Compilar: g++ -std=c++17 compiler.cpp -o compiler
//
// Uso:
//   ./compiler Tetris.brik -o tetris.json
//   ./compiler Snake.brik  -o snake.json
//   ./compiler --tetris Tetris.brik -o tetris.json
//   ./compiler --snake  Snake.brik  -o snake.json

#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <regex>
#include <map>
#include <algorithm>
#include <cctype>
#include "json.hpp"

using json = nlohmann::json;
using namespace std;

// -------- util básica --------
static string read_file(const string& path) {
    ifstream in(path);
    if (!in) throw runtime_error("No pude abrir: " + path);
    stringstream ss; ss << in.rdbuf();
    return ss.str();
}

static void write_file(const string& path, const string& data) {
    ofstream out(path);
    if (!out) throw runtime_error("No pude escribir: " + path);
    out << data;
}

// strip de comentarios preservando strings
static string strip_comments_safe(const string& input){
    string out; out.reserve(input.size());
    bool in_str = false;
    for (size_t i=0;i<input.size();++i){
        char c = input[i];
        char next = (i+1<input.size()? input[i+1] : '\0');
        char prev = (i>0? input[i-1] : '\0');

        if (in_str){
            out.push_back(c);
            if (c=='"' && prev!='\\') in_str=false;
            continue;
        }
        if (c=='"' && prev!='\\'){ in_str=true; out.push_back(c); continue; }

        // //
        if (c=='/' && next=='/'){
            while (i<input.size() && input[i]!='\n') ++i;
            out.push_back('\n');
            continue;
        }
        // /* ... */
        if (c=='/' && next=='*'){
            i+=2;
            while (i+1<input.size() && !(input[i]=='*' && input[i+1]=='/')) ++i;
            if (i+1<input.size()) ++i; // salta '/'
            continue;
        }
        out.push_back(c);
    }
    return out;
}

static string trim(const string& s){
    size_t a = s.find_first_not_of(" \t\r\n");
    if (a==string::npos) return "";
    size_t b = s.find_last_not_of(" \t\r\n");
    return s.substr(a, b-a+1);
}

static bool contains_icase(const string& hay, const string& needle){
    auto H = hay, N = needle;
    transform(H.begin(), H.end(), H.begin(), ::tolower);
    transform(N.begin(), N.end(), N.begin(), ::tolower);
    return H.find(N) != string::npos;
}

// -------- parse Tetris: shapes R0 -> matriz 0/1 --------
struct Coord { int x,y; };

static vector<Coord> parse_R0_tuples(const string& inside_brackets){
    vector<Coord> out;
    regex tup(R"(\(\s*([-+]?\d+)\s*,\s*([-+]?\d+)\s*\))");
    auto begin = sregex_iterator(inside_brackets.begin(), inside_brackets.end(), tup);
    auto end   = sregex_iterator();
    for (auto it = begin; it != end; ++it){
        int x = stoi((*it)[1].str());
        int y = stoi((*it)[2].str());
        out.push_back({x,y});
    }
    return out;
}

static vector<vector<int>> coords_to_matrix_bbox(const vector<Coord>& pts){
    if (pts.empty()) return {{0}};
    int minx=pts[0].x, maxx=pts[0].x, miny=pts[0].y, maxy=pts[0].y;
    for (auto& p: pts){
        minx=min(minx,p.x); maxx=max(maxx,p.x);
        miny=min(miny,p.y); maxy=max(maxy,p.y);
    }
    int W = maxx-minx+1, H = maxy-miny+1;
    vector<vector<int>> M(H, vector<int>(W, 0));
    for (auto& p: pts){
        int cx = p.x - minx;
        int cy = p.y - miny;
        if (cy>=0 && cy<H && cx>=0 && cx<W) M[cy][cx]=1;
    }
    return M;
}

static json build_tetris_json(const string& source_clean){
    json j;
    j["tipo_juego"] = "TETRIS";
    // grid por defecto (10x20) — en tus .brik el board tiene tipos, no valores
    j["config"]["grid_size"] = {10,20};

    // shapes { ID -> [ R0_matrix ] }
    json shapes = json::object();

    // buscador de piezas: ID { ... shape { R0 = [ (x,y), ... ] } }
    // ID esperado: I,O,T,S,Z,J,L (mayúscula)
    regex piece_block(R"(([IOTSZJL])\s*\{([^}]*)\})");
    auto it = sregex_iterator(source_clean.begin(), source_clean.end(), piece_block);
    auto end = sregex_iterator();
    for (; it!=end; ++it){
        string id = (*it)[1].str();
        string body = (*it)[2].str();

        smatch sm;
        // dentro de shape { ... R0 = [ ... ] ... }
        // capturamos el contenido de los corchetes de R0
        regex r0re(R"(shape\s*\{[^}]*R0\s*=\s*\[([^\]]*)\])", regex::icase);
        if (regex_search(body, sm, r0re)){
            string inside = sm[1].str();
            auto coords = parse_R0_tuples(inside);
            auto M = coords_to_matrix_bbox(coords);
            // volcamos una sola rotación (R0). El runtime.py acepta cantidad arbitraria.
            json rot0 = json::array();
            for (auto& row : M) {
                json jr = json::array();
                for (int v: row) jr.push_back(v);
                rot0.push_back(jr);
            }
            shapes[id] = json::array({ rot0 });
        }
    }
    j["shapes"] = shapes;

    // events mínimos compatibles con tu runtime.py
    // ON_START -> SPAWN (random)
    // ON_TICK  -> MOVE DOWN
    // ON_KEY_* -> mover/rotar
    j["events"] = json::object();
    j["events"]["ON_START"] = json::array({
        { {"accion","SPAWN"},{"objeto",nullptr},{"params",json::array()} }
    });
    j["events"]["ON_TICK"] = json::array({
        { {"accion","MOVE"},{"objeto",nullptr},{"params", json::array({"DOWN"})} }
    });
    j["events"]["ON_KEY_LEFT"] = json::array({
        { {"accion","MOVE"},{"objeto",nullptr},{"params", json::array({"LEFT"})} }
    });
    j["events"]["ON_KEY_RIGHT"] = json::array({
        { {"accion","MOVE"},{"objeto",nullptr},{"params", json::array({"RIGHT"})} }
    });
    j["events"]["ON_KEY_DOWN"] = json::array({
        { {"accion","MOVE"},{"objeto",nullptr},{"params", json::array({"DOWN"})} }
    });
    j["events"]["ON_KEY_UP"] = json::array({
        { {"accion","ROTATE"},{"objeto",nullptr},{"params", json::array()} }
    });
    // si quieres puntuar al limpiar líneas:
    j["events"]["ON_LINE_CLEAR"] = json::array({
        { {"accion","INCREASE_SCORE"},{"objeto",100},{"params", json::array()} }
    });

    return j;
}

// -------- build Snake JSON --------
static json build_snake_json(const string& /*source_clean*/){
    json j;
    j["tipo_juego"] = "SNAKE";
    j["config"]["grid_size"] = {20,20}; // tamaño por defecto

    // eventos mínimos para tu runtime.py
    j["events"] = json::object();
    // aparecer jugador en el centro y comida aleatoria
    j["events"]["ON_START"] = json::array({
        { {"accion","SPAWN"},{"objeto","PLAYER"},{"params", json::array({ json::array({10,10}) })} },
        { {"accion","SPAWN"},{"objeto","FOOD"},{"params", json::array({"RANDOM"})} }
    });
    // cada tick, mover
    j["events"]["ON_TICK"] = json::array({
        { {"accion","MOVE"},{"objeto","PLAYER"},{"params", json::array()} }
    });
    // al comer
    j["events"]["ON_EAT_FOOD"] = json::array({
        { {"accion","INCREASE_SCORE"},{"objeto",10},{"params", json::array()} },
        { {"accion","SPAWN"},{"objeto","FOOD"},{"params", json::array({"RANDOM"})} },
        { {"accion","GROW"},{"objeto",nullptr},{"params", json::array()} }
    });
    // colisiones
    j["events"]["ON_COLLISION_WALL"] = json::array({
        { {"accion","GAME_OVER"},{"objeto",nullptr},{"params", json::array()} }
    });
    j["events"]["ON_COLLISION_SELF"] = json::array({
        { {"accion","GAME_OVER"},{"objeto",nullptr},{"params", json::array()} }
    });

    return j;
}

// -------- CLI y pegamento --------
int main(int argc, char** argv){
    try{
        if (argc < 2){
            cerr << "Uso: " << argv[0] << " [--tetris|--snake] <archivo.brik> -o <salida.json>\n";
            return 1;
        }

        bool force_tetris=false, force_snake=false;
        string in_path, out_path;

        for (int i=1;i<argc;i++){
            string a = argv[i];
            if (a=="--tetris") force_tetris=true;
            else if (a=="--snake") force_snake=true;
            else if (a=="-o" && i+1<argc){ out_path = argv[++i]; }
            else if (a.size()>0 && a[0]!='-'){ in_path = a; }
        }
        if (in_path.empty()){
            cerr << "Falta archivo de entrada (.brik)\n";
            return 1;
        }
        if (out_path.empty()){
            // salida por defecto
            out_path = force_tetris? "tetris.json" : (force_snake? "snake.json" : "game.json");
        }

        string raw = read_file(in_path);
        string clean = strip_comments_safe(raw);

        // detectar tipo si no está forzado
        string type;
        if (force_tetris) type = "TETRIS";
        else if (force_snake) type = "SNAKE";
        else {
            if (contains_icase(clean, "game Tetris")) type = "TETRIS";
            else if (contains_icase(clean, "game Snake")) type = "SNAKE";
            else {
                // fallback: si hay "pieces" con I,O,T,S,Z,J,L asumimos Tetris
                if (contains_icase(clean, "pieces") && (clean.find(" I ")!=string::npos || clean.find(" T ")!=string::npos))
                    type = "TETRIS";
                else
                    type = "SNAKE";
            }
        }

        json out;
        if (type=="TETRIS") out = build_tetris_json(clean);
        else                out = build_snake_json(clean);

        // guarda
        write_file(out_path, out.dump(2));
        cout << "OK: " << out_path << "\n";
        return 0;
    } catch (const exception& e){
        cerr << "ERROR: " << e.what() << "\n";
        return 1;
    }
}
