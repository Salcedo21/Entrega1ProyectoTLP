#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <sstream>
#include <regex>
#include <stdexcept>
#include <algorithm>
#include <cctype>
#include <cstdlib>
#include <unordered_set>
#include <cstring>
#include <filesystem>

using namespace std;
typedef vector<string> vs;

// dia numero 7, buena suerte entendiendo lo que hice camilo y dilan :d
/**
 * @brief Recorre todas las coincidencias de un regex en un texto y ejecuta un callback por cada una.
 * @tparam Func Tipo del callback que recibe un const std::smatch&.
 * @param sourceText Texto donde buscar.
 * @param pattern Expresion regular compilada.
 * @param onMatch Funcion a ejecutar por cada coincidencia.
 */

template <class Func>
static void for_each_match(const string& sourceText, const regex& pattern, Func onMatch)    {
    smatch matchResult;
    string remainingText = sourceText;
    while (regex_search(remainingText, matchResult, pattern)) {
        onMatch(matchResult);
        remainingText = matchResult.suffix().str();
    }
}


/**
 * @brief Elimina espacios/tabs/saltos al inicio y final de una cadena.
 */
static inline string trim(const string& inputText){
    size_t firstNonWs = inputText.find_first_not_of(" \t\r\n");
    if( firstNonWs == string::npos ) {
        return "";
    }
    size_t lastNonWs = inputText.find_last_not_of(" \t\r\n");
    return inputText.substr(firstNonWs, lastNonWs-firstNonWs+1);
}

/**
 * @brief Lee un archivo completo a un string (simple).
 * @throws runtime_error si no se puede abrir.
 */
static string readFile(const string& path){
    ifstream inFile(path.c_str());
    if(!inFile.is_open()) throw runtime_error("Error: No se pudo abrir el archivo " + path);
    stringstream buffer; buffer << inFile.rdbuf();
    return buffer.str();
}

/**
 * @brief Escribe un string en un archivo (sobrescribe).
 * @throws runtime_error si no se puede abrir para escribir.
 */
static void writeFile(const string& path, const string& data){
    ofstream outFile(path.c_str());
    if(!outFile.is_open()) throw runtime_error("No se pudo escribir: " + path);
    outFile << data;
}

/**
 * @brief Quita comentarios
 *        Respeta texto entre comillas dobles.
 */
/**
 * @brief Elimina comentarios
 *        pero conserva el texto entre comillas ("...").
 *        Ideal para limpiar archivos .brik antes del análisis.
 */
static string stripCommentsSafe(const string& input){
    string output;
    output.reserve(input.size());
    bool insideString = false;

    for (size_t i = 0; i < input.size(); ++i) {
        char c = input[i];
        char next = (i + 1 < input.size()) ? input[i + 1] : '\0';

        if (insideString) {
            output.push_back(c);
            if (c == '"' && input[i - 1] != '\\')
                insideString = false;
            continue;
        }

        if (c == '"' && input[i - 1] != '\\') {
            insideString = true;
            output.push_back(c);
            continue;
        }
        if (c == '/' && next == '/') {
            // Saltar hasta el salto de línea
            while (i < input.size() && input[i] != '\n') ++i;
            output.push_back('\n'); // conservar salto de línea
            continue;
        }
        if (c == '/' && next == '*') {
            i += 2;
            while (i + 1 < input.size() && !(input[i] == '*' && input[i + 1] == '/')) ++i;
            i++; // saltar '/'
            continue;
        }
        output.push_back(c);
    }

    return output;
}


/* =============================
   LEXER
   ============================= */

/**
 * @brief Tipos de token reconocidos por el lexer.
 *        (Incluye GT/LT para imprimir bonito, aunque el parser guarda condiciones como texto.)
 */
enum class TokType {
    IDENT, KEYWORD, STRING, NUMBER,
    LBRACE, RBRACE, LBRACK, RBRACK, LPAREN, RPAREN,
    COMMA, SEMI, COLON, EQUALS, DOT,
    ARROW, SCOPE, GE, LE, EQ, NE, GT, LT,
    PLUS, MINUS, STAR, SLASH,
    END
};

/**
 * @brief Token lexico (tipo, texto y posicion de inicio).
 */
struct Token {
    TokType type;
    string  text;
    int     line;
    int     col;
};

/**
 * @brief Conjunto de palabras reservadas.
 */
static const unordered_set<string> KEYWORDS = {
    "type","record","ctes","movement","rotation","pieces","clock","scoring",
    "rules","game","using","when","transition","entity",
    "board","tablero","state","combo","line","drop","tickMs","speed","gravity",
    "PLAYING","PAUSE","WON","LOST","APPEARING","ACTIVE","LOCKED","CLEANING","ENTRY",
    "LEFT","RIGHT","DOWN","SPACE","ROTATECW","ROTATECCW","ROTATE180","HOLD",
    "UP"
};

/**
 * @brief Analizador lexico.
 */
class Lexer {
public:
    /**
     * @brief Crea el lexer con el texto fuente completo.
     */
    explicit Lexer(const string& sourceText):
        sourceText_(sourceText),
        sourceLength_(sourceText.size()),
        index_(0),
        lineNumber_(1),
        columnNumber_(1) {}

    /**
     * @brief Convierte el texto en una lista de tokens. Agrega token END al final.
     */
    vector<Token> tokenize(){
        vector<Token> tokenList;
        while(true){
            skipWhitespace_();
            if(index_ >= sourceLength_){
                tokenList.push_back(Token{TokType::END,"",lineNumber_,columnNumber_});
                break;
            }

            char currentChar = sourceText_[index_];

            // Cadenas "..."
            if(currentChar=='"'){
                int tokenLine = lineNumber_, 
                tokenCol = columnNumber_;
                ++index_; 
                ++columnNumber_;
                string stringValue;
                
                while((index_ < sourceLength_) && (sourceText_[index_] != '"')){
                    if((sourceText_[index_] == '\\') && (index_+1 < sourceLength_)){
                        stringValue.push_back(sourceText_[index_]);
                        ++index_; 
                        ++columnNumber_;
                    }
                    stringValue.push_back(sourceText_[index_]);
                    ++index_; 
                    ++columnNumber_;
                }

                if(index_ < sourceLength_ && sourceText_[index_]=='"'){ 
                    ++index_; 
                    ++columnNumber_; 
                }
                tokenList.push_back(Token{TokType::STRING, stringValue, tokenLine, tokenCol});
                continue;
            }

            // Numeros (negativos y con punto)
            if(isdigit((unsigned char)currentChar) || (currentChar=='-' && index_+1<sourceLength_ && isdigit((unsigned char)sourceText_[index_+1]))){
                int tokenLine = lineNumber_, tokenCol = columnNumber_;
                size_t scan = index_ + (currentChar == '-');
                bool hasDot = false;


                while(scan < sourceLength_){
                    if(isdigit((unsigned char)sourceText_[scan])) scan++;
                    else if(sourceText_[scan] == '.' && !hasDot){ 
                        hasDot=true; 
                        scan++; 
                    }
                    else break;
                }

                string numberLexeme = sourceText_.substr(index_, scan - index_);
                advance_(scan - index_);
                
                tokenList.push_back(Token{TokType::NUMBER, numberLexeme, tokenLine, tokenCol});
                
                continue;
            }

            // Identificadores y palabras clave
            if(isalpha((unsigned char)currentChar) || (currentChar == '_')){
                int tokenLine = lineNumber_, tokenCol = columnNumber_;

                size_t scan = index_;
                
                while(scan < sourceLength_ && (isalnum((unsigned char)sourceText_[scan]) || sourceText_[scan]=='_')) scan++;
                string identLexeme = sourceText_.substr(index_, scan - index_);
                advance_(scan - index_);
                if(KEYWORDS.count(identLexeme)) tokenList.push_back(Token{TokType::KEYWORD, identLexeme, tokenLine, tokenCol});
                else tokenList.push_back(Token{TokType::IDENT,   identLexeme, tokenLine, tokenCol});
                continue;
            }

            // Operadores de dos caracteres
            if(match_("->")){ 
                tokenList.push_back(Token{TokType::ARROW,"->",lineNumber_,columnNumber_}); 
                advance_(2);
                 
                continue; 
            }
            if(match_("::")){ 
                tokenList.push_back(Token{TokType::SCOPE,"::",lineNumber_,columnNumber_}); 
                advance_(2); 
                continue; 
            }
            if(match_(">=")){ 
                tokenList.push_back(Token{TokType::GE,">=",lineNumber_,columnNumber_}); 
                advance_(2); 
                continue; }
            if(match_("<=")){ 
                tokenList.push_back(Token{TokType::LE,"<=",lineNumber_,columnNumber_}); 
                advance_(2); 
                continue; 
            }
            if(match_("==")){ 
                tokenList.push_back(Token{TokType::EQ,"==",lineNumber_,columnNumber_});
                advance_(2); 
                continue; 
            }
            if(match_("!=")){ 
                tokenList.push_back(Token{TokType::NE,"!=",lineNumber_,columnNumber_}); 
                advance_(2); 
                continue; 
            }

            // Simbolos de un caracter
            int tokenLine = lineNumber_;
            int tokenCol = columnNumber_;
            switch(currentChar){
                case '{': tokenList.push_back(Token{TokType::LBRACE,"{",tokenLine,tokenCol}); advance_(1); break;
                case '}': tokenList.push_back(Token{TokType::RBRACE,"}",tokenLine,tokenCol}); advance_(1); break;
                case '[': tokenList.push_back(Token{TokType::LBRACK,"[",tokenLine,tokenCol}); advance_(1); break;
                case ']': tokenList.push_back(Token{TokType::RBRACK,"]",tokenLine,tokenCol}); advance_(1); break;
                case '(': tokenList.push_back(Token{TokType::LPAREN,"(",tokenLine,tokenCol}); advance_(1); break;
                case ')': tokenList.push_back(Token{TokType::RPAREN,")",tokenLine,tokenCol}); advance_(1); break;
                case ',': tokenList.push_back(Token{TokType::COMMA,",",tokenLine,tokenCol}); advance_(1); break;
                case ';': tokenList.push_back(Token{TokType::SEMI,";",tokenLine,tokenCol}); advance_(1); break;
                case ':': tokenList.push_back(Token{TokType::COLON,":",tokenLine,tokenCol}); advance_(1); break;
                case '=': tokenList.push_back(Token{TokType::EQUALS,"=",tokenLine,tokenCol}); advance_(1); break;
                case '.': tokenList.push_back(Token{TokType::DOT,".",tokenLine,tokenCol}); advance_(1); break;
                case '+': tokenList.push_back(Token{TokType::PLUS,"+",tokenLine,tokenCol}); advance_(1); break;
                case '-': tokenList.push_back(Token{TokType::MINUS,"-",tokenLine,tokenCol}); advance_(1); break;
                case '*': tokenList.push_back(Token{TokType::STAR,"*",tokenLine,tokenCol}); advance_(1); break;
                case '/': tokenList.push_back(Token{TokType::SLASH,"/",tokenLine,tokenCol}); advance_(1); break;
                default:  advance_(1); break;
            }
        }
        return tokenList;
    };

private:
    /**  Texto fuente y posiciones internas */
    const string sourceText_;
    size_t sourceLength_;
    size_t index_;
    int lineNumber_;
    int columnNumber_;

    /** @brief Compara el literal en la posicion actual. */
    bool match_(const char* literal){
        size_t len = strlen(literal);
        if(index_ + len > sourceLength_) return false;
        for(size_t k = 0; k < len; k++) if(sourceText_[index_+k] != literal[k]) return false;
        return true;
    }

    /** @brief Avanza count caracteres y actualiza linea/columna. */
    void advance_(size_t count){
        while(count--){
            if(index_ < sourceLength_){
                if(sourceText_[index_] == '\n'){ lineNumber_++; columnNumber_ = 1; }
                else { columnNumber_++; }
                index_++;
            }
        }
    }

    /** @brief Salta espacios en blanco (espacio, tab, CR, LF). */
    void skipWhitespace_(){
        while(index_ < sourceLength_){
            char ch = sourceText_[index_];
            if(ch==' '||ch=='\t'||ch=='\r'||ch=='\n') advance_(1);
            else break;
        }
    }
};

/**
 * @brief Convierte tipo de token a nombre legible.
 */
static string tokName(TokType t){
    switch(t){
        case TokType::IDENT: return "IDENT"; 
        case TokType::KEYWORD: return "KEYWORD";
        case TokType::STRING: return "STRING"; 
        case TokType::NUMBER: return "NUMBER";
        case TokType::LBRACE: return "LBRACE"; 
        case TokType::RBRACE: return "RBRACE";
        case TokType::LBRACK: return "LBRACKET"; 
        case TokType::RBRACK: return "RBRACKET";
        case TokType::LPAREN: return "LPAREN"; 
        case TokType::RPAREN: return "RPAREN";
        case TokType::COMMA: return "COMMA"; 
        case TokType::SEMI: return "SEMI";
        case TokType::COLON: return "COLON"; 
        case TokType::EQUALS: return "EQUALS";
        case TokType::DOT: return "DOT"; 
        case TokType::ARROW: return "ARROW"; 
        case TokType::SCOPE: return "SCOPE";
        case TokType::GE: return "GE"; 
        case TokType::LE: return "LE"; 
        case TokType::EQ: return "EQ"; 
        case TokType::NE: return "NE";
        case TokType::GT: return "GT"; 
        case TokType::LT: return "LT";
        case TokType::PLUS: return "PLUS"; 
        case TokType::MINUS: return "MINUS"; 
        case TokType::STAR: return "STAR"; 
        case TokType::SLASH: return "SLASH";
        case TokType::END: return "END";
    } return "UNK";
    // que maluquera
}

/**
 * @brief Imprime tokens en formato (Tipo, "texto", linea:col).
 */
static void printTokens(const vector<Token>& tokens){
    cout << "--- Analisis Lexico (Lexer) ---\n";
    cout << "Tokens reconocidos:\n";
    for(const auto& tk: tokens)
        cout << "(" << tokName(tk.type) << ", \"" << tk.text << "\", " << tk.line << ":" << tk.col << ")\n";
}

struct Ctes { 
    string name; 
    vs values; 
};
struct Record { 
    string name; 
    vector<pair<string,string>> 
    fields; 
};
struct MovementFunc { 
    string name; 
    string signature; 
};
struct MovementBlock { 
    string name; 
    vector<MovementFunc> funcs;
};
struct RotationFunc { 
    string name; 
    string signature; 
};
struct RotationBlock { 
    string name; 
    vector<RotationFunc> funcs; 
};
struct PieceDef { 
    string id; 
    string color; 
    string R0; 
};
struct PiecesBlock { 
    string name; 
    string usingRot; 
    vector<PieceDef> pieces; 
};
struct Entity { 
    string name; 
    vector<pair<string,string>> 
    fields; 
};
struct ClockConst { 
    string key; 
    string value; 
};
struct ClockFunc  { 
    string name; 
    string signature; 
};
struct ClockBlock { 
    string name; 
    vector<ClockConst> consts; 
    vector<ClockFunc> funcs; 
};
struct ScoringBlock { 
    string name; 
    map<string,int> line; 
    map<string,int> drop; 
    string comboSig; 
    map<string,string> simple; 
};
struct Rule { 
    string name; 
    vs conditions; 
    vs effects; 
};
struct RulesBlock { 
    string name; 
    vector<Rule> rules; 
};
struct GameUsing { 
    string kind; 
    string what; 
};
struct GameBoard { 
    string widthType, heightType, widthVal, heightVal; 
};
struct GameProp  { 
    string key, value; 
};
struct GameBlock { 
    string name; 
    GameBoard board; 
    vector<GameUsing> usings; 
    vector<GameProp> props; 
};

/* =============================
   El PARSERO
   ============================= */

static vector<Ctes> parseCtes(const string& src){
    vector<Ctes> out;
    
    regex re(R"(ctes\s+([A-Za-z_]\w*)\s*\{([^}]*)\})", regex::icase);
    for_each_match(src, re,
        // Funcion lambda
        [&](const smatch& m){
            Ctes c;
            c.name = trim(m[1].str());
            string body = m[2].str();
            regex itemRe(R"(([A-Za-z_]\w*))");
            for_each_match(body, itemRe,
                // funcion lamba
                [&](const smatch& im){ 
                    c.values.push_back(im[1].str()); 
                    });
            out.push_back(std::move(c));
            });
    return out;
}

static vector<Record> parseRecords(const string& src){
    vector<Record> out;
    
    regex re(R"(record\s+([A-Za-z_]\w*)\s*\(([^)]*)\)\s*;?)", regex::icase);
    for_each_match(src, re,
        // Funcion lambda 
        [&](const smatch& m){
            Record r;
            r.name = m[1].str();
            string fields = m[2].str();
            
            regex fre(R"(([A-Za-z_]\w*)\s*:\s*([A-Za-z_]\w*))");
            for_each_match(fields, fre, 
                //funcion lambda
                [&](const smatch& fm) {
                    r.fields.emplace_back(fm[1].str(), fm[2].str());
                    });
            out.push_back(std::move(r));
        });
    return out;
}

static vs parseTypes(const string& src){
    vs out;
    regex re(R"(\btype\s+([A-Za-z_]\w*)\s*;)", regex::icase);
    for_each_match(src, re, 
        // funciona lambda
        [&](const smatch& m){ 
            out.push_back(m[1].str()); 
        });
    return out;
}

static vector<MovementBlock> parseMovement(const string& src){
    vector<MovementBlock> out;
    regex re(R"(movement\s+([A-Za-z_]\w*)\s*\{([^}]*)\})", regex::icase);

    for_each_match(src, re, [&](const smatch& m){
        MovementBlock mb;
        mb.name = m[1].str(); 
        string body = m[2].str();     
        regex fr(R"(([A-Za-z_]\w*)\s*\(([^)]*)\)\s*->\s*([A-Za-z_]\w*)\s*;?)");
        for_each_match(body, fr, 
            // funcion lambda
            [&](const smatch& fm){ 
                mb.funcs.push_back({ fm[1].str(), fm[2].str() + " -> " + fm[3].str() }); 
                });
        out.push_back(std::move(mb));
    });
    return out;
}

static vector<RotationBlock> parseRotation(const string& src){
    vector<RotationBlock> out;
    regex re(R"(rotation\s+([A-Za-z_]\w*)\s*\{([^}]*)\})", regex::icase);

    for_each_match(src, re, 
        // funcion lamba
        [&](const smatch& m){
        RotationBlock rb;
        rb.name = m[1].str(); string body = m[2].str();
        regex fr(R"(([A-Za-z_]\w*)\s*\(([^)]*)\)\s*->\s*([A-Za-z_]\w*)\s*;?)");
        for_each_match(body, fr, 
            //funcion lambda
            [&](const smatch& fm){ 
                rb.funcs.push_back({ fm[1].str(), fm[2].str() + " -> " + fm[3].str() }); 
                });
        out.push_back(std::move(rb));
        });
    return out;
}

// pieces (soporta shape{...} y R0=[...])
static vector<PiecesBlock> parsePieces(const string& src){
    vector<PiecesBlock> out;
    regex header(R"(pieces\s+([A-Za-z_]\w*)(?:\s+using\s+([A-Za-z_]\w*))?\s*\{)", regex::icase);
    smatch m;
    string rest = src;
    size_t offset = 0;

    while (regex_search(rest, m, header)){
        PiecesBlock pb;
        pb.name = m[1].str();
        pb.usingRot = m[2].matched? m[2].str() : "";

        size_t headerPos = (size_t)m.position(0);
        size_t headerLen = (size_t)m.length(0);
        size_t bodyStart = offset + headerPos + headerLen;

        int depth=1;
        size_t k = bodyStart;
        while((k < src.size()) && (depth > 0)){
            if( src[k] == '{' ) depth++;
            else if( src[k] == '}' ) depth--;
            k++;
        }
        size_t bodyEnd = k-1;
        string body = src.substr(bodyStart, bodyEnd - bodyStart);

        size_t i = 0;
        while(i<body.size()){
            while ( (i < body.size()) && (isspace((unsigned char)body[i]))) i++;
            if(i>=body.size()) break;
            if(isupper((unsigned char)body[i]) && (i+1 < body.size())) {
                string id(1, body[i]);
                size_t j=i+1; 
                while(j < body.size() && isspace((unsigned char)body[j])) j++;

                if((j < body.size()) && (body[j] == '{')) {
                    int d=1;
                    size_t t=j+1;
                    while( (t < body.size()) && (d > 0)) {
                        if(body[t]=='{') d++;
                        else if(body[t]=='}') d--;
                        t++;
                    }
                    string pbody = body.substr(j+1, (t-1)-(j+1));
                    PieceDef pd;
                    pd.id = id;

                    smatch cmm;
                    if(regex_search(pbody, cmm, regex(R"(color\s*=\s*"([^"]*))"))) pd.color = cmm[1].str();

                    smatch smm;
                    if(regex_search(pbody, smm, regex(R"(shape\s*\{[^}]*R0\s*=\s*\[([^\]]*)\][^}]*\})", regex::icase))){
                        pd.R0 = "[" + trim(smm[1].str()) + "]";
                    }

                    pb.pieces.push_back(pd);
                    i = t;
                    continue;
                }
            }
            i++;
        }

        out.push_back(std::move(pb));
        size_t nextPos = headerPos + headerLen + (bodyEnd - bodyStart) + 1;
        offset += nextPos;
        rest = src.substr(offset);
    }
    return out;
}

static vector<Entity> parseEntities(const string& src){
    vector<Entity> out;
    regex re(R"(entity\s+([A-Za-z_]\w*)\s*\{([^}]*)\})", regex::icase);
    for_each_match(src, re, 
        // funcion lamdaba
        [&](const smatch& m){
        Entity e; e.name = m[1].str(); string body = m[2].str();
        regex fre(R"(([A-Za-z_]\w*)\s*:\s*([A-Za-z_]\w*(?:\[\])?)\s*;)");
        for_each_match(body, fre, 
            // funcion lambda
            [&](const smatch& fm){ 
                    e.fields.emplace_back(fm[1].str(), fm[2].str()); 
                });
        out.push_back(std::move(e));
        });
    return out;
}

static vector<ClockBlock> parseClock(const string& src){
    vector<ClockBlock> out;
    regex re(R"(clock\s+([A-Za-z_]\w*)\s*\{([^}]*)\})", regex::icase);
    for_each_match(src, re,
        // funcion lambda
        [&](const smatch& m){
            ClockBlock cb;
            cb.name = m[1].str();
            string body = m[2].str();
            
            regex kvr(R"(([A-Za-z_]\w*)\s*=\s*([^;{}]+);)");
            for_each_match(body, kvr, 
                // funcion lambda
                [&](const smatch& kv){ 
                    cb.consts.push_back({ trim(kv[1].str()), trim(kv[2].str()) }); 
                });
    
            regex fr(R"(([A-Za-z_]\w*)\s*\(([^)]*)\)\s*->\s*([A-Za-z_]\w*)\s*;?)");
            for_each_match(body, fr, 
                // funcion lambda
                [&](const smatch& fm){ 
                    cb.funcs.push_back({ fm[1].str(), fm[2].str() + " -> " + fm[3].str() }); 
                });
                
            out.push_back(std::move(cb));
        });
    return out;
}

static vector<ScoringBlock> parseScoring(const string& src){
    vector<ScoringBlock> out;
    regex re(R"(scoring\s+([A-Za-z_]\w*)\s*\{([^}]*)\})", regex::icase);
    for_each_match(src, re, 
        //funcion lambda
            [&](const smatch& m){
            ScoringBlock sb; sb.name = m[1].str(); string body = m[2].str();

            smatch lm;
            if(regex_search(body, lm, regex(R"(line\s*\{([^}]*)\})", regex::icase))){
                string lb = lm[1].str();
                regex rkv(R"(([0-9A-Za-z_]+)\s*=\s*([0-9]+)\s*;?)");
                for_each_match(lb, rkv, 
                    // funcion lambona
                    [&](const smatch& kv){ 
                        sb.line[kv[1].str()] = atoi(kv[2].str().c_str()); 
                        });
            }
            smatch dm;
            if(regex_search(body, dm, regex(R"(drop\s*\{([^}]*)\})", regex::icase))){
                string db = dm[1].str();
                regex rkv(R"(([A-Za-z_]\w*)\s*=\s*([0-9]+)\s*;?)");
                for_each_match(db, rkv, 
                    //funcion lambda
                    [&](const smatch& kv){ 
                        sb.drop[kv[1].str()] = atoi(kv[2].str().c_str()); 
                        });
            }
            smatch cm;
            if(regex_search(body, cm, regex(R"(combo\s*\(([^)]*)\)\s*->\s*([A-Za-z_]\w*)\s*;?)", regex::icase))){
                sb.comboSig = cm[1].str() + " -> " + cm[2].str();
            }
            regex simpRe(R"(([A-Za-z_]\w*)\s*:\s*([A-Za-z_]\w*)\s*;)");
            for_each_match(body, simpRe, 
                //funcion lambda
                [&](const smatch& simp){ 
                    sb.simple[simp[1].str()] = simp[2].str(); 
                });

            out.push_back(std::move(sb));
            });
    return out;
}

static vector<RulesBlock> parseRules(const string& src){
    vector<RulesBlock> out;
    regex re(R"(rules\s+([A-Za-z_]\w*)\s*\{([^}]*)\})", regex::icase);
    for_each_match(src, re, 
        //funcion lambda
        [&](const smatch& m){
            RulesBlock rb; rb.name = m[1].str(); string body = m[2].str();
            regex rr(R"(([A-Za-z_]\w*)\s*:\s*([^;]+))");
            for_each_match(body, rr, 
                //funcion lambda
                [&](const smatch& rm){
                    Rule r; r.name = rm[1].str(); string def = rm[2].str();
                    regex wc(R"(\bwhen\s+([^\n\-\r]+))", regex::icase);
                    for_each_match(def, wc, 
                        // funcion lambda
                        [&](const smatch& cm){ 
                            r.conditions.push_back(trim(cm[1].str())); 
                            });
                    smatch tb;
                    if(regex_search(def, tb, regex(R"(transition\s*\{([^}]*)\})", regex::icase))){
                        string actions = tb[1].str(); string line;
                        stringstream ss(actions);
                        while(getline(ss, line, ';')){ line = trim(line); if(!line.empty()) r.effects.push_back(line); }
                    } else {
                        size_t p = def.find("->");
                        if(p!=string::npos){
                            string rhs = trim(def.substr(p+2));
                            size_t cut = rhs.find("or when");
                            if(cut!=string::npos) rhs = trim(rhs.substr(0,cut));
                            if(!rhs.empty()) r.effects.push_back(rhs);
                        }
                    }
                    rb.rules.push_back(std::move(r));
                });
            out.push_back(std::move(rb));
        });
    return out;
}

static vector<GameBlock> parseGame(const string& src){
    vector<GameBlock> out;
    regex re(R"(game\s+([A-Za-z_]\w*)\s*\{([^}]*)\})", regex::icase);

    for_each_match(src, re,
        //Funcion Lambda 
        [&](const smatch& m){
            GameBlock gb;
            gb.name = m[1].str();
            string body = m[2].str();
            smatch bm;
            
            if(regex_search(body, bm, regex(R"((board|tablero)\s*\{([^}]*)\})", regex::icase))){
                string bb = bm[2].str(); smatch x;
                if(regex_search(bb, x, regex(R"(\bwidth\s*:\s*([A-Za-z_]\w*))"))) gb.board.widthType = x[1].str();
                if(regex_search(bb, x, regex(R"(\bheight\s*:\s*([A-Za-z_]\w*))"))) gb.board.heightType = x[1].str();
                if(regex_search(bb, x, regex(R"(\bwidth\s*:?=\s*([0-9]+))"))) gb.board.widthVal  = x[1].str();
                if(regex_search(bb, x, regex(R"(\bheight\s*:?=\s*([0-9]+))"))) gb.board.heightVal = x[1].str();
            }
            
            regex useRe(R"(using\s+([A-Za-z_]\w*)\s*=\s*([A-Za-z_]\w*)\s*;)");
            for_each_match(body, useRe,
                //Funcion lamba 
                [&](const smatch& um){
                    gb.usings.push_back({ um[1].str(), um[2].str() });
                    });
                    
            regex propRe(R"(([A-Za-z_]\w*)\s*(:\s*[A-Za-z_]\w*)?\s*(?:[:=]\s*([^;\n]+))?\s*;)");
            for_each_match(body, propRe,
                // Funcion lambda
                [&](const smatch& pm){
                    gb.props.push_back({ pm[1].str(), pm[3].matched? trim(pm[3].str()) : "" });
                    });
            out.push_back(std::move(gb));
            });
    return out;
}



// =============================
// Helpers AST
// =============================

// maicol
static string tupleListFromR0(const string& r0){
    string s = trim(r0);
    if(s.empty()) return "[]";
    if(s.front()=='[' && s.back()==']') s = s.substr(1, s.size()-2);
    vs tuplas; 
    string cur; 
    int par = 0;

    for(char ch: s){
        if(ch=='(') par++;
        if(ch==')') par--;
        cur.push_back(ch);
        if(par==0 && ch==')'){ 
            tuplas.push_back(trim(cur)); 
            cur.clear(); 
        }
    }
    ostringstream o; 
    o << "[";
    for(size_t i=0;i<tuplas.size();++i){ 
        if(i) o<<","; 
        o<<tuplas[i]; 
    }
    o << "]"; return o.str();
}


/* =============================
   Build AST text
   ============================= */

static string buildSymbolTableAst(const string& fileName, const string& rawSource){
    string sourceNoComments = stripCommentsSafe(rawSource);

    vs types = parseTypes(sourceNoComments);
    vector<Record> records = parseRecords(sourceNoComments);
    vector<Ctes> consts = parseCtes(sourceNoComments);
    vector<MovementBlock> moves = parseMovement(sourceNoComments);
    vector<RotationBlock> rots = parseRotation(sourceNoComments);
    vector<PiecesBlock> pieces = parsePieces(sourceNoComments);
    vector<Entity> ents = parseEntities(sourceNoComments);
    vector<ClockBlock> clocks = parseClock(sourceNoComments);
    vector<ScoringBlock> scorings = parseScoring(sourceNoComments);
    vector<RulesBlock> rules = parseRules(sourceNoComments);
    vector<GameBlock> games = parseGame(sourceNoComments);

    ostringstream out;
    out << "--- Analisis Sintactico (Parser) ---\n";
    out << "Sintaxis correcta. AST (Tabla de Simbolos) construido.\n";
    out << "Contenido del AST:\n";

    for(const auto& c : consts){
        out << "  ctes_" << c.name << ": [";
        for(size_t i=0;i<c.values.size();++i){ 
            if(i) out<<","; 
            out<<c.values[i]; 
        }
        out << "]\n";
    }
    if(!types.empty()){
        out << "  types: [";
        for(size_t i=0;i<types.size();++i){ 
            if(i) out<<","; 
            out<<types[i]; 
        }
        out << "]\n";
    }
    for(const auto& r: records){
        out << "  record_" << r.name << ": {";
        for(size_t j=0;j<r.fields.size();++j){
            out << r.fields[j].first << "=" << r.fields[j].second;
            if(j+1<r.fields.size()) out << ",";
        }
        out << "}\n";
    }
    for(const auto& mb: moves){
        out << "  movement_" << mb.name << ": [";
        for(size_t j=0;j<mb.funcs.size();++j){
            out << mb.funcs[j].name << "(" << mb.funcs[j].signature << ")";
            if(j+1<mb.funcs.size()) out << ",";
        }
        out << "]\n";
    }
    for(const auto& rb: rots){
        out << "  rotation_" << rb.name << ": [";
        for(size_t j=0;j<rb.funcs.size();++j){
            out << rb.funcs[j].name << "(" << rb.funcs[j].signature << ")";
            if(j+1<rb.funcs.size()) out << ",";
        }
        out << "]\n";
    }
    for(const auto& pb: pieces){
        out << "  pieces_set: " << pb.name;
        if(!pb.usingRot.empty()) out << " using=" << pb.usingRot;
        out << "\n";
        for(const auto& p: pb.pieces){
            out << "    piece_" << p.id << ": {";
            out << "color=" << p.color << ",";
            out << "R0=" << tupleListFromR0(p.R0);
            out << "}\n";
        }
    }
    for(const auto& e: ents){
        out << "  entity_" << e.name << ": {";
        for(size_t j=0;j<e.fields.size();++j){
            out << e.fields[j].first << ":" << e.fields[j].second;
            if(j+1<e.fields.size()) out << ",";
        }
        out << "}\n";
    }
    for(const auto& c: clocks){
        out << "  clock_" << c.name << ": {";
        for(size_t j=0;j<c.consts.size();++j){
            out << c.consts[j].key << "=" << c.consts[j].value;
            if(j+1<c.consts.size()) out << ",";
        }
        out << ",funcs=[";
        for(size_t j=0;j<c.funcs.size();++j){
            out << c.funcs[j].name << "(" << c.funcs[j].signature << ")";
            if(j+1<c.funcs.size()) out << ",";
        }
        out << "]}\n";
    }
    for(const auto& s: scorings){
        out << "  scoring_" << s.name << ": {";
        bool first=true;
        if(!s.line.empty()){
            out << "line={";
            for(auto it=s.line.begin(); it!=s.line.end(); ++it){
                out << it->first << "=" << it->second; auto it2=it; 
                ++it2; 
                if(it2!=s.line.end()) out<<",";
            }
            out << "}"; first=false;
        }
        if(!s.drop.empty()){
            if(!first) out << ",";
            out << "drop={";
            for(auto it=s.drop.begin(); it!=s.drop.end(); ++it){
                out << it->first << "=" << it->second; auto it2=it; ++it2; if(it2!=s.drop.end()) out<<",";
            }
            out << "}"; first=false;
        }
        if(!s.comboSig.empty()){
            if(!first) out << ",";
            out << "combo(" << s.comboSig << ")"; first=false;
        }
        for(auto& kv: s.simple){
            if(!first) out << ",";
            out << kv.first << ":" << kv.second; first=false;
        }
        out << "}\n";
    }
    for(const auto& rb: rules){
        for(const auto& r: rb.rules){
            out << "  rule_" << rb.name << "_" << r.name << ": {";
            out << "conditions=[";
            for(size_t c=0;c<r.conditions.size();++c){ if(c) out<<","; out<<r.conditions[c]; }
            out << "],effects=[";
            for(size_t e=0;e<r.effects.size();++e){ if(e) out<<","; out<<r.effects[e]; }
            out << "]}\n";
        }
    }
    for(const auto& g: games){
        if(!g.board.widthVal.empty() && !g.board.heightVal.empty())
            out << "  dimensiones_tablero: [" << g.board.widthVal << "," << g.board.heightVal << "]\n";
        
        else if(!g.board.widthType.empty() && !g.board.heightType.empty())
            out << "  dimensiones_tablero: [" << g.board.widthType << "," << g.board.heightType << "]\n";
        
        for(const auto& u: g.usings) out << "  using_" << u.kind << ": " << u.what << "\n";
        for(const auto& p: g.props)  out << "  " << p.key << ": " << (p.value.empty()? string("null") : p.value) << "\n";
        out << "  game_name: " << g.name << "\n";
    }
    return out.str();
}

/* =============================
   MAIN
   ============================= */

/**
 * @brief Ejecuta el pipeline: lee .brik, tokeniza, imprime tokens y genera arbol.ast.
 *        - Sin args: procesa "Tetris.brik".
 *        - Con args: procesa cada archivo, y si hay >1, genera arbol_<base>.ast por cada uno.
 */



int main(int argc, char** argv){
    try{
        namespace fs = std::filesystem;

        // === Ajuste universal de directorio de trabajo ===
        // Si existen Tetris.brik o Snake.brik en la carpeta del ejecutable, úsala.
        // Si no, usa la carpeta donde está el fuente (.cpp).
        fs::path execDir = fs::current_path();
        fs::path srcDir  = fs::path(argv[0]).parent_path();
        fs::path baseDir;

        if (fs::exists(execDir / "Tetris.brik") || fs::exists(execDir / "Snake.brik"))
            baseDir = execDir;
        else if (fs::exists(srcDir / "Tetris.brik") || fs::exists(srcDir / "Snake.brik"))
            baseDir = srcDir;
        else
            baseDir = execDir; // fallback

        // Mostrar para debug (opcional)
        cout << "Directorio base: " << baseDir << "\n";

        // ===============================================
        vs inputFiles;
        if( argc > 1 ){
            for(int arg=1; arg<argc; ++arg)
                inputFiles.push_back((baseDir / argv[arg]).string());
        } else {
            inputFiles.push_back((baseDir / "Tetris.brik").string());
        }

        for(size_t fileIndex=0; fileIndex<inputFiles.size(); ++fileIndex){

            const string filePath = inputFiles[fileIndex];
            string sourceText = readFile(filePath);

            Lexer lexer(sourceText);

            vector<Token> tokens = lexer.tokenize();
            printTokens(tokens);
            cout << "\n";

            string astText = buildSymbolTableAst(filePath, sourceText);
            cout << astText;

            string outName;

            if(inputFiles.size()==1) outName = "arbol.ast";
            else{
                string base = filePath; size_t dot = base.find_last_of('.');
                if( dot != string::npos ){
                    base = base.substr(0,dot);
                } 

                outName = string("arbol_") + base + ".ast";
            }
            writeFile(outName, astText);
            cout << "OK -> " << outName << "\n";
        }
    } catch(const exception& excep){
        cerr << excep.what() << "\n";
        return 1;
    }
    return 0;
}
