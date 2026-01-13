#include <iostream>
#include <filesystem>
#include <fstream>
#include <sstream>


// #include <frontend/parser.hpp>
// #include <frontend/analyzer.hpp>
// #include <frontend/lexer.hpp>
#include <frontend/source.hpp>

int main(int argc, char **argv) {
  for (auto i = 1; i < argc; ++i) {
    if (std::filesystem::exists(argv[i])) {
      std::stringstream raw;
      std::ifstream stream(argv[i]);
      raw << stream.rdbuf();
      std::string source = raw.str();

      source_t src(source, argv[1]);

      // lexer_t lexer(src);

      // {
      //   lexer_t lexer (source.c_str(), source.size());
      //   token_t token = lexer.next();
      //   while (token.type != token_type_t::specialEof) {
      //     std::cout << "Token: '" << lexer.string(token) << "' ("<<(int)token.type<<" " << token.location.start.line << "," << token.location.start.column << ")\n";
      //     token = lexer.next();
      //   }
      // }

      // lexer_t lexer (source.c_str(), source.size());
      // lexer.set_name(argv[i]);

      // parser_t parser(lexer);

      // auto unit = parser.parse();
      // analyzer_t analyzer (lexer);
      // analyzer.analyze(*unit);
      // dump_ast(*unit);

    }
  }

  std::cout << "Usage: jcc <source files...> -o <executable>\n";
}

