#include "backend/analyzer.hpp"
#include "backend/type.hpp"
#include "frontend/ast.hpp"
#include <cassert>
#include <filesystem>

#include "frontend/diagnostic.hpp"
#include "frontend/parser.hpp"

static bool _registered = []() {
  analyzer_t::register_analyzer<px_import_node_t>(
    [](px_import_node_t &node, analyzer_t &A) -> qualified_type_t * {
      auto void_type = A.registry_.resolve("void");

      std::string file;
      for (auto &segment : node.path->segments) {
        file.append(segment.identifier);
        file.append("/");
      }
      if (file.size() > 0)
        file.pop_back();
      file.append(".px");

      // Look for the file in our include directories
      bool was_included = false;
      for (auto &dir : A.include_directories) {
        auto path = std::filesystem::path(dir) / file;
        if (std::filesystem::exists(path)) {
          was_included = true;
          // Lex, parse, then do symbol stubbing.
          auto src = std::make_shared<source_t>(source_t::from_file(path));

          auto lex    = lexer_t(src);
          auto parser = parser_t(lex, src);
          auto tu     = parser.parse();
          A.pass_symbols(tu.declarations);
          break;
        }
      }

      if (was_included == false) {
        // Import not found, error
        A.error(node.get<node_location_t>(),
                diagnostic_t::code_t::import_not_found,
                { node.path->to_string() });
        UNREACHABLE;
        return nullptr;
      }

      return void_type;
    });
  return true;
}();
