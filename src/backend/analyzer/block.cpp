#include "backend/analyzer.hpp"
#include "backend/type.hpp"
#include "backend/type_registry.hpp"
#include <cassert>

#include "frontend/ast.hpp"

static bool _registered = []() {
  analyzer_t::register_analyzer<block_node_t>([](block_node_t &type, analyzer_t &A) {
    qualified_type_t *void_type         = A.registry_.resolve("void");
    qualified_type_t *block_result_type = void_type;
    for (auto &stmt : type.statements) {
      if (auto return_type = A.analyze(stmt.get())) {
        block_result_type = return_type;
      } else {
        block_result_type = void_type;
      }
    }
    return block_result_type;
  });

  return true;
}();
