#include "backend/analyzer.hpp"
#include "backend/type.hpp"
#include "frontend/ast.hpp"
#include <cassert>

static bool _registered = []() {
  analyzer_t::register_analyzer<while_node_t>(
    [](while_node_t &node, analyzer_t &A) -> qualified_type_t * {
      auto bool_type = A.registry_.resolve("bool");
      auto void_type = A.registry_.resolve("void");

      auto condition_type = A.analyze(node.condition.get());

      if (!condition_type->castable(cast_mode_t::eImplicit, *bool_type)) {
        A.error(node.condition->get<node_location_t>(),
                diagnostic_t::code_t::incompatible_implicit_cast,
                { condition_type->to_string(), bool_type->to_string() });
        UNREACHABLE;
        return nullptr;
      }

      A.analyze(node.body.get());
      return void_type;
    });
  return true;
}();
