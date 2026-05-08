#include "backend/analyzer.hpp"
#include "backend/type.hpp"
#include "frontend/ast.hpp"
#include "frontend/diagnostic.hpp"
#include <cassert>

static bool _registered = []() {
  analyzer_t::register_analyzer<if_node_t>(
    [](if_node_t &node, analyzer_t &A) -> qualified_type_t * {
      auto              bool_type   = A.registry_.resolve("bool");
      auto              void_type   = A.registry_.resolve("void");
      qualified_type_t *result_type = void_type;

      auto condition_type = A.analyze(node.condition.get());

      if (!condition_type->castable(cast_mode_t::eImplicit, *bool_type)) {
        A.error(node.condition->get<node_location_t>(),
                diagnostic_t::code_t::incompatible_implicit_cast,
                { condition_type->to_string(), bool_type->to_string() });
        UNREACHABLE;
        return nullptr;
      }

      qualified_type_t *pass_type = nullptr, *reject_type = nullptr;

      pass_type = A.analyze(node.pass.get());
      if (node.reject)
        reject_type = A.analyze(node.reject.get());

      if (pass_type && reject_type) {
        // If reject_type can be implicitly cast to pass_type, the if
        // resolves to that type.

        if (reject_type->castable(cast_mode_t::eImplicit, *pass_type)) {
          result_type = pass_type;
        } else {
          A.error(node.get<node_location_t>(),
                  diagnostic_t::code_t::branch_type_mismatch,
                  { reject_type->to_string(), pass_type->to_string() });
          UNREACHABLE;
          return nullptr;
        }
      }
      return result_type;
    });
  return true;
}();
