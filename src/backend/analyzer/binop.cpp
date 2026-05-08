#include "backend/analyzer.hpp"
#include "backend/type.hpp"
#include "backend/type_registry.hpp"
#include <cassert>

#include "frontend/ast.hpp"
#include "frontend/diagnostic.hpp"

static bool _registered = []() {
  analyzer_t::register_analyzer<binop_node_t>(
    [](binop_node_t &binop, analyzer_t &A) -> qualified_type_t * {
      auto LHS = A.analyze(binop.left.get());

      // Push type hint for LHS, allows for colon based enum lookups, etc.
      if (LHS)
        A.type_hints.push_back(LHS);

      auto RHS = A.analyze(binop.right.get());

      // Default to the biggest type
      qualified_type_t *result_type = LHS->size() > RHS->size() ? LHS : RHS;

      // Boolean operations return bool
      using BT = binop_type_t;
      switch (binop.op) {
        case binop_type_t::eAnd:
        case binop_type_t::eOr:
        case binop_type_t::eEqual:
        case binop_type_t::eNotEqual:
        case binop_type_t::eGT:
        case binop_type_t::eGTE:
        case binop_type_t::eLT:
        case binop_type_t::eLTE:
          result_type = A.registry_.resolve("bool");
          break;
        case binop_type_t::eAssign:
          // For assignments, we need to verify that the types are implicitly convertible.
          if (!RHS->castable(cast_mode_t::eImplicit, *LHS)) {
            A.error(binop.right->get<node_location_t>(),
                    diagnostic_t::code_t::incompatible_implicit_cast,
                    { RHS->to_string(), LHS->to_string() });
            UNREACHABLE;
            return nullptr;
          }
          // We fall back to LHS to chain them together.
          result_type = LHS;
          break;
        default:
          // Arithmetic types are only allowed within each class
          // i.e.
          // floating point on floating point
          // int on int
          // uint on uint
          if (LHS->equals(*RHS) == false) {
            A.error(binop.right->get<node_location_t>(),
                    diagnostic_t::code_t::incompatible_binary_operation,
                    { to_string(binop.op), LHS->to_string(), RHS->to_string() });
            UNREACHABLE;
            return nullptr;
          }
          break;
      }

      if (LHS)
        A.type_hints.pop_back();
      return result_type;
    });

  return true;
}();
