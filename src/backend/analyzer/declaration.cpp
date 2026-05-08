#include "backend/analyzer.hpp"
#include "backend/type.hpp"
#include <cassert>

#include "backend/types/nominal.hpp"
#include "frontend/ast.hpp"
#include "frontend/ast/function.hpp"
#include "frontend/diagnostic.hpp"

static auto _registered = []() {
  analyzer_t::register_analyzer<declaration_node_t>(
    [](declaration_node_t &node, analyzer_t &A) -> qualified_type_t * {
      // node.where; // Either destructure_pattern_node_t, or path_node_t

      // Simple case: Declaring a symbol `let sym: i64 = 0;`
      if (auto path = node.where->as<path_node_t>()) {
        if (path->is_templated()) {
          assert(false && "Templates not handled yet.");
        }

        bool              has_type_hint = false;
        qualified_type_t *type          = nullptr;
        if (node.declared_type) {
          type = A.analyze(node.declared_type.get());
          A.type_hints.push_back(type);
          has_type_hint = true;
        }

        qualified_type_t *value_type = A.analyze(node.value);
        // No type, then infer from value.
        if (!type)
          type = value_type;

        if (type->equals(*value_type) == false &&
            value_type->castable(cast_mode_t::eExplicit, *type) == false) {
          A.error(node.get<node_location_t>(),
                  diagnostic_t::code_t::incompatible_explicit_cast,
                  { type->to_string(), value_type->to_string() });
          UNREACHABLE;
          return nullptr;
        }

        if (node.value->is<function_node_t>() || node.value->is<struct_definition_node_t>() ||
            node.value->is<enum_definition_node_t>()) {

          // Not a symbol per-se, but a nominal type definition If it
          // already exists, grab the nominal type and update the
          // `base`.
          //
          // TODO: This is horrible, but if we leave this out, stuff
          // breaks in various places.  Say we use a pointer to a struct
          // in some functions, due to Phase 1 and Phase 2, we
          // preliminarily register the struct as `any_type_t`.  Then,
          // we resolve function parameters, which grab the nominal type
          // that is pointing to <any>, only after that do we actually
          // resolve and store the correct full definition of the
          // struct.
          //
          // Since we stored the nominal once, we /can/ overwrite it,
          // but that doesn't invalidate other types that refer to the
          // old `nominal_type_t` (such as pointers), which then still
          // point to `any`, even though the underlying type is not
          // fully defined.
          if (auto nominal = A.registry_.resolve(path->to_string())) {
            auto temp  = dynamic_cast<nominal_type_t *>(nominal);
            temp->base = type;
          } else {
            A.registry_.ensure_or_overwrite<nominal_type_t>(path->to_string(), type);
          }

          // ... if it's a function_node_t, we also have to track it in our symbol table.
          if (node.value->is<function_node_t>()) {
            A.scope().add(path->to_string(), type);
          }
        } else {
          // Actual symbol
          auto &scope = A.scope();
          scope.add(path->to_string(), type);
        }

        if (has_type_hint) {
          // Remove type hint again.
          A.type_hints.pop_back();
        }

        return type;
      } else {
        assert(false && "Unexpected LHS in declaration");
      }
      return nullptr;
    });
  return true;
}();
