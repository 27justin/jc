#include <cassert>
#include <iostream>

#include "backend/analyzer.hpp"
#include "backend/type.hpp"
#include "backend/type_registry.hpp"
#include "backend/types/struct.hpp"

#include "frontend/ast.hpp"
#include "frontend/diagnostic.hpp"

static bool _registered = []() {
  analyzer_t::register_analyzer<struct_definition_node_t>(
    [](struct_definition_node_t &type, analyzer_t &A) {
      struct_type_t S;
      for (auto &member : type.members) {
        S.add(member->name, A.analyze(member->type.get()));
      }
      return A.registry_.ensure<struct_type_t>(std::move(S));
    });

  analyzer_t::register_analyzer<struct_init_node_t>(
    [](struct_init_node_t &init, analyzer_t &A) -> qualified_type_t * {
      auto target_type = A.analyze(init.target_type.get());
      if (auto struct_type = target_type->as<struct_type_t>()) {
        // Check each field for conformity

        for (auto &field : init.fields) {
          auto member = struct_type->member_type_by_name(field.name);
          if (!member) {
            A.error(init.get<node_location_t>(),
                    diagnostic_t::code_t::member_not_found,
                    { target_type->to_string(), field.name });
            return nullptr;
          }

          // Check if they are castable
          auto value = A.analyze(field.value.get());
          if (!member->equals(*value) && !value->castable(cast_mode_t::eImplicit, *member)) {
            A.error(init.get<node_location_t>(),
                    value->castable(cast_mode_t::eExplicit, *member) == true
                      ? diagnostic_t::code_t::incompatible_implicit_cast
                      : diagnostic_t::code_t::incompatible_explicit_cast,
                    { value->to_string(), member->to_string() });
            return nullptr;
          }
        }

        return target_type;
      } else {
        A.error(init.get<node_location_t>(),
                diagnostic_t::code_t::not_a_struct,
                { target_type->to_string() });
        return nullptr;
      }
      UNREACHABLE;
      return nullptr;
    });

  return true;
}();
