#include "backend/types/enum.hpp"
#include "backend/analyzer.hpp"
#include "backend/type.hpp"
#include "backend/type_registry.hpp"
#include "backend/types/any.hpp"
#include <cassert>

#include "backend/types/tuple.hpp"
#include "frontend/ast.hpp"
#include "frontend/ast/constant.hpp"
#include "frontend/diagnostic.hpp"

static bool _registered = []() {
  analyzer_t::register_analyzer<enum_definition_node_t>([](enum_definition_node_t &type,
                                                           analyzer_t &A) -> qualified_type_t * {
    bool is_aggregate = false;
    for (auto &variant : type.variants) {
      is_aggregate |= variant->fields.size() > 0;
      if (is_aggregate)
        break;
    }

    if (is_aggregate) {
      aggregate_enum_type_t aggregate;
      for (auto &variant : type.variants) {
        if (variant->fields.size() > 0) {
          std::vector<tuple_member_t> variants;
          for (auto &field : variant->fields) {
            variants.emplace_back(tuple_member_t{ .is_positional = field->name.has_value() == false,
                                                  .name          = field->name.value_or(""),
                                                  .type          = A.analyze(field->type.get()) });
          }
          aggregate.add(variant->identifier, A.registry_.ensure<tuple_type_t>(std::move(variants)));
        } else {
          // Empty variants get the void type
          aggregate.add(variant->identifier, A.registry_.ensure<any_type_t>());
        }
      }
      return A.registry_.ensure<aggregate_enum_type_t>(std::move(aggregate));
    } else {
      scalar_enum_type_t scalar;

      for (auto &variant : type.variants) {
        if (variant->explicit_tag_value) {
          if (auto value = variant->explicit_tag_value->as<constant_value_node_t>()) {
            scalar.add(variant->identifier, std::stoll(value->value));
          } else {
            A.error(variant->explicit_tag_value->get<node_location_t>(),
                    diagnostic_t::code_t::non_const_expression,
                    {});
            UNREACHABLE;
            return nullptr;
          }
        } else {
          // Use MAX(variants) + 1
          scalar.add(variant->identifier);
        }
      }
      return A.registry_.ensure<scalar_enum_type_t>(std::move(scalar));
    }

    UNREACHABLE;
    return nullptr;
  });

  return true;
}();
