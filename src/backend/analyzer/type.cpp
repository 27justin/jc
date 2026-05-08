#include "backend/type.hpp"
#include "backend/analyzer.hpp"
#include "backend/type_registry.hpp"
#include <cassert>

#include "backend/types/pointer.hpp"

#include "frontend/ast.hpp"

static bool _registered = []() {
  analyzer_t::register_analyzer<identifier_type_node_t>(
    [](identifier_type_node_t &type, analyzer_t &A) -> qualified_type_t * {
      auto path      = type.path->to_string();
      auto qualified = A.registry_.resolve(path);

      if (!qualified) {
        A.error(type.get<node_location_t>(), diagnostic_t::code_t::type_not_found, { path });
        return nullptr;
      }

      return qualified;
    });

  analyzer_t::register_analyzer<pointer_type_node_t>(
    [](pointer_type_node_t &type, analyzer_t &A) -> qualified_type_t * {
      auto element = A.analyze(type.element_type.get());

      if (!element) {
        UNREACHABLE;
        return nullptr;
      }

      return A.registry_.ensure<pointer_type_t>(type.indirections, element);
    });

  return true;
}();
