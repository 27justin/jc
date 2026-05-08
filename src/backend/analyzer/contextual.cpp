#include "backend/analyzer.hpp"
#include "backend/type.hpp"
#include <cassert>

#include "backend/types/nominal.hpp"
#include "frontend/ast.hpp"
#include "frontend/diagnostic.hpp"

static auto _registered = []() {
  analyzer_t::register_analyzer<contextual_node_t>(
    [](contextual_node_t &node, analyzer_t &A) -> qualified_type_t * {
      if (A.type_hints.empty()) {
        A.error(node.get<node_location_t>(), diagnostic_t::code_t::infer_error, {});
        UNREACHABLE;
      }

      auto latest_hint = A.type_hints.back();
      if (node.primary) {
        auto member = node.primary->segments.back().identifier;

        if (auto result = A.resolve_type_path(latest_hint, member)) {
          return result;
        } else {
          A.error(node.get<node_location_t>(), diagnostic_t::code_t::infer_error, {});
        }
      }
      return latest_hint;
    });
  return true;
}();
