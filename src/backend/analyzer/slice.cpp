#include "backend/analyzer.hpp"
#include "backend/type.hpp"
#include <cassert>

#include "backend/types/slice.hpp"
#include "frontend/ast.hpp"

static auto _registered = []() {
  analyzer_t::register_analyzer<slice_type_node_t>(
    [](slice_type_node_t &node, analyzer_t &A) -> qualified_type_t * {
      return A.registry_.ensure<slice_type_t>(node.is_mutable, A.analyze(node.element_type.get()));
    });
  return true;
}();
