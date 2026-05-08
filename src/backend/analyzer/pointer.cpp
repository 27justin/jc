#include "backend/types/pointer.hpp"
#include "backend/analyzer.hpp"
#include "backend/type.hpp"
#include "backend/type_registry.hpp"
#include <cassert>

#include "frontend/ast.hpp"
#include "frontend/ast/type.hpp"

static bool _registered = []() {
  analyzer_t::register_analyzer<nil_node_t>([](nil_node_t &symbol, analyzer_t &A) {
    auto                        any_type = A.registry_.resolve("any");
    std::vector<pointer_kind_t> indirections{ PT_NULLABLE };
    return A.registry_.ensure<pointer_type_t>(indirections, any_type);
  });

  return true;
}();
