#include "backend/analyzer.hpp"
#include "backend/type.hpp"
#include "backend/types/pointer.hpp"
#include "frontend/ast.hpp"
#include "frontend/diagnostic.hpp"
#include <cassert>

bool
analyzer_t::is_lvalue(const ast_node_t *node) const {
  return node->is<symbol_node_t>();
}

static bool _registered = []() {
  analyzer_t::register_analyzer<zero_node_t>(
    [](zero_node_t &node, analyzer_t &A) -> qualified_type_t * {
      if (A.type_hints.empty()) {
        A.error(node.get<node_location_t>(), diagnostic_t::code_t::infer_error, {});
        UNREACHABLE;
        return nullptr;
      }
      return A.type_hints.back();
    });

  analyzer_t::register_analyzer<uninitialized_node_t>(
    [](uninitialized_node_t &node, analyzer_t &A) -> qualified_type_t * {
      if (A.type_hints.empty()) {
        A.error(node.get<node_location_t>(), diagnostic_t::code_t::infer_error, {});
        UNREACHABLE;
        return nullptr;
      }
      return A.type_hints.back();
    });

  analyzer_t::register_analyzer<address_of_node_t>(
    [](address_of_node_t &node, analyzer_t &A) -> qualified_type_t * {
      if (!A.is_lvalue(node.value.get())) {
        A.error(node.get<node_location_t>(), diagnostic_t::code_t::address_of_rvalue, {});
        UNREACHABLE;
        return nullptr;
      }

      std::vector<pointer_kind_t> indirections = { PT_NON_NULLABLE };
      return A.registry_.ensure<pointer_type_t>(indirections, A.analyze(node.value.get()));
    });

  analyzer_t::register_analyzer<deref_node_t>(
    [](deref_node_t &node, analyzer_t &A) -> qualified_type_t * {
      auto value_type = A.analyze(node.value);
      auto pointer    = value_type->as<pointer_type_t>();

      if (pointer == nullptr) {
        UNREACHABLE;
        return nullptr;
      }

      std::vector<pointer_kind_t> indirections = pointer->indirections();
      indirections.erase(indirections.begin());
      if (indirections.size() > 0)
        return A.registry_.ensure<pointer_type_t>(indirections, pointer->base_type());

      auto deref_result = pointer->base_type();
      return const_cast<qualified_type_t *>(deref_result);
    });

  return true;
}();
