#include "backend/analyzer.hpp"
#include "backend/type.hpp"
#include "backend/type_registry.hpp"
#include "backend/types/slice.hpp"
#include <cassert>

#include "frontend/ast.hpp"

static bool _registered = []() {
  analyzer_t::register_analyzer<constant_value_node_t>(
    [](constant_value_node_t &value, analyzer_t &A) {
      qualified_type_t *result_type = nullptr;

      if (value.type == token_type_t::literalString)
        result_type = A.registry_.ensure<slice_type_t>(false, A.registry_.resolve("u8"));

      if (value.type == token_type_t::literalInt)
        result_type = A.registry_.resolve("i32");

      if (value.type == token_type_t::literalFloat)
        result_type = A.registry_.resolve("f32");

      if (value.type == token_type_t::literalBool)
        result_type = A.registry_.resolve("bool");

      return result_type;
    });

  return true;
}();
