#include "backend/type_registry.hpp"
#include "backend/type.hpp"

#include "backend/types/any.hpp"
#include "backend/types/nominal.hpp"
#include "backend/types/number.hpp"

#include <format>
#include <memory>
#include <sstream>

type_registry_t::type_registry_t() {
  // Register built-ins

  ensure<uint_type_t>(BITSIZE(64));
  ensure<int_type_t>(BITSIZE(64));

  ensure<uint_type_t>(BITSIZE(32));
  ensure<int_type_t>(BITSIZE(32));

  ensure<uint_type_t>(BITSIZE(16));
  ensure<int_type_t>(BITSIZE(16));

  ensure<uint_type_t>(BITSIZE(8));
  ensure<int_type_t>(BITSIZE(8));

  ensure<float_type_t>(BITSIZE(32));
  ensure<float_type_t>(BITSIZE(64));

  ensure<nominal_type_t>("bool", ensure<bool_type_t>());

  ensure<nominal_type_t>("any", ensure<any_type_t>());
  ensure<nominal_type_t>("void", ensure<any_type_t>());
}

qualified_type_t *
type_registry_t::resolve(const std::string &name) {
  if (cache.contains(name)) {
    return cache.at(name);
  }
  return nullptr;
}
