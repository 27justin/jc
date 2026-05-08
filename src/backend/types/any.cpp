#include "backend/types/any.hpp"

bool
any_type_t::equals(const qualified_type_t &) const {
  return true;
}

ssize_t
any_type_t::size() const {
  return -1;
}

std::string
any_type_t::to_string() const {
  return "any";
}

bool
any_type_t::castable(cast_mode_t, const qualified_type_t &) const {
  return false;
}
