#include "backend/type.hpp"

bool
qualified_type_t::castable(cast_mode_t, const qualified_type_t &o) const {
  if (this == &o)
    return true;
  if (equals(o))
    return true;
  return false;
}

const qualified_type_t *
qualified_type_t::underlying_type() const {
  return this;
}
