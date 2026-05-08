#include "backend/types/nominal.hpp"

nominal_type_t::nominal_type_t(const std::string &name, qualified_type_t *base)
  : name(name)
  , base(base) {}

bool
nominal_type_t::equals(const qualified_type_t &type) const {
  if (base->equals(*type.underlying_type()))
    return true;

  auto *other = type.as<nominal_type_t>();
  if (other == nullptr)
    return false;
  if (other->base == base)
    return true;
  if (other->base->equals(*this->base))
    return true;

  return false;
}

bool
nominal_type_t::castable(cast_mode_t mode, const qualified_type_t &other) const {
  return base->castable(mode, *other.underlying_type());
}

ssize_t
nominal_type_t::size() const {
  return base->size();
}

std::string
nominal_type_t::to_string() const {
  return name;
}

const qualified_type_t *
nominal_type_t::underlying_type() const {
  return base->underlying_type();
}

bool
alias_type_t::equals(const qualified_type_t &type) const {
  auto *other = type.as<alias_type_t>();

  if (other == nullptr)
    return false;
  if (other->nominal != nominal)
    return false;

  return true;
}

ssize_t
alias_type_t::size() const {
  return nominal->size();
}

std::string
alias_type_t::to_string() const {
  return nominal->to_string();
}

const qualified_type_t *
alias_type_t::underlying_type() const {
  return nominal->underlying_type();
}
