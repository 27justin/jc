#include "backend/type.hpp"
#include "backend/types/enum.hpp"
#include "backend/types/number.hpp"
#include <sstream>

scalar_enum_type_t::scalar_enum_type_t() {}
scalar_enum_type_t::scalar_enum_type_t(scalar_enum_type_t &&other)
  : members(std::move(other.members)) {}

ssize_t
scalar_enum_type_t::max() const {
  ssize_t max = 0;
  for (auto &[_, v] : members)
    max = std::max(max, v);
  return max;
}

void
scalar_enum_type_t::add(const std::string &memb, ssize_t value) {
  members[memb] = value;
}

void
scalar_enum_type_t::add(const std::string &memb) {
  members[memb] = max() + 1;
}

bool
scalar_enum_type_t::equals(const qualified_type_t &other) const {
  if (&other == this)
    return true;
  return false;
}

ssize_t
scalar_enum_type_t::size() const {
  return BYTESIZE(4); // i32, TODO: this should be configurable
}

std::string
scalar_enum_type_t::to_string() const {
  ssize_t hash = 0;
  hash ^= std::hash<std::string>{}("enum");

  for (auto &memb : members) {
    hash ^= std::hash<std::string>{}(memb.first);
    hash ^= std::hash<ssize_t>{}(memb.second);
  }

  std::stringstream ss;
  ss << "enum" << hash;
  return ss.str();
}

bool
scalar_enum_type_t::castable(cast_mode_t mode, const qualified_type_t &o) const {
  if (equals(*o.underlying_type()))
    return true;

  if ((o.is<int_type_t>() || o.is<uint_type_t>()) && o.size() >= size() &&
      mode == cast_mode_t::eExplicit)
    return true;

  return false;
}

bool
scalar_enum_type_t::has_member(const std::string &name) const {
  return members.contains(name);
}
