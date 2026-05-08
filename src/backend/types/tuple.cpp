#include "backend/types/tuple.hpp"
#include <sstream>

tuple_type_t::tuple_type_t(const std::vector<tuple_member_t> &members)
  : members(members) {}
tuple_type_t::~tuple_type_t() {}

bool
tuple_type_t::equals(const qualified_type_t &other) const {
  auto *o = other.as<tuple_type_t>();
  if (other.is<tuple_type_t>() == false)
    return false;

  if (!o || members.size() != o->members.size())
    return false;

  for (size_t i = 0; i < members.size(); ++i) {
    if (members[i].name != o->members[i].name)
      return false;
    if (members[i].type != o->members[i].type)
      return false;
  }
  return true;
}

ssize_t
tuple_type_t::size() const {
  ssize_t sizes = 0;
  for (auto memb : members) {
    sizes += memb.type->size();
  }
  return sizes;
}

std::string
tuple_type_t::to_string() const {
  std::stringstream ss;
  ss << "(";
  for (auto i = 0; i < members.size(); ++i) {
    auto &memb = members[i];
    if (!memb.is_positional) {
      ss << *memb.name << ": ";
    }
    ss << memb.type->to_string();

    if (i < members.size() - 1)
      ss << ",";
  }

  ss << ")";
  return ss.str();
}
