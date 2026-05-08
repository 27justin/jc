#include "backend/types/struct.hpp"
#include <sstream>

struct_type_t::struct_type_t(struct_type_t &&other)
  : members(std::move(other.members)) {}

void
struct_type_t::add(const std::string &member_name, qualified_type_t *type) {
  members.emplace_back(member_name, type);
}

bool
struct_type_t::equals(const qualified_type_t &ty) const {
  if (auto o = ty.as<struct_type_t>()) {
    if (this == o)
      return true;
    if (size() != o->size())
      return false;
    if (members.size() != o->members.size())
      return false;

    for (auto i = 0; i < members.size(); ++i) {
      if (members[i].second->equals(*o->members[i].second) == false)
        return false;
    }
    return true;
  }
  return false;
}

ssize_t
struct_type_t::size() const {
  ssize_t size = 0;
  for (auto &memb : members) {
    size += memb.second->size();
  }
  return BITSIZE(size);
}

std::string
struct_type_t::to_string() const {
  ssize_t hash = 0;
  hash ^= std::hash<std::string>{}("struct");

  for (auto &memb : members) {
    hash ^= std::hash<std::string>{}(memb.first);
    hash ^= std::hash<std::string>{}(memb.second->to_string());
  }

  std::stringstream ss;
  ss << "struct" << hash;
  return ss.str();
}

qualified_type_t *
struct_type_t::member_type_by_name(const std::string &name) const {
  for (auto &memb : members) {
    if (memb.first == name)
      return memb.second;
  }
  return nullptr;
}
