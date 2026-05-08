#include "backend/type.hpp"
#include "backend/types/enum.hpp"
#include <sstream>

aggregate_enum_type_t::aggregate_enum_type_t()
  : bitsize(TAG_SIZE) {}
aggregate_enum_type_t::aggregate_enum_type_t(aggregate_enum_type_t &&other)
  : tags(std::move(other.tags))
  , bitsize(other.bitsize) {}

void
aggregate_enum_type_t::add(const std::string &tag, qualified_type_t *type) {
  if (tag_by_name(tag) != nullptr) {
    // TODO: Throw error, double definition of tag
  }
  tags.emplace_back(tag, type);
  bitsize = std::max(bitsize, type->size() + TAG_SIZE);
}

const qualified_type_t *
aggregate_enum_type_t::tag_by_name(const std::string &search) const {
  for (auto &[name, ty] : tags) {
    if (name == search)
      return ty;
  }
  return nullptr;
}

ssize_t
aggregate_enum_type_t::size() const {
  return bitsize;
}

std::string
aggregate_enum_type_t::to_string() const {
  ssize_t hash = 0;
  hash ^= std::hash<std::string>{}("enum");

  for (auto &memb : tags) {
    hash ^= std::hash<std::string>{}(memb.first);
    hash ^= std::hash<std::string>{}(memb.second->to_string());
  }

  std::stringstream ss;
  ss << "enum" << hash;
  return ss.str();
}

bool
aggregate_enum_type_t::equals(const qualified_type_t &other) const {
  if (&other == this)
    return true;
  return false;
}

bool
aggregate_enum_type_t::castable(cast_mode_t, const qualified_type_t &other) const {
  if (other.underlying_type() == this)
    return true;
  return false;
}

variant_constructor_type_t::variant_constructor_type_t(aggregate_enum_type_t &aggregate,
                                                       const std::string     &tag)
  : aggregate_(&aggregate)
  , tag_(tag) {
  variant_ = aggregate.tag_by_name(tag);
}

aggregate_enum_type_t *
variant_constructor_type_t::aggregate() {
  return aggregate_;
}

ssize_t
variant_constructor_type_t::size() const {
  return BYTESIZE(8);
}

std::string
variant_constructor_type_t::to_string() const {
  std::stringstream ss;
  ss << aggregate_->to_string() << "." << tag_;
  return ss.str();
}

bool
variant_constructor_type_t::equals(const qualified_type_t &other) const {
  if (&other == this)
    return true;
  return false;
}

bool
variant_constructor_type_t::castable(cast_mode_t, const qualified_type_t &other) const {
  if (&other == this)
    return true;
  return false;
}
