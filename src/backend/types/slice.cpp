#include "backend/types/slice.hpp"
#include <sstream>

slice_type_t::slice_type_t(bool is_mutable, qualified_type_t *element_type)
  : is_mutable(is_mutable)
  , element_type(element_type) {}

bool
slice_type_t::equals(const qualified_type_t &ty) const {
  if (auto other = ty.as<slice_type_t>()) {
    if (other->element_type->equals(*element_type) == false)
      return false;
    if (other->is_mutable != is_mutable)
      return false;
    return true;
  }
  return false;
}

ssize_t
slice_type_t::size() const {
  return BYTESIZE(8); // {ptr,size}
}

std::string
slice_type_t::to_string() const {
  std::stringstream ss;

  if (is_mutable)
    ss << "var ";
  ss << "[]";
  ss << element_type->to_string();

  return ss.str();
}
