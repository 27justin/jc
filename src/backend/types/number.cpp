#include "backend/types/number.hpp"
#include "backend/type.hpp"
#include <sstream>

float_type_t::float_type_t(ssize_t bitsize)
  : bitsize(bitsize) {}

int_type_t::int_type_t(ssize_t bitsize)
  : bitsize(bitsize) {}

uint_type_t::uint_type_t(ssize_t bitsize)
  : bitsize(bitsize) {}

bool_type_t::bool_type_t() {}

bool
float_type_t::equals(const qualified_type_t &ty) const {
  if (auto other = ty.as<float_type_t>()) {
    return other->bitsize == bitsize;
  }
  return false;
}

bool
int_type_t::equals(const qualified_type_t &ty) const {
  if (auto other = ty.as<int_type_t>()) {
    return other->bitsize == bitsize;
  }
  return false;
}

bool
uint_type_t::equals(const qualified_type_t &ty) const {
  if (auto other = ty.as<uint_type_t>()) {
    return other->bitsize == bitsize;
  }
  return false;
}

bool
bool_type_t::equals(const qualified_type_t &ty) const {
  return ty.is<bool_type_t>();
}

ssize_t
float_type_t::size() const {
  return BITSIZE(bitsize);
}

ssize_t
int_type_t::size() const {
  return BITSIZE(bitsize);
}

ssize_t
uint_type_t::size() const {
  return BITSIZE(bitsize);
}

ssize_t
bool_type_t::size() const {
  return BITSIZE(1);
}

std::string
float_type_t::to_string() const {
  std::stringstream ss;
  ss << "f" << bitsize;
  return ss.str();
}

std::string
int_type_t::to_string() const {
  std::stringstream ss;
  ss << "i" << bitsize;
  return ss.str();
}

std::string
uint_type_t::to_string() const {
  std::stringstream ss;
  ss << "u" << bitsize;
  return ss.str();
}

std::string
bool_type_t::to_string() const {
  return "bool";
}

// Conversion

bool
float_type_t::castable(cast_mode_t mode, const qualified_type_t &other) const {
  if (this == &other)
    return true;

  // Upcasting is always allowed, downcasting only if explicit.
  if (other.is<float_type_t>()) {
    // Upcast
    if (other.size() >= size())
      return true;

    // Downcast
    if (size() < other.size() && mode == cast_mode_t::eExplicit)
      return true;

    return false;
  }

  if (other.is<int_type_t>() || other.is<uint_type_t>()) {
    // Allowed, if >= 32 bits, and explicit
    return other.size() >= BITSIZE(32) && mode == cast_mode_t::eExplicit;
  }
  return false;
}

bool
int_type_t::castable(cast_mode_t mode, const qualified_type_t &other) const {
  if (this == &other)
    return true;

  // Upcasting is always allowed, downcasting only if explicit.
  if (other.is<int_type_t>()) {
    // Upcast
    if (other.size() >= size())
      return true;

    // Downcast
    if (size() < other.size() && mode == cast_mode_t::eExplicit)
      return true;

    return false;
  }

  if (mode == cast_mode_t::eExplicit && other.is<bool_type_t>()) {
    return true;
  }

  // We can also cast away the sign, if explicit.
  if (other.is<uint_type_t>() && mode == cast_mode_t::eExplicit)
    return true;

  return false;
}

bool
uint_type_t::castable(cast_mode_t mode, const qualified_type_t &other) const {
  if (this == &other)
    return true;

  // Upcasting is always allowed, downcasting only if explicit.
  if (other.is<uint_type_t>()) {
    // Upcast
    if (other.size() >= size())
      return true;

    // Downcast
    if (size() < other.size() && mode == cast_mode_t::eExplicit)
      return true;

    return false;
  }

  if (mode == cast_mode_t::eExplicit && other.is<bool_type_t>()) {
    return true;
  }

  // We can also cast to signed, if explicit.
  if (other.is<int_type_t>() && mode == cast_mode_t::eExplicit)
    return true;

  return false;
}

bool
bool_type_t::castable(cast_mode_t mode, const qualified_type_t &other) const {
  if (this == &other)
    return true;

  if (mode == cast_mode_t::eExplicit && (other.is<int_type_t>() || other.is<uint_type_t>())) {
    // We can cast into, and from integers. 0 = false, anything else = true
    return true;
  }

  return false;
}
