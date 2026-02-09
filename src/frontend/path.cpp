#include "frontend/path.hpp"
#include "frontend/ast.hpp"
#include "frontend/lexer.hpp"

#include <sstream>

std::string to_string(const generic_t &generic) {
  std::stringstream ss;
  ss << to_string(*generic.binding);

  if (generic.constraints.size() > 0) {
    ss << ": ";
    for (const path_t &elem : generic.constraints) {
      ss << to_string(elem);
    }
  }

  return ss.str();
}

std::string to_string(const path_t &path) {
  std::stringstream ss;
  for (auto i = 0; i < path.segments.size(); ++i) {
    auto &segment = path.segments[i];
    ss << segment.name;
    if (segment.generic_args.size() > 0) {
      ss << '<';
      for (auto j = 0; j < segment.generic_args.size(); ++j) {
        ss << to_string(segment.generic_args[j]);
        if (j < segment.generic_args.size() - 1) ss << ", ";
      }
      ss << ">";
    }
    if (i < path.segments.size() - 1) ss << '.';
  }
  return ss.str();
}

bool path_t::has_generic() const {
  for (auto &s : segments) {
    if (s.generic_args.size() > 0) return true;
  }
  return false;
}

bool generic_t::operator==(const generic_t &other) const {
  if (binding->name != other.binding->name) return false;
  if (constraints.size() != other.constraints.size()) return false;

  bool match = true;
  for (auto i = 0; i < constraints.size(); ++i) {
    auto &left = constraints[i];
    auto &right = other.constraints[i];

    match = match && left == right;
    if (!match) break;
  }
  return match;
}

bool path_element_t::operator==(const path_element_t &other) const {
  if (name != other.name) return false;
  if (generic_args.size() != other.generic_args.size()) return false;

  bool match = true;

  for (auto i = 0; i < generic_args.size(); ++i) {
    match = match && generic_args[i] == other.generic_args[i];

    if (!match) break;
  }
  return match;
}

bool path_t::operator==(const path_t &other) const {
  if (other.segments.size() != segments.size())
    return false;
  bool match = true;

  for (auto i = 0; i < segments.size(); ++i) {
    auto &left = segments[i];
    auto &right = other.segments[i];

    match = match && left == right;
    if (!match) break;
  }
  return match;
}
