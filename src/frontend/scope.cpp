#include "frontend/scope.hpp"

std::shared_ptr<symbol_t>
scope_t::resolve(const std::string &name) {
  if (symbols.contains(name))
    return symbols.at(name);

  if (parent != nullptr)
    return parent->resolve(name);
  return nullptr;
}
