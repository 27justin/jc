#include "backend/symbol.hpp"
#include "backend/scope.hpp"
#include "backend/type.hpp"

#include <memory>

scope_t::scope_t(SP<scope_t> parent) : parent(parent) {
}

SP<symbol_t> scope_t::resolve(const std::string &identifier) {
  if (symbols.contains(identifier))
    return symbols.at(identifier);

  if (parent)
    return parent->resolve(identifier);

  return nullptr;
}

SP<symbol_t> scope_t::add(const std::string &identifier, SP<type_t> type, bool is_mutable) {
  symbols[identifier] = std::make_shared<symbol_t>(symbol_t{
      .name = identifier,
      .type = type,
      .is_mutable = is_mutable
    });
  return symbols.at(identifier);
}


