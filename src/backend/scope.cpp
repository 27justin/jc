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

SP<symbol_t> scope_t::add(const std::string &identifier, SP<qualified_type_t> type) {
  symbols[identifier] = std::make_shared<symbol_t>(symbol_t {
    .name = identifier,
    .type = type
    });
  return symbols.at(identifier);
}


