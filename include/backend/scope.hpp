#pragma once

#include "backend/type.hpp"
#include <map>

struct scope_t {
  SP<symbol_t> resolve(const std::string &identifier);
  SP<symbol_t> add(const std::string &identifier, SP<type_t>, bool is_mutable = false);

  scope_t(SP<scope_t>);
private:
  std::map<std::string, SP<symbol_t>> symbols;
  SP<scope_t> parent;
};

