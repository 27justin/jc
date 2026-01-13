#pragma once

#include "frontend/symbol.hpp"

#include <map>
#include <memory>
#include <string>

struct scope_t {
  std::map<std::string, std::shared_ptr<symbol_t>> symbols;
  std::shared_ptr<symbol_t> resolve(const std::string &);

  std::shared_ptr<scope_t> parent;
};
