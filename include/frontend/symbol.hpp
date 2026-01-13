#pragma once

#include "frontend/type.hpp"

#include <vector>

enum class symbol_kind_t {
  eFunction, eVariable, eStruct, eNamespace
};

struct symbol_t;

struct symbol_t {
  std::string name;
  symbol_kind_t kind;

  std::shared_ptr<type_t> ty;
};

