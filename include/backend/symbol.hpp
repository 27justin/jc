#pragma once

#include <string>

#include "frontend/path.hpp"
#include "backend/type.hpp"

//
// Symbols are defined resolvable variables/functions.
//   Every symbol has an associated type.
//

enum class symbol_state_t {
  eAlive,
  eMoved
};

struct symbol_t {
  path_t name;
  SP<type_t> type;
  bool is_mutable;
};

