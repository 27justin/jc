#pragma once

#include <string>

#include "backend/type.hpp"

//
// Symbols are defined resolvable variables/functions.
//   Every symbol has an associated type.
//

struct symbol_t {
  std::string name;
  SP<qualified_type_t> type;
};

