#pragma once

#include "frontend/source.hpp"
#include <frontend/token.hpp>

#include <cstdint>
#include <cstddef>
#include <functional>
#include <string_view>
#include <string>

struct lexer_t {
public:
  lexer_t(source_t &source)
      : source(source),
        token() {}

  token_t next();
  token_t peek();
  bool eof() const;
private:
  source_t &source;
  token_t token;
};
