#pragma once

#include <frontend/token.hpp>

#include <cstdint>
#include <cstddef>
#include <functional>
#include <string_view>
#include <string>

bool is_identifier(char c);
bool is_whitespace(char c);

template <typename Pred>
auto negate(Pred p) {
    return [p](auto&&... args) {
        return !p(std::forward<decltype(args)>(args)...);
    };
}

struct lexer_state_t {
  size_t offset;
  uint64_t line, column;
};

struct lexer_t {
public:
  lexer_t(const char* source, size_t length)
    : begin_(source),
      cur_(source),
      end_(source + length),
      line_(1), column_(1) {}

  token_t next();

  bool eof() const;

  std::string_view string(const token_t &) const;

  lexer_state_t state() const;
  void set_state(const lexer_state_t&);

  std::string_view full_line(const token_t &) const;
  size_t line_no(const token_t &) const;
  size_t column_no(const token_t &) const;
  std::string name() const;
  void set_name(std::string new_name);

  std::string_view line(uint64_t) const;
private:
  const char* begin_;
  const char* cur_;
  const char* end_;
  uint64_t line_, column_;
  std::string name_;

  char peek() const;
  char peek(size_t nlen) const;
  char advance();
  size_t skip_until(const std::function<bool(char)> &pred);
};
