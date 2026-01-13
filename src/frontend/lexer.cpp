#include "frontend/token.hpp"
#include <frontend/lexer.hpp>
#include <cctype>
#include <functional>
#include <string>
#include <iostream>

bool is_identifier(char c) {
  return std::isalpha(c) || c == '_';
}

bool is_identifier_alnum(char c) {
  return std::isalnum(c) || c == '_';
}

bool is_whitespace(char c) {
  return std::isspace(c);
}

bool is_numeric(char c) {
  return isdigit(c);
}

bool is_number_literal(char c) {
  return std::isdigit(c) || c == '\'' || c == '.';
}

token_type_t delimiter(char c) {
  if (c == '(') return token_type_t::delimiterLParen;
  if (c == ')') return token_type_t::delimiterRParen;
  if (c == '{') return token_type_t::delimiterLBrace;
  if (c == '}') return token_type_t::delimiterRBrace;
  if (c == '[') return token_type_t::delimiterLBracket;
  if (c == ']') return token_type_t::delimiterRBracket;
  if (c == ';') return token_type_t::delimiterSemicolon;
  if (c == '<') return token_type_t::delimiterLAngle;
  if (c == '>') return token_type_t::delimiterRAngle;
  return token_type_t::specialInvalid;
}

token_type_t operator_(char c) {
  using enum token_type_t;
  switch (c) {
  case '+': return operatorPlus;
  case '-': return operatorMinus;
  case '/': return operatorDivide;
  case '*': return operatorMultiply;
  case '%': return operatorMod;
  case '.': return operatorDot;
  case '#': return operatorSize;
  case '=': return operatorEqual;
  case ',': return operatorComma;
  case '!': return operatorNegate;
  case ':': return operatorColon;
  case '`': return operatorLiteral;
  case '|': return operatorPipe;
  case '^': return operatorXor;
  case '&': return operatorAnd;
  case '@': return operatorAt;
  case '?': return operatorWhat;
  case '$': return operatorDollar;
  case '~': return operatorTilde;
  default: return specialInvalid;
  }
}

char lexer_t::peek() const {
    return eof() ? '\0' : *cur_;
}

char lexer_t::peek(size_t nlen) const {
  return (cur_ + (nlen + 1) < end_) ? cur_[nlen] : '\0';
}

char lexer_t::advance() {
  char c = *cur_;

  cur_++;

  if (c == '\n') {
    line_++;
    column_ = 1;
  } else {
    column_++;
  }

  return c;
}

bool lexer_t::eof() const {
    return cur_ >= end_;
}

token_t lexer_t::next() {
lex:
  if (is_whitespace(peek())) {
    advance();
    goto lex;
  }

  token_t token {
    .type = token_type_t::specialInvalid,
    .start = 0,
    .end = 0
  };

  token.start = cur_ - begin_;
  token.location.start = {line_, column_};

  char c = advance();
  if (eof()) {
    token.type = token_type_t::specialEof;
    return token;
  }

  if (is_identifier(c)) {
    // Skip until condition no longer satisfied We skip using
    // `is_identifier_alnum`, since identifier can contain numbers,
    // just not at the beginning.
    size_t total = skip_until(negate(is_identifier_alnum));
    token.end = token.start + total + 1;
    token.type = token_type_t::identifier;

    std::string_view value = string(token);

    if (value == "if") {
      token.type = token_type_t::keywordIf;
    }

    if (value == "fn") {
      token.type = token_type_t::keywordFn;
    }

    if (value == "return") {
      token.type = token_type_t::keywordReturn;
    }

    if (value == "for") {
      token.type = token_type_t::keywordFor;
    }

    if (value == "extend") {
      token.type = token_type_t::keywordExtend;
    }

    if (value == "with") {
      token.type = token_type_t::keywordWith;
    }

    if (value == "mixin") {
      token.type = token_type_t::keywordMixin;
    }

    if (value == "in") {
      token.type = token_type_t::keywordIn;
    }

    if (value == "auto") {
      token.type = token_type_t::keywordAuto;
    }

    if (value == "struct") {
      token.type = token_type_t::keywordStruct;
    }

    if (value == "static") {
      token.type = token_type_t::keywordStatic;
    }

    if (value == "type") {
      token.type = token_type_t::keywordType;
    }

    if (value == "extern") {
      token.type = token_type_t::keywordExtern;
    }
  }

  if (delimiter(c) != token_type_t::specialInvalid) {
    token.type = delimiter(c);
    token.end = token.start + 1;
  }

  if (c == '"' || c == '\'') {
    // Literal string
    auto total = skip_until([delim = c](char cur) { return cur == delim; });
    token.start++;
    token.end = token.start + total;

    token.type = token_type_t::literalString;
    advance(); // Consume closing "
    token.location.end = {line_, column_};
    return token;
  }

  if (is_numeric(c)) {
    bool is_float = false;

    const char *start = cur_;
    for (;;) {
      char next = peek();
      if (is_numeric(next)) {
        advance();
        continue;
      }

      if (!is_float && next == '.') {
        is_float = true;
        advance();
        continue;
      }

      if (next == '\'') {
        // Syntactic sugar (thousands separator), these are skipped.
        advance();
        continue;
      }

      if (!is_numeric(next)) {
        if (next == '.' && is_float) {
          // Backtrack, a literal can only ever have one .
          cur_--;
        }

        break;
      }
    }

    token.end = cur_ - start;
    token.type = is_float ? token_type_t::literalFloat : token_type_t::literalInt;
  }

  if (operator_(c) != token_type_t::specialInvalid) {
    token.type = operator_(c);
    switch (token.type) {
    case token_type_t::operatorDot:
      if (peek() == '.') {
        token.type = token_type_t::operatorRange;
        advance();
      }
      break;
    case token_type_t::operatorNegate:
      if (peek() == '!') {
        token.type = token_type_t::operatorNotEqual;
        advance();
      }
      break;
    case token_type_t::operatorEqual:
      if (peek() == '=') {
        token.type = token_type_t::operatorEquality;
        advance();
      }
      break;
    // Comments
    case token_type_t::operatorDivide:
      if (peek() == '*') {
        advance(); // Consume '*'
        while (!eof()) {
          if(peek() == '*' && peek(1) == '/') {
            advance(); advance();
            break;
          }
          advance();
        }
        return next();
      }

      if (peek() == '/') {
        advance(); // Consume second '/'
        while(!eof() && peek() != '\n')
          advance();
        advance(); // Consume newline
        return next();
      }
      break;
    }
  }

  token.end = cur_ - begin_;
  token.location.end = {line_, column_};
  return token;
}

size_t lexer_t::skip_until(const std::function<bool(char)> &pred) {
  const char *start = cur_;
  while (!eof() && !pred(*cur_)) {
    advance();
  }
  return cur_ - start;
}

lexer_state_t lexer_t::state() const {
  return lexer_state_t {
    .offset = static_cast<size_t>(cur_ - begin_),
    .line = line_,
    .column = column_
  };
}

void lexer_t::set_state(const lexer_state_t &state) {
  cur_ = begin_ + state.offset;
  line_ = state.line;
  column_ = state.column;
}

std::string_view lexer_t::string(const token_t &token) const {
  return std::string_view (begin_ + token.start, token.end - token.start);
}

std::string_view lexer_t::full_line(const token_t &token) const {
  uint32_t line_begin = 0,
           line_end = end_ - begin_;

  std::string_view source (begin_, end_ - begin_);
  // Search for line beginning
  for (auto i = token.start; i > 0; i--) {
    if (source[i] == '\n') {
      line_begin = i + 1;
      break;
    }
  }

  // Search for line end
  for (auto i = token.start; i < source.size(); i++) {
    if (source[i] == '\n') {
      line_end = i - 1;
      break;
    }
  }

  return source.substr(line_begin, line_end - line_begin + 1);
}

std::string_view lexer_t::line(uint64_t num) const {
  const char *start = begin_, *end = begin_;
  uint64_t line = 1;

  while (line != num) {
    if (*start++ == '\n') {
      line++;
    }
  }

  end = start;
  while (end != end_ && *++end != '\n');

  return std::string_view(start, end);
}

size_t lexer_t::line_no(const token_t &token) const {
  uint32_t line = 1;

  for (auto i = token.start; i > 0; i--) {
    if (*(begin_ + i) == '\n') line++;
  }

  return line;
}

size_t lexer_t::column_no(const token_t &token) const {
  uint32_t col = 0;

  for (auto i = token.start; i > 0; i--) {
    if (*(begin_ + i) != '\n') col++;
    else break;
  }

  return col - 1;
}

std::string lexer_t::name() const {
  return name_;
}

void lexer_t::set_name(std::string new_name) {
  name_ = new_name;
}
