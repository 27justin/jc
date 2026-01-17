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

bool is_number(char c) {
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
  case '!': return operatorExclamation;
  case ':': return operatorColon;
  case '`': return operatorLiteral;
  case '|': return operatorPipe;
  case '^': return operatorXor;
  case '&': return operatorAnd;
  case '@': return operatorAt;
  case '?': return operatorQuestion;
  case '$': return operatorDollar;
  case '~': return operatorTilde;
  default: return specialInvalid;
  }
}

token_type_t keyword(std::string_view kw) {
  using tt = token_type_t;

  if (kw == "if") return tt::keywordIf;
  if (kw == "for") return tt::keywordFor;
  if (kw == "while") return tt::keywordWhile;
  if (kw == "return") return tt::keywordReturn;
  if (kw == "fn") return tt::keywordFn;
  if (kw == "extend") return tt::keywordExtend;
  if (kw == "with") return tt::keywordWith;
  if (kw == "mixin") return tt::keywordMixin;
  if (kw == "in") return tt::keywordIn;
  if (kw == "auto") return tt::keywordAuto;
  if (kw == "struct") return tt::keywordStruct;
  if (kw == "static") return tt::keywordStatic;
  if (kw == "type") return tt::keywordType;
  if (kw == "extern") return tt::keywordExtern;
  if (kw == "let") return tt::keywordLet;
  if (kw == "var") return tt::keywordVar;
  if (kw == "self") return tt::keywordSelf;
  if (kw == "else") return tt::keywordElse;
  if (kw == "distinct") return tt::keywordDistinct;
  if (kw == "nil") return tt::keywordNil;

  return tt::identifier;
}

bool lexer_t::eof() const {
  return source.eof();
}

token_t lexer_t::next() {
  using tt = token_type_t;
start:
  if (eof()) {
    return token_t {.type = tt::specialEof};
  }

  token.location = {{ source.line(), source.column() }};
  token.type = tt::specialInvalid;
  char next = source.next();
  if (is_whitespace(next)) {
    goto start;
  }

  // Operators
  // ----------------------
  if (auto op = operator_(next); op != tt::specialInvalid) {
    token.type = op;

    // Merge certain operators
    switch (op) {
    case tt::operatorEqual:
      if (source.peek() == '=') {
        token.type = tt::operatorEquality;
        source.next();
      }
    case tt::operatorExclamation:
      if (source.peek() == '=') {
        token.type = tt::operatorNotEqual;
        source.next();
      }
    case tt::operatorDot:
      if (source.peek() == '.') {
        token.type = tt::operatorRange;
        source.next();
      }
    case tt::operatorDivide:
      if (source.peek() == '/') {
        while(!source.eof() && source.next() != '\n');
        goto start;
      }
    case tt::operatorAnd:
      if (source.peek() == '&') {
        token.type = tt::operatorBooleanAnd;
        source.next();
      }
    case tt::operatorPipe:
      if (source.peek() == '|') {
        token.type = tt::operatorBooleanOr;
        source.next();
      }
    case tt::operatorMinus:
      if (source.peek() == '>') {
        token.type = tt::operatorAs;
        source.next();
        goto next;
      }
    default:
      break;
    }

    token.location.end = {source.line(), source.column()};
  }

  // Identifiers & Keywords
  // ----------------------
  if (is_identifier(next)) {
    while (!source.eof() && is_identifier_alnum(source.peek())) source.next();
    token.location.end = {source.line(), source.column()};

    if (source.string(token.location) == "true" ||
        source.string(token.location) == "false") {
      token.type = tt::literalBool;
      return token;
    }

    token.type = keyword(source.string(token.location));
    return token;
  }

  // Delimiters
  // ----------
  if (auto del = delimiter(next); del != tt::specialInvalid) {
    token.type = del;
  }

  // String literals
  // ---------------
  if (next == '\'' || next == '"') {
    char delimiter = next;
    token.location.start = {source.line(), source.column()};

    while (!source.eof()) {
      char c = source.next();

      if (c == '\\') {
        if (source.eof()) break; // Handle trailing backslash error

        char escape = source.next();
        switch (escape) {
        case '\'': continue;
        case '\"': continue;
        default:
          break;
        }
      } else if (c == delimiter) {
        break; // End of string
      }
    }

    token.type = tt::literalString;
    token.location.end = {source.line(), source.column() - 1};
    return token;
  }

  // Integer & Float literals
  // ------------------------
  if (is_number(next)) {
    token.type = tt::literalInt;

    while (true) {
      char p = source.peek();
      // Allow ' as arbitrary thousands separator
      if (p == '\'') {
        source.next();
        continue;
      }

      // Check for float
      if (p == '.') {
        // Look ahead, is this '..' (Range) or '.[0-9]' (Float)?
        char p2 = source.peek(1);

        // If we are already a float, or the next char is '.', stop here
        if (token.type == tt::literalFloat || p2 == '.') {
          break;
        }

        // If its a dot followed by a number, its a float decimal
        if (is_number(p2)) {
          token.type = tt::literalFloat;
          source.next(); // consume '.'
          continue;
        } else {
          break; // Just a dot, not a float decimal (e.g. 1.something)
        }
      }

      if (is_number(p)) {
        source.next();
        continue;
      }

      // Anything else (space, operator, etc.) ends the number
      break;
    }
  }

  next:
  token.location.end = {source.line(), source.column()};
  return token;
}

token_t lexer_t::peek() {
  source.push();
  token_t tok = next();
  source.pop();
  return tok;
}
