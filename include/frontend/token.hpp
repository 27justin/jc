#pragma once

#include <cstdint>

#include "frontend/source.hpp"

enum class token_type_t {
  identifier,

  keywordIf,
  keywordFor,
  keywordReturn,
  keywordFn,
  keywordExtend,
  keywordWith,
  keywordMixin,
  keywordIn,
  keywordAuto,
  keywordStruct,
  keywordStatic,
  keywordType,
  keywordExtern,
  keywordLet,
  keywordVar,

  literalString,
  literalInt,
  literalFloat,
  literalBool,

  operatorPlus,
  operatorMinus,
  operatorDivide,
  operatorMultiply,
  operatorEqual,
  operatorEquality,
  operatorMod,
  operatorRange, // <operatorDot,operatorDot>
  operatorDot,
  operatorSize,
  operatorComma,
  operatorExclamation,
  operatorNotEqual,
  operatorColon,
  operatorLiteral,
  operatorPipe,
  operatorOr,
  operatorXor,
  operatorAnd,
  operatorAt,
  operatorQuestion,
  operatorDollar,
  operatorTilde,

  delimiterLParen,
  delimiterRParen,
  delimiterLBrace,
  delimiterRBrace,
  delimiterLBracket,
  delimiterRBracket,
  delimiterSemicolon,
  delimiterLAngle,
  delimiterRAngle,

  specialEof,
  specialInvalid
};

struct token_t {
  token_type_t type;
  // Relative span (line & column)
  source_location_t location;
};

const char *to_text(token_type_t);
