#include "frontend/token.hpp"

const char *to_text(token_type_t ty) {
  using tt = token_type_t;
  switch (ty) {
  case tt::identifier:
    return "<identifier>";
  case tt::keywordIf:
    return "if";
  case tt::keywordFor:
    return "for";
  case tt::keywordReturn:
    return "return";
  case tt::keywordFn:
    return "fn";
  case tt::keywordExtend:
    return "extend";
  case tt::keywordWith:
    return "with";
  case tt::keywordMixin:
    return "mixin";
  case tt::keywordIn:
    return "in";
  case tt::keywordAuto:
    return "auto";
  case tt::keywordStruct:
    return "struct";
  case tt::keywordStatic:
    return "static";
  case tt::keywordType:
    return "type";
  case tt::keywordExtern:
    return "extern";
  case tt::keywordLet:
    return "let";
  case tt::keywordVar:
    return "var";

  case tt::literalString:
    return "string literal";
  case tt::literalInt:
    return "int literal";
  case tt::literalFloat:
    return "float literal";
  case tt::literalBool:
    return "bool literal";

  case tt::operatorPlus:
    return "+";
  case tt::operatorMinus:
    return "-";
  case tt::operatorDivide:
    return "/";
  case tt::operatorMultiply:
    return "*";
  case tt::operatorEqual:
    return "=";
  case tt::operatorEquality:
    return "==";
  case tt::operatorMod:
    return "%";
  case tt::operatorRange:
    return "..";
  case tt::operatorSize:
    return "#";
  case tt::operatorComma:
    return ",";
  case tt::operatorExclamation:
    return "!";
  case tt::operatorNotEqual:
    return "!=";
  case tt::operatorColon:
    return ":";
  case tt::operatorLiteral:
    return "`";
  case tt::operatorPipe:
    return "|";
  case tt::operatorXor:
    return "^";
  case tt::operatorAnd:
    return "&";
  case tt::operatorAt:
    return "@";
  case tt::operatorQuestion:
    return "?";
  case tt::operatorDollar:
    return "$";
  case tt::operatorTilde:
    return "~";

  case tt::delimiterLParen:
    return "(";
  case tt::delimiterRParen:
    return ")";
  case tt::delimiterLBrace:
    return "{";
  case tt::delimiterRBrace:
    return "}";
  case tt::delimiterLBracket:
    return "[";
  case tt::delimiterRBracket:
    return "]";
  case tt::delimiterLAngle:
    return "<";
  case tt::delimiterRAngle:
    return ">";
  case tt::delimiterSemicolon:
    return ";";
  case tt::specialEof:
    return "<eof>";

  default: return "<unknown>";
  }
}
