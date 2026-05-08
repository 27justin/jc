#include "frontend/parser.hpp"
#include "frontend/ast.hpp"
#include "frontend/diagnostic.hpp"
#include "frontend/token.hpp"
#include <cassert>
#include <iostream>
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <variant>

using TT = token_type_t;
using P  = parser_t;
using TU = translation_unit_t;

using std::make_shared;
using std::make_unique;

std::string
substitute_string_escape_characters(const std::string &input) {
  std::string result;
  result.reserve(input.size());

  for (size_t i = 0; i < input.size(); ++i) {
    if (input[i] == '\\' && i + 1 < input.size()) {
      i++; // Consume the backslash
      switch (input[i]) {
        case 'n':
          result += '\n';
          break;
        case 'r':
          result += '\r';
          break;
        case 't':
          result += '\t';
          break;
        case '\\':
          result += '\\';
          break;
        case '\"':
          result += '\"';
          break;
        case 'x': {
          // Peek ahead to see how many hex digits we have
          std::string hex_str;
          size_t      j = i + 1;
          while (j < input.size() && j < i + 5 && isxdigit(input[j])) {
            hex_str += input[j];
            j++;
          }

          if (!hex_str.empty()) {
            // Convert hex string to integer
            unsigned long value = std::stoul(hex_str, nullptr, 16);

            // If it's 4 digits (\x0049), we treat it as two bytes: 0x00 and 0x49
            if (hex_str.size() > 2) {
              result += static_cast<char>((value >> 8) & 0xFF);
              result += static_cast<char>(value & 0xFF);
            } else {
              result += static_cast<char>(value & 0xFF);
            }
            i = j - 1;     // Advance the main loop counter
          } else {
            result += 'x'; // Just a literal 'x' if no digits follow
          }
          break;
        }
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7': {
          std::string oct_str;
          size_t      j = i; // 'i' is currently at the first digit

          // Octal escapes are usually up to 3 digits (e.g., \033)
          while (j < input.size() && j < i + 3 && input[j] >= '0' && input[j] <= '7') {
            oct_str += input[j];
            j++;
          }

          if (!oct_str.empty()) {
            unsigned long value = std::stoul(oct_str, nullptr, 8);
            result += static_cast<char>(value & 0xFF);
            i = j - 1; // Advance main loop
          }
          break;
        }
        default:
          result += input[i]; // Unknown escape, just keep the character
          break;
      }
    } else {
      result += input[i];
    }
  }
  return result;
}

std::pair<int, int>
get_binding_power(TT type) {
  switch (type) {
    // Assignment
    case TT::operatorEqual:
    case TT::operatorPlusEqual:
    case TT::operatorMinusEqual:
    case TT::operatorMultiplyEqual:
    case TT::operatorDivideEqual:
    case TT::operatorModEqual:
      return { 2, 1 };

    // Logical Operators
    case TT::operatorBooleanOr:
      return { 5, 6 };
    case TT::operatorBooleanAnd:
      return { 7, 8 };

    // Comparisons
    case TT::operatorEquality:
    case TT::operatorNotEqual:
    case TT::operatorGTE:
    case TT::operatorLTE:
    case TT::delimiterLAngle: // <
    case TT::delimiterRAngle: // >
      return { 10, 11 };

    // Range
    case TT::operatorRange:
      return { 15, 16 };

    // Addition / Subtraction
    case TT::operatorPlus:
    case TT::operatorMinus:
      return { 20, 21 };

    // Multiplication / Division / Bitwise operators
    case TT::operatorMultiply:
    case TT::operatorDivide:
    case TT::operatorMod:
    case TT::operatorShiftLeft:
    case TT::operatorShiftRight:
    case TT::operatorAnd:
    case TT::operatorPipe:
    case TT::operatorXor:
      return { 30, 31 };

    // Casting
    case TT::keywordAs:
      return { 40, 41 };

    // Postfix / Primary
    case TT::operatorExclamation:
      return { 60, 61 };
    case TT::operatorDot:
      return { 70, 71 };
    case TT::delimiterLBracket:
      return { 80, 81 }; // Array access
    case TT::delimiterLParen:
      return { 80, 81 }; // Function Call
    case TT::operatorDeref:
      return { 90, 91 };
    case TT::delimiterLBrace:
      return { 95, 0 }; // Struct literal

    default:
      return { -1, -1 };
  }
}

int
get_unary_binding_power(TT type) {
  switch (type) {
      // The `.` and `^` operator are special cases, they are used for
      // certain syntactic sugar operations, and therefore have the
      // highest precedence.
    case TT::operatorDot:
    case TT::operatorXor:
    case TT::operatorExclamation:
      return 75;
    case TT::operatorAnd:
      return 75;
    default:
      return 25;
  }
}

void
P::expect(TT ty) {
  auto next = lexer.peek();
  if (next.type == ty) {
    token = lexer.next();
    return;
  } else {
    // TODO: Try to recover to get as much information as possible.
    throw parse_error_t{
      .error = diagnostic_t{ .level     = diagnostic_level_t::eError,
                            .code      = diagnostic_t::code_t::syntax_error,
                            .arguments = { to_string(next.type), to_string(ty) },
                            .source    = source,
                            .origin    = token.location }
    };
  }
}

void
P::expect_any(std::vector<TT> types) {
  token_t current = lexer.peek();
  for (TT ty : types) {
    if (current.type == ty) {
      token = lexer.next();
      return;
    }
  }

  std::stringstream ss;
  for (int64_t i = 0; i < types.size(); ++i) {
    if (i > 0)
      ss << ", ";
    ss << "`" << to_string(types[i]) << "`";
  }

  throw parse_error_t{
    .error = diagnostic_t{ .level     = diagnostic_level_t::eError,
                          .code      = diagnostic_t::code_t::syntax_error,
                          .arguments = { to_string(current.type), ss.str() },
                          .source    = source,
                          .origin    = token.location }
  };
}

bool
P::peek_any(std::vector<TT> types, token_type_t *next) {
  token_t current = lexer.peek();
  for (TT ty : types) {
    if (current.type == ty) {
      if (next != nullptr)
        *next = ty;
      return true;
    }
  }
  return false;
}

bool
P::maybe(TT ty) {
  if (lexer.peek().type == ty) {
    // Advance if it matches
    token = lexer.next();
    return true;
  }
  return false;
}

bool
P::peek(TT ty) {
  if (lexer.peek().type == ty) {
    return true;
  }
  return false;
}

UP<block_node_t>
P::parse_block() {
  location_tracker_t tracker(*this);

  expect(TT::delimiterLBrace);

  std::vector<UP<ast_node_t>> statements;
  bool                        has_implicit_return = false;
  while (!peek(TT::delimiterRBrace)) {

    auto &stmt = statements.emplace_back(parse_statement());
    if (!stmt->template is<while_node_t>() && !stmt->template is<if_node_t>() &&
        !stmt->template is<do_while_node_t>() &&
        !stmt->template is<for_node_t>()) { // These nodes don't have to be terminated.
      if (!maybe(TT::delimiterSemicolon)) { // If we're missing the semicolon, this is an implicit
                                            // return.
        has_implicit_return = true;
        break;
      }
    }
  }
  expect(TT::delimiterRBrace);

  // We might have implicit return, if the last statement is a `if_node_t`
  if (statements.size() > 0 && statements.back()->is<if_node_t>()) {
    has_implicit_return = true;
  }

  return tracker.finalize(make_unique<block_node_t>(std::move(statements)));
}

UP<ast_node_t>
P::parse_struct_definition() {
  location_tracker_t tracker(*this);

  expect(TT::keywordStruct);
  expect(TT::delimiterLBrace);

  auto node = make_unique<struct_definition_node_t>();

  while (!peek(TT::delimiterRBrace)) {
    location_tracker_t tracker(*this);
    auto               member = make_unique<struct_member_node_t>();

    expect(TT::identifier);
    member->name = source->string(token.location);

    expect(TT::operatorColon);
    member->type = parse_type();

    node->members.push_back(tracker.finalize(std::move(member)));

    if (!maybe(TT::operatorComma)) {
      if (peek(TT::delimiterRBrace))
        break;
    }
  }

  expect(TT::delimiterRBrace);
  return tracker.finalize(std::move(node));
}

UP<ast_node_t>
P::parse_statement() {
  UP<ast_node_t> result = nullptr;
  if (peek_any({ TT::keywordLet, TT::keywordVar })) {
    result = std::move(parse_declaration());
    goto end;
  }

  if (peek(TT::delimiterLParen)) { // Might be destructuring
    lexer.push();
    try {
      result = parse_destructuring_declaration(declaration_node_t::let);
      lexer.commit();
      goto end;
    } catch (...) {
      lexer.pop();
    }
  }

  if (peek(TT::keywordFor)) {
    result = parse_for();
    goto end;
  }

  if (peek(TT::keywordWhile)) {
    result = parse_while();
    goto end;
  }

  if (peek(TT::keywordDo)) {
    result = parse_do_while();
    goto end;
  }

  // Default case is just an expression
  result = parse_expression();

end:
  return std::move(result);
}

std::vector<UP<parameter_node_t>>
P::parse_parameter_list() {
  std::vector<UP<parameter_node_t>> params;
  while (!peek(TT::delimiterRParen)) {
    auto               next = lexer.peek();
    location_tracker_t tracker(*this);

    bool is_mutable = next.type == TT::keywordVar;
    if (is_mutable)                       // Skip the `var` token
      token = lexer.next();

    if (next.type == TT::operatorRange) { // varags
      token = lexer.next();
      break;
    }

    if (lexer.peek(1).type == token_type_t::operatorColon) {
      expect(TT::identifier);
      std::string param_name = source->string(token.location);
      expect(TT::operatorColon);
      auto type = parse_type();
      params.emplace_back(
        tracker.finalize(make_unique<parameter_node_t>(param_name, std::move(type), is_mutable)));
    } else {
      auto type = parse_type();
      params.emplace_back(
        tracker.finalize(make_unique<parameter_node_t>(std::nullopt, std::move(type), is_mutable)));
    }

    if (maybe(TT::operatorComma) == false)
      break;
  }

  expect(TT::delimiterRParen);
  return std::move(params);
}

UP<enum_definition_node_t>
P::parse_enum_definition() {
  location_tracker_t tracker(*this);

  expect(TT::keywordEnum);
  expect(TT::delimiterLBrace);

  std::vector<UP<enum_variant_node_t>> members;
  while (!peek(TT::delimiterRBrace)) {
    location_tracker_t tracker(*this);
    expect(TT::identifier);
    auto name = source->string(token.location);
    if (maybe(TT::delimiterLParen)) {
      // Discriminated enum variant
      auto param_list = parse_parameter_list();
      members.emplace_back(
        tracker.finalize(make_unique<enum_variant_node_t>(name, std::move(param_list))));
    } else {
      // Normal enum member
      UP<ast_node_t> explicit_value = nullptr;
      if (maybe(TT::operatorEqual)) {
        // Explicit value
        explicit_value = parse_expression();
      }

      members.emplace_back(
        tracker.finalize(make_unique<enum_variant_node_t>(name, std::move(explicit_value))));
    }
    if (!maybe(TT::operatorComma))
      break;
  }
  expect(TT::delimiterRBrace);

  return tracker.finalize(make_unique<enum_definition_node_t>(std::move(members)));
}

UP<ast_node_t>
P::parse_function_definition() {
  location_tracker_t tracker(*this);

  // Parses the function header
  auto type = UP<function_type_node_t>(parse_type().release()->as<function_type_node_t>());

  auto block = parse_block();
  return tracker.finalize(make_unique<function_node_t>(std::move(type), std::move(block)));
}

UP<path_node_t>
P::parse_path() {
  location_tracker_t tracker(*this);
  auto               path = std::make_unique<path_node_t>();

  while (true) {
    path_segment_t segment;

    expect(TT::identifier);
    segment.identifier = source->string(token.location);

    if (peek(TT::delimiterLAngle)) {
      token = lexer.next();
      while (!maybe(TT::delimiterRAngle)) {
        segment.template_args.emplace_back(parse_type());
        if (peek(TT::operatorComma))
          token = lexer.next();
      }
    }

    path->segments.push_back(std::move(segment));
    if (peek(TT::operatorDot)) {
      token = lexer.next();
      continue;
    }
    break;
  }
  return tracker.finalize(std::move(path));
}

UP<ast_node_t>
P::parse_tuple() {
  location_tracker_t tracker(*this);
  expect(TT::delimiterLParen);

  std::vector<UP<tuple_member_node_t>> members;
  while (!peek(TT::delimiterRParen)) {
    location_tracker_t tracker(*this);
    if (lexer.peek(0).type == TT::identifier && lexer.peek(1).type == TT::operatorColon) {
      // named `<member>: value`
      expect(TT::identifier);
      auto name = source->string(token.location);
      expect(TT::operatorColon);

      members.emplace_back(
        tracker.finalize(make_unique<tuple_member_node_t>(name, parse_expression())));
    } else {
      // positional value
      members.emplace_back(tracker.finalize(make_unique<tuple_member_node_t>(parse_expression())));
    }
    if (!maybe(TT::operatorComma))
      break;
  }
  expect(TT::delimiterRParen);

  return tracker.finalize(make_unique<tuple_value_node_t>(std::move(members)));
}

UP<ast_node_t>
P::parse_primary() {
  location_tracker_t tracker(*this);

  auto next = lexer.peek();
  switch (next.type) {
    case TT::identifier: {
      return tracker.finalize(make_unique<symbol_node_t>(std::move(parse_path())));
    }

    case TT::keywordFn: {
      /* Function type */
      return parse_function_definition();
    }

    case TT::keywordEnum: {
      /* Enum type */
      return parse_enum_definition();
    }

    case TT::keywordStruct: {
      /* Struct type */
      return parse_struct_definition();
    }

    case TT::keywordImport: {
      return parse_import();
    }

    case TT::keywordIf: {
      return parse_if();
    }

    case TT::keywordNil: {
      expect(TT::keywordNil);
      return tracker.finalize(make_unique<nil_node_t>());
    }

    case TT::delimiterLParen: {
      // Tuple
      return parse_tuple();
    }

    case TT::operatorColon: {
      if (lexer.peek(1).type == TT::delimiterLBrace) {
        // Struct initializer
        return parse_struct_initializer();
      }

      // Contextual lookups
      expect(TT::operatorColon);
      return tracker.finalize(make_unique<contextual_node_t>(std::move(parse_path())));
    }

    case TT::delimiterLBrace: {
      return parse_block();
    }

    case TT::keywordUninitialized: {
      expect(TT::keywordUninitialized);
      return tracker.finalize(make_unique<uninitialized_node_t>());
    }

    case TT::keywordZero: {
      expect(TT::keywordZero);
      return tracker.finalize(make_unique<zero_node_t>());
    }

    case TT::operatorAnd: { // &-reference
      expect(TT::operatorAnd);
      return tracker.finalize(
        make_unique<address_of_node_t>(parse_expression(get_unary_binding_power(next.type))));
    }

    case TT::literalInt:
    case TT::literalBool:
    case TT::literalFloat:
    case TT::literalString:
      expect(next.type);
      return tracker.finalize(
        make_unique<constant_value_node_t>(next.type, source->string(token.location)));

    default:
      assert(false && "Unhandled parse_primary case");
  }
}

UP<import_node_t>
P::parse_import() {
  location_tracker_t tracker(*this);

  expect(TT::keywordImport);

  // <identifier>:
  // Import handlers, currently implemented:
  //  - file:"file.txt"
  //  - C:printf as ..
  if (lexer.peek(0).type == TT::identifier && lexer.peek(1).type == TT::operatorColon) {
    expect(TT::identifier);
    auto handler = source->string(token.location);
    expect(TT::operatorColon);

    if (handler == "C") {
      // Identifier
      expect(TT::identifier);
      std::string symbol_name = source->string(token.location);

      expect(TT::keywordAs);
      auto type = parse_type();

      return tracker.finalize(make_unique<c_import_node_t>(symbol_name, std::move(type)));
    } else if (handler == "file") {
      // Filenames are strings.
      expect(TT::literalString);
      return tracker.finalize(make_unique<file_import_node_t>(source->string(token.location)));
    }
  }

  // Otherwise we just read a path and import as PX
  auto path = parse_path();
  return tracker.finalize(std::make_unique<px_import_node_t>(std::move(path)));
}

UP<ast_node_t>
P::parse_expression(int min_binding_power) {
  location_tracker_t tracker(*this);
  auto               left = parse_primary();

  while (true) {
    auto next = lexer.peek();
    if (!is_operator(next.type))
      break;

    auto [left_binding, right_binding] = get_binding_power(next.type);

    if (left_binding < min_binding_power)
      break;

    switch (next.type) {
      case TT::delimiterLParen: {
        expect(TT::delimiterLParen);
        std::vector<UP<ast_node_t>> params;

        while (!peek(TT::delimiterRParen)) {
          location_tracker_t tracker(*this);
          UP<ast_node_t>     value;
          if (lexer.peek(0).type == TT::identifier && lexer.peek(1).type == TT::operatorColon) {
            // Named argument
            expect(TT::identifier);
            auto name = source->string(token.location);
            expect(TT::operatorColon);
            value = tracker.finalize(
              make_unique<named_argument_node_t>(name, parse_expression(right_binding)));
          } else {
            value = parse_expression(right_binding);
          }
          params.emplace_back(std::move(value));

          if (!maybe(TT::operatorComma))
            break;
        }
        expect(TT::delimiterRParen);

        left =
          tracker.finalize(make_unique<function_call_node_t>(std::move(left), std::move(params)));
        break;
      }
      case TT::operatorDeref: {
        expect(TT::operatorDeref);
        left = tracker.finalize(make_unique<deref_node_t>(std::move(left)));
        break;
      }
      default: {
        if (is_operator(next.type)) {
          expect(next.type);
          binop_node_t binop;
          binop.left  = std::move(left);
          binop.right = parse_expression(right_binding);
          binop.op    = binop_type(next);

          left = tracker.finalize(make_unique<binop_node_t>(std::move(binop)));
          break;
        }
      }
    }

    next = lexer.peek();
    switch (next.type) {
      case TT::keywordIs: {
        expect(TT::keywordIs);

        auto expr = parse_expression(0);
        expect(TT::keywordOr);
        auto fallback = parse_expression(0);
        left          = tracker.finalize(
          make_unique<result_check_node_t>(std::move(left), std::move(expr), std::move(fallback)));

        break;
      }
      case TT::keywordDefer:
        break;

      default:
        break;
    }
  }
  return left;
}

UP<type_node_t>
P::parse_type() {
  // []u8
  // []!u8
  // [][]u8
  // var []u8
  // !any
  // ?any
  // std.string
  // [12]u8
  // fn () -> !any
  // (u8, u8)
  // (named: u8)
  location_tracker_t tracker(*this);

  bool is_mutable = maybe(TT::keywordVar);

  if (peek(TT::delimiterLBracket)) {
    expect(TT::delimiterLBracket);
    if (peek(TT::identifier)) {
      // variable length array
      auto type = variable_array_type_node_t();
      type.size = std::move(parse_expression());

      expect(TT::delimiterRBracket);
      type.element_type = parse_type();
      return tracker.finalize(make_unique<variable_array_type_node_t>(std::move(type)));
    } else if (peek(TT::literalInt)) {
      // variable length array
      auto type = const_array_type_node_t();
      expect(TT::literalInt);
      type.size = std::stoll(source->string(token.location));

      expect(TT::delimiterRBracket);
      type.element_type = std::move(parse_type());
      return tracker.finalize(make_unique<const_array_type_node_t>(std::move(type)));
    } else {
      // runtime slice
      expect(TT::delimiterRBracket);
      slice_type_node_t type;
      type.element_type = std::move(parse_type());
      type.is_mutable   = is_mutable;
      return tracker.finalize(make_unique<slice_type_node_t>(std::move(type)));
    }
  }

  if (peek_any({ TT::operatorMultiply, TT::operatorExclamation, TT::operatorQuestion })) {
    pointer_type_node_t type;

    TT pointer_sigil;
    while (peek_any({ TT::operatorMultiply, TT::operatorExclamation, TT::operatorQuestion },
                    &pointer_sigil)) {
      pointer_kind_t kind = PT_NULLABLE;
      if (pointer_sigil == token_type_t::operatorExclamation)
        kind = PT_NON_NULLABLE;
      type.indirections.push_back(kind);

      expect(pointer_sigil);
    }

    type.element_type = std::move(parse_type());
    return tracker.finalize(make_unique<pointer_type_node_t>(std::move(type)));
  }

  if (maybe(TT::keywordFn)) {
    expect(TT::delimiterLParen);

    std::vector<UP<parameter_node_t>> params = parse_parameter_list();

    UP<type_node_t> ret_type = nullptr;
    if (maybe(TT::operatorArrow)) { // ->
      ret_type = std::move(parse_type());
    }
    return tracker.finalize(
      make_unique<function_type_node_t>(std::move(ret_type), std::move(params)));
  }

  if (maybe(TT::delimiterLParen)) {
    tuple_type_node_t tuple;

    ssize_t enumerator = 0;
    while (!peek(TT::delimiterRParen)) {
      // Named
      if (lexer.peek().type == TT::identifier && lexer.peek(1).type == TT::operatorColon) {
        expect(TT::identifier);
        std::string name = source->string(token.location);

        expect(TT::operatorColon);
        tuple.members.emplace_back(true, name, std::move(parse_type()));
      } else {
        std::string name = std::to_string(enumerator);
        tuple.members.emplace_back(false, name, std::move(parse_type()));
        enumerator++;
      }

      if (!maybe(TT::operatorComma))
        break;
    }
    expect(TT::delimiterRParen);
    return tracker.finalize(make_unique<tuple_type_node_t>(std::move(tuple)));
  }

  identifier_type_node_t type{};
  type.path = parse_path();
  return tracker.finalize(make_unique<identifier_type_node_t>(std::move(type)));
}

UP<ast_node_t>
P::parse_destructuring_declaration(declaration_node_t::mutability default_sc) {
  location_tracker_t tracker(*this);

  expect(TT::delimiterLParen);
  auto pattern = make_unique<destructure_pattern_node_t>();

  while (!peek(TT::delimiterRParen)) {
    auto item_sc = default_sc;

    // Check for overrides: (let a, var b)
    if (maybe(TT::keywordVar))
      item_sc = declaration_node_t::var;
    else if (maybe(TT::keywordLet))
      item_sc = declaration_node_t::let;

    auto path = parse_path();
    pattern->elements.push_back({ item_sc, std::move(path) });

    if (!maybe(TT::operatorComma))
      break;
  }
  expect(TT::delimiterRParen);

  expect(TT::operatorBind);

  auto decl           = make_unique<declaration_node_t>(parse_expression());
  decl->where         = tracker.finalize(std::move(pattern));
  decl->storage_class = default_sc;
  return decl;
}

UP<ast_node_t>
P::parse_declaration() {
  location_tracker_t tracker(*this);
  // (let a, var b) := ...
  if (peek(TT::delimiterLParen)) {
    return parse_destructuring_declaration(declaration_node_t::let);
  }

  expect_any({ TT::keywordLet, TT::keywordVar });
  bool is_mutable = token.type == TT::keywordVar;

  UP<ast_node_t> target;
  if (peek(TT::delimiterLParen)) {
    target = parse_destructuring_declaration(is_mutable ? declaration_node_t::var
                                                        : declaration_node_t::let);
    return target;
  }

  target               = parse_path();
  UP<type_node_t> type = nullptr;

  if (maybe(TT::operatorColon)) {
    type = parse_type();
    expect(TT::operatorEqual);
  } else {
    expect(TT::operatorBind);
  }

  auto declaration =
    tracker.finalize(make_unique<declaration_node_t>(std::move(parse_expression())));
  declaration->storage_class = is_mutable ? declaration_node_t::var : declaration_node_t::let;
  declaration->where         = std::move(target);
  declaration->declared_type = std::move(type);

  return declaration;
}

UP<ast_node_t>
P::parse_struct_initializer() {
  location_tracker_t tracker(*this);

  auto node = make_unique<struct_init_node_t>();

  maybe(TT::operatorColon);
  if (peek(TT::identifier)) {
    node->target_type = parse_type();
  } else {
    node->target_type = make_unique<contextual_node_t>();
  }

  expect(TT::delimiterLBrace);
  while (!peek(TT::delimiterRBrace)) {
    expect(TT::identifier);
    std::string name = source->string(token.location);

    expect(TT::operatorColon);
    auto value = parse_expression();

    node->fields.push_back({ name, std::move(value) });

    if (!maybe(TT::operatorComma))
      break;
  }

  expect(TT::delimiterRBrace);
  return tracker.finalize(std::move(node));
}

UP<ast_node_t>
P::parse_if() {
  location_tracker_t tracker(*this);
  expect(TT::keywordIf);

  auto           condition = parse_expression();
  UP<ast_node_t> pass      = parse_block();
  UP<ast_node_t> reject    = nullptr;

  if (maybe(TT::keywordElse)) {
    reject = parse_expression();
  }

  if_node_t node;
  node.condition = std::move(condition);
  node.pass      = std::move(pass);
  node.reject    = std::move(reject);
  return tracker.finalize(make_unique<if_node_t>(std::move(node)));
}

UP<ast_node_t>
P::parse_while() {
  location_tracker_t tracker(*this);

  expect(TT::keywordWhile);

  auto           condition = parse_expression();
  UP<ast_node_t> body      = parse_block();

  while_node_t node;
  node.condition = std::move(condition);
  node.body      = std::move(body);
  return tracker.finalize(make_unique<while_node_t>(std::move(node)));
}

UP<ast_node_t>
P::parse_for() {
  /*
    for i in 0..=10
    for i in 0..10
    for i in 10..99
    for i in 99..0
    for i: i64 in 0..10
    for i := 1; i < X; i += 1
    for i: i64 = 1; i < X; i += 1
   */

  expect(TT::keywordFor);

  return nullptr;
}

UP<ast_node_t>
P::parse_do_while() {
  location_tracker_t tracker(*this);

  expect(TT::keywordDo);
  UP<ast_node_t> body = parse_block();

  expect(TT::keywordWhile);
  auto condition = parse_expression();

  do_while_node_t node;
  node.body      = std::move(body);
  node.condition = std::move(condition);

  return tracker.finalize(make_unique<do_while_node_t>(std::move(node)));
}

translation_unit_t
P::parse() {
  unit.source = source;
  // Parse until EOF reached
  while (!lexer.eof()) {
    if (lexer.peek().type == TT::specialEof)
      break;
    unit.declarations.emplace_back(parse_statement());
  }
  return std::move(unit);
}

binop_type_t
P::binop_type(const token_t &tok) {
  using BT = binop_type_t;
  switch (tok.type) {
    case TT::operatorPlus:
      return BT::eAdd;
    case TT::operatorMinus:
      return BT::eSubtract;
    case TT::operatorDivide:
      return BT::eDivide;
    case TT::operatorMultiply:
      return BT::eMultiply;
    case TT::operatorBooleanAnd:
      return BT::eAnd;
    case TT::operatorBooleanOr:
      return BT::eOr;
    case TT::operatorEquality:
      return BT::eEqual;
    case TT::operatorNotEqual:
      return BT::eNotEqual;
    case TT::delimiterLAngle:
      return BT::eLT;
    case TT::delimiterRAngle:
      return BT::eGT;
    case TT::operatorGTE:
      return BT::eGTE;
    case TT::operatorLTE:
      return BT::eLTE;
    case TT::operatorMod:
      return BT::eMod;
    case TT::operatorAnd:
      return BT::eBitAnd;
    case TT::operatorXor:
      return BT::eXor;
    case TT::operatorPipe:
      return BT::eBitOr;
    case TT::operatorShiftLeft:
      return BT::eBitShiftLeft;
    case TT::operatorShiftRight:
      return BT::eBitShiftRight;
    case TT::operatorEqual:
      return BT::eAssign;
    default:
      assert(false && "Invalid binop token");
  }
}
