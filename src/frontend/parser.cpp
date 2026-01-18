#include "frontend/parser.hpp"
#include "backend/type.hpp"
#include "frontend/ast.hpp"
#include "frontend/token.hpp"
#include <stdexcept>
#include <iostream>
#include <sstream>
#include <cassert>

using TT = token_type_t;
using P = parser_t;
using TU = translation_unit_t;

using std::make_shared;

// Returns {Left Binding Power, Right Binding Power}
std::pair<int, int> get_binding_power(TT type) {
  switch (type) {
  case TT::operatorEqual:       return {2, 1};   // Right associative
  case TT::operatorPlus:
  case TT::operatorMinus:       return {10, 11}; // Left associative
  case TT::operatorMultiply:
  case TT::operatorDivide:      return {20, 21};
  case TT::delimiterLParen:     return {30, 0};  // Function Call
  case TT::delimiterLBracket:   return {31, 0};  // Array access
  case TT::operatorExclamation: return {38, 39}; // ! has lower precedence than ., allows us to chain like .member! to cast into non-nullable pointer more easily.
  case TT::operatorDot:         return {40, 41}; // Member Access
  case TT::operatorNotEqual:    return {40, 41};
  case TT::operatorEquality:    return {40, 41};
  case TT::operatorMod:         return {20, 21};
  case TT::operatorRange:       return {40, 41};
  case TT::operatorBooleanOr:   return {40, 41};
  case TT::operatorBooleanAnd:  return {40, 41};
  case TT::operatorXor:         return {20, 21};
  case TT::delimiterLAngle:     return {31, 0};
  case TT::delimiterRAngle:     return {31, 0};
  case TT::operatorAs:          return {49, 50};
  default:                      return {0, 0};
  }
}

int get_unary_binding_power(TT type) {
  switch (type) {
    // The `.` and `^` operator are special cases, they are used for
    // certain syntactic sugar operations, and therefore have the
    // highest precedence.
  case TT::operatorDot:
  case TT::operatorXor:
    return 50;
  default:
    return 25;
  }
}

void P::expect(TT ty) {
  if (lexer.peek().type == ty) {
    token = lexer.next();
    return;
  } else {
    diagnostics.messages.push_back(error(
        source, token.location, fmt(UNEXPECTED_TOKEN, to_text(token.type)),
        fmt(UNEXPECTED_TOKEN_DETAIL, to_text(token.type), to_text(ty))));
    // TODO: Try to recover to get as much information as possible.
    throw parse_error_t {.diagnostics = diagnostics};
  }
}

void P::expect_any(std::vector<TT> types) {
  token_t current = lexer.peek();
  for (TT ty : types) {
    if (current.type == ty) {
      token = lexer.next();
      return;
    }
  }

  std::stringstream ss;
  for (int64_t i = 0; i < types.size(); ++i) {
    if (i > 0) ss << ", ";
    ss << "`" << to_text(types[i]) << "`";
  }

  auto err = error(source, current.location,
                   fmt(UNEXPECTED_TOKEN, to_text(current.type)),
                   fmt(UNEXPECTED_TOKEN_ANY_DETAIL, to_text(current.type), ss.str()));
  diagnostics.messages.push_back(err);
  throw parse_error_t {.diagnostics = diagnostics};
}

token_type_t P::peek_any(std::vector<TT> types) {
  token_t current = lexer.peek();
  for (TT ty : types) {
    if (current.type == ty) {
      return current.type;
    }
  }

  std::stringstream ss;
  for (int64_t i = 0; i < types.size(); ++i) {
    if (i > 0) ss << ", ";
    ss << "`" << to_text(types[i]) << "`";
  }

  auto err = error(source, current.location,
                   fmt(UNEXPECTED_TOKEN, to_text(current.type)),
                   fmt(UNEXPECTED_TOKEN_ANY_DETAIL, to_text(current.type), ss.str()));
  diagnostics.messages.push_back(err);
  throw parse_error_t {.diagnostics = diagnostics};
}

bool P::maybe(TT ty) {
  if (lexer.peek().type == ty) {
    // Advance if it matches
    token = lexer.next();
    return true;
  }
  return false;
}

bool P::peek(TT ty) {
  if (lexer.peek().type == ty) {
    return true;
  }
  return false;
}

std::string P::parse_path() {
  if (peek(TT::identifier) == false) // We don't want to skip past yet.
    expect(TT::identifier); // Throw error if next is not an identifier

  auto start = lexer.peek().location.start;

  // Skip until end of identifier
  while (maybe(TT::identifier)) {
    if (!maybe(TT::operatorDot)) break;
  }

  auto end = token.location.end;
  return source.string({start, end});
}

SP<ast_node_t> P::parse_type() {
  auto start = lexer.peek().location.start;

  type_decl_t type_data {};

  if (maybe(TT::keywordVar)) {
    type_data.is_mutable = true;
  }

  while (maybe(TT::operatorExclamation)
         || maybe(TT::operatorQuestion)) {
    type_data.indirections.push_back(token.type == TT::operatorExclamation ? pointer_kind_t::eNonNullable : pointer_kind_t::eNullable);
  }

  if (type_data.is_mutable && type_data.indirections.size() == 0) {
    throw error(source, token.location, "Unexpected keyword `var`", "`var` was not expected here.", "Type declarations are only allowed to contain `var`, if they define a pointer. Otherwise this is not allowed.");
  }


  type_data.name = parse_path();

  return make_node<type_decl_t>(ast_node_t::eType, type_data, {start, token.location.end});
}


SP<ast_node_t> P::parse_function_call() {
  auto node = make_shared<ast_node_t>();
  call_expr_t call {};

  auto start = token.location.start;

  if (!peek(TT::delimiterRParen)) {
    bool done = false;
    while (!done) {
      call.arguments.push_back(parse_expression());
      TT ty = peek_any({TT::operatorComma, TT::delimiterRParen});
      switch (ty) {
      case TT::operatorComma:
        expect(TT::operatorComma);
        continue;
      case TT::delimiterRParen:
        expect(TT::delimiterRParen);
        done = true;
        continue;
      default:
        break;
      }
    }
  } else {
    expect(TT::delimiterRParen);
  }

  node->as.call_expr = new call_expr_t(call);
  node->kind = ast_node_t::eCall;
  node->location = {start, token.location.end};
  return node;
}

void escape_string_literal(std::string &str) {
  size_t write_idx = 0;

  for (size_t read_idx = 0; read_idx < str.length(); ++read_idx) {
    if (str[read_idx] == '\\' && read_idx + 1 < str.length()) {
      // Move to the character after the backslash
      read_idx++;
      char escape = str[read_idx];

      switch (escape) {
      case 'n':  str[write_idx++] = '\n'; break;
      case 'r':  str[write_idx++] = '\r'; break;
      case 't':  str[write_idx++] = '\t'; break;
      case '\\': str[write_idx++] = '\\'; break;
      case '\'': str[write_idx++] = '\''; break;
      case '\"': str[write_idx++] = '\"'; break;
      case '0':  str[write_idx++] = '\0'; break;

        // Hex escape: \xFF (2 digits)
      case 'x': {
        if (read_idx + 2 < str.length()) {
          std::string hex = str.substr(read_idx + 1, 2);
          str[write_idx++] = static_cast<char>(std::stoi(hex, nullptr, 16));
          read_idx += 2;
        }
        break;
      }

        // Unicode escape: \u1234 (4 digits)
      case 'u': {
        if (read_idx + 4 < str.length()) {
          std::string uni = str.substr(read_idx + 1, 4);
          uint32_t cp = static_cast<uint32_t>(std::stoul(uni, nullptr, 16));

          // Basic UTF-8 conversion for the codepoint
          if (cp <= 0x7F) {
            str[write_idx++] = static_cast<char>(cp);
          } else if (cp <= 0x7FF) {
            str[write_idx++] = static_cast<char>(0xC0 | (cp >> 6));
            str[write_idx++] = static_cast<char>(0x80 | (cp & 0x3F));
          } else { // 0x800 to 0xFFFF
            str[write_idx++] = static_cast<char>(0xE0 | (cp >> 12));
            str[write_idx++] = static_cast<char>(0x80 | ((cp >> 6) & 0x3F));
            str[write_idx++] = static_cast<char>(0x80 | (cp & 0x3F));
          }
          read_idx += 4;
        }
        break;
      }

      default:
        // Unknown escape, keep the character
        str[write_idx++] = escape;
        break;
      }
    } else {
      str[write_idx++] = str[read_idx];
    }
  }
  str.resize(write_idx);
}

SP<ast_node_t> P::parse_primary() {
  // Parse the smallest atomic unit we can find (symbol, literal, unary operator)
  auto node = make_shared<ast_node_t>();
  auto start = lexer.peek().location.start;

  if (maybe(TT::literalString) || maybe(TT::literalInt) ||
      maybe(TT::literalFloat) || maybe(TT::literalBool)) {
    auto str = source.string(token.location);
    literal_type_t ty {};
    if (token.type == TT::literalString) {
      escape_string_literal(str);
      ty = literal_type_t::eString;
    }
    if (token.type == TT::literalInt)
      ty = literal_type_t::eInteger;
    if (token.type == TT::literalFloat)
      ty = literal_type_t::eFloat;
    if (token.type == TT::literalBool)
      ty = literal_type_t::eBool;

    node->kind = ast_node_t::eLiteral;
    node->as.literal_expr = new literal_expr_t(str, ty);
    goto done;
  }

  if (maybe(TT::identifier)) {
    // Simple lookup
    symbol_expr_t resolver;
    resolver.identifier = source.string(token.location);
    node->kind = ast_node_t::eSymbol;
    node->as.symbol = new symbol_expr_t(resolver);
    goto done;
  }

  if (maybe(TT::keywordSelf)) {
    node->kind = ast_node_t::eSelf;
    goto done;
  }

  if (maybe(TT::keywordNil)) {
    node->kind = ast_node_t::eNil;
    goto done;
  }

  if (maybe(TT::delimiterLParen)) {
    node = parse_expression(0);
    expect(TT::delimiterRParen);
    goto done;
  }

  // Operators
  if (is_operator(lexer.peek().type)) {
    // Since we are parsing a primary atom, a operator here means we
    // have to parse a unary
    token = lexer.next();

    switch (token.type) {
    case token_type_t::operatorAnd: {
      addr_of_expr_t *addr = new addr_of_expr_t {};
      addr->value = parse_expression(get_unary_binding_power(token.type));

      node->as.addr_of = addr;
      node->kind = ast_node_t::eAddrOf;
      break;
    }
    default: {
      unary_expr_t *unary = new unary_expr_t {};
      unary->op = token.type;
      // Different unary operators have different binding power. The
      // highest binding power are designated for `.` (shorthand
      // operator), and `^` (shorthand return type operator)
      unary->value = parse_expression(get_unary_binding_power(token.type));

      node->as.unary = unary;
      node->kind = ast_node_t::eUnary;
    }
    }
    goto done;
  }

  done:
  node->location = {start, token.location.end};
  return node;
}

SP<ast_node_t> P::parse_expression(int min_power) {
  auto left = parse_primary();
  auto start = token.location.start;

  while (true) {
    auto op = lexer.peek();
    auto [left_binding_power, right_binding_power] = get_binding_power(op.type);

    if (left_binding_power <= min_power) break;

    // Consume the operator
    token = lexer.next();

    // Special case for `(`, those are function calls, not binary operators.
    if (op.type == TT::delimiterLParen) {
      auto call = parse_function_call();
      call->as.call_expr->callee = left;
      left = call;
      // Continue parsing the expression (allow for, e.g. `call() + 9 / 2`)
      continue;
    }

    // Special case for `[`, that is an array access
    if (op.type == TT::delimiterLBracket) {
      auto right = parse_primary();
      left = make_node<deref_expr_t>(ast_node_t::eDeref, {make_node<binop_expr_t>(ast_node_t::eBinop, {
          .op = binop_type_t::eAdd,
          .left = left,
          .right = right
            }, token.location)}, token.location);
      expect(TT::delimiterRBracket);
      continue;
    }

    // Special case for `.`, those are member access operators.
    if (op.type == TT::operatorDot) {
      // ... unless it is a `.*`, then it's a deref.
      if (maybe(TT::operatorMultiply)) {
        left = make_node<deref_expr_t>(ast_node_t::eDeref, { .value = left }, { start, token.location.end });
        continue;
      }

      expect(TT::identifier);

      // Member
      auto member_name = source.string(token.location);
      left = make_node<member_access_expr_t>(ast_node_t::eMemberAccess, {
          .object = left,
          .member = member_name
        }, {start, token.location.end});
      continue;
    }

    // Special case for `->`, that is the cast operator
    if (op.type == TT::operatorAs) {
      left = make_node<cast_expr_t>(ast_node_t::eCast, {
          .value = left,
          .type = parse_type()
        }, {start, token.location.end});
      continue;
    }

    // Special case for `=`, that is assignment
    if (op.type == TT::operatorEqual) {
      auto right = parse_expression(right_binding_power);
      left = make_node<assign_expr_t>(ast_node_t::eAssignment, {
                                                                .where = left,
                                                                .value = right
            }, left->location);
      continue;
    }

    // Special case for `!` that coerces nullable pointers into
    // non-nullable ones.
    if (op.type == TT::operatorExclamation) {
      left = make_node<unary_expr_t>(ast_node_t::eUnary,
                                     {.op = TT::operatorExclamation, .value = left},
                                     token.location);
      continue;
    }

    // Save to restore location
    auto op_token = token;

    auto right = parse_expression(right_binding_power);
    left = make_node<binop_expr_t>(ast_node_t::eBinop, {
        .op = binop_type(op_token),
        .left = left,
        .right = right
      }, {start, token.location.end});
  }

  left->location = {start, token.location.end};
  return left;
}

SP<ast_node_t>
P::parse_function_binding() {
  // Variable bindings for functions.
  // 1. name: !u8
  // 2. var name: !u8
  // 3. self <- pass by value (const bound)
  // 4. !self <- pass by reference (const bound)
  // 5. var self <- pass by value (mut bound)
  // 6. var !self <- pass by reference (mut bound)
  // 7. self: XYZ (overload self to only work on specific types)

  auto node = std::make_shared<ast_node_t>();
  auto start = token.location.start;

  function_parameter_t arg;
  if (maybe(TT::keywordVar)) {
    arg.is_mutable = true;
  }

  // Shorthand self syntax
  bool allow_implicit_type = false;
  if (maybe(TT::operatorExclamation)) {
    expect(TT::keywordSelf);
    arg.is_self = true;
    arg.is_self_ref = true; // Pass by reference
    arg.name = "self";
    allow_implicit_type = true;
  } else if (maybe(TT::keywordSelf)) {
    arg.is_self = true;
    arg.is_self_ref = false; // Pass by value
    arg.name = "self";
    allow_implicit_type = true;
  } else {
    expect(TT::identifier);
    arg.name = source.string(token.location);
  }

  if (allow_implicit_type == false) {
    expect(TT::operatorColon);
    arg.type = parse_type();
  }

  node->as.fn_param = new function_parameter_t(arg);
  node->kind = ast_node_t::eFunctionParameter;
  node->location = {start, token.location.end};
  return node;
}

SP<ast_node_t>
P::parse_binding() {
  // Variable bindings for functions & structs.
  // (var)? x: i32
  auto node = std::make_shared<ast_node_t>();
  declaration_t decl {};
  auto start = token.location.start;

  if (maybe(TT::keywordVar)) {
    decl.is_mutable = true;
  }

  expect(TT::identifier);
  decl.identifier = source.string(token.location);

  expect(TT::operatorColon);
  decl.type = parse_type();

  node->location = {start, token.location.end};
  node->as.declaration = new declaration_t(decl);
  node->kind = ast_node_t::eDeclaration;
  return node;
}

SP<ast_node_t>
P::parse_variable_decl() {
  // var/let x: i32 = 4;

  auto node = std::make_shared<ast_node_t>();
  declaration_t decl {};
  expect_any({TT::keywordLet, TT::keywordVar}); // var/let

  auto start = token.location.start;

  if (token.type == TT::keywordLet) {
    decl.is_mutable = false;
  } else { // var
    decl.is_mutable = true;
  }

  expect(TT::identifier); // x
  decl.identifier = source.string(token.location);

  if (maybe(TT::operatorColon)) { // i32
    decl.type = parse_type();
  }

  if (maybe(TT::operatorEqual)) { // =
    decl.value = parse_expression(); // Up until ;
  }
  expect(TT::delimiterSemicolon);

  node->as.declaration = new declaration_t(decl);
  node->kind = ast_node_t::eDeclaration;
  node->location = {start, token.location.end};
  return node;
}

SP<ast_node_t>
P::parse_function_header() {
  // fn <i32> stat (path: !u8, stat: !any)

  auto header = std::make_shared<ast_node_t>();
  function_decl_t fn {};

  auto start = token.location.start;

  expect(TT::keywordFn); // fn

  // Push the current lexer state before trying to parse the type.
  auto state = token;
  lexer.push();
  try {
    // First try to parse with a typename
    fn.type = parse_type();
    fn.name = parse_path();

    // Fully parsed with type info, we can throw away the lexer state
    lexer.commit();
  } catch (const parse_error_t &) {
    // Pop the last diagnostics
    diagnostics.messages.pop_back();

    // Restore the lexer to before trying to parser the type.
    lexer.pop();
    token = state;
    fn.type = nullptr;
    fn.name = parse_path();
  }

  expect(TT::delimiterLParen); // (

  while (!peek(TT::delimiterRParen)) {
    fn.parameters.push_back(parse_function_binding()); // path: !u8, stat: !any
    if (!maybe(TT::operatorComma)) {
      break;
    }

    // operatorRange is for varargs
    if (maybe(TT::operatorRange)) {
      fn.is_var_args = true;
    }
  }
  expect(TT::delimiterRParen); // )

  header->as.fn_decl = new function_decl_t(fn);
  header->kind = ast_node_t::eFunctionDecl;
  header->location = {start, token.location.end};
  return header;
}

SP<ast_node_t>
P::parse_extern_decl() {
  auto ext = std::make_shared<ast_node_t>();
  auto start = token.location.start;

  extern_decl_t decl {};

  expect(TT::keywordExtern);
  expect(TT::literalString);

  std::string convention = source.string(token.location);
  decl.convention = convention;

  // extern function decl
  if (peek(TT::keywordFn)) {
    decl.import = parse_function_header();
    expect(TT::delimiterSemicolon);
  }

  ext->as.extern_decl = new extern_decl_t(decl);
  ext->kind = ast_node_t::eExtern;
  ext->location = {start, token.location.end};
  return ext;
}

SP<ast_node_t>
P::parse_return() {
  auto node = std::make_shared<ast_node_t>();
  auto start = token.location.start;

  return_stmt_t ret;
  expect(TT::keywordReturn);

  if (maybe(TT::delimiterSemicolon)) {
    // return; <- void return
    return make_node<return_stmt_t>(ast_node_t::eReturn, {.value = nullptr}, token.location);
  }

  ret.value = parse_expression();

  expect(TT::delimiterSemicolon);
  node->as.return_stmt = new return_stmt_t(ret);
  node->kind = ast_node_t::eReturn;
  node->location = {start, token.location.end};
  return node;
}

SP<ast_node_t> P::parse_if() {
  auto node = std::make_shared<ast_node_t>();
  node->location = token.location;
  node->kind = ast_node_t::eIf;

  if_stmt_t *stmt = new if_stmt_t {};

  expect(TT::keywordIf);
  stmt->condition = parse_expression(0);

  stmt->pass = parse_block();
  if (maybe(TT::keywordElse)) {
    // If the next operator is an {, parse it as a block
    if (peek(TT::delimiterLBrace))
      stmt->reject = parse_block();
    else // Otherwise a statement
      stmt->reject = parse_statement();
    // This allows both
    // ... } else return XYZ;
    // and ... } else { ...
  }

  node->as.if_stmt = stmt;
  return node;
}

range_expr_t P::parse_range() {
  range_expr_t expr;
  auto min = parse_primary();

  expect(TT::operatorRange);
  if (maybe(TT::operatorEqual)) {
    expr.is_inclusive = true;
  } else {
    expr.is_inclusive = false;
  }

  auto max = parse_primary();
  expr.min = min;
  expr.max = max;
  return expr;
}

SP<ast_node_t> P::parse_for() {
  expect(TT::keywordFor);

  expect(TT::identifier);
  // Allowed formats:
  // `for i in 0..10`
  // `for i: i64 in 0..10`
  // `for i: i32 = 0; i < 10; i += 1`
  auto init = make_shared<ast_node_t>();
  init->kind = ast_node_t::eDeclaration;

  std::string init_identifier = source.string(token.location);
  SP<ast_node_t> init_type = nullptr;
  SP<ast_node_t> init_value = nullptr;

  if (maybe(TT::operatorColon)) {
    init_type = parse_type();
  }

  if (maybe(TT::operatorEqual)) {
    init_value = parse_expression();
  }

  SP<ast_node_t> condition = nullptr;
  SP<ast_node_t> action = nullptr;

  if (maybe(TT::keywordIn)) {
    // Shorthand syntax
    range_expr_t range = parse_range();
    init_value = range.min;
    condition = make_node<binop_expr_t>(ast_node_t::eBinop, {.op = range.is_inclusive ? binop_type_t::eLTE : binop_type_t::eLT, .left = make_node<symbol_expr_t>(ast_node_t::eSymbol, {.identifier = init_identifier}, token.location), .right = range.max}, token.location);

    // TODO: Yuck... Make this better!
    action = make_node<assign_expr_t>(ast_node_t::eAssignment, {
        .where = make_node<symbol_expr_t>(ast_node_t::eSymbol, {.identifier = init_identifier}, token.location),
        .value = make_node<binop_expr_t>(ast_node_t::eBinop, {
            .op = binop_type_t::eAdd,
            .left = make_node<symbol_expr_t>(ast_node_t::eSymbol, {.identifier = init_identifier}, token.location),
            .right = make_node<literal_expr_t>(ast_node_t::eLiteral, {.value = "1", .type = literal_type_t::eInteger}, token.location)
          },token.location)
      }, token.location);
  } else {
    // Classic syntax
  }

  auto block = parse_block();

  return make_node<for_stmt_t>(
      ast_node_t::eFor,
      {
          .init = make_node<declaration_t>(ast_node_t::eDeclaration,
                                           {.identifier = init_identifier,
                                            .type = init_type,
                                            .value = init_value,
                                            .is_mutable = true},
                                           token.location),
          .condition = condition,
          .action = action,
          .body = block
    }, token.location);
}

SP<ast_node_t> P::parse_statement() {
  TT next = peek_any({TT::keywordIf, TT::keywordLet, TT::keywordVar, TT::keywordReturn, TT::keywordSelf, TT::operatorDot, TT::keywordFor, TT::keywordWhile, TT::identifier});

  switch (next) {
  case TT::keywordLet: case TT::keywordVar:
    return parse_variable_decl();
  case TT::keywordReturn:
    return parse_return();
  case TT::identifier:
  case TT::keywordSelf:
  case TT::operatorDot: {
    auto qty = parse_expression(0);
    expect(TT::delimiterSemicolon);
    return qty;
  }
  case TT::keywordIf:
    return parse_if();
  case TT::keywordFor:
    return parse_for();
  default:
    assert(false && "Unhandled token in parse_statement");
  }

  return nullptr;
}

SP<ast_node_t>
P::parse_block() {
  auto node = std::make_shared<ast_node_t>();
  block_node_t block {};
  auto start = token.location.start;

  expect(TT::delimiterLBrace);
  while (!maybe(TT::delimiterRBrace)) {
    block.body.push_back(parse_statement());
  }

  node->location = {start, token.location.end};
  node->kind = ast_node_t::eBlock;
  node->as.block = new block_node_t(block);
  return node;
}

SP<ast_node_t>
P::parse_function_decl() {
  auto node = std::make_shared<ast_node_t>();
  function_impl_t impl;
  auto start = token.location.start;

  impl.declaration = parse_function_header();
  impl.block = parse_block();

  node->as.fn_impl = new function_impl_t(impl);
  node->kind = ast_node_t::eFunctionImpl;
  node->location = {start, token.location.end};
  return node;
}

SP<ast_node_t>
P::parse_struct_decl() {
  auto node = std::make_shared<ast_node_t>();
  struct_decl_t *decl = new struct_decl_t;

  expect(TT::keywordStruct); // struct
  auto start = token.location.start;

  decl->name = parse_path();

  expect(TT::delimiterLBrace); // {

  // Members
  while (!maybe(TT::delimiterRBrace)) { // }
    decl->members.push_back(parse_binding());
    expect(TT::delimiterSemicolon);
  }
  maybe(TT::delimiterSemicolon); // ;?

  node->location = {start, token.location.end};
  node->kind = ast_node_t::eStructDecl;
  node->as.struct_decl = decl;
  return node;
}

SP<ast_node_t>
P::parse_type_alias() {
  expect(TT::keywordType);
  bool is_distinct = maybe(TT::keywordDistinct);

  auto node = std::make_shared<ast_node_t>();
  node->location = token.location;

  type_alias_decl_t *decl = new type_alias_decl_t{};
  node->kind = ast_node_t::eTypeAlias;
  node->as.alias_decl = decl;

  expect(TT::identifier);
  decl->alias = source.string(token.location);

  expect(TT::operatorEqual);

  decl->is_distinct = is_distinct;
  decl->type = parse_type();

  expect(TT::delimiterSemicolon);

  return node;
}

SP<ast_node_t>
P::parse_attribute() {
  expect(TT::operatorAt);
  expect(TT::delimiterLParen);

  std::map<std::string, literal_expr_t> attributes;
  while (!maybe(TT::delimiterRParen)) {
    expect(TT::identifier);
    auto attr_name = source.string(token.location);

    expect(TT::operatorColon);
    auto value = parse_primary();

    if (value->kind != ast_node_t::eLiteral)
      throw error(source, token.location, "Only literals are allowed on attributes");

    attributes[attr_name] = *value->as.literal_expr;
  }

  auto node = make_node<attribute_decl_t>(ast_node_t::eAttribute, {std::move(attributes)}, token.location);

  if (peek(TT::keywordFn))
    node->as.attribute_decl->affect = parse_function_decl();

  if (peek(TT::keywordExtern))
    node->as.attribute_decl->affect = parse_extern_decl();

  return node;
}

translation_unit_t
P::parse() {
  translation_unit_t tu {.source = source};

  // Parse until EOF reached
  while (!lexer.eof()) {
    if (lexer.peek().type == TT::specialEof) break;
    TT next_type = peek_any({ TT::keywordFn, TT::keywordExtern, TT::keywordStruct, TT::keywordDistinct, TT::keywordType, TT::operatorAt });

    switch (next_type) {
    case TT::keywordFn:
      tu.declarations.push_back(parse_function_decl());
      continue;
    case TT::keywordExtern:
      tu.declarations.push_back(parse_extern_decl());
      continue;
    case TT::keywordStruct:
      tu.declarations.push_back(parse_struct_decl());
      continue;
    case TT::keywordType:
    case TT::keywordDistinct:
      tu.declarations.push_back(parse_type_alias());
      continue;
    case TT::operatorAt: //< Attribute, branches to parse_function_decl, or parse_extern
      tu.declarations.push_back(parse_attribute());
      break;
    default:
      assert(false && "Unhandled Token Type");
      continue;
    }
  }

  if (diagnostics.messages.size() > 0) {
    // Pass our diagnostics to the caller
    throw parse_error_t {.diagnostics = std::move(diagnostics)};
  }
  return tu;
}

binop_type_t
P::binop_type(const token_t &tok) {
  switch (tok.type) {
  case TT::operatorPlus:
    return binop_type_t::eAdd;
  case TT::operatorMinus:
    return binop_type_t::eSubtract;
  case TT::operatorMultiply:
    return binop_type_t::eMultiply;
  case TT::operatorDivide:
    return binop_type_t::eDivide;
  case TT::operatorBooleanAnd:
    return binop_type_t::eAnd;
  case TT::operatorBooleanOr:
    return binop_type_t::eOr;
  case TT::operatorEquality:
    return binop_type_t::eEqual;
  case TT::operatorNotEqual:
    return binop_type_t::eNotEqual;
  case TT::delimiterLAngle:
    return binop_type_t::eLT;
  case TT::delimiterRAngle:
    return binop_type_t::eGT;
  case TT::operatorLTE:
    return binop_type_t::eLTE;
  case TT::operatorGTE:
    return binop_type_t::eGTE;
  default:
    assert(false && "Invalid token type on parser_t::binop_type");
  }
}

