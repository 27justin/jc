#include "frontend/parser.hpp"
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
  case TT::operatorAs:          return {51, 50};
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

SP<ast_node_t> P::parse_type() {
  auto start_loc = lexer.peek().location;
  auto node = make_shared<ast_node_t>();

  ast_type_t type_data {};
  if (maybe(TT::operatorExclamation)) {
    type_data.is_pointer = true;
  } else if (maybe(TT::operatorQuestion)) {
    type_data.is_pointer = true;
    type_data.is_nullable = true;
  }

  expect(TT::identifier);
  // TODO: Allow chaining (namespace access, flattened)
  type_data.name = source.string(token.location);

  node->type = ast_node_t::eType;
  node->as.type = new ast_type_t(type_data);
  node->location = token.location;
  return node;
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
  node->type = ast_node_t::eCall;
  node->location = {start, token.location.end};
  return node;
}

SP<ast_node_t> P::parse_primary() {
  // Parse the smallest atomic unit we can find (symbol, literal, unary operator)
  auto node = make_shared<ast_node_t>();
  auto start = token.location.start;

  if (maybe(TT::literalString) || maybe(TT::literalInt) ||
      maybe(TT::literalFloat) || maybe(TT::literalBool)) {
    literal_type_t ty {};
    if (token.type == TT::literalString)
      ty = literal_type_t::eString;
    if (token.type == TT::literalInt)
      ty = literal_type_t::eInteger;
    if (token.type == TT::literalFloat)
      ty = literal_type_t::eFloat;
    if (token.type == TT::literalBool)
      ty = literal_type_t::eBool;

    node->type = ast_node_t::eLiteral;
    node->as.literal_expr = new literal_expr_t(source.string(token.location), ty);
    goto done;
  }

  if (maybe(TT::identifier)) {
    // Simple lookup
    symbol_expr_t resolver;
    resolver.identifier = source.string(token.location);
    node->type = ast_node_t::eSymbol;
    node->as.symbol = new symbol_expr_t(resolver);
    goto done;
  }

  if (maybe(TT::keywordSelf)) {
    node->type = ast_node_t::eSelf;
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
      node->type = ast_node_t::eAddrOf;
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
      node->type = ast_node_t::eUnary;
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

    // Special case for `.`, those are member access operators.
    if (op.type == TT::operatorDot) {
      expect(TT::identifier);

      // Member
      auto member_name = source.string(token.location);

      auto member_node = make_shared<ast_node_t>();
      member_node->type = ast_node_t::eMemberAccess;
      member_node->as.member_access = new member_access_expr_t {
        .object = left,
        .member = member_name
      };
      member_node->location = {start, token.location.end};
      left = member_node;
      continue;
    }

    // Special case for `->`, that is the cast operator
    if (op.type == TT::operatorAs) {
      auto cast = make_shared<ast_node_t>();
      cast->type = ast_node_t::eCast;
      cast->as.cast = new cast_expr_t {
        .value = left,
        .type = parse_type()
      };
      cast->location = {start, token.location.end};
      left = cast;
      continue;
    }

    // Save to restore location
    auto op_token = token;

    auto right = parse_expression(right_binding_power);
    auto binop = make_shared<ast_node_t>();
    binop->type = ast_node_t::eBinop;
    binop->as.binop = new binop_expr_t {
      .op = op.type,
      .left = left, .right = right,
    };
    binop->location = op_token.location;
    left = binop;
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
    arg.is_const = false;
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
  node->type = ast_node_t::eFunctionParameter;
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
  node->type = ast_node_t::eDeclaration;
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
  node->type = ast_node_t::eDeclaration;
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

  expect(TT::delimiterLAngle); // <
  fn.type = parse_type(); // i32
  expect(TT::delimiterRAngle); // >

  // Function names are allowed to have dots in the name.
  {
    expect(TT::identifier); // Expect first identifier (required in any case)
    auto start = token.location.start;

    // Skip until end of identifier
    while (maybe(TT::identifier) || maybe(TT::operatorDot));
    auto end = token.location.end;
    fn.name = source.string({start, end});
  }

  // Old names (only identifier allowed)
  // expect(TT::identifier); // stat
  // fn.name = source.string(token.location);

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
  header->type = ast_node_t::eFunctionDecl;
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
  ext->type = ast_node_t::eExtern;
  ext->location = {start, token.location.end};
  return ext;
}

SP<ast_node_t>
P::parse_return() {
  auto node = std::make_shared<ast_node_t>();
  auto start = token.location.start;

  return_stmt_t ret;
  expect(TT::keywordReturn);

  ret.value = parse_expression();

  expect(TT::delimiterSemicolon);
  node->as.return_stmt = new return_stmt_t(ret);
  node->type = ast_node_t::eReturn;
  node->location = {start, token.location.end};
  return node;
}

SP<ast_node_t> P::parse_if() {
  auto node = std::make_shared<ast_node_t>();
  node->location = token.location;
  node->type = ast_node_t::eIf;

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

SP<ast_node_t> P::parse_statement() {
  TT next = peek_any({TT::keywordIf, TT::keywordLet, TT::keywordVar, TT::keywordReturn, TT::keywordSelf, TT::operatorDot, TT::identifier});

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
  node->type = ast_node_t::eBlock;
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
  node->type = ast_node_t::eFunctionImpl;
  node->location = {start, token.location.end};
  return node;
}

SP<ast_node_t>
P::parse_struct_decl() {
  auto node = std::make_shared<ast_node_t>();
  struct_decl_t *decl = new struct_decl_t;

  expect(TT::keywordStruct); // struct
  auto start = token.location.start;

  expect(TT::identifier); // person
  decl->name = source.string(token.location);

  expect(TT::delimiterLBrace); // {

  // Members
  while (!maybe(TT::delimiterRBrace)) { // }
    decl->members.push_back(parse_binding());
    expect(TT::delimiterSemicolon);
  }
  maybe(TT::delimiterSemicolon); // ;?

  node->location = {start, token.location.end};
  node->type = ast_node_t::eStructDecl;
  node->as.struct_decl = decl;
  return node;
}

SP<ast_node_t>
P::parse_type_alias() {
  bool is_distinct = maybe(TT::keywordDistinct);
  expect(TT::keywordType);

  auto node = std::make_shared<ast_node_t>();
  node->location = token.location;

  type_alias_decl_t *decl = new type_alias_decl_t{};
  node->type = ast_node_t::eTypeAlias;
  node->as.alias_decl = decl;

  expect(TT::identifier);
  decl->alias = source.string(token.location);

  expect(TT::operatorEqual);

  decl->is_distinct = is_distinct;
  decl->type = parse_type();

  expect(TT::delimiterSemicolon);

  return node;
}

translation_unit_t
P::parse() {
  translation_unit_t tu {.source = source};

  // Parse until EOF reached
  while (!lexer.eof()) {
    if (lexer.peek().type == TT::specialEof) break;
    TT next_type = peek_any({ TT::keywordFn, TT::keywordExtern, TT::keywordStruct, TT::keywordDistinct, TT::keywordType });

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

