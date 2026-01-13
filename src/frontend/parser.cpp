#include "frontend/parser.hpp"
#include "frontend/ast.hpp"
#include "frontend/token.hpp"
#include <stdexcept>
#include <iostream>
#include <sstream>

using tt = token_type_t;

int get_precedence(tt type) {
    switch (type) {
        case tt::operatorEqual:
            return 1;
        case tt::operatorOr:
            return 2;
        case tt::operatorAnd:
            return 3;
        case tt::operatorEquality: case tt::operatorNotEqual:
            return 4;
        case tt::delimiterLAngle: case tt::delimiterRAngle: // < and >
            return 5;
        case tt::operatorPlus: case tt::operatorMinus:
            return 6;
        case tt::operatorMultiply: case tt::operatorDivide: case tt::operatorMod:
            return 7;
        default:
            return -1; // Not a binary operator
    }
}

void parser_t::consume() {
  current_token_ = lexer_.next();
}

void parser_t::expect(tt ty) {
  if (current_token_.type == ty) {
    consume();
  } else {
    std::stringstream ss;

    ss << "Syntax error on line " << current_token_.location.start.line << ":"<< current_token_.location.start.column <<"\n";
    ss << lexer_.full_line(current_token_) << "\n";

    std::string indent ("");
    for (int i = 0; i < current_token_.location.start.column; i++) {
      indent.push_back(' ');
    }

    ss << indent << "^\n";
    ss << indent << "Expected `"<<to_text(ty)<<"` got `"<<to_text(current_token_.type)<<"`\n\n";
    throw std::runtime_error(ss.str());
  }
}

bool parser_t::maybe(tt ty) {
    if (current_token_.type == ty) {
      consume();
      return true;
    }
    return false; // No match
}

bool parser_t::peek(tt ty) {
    if (current_token_.type == ty) {
      return true;
    }
    return false; // No match
}

std::unique_ptr<program_node_t>
parser_t::parse() {
  try {
    auto program = std::make_unique<program_node_t>();
    consume();
    while (current_token_.type != tt::specialEof) {

      if (peek(tt::keywordFn)) {
        auto function = std::make_unique<function_decl_t>();
        function->header = parse_function();
        if (peek(tt::delimiterLBrace)) {
          // Has block, otherwise forward definition
          function->body = parse_block();
        }
        program->declarations.emplace_back(std::move(function));
      } else if (peek(tt::keywordExtern)) {
        program->externs.push_back(parse_extern());
      } else if (peek(tt::keywordStruct)) {
        program->structs.push_back(parse_struct());
      }
    }
    return program;
  } catch (const std::runtime_error &err) {
    std::cerr << err.what();
    std::exit(1);
  }
}

std::unique_ptr<var_decl_t>
parser_t::parse_variable_decl() {
  source_range_t location = current_token_.location;
  auto type = parse_type();

  std::string name = std::string(lexer_.string(current_token_));
  expect(token_type_t::identifier);

  auto node = std::make_unique<var_decl_t>();
  node->type = std::move(type);
  node->identifier = name;
  location.end = current_token_.location.end;
  node->location = location;
  return node;
}

std::unique_ptr<block_t> parser_t::parse_block() {
  expect(tt::delimiterLBrace);

  auto block = std::make_unique<block_t>();
  while (current_token_.type != token_type_t::delimiterRBrace &&
         current_token_.type != token_type_t::specialEof) {
    block->statements.emplace_back(parse_statement());
  }

  expect(tt::delimiterRBrace);
  return block;
}

std::unique_ptr<type_stmt_t>
parser_t::parse_type() {
  auto ty = std::make_unique<type_stmt_t>();
  ty->location = current_token_.location;

  if (maybe(tt::operatorNegate)) {
    // Non-nullable pointer
    ty->is_pointer = true;
    ty->is_nullable = false;
  }

  if (maybe(tt::operatorWhat)) {
    ty->is_pointer = true;
    ty->is_nullable = true;
  }

  if (maybe(token_type_t::keywordAuto)) {
    ty->name = "auto";
    return ty;
  }

  // Parse the first segment
  std::stringstream ss;
  ss << lexer_.string(current_token_);
  expect(token_type_t::identifier);

  // While there is a '.', keep adding to the path
  while (maybe(token_type_t::operatorDot)) {
    ss << "." << lexer_.string(current_token_);
    expect(token_type_t::identifier);
  }

  ty->name = ss.str();

  ty->location.end = current_token_.location.end;
  return ty;
}

std::unique_ptr<node_t>
parser_t::parse_statement() {
  source_range_t location = current_token_.location;
  if (maybe(tt::keywordReturn)) {
    auto expr = parse_expression();
    auto stmt = std::make_unique<return_stmt_t>(std::move(expr));
    expect(tt::delimiterSemicolon);

    location.end = current_token_.location.end;
    stmt->location = location;
    return stmt;
  }

  // 1. Check for variable declaration <Type> <Identifier>
  if (peek(tt::identifier) || peek(tt::keywordAuto)) {
    auto saved_state = lexer_.state();
    auto saved_token = current_token_;

    try {
      auto type = parse_type();
      if (peek(tt::identifier)) {
        std::string var_name(lexer_.string(current_token_));
        expect(tt::identifier);

        auto decl = std::make_unique<var_decl_t>();
        decl->type = std::move(type);
        decl->identifier = var_name;

        if (maybe(tt::operatorEqual)) {
          decl->initial_value = parse_expression();
        }
        expect(tt::delimiterSemicolon);

        location.end = current_token_.location.end;
        decl->location = location;
        return decl;
      }
    } catch (const std::runtime_error &) {}
    lexer_.set_state(saved_state);
    current_token_ = saved_token;
  }

  // Default to expression statements (like function calls)
  auto expr = parse_expression();
  expect(tt::delimiterSemicolon);
  location.end = current_token_.location.end;
  expr->location = location;
  return expr;
}

// 1. Helper to handle the "Base" units: literals, names, dots, and calls
std::unique_ptr<node_t> parser_t::parse_primary() {
    std::unique_ptr<node_t> expr = nullptr;
    source_range_t location;

    if(peek(tt::operatorMinus) || peek(tt::operatorNegate) || peek(tt::operatorTilde)) {
      token_type_t op = current_token_.type;
      consume();

      // Recursively call parse_primary to handle things like - - 5 or !is_valid
      auto target = parse_primary();
      location.end = current_token_.location.end;
      target->location = location;
      return std::make_unique<unary_expr_t>(op, std::move(target));
    } else if (maybe(tt::delimiterLParen)) {
      expr = parse_expression(0);
      expect(tt::delimiterRParen);
    } else if (current_token_.type == tt::literalInt ||
        current_token_.type == tt::literalString ||
        current_token_.type == tt::literalFloat ||
        current_token_.type == tt::literalBool) {

        std::string value {lexer_.string(current_token_)};
        tt type = current_token_.type;
        consume();

        if (type == tt::literalString) {
            auto node = std::make_unique<string_literal_t>();
            node->value = value;
            expr = std::move(node);
        } else if (type == tt::literalInt) {
            auto node = std::make_unique<integer_literal_t>();
            node->value = value;
            expr = std::move(node);
        } else if (type == tt::literalFloat) {
            auto node = std::make_unique<float_literal_t>();
            node->value = value;
            expr = std::move(node);
        } else if (type == tt::literalBool) {
            auto node = std::make_unique<bool_literal_t>();
            node->value = (value == "true");
            expr = std::move(node);
        }
    } else if (current_token_.type == tt::identifier) {
        std::string name(lexer_.string(current_token_));
        consume();
        expr = std::make_unique<identifier_expr_t>(name);
    }

    if (!expr)
      return nullptr;

    // --- Suffixes (Dots and Function Calls) ---
    // We loop so we can handle: std.io.print("hi").some_other_call()
    while (true) {
        if (maybe(tt::operatorDot)) {
            std::string member(lexer_.string(current_token_));
            expect(tt::identifier);

            auto access = std::make_unique<member_access_expr_t>();
            access->object = std::move(expr);
            access->member_name = member;
            expr = std::move(access);
        }
        else if (maybe(tt::delimiterLParen)) {
          auto call = std::make_unique<call_expr_t>();
          call->callee = std::move(expr);

          // If the next token IS the closing paren, it's an empty call: f()
          if (current_token_.type != tt::delimiterRParen) {
            while (true) {
              call->arguments.emplace_back(parse_expression());

              // If there's no comma, we exit, otherwise we move past it
              if (!maybe(tt::operatorComma)) {
                break;
              }
            }
          }

          expect(tt::delimiterRParen);
          expr = std::move(call);
        }
        else {
            break;
        }
    }

    location.end = current_token_.location.end;
    expr->location = location;

    return expr;
}

// 2. The main expression entry point with Precedence Climbing
std::unique_ptr<node_t> parser_t::parse_expression(int min_precedence) {
  source_range_t location = current_token_.location;
  auto left = parse_primary();

    while (true) {
        int prec = get_precedence(current_token_.type);
        if (prec < min_precedence) break;

        tt op = current_token_.type;
        consume();

        // Recursively parse the right side with higher precedence floor
        auto right = parse_expression(prec + 1);

        auto binary = std::make_unique<binary_expr_t>();
        binary->op = op;
        binary->left = std::move(left);
        binary->right = std::move(right);

        left = std::move(binary);
        location.end = current_token_.location.end;
    }
    left->location = location;
    return left;
}

std::unique_ptr<function_header_t>
parser_t::parse_function() {
  source_range_t location = current_token_.location;

  auto fn = std::make_unique<function_header_t>();
  expect(tt::keywordFn);

  if (maybe(tt::delimiterLAngle)) {
    fn->return_type = parse_type();
    expect(tt::delimiterRAngle);
  } else {
    // implicit void
    fn->return_type = std::make_unique<type_stmt_t>("void");
  }

  // Function name
  fn->name = std::string(lexer_.string(current_token_));
  expect(tt::identifier);

  expect(tt::delimiterLParen);
  std::vector<std::unique_ptr<var_decl_t>> args;
  if (current_token_.type != tt::delimiterRParen) {
    args.push_back(parse_variable_decl());

    while (maybe(token_type_t::operatorComma)) {
      args.push_back(parse_variable_decl());
    }
  }
  fn->arguments = std::move(args);

  expect(tt::delimiterRParen);

  location.end = current_token_.location.end;
  fn->location = location;
  return fn;
}

std::unique_ptr<extern_decl_t>
parser_t::parse_extern() {
  auto ext = std::make_unique<extern_decl_t>();
  source_range_t location = current_token_.location;

  expect(tt::keywordExtern);

  // The calling convention literal
  std::string convention (lexer_.string(current_token_));
  expect(tt::literalString);

  ext->language = convention;
  ext->symbol = parse_function();

  expect(tt::delimiterSemicolon);
  location.end = current_token_.location.end;
  ext->location = location;
  return ext;
}

std::unique_ptr<struct_decl_t>
parser_t::parse_struct() {
  auto decl = std::make_unique<struct_decl_t>();
  source_range_t location = current_token_.location;

  expect(tt::keywordStruct);

  // The name (may be anonymous)
  if (peek(tt::identifier)) {
    std::string name (lexer_.string(current_token_));
    decl->name = name;
    consume();
  }

  // Members
  expect(tt::delimiterLBrace);

  while (!maybe(tt::delimiterRBrace)) {
    decl->members.push_back(parse_variable_decl());
    expect(tt::delimiterSemicolon);
  }

  maybe(tt::delimiterSemicolon);

  location.end = current_token_.location.end;
  decl->location = location;
  return decl;
}
