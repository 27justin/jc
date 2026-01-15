#pragma once

#include <memory>

#include "frontend/diagnostic.hpp"
#include "frontend/source.hpp"
#include "frontend/token.hpp"
#include "frontend/lexer.hpp"
#include "frontend/ast.hpp"

struct translation_unit_t {
  const source_t &source;
  std::vector<SP<ast_node_t>> declarations;
};

struct parse_error_t {
  diagnostic_stack_t diagnostics;
};

class parser_t {
public:
  static constexpr const char UNEXPECTED_TOKEN[] = "Unexpected token {}";
  static constexpr const char UNEXPECTED_TOKEN_DETAIL[] =
      "Unexpected token `{}`, expected `{}`";
  static constexpr const char UNEXPECTED_TOKEN_ANY_DETAIL[] = "Unexpected token `{}`, expected any of: {}";

  parser_t(lexer_t &lexer, const source_t &source) : lexer(lexer), source(source), token() {}

  translation_unit_t parse();

  diagnostic_stack_t diagnostics;
private:
  lexer_t &lexer;
  token_t token;
  const source_t &source;

  void consume();

  void expect(token_type_t);
  void expect_any(std::vector<token_type_t>);

  bool maybe(token_type_t);

  bool peek(token_type_t);
  token_type_t peek_any(std::vector<token_type_t>);

  // Parsing Functions
  SP<ast_node_t> parse_primary();
  SP<ast_node_t> parse_expression(int min_power = 0);

  SP<ast_node_t> parse_type();
  SP<ast_node_t> parse_variable_decl();
  SP<ast_node_t> parse_binding();

  SP<ast_node_t> parse_struct_decl();
  SP<ast_node_t> parse_extern_decl();

  SP<ast_node_t> parse_function_header();
  SP<ast_node_t> parse_function_decl();
  SP<ast_node_t> parse_block();
  SP<ast_node_t> parse_statement();
  SP<ast_node_t> parse_return();

  SP<ast_node_t> parse_function_call();
  SP<ast_node_t> parse_function_binding();

  SP<ast_node_t> parse_if();
  SP<ast_node_t> parse_type_alias();
};

