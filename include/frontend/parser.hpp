#pragma once

#include <memory>

#include "frontend/token.hpp"
#include "frontend/lexer.hpp"
#include "frontend/ast.hpp"

class parser_t {
public:
  parser_t(lexer_t &lexer) : lexer_(lexer) {}

  std::unique_ptr<program_node_t> parse();
private:
  lexer_t &lexer_;
  token_t current_token_;

  void consume();

  void expect(token_type_t);
  bool maybe(token_type_t);
  bool peek(token_type_t);

  std::unique_ptr<function_header_t> parse_function();

  std::unique_ptr<block_t> parse_block();
  std::unique_ptr<node_t> parse_statement();

  std::unique_ptr<node_t> parse_expression(int min_precendence = 0);
  std::unique_ptr<node_t> parse_primary();

  std::unique_ptr<type_stmt_t> parse_type();
  std::unique_ptr<var_decl_t> parse_variable_decl();
  std::unique_ptr<extern_decl_t> parse_extern();

  std::unique_ptr<struct_decl_t> parse_struct();

  void parse_call_arguments();
};

