#pragma once

#include <memory>

#include "frontend/ast.hpp"
#include "frontend/ast/declaration.hpp"
#include "frontend/ast/destructure.hpp"
#include "frontend/ast/function.hpp"
#include "frontend/diagnostic.hpp"
#include "frontend/lexer.hpp"
#include "frontend/source.hpp"
#include "frontend/token.hpp"

struct translation_unit_t {
  std::shared_ptr<source_t>   source;
  std::vector<UP<ast_node_t>> declarations;
};

struct parse_error_t {
  diagnostic_t error;
};

using std::make_unique;

class parser_t {
  public:
  static constexpr const char UNEXPECTED_TOKEN[]        = "Unexpected token {}";
  static constexpr const char UNEXPECTED_TOKEN_DETAIL[] = "Unexpected token `{}`, expected `{}`";
  static constexpr const char UNEXPECTED_TOKEN_ANY_DETAIL[] =
    "Unexpected token `{}`, expected any of: {}";

  parser_t(lexer_t &lexer, std::shared_ptr<source_t> source)
    : lexer(lexer)
    , source(source)
    , token() {}

  translation_unit_t
  parse();

  UP<ast_node_t>
  parse_statement();

  private:
  lexer_t                  &lexer;
  token_t                   token;
  std::shared_ptr<source_t> source;

  void
  consume();

  void expect(token_type_t);
  void expect_any(std::vector<token_type_t>);

  bool maybe(token_type_t);

  bool peek(token_type_t);
  bool
  peek_any(std::vector<token_type_t>, token_type_t *next = nullptr);

  UP<path_node_t>
  parse_path();

  UP<type_node_t>
  parse_type();

  UP<ast_node_t>
  parse_declaration();

  UP<ast_node_t>
  parse_function_definition();

  UP<block_node_t>
  parse_block();

  UP<ast_node_t>
  parse_primary();

  UP<import_node_t>
  parse_import();

  std::vector<UP<parameter_node_t>>
  parse_parameter_list();

  UP<enum_definition_node_t>
  parse_enum_definition();

  UP<ast_node_t>
  parse_expression(int min_binding_power = 0);

  UP<ast_node_t> parse_destructuring_declaration(declaration_node_t::mutability);

  UP<ast_node_t>
  parse_tuple();

  UP<ast_node_t>
  parse_struct_definition();

  UP<ast_node_t>
  parse_struct_initializer();

  // Controlflow
  UP<ast_node_t>
  parse_if();

  UP<ast_node_t>
  parse_while();

  UP<ast_node_t>
  parse_do_while();

  UP<ast_node_t>
  parse_for();

  binop_type_t
  binop_type(const token_t &);

  translation_unit_t unit;

  friend class location_tracker_t;
};

struct location_tracker_t {
  parser_t         &parser;
  source_location_t location;

  location_tracker_t(parser_t &p)
    : parser(p)
    , location(p.lexer.peek().location.start, {}) {}

  template<typename T>
  UP<T>
  finalize(UP<T> node) {
    if (node) {
      // token.location usually refers to the last consumed token
      auto end     = parser.token.location;
      location.end = end.end;
      node->template set<node_location_t>(node_location_t{ parser.source, location });
    }
    return node;
  }
};
