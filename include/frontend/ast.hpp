#pragma once

#include <vector>
#include <string>
#include <memory>
#include <map>

#include "frontend/path.hpp"
#include "frontend/token.hpp"
#include "backend/type.hpp"

template<typename T>
using SP = std::shared_ptr<T>;

struct type_decl_t;
struct declaration_t;
struct binop_expr_t;

enum class binop_type_t {
  eAdd,
  eSubtract,
  eDivide,
  eMultiply,
  eAnd,
  eOr,
  eEqual,
  eNotEqual,
  eGT,
  eLT,
  eLTE,
  eGTE,
  eMod
};

struct unary_expr_t;
struct symbol_expr_t;
struct struct_decl_t;
struct block_node_t;
struct function_decl_t;
struct function_impl_t;
struct extern_decl_t;
struct return_stmt_t;
struct call_expr_t;
struct literal_expr_t;
struct addr_of_expr_t;
struct self_decl_t;
struct function_parameter_t;
struct if_stmt_t;
struct for_stmt_t;
struct while_stmt_t;
struct type_alias_decl_t;
struct cast_expr_t;
struct deref_expr_t;
struct attribute_decl_t;
struct template_decl_t;

struct assign_expr_t;

struct member_access_expr_t;

enum class literal_type_t {eString, eInteger, eFloat, eBool};

struct ast_node_t {
  ~ast_node_t();
  void reset();

  enum kind_t { eInvalid, eType, eDeclaration, eBinop, eUnary, eSymbol, eStructDecl, eBlock, eFunctionDecl, eFunctionImpl, eExtern, eReturn, eCall, eLiteral, eSelf, eMemberAccess, eAddrOf, eFunctionParameter, eIf, eTypeAlias, eCast, eAssignment, eDeref, eNil, eAttribute, eFor, eWhile, eTemplate } kind;
  struct {
    union {
      type_decl_t *type;
      declaration_t *declaration;
      binop_expr_t *binop;
      unary_expr_t *unary;
      symbol_expr_t *symbol;
      struct_decl_t *struct_decl;
      block_node_t *block;
      function_decl_t *fn_decl;
      function_impl_t *fn_impl;
      function_parameter_t *fn_param;
      extern_decl_t *extern_decl;
      return_stmt_t *return_stmt;
      call_expr_t *call_expr;
      literal_expr_t *literal_expr;
      member_access_expr_t *member_access;
      addr_of_expr_t *addr_of;
      if_stmt_t *if_stmt;
      type_alias_decl_t *alias_decl;
      cast_expr_t *cast;
      assign_expr_t *assign_expr;
      deref_expr_t *deref_expr;
      attribute_decl_t *attribute_decl;
      for_stmt_t *for_stmt;
      while_stmt_t *while_stmt;
      template_decl_t *template_decl;
      void *raw;
    };
  } as;
  source_location_t location;
  SP<type_t> type; //< Type for code generation
};

struct type_decl_t {
  // !u8
  path_t name; //< u8
  std::vector<pointer_kind_t> indirections;
  bool is_mutable; //< is_mutable = var !/?, only applicable to pointers

  bool is_slice = false;
  size_t len; //< Stack array if > 0
};

struct declaration_t {
  // let x: i32 = 1;
  // x: i32;
  std::string identifier; //< x
  SP<ast_node_t> type; //< i32
  SP<ast_node_t> value; //< 1
  bool is_mutable; //< let/var, default = false
};

struct binop_expr_t {
  // 1 + 2
  binop_type_t op; // eAdd
  SP<ast_node_t> left, right; //< 1, 2
};

struct unary_expr_t {
  // !true
  token_type_t op; //< !
  SP<ast_node_t> value; //< true
};

struct symbol_expr_t {
  // printf("...", variable)
  // std.io.print("...")

  //std::string identifier; //< print/variable
  path_t path;
};

struct struct_decl_t {
  // struct name { name: !u8; age: i32; };
  std::string name;
  std::vector<SP<ast_node_t>> members;
};

struct block_node_t {
  std::vector<SP<ast_node_t>> body;
};

struct function_decl_t {
  // fn <i32> stat(file: !u8, statbuf: !any)
  path_t name; //< stat
  SP<ast_node_t> type; //< i32
  std::vector<std::string> template_params;
  std::vector<SP<ast_node_t>> parameters; //< file: !u8, statbuf: !any
  bool is_var_args = false;
  bool is_generic = false;
};

struct function_impl_t {
  SP<ast_node_t> declaration;
  SP<ast_node_t> block;
};

struct extern_decl_t {
  // extern "C" fn <i32> stat(file: !u8, statbuf: !any)
  std::string convention; //< C
  SP<ast_node_t> import;
};

struct return_stmt_t {
  SP<ast_node_t> value;
};

struct call_expr_t {
  SP<ast_node_t> callee;
  std::vector<SP<ast_node_t>> arguments;
  SP<ast_node_t> implicit_receiver;
};

struct literal_expr_t {
  std::string value;
  literal_type_t type;
};

struct member_access_expr_t {
  SP<ast_node_t> object;
  std::string member;
};

struct addr_of_expr_t {
  SP<ast_node_t> value;
};

struct self_decl_t {
  bool is_pointer;
  bool is_mutable;
  SP<ast_node_t> specifier;
};

struct function_parameter_t {
  std::string name;
  SP<ast_node_t> type;
  bool is_mutable = false;
  bool is_self = false;
  bool is_self_ref = false;
};

struct if_stmt_t {
  SP<ast_node_t> condition,
    pass,
    reject;
};

struct type_alias_decl_t {
  std::string alias;
  SP<ast_node_t> type;
  bool is_distinct;
};

struct cast_expr_t {
  SP<ast_node_t> value;
  SP<ast_node_t> type;
};

struct assign_expr_t {
  SP<ast_node_t> where;
  SP<ast_node_t> value;
};

struct deref_expr_t {
  SP<ast_node_t> value;
};

struct attribute_decl_t {
  std::map<std::string, literal_expr_t> attributes;
  SP<ast_node_t> affect;
};

struct for_stmt_t {
  SP<ast_node_t> init,
    condition,
    action; //< What happens after every iteration.
  SP<ast_node_t> body;
};

struct while_stmt_t {
  SP<ast_node_t> condition;
  SP<ast_node_t> body;
};

struct range_expr_t {
  SP<ast_node_t> min, max;
  bool is_inclusive;
};

struct template_decl_t {
  path_t name;
  SP<ast_node_t> tree;
};

std::string to_string(const type_decl_t &);

void dump_ast(ast_node_t &, size_t indent = 0);

template <typename Data>
SP<ast_node_t>
make_node(ast_node_t::kind_t kind, Data data, source_location_t loc) {
  auto node = std::make_shared<ast_node_t>();
  node->as.raw = new Data{std::move(data)};
  node->kind = kind;
  node->location = loc;
  return node;
}
