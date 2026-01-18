#pragma once

#include "backend/analyzer.hpp"
#include "backend/type.hpp"
#include "llvm/IR/Instructions.h"
#include <memory>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Target/TargetMachine.h>


struct llvm_value_t {
  llvm::Value *value;
  bool is_rvalue;
};

struct llvm_scope_t {
  std::map<std::string, llvm_value_t *> symbol_map;
  SP<llvm_scope_t> parent;

  llvm_scope_t(SP<llvm_scope_t> parent) : parent(parent) {}
  llvm_value_t *resolve(const std::string &);
  llvm_value_t *add(const std::string &, const llvm_value_t &);
};

struct codegen_t {
  std::unique_ptr<llvm::LLVMContext> context;
  std::unique_ptr<llvm::Module> module;
  std::unique_ptr<llvm::IRBuilder<>> builder;

  codegen_t(semantic_info_t &&su);

  void generate();
  void compile_to_object(const std::string &filename);
private:
  semantic_info_t info;
  std::map<SP<type_t>, llvm::Type *> llvm_type_cache;
  std::vector<SP<llvm_scope_t>> scopes;
  bool extern_ = false;

  void init_target();

  llvm::Type *map_type(SP<ast_node_t>);
  llvm::Type *ensure_type(SP<type_t>);

  llvm::Value *get_address_of_node(SP<ast_node_t> node);

  llvm_value_t load(llvm::Type *type, const llvm_value_t &val);
  llvm_value_t emit_store(llvm::Type *type, llvm::Value *value);

  // ----------
  //   Visitors
  // ----------
  llvm_value_t *address_of(SP<ast_node_t> node);

  llvm_value_t *visit_node(SP<ast_node_t>);
  llvm_value_t *visit_extern(SP<ast_node_t>);
  llvm_value_t *visit_block(SP<ast_node_t>);

  void visit_return(SP<ast_node_t>);
  llvm_value_t *visit_literal(SP<ast_node_t>);
  llvm_value_t *visit_call(SP<ast_node_t>);
  llvm_value_t *visit_symbol(SP<ast_node_t>);

  llvm_value_t *visit_declaration(SP<ast_node_t>);
  llvm_value_t *visit_struct_definition(SP<ast_node_t>);

  llvm_value_t *visit_binop(SP<ast_node_t>);
  llvm_value_t *visit_cast(SP<ast_node_t>);
  llvm_value_t *visit_assignment(SP<ast_node_t>);

  llvm_value_t *visit_member_access(SP<ast_node_t>);

  llvm_value_t *visit_attribute(SP<ast_node_t>);
  llvm_value_t *visit_deref(SP<ast_node_t>);
  llvm_value_t *visit_nil(SP<ast_node_t>);

  void visit_if(SP<ast_node_t>);
  void visit_for(SP<ast_node_t>);

  llvm_value_t *visit_unary(SP<ast_node_t>);

  llvm_value_t *visit_function_decl(SP<ast_node_t>);
  llvm_value_t *visit_function_impl(SP<ast_node_t>);

  llvm::Value *make_slice_from_array(SP<type_t> arr, llvm::Value *array_ptr);

  llvm::TargetMachine *target_machine;
};
