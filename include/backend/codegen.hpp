#pragma once

#include "backend/analyzer.hpp"
#include "backend/type.hpp"
#include "llvm/IR/Instructions.h"
#include <memory>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Target/TargetMachine.h>

struct llvm_scope_t {
  std::map<std::string, llvm::Value *> symbol_map;
  llvm::Value *self;
  llvm_scope_t *parent;

  llvm_scope_t(llvm_scope_t *parent) : parent(parent) {}
  llvm::Value *resolve(const std::string &);
  void add(const std::string &, llvm::Value *);
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
  std::vector<llvm_scope_t> scopes;

  void init_target();

  llvm::Type *map_type(SP<ast_node_t>);

  llvm::Type *ensure_type(SP<qualified_type_t>);
  llvm::Type *ensure_type(SP<type_t>);

  llvm::Value *visit_extern(SP<ast_node_t>);
  llvm::Value *visit_function(SP<ast_node_t>);
  llvm::Value *visit_node(SP<ast_node_t>);

  llvm::Value *visit_return(SP<ast_node_t>);

  llvm::Value *visit_literal(SP<ast_node_t>);
  llvm::Value *visit_binop(SP<ast_node_t>);
  llvm::Value *visit_assign(SP<ast_node_t>);
  llvm::Value *visit_symbol(SP<ast_node_t>);
  llvm::Value *visit_cast(SP<ast_node_t>);

  llvm::Value *visit_self(SP<ast_node_t>);
  llvm::Value *visit_declaration(SP<ast_node_t>);

  llvm::Value *visit_call(SP<ast_node_t>);
  llvm::Value *visit_addr_of(SP<ast_node_t> node);

  llvm::Value *visit_if(SP<ast_node_t> node);
  llvm::Value *visit_unary(SP<ast_node_t> node);

  llvm::Value *visit_member_access(SP<ast_node_t> node);

  llvm::Value *visit_block(SP<ast_node_t> node);

  llvm::AllocaInst *create_entry_block_alloca(llvm::Function *, llvm::Type *, const std::string &);
  llvm::Value *get_address_of_node(SP<ast_node_t> node);

  llvm::TargetMachine *target_machine;
};
