#include "backend/codegen.hpp"
#include "backend/analyzer.hpp"
#include "backend/type.hpp"
#include "frontend/ast.hpp"
#include "frontend/token.hpp"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/InstrTypes.h"

#include <filesystem>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/TargetParser/Triple.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/MC/TargetRegistry.h>

#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/PassManager.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/FileSystem.h>

#include <memory>
#include <stdexcept>

template<typename T> using SP = std::shared_ptr<T>;

llvm_value_t *
llvm_scope_t::add(const std::string &name, const llvm_value_t &value) {
  auto val = new llvm_value_t{value};
  symbol_map[name] = val;
  return val;
}

llvm_value_t *
llvm_scope_t::resolve(const std::string &name) {
  if (symbol_map.contains(name))
    return symbol_map[name];

  if (parent)
    return parent->resolve(name);

  return nullptr;
}

void
codegen_t::init_target() {
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllAsmPrinters();

  auto target_triple = llvm::Triple(llvm::sys::getDefaultTargetTriple());
  module->setTargetTriple(target_triple);

  std::string error;
  auto target = llvm::TargetRegistry::lookupTarget(target_triple, error);

  llvm::TargetOptions opts;
  auto pic_model = llvm::Reloc::Model::PIC_;
  target_machine = target->createTargetMachine(target_triple, "generic", "", opts, pic_model);

  module->setDataLayout(target_machine->createDataLayout());
}

void codegen_t::compile_to_object(const std::string &filename) {
  std::error_code EC;

  llvm::raw_fd_ostream ir_dest(std::filesystem::path(filename).filename().replace_extension(".ll").string(), EC, llvm::sys::fs::OF_None);
  module->print(ir_dest, nullptr);
  module->print(llvm::outs(), nullptr);

  // llvm::raw_fd_ostream dest(std::filesystem::path(filename).filename().replace_extension(".o").string(), EC, llvm::sys::fs::OF_None);

  // if (EC) {
  //   throw std::runtime_error("Could not open file: " + EC.message());
  // }

  // llvm::legacy::PassManager pass;

  // auto file_type = llvm::CodeGenFileType::ObjectFile;
  // if (target_machine->addPassesToEmitFile(pass, dest, nullptr, file_type)) {
  //   throw std::runtime_error("TargetMachine can't emit a file of this type");
  // }

  // pass.run(*module);
  // dest.flush();
}

codegen_t::codegen_t(semantic_info_t &&s) : info(std::move(s)) {
  context = std::make_unique<llvm::LLVMContext>();
  module = std::make_unique<llvm::Module>("jcc_module", *context);
  builder = std::make_unique<llvm::IRBuilder<>>(*context);

  scopes.push_back(std::make_shared<llvm_scope_t>(nullptr));

  init_target();
}

void
codegen_t::generate() {
  for (auto &node : info.unit.declarations) {
    visit_node(node);
  }
}

llvm::Type *codegen_t::ensure_type(SP<type_t> type) {
  if (llvm_type_cache.contains(type)) {
    return llvm_type_cache.at(type);
  }

  llvm::Type* result = nullptr;

  switch (type->kind) {
  case type_kind_t::eInt:
  case type_kind_t::eUint:
    result = llvm::Type::getIntNTy(*context, type->size);
    break;
  case type_kind_t::eBool:
    result = llvm::Type::getInt1Ty(*context);
    break;
  case type_kind_t::eStruct: {
    result = llvm::StructType::getTypeByName(*context, type->name);
    if (!result) {
      auto st = llvm::StructType::create(*context, type->name);

      std::vector<llvm::Type *> fields;
      auto layout = type->as.struct_layout;
      for (auto &member : layout->members) {
        fields.push_back(ensure_type(member.type));
      }
      st->setBody(fields);
      result = st;
    }
    break;
  }
  case type_kind_t::eFunction: {
    auto fn = type->as.function;
    llvm::Type* ret_ty = ensure_type(fn->return_type);
    std::vector<llvm::Type*> params;
    for (auto &p : fn->arg_types) {
      auto param_ty = ensure_type(p);
      params.push_back(param_ty);
    }

    result = llvm::FunctionType::get(ret_ty, params, fn->is_var_args);
    break;
  }
  case type_kind_t::eFloat: {
    result = type->size == 32 ? llvm::Type::getFloatTy(*context) : llvm::Type::getDoubleTy(*context);
    break;
  }
  case type_kind_t::eOpaque:
  case type_kind_t::eAlias: {
    result = ensure_type(type->as.alias->alias);
    break;
  }
  case type_kind_t::ePointer: {
    result = llvm::PointerType::get(*context, 0);
    break;
  }
  case type_kind_t::eVoid: {
    result = llvm::Type::getVoidTy(*context);
    break;
  }
  }

  // Cache the result
  llvm_type_cache[type] = result;
  return result;
}

llvm_value_t
codegen_t::load(llvm::Type *type, const llvm_value_t &val) {
    // If it's already a constant or a temporary result, don't load
    if (llvm::isa<llvm::Constant>(val.value)) {
        return val;
    }

    // If it's a pointer (like an alloca), we need the value inside
    if (!val.is_rvalue) {
      return llvm_value_t {builder->CreateLoad(type, val.value), true};
    }
    return val;
}

llvm_value_t *
codegen_t::address_of(SP<ast_node_t> node) {
  addr_of_expr_t *expr = node->as.addr_of;
  switch (expr->value->kind) {
  case ast_node_t::eSymbol: {
    auto id = expr->value->as.symbol->identifier;
    // The result of an address of is an rvalue (don't need to load,
    // that would be a deref.)
    return new llvm_value_t { scopes.back()->resolve(id)->value, true};
  }
  case ast_node_t::eDeref: {
    auto result = visit_node(node->as.deref_expr->value);
    return new llvm_value_t{load(ensure_type(node->type), *result)};
  }
  default:
    throw std::runtime_error("Non lvalue address of doesn't work.");
  }
  return nullptr;
}

llvm_value_t *
codegen_t::visit_extern(SP<ast_node_t> node) {
  extern_decl_t *decl = node->as.extern_decl;

  extern_ = true;
  auto *value = visit_node(decl->import);
  extern_ = false;

  return value;
}

llvm_value_t *
codegen_t::visit_function_decl(SP<ast_node_t> node) {
  auto fn_decl = node->as.fn_decl;

  auto fn_type = ensure_type(node->type);

  auto func = llvm::Function::Create(llvm::dyn_cast<llvm::FunctionType>(fn_type),
                                     extern_ ? llvm::GlobalValue::ExternalLinkage
                                 : llvm::GlobalValue::ExternalLinkage,
                         fn_decl->name, *module);

  if (!extern_) {
    llvm::BasicBlock *block = llvm::BasicBlock::Create(*context, "entry", func);
    builder->SetInsertPoint(block);
  }

  // Add the function to our scope.
  return scopes.back()->add(fn_decl->name, llvm_value_t{func, false});
}

llvm_value_t *
codegen_t::visit_function_impl(SP<ast_node_t> node) {
  auto fn_impl = node->as.fn_impl;
  llvm_value_t *value = nullptr;

  auto decl = fn_impl->declaration->as.fn_decl;

  value = visit_node(fn_impl->declaration);
  auto llvm_func = llvm::dyn_cast<llvm::Function>(value->value);

  scopes.emplace_back(std::make_shared<llvm_scope_t>(scopes.back()));

  // Create the stack space for the arguments
  for (int64_t i = 0; i < decl->parameters.size(); ++i) {
    auto type = ensure_type(decl->parameters[i]->type);
    auto param = decl->parameters[i]->as.fn_param;

    scopes.back()->add(param->name, llvm_value_t{llvm_func->args().begin() + i, true});
  }

  auto block = fn_impl->block->as.block;
  visit_node(fn_impl->block);

  scopes.pop_back();
  return value;
}

llvm_value_t *
codegen_t::visit_block(SP<ast_node_t> node) {
  auto block = node->as.block;

  for (auto &n : block->body)
    visit_node(n);

  return nullptr;
}

void codegen_t::visit_return(SP<ast_node_t> node) {
  return_stmt_t *stmt = node->as.return_stmt;
  if ( stmt->value )
    builder->CreateRet(load(ensure_type(stmt->value->type), *visit_node(stmt->value)).value);
  else
    builder->CreateRetVoid();
}

llvm_value_t *
codegen_t::visit_literal(SP<ast_node_t> node) {
  literal_expr_t *expr = node->as.literal_expr;

  llvm::Value *value = nullptr;
  switch (expr->type) {
  case literal_type_t::eInteger: {
    value = llvm::ConstantInt::get(ensure_type(node->type), std::stoi(expr->value));
    break;
  }
  case literal_type_t::eBool: {
    value = llvm::ConstantInt::get(ensure_type(node->type), expr->value == "true");
    break;
  }
  case literal_type_t::eFloat: {
    value = llvm::ConstantFP::get(ensure_type(node->type), std::stof(expr->value));
    break;
  }
  case literal_type_t::eString: {
    value = builder->CreateGlobalString(expr->value, "", 0, module.get());
    break;
  }
  default:
    assert(false && "Internal Compiler Error: unsupported literal");
    break;
  }
  return new llvm_value_t {value, true};
}

llvm_value_t *
codegen_t::visit_call(SP<ast_node_t> node) {
  auto call = node->as.call_expr;

  llvm::Value *value = nullptr;
  llvm::FunctionType *func = llvm::dyn_cast<llvm::FunctionType>(ensure_type(call->callee->type));

  std::vector<llvm::Value *> args;
  for (auto &arg : call->arguments) {
    auto value = visit_node(arg);
    args.push_back(load(ensure_type(arg->type), *value).value);
  }

  auto callee = visit_node(call->callee);
  value = builder->CreateCall(func, callee->value, args);
  return new llvm_value_t{value, true};
}

llvm_value_t *
codegen_t::visit_symbol(SP<ast_node_t> node) {
  symbol_expr_t *symbol = node->as.symbol;
  return scopes.back()->resolve(symbol->identifier);
}

llvm_value_t *
codegen_t::visit_declaration(SP<ast_node_t> node) {
  declaration_t *stmt = node->as.declaration;

  llvm::Type *value_type = ensure_type(node->type);
  llvm::Value *storage = builder->CreateAlloca(value_type);

  if (stmt->value) {
    auto init = visit_node(stmt->value);
    builder->CreateStore(load(ensure_type(stmt->value->type), *init).value, storage);
  } else {
    // Zero initialize by default
    builder->CreateStore(llvm::ConstantInt::getNullValue(value_type), storage);
  }

  return scopes.back()->add(stmt->identifier, llvm_value_t{storage, false});
}

llvm_value_t *
codegen_t::visit_struct_definition(SP<ast_node_t> node) {
  struct_decl_t *decl = node->as.struct_decl;
  ensure_type(node->type);
  return nullptr;
}

llvm_value_t *
codegen_t::visit_binop(SP<ast_node_t> node) {
  binop_expr_t *binop = node->as.binop;

  auto L = visit_node(binop->left);
  auto R = visit_node(binop->right);

  llvm::Value *result = nullptr;
  switch (binop->op) {
  case binop_type_t::eEqual: {
    result = builder->CreateCmp(llvm::CmpInst::ICMP_EQ,
                                load(R->value->getType(), *L).value,
                                load(R->value->getType(), *R).value);
    break;
  }
  case binop_type_t::eNotEqual: {
    result = builder->CreateCmp(llvm::CmpInst::ICMP_NE,
                                load(R->value->getType(), *L).value,
                                load(R->value->getType(), *R).value);
    break;
  }
  case binop_type_t::eLT: {
    result = builder->CreateCmp(llvm::CmpInst::ICMP_SLT,
                                load(R->value->getType(), *L).value,
                                load(R->value->getType(), *R).value);
    break;
  }
  case binop_type_t::eGT: {
    result = builder->CreateCmp(llvm::CmpInst::ICMP_SGT,
                                load(R->value->getType(), *L).value,
                                load(R->value->getType(), *R).value);
    break;
  }
  case binop_type_t::eLTE: {
    result = builder->CreateCmp(llvm::CmpInst::ICMP_SLE,
                                load(R->value->getType(), *L).value,
                                load(R->value->getType(), *R).value);
    break;
  }
  case binop_type_t::eGTE: {
    result = builder->CreateCmp(llvm::CmpInst::ICMP_SGE,
                                load(R->value->getType(), *L).value,
                                load(R->value->getType(), *R).value);
    break;
  }
  case binop_type_t::eSubtract: {
    result = builder->CreateSub(load(R->value->getType(), *L).value,
                                load(R->value->getType(), *R).value);
    break;
  }
  case binop_type_t::eAdd: {
    auto l_val = load(ensure_type(binop->left->type), *L).value;
    auto r_val = load(ensure_type(binop->right->type), *R).value;

    if (l_val->getType()->isPointerTy()) {
      // Pointer + Integer
      return new llvm_value_t{builder->CreateGEP(builder->getInt8Ty(), l_val, r_val), true};
    } else if (r_val->getType()->isPointerTy()) {
      // Integer + Pointer
      return new llvm_value_t{builder->CreateGEP(builder->getInt8Ty(), r_val, l_val), true};
    } else {
      // Normal Integer addition
      result = builder->CreateAdd(l_val, r_val);
    }
    break;
  }
  default: {
    assert(false && "Missing binop type!");
  }
  }
  return new llvm_value_t {result, true};
}

llvm_value_t *
codegen_t::visit_cast(SP<ast_node_t> node) {
  cast_expr_t *expr = node->as.cast;

  auto into = node->type;
  auto from = expr->value->type;

  if (ensure_type(into) != ensure_type(from)) {
    auto lhs = visit_node(expr->value);
    return new llvm_value_t{builder->CreateBitOrPointerCast(load(ensure_type(into), *lhs).value, ensure_type(into)), true};
  }

  return new llvm_value_t{load(ensure_type(into), *visit_node(expr->value))};
}

llvm_value_t *
codegen_t::visit_assignment(SP<ast_node_t> node) {
  assign_expr_t *expr = node->as.assign_expr;

  auto RHS = visit_node(expr->value);
  auto LHS = visit_node(expr->where);

  RHS = new llvm_value_t {load(ensure_type(expr->value->type), *RHS)};
  if (LHS->is_rvalue) {
    LHS = new llvm_value_t {load(ensure_type(expr->where->type), *LHS)};
  }

  return new llvm_value_t{builder->CreateStore(RHS->value, LHS->value), false};
}

llvm_value_t *
codegen_t::visit_member_access(SP<ast_node_t> node) {
  member_access_expr_t *expr = node->as.member_access;

  // Get the struct type
  auto native_type = expr->object->type;

  // Native type might be a pointer, if it is, we assume that we want
  // to access the member behind the pointer.
  if (native_type->kind == type_kind_t::ePointer) {
    native_type = native_type->as.pointer->base;
  }

  assert(native_type->kind == type_kind_t::eStruct);

  auto native_struct = native_type->as.struct_layout;

  auto llvm_type = ensure_type(native_type);
  assert(llvm_type->isStructTy());

  auto llvm_struct = llvm::dyn_cast<llvm::StructType>(llvm_type);

  int64_t member_id = 0;
  // Lookup the member in our struct
  auto member = native_struct->member(expr->member);
  if (!member) {
    throw std::runtime_error(expr->member + " not found in struct.");
  }

  member_id = member - &*native_struct->members.begin();
  std::vector<llvm::Value *> indices = {
    builder->getInt32(0),                  // Dereference the pointer
    builder->getInt32((uint32_t)member_id) // The specific field index
  };
  return new llvm_value_t {builder->CreateGEP(llvm_type, visit_node(expr->object)->value, indices), false};
}

llvm_value_t *
codegen_t::visit_attribute(SP<ast_node_t> node) {
  attribute_decl_t *decl = node->as.attribute_decl;
  auto &attributes = decl->attributes;

  auto value = visit_node(decl->affect);

  if (auto func = llvm::dyn_cast_or_null<llvm::Function>(value->value)) {
    if (attributes.contains("import")) {
      auto import_as = attributes.at("import");
      auto alias_name = func->getName();

      std::string asmAlias = ".set " + alias_name.str() + ", " + import_as.value;
      module->appendModuleInlineAsm(asmAlias);
    }
  }
  return value;
}

llvm_value_t *
codegen_t::visit_deref(SP<ast_node_t> node) {
  deref_expr_t *deref = node->as.deref_expr;
  auto value = visit_node(deref->value);

  return new llvm_value_t {builder->CreateLoad(ensure_type(deref->value->type), value->value), true};
}

llvm_value_t *
codegen_t::visit_nil(SP<ast_node_t> node) {
  return new llvm_value_t{llvm::ConstantPointerNull::get(llvm::dyn_cast<llvm::PointerType>(ensure_type(node->type))), true};
}

void
codegen_t::visit_if(SP<ast_node_t> node) {
  if_stmt_t *stmt = node->as.if_stmt;

  auto cond = visit_node(stmt->condition);

  auto *parent = builder->GetInsertBlock()->getParent();
  llvm::BasicBlock *pass = llvm::BasicBlock::Create(*context, "if.then", parent),
    *reject = llvm::BasicBlock::Create(*context, "if.else"),
    *merge = llvm::BasicBlock::Create(*context, "if.end");
  llvm::BasicBlock *else_dest = stmt->reject ? reject : merge;

  builder->CreateCondBr(load(builder->getIntNTy(1), *cond).value, pass, else_dest);

  builder->SetInsertPoint(pass);
  visit_block(stmt->pass);
  if (!builder->GetInsertBlock()->getTerminator()) {
    builder->CreateBr(merge);
  }

  if (stmt->reject) {
    builder->SetInsertPoint(reject);
    visit_block(stmt->reject);
    if (!builder->GetInsertBlock()->getTerminator()) {
      builder->CreateBr(merge);
    }
  }

  parent->insert(parent->end(), merge);
  builder->SetInsertPoint(merge);
}

void
codegen_t::visit_for(SP<ast_node_t> node) {
  for_stmt_t *stmt = node->as.for_stmt;

  auto *parent = builder->GetInsertBlock()->getParent();

  // 1. Create the blocks
  // We attach 'condition' immediately so the 'init' block has somewhere to jump to
  llvm::BasicBlock *cond = llvm::BasicBlock::Create(*context, "for.cond", parent);
  llvm::BasicBlock *body = llvm::BasicBlock::Create(*context, "for.body", parent);
  llvm::BasicBlock *action = llvm::BasicBlock::Create(*context, "for.inc", parent);
  llvm::BasicBlock *merge = llvm::BasicBlock::Create(*context, "for.end", parent);

  // --- INIT ---
  // Emit initialization code in the current block
  if (stmt->init) {
    visit_node(stmt->init);
  }
  // Jump from the current block into the condition block
  builder->CreateBr(cond);

  // --- CONDITION ---
  builder->SetInsertPoint(cond);
  if (stmt->condition) {
    auto cond_val = visit_node(stmt->condition);
    // Use your 'load' hack to ensure we have an i1 value
    llvm::Value *is_true = load(builder->getIntNTy(1), *cond_val).value;
    builder->CreateCondBr(is_true, body, merge);
  } else {
    // If no condition (e.g. for ;;), it's an infinite loop
    builder->CreateBr(body);
  }

  // --- BODY ---
  builder->SetInsertPoint(body);
  visit_block(stmt->body); // This is the loop body

  // After the body, we jump to the action (increment) block
  if (!builder->GetInsertBlock()->getTerminator()) {
    builder->CreateBr(action);
  }

  // --- ACTION (Increment) ---
  builder->SetInsertPoint(action);
  if (stmt->action) {
    visit_node(stmt->action);
  }
  // After incrementing, jump back to the condition to check again
  builder->CreateBr(cond);

  // --- MERGE (Exit) ---
  // All subsequent code will be emitted here
  builder->SetInsertPoint(merge);
}

llvm_value_t *
codegen_t::visit_unary(SP<ast_node_t> node) {
  unary_expr_t *expr = node->as.unary;

  switch (expr->op) {
  case token_type_t::operatorExclamation: {
    // Pointer coercion, does nothing here, LLVM doesn't care.
    return visit_node(expr->value);
  }
  default:
    assert(false && "Unhandled unary operation");
  }
  return nullptr;
}

llvm_value_t *
codegen_t::visit_node(SP<ast_node_t> node) {
  if (!node->type) {
    assert(false && "Internal Compiler Error: AST node has no type!");
  }

  llvm_value_t *result = nullptr;
  switch (node->kind) {
  case ast_node_t::eExtern:
    result = visit_extern(node);
    break;

  case ast_node_t::eFunctionImpl:
    result = visit_function_impl(node);
    break;

  case ast_node_t::eFunctionDecl:
    result = visit_function_decl(node);
    break;

  case ast_node_t::eBlock:
    visit_block(node);
    break;

  case ast_node_t::eReturn:
    visit_return(node);
    break;

  case ast_node_t::eCall:
    result = visit_call(node);
    break;

  case ast_node_t::eLiteral:
    result = visit_literal(node);
    break;

  case ast_node_t::eSymbol:
    result = visit_symbol(node);
    break;

  case ast_node_t::eDeclaration:
    result = visit_declaration(node);
    break;

  case ast_node_t::eStructDecl:
    result = visit_struct_definition(node);
    break;

  case ast_node_t::eBinop:
    result = visit_binop(node);
    break;

  case ast_node_t::eCast:
    result = visit_cast(node);
    break;

  case ast_node_t::eAssignment:
    result = visit_assignment(node);
    break;

  case ast_node_t::eSelf:
    result = scopes.back()->resolve("self");
    break;

  case ast_node_t::eAddrOf:
    result = address_of(node);
    break;

  case ast_node_t::eMemberAccess:
    result = visit_member_access(node);
    break;

  case ast_node_t::eAttribute:
    result = visit_attribute(node);
    break;

  case ast_node_t::eDeref:
    result = visit_deref(node);
    break;

  case ast_node_t::eNil:
    result = visit_nil(node);
    break;

  case ast_node_t::eIf:
    visit_if(node);
    break;

  case ast_node_t::eFor:
    visit_for(node);
    break;

  case ast_node_t::eUnary:
    result = visit_unary(node);
    break;


  case ast_node_t::eTypeAlias: // NO-OP
    break;

  default:
    assert(false && "Unexpected AST node in codegen");
  }
  return result;
}
