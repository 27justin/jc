#include "backend/codegen.hpp"
#include "backend/analyzer.hpp"
#include "backend/type.hpp"
#include "frontend/ast.hpp"
#include "frontend/token.hpp"
#include "llvm/IR/Function.h"

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

void
llvm_scope_t::add(const std::string &name, llvm::Value *value) {
  symbol_map[name] = value;
}

llvm::Value *
llvm_scope_t::resolve(const std::string &name) {
  if (symbol_map.contains(name))
    return symbol_map[name];

  if (parent)
    parent->resolve(name);

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

  scopes.push_back(llvm_scope_t {nullptr});
}

void
codegen_t::compile_to_object(const std::string& filename) {
  module->print(llvm::outs(), nullptr);
  // std::error_code EC;
  // llvm::raw_fd_ostream dest(filename, EC, llvm::sys::fs::OF_None);

  // if (EC) {
  //   throw std::runtime_error("Could not open file: " + EC.message());
  // }

  // llvm::legacy::PassManager pass;

  // auto file_type = llvm::CodeGenFileType::ObjectFile;
  // if (target_machine->addPassesToEmitFile(pass, dest, nullptr, file_type)) {
  //   throw std::runtime_error("TargetMachine can't emit a file of this type");
  // }

  // pass.run(*module);
  // dest.flush();;
}

codegen_t::codegen_t(semantic_info_t &&s) : info(std::move(s)) {
  context = std::make_unique<llvm::LLVMContext>();
  module = std::make_unique<llvm::Module>("jcc_module", *context);
  builder = std::make_unique<llvm::IRBuilder<>>(*context);

  init_target();
}

void
codegen_t::generate() {
  for (auto &node : info.unit.declarations) {
    visit_node(node);
  }
}

llvm::Type *codegen_t::ensure_type(SP<qualified_type_t> type) {
  if (type->is_pointer)
    return llvm::PointerType::get(*context, 0);
  return ensure_type(type->base);
}

llvm::Type *codegen_t::ensure_type(SP<type_t> base) {
  // 1. Check our side-table (Cache) first to avoid redundant work
  if (llvm_type_cache.contains(base)) {
    return llvm_type_cache.at(base);
  }

  llvm::Type* result = nullptr;

  switch (base->kind) {
  case type_t::eInt:
    result = llvm::Type::getIntNTy(*context, base->size * 8);
    break;
  case type_t::eUint:
    result = llvm::Type::getIntNTy(*context, base->size * 8);
    break;
  case type_t::eStruct: {
    // First, see if we already made a shell for this struct
    result = llvm::StructType::getTypeByName(*context, base->name);
    if (!result) {
      // If it's a new struct, create a named shell
      auto st = llvm::StructType::create(*context, base->name);

      std::vector<llvm::Type *> fields;
      auto layout = base->as.struct_layout;
      for (auto &member : layout->members) {
        fields.push_back(ensure_type(member.type));
      }
      st->setBody(fields);

      result = st;
    }
    break;
  }
  case type_t::eFunction: {
    auto fn = base->as.function;
    llvm::Type* ret_ty = ensure_type(fn->return_type);
    std::vector<llvm::Type*> params;
    for (auto &p : fn->arg_types)
      params.push_back(ensure_type(p));

    result = llvm::FunctionType::get(ret_ty, params, fn->is_var_args);
    break;
  }
  case type_t::eFloat: {
    result = base->size == 4 ? llvm::Type::getFloatTy(*context) : llvm::Type::getDoubleTy(*context);
    break;
  }
  case type_t::eOpaque:
  case type_t::eAlias: {
    result = ensure_type(base->as.alias->alias);
    break;
  }
  case type_t::ePointer: {
    result = llvm::PointerType::get(*context, 0);
    break;
  }
  case type_t::eVoid: {
    result = llvm::Type::getVoidTy(*context);
    break;
  }
  }

  // Cache the result
  llvm_type_cache[base] = result;
  return result;
}

llvm::Type *codegen_t::map_type(SP<ast_node_t> node) {
  if (info.resolved_types.contains(node)) {
    auto ty = info.resolved_types.at(node);
    return ensure_type(ty);
  }
  throw std::runtime_error("Node has no type, not allowed.");
  return nullptr;
}

llvm::Value *
codegen_t::visit_function(SP<ast_node_t> node) {
  auto func_impl = node->as.fn_impl;
  auto func_sig = func_impl->declaration->as.fn_decl;

  // 1. Check if the function was already declared (e.g. via extern or forward decl)
  llvm::Function* func = module->getFunction(func_impl->declaration->as.fn_decl->name);

  if (!func) {
    // Reuse logic from visit_extern or call a helper to create the Function*
    // ... (Create Function* and FunctionType here)
    llvm::FunctionType *func_type = llvm::dyn_cast<llvm::FunctionType>(map_type(func_impl->declaration));

    // 3. Register in the module
    func = llvm::Function::Create(
      func_type, llvm::Function::ExternalLinkage, func_impl->declaration->as.fn_decl->name, module.get()
      );
  }

  // 2. Create the "entry" Basic Block
  llvm::BasicBlock* entry = llvm::BasicBlock::Create(*context, "entry", func);
  builder->SetInsertPoint(entry);

  scopes.push_back(&scopes.back());

  int64_t idx = 0;
  for (auto &arg : func->args()) {
    auto ast_param = func_sig->parameters[idx++];

    llvm::Type *ty = arg.getType();
    llvm::AllocaInst *alloca = builder->CreateAlloca(ty, nullptr, ast_param->as.fn_param->name);
    builder->CreateStore(&arg, alloca);

    scopes.back().add(ast_param->as.fn_param->name, alloca);
  }

  for (auto& stmt : func_impl->block->as.block->body) {
    visit_node(stmt);
  }

  if (func->getReturnType()->isVoidTy()) {
    builder->CreateRetVoid();
  } else if (func_impl->declaration->as.fn_decl->name == "main") {
    // Special case: ensure main always returns 0 if empty
    builder->CreateRet(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context), 0));
  }

  scopes.pop_back();

  return func;
}

llvm::Value *codegen_t::visit_extern(SP<ast_node_t> node) {
  auto ext = node->as.extern_decl; // Adjust to your AST naming

  switch (ext->import->type) {
  case ast_node_t::eFunctionDecl: {
    auto fn = ext->import->as.fn_decl;

    // 1. Convert your return type and param types to LLVM types
    llvm::Type* ret_type = map_type(fn->type);
    std::vector<llvm::Type*> param_types;
    for (auto& p : fn->parameters) {
      param_types.push_back(map_type(p));
    }

    // 2. Create the FunctionType
    llvm::FunctionType* func_type = llvm::FunctionType::get(
      ret_type, param_types, fn->is_var_args
      );

    // 3. Register in the module
    llvm::Function* func = llvm::Function::Create(
      func_type, llvm::Function::ExternalLinkage, fn->name, module.get()
      );

    return func;
    break;
  }
  default:
    assert(false && "Only extern functions allowed currently");
  }
}

llvm::Value *
codegen_t::visit_return(SP<ast_node_t> node) {
  return_stmt_t *stmt = node->as.return_stmt;
  return builder->CreateRet(visit_node(stmt->value));
}

llvm::Value *
codegen_t::visit_literal(SP<ast_node_t> node) {
    auto lit = node->as.literal_expr;
    auto l_type = map_type(node);

    switch (lit->type) {
        case literal_type_t::eInteger:
          // APInt (Arbitrary Precision Integer) handles the bit width
          return llvm::ConstantInt::get(l_type, std::stoi(lit->value), true);
        case literal_type_t::eFloat:
          // ConstantFP handles f32, f64, etc.
          return llvm::ConstantFP::get(l_type, std::stof(lit->value));
        case literal_type_t::eString:
            // CreateGlobalStringPtr handles creating the global array and returning a pointe
            return builder->CreateGlobalString(lit->value);
        case literal_type_t::eBool:
            // Booleans in LLVM are just i1 (1-bit integers)
            return llvm::ConstantInt::get(l_type, lit->value == "true" ? 1 : 0);
    }
    return nullptr;
}


llvm::Value* codegen_t::visit_cast(SP<ast_node_t> node) {
  auto cast = node->as.cast;

  llvm::Value* source_val = visit_node(cast->value);
  llvm::Type* src_type = source_val->getType();
  llvm::Type* dst_type = map_type(cast->type);

  // 1. Bitcast (Same size, different interpretation)
  // In LLVM 21, ptr -> ptr casts are identity (no-op)
  if (src_type == dst_type) return source_val;

  // 2. Integer to Integer
  if (src_type->isIntegerTy() && dst_type->isIntegerTy()) {
    unsigned src_bits = src_type->getIntegerBitWidth();
    unsigned dst_bits = dst_type->getIntegerBitWidth();

    if (src_bits < dst_bits) {
      // Widening: Check if signed or unsigned from your analyzer
      // if (map_type(cast->type)) return builder->CreateSExt(source_val, dst_type, "sext");
      return builder->CreateZExt(source_val, dst_type, "zext");
    } else {
      // Narrowing
      return builder->CreateTrunc(source_val, dst_type, "trunc");
    }
  }

  // 3. Integer <-> Float
  if (src_type->isIntegerTy() && dst_type->isFloatingPointTy()) {
    // if (cast->is_signed) return builder->CreateSIToFP(source_val, dst_type, "sitofp");
    return builder->CreateUIToFP(source_val, dst_type, "uitofp");
  }
  if (src_type->isFloatingPointTy() && dst_type->isIntegerTy()) {
    // if (cast->is_signed) return builder->CreateFPToSI(source_val, dst_type, "fptosi");
    return builder->CreateFPToUI(source_val, dst_type, "fptoui");
  }

  // 4. Pointer <-> Integer
  if (src_type->isPointerTy() && dst_type->isIntegerTy()) {
    return builder->CreatePtrToInt(source_val, dst_type, "ptrtoint");
  }
  if (src_type->isIntegerTy() && dst_type->isPointerTy()) {
    return builder->CreateIntToPtr(source_val, dst_type, "inttoptr");
  }

  return source_val; // Fallback
}

llvm::Value* codegen_t::visit_binop(SP<ast_node_t> node) {
  auto bin = node->as.binop;

  // Recursively generate code for the left and right sides
  llvm::Value* L = visit_node(bin->left);
  llvm::Value* R = visit_node(bin->right);

  bool is_float = L->getType()->isFloatingPointTy();

  switch (bin->op) {
    // --- Arithmetic ---
  case token_type_t::operatorPlus:
    return is_float ? builder->CreateFAdd(L, R, "addtmp") 
      : builder->CreateAdd(L, R, "addtmp");
  case token_type_t::operatorMinus:
    return is_float ? builder->CreateFSub(L, R, "subtmp") 
      : builder->CreateSub(L, R, "subtmp");
  case token_type_t::operatorMultiply:
    return is_float ? builder->CreateFMul(L, R, "multmp") 
      : builder->CreateMul(L, R, "multmp");
  case token_type_t::operatorDivide:
    return is_float ? builder->CreateFDiv(L, R, "divtmp") 
      : builder->CreateSDiv(L, R, "divtmp"); // SDiv = Signed Div
  case token_type_t::operatorEqual:
    return builder->CreateStore(L, R);

    // --- Comparison (Logic) ---
  case token_type_t::operatorEquality: {
    llvm::Value *cmp;
    if (is_float) cmp = builder->CreateFCmpOEQ(L, R, "eqtmp"); // OEQ = Ordered & Equal
    cmp = builder->CreateICmpEQ(L, R, "eqtmp");
    return builder->CreateZExt(cmp, llvm::Type::getInt8Ty(*context), "booltmp");
  }

  case token_type_t::operatorNotEqual: {
    llvm::Value *cmp;
    if (is_float) cmp = builder->CreateFCmpONE(L, R, "netmp");
    cmp = builder->CreateICmpNE(L, R, "netmp");
    return builder->CreateZExt(cmp, llvm::Type::getInt8Ty(*context), "booltmp");
  }

  case token_type_t::delimiterLAngle: {
    llvm::Value *cmp;
    if (is_float) cmp = builder->CreateFCmpOLT(L, R, "lttmp");
    cmp = builder->CreateICmpSLT(L, R, "lttmp"); // SLT = Signed Less Than
    return builder->CreateZExt(cmp, llvm::Type::getInt8Ty(*context), "booltmp");
  }

  case token_type_t::delimiterRAngle: {
    llvm::Value *cmp;
    if (is_float) cmp = builder->CreateFCmpOGT(L, R, "gttmp");
    cmp = builder->CreateICmpSGT(L, R, "gttmp");
    return builder->CreateZExt(cmp, llvm::Type::getInt8Ty(*context), "booltmp");
  }
  default:
    assert(false && "Unknown binary operation");
  }
  return nullptr;
}

llvm::Value *codegen_t::visit_self(SP<ast_node_t>) {
  return scopes.back().resolve("self");
}

llvm::AllocaInst* codegen_t::create_entry_block_alloca(llvm::Function* func,
                                                       llvm::Type* type,
                                                       const std::string& name) {
  // Create a temporary builder pointing to the start of the entry block
  llvm::IRBuilder<> tmp_builder(&func->getEntryBlock(), func->getEntryBlock().begin());
  return tmp_builder.CreateAlloca(type, nullptr, name);
}

llvm::Value *codegen_t::visit_declaration(SP<ast_node_t> node) {
  auto &scope = scopes.back();

  declaration_t *decl = node->as.declaration;

  // Get the type we want to store.
  llvm::Type *type = map_type(decl->type);

  llvm::Function *current_fn = builder->GetInsertBlock()->getParent();
  auto alloca = create_entry_block_alloca(current_fn, type, decl->identifier);

  if (decl->value) {
    // Compute the value
    llvm::Value *expr = visit_node(decl->value);
    builder->CreateStore(expr, alloca);
  } else {
    // Zero-initialize by default.
    builder->CreateStore(llvm::Constant::getNullValue(type), alloca);
  }

  scope.add(decl->identifier, alloca);
  return alloca;
}

llvm::Value *codegen_t::visit_call(SP<ast_node_t> node) {
  auto call = node->as.call_expr;

  auto fn = llvm::dyn_cast<llvm::Function>(visit_node(call->callee));

  std::vector<llvm::Value *> args;
  for (auto &arg : call->arguments) {
    args.push_back(visit_node(arg));
  }

  auto call_inst = builder->CreateCall(fn->getFunctionType(), fn, args);
  return call_inst;
}

llvm::Value* codegen_t::visit_symbol(SP<ast_node_t> node) {
  auto sym = node->as.symbol;
  llvm::Value* address = scopes.back().resolve(sym->identifier);

  if (!address) {
    // Might be a function
    return module->getFunction(sym->identifier);
  }

  // Get the LLVM type of the variable
  llvm::Type* llvm_type = map_type(node);

  // Emit: %1 = load i32, ptr %address
  // Note: LLVM 21 requires the Type to be passed to CreateLoad
  return builder->CreateLoad(llvm_type, address, sym->identifier);
}

llvm::Value* codegen_t::get_address_of_node(SP<ast_node_t> node) {
    switch (node->type) {
        case ast_node_t::eSymbol: {
            auto id = node->as.symbol->identifier;
            // Return the AllocaInst* or GlobalVariable* we stored earlier
            return scopes.back().resolve(id);
        }

        // case ast_node_t::eDeref: {
        //     // If we have *ptr, and we take the address &(*ptr), 
        //     // it's just the value of 'ptr' itself.
        //     return visit_node(node->as.unary_op->expr);
        // }

        // case ast_node_t::eMemberAccess: {
        //     // This is where GetElementPtr (GEP) happens for person.age
        //     return visit_member_access_address(node);
        // }

        default:
            throw std::runtime_error("Cannot take the address of a non-lvalue");
    }
}

llvm::Value* codegen_t::visit_if(SP<ast_node_t> node) {
  auto if_node = node->as.if_stmt;

  // 1. Evaluate the condition
  llvm::Value* cond_v = visit_node(if_node->condition);
  // LLVM 'br' requires an i1 (boolean). If your condition is i32,
  // you must compare it: builder->CreateICmpNE(cond_v, zero)
  if (!cond_v->getType()->isIntegerTy(1)) {
    cond_v = builder->CreateICmpNE(
      cond_v, llvm::ConstantInt::get(cond_v->getType(), 0), "ifcond"
      );
  }

  // 2. Setup the blocks
  llvm::Function* the_func = builder->GetInsertBlock()->getParent();

  llvm::BasicBlock* then_bb  = llvm::BasicBlock::Create(*context, "then", the_func);
  llvm::BasicBlock* else_bb  = llvm::BasicBlock::Create(*context, "else");
  llvm::BasicBlock* merge_bb = llvm::BasicBlock::Create(*context, "ifcont");

  // 3. Emit the conditional branch
  builder->CreateCondBr(cond_v, then_bb, else_bb);

  // --- Generate 'Then' Block ---
  builder->SetInsertPoint(then_bb);
  visit_node(if_node->pass);
  // Every block must end with a terminator. We jump to merge.
  builder->CreateBr(merge_bb);
  // Update then_bb in case visit_node created new nested blocks
  then_bb = builder->GetInsertBlock();

  // --- Generate 'Else' Block ---
  // We didn't add else_bb to the function earlier to keep order clean
  the_func->insert(the_func->end(), else_bb);
  builder->SetInsertPoint(else_bb);
  if (if_node->reject) {
    visit_node(if_node->reject);
  }
  builder->CreateBr(merge_bb);
  else_bb = builder->GetInsertBlock();

  // --- Continue at Merge Block ---
  the_func->insert(the_func->end(), merge_bb);
  builder->SetInsertPoint(merge_bb);

  return nullptr; // Statements don't return values
}

llvm::Value* codegen_t::visit_unary(SP<ast_node_t> node) {
  auto un = node->as.unary;

  // 1. Generate the value to be operated on
  llvm::Value* v = visit_node(un->value);
  bool is_float = v->getType()->isFloatingPointTy();

  switch (un->op) {
  case token_type_t::operatorMinus:
    // In LLVM, negation is implemented as (0 - V)
    if (is_float) {
      return builder->CreateFNeg(v, "negtmp");
    } else {
      return builder->CreateNeg(v, "negtmp");
    }

  case token_type_t::operatorExclamation:
    // Logical NOT (!)
    // If it's a boolean (i1), we XOR it with 1 (true)
    // If it's an integer, we compare it to zero (v == 0)
    if (v->getType()->isIntegerTy(1)) {
      return builder->CreateNot(v, "nottmp");
    } else {
      return builder->CreateICmpEQ(
        v, llvm::ConstantInt::get(v->getType(), 0), "nottmp"
        );
    }

  case token_type_t::operatorAnd:
    // We already covered this, but often it lives here in the dispatcher
    return visit_addr_of(node);

  // case unary_op_t::eDeref:
  //   // The inverse of AddrOf
  //   return visit_deref(node);

  // case unary_op_t::eBitwiseNot:
  //   // The ~ operator
  //   return builder->CreateNot(v, "bitnot");
  default:
    break;
  }

  return nullptr;
}

llvm::Value* codegen_t::visit_addr_of(SP<ast_node_t> node) {
    auto addr_of = node->as.addr_of; // Assuming & is a unary operator
    return get_address_of_node(addr_of->value);
}

llvm::Value *codegen_t::visit_member_access(SP<ast_node_t> node) {
  auto access = node->as.member_access;

  auto llvm_type = map_type(access->object);
  auto qtype = info.resolved_types[access->object];

  assert(qtype->base->kind == type_t::kind_t::eStruct);

  size_t member_idx = 0;
  // Figure out the member pointer
  auto layout = qtype->base->as.struct_layout;
  auto p = layout->member(access->member);
  assert(p != nullptr);

  member_idx = p - &*layout->members.begin();

  auto load = builder->CreateStructGEP(llvm_type, get_address_of_node(access->object), member_idx);

  return load;
}

llvm::Value *
codegen_t::visit_block(SP<ast_node_t> node) {
  block_node_t *block = node->as.block;
  for (auto &node : block->body)
    visit_node(node);

  return nullptr;
}

llvm::Value *
codegen_t::visit_node(SP<ast_node_t> node) {
  switch (node->type) {
  case ast_node_t::eExtern:
    return visit_extern(node);

  case ast_node_t::eFunctionImpl:
    return visit_function(node);

  case ast_node_t::eTypeAlias:
    return nullptr;

  case ast_node_t::eStructDecl:
    return nullptr;

  case ast_node_t::eReturn:
    return visit_return(node);

  case ast_node_t::eBinop:
    return visit_binop(node);

  case ast_node_t::eLiteral:
    return visit_literal(node);

  case ast_node_t::eCast:
    return visit_cast(node);

  case ast_node_t::eSelf:
    return visit_self(node);

  case ast_node_t::eDeclaration:
    return visit_declaration(node);

  case ast_node_t::eCall:
    return visit_call(node);

  case ast_node_t::eSymbol:
    return visit_symbol(node);

  case ast_node_t::eAddrOf:
    return visit_addr_of(node);

  case ast_node_t::eIf:
    return visit_if(node);

  case ast_node_t::eUnary:
    return visit_unary(node);

  case ast_node_t::eMemberAccess:
    return visit_member_access(node);

  case ast_node_t::eBlock:
    return visit_block(node);

  default:
    assert(false && "Unexpected AST node in codegen");
  }
}

