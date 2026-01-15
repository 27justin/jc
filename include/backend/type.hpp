#pragma once

#include <memory>
#include <vector>

template<typename T>
using SP = std::shared_ptr<T>;

struct symbol_t;
struct type_t;
struct qualified_type_t;

struct struct_layout_t {
  struct field_t {
    std::string name;
    SP<qualified_type_t> type;
    size_t offset; //< Offset from struct start
  };

  uint64_t alignment = 0, size = 0;
  std::vector<field_t> members;

  field_t *member(const std::string &name);
  void compute_memory_layout();
};

struct function_signature_t {
  std::shared_ptr<qualified_type_t> return_type;
  std::vector<std::shared_ptr<qualified_type_t>> arg_types;
  SP<qualified_type_t> receiver;
  bool is_var_args;
};

struct type_alias_t {
  SP<qualified_type_t> alias;
};

struct type_t {
  enum kind_t { eStruct, eFunction, eInt, eUint, eFloat, ePointer, eOpaque, eAlias, eVoid } kind;

  std::string name;

  size_t size, alignment;
  SP<symbol_t> deinit; //< Called on scope exit

  struct {
    union {
      function_signature_t *function;
      struct_layout_t *struct_layout;
      type_alias_t *alias;
    };
  } as;

  ~type_t() = default;

  bool is_numeric() const;
};

struct qualified_type_t {
  SP<type_t> base;
  bool is_pointer = false;
  bool is_nullable = false;
  bool is_const = true;

  operator std::string() const;
};

size_t size_of(SP<qualified_type_t>);
size_t alignment_of(SP<qualified_type_t>);
