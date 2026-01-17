#pragma once

#include <memory>
#include <vector>

template<typename T>
using SP = std::shared_ptr<T>;

struct symbol_t;
struct type_t;

struct struct_layout_t {
  struct field_t {
    std::string name;
    SP<type_t> type;
    size_t offset; //< Offset from struct start
  };

  uint64_t alignment = 0, size = 0; //< Alignment in bytes, size in bits
  std::vector<field_t> members;

  field_t *member(const std::string &name);
  void compute_memory_layout();
};

struct function_signature_t {
  std::shared_ptr<type_t> return_type;
  std::vector<std::shared_ptr<type_t>> arg_types;
  SP<type_t> receiver;
  bool is_var_args;
};

struct type_alias_t {
  SP<type_t> alias;
};

enum class pointer_kind_t {
  eNullable,
  eNonNullable
};

struct pointer_t {
  // ?!u8    -> nullable pointer to non-nullable
  //            pointer_t { mutable = false, indirections = (eNullable, eNonNullable) }
  // var !u8 -> non-nullable pointer to mutable data
  bool is_mutable = false;
  std::vector<pointer_kind_t> indirections;
  SP<type_t> base;
};

enum type_kind_t { eStruct, eFunction, eInt, eUint, eFloat, ePointer, eOpaque, eAlias, eVoid, eBool };
struct type_t {
  type_kind_t kind;
  std::string name;

  size_t size, alignment; //< Alignment in bytes, size in bits.
  SP<symbol_t> deinit; //< Called on scope exit

  struct {
    union {
      function_signature_t *function;
      struct_layout_t *struct_layout;
      type_alias_t *alias;
      pointer_t *pointer;
    };
  } as;

  ~type_t() = default;

  bool is_numeric() const;
};

/// @brief Return bitsize of type
size_t size_of(SP<type_t>);
size_t alignment_of(SP<type_t>);

size_t size_of(const type_t &);
size_t alignment_of(const type_t &);

std::string to_string(const type_t &);
std::string to_string(SP<type_t>);
