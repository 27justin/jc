#pragma once

#include <string>
#include <memory>
#include <cstdint>
#include <variant>
#include <vector>

struct symbol_t;
struct type_t;

struct struct_member_t {
  std::shared_ptr<type_t> type;
  std::string name;
  uint64_t offset;
};

struct struct_type_t {
  uint64_t alignment = 0, size = 0;
  std::vector<struct_member_t> members;

  void compute_memory_layout();
};

struct function_type_t {
  std::shared_ptr<type_t> return_type;
  std::vector<std::shared_ptr<type_t>> arg_types;
};

struct type_t {
  std::string name;
  uint64_t size, alignment;

  std::shared_ptr<symbol_t> deinit;
  std::variant<struct_type_t, function_type_t> detail;
};


