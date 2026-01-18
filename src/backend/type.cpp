#include "backend/type.hpp"
#include <memory>
#include <sstream>

bool type_t::is_numeric() const {
  return kind == eInt || kind == eUint || kind == eFloat;
}

void
struct_layout_t::compute_memory_layout() {
  size_t current_offset = 0;
  size_t max_align = 1; // Minimum alignment is typically 1 byte

  for (auto& member : members) {
    size_t member_align = alignment_of(member.type);
    size_t member_size  = size_of(member.type);

    if (current_offset % member_align != 0) {
      current_offset += (member_align - (current_offset % member_align));
    }

    member.offset = current_offset;
    current_offset += member_size;

    if (member_align > max_align) {
      max_align = member_align;
    }
  }

  size_t final_size = current_offset;
  if (final_size % max_align != 0) {
    final_size += (max_align - (final_size % max_align));
  }

  this->size = final_size * 8; // We store size in bits
  this->alignment = max_align;
}

struct_layout_t::field_t *
struct_layout_t::member(const std::string &name) {
  auto it = members.begin();
  for (; it != members.end(); ++it) {
    if (it->name == name)
      return &*it;
  }
  return nullptr;
}

size_t size_of(SP<type_t> type) {
  return size_of(*type);
}

size_t alignment_of(SP<type_t> type) {
  return alignment_of(*type);
}

size_t size_of(const type_t &type) {
  if (type.kind != type_kind_t::ePointer)
    return type.size;
  return sizeof(void*);
}

size_t alignment_of(const type_t &type) {
  if (type.kind != type_kind_t::ePointer)
    return type.alignment;
  return alignof(void*);
}

std::string to_string(SP<type_t> type) {
  return to_string(*type);
}

std::string to_string(const type_t &type) {
  std::stringstream ss;

  switch (type.kind) {
  case type_kind_t::ePointer: {
    pointer_t *ptr = type.as.pointer;
    if (ptr->is_mutable) ss << "var ";
    for (auto &indirection : ptr->indirections) {
      ss << (indirection == pointer_kind_t::eNonNullable ? '!' : '?');
    }
  }
  default:
    break;
  }

  ss << type.name;
  return ss.str();
}

SP<type_t>
pointer_t::deref() const {
  if (indirections.size() == 1) {
    return base;
  } else {
    // Strip away first indirection and return that as a new type.
    auto indirections = this->indirections;
    indirections.erase(indirections.begin());

    auto type = std::make_shared<type_t>();
    type->kind = type_kind_t::ePointer;
    type->size = sizeof(void*);
    type->alignment = sizeof(void*);
    type->as.pointer = new pointer_t {
      .is_mutable = this->is_mutable,
      .indirections = indirections,
      .base = this->base
    };
    return type;
  }
}

