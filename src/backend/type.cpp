#include "backend/type.hpp"

bool type_t::is_numeric() const {
  return kind == eInt || kind == eUint || kind == eFloat;
}

void
struct_layout_t::compute_memory_layout() {
  size_t current_offset = 0;
  size_t max_align = 1; // Minimum alignment is typically 1 byte

  for (auto& member : members) {
    size_t member_align = member.type->is_pointer ? sizeof(void*) : member.type->base->alignment;
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

  this->size = final_size;
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

size_t size_of(SP<qualified_type_t> type) {
  if (!type->is_pointer)
    return type->base->size;
  return sizeof(void*);
}

size_t alignment_of(SP<qualified_type_t> type) {
  if (!type->is_pointer)
    return type->base->alignment;
  return sizeof(void*);
}

