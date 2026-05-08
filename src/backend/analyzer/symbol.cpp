#include "backend/analyzer.hpp"
#include "backend/type.hpp"
#include "backend/type_registry.hpp"
#include <cassert>
#include <span>

#include "backend/types/any.hpp"
#include "backend/types/enum.hpp"
#include "backend/types/pointer.hpp"
#include "backend/types/struct.hpp"
#include "backend/types/tuple.hpp"

#include "frontend/ast.hpp"

qualified_type_t *
resolve_member_access(const qualified_type_t *type, const std::string &member) {
  if (auto ptr = type->as<pointer_type_t>()) {
    // Deref all indirections away
    type = ptr->base_type();
  }

  if (auto struct_type = type->as<struct_type_t>()) {
    return struct_type->member_type_by_name(member);
  }

  if (auto aggregate = type->as<aggregate_enum_type_t>()) {
    return const_cast<qualified_type_t *>(aggregate->tag_by_name(member));
  }
  return nullptr;
}

qualified_type_t *
analyzer_t::resolve_type_path(qualified_type_t *type, const std::string &member) {

  if (auto aggregate = type->as<aggregate_enum_type_t>()) {
    auto member_type = aggregate->tag_by_name(member);
    // Member does not exist on this aggregate
    if (!member_type)
      return nullptr;

    if (member_type->is<any_type_t>()) {
      // Has no state, just change the tag, no function call needed.
      return aggregate;
    } else {
      // Stateful, needs a function call with the value.
      return registry_.ensure<variant_constructor_type_t>(*aggregate, member);
    }
  }

  if (auto scalar = type->as<scalar_enum_type_t>()) {
    if (scalar->has_member(member)) {
      // Scalar enum members decay to the enum type itself.
      return scalar;
    } else {
      // Member does not exist on this enum.
      return nullptr;
    }
  }

  return nullptr;
}

std::string
join(std::span<const std::string> slices, std::string seperator) {
  std::string accum;

  for (auto &s : slices) {
    accum.append(s);
    accum.append(seperator);
  }

  if (accum.size() > 0) {
    ssize_t i = 0;
    while (i++ < seperator.size())
      accum.pop_back();
  }
  return accum;
}

qualified_type_t *
analyzer_t::resolve_fully_qualified_path(const std::vector<std::string> &path) {
  // Partout resolve the path, but only consider nominal types, not local symbols.

  for (auto i = 0; i < path.size(); ++i) {
    auto range = std::span<const std::string>(path).subspan(0, i);
    auto type  = registry_.resolve(join(range, "."));
    if (auto result = resolve_member_access(type, path[i + 1])) {
      return result;
    }
  }
  return nullptr;
}

static bool _registered = []() {
  analyzer_t::register_analyzer<symbol_node_t>(
    [](symbol_node_t &symbol, analyzer_t &A) -> qualified_type_t * {
      auto &path = symbol.path;

      // We might be able to resolve the fully qualified path as a symbol, e.g. functions.
      if (auto qt = A.scope().resolve(path->to_string())) {
        return qt;
      }

      {
        // The path might also refer to an enum
        std::vector<std::string> type_path;
        for (auto i = 0; i < path->segments.size(); ++i) {
          type_path.push_back(path->segments[i].identifier);
        }

        for (auto i = 0; i < type_path.size() - 1; ++i) {
          auto prefix = std::span<std::string>(type_path).subspan(0, i + 1);
          if (prefix.size() == 0)
            continue;

          if (auto type = A.registry_.resolve(join(prefix, "."))) {
            if (auto member = A.resolve_type_path(type, type_path[i + 1])) {
              return member;
            }
          }
        }
      }

      // ... but if we do not, we probably have a member access expression.
      qualified_type_t *left = A.scope().resolve(path->segments.front().identifier);
      if (!left) {
        // No good, symbol doesn't exist.
        A.error(symbol.get<node_location_t>(),
                diagnostic_t::code_t::symbol_not_found,
                { path->to_string() });
        return nullptr;
      }

      // If we have a symbol to our left, we can continue descending into it, until the path is
      // empty.
      std::vector<path_segment_t> &segments = path->segments;
      for (auto i = 1; i < segments.size(); ++i) {
        auto current = resolve_member_access(left, segments[i].identifier);
        if (!current) {
          // Dead / Invalid path, never mind and exit.
          A.error(symbol.get<node_location_t>(),
                  diagnostic_t::code_t::member_not_found,
                  { left->to_string(), segments[i].identifier });
          return nullptr;
        }
        left = current;
      }
      return left;
    });

  return true;
}();
