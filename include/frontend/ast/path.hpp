#pragma once

#include <algorithm>
#include <memory>
#include <string>
#include <vector>

class type_node_t;
struct path_segment_t {
  std::string                  identifier;
  std::vector<UP<type_node_t>> template_args;

  bool
  is_templated() const {
    return !template_args.empty();
  }
};

struct path_node_t : public ast_node_t {
  std::vector<path_segment_t> segments;

  bool
  is_templated() const {
    return std::any_of(segments.begin(), segments.end(), [](auto &V) { return V.is_templated(); });
  }

  std::string
  to_string() const;
};
