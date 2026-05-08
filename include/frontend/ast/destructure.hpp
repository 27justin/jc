#pragma once

struct destructure_pattern_node_t : public ast_node_t {
  struct element {
    bool            is_mutable;
    UP<path_node_t> path;
  };
  std::vector<element> elements;
};
