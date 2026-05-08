#pragma once

#include <string>

// Unescaped string literal
class string_node_t : public ast_node_t {
  public:
  string_node_t(const std::string &value);

  std::string value;
};
