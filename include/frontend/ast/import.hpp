#pragma once

#include "frontend/ast.hpp"
#include <string>

class import_node_t : public ast_node_t {};

// `import std.vector`
class px_import_node_t : public import_node_t {
  public:
  px_import_node_t(UP<path_node_t> &&path);
  UP<path_node_t> path;
};

// `import file:<"./pixel_shader.ps":path>`
class file_import_node_t : public import_node_t {
  public:
  file_import_node_t(const std::string &path);
  std::string path;
};

// `import C:<printf:identifier> as <fn (!u8, ..) -> i32:type>`
class c_import_node_t : public import_node_t {
  public:
  c_import_node_t(const std::string &, UP<type_node_t> &&);
  std::string     identifier;
  UP<type_node_t> type;
};
