#pragma once

class contextual_node_t : public ast_node_t {
  public:
  contextual_node_t(UP<path_node_t> &&primary);
  contextual_node_t();

  UP<path_node_t> primary;
};
