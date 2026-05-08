#pragma once

#include <any>
#include <concepts>
#include <functional>
#include <map>
#include <memory>
#include <typeindex>
#include <vector>

#include "backend/type.hpp"
#include "frontend/source.hpp"
#include "frontend/token.hpp"

template<typename T>
using SP = std::shared_ptr<T>;

template<typename T>
using UP = std::unique_ptr<T>;

enum class binop_type_t {
  eAdd,
  eSubtract,
  eDivide,
  eMultiply,
  eAnd,
  eOr,
  eEqual,
  eNotEqual,
  eGT,
  eLT,
  eLTE,
  eGTE,
  eMod,
  eBitAnd,
  eBitOr,
  eBitShiftLeft,
  eBitShiftRight,
  eXor,
  eAssign
};

std::string to_string(binop_type_t);

struct node_location_t {
  SP<source_t>      source;
  source_location_t location;
};

class ast_node_t {
  protected:
  ast_node_t();

  std::map<std::type_index, std::any> data;

  public:
  ast_node_t(ast_node_t &) = delete;
  ast_node_t(ast_node_t &&);

  virtual ~ast_node_t();

  void
  operator=(ast_node_t &) = delete;
  void
  operator=(ast_node_t &&) = delete;

  template<typename _Derived>
    requires std::derived_from<_Derived, ast_node_t>
  _Derived *
  as() {
    return dynamic_cast<_Derived *>(this);
  }

  template<typename _Derived>
    requires std::derived_from<_Derived, ast_node_t>
  const _Derived *
  as() const {
    return dynamic_cast<const _Derived *>(this);
  }

  template<typename _Derived>
    requires std::derived_from<_Derived, ast_node_t>
  bool
  is() const {
    return dynamic_cast<const _Derived *>(this) != nullptr;
  }

  template<typename _Data, typename... Args>
  void
  set(Args &&...args) {
    data[typeid(_Data)] = std::make_any<_Data>(std::forward<Args>(args)...);
  }

  template<typename _Data>
  _Data &
  get() {
    return std::any_cast<_Data &>(data[typeid(_Data)]);
  }

  void
  visit(const std::function<void(ast_node_t &)>       &visitor,
        const std::function<bool(const ast_node_t &)> &filter);
  virtual void
  visit(const std::function<void(ast_node_t &)> &visitor);
};

#include "ast/path.hpp"

#include "ast/constant.hpp"
#include "ast/import.hpp"
#include "ast/string.hpp"
#include "ast/type.hpp"

#include "ast/block.hpp"
#include "ast/declaration.hpp"
#include "ast/symbol.hpp"

#include "ast/contextual.hpp"
#include "ast/function.hpp"
#include "ast/result_check.hpp"

#include "ast/destructure.hpp"
#include "ast/enum.hpp"
#include "ast/memory.hpp"
#include "ast/struct.hpp"
#include "ast/tuple.hpp"

#include "ast/for.hpp"
#include "ast/if.hpp"
#include "ast/while.hpp"

#include "ast/binop.hpp"
