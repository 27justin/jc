#pragma once

#include "type.hpp"

#include <map>
#include <set>
#include <string>
#include <unordered_map>

class type_registry_t {
  std::vector<std::unique_ptr<qualified_type_t>> pool;
  std::map<std::string, qualified_type_t *>      cache;

  // u32,i32,u16,i16,u8,i8,i64,u64,f32,f64,bool,any,void
  public:
  type_registry_t();

  qualified_type_t *
  resolve(const std::string &);

  template<typename T, typename... Args>
  qualified_type_t *
  ensure(Args &&...args) {
    auto temp = std::make_unique<T>(std::forward<Args>(args)...);
    auto name = temp->to_string();

    if (cache.contains(name)) {
      return cache.at(name);
    }

    T *result = temp.get();
    pool.push_back(std::move(temp));
    cache[name] = result;
    return result;
  }

  template<typename T, typename... Args>
  qualified_type_t *
  ensure_or_overwrite(Args &&...args) {
    auto temp = std::make_unique<T>(std::forward<Args>(args)...);
    auto name = temp->to_string();

    T *result = temp.get();
    pool.push_back(std::move(temp));
    cache[name] = result;
    return result;
  }
};
