cmake_minimum_required(VERSION 3.20)

project(jc CXX)

set(CMAKE_CXX_STANDARD 23)

add_executable(jcc
  src/frontend/source.cpp
  src/frontend/diagnostic.cpp
  src/frontend/token.cpp
  # src/frontend/lexer.cpp
  # src/frontend/parser.cpp
  # src/frontend/ast.cpp
  # src/frontend/type_registry.cpp
  # src/frontend/scope.cpp
  # src/frontend/analyzer.cpp
  src/frontend/compiler.cpp)

target_include_directories(jcc PUBLIC include)

