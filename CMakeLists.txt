cmake_minimum_required(VERSION 3.20)

project(px CXX)

set(CMAKE_CXX_STANDARD 23)

add_executable(pxc
  src/frontend/source.cpp
  src/frontend/diagnostic.cpp
  src/frontend/token.cpp
  src/frontend/lexer.cpp
  src/frontend/parser.cpp
  src/frontend/ast.cpp
  src/frontend/path.cpp
  src/frontend/ast/declaration.cpp
  src/frontend/ast/constant.cpp
  src/frontend/ast/symbol.cpp
  src/frontend/ast/block.cpp
  src/frontend/ast/function.cpp
  src/frontend/ast/string.cpp
  src/frontend/ast/import.cpp
  src/frontend/ast/type.cpp
  src/frontend/ast/path.cpp
  src/frontend/ast/result_check.cpp
  src/frontend/ast/contextual.cpp
  src/frontend/ast/enum.cpp
  src/frontend/ast/tuple.cpp
  src/frontend/ast/memory.cpp
  src/frontend/dumper.cpp


  src/backend/analyzer.cpp
  src/backend/type.cpp
  src/backend/type_registry.cpp

  src/backend/types/tuple.cpp
  src/backend/types/nominal.cpp
  src/backend/types/any.cpp
  src/backend/types/function.cpp
  src/backend/types/number.cpp
  src/backend/types/struct.cpp
  src/backend/types/pointer.cpp
  src/backend/types/slice.cpp
  src/backend/types/aggregate_enum.cpp
  src/backend/types/scalar_enum.cpp

  src/backend/analyzer/declaration.cpp
  src/backend/analyzer/function.cpp
  src/backend/analyzer/type.cpp
  src/backend/analyzer/struct.cpp
  src/backend/analyzer/block.cpp
  src/backend/analyzer/binop.cpp
  src/backend/analyzer/symbol.cpp
  src/backend/analyzer/pointer.cpp
  src/backend/analyzer/constant.cpp
  src/backend/analyzer/slice.cpp
  src/backend/analyzer/contextual.cpp
  src/backend/analyzer/enum.cpp
  src/backend/analyzer/while.cpp
  src/backend/analyzer/if.cpp
  src/backend/analyzer/px_import.cpp
  src/backend/analyzer/memory.cpp

  src/codegen/codegen.cpp


  src/frontend/compiler.cpp)

target_include_directories(pxc PRIVATE third_party)
target_include_directories(pxc PRIVATE include)

# =======
#   LLVM
# =======
find_package(LLVM REQUIRED CONFIG)

llvm_map_components_to_libnames(llvm_libs
  core support bitwriter bitreader irreader irprinter extensions linker lto
  passes target targetparser x86info x86disassembler x86targetmca x86asmparser
  x86codegen)
target_include_directories(pxc PRIVATE ${LLVM_INCLUDE_DIRS})

#target_link_libraries(pxc ${llvm_libs})
target_link_libraries(pxc LLVM)
