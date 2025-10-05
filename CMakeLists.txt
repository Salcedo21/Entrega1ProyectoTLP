cmake_minimum_required(VERSION 3.28.3)
project(Proyecto_Practico_TLP)

set(CMAKE_CXX_STANDARD 17)

add_executable(analizador main.cpp)
configure_file(${CMAKE_SOURCE_DIR}/Tetris.brik ${CMAKE_CURRENT_BINARY_DIR}/Tetris.brik COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/Snake.brik  ${CMAKE_CURRENT_BINARY_DIR}/Snake.brik  COPYONLY)