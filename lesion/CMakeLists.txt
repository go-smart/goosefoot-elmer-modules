
cmake_minimum_required(VERSION 2.8)

find_package(VTK REQUIRED)
include(${VTK_USE_FILE})

SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x")

add_executable(go-smart-lesion go-smart-lesion.cpp)

target_link_libraries(go-smart-lesion ${VTK_LIBRARIES} boost_program_options tinyxml2)

INSTALL(TARGETS go-smart-lesion RUNTIME DESTINATION "bin" LIBRARY DESTINATION "lib") 
