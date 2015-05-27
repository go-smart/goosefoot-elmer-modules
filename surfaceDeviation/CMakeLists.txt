
cmake_minimum_required(VERSION 2.8)

find_package(VTK REQUIRED)
include(${VTK_USE_FILE})

add_executable(surface-deviation distance-ave.cpp)

target_link_libraries(surface-deviation ${VTK_LIBRARIES} boost_program_options)

INSTALL(TARGETS surface-deviation RUNTIME DESTINATION "bin" LIBRARY DESTINATION "lib") 
