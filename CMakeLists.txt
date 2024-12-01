cmake_minimum_required(VERSION 3.29)
project(StructureAnalysis Fortran)

file(GLOB_RECURSE my_service_modules ${CMAKE_BINARY_DIR}/../services/*.f90)
file(GLOB_RECURSE my_model_modules ${CMAKE_BINARY_DIR}/../models/*.f90)
file(GLOB_RECURSE my_manager_modules ${CMAKE_BINARY_DIR}/../managers/*.f90)
file(GLOB_RECURSE my_data_structure_modules ${CMAKE_BINARY_DIR}/../data_structures/*.f90)

#stdlib
#file(GLOB_RECURSE stdlib_sources ${CMAKE_BINARY_DIR}/../lib/stdlib/*.f90)
#fig
#file(GLOB_RECURSE fig_sources ${CMAKE_BINARY_DIR}/../lib/fig/*.f90)

enable_language(Fortran)
add_executable(StructureAnalysis
        ${my_manager_modules}
        ${my_service_modules}
        ${my_model_modules}
        ${my_data_structure_modules}
        #${stdlib_sources}
        #${fig_sources}
        main.f90)

