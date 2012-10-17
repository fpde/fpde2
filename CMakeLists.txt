# Main CMake config file for fpde2 project
# @todo move compiler selecting, flag settings stuff out of this file
# @todo design a better test machine (which will select some predefined
# types of tests for different files)

cmake_minimum_required(VERSION 2.8)
# project name and language
project( fpde2 Fortran )
# library name
set(project_lib_name "${CMAKE_PROJECT_NAME}")
# @todo add VERSION and SOVERSION of fpde2 (in future)
message(STATUS "Building up ${CMAKE_PROJECT_NAME}")

# selecting Fortran compiler
include(CMakeForceCompiler)
# commented since this instruction does not seems
# to work with cmake 2.8
# cmake_force_fortran_compiler(ifort "Intel")

# the tools for creating libraries
# selecting archiver
find_program(xiar xiar)
	if(xiar)
	  set(CMAKE_AR "${xiar}")
	  set(CMAKE_RANLIB "${xiar}")
	endif(xiar)
mark_as_advanced(xiar)
#SET (CMAKE_C_ARCHIVE_APPEND "echo append")
# selecting linker
find_program(xild xild)
	if(xild)
	  set(CMAKE_LINKER "${xild}")
	endif(xild)
mark_as_advanced(xild)
# printing some software info
message(STATUS "Fortran compiler: ${CMAKE_Fortran_COMPILER_ID}")
message(STATUS "Fortran compiler: ${CMAKE_Fortran_COMPILER}")
message(STATUS "Archiver: ${CMAKE_AR}")
message(STATUS "Linker: ${CMAKE_LINKER}")
message(STATUS ${os})
SET (CMAKE_Fortran_CREATE_STATIC_LIBRARY "<CMAKE_AR> cru <LINK_FLAGS> <TARGET> <OBJECTS>")
# message(STATUS "${CMAKE_Fortran_CREATE_STATIC_LIBRARY}")
# variables used in out of source build
set( LIBRARY_OUTPUT_PATH ${CMAKE_CURRENT_BINARY_DIR}/lib )
# .mod files output directory (fortran specific)
set( CMAKE_Fortran_MODULE_DIRECTORY ${LIBRARY_OUTPUT_PATH}/mod)
# add the above path to include path
include_directories(${LIBRARY_OUTPUT_PATH})

# Set the compiler flags using a configuration file
set(Fortran90_FLAGS "@${PROJECT_SOURCE_DIR}/config/ifort/compiler.cfg")

set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/config ${CMAKE_MODULE_PATH})

# set external libraries required by this project
find_package(LuaJIT REQUIRED)
add_definitions("-Dluajit")

find_package(MKL REQUIRED)
include_directories(${MKL_INCLUDE_DIRS})

# define linker flags
set(Fortran90_LFLAGS ${LUAJIT_LIBRARY} ${MKL_LIBRARIES})

# add a subdirectory with source files for the project
add_subdirectory(src)

# subdirectory with examples
add_subdirectory(example EXCLUDE_FROM_ALL)

# subdirectory with atomic tests
add_subdirectory(atomic EXCLUDE_FROM_ALL)

# subdirectory with tests
add_subdirectory(test)
include(${PROJECT_SOURCE_DIR}/test/CMakeTests.cmake)
include(${CMAKE_CURRENT_SOURCE_DIR}/config/doxygen.cmake)
