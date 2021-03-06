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
cmake_force_fortran_compiler(ifort "Intel")
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
message(STATUS "Archiver: ${CMAKE_AR}")
message(STATUS "Linker: ${CMAKE_LINKER}")

# variables used in out of source build
set( LIBRARY_OUTPUT_PATH ${CMAKE_CURRENT_BINARY_DIR}/lib )
#set( CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/CMake )
# .mod files output directory (fortran specific)
set( CMAKE_Fortran_MODULE_DIRECTORY ${LIBRARY_OUTPUT_PATH}/mod)
# add the above path to include path
include_directories( ${LIBRARY_OUTPUT_PATH} )

# set Intel Fortran compiler flags
# @todo this should be done better
# FLAGS - generic flags
# DFLAGS - debugging flags
# OFLAGS - optimization flags
# PFLAGS - precision flags
set(Fortran77_FLAGS "" )
set(Fortran77_DFLAGS "")
set(Fortran77_OFLAGS "-O3")
set(Fortran77_PFLAGS "-fp-model strict -double-size 128 -integer-size 64")

set(Fortran90_FLAGS "-stand f08 -u -warn all -warn nounused -check all -cpp" )
set(Fortran90_DFLAGS "-debug all -gen-interfaces -traceback")
set(Fortran90_OFLAGS "-O3")
set(Fortran90_PFLAGS "-fp-model strict -real-size 128 -integer-size 64")

# set external libraries required by this project
set(external_libs "")

# add a subdirectory with source files for the project
add_subdirectory(src)

# subdirectory with tests
add_subdirectory(test)

include(${PROJECT_SOURCE_DIR}/test/CMakeTests.cmake)

include(${CMAKE_CURRENT_SOURCE_DIR}/config/doxygen.cmake)