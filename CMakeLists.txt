# Main CMake config file for fpde2 project
cmake_minimum_required(VERSION 2.8)

# project name and language
project( fpde2 Fortran )
# library name
set(project_lib_name "${CMAKE_PROJECT_NAME}")
# @todo add VERSION and SOVERSION in future
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
set( CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/CMake )
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

# building tests target
# search for source files in ./test
file(GLOB tests_list RELATIVE ${PROJECT_SOURCE_DIR}/test ${PROJECT_SOURCE_DIR}/test/*.f90)
# create targets for each file from ./test/
foreach(bin ${tests_list})
  string(REPLACE ".f90" "" bin ${bin})
  add_executable(${bin} ${PROJECT_SOURCE_DIR}/test/${bin}.f90)
  # set the target binary file location
  set_target_properties(${bin}
    PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY
    ${CMAKE_CURRENT_BINARY_DIR}/test)
  # set the additional compile flags
  set_target_properties(${bin}
    PROPERTIES
    COMPILE_FLAGS
    "${Fortran90_FLAGS} ${Fortran90_DFLAGS} ${Fortran90_OFLAGS} ${Fortran90_PFLAGS}")
  # link to the helper libraries
  target_link_libraries(${bin} project_lib_shared ${external_libs})
endforeach(bin)

ENABLE_TESTING()
foreach(bin ${tests_list})
  string(REPLACE ".f90" "" bin ${bin})

  #add_test(NAME ${bin} COMMAND ${bin})

  add_test(NAME ${bin}2
    COMMAND ${CMAKE_COMMAND}
    -DTEST_PROG=$<TARGET_FILE:${bin}>
    -DOUTPUT_FILE=${CMAKE_CURRENT_BINARY_DIR}/test/${bin}_output.txt
    -DEXPECTED_FILE=${PROJECT_SOURCE_DIR}/test/${bin}_expected.txt
    # -DSOURCEDIR=${PROJECT_SOURCE_DIR}/test # @todo this varaible should be set for given bin
    -P ${CMAKE_CURRENT_SOURCE_DIR}/config/runtest.cmake)

endforeach(bin)

include(${CMAKE_CURRENT_SOURCE_DIR}/config/doxygen.cmake)