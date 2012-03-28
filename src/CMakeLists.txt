# CMake config file for all sources in src/ subdir
include_directories(.)

# Search for the source files recursively and add them to src_sources
file(GLOB_RECURSE src_sources77 "[^.]*.f")
file(GLOB_RECURSE src_sources90 "[^.]*.f90")


# set the additional compile flags
set_source_files_properties(${src_sources77} PROPERTIES COMPILE_FLAGS
  "${Fortran77_FLAGS} ${Fortran77_DFLAGS} ${Fortran77_OFLAGS} ${Fortran77_PFLAGS}")

set_source_files_properties(${src_sources90} PROPERTIES COMPILE_FLAGS
  "${Fortran90_FLAGS} ${Fortran90_DFLAGS} ${Fortran90_OFLAGS} ${Fortran90_PFLAGS}")

# set objects for static library
add_library(project_lib_shared STATIC ${src_sources77} ${src_sources90})
set_target_properties(project_lib_shared PROPERTIES OUTPUT_NAME ${project_lib_name} )

# build shared library out of static one
# @todo check linking with -ipo -fast etc.
add_custom_command(TARGET project_lib_shared
  POST_BUILD
  COMMAND ${CMAKE_AR} -x lib${project_lib_name}.a
  COMMAND ${CMAKE_LINKER} -shared *.o -o lib${project_lib_name}.so
  COMMAND rm *.o
  WORKING_DIRECTORY ${LIBRARY_OUTPUT_PATH})
