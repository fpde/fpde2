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

# set objects for shared library
add_library(project_lib_shared SHARED ${src_sources77} ${src_sources90})
set_target_properties(project_lib_shared PROPERTIES OUTPUT_NAME ${project_lib_name} )

# @todo
# # static library can be generated directly from shared lib
# # object (what about ipo?)
# # set objects for static library
# add_library(fpde2 STATIC ${src_sources77} ${src_sources90})

# @todo if there is
# target_link_libraries(mystuff blas lapack)