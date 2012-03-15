# CMake config file for all sources in test/ subdir
include_directories(.)

# Search for the source files recursively and add them to src_sources
file(GLOB_RECURSE tests_list RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "[^.]*.f90")

# @todo remove
# message("")
# message(STATUS "Tests to build:")
# message(STATUS "${tests_list}\n")

# Build configuration

# create targets for each file from ./test/
foreach(bin ${tests_list})
  string(REPLACE ".f90" "" bin ${bin})
  add_executable(${bin} ${bin}.f90)
  # set the target binary file location
  set_target_properties(${bin}
    PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY
    ${CMAKE_CURRENT_BINARY_DIR})
  
  get_filename_component(bin_path ${bin}.f90 PATH)

  if( bin_path STRGREATER "" )
    #MESSAGE( "nie pusty: ${CMAKE_CURRENT_BINARY_DIR}/tests/${ttt}" )
    file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${bin_path})
  endif( )

  # set the additional compile flags
  set_target_properties(${bin}
    PROPERTIES
    COMPILE_FLAGS
    "${Fortran90_FLAGS} ${Fortran90_DFLAGS} ${Fortran90_OFLAGS} ${Fortran90_PFLAGS}")
  # link to the helper libraries
  target_link_libraries(${bin} project_lib_shared ${external_libs})
endforeach(bin)