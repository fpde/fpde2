# CMake config file for all sources in ${PROJECT_SOURCE_DIR}/example/ subdir
add_custom_target(build_examples)

include_directories(.)

# Search for the source files recursively and add them to examples_list
file(GLOB_RECURSE examples_list RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "[^.]*.f90")

# Build configuration

# Create targets for each file file in the examples_list
foreach(bin ${examples_list})
  # cut the extension from the filename
  string(REPLACE ".f90" "" bin ${bin})
  add_executable(${bin} ${bin}.f90)
  # set the target binary file location
  set_target_properties(${bin}
    PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY
    ${CMAKE_CURRENT_BINARY_DIR})

  get_filename_component(bin_path ${bin} PATH)

  if( bin_path STRGREATER "" )
    file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${bin_path})
  endif( )
  # set the additional compile flags
  set_target_properties(${bin}
    PROPERTIES
    COMPILE_FLAGS
    "${Fortran90_FLAGS}")
  # link to the helper libraries
  target_link_libraries(${bin} project_lib_shared ${Fortran90_LFLAGS})

  add_dependencies(build_examples ${bin})

endforeach(bin)
