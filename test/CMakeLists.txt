# CMake config file for all sources in ${PROJECT_SOURCE_DIR}/test/ subdir
include_directories(.)

# Search for the source files recursively and add them to tests_list
file(GLOB_RECURSE tests_list RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "[^.]*.f90")

# Build configuration

# Create targets for each file in the testslist
foreach(bin ${tests_list})
  # cut the extension from the filename
  string(REPLACE ".f90" "" bin ${bin})
  add_executable(${bin} ${bin}.f90)
  # set the target binary file location
  set_target_properties(${bin}
    PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY
    ${CMAKE_CURRENT_BINARY_DIR})

  get_filename_component(bin_path ${bin}.f90 PATH)

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
endforeach(bin)
