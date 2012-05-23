# Testing configuration
add_custom_target(build_tests)

enable_testing()

# Search for the source files recursively and add them to src_sources
file(GLOB_RECURSE tests_list RELATIVE
  ${PROJECT_SOURCE_DIR}/test/
  ${PROJECT_SOURCE_DIR}/test/*.f90)

message("")
message(STATUS "Tests to build:")
message(STATUS "${tests_list}\n")

foreach(bin ${tests_list})
  string(REPLACE ".f90" "" bin ${bin})

  # select test type depending on suffix of the file
  # _RUN - simple execution check
  # _SEG - checks if program ended in segmentation fault
  # _OUT - output compare test

  get_filename_component(bin_name ${bin} NAME)

  if( bin_name MATCHES "[^.]*_RUN$" )
    # simple execution check
    add_test(NAME ${bin} COMMAND test/${bin})

  elseif( bin_name MATCHES "[^.]*_SEG$" )
    # checks if program ended in segmentation fault
    add_test(NAME ${bin} COMMAND test/${bin})
    set_property(TEST ${bin} PROPERTY WILL_FAIL TRUE)

  elseif( bin_name MATCHES "[^.]*_OUT$" )
    # output compare test
    add_test(NAME ${bin}
      COMMAND ${CMAKE_COMMAND}
      -DTEST_PROG=test/${bin}
      -DOUTPUT_FILE=${CMAKE_CURRENT_BINARY_DIR}/test/${bin}.out
      -DEXPECTED_FILE=${PROJECT_SOURCE_DIR}/test/${bin}.txt
      -P ${CMAKE_CURRENT_SOURCE_DIR}/test/CompareTest.cmake
      )
  endif( )

  set_target_properties(${bin}
    PROPERTIES
    EXCLUDE_FROM_ALL "TRUE")

  # set_tests_properties(${bin}
  #   PROPERTIES
  #   DEPENDS
  #   tests)

  add_dependencies(build_tests ${bin})

endforeach(bin)

add_custom_target(check COMMAND ${CMAKE_CTEST_COMMAND}
  DEPENDS build_tests)
