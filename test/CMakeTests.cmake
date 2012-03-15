# Testing configuration
enable_testing()

# Search for the source files recursively and add them to src_sources
file(GLOB_RECURSE tests_list RELATIVE 
  ${PROJECT_SOURCE_DIR}/test/
  ${PROJECT_SOURCE_DIR}/test/*.f90)

message("")
message(STATUS "Tests to test:")
message(STATUS "${tests_list}\n")

foreach(bin ${tests_list})
  string(REPLACE ".f90" "" bin ${bin})

  # select test type depending on suffix of the file
  # _OLEK  - simple execution check
  # _BOLEK - output compare test

  get_filename_component(bin_name ${bin} NAME)
  
  if( bin_name MATCHES "[^.]*_OLEK$" )
    # simple execution check
    add_test(NAME ${bin} COMMAND test/${bin})

  elseif( bin_name MATCHES "[^.]*_BOLEK$" )
    # output compare test
    add_test(NAME ${bin}
      COMMAND ${CMAKE_COMMAND}
      -DTEST_PROG=${bin}
      -DOUTPUT_FILE=${CMAKE_CURRENT_BINARY_DIR}/${bin}.out
      -DEXPECTED_FILE=${PROJECT_SOURCE_DIR}/${bin}.txt
      -P ${CMAKE_CURRENT_SOURCE_DIR}/CompareTest.cmake
      )
  endif( )
endforeach(bin)
