# execute test program and catch returned signal/status in is_error
execute_process(COMMAND ${TEST_PROG}
  RESULT_VARIABLE is_error
  OUTPUT_FILE ${OUTPUT_FILE})

# if error ocured during the command run message this
if(is_error)
  message(FATAL_ERROR "Test failed")
endif()
# perform extended test - compare expected and generated
# output files
execute_process(COMMAND ${CMAKE_COMMAND} -E compare_files
  ${OUTPUT_FILE} ${EXPECTED_FILE}
  RESULT_VARIABLE DIFFERENT)

if(DIFFERENT)
    message(FATAL_ERROR "Test failed - files differ")
endif()
