#define logdebug
#include "/home/pawel/Sources/fpde2/src/logger/logger.h"
program logger_test

  use logger_module

  type(named) :: n1, n2

  call set_log_level(4)

  n1%name = "n1"
  n2%name = "n2"

  call log_newdir("abc/")

  call n1%log_newfile("abc.log")
  call n1%loge("To abc.log")

  call n2%logw("To stdout")
  call n2%log_newfile("abc.log")
  call n2%logw("To abc.log")

end program logger_test
