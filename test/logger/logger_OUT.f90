program logger_test

   use logger_module

   type(named) :: o
   integer :: i
   integer :: ll(4) = [ FPDE_LOG_ERROR, FPDE_LOG_WARNING, &
        &               FPDE_LOG_INFO, FPDE_LOG_DEBUG ]

   logger%path="./"
   o%logfile_unit=FPDE_STDOUT
   print '(*(g0))', "Logger level: ", logger%log_level
   print '(*(g0))', "Logger path: ", "'"//trim(logger%path)//"'"
   print '(*(g0))', "Object logfile unit: ", o%logfile_unit

   do i=1,4
      print '(*(g0))',""
      call set_log_level(ll(i))
      print '(*(g0))', "Calling logs with logger level: ", logger%log_level
      call call_logs(o)
   end do

contains

   subroutine call_logs(o)
      type(named) :: o

      call o%log(FPDE_LOG_ERROR,"this is a fancy error log message")
      call o%log(FPDE_LOG_WARNING,"this is a fancy warning message")
      call o%log(FPDE_LOG_INFO,"this is a fancy info message")
      call o%log(FPDE_LOG_DEBUG,"this is a fancy debug message")

   end subroutine call_logs

end program logger_test
