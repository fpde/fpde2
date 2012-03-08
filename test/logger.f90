program logger_test

   use logger_module

   type(named) :: o

   print *, "Current logger level: ", logger%log_level
   print *, "Current logger path: ", "'"//trim(logger%path)//"'"

   call set_log_level(FPDE_LOG_ERROR)
   print *, "Current logger level: ", logger%log_level

   ! call get_new_logfile_unit(o%logfile_unit,"log.log")
   ! print *, "Object logfile unit", o%logfile_unit

   logger%path="./"
   print *, "Current logger path: ", "'"//trim(logger%path)//"'"

   ! o%logfile_unit=0
   print *, "Object logfile unit", o%logfile_unit

   call call_logs(o)

contains

   subroutine call_logs(o)
      type(named) :: o

      call o%log(FPDE_LOG_ERROR,"this is fancy error log message")
      call o%log(FPDE_LOG_WARNING,"this is fancy warning message")
      call o%log(FPDE_LOG_INFO,"this is fancy info message")
      call o%log(FPDE_LOG_DEBUG,"this is fancy debug message")
   
   end subroutine call_logs

end program logger_test
