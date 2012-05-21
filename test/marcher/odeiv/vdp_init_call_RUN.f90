program vdp_init_call_test

   use constants_module
   use logger_module
   use class_odeiv_van_der_Pol

   type(odeiv_vdp) :: vdp
   real :: t
   real, pointer :: dydt(:), dfdt(:), dfdy(:,:)

   call set_log_level(FPDE_LOG_DEBUG)

   call vdp%init()

   call vdp%info()

   t = 0.0
   allocate( dydt(vdp%sys%dim) )
   allocate( dfdt(vdp%sys%dim) )
   allocate( dfdy(vdp%sys%dim,vdp%sys%dim) )

   print *, "y0: ",  vdp%y0

   call vdp%log(FPDE_LOG_INFO,"call vdp%sys%fun")
   call vdp%sys%fun(t,vdp%y0,dydt,vdp%sys%params)
   print *, "dydt: ",  dydt

   call vdp%log(FPDE_LOG_INFO,"call vdp%sys%jac")
   call vdp%sys%jac(t,vdp%y0,dfdy,dfdt,vdp%sys%params)
   print *, "dfdt: ",  dfdt
   print *, "dfdy(1,:): ",  dfdy(1,:)
   print *, "dfdy(2,:): ",  dfdy(2,:)

   deallocate(dydt)
   deallocate(dfdt)
   deallocate(dfdy)

   call vdp%free()

end program vdp_init_call_test
