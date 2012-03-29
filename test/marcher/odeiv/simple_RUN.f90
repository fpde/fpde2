program simple

   use class_odeiv_aren

   type(odeiv_aren) :: aren

   call aren%init()

   call aren%info()

   call aren%free()

end program simple
