program icicles_register_create_icicles
  use constants_module
  use icicles_module
  use icicles_register_module

  type(icicles_register) :: reg
  type(icicles), pointer :: ic => null()
  integer :: err

  allocate(ic)

  call reg%create_icicles(ic, error = err)

  if( err /= FPDE_STATUS_ERROR ) then
     deallocate(ic)
     stop 1
  end if

  deallocate(ic)

end program icicles_register_create_icicles

