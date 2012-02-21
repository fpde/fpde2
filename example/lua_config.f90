program lua_config_test
  use iso_c_binding
  use flu_module

  type(flu) :: l
  real :: x1 = 1
  real, pointer :: x(:), f(:)
  integer :: i,n
  character(len=100) :: str = "", id = ""
  character(len=10) :: key(2) = ""


  call l%init()

  call l%load_file("../example/test.lua")

  key(1) = "solver"

  key(2) = "x1"
  id = l%array_to_key(key)
  if( l%get(key, x1) == 0 ) then
     print *, trim(id)//" = ", x1
  else
     print *, "unable to read real ", trim(id)
  end if

  key(2) = "name"
  id = l%array_to_key(key)
  if( l%get(key, str) == 0 ) then
     print *, trim(id)//" = ", trim(str)
  else
     print *, "unable to read string ", trim(id)
  end if

  key(2) = "x"
  id = l%array_to_key(key)
  if( l%get_len(key, n) == 0 ) then
     allocate(x(n))
     if( l%get(key, x) /= 0 ) then
        print *, "unable to read array ", trim(id)
     end if
  else
     print *, "unable to read length of an array ", trim(id)
  end if

  key(2) = "f"
  id = l%array_to_key(key)
  if( l%get_len(key, n) == 0 ) then
     allocate(f(n))
     if( l%get(key, f) /= 0 ) then
        print *, "unable to read array ", trim(id)
     end if
  else
     print *, "unable to read length of an array ", trim(id)
  end if

  do i = 1, n
     print *, x(i), f(i)
  end do


  call l%free()

end program lua_config_test


