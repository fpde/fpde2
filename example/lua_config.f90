program lua_config_test
  use iso_c_binding
  use flu_module

  type(flu) :: l
  real :: x = 1
  real, pointer :: tab(:)
  integer :: n
  character(len=100) :: str = "", id = ""
  character(len=5), target :: key1(3), key2(3), key3(1)
  character(len=5), pointer :: key(:)


  call l%init()

  call l%load_file("../test/test.lua")

  key1 = ["b", "c", "d"]
  key2 = ["p", "q", "r"]
  key3 = ["table"]

  key => key1
  id = l%array_to_key(key)
  if( l%get(key, x) == 0 ) then
     print *, trim(id)//"=", x
  else
     print *, "unable to read real ", l%array_to_key(key)
  end if

  key => key2
  id = l%array_to_key(key)
  if( l%get(key, str) == 0 ) then
     print *, trim(id)//"=", trim(str)
  else
     print *, "unable to read string ", l%array_to_key(key)
  end if

  key => key3
  id = l%array_to_key(key)
  if( l%get_len(key, n) == 0 ) then
     allocate(tab(n))
     if( l%get(key, tab) == 0 ) then
        print *, trim(id)//"=", tab
     else
        print *, "unable to read array ", l%array_to_key(key)
     end if
  else
     print *, "unable to read length of an array ", l%array_to_key(key)
  end if

  ! print *, l%array_to_key(["p", "q", "r"])

  call l%free()

end program lua_config_test


