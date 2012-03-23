module c_helper_module

  use iso_c_binding

contains

  subroutine c_ptr_to_str(ptr, str, len)
    character(len=*), intent(out) :: str
    type(c_ptr) :: ptr
    integer :: len, i
    character(c_char), pointer :: c_str(:)

    call c_f_pointer(ptr, c_str, [len])

    write (str,*) (c_str(i), i=1,len)
    str=trim(adjustl(str))

  end subroutine c_ptr_to_str

end module c_helper_module


