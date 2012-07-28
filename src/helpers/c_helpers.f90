module iso_c_utilities
  use constants_module
   use iso_c_binding ! intrinsic module

   character(c_char), dimension(1), save, target, private :: dummy_string="?"

 contains
  function c_f_string(cptr) result(str)

    ! convert a null-terminated c string into a fortran character array pointer
    type(c_ptr), intent(in) :: cptr ! the c address
    character(len=NAME_LEN) :: str

    integer :: i
    character(kind=c_char), dimension(:), pointer :: fptr

    interface ! strlen is a standard c function from <string.h>
       ! int strlen(char *string)
       function strlen(string) result(len) bind(c,name="strlen")
         use iso_c_binding
         type(c_ptr), value :: string ! a c pointer
         integer(c_int) :: len
       end function strlen
    end interface

    str = ""

    if(c_associated(cptr)) then
       call c_f_pointer(fptr=fptr, cptr=cptr, shape=[strlen(cptr)])
       do i = 1, strlen(cptr)
          str(i:i) = fptr(i)
       end do
    else
       ! to avoid segfaults, associate fptr with a dummy target:
       fptr=>dummy_string
    end if

  end function c_f_string

end module iso_c_utilities
