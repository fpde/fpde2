!>
!! @file   from_object.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Fri Jul 20 14:00:09 2012
!!
!! @brief
!!
!!
!!

module class_generic_function_from_object_interfaces

  abstract interface
     subroutine library_function_interface(s)
       use, intrinsic :: iso_c_binding
       type(c_ptr), value :: s
       ! real(c_double), value :: s
       ! integer(c_int) :: s
     end subroutine library_function_interface
  end interface

end module class_generic_function_from_object_interfaces

module class_generic_function_from_object

  use flu_module
  use flu_get_module
  use class_generic_function
  use iso_c_binding
  use iso_c_utilities
  use class_generic_function_from_object_interfaces

  private

  character(len=NAME_LEN), parameter :: &
       TAG_FUNCNAME = "func",&
       TAG_LIBNAME  = "lib"
  integer(c_int), parameter :: &
       RTLD_LAZY = 1,&
       RTLD_NOW = 2,&
       RTLD_GLOBAL = 256,&
       RTLD_LOCAL = 0

  type, public, extends(generic_function) :: generic_function_from_object
     ! private
     type(c_ptr) :: handle = c_null_ptr
     procedure(library_function_interface), pointer, nopass :: fun => null()
   contains
     procedure :: call
     procedure :: from_lua
     procedure :: free
  end type generic_function_from_object



  interface

     function dlopen( file, mode ) bind(C, name="dlopen")
       use iso_c_binding
       character(kind=c_char) :: file
       integer(c_int), value :: mode
       type(c_ptr) :: dlopen
     end function dlopen

     function dlsym( handle, name ) bind(C, name="dlsym")
       use iso_c_binding
       type(c_ptr), value :: handle
       character(kind=c_char) :: name
       type(c_funptr) :: dlsym
     end function dlsym

     function dlerror() bind(c,name="dlerror")
       use iso_c_binding
       type(c_ptr) :: dlerror
     end function dlerror

     subroutine dlclose( handle ) bind(c, name="dlclose")
       use iso_c_binding
       type(c_ptr), value :: handle
     end subroutine dlclose

  end interface

contains

  subroutine call(this, solver, error)
    class(generic_function_from_object) :: this
    class(*), pointer :: solver
    integer, optional, intent(out) :: error

    if(present(error)) error = FPDE_STATUS_OK

    if( associated(this%fun) ) then
       call this % fun(c_loc(solver))
    else
       call this%log(FPDE_LOG_ERROR,&
       "Call error: function pointer is null")
       if(present(error)) error = FPDE_STATUS_ERROR
    end if

  end subroutine call

  subroutine from_lua(p, l, error)
    class(generic_function_from_object) :: p
    type(flu) :: l
    integer, optional, intent(out) :: error

    character(len=NAME_LEN) :: libname, funcname
    type(c_funptr) :: cfun
    integer :: err

    call p%platonic%from_lua(l)
    if( p%name == "" ) p%name = "generic_function"
    p%type = "generic_function_from_object_file"

    call flu_get_atomic(l,&
         char  = libname,&
         key   = TAG_LIBNAME,&
         error = err )

    if( err /= FPDE_STATUS_OK ) then
       call p%log(FPDE_LOG_ERROR,&
       "Invalid or missing object file name")
       return
    end if

    call flu_get_atomic(l,&
         char = funcname,&
         key  = TAG_FUNCNAME,&
         error = err )

    if( err /= FPDE_STATUS_OK ) then
       call p%log(FPDE_LOG_ERROR,&
       "Invalid or missing symbol name")
       return
    end if

    p%handle = dlopen(trim(libname)//c_null_char, RTLD_NOW)

    if(.not. c_associated(p%handle)) then
       call p%log(FPDE_LOG_ERROR,&
       "Couldn't open library ["//trim(libname)//"]")
       call p%log(FPDE_LOG_ERROR,&
       "dlerror: "//c_f_string(dlerror()))
       if(present(error)) error = FPDE_STATUS_ERROR
       return
    end if

    cfun = dlsym(p%handle, trim(funcname)//c_null_char)
    if(.not. c_associated(cfun)) then
       call p%log(FPDE_LOG_ERROR,&
       "Couldn't open function ["//trim(funcname)//"]")
       call p%log(FPDE_LOG_ERROR,&
       "dlerror: "//c_f_string(dlerror()))
       if(present(error)) error = FPDE_STATUS_ERROR
       return
    end if

    call c_f_procpointer(cfun, p % fun)

    if(present(error)) error = FPDE_STATUS_OK

  end subroutine from_lua

  subroutine free(p, error)
    class(generic_function_from_object) :: p
    integer, optional, intent(out) :: error

    if(c_associated(p%handle)) then
       call dlclose(p%handle)
    end if

    if(present(error)) error = FPDE_STATUS_OK
  end subroutine free

end module class_generic_function_from_object
