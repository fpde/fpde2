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


  interface generic_function_from_object
     module procedure :: gffo_constructor
  end interface generic_function_from_object


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

  function gffo_constructor(lib, func, error) result(gffo)
    character(len=*), intent(in) :: lib, func
    integer, optional, intent(out) :: error

    type(generic_function_from_object), pointer :: gffo

    type(c_funptr) :: cfun

    allocate(gffo)

    if(present(error)) error = FPDE_STATUS_ERROR

    gffo%name = "generic_function"
    gffo%type = "generic_function_from_object_file"

    gffo%handle = dlopen(trim(lib)//c_null_char, RTLD_NOW)

    if(.not. c_associated(gffo%handle)) then
       call gffo%log(FPDE_LOG_ERROR,&
       "Couldn't open library ["//trim(lib)//"]")
       call gffo%log(FPDE_LOG_ERROR,&
       "dlerror: "//c_f_string(dlerror()))
       return
    end if

    cfun = dlsym(gffo%handle, trim(func)//c_null_char)

    if(.not. c_associated(cfun)) then
       call gffo%log(FPDE_LOG_ERROR,&
       "Couldn't open function ["//trim(func)//"]")
       call gffo%log(FPDE_LOG_ERROR,&
       "dlerror: "//c_f_string(dlerror()))
       return
    end if

    if(present(error)) error = FPDE_STATUS_OK

    call c_f_procpointer(cfun, gffo%fun)

  end function gffo_constructor


  subroutine call(this, solver, error)
    class(generic_function_from_object) :: this
    class(*) :: solver
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
    type(generic_function_from_object) :: gffo
    integer :: err

    if(present(error)) error = FPDE_STATUS_ERROR
    call p%platonic%from_lua(l)

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

    gffo = generic_function_from_object(&
         lib = libname, func = funcname)

    p%handle = gffo%handle
    p%fun => gffo%fun

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
