module flu_meta_module

  use flu_primitives_module
  use iso_c_binding

contains

  subroutine lua_add_to_metatable(l,meta_name,fun_name,fun_p)
    use iso_c_binding, only: c_ptr, c_int, c_funptr
    type(c_ptr) :: l
    integer :: n
    character(len=*) :: fun_name, meta_name
    ! we declare fun as c_funptr because using f_c_funptr does not
    ! commute with passing a pointer to function (at least in ifort)
    type(c_funptr) :: fun
    procedure(lua_cfunction) :: fun_p

    fun = c_funloc(fun_p)

    ! @todo check if object at index idx is userdata

    n = luaL_newmetatable(l,trim(meta_name)//c_null_char)
    call lua_pushcclosure(l,fun,0)
    call lua_setfield(l,-2,trim(fun_name)//c_null_char)
    call lua_pop(l,1)
  end subroutine lua_add_to_metatable

end module flu_meta_module
