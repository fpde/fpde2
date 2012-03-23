module flu_meta_module

  use flu_module
  use iso_c_binding

contains

  subroutine lua_add_to_metatable(l,meta_name,fun_name,fun)
    use iso_c_binding, only: c_ptr, c_int, c_funptr
    type(flu) :: l
    logical :: n
    character(len=*) :: fun_name, meta_name
    ! we declare fun as c_funptr because using f_c_funptr does not
    ! commute with passing a pointer to function (at least in ifort)
    procedure(c_lua_cfunction) :: fun

    ! @todo check if object at index idx is userdata

    n = luaL_newmetatable(l, trim(meta_name))
    call lua_pushcclosure(l, fun, 0)
    call lua_setfield(l, -2, trim(fun_name))
    call lua_pop(l, 1)
  end subroutine lua_add_to_metatable

end module flu_meta_module
