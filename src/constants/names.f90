module names_module

  use helper_module

  private

  public boundary_param_name

contains

  function boundary_param_name(x, side, pnum, pname) result(r)
    integer, intent(in) :: x, side, pnum
    character(len=*), intent(in) :: pname

    character(len=:), allocatable :: r

    r = merge(pname, "p"//itoa(pnum), pname == "")

    r = "B(&
         &x="//itoa(x)//",&
         &side="//itoa(side)//",&
         &pnum="//itoa(pnum)//",&
         &name="//r//")"

  end function boundary_param_name


end module names_module
