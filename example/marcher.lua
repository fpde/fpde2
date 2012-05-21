m = {
   type	="ode_marcher_simple",
   name	="Simple Marcher",
   dim	= 38,
   step_control = {
      type	= "standard",
      abs_err	= 1.0e-10,
      rel_err   = 1.0e-12,
      a_y	= 1.0
   }
}
