-- import math functions to global context
for k,v in pairs(math) do
   _G[k]=v
end

nx = 10
x0 = 0
x1 = pi

x = {}
f = {}
for i=1, nx do
   xx = x0+(x1-x0)*(i-1)/(nx-1)
   x[i] = xx
   f[i] = sin(xx)
end

solver = {
   type = "elliptic",
   name = "one",
   nx = nx,
   x0 = x0,
   x1 = x1,
   x = x,
   abc = {y = {z = {1,2}}},
   f = f,
   nx = nx,
   scalars = {t = 1., "dt", "mod1"}
   vectors = {"u", "v"}
   modules = {
      {
	 type="box",
	 name="box",
	 param1 = "1",
	 u = "u",
	 v = "v"
	 s = "mod1"
      },
      {
	 type="box",
	 name="box",
	 param1 = "1",
	 var = "mod1"
      },
      {
	 type="write",
	 name="write1",
	 draw = {"t","u"}
      }
   }
}



solver.nf = #solver.evolved
print(solver.nf)
