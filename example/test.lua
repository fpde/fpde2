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
   name = "one",
   nx = nx,
   x0 = x0,
   x1 = x1,
   x = x,
   f = f,
   nx = nx,
   evolved = {"u", "v"}
   -- nf = #evolved		-- number of evolved functions
}
