function x() : (integer, boolean)|(string, integer)
   if 0 > 1 then
      return 1, true
   else
      return "wat", 9
   end
end

local baz, foo, bar = 10, x()
if foo then
   local bla = foo .. " world" -- error, foo is i|s
end
baz = baz + 10
