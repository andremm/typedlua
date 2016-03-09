interface ELEM
   x: integer
end

interface OBJ
   parent: OBJ?
   atr: ELEM -- if you comment this line or change ELEM to a base type like string, it works
end

local function f(o:OBJ)
   local parent = o.parent
   if parent then
      f(parent)
   end
end
