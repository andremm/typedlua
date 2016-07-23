interface ELEM
   x: integer
end

interface OBJ
   parent: OBJ?
   atr: ELEM -- if I change ELEM to {"x":integer}, I no longer get an error
end

local function f(o:OBJ)
   local parent = o.parent
   if parent then
      f({atr={x=2}}) -- isn't this structurally equivalent to OBJ?
      print(parent + 10) -- just to force an error
   end
end
