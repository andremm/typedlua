
-- The following two mutually recursive types are the same structurally
interface Tree1
  label: integer
  left: Tree2?
  right: Tree2?
end

interface Tree2
  label: integer
  left: Tree1?
  right: Tree1?
end

-- the following type has a forward reference
interface OBJ
   parent: OBJ?
   atr: ELEM
end

interface ELEM
   x: integer
end

local function f(o:OBJ)
   local t1: Tree1 = { label = 2 }
   local t2: Tree2 = { label = 3 }
   t1 = t2
   t2 = t1
   local parent = o.parent
   if parent then
      f({atr={x=2}}) -- isn't this structurally equivalent to OBJ?
      print(parent + 10) -- just to supress generation of .lua file
   end
end
