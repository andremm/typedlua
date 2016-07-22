local bug47 = require("bug47")
local function main()
   local maybe_u, rerr = bug47.foo()
   print(maybe_u + 10) -- maybe_u is U?
   if type(maybe_u) == "nil" then
      print(maybe_u + 10) -- maybe_u is nil
      print(rerr .. "foo")
      if type(rerr) == "string" then
         error("error - "..rerr, nil)
      end
      return -- unreacheable
   end
   -- maybe_u is a U here
   local u = maybe_u
   print(u + 10)
end
main()
