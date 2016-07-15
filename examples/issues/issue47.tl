local bug47 = require("bug47")
local function main()
   local maybe_u, rerr = bug47.foo()
   if type(maybe_u) == "nil" then
      print(maybe_u + 10)
      print(rerr .. "foo")
      if type(rerr) == "string" then
         error("error - "..rerr, nil)
      end
      return
   end
   local u = maybe_u
   print(u + 10)
end
main()
