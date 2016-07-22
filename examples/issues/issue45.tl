-- bug45driver.tl
local bug45 = require("bug45")

local r = bug45.makeR()
local s, err = bug45.makeS()
if r then
  if s then
    s = r:makeT(s) -- crashes setting s, or a new global. Doesn't crash if using `local`
  end
end

print(nil + 5) -- just to supress generation of .lua
