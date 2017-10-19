local typealias T = {"i":integer, "s":string, "f":(integer) -> (integer)}

local function get_s (t:T):string
  t.y = 20.0 -- not ok
  return t.s
end

local open = { i = 1 }
open.s = "foo"
open.f = function (x:integer):integer return x + 1 end
-- open has type T

local closed1 = open -- closed1 has type T
closed1.x = 10.0 -- not ok
open.x = 10.0 -- ok
closed1.s = closed1.s:reverse()
print(string.format("%q", get_s(open)))

local closed2:T = open
closed2.y = 20.0 -- not ok
open.y = 20.0 -- ok
