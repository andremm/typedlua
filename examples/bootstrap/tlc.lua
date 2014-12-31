local x
local y = x
if type(x) == "number" then
  x = x + 1
elseif type(y) == "string" then
  y = y .. "hello"
elseif type(x) == "string" then
  x = x .. "hello"
elseif type(y) == "number" then
  y = y + 1
end
x = y
y = x

