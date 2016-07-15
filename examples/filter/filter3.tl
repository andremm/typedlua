local x:number?
local y:string?

if type(x) == "number" then
  x = x + 1
elseif type(y) == "string" then
  y = y .. "hello"
else
  x = x + 1
  y = y + 1
end

x = x + 1
y = y .. "hello"
