local x: number|string|nil = 10

if math.random() > 10 then
  print(x+10) -- error, x number|string|nil
elseif math.random() > 10 then
  error("is a number")
elseif type(x) == "number" then
    error("is a number")
end

x = x + 10 -- x can be a number here

local x: number|string|nil = 10

if not x then
  print(x+10) -- error, x is nil
elseif type(x) == "number" then
  error("is a number")
end

x = x + 10 -- x cannot be a number here
