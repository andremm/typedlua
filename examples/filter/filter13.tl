local function f(x: number|string|nil)

if math.random() > 0.5 then
  print(x+10) -- error, x number|string|nil
elseif math.random() > 0.5 then
  error("random")
elseif type(x) == "number" then
  error("is a number")
end

x = x + 10 -- error, x n|s|nil (math.random > 0.5)

if not x then
  print(x+10) -- error, x is nil
elseif type(x) == "number" then
  error("is a number")
end

-- x cannot be number here

x = x + 10 -- error, x string?
end
