local function idiv (d1:number, d2:number):(number, number) | (nil, string)
  if d2 == 0 then
    return nil, "division by zero"
  else
    local r = d1 % d2
    local q = (d1 - r) / d2
    return q, r
  end
end

local n1, n2 = 4, 4
local q, r = idiv(n1, n2)
local x:number, msg:string = 0, ""
if q then
  x = q + r
  print(r .. "foo") -- error, r number
else
  msg = r
  print(r + 10) -- error, r string
end
