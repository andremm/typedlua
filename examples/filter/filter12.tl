local A = assert -- now works!

local function double_number(x: number?)
  if not x then
    error("Not giving p1 to double_number is deprecated")
  end
  return 2 * x -- x is filtered to number
end

local x: number? = 10
A(x)
x = x + 10
x = nil -- revert
x = x + 10

if math.type(x) == "integer" then
  print(x + 10)
end
