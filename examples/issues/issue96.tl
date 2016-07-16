local function double_number(x: number?)
  if not x then
    error("Not giving p1 to double_number is deprecated")
  end
  return 2 * x -- x is filtered to number
end
