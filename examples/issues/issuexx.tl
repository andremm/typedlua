
do
  local x: number | string | nil = 10
  local y: number | string = 20

  if type(x) == "number" and type(y) ~= "number" then
    print(x + 10) -- ok
    print(x + y)  -- error, y: string
    print(x .. "foo") -- error, x: number
    print(y .. "bar") -- ok
  elseif x then
    print(x + 10) -- error, x: number|string
    if type(x) == "string" then
      print(x .. "foo") -- ok
    end
    print(x .. "foo") -- error, x: number|string
  else
    print(x .. "foo") -- error, x: nil
    print(y + 10) -- error, y: number|string
  end

  x = x + 10 -- error, x: number|string|nil
end

do
  local x: number | string | nil = 10
  local y: number | string = 20

  local function f()
    x = 10
  end

  -- the previous function makes x unfilterable

  if type(x) == "number" and type(y) ~= "number" then
    print(x + 10)
    print(x + y)  -- error, y: string
    print(x .. "foo")
    print(y .. "bar")
  elseif x then
    print(x + 10)
    if type(x) == "string" then
      print(x .. "foo")
    end
    print(x .. "foo")
  else
    print(x .. "foo")
    print(y + 10) -- error, y: number|string
  end

  x = x + 10 -- error, x: number|string|nil
end

do
  local x: number | string | nil = 10
  local y: number | string = 20

  if type(x) == "number" and type(y) ~= "number" then
    print(x + 10) -- ok
    print(x + y)  -- error, y: string
    local function f()
      print(x + 10) -- error because x: number|string|nil here, but does not change x
    end
    print(x .. "foo") -- error, x: number
    print(y .. "bar") -- ok
  elseif x then
    print(x + 10) -- error, x: number|string
    if type(x) == "string" then
      print(x .. "foo") -- ok
    end
    print(x .. "foo") -- error, x: number|string
  else
    print(x .. "foo") -- error, x: nil
    print(y + 10) -- error, y: number|string
  end

  x = x + 10 -- error, x: number|string|nil
end

do
  local x: number | string | nil = 10
  local y: number | string = 20

  if type(x) == "number" and type(y) ~= "number" then
    print(x + 10) -- ok
    print(x + y)  -- error, y: string
    local function f()
      x = 10 -- reverts x to original type, from now x unfilterable
    end
    print(x .. "foo") -- error, x: number
    print(y .. "bar") -- ok
  elseif x then
    print(x + 10) -- error, x: number|string
    if type(x) == "string" then
      print(x .. "foo") -- ok
    end
    print(x .. "foo") -- error, x: number|string
  else
    print(x .. "foo") -- error, x: nil
    print(y + 10) -- error, y: number|string
  end

  x = x + 10 -- error, x: number|string|nil
end

do
  local x: number | string | nil = 10
  local y: number | string = 20

  while type(x) == "number" and type(y) ~= "number" do
    print(x + 10) -- ok
    print(x + y)  -- error, y: string
    print(x .. "foo") -- error, x: number
    print(y .. "bar") -- ok
  end

  x = x + 10 -- error, x: number|string|nil
end

do
  local x: number | string | nil = 10
  local y: number | string = 20

  while type(x) == "number" do
    print(x + 10) -- ok
    if type(y) == "string" then -- y unfilterable inside the loop
      print(x + y)  -- error, y: number|string
      print(x .. "foo") -- error, x: number
      print(y .. "bar") -- error, y: number|string
    end
  end

  x = x + 10 -- error, x: number|string|nil
end

do
  local x: number | string | nil = 10
  local y: number | string = 20

  while type(x) == "number" and type(y) ~= "number" do
    print(x + 10) -- ok
    print(x + y)  -- error, y: string
    print(x .. "foo") -- error, x: number
    print(y .. "bar") -- ok
    x = "foo" -- x reverts to number|string|nil
    print(x + 10) -- error, x:number|string|nil
  end

  x = x + 10 -- error, x: number|string|nil
end

do
  local x: number | string | nil = 10
  local y: number | string = 20

  if x then
    while type(x) == "number" and type(y) ~= "number" do
      print(x + 10) -- ok
      print(x + y)  -- error, y: string
      print(x .. "foo") -- error, x: number
      print(y .. "bar") -- ok
      x = "foo" -- x reverts to number|string
      print(x + 10) -- error, x:number|string
    end
  end
  x = x + 10 -- error, x: number|string|nil
end
