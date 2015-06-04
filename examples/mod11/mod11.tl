local mod11 = {}

interface mod11
  verify:{number|string}
  allowed:{string:string}
  new:(self, string, string?) -> (self)
  getverify:(self) -> (string)
  getallowed:(self) -> (string)
  calc:(self, string) -> (string, string)
  split:(self, string) -> (string, string)
  check:(self, string) -> (boolean, string?)?
  foreach:(self, string, number?, number?) -> (() -> (string, number, number) | (nil, nil, nil))
end

local clean = function(str:string):string
  str = tostring(str)
  local r = ""
  for w in string.gmatch(str, "%d") do
    r = r .. w
  end
  return r
end

local makeset = function(str:string, add_to:{string:string}?):{string:string}
  assert(type(str)=="string", "Expected string, got ".. type(str))
  local r:{string:string} = add_to or {}
  assert(type(r)=="table", "Expected tabe, got ".. type(add_to))
  for n = 1, #str do
    r[str:sub(n,n)] = str:sub(n,n)
  end
  return r
end

local checkverify = function(v:string):({number|string}, nil)?
  if not (type(v)=="string") then
    return nil, "Expected string, got "..type(v)
  end
  if not (#v == 8) then
    return nil, "Expected string with length 8, not " .. tostring(#v)
  end
  if v:find("%D") then
    return nil, "Bad value, only numbers 2 to 8 in random order, without doubles allowed"
  end

  local t:{number} = {1,2,3,4,5,6,7,8,9}
  local r:{number|string} = {}
  t[1]=nil
  for w in string.gmatch(v, "%d") do
    local n = tonumber(w)
    if n then
      if not t[n] then
        return nil, "Bad value "..w.." in "..v..". Only numbers 2 to 8 in random order, without doubles allowed"
      else
        t[n] = nil
        table.insert(r, n)
      end
    end
  end
  return r
end

local checkvalid = function(str:string, allowed:{string:string}):(string, nil)?
  if not (type(str)=="string") then
    return nil, "Expected string, got ".. type(str)
  end
  for n = 1, #str do
    if not allowed[str:sub(n,n)] then
      return nil, "Character '"..str:sub(n,n).."' is not an allowed character"
    end
  end
  return str
end

local ct:{number} = { [10] = 1, [11] = 0 }  -- 10 and 11 are special cases

local getmodulo = function(str:string, verify:{number|string}):string
  local total = 0
  local multiplpos = 1
  local result = ""
  for n = #str,1,-1 do    -- calculate right to left
    -- calculate value and add to total
    total = total + (tonumber(str:sub(n,n)) or 0) * (tonumber(verify[multiplpos]) or 0)
    -- Update multiplier position for next run
    multiplpos = multiplpos + 1     -- take next position
    if (multiplpos > #verify) or (n == 1) then
      -- reached end of series
      -- now calculate verification from total
      total = 11 - math.fmod(total,11)
      total = ct[total] or total   -- 10 and 11 are special cases
      -- set returned value (convert to string)
      result = tostring(total) .. result
      -- Reset position
      multiplpos = 1
      total = 0
    end
  end
  return result
end

local init_verify:{number|string} = {}
mod11.verify = init_verify
local init_allowed:{string:string} = {}
mod11.allowed = init_allowed

function mod11:new (v:string, a:string?):self
  local o = setmetatable({}, { __index = self })
  local verify, err = checkverify(v)
  if verify then
    if type(a) ~= "string" then error("Expected string, got "..type(a), 2) end
    local allowed = makeset(a or "")
    allowed = makeset("0123456789", allowed)
    o.verify = verify
    o.allowed = allowed
  else
    error(err, 2)
  end
  return o
end

function mod11:getverify ():string
  return table.concat(self.verify)
end

function mod11:getallowed ():string
  local r = ""
  for k in pairs(self.allowed) do
    r = r .. tostring(k)
  end
  return r
end

function mod11:calc (inp:string):(string, string)
  local ok, err = checkvalid(inp, self.allowed)
  if not ok then
    error(err, 2)
  end
  local cinp = clean(inp)
  if #cinp == 0 then error("Invalid input, empty string (no digits)", 2) end
  local chk = getmodulo(cinp, self.verify)
  return inp..chk, chk
end

function mod11:split (inp:string):(string, string)
  local ok, err = checkvalid(inp, self.allowed)
  if not ok then
    error(err, 2)
  end
  inp = clean(inp)
  if (math.fmod(#inp,9) == 1) or (#inp == 0) then
    error("The input has an invalid length",2)
  end
  local chk = ""
  while #inp - #chk * 8 >= 0 do
    chk = inp:sub(-1,-1) .. chk
    inp = inp:sub(1,-2)
  end
  return inp, chk
end

function mod11:check (inp:string):(boolean, string?)?
  local ok, err = checkvalid(inp, self.allowed)
  if not ok then
    return nil, err
  end
  inp = clean(inp)
  if (math.fmod(#inp,9) == 1) or (#inp == 0) then
    return nil, "The input has an invalid length"
  end
  local chk = ""
  inp, chk = self:split(inp)
  local c = getmodulo(inp, self.verify)
  if c == chk then
    return true
  else
    return false, "Check nr for "..inp.." is not "..chk.." but should have been "..c
  end
end

function mod11:foreach (text:string, minsize:number?, maxsize:number?):
(() -> (string, number, number) | (nil, nil, nil))
  local spos = 1
  local epos = 0
  local size = 0
  minsize = minsize or 2
  assert(type(minsize) == "number", "Expected number got "..type(minsize))
  assert(minsize >= 2, "Minimum size cannot be less than 2. Got "..tostring(minsize))
  maxsize = maxsize or #text
  assert(type(maxsize) == "number", "Expected number got "..type(maxsize))
  assert(maxsize >= minsize, "Maximum size cannot be less than minimum size")
  return function ()
    while true do
      local p = text:find("%d", spos)
      if not p then return nil else spos = p end
      epos = spos
      size = 0
      while true do
        local c = text:sub(epos, epos)
        if self.allowed[c] then
          if tonumber(c) then
            size = size + 1
          end
          epos = epos + 1
        end
        if (not self.allowed[c]) or (epos > #text) then
          if size < minsize or size > maxsize then
            local p = text:find("%D", spos)
            if not p then return nil else spos = p end
            break
          end
          local inp = text:sub(spos, epos-1)
          local success, ok = pcall(self.check, self, inp)
          if success and ok then
            local s, e = spos, epos-1
            spos = epos
            return inp, s, e
          end
          local p = text:find("%D", spos)
          if not p then return nil else spos = p end
          break
        end
      end
    end
  end
end

return mod11
