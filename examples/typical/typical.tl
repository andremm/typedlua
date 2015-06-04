local typical = {}

local function metatype (value:value):string?
  local value_mt:{ "__type":string|nil|(value) -> (string?) }? = getmetatable(value)
  if value_mt then
    local metatype = value_mt.__type
    if type(metatype) == "nil" then
      return nil
    elseif type(metatype) == "string" then
      return metatype
    else
      return metatype(value)
    end
  end
end

local resolvers:{(value) -> (string?)} = {
  metatype,
  io.type,
}

function typical.type (value:value):string
  for i = 1, #resolvers do
    local resolver = resolvers[i]
    if resolver then
      local type = resolver(value)
      if type then
        return type
      end
    end
  end
  return type(value)
end

return typical
