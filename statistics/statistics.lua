
local parser = require "parser"

local function generate (filename)
  assert(type(filename) == "string")
  local ast,errormsg = parser.parse(filename)
  if not ast then
    error(errormsg)
  end
  return ast
end

return {
  generate = generate,
}
