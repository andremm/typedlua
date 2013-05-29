
local parser = require "parser"
local pp = require "pp"

local function print_local_functions (vl, el)
  for k,v in pairs(el) do
    if v.tag == "ExpFunction" then
      print(vl[k])
    end
  end
end

local function count_functions (ast)
  if ast.tag == "StmBlock" then
    for k,v in ipairs(ast) do
      if v.tag == "StmFunction" then
      elseif v.tag == "StmLocalFunction" then
        print("function", v[1])
      elseif v.tag == "StmAssign" then
      elseif v.tag == "StmLocalVar" then
        print_local_functions(v[1], v[2])
      end
    end
  end
end

local function generate (filename)
  assert(type(filename) == "string")
  local ast,errormsg = parser.parse(filename)
  if not ast then
    error(errormsg)
  end
  count_functions(ast)
  return ast
end

return {
  generate = generate,
}
