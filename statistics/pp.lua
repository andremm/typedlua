-- pretty print module

local print_var, print_varlist
local print_name, print_namelist
local print_exp, print_explist
local print_fieldlist
local print_stm, print_block

local function printt(t, i)
  io.write (string.format (string.rep(" ", i) .. "{tag = %s,\n", t.tag))
  for k,v in ipairs(t) do
    if type (v) == "table" then
      printt(v, i+2)
    else
      io.write (string.format (string.rep(" ", i+2) .. tostring(v) .. "\n"))
    end
  end
  io.write (string.format (string.rep(" ", i) .. "}\n"))
end

print_var = function (v)
  io.write(string.format("%s", v.tag))
  if v.tag == "VarID" then
    io.write(string.format(" \"%s\"", v[1]))
  elseif v.tag == "VarIndex" then
    io.write(" (")
    print_exp(v[1])
    io.write(") (")
    print_exp(v[2])
    io.write(")")
  end
end

print_varlist = function (vl)
  io.write(" [")
  if #vl > 0 then
    print_var(vl[1])
    for i=2,#vl do
      io.write(",")
      print_var(vl[i])
    end
  end
  io.write("]")
end

print_name = function (x)
  io.write(string.format("\"%s\"", x))
end

print_namelist = function (nl)
  io.write("[")
  if #nl > 0 then
    print_name(nl[1])
    for i=2,#nl do
      io.write(",")
      print_name(nl[i])
    end
  end
  io.write("]")
end

local function iscntrl(x)
  if (x >= 0 and x <= 31) or (x == 127) then return true end
  return false
end

local function isprint(x)
  return not iscntrl(x)
end

local function fixed_string(str)
  local new_str = ""
  for i=1,string.len(str) do
    char = string.byte(str, i)
    if char == 34 then new_str = new_str .. string.format("\\\"")
    elseif char == 92 then new_str = new_str .. string.format("\\\\")
    elseif char == 7 then new_str = new_str .. string.format("\\a")
    elseif char == 8 then new_str = new_str .. string.format("\\b")
    elseif char == 12 then new_str = new_str .. string.format("\\f")
    elseif char == 10 then new_str = new_str .. string.format("\\n")
    elseif char == 13 then new_str = new_str .. string.format("\\r")
    elseif char == 9 then new_str = new_str .. string.format("\\t")
    elseif char == 11 then new_str = new_str .. string.format("\\v")
    else
      if isprint(char) then
        new_str = new_str .. string.format("%c", char)
      else
        new_str = new_str .. string.format("\\%03d", char)
      end
    end
  end
  return new_str
end

print_exp = function (e)
  io.write(string.format("%s", e.tag))
  if e.tag == "ExpNum" then
    io.write(string.format(" %.1f", e[1]))
  elseif e.tag == "ExpStr" then
    io.write(string.format(" \"%s\"", fixed_string(e[1])))
  elseif e.tag == "ExpVar" then
    io.write(" (")
    print_var(e[1])
    io.write(")")
  elseif e.tag == "ExpFunction" then
    io.write(" ((")
    print_namelist(e[1])
    if e[1].is_vararg then
      io.write(",True),")
    else
      io.write(",False),")
    end
    print_stm(e[2])
    io.write(")")
  elseif e.tag == "ExpTableConstructor" then
    print_fieldlist(e[1])
  elseif e.tag == "ExpMethodCall" then
    io.write(" (")
    print_exp(e[1])
    io.write(string.format(") \"%s\"", e[2]))
    print_explist(e[3])
  elseif e.tag == "ExpFunctionCall" then
    io.write(" (")
    print_exp(e[1])
    io.write(")")
    print_explist(e[2])
  elseif e.tag == "ExpAdd" or
         e.tag == "ExpSub" or
         e.tag == "ExpMul" or
         e.tag == "ExpDiv" or
         e.tag == "ExpMod" or
         e.tag == "ExpPow" or
         e.tag == "ExpConcat" or
         e.tag == "ExpNE" or
         e.tag == "ExpEQ" or
         e.tag == "ExpLT" or
         e.tag == "ExpLE" or
         e.tag == "ExpGT" or
         e.tag == "ExpGE" then
      io.write(" (")
      print_exp(e[1])
      io.write(") (")
      print_exp(e[2])
      io.write(")")
    elseif e.tag == "ExpNot" or
           e.tag == "ExpMinus" or
           e.tag == "ExpLen" then
      io.write(" (")
      print_exp(e[1])
      io.write(")")
  end
end

print_explist = function (el)
  io.write(" [")
  if #el > 0 then
    print_exp(el[1])
    for i=2,#el do
      io.write(",")
      print_exp(el[i])
    end
  end
  io.write("]")
end

print_fieldlist = function (fl)
  io.write(" ([")
  if #fl[1] > 0 then
    print_exp(fl[1][1][1])
    for i=2,#fl[1] do
      io.write(",")
      print_exp(fl[1][i][1])
    end
  end
  io.write("],[")
  if #fl[2] > 0 then
    io.write("(")
    print_exp(fl[2][1][1])
    io.write(",")
    print_exp(fl[2][1][2])
    io.write(")")
    for i=2,#fl[2] do
    io.write(",(")
    print_exp(fl[2][i][1])
    io.write(",")
    print_exp(fl[2][i][2])
    io.write(")")
    end
  end
  io.write("])")
end

print_stm = function(s)
  io.write(string.format("%s", s.tag))
  if s.tag == "StmBlock" then
    io.write(" [")
    print_block(s)
    io.write("]")
  elseif s.tag == "StmIfElse" then
    io.write(" (")
    print_exp(s[1])
    io.write(") (")
    print_stm(s[2])
    io.write(") (")
    print_stm(s[3])
    io.write(")")
  elseif s.tag == "StmWhile" then
    io.write(" (")
    print_exp(s[1])
    io.write(") (")
    print_stm(s[2])
    io.write(")")
  elseif s.tag == "StmForNum" then
    io.write(string.format(" \"%s\" (", s[1]))
    print_exp(s[2])
    io.write(") (")
    print_exp(s[3])
    io.write(") (")
    print_exp(s[4])
    io.write(") (")
    print_stm(s[5])
    io.write(")")
  elseif s.tag == "StmForGen" then
    io.write(" ")
    print_namelist(s[1])
    print_explist(s[2])
    io.write(" (")
    print_stm(s[3])
    io.write(")")
  elseif s.tag == "StmLocalFunction" then
    io.write(string.format(" \"%s\" ((", s[1]))
    print_namelist(s[2])
    if s[2].is_vararg then
      io.write(",True),")
    else
      io.write(",False),")
    end
    print_stm(s[3])
    io.write(")")
  elseif s.tag == "StmRepeat" then
    io.write(" (")
    print_stm(s[1])
    io.write(") (")
    print_exp(s[2])
    io.write(")")
  elseif s.tag == "StmFunction" then
    io.write(string.format(" (%s ", s[1].tag))
    print_namelist(s[1])
    io.write(") ((")
    print_namelist(s[2])
    if s[2].is_vararg then
      io.write(",True),")
    else
      io.write(",False),")
    end
    print_stm(s[3])
    io.write(")")
  elseif s.tag == "StmLabel" or
         s.tag == "StmGoTo" then
    io.write(string.format(" \"%s\"", s[1]))
  elseif s.tag == "StmAssign" then
    print_varlist(s[1])
    print_explist(s[2])
  elseif s.tag == "StmLocalVar" then
    io.write(" ")
    print_namelist(s[1])
    print_explist(s[2])
  elseif s.tag == "StmRet" then
    print_explist(s[1])
  elseif s.tag == "StmCall" then
    io.write(" (")
    print_exp(s[1])
    io.write(")")
  end
end

print_block = function (b)
  if #b > 0 then
    print_stm(b[1])
    for i=2,#b do
      io.write(",")
      print_stm(b[i])
    end
  end
end

-- print AST in Haskell style
local function prinths(ast)
  print_stm(ast)
  print()
end

local pp = {
  printt = printt,
  prinths = prinths,
}

return pp
