#!/usr/bin/env lua

local abort_on_error = false

local loadstring = loadstring or load

local tlast = require "typedlua.tlast"
local tlparser = require "typedlua.tlparser"
local tltype = require "typedlua.tltype"
local tlchecker = require "typedlua.tlchecker"
local tlcode = require "typedlua.tlcode"

local typedlua = require "typedlua" -- For the module loader

-- expected result, result, subject
local e, r, s

local filename = "test.lua"

local function parse (my_s)
  local t,m = tlparser.parse(my_s,filename,false,false)
  local my_r
  if not t then
    my_r = m
  else
    my_r = tlast.tostring(t)
  end
  return my_r .. "\n"
end

local function typecheck (my_s)
  local t,m = tlparser.parse(my_s,filename,false,false)
  local my_r
  if not t then
    error(m)
    os.exit(1)
  end
  m = tlchecker.typecheck(t,my_s,filename,false,false,false)
  m = tlchecker.error_msgs(m,false)
  if m then
    my_r = m
  else
    my_r = tlast.tostring(t)
  end
  return my_r .. "\n"
end

local function generate (my_s)
  local t,m = tlparser.parse(my_s,filename,false,false)
  if not t then
    error(m)
    os.exit(1)
  end
  m = tlchecker.typecheck(t,my_s,filename,false,false,false)
  m = tlchecker.error_msgs(m,false)
  if m then
    return m .. "\n"
  else
    return tlcode.generate(t)
  end
end

local function test_loader (s)
  local ok, ret1, ret2 = pcall(loadstring, s, "test.tl")
  if not ok then
    return ret1
  else
    if ret1 then
      return ret1()
    else
      return ret2 .. "\n"
    end
  end
end

local passed_tests = 0
local failed_tests = 0

local function check (e, r)
  r = r or e
  if type(e) == "string" then
    e = e:gsub("%s+(%s)", "%1"):gsub("([^%w]) ([%w{(\"\'])", "%1%2")
  end
  if type(r) == "string" then
    r = r:gsub("%s+(%s)", "%1"):gsub("([^%w]) ([%w{(\"\'])", "%1%2")
  end
  if e == r then
    passed_tests = passed_tests + 1
  else
    failed_tests = failed_tests + 1
    print("AT LINE: ", debug.getinfo(2, "l").currentline)
    print("e:")
    print(e)
    print("r:")
    print(r)
  end

  if abort_on_error then
    assert(e == r)
  end
end

local function fixint (s)
  return _VERSION < "Lua 5.3" and s:gsub("%.0","") or s
end

print("> testing lexer...")

-- syntax ok

-- empty files

s = [=[
]=]
e = [=[
{  }
]=]

r = parse(s)
check(e, r)

s = [=[
-- testing empty file
]=]
e = [=[
{  }
]=]

r = parse(s)
check(e, r)

-- expressions

s = [=[
local _nil,_false,_true,_dots = nil,false,true,...
]=]
e = [=[
{ `Local{ { `Id "_nil", `Id "_false", `Id "_true", `Id "_dots" }, { `Nil, `False, `True, `Dots } } }
]=]

r = parse(s)
check(e, r)

-- floating points

s = [=[
local f1 = 1.
local f2 = 1.1
]=]
e = [=[
{ `Local{ { `Id "f1" }, { `Number "1.0" } }, `Local{ { `Id "f2" }, { `Number "1.1" } } }
]=]

r = parse(s)
check(fixint(e), r)

s = [=[
local f1 = 1.e-1
local f2 = 1.e1
]=]
e = [=[
{ `Local{ { `Id "f1" }, { `Number "0.1" } }, `Local{ { `Id "f2" }, { `Number "10.0" } } }
]=]

r = parse(s)
check(fixint(e), r)

s = [=[
local f1 = 1.1e+1
local f2 = 1.1e1
]=]
e = [=[
{ `Local{ { `Id "f1" }, { `Number "11.0" } }, `Local{ { `Id "f2" }, { `Number "11.0" } } }
]=]

r = parse(s)
check(fixint(e), r)

s = [=[
local f1 = .1
local f2 = .1e1
]=]
e = [=[
{ `Local{ { `Id "f1" }, { `Number "0.1" } }, `Local{ { `Id "f2" }, { `Number "1.0" } } }
]=]

r = parse(s)
check(fixint(e), r)

s = [=[
local f1 = 1E1
local f2 = 1e-1
]=]
e = [=[
{ `Local{ { `Id "f1" }, { `Number "10.0" } }, `Local{ { `Id "f2" }, { `Number "0.1" } } }
]=]

r = parse(s)
check(fixint(e), r)

-- integers

s = [=[
local i = 1
local h = 0xff
]=]
e = [=[
{ `Local{ { `Id "i" }, { `Number "1" } }, `Local{ { `Id "h" }, { `Number "255" } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local h = 0x76c
local i = 4294967296 -- 2^32
]=]
e = [=[
{ `Local{ { `Id "h" }, { `Number "1900" } }, `Local{ { `Id "i" }, { `Number "4294967296" } } }
]=]

r = parse(s)
check(e, r)

-- long comments

s = [=[
--[======[
testing
long
comment
[==[ one ]==]
[===[ more ]===]
[====[ time ]====]
bye
]======]
]=]
e = [=[
{  }
]=]

r = parse(s)
check(e, r)

-- long strings

s = [=[
--[[
testing long string1 begin
]]

local ls1 =
[[
testing long string
]]

--[[
testing long string1 end
]]
]=]
e = [=[
{ `Local{ { `Id "ls1" }, { `String "testing long string\n" } } }
]=]

r = parse(s)
check(e, r)

s = [=[
--[==[
testing long string2 begin
]==]

local ls2 = [==[ testing \n [[ long ]] \t [===[ string ]===]
\a ]==]

--[==[
[[ testing long string2 end ]]
]==]
]=]
e = [=[
{ `Local{ { `Id "ls2" }, { `String " testing \\n [[ long ]] \\t [===[ string ]===]\n\\a " } } }
]=]

r = parse(s)
check(e, r)

-- short strings

s = [=[
-- short string test begin

local ss1_a = "ola mundo\a"
local ss1_b = 'ola mundo\a'

-- short string test end
]=]
e = [=[
{ `Local{ { `Id "ss1_a" }, { `String "ola mundo\a" } }, `Local{ { `Id "ss1_b" }, { `String "ola mundo\a" } } }
]=]

r = parse(s)
check(e, r)

s = [=[
-- short string test begin

local ss2_a = "testando,\tteste\n1\n2\n3 --> \"tchau\""
local ss2_b = 'testando,\tteste\n1\n2\n3 --> \'tchau\''

-- short string test end
]=]
e = [=[
{ `Local{ { `Id "ss2_a" }, { `String "testando,\tteste\n1\n2\n3 --> \"tchau\"" } }, `Local{ { `Id "ss2_b" }, { `String "testando,\tteste\n1\n2\n3 --> 'tchau'" } } }
]=]

r = parse(s)
check(e, r)

s = [=[
-- short string test begin

local ss3_a = "ola \
'mundo'!"

local ss3_b = 'ola \
"mundo"!'

-- short string test end
]=]
e = [=[
{ `Local{ { `Id "ss3_a" }, { `String "ola \n'mundo'!" } }, `Local{ { `Id "ss3_b" }, { `String "ola \n\"mundo\"!" } } }
]=]

r = parse(s)
check(e, r)

s = [=[
-- short string test begin

local ss4_a = "C:\\Temp/"

local ss4_b = 'C:\\Temp/'

-- short string test end
]=]
e = [=[
{ `Local{ { `Id "ss4_a" }, { `String "C:\\Temp/" } }, `Local{ { `Id "ss4_b" }, { `String "C:\\Temp/" } } }
]=]

r = parse(s)
check(e, r)

s = [=[
-- short string test begin

local ss5_a = "ola \
mundo \\ \
cruel"

local ss5_b = 'ola \
mundo \\ \
cruel'

-- short string test end
]=]
e = [=[
{ `Local{ { `Id "ss5_a" }, { `String "ola \nmundo \\ \ncruel" } }, `Local{ { `Id "ss5_b" }, { `String "ola \nmundo \\ \ncruel" } } }
]=]

r = parse(s)
check(e, r)

-- syntax error

-- floating points

s = [=[
local f = 9e
]=]
e = [=[
test.lua:2:1: syntax error, unexpected 'EOF', expecting '=', ',', 'String', '{', '(', ':', '[', '.'
]=]

r = parse(s)
check(e, r)

s = [=[
local f = 5.e
]=]
e = [=[
test.lua:2:1: syntax error, unexpected 'EOF', expecting '=', ',', 'String', '{', '(', ':', '[', '.'
]=]

r = parse(s)
check(e, r)

s = [=[
local f = .9e-
]=]
e = [=[
test.lua:1:14: syntax error, unexpected '-', expecting '=', ',', 'String', '{', '(', ':', '[', '.'
]=]

r = parse(s)
check(e, r)

s = [=[
local f = 5.9e+
]=]
e = [=[
test.lua:1:15: syntax error, unexpected '+', expecting '=', ',', 'String', '{', '(', ':', '[', '.'
]=]

r = parse(s)
check(e, r)

-- integers

s = [=[
-- invalid hexadecimal number

local hex = 0xG
]=]
e = [=[
test.lua:4:1: syntax error, unexpected 'EOF', expecting '=', ',', 'String', '{', '(', ':', '[', '.'
]=]

r = parse(s)
check(e, r)

-- long strings

s = [=[
--[==[
testing long string3 begin
]==]

local ls3 = [===[
testing
unfinised
long string
]==]

--[==[
[[ testing long string3 end ]]
]==]
]=]
e = [=[
test.lua:5:13: syntax error, unexpected '[', expecting '(', 'Name', '{', 'function', '...', 'true', 'false', 'nil', 'String', 'Number', '#', '~', '-', 'not'
]=]

r = parse(s)
check(e, r)

-- short strings

s = [=[
-- short string test begin

local ss6 = "testing unfinished string

-- short string test end
]=]
e = [=[
test.lua:3:13: syntax error, unexpected '"', expecting '(', 'Name', '{', 'function', '...', 'true', 'false', 'nil', 'String', 'Number', '#', '~', '-', 'not'
]=]

r = parse(s)
check(e, r)

s = [=[
-- short string test begin

local ss7 = 'testing \\
unfinished \\
string'

-- short string test end
]=]
e = [=[
]=]

r = parse(s)
--check(e, r)

-- unfinished comments

s = [=[
--[[ testing
unfinished
comment
]=]
e = [=[
test.lua:3:1: syntax error, unexpected 'comment', expecting '=', ',', 'String', '{', '(', ':', '[', '.'
]=]

r = parse(s)
check(e, r)

print("> testing parser...")

-- syntax ok

-- anonymous functions

s = [=[
local a,b,c = function () end
]=]
e = [=[
{ `Local{ { `Id "a", `Id "b", `Id "c" }, { `Function{ {  }, {  } } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local test = function ( a , b , ... ) end
]=]
e = [=[
{ `Local{ { `Id "test" }, { `Function{ { `Id "a", `Id "b", `Dots }, {  } } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local test = function (...) return ...,0 end
]=]
e = [=[
{ `Local{ { `Id "test" }, { `Function{ { `Dots }, { `Return{ `Dots, `Number "0" } } } } } }
]=]

r = parse(s)
check(e, r)

-- arithmetic expressions

s = [=[
local arithmetic = 1 - 2 * 3 + 4
]=]
e = [=[
{ `Local{ { `Id "arithmetic" }, { `Op{ "add", `Op{ "sub", `Number "1", `Op{ "mul", `Number "2", `Number "3" } }, `Number "4" } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local pow = -3^-2^2
]=]
e = [=[
{ `Local{ { `Id "pow" }, { `Op{ "unm", `Op{ "pow", `Number "3", `Op{ "unm", `Op{ "pow", `Number "2", `Number "2" } } } } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
q, r, f = 3//2, 3%2, 3/2
]=]
e = [=[
{ `Set{ { `Index{ `Id "_ENV", `String "q" }, `Index{ `Id "_ENV", `String "r" }, `Index{ `Id "_ENV", `String "f" } }, { `Op{ "idiv", `Number "3", `Number "2" }, `Op{ "mod", `Number "3", `Number "2" }, `Op{ "div", `Number "3", `Number "2" } } } }
]=]

r = parse(s)
check(e, r)

-- assignments

s = [=[
a = f()[1]
]=]
e = [=[
{ `Set{ { `Index{ `Id "_ENV", `String "a" } }, { `Index{ `Call{ `Index{ `Id "_ENV", `String "f" } }, `Number "1" } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
a()[1] = 1;
]=]
e = [=[
{ `Set{ { `Index{ `Call{ `Index{ `Id "_ENV", `String "a" } }, `Number "1" } }, { `Number "1" } } }
]=]

r = parse(s)
check(e, r)

s = [=[
i = a.f(1)
]=]
e = [=[
{ `Set{ { `Index{ `Id "_ENV", `String "i" } }, { `Call{ `Index{ `Index{ `Id "_ENV", `String "a" }, `String "f" }, `Number "1" } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
i = a[f(1)]
]=]
e = [=[
{ `Set{ { `Index{ `Id "_ENV", `String "i" } }, { `Index{ `Index{ `Id "_ENV", `String "a" }, `Call{ `Index{ `Id "_ENV", `String "f" }, `Number "1" } } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
a[f()] = sub
i = i + 1
]=]
e = [=[
{ `Set{ { `Index{ `Index{ `Id "_ENV", `String "a" }, `Call{ `Index{ `Id "_ENV", `String "f" } } } }, { `Index{ `Id "_ENV", `String "sub" } } }, `Set{ { `Index{ `Id "_ENV", `String "i" } }, { `Op{ "add", `Index{ `Id "_ENV", `String "i" }, `Number "1" } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
a:b(1)._ = some_value
]=]
e = [=[
{ `Set{ { `Index{ `Invoke{ `Index{ `Id "_ENV", `String "a" }, `String "b", `Number "1" }, `String "_" } }, { `Index{ `Id "_ENV", `String "some_value" } } } }
]=]

r = parse(s)
check(e, r)

-- bitwise expressions

s = [=[
b = 1 & 0 | 1 ~ 1
]=]
e = [=[
{ `Set{ { `Index{ `Id "_ENV", `String "b" } }, { `Op{ "bor", `Op{ "band", `Number "1", `Number "0" }, `Op{ "bxor", `Number "1", `Number "1" } } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
b = 1 & 0 | 1 >> 1 ~ 1
]=]
e = [=[
{ `Set{ { `Index{ `Id "_ENV", `String "b" } }, { `Op{ "bor", `Op{ "band", `Number "1", `Number "0" }, `Op{ "bxor", `Op{ "shr", `Number "1", `Number "1" }, `Number "1" } } } } }
]=]

r = parse(s)
check(e, r)

-- break

s = [=[
while 1 do
  break
end
]=]
e = [=[
{ `While{ `Number "1", { `Break } } }
]=]

r = parse(s)
check(e, r)

s = [=[
while 1 do
  while 1 do
    break
  end
  break
end
]=]
e = [=[
{ `While{ `Number "1", { `While{ `Number "1", { `Break } }, `Break } } }
]=]

r = parse(s)
check(e, r)

s = [=[
repeat
  if 2 > 1 then break end
until 1
]=]
e = [=[
{ `Repeat{ { `If{ `Op{ "lt", `Number "1", `Number "2" }, { `Break } } }, `Number "1" } }
]=]

r = parse(s)
check(e, r)

s = [=[
for i=1,10 do
  do
    break
    break
    return
  end
end
]=]
e = [=[
{ `Fornum{ `Id "i", `Number "1", `Number "10", { `Do{ `Break, `Break, `Return } } } }
]=]

r = parse(s)
check(e, r)

-- block statements

s = [=[
do
  local var = 2+2;
  return
end
]=]
e = [=[
{ `Do{ `Local{ { `Id "var" }, { `Op{ "add", `Number "2", `Number "2" } } }, `Return } }
]=]

r = parse(s)
check(e, r)

-- calls

s = [=[
f()
t:m()
]=]
e = [=[
{ `Call{ `Index{ `Id "_ENV", `String "f" } }, `Invoke{ `Index{ `Id "_ENV", `String "t" }, `String "m" } }
]=]

r = parse(s)
check(e, r)

-- concatenation expressions

s = [=[
local concat1 = 1 .. 2^3
]=]
e = [=[
{ `Local{ { `Id "concat1" }, { `Op{ "concat", `Number "1", `Op{ "pow", `Number "2", `Number "3" } } } } }
]=]

r = parse(s)
check(e, r)

-- empty files

s = [=[
;
]=]
e = [=[
{  }
]=]

r = parse(s)
check(e, r)

-- for generic

s = [=[
for k,v in pairs(t) do print (k,v) end
]=]
e = [=[
{ `Forin{ { `Id "k", `Id "v" }, { `Call{ `Index{ `Id "_ENV", `String "pairs" }, `Index{ `Id "_ENV", `String "t" } } }, { `Call{ `Index{ `Id "_ENV", `String "print" }, `Id "k", `Id "v" } } } }
]=]

r = parse(s)
check(e, r)

-- for numeric

s = [=[
for i = 1 , 10 , 2 do end
]=]
e = [=[
{ `Fornum{ `Id "i", `Number "1", `Number "10", `Number "2", {  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
for i=1,10 do end
]=]
e = [=[
{ `Fornum{ `Id "i", `Number "1", `Number "10", {  } } }
]=]

r = parse(s)
check(e, r)

-- global functions

s = [=[
function test(a , b , ...) end
]=]
e = [=[
{ `Set{ { `Index{ `Id "_ENV", `String "test" } }, { `Function{ { `Id "a", `Id "b", `Dots }, {  } } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
function test (...) end
]=]
e = [=[
{ `Set{ { `Index{ `Id "_ENV", `String "test" } }, { `Function{ { `Dots }, {  } } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
function t.a:b() end
]=]
e = [=[
{ `Set{ { `Index{ `Index{ `Index{ `Id "_ENV", `String "t" }, `String "a" }, `String "b" } }, { `Function{ { `Id "self" }, {  } } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
function t.a() end
]=]
e = [=[
{ `Set{ { `Index{ `Index{ `Id "_ENV", `String "t" }, `String "a" } }, { `Function{ {  }, {  } } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
function testando . funcao . com : espcacos ( e, com , parametros, ... ) end
]=]
e = [=[
{ `Set{ { `Index{ `Index{ `Index{ `Index{ `Id "_ENV", `String "testando" }, `String "funcao" }, `String "com" }, `String "espcacos" } }, { `Function{ { `Id "self", `Id "e", `Id "com", `Id "parametros", `Dots }, {  } } } } }
]=]

r = parse(s)
check(e, r)

-- goto

s = [=[
goto label
:: label :: return
]=]
e = [=[
{ `Goto{ "label" }, `Label{ "label" }, `Return }
]=]

r = parse(s)
check(e, r)

s = [=[
::label::
goto label
]=]
e = [=[
{ `Label{ "label" }, `Goto{ "label" } }
]=]

r = parse(s)
check(e, r)

s = [=[
goto label
::label::
]=]
e = [=[
{ `Goto{ "label" }, `Label{ "label" } }
]=]

r = parse(s)
check(e, r)

s = [=[
::label::
do ::label:: goto label end
]=]
e = [=[
{ `Label{ "label" }, `Do{ `Label{ "label" }, `Goto{ "label" } } }
]=]

r = parse(s)
check(e, r)

s = [=[
::label::
do goto label ; ::label:: end
]=]
e = [=[
{ `Label{ "label" }, `Do{ `Goto{ "label" }, `Label{ "label" } } }
]=]

r = parse(s)
check(e, r)

s = [=[
::label::
do goto label end
]=]
e = [=[
{ `Label{ "label" }, `Do{ `Goto{ "label" } } }
]=]

r = parse(s)
check(e, r)

s = [=[
do goto label end
::label::
]=]
e = [=[
{ `Do{ `Goto{ "label" } }, `Label{ "label" } }
]=]

r = parse(s)
check(e, r)

s = [=[
do do do do do goto label end end end end end
::label::
]=]
e = [=[
{ `Do{ `Do{ `Do{ `Do{ `Do{ `Goto{ "label" } } } } } }, `Label{ "label" } }
]=]

r = parse(s)
check(e, r)

-- if-else

s = [=[
if a then end
]=]
e = [=[
{ `If{ `Index{ `Id "_ENV", `String "a" }, {  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
if a then return a else return end
]=]
e = [=[
{ `If{ `Index{ `Id "_ENV", `String "a" }, { `Return{ `Index{ `Id "_ENV", `String "a" } } }, { `Return } } }
]=]

r = parse(s)
check(e, r)

s = [=[
if a then
  return a
else
  local c = d
  d = d + 1
  return d
end
]=]
e = [=[
{ `If{ `Index{ `Id "_ENV", `String "a" }, { `Return{ `Index{ `Id "_ENV", `String "a" } } }, { `Local{ { `Id "c" }, { `Index{ `Id "_ENV", `String "d" } } }, `Set{ { `Index{ `Id "_ENV", `String "d" } }, { `Op{ "add", `Index{ `Id "_ENV", `String "d" }, `Number "1" } } }, `Return{ `Index{ `Id "_ENV", `String "d" } } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
if a then
  return a
elseif b then
  return b
elseif c then
  return c
end
]=]
e = [=[
{ `If{ `Index{ `Id "_ENV", `String "a" }, { `Return{ `Index{ `Id "_ENV", `String "a" } } }, `Index{ `Id "_ENV", `String "b" }, { `Return{ `Index{ `Id "_ENV", `String "b" } } }, `Index{ `Id "_ENV", `String "c" }, { `Return{ `Index{ `Id "_ENV", `String "c" } } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
if a then return a
elseif b then return
else ;
end
]=]
e = [=[
{ `If{ `Index{ `Id "_ENV", `String "a" }, { `Return{ `Index{ `Id "_ENV", `String "a" } } }, `Index{ `Id "_ENV", `String "b" }, { `Return }, {  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
if a then
  return
elseif c then
end
]=]
e = [=[
{ `If{ `Index{ `Id "_ENV", `String "a" }, { `Return }, `Index{ `Id "_ENV", `String "c" }, {  } } }
]=]

r = parse(s)
check(e, r)

-- interfaces

s = [=[
local interface Empty end
]=]
e = [=[
{ `Interface{ Empty, `TTable{  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local interface X
  x, y, z:number
end
]=]
e = [=[
{ `Interface{ X, `TTable{ `TLiteral x:`TBase number, `TLiteral y:`TBase number, `TLiteral z:`TBase number } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local interface Person
  firstname:string
  lastname:string
end
]=]
e = [=[
{ `Interface{ Person, `TTable{ `TLiteral firstname:`TBase string, `TLiteral lastname:`TBase string } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local interface Element
  info:number
  next:Element?
end
]=]
e = [=[
{ `Interface{ Element, `TRecursive{ Element, `TTable{ `TLiteral info:`TBase number, `TLiteral next:`TUnion{ `TVariable Element, `TNil } } } } }
]=]

r = parse(s)
check(e, r)

-- labels

s = [=[
::label::
do ::label:: end
::other_label::
]=]
e = [=[
{ `Label{ "label" }, `Do{ `Label{ "label" } }, `Label{ "other_label" } }
]=]

r = parse(s)
check(e, r)

-- locals

s = [=[
local a
]=]
e = [=[
{ `Local{ { `Id "a" }, {  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local a,b,c
]=]
e = [=[
{ `Local{ { `Id "a", `Id "b", `Id "c" }, {  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local a = 1 , 1 + 2, 5.1
]=]
e = [=[
{ `Local{ { `Id "a" }, { `Number "1", `Op{ "add", `Number "1", `Number "2" }, `Number "5.1" } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local a,b,c = 1.9
]=]
e = [=[
{ `Local{ { `Id "a", `Id "b", `Id "c" }, { `Number "1.9" } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local function test() end
]=]
e = [=[
{ `Localrec{ { `Id "test" }, { `Function{ {  }, {  } } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local function test ( a , b , c , ... ) end
]=]
e = [=[
{ `Localrec{ { `Id "test" }, { `Function{ { `Id "a", `Id "b", `Id "c", `Dots }, {  } } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local function test(...) return ... end
]=]
e = [=[
{ `Localrec{ { `Id "test" }, { `Function{ { `Dots }, { `Return{ `Dots } } } } } }
]=]

r = parse(s)
check(e, r)

-- relational expressions

s = [=[
local relational = 1 < 2 >= 3 == 4 ~= 5 < 6 <= 7
]=]
e = [=[
{ `Local{ { `Id "relational" }, { `Op{ "le", `Op{ "lt", `Op{ "not", `Op{ "eq", `Op{ "eq", `Op{ "le", `Number "3", `Op{ "lt", `Number "1", `Number "2" } }, `Number "4" }, `Number "5" } }, `Number "6" }, `Number "7" } } } }
]=]

r = parse(s)
check(e, r)

-- repeat

s = [=[
repeat
  local a,b,c = 1+1,2+2,3+3
  break
until a < 1
]=]
e = [=[
{ `Repeat{ { `Local{ { `Id "a", `Id "b", `Id "c" }, { `Op{ "add", `Number "1", `Number "1" }, `Op{ "add", `Number "2", `Number "2" }, `Op{ "add", `Number "3", `Number "3" } } }, `Break }, `Op{ "lt", `Index{ `Id "_ENV", `String "a" }, `Number "1" } } }
]=]

r = parse(s)
check(e, r)

-- return

s = [=[
return
]=]
e = [=[
{ `Return }
]=]

r = parse(s)
check(e, r)

s = [=[
return 1
]=]
e = [=[
{ `Return{ `Number "1" } }
]=]

r = parse(s)
check(e, r)

s = [=[
return 1,1-2*3+4,"alo"
]=]
e = [=[
{ `Return{ `Number "1", `Op{ "add", `Op{ "sub", `Number "1", `Op{ "mul", `Number "2", `Number "3" } }, `Number "4" }, `String "alo" } }
]=]

r = parse(s)
check(e, r)

s = [=[
return;
]=]
e = [=[
{ `Return }
]=]

r = parse(s)
check(e, r)

s = [=[
return 1;
]=]
e = [=[
{ `Return{ `Number "1" } }
]=]

r = parse(s)
check(e, r)

s = [=[
return 1,1-2*3+4,"alo";
]=]
e = [=[
{ `Return{ `Number "1", `Op{ "add", `Op{ "sub", `Number "1", `Op{ "mul", `Number "2", `Number "3" } }, `Number "4" }, `String "alo" } }
]=]

r = parse(s)
check(e, r)

-- tables

s = [=[
local t = { [1] = "alo", alo = 1, 2; }
]=]
e = [=[
{ `Local{ { `Id "t" }, { `Table{ `Pair{ `Number "1", `String "alo" }, `Pair{ `String "alo", `Number "1" }, `Number "2" } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local t = { 1.5 }
]=]
e = [=[
{ `Local{ { `Id "t" }, { `Table{ `Number "1.5" } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local t = {1,2;
3,
4,



5}
]=]
e = [=[
{ `Local{ { `Id "t" }, { `Table{ `Number "1", `Number "2", `Number "3", `Number "4", `Number "5" } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local t = {[1]=1,[2]=2;
[3]=3,
[4]=4,



[5]=5}
]=]
e = [=[
{ `Local{ { `Id "t" }, { `Table{ `Pair{ `Number "1", `Number "1" }, `Pair{ `Number "2", `Number "2" }, `Pair{ `Number "3", `Number "3" }, `Pair{ `Number "4", `Number "4" }, `Pair{ `Number "5", `Number "5" } } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local t = {{{}}, {"alo"}}
]=]
e = [=[
{ `Local{ { `Id "t" }, { `Table{ `Table{ `Table }, `Table{ `String "alo" } } } } }
]=]

r = parse(s)
check(e, r)

-- vararg

s = [=[
local f = function (...)
  return ...
end
]=]
e = [=[
{ `Local{ { `Id "f" }, { `Function{ { `Dots }, { `Return{ `Dots } } } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local f = function ()
  local g = function (x, y, ...)
    return ...,...,...
  end
end
]=]
e = [=[
{ `Local{ { `Id "f" }, { `Function{ {  }, { `Local{ { `Id "g" }, { `Function{ { `Id "x", `Id "y", `Dots }, { `Return{ `Dots, `Dots, `Dots } } } } } } } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local function f (x, ...)
  return ...
end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ { `Id "x", `Dots }, { `Return{ `Dots } } } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local f = function (x, ...)
  return ...
end
]=]
e = [=[
{ `Local{ { `Id "f" }, { `Function{ { `Id "x", `Dots }, { `Return{ `Dots } } } } } }
]=]

r = parse(s)
check(e, r)

-- while

s = [=[
local i = 0
while (i < 10)
do
  i = i + 1
end
]=]
e = [=[
{ `Local{ { `Id "i" }, { `Number "0" } }, `While{ `Paren{ `Op{ "lt", `Id "i", `Number "10" } }, { `Set{ { `Id "i" }, { `Op{ "add", `Id "i", `Number "1" } } } } } }
]=]

r = parse(s)
check(e, r)

-- type annotations

s = [=[
local x:nil
]=]
e = [=[
{ `Local{ { `Id "x":`TNil }, {  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local x:false, y:true
]=]
e = [=[
{ `Local{ { `Id "x":`TLiteral false, `Id "y":`TLiteral true }, {  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local x:1, y:1.1
]=]
e = [=[
{ `Local{ { `Id "x":`TLiteral 1, `Id "y":`TLiteral 1.1 }, {  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local x:"hello", y:'world'
]=]
e = [=[
{ `Local{ { `Id "x":`TLiteral hello, `Id "y":`TLiteral world }, {  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local x:boolean, y:number, z:string
]=]
e = [=[
{ `Local{ { `Id "x":`TBase boolean, `Id "y":`TBase number, `Id "z":`TBase string }, {  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local x:any
]=]
e = [=[
{ `Local{ { `Id "x":`TAny }, {  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local x:number?
]=]
e = [=[
{ `Local{ { `Id "x":`TUnion{ `TBase number, `TNil } }, {  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local x:number|nil
]=]
e = [=[
{ `Local{ { `Id "x":`TUnion{ `TBase number, `TNil } }, {  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local x:number|string|nil
]=]
e = [=[
{ `Local{ { `Id "x":`TUnion{ `TBase number, `TBase string, `TNil } }, {  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local x:number|nil|nil|nil|nil
]=]
e = [=[
{ `Local{ { `Id "x":`TUnion{ `TBase number, `TNil } }, {  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local x:number|nil|string|nil|number|boolean|string
]=]
e = [=[
{ `Local{ { `Id "x":`TUnion{ `TNil, `TBase number, `TBase boolean, `TBase string } }, {  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local x:number|string?
]=]
e = [=[
{ `Local{ { `Id "x":`TUnion{ `TBase number, `TBase string, `TNil } }, {  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local x:(number) -> (number)
]=]
e = [=[
{ `Local{ { `Id "x":`TFunction{ `TTuple{ `TBase number, `TVararg{ `TValue } }, `TTuple{ `TBase number, `TVararg{ `TNil } } } }, {  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local x:(value*) -> (nil*)
]=]
e = [=[
{ `Local{ { `Id "x":`TFunction{ `TTuple{ `TVararg{ `TValue } }, `TTuple{ `TVararg{ `TNil } } } }, {  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local x:(number,string,boolean) -> (string,number,boolean)
]=]
e = [=[
{ `Local{ { `Id "x":`TFunction{ `TTuple{ `TBase number, `TBase string, `TBase boolean, `TVararg{ `TValue } }, `TTuple{ `TBase string, `TBase number, `TBase boolean, `TVararg{ `TNil } } } }, {  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local x:(number,string,value*) -> (string,number,nil*)
]=]
e = [=[
{ `Local{ { `Id "x":`TFunction{ `TTuple{ `TBase number, `TBase string, `TVararg{ `TValue } }, `TTuple{ `TBase string, `TBase number, `TVararg{ `TNil } } } }, {  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local x:{}
]=]
e = [=[
{ `Local{ { `Id "x":`TTable{  } }, {  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local x:{{{{{}}}}}
]=]
e = [=[
{ `Local{ { `Id "x":`TTable{ `TBase number:`TUnion{ `TTable{ `TBase number:`TUnion{ `TTable{ `TBase number:`TUnion{ `TTable{ `TBase number:`TUnion{ `TTable{  }, `TNil } }, `TNil } }, `TNil } }, `TNil } } }, {  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local x:{string}
]=]
e = [=[
{ `Local{ { `Id "x":`TTable{ `TBase number:`TUnion{ `TBase string, `TNil } } }, {  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local x:{string:number}
]=]
e = [=[
{ `Local{ { `Id "x":`TTable{ `TBase string:`TUnion{ `TBase number, `TNil } } }, {  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local x:{'firstname':string, 'lastname':string}
]=]
e = [=[
{ `Local{ { `Id "x":`TTable{ `TLiteral firstname:`TBase string, `TLiteral lastname:`TBase string } }, {  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local x:{'tag':string, number:string}
]=]
e = [=[
{ `Local{ { `Id "x":`TTable{ `TLiteral tag:`TBase string, `TBase number:`TUnion{ `TBase string, `TNil } } }, {  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local x:{'f':(number) -> (number), 't':{number:number}}
]=]
e = [=[
{ `Local{ { `Id "x":`TTable{ `TLiteral f:`TFunction{ `TTuple{ `TBase number, `TVararg{ `TValue } }, `TTuple{ `TBase number, `TVararg{ `TNil } } }, `TLiteral t:`TTable{ `TBase number:`TUnion{ `TBase number, `TNil } } } }, {  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
for k:number, v:string in ipairs({"hello", "world"}) do end
]=]
e = [=[
{ `Forin{ { `Id "k":`TBase number, `Id "v":`TBase string }, { `Call{ `Index{ `Id "_ENV", `String "ipairs" }, `Table{ `String "hello", `String "world" } } }, {  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
for k:string, v in pairs({}) do end
]=]
e = [=[
{ `Forin{ { `Id "k":`TBase string, `Id "v" }, { `Call{ `Index{ `Id "_ENV", `String "pairs" }, `Table } }, {  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
for k, v:boolean in pairs({}) do end
]=]
e = [=[
{ `Forin{ { `Id "k", `Id "v":`TBase boolean }, { `Call{ `Index{ `Id "_ENV", `String "pairs" }, `Table } }, {  } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local function f (x:any) end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ { `Id "x":`TAny }, {  } } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local function f (x:any):(any) end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ { `Id "x":`TAny }:`TTuple{ `TAny, `TVararg{ `TNil } }, {  } } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local function f (...:any) end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ { `Dots:`TAny }, {  } } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local function f (x:any, ...:any) end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ { `Id "x":`TAny, `Dots:`TAny }, {  } } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local function f (x, ...:any) end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ { `Id "x", `Dots:`TAny }, {  } } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local function f (x:any, ...) end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ { `Id "x":`TAny, `Dots }, {  } } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local function f (x:any, ...:any):(any) end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ { `Id "x":`TAny, `Dots:`TAny }:`TTuple{ `TAny, `TVararg{ `TNil } }, {  } } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local function f (x:(any) -> (any)):((any) -> (any)) end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ { `Id "x":`TFunction{ `TTuple{ `TAny, `TVararg{ `TValue } }, `TTuple{ `TAny, `TVararg{ `TNil } } } }:`TTuple{ `TFunction{ `TTuple{ `TAny, `TVararg{ `TValue } }, `TTuple{ `TAny, `TVararg{ `TNil } } }, `TVararg{ `TNil } }, {  } } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local function f (x:(number, number) -> (number, nil*)):(number*) end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ { `Id "x":`TFunction{ `TTuple{ `TBase number, `TBase number, `TVararg{ `TValue } }, `TTuple{ `TBase number, `TVararg{ `TNil } } } }:`TTuple{ `TVararg{ `TBase number } }, {  } } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local function f ():(number, nil*) end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ {  }:`TTuple{ `TBase number, `TVararg{ `TNil } }, {  } } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local function f ():number end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ {  }:`TTuple{ `TBase number, `TVararg{ `TNil } }, {  } } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local function f ():number? end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ {  }:`TTuple{ `TUnion{ `TBase number, `TNil }, `TVararg{ `TNil } }, {  } } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local function f ():(number) | (nil,string) end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ {  }:`TUnionlist{ `TTuple{ `TBase number, `TVararg{ `TNil } }, `TTuple{ `TNil, `TBase string, `TVararg{ `TNil } } }, {  } } } } }
]=]

r = parse(s)
check(e, r)

s = [=[
local function f ():(number)? end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ {  }:`TUnionlist{ `TTuple{ `TBase number, `TVararg{ `TNil } }, `TTuple{ `TNil, `TBase string, `TVararg{ `TNil } } }, {  } } } } }
]=]

r = parse(s)
check(e, r)

-- syntax error

-- anonymous functions

s = [=[
a = function (a,b,) end
]=]
e = [=[
test.lua:1:19: syntax error, unexpected ')', expecting '...', 'Name'
]=]

r = parse(s)
check(e, r)

s = [=[
a = function (...,a) end
]=]
e = [=[
test.lua:1:18: syntax error, unexpected ',', expecting ')', ':'
]=]

r = parse(s)
check(e, r)

s = [=[
local a = function (1) end
]=]
e = [=[
test.lua:1:21: syntax error, unexpected '1', expecting ')', '...', 'Name'
]=]

r = parse(s)
check(e, r)

s = [=[
local test = function ( a , b , c , ... )
]=]
e = [=[
test.lua:2:1: syntax error, unexpected 'EOF', expecting 'end', 'return', '(', 'Name', 'typealias', 'interface', 'goto', 'break', '::', 'local', 'function', 'const', 'repeat', 'for', 'do', 'while', 'if', ';', ':'
]=]

r = parse(s)
check(e, r)

-- arithmetic expressions

s = [=[
a = 3 / / 2
]=]
e = [=[
test.lua:1:9: syntax error, unexpected '/', expecting '(', 'Name', '{', 'function', '...', 'true', 'false', 'nil', 'String', 'Number', '#', '~', '-', 'not'
]=]

r = parse(s)
check(e, r)

-- bitwise expressions

s = [=[
b = 1 && 1
]=]
e = [=[
test.lua:1:8: syntax error, unexpected '&', expecting '(', 'Name', '{', 'function', '...', 'true', 'false', 'nil', 'String', 'Number', '#', '~', '-', 'not'
]=]

r = parse(s)
check(e, r)

s = [=[
b = 1 <> 0
]=]
e = [=[
test.lua:1:8: syntax error, unexpected '>', expecting '(', 'Name', '{', 'function', '...', 'true', 'false', 'nil', 'String', 'Number', '#', '~', '-', 'not'
]=]

r = parse(s)
check(e, r)

s = [=[
b = 1 < < 0
]=]
e = [=[
test.lua:1:9: syntax error, unexpected '<', expecting '(', 'Name', '{', 'function', '...', 'true', 'false', 'nil', 'String', 'Number', '#', '~', '-', 'not'
]=]

r = parse(s)
check(e, r)

-- break

s = [=[
break
]=]
e = [=[
test.lua:1:1: syntax error, <break> not inside a loop
]=]

r = parse(s)
check(e, r)

s = [=[
function f (x)
  if 1 then break end
end
]=]
e = [=[
test.lua:2:13: syntax error, <break> not inside a loop
]=]

r = parse(s)
check(e, r)

s = [=[
while 1 do
end
break
]=]
e = [=[
test.lua:3:1: syntax error, <break> not inside a loop
]=]

r = parse(s)
check(e, r)

-- concatenation expressions

s = [=[
concat2 = 2^3..1
]=]
e = [=[
test.lua:1:15: syntax error, unexpected '.1', expecting 'return', '(', 'Name', 'typealias', 'interface', 'goto', 'break', '::', 'local', 'function', 'const', 'repeat', 'for', 'do', 'while', 'if', ';', ',', 'or', 'and', '>', '<', '>=', '<=', '==', '~=', '|', '~', '&', '>>', '<<', '..', '-', '+', '%', '/', '//', '*', '^'
]=]

r = parse(s)
check(e, r)

-- for generic

s = [=[
for k;v in pairs(t) do end
]=]
e = [=[
test.lua:1:6: syntax error, unexpected ';', expecting 'in', ',', ':', '='
]=]

r = parse(s)
check(e, r)

s = [=[
for k,v in pairs(t:any) do end
]=]
e = [=[
test.lua:1:23: syntax error, unexpected ')', expecting 'String', '{', '('
]=]

r = parse(s)
check(e, r)

-- for numeric

s = [=[
for i=1,10, do end
]=]
e = [=[
test.lua:1:13: syntax error, unexpected 'do', expecting '(', 'Name', '{', 'function', '...', 'true', 'false', 'nil', 'String', 'Number', '#', '~', '-', 'not'
]=]

r = parse(s)
check(e, r)

s = [=[
for i=1,n:number do end
]=]
e = [=[
test.lua:1:18: syntax error, unexpected 'do', expecting 'String', '{', '('
]=]

r = parse(s)
check(e, r)

-- global functions

s = [=[
function func(a,b,c,) end
]=]
e = [=[
test.lua:1:21: syntax error, unexpected ')', expecting '...', 'Name'
]=]

r = parse(s)
check(e, r)

s = [=[
function func(...,a) end
]=]
e = [=[
test.lua:1:18: syntax error, unexpected ',', expecting ')', ':'
]=]

r = parse(s)
check(e, r)

s = [=[
function a.b:c:d () end
]=]
e = [=[
test.lua:1:15: syntax error, unexpected ':', expecting '('
]=]

r = parse(s)
check(e, r)

-- goto

s = [=[
:: label :: return
goto label
]=]
e = [=[
test.lua:2:1: syntax error, unexpected 'goto', expecting ';', '(', 'Name', '{', 'function', '...', 'true', 'false', 'nil', 'String', 'Number', '#', '~', '-', 'not'
]=]

r = parse(s)
check(e, r)

s = [=[
goto label
]=]
e = [=[
test.lua:1:1: syntax error, no visible label 'label' for <goto>
]=]

r = parse(s)
check(e, r)

s = [=[
goto label
::other_label::
]=]
e = [=[
test.lua:1:1: syntax error, no visible label 'label' for <goto>
]=]

r = parse(s)
check(e, r)

s = [=[
::other_label::
do do do goto label end end end
]=]
e = [=[
test.lua:2:10: syntax error, no visible label 'label' for <goto>
]=]

r = parse(s)
check(e, r)

-- if-else

s = [=[
if a then
]=]
e = [=[
test.lua:2:1: syntax error, unexpected 'EOF', expecting 'end', 'else', 'elseif', 'return', '(', 'Name', 'typealias', 'interface', 'goto', 'break', '::', 'local', 'function', 'const', 'repeat', 'for', 'do', 'while', 'if', ';'
]=]

r = parse(s)
check(e, r)

s = [=[
if a then else
]=]
e = [=[
test.lua:2:1: syntax error, unexpected 'EOF', expecting 'end', 'return', '(', 'Name', 'typealias', 'interface', 'goto', 'break', '::', 'local', 'function', 'const', 'repeat', 'for', 'do', 'while', 'if', ';'
]=]

r = parse(s)
check(e, r)

s = [=[
if a then
  return a
elseif b then
  return b
elseif

end
]=]
e = [=[
test.lua:7:1: syntax error, unexpected 'end', expecting '(', 'Name', '{', 'function', '...', 'true', 'false', 'nil', 'String', 'Number', '#', '~', '-', 'not'
]=]

r = parse(s)
check(e, r)

s = [=[
if a:any then else end
]=]
e = [=[
test.lua:1:10: syntax error, unexpected 'then', expecting 'String', '{', '('
]=]

r = parse(s)
check(e, r)

-- interfaces

s = [=[
local interface X
 x:number
 y:number
 z:number
 x:number
end
]=]
e = [=[
test.lua:1:7: syntax error, attempt to redeclare field 'x'
]=]

r = parse(s)
check(e, r)

s = [=[
local interface X
 x, y, z, x:number
end
]=]
e = [=[
test.lua:1:7: syntax error, attempt to redeclare field 'x'
]=]

r = parse(s)
check(e, r)

s = [=[
local interface boolean end
]=]
e = [=[
test.lua:1:7: syntax error, attempt to redeclare type 'boolean'
]=]

r = parse(s)
check(e, r)

s = [=[
local interface number end
]=]
e = [=[
test.lua:1:7: syntax error, attempt to redeclare type 'number'
]=]

r = parse(s)
check(e, r)

s = [=[
local interface string end
]=]
e = [=[
test.lua:1:7: syntax error, attempt to redeclare type 'string'
]=]

r = parse(s)
check(e, r)

s = [=[
local interface value end
]=]
e = [=[
test.lua:1:7: syntax error, attempt to redeclare type 'value'
]=]

r = parse(s)
check(e, r)

s = [=[
local interface any end
]=]
e = [=[
test.lua:1:7: syntax error, attempt to redeclare type 'any'
]=]

r = parse(s)
check(e, r)

s = [=[
local interface self end
]=]
e = [=[
test.lua:1:7: syntax error, attempt to redeclare type 'self'
]=]

r = parse(s)
check(e, r)

s = [=[
local interface const end
]=]
e = [=[
test.lua:1:7: syntax error, attempt to redeclare type 'const'
]=]

r = parse(s)
check(e, r)

-- labels

s = [=[
:: blah ::
:: not ::
]=]
e = [=[
test.lua:2:4: syntax error, unexpected 'not', expecting 'Name'
]=]

r = parse(s)
check(e, r)

s = [=[
::label::
::other_label::
::label::
]=]
e = [=[
test.lua:3:1: syntax error, label 'label' already defined
]=]

r = parse(s)
check(e, r)

-- locals

s = [=[
local a =
]=]
e = [=[
test.lua:2:1: syntax error, unexpected 'EOF', expecting '(', 'Name', '{', 'function', '...', 'true', 'false', 'nil', 'String', 'Number', '#', '~', '-', 'not'
]=]

r = parse(s)
check(e, r)

s = [=[
local function t.a() end
]=]
e = [=[
test.lua:1:17: syntax error, unexpected '.', expecting '('
]=]

r = parse(s)
check(e, r)

s = [=[
local function test (a,) end
]=]
e = [=[
test.lua:1:24: syntax error, unexpected ')', expecting '...', 'Name'
]=]

r = parse(s)
check(e, r)

s = [=[
local function test(...,a) end
]=]
e = [=[
test.lua:1:24: syntax error, unexpected ',', expecting ')', ':'
]=]

r = parse(s)
check(e, r)

s = [=[
local function (a, b, c, ...) end
]=]
e = [=[
test.lua:1:16: syntax error, unexpected '(', expecting 'Name'
]=]

r = parse(s)
check(e, r)

-- repeat

s = [=[
repeat
  a,b,c = 1+1,2+2,3+3
  break
]=]
e = [=[
test.lua:4:1: syntax error, unexpected 'EOF', expecting 'until', 'return', '(', 'Name', 'typealias', 'interface', 'goto', 'break', '::', 'local', 'function', 'const', 'repeat', 'for', 'do', 'while', 'if', ';'
]=]

r = parse(s)
check(e, r)

-- return

s = [=[
return
return 1
return 1,1-2*3+4,"alo"
return;
return 1;
return 1,1-2*3+4,"alo";
]=]
e = [=[
test.lua:2:1: syntax error, unexpected 'return', expecting ';', '(', 'Name', '{', 'function', '...', 'true', 'false', 'nil', 'String', 'Number', '#', '~', '-', 'not'
]=]

r = parse(s)
check(e, r)

-- tables

s = [=[
t = { , }
]=]
e = [=[
test.lua:1:7: syntax error, unexpected ',', expecting '}', '(', '{', 'function', '...', 'true', 'false', 'nil', 'String', 'Number', '#', '~', '-', 'not', 'Name', '[', 'const'
]=]

r = parse(s)
check(e, r)

-- vararg

s = [=[
function f ()
  return ...
end
]=]
e = [=[
test.lua:2:10: syntax error, cannot use '...' outside a vararg function
]=]

r = parse(s)
check(e, r)

s = [=[
function f ()
  function g (x, y)
    return ...,...,...
  end
end
]=]
e = [=[
test.lua:3:12: syntax error, cannot use '...' outside a vararg function
]=]

r = parse(s)
check(e, r)

s = [=[
local function f (x)
  return ...
end
]=]
e = [=[
test.lua:2:10: syntax error, cannot use '...' outside a vararg function
]=]

r = parse(s)
check(e, r)

s = [=[
local f = function (x)
  return ...
end
]=]
e = [=[
test.lua:2:10: syntax error, cannot use '...' outside a vararg function
]=]

r = parse(s)
check(e, r)

-- while

s = [=[
i = 0
while (i < 10)
  i = i + 1
end
]=]
e = [=[
test.lua:3:3: syntax error, unexpected 'i', expecting 'do', 'or', 'and', '>', '<', '>=', '<=', '==', '~=', '|', '~', '&', '>>', '<<', '..', '-', '+', '%', '/', '//', '*', '^', 'String', '{', '(', ':', '[', '.'
]=]

r = parse(s)
check(e, r)

-- type annotations

s = [=[
t[x:any] = 1
]=]
e = [=[
test.lua:1:8: syntax error, unexpected ']', expecting 'String', '{', '('
]=]

r = parse(s)
check(e, r)

s = [=[
x:number, y, z:boolean = 1, nil, true
]=]
e = [=[
test.lua:1:9: syntax error, unexpected ',', expecting 'String', '{', '('
]=]

r = parse(s)
check(e, r)

s = [=[
x = x:any
]=]
e = [=[
test.lua:2:1: syntax error, unexpected 'EOF', expecting 'String', '{', '('
]=]

r = parse(s)
check(e, r)

s = [=[
x = ...:any
]=]
e = [=[
test.lua:1:8: syntax error, unexpected ':', expecting 'return', '(', 'Name', 'typealias', 'interface', 'goto', 'break', '::', 'local', 'function', 'const', 'repeat', 'for', 'do', 'while', 'if', ';', ',', 'or', 'and', '>', '<', '>=', '<=', '==', '~=', '|', '~', '&', '>>', '<<', '..', '-', '+', '%', '/', '//', '*', '^'
]=]

r = parse(s)
check(e, r)

s = [=[
f(x:any)
]=]
e = [=[
test.lua:1:8: syntax error, unexpected ')', expecting 'String', '{', '('
]=]

r = parse(s)
check(e, r)

s = [=[
f(...:any)
]=]
e = [=[
test.lua:1:6: syntax error, unexpected ':', expecting ')', ',', 'or', 'and', '>', '<', '>=', '<=', '==', '~=', '|', '~', '&', '>>', '<<', '..', '-', '+', '%', '/', '//', '*', '^'
]=]

r = parse(s)
check(e, r)

s = [=[
local x:number*
]=]
e = [=[
test.lua:1:15: syntax error, unexpected '*', expecting 'return', '(', 'Name', 'typealias', 'interface', 'goto', 'break', '::', 'local', 'function', 'const', 'repeat', 'for', 'do', 'while', 'if', ';', '=', ',', '?', '|'
]=]

r = parse(s)
check(e, r)

s = [=[
local x:number|
]=]
e = [=[
test.lua:2:1: syntax error, unexpected 'EOF', expecting '{', '(', 'Type'
]=]

r = parse(s)
check(e, r)

s = [=[
local x:number?|string?
]=]
e = [=[
test.lua:1:16: syntax error, unexpected '|', expecting 'return', '(', 'Name', 'typealias', 'interface', 'goto', 'break', '::', 'local', 'function', 'const', 'repeat', 'for', 'do', 'while', 'if', ';', '=', ','
]=]

r = parse(s)
check(e, r)

s = [=[
local x:() -> number
]=]
e = [=[
test.lua:1:15: syntax error, unexpected 'number', expecting '('
]=]

r = parse(s)
check(e, r)

s = [=[
local x:() -> (number)? | (string)?
]=]
e = [=[
test.lua:1:35: syntax error, unexpected '?', expecting '->'
]=]

r = parse(s)
check(e, r)

s = [=[
local x:{()->():string}
]=]
e = [=[
test.lua:1:16: syntax error, unexpected ':', expecting '}', '?', '|'
]=]

r = parse(s)
check(e, r)

s = [=[
local x:{string:t 1}
]=]
e = [=[
test.lua:1:19: syntax error, unexpected '1', expecting '}', '?', '|'
]=]

r = parse(s)
check(e, r)

s = [=[
local x:{{{{{}}}}
]=]
e = [=[
test.lua:2:1: syntax error, unexpected 'EOF', expecting '}', '?', '|'
]=]

r = parse(s)
check(e, r)

print("> testing types...")

-- literal types

local False = tltype.False()
local True = tltype.True()
local Double = tltype.Literal(1.1)
local Integer = tltype.Literal(1)
local Word = tltype.Literal("w")

-- base types

local Boolean = tltype.Boolean()
local Number = tltype.Number()
local String = tltype.String()

-- nil type

local Nil = tltype.Nil()

-- top type

local Value = tltype.Value()

-- dynamic type

local Any = tltype.Any()

-- test types
local t, t1, t2, t3, t4

-- type equality

check(tltype.isLiteral(False))
check(tltype.isFalse(False))

check(tltype.isLiteral(True))
check(tltype.isTrue(True))

check(tltype.isLiteral(Double))
check(tltype.isNum(Double))

check(tltype.isLiteral(Integer))
check(tltype.isNum(Integer))

check(tltype.isLiteral(Word))
check(tltype.isStr(Word))

check(tltype.isBase(Boolean))
check(tltype.isBoolean(Boolean))

check(tltype.isBase(Number))
check(tltype.isNumber(Number))

check(tltype.isBase(String))
check(tltype.isString(String))

check(tltype.isNil(Nil))

check(tltype.isValue(Value))

check(tltype.isAny(Any))

check(tltype.isUnion(tltype.Union(Number,Nil)))
check(tltype.isUnion(tltype.Union(tltype.Union(Number,String),Nil)))
check(tltype.isUnion(tltype.Union(tltype.Union(Number,Nil),String)))
check(tltype.isUnion(tltype.Union(tltype.Union(Nil,Number),String)))

check(tltype.isUnion(tltype.Union(Number,Nil),Nil))
check(tltype.isUnion(tltype.Union(Number,String,Nil),Nil))
check(tltype.isUnion(tltype.Union(Number,Nil,String),Nil))
check(tltype.isUnion(tltype.Union(Nil,Number,String),Nil))

check(not tltype.isUnion(tltype.Union(Number,Boolean),Nil))
check(not tltype.isUnion(tltype.Union(tltype.Union(Number,String),Boolean),Nil))
check(not tltype.isUnion(tltype.Union(tltype.Union(Number,Boolean),String),Nil))
check(not tltype.isUnion(tltype.Union(tltype.Union(Boolean,Number),String),Nil))

t1 = tltype.Vararg(Value)
t2 = tltype.Vararg(Nil)

check(tltype.isFunction(tltype.Function(tltype.Tuple(t1), tltype.Tuple(t2))))
check(tltype.isTuple(tltype.Tuple(t1)))
check(tltype.isTuple(tltype.Tuple(t2)))
check(tltype.isVararg(t1))
check(tltype.isVararg(t2))

check(not tltype.isFunction(Nil))
check(not tltype.isTuple(t1))
check(not tltype.isTuple(t2))
check(not tltype.isVararg(tltype.Tuple(t1)))
check(not tltype.isVararg(tltype.Tuple(t2)))

-- subtyping

check(tltype.subtype(False,False))
check(tltype.subtype(True,True))
check(tltype.subtype(Double,Double))
check(tltype.subtype(Integer,Integer))
check(tltype.subtype(Word,Word))

check(tltype.subtype(False,Boolean))
check(tltype.subtype(True,Boolean))
check(tltype.subtype(Double,Number))
check(tltype.subtype(Integer,Number))
check(tltype.subtype(Word,String))

check(not tltype.subtype(Nil,False))
check(not tltype.subtype(False,True))
check(not tltype.subtype(True,Double))
check(not tltype.subtype(Double,Integer))
check(not tltype.subtype(Integer,Word))
check(not tltype.subtype(Word,Nil))

check(tltype.subtype(Nil,Nil))
check(tltype.subtype(Boolean,Boolean))
check(tltype.subtype(Number,Number))
check(tltype.subtype(String,String))

check(not tltype.subtype(Boolean,False))
check(not tltype.subtype(Boolean,True))
check(not tltype.subtype(Number,Double))
check(not tltype.subtype(Number,Integer))
check(not tltype.subtype(String,Word))

check(tltype.subtype(False,Value))
check(tltype.subtype(True,Value))
check(tltype.subtype(Double,Value))
check(tltype.subtype(Integer,Value))
check(tltype.subtype(Word,Value))
check(tltype.subtype(Nil,Value))
check(tltype.subtype(Boolean,Value))
check(tltype.subtype(Number,Value))
check(tltype.subtype(String,Value))
check(tltype.subtype(Value,Value))
check(tltype.subtype(Any,Value))
check(tltype.subtype(tltype.Union(Number,Nil),Value))

check(not tltype.subtype(Value,False))
check(not tltype.subtype(Value,True))
check(not tltype.subtype(Value,Double))
check(not tltype.subtype(Value,Integer))
check(not tltype.subtype(Value,Word))
check(not tltype.subtype(Value,Nil))
check(not tltype.subtype(Value,Boolean))
check(not tltype.subtype(Value,Number))
check(not tltype.subtype(Value,String))
check(not tltype.subtype(Value,Any))
check(not tltype.subtype(Value,tltype.Union(Number,Nil)))

check(tltype.subtype(Any,Any))

check(not tltype.subtype(Nil,Any))
check(not tltype.subtype(False,Any))
check(not tltype.subtype(True,Any))
check(not tltype.subtype(Double,Any))
check(not tltype.subtype(Integer,Any))
check(not tltype.subtype(Word,Any))

check(not tltype.subtype(Boolean,Any))
check(not tltype.subtype(Number,Any))
check(not tltype.subtype(String,Any))

check(not tltype.subtype(Any,Nil))
check(not tltype.subtype(Any,False))
check(not tltype.subtype(Any,True))
check(not tltype.subtype(Any,Double))
check(not tltype.subtype(Any,Integer))
check(not tltype.subtype(Any,Word))

check(not tltype.subtype(Any,Boolean))
check(not tltype.subtype(Any,Number))
check(not tltype.subtype(Any,String))

t = tltype.Union(Number,Nil)

check(tltype.subtype(Number,t))
check(tltype.subtype(Nil,t))
check(tltype.subtype(t,t))

check(not tltype.subtype(t,Number))
check(not tltype.subtype(t,Nil))

t = tltype.Union(Number,Any)

check(tltype.subtype(Any,t))
check(tltype.subtype(t,Any))
check(tltype.subtype(t,t))

check(not tltype.subtype(String,t))
check(not tltype.subtype(Number,t))

t1 = tltype.Recursive("Element",
  tltype.Table(tltype.Field(false, tltype.Literal("info"), tltype.Number()),
               tltype.Field(false, tltype.Literal("next"), tltype.Union(tltype.Variable("Element", tltype.Nil())))))
t2 = tltype.Recursive("List",
  tltype.Table(tltype.Field(false, tltype.Literal("info"), tltype.Number()),
               tltype.Field(false, tltype.Literal("next"), tltype.Union(tltype.Variable("List", tltype.Nil())))))
t3 = tltype.Recursive("Node",
  tltype.Table(tltype.Field(false, tltype.Literal("info"), tltype.Number()),
               tltype.Field(false, tltype.Literal("left"), tltype.Union(tltype.Variable("Node", tltype.Nil()))),
               tltype.Field(false, tltype.Literal("right"), tltype.Union(tltype.Variable("Node", tltype.Nil())))))
t4 = tltype.Recursive("Tree",
  tltype.Table(tltype.Field(false, tltype.Literal("info"), tltype.Number()),
               tltype.Field(false, tltype.Literal("left"), tltype.Union(tltype.Variable("Tree", tltype.Nil()))),
               tltype.Field(false, tltype.Literal("right"), tltype.Union(tltype.Variable("Tree", tltype.Nil())))))

check(tltype.subtype( t1, t1))
check(tltype.subtype(t2, t2))
check(tltype.subtype(t3, t3))
check(tltype.subtype(t4, t4))

check(not tltype.subtype(t1, t3))
check(not tltype.subtype(t1, t4))
check(not tltype.subtype(t2, t3))
check(not tltype.subtype(t2, t4))

t1 = tltype.Tuple({ Value }, true)
t2 = tltype.Tuple({ Nil }, true)

check(tltype.subtype(tltype.Function(t1,t2), tltype.Function(t1,t2)))
check(tltype.subtype(tltype.Function(t1,t2), tltype.Function(t2,t1)))
check(tltype.subtype(tltype.Function(t2,t1), tltype.Function(t2,t1)))
check(not tltype.subtype(tltype.Function(t2,t1), tltype.Function(t1,t2)))

t1 = tltype.Tuple({ Number }, true)

check(tltype.subtype(tltype.Function(t1,t2), tltype.Function(t1,t2)))
check(tltype.subtype(tltype.Function(t1,t2), tltype.Function(t2,t1)))
check(tltype.subtype(tltype.Function(t2,t1), tltype.Function(t2,t1)))
check(not tltype.subtype(tltype.Function(t2,t1), tltype.Function(t1,t2)))

t3 = tltype.Vararg(Nil)
t4 = tltype.Tuple({ Number, Number, t3 })

check(tltype.subtype(tltype.Function(t1,t2), tltype.Function(t2,t2)))
check(not tltype.subtype(tltype.Function(t3,t2), tltype.Function(t1,t2)))

t3 = tltype.Vararg(Number)
t4 = tltype.Tuple({ Number, Number, Number, t3 })

check(tltype.subtype(tltype.Function(t1,t1), tltype.Function(t4,t1)))
check(not tltype.subtype(tltype.Function(t4,t1), tltype.Function(t1,t1)))

-- consistent-subtyping

check(tltype.consistent_subtype(False,False))
check(tltype.consistent_subtype(True,True))
check(tltype.consistent_subtype(Double,Double))
check(tltype.consistent_subtype(Integer,Integer))
check(tltype.consistent_subtype(Word,Word))

check(tltype.consistent_subtype(False,Boolean))
check(tltype.consistent_subtype(True,Boolean))
check(tltype.consistent_subtype(Double,Number))
check(tltype.consistent_subtype(Integer,Number))
check(tltype.consistent_subtype(Word,String))

check(not tltype.consistent_subtype(Nil,False))
check(not tltype.consistent_subtype(False,True))
check(not tltype.consistent_subtype(True,Double))
check(not tltype.consistent_subtype(Double,Integer))
check(not tltype.consistent_subtype(Integer,Word))
check(not tltype.consistent_subtype(Word,Nil))

check(tltype.consistent_subtype(Nil,Nil))
check(tltype.consistent_subtype(Boolean,Boolean))
check(tltype.consistent_subtype(Number,Number))
check(tltype.consistent_subtype(String,String))

check(not tltype.consistent_subtype(Boolean,False))
check(not tltype.consistent_subtype(Boolean,True))
check(not tltype.consistent_subtype(Number,Double))
check(not tltype.consistent_subtype(Number,Integer))
check(not tltype.consistent_subtype(String,Word))

check(tltype.consistent_subtype(False,Value))
check(tltype.consistent_subtype(True,Value))
check(tltype.consistent_subtype(Double,Value))
check(tltype.consistent_subtype(Integer,Value))
check(tltype.consistent_subtype(Word,Value))
check(tltype.consistent_subtype(Nil,Value))
check(tltype.consistent_subtype(Boolean,Value))
check(tltype.consistent_subtype(Number,Value))
check(tltype.consistent_subtype(String,Value))
check(tltype.consistent_subtype(Value,Value))
check(tltype.consistent_subtype(tltype.Union(Number,Nil),Value))

check(not tltype.consistent_subtype(Value,False))
check(not tltype.consistent_subtype(Value,True))
check(not tltype.consistent_subtype(Value,Double))
check(not tltype.consistent_subtype(Value,Integer))
check(not tltype.consistent_subtype(Value,Word))
check(not tltype.consistent_subtype(Value,Nil))
check(not tltype.consistent_subtype(Value,Boolean))
check(not tltype.consistent_subtype(Value,Number))
check(not tltype.consistent_subtype(Value,String))
check(not tltype.consistent_subtype(Value,tltype.Union(Number,Nil)))

check(tltype.consistent_subtype(Any,Any))

check(tltype.consistent_subtype(Any,Value))
check(tltype.consistent_subtype(Value,Any))

check(tltype.consistent_subtype(Nil,Any))
check(tltype.consistent_subtype(False,Any))
check(tltype.consistent_subtype(True,Any))
check(tltype.consistent_subtype(Double,Any))
check(tltype.consistent_subtype(Integer,Any))
check(tltype.consistent_subtype(Word,Any))

check(tltype.consistent_subtype(Boolean,Any))
check(tltype.consistent_subtype(Number,Any))
check(tltype.consistent_subtype(String,Any))

check(tltype.consistent_subtype(Any,Nil))
check(tltype.consistent_subtype(Any,False))
check(tltype.consistent_subtype(Any,True))
check(tltype.consistent_subtype(Any,Double))
check(tltype.consistent_subtype(Any,Integer))
check(tltype.consistent_subtype(Any,Word))

check(tltype.consistent_subtype(Any,Boolean))
check(tltype.consistent_subtype(Any,Number))
check(tltype.consistent_subtype(Any,String))

t = tltype.Union(Number,Nil)

check(tltype.consistent_subtype(Number,t))
check(tltype.consistent_subtype(Nil,t))
check(tltype.consistent_subtype(t,t))

check(not tltype.consistent_subtype(t,Number))
check(not tltype.consistent_subtype(t,Nil))

t = tltype.Union(Number,Any)

check(tltype.consistent_subtype(Number,t))
check(tltype.consistent_subtype(Any,t))
check(tltype.consistent_subtype(t,t))
check(tltype.consistent_subtype(String,t))
check(tltype.consistent_subtype(t,Any))
check(tltype.consistent_subtype(t,String))

t1 = tltype.Tuple({ Any }, true)
t2 = tltype.Tuple({ Nil }, true)

check(tltype.consistent_subtype(tltype.Function(t1,t2), tltype.Function(t1,t2)))
check(tltype.consistent_subtype(tltype.Function(t1,t2), tltype.Function(t2,t1)))
check(tltype.consistent_subtype(tltype.Function(t2,t1), tltype.Function(t2,t1)))
check(tltype.consistent_subtype(tltype.Function(t2,t1), tltype.Function(t1,t2)))

t2 = tltype.Tuple({ Number }, true)

check(tltype.consistent_subtype(tltype.Function(t1,t2), tltype.Function(t1,t2)))
check(tltype.consistent_subtype(tltype.Function(t1,t2), tltype.Function(t2,t1)))
check(tltype.consistent_subtype(tltype.Function(t2,t1), tltype.Function(t2,t1)))
check(tltype.consistent_subtype(tltype.Function(t2,t1), tltype.Function(t1,t2)))

t3 = tltype.Vararg(Any)
t4 = tltype.Tuple({ Any, Any, t3 }, false)

check(tltype.consistent_subtype(tltype.Function(t1,t2), tltype.Function(t2,t1)))
check(tltype.consistent_subtype(tltype.Function(t4,t2), tltype.Function(t1,t2)))
check(tltype.consistent_subtype(tltype.Function(t1,t2), tltype.Function(t4,t2)))

print("> testing type checker...")

-- type check

s = [=[
local x:value, y:value, z:value = 1, "foo"
]=]
e = [=[
{ `Local{ { `Id "x":`TValue, `Id "y":`TValue, `Id "z":`TValue }, { `Number "1", `String "foo" } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x, y, z = 1, "foo", false
]=]
e = [=[
{ `Local{ { `Id "x", `Id "y", `Id "z" }, { `Number "1", `String "foo", `False } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x:number, y:string, z:boolean = 1, "foo", false
]=]
e = [=[
{ `Local{ { `Id "x":`TBase number, `Id "y":`TBase string, `Id "z":`TBase boolean }, { `Number "1", `String "foo", `False } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x:boolean, y:nil = true, nil
]=]
e = [=[
{ `Local{ { `Id "x":`TBase boolean, `Id "y":`TNil }, { `True, `Nil } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x:number?, y:number|nil = 1
]=]
e = [=[
{ `Local{ { `Id "x":`TUnion{ `TBase number, `TNil }, `Id "y":`TUnion{ `TBase number, `TNil } }, { `Number "1" } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x:number = 1 + 1
]=]
e = [=[
{ `Local{ { `Id "x":`TBase number }, { `Op{ "add", `Number "1", `Number "1" } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x:string = "hello" .. "world"
]=]
e = [=[
{ `Local{ { `Id "x":`TBase string }, { `Op{ "concat", `String "hello", `String "world" } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x:boolean, y:boolean = nil == false, false == true
]=]
e = [=[
{ `Local{ { `Id "x":`TBase boolean, `Id "y":`TBase boolean }, { `Op{ "eq", `Nil, `False }, `Op{ "eq", `False, `True } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x:boolean, y:boolean = 1 == 2, "foo" == "bar"
]=]
e = [=[
{ `Local{ { `Id "x":`TBase boolean, `Id "y":`TBase boolean }, { `Op{ "eq", `Number "1", `Number "2" }, `Op{ "eq", `String "foo", `String "bar" } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x:boolean, y:boolean = 1 < 2, "foo" < "bar"
]=]
e = [=[
{ `Local{ { `Id "x":`TBase boolean, `Id "y":`TBase boolean }, { `Op{ "lt", `Number "1", `Number "2" }, `Op{ "lt", `String "foo", `String "bar" } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x:nil, y:boolean = nil and 1, false and 1
]=]
e = [=[
{ `Local{ { `Id "x":`TNil, `Id "y":`TBase boolean }, { `Op{ "and", `Nil, `Number "1" }, `Op{ "and", `False, `Number "1" } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x:number, y:string? = 1 and 2, "foo" and nil
]=]
e = [=[
{ `Local{ { `Id "x":`TBase number, `Id "y":`TUnion{ `TBase string, `TNil } }, { `Op{ "and", `Number "1", `Number "2" }, `Op{ "and", `String "foo", `Nil } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x:number, y:number = nil or 1, false or 1
]=]
e = [=[
{ `Local{ { `Id "x":`TBase number, `Id "y":`TBase number }, { `Op{ "or", `Nil, `Number "1" }, `Op{ "or", `False, `Number "1" } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x:number, y:string? = 1 or 2, "foo" or nil
]=]
e = [=[
{ `Local{ { `Id "x":`TBase number, `Id "y":`TUnion{ `TBase string, `TNil } }, { `Op{ "or", `Number "1", `Number "2" }, `Op{ "or", `String "foo", `Nil } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x:number?
local y:number = x or 0
]=]
e = [=[
{ `Local{ { `Id "x":`TUnion{ `TBase number, `TNil } }, {  } }, `Local{ { `Id "y":`TBase number }, { `Op{ "or", `Id "x", `Number "0" } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x:boolean, y:boolean = not nil, not false
]=]
e = [=[
{ `Local{ { `Id "x":`TBase boolean, `Id "y":`TBase boolean }, { `Op{ "not", `Nil }, `Op{ "not", `False } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x:number = -1
]=]
e = [=[
{ `Local{ { `Id "x":`TBase number }, { `Op{ "unm", `Number "1" } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x:number = #"foo"
]=]
e = [=[
{ `Local{ { `Id "x":`TBase number }, { `Op{ "len", `String "foo" } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
while 1 do break end
]=]
e = [=[
{ `While{ `Number "1", { `Break } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
repeat break until 1
]=]
e = [=[
{ `Repeat{ { `Break }, `Number "1" } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
if 1 then local x = 1 end
]=]
e = [=[
{ `If{ `Number "1", { `Local{ { `Id "x" }, { `Number "1" } } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
if 1 then local x = 1 else local x = "foo" end
]=]
e = [=[
{ `If{ `Number "1", { `Local{ { `Id "x" }, { `Number "1" } } }, { `Local{ { `Id "x" }, { `String "foo" } } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
if 1 then
  local x = 1
elseif 2 then
  local x = 2
elseif 3 then
  local x = 3
elseif 4 then
  local x = 4
else
  local x = "foo"
end
]=]
e = [=[
{ `If{ `Number "1", { `Local{ { `Id "x" }, { `Number "1" } } }, `Number "2", { `Local{ { `Id "x" }, { `Number "2" } } }, `Number "3", { `Local{ { `Id "x" }, { `Number "3" } } }, `Number "4", { `Local{ { `Id "x" }, { `Number "4" } } }, { `Local{ { `Id "x" }, { `String "foo" } } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
function f(x:number?)
if x then
  x = x + 1
else
  print("x is nil")
end
end
]=]
e = [=[
{ `Set{{ `Index{ `Id "_ENV", `String "f" } },{ `Function{{ `Id "x":`TUnion{ `TBase number, `TNil } },{ `If{ `Id "x",{ `Set{{ `Id "x" },{ `Op{"add", `Id "x", `Number "1" } } } },{ `Call{ `Index{ `Id "_ENV", `String "print" }, `String "x is nil" } } } } } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
function f(x:number?)
if not x then
  print("x is nil")
else
  x = x + 1
end
end
]=]
e = [=[
{ `Set{{ `Index{ `Id "_ENV", `String "f" } },{ `Function{{ `Id "x":`TUnion{ `TBase number, `TNil } },{ `If{ `Op{"not", `Id "x" },{ `Call{ `Index{ `Id "_ENV", `String "print" }, `String "x is nil" } },{ `Set{{ `Id "x" },{ `Op{"add", `Id "x", `Number "1" } } } } } } } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
function f(x:number?)
if type(x) == "number" then
  x = x + 1
else
  print("x is nil")
end
end
]=]
e = [=[
{ `Set{{ `Index{ `Id "_ENV", `String "f" } },{ `Function{{ `Id "x":`TUnion{ `TBase number, `TNil } },{ `If{ `Op{"eq", `Call{ `Index{ `Id "_ENV", `String "type" }, `Id "x" }, `String "number" },{ `Set{{ `Id "x" },{ `Op{"add", `Id "x", `Number "1" } } } },{ `Call{ `Index{ `Id "_ENV", `String "print" }, `String "x is nil" } } } } } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
function f(x:number?)
if type(x) ~= "number" then
  print("x is nil")
else
  x = x + 1
end
end
]=]
e = [=[
{ `Set{{ `Index{ `Id "_ENV", `String "f" } },{ `Function{{ `Id "x":`TUnion{ `TBase number, `TNil } },{ `If{ `Op{"not", `Op{"eq", `Call{ `Index{ `Id "_ENV", `String "type" },
 `Id "x" }, `String "number" } },{ `Call{ `Index{ `Id "_ENV", `String "print" }, `String "x is nil" } },{ `Set{{ `Id "x" },{ `Op{"add", `Id "x", `Number "1" } } } } } } } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local function f(x:number|string?)
local y = x
if type(x) == "number" then
  x = x + 1
elseif type(y) == "string" then
  y = y .. "hello"
elseif type(x) == "string" then
  x = x .. "hello"
elseif type(y) == "number" then
  y = y + 1
end
x = y
y = x
end
]=]
e = [=[
{ `Localrec{{ `Id "f":`TFunction{ `TTuple{ `TUnion{ `TBase number, `TBase string, `TNil }, `TVararg{ `TValue } }, `TTuple{ `TVararg{ `TNil } } } },{ `Function{{ `Id "x":`TUnion{ `TBase number, `TBase string, `TNil } },{ `Local{{ `Id "y" },{ `Id "x" } }, `If{ `Op{"eq", `Call{ `Index{ `Id "_ENV", `String "type" }, `Id "x" }, `String "number" },{ `Set{{ `Id "x" },{ `Op{"add", `Id "x", `Number "1" } } } }, `Op{"eq", `Call{ `Index{ `Id "_ENV", `String "type" }, `Id "y" }, `String "string" },{ `Set{{ `Id "y" },{ `Op{"concat", `Id "y", `String "hello" } } } }, `Op{"eq", `Call{ `Index{ `Id "_ENV", `String "type" }, `Id "x" }, `String "string" },{ `Set{{ `Id "x" },{ `Op{"concat", `Id "x", `String "hello" } } } }, `Op{"eq", `Call{ `Index{ `Id "_ENV", `String "type" }, `Id "y" }, `String "number" },{ `Set{{ `Id "y" },{ `Op{"add", `Id "y", `Number "1" } } } } }, `Set{{ `Id "x" },{ `Id "y" } }, `Set{{ `Id "y" },{ `Id "x" } } } } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x:number|string?
local y = x
if type(x) == "nil" then
  print("x is nil")
elseif type(y) == "nil" then
  print("y is nil")
elseif type(x) == "string" then
  x = x .. "hello"
elseif type(y) == "number" then
  y = y + 1
else
  x = x + 1
  y = y .. "hello"
end
x = y
y = x
]=]
e = [=[
{ `Local{ { `Id "x":`TUnion{ `TBase number, `TBase string, `TNil } }, {  } }, `Local{ { `Id "y" }, { `Id "x" } }, `If{ `Op{ "eq", `Call{ `Index{ `Id "_ENV", `String "type" }, `Id "x" }, `String "nil" }, { `Call{ `Index{ `Id "_ENV", `String "print" }, `String "x is nil" } }, `Op{ "eq", `Call{ `Index{ `Id "_ENV", `String "type" }, `Id "y" }, `String "nil" }, { `Call{ `Index{ `Id "_ENV", `String "print" }, `String "y is nil" } }, `Op{ "eq", `Call{ `Index{ `Id "_ENV", `String "type" }, `Id "x" }, `String "string" }, { `Set{ { `Id "x" }, { `Op{ "concat", `Id "x", `String "hello" } } } }, `Op{ "eq", `Call{ `Index{ `Id "_ENV", `String "type" }, `Id "y" }, `String "number" }, { `Set{ { `Id "y" }, { `Op{ "add", `Id "y", `Number "1" } } } }, { `Set{ { `Id "x" }, { `Op{ "add", `Id "x", `Number "1" } } }, `Set{ { `Id "y" }, { `Op{ "concat", `Id "y", `String "hello" } } } } }, `Set{ { `Id "x" }, { `Id "y" } }, `Set{ { `Id "y" }, { `Id "x" } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
for i = 1, 10 do local x = i end
]=]
e = [=[
{ `Fornum{ `Id "i", `Number "1", `Number "10", { `Local{ { `Id "x" }, { `Id "i" } } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
for i = 10, 1, -1 do local x = i end
]=]
e = [=[
{ `Fornum{ `Id "i", `Number "10", `Number "1", `Op{ "unm", `Number "1" }, { `Local{ { `Id "x" }, { `Id "i" } } } } }
]=]

r = typecheck(s)
check(e, r)

-- do not type check

s = [=[
local v:value
local a:boolean, b:number, c:string = v, v, v
]=]
e = [=[
test.lua:2:7: type error, attempt to assign 'value' to 'boolean'
test.lua:2:18: type error, attempt to assign 'value' to 'number'
test.lua:2:28: type error, attempt to assign 'value' to 'string'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x:boolean, y:boolean, z:number = 1, "foo"
z = 10
z = nil
]=]
e = [=[
test.lua:1:7: type error, attempt to assign '1' to 'boolean'
test.lua:1:18: type error, attempt to assign '"foo"' to 'boolean'
test.lua:3:1: type error, attempt to assign '(nil)' to '(number,value*)'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x:number, y:number, z:string = false, true
z = "foo"
z = nil
]=]
e = [=[
test.lua:1:7: type error, attempt to assign 'false' to 'number'
test.lua:1:17: type error, attempt to assign 'true' to 'number'
test.lua:3:1: type error, attempt to assign '(nil)' to '(string,value*)'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x, y = 1 + "foo", "foo" + 1
local a:any
a = a + 1
a = 1 + a
]=]
e = [=[
test.lua:1:18: type error, attempt to perform arithmetic on a 'string'
test.lua:1:25: type error, attempt to perform arithmetic on a 'string'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x, y = "foo" .. 1, 1 .. "foo"
local a:any
a = a .. "foo"
a = "foo" .. a
]=]
e = [=[
test.lua:1:23: type error, attempt to concatenate a 'number'
test.lua:1:26: type error, attempt to concatenate a 'number'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x, y = 1 < "foo", "foo" < 1
local a:any
a = a < 1
a = 1 < a
]=]
e = [=[
test.lua:1:14: type error, attempt to compare 'number' with 'string'
test.lua:1:25: type error, attempt to compare 'string' with 'number'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x, y = nil < 1, true < "false"
]=]
e = [=[
test.lua:1:14: type error, attempt to compare 'nil' with 'number'
test.lua:1:23: type error, attempt to compare 'boolean' with 'string'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x:number, y:number = nil and 1, false and 1
local a:number?, b:number|false = 1, 1
a = a and 1
b = b and 1
]=]
e = [=[
test.lua:1:17: type error, attempt to assign 'false' to 'number'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x:string, y:number|string = 1 and 2, "foo" and nil
]=]
e = [=[
test.lua:1:7: type error, attempt to assign '1 | 2' to 'string'
test.lua:1:17: type error, attempt to assign '"foo"?' to 'number | string'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x:nil, y:boolean = nil or 1, false or 1
local a:number?, b:number|false = 1, 1
a = a or 1
b = b or 1
]=]
e = [=[
test.lua:1:7: type error, attempt to assign '1' to 'nil'
test.lua:1:14: type error, attempt to assign '1' to 'boolean'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x:string, y:number|string = 1 or 2, "foo" or nil
]=]
e = [=[
test.lua:1:7: type error, attempt to assign '1 | 2' to 'string'
test.lua:1:17: type error, attempt to assign '"foo"?' to 'number | string'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x:number?
local y:number = x or nil
y = 10
y = nil
]=]
e = [=[
test.lua:4:1: type error, attempt to assign '(nil)'to '(number,value*)'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x:number, y:string = not nil, not false
]=]
e = [=[
test.lua:1:7: type error, attempt to assign 'boolean' to 'number'
test.lua:1:17: type error, attempt to assign 'boolean' to 'string'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x:number, y:string = not 1, not "foo"
]=]
e = [=[
test.lua:1:7: type error, attempt to assign 'boolean' to 'number'
test.lua:1:17: type error, attempt to assign 'boolean' to 'string'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x = -"foo"
local a:any
a = -a
]=]
e = [=[
test.lua:1:12: type error, attempt to perform arithmetic on a 'string'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x = #1
local a:any
a = #a
]=]
e = [=[
test.lua:1:12: type error, attempt to get length of a 'number'
]=]

r = typecheck(s)
check(e, r)

s = [=[
while 1 + "foo" do break end
]=]
e = [=[
test.lua:1:11: type error, attempt to perform arithmetic on a 'string'
]=]

r = typecheck(s)
check(e, r)

s = [=[
repeat break until 1 + nil
]=]
e = [=[
test.lua:1:24: type error, attempt to perform arithmetic on a 'nil'
]=]

r = typecheck(s)
check(e, r)

s = [=[
if 1 then local x:string = 1 end
]=]
e = [=[
test.lua:1:17: type error, attempt to assign '1' to 'string'
]=]

r = typecheck(s)
check(e, r)

s = [=[
if 1 then local x:number = 1 else local x:number = "foo" end
]=]
e = [=[
test.lua:1:41: type error, attempt to assign '"foo"' to 'number'
]=]

r = typecheck(s)
check(e, r)

s = [=[
if 1 then
  local x = 1
elseif 2 then
  local x = 2
elseif 3 then
  local x:string = 3
elseif 4 then
  local x:boolean = 4
else
  local x = "foo"
end
]=]
e = [=[
test.lua:6:9: type error, attempt to assign '3' to 'string'
test.lua:8:9: type error, attempt to assign '4' to 'boolean'
]=]

r = typecheck(s)
check(e, r)

s = [=[
function f(x:number?, y:string?)


if type(x) == "number" then
  x = x + 1
elseif type(y) == "string" then
  y = y .. "hello"
else
  x = x + 1
  y = y + 1
end

x = x + 1
y = y .. "hello"
end
]=]
e = [=[
test.lua:9:7: type error, attempt to perform arithmetic on a 'nil'
test.lua:10:7: type error, attempt to perform arithmetic on a 'nil'
test.lua:13:5: type error, attempt to perform arithmetic on a 'number?'
test.lua:14:5: type error, attempt to concatenate a 'string?'
]=]

r = typecheck(s)
check(e, r)

s = [=[
function f(x:boolean|number|string?)

if type(x) == "number" then
  x = x + 1
elseif type(x) == "string" then
  x = x .. "hello"
elseif type(x) == "boolean" then
  x = false
elseif math.random() > 0.5 then
  error("break")
end

x = x + 1
end
]=]
e = [=[
test.lua:13:5: type error, attempt to perform arithmetic on a 'boolean | number | string | nil'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x:number?

if type(y) == "number" then
  print("y is number")
else
  print("y is nil")
end
]=]
e = [=[
test.lua:3:9: type error, attempt to access undeclared global 'y'
]=]

r = typecheck(s)
check(e, r)

s = [=[
for i = nil, 10 do local x = i end
]=]
e = [=[
test.lua:1:9: type error, 'for' initial value must be a number
]=]

r = typecheck(s)
check(e, r)

s = [=[
for i = 1, "foo" do local x = i end
]=]
e = [=[
test.lua:1:12: type error, 'for' limit must be a number
]=]

r = typecheck(s)
check(e, r)

s = [=[
for i = 10, 1, false do local x = i end
]=]
e = [=[
test.lua:1:16: type error, 'for' step must be a number
]=]

r = typecheck(s)
check(e, r)

-- new tests

s = [=[
local x = setmetatable({}, {})
local y = setmetatable()
local z = require(x)
local w = require()
]=]
e = [=[
test.lua:2:11: type error, attempt to pass '()' to global 'setmetatable' of input type '({any:any},{any:any}?,value*)'
test.lua:3:11: type error, attempt to pass '({any:any})' to global 'require' of input type '(string,value*)'
test.lua:4:11: type error, attempt to pass '()' to global 'require' of input type '(string,value*)'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local function f ():(number, number) return 1, 2 end
local x:number, y:number, z:number = f()
z = 10
z = nil
]=]
e = [=[
test.lua:4:1: type error, attempt to assign '(nil)'to '(number,value*)'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local function f ():(number) end
]=]
e = [=[
test.lua:1:18: type error, return type '()' does not match '(number)'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local function f ():(any) return 1 end
]=]
e = [=[
{ `Localrec{ { `Id "f":`TFunction{ `TTuple{ `TVararg{ `TValue } }, `TTuple{ `TAny, `TVararg{ `TNil } } } }, { `Function{ {  }:`TTuple{ `TAny, `TVararg{ `TNil } }, { `Return{ `Number "1" } } } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x:number?
local t = { [function () end] = 1, [x] = 2 }
]=]
e = [=[
test.lua:2:37: type error, table index can be nil
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x:number = 1
local x:string = "foo"
]=]
e = [=[
{ `Local{ { `Id "x":`TBase number }, { `Number "1" } }, `Local{ { `Id "x":`TBase string }, { `String "foo" } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local function fib (n:number)
  if n == 0 then
    return 0
  elseif n == 1 then
    return 1
  else
    return fib(n - 1) + fib(n - 2)
  end
end
]=]
e = [=[
test.lua:7:12: type error, attempt to perform arithmetic on a 'nil'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local function fib (n:number):number
  if n == 0 then
    return 0
  elseif n == 1 then
    return 1
  else
    return fib(n - 1) + fib(n - 2)
  end
end
]=]
e = [=[
{ `Localrec{ { `Id "fib":`TFunction{ `TTuple{ `TBase number, `TVararg{ `TValue } }, `TTuple{ `TBase number, `TVararg{ `TNil } } } }, { `Function{ { `Id "n":`TBase number }:`TTuple{ `TBase number, `TVararg{ `TNil } }, { `If{ `Op{ "eq", `Id "n", `Number "0" }, { `Return{ `Number "0" } }, `Op{ "eq", `Id "n", `Number "1" }, { `Return{ `Number "1" } }, { `Return{ `Op{ "add", `Call{ `Id "fib", `Op{ "sub", `Id "n", `Number "1" } }, `Call{ `Id "fib", `Op{ "sub", `Id "n", `Number "2" } } } } } } } } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
for i,j,k in 1,2,3 do
end

for w in string.gmatch("foo bar", "(%w+)") do
  w = w .. "foo"
end

for w in ("foo bar"):gmatch("(%w+)") do
  w = w .. "foo"
end
]=]
e = [=[
test.lua:1:5: type error, attempt to iterate over '1'
]=]

r = typecheck(s)
check(e, r)

s = [=[
for k, v in pairs({ foo = 1, bar = 2}) do
  print(k .. "1", v)
end

for k, v in ipairs({1,2,3}) do
  print(k + 1, v)
end

for k, v in ipairs({ foo = 1, bar = 2}) do
  print(k .. "1", v)
end
]=]
e = [=[
test.lua:10:9: type error, attempt to concatenate a 'number'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x:any = 1
for n = x, x, x do end
for k in x do end
]=]
e = [=[
{ `Local{ { `Id "x":`TAny }, { `Number "1" } }, `Fornum{ `Id "n", `Id "x", `Id "x", `Id "x", {  } }, `Forin{ { `Id "k" }, { `Id "x" }, {  } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x
local y = x.z, y.z
x.z = 1
z.z = 2
]=]
e = [=[
test.lua:2:11:type error,attempt to index 'nil'with '"z"'
test.lua:2:16: type error, attempt to access undeclared global 'y'
test.lua:2:16: type error, attempt to index 'nil' with '"z"'
test.lua:3:1:type error,attempt to index 'nil'with '"z"'
test.lua:3:1:type error,attempt to assign '(1)'to '(nil,value*)'
test.lua:4:1: type error, attempt to access undeclared global 'z'
test.lua:4:1: type error, attempt to index 'nil' with '"z"'
test.lua:4:1: type error, attempt to assign '(2)' to '(nil, value*)'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local x
local y = 1
x()
y()
]=]
e = [=[
test.lua:3:1:type error,attempt to call local 'x'of type 'nil'
test.lua:4:1: type error, attempt to call local 'y' of type 'number'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local t:{"x":any} = {}
local x:any
local y:number = 1
t:x()
t:y()
x:y()
y:x()
]=]
e = [=[
test.lua:5:1: type error, attempt to call method 'y' of type 'nil'
test.lua:7:1: type error, attempt to index 'number' with '"x"'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local o = { x = 1 }
const function o:set_x (x:number) self.x = x end
local x = o.set_x
x(5, 1) -- first parameter has 'any' type
x(5)    -- force error
]=]
e = [=[
test.lua:5:1: type error, attempt to pass '(5)' to local 'x' of input type '(any, number, value*)'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local function f ():() return end
local function g (x:number):(number)?
  if x < 0 then
    return nil, "negative"
  else
    return x
  end
end
local function h (x:number):(number)?
  if x > 0 then
    return x
  else
    return g(x)
  end
end
]=]
e = [=[
{ `Localrec{ { `Id "f":`TFunction{ `TTuple{ `TVararg{ `TValue } }, `TTuple{ `TVararg{ `TNil } } } }, { `Function{ {  }:`TTuple{ `TVararg{ `TNil } }, { `Return } } } }, `Localrec{ { `Id "g":`TFunction{ `TTuple{ `TBase number, `TVararg{ `TValue } }, `TUnionlist{ `TTuple{ `TBase number, `TVararg{ `TNil } }, `TTuple{ `TNil, `TBase string, `TVararg{ `TNil } } } } }, { `Function{ { `Id "x":`TBase number }:`TUnionlist{ `TTuple{ `TBase number, `TVararg{ `TNil } }, `TTuple{ `TNil, `TBase string, `TVararg{ `TNil } } }, { `If{ `Op{ "lt", `Id "x", `Number "0" }, { `Return{ `Nil, `String "negative" } }, { `Return{ `Id "x" } } } } } } }, `Localrec{ { `Id "h":`TFunction{ `TTuple{ `TBase number, `TVararg{ `TValue } }, `TUnionlist{ `TTuple{ `TBase number, `TVararg{ `TNil } }, `TTuple{ `TNil, `TBase string, `TVararg{ `TNil } } } } }, { `Function{ { `Id "x":`TBase number }:`TUnionlist{ `TTuple{ `TBase number, `TVararg{ `TNil } }, `TTuple{ `TNil, `TBase string, `TVararg{ `TNil } } }, { `If{ `Op{ "lt", `Number "0", `Id "x" }, { `Return{ `Id "x" } }, { `Return{ `Call{ `Id "g", `Id "x" } } } } } } } } }
]=]

r = typecheck(s)
check(e, r)

-- paper dyla

s = [=[
local function factorial(n:number):number
  if n == 0 then
    return 1
  else
    return n * factorial(n - 1)
  end
end
local x = 5
print(factorial(x))
]=]
e = [=[
{ `Localrec{ { `Id "factorial":`TFunction{ `TTuple{ `TBase number, `TVararg{ `TValue } }, `TTuple{ `TBase number, `TVararg{ `TNil } } } }, { `Function{ { `Id "n":`TBase number }:`TTuple{ `TBase number, `TVararg{ `TNil } }, { `If{ `Op{ "eq", `Id "n", `Number "0" }, { `Return{ `Number "1" } }, { `Return{ `Op{ "mul", `Id "n", `Call{ `Id "factorial", `Op{ "sub", `Id "n", `Number "1" } } } } } } } } } }, `Local{ { `Id "x" }, { `Number "5" } }, `Call{ `Index{ `Id "_ENV", `String "print" }, `Call{ `Id "factorial", `Id "x" } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local function abs(n:number)
  if n < 0 then
    return -n
  else
    return n
  end
end

local function distance(x, y)
  return abs(x - y)
end
]=]
e = [=[
{ `Localrec{ { `Id "abs":`TFunction{ `TTuple{ `TBase number, `TVararg{ `TValue } }, `TTuple{ `TBase number, `TVararg{ `TNil } } } }, { `Function{ { `Id "n":`TBase number }, { `If{ `Op{ "lt", `Id "n", `Number "0" }, { `Return{ `Op{ "unm", `Id "n" } } }, { `Return{ `Id "n" } } } } } } }, `Localrec{ { `Id "distance":`TFunction{ `TTuple{ `TAny, `TAny, `TVararg{ `TValue } }, `TTuple{ `TBase number, `TVararg{ `TNil } } } }, { `Function{ { `Id "x":`TAny, `Id "y":`TAny }, { `Return{ `Call{ `Id "abs", `Op{ "sub", `Id "x", `Id "y" } } } } } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local function multiple ()
  return 2, "foo"
end
local function sum(x:number, y:number)
  return x + y
end
local x, y, z = multiple(), multiple()
print(sum(multiple(), multiple()))
]=]
e = [=[
{ `Localrec{ { `Id "multiple":`TFunction{ `TTuple{ `TVararg{ `TValue } }, `TTuple{ `TBase number, `TBase string, `TVararg{ `TNil } } } }, { `Function{ {  }, { `Return{ `Number "2", `String "foo" } } } } }, `Localrec{ { `Id "sum":`TFunction{ `TTuple{ `TBase number, `TBase number, `TVararg{ `TValue } }, `TTuple{ `TBase number, `TVararg{ `TNil } } } }, { `Function{ { `Id "x":`TBase number, `Id "y":`TBase number }, { `Return{ `Op{ "add", `Id "x", `Id "y" } } } } } }, `Local{ { `Id "x", `Id "y", `Id "z" }, { `Call{ `Id "multiple" }, `Call{ `Id "multiple" } } }, `Call{ `Index{ `Id "_ENV", `String "print" }, `Call{ `Id "sum", `Call{ `Id "multiple" }, `Call{ `Id "multiple" } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local function message (name:string, greeting:string?)
  local greeting = greeting or "Hello "
  return greeting .. name
end

print(message("Lua"))
print(message("Lua", "Hi"))
]=]
e = [=[
{ `Localrec{ { `Id "message":`TFunction{ `TTuple{ `TBase string, `TUnion{ `TBase string, `TNil }, `TVararg{ `TValue } }, `TTuple{ `TBase string, `TVararg{ `TNil } } } }, { `Function{ { `Id "name":`TBase string, `Id "greeting":`TUnion{ `TBase string, `TNil } }, { `Local{ { `Id "greeting" }, { `Op{ "or", `Id "greeting", `String "Hello " } } }, `Return{ `Op{ "concat", `Id "greeting", `Id "name" } } } } } }, `Call{ `Index{ `Id "_ENV", `String "print" }, `Call{ `Id "message", `String "Lua" } }, `Call{ `Index{ `Id "_ENV", `String "print" }, `Call{ `Id "message", `String "Lua", `String "Hi" } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local function message (name:string, greeting:string?)
  greeting = greeting or "Hello "
  return greeting .. name
end
]=]
e = [=[
{ `Localrec{ { `Id "message":`TFunction{ `TTuple{ `TBase string, `TUnion{ `TBase string, `TNil }, `TVararg{ `TValue } }, `TTuple{ `TBase string, `TVararg{ `TNil } } } }, { `Function{ { `Id "name":`TBase string, `Id "greeting":`TUnion{ `TBase string, `TNil } }, { `Set{ { `Id "greeting" }, { `Op{ "or", `Id "greeting", `String "Hello " } } }, `Return{ `Op{ "concat", `Id "greeting", `Id "name" } } } } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local function f (s:string?)
  local function f ()
    s = nil
  end
  s = s or "hi"
  s = s .. "hello"
end
]=]
e = [=[
test.lua:6:7: type error, attempt to concatenate a 'string?'
]=]

r = typecheck(s)
check(e, r)

s = [=[
function f(s:string?)

while true do
  s = s or "foo"
end

s = s .. "bar"
end
]=]
e = [=[
test.lua:7:5: type error, attempt to concatenate a 'string?'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local function rep (s:string, n:number, sep:string?):string
  sep = sep or ""
  local r = ""
  for i = 1, n - 1 do
    r = r .. s .. sep
  end
  return r .. s
end

local function overload (s1:string, s2:string|number)
  if type(s2) == "string" then
    return s1 .. s2
  else
    return rep(s1, s2)
  end
end
]=]
e = [=[
{ `Localrec{ { `Id "rep":`TFunction{ `TTuple{ `TBase string, `TBase number, `TUnion{ `TBase string, `TNil }, `TVararg{ `TValue } }, `TTuple{ `TBase string, `TVararg{ `TNil } } } }, { `Function{ { `Id "s":`TBase string, `Id "n":`TBase number, `Id "sep":`TUnion{ `TBase string, `TNil } }:`TTuple{ `TBase string, `TVararg{ `TNil } }, { `Set{ { `Id "sep" }, { `Op{ "or", `Id "sep", `String "" } } }, `Local{ { `Id "r" }, { `String "" } }, `Fornum{ `Id "i", `Number "1", `Op{ "sub", `Id "n", `Number "1" }, { `Set{ { `Id "r" }, { `Op{ "concat", `Id "r", `Op{ "concat", `Id "s", `Id "sep" } } } } } }, `Return{ `Op{ "concat", `Id "r", `Id "s" } } } } } }, `Localrec{ { `Id "overload":`TFunction{ `TTuple{ `TBase string, `TUnion{ `TBase string, `TBase number }, `TVararg{ `TValue } }, `TTuple{ `TBase string, `TVararg{ `TNil } } } }, { `Function{ { `Id "s1":`TBase string, `Id "s2":`TUnion{ `TBase string, `TBase number } }, { `If{ `Op{ "eq", `Call{ `Index{ `Id "_ENV", `String "type" }, `Id "s2" }, `String "string" }, { `Return{ `Op{ "concat", `Id "s1", `Id "s2" } } }, { `Return{ `Call{ `Id "rep", `Id "s1", `Id "s2" } } } } } } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local function overload (s1:string, s2:string|number)
  if type(s2) == "string" then
    return s1 .. s2
  else
    return string.rep(s1, s2)
  end
end
]=]
e = [=[
{ `Localrec{ { `Id "overload":`TFunction{ `TTuple{ `TBase string, `TUnion{ `TBase string, `TBase number }, `TVararg{ `TValue } }, `TTuple{ `TBase string, `TVararg{ `TNil } } } }, { `Function{ { `Id "s1":`TBase string, `Id "s2":`TUnion{ `TBase string, `TBase number } }, { `If{ `Op{ "eq", `Call{ `Index{ `Id "_ENV", `String "type" }, `Id "s2" }, `String "string" }, { `Return{ `Op{ "concat", `Id "s1", `Id "s2" } } }, { `Return{ `Call{ `Index{ `Index{ `Id "_ENV", `String "string" }, `String "rep" }, `Id "s1", `Id "s2" } } } } } } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
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
else
  msg = r
end
]=]
e = [=[
{ `Localrec{ { `Id "idiv":`TFunction{ `TTuple{ `TBase number, `TBase number, `TVararg{ `TValue } }, `TUnionlist{ `TTuple{ `TBase number, `TBase number, `TVararg{ `TNil } }, `TTuple{ `TNil, `TBase string, `TVararg{ `TNil } } } } }, { `Function{ { `Id "d1":`TBase number, `Id "d2":`TBase number }:`TUnionlist{ `TTuple{ `TBase number, `TBase number, `TVararg{ `TNil } }, `TTuple{ `TNil, `TBase string, `TVararg{ `TNil } } }, { `If{ `Op{ "eq", `Id "d2", `Number "0" }, { `Return{ `Nil, `String "division by zero" } }, { `Local{ { `Id "r" }, { `Op{ "mod", `Id "d1", `Id "d2" } } }, `Local{ { `Id "q" }, { `Op{ "div", `Paren{ `Op{ "sub", `Id "d1", `Id "r" } }, `Id "d2" } } }, `Return{ `Id "q", `Id "r" } } } } } } }, `Local{ { `Id "n1", `Id "n2" }, { `Number "4", `Number "4" } }, `Local{ { `Id "q", `Id "r" }, { `Call{ `Id "idiv", `Id "n1", `Id "n2" } } }, `Local{ { `Id "x":`TBase number, `Id "msg":`TBase string }, { `Number "0", `String "" } }, `If{ `Id "q", { `Set{ { `Id "x" }, { `Op{ "add", `Id "q", `Id "r" } } } }, { `Set{ { `Id "msg" }, { `Id "r" } } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
function x() : (number, boolean)|(string, number)
   if 0 > 1 then
      return 1, true
   else
      return "wat", 9
   end
end

local foo, bar = x()
if foo then
   local bla = foo .. " world"
end
]=]
e = [=[
test.lua:11:16: type error, attempt to concatenate a 'number | string'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local t:{string:number} = { foo = 1 }
local x:number? = t.foo
local y:number = t["bar"] or 0
]=]
e = [=[
{ `Local{ { `Id "t":`TTable{ `TBase string:`TUnion{ `TBase number, `TNil } } }, { `Table{ `Pair{ `String "foo", `Number "1" } } } }, `Local{ { `Id "x":`TUnion{ `TBase number, `TNil } }, { `Index{ `Id "t", `String "foo" } } }, `Local{ { `Id "y":`TBase number }, { `Op{ "or", `Index{ `Id "t", `String "bar" }, `Number "0" } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local t:{string:number?} = { foo = 1 or nil }
local x:number = t.foo
local y:number = t.bar or 0
local z:number? = t["bar"]
]=]
e = [=[
test.lua:2:7: type error, attempt to assign 'number?' to 'number'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local t1:{"foo":number} = { foo = 1, bar = "foo" }
local t2:{string:number} = t1
]=]
e = [=[
test.lua:2:7: type error, attempt to assign '{"foo":number}' to '{string:number?}'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local days:{string} = { "Sunday", "Monday", "Tuesday", "Wednesday",
  "Thursday", "Friday", "Saturday" }
local x = days[1]
local y = days[8]
]=]
e = [=[
{ `Local{ { `Id "days":`TTable{ `TBase number:`TUnion{ `TBase string, `TNil } } }, { `Table{ `String "Sunday", `String "Monday", `String "Tuesday", `String "Wednesday", `String "Thursday", `String "Friday", `String "Saturday" } } }, `Local{ { `Id "x" }, { `Index{ `Id "days", `Number "1" } } }, `Local{ { `Id "y" }, { `Index{ `Id "days", `Number "8" } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local days = { "Sunday", "Monday", "Tuesday", "Wednesday",
  "Thursday", "Friday", "Saturday" }
]=]
e = [=[
{ `Local{ { `Id "days" }, { `Table{ `String "Sunday", `String "Monday", `String "Tuesday", `String "Wednesday", `String "Thursday", `String "Friday", `String "Saturday" } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local days = { "Sunday", "Monday", "Tuesday", "Wednesday",
  "Thursday", "Friday", "Saturday" }
local t1:{string} = days
local t2:{string?} = days
t2 = t1
]=]
e = [=[
test.lua:3:7: type error, attempt to assign '{1:string, 2:string, 3:string, 4:string, 5:string, 6:string, 7:string}' to '{number:string?}'
test.lua:4:7: type error, attempt to assign '{1:string, 2:string, 3:string, 4:string, 5:string, 6:string, 7:string}' to '{number:string?}'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local t1:{{string}} = { { "foo", "bar", "z" }, { "z", "bar", "foo" }, 4 }
local t2:{string} = { "foo", "bar", "z", function () end }
local t3:{"foo":number, number:string} = { foo = 1, [1] = true }
]=]
e = [=[
test.lua:1:7: type error, attempt to assign '{1:{1:string, 2:string, 3:string}, 2:{1:string, 2:string, 3:string}, 3:number}' to '{number:{number:string?}?}'
test.lua:2:7: type error, attempt to assign '{1:string, 2:string, 3:string, 4:(value*) -> ()}' to '{number:string?}'
test.lua:3:7: type error, attempt to assign '{"foo":number, 1:boolean}' to '{"foo":number, number:string?}'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local t:{const "foo":string?} = { const foo = "foo" or nil }
local s:{const "foo":string} = { const foo = "foo" }
local r:{"foo":string?} = { foo = "foo" or nil }

t = s
r = t
r.foo = nil
]=]
e = [=[
test.lua:6:1: type error, attempt to assign '({const "foo":string?})' to '({"foo":string?}, value*)'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local t = { ... }
for i = 1, #t do
  print(t[i])
end
]=]
e = [=[
{ `Local{ { `Id "t" }, { `Table{ `Dots } } }, `Fornum{ `Id "i", `Number "1", `Op{ "len", `Id "t" }, { `Call{ `Index{ `Id "_ENV", `String "print" }, `Index{ `Id "t", `Id "i" } } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local t = { ... }
t[1] = 1
]=]
e = [=[
test.lua:2:1: type error, attempt to assign '(1)' to '(string?, value*)'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local t = { ... }
t[1] = "foo"
t.foo = 1
]=]
e = [=[
{ `Local{ { `Id "t" }, { `Table{ `Dots } } }, `Set{ { `Index{ `Id "t", `Number "1" } }, { `String "foo" } }, `Set{ { `Index{ `Id "t", `String "foo" } }, { `Number "1" } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local t1:{"foo":number, number:string|nil} = { foo = 1, ... }
local t2:{"foo":number, 2:string|nil, "bar":number} = { foo = 1, ..., bar = 2 }
local t3:{1:string|nil, "foo":number} = { ..., foo = 1 }
local t4:{"foo":number, "bar":number, number:string|nil} = { foo = 1, bar = 2, ... }
local t5:{"foo":number, "bar":number, 3:string|nil, 2:number} = { foo = 1, bar = 2, ..., 3 }
]=]
e = [=[
{ `Local{ { `Id "t1":`TTable{ `TLiteral foo:`TBase number, `TBase number:`TUnion{ `TBase string, `TNil } } }, { `Table{ `Pair{ `String "foo", `Number "1" }, `Dots } } }, `Local{ { `Id "t2":`TTable{ `TLiteral foo:`TBase number, `TLiteral 2:`TUnion{ `TBase string, `TNil }, `TLiteral bar:`TBase number } }, { `Table{ `Pair{ `String "foo", `Number "1" }, `Dots, `Pair{ `String "bar", `Number "2" } } } }, `Local{ { `Id "t3":`TTable{ `TLiteral 1:`TUnion{ `TBase string, `TNil }, `TLiteral foo:`TBase number } }, { `Table{ `Dots, `Pair{ `String "foo", `Number "1" } } } }, `Local{ { `Id "t4":`TTable{ `TLiteral foo:`TBase number, `TLiteral bar:`TBase number, `TBase number:`TUnion{ `TBase string, `TNil } } }, { `Table{ `Pair{ `String "foo", `Number "1" }, `Pair{ `String "bar", `Number "2" }, `Dots } } }, `Local{ { `Id "t5":`TTable{ `TLiteral foo:`TBase number, `TLiteral bar:`TBase number, `TLiteral 3:`TUnion{ `TBase string, `TNil }, `TLiteral 2:`TBase number } }, { `Table{ `Pair{ `String "foo", `Number "1" }, `Pair{ `String "bar", `Number "2" }, `Dots, `Number "3" } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local person:{"firstname":string, "lastname":string} =
  { firstname = "Lou", lastname = "Reed" }
]=]
e = [=[
{ `Local{ { `Id "person":`TTable{ `TLiteral firstname:`TBase string, `TLiteral lastname:`TBase string } }, { `Table{ `Pair{ `String "firstname", `String "Lou" }, `Pair{ `String "lastname", `String "Reed" } } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local person:Person = { firstname = "Lou", lastname = "Reed" }
]=]
e = [=[
test.lua:1:7: type error, type alias 'Person' is not defined
test.lua:1:7: type error, attempt to assign '{"firstname":string, "lastname":string}' to 'nil'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local interface Person
  firstname:string
  lastname:string
end

local function greet (person:Person)
  return "Hello, " .. person.firstname .. " " .. person.lastname
end
local user1 = { firstname = "Lewis", middlename = "Allan", lastname = "Reed" }
local user2 = { firstname = "Lou" }
local user3 = { lastname = "Reed", firstname = "Lou" }
local user4 = { "Lou", "Reed" }
print(greet(user1))
print(greet(user2))
print(greet(user3))
print(greet(user4))
]=]
e = [=[
test.lua:14:7: type error, attempt to pass '({"firstname":string})' to local 'greet' of input type '(Person, value*)'
test.lua:16:7: type error, attempt to pass '({1:string, 2:string})' to local 'greet' of input type '(Person, value*)'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local interface Person
  firstname:string
  middlename:string?
  lastname:string
end

local user1:Person = { firstname = "Lewis", middlename = "Allan", lastname = "Reed" }
local user2:Person = { lastname = "Reed", firstname = "Lou" }
]=]
e = [=[
{ `Interface{ Person, `TTable{ `TLiteral firstname:`TBase string, `TLiteral middlename:`TUnion{ `TBase string, `TNil }, `TLiteral lastname:`TBase string } }, `Local{ { `Id "user1":`TVariable Person }, { `Table{ `Pair{ `String "firstname", `String "Lewis" }, `Pair{ `String "middlename", `String "Allan" }, `Pair{ `String "lastname", `String "Reed" } } } }, `Local{ { `Id "user2":`TVariable Person }, { `Table{ `Pair{ `String "lastname", `String "Reed" }, `Pair{ `String "firstname", `String "Lou" } } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local interface Person
  firstname:string
  middlename:string?
  lastname:string
end

local user1:Person = { firstname = "Lewis", middlename = "Allan" or nil, lastname = "Reed" }
local user2:Person = { lastname = "Reed", firstname = "Lou" }
]=]
e = [=[
{ `Interface{ Person, `TTable{ `TLiteral firstname:`TBase string, `TLiteral middlename:`TUnion{ `TBase string, `TNil }, `TLiteral lastname:`TBase string } }, `Local{ { `Id "user1":`TVariable Person }, { `Table{ `Pair{ `String "firstname", `String "Lewis" }, `Pair{ `String "middlename", `Op{ "or", `String "Allan", `Nil } }, `Pair{ `String "lastname", `String "Reed" } } } }, `Local{ { `Id "user2":`TVariable Person }, { `Table{ `Pair{ `String "lastname", `String "Reed" }, `Pair{ `String "firstname", `String "Lou" } } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local interface Person
  firstname:string
  middlename:string?
  lastname:string
end

local interface Person end
]=]
e = [=[
test.lua:7:7: type error, attempt to redeclare interface 'Person'
]=]

r = typecheck(s)
check(e, r)

s = [=[
interface Test
    a: string
    b: boolean
    c: number?
end

local function fn(x: Test): number
    return 44
end

fn{a="", b = false, c = 33}
]=]
e = [=[
{ `Interface{ Test, `TTable{ `TLiteral a:`TBase string, `TLiteral b:`TBase boolean, `TLiteral c:`TUnion{ `TBase number, `TNil } } }, `Localrec{ { `Id "fn":`TFunction{ `TTuple{ `TTable{ `TLiteral a:`TBase string, `TLiteral b:`TBase boolean, `TLiteral c:`TUnion{ `TBase number, `TNil } }, `TVararg{ `TValue } }, `TTuple{ `TBase number, `TVararg{ `TNil } } } }, { `Function{ { `Id "x":`TTable{ `TLiteral a:`TBase string, `TLiteral b:`TBase boolean, `TLiteral c:`TUnion{ `TBase number, `TNil } } }:`TTuple{ `TBase number, `TVararg{ `TNil } }, { `Return{ `Number "44" } } } } }, `Call{ `Id "fn", `Table{ `Pair{ `String "a", `String "" }, `Pair{ `String "b", `False }, `Pair{ `String "c", `Number "33" } } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
interface Attribute
    name: string
    value: string
    namespace: string?
end

interface Element
    tag: string
    tag_namespace: string?
    attr: {Attribute?}
end

local elem: Element = {
    tag = "html",
    attr = {
        {
            name = "lang",
            value = "en"
        }
    }
}
]=]
e = [=[
{ `Interface{ Attribute, `TTable{ `TLiteral name:`TBase string, `TLiteral value:`TBase string, `TLiteral namespace:`TUnion{ `TBase string, `TNil } } }, `Interface{ Element, `TTable{ `TLiteral tag:`TBase string, `TLiteral tag_namespace:`TUnion{ `TBase string, `TNil }, `TLiteral attr:`TTable{ `TBase number:`TUnion{ `TTable{ `TLiteral name:`TBase string, `TLiteral value:`TBase string, `TLiteral namespace:`TUnion{ `TBase string, `TNil } }, `TNil } } } }, `Local{ { `Id "elem":`TVariable Element }, { `Table{ `Pair{ `String "tag", `String "html" }, `Pair{ `String "attr", `Table{ `Table{ `Pair{ `String "name", `String "lang" }, `Pair{ `String "value", `String "en" } } } } } } } }
]=]

r = typecheck(s)
check(e, r)

--[[ FAILING TEST
s = [=[
local interface Element
  info:number
  next:Elment?
end
]=]
e = [=[
{ `Interface{ Element, `TTable{ `TLiteral info:`TBase number, `TLiteral next:`TUnion{ `TGlobalVariable Elment, `TNil } } } }
]=]

r = typecheck(s)
check(e, r)
--]]

s = [=[
local person = {}
person.firstname = "Lou"
person.lastname = "Reed"
]=]
e = [=[
{ `Local{ { `Id "person" }, { `Table } }, `Set{ { `Index{ `Id "person", `String "firstname" } }, { `String "Lou" } }, `Set{ { `Index{ `Id "person", `String "lastname" } }, { `String "Reed" } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local bogus = { firstname = 1 }
local person:{} = bogus
person.firstname = "Lou"
person.lastname = "Reed"
]=]
e = [=[
test.lua:3:1: type error, attempt to use '"firstname"' to index closed table
test.lua:3:1: type error, attempt to assign '("Lou")' to '(nil, value*)'
test.lua:4:1: type error, attempt to use '"lastname"' to index closed table
test.lua:4:1: type error, attempt to assign '("Reed")' to '(nil, value*)'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local person = {}
local bogus = person
bogus.firstname = 1
person.firstname = "Lou"
person.lastname = "Reed"
]=]
e = [=[
test.lua:3:1: type error, attempt to use '"firstname"' to index closed table
test.lua:3:1: type error, attempt to assign '(1)' to '(nil, value*)'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local person = {}
local bogus = { firstname = 1 }
do
  person.firstname = 1
  bogus = person
end
do
  person.firstname = "Lou"
end
]=]
e = [=[
test.lua:4:3: type error, attempt to use '"firstname"' to index closed table
test.lua:4:3: type error, attempt to assign '(1)' to '(nil, value*)'
test.lua:5:3: type error, attempt to assign '({})' to '({"firstname":number}, value*)'
test.lua:8:3: type error, attempt to use '"firstname"' to index closed table
test.lua:8:3: type error, attempt to assign '("Lou")' to '(nil, value*)'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local t = {}
c = { 1 }
t.c = { 1 }
local a = c[1]
local b = t.c[1]
local x = c[2]
local y = t.c[2]
local z = g
]=]
e = [=[
test.lua:6:11: type error, attempt to index '{1:number}' with '2'
test.lua:7:11: type error, attempt to index '{1:number}' with '2'
test.lua:8:11: type error, attempt to access undeclared global 'g'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local interface Person
  firstname:string
  middlename:string?
  lastname:string
end

local user = {}
user.firstname = "Lou"
user.lastname = "Reed"
local person:Person = user
]=]
e = [=[
{ `Interface{ Person, `TTable{ `TLiteral firstname:`TBase string, `TLiteral middlename:`TUnion{ `TBase string, `TNil }, `TLiteral lastname:`TBase string } }, `Local{ { `Id "user" }, { `Table } }, `Set{ { `Index{ `Id "user", `String "firstname" } }, { `String "Lou" } }, `Set{ { `Index{ `Id "user", `String "lastname" } }, { `String "Reed" } }, `Local{ { `Id "person":`TVariable Person }, { `Id "user" } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local interface Person
  firstname:string
  middlename:string?
  lastname:string
end

local user = {}
user.firstname = "Lewis"
user.middlename = "Allan"
user.lastname = "Reed"
local person:Person = user
]=]
e = [=[
test.lua:11:7: type error, attempt to assign '{"firstname":string, "middlename":string, "lastname":string}' to 'Person'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local interface Person
  firstname:string
  middlename:string?
  lastname:string
end

local user = {}
user.firstname = "Lewis"
user.middlename = "Allan" or nil
user.lastname = "Reed"
local person:Person = user
]=]
e = [=[
{ `Interface{ Person, `TTable{ `TLiteral firstname:`TBase string, `TLiteral middlename:`TUnion{ `TBase string, `TNil }, `TLiteral lastname:`TBase string } }, `Local{ { `Id "user" }, { `Table } }, `Set{ { `Index{ `Id "user", `String "firstname" } }, { `String "Lewis" } }, `Set{ { `Index{ `Id "user", `String "middlename" } }, { `Op{ "or", `String "Allan", `Nil } } }, `Set{ { `Index{ `Id "user", `String "lastname" } }, { `String "Reed" } }, `Local{ { `Id "person":`TVariable Person }, { `Id "user" } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local str = "foo"
print(str:byte())
print(("foo"):byte())
]=]
e = [=[
{ `Local{ { `Id "str" }, { `String "foo" } }, `Call{ `Index{ `Id "_ENV", `String "print" }, `Invoke{ `Id "str", `String "byte" } }, `Call{ `Index{ `Id "_ENV", `String "print" }, `Invoke{ `Paren{ `String "foo" }, `String "byte" } } }
]=]

r = typecheck(s)
check(e, r)

s = [=[
local str = "foo"
print(str:char())
]=]
e = [=[
test.lua:2:7: type error, attempt to pass '(string)' to field of input type '(number*)'
]=]

r = typecheck(s)
check(e, r)

s = [=[
local function f (...:{1:string})
  local t:{string} = {...}
end
f = function (...:number):(string*)
  return ...
end
]=]
e = [=[
test.lua:2:9: type error, attempt to assign '{number:{1:string}?}' to '{number:string?}'
test.lua:4:1: type error, attempt to assign '((number*) -> (string*))' to '(({1:string}*) -> (), value*)'
test.lua:4:14: type error, return type '(number*)' does not match '(string*)'
]=]

r = typecheck(s)
check(e, r)

s = [=[
interface Shape
  x, y:number
  const new:(number, number) => (self)
  const move:(number, number) => ()
end

local Shape = { x = 0, y = 0 }

const function Shape:new (x:number, y:number)
  local s = setmetatable({}, { __index = self })
  s.x = x
  s.y = y
  return s
end

const function Shape:move (dx:number, dy:number)
  self.x = self.x + dx
  self.y = self.y + dy
end

local shape1 = Shape:new(0, 5)
local shape2:Shape = Shape:new(10, 10)

interface Circle
  x, y, radius:number
  const new:(number, number, value) => (self)
  const move:(number, number) => ()
  const area:() => (number)
end

local Circle = setmetatable({}, { __index = Shape })

Circle.radius = 0

const function Circle:new (x:number, y:number, radius:value)
  local c = setmetatable(Shape:new(x, y), { __index = self })
  c.radius = tonumber(radius) or 0
  return c
end

const function Circle:area ()
  return 3.14 * self.radius * self.radius
end

local circle1 = Circle:new(0, 5, 10)
local circle2:Circle = Circle:new(10, 10, 15)
]=]
e = [=[
{ `Interface{ Shape, `TTable{ `TLiteral x:`TBase number, `TLiteral y:`TBase number, `TLiteral new:`TFunction{ `TTuple{ `TSelf, `TBase number, `TBase number, `TVararg{ `TValue } }, `TTuple{ `TSelf, `TVararg{ `TNil } } }, `TLiteral move:`TFunction{ `TTuple{ `TSelf, `TBase number, `TBase number, `TVararg{ `TValue } }, `TTuple{ `TVararg{ `TNil } } } } }, `Local{ { `Id "Shape" }, { `Table{ `Pair{ `String "x", `Number "0" }, `Pair{ `String "y", `Number "0" } } } }, `Set{ { `Index{ `Id "Shape", `String "new" } }, { `Function{ { `Id "self":`TSelf, `Id "x":`TBase number, `Id "y":`TBase number }, { `Local{ { `Id "s" }, { `Call{ `Index{ `Id "_ENV", `String "setmetatable" }, `Table, `Table{ `Pair{ `String "__index", `Id "self" } } } } }, `Set{ { `Index{ `Id "s", `String "x" } }, { `Id "x" } }, `Set{ { `Index{ `Id "s", `String "y" } }, { `Id "y" } }, `Return{ `Id "s" } } } } }, `Set{ { `Index{ `Id "Shape", `String "move" } }, { `Function{ { `Id "self":`TSelf, `Id "dx":`TBase number, `Id "dy":`TBase number }, { `Set{ { `Index{ `Id "self", `String "x" } }, { `Op{ "add", `Index{ `Id "self", `String "x" }, `Id "dx" } } }, `Set{ { `Index{ `Id "self", `String "y" } }, { `Op{ "add", `Index{ `Id "self", `String "y" }, `Id "dy" } } } } } } }, `Local{ { `Id "shape1" }, { `Invoke{ `Id "Shape", `String "new", `Number "0", `Number "5" } } }, `Local{ { `Id "shape2":`TVariable Shape }, { `Invoke{ `Id "Shape", `String "new", `Number "10", `Number "10" } } }, `Interface{ Circle, `TTable{ `TLiteral x:`TBase number, `TLiteral y:`TBase number, `TLiteral radius:`TBase number, `TLiteral new:`TFunction{ `TTuple{ `TSelf, `TBase number, `TBase number, `TValue, `TVararg{ `TValue } }, `TTuple{ `TSelf, `TVararg{ `TNil } } }, `TLiteral move:`TFunction{ `TTuple{ `TSelf, `TBase number, `TBase number, `TVararg{ `TValue } }, `TTuple{ `TVararg{ `TNil } } }, `TLiteral area:`TFunction{ `TTuple{ `TSelf, `TVararg{ `TValue } }, `TTuple{ `TBase number, `TVararg{ `TNil } } } } }, `Local{ { `Id "Circle" }, { `Call{ `Index{ `Id "_ENV", `String "setmetatable" }, `Table, `Table{ `Pair{ `String "__index", `Id "Shape" } } } } }, `Set{ { `Index{ `Id "Circle", `String "radius" } }, { `Number "0" } }, `Set{ { `Index{ `Id "Circle", `String "new" } }, { `Function{ { `Id "self":`TSelf, `Id "x":`TBase number, `Id "y":`TBase number, `Id "radius":`TValue }, { `Local{ { `Id "c" }, { `Call{ `Index{ `Id "_ENV", `String "setmetatable" }, `Invoke{ `Id "Shape", `String "new", `Id "x", `Id "y" }, `Table{ `Pair{ `String "__index", `Id "self" } } } } }, `Set{ { `Index{ `Id "c", `String "radius" } }, { `Op{ "or", `Call{ `Index{ `Id "_ENV", `String "tonumber" }, `Id "radius" }, `Number "0" } } }, `Return{ `Id "c" } } } } }, `Set{ { `Index{ `Id "Circle", `String "area" } }, { `Function{ { `Id "self":`TSelf }, { `Return{ `Op{ "mul", `Op{ "mul", `Number "3.14", `Index{ `Id "self", `String "radius" } }, `Index{ `Id "self", `String "radius" } } } } } } }, `Local{ { `Id "circle1" }, { `Invoke{ `Id "Circle", `String "new", `Number "0", `Number "5", `Number "10" } } }, `Local{ { `Id "circle2":`TVariable Circle }, { `Invoke{ `Id "Circle", `String "new", `Number "10", `Number "10", `Number "15" } } } }
]=]

r = typecheck(s)
check(e, r)

-- filter20.tl

s = [=[
local x: number | string | nil = 10
local y: number | string = 20

if type(x) == "number" and type(y) ~= "number" then -- dead
  print(x + 10)
  print(x + y)
  local function f()
    x = 10
  end
  print(x .. "foo")
  print(y .. "bar")
elseif x then
  print(x + 10) -- ok
  if type(x) == "string" then -- dead
    print(x .. "foo")
  end
  print(x .. "foo") -- error, x integer
else -- dead
  print(x .. "foo")
  print(y + 10)
end

x = x + 10 -- ok
]=]

e = [=[
test.lua:4:4: type error, this arm of the 'if' is unreacheable
test.lua:14:6: type error, this arm of the 'if' is unreacheable
test.lua:17:9: type error, attempt to concatenate a 'number'
test.lua:19:3: type error, 'else' block is unreacheable
]=]

r = typecheck(s)
check(e, r)

-- filter12.tl

s = [=[
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
x = nil
x = x + 10 -- error, x mil

if type(x) == "number" then -- dead
  print(x + 10)
end
]=]

e = [=[
test.lua:14:5: type error, attempt to perform arithmetic on a 'nil'
test.lua:16:4: type error, this arm of the 'if' is unreacheable
]=]

r = typecheck(s)
check(e, r)

-- filter17.tl

s = [=[
local x: number | string | nil = 10
local y: number | string = 20

if x then
  while type(x) == "number" and type(y) ~= "number" do -- while body unreacheable
    print(x + 10)
    print(x + y)
    print(x .. "foo")
    print(y .. "bar")
    x = "foo"
    x = nil
    print(x + 10)
  end
  print(x .. "foo") -- error, x is integer
end
x = x + 10 -- ok
]=]

e = [=[
test.lua:5:3: type error, 'while' body is unreacheable
test.lua:14:9: type error, attempt to concatenate a 'number'
]=]

r = typecheck(s)
check(e, r)

-- filter14.tl

s = [=[
local x: number|string|nil = 10 -- x is actually integer
local y: number|string = 20 -- y is actually integer

if type(x) == "number" and type(y) ~= "number" then -- this arm is dead
  print(x + 10)
  print(x + y)
  print(x .. "foo")
  print(y .. "bar")
elseif x then
  print(x + 10) -- ok
  if type(x) == "string" then -- this arm is dead
    print(x .. "foo")
  end
  print(x .. "foo") -- error, x integer
else -- else block is dead
  print(x .. "foo")
  print(y + 10)
end

x = x + 10 -- ok
]=]

e = [=[
test.lua:4:4: type error, this arm of the 'if' is unreacheable
test.lua:11:6: type error, this arm of the 'if' is unreacheable
test.lua:14:9: type error, attempt to concatenate a 'number'
test.lua:16:3: type error, 'else' block is unreacheable
]=]

r = typecheck(s)
check(e, r)

-- filter22.tl

s = [=[
local x: number | string | nil = 10
local y: number | string = 20

local function f()
  x = 10
end

-- the previous function makes x unfilterable

if type(x) == "number" and type(y) ~= "number" then -- dead because of y
  print(x + 10)
  print(x + y)
  print(x .. "foo")
  print(y .. "bar")
elseif x then
  print(x + 10) -- error, x: number|string|nil
  if type(x) == "string" then
    print(x .. "foo") -- error, x: number|string|nil
  end
  print(x .. "foo") -- error, x: number|string|nil
else
  print(x .. "foo") -- error, x: number|string|nil
  print(y + 10) -- ok
end

x = x + 10 -- error, x: number|string|nil
]=]

e = [=[
test.lua:10:4: type error, this arm of the 'if' is unreacheable
test.lua:16:9: type error, attempt to perform arithmetic on a 'number | string | nil'
test.lua:18:11: type error, attempt to concatenate a 'number | string | nil'
test.lua:20:9: type error, attempt to concatenate a 'number | string | nil'
test.lua:22:9: type error, attempt to concatenate a 'number | string | nil'
test.lua:26:5: type error, attempt to perform arithmetic on a 'number | string | nil'
]=]

r = typecheck(s)
check(e, r)

-- filter9.tl

s = [=[
local function f(x: number | string | nil, y: number | string)

local function f()
  x = 10
end

-- the previous function makes x unfilterable

if type(x) == "number" and type(y) ~= "number" then
  print(x + 10) -- error, x: number|string|nil
  print(x + y)  -- error, x: number|string|nil
  print(x .. "foo") -- error, x: number|string|nil
  print(y .. "bar") -- ok
elseif x then
  print(x + 10) -- error, x: number|string|nil
  if type(x) == "string" then
    print(x .. "foo") -- error, x: number|string|nil
  end
  print(x .. "foo") -- error, x: number|string|nil
else
  print(x .. "foo") -- error, x: number|string|nil
  print(y + 10) -- error, y: number|string
end

x = x + 10 -- error, x: number|string|nil

end
]=]

e = [=[
test.lua:10:9: type error, attempt to perform arithmetic on a 'number | string | nil'
test.lua:11:9: type error, attempt to perform arithmetic on a 'number | string | nil'
test.lua:12:9: type error, attempt to concatenate a 'number | string | nil'
test.lua:15:9: type error, attempt to perform arithmetic on a 'number | string | nil'
test.lua:17:11: type error, attempt to concatenate a 'number | string | nil'
test.lua:19:9: type error, attempt to concatenate a 'number | string | nil'
test.lua:21:9: type error, attempt to concatenate a 'number | string | nil'
test.lua:22:9: type error, attempt to perform arithmetic on a 'number | string'
test.lua:25:5: type error, attempt to perform arithmetic on a 'number | string | nil'
]=]

r = typecheck(s)
check(e, r)

-- filter18.tl

s = [=[
local x: number | string | nil = 10
local y: number | string = 20

while type(x) == "number" do
  print(x + 10) -- ok
  if type(y) == "string" then -- dead
    print(x + y)
    print(x .. "foo")
    print(y .. "bar")
  end
end

x = x + 10 -- ok
]=]

e = [=[
test.lua:6:6: type error, this arm of the 'if' is unreacheable
]=]

r = typecheck(s)
check(e, r)

-- filter3.tl

s = [=[
local x:number? -- we know that x is actually nil
local y:string? -- ditto for y

if type(x) == "number" then -- dead
  x = x + 1
elseif type(y) == "string" then -- dead
  y = y .. "hello"
else
  x = x + 1 -- error, x nil
  y = y + 1 -- error, y nil
end

x = x + 1 -- error, x nil
y = y .. "hello" -- ditto
]=]

e = [=[
test.lua:4:4: type error, this arm of the 'if' is unreacheable
test.lua:6:8: type error, this arm of the 'if' is unreacheable
test.lua:9:7: type error, attempt to perform arithmetic on a 'nil'
test.lua:10:7: type error, attempt to perform arithmetic on a 'nil'
test.lua:13:5: type error, attempt to perform arithmetic on a 'nil'
test.lua:14:5: type error, attempt to concatenate a 'nil'
]=]

r = typecheck(s)
check(e, r)

-- filter21.tl

s = [=[
local x: number | string | nil = 10
local y: number | string = 20

if type(x) == "number" and type(y) ~= "number" then -- dead
  print(x + 10)
  print(x + y)
  local function f()
    print(x + 10)
  end
  print(x .. "foo")
  print(y .. "bar")
elseif x then
  print(x + 10) -- ok
  if type(x) == "string" then -- dead
    print(x .. "foo")
  end
  print(x .. "foo") -- error, x integer
else -- dead
  print(x .. "foo") -- error, x: nil
  print(y + 10) -- error, y: number|string
end

x = x + 10 -- ok
]=]

e = [=[
test.lua:4:4: type error, this arm of the 'if' is unreacheable
test.lua:14:6: type error, this arm of the 'if' is unreacheable
test.lua:17:9: type error, attempt to concatenate a 'number'
test.lua:19:3: type error, 'else' block is unreacheable
]=]

r = typecheck(s)
check(e, r)

-- filter24.tl

s = [=[
local x: number|string|nil = 10

if math.random() > 10 then
  print(x+10) -- ok
elseif math.random() > 10 then
  error("is a number")
elseif type(x) == "number" then
    error("is a number")
end

x = x + 10 -- ok

if not x then -- dead
  print(x+10) -- error, x is nil
elseif type(x) == "number" then
  error("is a number")
end

-- dead code
x = x .. "foo"
]=]

e = [=[
test.lua:13:8: type error, this arm of the 'if' is unreacheable
test.lua:20:1: type error, unreacheable statement
]=]

r = typecheck(s)
check(e, r)

-- filter8.tl

s = [=[
local function f(x: number | string | nil, y: number | string)

if type(x) == "number" and type(y) ~= "number" then
  print(x + 10) -- ok
  print(x + y)  -- error, y: string
  local function f()
    print(x + 10) -- error, x n|s|nil
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
]=]

e = [=[
test.lua:5:13: type error, attempt to perform arithmetic on a 'string'
test.lua:7:11: type error, attempt to perform arithmetic on a 'number | string | nil'
test.lua:9:9: type error, attempt to concatenate a 'number'
test.lua:12:9: type error, attempt to perform arithmetic on a 'number | string'
test.lua:16:9: type error, attempt to concatenate a 'number | string'
test.lua:18:9: type error, attempt to concatenate a 'nil'
test.lua:19:9: type error, attempt to perform arithmetic on a 'number | string'
test.lua:22:5: type error, attempt to perform arithmetic on a 'number | string | nil'
]=]

r = typecheck(s)
check(e, r)

-- filter1.tl

s = [=[
local function foo(x: number|string|nil, y: number|string)

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
]=]

e = [=[
test.lua:5:13: type error, attempt to perform arithmetic on a 'string'
test.lua:6:9: type error, attempt to concatenate a 'number'
test.lua:9:9: type error, attempt to perform arithmetic on a 'number | string'
test.lua:13:9: type error, attempt to concatenate a 'number | string'
test.lua:15:9: type error, attempt to concatenate a 'nil'
test.lua:16:9: type error, attempt to perform arithmetic on a 'number | string'
test.lua:19:5: type error, attempt to perform arithmetic on a 'number | string | nil'
]=]

r = typecheck(s)
check(e, r)

-- filter6.tl

s = [=[
local function f(x: number | string | nil, y: number | string)

local f = function () end

while type(x) == "number" do
  f()
  print(x + 10) -- ok
  f = function ()
    x = "foo" -- error, cannot revert across loop
  end
  if type(y) == "string" then
    print(x + y)  -- error, x n|s|nil
    print(x .. "foo") -- error, x n|s|nil
    print(y .. "bar") -- ok
  end
end

x = x + 10 -- error, x: number|string|nil
end
]=]

e = [=[
test.lua:9:5: type error, attempt to assign to filtered upvalue 'x' across a loop
test.lua:12:11: type error, attempt to perform arithmetic on a 'number | string | nil'
test.lua:13:11: type error, attempt to concatenate a 'number | string | nil'
test.lua:18:5: type error, attempt to perform arithmetic on a 'number | string | nil'
]=]

r = typecheck(s)
check(e, r)

-- filter5.tl

s = [=[
local function f(x: number | string | nil, y: number | string)

while type(x) == "number" do
  print(x + 10) -- ok
  if type(y) == "string" then
    print(x + y)  -- error, y: string
    print(x .. "foo") -- error, x: number
    print(y .. "bar") -- ok
  end
end

x = x + 10 -- error, x: number|string|nil
end
]=]

e = [=[
test.lua:6:15: type error, attempt to perform arithmetic on a 'string'
test.lua:7:11: type error, attempt to concatenate a 'number'
test.lua:12:5: type error, attempt to perform arithmetic on a 'number | string | nil'
]=]

r = typecheck(s)
check(e, r)

-- filter4.tl

s = [=[
local function f(x: number | string | nil, y: number | string)

if x then
  while type(x) == "number" and type(y) ~= "number" do
    print(x + 10) -- ok
    print(x + y)  -- error, y: string
    print(x .. "foo") -- error, x: number
    print(y .. "bar") -- ok
    local function g() print(x+10) end -- error, x string|number|nil
    x = "foo" -- x now is string
    print(x + 10) -- error, x string
    x = nil   -- error, x was string|number when entered loop
    print(x + 10) -- error
  end
end
x = x + 10 -- error, x: number|string|nil

end
]=]

e = [=[
test.lua:6:15: type error, attempt to perform arithmetic on a 'string'
test.lua:7:11: type error, attempt to concatenate a 'number'
test.lua:9:30: type error, attempt to perform arithmetic on a 'number | string | nil'
test.lua:11:11: type error, attempt to perform arithmetic on a 'string'
test.lua:12:5: type error,variable 'x' is looping back with type 'nil' incompatible with type 'number | string' that it entered loop
test.lua:13:11: type error, attempt to perform arithmetic on a 'nil'
test.lua:16:5: type error, attempt to perform arithmetic on a 'number | string | nil'
]=]

r = typecheck(s)
check(e, r)

-- filter7.tl

s = [=[
local function f(x: number | string | nil, y: number | string)

if type(x) == "number" and type(y) ~= "number" then
  print(x + 10) -- ok
  print(x + y)  -- error, y: string
  local function f()
    x = 10 -- ok, x is now n|s|nil
  end
  print(x .. "foo") -- error, x is n|s|nil
  print(y .. "bar") -- ok
elseif x then -- x cannot downcast anymore because of f
  print(x + 10) -- error, x: number|string|nil
  if type(x) == "string" then
    print(x .. "foo") -- error, x: number|string|nil
  end
  print(x .. "foo") -- error, x: number|string|nil
else
  print(x .. "foo") -- error, x: number|string|nil
  print(y + 10) -- error, y: number|string
end

x = x + 10 -- error, x: number|string|nil

end
]=]

e = [=[
test.lua:5:13: type error, attempt to perform arithmetic on a 'string'
test.lua:9:9: type error, attempt to concatenate a 'number | string | nil'
test.lua:12:9: type error, attempt to perform arithmetic on a 'number | string | nil'
test.lua:14:11: type error, attempt to concatenate a 'number | string | nil'
test.lua:16:9: type error, attempt to concatenate a 'number | string | nil'
test.lua:18:9: type error, attempt to concatenate a 'number | string | nil'
test.lua:19:9: type error, attempt to perform arithmetic on a 'number | string'
test.lua:22:5: type error, attempt to perform arithmetic on a 'number | string | nil'
]=]

r = typecheck(s)
check(e, r)

-- filter16.tl

s = [=[
local function f(x:number?, y:string?)

if type(x) == "number" then
  x = x + 1
elseif type(y) == "string" then
  y = y .. "hello"
else
  x = x + 1 -- error, x nil
  y = y + 1 -- error, y nil
end

x = x + 1 -- error, x number?
y = y .. "hello" -- error, y string?

end
]=]

e = [=[
test.lua:8:7: type error, attempt to perform arithmetic on a 'nil'
test.lua:9:7: type error, attempt to perform arithmetic on a 'nil'
test.lua:12:5: type error, attempt to perform arithmetic on a 'number?'
test.lua:13:5: type error, attempt to concatenate a 'string?'
]=]

r = typecheck(s)
check(e, r)

-- filter13.tl

s = [=[
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
]=]

e = [=[
test.lua:4:9: type error, attempt to perform arithmetic on a 'number | string | nil'
test.lua:11:5: type error, attempt to perform arithmetic on a 'number | string | nil'
test.lua:14:9: type error, attempt to perform arithmetic on a 'nil'
test.lua:21:5: type error, attempt to perform arithmetic on a 'string?'
]=]

r = typecheck(s)
check(e, r)

-- filter2.tl

s = [=[
local function foo(x: number|string|nil, y: number|string)

while type(x) == "number" and type(y) ~= "number" do
  print(x + 10) -- ok
  print(x + y) -- error, y is string
  print(x .. "foo") -- error, x is number
  print(y .. "bar") -- ok
  x = "foo" -- ok, x is now string
  print(x + 10) -- error, x is string
end

x = x + 10 -- error, x is number|string|nil

end
]=]

e = [=[
test.lua:5:13: type error, attempt to perform arithmetic on a 'string'
test.lua:6:9: type error, attempt to concatenate a 'number'
test.lua:9:9: type error, attempt to perform arithmetic on a 'string'
test.lua:12:5: type error, attempt to perform arithmetic on a 'number | string | nil'
]=]

r = typecheck(s)
check(e, r)

-- filter23.tl

s = [=[
local x: number | string | nil = 10
local y: number | string = 20

while type(x) == "number" and type(y) ~= "number" do -- dead
  print(x + 10) -- ok
  print(x + y)  -- error, y: string
  print(x .. "foo") -- error, x: number
  print(y .. "bar") -- ok
end

x = x + 10 -- ok
]=]

e = [=[
test.lua:4:1: type error, 'while' body is unreacheable
]=]

r = typecheck(s)
check(e, r)

-- filter15.tl

s = [=[
local x: number | string | nil = 10 -- x is actually integer
local y: number | string = 20 -- y is actually integer

while type(x) == "number" and type(y) ~= "number" do -- while block unreacheable
  print(x + 10)
  print(x + y)
  print(x .. "foo")
  print(y .. "bar")
  x = "foo"
  print(x + 10)
end

x = x + 10 -- ok
]=]

e = [=[
test.lua:4:1: type error, 'while' body is unreacheable
]=]

r = typecheck(s)
check(e, r)

-- filter11.tl

s = [=[
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
]=]

e = [=[
test.lua:16:9: type error, attempt to concatenate a 'number'
test.lua:19:9: type error, attempt to perform arithmetic on a 'string'
]=]

r = typecheck(s)
check(e, r)

-- filter10.tl

s = [=[
local function f(x: number | string | nil, y: number | string)

while type(x) == "number" and type(y) ~= "number" do
  print(x + 10) -- ok
  print(x + y)  -- error, y: string
  print(x .. "foo") -- error, x: number
  print(y .. "bar") -- ok
end

x = x + 10 -- error, x: number|string|nil
end
]=]

e = [=[
test.lua:5:13: type error, attempt to perform arithmetic on a 'string'
test.lua:6:9: type error, attempt to concatenate a 'number'
test.lua:10:5: type error, attempt to perform arithmetic on a 'number | string | nil'
]=]

r = typecheck(s)
check(e, r)

-- filter19.tl

s = [=[
local x: number | string | nil = 10
local y: number | string = 20

while type(x) == "number" do
  local function f()
    x = 10 -- error, assigning to filtered upvalue across loop
  end
  print(x + 10) -- error, x n|s|nil
  if type(y) == "string" then -- dead
    print(x + y)
    print(x .. "foo")
    print(y .. "bar")
  end
end

x = x + 10 -- error, x n|s|nil because of f
]=]

e = [=[
test.lua:4:7: type error,variable 'x' is looping back with type 'number | string | nil' incompatible with type 'number'that it entered loop
test.lua:6:5: type error, attempt to assign to filtered upvalue 'x' across a loop
test.lua:8:9: type error, attempt to perform arithmetic on a 'number | string | nil'
test.lua:9:6: type error, this arm of the 'if' is unreacheable
test.lua:16:5: type error, attempt to perform arithmetic on a 'number | string | nil'
]=]

r = typecheck(s)
check(e, r)

-- filter25.tl

s = [=[
local function f(t: { "foo": string }?)
  local x: string = t and t.foo or "bar"
  print(x + 10)
  local t = t or error("nil!")
  print(t.foo + 10)
end
]=]

e = [=[
test.lua:3:9: type error, attempt to perform arithmetic on a 'string'
test.lua:5:9: type error, attempt to perform arithmetic on a 'string'
]=]

r = typecheck(s)
check(e, r)

-- issue96.tl

s = [=[
local function double_number(x: number?)
  if not x then
    error("Not giving p1 to double_number is deprecated")
  end
  print(x .. "foo") -- error, x number
  return 2 * x -- x is filtered to number
end
]=]

e = [=[
test.lua:5:9: type error, attempt to concatenate a 'number'
]=]

r = typecheck(s)
check(e, r)

-- issue58.tl

s = [=[
typealias Cmd = string|{string}

function f(cmd:Cmd) -- if I change to cmd:string|{string}, it works
   local c: {string} = {}
   if type(cmd) == "string" then
      table.insert(c, cmd)
      print(cmd + 10) -- error, cmd is string
   else
      c = cmd
      print(cmd + 10) -- error, cmd is array
   end
end
]=]

e = [=[
test.lua:7:13: type error, attempt to perform arithmetic on a 'string'
test.lua:10:13: type error, attempt to perform arithmetic on a '{number: string?}'
]=]

r = typecheck(s)
check(e, r)

-- issue55.tl

s = [=[
interface ELEM
   x: number
end

interface OBJ
   parent: OBJ?
   atr: ELEM -- if you comment this line or change ELEM to a base type like string, it works
end

local function f(o:OBJ)
   local parent = o.parent
   if parent then
      print(parent + 10) -- just to supress generation of .lua
      f(parent)
   end
end
]=]

e = [=[
test.lua:13:13: type error, attempt to perform arithmetic on a '{"parent": OBJ?, "atr": {"x": number}}'
]=]

r = typecheck(s)
check(e, r)

-- issue68.tl

s = [=[

-- The following two mutually recursive types are the same structurally
interface Tree1
  label: number
  left: Tree2?
  right: Tree2?
end

interface Tree2
  label: number
  left: Tree1?
  right: Tree1?
end

-- the following type has a forward reference
interface OBJ
   parent: OBJ?
   atr: ELEM
end

interface ELEM
   x: number
end

local function f(o:OBJ)
   local t1: Tree1 = { label = 2 }
   local t2: Tree2 = { label = 3 }
   t1 = t2
   t2 = t1
   local parent = o.parent
   if parent then
      f({atr={x=2}}) -- isn't this structurally equivalent to OBJ?
      print(parent + 10) -- just to supress generation of .lua file
   end
end
]=]

e = [=[
test.lua:33:13: type error, attempt to perform arithmetic on a '{"parent": OBJ?, "atr": ELEM}'
]=]

r = typecheck(s)
check(e, r)

-- issue16.tl

s = [=[
interface SubA
  subaname: string
end

local A = {}
A.aname = '<class A proto>'
A.child = {subaname='the subaname'}

function A:new(): self
  local t = {}
  t.aname = '<class A>'
  t.child = {subaname='the subaname'}

  local s = setmetatable(t, {__index = self})
  return s
end

function A:afoo(): self
  local s: string = self.aname
  print('Hello ' .. s)
  return self
end

function A:arun(name: string)
  print('arun', name)
end

function A:awrong(x: self) -- error, self cannot appear here!
end

function A:abar(obj: SubA)
  self:arun(obj.subaname) -- Does not compile cleanly.

  -- This works:
  local s = obj.subaname
  self:arun(s)

  -- This also works:
  self.arun(self, obj.subaname)
end

return A
]=]

e = [=[
test.lua:28:18: type error, self type appearing in a place that is not a first parameter or a return type inside type '(self, self, value*) -> ()'
]=]

r = typecheck(s)
check(e, r)

-- issue98.tl

s = [=[
local function f(i: number?) -- to force i to be number?
  print(1 + (i or 0)) -- ok
  print(i .. "foo") -- error, i number?
end
]=]

e = [=[
test.lua:3:9: type error, attempt to concatenate a 'number?'
]=]

r = typecheck(s)
check(e, r)

-- issue99.tl

s = [=[
local f, err = io.open("wednesday")
do
  f = f or io.stdin -- breaks f out of the projection
  print(f + 10) -- f is a file
  print(err + 10) -- err is string?
  if type(err) == "string" then
    print(f + 10) -- f is still file
  end
end
print(f + 10) -- f is file
print(err + 10) -- err is string?
if f then
  print(err + 10) -- err is still string?
end
]=]

e = [=[
test.lua:4:9: type error, attempt to perform arithmetic on a 'file'
test.lua:5:9: type error, attempt to perform arithmetic on a 'string?'
test.lua:7:11: type error, attempt to perform arithmetic on a 'file'
test.lua:10:7: type error, attempt to perform arithmetic on a 'file'
test.lua:11:7: type error, attempt to perform arithmetic on a 'string?'
test.lua:13:9: type error, attempt to perform arithmetic on a 'string?'
]=]

r = typecheck(s)
check(e, r)

-- issue76.tl

s = [=[
local Shape = { x = 0.0, y = 0.0 }

const function Shape:new (x:number, y:number):self
  local s = setmetatable({}, { __index = self })
  s.x = x
  s.y = y
  return s
end

const function Shape:move (dx:number, dy:number):()
  self.x = self.x + dx
  self.y = self.y + dy
end

local s = Shape:new(3,2)
s:move(20,25)
print(s + 10) -- to supress generation of .lua
]=]

e = [=[
test.lua:17:7: type error, attempt to perform arithmetic on a '{"x": number, "y": number, const "new": (self, number, number, value*) -> (self), const "move": (self, number, number, value*) -> ()}'
]=]

r = typecheck(s)
check(e, r)

-- issue45.tl

s = [=[
-- bug45driver.tl
local bug45 = require("examples.issues.bug45")

local r = bug45.makeR()
local s, err = bug45.makeS()
if r then
  if s then
    s = r:makeT(s) -- crashes setting s, or a new global. Doesn't crash if using `local`
  end
end

print(nil + 5) -- just to supress generation of .lua
]=]

e = [=[
test.lua:12:7: type error, attempt to perform arithmetic on a 'nil'
]=]

r = typecheck(s)
check(e, r)

-- issue69.tl

s = [=[
local person:Person = { firstname = "Lou", lastname = "Reed" }
]=]

e = [=[
test.lua:1:7: type error, type alias 'Person' is not defined
test.lua:1:7: type error, attempt to assign '{"firstname": string, "lastname": string}' to 'nil'
]=]

r = typecheck(s)
check(e, r)

-- issue84.tl

s = [=[
local input = io.open("test")

if input then
  local stuff = input:read("a")
  input:close()
  print(input + 10) -- error, input is file
end
]=]

e = [=[
test.lua:6:9: type error, attempt to perform arithmetic on a 'file'
]=]

r = typecheck(s)
check(e, r)

-- issue88.tl

s = [=[
local diag_test: {number: string|number|nil} = {"HI", 1, {"another table"}}
]=]

e = [=[
test.lua:1:7: type error, attempt to assign '{1: string, 2: number, 3: {1: string}}' to '{number: number | string | nil}'
]=]

r = typecheck(s)
check(e, r)

-- issue87.tl

s = [=[
local my_lambda: () -> () = function () end

my_lambda()
print(my_lambda + 10) -- just to force an error
]=]

e = [=[
test.lua:4:7: type error, attempt to perform arithmetic on a '(value*) -> ()'
]=]

r = typecheck(s)
check(e, r)

-- pr82.tl

s = [=[

local function factorial(n: number): number
  if n == 0 then
    return 1
  else
    return n * factorial(n - 1)
  end
end
local x = 5

print(factorial(x))
print(factorial(x, nil))
print(factorial(x, 5))

print(factorial + 10) -- just to force an error
]=]

e = [=[
test.lua:15:7: type error, attempt to perform arithmetic on a '(number, value*) -> (number)'
]=]

r = typecheck(s)
check(e, r)

-- issue50.tl

s = [=[
interface IStackOverflow
   const my_method: (IStackOverflow) -> ()
end

function new(): IStackOverflow
   local obj: IStackOverflow = {}
   obj.my_method = my_method
   return obj
end
]=]

e = [=[
test.lua:5:13: type error, return type '({})' does not match '(IStackOverflow)'
test.lua:6:10: type error, attempt to assign '{}' to '{const "my_method": (IStackOverflow, value*) -> ()}'
test.lua:7:4: type error, attempt to use '"my_method"' to index closed table
test.lua:7:20: type error, attempt to access undeclared global 'my_method'
]=]

r = typecheck(s)
check(e, r)

-- issue52.tl

s = [=[
function x() : (number, boolean)|(string, number)
   if 0 > 1 then
      return 1, true
   else
      return "wat", 9
   end
end

local baz, foo, bar = 10, x()
if foo then
   local bla = foo .. " world" -- error, foo is i|s
end
baz = baz + 10
]=]

e = [=[
test.lua:11:16: type error, attempt to concatenate a 'number | string'
]=]

r = typecheck(s)
check(e, r)

-- pr56.tl

s = [=[
interface ELEM
   x: number
end

interface OBJ
   parent: OBJ?
   atr: ELEM -- if I change ELEM to {"x":number}, I no longer get an error
end

local function f(o:OBJ)
   local parent = o.parent
   if parent then
      f({atr={x=2}}) -- isn't this structurally equivalent to OBJ?
      print(parent + 10) -- just to force an error
   end
end
]=]

e = [=[
test.lua:14:13: type error, attempt to perform arithmetic on a '{"parent": OBJ?, "atr": {"x": number}}'
]=]

r = typecheck(s)
check(e, r)

-- issue47.tl

s = [=[
local bug47 = require("examples.issues.bug47")
local function main()
   local maybe_u, rerr = bug47.foo()
   print(maybe_u + 10) -- maybe_u is U?
   if type(maybe_u) == "nil" then
      print(maybe_u + 10) -- maybe_u is nil
      print(rerr .. "foo")
      if type(rerr) == "string" then
         error("error - "..rerr, nil)
      end
      return -- unreacheable
   end
   -- maybe_u is a U here
   local u = maybe_u
   print(u + 10)
end
main()
]=]

e = [=[
test.lua:4:10: type error, attempt to perform arithmetic on a 'U?'
test.lua:6:13: type error, attempt to perform arithmetic on a 'nil'
test.lua:11:7: type error, unreacheable statement
test.lua:15:10: type error, attempt to perform arithmetic on a 'U'
]=]

r = typecheck(s)
check(e, r)

-- issue71

s = [=[
require "examples.issues.bug71.a"
]=]

e = [=[
./examples/issues/bug71/b.tl:1:1: type error, circular require
]=]

r = typecheck(s)
check(e, r)

s = [=[
require "examples.issues.bug71.testfile1"
]=]

e = [=[
./examples/issues/bug71/testfile3.tl:1:1: type error, circular require
]=]

r = typecheck(s)
check(e, r)

s = [=[
require "examples.issues.bug71.ok1"
]=]

e = [=[
{ `Call{ `Index{ `Id "_ENV", `String "require" }, `String "examples.issues.bug71.ok1" } }
]=]

r = typecheck(s)
check(e, r)

print("> testing code generation...")

-- assignments

s = [=[
zero,um = false,true
]=]
e = [=[
zero, um = false, true
]=]

r = generate(s)
check(e, r)

s = [=[
n,s = 1, "alo"
]=]
e = [=[
n, s = 1, "alo"
]=]

r = generate(s)
check(e, r)

s = [=[
t = ...,nil
]=]
e = [=[
t = ..., nil
]=]

r = generate(s)
check(e, r)

s = [=[
a = 2 * 3 + 5
]=]
e = [=[
a = 2 * 3 + 5
]=]

r = generate(s)
check(e, r)

s = [=[
a = (2 * 3) + 5
]=]
e = [=[
a = (2 * 3) + 5
]=]

r = generate(s)
check(e, r)

s = [=[
a = 1 - 2 / 3 % 4 ^ 5
]=]
e = [=[
a = 1 - 2 / 3 % 4 ^ 5
]=]

r = generate(s)
check(e, r)

s = [=[
c = "alo" .. "mundo"
]=]
e = [=[
c = "alo" .. "mundo"
]=]

r = generate(s)
check(e, r)

s = [=[
a = 1 == 2
b = 1 ~= 2
c = 1 < 2
d = 1 <= 2
e = 1 > 2
f = 1 >= 2
]=]
e = [=[
a = 1 == 2
b = not (1 == 2)
c = 1 < 2
d = 1 <= 2
e = 2 < 1
f = 2 <= 1

]=]

r = generate(s)
check(e, r)

s = [=[
a = not 1 and 2 or 3
]=]
e = [=[
a = not (1) and 2 or 3
]=]

r = generate(s)
check(e, r)

s = [=[
t = { 1, 2, 3 }
]=]
e = [=[
t = {1, 2, 3}
]=]

r = generate(s)
check(e, r)

-- do

s = [=[
do do do do do end end end end end
]=]
e = [=[
do do do do do  end end end end end
]=]

r = generate(s)
check(e, r)

-- for

s = [=[
for i=1, 10 do break end
]=]
e = [=[
for i = 1, 10 do break end
]=]

r = generate(s)
check(e, r)

s = [=[
for i=1,10,-1 do break end
]=]
e = [=[
for i = 1, 10, -(1) do break end
]=]

r = generate(s)
check(e, r)

s = [=[
for k,v in pairs({}) do end
]=]
e = [=[
for k, v in pairs({}) do  end
]=]

r = generate(s)
check(e, r)

-- function

s = [=[
function f ():() end
]=]
e = [=[
f = function ()  end
]=]

r = generate(s)
check(e, r)

s = [=[
function f () end
]=]
e = [=[
f = function ()  end
]=]

r = generate(s)
check(e, r)

s = [=[
function f (a) return a end
]=]
e = [=[
f = function (a) return a end
]=]

r = generate(s)
check(e, r)

s = [=[
function f (a, b, c) end
]=]
e = [=[
f = function (a, b, c)  end
]=]

r = generate(s)
check(e, r)

s = [=[
function f (a, b, c, ...) end
]=]
e = [=[
f = function (a, b, c, ...)  end
]=]

r = generate(s)
check(e, r)

s = [=[
function f (...) end
]=]
e = [=[
f = function (...)  end
]=]

r = generate(s)
check(e, r)

s = [=[
local function f () end
]=]
e = [=[
local function f ()  end
]=]

r = generate(s)
check(e, r)

s = [=[
local function f (a) return a end
]=]
e = [=[
local function f (a) return a end
]=]

r = generate(s)
check(e, r)

s = [=[
local function f (a, b, c) end
]=]
e = [=[
local function f (a, b, c)  end
]=]

r = generate(s)
check(e, r)

s = [=[
local function f (a, b, c, ...) end
]=]
e = [=[
local function f (a, b, c, ...)  end
]=]

r = generate(s)
check(e, r)

s = [=[
local function f (...) end
]=]
e = [=[
local function f (...)  end
]=]

r = generate(s)
check(e, r)

-- goto

s = [=[
do goto eof end
:: eof ::
]=]
e = [=[
do goto eof end
::eof::

]=]

r = generate(s)
check(e, r)

-- if

s = [=[
if 1 then
  return 1
end
]=]
e = [=[
if 1 then
  return 1
end

]=]

r = generate(s)
check(e, r)

s = [=[
if 1 then
  return 1
else
  return 2
end
]=]
e = [=[
if 1 then
  return 1
else
  return 2
end

]=]

r = generate(s)
check(e, r)

s = [=[
if 1 then
  return 1
elseif 2 then
  return 2
elseif 3 then
  return 3
elseif 4 then
  return 4
end
]=]
e = [=[
if 1 then
  return 1
elseif 2 then
  return 2
elseif 3 then
  return 3
elseif 4 then
  return 4
end

]=]

r = generate(s)
check(e, r)

s = [=[
if 1 then
  return 1
elseif 2 then
  return 2
elseif 3 then
  return 3
elseif 4 then
  return 4
else
  return 5
end
]=]
e = [=[
if 1 then
  return 1
elseif 2 then
  return 2
elseif 3 then
  return 3
elseif 4 then
  return 4
else
  return 5
end

]=]

r = generate(s)
check(e, r)

s = [=[
if 1 then
  if "hello" then
    return "hello"
  end
  return 1
elseif 2 then
  return 2
elseif 3 then
  if "foo" then
    return "foo"
  end
  return 3
elseif 4 then
  return 4
else
  if "bar" then
    return "bar"
  end
  return 5
end
]=]
e = [=[
if 1 then
  if "hello" then
    return "hello"
  end
  return 1
elseif 2 then
  return 2
elseif 3 then
  if "foo" then
    return "foo"
  end
  return 3
elseif 4 then
  return 4
else
  if "bar" then
    return "bar"
  end
  return 5
end

]=]

r = generate(s)
check(e, r)

s = [=[
local function f(x:number?)
  if not x then print("error") else x = x + 1 end
  if type(x) == "nil" then print("error") else x = x + 1 end
end
]=]
e = [=[
local function f (x)
  if not (x) then print("error") else x = x + 1 end
  if type(x) == "nil" then print("error") else x = x + 1 end
end

]=]

r = generate(s)
check(e, r)

-- local

s = [=[
local a:any?
]=]
e = [=[
local a
]=]

r = generate(s)
check(e, r)

s = [=[
local a:any?, b:any?, c:any?
]=]
e = [=[
local a, b, c
]=]

r = generate(s)
check(e, r)

s = [=[
local a = 1
]=]
e = [=[
local a = 1
]=]

r = generate(s)
check(e, r)

s = [=[
local a, b:any? = 1
]=]
e = [=[
local a, b = 1
]=]

r = generate(s)
check(e, r)

-- repeat

s = [=[
repeat break until true
]=]
e = [=[
repeat break until true
]=]

r = generate(s)
check(e, r)

-- while

s = [=[
while 1 do
  break
end
]=]
e = [=[
while 1 do
  break
end

]=]

r = generate(s)
check(e, r)

-- complete examples

s = [=[
local interface Element
  info:number
  next:Element?
end

local function insert_s (e:Element?, v:number):Element
  return { info = v, next = e }
end

local function insert_f (e:Element?, v:number):Element
  if e then
    e.next = insert_f(e.next, v)
    return e
  end
  return { info = v, next = e }
end

local function print_l (e:Element?)
  if e then
    print(e.info)
    print_l(e.next)
  end
end

local e:Element?

e = insert_s(e, 2)
e = insert_s(e, 1)
e = insert_s(e, 3)
e = insert_s(e, 4)
e = insert_s(e, 0)

print_l(e)

e = nil

e = insert_f(e, 2)
e = insert_f(e, 1)
e = insert_f(e, 3)
e = insert_f(e, 4)
e = insert_f(e, 0)

print_l(e)
]=]
e = [=[





local function insert_s (e, v)
  return {info = v, next = e}
end

local function insert_f (e, v)
  if e then
    e.next = insert_f(e.next,v)
    return e
  end
  return {info = v, next = e}
end

local function print_l (e)
  if e then
    print(e.info)
    print_l(e.next)
  end
end

local e

e = insert_s(e,2)
e = insert_s(e,1)
e = insert_s(e,3)
e = insert_s(e,4)
e = insert_s(e,0)

print_l(e)

e = nil

e = insert_f(e,2)
e = insert_f(e,1)
e = insert_f(e,3)
e = insert_f(e,4)
e = insert_f(e,0)

print_l(e)

]=]

r = generate(s)
check(e, r)

s = [=[
local interface NoArv
  info:string
  left, right: NoArv?
end

local function create (v:string, l:NoArv?, r:NoArv?):NoArv
  return { info = v, left = l, right = r }
end

local function print_tree (t:NoArv?)
  if t then
    print(t.info)
    print_tree(t.left)
    print_tree(t.right)
  end
end

local a1 = create("d")
local a2 = create("b", nil, a1)
local a3 = create("e")
local a4 = create("f")
local a5 = create("c", a3, a4)
local a = create("a", a2, a5)

print_tree(a)
]=]
e = [=[





local function create (v, l, r)
  return {info = v, left = l, right = r}
end

local function print_tree (t)
  if t then
    print(t.info)
    print_tree(t.left)
    print_tree(t.right)
  end
end

local a1 = create("d")
local a2 = create("b",nil,a1)
local a3 = create("e")
local a4 = create("f")
local a5 = create("c",a3,a4)
local a = create("a",a2,a5)

print_tree(a)

]=]

r = generate(s)
check(e, r)

s = [=[
interface Shape
  x, y:number
  const new:(number, number) => (self)
  const move:(number, number) => ()
end

local Shape = { x = 0, y = 0 }

const function Shape:new (x:number, y:number)
  local s = setmetatable({}, { __index = self })
  s.x = x
  s.y = y
  return s
end

const function Shape:move (dx:number, dy:number)
  self.x = self.x + dx
  self.y = self.y + dy
end

local shape1 = Shape:new(0, 5)
local shape2:Shape = Shape:new(10, 10)

interface Circle
  x, y, radius:number
  const new:(number, number, value) => (self)
  const move:(number, number) => ()
  const area:() => (number)
end

local Circle = setmetatable({}, { __index = Shape })

Circle.radius = 0

const function Circle:new (x:number, y:number, radius:value)
  local c = setmetatable(Shape:new(x, y), { __index = self })
  c.radius = tonumber(radius) or 0
  return c
end

const function Circle:area ()
  return 3.14 * self.radius * self.radius
end

local circle1 = Circle:new(0, 5, 10)
local circle2:Circle = Circle:new(10, 10, 15)
circle1:area()
circle2:area()
]=]
e = [=[






local Shape = {x = 0, y = 0}

Shape.new = function (self, x, y)
  local s = setmetatable({},{__index = self})
  s.x = x
  s.y = y
  return s
end

Shape.move = function (self, dx, dy)
  self.x = self.x + dx
  self.y = self.y + dy
end

local shape1 = Shape:new(0,5)
local shape2 = Shape:new(10,10)








local Circle = setmetatable({},{__index = Shape})

Circle.radius = 0

Circle.new = function (self, x, y, radius)
  local c = setmetatable(Shape:new(x,y),{__index = self})
  c.radius = tonumber(radius) or 0
  return c
end

Circle.area = function (self)
  return 3.14 * self.radius * self.radius
end

local circle1 = Circle:new(0,5,10)
local circle2 = Circle:new(10,10,15)
circle1:area()
circle2:area()

]=]

r = generate(s)
check(e, r)

print("> testing module loader...")

s = [=[1]=]
e = [=[
test.tl:1:1: syntax error, unexpected '1', expecting 'return', '(', 'Name', 'typealias', 'interface', 'goto', 'break', '::', 'local', 'function', 'const', 'repeat', 'for', 'do', 'while', 'if', ';'
]=]

r = test_loader(s)
check(r == e)

s = [=[return 1]=]
e = 1

r = test_loader(s)
check(r == e)

s = [=[
local function a(b: string, c: string) : string
  return
end

return a("b", 2)
]=]
e = [=[
test.tl:1:17: type error, return type '()' does not match '(string)'
test.tl:1:18: warning, unused local 'b'
test.tl:1:29: warning, unused local 'c'
test.tl:5:8: type error, attempt to pass '(b, 2)' to local 'a' of input type '(string, string)'
]=]

r = test_loader(s)
check(r == e)

s = [=[
local function a(b: string, c: string) : (string, nil*)
  return b .. "-" .. c
end

return a("b", "c")
]=]
e = [=[b-c]=]

r = test_loader(s)
check(r == e)

if failed_tests == 0 then
  print("OK: " .. passed_tests .. " PASSED TESTS")
  os.exit(0)
else
  print(failed_tests .. " FAILED TESTS, " .. passed_tests .. " PASSED TESTS")
  os.exit(1)
end
