#!/usr/bin/env lua

local tlast = require "tlast"
local tlparser = require "tlparser"
local tltype = require "tltype"
local tlchecker = require "tlchecker"
local tlcode = require "tlcode"

package.path = "./typedlua/?.lua;" .. package.path

-- expected result, result, subject
local e, r, s

local filename = "test.lua"

local function parse (s)
  local t,m = tlparser.parse(s,filename,false)
  local r
  if not t then
    r = m
  else
    r = tlast.tostring(t)
  end
  return r .. "\n"
end

local function typecheck (s)
  local t,m = tlparser.parse(s,filename,false)
  local r
  if not t then
    error(m)
    os.exit(1)
  end
  m = tlchecker.typecheck(t,s,filename,false)
  m = tlchecker.error_msgs(m,false)
  if m then
    r = m
  else
    r = tlast.tostring(t)
  end
  return r .. "\n"
end

local function generate (s)
  local t,m = tlparser.parse(s,filename,false)
  if not t then
    error(m)
    os.exit(1)
  end
  m = tlchecker.typecheck(t,s,filename,false)
  m = tlchecker.error_msgs(m,false)
  if m then
    return m .. "\n"
  else
    return tlcode.generate(t)
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
assert(r == e)

s = [=[
-- testing empty file
]=]
e = [=[
{  }
]=]

r = parse(s)
assert(r == e)

-- expressions

s = [=[
local _nil,_false,_true,_dots = nil,false,true,...
]=]
e = [=[
{ `Local{ { `Id "_nil", `Id "_false", `Id "_true", `Id "_dots" }, { `Nil, `False, `True, `Dots } } }
]=]

r = parse(s)
assert(r == e)

-- floating points

s = [=[
local f1 = 1.
local f2 = 1.1
]=]
e = [=[
{ `Local{ { `Id "f1" }, { `Number "1.0" } }, `Local{ { `Id "f2" }, { `Number "1.1" } } }
]=]

r = parse(s)
assert(r == fixint(e))

s = [=[
local f1 = 1.e-1
local f2 = 1.e1
]=]
e = [=[
{ `Local{ { `Id "f1" }, { `Number "0.1" } }, `Local{ { `Id "f2" }, { `Number "10.0" } } }
]=]

r = parse(s)
assert(r == fixint(e))

s = [=[
local f1 = 1.1e+1
local f2 = 1.1e1
]=]
e = [=[
{ `Local{ { `Id "f1" }, { `Number "11.0" } }, `Local{ { `Id "f2" }, { `Number "11.0" } } }
]=]

r = parse(s)
assert(r == fixint(e))

s = [=[
local f1 = .1
local f2 = .1e1
]=]
e = [=[
{ `Local{ { `Id "f1" }, { `Number "0.1" } }, `Local{ { `Id "f2" }, { `Number "1.0" } } }
]=]

r = parse(s)
assert(r == fixint(e))

s = [=[
local f1 = 1E1
local f2 = 1e-1
]=]
e = [=[
{ `Local{ { `Id "f1" }, { `Number "10.0" } }, `Local{ { `Id "f2" }, { `Number "0.1" } } }
]=]

r = parse(s)
assert(r == fixint(e))

-- integers

s = [=[
local i = 1
local h = 0xff
]=]
e = [=[
{ `Local{ { `Id "i" }, { `Number "1" } }, `Local{ { `Id "h" }, { `Number "255" } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local h = 0x76c
local i = 4294967296 -- 2^32
]=]
e = [=[
{ `Local{ { `Id "h" }, { `Number "1900" } }, `Local{ { `Id "i" }, { `Number "4294967296" } } }
]=]

r = parse(s)
assert(r == e)

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
assert(r == e)

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
{ `Local{ { `Id "ls1" }, { `String "testing long string
" } } }
]=]

r = parse(s)
assert(r == e)

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
{ `Local{ { `Id "ls2" }, { `String " testing \\n [[ long ]] \\t [===[ string ]===]
\\a " } } }
]=]

r = parse(s)
assert(r == e)

-- short strings

s = [=[
-- short string test begin

local ss1_a = "ola mundo\a"
local ss1_b = 'ola mundo\a'

-- short string test end
]=]
e = [=[
{ `Local{ { `Id "ss1_a" }, { `String "ola mundo\\a" } }, `Local{ { `Id "ss1_b" }, { `String "ola mundo\\a" } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
-- short string test begin

local ss2_a = "testando,\tteste\n1\n2\n3 --> \"tchau\""
local ss2_b = 'testando,\tteste\n1\n2\n3 --> \'tchau\''

-- short string test end
]=]
e = [=[
{ `Local{ { `Id "ss2_a" }, { `String "testando,\\tteste\\n1\\n2\\n3 --> \"tchau\"" } }, `Local{ { `Id "ss2_b" }, { `String "testando,\\tteste\\n1\\n2\\n3 --> 'tchau'" } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
-- short string test begin

local ss3_a = "ola \
'mundo'!"

local ss3_b = 'ola \
"mundo"!'

-- short string test end
]=]
e = [=[
{ `Local{ { `Id "ss3_a" }, { `String "ola 
'mundo'!" } }, `Local{ { `Id "ss3_b" }, { `String "ola 
\"mundo\"!" } } }
]=]

r = parse(s)
assert(r == e)

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
assert(r == e)

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
{ `Local{ { `Id "ss5_a" }, { `String "ola 
mundo \\ 
cruel" } }, `Local{ { `Id "ss5_b" }, { `String "ola 
mundo \\ 
cruel" } } }
]=]

r = parse(s)
assert(r == e)

-- syntax error

-- floating points

s = [=[
local f = 9e
]=]
e = [=[
test.lua:2:1: syntax error, unexpected 'EOF', expecting '=', ',', 'String', '{', '(', ':', '[', '.'
]=]

r = parse(s)
assert(r == e)

s = [=[
local f = 5.e
]=]
e = [=[
test.lua:2:1: syntax error, unexpected 'EOF', expecting '=', ',', 'String', '{', '(', ':', '[', '.'
]=]

r = parse(s)
assert(r == e)

s = [=[
local f = .9e-
]=]
e = [=[
test.lua:1:14: syntax error, unexpected '-', expecting '=', ',', 'String', '{', '(', ':', '[', '.'
]=]

r = parse(s)
assert(r == e)

s = [=[
local f = 5.9e+
]=]
e = [=[
test.lua:1:15: syntax error, unexpected '+', expecting '=', ',', 'String', '{', '(', ':', '[', '.'
]=]

r = parse(s)
assert(r == e)

-- integers

s = [=[
-- invalid hexadecimal number

local hex = 0xG
]=]
e = [=[
test.lua:4:1: syntax error, unexpected 'EOF', expecting '=', ',', 'String', '{', '(', ':', '[', '.'
]=]

r = parse(s)
assert(r == e)

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
test.lua:5:13: syntax error, unexpected '[', expecting '(', 'Name', '{', 'function', '...', 'true', 'false', 'nil', 'String', 'Number', '#', '-', 'not'
]=]

r = parse(s)
assert(r == e)

-- short strings

s = [=[
-- short string test begin

local ss6 = "testing unfinished string

-- short string test end
]=]
e = [=[
test.lua:3:13: syntax error, unexpected '"', expecting '(', 'Name', '{', 'function', '...', 'true', 'false', 'nil', 'String', 'Number', '#', '-', 'not'
]=]

r = parse(s)
assert(r == e)

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
--assert(r == e)

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
assert(r == e)

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
assert(r == e)

s = [=[
local test = function ( a , b , ... ) end
]=]
e = [=[
{ `Local{ { `Id "test" }, { `Function{ { `Id "a", `Id "b", `Dots }, {  } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local test = function (...) return ...,0 end
]=]
e = [=[
{ `Local{ { `Id "test" }, { `Function{ { `Dots }, { `Return{ `Dots, `Number "0" } } } } } }
]=]

r = parse(s)
assert(r == e)

-- arithmetic expressions

s = [=[
local arithmetic = 1 - 2 * 3 + 4
]=]
e = [=[
{ `Local{ { `Id "arithmetic" }, { `Op{ "add", `Op{ "sub", `Number "1", `Op{ "mul", `Number "2", `Number "3" } }, `Number "4" } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local pow = -3^-2^2
]=]
e = [=[
{ `Local{ { `Id "pow" }, { `Op{ "unm", `Op{ "pow", `Number "3", `Op{ "unm", `Op{ "pow", `Number "2", `Number "2" } } } } } } }
]=]

r = parse(s)
assert(r == e)

-- assignments

s = [=[
a = f()[1]
]=]
e = [=[
{ `Set{ { `Index{ `Id "_ENV", `String "a" } }, { `Index{ `Call{ `Index{ `Id "_ENV", `String "f" } }, `Number "1" } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
a()[1] = 1;
]=]
e = [=[
{ `Set{ { `Index{ `Call{ `Index{ `Id "_ENV", `String "a" } }, `Number "1" } }, { `Number "1" } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
i = a.f(1)
]=]
e = [=[
{ `Set{ { `Index{ `Id "_ENV", `String "i" } }, { `Call{ `Index{ `Index{ `Id "_ENV", `String "a" }, `String "f" }, `Number "1" } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
i = a[f(1)]
]=]
e = [=[
{ `Set{ { `Index{ `Id "_ENV", `String "i" } }, { `Index{ `Index{ `Id "_ENV", `String "a" }, `Call{ `Index{ `Id "_ENV", `String "f" }, `Number "1" } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
a[f()] = sub
i = i + 1
]=]
e = [=[
{ `Set{ { `Index{ `Index{ `Id "_ENV", `String "a" }, `Call{ `Index{ `Id "_ENV", `String "f" } } } }, { `Index{ `Id "_ENV", `String "sub" } } }, `Set{ { `Index{ `Id "_ENV", `String "i" } }, { `Op{ "add", `Index{ `Id "_ENV", `String "i" }, `Number "1" } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
a:b(1)._ = some_value
]=]
e = [=[
{ `Set{ { `Index{ `Invoke{ `Index{ `Id "_ENV", `String "a" }, `String "b", `Number "1" }, `String "_" } }, { `Index{ `Id "_ENV", `String "some_value" } } } }
]=]

r = parse(s)
assert(r == e)

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
assert(r == e)

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
assert(r == e)

s = [=[
repeat
  if 2 > 1 then break end
until 1
]=]
e = [=[
{ `Repeat{ { `If{ `Op{ "lt", `Number "1", `Number "2" }, { `Break } } }, `Number "1" } }
]=]

r = parse(s)
assert(r == e)

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
assert(r == e)

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
assert(r == e)

-- concatenation expressions

s = [=[
local concat1 = 1 .. 2^3
]=]
e = [=[
{ `Local{ { `Id "concat1" }, { `Op{ "concat", `Number "1", `Op{ "pow", `Number "2", `Number "3" } } } } }
]=]

r = parse(s)
assert(r == e)

-- empty files

s = [=[
;
]=]
e = [=[
{  }
]=]

r = parse(s)
assert(r == e)

-- for generic

s = [=[
for k,v in pairs(t) do print (k,v) end
]=]
e = [=[
{ `Forin{ { `Id "k", `Id "v" }, { `Call{ `Index{ `Id "_ENV", `String "pairs" }, `Index{ `Id "_ENV", `String "t" } } }, { `Call{ `Index{ `Id "_ENV", `String "print" }, `Id "k", `Id "v" } } } }
]=]

r = parse(s)
assert(r == e)

-- for numeric

s = [=[
for i = 1 , 10 , 2 do end
]=]
e = [=[
{ `Fornum{ `Id "i", `Number "1", `Number "10", `Number "2", {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
for i=1,10 do end
]=]
e = [=[
{ `Fornum{ `Id "i", `Number "1", `Number "10", {  } } }
]=]

r = parse(s)
assert(r == e)

-- global functions

s = [=[
function test(a , b , ...) end
]=]
e = [=[
{ `Set{ { `Index{ `Id "_ENV", `String "test" } }, { `Function{ { `Id "a", `Id "b", `Dots }, {  } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
function test (...) end
]=]
e = [=[
{ `Set{ { `Index{ `Id "_ENV", `String "test" } }, { `Function{ { `Dots }, {  } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
function t.a:b() end
]=]
e = [=[
{ `Set{ { `Index{ `Index{ `Index{ `Id "_ENV", `String "t" }, `String "a" }, `String "b" } }, { `Function{ { `Id "self" }, {  } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
function t.a() end
]=]
e = [=[
{ `Set{ { `Index{ `Index{ `Id "_ENV", `String "t" }, `String "a" } }, { `Function{ {  }, {  } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
function testando . funcao . com : espcacos ( e, com , parametros, ... ) end
]=]
e = [=[
{ `Set{ { `Index{ `Index{ `Index{ `Index{ `Id "_ENV", `String "testando" }, `String "funcao" }, `String "com" }, `String "espcacos" } }, { `Function{ { `Id "self", `Id "e", `Id "com", `Id "parametros", `Dots }, {  } } } } }
]=]

r = parse(s)
assert(r == e)

-- goto

s = [=[
goto label
:: label :: return
]=]
e = [=[
{ `Goto{ "label" }, `Label{ "label" }, `Return }
]=]

r = parse(s)
assert(r == e)

s = [=[
::label::
goto label
]=]
e = [=[
{ `Label{ "label" }, `Goto{ "label" } }
]=]

r = parse(s)
assert(r == e)

s = [=[
goto label
::label::
]=]
e = [=[
{ `Goto{ "label" }, `Label{ "label" } }
]=]

r = parse(s)
assert(r == e)

s = [=[
::label::
do ::label:: goto label end
]=]
e = [=[
{ `Label{ "label" }, `Do{ `Label{ "label" }, `Goto{ "label" } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
::label::
do goto label ; ::label:: end
]=]
e = [=[
{ `Label{ "label" }, `Do{ `Goto{ "label" }, `Label{ "label" } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
::label::
do goto label end
]=]
e = [=[
{ `Label{ "label" }, `Do{ `Goto{ "label" } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
do goto label end
::label::
]=]
e = [=[
{ `Do{ `Goto{ "label" } }, `Label{ "label" } }
]=]

r = parse(s)
assert(r == e)

s = [=[
do do do do do goto label end end end end end
::label::
]=]
e = [=[
{ `Do{ `Do{ `Do{ `Do{ `Do{ `Goto{ "label" } } } } } }, `Label{ "label" } }
]=]

r = parse(s)
assert(r == e)

-- if-else

s = [=[
if a then end
]=]
e = [=[
{ `If{ `Index{ `Id "_ENV", `String "a" }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
if a then return a else return end
]=]
e = [=[
{ `If{ `Index{ `Id "_ENV", `String "a" }, { `Return{ `Index{ `Id "_ENV", `String "a" } } }, { `Return } } }
]=]

r = parse(s)
assert(r == e)

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
assert(r == e)

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
assert(r == e)

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
assert(r == e)

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
assert(r == e)

-- interfaces

s = [=[
local interface Empty end
]=]
e = [=[
{ `Interface{ Empty, `TTable{  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local interface X
  x, y, z:number
end
]=]
e = [=[
{ `Interface{ X, `TTable{ `TLiteral x:`TBase number, `TLiteral y:`TBase number, `TLiteral z:`TBase number } } }
]=]

r = parse(s)
assert(r == e)

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
assert(r == e)

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
assert(r == e)

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
assert(r == e)

-- locals

s = [=[
local a
]=]
e = [=[
{ `Local{ { `Id "a" }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local a,b,c
]=]
e = [=[
{ `Local{ { `Id "a", `Id "b", `Id "c" }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local a = 1 , 1 + 2, 5.1
]=]
e = [=[
{ `Local{ { `Id "a" }, { `Number "1", `Op{ "add", `Number "1", `Number "2" }, `Number "5.1" } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local a,b,c = 1.9
]=]
e = [=[
{ `Local{ { `Id "a", `Id "b", `Id "c" }, { `Number "1.9" } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local function test() end
]=]
e = [=[
{ `Localrec{ { `Id "test" }, { `Function{ {  }, {  } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local function test ( a , b , c , ... ) end
]=]
e = [=[
{ `Localrec{ { `Id "test" }, { `Function{ { `Id "a", `Id "b", `Id "c", `Dots }, {  } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local function test(...) return ... end
]=]
e = [=[
{ `Localrec{ { `Id "test" }, { `Function{ { `Dots }, { `Return{ `Dots } } } } } }
]=]

r = parse(s)
assert(r == e)

-- relational expressions

s = [=[
local relational = 1 < 2 >= 3 == 4 ~= 5 < 6 <= 7
]=]
e = [=[
{ `Local{ { `Id "relational" }, { `Op{ "le", `Op{ "lt", `Op{ "not", `Op{ "eq", `Op{ "eq", `Op{ "le", `Number "3", `Op{ "lt", `Number "1", `Number "2" } }, `Number "4" }, `Number "5" } }, `Number "6" }, `Number "7" } } } }
]=]

r = parse(s)
assert(r == e)

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
assert(r == e)

-- return

s = [=[
return
]=]
e = [=[
{ `Return }
]=]

r = parse(s)
assert(r == e)

s = [=[
return 1
]=]
e = [=[
{ `Return{ `Number "1" } }
]=]

r = parse(s)
assert(r == e)

s = [=[
return 1,1-2*3+4,"alo"
]=]
e = [=[
{ `Return{ `Number "1", `Op{ "add", `Op{ "sub", `Number "1", `Op{ "mul", `Number "2", `Number "3" } }, `Number "4" }, `String "alo" } }
]=]

r = parse(s)
assert(r == e)

s = [=[
return;
]=]
e = [=[
{ `Return }
]=]

r = parse(s)
assert(r == e)

s = [=[
return 1;
]=]
e = [=[
{ `Return{ `Number "1" } }
]=]

r = parse(s)
assert(r == e)

s = [=[
return 1,1-2*3+4,"alo";
]=]
e = [=[
{ `Return{ `Number "1", `Op{ "add", `Op{ "sub", `Number "1", `Op{ "mul", `Number "2", `Number "3" } }, `Number "4" }, `String "alo" } }
]=]

r = parse(s)
assert(r == e)

-- tables

s = [=[
local t = { [1] = "alo", alo = 1, 2; }
]=]
e = [=[
{ `Local{ { `Id "t" }, { `Table{ `Pair{ `Number "1", `String "alo" }, `Pair{ `String "alo", `Number "1" }, `Number "2" } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local t = { 1.5 }
]=]
e = [=[
{ `Local{ { `Id "t" }, { `Table{ `Number "1.5" } } } }
]=]

r = parse(s)
assert(r == e)

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
assert(r == e)

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
assert(r == e)

s = [=[
local t = {{{}}, {"alo"}}
]=]
e = [=[
{ `Local{ { `Id "t" }, { `Table{ `Table{ `Table }, `Table{ `String "alo" } } } } }
]=]

r = parse(s)
assert(r == e)

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
assert(r == e)

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
assert(r == e)

s = [=[
local function f (x, ...)
  return ...
end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ { `Id "x", `Dots }, { `Return{ `Dots } } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local f = function (x, ...)
  return ...
end
]=]
e = [=[
{ `Local{ { `Id "f" }, { `Function{ { `Id "x", `Dots }, { `Return{ `Dots } } } } } }
]=]

r = parse(s)
assert(r == e)

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
assert(r == e)

-- type annotations

s = [=[
local x:nil
]=]
e = [=[
{ `Local{ { `Id "x":`TNil }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:false, y:true
]=]
e = [=[
{ `Local{ { `Id "x":`TLiteral false, `Id "y":`TLiteral true }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:1, y:1.1
]=]
e = [=[
{ `Local{ { `Id "x":`TLiteral 1, `Id "y":`TLiteral 1.1 }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:"hello", y:'world' 
]=]
e = [=[
{ `Local{ { `Id "x":`TLiteral hello, `Id "y":`TLiteral world }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:boolean, y:number, z:string 
]=]
e = [=[
{ `Local{ { `Id "x":`TBase boolean, `Id "y":`TBase number, `Id "z":`TBase string }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:any
]=]
e = [=[
{ `Local{ { `Id "x":`TAny }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:number?
]=]
e = [=[
{ `Local{ { `Id "x":`TUnion{ `TBase number, `TNil } }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:number|nil
]=]
e = [=[
{ `Local{ { `Id "x":`TUnion{ `TBase number, `TNil } }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:number|string|nil
]=]
e = [=[
{ `Local{ { `Id "x":`TUnion{ `TBase number, `TBase string, `TNil } }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:number|nil|nil|nil|nil
]=]
e = [=[
{ `Local{ { `Id "x":`TUnion{ `TBase number, `TNil } }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:number|nil|string|nil|number|boolean|string
]=]
e = [=[
{ `Local{ { `Id "x":`TUnion{ `TNil, `TBase number, `TBase boolean, `TBase string } }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:number|string?
]=]
e = [=[
{ `Local{ { `Id "x":`TUnion{ `TBase number, `TBase string, `TNil } }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:(number) -> (number)
]=]
e = [=[
{ `Local{ { `Id "x":`TFunction{ `TTuple{ `TBase number, `TVararg{ `TValue } }, `TTuple{ `TBase number, `TVararg{ `TNil } } } }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:(value*) -> (nil*)
]=]
e = [=[
{ `Local{ { `Id "x":`TFunction{ `TTuple{ `TVararg{ `TValue } }, `TTuple{ `TVararg{ `TNil } } } }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:(number,string,boolean) -> (string,number,boolean)
]=]
e = [=[
{ `Local{ { `Id "x":`TFunction{ `TTuple{ `TBase number, `TBase string, `TBase boolean, `TVararg{ `TValue } }, `TTuple{ `TBase string, `TBase number, `TBase boolean, `TVararg{ `TNil } } } }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:(number,string,value*) -> (string,number,nil*)
]=]
e = [=[
{ `Local{ { `Id "x":`TFunction{ `TTuple{ `TBase number, `TBase string, `TVararg{ `TValue } }, `TTuple{ `TBase string, `TBase number, `TVararg{ `TNil } } } }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:{}
]=]
e = [=[
{ `Local{ { `Id "x":`TTable{  } }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:{{{{{}}}}}
]=]
e = [=[
{ `Local{ { `Id "x":`TTable{ `TBase number:`TUnion{ `TTable{ `TBase number:`TUnion{ `TTable{ `TBase number:`TUnion{ `TTable{ `TBase number:`TUnion{ `TTable{  }, `TNil } }, `TNil } }, `TNil } }, `TNil } } }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:{string}
]=]
e = [=[
{ `Local{ { `Id "x":`TTable{ `TBase number:`TUnion{ `TBase string, `TNil } } }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:{string:number}
]=]
e = [=[
{ `Local{ { `Id "x":`TTable{ `TBase string:`TUnion{ `TBase number, `TNil } } }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:{'firstname':string, 'lastname':string}
]=]
e = [=[
{ `Local{ { `Id "x":`TTable{ `TLiteral firstname:`TBase string, `TLiteral lastname:`TBase string } }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:{'tag':string, number:string}
]=]
e = [=[
{ `Local{ { `Id "x":`TTable{ `TLiteral tag:`TBase string, `TBase number:`TUnion{ `TBase string, `TNil } } }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:{'f':(number) -> (number), 't':{number:number}}
]=]
e = [=[
{ `Local{ { `Id "x":`TTable{ `TLiteral f:`TFunction{ `TTuple{ `TBase number, `TVararg{ `TValue } }, `TTuple{ `TBase number, `TVararg{ `TNil } } }, `TLiteral t:`TTable{ `TBase number:`TUnion{ `TBase number, `TNil } } } }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
for k:number, v:string in ipairs({"hello", "world"}) do end
]=]
e = [=[
{ `Forin{ { `Id "k":`TBase number, `Id "v":`TBase string }, { `Call{ `Index{ `Id "_ENV", `String "ipairs" }, `Table{ `String "hello", `String "world" } } }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
for k:string, v in pairs({}) do end
]=]
e = [=[
{ `Forin{ { `Id "k":`TBase string, `Id "v" }, { `Call{ `Index{ `Id "_ENV", `String "pairs" }, `Table } }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
for k, v:boolean in pairs({}) do end
]=]
e = [=[
{ `Forin{ { `Id "k", `Id "v":`TBase boolean }, { `Call{ `Index{ `Id "_ENV", `String "pairs" }, `Table } }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local function f (x:any) end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ { `Id "x":`TAny }, {  } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local function f (x:any):(any) end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ { `Id "x":`TAny }:`TTuple{ `TAny, `TVararg{ `TNil } }, {  } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local function f (...:any) end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ { `Dots:`TAny }, {  } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local function f (x:any, ...:any) end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ { `Id "x":`TAny, `Dots:`TAny }, {  } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local function f (x, ...:any) end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ { `Id "x", `Dots:`TAny }, {  } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local function f (x:any, ...) end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ { `Id "x":`TAny, `Dots }, {  } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local function f (x:any, ...:any):(any) end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ { `Id "x":`TAny, `Dots:`TAny }:`TTuple{ `TAny, `TVararg{ `TNil } }, {  } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local function f (x:(any) -> (any)):((any) -> (any)) end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ { `Id "x":`TFunction{ `TTuple{ `TAny, `TVararg{ `TValue } }, `TTuple{ `TAny, `TVararg{ `TNil } } } }:`TTuple{ `TFunction{ `TTuple{ `TAny, `TVararg{ `TValue } }, `TTuple{ `TAny, `TVararg{ `TNil } } }, `TVararg{ `TNil } }, {  } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local function f (x:(number, number) -> (number, nil*)):(number*) end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ { `Id "x":`TFunction{ `TTuple{ `TBase number, `TBase number, `TVararg{ `TValue } }, `TTuple{ `TBase number, `TVararg{ `TNil } } } }:`TTuple{ `TVararg{ `TBase number } }, {  } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local function f ():(number, nil*) end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ {  }:`TTuple{ `TBase number, `TVararg{ `TNil } }, {  } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local function f ():number end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ {  }:`TTuple{ `TBase number, `TVararg{ `TNil } }, {  } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local function f ():number? end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ {  }:`TTuple{ `TUnion{ `TBase number, `TNil }, `TVararg{ `TNil } }, {  } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local function f ():(number) | (nil,string) end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ {  }:`TUnionlist{ `TTuple{ `TBase number, `TVararg{ `TNil } }, `TTuple{ `TNil, `TBase string, `TVararg{ `TNil } } }, {  } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local function f ():(number)? end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ {  }:`TUnionlist{ `TTuple{ `TBase number, `TVararg{ `TNil } }, `TTuple{ `TNil, `TBase string, `TVararg{ `TNil } } }, {  } } } } }
]=]

r = parse(s)
assert(r == e)

-- syntax error

-- anonymous functions

s = [=[
a = function (a,b,) end
]=]
e = [=[
test.lua:1:19: syntax error, unexpected ')', expecting '...', 'Name'
]=]

r = parse(s)
assert(r == e)

s = [=[
a = function (...,a) end
]=]
e = [=[
test.lua:1:18: syntax error, unexpected ',', expecting ')', ':'
]=]

r = parse(s)
assert(r == e)

s = [=[
local a = function (1) end
]=]
e = [=[
test.lua:1:21: syntax error, unexpected '1', expecting ')', '...', 'Name'
]=]

r = parse(s)
assert(r == e)

s = [=[
local test = function ( a , b , c , ... )
]=]
e = [=[
test.lua:2:1: syntax error, unexpected 'EOF', expecting 'end', 'return', '(', 'Name', 'interface', 'goto', 'break', '::', 'local', 'function', 'const', 'repeat', 'for', 'do', 'while', 'if', ';', ':'
]=]

r = parse(s)
assert(r == e)

-- break

s = [=[
break
]=]
e = [=[
test.lua:1:1: syntax error, <break> not inside a loop
]=]

r = parse(s)
assert(r == e)

s = [=[
function f (x)
  if 1 then break end
end
]=]
e = [=[
test.lua:2:13: syntax error, <break> not inside a loop
]=]

r = parse(s)
assert(r == e)

s = [=[
while 1 do
end
break
]=]
e = [=[
test.lua:3:1: syntax error, <break> not inside a loop
]=]

r = parse(s)
assert(r == e)

-- concatenation expressions

s = [=[
concat2 = 2^3..1
]=]
e = [=[
test.lua:1:15: syntax error, unexpected '.1', expecting 'return', '(', 'Name', 'interface', 'goto', 'break', '::', 'local', 'function', 'const', 'repeat', 'for', 'do', 'while', 'if', ';', ',', 'or', 'and', '>', '<', '>=', '<=', '==', '~=', '..', '-', '+', '%', '/', '*', '^'
]=]

r = parse(s)
assert(r == e)

-- for generic

s = [=[
for k;v in pairs(t) do end
]=]
e = [=[
test.lua:1:6: syntax error, unexpected ';', expecting 'in', ',', ':', '='
]=]

r = parse(s)
assert(r == e)

s = [=[
for k,v in pairs(t:any) do end
]=]
e = [=[
test.lua:1:23: syntax error, unexpected ')', expecting 'String', '{', '('
]=]

r = parse(s)
assert(r == e)

-- for numeric

s = [=[
for i=1,10, do end
]=]
e = [=[
test.lua:1:13: syntax error, unexpected 'do', expecting '(', 'Name', '{', 'function', '...', 'true', 'false', 'nil', 'String', 'Number', '#', '-', 'not'
]=]

r = parse(s)
assert(r == e)

s = [=[
for i=1,n:number do end
]=]
e = [=[
test.lua:1:18: syntax error, unexpected 'do', expecting 'String', '{', '('
]=]

r = parse(s)
assert(r == e)

-- global functions

s = [=[
function func(a,b,c,) end
]=]
e = [=[
test.lua:1:21: syntax error, unexpected ')', expecting '...', 'Name'
]=]

r = parse(s)
assert(r == e)

s = [=[
function func(...,a) end
]=]
e = [=[
test.lua:1:18: syntax error, unexpected ',', expecting ')', ':'
]=]

r = parse(s)
assert(r == e)

s = [=[
function a.b:c:d () end
]=]
e = [=[
test.lua:1:15: syntax error, unexpected ':', expecting '('
]=]

r = parse(s)
assert(r == e)

-- goto

s = [=[
:: label :: return
goto label
]=]
e = [=[
test.lua:2:1: syntax error, unexpected 'goto', expecting ';', '(', 'Name', '{', 'function', '...', 'true', 'false', 'nil', 'String', 'Number', '#', '-', 'not'
]=]

r = parse(s)
assert(r == e)

s = [=[
goto label
]=]
e = [=[
test.lua:1:1: syntax error, no visible label 'label' for <goto>
]=]

r = parse(s)
assert(r == e)

s = [=[
goto label
::other_label::
]=]
e = [=[
test.lua:1:1: syntax error, no visible label 'label' for <goto>
]=]

r = parse(s)
assert(r == e)

s = [=[
::other_label::
do do do goto label end end end
]=]
e = [=[
test.lua:2:10: syntax error, no visible label 'label' for <goto>
]=]

r = parse(s)
assert(r == e)

-- if-else

s = [=[
if a then
]=]
e = [=[
test.lua:2:1: syntax error, unexpected 'EOF', expecting 'end', 'else', 'elseif', 'return', '(', 'Name', 'interface', 'goto', 'break', '::', 'local', 'function', 'const', 'repeat', 'for', 'do', 'while', 'if', ';'
]=]

r = parse(s)
assert(r == e)

s = [=[
if a then else
]=]
e = [=[
test.lua:2:1: syntax error, unexpected 'EOF', expecting 'end', 'return', '(', 'Name', 'interface', 'goto', 'break', '::', 'local', 'function', 'const', 'repeat', 'for', 'do', 'while', 'if', ';'
]=]

r = parse(s)
assert(r == e)

s = [=[
if a then
  return a
elseif b then
  return b
elseif

end
]=]
e = [=[
test.lua:7:1: syntax error, unexpected 'end', expecting '(', 'Name', '{', 'function', '...', 'true', 'false', 'nil', 'String', 'Number', '#', '-', 'not'
]=]

r = parse(s)
assert(r == e)

s = [=[
if a:any then else end
]=]
e = [=[
test.lua:1:10: syntax error, unexpected 'then', expecting 'String', '{', '('
]=]

r = parse(s)
assert(r == e)

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
assert(r == e)

s = [=[
local interface X
 x, y, z, x:number
end
]=]
e = [=[
test.lua:1:7: syntax error, attempt to redeclare field 'x'
]=]

r = parse(s)
assert(r == e)

s = [=[
local interface boolean end
]=]
e = [=[
test.lua:1:7: syntax error, attempt to redeclare type 'boolean'
]=]

r = parse(s)
assert(r == e)

s = [=[
local interface number end
]=]
e = [=[
test.lua:1:7: syntax error, attempt to redeclare type 'number'
]=]

r = parse(s)
assert(r == e)

s = [=[
local interface string end
]=]
e = [=[
test.lua:1:7: syntax error, attempt to redeclare type 'string'
]=]

r = parse(s)
assert(r == e)

s = [=[
local interface value end
]=]
e = [=[
test.lua:1:7: syntax error, attempt to redeclare type 'value'
]=]

r = parse(s)
assert(r == e)

s = [=[
local interface any end
]=]
e = [=[
test.lua:1:7: syntax error, attempt to redeclare type 'any'
]=]

r = parse(s)
assert(r == e)

s = [=[
local interface self end
]=]
e = [=[
test.lua:1:7: syntax error, attempt to redeclare type 'self'
]=]

r = parse(s)
assert(r == e)

s = [=[
local interface const end
]=]
e = [=[
test.lua:1:7: syntax error, attempt to redeclare type 'const'
]=]

r = parse(s)
assert(r == e)

-- labels

s = [=[
:: blah ::
:: not ::
]=]
e = [=[
test.lua:2:4: syntax error, unexpected 'not', expecting 'Name'
]=]

r = parse(s)
assert(r == e)

s = [=[
::label::
::other_label::
::label::
]=]
e = [=[
test.lua:3:1: syntax error, label 'label' already defined
]=]

r = parse(s)
assert(r == e)

-- locals

s = [=[
local a =
]=]
e = [=[
test.lua:2:1: syntax error, unexpected 'EOF', expecting '(', 'Name', '{', 'function', '...', 'true', 'false', 'nil', 'String', 'Number', '#', '-', 'not'
]=]

r = parse(s)
assert(r == e)

s = [=[
local function t.a() end
]=]
e = [=[
test.lua:1:17: syntax error, unexpected '.', expecting '('
]=]

r = parse(s)
assert(r == e)

s = [=[
local function test (a,) end
]=]
e = [=[
test.lua:1:24: syntax error, unexpected ')', expecting '...', 'Name'
]=]

r = parse(s)
assert(r == e)

s = [=[
local function test(...,a) end
]=]
e = [=[
test.lua:1:24: syntax error, unexpected ',', expecting ')', ':'
]=]

r = parse(s)
assert(r == e)

s = [=[
local function (a, b, c, ...) end
]=]
e = [=[
test.lua:1:16: syntax error, unexpected '(', expecting 'Name'
]=]

r = parse(s)
assert(r == e)

-- repeat

s = [=[
repeat
  a,b,c = 1+1,2+2,3+3
  break
]=]
e = [=[
test.lua:4:1: syntax error, unexpected 'EOF', expecting 'until', 'return', '(', 'Name', 'interface', 'goto', 'break', '::', 'local', 'function', 'const', 'repeat', 'for', 'do', 'while', 'if', ';'
]=]

r = parse(s)
assert(r == e)

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
test.lua:2:1: syntax error, unexpected 'return', expecting ';', '(', 'Name', '{', 'function', '...', 'true', 'false', 'nil', 'String', 'Number', '#', '-', 'not'
]=]

r = parse(s)
assert(r == e)

-- tables

s = [=[
t = { , }
]=]
e = [=[
test.lua:1:7: syntax error, unexpected ',', expecting '}', '(', '{', 'function', '...', 'true', 'false', 'nil', 'String', 'Number', '#', '-', 'not', 'Name', '[', 'const'
]=]

r = parse(s)
assert(r == e)

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
assert(r == e)

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
assert(r == e)

s = [=[
local function f (x)
  return ...
end
]=]
e = [=[
test.lua:2:10: syntax error, cannot use '...' outside a vararg function
]=]

r = parse(s)
assert(r == e)

s = [=[
local f = function (x)
  return ...
end
]=]
e = [=[
test.lua:2:10: syntax error, cannot use '...' outside a vararg function
]=]

r = parse(s)
assert(r == e)

-- while

s = [=[
i = 0
while (i < 10)
  i = i + 1
end
]=]
e = [=[
test.lua:3:3: syntax error, unexpected 'i', expecting 'do', 'or', 'and', '>', '<', '>=', '<=', '==', '~=', '..', '-', '+', '%', '/', '*', '^', 'String', '{', '(', ':', '[', '.'
]=]

r = parse(s)
assert(r == e)

-- type annotations

s = [=[
t[x:any] = 1
]=]
e = [=[
test.lua:1:8: syntax error, unexpected ']', expecting 'String', '{', '('
]=]

r = parse(s)
assert(r == e)

s = [=[
x:number, y, z:boolean = 1, nil, true
]=]
e = [=[
test.lua:1:9: syntax error, unexpected ',', expecting 'String', '{', '('
]=]

r = parse(s)
assert(r == e)

s = [=[
x = x:any
]=]
e = [=[
test.lua:2:1: syntax error, unexpected 'EOF', expecting 'String', '{', '('
]=]

r = parse(s)
assert(r == e)

s = [=[
x = ...:any
]=]
e = [=[
test.lua:1:8: syntax error, unexpected ':', expecting 'return', '(', 'Name', 'interface', 'goto', 'break', '::', 'local', 'function', 'const', 'repeat', 'for', 'do', 'while', 'if', ';', ',', 'or', 'and', '>', '<', '>=', '<=', '==', '~=', '..', '-', '+', '%', '/', '*', '^'
]=]

r = parse(s)
assert(r == e)

s = [=[
f(x:any)
]=]
e = [=[
test.lua:1:8: syntax error, unexpected ')', expecting 'String', '{', '('
]=]

r = parse(s)
assert(r == e)

s = [=[
f(...:any)
]=]
e = [=[
test.lua:1:6: syntax error, unexpected ':', expecting ')', ',', 'or', 'and', '>', '<', '>=', '<=', '==', '~=', '..', '-', '+', '%', '/', '*', '^'
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:number*
]=]
e = [=[
test.lua:1:15: syntax error, unexpected '*', expecting 'return', '(', 'Name', 'interface', 'goto', 'break', '::', 'local', 'function', 'const', 'repeat', 'for', 'do', 'while', 'if', ';', '=', ',', '?', '|'
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:number|
]=]
e = [=[
test.lua:2:1: syntax error, unexpected 'EOF', expecting '{', '(', 'Type'
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:number?|string?
]=]
e = [=[
test.lua:1:16: syntax error, unexpected '|', expecting 'return', '(', 'Name', 'interface', 'goto', 'break', '::', 'local', 'function', 'const', 'repeat', 'for', 'do', 'while', 'if', ';', '=', ','
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:() -> number
]=]
e = [=[
test.lua:1:15: syntax error, unexpected 'number', expecting '('
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:() -> (number)? | (string)?
]=]
e = [=[
test.lua:1:35: syntax error, unexpected '?', expecting '->'
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:{()->():string}
]=]
e = [=[
test.lua:1:16: syntax error, unexpected ':', expecting '}', '?', '|'
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:{string:t 1}
]=]
e = [=[
test.lua:1:19: syntax error, unexpected '1', expecting '}', '?', '|'
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:{{{{{}}}}
]=]
e = [=[
test.lua:2:1: syntax error, unexpected 'EOF', expecting '}', '?', '|'
]=]

r = parse(s)
assert(r == e)

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

assert(tltype.isLiteral(False))
assert(tltype.isFalse(False))

assert(tltype.isLiteral(True))
assert(tltype.isTrue(True))

assert(tltype.isLiteral(Double))
assert(tltype.isNum(Double))

assert(tltype.isLiteral(Integer))
assert(tltype.isNum(Integer))

assert(tltype.isLiteral(Word))
assert(tltype.isStr(Word))

assert(tltype.isBase(Boolean))
assert(tltype.isBoolean(Boolean))

assert(tltype.isBase(Number))
assert(tltype.isNumber(Number))

assert(tltype.isBase(String))
assert(tltype.isString(String))

assert(tltype.isNil(Nil))

assert(tltype.isValue(Value))

assert(tltype.isAny(Any))

assert(tltype.isUnion(tltype.Union(Number,Nil)))
assert(tltype.isUnion(tltype.Union(tltype.Union(Number,String),Nil)))
assert(tltype.isUnion(tltype.Union(tltype.Union(Number,Nil),String)))
assert(tltype.isUnion(tltype.Union(tltype.Union(Nil,Number),String)))

assert(tltype.isUnion(tltype.Union(Number,Nil),Nil))
assert(tltype.isUnion(tltype.Union(Number,String,Nil),Nil))
assert(tltype.isUnion(tltype.Union(Number,Nil,String),Nil))
assert(tltype.isUnion(tltype.Union(Nil,Number,String),Nil))

assert(not tltype.isUnion(tltype.Union(Number,Boolean),Nil))
assert(not tltype.isUnion(tltype.Union(tltype.Union(Number,String),Boolean),Nil))
assert(not tltype.isUnion(tltype.Union(tltype.Union(Number,Boolean),String),Nil))
assert(not tltype.isUnion(tltype.Union(tltype.Union(Boolean,Number),String),Nil))

t1 = tltype.Vararg(Value)
t2 = tltype.Vararg(Nil)

assert(tltype.isFunction(tltype.Function(tltype.Tuple(t1), tltype.Tuple(t2))))
assert(tltype.isTuple(tltype.Tuple(t1)))
assert(tltype.isTuple(tltype.Tuple(t2)))
assert(tltype.isVararg(t1))
assert(tltype.isVararg(t2))

assert(not tltype.isFunction(Nil))
assert(not tltype.isTuple(t1))
assert(not tltype.isTuple(t2))
assert(not tltype.isVararg(tltype.Tuple(t1)))
assert(not tltype.isVararg(tltype.Tuple(t2)))

-- subtyping

assert(tltype.subtype(False,False))
assert(tltype.subtype(True,True))
assert(tltype.subtype(Double,Double))
assert(tltype.subtype(Integer,Integer))
assert(tltype.subtype(Word,Word))

assert(tltype.subtype(False,Boolean))
assert(tltype.subtype(True,Boolean))
assert(tltype.subtype(Double,Number))
assert(tltype.subtype(Integer,Number))
assert(tltype.subtype(Word,String))

assert(not tltype.subtype(Nil,False))
assert(not tltype.subtype(False,True))
assert(not tltype.subtype(True,Double))
assert(not tltype.subtype(Double,Integer))
assert(not tltype.subtype(Integer,Word))
assert(not tltype.subtype(Word,Nil))

assert(tltype.subtype(Nil,Nil))
assert(tltype.subtype(Boolean,Boolean))
assert(tltype.subtype(Number,Number))
assert(tltype.subtype(String,String))

assert(not tltype.subtype(Boolean,False))
assert(not tltype.subtype(Boolean,True))
assert(not tltype.subtype(Number,Double))
assert(not tltype.subtype(Number,Integer))
assert(not tltype.subtype(String,Word))

assert(tltype.subtype(False,Value))
assert(tltype.subtype(True,Value))
assert(tltype.subtype(Double,Value))
assert(tltype.subtype(Integer,Value))
assert(tltype.subtype(Word,Value))
assert(tltype.subtype(Nil,Value))
assert(tltype.subtype(Boolean,Value))
assert(tltype.subtype(Number,Value))
assert(tltype.subtype(String,Value))
assert(tltype.subtype(Value,Value))
assert(tltype.subtype(Any,Value))
assert(tltype.subtype(tltype.Union(Number,Nil),Value))

assert(not tltype.subtype(Value,False))
assert(not tltype.subtype(Value,True))
assert(not tltype.subtype(Value,Double))
assert(not tltype.subtype(Value,Integer))
assert(not tltype.subtype(Value,Word))
assert(not tltype.subtype(Value,Nil))
assert(not tltype.subtype(Value,Boolean))
assert(not tltype.subtype(Value,Number))
assert(not tltype.subtype(Value,String))
assert(not tltype.subtype(Value,Any))
assert(not tltype.subtype(Value,tltype.Union(Number,Nil)))

assert(tltype.subtype(Any,Any))

assert(not tltype.subtype(Nil,Any))
assert(not tltype.subtype(False,Any))
assert(not tltype.subtype(True,Any))
assert(not tltype.subtype(Double,Any))
assert(not tltype.subtype(Integer,Any))
assert(not tltype.subtype(Word,Any))

assert(not tltype.subtype(Boolean,Any))
assert(not tltype.subtype(Number,Any))
assert(not tltype.subtype(String,Any))

assert(not tltype.subtype(Any,Nil))
assert(not tltype.subtype(Any,False))
assert(not tltype.subtype(Any,True))
assert(not tltype.subtype(Any,Double))
assert(not tltype.subtype(Any,Integer))
assert(not tltype.subtype(Any,Word))

assert(not tltype.subtype(Any,Boolean))
assert(not tltype.subtype(Any,Number))
assert(not tltype.subtype(Any,String))

t = tltype.Union(Number,Nil)

assert(tltype.subtype(Number,t))
assert(tltype.subtype(Nil,t))
assert(tltype.subtype(t,t))

assert(not tltype.subtype(t,Number))
assert(not tltype.subtype(t,Nil))

t = tltype.Union(Number,Any)

assert(tltype.subtype(Any,t))
assert(tltype.subtype(t,Any))
assert(tltype.subtype(t,t))

assert(not tltype.subtype(String,t))
assert(not tltype.subtype(Number,t))

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

assert(tltype.subtype( t1, t1))
assert(tltype.subtype(t2, t2))
assert(tltype.subtype(t3, t3))
assert(tltype.subtype(t4, t4))

assert(not tltype.subtype(t1, t3))
assert(not tltype.subtype(t1, t4))
assert(not tltype.subtype(t2, t3))
assert(not tltype.subtype(t2, t4))

t1 = tltype.Tuple({ Value }, true)
t2 = tltype.Tuple({ Nil }, true)

assert(tltype.subtype(tltype.Function(t1,t2), tltype.Function(t1,t2)))
assert(tltype.subtype(tltype.Function(t1,t2), tltype.Function(t2,t1)))
assert(tltype.subtype(tltype.Function(t2,t1), tltype.Function(t2,t1)))
assert(not tltype.subtype(tltype.Function(t2,t1), tltype.Function(t1,t2)))

t1 = tltype.Tuple({ Number }, true)

assert(tltype.subtype(tltype.Function(t1,t2), tltype.Function(t1,t2)))
assert(tltype.subtype(tltype.Function(t1,t2), tltype.Function(t2,t1)))
assert(tltype.subtype(tltype.Function(t2,t1), tltype.Function(t2,t1)))
assert(not tltype.subtype(tltype.Function(t2,t1), tltype.Function(t1,t2)))

t3 = tltype.Vararg(Nil)
t4 = tltype.Tuple({ Number, Number, t3 })

assert(tltype.subtype(tltype.Function(t1,t2), tltype.Function(t2,t2)))
assert(not tltype.subtype(tltype.Function(t3,t2), tltype.Function(t1,t2)))

t3 = tltype.Vararg(Number)
t4 = tltype.Tuple({ Number, Number, Number, t3 })

assert(tltype.subtype(tltype.Function(t1,t1), tltype.Function(t4,t1)))
assert(not tltype.subtype(tltype.Function(t4,t1), tltype.Function(t1,t1)))

-- consistent-subtyping

assert(tltype.consistent_subtype(False,False))
assert(tltype.consistent_subtype(True,True))
assert(tltype.consistent_subtype(Double,Double))
assert(tltype.consistent_subtype(Integer,Integer))
assert(tltype.consistent_subtype(Word,Word))

assert(tltype.consistent_subtype(False,Boolean))
assert(tltype.consistent_subtype(True,Boolean))
assert(tltype.consistent_subtype(Double,Number))
assert(tltype.consistent_subtype(Integer,Number))
assert(tltype.consistent_subtype(Word,String))

assert(not tltype.consistent_subtype(Nil,False))
assert(not tltype.consistent_subtype(False,True))
assert(not tltype.consistent_subtype(True,Double))
assert(not tltype.consistent_subtype(Double,Integer))
assert(not tltype.consistent_subtype(Integer,Word))
assert(not tltype.consistent_subtype(Word,Nil))

assert(tltype.consistent_subtype(Nil,Nil))
assert(tltype.consistent_subtype(Boolean,Boolean))
assert(tltype.consistent_subtype(Number,Number))
assert(tltype.consistent_subtype(String,String))

assert(not tltype.consistent_subtype(Boolean,False))
assert(not tltype.consistent_subtype(Boolean,True))
assert(not tltype.consistent_subtype(Number,Double))
assert(not tltype.consistent_subtype(Number,Integer))
assert(not tltype.consistent_subtype(String,Word))

assert(tltype.consistent_subtype(False,Value))
assert(tltype.consistent_subtype(True,Value))
assert(tltype.consistent_subtype(Double,Value))
assert(tltype.consistent_subtype(Integer,Value))
assert(tltype.consistent_subtype(Word,Value))
assert(tltype.consistent_subtype(Nil,Value))
assert(tltype.consistent_subtype(Boolean,Value))
assert(tltype.consistent_subtype(Number,Value))
assert(tltype.consistent_subtype(String,Value))
assert(tltype.consistent_subtype(Value,Value))
assert(tltype.consistent_subtype(tltype.Union(Number,Nil),Value))

assert(not tltype.consistent_subtype(Value,False))
assert(not tltype.consistent_subtype(Value,True))
assert(not tltype.consistent_subtype(Value,Double))
assert(not tltype.consistent_subtype(Value,Integer))
assert(not tltype.consistent_subtype(Value,Word))
assert(not tltype.consistent_subtype(Value,Nil))
assert(not tltype.consistent_subtype(Value,Boolean))
assert(not tltype.consistent_subtype(Value,Number))
assert(not tltype.consistent_subtype(Value,String))
assert(not tltype.consistent_subtype(Value,tltype.Union(Number,Nil)))

assert(tltype.consistent_subtype(Any,Any))

assert(tltype.consistent_subtype(Any,Value))
assert(tltype.consistent_subtype(Value,Any))

assert(tltype.consistent_subtype(Nil,Any))
assert(tltype.consistent_subtype(False,Any))
assert(tltype.consistent_subtype(True,Any))
assert(tltype.consistent_subtype(Double,Any))
assert(tltype.consistent_subtype(Integer,Any))
assert(tltype.consistent_subtype(Word,Any))

assert(tltype.consistent_subtype(Boolean,Any))
assert(tltype.consistent_subtype(Number,Any))
assert(tltype.consistent_subtype(String,Any))

assert(tltype.consistent_subtype(Any,Nil))
assert(tltype.consistent_subtype(Any,False))
assert(tltype.consistent_subtype(Any,True))
assert(tltype.consistent_subtype(Any,Double))
assert(tltype.consistent_subtype(Any,Integer))
assert(tltype.consistent_subtype(Any,Word))

assert(tltype.consistent_subtype(Any,Boolean))
assert(tltype.consistent_subtype(Any,Number))
assert(tltype.consistent_subtype(Any,String))

t = tltype.Union(Number,Nil)

assert(tltype.consistent_subtype(Number,t))
assert(tltype.consistent_subtype(Nil,t))
assert(tltype.consistent_subtype(t,t))

assert(not tltype.consistent_subtype(t,Number))
assert(not tltype.consistent_subtype(t,Nil))

t = tltype.Union(Number,Any)

assert(tltype.consistent_subtype(Number,t))
assert(tltype.consistent_subtype(Any,t))
assert(tltype.consistent_subtype(t,t))
assert(tltype.consistent_subtype(String,t))
assert(tltype.consistent_subtype(t,Any))
assert(tltype.consistent_subtype(t,String))

t1 = tltype.Tuple({ Any }, true)
t2 = tltype.Tuple({ Nil }, true)

assert(tltype.consistent_subtype(tltype.Function(t1,t2), tltype.Function(t1,t2)))
assert(tltype.consistent_subtype(tltype.Function(t1,t2), tltype.Function(t2,t1)))
assert(tltype.consistent_subtype(tltype.Function(t2,t1), tltype.Function(t2,t1)))
assert(tltype.consistent_subtype(tltype.Function(t2,t1), tltype.Function(t1,t2)))

t2 = tltype.Tuple({ Number }, true)

assert(tltype.consistent_subtype(tltype.Function(t1,t2), tltype.Function(t1,t2)))
assert(tltype.consistent_subtype(tltype.Function(t1,t2), tltype.Function(t2,t1)))
assert(tltype.consistent_subtype(tltype.Function(t2,t1), tltype.Function(t2,t1)))
assert(tltype.consistent_subtype(tltype.Function(t2,t1), tltype.Function(t1,t2)))

t3 = tltype.Vararg(Any)
t4 = tltype.Tuple({ Any, Any, t3 }, false)

assert(tltype.consistent_subtype(tltype.Function(t1,t2), tltype.Function(t2,t1)))
assert(tltype.consistent_subtype(tltype.Function(t4,t2), tltype.Function(t1,t2)))
assert(tltype.consistent_subtype(tltype.Function(t1,t2), tltype.Function(t4,t2)))

print("> testing type checker...")

-- type check

s = [=[
local x:value, y:value, z:value = 1, "foo"
]=]
e = [=[
{ `Local{ { `Id "x":`TValue, `Id "y":`TValue, `Id "z":`TValue }, { `Number "1", `String "foo" } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x, y, z = 1, "foo", false
]=]
e = [=[
{ `Local{ { `Id "x", `Id "y", `Id "z" }, { `Number "1", `String "foo", `False } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:number, y:string, z:boolean = 1, "foo", false
]=]
e = [=[
{ `Local{ { `Id "x":`TBase number, `Id "y":`TBase string, `Id "z":`TBase boolean }, { `Number "1", `String "foo", `False } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:boolean, y:nil = true, nil
]=]
e = [=[
{ `Local{ { `Id "x":`TBase boolean, `Id "y":`TNil }, { `True, `Nil } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:number?, y:number|nil = 1 
]=]
e = [=[
{ `Local{ { `Id "x":`TUnion{ `TBase number, `TNil }, `Id "y":`TUnion{ `TBase number, `TNil } }, { `Number "1" } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:number = 1 + 1
]=]
e = [=[
{ `Local{ { `Id "x":`TBase number }, { `Op{ "add", `Number "1", `Number "1" } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:string = "hello" .. "world"
]=]
e = [=[
{ `Local{ { `Id "x":`TBase string }, { `Op{ "concat", `String "hello", `String "world" } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:boolean, y:boolean = nil == false, false == true
]=]
e = [=[
{ `Local{ { `Id "x":`TBase boolean, `Id "y":`TBase boolean }, { `Op{ "eq", `Nil, `False }, `Op{ "eq", `False, `True } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:boolean, y:boolean = 1 == 2, "foo" == "bar"
]=]
e = [=[
{ `Local{ { `Id "x":`TBase boolean, `Id "y":`TBase boolean }, { `Op{ "eq", `Number "1", `Number "2" }, `Op{ "eq", `String "foo", `String "bar" } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:boolean, y:boolean = 1 < 2, "foo" < "bar"
]=]
e = [=[
{ `Local{ { `Id "x":`TBase boolean, `Id "y":`TBase boolean }, { `Op{ "lt", `Number "1", `Number "2" }, `Op{ "lt", `String "foo", `String "bar" } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:nil, y:boolean = nil and 1, false and 1 
]=]
e = [=[
{ `Local{ { `Id "x":`TNil, `Id "y":`TBase boolean }, { `Op{ "and", `Nil, `Number "1" }, `Op{ "and", `False, `Number "1" } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:number, y:string? = 1 and 2, "foo" and nil
]=]
e = [=[
{ `Local{ { `Id "x":`TBase number, `Id "y":`TUnion{ `TBase string, `TNil } }, { `Op{ "and", `Number "1", `Number "2" }, `Op{ "and", `String "foo", `Nil } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:number, y:number = nil or 1, false or 1 
]=]
e = [=[
{ `Local{ { `Id "x":`TBase number, `Id "y":`TBase number }, { `Op{ "or", `Nil, `Number "1" }, `Op{ "or", `False, `Number "1" } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:number, y:string? = 1 or 2, "foo" or nil
]=]
e = [=[
{ `Local{ { `Id "x":`TBase number, `Id "y":`TUnion{ `TBase string, `TNil } }, { `Op{ "or", `Number "1", `Number "2" }, `Op{ "or", `String "foo", `Nil } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:number?
local y:number = x or 0
]=]
e = [=[
{ `Local{ { `Id "x":`TUnion{ `TBase number, `TNil } }, {  } }, `Local{ { `Id "y":`TBase number }, { `Op{ "or", `Id "x", `Number "0" } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:boolean, y:boolean = not nil, not false
]=]
e = [=[
{ `Local{ { `Id "x":`TBase boolean, `Id "y":`TBase boolean }, { `Op{ "not", `Nil }, `Op{ "not", `False } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:number = -1
]=]
e = [=[
{ `Local{ { `Id "x":`TBase number }, { `Op{ "unm", `Number "1" } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:number = #"foo"
]=]
e = [=[
{ `Local{ { `Id "x":`TBase number }, { `Op{ "len", `String "foo" } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
while 1 do break end
]=]
e = [=[
{ `While{ `Number "1", { `Break } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
repeat break until 1
]=]
e = [=[
{ `Repeat{ { `Break }, `Number "1" } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
if 1 then local x = 1 end
]=]
e = [=[
{ `If{ `Number "1", { `Local{ { `Id "x" }, { `Number "1" } } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
if 1 then local x = 1 else local x = "foo" end
]=]
e = [=[
{ `If{ `Number "1", { `Local{ { `Id "x" }, { `Number "1" } } }, { `Local{ { `Id "x" }, { `String "foo" } } } } }
]=]

r = typecheck(s)
assert(r == e)

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
assert(r == e)

s = [=[
local x:number?
if x then
  x = x + 1
else
  print("x is nil")
end
]=]
e = [=[
{ `Local{ { `Id "x":`TUnion{ `TBase number, `TNil } }, {  } }, `If{ `Id "x", { `Set{ { `Id "x" }, { `Op{ "add", `Id "x", `Number "1" } } } }, { `Call{ `Index{ `Id "_ENV", `String "print" }, `String "x is nil" } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:number?
if not x then
  print("x is nil")
else
  x = x + 1
end
]=]
e = [=[
{ `Local{ { `Id "x":`TUnion{ `TBase number, `TNil } }, {  } }, `If{ `Op{ "not", `Id "x" }, { `Call{ `Index{ `Id "_ENV", `String "print" }, `String "x is nil" } }, { `Set{ { `Id "x" }, { `Op{ "add", `Id "x", `Number "1" } } } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:number?
if type(x) == "number" then
  x = x + 1
else
  print("x is nil")
end
]=]
e = [=[
{ `Local{ { `Id "x":`TUnion{ `TBase number, `TNil } }, {  } }, `If{ `Op{ "eq", `Call{ `Index{ `Id "_ENV", `String "type" }, `Id "x" }, `String "number" }, { `Set{ { `Id "x" }, { `Op{ "add", `Id "x", `Number "1" } } } }, { `Call{ `Index{ `Id "_ENV", `String "print" }, `String "x is nil" } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:number?
if type(x) ~= "number" then
  print("x is nil")
else
  x = x + 1
end
]=]
e = [=[
{ `Local{ { `Id "x":`TUnion{ `TBase number, `TNil } }, {  } }, `If{ `Op{ "not", `Op{ "eq", `Call{ `Index{ `Id "_ENV", `String "type" }, `Id "x" }, `String "number" } }, { `Call{ `Index{ `Id "_ENV", `String "print" }, `String "x is nil" } }, { `Set{ { `Id "x" }, { `Op{ "add", `Id "x", `Number "1" } } } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:number|string?
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
]=]
e = [=[
{ `Local{ { `Id "x":`TUnion{ `TBase number, `TBase string, `TNil } }, {  } }, `Local{ { `Id "y" }, { `Id "x" } }, `If{ `Op{ "eq", `Call{ `Index{ `Id "_ENV", `String "type" }, `Id "x" }, `String "number" }, { `Set{ { `Id "x" }, { `Op{ "add", `Id "x", `Number "1" } } } }, `Op{ "eq", `Call{ `Index{ `Id "_ENV", `String "type" }, `Id "y" }, `String "string" }, { `Set{ { `Id "y" }, { `Op{ "concat", `Id "y", `String "hello" } } } }, `Op{ "eq", `Call{ `Index{ `Id "_ENV", `String "type" }, `Id "x" }, `String "string" }, { `Set{ { `Id "x" }, { `Op{ "concat", `Id "x", `String "hello" } } } }, `Op{ "eq", `Call{ `Index{ `Id "_ENV", `String "type" }, `Id "y" }, `String "number" }, { `Set{ { `Id "y" }, { `Op{ "add", `Id "y", `Number "1" } } } } }, `Set{ { `Id "x" }, { `Id "y" } }, `Set{ { `Id "y" }, { `Id "x" } } }
]=]

r = typecheck(s)
assert(r == e)

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
assert(r == e)

s = [=[
for i = 1, 10 do local x = i end
]=]
e = [=[
{ `Fornum{ `Id "i":`TBase number, `Number "1", `Number "10", { `Local{ { `Id "x" }, { `Id "i" } } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
for i = 10, 1, -1 do local x = i end
]=]
e = [=[
{ `Fornum{ `Id "i":`TBase number, `Number "10", `Number "1", `Op{ "unm", `Number "1" }, { `Local{ { `Id "x" }, { `Id "i" } } } } }
]=]

r = typecheck(s)
assert(r == e)

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
assert(r == e)

s = [=[
local x:boolean, y:boolean, z:number = 1, "foo"
]=]
e = [=[
test.lua:1:7: type error, attempt to assign '1' to 'boolean'
test.lua:1:18: type error, attempt to assign 'foo' to 'boolean'
test.lua:1:29: type error, attempt to assign 'nil' to 'number'
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:number, y:number, z:string = false, true
]=]
e = [=[
test.lua:1:7: type error, attempt to assign 'false' to 'number'
test.lua:1:17: type error, attempt to assign 'true' to 'number'
test.lua:1:27: type error, attempt to assign 'nil' to 'string'
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x, y = 1 + "foo", "foo" + 1
]=]
e = [=[
test.lua:1:18: type error, attempt to perform arithmetic on a 'string'
test.lua:1:25: type error, attempt to perform arithmetic on a 'string'
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x, y = "foo" .. 1, 1 .. "foo"
]=]
e = [=[
test.lua:1:23: type error, attempt to concatenate a 'number'
test.lua:1:26: type error, attempt to concatenate a 'number'
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x, y = 1 < "foo", "foo" < 1
]=]
e = [=[
test.lua:1:14: type error, attempt to compare 'number' with 'string'
test.lua:1:25: type error, attempt to compare 'string' with 'number'
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x, y = nil < 1, true < "false"
]=]
e = [=[
test.lua:1:14: type error, attempt to compare 'nil' with 'number'
test.lua:1:23: type error, attempt to compare 'boolean' with 'string'
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:number, y:number = nil and 1, false and 1 
]=]
e = [=[
test.lua:1:7: type error, attempt to assign 'nil' to 'number'
test.lua:1:17: type error, attempt to assign 'false' to 'number'
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:string, y:number|string = 1 and 2, "foo" and nil
]=]
e = [=[
test.lua:1:7: type error, attempt to assign '(1 | 2)' to 'string'
test.lua:1:17: type error, attempt to assign '(foo | nil)' to '(number | string)'
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:nil, y:boolean = nil or 1, false or 1 
]=]
e = [=[
test.lua:1:7: type error, attempt to assign '1' to 'nil'
test.lua:1:14: type error, attempt to assign '1' to 'boolean'
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:string, y:number|string = 1 or 2, "foo" or nil
]=]
e = [=[
test.lua:1:7: type error, attempt to assign '(1 | 2)' to 'string'
test.lua:1:17: type error, attempt to assign '(foo | nil)' to '(number | string)'
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:number?
local y:number = x or nil
]=]
e = [=[
test.lua:2:7: type error, attempt to assign '(number | nil)' to 'number'
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:number, y:string = not nil, not false
]=]
e = [=[
test.lua:1:7: type error, attempt to assign 'boolean' to 'number'
test.lua:1:17: type error, attempt to assign 'boolean' to 'string'
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:number, y:string = not 1, not "foo"
]=]
e = [=[
test.lua:1:7: type error, attempt to assign 'boolean' to 'number'
test.lua:1:17: type error, attempt to assign 'boolean' to 'string'
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x = -"foo"
]=]
e = [=[
test.lua:1:12: type error, attempt to perform arithmetic on a 'string'
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x = #1
]=]
e = [=[
test.lua:1:12: type error, attempt to get length of a 'number'
]=]

r = typecheck(s)
assert(r == e)

s = [=[
while 1 + "foo" do break end
]=]
e = [=[
test.lua:1:11: type error, attempt to perform arithmetic on a 'string'
]=]

r = typecheck(s)
assert(r == e)

s = [=[
repeat break until 1 + nil
]=]
e = [=[
test.lua:1:24: type error, attempt to perform arithmetic on a 'nil'
]=]

r = typecheck(s)
assert(r == e)

s = [=[
if 1 then local x:string = 1 end
]=]
e = [=[
test.lua:1:17: type error, attempt to assign '1' to 'string'
]=]

r = typecheck(s)
assert(r == e)

s = [=[
if 1 then local x:number = 1 else local x:number = "foo" end
]=]
e = [=[
test.lua:1:41: type error, attempt to assign 'foo' to 'number'
]=]

r = typecheck(s)
assert(r == e)

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
assert(r == e)

s = [=[
local x:number?
local y:string?

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
]=]
e = [=[
test.lua:9:7: type error, attempt to perform arithmetic on a 'nil'
test.lua:10:7: type error, attempt to perform arithmetic on a 'nil'
test.lua:13:5: type error, attempt to perform arithmetic on a '(number | nil)'
test.lua:14:5: type error, attempt to concatenate a '(string | nil)'
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:boolean|number|string?

if type(x) == "number" then
  x = x + 1
elseif type(x) == "string" then
  x = x .. "hello"
elseif type(x) == "boolean" then
  x = false
end

x = x + 1
]=]
e = [=[
test.lua:11:5: type error, attempt to perform arithmetic on a '(boolean | number | string | nil)'
]=]

r = typecheck(s)
assert(r == e)

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
assert(r == e)

s = [=[
for i = nil, 10 do local x = i end
]=]
e = [=[
test.lua:1:9: type error, 'for' initial value must be a number
]=]

r = typecheck(s)
assert(r == e)

s = [=[
for i = 1, "foo" do local x = i end
]=]
e = [=[
test.lua:1:12: type error, 'for' limit must be a number
]=]

r = typecheck(s)
assert(r == e)

s = [=[
for i = 10, 1, false do local x = i end
]=]
e = [=[
test.lua:1:16: type error, 'for' step must be a number
]=]

r = typecheck(s)
assert(r == e)

-- new tests

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
assert(r == e)

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
assert(r == e)

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
test.lua:1:5: type error, attempt to iterate over 1
]=]

r = typecheck(s)
assert(r == e)

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
assert(r == e)

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
assert(r == e)

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
{ `Localrec{ { `Id "abs":`TFunction{ `TTuple{ `TBase number, `TVararg{ `TValue } }, `TTuple{ `TBase number, `TVararg{ `TNil } } } }, { `Function{ { `Id "n":`TBase number }, { `If{ `Op{ "lt", `Id "n", `Number "0" }, { `Return{ `Op{ "unm", `Id "n" } } }, { `Return{ `Id "n" } } } } } } }, `Localrec{ { `Id "distance":`TFunction{ `TTuple{ `TAny, `TAny, `TVararg{ `TValue } }, `TTuple{ `TBase number, `TNil, `TVararg{ `TNil } } } }, { `Function{ { `Id "x":`TAny, `Id "y":`TAny }, { `Return{ `Call{ `Id "abs", `Op{ "sub", `Id "x", `Id "y" } } } } } } } }
]=]

r = typecheck(s)
assert(r == e)

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
assert(r == e)

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
assert(r == e)

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
assert(r == e)

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
test.lua:6:7: type error, attempt to concatenate a '(string | nil)'
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local s:string?

while true do
  s = s or "foo"
end

s = s .. "bar"
]=]
e = [=[
test.lua:7:5: type error, attempt to concatenate a '(string | nil)'
]=]

r = typecheck(s)
assert(r == e)

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
{ `Localrec{ { `Id "rep":`TFunction{ `TTuple{ `TBase string, `TBase number, `TUnion{ `TBase string, `TNil }, `TVararg{ `TValue } }, `TTuple{ `TBase string, `TVararg{ `TNil } } } }, { `Function{ { `Id "s":`TBase string, `Id "n":`TBase number, `Id "sep":`TUnion{ `TBase string, `TNil } }:`TTuple{ `TBase string, `TVararg{ `TNil } }, { `Set{ { `Id "sep" }, { `Op{ "or", `Id "sep", `String "" } } }, `Local{ { `Id "r" }, { `String "" } }, `Fornum{ `Id "i":`TBase number, `Number "1", `Op{ "sub", `Id "n", `Number "1" }, { `Set{ { `Id "r" }, { `Op{ "concat", `Id "r", `Op{ "concat", `Id "s", `Id "sep" } } } } } }, `Return{ `Op{ "concat", `Id "r", `Id "s" } } } } } }, `Localrec{ { `Id "overload":`TFunction{ `TTuple{ `TBase string, `TUnion{ `TBase string, `TBase number }, `TVararg{ `TValue } }, `TTuple{ `TBase string, `TNil, `TVararg{ `TNil } } } }, { `Function{ { `Id "s1":`TBase string, `Id "s2":`TUnion{ `TBase string, `TBase number } }, { `If{ `Op{ "eq", `Call{ `Index{ `Id "_ENV", `String "type" }, `Id "s2" }, `String "string" }, { `Return{ `Op{ "concat", `Id "s1", `Id "s2" } } }, { `Return{ `Call{ `Id "rep", `Id "s1", `Id "s2" } } } } } } } } }
]=]

r = typecheck(s)
assert(r == e)

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
{ `Localrec{ { `Id "overload":`TFunction{ `TTuple{ `TBase string, `TUnion{ `TBase string, `TBase number }, `TVararg{ `TValue } }, `TTuple{ `TBase string, `TNil, `TVararg{ `TNil } } } }, { `Function{ { `Id "s1":`TBase string, `Id "s2":`TUnion{ `TBase string, `TBase number } }, { `If{ `Op{ "eq", `Call{ `Index{ `Id "_ENV", `String "type" }, `Id "s2" }, `String "string" }, { `Return{ `Op{ "concat", `Id "s1", `Id "s2" } } }, { `Return{ `Call{ `Index{ `Index{ `Id "_ENV", `String "string" }, `String "rep" }, `Id "s1", `Id "s2" } } } } } } } } }
]=]

r = typecheck(s)
assert(r == e)

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
assert(r == e)

s = [=[
local t:{string:number} = { foo = 1 }
local x:number? = t.foo
local y:number = t["bar"] or 0
]=]
e = [=[
{ `Local{ { `Id "t":`TTable{ `TBase string:`TUnion{ `TBase number, `TNil } } }, { `Table{ `Pair{ `String "foo", `Number "1" } } } }, `Local{ { `Id "x":`TUnion{ `TBase number, `TNil } }, { `Index{ `Id "t", `String "foo" } } }, `Local{ { `Id "y":`TBase number }, { `Op{ "or", `Index{ `Id "t", `String "bar" }, `Number "0" } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local t:{string:number?} = { foo = 1 or nil }
local x:number = t.foo
local y:number = t.bar or 0
local z:number? = t["bar"]
]=]
e = [=[
test.lua:2:7: type error, attempt to assign '(number | nil)' to 'number'
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local t1:{"foo":number} = { foo = 1, bar = "foo" }
local t2:{string:number} = t1
]=]
e = [=[
test.lua:2:7: type error, attempt to assign '{foo:number}' to '{string:(number | nil)}'
]=]

r = typecheck(s)
assert(r == e)

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
assert(r == e)

s = [=[
local days = { "Sunday", "Monday", "Tuesday", "Wednesday",
  "Thursday", "Friday", "Saturday" }
]=]
e = [=[
{ `Local{ { `Id "days" }, { `Table{ `String "Sunday", `String "Monday", `String "Tuesday", `String "Wednesday", `String "Thursday", `String "Friday", `String "Saturday" } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local days = { "Sunday", "Monday", "Tuesday", "Wednesday",
  "Thursday", "Friday", "Saturday" }
local t1:{string} = days
local t2:{string?} = days
t2 = t1
]=]
e = [=[
test.lua:3:7: type error, attempt to assign '{1:string, 2:string, 3:string, 4:string, 5:string, 6:string, 7:string}' to '{number:(string | nil)}'
test.lua:4:7: type error, attempt to assign '{1:string, 2:string, 3:string, 4:string, 5:string, 6:string, 7:string}' to '{number:(string | nil)}'
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local t1:{{string}} = { { "foo", "bar", "z" }, { "z", "bar", "foo" }, 4 }
local t2:{string} = { "foo", "bar", "z", function () end }
local t3:{"foo":number, number:string} = { foo = 1, [1] = true }
]=]
e = [=[
test.lua:1:7: type error, attempt to assign '{1:{1:string, 2:string, 3:string}, 2:{1:string, 2:string, 3:string}, 3:number}' to '{number:({number:(string | nil)} | nil)}'
test.lua:2:7: type error, attempt to assign '{1:string, 2:string, 3:string, 4:(value*) -> (nil*)}' to '{number:(string | nil)}'
test.lua:3:7: type error, attempt to assign '{foo:number, 1:boolean}' to '{foo:number, number:(string | nil)}'
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local t:{const "foo":string?} = { const foo = "foo" or nil }
local s:{const "foo":string} = { const foo = "foo" }
local r:{"foo":string?} = { foo = "foo" or nil }

t = s
r = t
r.foo = nil
]=]
e = [=[
test.lua:6:1: type error, attempt to assign '({const foo:(string | nil)}, nil*)' to '({foo:(string | nil)}, value*)'
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local t = { ... }
for i = 1, #t do
  print(t[i])
end
]=]
e = [=[
{ `Local{ { `Id "t" }, { `Table{ `Dots } } }, `Fornum{ `Id "i":`TBase number, `Number "1", `Op{ "len", `Id "t" }, { `Call{ `Index{ `Id "_ENV", `String "print" }, `Index{ `Id "t", `Id "i" } } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local t = { ... }
t[1] = 1
]=]
e = [=[
test.lua:2:1: type error, attempt to assign '(1, nil*)' to '((string | nil), value*)'
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local t = { ... }
t[1] = "foo"
t.foo = 1
]=]
e = [=[
{ `Local{ { `Id "t" }, { `Table{ `Dots } } }, `Set{ { `Index{ `Id "t", `Number "1" } }, { `String "foo" } }, `Set{ { `Index{ `Id "t", `String "foo" } }, { `Number "1" } } }
]=]

r = typecheck(s)
assert(r == e)

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
assert(r == e)

s = [=[
local person:{"firstname":string, "lastname":string} =
  { firstname = "Lou", lastname = "Reed" }
]=]
e = [=[
{ `Local{ { `Id "person":`TTable{ `TLiteral firstname:`TBase string, `TLiteral lastname:`TBase string } }, { `Table{ `Pair{ `String "firstname", `String "Lou" }, `Pair{ `String "lastname", `String "Reed" } } } } }
]=]

r = typecheck(s)
assert(r == e)

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
test.lua:14:7: type error, attempt to pass '({firstname:string}, nil*)' to local 'greet' of input type '({firstname:string, lastname:string}, value*)'
test.lua:16:7: type error, attempt to pass '({1:string, 2:string}, nil*)' to local 'greet' of input type '({firstname:string, lastname:string}, value*)'
]=]

r = typecheck(s)
assert(r == e)

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
assert(r == e)

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
assert(r == e)

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
assert(r == e)

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
assert(r == e)

s = [=[
local interface Element
  info:number
  next:Elment?
end
]=]
e = [=[
{ `Interface{ Element, `TTable{ `TLiteral info:`TBase number, `TLiteral next:`TUnion{ `TVariable Elment, `TNil } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local person = {}
person.firstname = "Lou"
person.lastname = "Reed"
]=]
e = [=[
{ `Local{ { `Id "person" }, { `Table } }, `Set{ { `Index{ `Id "person", `String "firstname" } }, { `String "Lou" } }, `Set{ { `Index{ `Id "person", `String "lastname" } }, { `String "Reed" } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local bogus = { firstname = 1 }
local person:{} = bogus
person.firstname = "Lou"
person.lastname = "Reed"
]=]
e = [=[
test.lua:3:1: type error, attempt to use 'firstname' to index closed table
test.lua:3:1: type error, attempt to assign '(Lou, nil*)' to '(nil, value*)'
test.lua:4:1: type error, attempt to use 'lastname' to index closed table
test.lua:4:1: type error, attempt to assign '(Reed, nil*)' to '(nil, value*)'
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local person = {}
local bogus = person
bogus.firstname = 1
person.firstname = "Lou"
person.lastname = "Reed"
]=]
e = [=[
test.lua:3:1: type error, attempt to use 'firstname' to index closed table
test.lua:3:1: type error, attempt to assign '(1, nil*)' to '(nil, value*)'
]=]

r = typecheck(s)
assert(r == e)

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
test.lua:4:3: type error, attempt to use 'firstname' to index closed table
test.lua:4:3: type error, attempt to assign '(1, nil*)' to '(nil, value*)'
test.lua:5:3: type error, attempt to assign '({}, nil*)' to '({firstname:number}, value*)'
test.lua:8:3: type error, attempt to use 'firstname' to index closed table
test.lua:8:3: type error, attempt to assign '(Lou, nil*)' to '(nil, value*)'
]=]

r = typecheck(s)
assert(r == e)

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
assert(r == e)

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
assert(r == e)

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
test.lua:11:7: type error, attempt to assign '{firstname:string, middlename:string, lastname:string}' to '{firstname:string, middlename:(string | nil), lastname:string}'
]=]

r = typecheck(s)
assert(r == e)

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
assert(r == e)

s = [=[
local str = "foo"
print(str:byte())
print(("foo"):byte())
]=]
e = [=[
{ `Local{ { `Id "str" }, { `String "foo" } }, `Call{ `Index{ `Id "_ENV", `String "print" }, `Invoke{ `Id "str", `String "byte" } }, `Call{ `Index{ `Id "_ENV", `String "print" }, `Invoke{ `Paren{ `String "foo" }, `String "byte" } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local str = "foo"
print(str:char())
print(("foo"):dump())
]=]
e = [=[
test.lua:2:7: type error, attempt to pass '(string, nil*)' to field of input type '(number*)'
test.lua:3:7: type error, attempt to pass '(string, nil*)' to field of input type '((value*) -> (value*), value*)'
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local function f (...:{1:string})
  local t:{string} = {...}
end
f = function (...:number):(string*)
  return ...
end
]=]
e = [=[
test.lua:2:9: type error, attempt to assign '{number:({1:string} | nil)}' to '{number:(string | nil)}'
test.lua:4:14: type error, return type '(number*)' does not match '(string*)'
test.lua:4:1: type error, attempt to assign '((number*) -> (string*), nil*)' to '(({1:string}*) -> (nil*), value*)'
]=]

r = typecheck(s)
assert(r == e)

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
assert(r == e)

print("> testing code generation...")

-- assignments

s = [=[
zero,um = false,true
]=]
e = [=[
zero, um = false, true
]=]

r = generate(s)
assert(r == e)

s = [=[
n,s = 1, "alo"
]=]
e = [=[
n, s = 1, "alo"
]=]

r = generate(s)
assert(r == e)

s = [=[
t = ...,nil
]=]
e = [=[
t = ..., nil
]=]

r = generate(s)
assert(r == e)

s = [=[
a = 2 * 3 + 5
]=]
e = [=[
a = 2 * 3 + 5
]=]

r = generate(s)
assert(r == e)

s = [=[
a = (2 * 3) + 5
]=]
e = [=[
a = (2 * 3) + 5
]=]

r = generate(s)
assert(r == e)

s = [=[
a = 1 - 2 / 3 % 4 ^ 5
]=]
e = [=[
a = 1 - 2 / 3 % 4 ^ 5
]=]

r = generate(s)
assert(r == e)

s = [=[
c = "alo" .. "mundo" 
]=]
e = [=[
c = "alo" .. "mundo"
]=]

r = generate(s)
assert(r == e)

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
assert(r == e)

s = [=[
a = not 1 and 2 or 3
]=]
e = [=[
a = not (1) and 2 or 3
]=]

r = generate(s)
assert(r == e)

-- do

s = [=[
do do do do do end end end end end
]=]
e = [=[
do
  do
    do
      do
        do

        end
      end
    end
  end
end
]=]

r = generate(s)
assert(r == e)

-- for

s = [=[
for i=1, 10 do break end
]=]
e = [=[
for i = 1, 10 do
  break
end
]=]

r = generate(s)
assert(r == e)

s = [=[
for i=1,10,-1 do break end
]=]
e = [=[
for i = 1, 10, -(1) do
  break
end
]=]

r = generate(s)
assert(r == e)

-- function

s = [=[
function f () end
]=]
e = [=[
f = function ()

end
]=]

r = generate(s)
assert(r == e)

s = [=[
function f (a) return a end
]=]
e = [=[
f = function (a)
  return a
end
]=]

r = generate(s)
assert(r == e)

s = [=[
function f (a, b, c) end
]=]
e = [=[
f = function (a, b, c)

end
]=]

r = generate(s)
assert(r == e)

s = [=[
function f (a, b, c, ...) end
]=]
e = [=[
f = function (a, b, c, ...)

end
]=]

r = generate(s)
assert(r == e)

s = [=[
function f (...) end
]=]
e = [=[
f = function (...)

end
]=]

r = generate(s)
assert(r == e)

s = [=[
local function f () end
]=]
e = [=[
local function f ()

end
]=]

r = generate(s)
assert(r == e)

s = [=[
local function f (a) return a end
]=]
e = [=[
local function f (a)
  return a
end
]=]

r = generate(s)
assert(r == e)

s = [=[
local function f (a, b, c) end
]=]
e = [=[
local function f (a, b, c)

end
]=]

r = generate(s)
assert(r == e)

s = [=[
local function f (a, b, c, ...) end
]=]
e = [=[
local function f (a, b, c, ...)

end
]=]

r = generate(s)
assert(r == e)

s = [=[
local function f (...) end
]=]
e = [=[
local function f (...)

end
]=]

r = generate(s)
assert(r == e)

-- goto

s = [=[
do goto eof end
:: eof ::
]=]
e = [=[
do
  goto eof
end
::eof::
]=]

r = generate(s)
assert(r == e)

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
assert(r == e)

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
assert(r == e)

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
assert(r == e)

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
assert(r == e)

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
assert(r == e)

-- local

s = [=[
local a:any?
]=]
e = [=[
local a
]=]

r = generate(s)
assert(r == e)

s = [=[
local a:any?, b:any?, c:any?
]=]
e = [=[
local a, b, c
]=]

r = generate(s)
assert(r == e)

s = [=[
local a = 1
]=]
e = [=[
local a = 1
]=]

r = generate(s)
assert(r == e)

s = [=[
local a, b:any? = 1
]=]
e = [=[
local a, b = 1
]=]

r = generate(s)
assert(r == e)

-- repeat

s = [=[
repeat break until true
]=]
e = [=[
repeat
  break
until true
]=]

r = generate(s)
assert(r == e)

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
assert(r == e)

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
  return {["info"] = v, ["next"] = e}
end
local function insert_f (e, v)
  if e then
    e["next"] = insert_f(e["next"],v)
    return e
  end
  return {["info"] = v, ["next"] = e}
end
local function print_l (e)
  if e then
    print(e["info"])
    print_l(e["next"])
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
assert(r == e)

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
  return {["info"] = v, ["left"] = l, ["right"] = r}
end
local function print_tree (t)
  if t then
    print(t["info"])
    print_tree(t["left"])
    print_tree(t["right"])
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
assert(r == e)

print("OK")
