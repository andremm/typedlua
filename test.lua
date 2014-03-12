#!/usr/bin/env lua

local code = require "typedlua.code"
local checker = require "typedlua.checker"
local parser = require "typedlua.parser"
local pp = require "typedlua.pp"
local types = require "typedlua.types"

-- expected result, result, subject
local e, r, s

local filename = "test.lua"

local function parse (s)
  local t,m = parser.parse(s,filename)
  local r
  if not t then
    r = m
  else
    r = pp.tostring(t)
  end
  return r .. "\n"
end

local function typecheck (s)
  local t,m = parser.parse(s,filename)
  local r
  if not t then
    error(m)
    os.exit(1)
  end
  t,m = checker.typecheck(t,s,filename)
  if m then
    r = m
  else
    r = pp.tostring(t)
  end
  return r .. "\n"
end

local function generate (s)
  local t,m = parser.parse(s,filename)
  if not t then
    error(m)
    os.exit(1)
  end
  return code.generate(t)
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
_nil,_false,_true,_dots = nil,false,true,...
]=]
e = [=[
{ `Set{ { `Id "_nil", `Id "_false", `Id "_true", `Id "_dots" }, { `Nil, `False, `True, `Dots } } }
]=]

r = parse(s)
assert(r == e)

-- floating points

s = [=[
f1 = 1.
f2 = 1.1
]=]
e = [=[
{ `Set{ { `Id "f1" }, { `Number "1" } }, `Set{ { `Id "f2" }, { `Number "1.1" } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
f1 = 1.e-1
f2 = 1.e1
]=]
e = [=[
{ `Set{ { `Id "f1" }, { `Number "0.1" } }, `Set{ { `Id "f2" }, { `Number "10" } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
f1 = 1.1e+1
f2 = 1.1e1
]=]
e = [=[
{ `Set{ { `Id "f1" }, { `Number "11" } }, `Set{ { `Id "f2" }, { `Number "11" } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
f1 = .1
f2 = .1e1
]=]
e = [=[
{ `Set{ { `Id "f1" }, { `Number "0.1" } }, `Set{ { `Id "f2" }, { `Number "1" } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
f1 = 1E1
f2 = 1e-1
]=]
e = [=[
{ `Set{ { `Id "f1" }, { `Number "10" } }, `Set{ { `Id "f2" }, { `Number "0.1" } } }
]=]

r = parse(s)
assert(r == e)

-- integers

s = [=[
i = 1
h = 0xff
]=]
e = [=[
{ `Set{ { `Id "i" }, { `Number "1" } }, `Set{ { `Id "h" }, { `Number "255" } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
h = 0x76c
i = 4294967296 -- 2^32
]=]
e = [=[
{ `Set{ { `Id "h" }, { `Number "1900" } }, `Set{ { `Id "i" }, { `Number "4294967296" } } }
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

ls1 =
[[
testing long string
]]

--[[
testing long string1 end
]]
]=]
e = [=[
{ `Set{ { `Id "ls1" }, { `String "testing long string\n" } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
--[==[
testing long string2 begin
]==]

ls2 = [==[ testing \n [[ long ]] \t [===[ string ]===]
\a ]==]

--[==[
[[ testing long string2 end ]]
]==]
]=]
e = [=[
{ `Set{ { `Id "ls2" }, { `String " testing \\n [[ long ]] \\t [===[ string ]===]\n\\a " } } }
]=]

r = parse(s)
assert(r == e)

-- short strings

s = [=[
-- short string test begin

ss1_a = "ola mundo\a"
ss1_b = 'ola mundo\a'

-- short string test end
]=]
e = [=[
{ `Set{ { `Id "ss1_a" }, { `String "ola mundo\a" } }, `Set{ { `Id "ss1_b" }, { `String "ola mundo\a" } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
-- short string test begin

ss2_a = "testando,\tteste\n1\n2\n3 --> \"tchau\""
ss2_b = 'testando,\tteste\n1\n2\n3 --> \'tchau\''

-- short string test end
]=]
e = [=[
{ `Set{ { `Id "ss2_a" }, { `String "testando,\tteste\n1\n2\n3 --> \"tchau\"" } }, `Set{ { `Id "ss2_b" }, { `String "testando,\tteste\n1\n2\n3 --> 'tchau'" } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
-- short string test begin

ss3_a = "ola \
'mundo'!"

ss3_b = 'ola \
"mundo"!'

-- short string test end
]=]
e = [=[
{ `Set{ { `Id "ss3_a" }, { `String "ola \n'mundo'!" } }, `Set{ { `Id "ss3_b" }, { `String "ola \n\"mundo\"!" } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
-- short string test begin

ss4_a = "C:\\Temp/"

ss4_b = 'C:\\Temp/'

-- short string test end
]=]
e = [=[
{ `Set{ { `Id "ss4_a" }, { `String "C:\\Temp/" } }, `Set{ { `Id "ss4_b" }, { `String "C:\\Temp/" } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
-- short string test begin

ss5_a = "ola \
mundo \\ \
cruel"

ss5_b = 'ola \
mundo \\ \
cruel'

-- short string test end
]=]
e = [=[
{ `Set{ { `Id "ss5_a" }, { `String "ola \nmundo \\ \ncruel" } }, `Set{ { `Id "ss5_b" }, { `String "ola \nmundo \\ \ncruel" } } }
]=]

r = parse(s)
assert(r == e)

-- syntax error

-- floating points

s = [=[
f = 9e
]=]
e = [=[
test.lua:2:1: syntax error, unexpected 'EOF', expecting '=', ',', 'String', '{', '(', '[', '.', ':'
]=]

r = parse(s)
assert(r == e)

s = [=[
f = 5.e
]=]
e = [=[
test.lua:2:1: syntax error, unexpected 'EOF', expecting '=', ',', 'String', '{', '(', '[', '.', ':'
]=]

r = parse(s)
assert(r == e)

s = [=[
f = .9e-
]=]
e = [=[
test.lua:1:8: syntax error, unexpected '-', expecting '=', ',', 'String', '{', '(', '[', '.', ':'
]=]

r = parse(s)
assert(r == e)

s = [=[
f = 5.9e+
]=]
e = [=[
test.lua:1:9: syntax error, unexpected '+', expecting '=', ',', 'String', '{', '(', '[', '.', ':'
]=]

r = parse(s)
assert(r == e)

-- integers

s = [=[
-- invalid hexadecimal number

hex = 0xG
]=]
e = [=[
test.lua:4:1: syntax error, unexpected 'EOF', expecting '=', ',', 'String', '{', '(', '[', '.', ':'
]=]

r = parse(s)
assert(r == e)

-- long strings

s = [=[
--[==[
testing long string3 begin
]==]

ls3 = [===[
testing
unfinised
long string
]==]

--[==[
[[ testing long string3 end ]]
]==]
]=]
e = [=[
test.lua:5:7: syntax error, unexpected '[', expecting '(', 'Name', '{', 'function', '...', 'true', 'false', 'nil', 'String', 'Number', '#', '-', 'not'
]=]

r = parse(s)
assert(r == e)

-- short strings

s = [=[
-- short string test begin

ss6 = "testing unfinished string

-- short string test end
]=]
e = [=[
test.lua:3:7: syntax error, unexpected '"', expecting '(', 'Name', '{', 'function', '...', 'true', 'false', 'nil', 'String', 'Number', '#', '-', 'not'
]=]

r = parse(s)
assert(r == e)

s = [=[
-- short string test begin

ss7 = 'testing \\
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
test.lua:3:1: syntax error, unexpected 'comment', expecting '=', ',', 'String', '{', '(', '[', '.', ':'
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
test = function (...) return ...,0 end
]=]
e = [=[
{ `Set{ { `Id "test" }, { `Function{ { `Dots }, { `Return{ `Dots, `Number "0" } } } } } }
]=]

r = parse(s)
assert(r == e)

-- arithmetic expressions

s = [=[
arithmetic = 1 - 2 * 3 + 4
]=]
e = [=[
{ `Set{ { `Id "arithmetic" }, { `Op{ "add", `Op{ "sub", `Number "1", `Op{ "mul", `Number "2", `Number "3" } }, `Number "4" } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
pow = -3^-2^2
]=]
e = [=[
{ `Set{ { `Id "pow" }, { `Op{ "unm", `Op{ "pow", `Number "3", `Op{ "unm", `Op{ "pow", `Number "2", `Number "2" } } } } } } }
]=]

r = parse(s)
assert(r == e)

-- assignments

s = [=[
a = f()[1]
]=]
e = [=[
{ `Set{ { `Id "a" }, { `Index{ `Call{ `Id "f" }, `Number "1" } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
a()[1] = 1;
]=]
e = [=[
{ `Set{ { `Index{ `Call{ `Id "a" }, `Number "1" } }, { `Number "1" } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
i = a.f(1)
]=]
e = [=[
{ `Set{ { `Id "i" }, { `Call{ `Index{ `Id "a", `String "f" }, `Number "1" } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
i = a[f(1)]
]=]
e = [=[
{ `Set{ { `Id "i" }, { `Index{ `Id "a", `Call{ `Id "f", `Number "1" } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
a[f()] = sub
i = i + 1
]=]
e = [=[
{ `Set{ { `Index{ `Id "a", `Call{ `Id "f" } } }, { `Id "sub" } }, `Set{ { `Id "i" }, { `Op{ "add", `Id "i", `Number "1" } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
a:b(1)._ = some_value
]=]
e = [=[
{ `Set{ { `Index{ `Invoke{ `Id "a", `String "b", `Number "1" }, `String "_" } }, { `Id "some_value" } } }
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
  var = 2+2;
  return
end
]=]
e = [=[
{ `Do{ `Set{ { `Id "var" }, { `Op{ "add", `Number "2", `Number "2" } } }, `Return } }
]=]

r = parse(s)
assert(r == e)

-- concatenation expressions

s = [=[
concat1 = 1 .. 2^3
]=]
e = [=[
{ `Set{ { `Id "concat1" }, { `Op{ "concat", `Number "1", `Op{ "pow", `Number "2", `Number "3" } } } } }
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
{ `Forin{ { `Id "k", `Id "v" }, { `Call{ `Id "pairs", `Id "t" } }, { `Call{ `Id "print", `Id "k", `Id "v" } } } }
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
{ `Set{ { `Id "test" }, { `Function{ { `Id "a", `Id "b", `Dots }, {  } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
function test (...) end
]=]
e = [=[
{ `Set{ { `Id "test" }, { `Function{ { `Dots }, {  } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
function t.a:b() end
]=]
e = [=[
{ `Set{ { `Index{ `Index{ `Id "t", `String "a" }, `String "b" } }, { `Function{ { `Id "self" }, {  } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
function t.a() end
]=]
e = [=[
{ `Set{ { `Index{ `Id "t", `String "a" } }, { `Function{ {  }, {  } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
function testando . funcao . com : espcacos ( e, com , parametros, ... ) end
]=]
e = [=[
{ `Set{ { `Index{ `Index{ `Index{ `Id "testando", `String "funcao" }, `String "com" }, `String "espcacos" } }, { `Function{ { `Id "self", `Id "e", `Id "com", `Id "parametros", `Dots }, {  } } } } }
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
{ `If{ `Id "a", {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
if a then return a else return end
]=]
e = [=[
{ `If{ `Id "a", { `Return{ `Id "a" } }, { `Return } } }
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
{ `If{ `Id "a", { `Return{ `Id "a" } }, { `Local{ { `Id "c" }, { `Id "d" } }, `Set{ { `Id "d" }, { `Op{ "add", `Id "d", `Number "1" } } }, `Return{ `Id "d" } } } }
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
{ `If{ `Id "a", { `Return{ `Id "a" } }, `Id "b", { `Return{ `Id "b" } }, `Id "c", { `Return{ `Id "c" } } } }
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
{ `If{ `Id "a", { `Return{ `Id "a" } }, `Id "b", { `Return }, {  } } }
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
{ `If{ `Id "a", { `Return }, `Id "c", {  } } }
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
relational = 1 < 2 >= 3 == 4 ~= 5 < 6 <= 7
]=]
e = [=[
{ `Set{ { `Id "relational" }, { `Op{ "le", `Op{ "lt", `Op{ "not", `Op{ "eq", `Op{ "eq", `Op{ "le", `Number "3", `Op{ "lt", `Number "1", `Number "2" } }, `Number "4" }, `Number "5" } }, `Number "6" }, `Number "7" } } } }
]=]

r = parse(s)
assert(r == e)

-- repeat

s = [=[
repeat
  a,b,c = 1+1,2+2,3+3
  break
until a < 1
]=]
e = [=[
{ `Repeat{ { `Set{ { `Id "a", `Id "b", `Id "c" }, { `Op{ "add", `Number "1", `Number "1" }, `Op{ "add", `Number "2", `Number "2" }, `Op{ "add", `Number "3", `Number "3" } } }, `Break }, `Op{ "lt", `Id "a", `Number "1" } } }
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
t = { [1] = "alo", alo = 1, 2; }
]=]
e = [=[
{ `Set{ { `Id "t" }, { `Table{ `Pair{ `Number "1", `String "alo" }, `Pair{ `String "alo", `Number "1" }, `Number "2" } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
t = { 1.5 }
]=]
e = [=[
{ `Set{ { `Id "t" }, { `Table{ `Number "1.5" } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
t = {1,2;
3,
4,



5}
]=]
e = [=[
{ `Set{ { `Id "t" }, { `Table{ `Number "1", `Number "2", `Number "3", `Number "4", `Number "5" } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
t = {[1]=1,[2]=2;
[3]=3,
[4]=4,



[5]=5}
]=]
e = [=[
{ `Set{ { `Id "t" }, { `Table{ `Pair{ `Number "1", `Number "1" }, `Pair{ `Number "2", `Number "2" }, `Pair{ `Number "3", `Number "3" }, `Pair{ `Number "4", `Number "4" }, `Pair{ `Number "5", `Number "5" } } } } }
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
function f (...)
  return ...
end
]=]
e = [=[
{ `Set{ { `Id "f" }, { `Function{ { `Dots }, { `Return{ `Dots } } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
function f ()
  function g (x, y, ...)
    return ...,...,...
  end
end
]=]
e = [=[
{ `Set{ { `Id "f" }, { `Function{ {  }, { `Set{ { `Id "g" }, { `Function{ { `Id "x", `Id "y", `Dots }, { `Return{ `Dots, `Dots, `Dots } } } } } } } } } }
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
i = 0
while (i < 10)
do
  i = i + 1
end
]=]
e = [=[
{ `Set{ { `Id "i" }, { `Number "0" } }, `While{ `Paren{ `Op{ "lt", `Id "i", `Number "10" } }, { `Set{ { `Id "i" }, { `Op{ "add", `Id "i", `Number "1" } } } } } }
]=]

r = parse(s)
assert(r == e)

-- type annotations

s = [=[
local x:nil
]=]
e = [=[
{ `Local{ { `Id "x":`Literal nil }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:false, y:true
]=]
e = [=[
{ `Local{ { `Id "x":`Literal false, `Id "y":`Literal true }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:1, y:1.1
]=]
e = [=[
{ `Local{ { `Id "x":`Literal 1, `Id "y":`Literal 1.1 }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:"hello", y:'world' 
]=]
e = [=[
{ `Local{ { `Id "x":`Literal hello, `Id "y":`Literal world }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:boolean, y:number, z:string 
]=]
e = [=[
{ `Local{ { `Id "x":`Base boolean, `Id "y":`Base number, `Id "z":`Base string }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:any
]=]
e = [=[
{ `Local{ { `Id "x":`Any }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:number?
]=]
e = [=[
{ `Local{ { `Id "x":`Union{ `Base number, `Literal nil } }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:number|nil
]=]
e = [=[
{ `Local{ { `Id "x":`Union{ `Base number, `Literal nil } }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:number|string|nil
]=]
e = [=[
{ `Local{ { `Id "x":`Union{ `Union{ `Base number, `Base string }, `Literal nil } }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:number|string?
]=]
e = [=[
{ `Local{ { `Id "x":`Union{ `Base number, `Union{ `Base string, `Literal nil } } }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
x:number, y, z:boolean = 1, nil, true
]=]
e = [=[
{ `Set{ { `Id "x":`Base number, `Id "y", `Id "z":`Base boolean }, { `Number "1", `Nil, `True } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
for k:number, v:string in ipairs({"hello", "world"}) do end
]=]
e = [=[
{ `Forin{ { `Id "k":`Base number, `Id "v":`Base string }, { `Call{ `Id "ipairs", `Table{ `String "hello", `String "world" } } }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
for k:string, v in pairs({}) do end
]=]
e = [=[
{ `Forin{ { `Id "k":`Base string, `Id "v" }, { `Call{ `Id "pairs", `Table } }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
for k, v:boolean in pairs({}) do end
]=]
e = [=[
{ `Forin{ { `Id "k", `Id "v":`Base boolean }, { `Call{ `Id "pairs", `Table } }, {  } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local function f (x:any) end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ { `Id "x":`Any }, {  } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local function f (x:any):any end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ { `Id "x":`Any }:`Any, {  } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local function f (...:any) end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ { `Dots:`Any }, {  } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local function f (x:any, ...:any) end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ { `Id "x":`Any, `Dots:`Any }, {  } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local function f (x, ...:any) end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ { `Id "x", `Dots:`Any }, {  } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local function f (x:any, ...) end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ { `Id "x":`Any, `Dots }, {  } } } } }
]=]

r = parse(s)
assert(r == e)

s = [=[
local function f (x:any, ...:any):any end
]=]
e = [=[
{ `Localrec{ { `Id "f" }, { `Function{ { `Id "x":`Any, `Dots:`Any }:`Any, {  } } } } }
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
test.lua:2:1: syntax error, unexpected 'EOF', expecting 'end', 'return', '(', 'Name', 'goto', 'break', '::', 'local', 'function', 'repeat', 'for', 'do', 'while', 'if', ';', ':'
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
test.lua:1:15: syntax error, unexpected '.1', expecting 'return', '(', 'Name', 'goto', 'break', '::', 'local', 'function', 'repeat', 'for', 'do', 'while', 'if', ';', ',', 'or', 'and', '>', '<', '>=', '<=', '==', '~=', '..', '-', '+', '%', '/', '*', '^'
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
test.lua:2:1: syntax error, unexpected 'EOF', expecting 'end', 'else', 'elseif', 'return', '(', 'Name', 'goto', 'break', '::', 'local', 'function', 'repeat', 'for', 'do', 'while', 'if', ';'
]=]

r = parse(s)
assert(r == e)

s = [=[
if a then else
]=]
e = [=[
test.lua:2:1: syntax error, unexpected 'EOF', expecting 'end', 'return', '(', 'Name', 'goto', 'break', '::', 'local', 'function', 'repeat', 'for', 'do', 'while', 'if', ';'
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
test.lua:3:1: syntax error, label 'label' already defined at line 1
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
test.lua:4:1: syntax error, unexpected 'EOF', expecting 'until', 'return', '(', 'Name', 'goto', 'break', '::', 'local', 'function', 'repeat', 'for', 'do', 'while', 'if', ';'
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
test.lua:1:7: syntax error, unexpected ',', expecting '}', '(', '{', 'function', '...', 'true', 'false', 'nil', 'String', 'Number', '#', '-', 'not', 'Name', '['
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
x:any = x:any
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
test.lua:1:8: syntax error, unexpected ':', expecting 'return', '(', 'Name', 'goto', 'break', '::', 'local', 'function', 'repeat', 'for', 'do', 'while', 'if', ';', ',', 'or', 'and', '>', '<', '>=', '<=', '==', '~=', '..', '-', '+', '%', '/', '*', '^'
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
test.lua:1:15: syntax error, unexpected '*', expecting 'return', '(', 'Name', 'goto', 'break', '::', 'local', 'function', 'repeat', 'for', 'do', 'while', 'if', ';', '=', ',', '|', '?'
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:number|
]=]
e = [=[
test.lua:2:1: syntax error, unexpected 'EOF', expecting 'Type'
]=]

r = parse(s)
assert(r == e)

print("> testing types...")

-- literal types

local Nil = types.Nil
local False = types.False
local True = types.True
local Double = types.Literal(1.1)
local Integer = types.Literal(1)
local Word = types.Literal("w")

-- base types

local Boolean = types.Boolean
local Number = types.Number
local String = types.String

-- dynamic type

local Any = types.Any

-- test types
local t, t1, t2

-- type equality

assert(types.isLiteral(Nil))
assert(types.isNil(Nil))

assert(types.isLiteral(False))
assert(types.isFalse(False))

assert(types.isLiteral(True))
assert(types.isTrue(True))

assert(types.isLiteral(Double))
assert(types.isLiteralNumber(Double))

assert(types.isLiteral(Integer))
assert(types.isLiteralNumber(Integer))

assert(types.isLiteral(Word))
assert(types.isLiteralString(Word))

assert(types.isBase(Boolean))
assert(types.isBoolean(Boolean))

assert(types.isBase(Number))
assert(types.isNumber(Number))

assert(types.isBase(String))
assert(types.isString(String))

assert(types.isAny(Any))

assert(types.isUnion(types.Union(Number,Nil)))
assert(types.isUnion(types.Union(types.Union(Number,String),Nil)))
assert(types.isUnion(types.Union(types.Union(Number,Nil),String)))
assert(types.isUnion(types.Union(types.Union(Nil,Number),String)))

assert(types.isUnionNil(types.Union(Number,Nil)))
assert(types.isUnionNil(types.Union(types.Union(Number,String),Nil)))
assert(types.isUnionNil(types.Union(types.Union(Number,Nil),String)))
assert(types.isUnionNil(types.Union(types.Union(Nil,Number),String)))

assert(not types.isUnionNil(types.Union(Number,Boolean)))
assert(not types.isUnionNil(types.Union(types.Union(Number,String),Boolean)))
assert(not types.isUnionNil(types.Union(types.Union(Number,Boolean),String)))
assert(not types.isUnionNil(types.Union(types.Union(Boolean,Number),String)))

-- subtyping

assert(types.subtype(Nil,Nil))
assert(types.subtype(False,False))
assert(types.subtype(True,True))
assert(types.subtype(Double,Double))
assert(types.subtype(Integer,Integer))
assert(types.subtype(Word,Word))

assert(types.subtype(False,Boolean))
assert(types.subtype(True,Boolean))
assert(types.subtype(Double,Number))
assert(types.subtype(Integer,Number))
assert(types.subtype(Word,String))

assert(not types.subtype(Nil,False))
assert(not types.subtype(False,True))
assert(not types.subtype(True,Double))
assert(not types.subtype(Double,Integer))
assert(not types.subtype(Integer,Word))
assert(not types.subtype(Word,Nil))

assert(types.subtype(Boolean,Boolean))
assert(types.subtype(Number,Number))
assert(types.subtype(String,String))

assert(not types.subtype(Boolean,False))
assert(not types.subtype(Boolean,True))
assert(not types.subtype(Number,Double))
assert(not types.subtype(Number,Integer))
assert(not types.subtype(String,Word))

assert(types.subtype(Any,Any))

assert(not types.subtype(Nil,Any))
assert(not types.subtype(False,Any))
assert(not types.subtype(True,Any))
assert(not types.subtype(Double,Any))
assert(not types.subtype(Integer,Any))
assert(not types.subtype(Word,Any))

assert(not types.subtype(Boolean,Any))
assert(not types.subtype(Number,Any))
assert(not types.subtype(String,Any))

assert(not types.subtype(Any,Nil))
assert(not types.subtype(Any,False))
assert(not types.subtype(Any,True))
assert(not types.subtype(Any,Double))
assert(not types.subtype(Any,Integer))
assert(not types.subtype(Any,Word))

assert(not types.subtype(Any,Boolean))
assert(not types.subtype(Any,Number))
assert(not types.subtype(Any,String))

t = types.Union(Number,Nil)

assert(types.subtype(Number,t))
assert(types.subtype(Nil,t))
assert(types.subtype(t,t))

assert(not types.subtype(t,Number))
assert(not types.subtype(t,Nil))

t = types.Union(Number,Any)

assert(types.subtype(Number,t))
assert(types.subtype(Any,t))
assert(types.subtype(t,t))

assert(not types.subtype(String,t))
assert(not types.subtype(t,Any))
assert(not types.subtype(t,Any))

-- consistent-subtyping

assert(types.consistent_subtype(Nil,Nil))
assert(types.consistent_subtype(False,False))
assert(types.consistent_subtype(True,True))
assert(types.consistent_subtype(Double,Double))
assert(types.consistent_subtype(Integer,Integer))
assert(types.consistent_subtype(Word,Word))

assert(types.consistent_subtype(False,Boolean))
assert(types.consistent_subtype(True,Boolean))
assert(types.consistent_subtype(Double,Number))
assert(types.consistent_subtype(Integer,Number))
assert(types.consistent_subtype(Word,String))

assert(not types.consistent_subtype(Nil,False))
assert(not types.consistent_subtype(False,True))
assert(not types.consistent_subtype(True,Double))
assert(not types.consistent_subtype(Double,Integer))
assert(not types.consistent_subtype(Integer,Word))
assert(not types.consistent_subtype(Word,Nil))

assert(types.consistent_subtype(Boolean,Boolean))
assert(types.consistent_subtype(Number,Number))
assert(types.consistent_subtype(String,String))

assert(not types.consistent_subtype(Boolean,False))
assert(not types.consistent_subtype(Boolean,True))
assert(not types.consistent_subtype(Number,Double))
assert(not types.consistent_subtype(Number,Integer))
assert(not types.consistent_subtype(String,Word))

assert(types.consistent_subtype(Any,Any))

assert(types.consistent_subtype(Nil,Any))
assert(types.consistent_subtype(False,Any))
assert(types.consistent_subtype(True,Any))
assert(types.consistent_subtype(Double,Any))
assert(types.consistent_subtype(Integer,Any))
assert(types.consistent_subtype(Word,Any))

assert(types.consistent_subtype(Boolean,Any))
assert(types.consistent_subtype(Number,Any))
assert(types.consistent_subtype(String,Any))

assert(types.consistent_subtype(Any,Nil))
assert(types.consistent_subtype(Any,False))
assert(types.consistent_subtype(Any,True))
assert(types.consistent_subtype(Any,Double))
assert(types.consistent_subtype(Any,Integer))
assert(types.consistent_subtype(Any,Word))

assert(types.consistent_subtype(Any,Boolean))
assert(types.consistent_subtype(Any,Number))
assert(types.consistent_subtype(Any,String))

t = types.Union(Number,Nil)

assert(types.consistent_subtype(Number,t))
assert(types.consistent_subtype(Nil,t))
assert(types.consistent_subtype(t,t))

assert(not types.consistent_subtype(t,Number))
assert(not types.consistent_subtype(t,Nil))

t = types.Union(Number,Any)

assert(types.consistent_subtype(Number,t))
assert(types.consistent_subtype(Any,t))
assert(types.consistent_subtype(t,t))
assert(types.consistent_subtype(String,t))
assert(types.consistent_subtype(t,Any))

assert(not types.consistent_subtype(t,String))

print("> testing type checker...")

-- type check

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
{ `Local{ { `Id "x":`Base number, `Id "y":`Base string, `Id "z":`Base boolean }, { `Number "1", `String "foo", `False } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:boolean, y:nil = true, nil
]=]
e = [=[
{ `Local{ { `Id "x":`Base boolean, `Id "y":`Literal nil }, { `True, `Nil } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:number?, y:number|nil = 1 
]=]
e = [=[
{ `Local{ { `Id "x":`Union{ `Base number, `Literal nil }, `Id "y":`Union{ `Base number, `Literal nil } }, { `Number "1" } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:number = 1 + 1
]=]
e = [=[
{ `Local{ { `Id "x":`Base number }, { `Op{ "add", `Number "1", `Number "1" } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:string = "hello" .. "world"
]=]
e = [=[
{ `Local{ { `Id "x":`Base string }, { `Op{ "concat", `String "hello", `String "world" } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:boolean, y:boolean = nil == false, false == true
]=]
e = [=[
{ `Local{ { `Id "x":`Base boolean, `Id "y":`Base boolean }, { `Op{ "eq", `Nil, `False }, `Op{ "eq", `False, `True } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:boolean, y:boolean = 1 == 2, "foo" == "bar"
]=]
e = [=[
{ `Local{ { `Id "x":`Base boolean, `Id "y":`Base boolean }, { `Op{ "eq", `Number "1", `Number "2" }, `Op{ "eq", `String "foo", `String "bar" } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:boolean, y:boolean = 1 < 2, "foo" < "bar"
]=]
e = [=[
{ `Local{ { `Id "x":`Base boolean, `Id "y":`Base boolean }, { `Op{ "lt", `Number "1", `Number "2" }, `Op{ "lt", `String "foo", `String "bar" } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:nil, y:boolean = nil and 1, false and 1 
]=]
e = [=[
{ `Local{ { `Id "x":`Literal nil, `Id "y":`Base boolean }, { `Op{ "and", `Nil, `Number "1" }, `Op{ "and", `False, `Number "1" } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:number, y:string? = 1 and 2, "foo" and nil
]=]
e = [=[
{ `Local{ { `Id "x":`Base number, `Id "y":`Union{ `Base string, `Literal nil } }, { `Op{ "and", `Number "1", `Number "2" }, `Op{ "and", `String "foo", `Nil } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:number, y:number = nil or 1, false or 1 
]=]
e = [=[
{ `Local{ { `Id "x":`Base number, `Id "y":`Base number }, { `Op{ "or", `Nil, `Number "1" }, `Op{ "or", `False, `Number "1" } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:number, y:string? = 1 or 2, "foo" or nil
]=]
e = [=[
{ `Local{ { `Id "x":`Base number, `Id "y":`Union{ `Base string, `Literal nil } }, { `Op{ "or", `Number "1", `Number "2" }, `Op{ "or", `String "foo", `Nil } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:number?
local y:number = x or 0
]=]
e = [=[
{ `Local{ { `Id "x":`Union{ `Base number, `Literal nil } }, {  } }, `Local{ { `Id "y":`Base number }, { `Op{ "or", `Id "x", `Number "0" } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:boolean, y:boolean = not nil, not false
]=]
e = [=[
{ `Local{ { `Id "x":`Base boolean, `Id "y":`Base boolean }, { `Op{ "not", `Nil }, `Op{ "not", `False } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:number = -1
]=]
e = [=[
{ `Local{ { `Id "x":`Base number }, { `Op{ "unm", `Number "1" } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:number = #"foo"
]=]
e = [=[
{ `Local{ { `Id "x":`Base number }, { `Op{ "len", `String "foo" } } } }
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
for i = 1, 10 do local x = i end
]=]
e = [=[
{ `Fornum{ `Id "i":`Base number, `Number "1", `Number "10", { `Local{ { `Id "x" }, { `Id "i" } } } } }
]=]

r = typecheck(s)
assert(r == e)

s = [=[
for i = 10, 1, -1 do local x = i end
]=]
e = [=[
{ `Fornum{ `Id "i":`Base number, `Number "10", `Number "1", `Op{ "unm", `Number "1" }, { `Local{ { `Id "x" }, { `Id "i" } } } } }
]=]

r = typecheck(s)
assert(r == e)

-- do not type check

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
test.lua:1:12: type error, attempt to get length of a 'number' value
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
if 1 + false then local x:number = 1 else local x:number = "foo" end
]=]
e = [=[
test.lua:1:8: type error, attempt to perform arithmetic on a 'boolean'
test.lua:1:49: type error, attempt to assign 'foo' to 'number'
]=]

r = typecheck(s)
assert(r == e)

s = [=[
if 1 then
  local x = 1
elseif 2 + nil then
  local x = 2
elseif 3 + true then
  local x:string = 3
elseif 4 then
  local x:boolean = 4
else
  local x = "foo"
end
]=]
e = [=[
test.lua:3:12: type error, attempt to perform arithmetic on a 'nil'
test.lua:5:12: type error, attempt to perform arithmetic on a 'boolean'
test.lua:6:9: type error, attempt to assign '3' to 'string'
test.lua:8:9: type error, attempt to assign '4' to 'boolean'
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
number,string = 1, "alo"
]=]
e = [=[
number, string = 1, "alo"
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
b = not 1 == 2
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
a = not 1 and 2 or 3
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
for i = 1, 10, -1 do
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
local a
]=]
e = [=[
local a
]=]

r = generate(s)
assert(r == e)

s = [=[
local a, b, c
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
local a, b = 1
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

print("OK")
