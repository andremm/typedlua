#!/usr/bin/env lua

local parser = require "typedlua.parser"
local pp = require "typedlua.pp"

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
test.lua:2:1: syntax error, unexpected 'EOF', expecting '=', ',', 'String', '{', '(', ':', '[', '.'
]=]

r = parse(s)
assert(r == e)

s = [=[
f = 5.e
]=]
e = [=[
test.lua:2:1: syntax error, unexpected 'EOF', expecting '=', ',', 'String', '{', '(', ':', '[', '.'
]=]

r = parse(s)
assert(r == e)

s = [=[
f = .9e-
]=]
e = [=[
test.lua:1:8: syntax error, unexpected '-', expecting '=', ',', 'String', '{', '(', ':', '[', '.'
]=]

r = parse(s)
assert(r == e)

s = [=[
f = 5.9e+
]=]
e = [=[
test.lua:1:9: syntax error, unexpected '+', expecting '=', ',', 'String', '{', '(', ':', '[', '.'
]=]

r = parse(s)
assert(r == e)

-- integers

s = [=[
-- invalid hexadecimal number

hex = 0xG
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
test.lua:1:18: syntax error, unexpected ',', expecting ')'
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
test.lua:2:1: syntax error, unexpected 'EOF', expecting 'end', 'return', '(', 'Name', 'goto', 'break', '::', 'local', 'function', 'repeat', 'for', 'do', 'while', 'if', ';'
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
test.lua:1:6: syntax error, unexpected ';', expecting 'in', ',', '='
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
test.lua:1:18: syntax error, unexpected ',', expecting ')'
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
test.lua:1:24: syntax error, unexpected ',', expecting ')'
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

print("OK")
