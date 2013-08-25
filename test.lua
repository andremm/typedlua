#!/usr/bin/env lua

local ast = require "ast"
local checker = require "checker"
local code = require "code"
local parser = require "parser"
local types = require "types"

-- expected result, result, subject
local e, r, s

local filename = "test.lua"

local function parse (s)
  local t,m = parser.parse(s,filename)
  local r
  if not t then
    r = m
  else
    r = ast.tostring(t)
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
  r,m = checker.typecheck(t,s,filename)
  if not r then
    r = m
  else
    r = ast.tostring(t)
  end
  return r .. "\n"
end

local function generatecode (s)
  local t,m = parser.parse(s,filename)
  if not t then
    error(m)
    os.exit(1)
  end
  t,m = checker.typecheck(t,s,filename)
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
StmBlock []
]=]

r = parse(s)
assert(r == e)

s = [=[
-- testing empty file
]=]
e = [=[
StmBlock []
]=]

r = parse(s)
assert(r == e)

-- expressions

s = [=[
_nil,_false,_true,_dots = nil,false,true,...
]=]
e = [=[
StmBlock [StmAssign [VarID ("_nil","any"),VarID ("_false","any"),VarID ("_true","any"),VarID ("_dots","any")] [ExpNil,ExpFalse,ExpTrue,ExpDots]]
]=]

r = parse(s)
assert(r == e)

-- floating points

s = [=[
f1 = 1.
f2 = 1.1
]=]
e = [=[
StmBlock [StmAssign [VarID ("f1","any")] [ExpNum 1.0],StmAssign [VarID ("f2","any")] [ExpNum 1.1]]
]=]

r = parse(s)
assert(r == e)

s = [=[
f1 = 1.e-1
f2 = 1.e1
]=]
e = [=[
StmBlock [StmAssign [VarID ("f1","any")] [ExpNum 0.1],StmAssign [VarID ("f2","any")] [ExpNum 10.0]]
]=]

r = parse(s)
assert(r == e)

s = [=[
f1 = 1.1e+1
f2 = 1.1e1
]=]
e = [=[
StmBlock [StmAssign [VarID ("f1","any")] [ExpNum 11.0],StmAssign [VarID ("f2","any")] [ExpNum 11.0]]
]=]

r = parse(s)
assert(r == e)

s = [=[
f1 = .1
f2 = .1e1
]=]
e = [=[
StmBlock [StmAssign [VarID ("f1","any")] [ExpNum 0.1],StmAssign [VarID ("f2","any")] [ExpNum 1.0]]
]=]

r = parse(s)
assert(r == e)

s = [=[
f1 = 1E1
f2 = 1e-1
]=]
e = [=[
StmBlock [StmAssign [VarID ("f1","any")] [ExpNum 10.0],StmAssign [VarID ("f2","any")] [ExpNum 0.1]]
]=]

r = parse(s)
assert(r == e)

-- integers

s = [=[
i = 1
h = 0xff
]=]
e = [=[
StmBlock [StmAssign [VarID ("i","any")] [ExpNum 1.0],StmAssign [VarID ("h","any")] [ExpNum 255.0]]
]=]

r = parse(s)
assert(r == e)

s = [=[
h = 0x76c
i = 4294967296 -- 2^32
]=]
e = [=[
StmBlock [StmAssign [VarID ("h","any")] [ExpNum 1900.0],StmAssign [VarID ("i","any")] [ExpNum 4294967296.0]]
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
StmBlock []
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
StmBlock [StmAssign [VarID ("ls1","any")] [ExpStr "testing long string\n"]]
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
StmBlock [StmAssign [VarID ("ls2","any")] [ExpStr " testing \\n [[ long ]] \\t [===[ string ]===]\n\\a "]]
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
StmBlock [StmAssign [VarID ("ss1_a","any")] [ExpStr "ola mundo\a"],StmAssign [VarID ("ss1_b","any")] [ExpStr "ola mundo\a"]]
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
StmBlock [StmAssign [VarID ("ss2_a","any")] [ExpStr "testando,\tteste\n1\n2\n3 --> \"tchau\""],StmAssign [VarID ("ss2_b","any")] [ExpStr "testando,\tteste\n1\n2\n3 --> 'tchau'"]]
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
StmBlock [StmAssign [VarID ("ss3_a","any")] [ExpStr "ola \n'mundo'!"],StmAssign [VarID ("ss3_b","any")] [ExpStr "ola \n\"mundo\"!"]]
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
StmBlock [StmAssign [VarID ("ss4_a","any")] [ExpStr "C:\\Temp/"],StmAssign [VarID ("ss4_b","any")] [ExpStr "C:\\Temp/"]]
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
StmBlock [StmAssign [VarID ("ss5_a","any")] [ExpStr "ola \nmundo \\ \ncruel"],StmAssign [VarID ("ss5_b","any")] [ExpStr "ola \nmundo \\ \ncruel"]]
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
test.lua:5:7: syntax error, unexpected '[===[', expecting '(', 'Name', '{', 'function', '...', 'true', 'false', 'nil', 'String', 'Number', '#', '-', 'not'
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
test.lua:3:7: syntax error, unexpected '"testing', expecting '(', 'Name', '{', 'function', '...', 'true', 'false', 'nil', 'String', 'Number', '#', '-', 'not'
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

--r = parse(s)
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
StmBlock [StmLocalVar [("a","any"),("b","any"),("c","any")] [ExpFunction ([]) "any" (StmBlock [])]]
]=]

r = parse(s)
assert(r == e)

s = [=[
local test = function ( a , b , ... ) end
]=]
e = [=[
StmBlock [StmLocalVar [("test","any")] [ExpFunction ([("a","any"),("b","any"),("...","any")]) "any" (StmBlock [])]]
]=]

r = parse(s)
assert(r == e)

s = [=[
local test = function (x:number,y:string) : nil end
]=]
e = [=[
StmBlock [StmLocalVar [("test","any")] [ExpFunction ([("x","number"),("y","string")]) "nil" (StmBlock [])]]
]=]

r = parse(s)
assert(r == e)

s = [=[
local test = function (x:number,t,a:boolean) : nil end
]=]
e = [=[
StmBlock [StmLocalVar [("test","any")] [ExpFunction ([("x","number"),("t","any"),("a","boolean")]) "nil" (StmBlock [])]]
]=]

r = parse(s)
assert(r == e)

s = [=[
test = function (...) return ...,0 end
]=]
e = [=[
StmBlock [StmAssign [VarID ("test","any")] [ExpFunction ([("...","any")]) "any" (StmBlock [StmRet [ExpDots,ExpNum 0.0]])]]
]=]

r = parse(s)
assert(r == e)

-- arithmetic expressions

s = [=[
arithmetic = 1 - 2 * 3 + 4
]=]
e = [=[
StmBlock [StmAssign [VarID ("arithmetic","any")] [ExpAdd (ExpSub (ExpNum 1.0) (ExpMul (ExpNum 2.0) (ExpNum 3.0))) (ExpNum 4.0)]]
]=]

r = parse(s)
assert(r == e)

s = [=[
pow = -3^-2^2
]=]
e = [=[
StmBlock [StmAssign [VarID ("pow","any")] [ExpMinus (ExpPow (ExpNum 3.0) (ExpMinus (ExpPow (ExpNum 2.0) (ExpNum 2.0))))]]
]=]

r = parse(s)
assert(r == e)

-- assignments

s = [=[
a = f()[1]
]=]
e = [=[
StmBlock [StmAssign [VarID ("a","any")] [ExpVar (VarIndex (ExpFunctionCall (ExpVar (VarID ("f","any"))) []) (ExpNum 1.0))]]
]=]

r = parse(s)
assert(r == e)

s = [=[
a()[1] = 1;
]=]
e = [=[
StmBlock [StmAssign [VarIndex (ExpFunctionCall (ExpVar (VarID ("a","any"))) []) (ExpNum 1.0)] [ExpNum 1.0]]
]=]

r = parse(s)
assert(r == e)

s = [=[
i = a.f(1)
]=]
e = [=[
StmBlock [StmAssign [VarID ("i","any")] [ExpFunctionCall (ExpVar (VarIndex (ExpVar (VarID ("a","any"))) (ExpStr "f"))) [ExpNum 1.0]]]
]=]

r = parse(s)
assert(r == e)

s = [=[
i = a[f(1)]
]=]
e = [=[
StmBlock [StmAssign [VarID ("i","any")] [ExpVar (VarIndex (ExpVar (VarID ("a","any"))) (ExpFunctionCall (ExpVar (VarID ("f","any"))) [ExpNum 1.0]))]]
]=]

r = parse(s)
assert(r == e)

s = [=[
a[f()] = sub
i = i + 1
]=]
e = [=[
StmBlock [StmAssign [VarIndex (ExpVar (VarID ("a","any"))) (ExpFunctionCall (ExpVar (VarID ("f","any"))) [])] [ExpVar (VarID ("sub","any"))],StmAssign [VarID ("i","any")] [ExpAdd (ExpVar (VarID ("i","any"))) (ExpNum 1.0)]]
]=]

r = parse(s)
assert(r == e)

s = [=[
a:b(1)._ = some_value
]=]
e = [=[
StmBlock [StmAssign [VarIndex (ExpMethodCall (ExpVar (VarID ("a","any"))) "b" [ExpNum 1.0]) (ExpStr "_")] [ExpVar (VarID ("some_value","any"))]]
]=]

r = parse(s)
assert(r == e)

s = [=[
x:number,y:boolean,z:nil = 1,true
]=]
e = [=[
StmBlock [StmAssign [VarID ("x","number"),VarID ("y","boolean"),VarID ("z","nil")] [ExpNum 1.0,ExpTrue]]
]=]

r = parse(s)
assert(r == e)

s = [=[
x:number,t.f,a:b()[0],z:nil = 1,function () end,"alo"
]=]
e = [=[
StmBlock [StmAssign [VarID ("x","number"),VarIndex (ExpVar (VarID ("t","any"))) (ExpStr "f"),VarIndex (ExpMethodCall (ExpVar (VarID ("a","any"))) "b" []) (ExpNum 0.0),VarID ("z","nil")] [ExpNum 1.0,ExpFunction ([]) "any" (StmBlock []),ExpStr "alo"]]
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
StmBlock [StmWhile (ExpNum 1.0) (StmBlock [StmBreak])]
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
StmBlock [StmWhile (ExpNum 1.0) (StmBlock [StmWhile (ExpNum 1.0) (StmBlock [StmBreak]),StmBreak])]
]=]

r = parse(s)
assert(r == e)

s = [=[
repeat
  if 2 > 1 then break end
until 1
]=]
e = [=[
StmBlock [StmRepeat (StmBlock [StmIfElse (ExpGT (ExpNum 2.0) (ExpNum 1.0)) (StmBlock [StmBreak]) (StmBlock [])]) (ExpNum 1.0)]
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
StmBlock [StmForNum ("i","number") (ExpNum 1.0) (ExpNum 10.0) (ExpNum 1.0) (StmBlock [StmBlock [StmBreak,StmBreak,StmRet []]])]
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
StmBlock [StmBlock [StmAssign [VarID ("var","any")] [ExpAdd (ExpNum 2.0) (ExpNum 2.0)],StmRet []]]
]=]

r = parse(s)
assert(r == e)

-- concatenation expressions

s = [=[
concat1 = 1 .. 2^3
]=]
e = [=[
StmBlock [StmAssign [VarID ("concat1","any")] [ExpConcat (ExpNum 1.0) (ExpPow (ExpNum 2.0) (ExpNum 3.0))]]
]=]

r = parse(s)
assert(r == e)

-- empty files

s = [=[
;
]=]
e = [=[
StmBlock []
]=]

r = parse(s)
assert(r == e)

-- for generic

s = [=[
for k,v in pairs(t) do print (k,v) end
]=]
e = [=[
StmBlock [StmForGen [("k","any"),("v","any")] [ExpFunctionCall (ExpVar (VarID ("pairs","any"))) [ExpVar (VarID ("t","any"))]] (StmBlock [StmCall (ExpFunctionCall (ExpVar (VarID ("print","any"))) [ExpVar (VarID ("k","any")),ExpVar (VarID ("v","any"))])])]
]=]

r = parse(s)
assert(r == e)

s = [=[
for k:number,v:any in pairs(t) do print (k,v) end
]=]
e = [=[
StmBlock [StmForGen [("k","number"),("v","any")] [ExpFunctionCall (ExpVar (VarID ("pairs","any"))) [ExpVar (VarID ("t","any"))]] (StmBlock [StmCall (ExpFunctionCall (ExpVar (VarID ("print","any"))) [ExpVar (VarID ("k","any")),ExpVar (VarID ("v","any"))])])]
]=]

r = parse(s)
assert(r == e)

-- for numeric

s = [=[
for i = 1 , 10 , 2 do end
]=]
e = [=[
StmBlock [StmForNum ("i","number") (ExpNum 1.0) (ExpNum 10.0) (ExpNum 2.0) (StmBlock [])]
]=]

r = parse(s)
assert(r == e)

s = [=[
for i=1,10 do end
]=]
e = [=[
StmBlock [StmForNum ("i","number") (ExpNum 1.0) (ExpNum 10.0) (ExpNum 1.0) (StmBlock [])]
]=]

r = parse(s)
assert(r == e)

-- global functions

s = [=[
function test(a , b , ...) end
]=]
e = [=[
StmBlock [StmFunction (Function ["test"]) ([("a","any"),("b","any"),("...","any")]) "any" (StmBlock [])]
]=]

r = parse(s)
assert(r == e)

s = [=[
function test (...) end
]=]
e = [=[
StmBlock [StmFunction (Function ["test"]) ([("...","any")]) "any" (StmBlock [])]
]=]

r = parse(s)
assert(r == e)

s = [=[
function t.a:b() end
]=]
e = [=[
StmBlock [StmFunction (Method ["t","a","b"]) ([]) "any" (StmBlock [])]
]=]

r = parse(s)
assert(r == e)

s = [=[
function t.a() end
]=]
e = [=[
StmBlock [StmFunction (Function ["t","a"]) ([]) "any" (StmBlock [])]
]=]

r = parse(s)
assert(r == e)

s = [=[
function testando . funcao . com : espcacos ( e, com , parametros, ... ) end
]=]
e = [=[
StmBlock [StmFunction (Method ["testando","funcao","com","espcacos"]) ([("e","any"),("com","any"),("parametros","any"),("...","any")]) "any" (StmBlock [])]
]=]

r = parse(s)
assert(r == e)

s = [=[
function f (x:number,y:string) : nil end
]=]
e = [=[
StmBlock [StmFunction (Function ["f"]) ([("x","number"),("y","string")]) "nil" (StmBlock [])]
]=]

r = parse(s)
assert(r == e)

s = [=[
function f (x:number,y:string,...) : nil end
]=]
e = [=[
StmBlock [StmFunction (Function ["f"]) ([("x","number"),("y","string"),("...","any")]) "nil" (StmBlock [])]
]=]

r = parse(s)
assert(r == e)

s = [=[
function f (x:number,t,a:boolean) : nil end
]=]
e = [=[
StmBlock [StmFunction (Function ["f"]) ([("x","number"),("t","any"),("a","boolean")]) "nil" (StmBlock [])]
]=]

r = parse(s)
assert(r == e)

s = [=[
function f (x:number,...:string) : nil end
]=]
e = [=[
StmBlock [StmFunction (Function ["f"]) ([("x","number"),("...","string")]) "nil" (StmBlock [])]
]=]

r = parse(s)
assert(r == e)

s = [=[
function f (...:string) : nil end
]=]
e = [=[
StmBlock [StmFunction (Function ["f"]) ([("...","string")]) "nil" (StmBlock [])]
]=]

r = parse(s)
assert(r == e)

-- goto

s = [=[
goto label
:: label :: return
]=]
e = [=[
StmBlock [StmGoTo "label",StmLabel "label",StmRet []]
]=]

r = parse(s)
assert(r == e)

s = [=[
::label::
goto label
]=]
e = [=[
StmBlock [StmLabel "label",StmGoTo "label"]
]=]

r = parse(s)
assert(r == e)

s = [=[
goto label
::label::
]=]
e = [=[
StmBlock [StmGoTo "label",StmLabel "label"]
]=]

r = parse(s)
assert(r == e)

s = [=[
::label::
do ::label:: goto label end
]=]
e = [=[
StmBlock [StmLabel "label",StmBlock [StmLabel "label",StmGoTo "label"]]
]=]

r = parse(s)
assert(r == e)

s = [=[
::label::
do goto label ; ::label:: end
]=]
e = [=[
StmBlock [StmLabel "label",StmBlock [StmGoTo "label",StmLabel "label"]]
]=]

r = parse(s)
assert(r == e)

s = [=[
::label::
do goto label end
]=]
e = [=[
StmBlock [StmLabel "label",StmBlock [StmGoTo "label"]]
]=]

r = parse(s)
assert(r == e)

s = [=[
do goto label end
::label::
]=]
e = [=[
StmBlock [StmBlock [StmGoTo "label"],StmLabel "label"]
]=]

r = parse(s)
assert(r == e)

s = [=[
do do do do do goto label end end end end end
::label::
]=]
e = [=[
StmBlock [StmBlock [StmBlock [StmBlock [StmBlock [StmBlock [StmGoTo "label"]]]]],StmLabel "label"]
]=]

r = parse(s)
assert(r == e)

-- if-else

s = [=[
if a then end
]=]
e = [=[
StmBlock [StmIfElse (ExpVar (VarID ("a","any"))) (StmBlock []) (StmBlock [])]
]=]

r = parse(s)
assert(r == e)

s = [=[
if a then return a else return end
]=]
e = [=[
StmBlock [StmIfElse (ExpVar (VarID ("a","any"))) (StmBlock [StmRet [ExpVar (VarID ("a","any"))]]) (StmBlock [StmRet []])]
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
StmBlock [StmIfElse (ExpVar (VarID ("a","any"))) (StmBlock [StmRet [ExpVar (VarID ("a","any"))]]) (StmBlock [StmLocalVar [("c","any")] [ExpVar (VarID ("d","any"))],StmAssign [VarID ("d","any")] [ExpAdd (ExpVar (VarID ("d","any"))) (ExpNum 1.0)],StmRet [ExpVar (VarID ("d","any"))]])]
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
StmBlock [StmIfElse (ExpVar (VarID ("a","any"))) (StmBlock [StmRet [ExpVar (VarID ("a","any"))]]) (StmIfElse (ExpVar (VarID ("b","any"))) (StmBlock [StmRet [ExpVar (VarID ("b","any"))]]) (StmIfElse (ExpVar (VarID ("c","any"))) (StmBlock [StmRet [ExpVar (VarID ("c","any"))]]) (StmBlock [])))]
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
StmBlock [StmIfElse (ExpVar (VarID ("a","any"))) (StmBlock [StmRet [ExpVar (VarID ("a","any"))]]) (StmIfElse (ExpVar (VarID ("b","any"))) (StmBlock [StmRet []]) (StmBlock []))]
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
StmBlock [StmIfElse (ExpVar (VarID ("a","any"))) (StmBlock [StmRet []]) (StmIfElse (ExpVar (VarID ("c","any"))) (StmBlock []) (StmBlock []))]
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
StmBlock [StmLabel "label",StmBlock [StmLabel "label"],StmLabel "other_label"]
]=]

r = parse(s)
assert(r == e)

-- locals

s = [=[
local a
]=]
e = [=[
StmBlock [StmLocalVar [("a","any")] []]
]=]

r = parse(s)
assert(r == e)

s = [=[
local a,b,c
]=]
e = [=[
StmBlock [StmLocalVar [("a","any"),("b","any"),("c","any")] []]
]=]

r = parse(s)
assert(r == e)

s = [=[
local a = 1 , 1 + 2, 5.1
]=]
e = [=[
StmBlock [StmLocalVar [("a","any")] [ExpNum 1.0,ExpAdd (ExpNum 1.0) (ExpNum 2.0),ExpNum 5.1]]
]=]

r = parse(s)
assert(r == e)

s = [=[
local a,b,c = 1.9
]=]
e = [=[
StmBlock [StmLocalVar [("a","any"),("b","any"),("c","any")] [ExpNum 1.9]]
]=]

r = parse(s)
assert(r == e)

s = [=[
local x:number,y:boolean,z:nil
]=]
e = [=[
StmBlock [StmLocalVar [("x","number"),("y","boolean"),("z","nil")] []]
]=]

r = parse(s)
assert(r == e)

s = [=[
local a:number,b,c:nil = 1.9
]=]
e = [=[
StmBlock [StmLocalVar [("a","number"),("b","any"),("c","nil")] [ExpNum 1.9]]
]=]

r = parse(s)
assert(r == e)

s = [=[
local function test() end
]=]
e = [=[
StmBlock [StmLocalFunction "test" ([]) "any" (StmBlock [])]
]=]

r = parse(s)
assert(r == e)

s = [=[
local function test ( a , b , c , ... ) end
]=]
e = [=[
StmBlock [StmLocalFunction "test" ([("a","any"),("b","any"),("c","any"),("...","any")]) "any" (StmBlock [])]
]=]

r = parse(s)
assert(r == e)

s = [=[
local function test(...) return ... end
]=]
e = [=[
StmBlock [StmLocalFunction "test" ([("...","any")]) "any" (StmBlock [StmRet [ExpDots]])]
]=]

r = parse(s)
assert(r == e)

s = [=[
local function test(x:number,y:string) : nil end
]=]
e = [=[
StmBlock [StmLocalFunction "test" ([("x","number"),("y","string")]) "nil" (StmBlock [])]
]=]

r = parse(s)
assert(r == e)

s = [=[
local function test(x:number,t:any,...) : nil end
]=]
e = [=[
StmBlock [StmLocalFunction "test" ([("x","number"),("t","any"),("...","any")]) "nil" (StmBlock [])]
]=]

r = parse(s)
assert(r == e)

s = [=[
local function test(x:number,t,a:boolean) : nil end
]=]
e = [=[
StmBlock [StmLocalFunction "test" ([("x","number"),("t","any"),("a","boolean")]) "nil" (StmBlock [])]
]=]

r = parse(s)
assert(r == e)

-- relational expressions

s = [=[
relational = 1 < 2 >= 3 == 4 ~= 5 < 6 <= 7
]=]
e = [=[
StmBlock [StmAssign [VarID ("relational","any")] [ExpLE (ExpLT (ExpNE (ExpEQ (ExpGE (ExpLT (ExpNum 1.0) (ExpNum 2.0)) (ExpNum 3.0)) (ExpNum 4.0)) (ExpNum 5.0)) (ExpNum 6.0)) (ExpNum 7.0)]]
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
StmBlock [StmRepeat (StmBlock [StmAssign [VarID ("a","any"),VarID ("b","any"),VarID ("c","any")] [ExpAdd (ExpNum 1.0) (ExpNum 1.0),ExpAdd (ExpNum 2.0) (ExpNum 2.0),ExpAdd (ExpNum 3.0) (ExpNum 3.0)],StmBreak]) (ExpLT (ExpVar (VarID ("a","any"))) (ExpNum 1.0))]
]=]

r = parse(s)
assert(r == e)

-- return

s = [=[
return
]=]
e = [=[
StmBlock [StmRet []]
]=]

r = parse(s)
assert(r == e)

s = [=[
return 1
]=]
e = [=[
StmBlock [StmRet [ExpNum 1.0]]
]=]

r = parse(s)
assert(r == e)

s = [=[
return 1,1-2*3+4,"alo"
]=]
e = [=[
StmBlock [StmRet [ExpNum 1.0,ExpAdd (ExpSub (ExpNum 1.0) (ExpMul (ExpNum 2.0) (ExpNum 3.0))) (ExpNum 4.0),ExpStr "alo"]]
]=]

r = parse(s)
assert(r == e)

s = [=[
return;
]=]
e = [=[
StmBlock [StmRet []]
]=]

r = parse(s)
assert(r == e)

s = [=[
return 1;
]=]
e = [=[
StmBlock [StmRet [ExpNum 1.0]]
]=]

r = parse(s)
assert(r == e)

s = [=[
return 1,1-2*3+4,"alo";
]=]
e = [=[
StmBlock [StmRet [ExpNum 1.0,ExpAdd (ExpSub (ExpNum 1.0) (ExpMul (ExpNum 2.0) (ExpNum 3.0))) (ExpNum 4.0),ExpStr "alo"]]
]=]

r = parse(s)
assert(r == e)

-- tables

s = [=[
t = { [1] = "alo", alo = 1, 2; }
]=]
e = [=[
StmBlock [StmAssign [VarID ("t","any")] [ExpTableConstructor ([ExpNum 2.0],[(ExpNum 1.0,ExpStr "alo"),(ExpStr "alo",ExpNum 1.0)])]]
]=]

r = parse(s)
assert(r == e)

s = [=[
t = { 1.5 }
]=]
e = [=[
StmBlock [StmAssign [VarID ("t","any")] [ExpTableConstructor ([ExpNum 1.5],[])]]
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
StmBlock [StmAssign [VarID ("t","any")] [ExpTableConstructor ([ExpNum 1.0,ExpNum 2.0,ExpNum 3.0,ExpNum 4.0,ExpNum 5.0],[])]]
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
StmBlock [StmAssign [VarID ("t","any")] [ExpTableConstructor ([],[(ExpNum 1.0,ExpNum 1.0),(ExpNum 2.0,ExpNum 2.0),(ExpNum 3.0,ExpNum 3.0),(ExpNum 4.0,ExpNum 4.0),(ExpNum 5.0,ExpNum 5.0)])]]
]=]

r = parse(s)
assert(r == e)

s = [=[
local t = {{{}}, {"alo"}}
]=]
e = [=[
StmBlock [StmLocalVar [("t","any")] [ExpTableConstructor ([ExpTableConstructor ([ExpTableConstructor ([],[])],[]),ExpTableConstructor ([ExpStr "alo"],[])],[])]]
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
StmBlock [StmFunction (Function ["f"]) ([("...","any")]) "any" (StmBlock [StmRet [ExpDots]])]
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
StmBlock [StmFunction (Function ["f"]) ([]) "any" (StmBlock [StmFunction (Function ["g"]) ([("x","any"),("y","any"),("...","any")]) "any" (StmBlock [StmRet [ExpDots,ExpDots,ExpDots]])])]
]=]

r = parse(s)
assert(r == e)

s = [=[
local function f (x, ...)
  return ...
end
]=]
e = [=[
StmBlock [StmLocalFunction "f" ([("x","any"),("...","any")]) "any" (StmBlock [StmRet [ExpDots]])]
]=]

r = parse(s)
assert(r == e)

s = [=[
local f = function (x, ...)
  return ...
end
]=]
e = [=[
StmBlock [StmLocalVar [("f","any")] [ExpFunction ([("x","any"),("...","any")]) "any" (StmBlock [StmRet [ExpDots]])]]
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
StmBlock [StmAssign [VarID ("i","any")] [ExpNum 0.0],StmWhile (ExpLT (ExpVar (VarID ("i","any"))) (ExpNum 10.0)) (StmBlock [StmAssign [VarID ("i","any")] [ExpAdd (ExpVar (VarID ("i","any"))) (ExpNum 1.0)]])]
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
test.lua:1:18: syntax error, unexpected ',a)', expecting ')', ':'
]=]

r = parse(s)
assert(r == e)

s = [=[
local a = function (1) end
]=]
e = [=[
test.lua:1:21: syntax error, unexpected '1)', expecting ')', '...', 'Name'
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

-- assignments

s = [=[
x:int = y:int
]=]
e = [=[
test.lua:2:1: syntax error, unexpected 'EOF', expecting 'String', '{', '('
]=]

r = parse(s)
assert(r == e)

s = [=[
x:int = t[f:int]
]=]
e = [=[
test.lua:1:16: syntax error, unexpected ']', expecting 'String', '{', '('
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
test.lua:1:6: syntax error, unexpected ';v', expecting 'in', ',', ':', '='
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

s = [=[
for i:number=1,10 do end
]=]
e = [=[
test.lua:1:13: syntax error, unexpected '=1,10', expecting 'in', ','
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
test.lua:1:18: syntax error, unexpected ',a)', expecting ')', ':'
]=]

r = parse(s)
assert(r == e)

s = [=[
function func(...:) end
]=]
e = [=[
test.lua:1:19: syntax error, unexpected ')', expecting 'Type'
]=]

r = parse(s)
assert(r == e)

s = [=[
function func(a,...:) end
]=]
e = [=[
test.lua:1:21: syntax error, unexpected ')', expecting 'Type'
]=]

r = parse(s)
assert(r == e)

s = [=[
function a.b:c:d () end
]=]
e = [=[
test.lua:1:15: syntax error, unexpected ':d', expecting '('
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
local test = z:nil,1
]=]
e = [=[
test.lua:1:16: syntax error, unexpected 'nil,1', expecting 'Name'
]=]

r = parse(s)
assert(r == e)

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
test.lua:1:17: syntax error, unexpected '.a()', expecting '('
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
test.lua:1:24: syntax error, unexpected ',a)', expecting ')', ':'
]=]

r = parse(s)
assert(r == e)

s = [=[
local function (a, b, c, ...) end
]=]
e = [=[
test.lua:1:16: syntax error, unexpected '(a,', expecting 'Name'
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

print("> testing subtyping...")

-- constant types

local Nil = types.Nil()
local False = types.False()
local True = types.True()
local Double = types.ConstantNumber(1.1)
local Integer = types.ConstantNumber(1)
local Word = types.ConstantString("w")

-- basic types

local Boolean = types.Boolean()
local Number = types.Number()
local String = types.String()

-- other types

local Object = types.Object()
local Any = types.Any()
local t1, t2

assert(types.isConstant(Nil))
assert(types.isConstant(False))
assert(types.isConstant(True))
assert(types.isConstant(Double))
assert(types.isConstant(Integer))
assert(types.isConstant(Word))
assert(not types.isConstant(String))

assert(types.isBasic(Boolean))
assert(types.isBasic(Number))
assert(types.isBasic(String))
assert(not types.isBasic(Word))

assert(types.isObject(Object))
assert(not types.isObject(Any))

assert(types.isAny(Any))
assert(not types.isAny(Object))

-- subtyping

-- constant types

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

assert(not types.subtype(False,Integer))
assert(not types.subtype(True,String))
assert(not types.subtype(Double,Boolean))
assert(not types.subtype(Integer,Nil))
assert(not types.subtype(Word,Number))

-- basic types

assert(types.subtype(Boolean,Boolean))
assert(types.subtype(Number,Number))
assert(types.subtype(String,String))

assert(not types.subtype(Boolean,False))
assert(not types.subtype(Number,Boolean))
assert(not types.subtype(String,Number))

-- object

assert(types.subtype(Object,Object))
assert(types.subtype(Nil,Object))
assert(types.subtype(False,Object))
assert(types.subtype(True,Object))
assert(types.subtype(Double,Object))
assert(types.subtype(Integer,Object))
assert(types.subtype(Word,Object))
assert(types.subtype(Boolean,Object))
assert(types.subtype(Number,Object))
assert(types.subtype(String,Object))
assert(types.subtype(Any,Object))

assert(not types.subtype(Object,Nil))
assert(not types.subtype(Object,False))
assert(not types.subtype(Object,True))
assert(not types.subtype(Object,Double))
assert(not types.subtype(Object,Integer))
assert(not types.subtype(Object,Word))
assert(not types.subtype(Object,Boolean))
assert(not types.subtype(Object,Number))
assert(not types.subtype(Object,String))
assert(not types.subtype(Object,Any))

-- any

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

-- union

t1 = types.Union(Nil,Boolean)
assert(types.subtype(Nil,t1))
assert(types.subtype(False,t1))
assert(types.subtype(True,t1))
assert(not types.subtype(t1,Nil))
assert(not types.subtype(t1,False))
assert(not types.subtype(t1,True))

t1 = types.Union(Nil,Number)
assert(types.subtype(Nil,t1))
assert(types.subtype(Double,t1))
assert(types.subtype(Integer,t1))
assert(not types.subtype(t1,Nil))
assert(not types.subtype(t1,Double))
assert(not types.subtype(t1,Integer))

t1 = types.Union(Number,String)
assert(types.subtype(Double,t1))
assert(types.subtype(Integer,t1))
assert(types.subtype(Word,t1))
assert(not types.subtype(t1,Double))
assert(not types.subtype(t1,Integer))
assert(not types.subtype(t1,Word))

t1 = types.Union(True,False)
assert(types.subtype(t1,Boolean))
assert(not types.subtype(Boolean,t1))

t1 = types.Union(Integer,Double)
assert(types.subtype(t1,Number))
assert(not types.subtype(Number,t1))

t1 = types.Union(Boolean,Number)
t2 = types.Union(t1,String)
assert(types.subtype(Boolean,t2))
assert(types.subtype(Number,t2))
assert(types.subtype(String,t2))
assert(types.subtype(t2,Object))
assert(not types.subtype(t2,Boolean))
assert(not types.subtype(t2,Number))
assert(not types.subtype(t2,String))
assert(not types.subtype(Object,t2))
assert(not types.subtype(Any,t2))
assert(not types.subtype(t2,Any))

print("> testing type checker...")

-- tests that type check

-- arithmetic expressions

s = [=[
local x:number = 1 + 1
]=]
e = [=[
StmBlock [StmLocalVar [("x","number")] [ExpAdd (ExpNum 1.0) (ExpNum 1.0)]]
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:number = -1
]=]
e = [=[
StmBlock [StmLocalVar [("x","number")] [ExpMinus (ExpNum 1.0)]]
]=]

r = typecheck(s)
assert(r == e)

-- assignments

s = [=[
x:number,y:number,z:string = 1,2,""
z = "alo"
]=]
e = [=[
StmBlock [StmAssign [VarID ("x","number"),VarID ("y","number"),VarID ("z","string")] [ExpNum 1.0,ExpNum 2.0,ExpStr ""],StmAssign [VarID ("z","any")] [ExpStr "alo"]]
]=]

r = typecheck(s)
assert(r == e)

-- concatenation expressions

s = [=[
local x:string = "hello" .. "world"
]=]
e = [=[
StmBlock [StmLocalVar [("x","string")] [ExpConcat (ExpStr "hello") (ExpStr "world")]]
]=]

r = typecheck(s)
assert(r == e)

-- for numeric

s = [=[
for i=1,10 do end
]=]
e = [=[
StmBlock [StmForNum ("i","number") (ExpNum 1.0) (ExpNum 10.0) (ExpNum 1.0) (StmBlock [])]
]=]

r = typecheck(s)
assert(r == e)

-- length operator

s = [=[
local x:number = #"hello world"
]=]
e = [=[
StmBlock [StmLocalVar [("x","number")] [ExpLen (ExpStr "hello world")]]
]=]

r = typecheck(s)
assert(r == e)

-- order expressions

s = [=[
local x:boolean = 1 > 2 
]=]
e = [=[
StmBlock [StmLocalVar [("x","boolean")] [ExpGT (ExpNum 1.0) (ExpNum 2.0)]]
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x:boolean = "hello" < "world"
]=]
e = [=[
StmBlock [StmLocalVar [("x","boolean")] [ExpLT (ExpStr "hello") (ExpStr "world")]]
]=]

r = typecheck(s)
assert(r == e)

-- tests that do not type check

-- arithmetic expressions

s = [=[
local x = 1 + "hello"
]=]
e = [=[
test.lua:1:15: type error, attempt to perform arithmetic on a string
]=]

r = typecheck(s)
assert(r == e)

s = [=[
local x = -"hello"
]=]
e = [=[
test.lua:1:12: type error, attempt to perform arithmetic on a string
]=]

r = typecheck(s)
assert(r == e)

-- assignments

s = [=[
x:number,y:number,z:boolean = 1,2
]=]
e = [=[
test.lua:1:19: type error, attempt to assign 'nil' to 'boolean'
]=]

r = typecheck(s)
assert(r == e)

-- concatenation expressions

s = [=[
local x = 1 .. "hello"
]=]
e = [=[
test.lua:1:11: type error, attempt to concatenate a number
]=]

r = typecheck(s)
assert(r == e)

-- for numeric

s = [=[
for i=nil,10 do end
]=]
e = [=[
test.lua:1:7: type error, 'for' initial value must be a number
]=]

r = typecheck(s)
assert(r == e)

s = [=[
for i=1,nil do end
]=]
e = [=[
test.lua:1:9: type error, 'for' limit must be a number
]=]

r = typecheck(s)
assert(r == e)

s = [=[
for i=1,10,nil do end
]=]
e = [=[
test.lua:1:12: type error, 'for' step must be a number
]=]

r = typecheck(s)
assert(r == e)

-- length operator

s = [=[
local x = #1
]=]
e = [=[
test.lua:1:12: type error, attempt to get length of a number value
]=]

r = typecheck(s)
assert(r == e)

-- order expressions

s = [=[
local x:boolean = 1 < "hello"
]=]
e = [=[
test.lua:1:19: type error, attempt to compare number with string
]=]

r = typecheck(s)
assert(r == e)

print("> testing code generation...")

-- simple tests

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

r = generatecode(s)
assert(r == e)

-- for

s = [=[
for i=1,10 do end
]=]
e = [=[
for i=1,10,1 do

end
]=]

r = generatecode(s)
assert(r == e)

s = [=[
for i=1,10,-1 do end
]=]
e = [=[
for i=1,10,(-1) do

end
]=]

r = generatecode(s)
assert(r == e)

-- functions

s = [=[
function f (x, y, z)
  return function (a, ...) end
end
]=]
e = [=[
function f (x, y, z)
  return function (a, ...)
  end
end
]=]

--r = generatecode(s)
--assert(r == e)

s = [=[
function f.m (x)
  return {1,2,3}
end
]=]
e = [=[
function f.m (x)
  return { 1, 2, 3, }
end
]=]

r = generatecode(s)
assert(r == e)

s = [=[
function f:m (x)
  return {1,2,3}
end
]=]
e = [=[
function f:m (x)
  return { 1, 2, 3, }
end
]=]

r = generatecode(s)
assert(r == e)

-- if-else

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

r = generatecode(s)
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

r = generatecode(s)
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

r = generatecode(s)
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

r = generatecode(s)
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

r = generatecode(s)
assert(r == e)

-- local functions

s = [=[
local function f (x)
  return { foo = "bar", f = function () end }
end
]=]
e = [=[
local function f (x)
  return { [ "foo" ] = "bar", [ "f" ] = function ()
  end, }
end
]=]

r = generatecode(s)
assert(r == e)

-- repeat

s = [=[
repeat
  do
    goto eof
  end
until 1
::eof::
]=]
e = [=[
repeat
  do
    goto eof
  end
until 1
::eof::
]=]

r = generatecode(s)
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

r = generatecode(s)
assert(r == e)

print("OK")
