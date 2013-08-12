--[[
This file implements the Typed Lua parser using LPeg
]]

local parser = {}

local lpeg = require("lpeg")

lpeg.locale(lpeg)

local P,S,V = lpeg.P,lpeg.S,lpeg.V
local C,Carg,Cb,Cc = lpeg.C,lpeg.Carg,lpeg.Cb,lpeg.Cc
local Cf,Cg,Cmt,Cp,Ct = lpeg.Cf,lpeg.Cg,lpeg.Cmt,lpeg.Cp,lpeg.Ct
local alpha,digit,alnum = lpeg.alpha,lpeg.digit,lpeg.alnum
local xdigit = lpeg.xdigit
local space = lpeg.space

-- error message auxiliary functions

-- trim
local function trim (s)
  return s:gsub("^%s+", ""):gsub("%s+$", "")
end

-- gets line number and column number
function parser.lineno (s, i)
  if i == 1 then return 1,1 end
  local n,lastline = 0,""
  s = s:sub(1,i) .. "\n"
  for line in s:gmatch("[^\n]*[\n]") do
    n = n + 1
    lastline = line
  end
  return n,lastline:len()-1
end

-- creates an error message for the input string
local function errormsg (s, t)
  local i = t.ffp or 1
  local lineno,colno = parser.lineno(s,i)
  local u = lpeg.match(C((1 - space)^0), s, i)
  local msg
  if u == '' then
    u = "EOF"
    colno = 1
  end
  msg = ("%s:%d:%d: syntax error, unexpected '%s'"):
        format(t.filename, lineno, colno, u)
  if t.expected then
    msg = msg .. (", expecting %s"):format(t.expected)
  end
  return msg
end

-- aborts with an error message
local function report_error (s, i, t)
  print(errormsg(s, t))
  os.exit(1)
end

-- sets the farthest failure position and the expected tokens
local function setffp (s, i, t, n)
  if not t.ffp or i > t.ffp then
    t.ffp = i
    t.list = {} ; t.list[n] = n
    t.expected = "'" .. n .. "'"
  elseif i == t.ffp then
    if not t.list[n] then
      t.list[n] = n
      t.expected = "'" .. n .. "', " .. t.expected
    end
  end
  return false
end

local function updateffp (name)
  return Cmt(Carg(1) * Cc(name), setffp)
end

-- regular combinators and auxiliary functions

local function token (pat, name)
  return pat * V"Skip" + updateffp(name) * P(false)
end

local function symb (str)
  return token (P(str), str)
end

local function kw (str)
  return token (P(str) * -V"idRest", str)
end

local function taggedCap (tag, pat)
  return Ct(Cg(Cp(), "pos") * Cg(Cc(tag), "tag") * pat)
end

local function unaryop (op, e)
  return { tag = op, pos = e.pos, [1] = e }
end

local function binaryop (e1, op, e2)
  if not op then return e1 end
  return { tag = op, pos = e1.pos, [1] = e1, [2] = e2 }
end

local function chainl (pat, sep, a)
  return Cf(pat * Cg(sep * pat)^0, binaryop) + a
end

local function chainl1 (pat, sep)
  return Cf(pat * Cg(sep * pat)^0, binaryop)
end

local function sepby (pat, sep, tag)
  return taggedCap(tag, (pat * (sep * pat)^0)^-1)
end

local function sepby1 (pat, sep, tag)
  return taggedCap(tag, pat * (sep * pat)^0)
end

local function expl2varl (s, i, el)
  local vl = {}
  for k,v in ipairs(el) do
    if v.tag == "ExpVar" then
      vl[k] = v[1]
    else
      return false
    end
  end
  return true,vl
end

local function fix_str (str)
  str = string.gsub(str, "\\a", "\a")
  str = string.gsub(str, "\\b", "\b")
  str = string.gsub(str, "\\f", "\f")
  str = string.gsub(str, "\\n", "\n")
  str = string.gsub(str, "\\r", "\r")
  str = string.gsub(str, "\\t", "\t")
  str = string.gsub(str, "\\v", "\v")
  str = string.gsub(str, "\\\n", "\n")
  str = string.gsub(str, "\\\r", "\n")
  str = string.gsub(str, "\\'", "'")
  str = string.gsub(str, '\\"', '"')
  str = string.gsub(str, '\\\\', '\\')
  return str
end

-- grammar

local G = { V"Lua",
  -- parser
  Lua = V"Shebang"^-1 * V"Skip" * V"Chunk" * -1;
  Chunk = V"Block";
  Type = token(V"Name", "Type") +
         token(C"nil", "Type");
  OptionalType = (symb(":") * V"Type") + V"DynamicType";
  DynamicType = Cc("any");
  TypedName = taggedCap("Name", token(V"Name", "Name") * V"OptionalType");
  TypedVar = taggedCap("VarID", token(V"Name", "Name") * symb(":") * V"Type");
  TypedGlobal = taggedCap("ExpVar", V"TypedVar" * -V"FuncArgs");
  TypedVarArg = taggedCap("Name", token(C("..."), "...") * V"OptionalType");
  StatList = (symb(";") + V"Stat")^0;
  Var = taggedCap("VarID", token(V"Name","Name") * V"DynamicType");
  FunctionDef = taggedCap("ExpFunction", kw("function") * V"FuncBody");
  FieldSep = symb(",") + symb(";");
  Field = (Cc(function (t, e) local i = #t[2]+1; t[2][i] = e; return t end) *
            (Ct(symb("[") * V"Expr" * symb("]") * symb("=") * V"Expr") +
             Ct(taggedCap("ExpStr", token(V"Name","Name")) * symb("=") * V"Expr"))) +
          Cc(function (t, e) local i = #t[1]+1; t[1][i] = e ; return t end) *
             Ct(V"Expr");
  FieldList = Cp() * (V"Field" * (V"FieldSep" * V"Field")^0 * V"FieldSep"^-1)^-1 /
              function (p, ...)
                local t = {{},{}}
                local args = {...}
                local len = #args
                t.tag = "FieldList"
                t.pos = p
                if len > 1 then
                  for i=1,len,2 do
                    t = args[i](t, args[i+1])
                  end
                end
                return t
              end;
  Constructor = taggedCap("ExpTableConstructor", symb("{") * V"FieldList" * symb("}"));
  NameList = sepby1(V"TypedName", symb(","), "NameList");
  ExpList = sepby1(V"Expr", symb(","), "ExpList");
  FuncArgs = symb("(") * (V"ExpList" + taggedCap("ExpList", Cc())) * symb(")") +
             taggedCap("ExpList", V"Constructor") +
             taggedCap("ExpList", taggedCap("ExpStr", token(V"String","String")));
  Expr = V"SubExpr_1";
  SubExpr_1 = chainl1(V"SubExpr_2", V"OrOp");
  SubExpr_2 = chainl1(V"SubExpr_3", V"AndOp");
  SubExpr_3 = chainl1(V"SubExpr_4", V"RelOp");
  SubExpr_4 = V"SubExpr_5" * V"ConOp" * V"SubExpr_4" / binaryop +
              V"SubExpr_5";
  SubExpr_5 = chainl1(V"SubExpr_6", V"AddOp");
  SubExpr_6 = chainl1(V"SubExpr_7", V"MulOp");
  SubExpr_7 = V"UnOp" * V"SubExpr_7" / unaryop +
              V"SubExpr_8";
  SubExpr_8 = V"SimpleExp" * (V"PowOp" * V"SubExpr_7")^-1 / binaryop;
  SimpleExp = taggedCap("ExpNum", token(V"Number","Number")) +
              taggedCap("ExpStr", token(V"String","String")) +
              taggedCap("ExpNil", kw("nil")) +
              taggedCap("ExpFalse", kw("false")) +
              taggedCap("ExpTrue", kw("true")) +
              taggedCap("ExpDots", symb("...")) +
              V"FunctionDef" +
              V"Constructor" +
              V"SuffixedExp";
  SuffixedExp = Cf(V"PrimaryExp" * (
                  taggedCap("DotIndex", symb(".") * taggedCap("ExpStr", token(V"Name","Name"))) +
                  taggedCap("ArrayIndex", symb("[") * V"Expr" * symb("]")) +
                  taggedCap("ExpMethodCall", Cg(symb(":") * token(V"Name","Name") * V"FuncArgs")) +
                  taggedCap("ExpFunctionCall", V"FuncArgs")
                )^0, function (t1, t2)
                       if t2 then
                         if t2.tag == "ExpMethodCall" then
                           return {tag = t2.tag, pos = t1.pos, [1] = t1, [2] = t2[1], [3] = t2[2]}
                         elseif t2.tag == "ExpFunctionCall" then
                           return {tag = t2.tag, pos = t1.pos, [1] = t1, [2] = t2[1]}
                         else
                           return {tag = "ExpVar", pos = t1.pos, [1] = {tag = "VarIndex", pos = t1.pos, [1] = t1, [2] = t2[1]}}
                         end
                       end
                       return t1
                     end);
  PrimaryExp = taggedCap("ExpVar", V"Var") +
               symb("(") * V"Expr" * symb(")");
  Block = taggedCap("StmBlock", V"StatList" * V"RetStat"^-1);
  ElseIf = taggedCap("StmIfElse",
             kw("elseif") * V"Expr" * kw("then") * V"Block" *
               (V"ElseIf" + taggedCap("StmBlock", Cc()))) +
             kw("else") * V"Block";
  IfStat = taggedCap("StmIfElse",
             kw("if") * V"Expr" * kw("then") * V"Block" *
               (V"ElseIf" + taggedCap("StmBlock", Cc())) * kw("end"));
  WhileStat = taggedCap("StmWhile", kw("while") * V"Expr" *
                kw("do") * V"Block" * kw("end"));
  DoStat = kw("do") * V"Block" * kw("end");
  ForBody = kw("do") * V"Block";
  ForNum = taggedCap("StmForNum",
             V"TypedName" * symb("=") * V"Expr" * symb(",") *
             V"Expr" * ((symb(",") * V"Expr") + Cc({tag = "ExpNum", [1] = 1})) *
             V"ForBody");
  ForGen = taggedCap("StmForGen", V"NameList" * kw("in") * V"ExpList" * V"ForBody");
  ForStat = kw("for") * (V"ForNum" + V"ForGen") * kw("end");
  RepeatStat = taggedCap("StmRepeat", kw("repeat") * V"Block" *
                 kw("until") * V"Expr");
  FuncName = sepby1(token(V"Name","Name"), symb("."), "IDList") * (symb(":") * token(V"Name","Name"))^-1 /
             function (t, n)
               if n then
                 t.tag = "Method"
                 t[#t+1] = n
               else
                 t.tag = "Function"
               end
               return t
             end;
  ParList = V"NameList" * (symb(",") * V"TypedVarArg")^-1 /
            function (t, v)
              if v then table.insert(t, v) end
              return t
            end +
            taggedCap("NameList", V"TypedVarArg"^-1);
  FuncBody = symb("(") * V"ParList" * symb(")") *
             V"OptionalType" * V"Block" * kw("end");
  FuncStat = taggedCap("StmFunction", kw("function") * V"FuncName" * V"FuncBody");
  LocalFunc = taggedCap("StmLocalFunction", kw("function") * token(V"Name","Name") * V"FuncBody");
  LocalAssign = taggedCap("StmLocalVar", V"NameList" * ((symb("=") * V"ExpList") + Ct(Cc())));
  LocalStat = kw("local") * (V"LocalFunc" + V"LocalAssign");
  LabelStat = taggedCap("StmLabel", symb("::") * token(V"Name","Name") * symb("::"));
  BreakStat = taggedCap("StmBreak", kw("break"));
  GoToStat = taggedCap("StmGoTo", kw("goto") * token(V"Name","Name"));
  RetStat = taggedCap("StmRet", kw("return") * (V"ExpList" + Ct(Cc())) * symb(";")^-1);
  ExprStat = Cmt(
             ((V"TypedGlobal" + V"SuffixedExp") *
                (Cc(function (...)
                           local vl = {...}
                           local el = vl[#vl]
                           table.remove(vl)
                           for k,v in ipairs(vl) do
                             if v.tag == "ExpVar" then
                               vl[k] = v[1]
                             else
                               -- invalid assignment
                               return false
                             end
                           end
                           vl.tag = "VarList"
                           vl.pos = vl[1].pos
                           return true,{tag = "StmAssign", pos = vl.pos, [1] = vl, [2] = el}
                         end) * V"Assignment"))
             +
             (V"SuffixedExp" *
                (Cc(function (s)
                           if s.tag == "ExpMethodCall" or
                              s.tag == "ExpFunctionCall" then
                             return true,{tag = "StmCall", pos = s.pos, [1] = s}
                           end
                           -- invalid statement
                           return false
                         end)))
             , function (s, i, s1, f, ...) return f(s1,...) end);
  Assignment = ((symb(",") * (V"TypedGlobal" + V"SuffixedExp"))^1)^-1 * symb("=") * V"ExpList";
  Stat = V"IfStat" + V"WhileStat" + V"DoStat" + V"ForStat" +
         V"RepeatStat" + V"FuncStat" + V"LocalStat" + V"LabelStat" +
         V"BreakStat" + V"GoToStat" + V"ExprStat";
  -- lexer
  Space = space^1;
  Equals = P"="^0;
  Open = "[" * Cg(V"Equals", "init") * "[" * P"\n"^-1;
  Close = "]" * C(V"Equals") * "]";
  CloseEQ = Cmt(V"Close" * Cb("init"),
            function (s, i, a, b) return a == b end);
  LongString = V"Open" * C((P(1) - V"CloseEQ")^0) * V"Close" /
               function (s, o) return s end;
  Comment = P"--" * V"LongString" / function () return end +
            P"--" * (P(1) - P"\n")^0;
  Skip = (V"Space" + V"Comment")^0;
  idStart = alpha + P("_");
  idRest = alnum + P("_");
  Keywords = P("and") + "break" + "do" + "elseif" + "else" + "end" +
             "false" + "for" + "function" + "goto" + "if" + "in" +
             "local" + "nil" + "not" + "or" + "repeat" + "return" +
             "then" + "true" + "until" + "while";
  Reserved = V"Keywords" * -V"idRest";
  Identifier = V"idStart" * V"idRest"^0;
  Name = -V"Reserved" * C(V"Identifier") * -V"idRest";
  Hex = (P("0x") + P("0X")) * xdigit^1;
  Expo = S("eE") * S("+-")^-1 * digit^1;
  Float = (((digit^1 * P(".") * digit^0) +
          (P(".") * digit^1)) * V"Expo"^-1) +
          (digit^1 * V"Expo");
  Int = digit^1;
  Number = C(V"Hex" + V"Float" + V"Int") /
           function (n) return tonumber(n) end;
  ShortString = P'"' * C(((P'\\' * P(1)) + (P(1) - P'"'))^0) * P'"' +
                P"'" * C(((P"\\" * P(1)) + (P(1) - P"'"))^0) * P"'";
  String = V"LongString" + (V"ShortString" / function (s) return fix_str(s) end);
  OrOp = kw("or") / "ExpOr";
  AndOp = kw("and") / "ExpAnd";
  RelOp = symb("~=") / "ExpNE" +
          symb("==") / "ExpEQ" +
          symb("<=") / "ExpLE" +
          symb(">=") / "ExpGE" +
          symb("<") / "ExpLT" +
          symb(">") / "ExpGT";
  ConOp = symb("..") / "ExpConcat";
  AddOp = symb("+") / "ExpAdd" +
          symb("-") / "ExpSub";
  MulOp = symb("*") / "ExpMul" +
          symb("/") / "ExpDiv" +
          symb("%") / "ExpMod";
  UnOp = kw("not") / "ExpNot" +
         symb("-") / "ExpMinus" +
         symb("#") / "ExpLen";
  PowOp = symb("^") / "ExpPow";
  Shebang = P"#" * (P(1) - P"\n")^0 * P"\n";
}

local function getcontents (filename)
  local file = assert(io.open(filename, "r"))
  local contents = file:read("*a")
  file:close()
  return contents
end

function parser.parse_from_file (filename)
  local errorinfo = { filename = filename, loop = 0 }
  local input = getcontents(filename)
  lpeg.setmaxstack(1000)
  local ast = lpeg.match(G, input, nil, errorinfo)
  if not ast then
    return nil,errormsg(input, errorinfo)
  end
  return ast
end

function parser.parse (subject, filename)
  local errorinfo = { filename = filename, loop = 0 }
  lpeg.setmaxstack(1000)
  local ast = lpeg.match(G, subject, nil, errorinfo)
  if not ast then
    return nil,errormsg(subject, errorinfo)
  end
  return ast
end

return parser
