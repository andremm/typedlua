local parser = {}

local lpeg = require "lpeg"

lpeg.locale(lpeg)

local P, S, V = lpeg.P, lpeg.S, lpeg.V
local C, Carg, Cb, Cc = lpeg.C, lpeg.Carg, lpeg.Cb, lpeg.Cc
local Cf, Cg, Cmt, Cp, Ct = lpeg.Cf, lpeg.Cg, lpeg.Cmt, lpeg.Cp, lpeg.Ct
local alpha, digit, alnum = lpeg.alpha, lpeg.digit, lpeg.alnum
local xdigit = lpeg.xdigit
local space = lpeg.space

-- error message auxiliary functions

-- trim
local function trim (s)
  return s:gsub("^%s+", ""):gsub("%s+$", "")
end

-- returns line number and column number
local function lineno (s, i)
  if i == 1 then return 1, 1 end
  local l, lastline = 0, ""
  s = s:sub(1, i) .. "\n"
  for line in s:gmatch("[^\n]*[\n]") do
    l = l + 1
    lastline = line
  end
  local c = lastline:len() - 1
  return l, c ~= 0 and c or 1
end

-- creates an error message for the input string
local function syntaxerror (errorinfo, pos, msg)
  local l, c = lineno(errorinfo.subject, pos)
  local error_msg = "%s:%d:%d: syntax error, %s"
  return string.format(error_msg, errorinfo.filename, l, c, msg)
end

-- gets the farthest failure position
local function getffp (s, i, t)
  return t.ffp or i, t
end

-- gets the table that contains the error information
local function geterrorinfo ()
  return Cmt(Carg(1), getffp) * (C(V"OneWord") + Cc("EOF")) /
  function (t, u)
    t.unexpected = u
    return t
  end
end

-- creates an errror message using the farthest failure position
local function errormsg ()
  return geterrorinfo() /
  function (t)
    local p = t.ffp or 1
    local msg = "unexpected '%s', expecting %s"
    msg = string.format(msg, t.unexpected, t.expected)
    return nil, syntaxerror(t, p, msg)
  end
end

-- reports a syntactic error
local function report_error ()
  return errormsg()
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
  return { tag = "Op", pos = e.pos, [1] = op, [2] = e }
end

local function binaryop (e1, op, e2)
  if not op then
    return e1
  elseif op == "add" or
         op == "sub" or
         op == "mul" or
         op == "div" or
         op == "mod" or
         op == "pow" or
         op == "concat" or
         op == "eq" or
         op == "lt" or
         op == "le" or
         op == "and" or
         op == "or" then
    return { tag = "Op", pos = e1.pos, [1] = op, [2] = e1, [3] = e2 }
  elseif op == "ne" then
    return unaryop ("not", { tag = "Op", pos = e1.pos, [1] = "eq", [2] = e1, [3] = e2 })
  elseif op == "gt" then
    return { tag = "Op", pos = e1.pos, [1] = "lt", [2] = e2, [3] = e1 }
  elseif op == "ge" then
    return { tag = "Op", pos = e1.pos, [1] = "le", [2] = e2, [3] = e1 }
  end
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
  Lua = V"Shebang"^-1 * V"Skip" * V"Chunk" * -1 + report_error();
  -- parser
  Chunk = V"Block";
  StatList = (symb(";") + V"Stat")^0;
  Var = V"Id";
  Id = taggedCap("Id", token(V"Name", "Name"));
  FunctionDef = kw("function") * V"FuncBody";
  FieldSep = symb(",") + symb(";");
  Field = taggedCap("Pair", (symb("[") * V"Expr" * symb("]") * symb("=") * V"Expr") +
                    (taggedCap("String", token(V"Name", "Name")) * symb("=") * V"Expr")) +
          V"Expr";
  FieldList = (V"Field" * (V"FieldSep" * V"Field")^0 * V"FieldSep"^-1)^-1;
  Constructor = taggedCap("Table", symb("{") * V"FieldList" * symb("}"));
  NameList = sepby1(V"Id", symb(","), "NameList");
  ExpList = sepby1(V"Expr", symb(","), "ExpList");
  FuncArgs = symb("(") * (V"Expr" * (symb(",") * V"Expr")^0)^-1 * symb(")") +
             V"Constructor" +
             taggedCap("String", token(V"String", "String"));
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
  SimpleExp = taggedCap("Number", token(V"Number", "Number")) +
              taggedCap("String", token(V"String", "String")) +
              taggedCap("Nil", kw("nil")) +
              taggedCap("False", kw("false")) +
              taggedCap("True", kw("true")) +
              taggedCap("Dots", symb("...")) +
              V"FunctionDef" +
              V"Constructor" +
              V"SuffixedExp";
  SuffixedExp = Cf(V"PrimaryExp" * (
                  taggedCap("DotIndex", symb(".") * taggedCap("String", token(V"Name", "Name"))) +
                  taggedCap("ArrayIndex", symb("[") * V"Expr" * symb("]")) +
                  taggedCap("Invoke", Cg(symb(":") * taggedCap("String", token(V"Name", "Name")) * V"FuncArgs")) +
                  taggedCap("Call", V"FuncArgs")
                )^0, function (t1, t2)
                       if t2 then
                         if t2.tag == "Call" or t2.tag == "Invoke" then
                           local t = {tag = t2.tag, pos = t1.pos, [1] = t1}
                           for k, v in ipairs(t2) do
                             table.insert(t, v)
                           end
                           return t
                         else
                           return {tag = "Index", pos = t1.pos, [1] = t1, [2] = t2[1]}
                         end
                       end
                       return t1
                     end);
  PrimaryExp = V"Var" +
               taggedCap("Paren", symb("(") * V"Expr" * symb(")"));
  Block = taggedCap("Block", V"StatList" * V"RetStat"^-1);
  IfStat = taggedCap("If",
             kw("if") * V"Expr" * kw("then") * V"Block" *
             (kw("elseif") * V"Expr" * kw("then") * V"Block")^0 *
             (kw("else") * V"Block")^-1 *
             kw("end"));
  WhileStat = taggedCap("While", kw("while") * V"Expr" *
                kw("do") * V"Block" * kw("end"));
  DoStat = kw("do") * V"Block" * kw("end") /
           function (t)
             t.tag = "Do"
             return t
           end;
  ForBody = kw("do") * V"Block";
  ForNum = taggedCap("Fornum",
             V"Id" * symb("=") * V"Expr" * symb(",") *
             V"Expr" * (symb(",") * V"Expr")^-1 *
             V"ForBody");
  ForGen = taggedCap("Forin", V"NameList" * kw("in") * V"ExpList" * V"ForBody");
  ForStat = kw("for") * (V"ForNum" + V"ForGen") * kw("end");
  RepeatStat = taggedCap("Repeat", kw("repeat") * V"Block" *
                 kw("until") * V"Expr");
  FuncName = Cf(V"Id" * (symb(".") * taggedCap("String", token(V"Name", "Name")))^0,
             function (t1, t2)
               if t2 then
                 return {tag = "Index", pos = t1.pos, [1] = t1, [2] = t2}
               end
               return t1
             end) * (symb(":") * taggedCap("String", token(V"Name", "Name")))^-1 /
             function (t1, t2)
               if t2 then
                 return {tag = "Index", pos = t1.pos, is_method = true, [1] = t1, [2] = t2}
               end
               return t1
             end;
  ParList = V"NameList" * (symb(",") * symb("...") * taggedCap("Dots", Cp()))^-1 /
            function (t, v)
              if v then table.insert(t, v) end
              return t
            end +
            symb("...") * taggedCap("Dots", Cp()) /
            function (v)
              return {v}
            end +
            P(true) / function () return {} end;
  FuncBody = taggedCap("Function", symb("(") * V"ParList" * symb(")") * V"Block" * kw("end"));
  FuncStat = taggedCap("Set", kw("function") * V"FuncName" * V"FuncBody") /
             function (t)
               if t[1].is_method then table.insert(t[2][1], 1, {tag = "Id", [1] = "self"}) end
               t[1] = {t[1]}
               t[2] = {t[2]}
               return t
             end;
  LocalFunc = taggedCap("Localrec", kw("function") * V"Id" * V"FuncBody") /
              function (t)
                t[1] = {t[1]}
                t[2] = {t[2]}
                return t
              end;
  LocalAssign = taggedCap("Local", V"NameList" * ((symb("=") * V"ExpList") + Ct(Cc())));
  LocalStat = kw("local") * (V"LocalFunc" + V"LocalAssign");
  LabelStat = taggedCap("Label", symb("::") * token(V"Name", "Name") * symb("::"));
  BreakStat = taggedCap("Break", kw("break"));
  GoToStat = taggedCap("Goto", kw("goto") * token(V"Name", "Name"));
  RetStat = taggedCap("Return", kw("return") * (V"Expr" * (symb(",") * V"Expr")^0)^-1 * symb(";")^-1);
  ExprStat = Cmt(
             (V"SuffixedExp" *
                (Cc(function (...)
                           local vl = {...}
                           local el = vl[#vl]
                           table.remove(vl)
                           for k, v in ipairs(vl) do
                             if v.tag == "Id" or v.tag == "Index" then
                               vl[k] = v
                             else
                               -- invalid assignment
                               return false
                             end
                           end
                           vl.tag = "VarList"
                           vl.pos = vl[1].pos
                           return true, {tag = "Set", pos = vl.pos, [1] = vl, [2] = el}
                         end) * V"Assignment"))
             +
             (V"SuffixedExp" *
                (Cc(function (s)
                           if s.tag == "Call" or
                              s.tag == "Invoke" then
                             return true, s
                           end
                           -- invalid statement
                           return false
                         end)))
             , function (s, i, s1, f, ...) return f(s1, ...) end);
  Assignment = ((symb(",") * V"SuffixedExp")^1)^-1 * symb("=") * V"ExpList";
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
  OrOp = kw("or") / "or";
  AndOp = kw("and") / "and";
  RelOp = symb("~=") / "ne" +
          symb("==") / "eq" +
          symb("<=") / "le" +
          symb(">=") / "ge" +
          symb("<") / "lt" +
          symb(">") / "gt";
  ConOp = symb("..") / "concat";
  AddOp = symb("+") / "add" +
          symb("-") / "sub";
  MulOp = symb("*") / "mul" +
          symb("/") / "div" +
          symb("%") / "mod";
  UnOp = kw("not") / "not" +
         symb("-") / "unm" +
         symb("#") / "len";
  PowOp = symb("^") / "pow";
  Shebang = P"#" * (P(1) - P"\n")^0 * P"\n";
  -- for error reporting
  OneWord = V"Name" + V"Number" + V"String" + V"Reserved" + P("...") + P(1);
}

local function getcontents (filename)
  local file = assert(io.open(filename, "r"))
  local contents = file:read("*a")
  file:close()
  return contents
end

function parser.parse_from_file (filename)
  local subject = getcontents(filename)
  local errorinfo = { subject = subject, filename = filename }
  lpeg.setmaxstack(1000)
  return lpeg.match(G, subject, nil, errorinfo)
end

function parser.parse (subject, filename)
  local errorinfo = { subject = subject, filename = filename }
  lpeg.setmaxstack(1000)
  return lpeg.match(G, subject, nil, errorinfo)
end

return parser
