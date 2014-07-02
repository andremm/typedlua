--[[
This module implements Typed Lua parser
]]

local tlparser = {}

local lpeg = require "lpeg"
lpeg.locale(lpeg)

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
  return lpeg.Cmt(lpeg.Carg(1) * lpeg.Cc(name), setffp)
end

-- lexer rules

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

local Space = lpeg.space^1;

local Equals = lpeg.P("=")^0;
local Open = "[" * lpeg.Cg(Equals, "init") * "[" * lpeg.P("\n")^-1;
local Close = "]" * lpeg.C(Equals) * "]";
local CloseEQ = lpeg.Cmt(Close * lpeg.Cb("init"),
                         function (s, i, a, b) return a == b end);

local LongString = Open * lpeg.C((lpeg.P(1) - CloseEQ)^0) * Close /
                   function (s, o) return s end;

local Comment = lpeg.P("--") * LongString /
                function () return end +
                lpeg.P("--") * (lpeg.P(1) - lpeg.P("\n"))^0;

local Skip = (Space + Comment)^0;

local idStart = lpeg.alpha + lpeg.P("_");
local idRest = lpeg.alnum + lpeg.P("_");

local Keywords = lpeg.P("and") + "break" + "do" + "elseif" + "else" + "end" +
                 "false" + "for" + "function" + "goto" + "if" + "in" +
                 "local" + "nil" + "not" + "or" + "repeat" + "return" +
                 "then" + "true" + "until" + "while";

local Reserved = Keywords * -idRest;
local Identifier = idStart * idRest^0;
local Name = -Reserved * lpeg.C(Identifier) * -idRest;

local function token (pat, name)
  return pat * Skip + updateffp(name) * lpeg.P(false)
end

local function symb (str)
  return token(lpeg.P(str), str)
end

local function kw (str)
  return token(lpeg.P(str) * -idRest, str)
end

local Hex = (lpeg.P("0x") + lpeg.P("0X")) * lpeg.xdigit^1;
local Expo = lpeg.S("eE") * lpeg.S("+-")^-1 * lpeg.digit^1;
local Float = (((lpeg.digit^1 * lpeg.P(".") * lpeg.digit^0) +
              (lpeg.P(".") * lpeg.digit^1)) * Expo^-1) +
              (lpeg.digit^1 * Expo);
local Int = lpeg.digit^1;

local Number = lpeg.C(Hex + Float + Int) /
               function (n) return tonumber(n) end;

local ShortString = lpeg.P('"') *
                    lpeg.C(((lpeg.P('\\') * lpeg.P(1)) + (lpeg.P(1) - lpeg.P('"')))^0) *
                    lpeg.P('"') +
                    lpeg.P("'") *
                    lpeg.C(((lpeg.P("\\") * lpeg.P(1)) + (lpeg.P(1) - lpeg.P("'")))^0) *
                    lpeg.P("'");

local String = LongString + (ShortString / function (s) return fix_str(s) end);

local OrOp = kw("or") / "or";
local AndOp = kw("and") / "and";
local RelOp = symb("~=") / "ne" +
              symb("==") / "eq" +
              symb("<=") / "le" +
              symb(">=") / "ge" +
              symb("<") / "lt" +
              symb(">") / "gt";
local ConOp = symb("..") / "concat";
local AddOp = symb("+") / "add" +
              symb("-") / "sub";
local MulOp = symb("*") / "mul" +
              symb("/") / "div" +
              symb("%") / "mod";
local UnOp = kw("not") / "not" +
             symb("-") / "unm" +
             symb("#") / "len";
local PowOp = symb("^") / "pow";
local Shebang = lpeg.P("#") * (lpeg.P(1) - lpeg.P("\n"))^0 * lpeg.P("\n");

-- for error reporting
local OneWord = Name + Number + String + Reserved + lpeg.P("...") + lpeg.P(1);

-- parser rules

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

local function syntaxerror (errorinfo, pos, msg)
  local l, c = lineno(errorinfo.subject, pos)
  local error_msg = "%s:%d:%d: syntax error, %s"
  return string.format(error_msg, errorinfo.filename, l, c, msg)
end

local function getffp (s, i, t)
  return t.ffp or i, t
end

local function geterrorinfo ()
  return lpeg.Cmt(lpeg.Carg(1), getffp) * (lpeg.C(OneWord) + lpeg.Cc("EOF")) /
  function (t, u)
    t.unexpected = u
    return t
  end
end

local function errormsg ()
  return geterrorinfo() /
  function (t)
    local p = t.ffp or 1
    local msg = "unexpected '%s', expecting %s"
    msg = string.format(msg, t.unexpected, t.expected)
    return nil, syntaxerror(t, p, msg)
  end
end

local function report_error ()
  return errormsg()
end

local function taggedCap (tag, pat)
  return lpeg.Ct(lpeg.Cg(lpeg.Cp(), "pos") * lpeg.Cg(lpeg.Cc(tag), "tag") * pat)
end

local function sepby1 (pat, sep, tag)
  return taggedCap(tag, pat * (sep * pat)^0)
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

local function chainl1 (pat, sep)
  return lpeg.Cf(pat * lpeg.Cg(sep * pat)^0, binaryop)
end

local G = { lpeg.V("TypedLua"),
  TypedLua = Shebang^-1 * Skip * lpeg.V("Chunk") * -1 + report_error();
  -- type language
  Type = lpeg.V("NilableType");
  NilableType = lpeg.V("UnionType") * (symb("?") * taggedCap("Nil", lpeg.P(true)))^-1 /
                function (t, n)
                  if n then t[#t + 1] = n end
                  if #t > 1 then
                    return t
                  else
                    return t[1]
                  end
                end;
  UnionType = taggedCap("Union",
              lpeg.V("PrimaryType") * lpeg.Cg(symb("|") * lpeg.V("PrimaryType"))^0);
  PrimaryType = lpeg.V("LiteralType") +
                lpeg.V("BaseType") +
                lpeg.V("NilType") +
                lpeg.V("TopType") +
                lpeg.V("DynamicType") +
                lpeg.V("SelfType") +
                lpeg.V("FunctionType") +
                lpeg.V("TableType") +
                lpeg.V("VariableType");
  LiteralType = taggedCap("Literal",
                lpeg.V("LiteralFalse") +
                lpeg.V("LiteralTrue") +
                lpeg.V("LiteralNumber") +
                lpeg.V("LiteralString"));
  LiteralFalse = token("false", "Type") * lpeg.Cc(false);
  LiteralTrue = token("true", "Type") * lpeg.Cc(true);
  LiteralNumber = token(Number, "Type");
  LiteralString = token(String, "Type");
  BaseType = taggedCap("Base",
             lpeg.V("BaseBoolean") + lpeg.V("BaseNumber") + lpeg.V("BaseString"));
  BaseBoolean = token("boolean", "Type") * lpeg.Cc("boolean");
  BaseNumber = token("number", "Type") * lpeg.Cc("number");
  BaseString = token("string", "Type") * lpeg.Cc("string");
  NilType = taggedCap("Nil", token("nil", "Type"));
  TopType = taggedCap("Value", token("value", "Type"));
  DynamicType = taggedCap("Any", token("any", "Type"));
  SelfType = taggedCap("Self", token("self", "Type"));
  FunctionType = taggedCap("Function",
                 lpeg.V("ArgTypeList") * symb("->") * lpeg.V("NilableRetTypeList"));
  MethodType = taggedCap("Function",
               lpeg.V("ArgTypeList") * symb("=>") * lpeg.V("NilableRetTypeList")) /
               function (t)
                 table.insert(t[1], 1, { tag = "Self" })
                 return t
               end;
  ArgTypeList = symb("(") * (lpeg.V("TypeList") + lpeg.V("ValueStar")) * symb(")") /
                function (t)
                  if t[#t].tag ~= "Vararg" then
                    t[#t + 1] = { tag = "Vararg", [1] = { tag = "Value" } }
                  end
                  return t
                end;
  NilableRetTypeList = lpeg.V("UnionRetTypeList") * (symb("?") * lpeg.V("RetError"))^-1 /
                       function (t, n)
                         if n then t[#t + 1] = n end
                         if #t > 1 then
                           return t
                         else
                           return t[1]
                         end
                       end;
  UnionRetTypeList = taggedCap("Unionlist",
                     lpeg.V("RetTypeList") * lpeg.Cg(symb("|") * lpeg.V("RetTypeList"))^0);
  RetTypeList = symb("(") * (lpeg.V("TypeList") + lpeg.V("NilStar")) * symb(")") /
                function (t)
                  if t[#t].tag ~= "Vararg" then
                    t[#t + 1] = { tag = "Vararg", [1] = { tag = "Nil" } }
                  end
                  return t
                end;
  TypeList = sepby1(lpeg.V("Type"), symb(","), "Tuple") * taggedCap("Vararg", symb("*"))^-1 /
             function (t, v)
               if v then
                 v[1] = t[#t]
                 t[#t] = v
               end
               return t
             end;
  ValueStar = taggedCap("Tuple", taggedCap("Vararg", taggedCap("Value", lpeg.P(true))));
  NilStar = taggedCap("Tuple", taggedCap("Vararg", taggedCap("Nil", lpeg.P(true))));
  RetError = taggedCap("Tuple", taggedCap("Nil", lpeg.P(true)) *
                                taggedCap("Base", lpeg.Cc("string")) *
                                taggedCap("Vararg", taggedCap("Nil", lpeg.P(true))));
  TableType = taggedCap("Table", symb("{") * (lpeg.V("TableTypeBody") + lpeg.Cc(nil)) * symb("}"));
  TableTypeBody = lpeg.V("FieldTypeList") +
                  taggedCap("Field", taggedCap("Base", lpeg.Cc("number")) * lpeg.V("Type"));
  FieldTypeList = lpeg.V("FieldType") * (symb(",") * lpeg.V("FieldType"))^0;
  FieldType = taggedCap("Const", kw("const") * lpeg.V("KeyType") * symb(":") * lpeg.V("Type")) +
              taggedCap("Field", lpeg.V("KeyType") * symb(":") * lpeg.V("Type"));
  KeyType = lpeg.V("LiteralType") +
	    lpeg.V("BaseType") +
            lpeg.V("TopType") +
            lpeg.V("DynamicType");
  VariableType = taggedCap("Variable", token(Name, "Type"));
  RetType = lpeg.V("NilableRetTypeList") +
            taggedCap("Tuple", lpeg.V("Type") * taggedCap("Vararg", taggedCap("Nil", lpeg.P(true))));
  -- parser
  Chunk = lpeg.V("Block");
  StatList = (symb(";") + lpeg.V("Stat"))^0;
  Var = lpeg.V("Id");
  Id = taggedCap("Id", token(Name, "Name"));
  TypedId = taggedCap("Id", token(Name, "Name") * (symb(":") * lpeg.V("Type"))^-1);
  FunctionDef = kw("function") * lpeg.V("FuncBody");
  FieldSep = symb(",") + symb(";");
  Field = taggedCap("Const", kw("const") * ((symb("[") * lpeg.V("Expr") * symb("]") * symb("=") * lpeg.V("Expr")) +
                    (taggedCap("String", token(Name, "Name")) * symb("=") * lpeg.V("Expr")))) +
          taggedCap("Pair", (symb("[") * lpeg.V("Expr") * symb("]") * symb("=") * lpeg.V("Expr")) +
                    (taggedCap("String", token(Name, "Name")) * symb("=") * lpeg.V("Expr"))) +
          lpeg.V("Expr");
  FieldList = (lpeg.V("Field") * (lpeg.V("FieldSep") * lpeg.V("Field"))^0 * lpeg.V("FieldSep")^-1)^-1;
  Constructor = taggedCap("Table", symb("{") * lpeg.V("FieldList") * symb("}"));
  NameList = sepby1(lpeg.V("TypedId"), symb(","), "NameList");
  ExpList = sepby1(lpeg.V("Expr"), symb(","), "ExpList");
  FuncArgs = symb("(") * (lpeg.V("Expr") * (symb(",") * lpeg.V("Expr"))^0)^-1 * symb(")") +
             lpeg.V("Constructor") +
             taggedCap("String", token(String, "String"));
  Expr = lpeg.V("SubExpr_1");
  SubExpr_1 = chainl1(lpeg.V("SubExpr_2"), OrOp);
  SubExpr_2 = chainl1(lpeg.V("SubExpr_3"), AndOp);
  SubExpr_3 = chainl1(lpeg.V("SubExpr_4"), RelOp);
  SubExpr_4 = lpeg.V("SubExpr_5") * ConOp * lpeg.V("SubExpr_4") / binaryop +
              lpeg.V("SubExpr_5");
  SubExpr_5 = chainl1(lpeg.V("SubExpr_6"), AddOp);
  SubExpr_6 = chainl1(lpeg.V("SubExpr_7"), MulOp);
  SubExpr_7 = UnOp * lpeg.V("SubExpr_7") / unaryop +
              lpeg.V("SubExpr_8");
  SubExpr_8 = lpeg.V("SimpleExp") * (PowOp * lpeg.V("SubExpr_7"))^-1 / binaryop;
  SimpleExp = taggedCap("Number", token(Number, "Number")) +
              taggedCap("String", token(String, "String")) +
              taggedCap("Nil", kw("nil")) +
              taggedCap("False", kw("false")) +
              taggedCap("True", kw("true")) +
              taggedCap("Dots", symb("...")) +
              lpeg.V("FunctionDef") +
              lpeg.V("Constructor") +
              lpeg.V("SuffixedExp");
  SuffixedExp = lpeg.Cf(lpeg.V("PrimaryExp") * (
                  taggedCap("DotIndex", symb(".") * taggedCap("String", token(Name, "Name"))) +
                  taggedCap("ArrayIndex", symb("[") * lpeg.V("Expr") * symb("]")) +
                  taggedCap("Invoke", lpeg.Cg(symb(":") * taggedCap("String", token(Name, "Name")) * lpeg.V("FuncArgs"))) +
                  taggedCap("Call", lpeg.V("FuncArgs"))
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
  PrimaryExp = lpeg.V("Var") +
               taggedCap("Paren", symb("(") * lpeg.V("Expr") * symb(")"));
  Block = taggedCap("Block", lpeg.V("StatList") * lpeg.V("RetStat")^-1);
  IfStat = taggedCap("If",
             kw("if") * lpeg.V("Expr") * kw("then") * lpeg.V("Block") *
             (kw("elseif") * lpeg.V("Expr") * kw("then") * lpeg.V("Block"))^0 *
             (kw("else") * lpeg.V("Block"))^-1 *
             kw("end"));
  WhileStat = taggedCap("While", kw("while") * lpeg.V("Expr") *
                kw("do") * lpeg.V("Block") * kw("end"));
  DoStat = kw("do") * lpeg.V("Block") * kw("end") /
           function (t)
             t.tag = "Do"
             return t
           end;
  ForBody = kw("do") * lpeg.V("Block");
  ForNum = taggedCap("Fornum",
             lpeg.V("Id") * symb("=") * lpeg.V("Expr") * symb(",") *
             lpeg.V("Expr") * (symb(",") * lpeg.V("Expr"))^-1 *
             lpeg.V("ForBody"));
  ForGen = taggedCap("Forin", lpeg.V("NameList") * kw("in") * lpeg.V("ExpList") * lpeg.V("ForBody"));
  ForStat = kw("for") * (lpeg.V("ForNum") + lpeg.V("ForGen")) * kw("end");
  RepeatStat = taggedCap("Repeat", kw("repeat") * lpeg.V("Block") *
                 kw("until") * lpeg.V("Expr"));
  FuncName = lpeg.Cf(lpeg.V("Id") * (symb(".") * taggedCap("String", token(Name, "Name")))^0,
             function (t1, t2)
               if t2 then
                 return {tag = "Index", pos = t1.pos, [1] = t1, [2] = t2}
               end
               return t1
             end) * (symb(":") * taggedCap("String", token(Name, "Name")))^-1 /
             function (t1, t2)
               if t2 then
                 return {tag = "Index", pos = t1.pos, is_method = true, [1] = t1, [2] = t2}
               end
               return t1
             end;
  ParList = lpeg.V("NameList") * (symb(",") * lpeg.V("TypedVarArg"))^-1 /
            function (t, v)
              if v then table.insert(t, v) end
              return t
            end +
            lpeg.V("TypedVarArg") /
            function (v)
              return {v}
            end +
            lpeg.P(true) / function () return {} end;
  TypedVarArg = taggedCap("Dots", symb("...") * (symb(":") * lpeg.V("Type"))^-1);
  FuncBody = taggedCap("Function", symb("(") * lpeg.V("ParList") * symb(")") *
             (symb(":") * lpeg.V("RetType"))^-1 * lpeg.V("Block") * kw("end"));
  FuncStat = taggedCap("Set", kw("function") * lpeg.V("FuncName") * lpeg.V("FuncBody")) /
             function (t)
               if t[1].is_method then table.insert(t[2][1], 1, {tag = "Id", [1] = "self"}) end
               t[1] = {t[1]}
               t[2] = {t[2]}
               return t
             end;
  LocalFunc = taggedCap("Localrec", kw("function") * lpeg.V("Id") * lpeg.V("FuncBody")) /
              function (t)
                t[1] = {t[1]}
                t[2] = {t[2]}
                return t
              end;
  LocalAssign = taggedCap("Local", lpeg.V("NameList") * ((symb("=") * lpeg.V("ExpList")) + lpeg.Ct(lpeg.Cc())));
  LocalStat = kw("local") * (lpeg.V("LocalInterface") + lpeg.V("LocalFunc") + lpeg.V("LocalAssign"));
  LabelStat = taggedCap("Label", symb("::") * token(Name, "Name") * symb("::"));
  BreakStat = taggedCap("Break", kw("break"));
  GoToStat = taggedCap("Goto", kw("goto") * token(Name, "Name"));
  RetStat = taggedCap("Return", kw("return") * (lpeg.V("Expr") * (symb(",") * lpeg.V("Expr"))^0)^-1 * symb(";")^-1);
  InterfaceStat = taggedCap("Interface", kw("interface") * token(Name, "Name") *
                  lpeg.V("InterfaceDec")^1 * kw("end"));
  LocalInterface = taggedCap("LocalInterface", kw("interface") * token(Name, "Name") *
                   lpeg.V("InterfaceDec")^1 * kw("end"));
  InterfaceDec = ((kw("const") * lpeg.Cc("Const")) + lpeg.Cc("Field")) * lpeg.V("IdList") * symb(":") * (lpeg.V("Type") + lpeg.V("MethodType")) /
                  function (tag, idlist, t)
                    local l = {}
                    for k, v in ipairs(idlist) do
                      local f = { tag = tag, pos = v.pos }
                      f[1] = { tag = "Literal", [1] = v[1] }
                      f[2] = t
                      table.insert(l, f)
                    end
                    return table.unpack(l)
                  end;
  IdList = sepby1(lpeg.V("Id"), symb(","), "IdList");
  ExprStat = lpeg.Cmt(
             (lpeg.V("SuffixedExp") *
                (lpeg.Cc(function (...)
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
                         end) * lpeg.V("Assignment")))
             +
             (lpeg.V("SuffixedExp") *
                (lpeg.Cc(function (s)
                           if s.tag == "Call" or
                              s.tag == "Invoke" then
                             return true, s
                           end
                           -- invalid statement
                           return false
                         end)))
             , function (s, i, s1, f, ...) return f(s1, ...) end);
  Assignment = ((symb(",") * lpeg.V("SuffixedExp"))^1)^-1 * symb("=") * lpeg.V("ExpList");
  ConstStat = kw("const") * (lpeg.V("ConstFunc") + lpeg.V("ConstAssignment"));
  ConstFunc = taggedCap("ConstSet", kw("function") * lpeg.V("FuncName") * lpeg.V("FuncBody")) /
              function (t)
                if t[1].is_method then table.insert(t[2][1], 1, {tag = "Id", [1] = "self"}) end
                return t
              end;
  ConstAssignment = lpeg.Cmt(lpeg.V("SuffixedExp") * symb("=") * lpeg.V("Expr"),
                    function (s, i, se, e)
                      if se.tag == "Id" or se.tag == "Index" then
                        return true, { tag = "ConstSet", pos = se.pos, [1] = se, [2] = e }
                      else
                        -- invalid assignment
                        return false
                      end
                    end);
  Stat = lpeg.V("IfStat") + lpeg.V("WhileStat") + lpeg.V("DoStat") + lpeg.V("ForStat") +
         lpeg.V("RepeatStat") + lpeg.V("FuncStat") + lpeg.V("LocalStat") +
         lpeg.V("LabelStat") + lpeg.V("BreakStat") + lpeg.V("GoToStat") +
         lpeg.V("InterfaceStat") + lpeg.V("ExprStat") + lpeg.V("ConstStat");
}

function tlparser.parse (subject, filename)
  local errorinfo = { subject = subject, filename = filename }
  lpeg.setmaxstack(1000)
  local ast, error_msg = lpeg.match(G, subject, nil, errorinfo)
  if not ast then return ast, error_msg end
  return ast
end

return tlparser
