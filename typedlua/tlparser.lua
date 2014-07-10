--[[
This module implements Typed Lua parser
]]

local tlparser = {}

local lpeg = require "lpeg"
lpeg.locale(lpeg)

local tlast = require "typedlua.tlast"
local tltype = require "typedlua.tltype"

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

local function chainl1 (pat, sep)
  return lpeg.Cf(pat * lpeg.Cg(sep * pat)^0, tlast.exprBinaryOp)
end

local G = { lpeg.V("TypedLua"),
  TypedLua = Shebang^-1 * Skip * lpeg.V("Chunk") * -1 + report_error();
  -- type language
  Type = lpeg.V("NilableType");
  NilableType = lpeg.V("UnionType") * (symb("?") * lpeg.Cc(true))^-1 /
                tltype.UnionNil;
  UnionType = lpeg.V("PrimaryType") * (lpeg.Cg(symb("|") * lpeg.V("PrimaryType"))^0) /
              tltype.Union;
  PrimaryType = lpeg.V("LiteralType") +
                lpeg.V("BaseType") +
                lpeg.V("NilType") +
                lpeg.V("ValueType") +
                lpeg.V("AnyType") +
                lpeg.V("SelfType") +
                lpeg.V("FunctionType") +
                lpeg.V("TableType") +
                lpeg.V("VariableType");
  LiteralType = ((token("false", "Type") * lpeg.Cc(false)) +
                (token("true", "Type") * lpeg.Cc(true)) +
                token(Number, "Type") +
                token(String, "Type")) /
                tltype.Literal;
  BaseType = token("boolean", "Type") / tltype.Boolean +
             token("number", "Type") / tltype.Number +
             token("string", "Type") / tltype.String;
  NilType = token("nil", "Type") / tltype.Nil;
  ValueType = token("value", "Type") / tltype.Value;
  AnyType = token("any", "Type") / tltype.Any;
  SelfType = token("self", "Type") / tltype.Self;
  FunctionType = lpeg.V("InputType") * symb("->") * lpeg.V("NilableTuple") /
                 tltype.Function;
  MethodType = lpeg.V("InputType") * symb("=>") * lpeg.V("NilableTuple") *
               lpeg.Cc(true) / tltype.Function;
  InputType = symb("(") * (lpeg.V("TupleType") + lpeg.Cc(nil)) * symb(")") *
              lpeg.Carg(2) /
              tltype.inputTuple;
  NilableTuple = lpeg.V("UnionlistType") * (symb("?") * lpeg.Carg(2))^-1 /
                 tltype.UnionlistNil;
  UnionlistType = lpeg.V("OutputType") * (lpeg.Cg(symb("|") * lpeg.V("OutputType"))^0) /
                  tltype.Unionlist;
  OutputType = symb("(") * (lpeg.V("TupleType") + lpeg.Cc(nil)) * symb(")") *
               lpeg.Carg(2) /
               tltype.outputTuple;
  TupleType = lpeg.Ct(lpeg.V("Type") * (symb(",") * lpeg.V("Type"))^0) *
              (symb("*") * lpeg.Cc(true))^-1 /
              tltype.Tuple;
  TableType = symb("{") *
              ((lpeg.V("FieldType") * (symb(",") * lpeg.V("FieldType"))^0) +
              (lpeg.Cc(false) * lpeg.Cc(tltype.Number()) * lpeg.V("Type")) / tltype.Field +
              lpeg.Cc(nil)) *
              symb("}") /
              tltype.Table;
  FieldType = ((kw("const") * lpeg.Cc(true)) + lpeg.Cc(false)) *
              lpeg.V("KeyType") * symb(":") * lpeg.V("Type") / tltype.Field;
  KeyType = lpeg.V("LiteralType") +
	    lpeg.V("BaseType") +
            lpeg.V("ValueType") +
            lpeg.V("AnyType");
  VariableType = token(Name, "Type") / tltype.Variable;
  RetType = lpeg.V("NilableTuple") +
            lpeg.V("Type") * lpeg.Carg(2) / tltype.retType;
  -- parser
  Chunk = lpeg.V("Block");
  StatList = (symb(";") + lpeg.V("Stat"))^0;
  Var = lpeg.V("Id");
  Id = lpeg.Cp() * token(Name, "Name") / tlast.ident;
  TypedId = lpeg.Cp() * token(Name, "Name") * (symb(":") * lpeg.V("Type"))^-1 / tlast.ident;
  FunctionDef = kw("function") * lpeg.V("FuncBody");
  FieldSep = symb(",") + symb(";");
  Field = lpeg.Cp() * kw("const") *
          ((symb("[") * lpeg.V("Expr") * symb("]")) +
           (lpeg.Cp() * token(Name, "Name") / tlast.exprString)) *
          symb("=") * lpeg.V("Expr") / tlast.fieldConst +
          lpeg.Cp() *
          ((symb("[") * lpeg.V("Expr") * symb("]")) +
           (lpeg.Cp() * token(Name, "Name") / tlast.exprString)) *
          symb("=") * lpeg.V("Expr") / tlast.fieldPair +
          lpeg.V("Expr");
  FieldList = (lpeg.V("Field") * (lpeg.V("FieldSep") * lpeg.V("Field"))^0 *
              lpeg.V("FieldSep")^-1)^-1;
  Constructor = lpeg.Cp() * symb("{") * lpeg.V("FieldList") * symb("}") / tlast.exprTable;
  NameList = lpeg.Cp() * lpeg.V("TypedId") * (symb(",") * lpeg.V("TypedId"))^0 /
             tlast.namelist;
  ExpList = lpeg.Cp() * lpeg.V("Expr") * (symb(",") * lpeg.V("Expr"))^0 /
            tlast.explist;
  FuncArgs = symb("(") *
             (lpeg.V("Expr") * (symb(",") * lpeg.V("Expr"))^0)^-1 *
             symb(")") +
             lpeg.V("Constructor") +
             lpeg.Cp() * token(String, "String") / tlast.exprString;
  Expr = lpeg.V("SubExpr_1");
  SubExpr_1 = chainl1(lpeg.V("SubExpr_2"), OrOp);
  SubExpr_2 = chainl1(lpeg.V("SubExpr_3"), AndOp);
  SubExpr_3 = chainl1(lpeg.V("SubExpr_4"), RelOp);
  SubExpr_4 = lpeg.V("SubExpr_5") * ConOp * lpeg.V("SubExpr_4") / tlast.exprBinaryOp +
              lpeg.V("SubExpr_5");
  SubExpr_5 = chainl1(lpeg.V("SubExpr_6"), AddOp);
  SubExpr_6 = chainl1(lpeg.V("SubExpr_7"), MulOp);
  SubExpr_7 = UnOp * lpeg.V("SubExpr_7") / tlast.exprUnaryOp +
              lpeg.V("SubExpr_8");
  SubExpr_8 = lpeg.V("SimpleExp") * (PowOp * lpeg.V("SubExpr_7"))^-1 / tlast.exprBinaryOp;
  SimpleExp = lpeg.Cp() * token(Number, "Number") / tlast.exprNumber +
              lpeg.Cp() * token(String, "String") / tlast.exprString +
              lpeg.Cp() * kw("nil") / tlast.exprNil +
              lpeg.Cp() * kw("false") / tlast.exprFalse +
              lpeg.Cp() * kw("true") / tlast.exprTrue +
              lpeg.Cp() * symb("...") / tlast.exprDots +
              lpeg.V("FunctionDef") +
              lpeg.V("Constructor") +
              lpeg.V("SuffixedExp");
  SuffixedExp = lpeg.Cf(lpeg.V("PrimaryExp") * (
                (lpeg.Cp() * symb(".") *
                  (lpeg.Cp() * token(Name, "Name") / tlast.exprString)) / tlast.exprIndex +
                (lpeg.Cp() * symb("[") * lpeg.V("Expr") * symb("]")) / tlast.exprIndex +
                (lpeg.Cp() * lpeg.Cg(symb(":") *
                   (lpeg.Cp() * token(Name, "Name") / tlast.exprString) *
                   lpeg.V("FuncArgs"))) / tlast.invoke +
                (lpeg.Cp() * lpeg.V("FuncArgs")) / tlast.call)^0, tlast.exprSuffixed);
  PrimaryExp = lpeg.V("Var") +
               lpeg.Cp() * symb("(") * lpeg.V("Expr") * symb(")") / tlast.exprParen;
  Block = lpeg.Cp() * lpeg.V("StatList") * lpeg.V("RetStat")^-1 / tlast.block;
  IfStat = lpeg.Cp() * kw("if") * lpeg.V("Expr") * kw("then") * lpeg.V("Block") *
           (kw("elseif") * lpeg.V("Expr") * kw("then") * lpeg.V("Block"))^0 *
           (kw("else") * lpeg.V("Block"))^-1 *
           kw("end") / tlast.statIf;
  WhileStat = lpeg.Cp() * kw("while") * lpeg.V("Expr") *
              kw("do") * lpeg.V("Block") * kw("end") / tlast.statWhile;
  DoStat = kw("do") * lpeg.V("Block") * kw("end") / tlast.statDo;
  ForBody = kw("do") * lpeg.V("Block");
  ForNum = lpeg.Cp() *
           lpeg.V("Id") * symb("=") * lpeg.V("Expr") * symb(",") *
           lpeg.V("Expr") * (symb(",") * lpeg.V("Expr"))^-1 *
           lpeg.V("ForBody") / tlast.statFornum;
  ForGen = lpeg.Cp() * lpeg.V("NameList") * kw("in") *
           lpeg.V("ExpList") * lpeg.V("ForBody") / tlast.statForin;
  ForStat = kw("for") * (lpeg.V("ForNum") + lpeg.V("ForGen")) * kw("end");
  RepeatStat = lpeg.Cp() * kw("repeat") * lpeg.V("Block") *
               kw("until") * lpeg.V("Expr") / tlast.statRepeat;
  FuncName = lpeg.Cf(lpeg.V("Id") * (symb(".") *
             (lpeg.Cp() * token(Name, "Name") / tlast.exprString))^0, tlast.funcName) *
             (symb(":") * (lpeg.Cp() * token(Name, "Name") / tlast.exprString) *
               lpeg.Cc(true))^-1 /
             tlast.funcName;
  ParList = lpeg.Cp() * lpeg.V("NameList") * (symb(",") * lpeg.V("TypedVarArg"))^-1 /
            tlast.parList2 +
            lpeg.Cp() * lpeg.V("TypedVarArg") / tlast.parList1 +
            lpeg.Cp() / tlast.parList0;
  TypedVarArg = lpeg.Cp() * symb("...") * (symb(":") * lpeg.V("Type"))^-1 /
                tlast.identDots;
  FuncBody = lpeg.Cp() * symb("(") * lpeg.V("ParList") * symb(")") *
             (symb(":") * lpeg.V("RetType"))^-1 *
             lpeg.V("Block") * kw("end") / tlast.exprFunction;
  FuncStat = lpeg.Cp() * kw("function") * lpeg.V("FuncName") * lpeg.V("FuncBody") /
             tlast.statFuncSet;
  LocalFunc = lpeg.Cp() * kw("function") *
              lpeg.V("Id") * lpeg.V("FuncBody") / tlast.statLocalrec;
  LocalAssign = lpeg.Cp() * lpeg.V("NameList") *
                ((symb("=") * lpeg.V("ExpList")) + lpeg.Ct(lpeg.Cc())) / tlast.statLocal;
  LocalStat = kw("local") *
              (lpeg.V("LocalInterface") + lpeg.V("LocalFunc") + lpeg.V("LocalAssign"));
  LabelStat = lpeg.Cp() * symb("::") * token(Name, "Name") * symb("::") / tlast.statLabel;
  BreakStat = lpeg.Cp() * kw("break") / tlast.statBreak;
  GoToStat = lpeg.Cp() * kw("goto") * token(Name, "Name") / tlast.statGoto;
  RetStat = lpeg.Cp() * kw("return") *
            (lpeg.V("Expr") * (symb(",") * lpeg.V("Expr"))^0)^-1 *
            symb(";")^-1 / tlast.statReturn;
  InterfaceStat = lpeg.Cp() * kw("interface") * token(Name, "Name") *
                  lpeg.V("InterfaceDec")^1 * kw("end") / tlast.statInterface;
  LocalInterface = lpeg.Cp() * kw("interface") * token(Name, "Name") *
                   lpeg.V("InterfaceDec")^1 * kw("end") / tlast.statLocalInterface;
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
  IdList = lpeg.Cp() * lpeg.V("Id") * (symb(",") * lpeg.V("Id"))^0 / tlast.namelist;
  ExprStat = lpeg.Cmt(
             (lpeg.V("SuffixedExp") * (lpeg.Cc(tlast.statSet) * lpeg.V("Assignment"))) +
             (lpeg.V("SuffixedExp") * (lpeg.Cc(tlast.statApply))),
             function (s, i, s1, f, ...) return f(s1, ...) end);
  Assignment = ((symb(",") * lpeg.V("SuffixedExp"))^1)^-1 * symb("=") * lpeg.V("ExpList");
  ConstStat = kw("const") * (lpeg.V("ConstFunc") + lpeg.V("ConstAssignment"));
  ConstFunc = lpeg.Cp() * kw("function") * lpeg.V("FuncName") * lpeg.V("FuncBody") /
              tlast.statConstFuncSet;
  ConstAssignment = lpeg.Cmt(lpeg.V("SuffixedExp") * lpeg.Cc(tlast.statConstSet) *
                    symb("=") * lpeg.V("Expr"),
                    function (s, i, s1, f, e1) return f(s1, e1) end);
  Stat = lpeg.V("IfStat") + lpeg.V("WhileStat") + lpeg.V("DoStat") + lpeg.V("ForStat") +
         lpeg.V("RepeatStat") + lpeg.V("FuncStat") + lpeg.V("LocalStat") +
         lpeg.V("LabelStat") + lpeg.V("BreakStat") + lpeg.V("GoToStat") +
         lpeg.V("InterfaceStat") + lpeg.V("ExprStat") + lpeg.V("ConstStat");
}

function tlparser.parse (subject, filename, strict)
  local errorinfo = { subject = subject, filename = filename }
  lpeg.setmaxstack(1000)
  local ast, error_msg = lpeg.match(G, subject, nil, errorinfo, strict)
  if not ast then return ast, error_msg end
  return ast
end

return tlparser
