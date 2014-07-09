--[[
This module implements Typed Lua parser
]]

local tlparser = {}

local lpeg = require "lpeg"
lpeg.locale(lpeg)

local ast = require "typedlua.tlast"

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
  return lpeg.Cf(pat * lpeg.Cg(sep * pat)^0, ast.exprBinaryOp)
end

local G = { lpeg.V("TypedLua"),
  TypedLua = Shebang^-1 * Skip * lpeg.V("Chunk") * -1 + report_error();
  -- type language
  Type = lpeg.V("NilableType");
  NilableType = lpeg.V("UnionType") * (symb("?") * lpeg.Cc(true))^-1 / ast.typeUnionNil;
  UnionType = lpeg.Cp() * lpeg.V("PrimaryType") *
              (lpeg.Cg(symb("|") * lpeg.V("PrimaryType"))^0) / ast.typeUnion;
  PrimaryType = lpeg.V("LiteralType") +
                lpeg.V("BaseType") +
                lpeg.V("NilType") +
                lpeg.V("ValueType") +
                lpeg.V("AnyType") +
                lpeg.V("SelfType") +
                lpeg.V("FunctionType") +
                lpeg.V("TableType") +
                lpeg.V("VariableType");
  LiteralType = lpeg.Cp() * token("false", "Type") / ast.typeFalse +
                lpeg.Cp() * token("true", "Type") / ast.typeTrue +
                lpeg.Cp() * token(Number, "Type") / ast.typeNum +
                lpeg.Cp() * token(String, "Type") / ast.typeStr;
  BaseType = lpeg.Cp() * token("boolean", "Type") / ast.typeBoolean +
             lpeg.Cp() * token("number", "Type") / ast.typeNumber +
             lpeg.Cp() * token("string", "Type") / ast.typeString;
  NilType = lpeg.Cp() * token("nil", "Type") / ast.typeNil;
  ValueType = lpeg.Cp() * token("value", "Type") / ast.typeValue;
  AnyType = lpeg.Cp() * token("any", "Type") / ast.typeAny;
  SelfType = lpeg.Cp() * token("self", "Type") / ast.typeSelf;
  FunctionType = lpeg.Cp() * lpeg.V("ArgTypeList") * symb("->") *
                 lpeg.V("NilableRetTypeList") / ast.typeFunction;
  MethodType = lpeg.Cp() * lpeg.V("ArgTypeList") * symb("=>") *
               lpeg.V("NilableRetTypeList") * lpeg.Cc(true) / ast.typeFunction;
  ArgTypeList = lpeg.Cp() * symb("(") * (lpeg.V("TypeList") + lpeg.Cc(nil)) * symb(")") /
                ast.typeArgList;
  NilableRetTypeList = lpeg.V("UnionRetTypeList") * (symb("?") * lpeg.Cc(true))^-1 /
                       ast.typeUnionlistNil;
  UnionRetTypeList = lpeg.Cp() * lpeg.V("RetTypeList") *
                     (lpeg.Cg(symb("|") * lpeg.V("RetTypeList"))^0) / ast.typeUnionlist;
  RetTypeList = lpeg.Cp() * symb("(") * (lpeg.V("TypeList") + lpeg.Cc(nil)) * symb(")") /
                ast.typeRetList;
  TypeList = lpeg.Cp() * lpeg.Ct(lpeg.V("Type") * (symb(",") * lpeg.V("Type"))^0) *
             (symb("*") * lpeg.Cc(true))^-1 / ast.typelist;
  TableType = lpeg.Cp() *
              symb("{") * (lpeg.V("TableTypeBody") + lpeg.Cc(nil)) * symb("}") /
              ast.typeTable;
  TableTypeBody = lpeg.V("FieldTypeList") +
                  lpeg.Cp() * lpeg.V("Type") / ast.typeArrayField;
  FieldTypeList = lpeg.V("FieldType") * (symb(",") * lpeg.V("FieldType"))^0;
  FieldType = lpeg.Cp() * kw("const") *
              lpeg.V("KeyType") * symb(":") * lpeg.V("Type") / ast.typeConstField +
              lpeg.Cp() *
              lpeg.V("KeyType") * symb(":") * lpeg.V("Type") / ast.typeField;
  KeyType = lpeg.V("LiteralType") +
	    lpeg.V("BaseType") +
            lpeg.V("ValueType") +
            lpeg.V("AnyType");
  VariableType = lpeg.Cp() * token(Name, "Type") / ast.typeVariable;
  RetType = lpeg.V("NilableRetTypeList") +
            lpeg.V("Type") / ast.typeRetType;
  -- parser
  Chunk = lpeg.V("Block");
  StatList = (symb(";") + lpeg.V("Stat"))^0;
  Var = lpeg.V("Id");
  Id = lpeg.Cp() * token(Name, "Name") / ast.ident;
  TypedId = lpeg.Cp() * token(Name, "Name") * (symb(":") * lpeg.V("Type"))^-1 / ast.ident;
  FunctionDef = kw("function") * lpeg.V("FuncBody");
  FieldSep = symb(",") + symb(";");
  Field = lpeg.Cp() * kw("const") *
          ((symb("[") * lpeg.V("Expr") * symb("]")) +
           (lpeg.Cp() * token(Name, "Name") / ast.exprString)) *
          symb("=") * lpeg.V("Expr") / ast.fieldConst +
          lpeg.Cp() *
          ((symb("[") * lpeg.V("Expr") * symb("]")) +
           (lpeg.Cp() * token(Name, "Name") / ast.exprString)) *
          symb("=") * lpeg.V("Expr") / ast.fieldPair +
          lpeg.V("Expr");
  FieldList = (lpeg.V("Field") * (lpeg.V("FieldSep") * lpeg.V("Field"))^0 *
              lpeg.V("FieldSep")^-1)^-1;
  Constructor = lpeg.Cp() * symb("{") * lpeg.V("FieldList") * symb("}") / ast.exprTable;
  NameList = lpeg.Cp() * lpeg.V("TypedId") * (symb(",") * lpeg.V("TypedId"))^0 /
             ast.namelist;
  ExpList = lpeg.Cp() * lpeg.V("Expr") * (symb(",") * lpeg.V("Expr"))^0 /
            ast.explist;
  FuncArgs = symb("(") *
             (lpeg.V("Expr") * (symb(",") * lpeg.V("Expr"))^0)^-1 *
             symb(")") +
             lpeg.V("Constructor") +
             lpeg.Cp() * token(String, "String") / ast.exprString;
  Expr = lpeg.V("SubExpr_1");
  SubExpr_1 = chainl1(lpeg.V("SubExpr_2"), OrOp);
  SubExpr_2 = chainl1(lpeg.V("SubExpr_3"), AndOp);
  SubExpr_3 = chainl1(lpeg.V("SubExpr_4"), RelOp);
  SubExpr_4 = lpeg.V("SubExpr_5") * ConOp * lpeg.V("SubExpr_4") / ast.exprBinaryOp +
              lpeg.V("SubExpr_5");
  SubExpr_5 = chainl1(lpeg.V("SubExpr_6"), AddOp);
  SubExpr_6 = chainl1(lpeg.V("SubExpr_7"), MulOp);
  SubExpr_7 = UnOp * lpeg.V("SubExpr_7") / ast.exprUnaryOp +
              lpeg.V("SubExpr_8");
  SubExpr_8 = lpeg.V("SimpleExp") * (PowOp * lpeg.V("SubExpr_7"))^-1 / ast.exprBinaryOp;
  SimpleExp = lpeg.Cp() * token(Number, "Number") / ast.exprNumber +
              lpeg.Cp() * token(String, "String") / ast.exprString +
              lpeg.Cp() * kw("nil") / ast.exprNil +
              lpeg.Cp() * kw("false") / ast.exprFalse +
              lpeg.Cp() * kw("true") / ast.exprTrue +
              lpeg.Cp() * symb("...") / ast.exprDots +
              lpeg.V("FunctionDef") +
              lpeg.V("Constructor") +
              lpeg.V("SuffixedExp");
  SuffixedExp = lpeg.Cf(lpeg.V("PrimaryExp") * (
                (lpeg.Cp() * symb(".") *
                  (lpeg.Cp() * token(Name, "Name") / ast.exprString)) / ast.exprIndex +
                (lpeg.Cp() * symb("[") * lpeg.V("Expr") * symb("]")) / ast.exprIndex +
                (lpeg.Cp() * lpeg.Cg(symb(":") *
                   (lpeg.Cp() * token(Name, "Name") / ast.exprString) *
                   lpeg.V("FuncArgs"))) / ast.invoke +
                (lpeg.Cp() * lpeg.V("FuncArgs")) / ast.call)^0, ast.exprSuffixed);
  PrimaryExp = lpeg.V("Var") +
               lpeg.Cp() * symb("(") * lpeg.V("Expr") * symb(")") / ast.exprParen;
  Block = lpeg.Cp() * lpeg.V("StatList") * lpeg.V("RetStat")^-1 / ast.block;
  IfStat = lpeg.Cp() * kw("if") * lpeg.V("Expr") * kw("then") * lpeg.V("Block") *
           (kw("elseif") * lpeg.V("Expr") * kw("then") * lpeg.V("Block"))^0 *
           (kw("else") * lpeg.V("Block"))^-1 *
           kw("end") / ast.statIf;
  WhileStat = lpeg.Cp() * kw("while") * lpeg.V("Expr") *
              kw("do") * lpeg.V("Block") * kw("end") / ast.statWhile;
  DoStat = kw("do") * lpeg.V("Block") * kw("end") / ast.statDo;
  ForBody = kw("do") * lpeg.V("Block");
  ForNum = lpeg.Cp() *
           lpeg.V("Id") * symb("=") * lpeg.V("Expr") * symb(",") *
           lpeg.V("Expr") * (symb(",") * lpeg.V("Expr"))^-1 *
           lpeg.V("ForBody") / ast.statFornum;
  ForGen = lpeg.Cp() * lpeg.V("NameList") * kw("in") *
           lpeg.V("ExpList") * lpeg.V("ForBody") / ast.statForin;
  ForStat = kw("for") * (lpeg.V("ForNum") + lpeg.V("ForGen")) * kw("end");
  RepeatStat = lpeg.Cp() * kw("repeat") * lpeg.V("Block") *
               kw("until") * lpeg.V("Expr") / ast.statRepeat;
  FuncName = lpeg.Cf(lpeg.V("Id") * (symb(".") *
             (lpeg.Cp() * token(Name, "Name") / ast.exprString))^0, ast.funcName) *
             (symb(":") * (lpeg.Cp() * token(Name, "Name") / ast.exprString) *
               lpeg.Cc(true))^-1 /
             ast.funcName;
  ParList = lpeg.Cp() * lpeg.V("NameList") * (symb(",") * lpeg.V("TypedVarArg"))^-1 /
            ast.parList2 +
            lpeg.Cp() * lpeg.V("TypedVarArg") / ast.parList1 +
            lpeg.Cp() / ast.parList0;
  TypedVarArg = lpeg.Cp() * symb("...") * (symb(":") * lpeg.V("Type"))^-1 /
                ast.identDots;
  FuncBody = lpeg.Cp() * symb("(") * lpeg.V("ParList") * symb(")") *
             (symb(":") * lpeg.V("RetType"))^-1 *
             lpeg.V("Block") * kw("end") / ast.exprFunction;
  FuncStat = lpeg.Cp() * kw("function") * lpeg.V("FuncName") * lpeg.V("FuncBody") /
             ast.statFuncSet;
  LocalFunc = lpeg.Cp() * kw("function") *
              lpeg.V("Id") * lpeg.V("FuncBody") / ast.statLocalrec;
  LocalAssign = lpeg.Cp() * lpeg.V("NameList") *
                ((symb("=") * lpeg.V("ExpList")) + lpeg.Ct(lpeg.Cc())) / ast.statLocal;
  LocalStat = kw("local") *
              (lpeg.V("LocalInterface") + lpeg.V("LocalFunc") + lpeg.V("LocalAssign"));
  LabelStat = lpeg.Cp() * symb("::") * token(Name, "Name") * symb("::") / ast.statLabel;
  BreakStat = lpeg.Cp() * kw("break") / ast.statBreak;
  GoToStat = lpeg.Cp() * kw("goto") * token(Name, "Name") / ast.statGoto;
  RetStat = lpeg.Cp() * kw("return") *
            (lpeg.V("Expr") * (symb(",") * lpeg.V("Expr"))^0)^-1 *
            symb(";")^-1 / ast.statReturn;
  InterfaceStat = lpeg.Cp() * kw("interface") * token(Name, "Name") *
                  lpeg.V("InterfaceDec")^1 * kw("end") / ast.statInterface;
  LocalInterface = lpeg.Cp() * kw("interface") * token(Name, "Name") *
                   lpeg.V("InterfaceDec")^1 * kw("end") / ast.statLocalInterface;
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
  IdList = lpeg.Cp() * lpeg.V("Id") * (symb(",") * lpeg.V("Id"))^0 / ast.namelist;
  ExprStat = lpeg.Cmt(
             (lpeg.V("SuffixedExp") * (lpeg.Cc(ast.statSet) * lpeg.V("Assignment"))) +
             (lpeg.V("SuffixedExp") * (lpeg.Cc(ast.statApply))),
             function (s, i, s1, f, ...) return f(s1, ...) end);
  Assignment = ((symb(",") * lpeg.V("SuffixedExp"))^1)^-1 * symb("=") * lpeg.V("ExpList");
  ConstStat = kw("const") * (lpeg.V("ConstFunc") + lpeg.V("ConstAssignment"));
  ConstFunc = lpeg.Cp() * kw("function") * lpeg.V("FuncName") * lpeg.V("FuncBody") /
              ast.statConstFuncSet;
  ConstAssignment = lpeg.Cmt(lpeg.V("SuffixedExp") * lpeg.Cc(ast.statConstSet) *
                    symb("=") * lpeg.V("Expr"),
                    function (s, i, s1, f, e1) return f(s1, e1) end);
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
