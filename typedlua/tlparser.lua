--[[
This module implements Typed Lua parser
]]

local tlparser = {}

local lpeg = require "lpeg"
lpeg.locale(lpeg)

local tlast = require "typedlua.tlast"
local tlst = require "typedlua.tlst"
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

local function syntaxerror (subject, pos, filename, msg)
  local l, c = lineno(subject, pos)
  local error_msg = "%s:%d:%d: syntax error, %s"
  return string.format(error_msg, filename, l, c, msg)
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
    return nil, syntaxerror(t.subject, p, t.filename, msg)
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

local traverse_stm, traverse_exp, traverse_var
local traverse_block, traverse_explist, traverse_varlist, traverse_parlist

function traverse_parlist (env, parlist)
  local len = #parlist
  local is_vararg = false
  if len > 0 and parlist[len].tag == "Dots" then
    tlst.set_vararg(env)
  end
  return true
end

local function traverse_function (env, exp)
  tlst.begin_function(env)
  tlst.begin_scope(env)
  local status, msg = traverse_parlist(env, exp[1])
  if not status then return status, msg end
  if not exp[3] then
    status, msg = traverse_block(env, exp[2])
    if not status then return status, msg end
  else
    status, msg = traverse_block(env, exp[3])
    if not status then return status, msg end
  end
  tlst.end_scope(env)
  tlst.end_function(env)
  return true
end

local function traverse_op (env, exp)
  local status, msg = traverse_exp(env, exp[2])
  if not status then return status, msg end
  if exp[3] then
    status, msg = traverse_exp(env, exp[3])
    if not status then return status, msg end
  end
  return true
end

local function traverse_paren (env, exp)
  local status, msg = traverse_exp(env, exp[1])
  if not status then return status, msg end
  return true
end

local function traverse_table (env, fieldlist)
  for k, v in ipairs(fieldlist) do
    local tag = v.tag
    if tag == "Pair" or tag == "Const" then
      local status, msg = traverse_exp(env, v[1])
      if not status then return status, msg end
      status, msg = traverse_exp(env, v[2])
      if not status then return status, msg end
    else
      local status, msg = traverse_exp(env, v)
      if not status then return status, msg end
    end
  end
  return true
end

local function traverse_vararg (env, exp)
  if not tlst.is_vararg(env) then
    local msg = "cannot use '...' outside a vararg function"
    return nil, syntaxerror(env.subject, exp.pos, env.filename, msg)
  end
  return true
end

local function traverse_call (env, call)
  local status, msg = traverse_exp(env, call[1])
  if not status then return status, msg end
  for i=2, #call do
    status, msg = traverse_exp(env, call[i])
    if not status then return status, msg end
  end
  return true
end

local function traverse_invoke (env, invoke)
  local status, msg = traverse_exp(env, invoke[1])
  if not status then return status, msg end
  for i=3, #invoke do
    status, msg = traverse_exp(env, invoke[i])
    if not status then return status, msg end
  end
  return true
end

local function traverse_assignment (env, stm)
  local status, msg = traverse_varlist(env, stm[1])
  if not status then return status, msg end
  status, msg = traverse_explist(env, stm[2])
  if not status then return status, msg end
  return true
end

local function traverse_const_assignment (env, stm)
  local status, msg = traverse_var(env, stm[1])
  if not status then return status, msg end
  status, msg = traverse_exp(env, stm[2])
  if not status then return status, msg end
  return true
end

local function traverse_break (env, stm)
  if not tlst.insideloop(env) then
    local msg = "<break> not inside a loop"
    return nil, syntaxerror(env.subject, stm.pos, env.filename, msg)
  end
  return true
end

local function traverse_forin (env, stm)
  tlst.begin_loop(env)
  tlst.begin_scope(env)
  local status, msg = traverse_explist(env, stm[2])
  if not status then return status, msg end
  status, msg = traverse_block(env, stm[3])
  if not status then return status, msg end
  tlst.end_scope(env)
  tlst.end_loop(env)
  return true
end

local function traverse_fornum (env, stm)
  local status, msg
  tlst.begin_loop(env)
  tlst.begin_scope(env)
  status, msg = traverse_exp(env, stm[2])
  if not status then return status, msg end
  status, msg = traverse_exp(env, stm[3])
  if not status then return status, msg end
  if stm[5] then
    status, msg = traverse_exp(env, stm[4])
    if not status then return status, msg end
    status, msg = traverse_block(env, stm[5])
    if not status then return status, msg end
  else
    status, msg = traverse_block(env, stm[4])
    if not status then return status, msg end
  end
  tlst.end_scope(env)
  tlst.end_loop(env)
  return true
end

local function traverse_goto (env, stm)
  tlst.set_pending_goto(env, stm)
  return true
end

local function traverse_if (env, stm)
  local len = #stm
  if len % 2 == 0 then
    for i=1, len, 2 do
      local status, msg = traverse_exp(env, stm[i])
      if not status then return status, msg end
      status, msg = traverse_block(env, stm[i+1])
      if not status then return status, msg end
    end
  else
    for i=1, len-1, 2 do
      local status, msg = traverse_exp(env, stm[i])
      if not status then return status, msg end
      status, msg = traverse_block(env, stm[i+1])
      if not status then return status, msg end
    end
    local status, msg = traverse_block(env, stm[len])
    if not status then return status, msg end
  end
  return true
end

local function traverse_label (env, stm)
  if not tlst.set_label(env, stm[1]) then
    local msg = string.format("label '%s' already defined", stm[1])
    return nil, syntaxerror(env.subject, stm.pos, env.filename, msg)
  else
    return true
  end
end

local function traverse_local (env, stm)
  local status, msg = traverse_explist(env, stm[2])
  if not status then return status, msg end
  return true
end

local function traverse_localrec (env, stm)
  local status, msg = traverse_exp(env, stm[2][1])
  if not status then return status, msg end
  return true
end

local function traverse_repeat (env, stm)
  tlst.begin_loop(env)
  local status, msg = traverse_block(env, stm[1])
  if not status then return status, msg end
  status, msg = traverse_exp(env, stm[2])
  if not status then return status, msg end
  tlst.end_loop(env)
  return true
end

local function traverse_return (env, stm)
  local status, msg = traverse_explist(env, stm)
  if not status then return status, msg end
  return true
end

local function traverse_while (env, stm)
  tlst.begin_loop(env)
  local status, msg = traverse_exp(env, stm[1])
  if not status then return status, msg end
  status, msg = traverse_block(env, stm[2])
  if not status then return status, msg end
  tlst.end_loop(env)
  return true
end

function traverse_var (env, var)
  local tag = var.tag
  if tag == "Id" then
    return true
  elseif tag == "Index" then
    local status, msg = traverse_exp(env, var[1])
    if not status then return status, msg end
    status, msg = traverse_exp(env, var[2])
    if not status then return status, msg end
    return true
  else
    error("trying to traverse a variable, but got a " .. tag)
  end
end

function traverse_varlist (env, varlist)
  for k, v in ipairs(varlist) do
    local status, msg = traverse_var(env, v)
    if not status then return status, msg end
  end
  return true
end

function traverse_exp (env, exp)
  local tag = exp.tag
  if tag == "Nil" or
     tag == "True" or
     tag == "False" or
     tag == "Number" or
     tag == "String" then
    return true
  elseif tag == "Dots" then
    return traverse_vararg(env, exp)
  elseif tag == "Function" then
    return traverse_function(env, exp)
  elseif tag == "Table" then
    return traverse_table(env, exp)
  elseif tag == "Op" then
    return traverse_op(env, exp)
  elseif tag == "Paren" then
    return traverse_paren(env, exp)
  elseif tag == "Call" then
    return traverse_call(env, exp)
  elseif tag == "Invoke" then
    return traverse_invoke(env, exp)
  elseif tag == "Id" or
         tag == "Index" then
    return traverse_var(env, exp)
  else
    error("trying to traverse an expression, but got a " .. tag)
  end
end

function traverse_explist (env, explist)
  for k, v in ipairs(explist) do
    local status, msg = traverse_exp(env, v)
    if not status then return status, msg end
  end
  return true
end

function traverse_stm (env, stm)
  local tag = stm.tag
  if tag == "Do" then
    return traverse_block(env, stm)
  elseif tag == "Set" then
    return traverse_assignment(env, stm)
  elseif tag == "ConstSet" then
    return traverse_const_assignment(env, stm)
  elseif tag == "While" then
    return traverse_while(env, stm)
  elseif tag == "Repeat" then
    return traverse_repeat(env, stm)
  elseif tag == "If" then
    return traverse_if(env, stm)
  elseif tag == "Fornum" then
    return traverse_fornum(env, stm)
  elseif tag == "Forin" then
    return traverse_forin(env, stm)
  elseif tag == "Local" then
    return traverse_local(env, stm)
  elseif tag == "Localrec" then
    return traverse_localrec(env, stm)
  elseif tag == "Goto" then
    return traverse_goto(env, stm)
  elseif tag == "Label" then
    return traverse_label(env, stm)
  elseif tag == "Return" then
    return traverse_return(env, stm)
  elseif tag == "Break" then
    return traverse_break(env, stm)
  elseif tag == "Call" then
    return traverse_call(env, stm)
  elseif tag == "Invoke" then
    return traverse_invoke(env, stm)
  elseif tag == "Interface" or
         tag == "LocalInterface" then
    return true
  else
    error("trying to traverse a statement, but got a " .. tag)
  end
end

function traverse_block (env, block)
  local l = {}
  tlst.begin_scope(env)
  for k, v in ipairs(block) do
    local status, msg = traverse_stm(env, v)
    if not status then return status, msg end
  end
  tlst.end_scope(env)
  return true
end

local function verify_pending_gotos (env)
  for s = tlst.get_maxscope(env), 1, -1 do
    for k, v in ipairs(tlst.get_pending_gotos(env, s)) do
      local l = v[1]
      if not tlst.exist_label(env, s, l) then
        local msg = string.format("no visible label '%s' for <goto>", l)
        return nil, syntaxerror(env.subject, v.pos, env.filename, msg)
      end
    end
  end
  return true
end

local function traverse (ast, errorinfo, strict)
  assert(type(ast) == "table")
  assert(type(errorinfo) == "table")
  assert(type(strict) == "boolean")
  local env = tlst.new_env(errorinfo.subject, errorinfo.filename)
  tlst.begin_function(env)
  tlst.set_vararg(env)
  local status, msg = traverse_block(env, ast)
  if not status then return status, msg end
  tlst.end_function(env)
  status, msg = verify_pending_gotos(env)
  if not status then return status, msg end
  return ast
end

function tlparser.parse (subject, filename, strict)
  local errorinfo = { subject = subject, filename = filename }
  lpeg.setmaxstack(1000)
  local ast, error_msg = lpeg.match(G, subject, nil, errorinfo, strict)
  if not ast then return ast, error_msg end
  return traverse(ast, errorinfo, strict)
end

return tlparser
