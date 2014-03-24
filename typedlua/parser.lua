--[[
This module implements Typed Lua parser, and generates an
Abstract Syntax Tree that extends the format implemented by Metalua.
For more information about Metalua, please, visit:
https://github.com/fab13n/metalua-parser

block: { stat* }

stat:
  `Do{ stat* }
  | `Set{ {lhs+} {expr+} }                    -- lhs1, lhs2... = e1, e2...
  | `While{ expr block }                      -- while e do b end
  | `Repeat{ block expr }                     -- repeat b until e
  | `If{ (expr block)+ block? }               -- if e1 then b1 [elseif e2 then b2] ... [else bn] end
  | `Fornum{ ident expr expr expr? block }    -- for ident = e, e[, e] do b end
  | `Forin{ {ident+} {expr+} block }          -- for i1, i2... in e1, e2... do b end
  | `Local{ {ident+} {expr+}? }               -- local i1, i2... = e1, e2...
  | `Localrec{ ident expr }                   -- only used for 'local function'
  | `Goto{ <string> }                         -- goto str
  | `Label{ <string> }                        -- ::str::
  | `Return{ <expr*> }                        -- return e1, e2...
  | `Break                                    -- break
  | apply

expr:
  `Nil
  | `Dots
  | `True
  | `False
  | `Number{ <number> }
  | `String{ <string> }
  | `Function{ { ident* { `Dots type? }? } typelist? block }
  | `Table{ ( `Pair{ expr expr } | expr )* }
  | `Op{ opid expr expr? }
  | `Paren{ expr }       -- significant to cut multiple values returns
  | apply
  | lhs

apply:
  `Call{ expr expr* }
  | `Invoke{ expr `String{ <string> } expr* }

lhs: ident | `Index{ expr expr }

ident: `Id{ <string> type? }

opid: 'add' | 'sub' | 'mul' | 'div' | 'mod' | 'pow' | 'concat'
  | 'eq' | 'lt' | 'le' | 'and' | 'or' | 'not' | 'unm' | 'len'

type:
  `Literal{ literal }
  | `Base{ base }
  | `Value
  | `Any
  | `Union{ type type type* }
  | `Function{ typelist typelist }

literal: false | true | <number> | <string>

base: 'nil' | 'boolean' | 'number' | 'string'

typelist: `Tuple{ type+ { `VarArg type }? } | `Tuple{ type* { `VarArg type } }
]]
local parser = {}

local lpeg = require "lpeg"
local scope = require "typedlua.scope"

lpeg.locale(lpeg)

local P, S, V = lpeg.P, lpeg.S, lpeg.V
local C, Carg, Cb, Cc = lpeg.C, lpeg.Carg, lpeg.Cb, lpeg.Cc
local Cf, Cg, Cmt, Cp, Ct = lpeg.Cf, lpeg.Cg, lpeg.Cmt, lpeg.Cp, lpeg.Ct
local alpha, digit, alnum = lpeg.alpha, lpeg.digit, lpeg.alnum
local xdigit = lpeg.xdigit
local space = lpeg.space

local lineno = scope.lineno
local begin_scope, end_scope = scope.begin_scope, scope.end_scope
local begin_function, end_function = scope.begin_function, scope.end_function
local begin_loop, end_loop = scope.begin_loop, scope.end_loop
local insideloop = scope.insideloop

-- error message auxiliary functions

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

local G = { V"TypedLua",
  TypedLua = V"Shebang"^-1 * V"Skip" * V"Chunk" * -1 + report_error();
  -- type language
  Type = V"NullableType";
  NullableType = V"UnionType" * taggedCap("Base", symb("?") * Cc("nil"))^-1 /
                 function (t, nullable)
                   if nullable then
                     t[#t + 1] = nullable
                   end
                   if #t == 1 then
                     return t[1]
                   else
                     return t
                   end
                 end;
  UnionType = taggedCap("Union", V"PrimaryType" * Cg(symb("|") * V"PrimaryType")^0);
  PrimaryType = taggedCap("Literal", V"LiteralType") +
                taggedCap("Base", V"BaseType") +
                taggedCap("Value", V"TopType") +
                taggedCap("Any", V"DynamicType") +
                taggedCap("Function", V"FunctionType");
  LiteralType = V"FalseType" + V"TrueType" + V"NumberType" + V"StringType";
  FalseType = token("false", "Type") * Cc(false);
  TrueType = token("true", "Type") * Cc(true);
  NumberType = token(V"Number", "Type");
  StringType = token(V"String", "Type");
  BaseType = token(C"nil", "Type") +
             token(C"boolean", "Type") +
             token(C"number", "Type") +
             token(C"string", "Type");
  TopType = token("value", "Type");
  DynamicType = token("any", "Type");
  FunctionType = V"TypeList" * symb("->") * V"TypeList";
  TypeList = symb("(") * sepby1(V"Type", symb(","), "Tuple") *
             taggedCap("Vararg", symb("*"))^-1 * symb(")") /
             function (t, vararg)
               if vararg then
                 vararg[1] = t[#t]
                 table.remove(t)
                 table.insert(t, vararg)
               end
               return t
             end;
  -- parser
  Chunk = V"Block";
  StatList = (symb(";") + V"Stat")^0;
  Var = V"Id";
  Id = taggedCap("Id", token(V"Name", "Name"));
  TypedId = taggedCap("Id", token(V"Name", "Name") * (symb(":") * V"Type")^-1);
  FunctionDef = kw("function") * V"FuncBody";
  FieldSep = symb(",") + symb(";");
  Field = taggedCap("Pair", (symb("[") * V"Expr" * symb("]") * symb("=") * V"Expr") +
                    (taggedCap("String", token(V"Name", "Name")) * symb("=") * V"Expr")) +
          V"Expr";
  FieldList = (V"Field" * (V"FieldSep" * V"Field")^0 * V"FieldSep"^-1)^-1;
  Constructor = taggedCap("Table", symb("{") * V"FieldList" * symb("}"));
  NameList = sepby1(V"TypedId", symb(","), "NameList");
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
  ParList = V"NameList" * (symb(",") * V"TypedVarArg")^-1 /
            function (t, v)
              if v then table.insert(t, v) end
              return t
            end +
            V"TypedVarArg" /
            function (v)
              return {v}
            end +
            P(true) / function () return {} end;
  TypedVarArg = taggedCap("Dots", symb("...") * (symb(":") * V"Type")^-1);
  FuncBody = taggedCap("Function", symb("(") * V"ParList" * symb(")") *
             (symb(":") * V"TypeList")^-1 * V"Block" * kw("end"));
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

local function exist_label (env, scope, stm)
  local l = stm[1]
  for s=scope, 0, -1 do
    if env[s]["label"][l] then return true end
  end
  return false
end

local function set_label (env, label, pos)
  local scope = env.scope
  local l = env[scope]["label"][label]
  if not l then
    env[scope]["label"][label] = { name = label, pos = pos }
    return true
  else
    local msg = "label '%s' already defined at line %d"
    local line = lineno(env.errorinfo.subject, l.pos)
    msg = string.format(msg, label, line)
    return nil, syntaxerror(env.errorinfo, pos, msg)
  end
end

local function set_pending_goto (env, stm)
  local scope = env.scope
  table.insert(env[scope]["goto"], stm)
  return true
end

local function verify_pending_gotos (env)
  for s=env.maxscope, 0, -1 do
    for k, v in ipairs(env[s]["goto"]) do
      if not exist_label(env, s, v) then
        local msg = "no visible label '%s' for <goto>"
        msg = string.format(msg, v[1])
        return nil, syntaxerror(env.errorinfo, v.pos, msg)
      end
    end
  end
  return true
end

local function set_vararg (env, is_vararg)
  env["function"][env.fscope].is_vararg = is_vararg
end

local traverse_stm, traverse_exp, traverse_var
local traverse_block, traverse_explist, traverse_varlist, traverse_parlist

function traverse_parlist (env, parlist)
  local len = #parlist
  local is_vararg = false
  if len > 0 and parlist[len].tag == "Dots" then
    is_vararg = true
  end
  set_vararg(env, is_vararg)
  return true
end

local function traverse_function (env, exp)
  begin_function(env)
  begin_scope(env)
  local status, msg = traverse_parlist(env, exp[1])
  if not status then return status, msg end
  if not exp[3] then
    status, msg = traverse_block(env, exp[2])
    if not status then return status, msg end
  else
    status, msg = traverse_block(env, exp[3])
    if not status then return status, msg end
  end
  end_scope(env)
  end_function(env)
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
    if tag == "Pair" then
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
  if not env["function"][env.fscope].is_vararg then
    local msg = "cannot use '...' outside a vararg function"
    return nil, syntaxerror(env.errorinfo, exp.pos, msg)
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

local function traverse_break (env, stm)
  if not insideloop(env) then
    local msg = "<break> not inside a loop"
    return nil, syntaxerror(env.errorinfo, stm.pos, msg)
  end
  return true
end

local function traverse_forin (env, stm)
  begin_loop(env)
  begin_scope(env)
  local status, msg = traverse_explist(env, stm[2])
  if not status then return status, msg end
  status, msg = traverse_block(env, stm[3])
  if not status then return status, msg end
  end_scope(env)
  end_loop(env)
  return true
end

local function traverse_fornum (env, stm)
  local status, msg
  begin_loop(env)
  begin_scope(env)
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
  end_scope(env)
  end_loop(env)
  return true
end

local function traverse_goto (env, stm)
  local status, msg = set_pending_goto(env, stm)
  if not status then return status, msg end
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
  local status, msg = set_label(env, stm[1], stm.pos)
  if not status then return status, msg end
  return true
end

local function traverse_let (env, stm)
  local status, msg = traverse_explist(env, stm[2])
  if not status then return status, msg end
  return true
end

local function traverse_letrec (env, stm)
  local status, msg = traverse_exp(env, stm[2][1])
  if not status then return status, msg end
  return true
end

local function traverse_repeat (env, stm)
  begin_loop(env)
  local status, msg = traverse_block(env, stm[1])
  if not status then return status, msg end
  status, msg = traverse_exp(env, stm[2])
  if not status then return status, msg end
  end_loop(env)
  return true
end

local function traverse_return (env, stm)
  local status, msg = traverse_explist(env, stm)
  if not status then return status, msg end
  return true
end

local function traverse_while (env, stm)
  begin_loop(env)
  local status, msg = traverse_exp(env, stm[1])
  if not status then return status, msg end
  status, msg = traverse_block(env, stm[2])
  if not status then return status, msg end
  end_loop(env)
  return true
end

function traverse_var (env, var)
  local tag = var.tag
  if tag == "Id" then -- `Id{ <string> type? }
    return true
  elseif tag == "Index" then -- `Index{ expr expr }
    local status, msg = traverse_exp(env, var[1])
    if not status then return status, msg end
    status, msg = traverse_exp(env, var[2])
    if not status then return status, msg end
    return true
  else
    error("expecting a variable, but got a " .. tag)
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
     tag == "Number" or -- `Number{ <number> }
     tag == "String" then -- `String{ <string> }
    return true
  elseif tag == "Dots" then
    return traverse_vararg(env, exp)
  elseif tag == "Function" then -- `Function{ { ident* { `Dots type? }? } type? block } 
    return traverse_function(env, exp)
  elseif tag == "Table" then -- `Table{ ( `Pair{ expr expr } | expr )* }
    return traverse_table(env, exp)
  elseif tag == "Op" then -- `Op{ opid expr expr? }
    return traverse_op(env, exp)
  elseif tag == "Paren" then -- `Paren{ expr }
    return traverse_paren(env, exp)
  elseif tag == "Call" then -- `Call{ expr expr* }
    return traverse_call(env, exp)
  elseif tag == "Invoke" then -- `Invoke{ expr `String{ <string> expr* }
    return traverse_invoke(env, exp)
  elseif tag == "Id" or -- `Id{ <string> }
         tag == "Index" then -- `Index{ expr expr }
    return traverse_var(env, exp)
  else
    error("expecting an expression, but got a " .. tag)
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
  if tag == "Do" then -- `Do{ stat* }
    return traverse_block(env, stm)
  elseif tag == "Set" then -- `Set{ {lhs+} {expr+} }
    return traverse_assignment(env, stm)
  elseif tag == "While" then -- `While{ expr block }
    return traverse_while(env, stm)
  elseif tag == "Repeat" then -- `Repeat{ block expr }
    return traverse_repeat(env, stm)
  elseif tag == "If" then -- `If{ (expr block)+ block? }
    return traverse_if(env, stm)
  elseif tag == "Fornum" then -- `Fornum{ ident expr expr expr? block }
    return traverse_fornum(env, stm)
  elseif tag == "Forin" then -- `Forin{ {ident+} {expr+} block }
    return traverse_forin(env, stm)
  elseif tag == "Local" then -- `Local{ {ident+} {expr+}? }
    return traverse_let(env, stm)
  elseif tag == "Localrec" then -- `Localrec{ ident expr }
    return traverse_letrec(env, stm)
  elseif tag == "Goto" then -- `Goto{ <string> }
    return traverse_goto(env, stm)
  elseif tag == "Label" then -- `Label{ <string> }
    return traverse_label(env, stm)
  elseif tag == "Return" then -- `Return{ <expr>* }
    return traverse_return(env, stm)
  elseif tag == "Break" then
    return traverse_break(env, stm)
  elseif tag == "Call" then -- `Call{ expr expr* }
    return traverse_call(env, stm)
  elseif tag == "Invoke" then -- `Invoke{ expr `String{ <string> } expr* }
    return traverse_invoke(env, stm)
  else
    error("expecting a statement, but got a " .. tag)
  end
end

function traverse_block (env, block)
  local l = {}
  begin_scope(env)
  for k, v in ipairs(block) do
    local status, msg = traverse_stm(env, v)
    if not status then return status, msg end
  end
  end_scope(env)
  return true
end


local function traverse (ast, errorinfo)
  assert(type(ast) == "table")
  assert(type(errorinfo) == "table")
  local env = { errorinfo = errorinfo, ["function"] = {} }
  begin_function(env)
  set_vararg(env, true)
  local status, msg = traverse_block(env, ast)
  if not status then return status, msg end
  end_function(env)
  status, msg = verify_pending_gotos(env)
  if not status then return status, msg end
  return ast
end

function parser.parse (subject, filename)
  local errorinfo = { subject = subject, filename = filename }
  lpeg.setmaxstack(1000)
  local ast, error_msg = lpeg.match(G, subject, nil, errorinfo)
  if not ast then return ast, error_msg end
  return traverse(ast, errorinfo)
end

return parser
