--[[
This file implements the Typed Lua parser using LPeg
]]

local parser = {}

local lpeg = require "lpeg"
local st = require "typedlua.st"

lpeg.locale(lpeg)

local P, S, V = lpeg.P, lpeg.S, lpeg.V
local C, Carg, Cb, Cc = lpeg.C, lpeg.Carg, lpeg.Cb, lpeg.Cc
local Cf, Cg, Cmt, Cp, Ct = lpeg.Cf, lpeg.Cg, lpeg.Cmt, lpeg.Cp, lpeg.Ct
local alpha, digit, alnum = lpeg.alpha, lpeg.digit, lpeg.alnum
local xdigit = lpeg.xdigit
local space = lpeg.space

local lineno = st.lineno
local begin_scope, end_scope = st.begin_scope, st.end_scope
local begin_function, end_function = st.begin_function, st.end_function
local begin_loop, end_loop = st.begin_loop, st.end_loop
local insideloop = st.insideloop

-- error message auxiliary functions

-- trim
local function trim (s)
  return s:gsub("^%s+", ""):gsub("%s+$", "")
end

-- creates an error message for the input string
local function syntaxerror (errorinfo, pos, msg)
  local l, c = lineno(errorinfo.subject, pos)
  if c == 0 then c = 1 end
  local error_msg = "%s:%d:%d: syntax error, %s"
  return string.format(error_msg, errorinfo.filename, l, c, msg)
end

-- creates an errror message using the farthest failure position
local function errormsg (s, t)
  local i = t.ffp or 1
  local u = lpeg.match(C((1 - space)^0), s, i)
  if u == '' then u = "EOF" end
  local msg = string.format("unexpected '%s'", u)
  if t.expected then
    msg = string.format(msg .. ", expecting %s", t.expected)
  end
  return syntaxerror(t, i, msg)
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
  for k, v in ipairs(el) do
    if v.tag == "ExpVar" then
      vl[k] = v[1]
    else
      return false
    end
  end
  return true, vl
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
  TypedLua = V"Shebang"^-1 * V"Skip" * V"Chunk" * -1;
  -- type language
  Type = V"UnionType";
  UnionType = chainl1(V"PrimaryType", V"UnionOp");
  UnionOp = symb("|") / "TypeUnion";
  FunctionType = taggedCap("TypeFunction",
                 symb("(") * (V"Type2" + V"TypeVoid") * symb(")") *
                 symb("->") *
                 symb("(") * (V"Type2" + V"TypeListAny") * symb(")"));
  PrimaryType = V"ObjectType" +
                V"DynamicType" +
                V"NilType" +
                V"BaseType" +
                V"NameType" +
                V"FunctionType";
  ObjectType = taggedCap("TypeObject", token("object", "Type"));
  DynamicType = taggedCap("TypeAny", token("any", "Type"));
  NilType = taggedCap("TypeConstant", token("nil", "Type"));
  BaseType = taggedCap("TypeBase", V"GroundType");
  GroundType = token(C"boolean", "Type") +
               token(C"number", "Type") +
               token(C"string", "Type");
  NameType = taggedCap("TypeName", token(V"Name", "Type"));
  TypeVoid = Cp() /
    function (p)
      local t = { tag = "TypeTuple", pos = p, [1] = {} }
      t[1] = { tag = "TypeList", pos = p }
      return t
    end;
  TypeListAny = Cp() /
    function (p)
      local t = { tag = "TypeTuple", pos = p, [1] = {} }
      t[1] = { tag = "TypeList", pos = p, [1] = {} }
      t[1][1] = { tag = "TypeVarArg", pos = p, [1] = {} }
      t[1][1][1] = { tag = "TypeAny", pos = p }
      return t
    end;
  Type2 = V"VoidType" + V"TupleType";
  VoidType = token("void", "Type") * V"TypeVoid";
  TupleType = taggedCap("TypeTuple", V"TypeList");
  TypeList = sepby1(V"Type", symb(","), "TypeList") * V"VarArgOp"^-1 /
             function (t, is_vararg)
               if is_vararg then
                 local v = t[#t]
                 table.remove(t)
                 table.insert(t, { tag = is_vararg, pos = v.pos, [1] = v })
               end
               return t
             end;
  VarArgOp = symb("*") / "TypeVarArg";
  OptionalType = (symb(":") * V"Type") + V"UndefinedType";
  OptionalType2 = (symb(":") * V"Type2") + V"UndefinedType";
  UndefinedType = taggedCap("TypeUndefined", P(true));
  TypedName = taggedCap("Name", token(V"Name", "Name") * V"OptionalType");
  TypedVar = taggedCap("VarID", token(V"Name", "Name") * symb(":") * V"Type");
  TypedGlobal = taggedCap("ExpVar", V"TypedVar" * -V"FuncArgs");
  TypedVarArg = taggedCap("Name", token(C("..."), "...") * V"OptionalType");
  -- parser
  Chunk = V"Block";
  StatList = (symb(";") + V"Stat")^0;
  Var = taggedCap("VarID", token(V"Name", "Name") * V"UndefinedType");
  FunctionDef = taggedCap("ExpFunction", kw("function") * V"FuncBody");
  FieldSep = symb(",") + symb(";");
  Field = (Cc(function (t, e) local i = #t[2]+1; t[2][i] = e; return t end) *
            (Ct(symb("[") * V"Expr" * symb("]") * symb("=") * V"Expr") +
             Ct(taggedCap("ExpStr", token(V"Name", "Name")) * symb("=") * V"Expr"))) +
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
                  for i=1, len, 2 do
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
             taggedCap("ExpList", taggedCap("ExpStr", token(V"String", "String")));
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
  SimpleExp = taggedCap("ExpNum", token(V"Number", "Number")) +
              taggedCap("ExpStr", token(V"String", "String")) +
              taggedCap("ExpNil", kw("nil")) +
              taggedCap("ExpFalse", kw("false")) +
              taggedCap("ExpTrue", kw("true")) +
              taggedCap("ExpDots", symb("...")) +
              V"FunctionDef" +
              V"Constructor" +
              V"SuffixedExp";
  SuffixedExp = Cf(V"PrimaryExp" * (
                  taggedCap("DotIndex", symb(".") * taggedCap("ExpStr", token(V"Name", "Name"))) +
                  taggedCap("ArrayIndex", symb("[") * V"Expr" * symb("]")) +
                  taggedCap("ExpMethodCall", Cg(symb(":") * token(V"Name", "Name") * V"FuncArgs")) +
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
  ForNumName = taggedCap("Name", token(V"Name", "Name") * -symb(":")) /
               function (t)
                 t[2] = { tag = "TypeBase", pos = t.pos, [1] = "number" }
                 return t
               end;
  ForNum = taggedCap("StmForNum",
             V"ForNumName" * symb("=") * V"Expr" * symb(",") *
             V"Expr" * ((symb(",") * V"Expr") + Cc({tag = "ExpNum", [1] = 1})) *
             V"ForBody");
  ForGen = taggedCap("StmForGen", V"NameList" * kw("in") * V"ExpList" * V"ForBody");
  ForStat = kw("for") * (V"ForNum" + V"ForGen") * kw("end");
  RepeatStat = taggedCap("StmRepeat", kw("repeat") * V"Block" *
                 kw("until") * V"Expr");
  FuncName = sepby1(token(V"Name", "Name"), symb("."), "IDList") * (symb(":") * token(V"Name", "Name"))^-1 /
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
             V"OptionalType2" * V"Block" * kw("end");
  FuncStat = taggedCap("StmFunction", kw("function") * V"FuncName" * V"FuncBody");
  LocalFunc = taggedCap("StmLocalFunction", kw("function") * token(V"Name", "Name") * V"FuncBody");
  LocalAssign = taggedCap("StmLocalVar", V"NameList" * ((symb("=") * V"ExpList") + Ct(Cc())));
  LocalStat = kw("local") * (V"LocalFunc" + V"LocalAssign");
  LabelStat = taggedCap("StmLabel", symb("::") * token(V"Name", "Name") * symb("::"));
  BreakStat = taggedCap("StmBreak", kw("break"));
  GoToStat = taggedCap("StmGoTo", kw("goto") * token(V"Name", "Name"));
  RetStat = taggedCap("StmRet", kw("return") * (V"ExpList" + Ct(Cc())) * symb(";")^-1);
  ExprStat = Cmt(
             ((V"TypedGlobal" + V"SuffixedExp") *
                (Cc(function (...)
                           local vl = {...}
                           local el = vl[#vl]
                           table.remove(vl)
                           for k, v in ipairs(vl) do
                             if v.tag == "ExpVar" then
                               vl[k] = v[1]
                             else
                               -- invalid assignment
                               return false
                             end
                           end
                           vl.tag = "VarList"
                           vl.pos = vl[1].pos
                           return true, {tag = "StmAssign", pos = vl.pos, [1] = vl, [2] = el}
                         end) * V"Assignment"))
             +
             (V"SuffixedExp" *
                (Cc(function (s)
                           if s.tag == "ExpMethodCall" or
                              s.tag == "ExpFunctionCall" then
                             return true, {tag = "StmCall", pos = s.pos, [1] = s}
                           end
                           -- invalid statement
                           return false
                         end)))
             , function (s, i, s1, f, ...) return f(s1, ...) end);
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
  local fscope = env.fscope
  env["function"][fscope]["is_vararg"] = is_vararg
end

local traverse_stm, traverse_exp, traverse_var
local traverse_block, traverse_explist, traverse_varlist, traverse_parlist

local function traverse_anonymous_function (env, exp)
  begin_function(env)
  begin_scope(env)
  local status, msg
  status, msg = traverse_parlist(env, exp[1])
  if not status then return status, msg end
  status, msg = traverse_stm(env, exp[3])
  if not status then return status, msg end
  end_scope(env)
  end_function(env)
  return true
end

local function traverse_bin_exp (env, exp1, exp2)
  local status, msg
  status, msg = traverse_exp(env, exp1)
  if not status then return status, msg end
  status, msg = traverse_exp(env, exp2)
  if not status then return status, msg end
  return true
end

local function traverse_exp_call (env, exp, explist)
  local status, msg
  status, msg = traverse_exp(env, exp)
  if not status then return status, msg end
  status, msg = traverse_explist(env, explist)
  if not status then return status, msg end
  return true
end

local function traverse_table (env, fieldlist)
  local status, msg
  for k, v in ipairs(fieldlist[1]) do
    status, msg = traverse_exp(env, v[1])
    if not status then return status, msg end
  end
  for k, v in ipairs(fieldlist[2]) do
    status, msg = traverse_exp(env, v[1])
    if not status then return status, msg end
    status, msg = traverse_exp(env, v[2])
    if not status then return status, msg end
  end
  return true
end

local function traverse_vararg (env, exp)
  local fscope = env.fscope
  if not env["function"][fscope]["is_vararg"] then
    local msg = "cannot use '...' outside a vararg function"
    return nil, syntaxerror(env.errorinfo, exp.pos, msg)
  end
  return true
end

local function traverse_assignment (env, stm)
  local status, msg
  status, msg = traverse_varlist(env, stm[1])
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

local function traverse_stm_call (env, stm)
  local status, msg = traverse_exp(env, stm[1])
  if not status then return status, msg end
  return true
end

local function traverse_for_generic (env, stm)
  begin_loop(env)
  begin_scope(env)
  local status, msg = traverse_explist(env, stm[2])
  if not status then return status, msg end
  end_scope(env)
  end_loop(env)
  return true
end

local function traverse_for_numeric (env, stm)
  local status, msg
  begin_loop(env)
  begin_scope(env)
  status, msg = traverse_exp(env, stm[2])
  if not status then return status, msg end
  status, msg = traverse_exp(env, stm[3])
  if not status then return status, msg end
  status, msg = traverse_exp(env, stm[4])
  if not status then return status, msg end
  status, msg = traverse_stm(env, stm[5])
  if not status then return status, msg end
  end_scope(env)
  end_loop(env)
  return true
end

local function traverse_global_function (env, stm)
  begin_function(env)
  begin_scope(env)
  local status, msg
  status, msg = traverse_parlist(env, stm[2])
  if not status then return status, msg end
  status, msg = traverse_stm(env, stm[4])
  if not status then return status, msg end
  end_scope(env)
  end_function(env)
  return true
end

local function traverse_if_else (env, stm)
  local status, msg
  status, msg = traverse_exp(env, stm[1])
  if not status then return status, msg end
  status, msg = traverse_stm(env, stm[2])
  if not status then return status, msg end
  status, msg = traverse_stm(env, stm[3])
  if not status then return status, msg end
  return true
end

local function traverse_label (env, stm)
  local status, msg = set_label(env, stm[1], stm.pos)
  if not status then return status, msg end
  return true
end

local function traverse_local_function (env, stm)
  begin_function(env)
  begin_scope(env)
  local status, msg
  status, msg = traverse_parlist(env, stm[2])
  if not status then return status, msg end
  status, msg = traverse_stm(env, stm[4])
  if not status then return status, msg end
  end_scope(env)
  end_function(env)
  return true
end

local function traverse_local_var (env, stm)
  local status, msg = traverse_explist(env, stm[2])
  if not status then return status, msg end
  return true
end

local function traverse_goto (env, stm)
  local status, msg = set_pending_goto(env, stm)
  if not status then return status, msg end
  return true
end

local function traverse_repeat (env, stm)
  local status, msg
  begin_loop(env)
  status, msg = traverse_stm(env, stm[1])
  if not status then return status, msg end
  status, msg = traverse_exp(env, stm[2])
  if not status then return status, msg end
  end_loop(env)
  return true
end

local function traverse_return (env, stm)
  local status, msg = traverse_explist(env, stm[1])
  if not status then return status, msg end
  return true
end

local function traverse_while (env, stm)
  local status, msg
  begin_loop(env)
  status, msg = traverse_exp(env, stm[1])
  if not status then return status, msg end
  status, msg = traverse_stm(env, stm[2])
  if not status then return status, msg end
  end_loop(env)
  return true
end

function traverse_parlist (env, idlist)
  local len = #idlist
  if len > 0 then
    if idlist[len][1] == "..." then
      set_vararg(env, true)
      return true
    end
  end
  set_vararg(env, false)
  return true
end

function traverse_varlist (env, varlist)
  for k, v in ipairs(varlist) do
    local status, msg = traverse_var(env, v)
    if not status then return status, msg end
  end
  return true
end

function traverse_explist (env, explist)
  for k, v in ipairs(explist) do
    local status, msg = traverse_exp(env, v)
    if not status then return status, msg end
  end
  return true
end

function traverse_var (env, var)
  local tag = var.tag
  if tag == "VarID" then
    return true
  elseif tag == "VarIndex" then
    local status, msg
    status, msg = traverse_exp(env, var[1])
    if not status then return status, msg end
    status, msg = traverse_exp(env, var[2])
    if not status then return status, msg end
    return true
  else
    error("trying to traverse " .. tag)
  end
end

function traverse_exp (env, exp)
  local tag = exp.tag
  if tag == "ExpNil" or
     tag == "ExpFalse" or
     tag == "ExpTrue" or
     tag == "ExpNum" or -- ExpNum Double
     tag == "ExpStr" then -- ExpStr String
    return true
  elseif tag == "ExpDots" then
    return traverse_vararg(env, exp)
  elseif tag == "ExpVar" then -- ExpVar Var
    return traverse_var(env, exp[1])
  elseif tag == "ExpFunction" then -- ExpFunction [ID] Type Stm
    return traverse_anonymous_function(env, exp)
  elseif tag == "ExpTableConstructor" then -- ExpTableConstructor FieldList
    return traverse_table(env, exp[1])
  elseif tag == "ExpMethodCall" then -- ExpMethodCall Exp Name [Exp]
    return traverse_exp_call(env, exp[1], exp[3])
  elseif tag == "ExpFunctionCall" then -- ExpFunctionCall Exp [Exp]
    return traverse_exp_call(env, exp[1], exp[2])
  elseif tag == "ExpAdd" or -- ExpAdd Exp Exp 
         tag == "ExpSub" or -- ExpSub Exp Exp
         tag == "ExpMul" or -- ExpMul Exp Exp
         tag == "ExpDiv" or -- ExpDiv Exp Exp
         tag == "ExpMod" or -- ExpMod Exp Exp
         tag == "ExpPow" or -- ExpPow Exp Exp
         tag == "ExpConcat" or -- ExpConcat Exp Exp
         tag == "ExpNE" or -- ExpNE Exp Exp
         tag == "ExpEQ" or -- ExpEQ Exp Exp
         tag == "ExpLT" or -- ExpLT Exp Exp
         tag == "ExpLE" or -- ExpLE Exp Exp
         tag == "ExpGT" or -- ExpGT Exp Exp
         tag == "ExpGE" or -- ExpGE Exp Exp
         tag == "ExpAnd" or -- ExpAnd Exp Exp
         tag == "ExpOr" then -- ExpOr Exp Exp
    return traverse_bin_exp(env, exp[1], exp[2])
  elseif tag == "ExpNot" or -- ExpNot Exp
         tag == "ExpMinus" or -- ExpMinus Exp
         tag == "ExpLen" then -- ExpLen Exp
    return traverse_exp(env, exp[1])
  else
    error("trying to traverse " .. tag)
  end
end

function traverse_stm (env, stm)
  local tag = stm.tag
  if tag == "StmBlock" then -- StmBlock [Stm]
    return traverse_block(env, stm)
  elseif tag == "StmIfElse" then -- StmIfElse Exp Stm Stm
    return traverse_if_else(env, stm)
  elseif tag == "StmWhile" then -- StmWhile Exp Stm
    return traverse_while(env, stm)
  elseif tag == "StmForNum" then -- StmForNum ID Exp Exp Exp Stm
    return traverse_for_numeric(env, stm)
  elseif tag == "StmForGen" then -- StmForGen [ID] [Exp] Stm
    return traverse_for_generic(env, stm)
  elseif tag == "StmRepeat" then -- StmRepeat Stm Exp
    return traverse_repeat(env, stm)
  elseif tag == "StmFunction" then -- StmFunction FuncName [ID] Type Stm
    return traverse_global_function(env, stm)
  elseif tag == "StmLocalFunction" then -- StmLocalFunction Name [ID] Type Stm
    return traverse_local_function(env, stm)
  elseif tag == "StmLabel" then -- StmLabel Name
    return traverse_label(env, stm)
  elseif tag == "StmGoTo" then -- StmGoTo Name
    return traverse_goto(env, stm)
  elseif tag == "StmBreak" then -- StmBreak
    return traverse_break(env, stm)
  elseif tag == "StmAssign" then -- StmAssign [Var] [Exp]
    return traverse_assignment(env, stm)
  elseif tag == "StmLocalVar" then -- StmLocalVar [ID] [Exp]
    return traverse_local_var(env, stm)
  elseif tag == "StmRet" then -- StmRet [Exp]
    return traverse_return(env, stm)
  elseif tag == "StmCall" then -- StmCall Exp
    return traverse_stm_call(env, stm)
  else
    error("trying to traverse " .. tag)
  end
end

function traverse_block (env, block)
  local tag = block.tag
  if tag ~= "StmBlock" then
    error("trying to traverse " .. tag)
  end
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
  local status, msg
  begin_function(env)
  set_vararg(env, true)
  status, msg = traverse_block(env, ast)
  if not status then return status, msg end
  end_function(env)
  status, msg = verify_pending_gotos(env)
  if not status then return status, msg end
  return ast
end

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
  local ast = lpeg.match(G, subject, nil, errorinfo)
  if not ast then
    return nil, errormsg(subject, errorinfo)
  end
  return traverse(ast, errorinfo)
end

function parser.parse (subject, filename)
  local errorinfo = { subject = subject, filename = filename }
  lpeg.setmaxstack(1000)
  local ast = lpeg.match(G, subject, nil, errorinfo)
  if not ast then
    return nil, errormsg(subject, errorinfo)
  end
  return traverse(ast, errorinfo)
end

return parser
