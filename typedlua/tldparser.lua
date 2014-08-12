--[[
This module implements the parser for Typed Lua description files.
]]

local tldparser = {}

local lpeg = require "lpeg"
lpeg.locale(lpeg)

local tlast = require "typedlua.tlast"
local tllexer = require "typedlua.tllexer"
local tlst = require "typedlua.tlst"
local tltype = require "typedlua.tltype"

local G = lpeg.P { "TypedLuaDescription";
  TypedLuaDescription = tllexer.Skip * lpeg.V("DescriptionList") * -1 +
                        tllexer.report_error();
  -- type language
  Type = lpeg.V("NilableType");
  NilableType = lpeg.V("UnionType") * (tllexer.symb("?") * lpeg.Cc(true))^-1 /
                tltype.UnionNil;
  UnionType = lpeg.V("PrimaryType") * (lpeg.Cg(tllexer.symb("|") * lpeg.V("PrimaryType"))^0) /
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
  LiteralType = ((tllexer.token("false", "Type") * lpeg.Cc(false)) +
                (tllexer.token("true", "Type") * lpeg.Cc(true)) +
                tllexer.token(tllexer.Number, "Type") +
                tllexer.token(tllexer.String, "Type")) /
                tltype.Literal;
  BaseType = tllexer.token("boolean", "Type") / tltype.Boolean +
             tllexer.token("number", "Type") / tltype.Number +
             tllexer.token("string", "Type") / tltype.String;
  NilType = tllexer.token("nil", "Type") / tltype.Nil;
  ValueType = tllexer.token("value", "Type") / tltype.Value;
  AnyType = tllexer.token("any", "Type") / tltype.Any;
  SelfType = tllexer.token("self", "Type") / tltype.Self;
  FunctionType = lpeg.V("InputType") * tllexer.symb("->") * lpeg.V("NilableTuple") /
                 tltype.Function;
  MethodType = lpeg.V("InputType") * tllexer.symb("=>") * lpeg.V("NilableTuple") *
               lpeg.Cc(true) / tltype.Function;
  InputType = tllexer.symb("(") * (lpeg.V("TupleType") + lpeg.Cc(nil)) * tllexer.symb(")") *
              lpeg.Carg(2) /
              tltype.inputTuple;
  NilableTuple = lpeg.V("UnionlistType") * (tllexer.symb("?") * lpeg.Carg(2))^-1 /
                 tltype.UnionlistNil;
  UnionlistType = lpeg.V("OutputType") * (lpeg.Cg(tllexer.symb("|") * lpeg.V("OutputType"))^0) /
                  tltype.Unionlist;
  OutputType = tllexer.symb("(") * (lpeg.V("TupleType") + lpeg.Cc(nil)) * tllexer.symb(")") *
               lpeg.Carg(2) /
               tltype.outputTuple;
  TupleType = lpeg.Ct(lpeg.V("Type") * (tllexer.symb(",") * lpeg.V("Type"))^0) *
              (tllexer.symb("*") * lpeg.Cc(true))^-1 /
              tltype.Tuple;
  TableType = tllexer.symb("{") *
              ((lpeg.V("FieldType") * (tllexer.symb(",") * lpeg.V("FieldType"))^0) +
              (lpeg.Cc(false) * lpeg.Cc(tltype.Number()) * lpeg.V("Type")) / tltype.Field +
              lpeg.Cc(nil)) *
              tllexer.symb("}") /
              tltype.Table;
  FieldType = ((tllexer.kw("const") * lpeg.Cc(true)) + lpeg.Cc(false)) *
              lpeg.V("KeyType") * tllexer.symb(":") * lpeg.V("Type") / tltype.Field;
  KeyType = lpeg.V("LiteralType") +
	    lpeg.V("BaseType") +
            lpeg.V("ValueType") +
            lpeg.V("AnyType");
  VariableType = tllexer.token(tllexer.Name, "Type") / tltype.Variable;
  RetType = lpeg.V("NilableTuple") +
            lpeg.V("Type") * lpeg.Carg(2) / tltype.retType;
  Id = lpeg.Cp() * tllexer.token(tllexer.Name, "Name") / tlast.ident;
  TypeDecId = (tllexer.kw("const") * lpeg.V("Id") / tlast.setConst) +
              lpeg.V("Id");
  IdList = lpeg.Cp() * lpeg.V("TypeDecId") * (tllexer.symb(",") * lpeg.V("TypeDecId"))^0 /
           tlast.namelist;
  IdDec = lpeg.V("IdList") * tllexer.symb(":") *
          (lpeg.V("Type") + lpeg.V("MethodType")) / tltype.fieldlist;
  IdDecList = (lpeg.V("IdDec")^1 + lpeg.Cc(nil)) / tltype.Table;
  TypeDec = tllexer.token(tllexer.Name, "Name") * lpeg.V("IdDecList") * tllexer.kw("end");
  Interface = lpeg.Cp() * tllexer.kw("interface") * lpeg.V("TypeDec") /
              tlast.statInterface;
  Userdata = lpeg.Cp() * tllexer.kw("userdata") * lpeg.V("TypeDec") /
             tlast.statUserdata;
  -- parser
  DescriptionList = lpeg.V("DescriptionItem")^1 / function (...) return {...} end;
  DescriptionItem = lpeg.V("TypedId") + lpeg.V("Interface") + lpeg.V("Userdata");
  TypedId = lpeg.Cp() * tllexer.token(tllexer.Name, "Name") *
            tllexer.symb(":") * lpeg.V("Type") / tlast.ident;
}

local function traverse (ast, errorinfo, strict)
  assert(type(ast) == "table")
  assert(type(errorinfo) == "table")
  assert(type(strict) == "boolean")
  local t = tltype.Table()
  for k, v in ipairs(ast) do
    local tag = v.tag
    if tag == "Id" then
      table.insert(t, tltype.Field(v.const, tltype.Literal(v[1]), v[2]))
    elseif tag == "Interface" then
      local name, t = v[1], v[2]
      local status, msg = tltype.checkTypeDec(t)
      if not status then
        return nil, tllexer.syntaxerror(env.subject, v.pos, env.filename, msg)
      end
      if tltype.checkRecursive(t, name) then
        v[2] = tltype.Recursive(name, t)
      end
    elseif tag == "Userdata" then
      local name, t = v[1], v[2]
      local status, msg = tltype.checkTypeDec(t)
      if not status then
        return nil, tllexer.syntaxerror(env.subject, v.pos, env.filename, msg)
      end
      if tltype.checkRecursive(t, name) then
        local msg = string.format("userdata '%s' is recursive", name)
        return nil, tllexer.syntaxerror(env.subject, v.pos, env.filename, msg)
      end
    else
      error("trying to traverse a description item, but got a " .. tag)
    end
  end
  local status, msg = tltype.checkTypeDec(t)
  if not status then
    return nil, tllexer.syntaxerror(env.subject, 1, env.filename, msg)
  else
    return t
  end
end

function tldparser.parse (filename, strict)
  local file = assert(io.open(filename, "r"))
  local subject = file:read("*a")
  file:close()
  local errorinfo = { subject = subject, filename = filename }
  lpeg.setmaxstack(1000)
  local ast, error_msg = lpeg.match(G, subject, nil, errorinfo, strict)
  if not ast then return ast, error_msg end
  return traverse(ast, errorinfo, strict)
end

return tldparser
