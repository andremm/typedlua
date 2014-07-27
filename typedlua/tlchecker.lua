--[[
This file implements Typed Lua type checker
]]

local tlchecker = {}

local tlst = require "typedlua.tlst"
local tltype = require "typedlua.tltype"
local tldparser = require "typedlua.tldparser"

local check_block, check_stm, check_exp

function check_exp (env, exp, strict, warnings)
  local tag = exp.tag
  if tag == "Nil" then
  elseif tag == "Dots" then
  elseif tag == "True" then
  elseif tag == "False" then
  elseif tag == "Number" then
  elseif tag == "String" then
  elseif tag == "Function" then
  elseif tag == "Table" then
  elseif tag == "Op" then
  elseif tag == "Paren" then
  elseif tag == "Call" then
  elseif tag == "Invoke" then
  elseif tag == "Id" then
  elseif tag == "Index" then
  else
    error("cannot type check expression " .. tag)
  end
end

function check_stm (env, stm, strict, warnings)
  local tag = stm.tag
  if tag == "Do" then
  elseif tag == "Set" then
  elseif tag == "While" then
  elseif tag == "Repeat" then
  elseif tag == "If" then
  elseif tag == "Fornum" then
  elseif tag == "Forin" then
  elseif tag == "Local" then
  elseif tag == "Localrec" then
  elseif tag == "Goto" then
  elseif tag == "Label" then
  elseif tag == "Return" then
  elseif tag == "Break" then
  elseif tag == "Call" then
  elseif tag == "Invoke" then
  elseif tag == "Interface" then
  elseif tag == "LocalInterface" then
  else
    error("cannot type check statement " .. tag)
  end
end

function check_block (env, block, strict, warnings)
  tlst.begin_scope(env)
  for k, v in ipairs(block) do
    check_stm(env, v, strict, warnings)
  end
  tlst.end_scope(env)
end

function tlchecker.typecheck (ast, errorinfo, strict, warnings)
  assert(type(ast) == "table")
  assert(type(errorinfo) == "table")
  assert(type(strict) == "boolean")
  assert(type(warnings) == "boolean")
  local env = tlst.new_env(errorinfo.subject, errorinfo.filename)
  tlst.begin_function(env)
  tlst.begin_scope(env)
  for k, v in ipairs(ast) do
    check_stm(env, v, strict, warnings)
  end
  tlst.end_scope(env)
  tlst.end_function(env)
end

return tlchecker
