
local statistics = {}

local parser = require "parser"
local pp = require "pp"

local number_of_functions = 0
local anonymousf = 0
local globalf = 0
local localf = 0
local result = {}

local count_block, count_stm
local count_exp, count_var
local count_explist, count_varlist
local count_fieldlist

local check_block, check_stm
local check_exp, check_var
local check_explist, check_varlist
local check_fieldlist
local check_varindex_in_exp, check_varindex_in_explist
local check_varindex_in_var, check_varindex_in_varlist

local function new_func_def (func_type)
  result.number_of_functions = result.number_of_functions + 1
  local n = result.number_of_functions
  result[n] = {}
  result[n].use_type = 0
  result[n].type_id = 0
  result[n].type_other = 0
  result[n].is_method = 0
  result[n].table_field = 0
  result[n].ret_nil_se = 0
  result[n].ret_false_se = 0
  result[n].use_setmetatable = 0
  result[n].use_getmetatable = 0
  result[n].number_of_constructs = 0
  result[n].empty_construct = 0
  result[n].only_static = 0
  result[n].only_dynamic = 0
  result[n].static_and_dynamic = 0
  result[n].varindex = 0
  result[n].varindex_literal = 0
  result[n].varindex_non_literal = 0
  result[n].old_varindex = 0
  result[n].varindex_function_call = 0
  result[n].varindex_somewhere_else = 0
  if func_type == "anonymous" then
    result[n].func_type = "A"
  elseif func_type == "global" then
    result[n].func_type = "G"
  elseif func_type == "local" then
    result[n].func_type = "L"
  elseif func_type == "main" then
    result[n].func_type = "M"
  else
    error("func_type '" .. func_type .. "' not known")
  end
end

count_fieldlist = function (fieldlist)
  for k,v in ipairs(fieldlist[1]) do
    count_exp(v[1])
  end
  for k,v in ipairs(fieldlist[2]) do
    count_exp(v[1])
    count_exp(v[2])
  end
end

check_fieldlist = function (fieldlist, func_name, func_id)
  local id = func_id
  local only_static,only_dynamic = true,true
  -- statistics of the use of table constructs
  if #fieldlist[1] == 0 and #fieldlist[2] == 0 then
    result[func_id].empty_construct = result[func_id].empty_construct + 1
  elseif #fieldlist[1] > 0 and #fieldlist[2] == 0 then
    result[func_id].only_static = result[func_id].only_static + 1
  elseif #fieldlist[1] == 0 and #fieldlist[2] > 0 then
    for k,v in ipairs(fieldlist[2]) do
      if v[1].tag == "ExpFunctionCall" or v[1].tag == "ExpMethodCall" then
        only_static = false
      else
        only_dynamic = false
      end
    end
    if only_static then
      result[func_id].only_static = result[func_id].only_static + 1
    elseif only_dynamic then
      result[func_id].only_dynamic = result[func_id].only_dynamic + 1
    else
      result[func_id].static_and_dynamic = result[func_id].static_and_dynamic + 1
    end
  elseif #fieldlist[1] > 0 and #fieldlist[2] > 0 then
    only_dynamic = false
    for k,v in ipairs(fieldlist[2]) do
      if v[1].tag == "ExpFunctionCall" or v[1].tag == "ExpMethodCall" then
        only_static = false
      end
    end
    if only_static then
      result[func_id].only_static = result[func_id].only_static + 1
    else
      result[func_id].static_and_dynamic = result[func_id].static_and_dynamic + 1
    end
  end

  for k,v in ipairs(fieldlist[1]) do
    check_exp(v[1], func_name, func_id)
    -- statistics of the use of function declaration as table field
    if v[1].tag == "ExpFunction" then
      id = id + 1
      result[id].table_field = 1
    end
  end
  for k,v in ipairs(fieldlist[2]) do
    -- statistics of the use of function declaration as table field
    check_exp(v[1], func_name, func_id)
    check_exp(v[2], func_name, func_id)
    if v[2].tag == "ExpFunction" then
      id = id + 1
      result[id].table_field = 1
    end
  end
end

count_explist = function (explist)
  for k,v in ipairs(explist) do
    count_exp(v)
  end
end

check_explist = function (explist, func_name, func_id)
  for k,v in ipairs(explist) do
    check_exp(v, func_name, func_id)
  end
end

count_varlist = function (varlist)
  for k,v in ipairs(varlist) do
    count_var(v)
  end
end

check_varlist = function (varlist, func_name, func_id)
  for k,v in ipairs(varlist) do
    check_var(v, func_name, func_id)
  end
end

count_var = function (var)
  -- VarID ID
  if var.tag == "VarID" then
  -- VarIndex Exp Exp
  elseif var.tag == "VarIndex" then
    count_exp(var[1])
    count_exp(var[2])
  end
end

check_var = function (var, func_name, func_id)
  if var.tag == "VarID" then -- VarID ID
  elseif var.tag == "VarIndex" then -- VarIndex Exp Exp
    -- statistics of the use of table indexing
    result[func_id].varindex = result[func_id].varindex + 1
    local tag = var[2].tag
    if tag == "ExpNil" or
       tag == "ExpFalse" or
       tag == "ExpTrue" or
       tag == "ExpNum" or
       tag == "ExpStr" or
       tag == "ExpFunction" then
      result[func_id].varindex_literal = result[func_id].varindex_literal + 1
    else
      result[func_id].varindex_non_literal = result[func_id].varindex_non_literal + 1
    end
    check_exp(var[1], func_name, func_id)
    check_exp(var[2], func_name, func_id)
  end
end

check_varindex_in_exp = function (exp, where, func_id)
  if exp.tag == "ExpVar" and
     exp[1].tag == "VarIndex" then
    result[func_id].old_varindex = result[func_id].old_varindex + 1
    if where == "call" then
      result[func_id].varindex_function_call = result[func_id].varindex_function_call + 1
    else
      result[func_id].varindex_somewhere_else = result[func_id].varindex_somewhere_else + 1
    end
  end
end

check_varindex_in_explist = function (explist, where, func_id)
  for k,v in ipairs(explist) do
    check_varindex_in_exp(v, where, func_id)
  end
end

check_varindex_in_var = function (var, where, func_id)
  if var.tag == "VarIndex" then
    result[func_id].old_varindex = result[func_id].old_varindex + 1
    if where == "call" then
      result[func_id].varindex_function_call = result[func_id].varindex_function_call + 1
    else
      result[func_id].varindex_somewhere_else = result[func_id].varindex_somewhere_else + 1
    end
  end
end

check_varindex_in_varlist = function (varlist, where, func_id)
  for k,v in ipairs(varlist) do
    check_varindex_in_var(v, where, func_id)
  end
end

count_exp = function (exp)
  if exp.tag == "ExpNil" or
     exp.tag == "ExpFalse" or
     exp.tag == "ExpTrue" or
     exp.tag == "ExpDots" then
  -- ExpNum Double
  elseif exp.tag == "ExpNum" then
  -- ExpStr String
  elseif exp.tag == "ExpStr" then
  -- ExpVar Var
  elseif exp.tag == "ExpVar" then
    count_var(exp[1])
  -- ExpFunction ([ID],IsVarArg) Stm 
  elseif exp.tag == "ExpFunction" then
    number_of_functions = number_of_functions + 1
    anonymousf = anonymousf + 1
    count_stm(exp[2])
  -- ExpTableConstructor [Exp] {[Exp] [Exp]}
  elseif exp.tag == "ExpTableConstructor" then
    count_fieldlist(exp[1])
  -- ExpMethodCall Exp ID [Exp]
  elseif exp.tag == "ExpMethodCall" then
    count_exp(exp[1])
    count_explist(exp[3])
  -- ExpFunctionCall Exp [Exp]
  elseif exp.tag == "ExpFunctionCall" then
    count_exp(exp[1])
    count_explist(exp[2])
  -- ExpBin Exp Exp
  elseif exp.tag == "ExpAnd" or
         exp.tag == "ExpOr" or
         exp.tag == "ExpAdd" or
         exp.tag == "ExpSub" or
         exp.tag == "ExpMul" or
         exp.tag == "ExpDiv" or
         exp.tag == "ExpMod" or
         exp.tag == "ExpPow" or
         exp.tag == "ExpConcat" or
         exp.tag == "ExpNE" or
         exp.tag == "ExpEQ" or
         exp.tag == "ExpLT" or
         exp.tag == "ExpLE" or
         exp.tag == "ExpGT" or
         exp.tag == "ExpGE" then
    count_exp(exp[1])
    count_exp(exp[2])
  -- ExpUn Exp
  elseif exp.tag == "ExpNot" or
         exp.tag == "ExpMinus" or
         exp.tag == "ExpLen" then
    count_exp(exp[1])
  else
    error("expecting an expression, but got a " .. exp.tag)
  end
end

check_exp = function (exp, func_name, func_id)
  if exp.tag == "ExpNil" or
     exp.tag == "ExpFalse" or
     exp.tag == "ExpTrue" or
     exp.tag == "ExpDots" or
     exp.tag == "ExpNum" or -- ExpNum Double
     exp.tag == "ExpStr" then -- ExpStr String
  elseif exp.tag == "ExpVar" then -- ExpVar Var
    check_var(exp[1], func_name, func_id)
  elseif exp.tag == "ExpFunction" then -- ExpFunction ParList Stm
    new_func_def("anonymous")
    local id = result.number_of_functions
    if #exp[1] > 0 then
      if exp[1][1] == "self" or exp[1][1] == "this" then
        result[id].is_method = 1
      end
    end
    check_stm(exp[2], func_name, result.number_of_functions)
  elseif exp.tag == "ExpTableConstructor" then -- ExpTableConstructor FieldList
    result[func_id].number_of_constructs = result[func_id].number_of_constructs + 1
    check_fieldlist(exp[1], func_name, func_id)
  elseif exp.tag == "ExpMethodCall" then -- ExpMethodCall Exp ID [Exp]
    result[func_id].old_varindex = result[func_id].old_varindex + 1
    result[func_id].varindex_function_call = result[func_id].varindex_function_call + 1
    check_varindex_in_explist(exp[3], "se", func_id)
    check_exp(exp[1], func_name, func_id)
    check_explist(exp[3], func_name, func_id)
  elseif exp.tag == "ExpFunctionCall" then -- ExpFunctionCall Exp [Exp]
    if exp[1].tag == "ExpVar" and exp[1][1].tag == "VarID" then
      -- statistics of the use of type
      local fname = exp[1][1][1]
      if fname == "type" then
        result[func_id].use_type = result[func_id].use_type + 1
        if exp[2][1].tag == "ExpVar" and exp[2][1][1].tag == "VarID" then
          result[func_id].type_id = result[func_id].type_id + 1
        else
          result[func_id].type_other = result[func_id].type_other + 1
        end
      -- statistics of the use of setmetatable
      elseif fname == "setmetatable" then
        result[func_id].use_setmetatable = 1
      -- statistics of the use of getmetatable
      elseif fname == "getmetatable" then
        result[func_id].use_getmetatable = 1
      end
    end
    check_varindex_in_exp(exp[1], "call", func_id)
    check_varindex_in_explist(exp[2], "se", func_id)
    check_exp(exp[1], func_name, func_id)
    check_explist(exp[2], func_name, func_id)
  elseif exp.tag == "ExpAdd" or -- ExpAdd Exp Exp 
         exp.tag == "ExpSub" or -- ExpSub Exp Exp
         exp.tag == "ExpMul" or -- ExpMul Exp Exp
         exp.tag == "ExpDiv" or -- ExpDiv Exp Exp
         exp.tag == "ExpMod" or -- ExpMod Exp Exp
         exp.tag == "ExpPow" or -- ExpPow Exp Exp
         exp.tag == "ExpConcat" or -- ExpConcat Exp Exp
         exp.tag == "ExpNE" or -- ExpNE Exp Exp
         exp.tag == "ExpEQ" or -- ExpEQ Exp Exp
         exp.tag == "ExpLT" or -- ExpLT Exp Exp
         exp.tag == "ExpLE" or -- ExpLE Exp Exp
         exp.tag == "ExpGT" or -- ExpGT Exp Exp
         exp.tag == "ExpGE" or -- ExpGE Exp Exp
         exp.tag == "ExpAnd" or -- ExpGE Exp Exp
         exp.tag == "ExpOr" then -- ExpOr Exp Exp
    check_varindex_in_exp(exp[1], "se", func_id)
    check_varindex_in_exp(exp[2], "se", func_id)
    check_exp(exp[1], func_name, func_id)
    check_exp(exp[2], func_name, func_id)
  elseif exp.tag == "ExpNot" or -- ExpNot Exp
         exp.tag == "ExpMinus" or -- ExpMinus Exp
         exp.tag == "ExpLen" then -- ExpLen Exp
    check_varindex_in_exp(exp[1], "se", func_id)
    check_exp(exp[1], func_name, func_id)
  else
    error("expecting an expression, but got a " .. exp.tag)
  end
end

count_stm = function (stm)
  -- StmBlock [Stm]
  if stm.tag == "StmBlock" then
    count_block(stm)
  -- StmIfElse Exp Stm Stm
  elseif stm.tag == "StmIfElse" then
    count_exp(stm[1])
    count_stm(stm[2])
    count_stm(stm[3])
  -- StmWhile Exp Stm
  elseif stm.tag == "StmWhile" then
    count_exp(stm[1])
    count_stm(stm[2])
  -- StmRepeat Stm Exp
  elseif stm.tag == "StmRepeat" then
    count_stm(stm[1])
    count_exp(stm[2])
  -- StmForNum ID Exp Exp Exp Stm
  elseif stm.tag == "StmForNum" then
    count_exp(stm[2])
    count_exp(stm[3])
    count_exp(stm[4])
    count_stm(stm[5])
  -- StmForGen [ID] [Exp] Stm
  elseif stm.tag == "StmForGen" then
    count_explist(stm[2])
    count_stm(stm[3])
  -- StmLocalFunction ID ([ID],IsVarArg) Stm
  elseif stm.tag == "StmLocalFunction" then
    number_of_functions = number_of_functions + 1
    localf = localf + 1
    count_stm(stm[3])
  -- StmFunction FuncName ([ID],IsVarArg) Stm
  elseif stm.tag == "StmFunction" then
    number_of_functions = number_of_functions + 1
    globalf = globalf + 1
    count_stm(stm[3])
  -- StmLabel ID
  elseif stm.tag == "StmLabel" then
  -- StmGoTo ID
  elseif stm.tag == "StmGoTo" then
  -- StmBreak
  elseif stm.tag == "StmBreak" then
  -- StmAssign [Var] [Exp]
  elseif stm.tag == "StmAssign" then
    count_varlist(stm[1])
    count_explist(stm[2])
  -- StmLocalVar [ID] [Exp]
  elseif stm.tag == "StmLocalVar" then
    count_explist(stm[2])
  -- StmRet [Exp]
  elseif stm.tag == "StmRet" then
    count_explist(stm[1])
  -- StmCall Exp
  elseif stm.tag == "StmCall" then
    count_exp(stm[1])
  else
    error("expecting a statement, but got a " .. stm.tag)
  end
end

check_stm = function (stm, func_name, func_id)
  if stm.tag == "StmBlock" then -- StmBlock [Stm]
    check_block(stm, func_name, func_id)
  elseif stm.tag == "StmIfElse" then -- StmIfElse Exp Stm Stm
    check_varindex_in_exp(stm[1], "se", func_id)
    check_exp(stm[1], func_name, func_id)
    check_stm(stm[2], func_name, func_id)
    check_stm(stm[3], func_name, func_id)
  elseif stm.tag == "StmWhile" then -- StmWhile Exp Stm
    check_varindex_in_exp(stm[1], "se", func_id)
    check_exp(stm[1], func_name, func_id)
    check_stm(stm[2], func_name, func_id)
  elseif stm.tag == "StmForNum" then -- StmForNum ID Exp Exp Exp Stm
    check_varindex_in_exp(stm[2], "se", func_id)
    check_varindex_in_exp(stm[3], "se", func_id)
    check_varindex_in_exp(stm[4], "se", func_id)
    check_exp(stm[2], func_name, func_id)
    check_exp(stm[3], func_name, func_id)
    check_exp(stm[4], func_name, func_id)
    check_stm(stm[5], func_name, func_id)
  elseif stm.tag == "StmForGen" then -- StmForGen [ID] [Exp] Stm
    check_varindex_in_explist(stm[2], "se", func_id)
    check_explist(stm[2], func_name, func_id)
    check_stm(stm[3], func_name, func_id)
  elseif stm.tag == "StmRepeat" then -- StmRepeat Stm Exp
    check_stm(stm[1], func_name, func_id)
    check_exp(stm[2], func_name, func_id)
  elseif stm.tag == "StmFunction" then -- StmFunction FuncName ParList Stm
    new_func_def("global")
    if stm[1].tag == "Method" then
      local id = result.number_of_functions
      -- statistics of the use of method definition
      result[id].is_method = 1
      -- statistics of the use of function declaration as table field
      result[id].table_field = 1
    elseif stm[1].tag == "Function" then
      local id = result.number_of_functions
      -- statistics of the use of method definition
      if #stm[2] > 0 then
        if stm[2][1] == "self" or stm[2][1] == "this" then
          result[id].is_method = 1
        end
        -- statistics of the use of function declaration as table field
        if #stm[2] > 1 then
          result[id].table_field = 1
        end
      end
    end
    check_stm(stm[3], func_name, result.number_of_functions)
  elseif stm.tag == "StmLocalFunction" then -- StmLocalFunction ID ParList Stm
    new_func_def("local")
    check_stm(stm[3], func_name, result.number_of_functions)
  elseif stm.tag == "StmLabel" or -- StmLabel ID
         stm.tag == "StmGoTo" or -- StmGoTo ID
         stm.tag == "StmBreak" then
  elseif stm.tag == "StmAssign" then -- StmAssign [Var] [Exp]
    check_varindex_in_varlist(stm[1], "se", func_id)
    check_varindex_in_explist(stm[2], "se", func_id)
    check_varlist(stm[1], func_name, func_id)
    check_explist(stm[2], func_name, func_id)
  elseif stm.tag == "StmLocalVar" then -- StmLocalVar [ID] [Exp]
    check_varindex_in_explist(stm[2], "se", func_id)
    check_explist(stm[2], func_name, func_id)
  elseif stm.tag == "StmRet" then -- StmRet [Exp]
    check_varindex_in_explist(stm[1], "se", func_id)
    -- statistics of the use of return
    local explist_size = #stm[1]
    if explist_size > 1 then
      if stm[1][1].tag == "ExpNil" then
        result[func_id].ret_nil_se = 1
      elseif stm[1][1].tag == "ExpFalse" then
        result[func_id].ret_false_se = 1
      end
    end
    check_explist(stm[1], func_name, func_id)
  elseif stm.tag == "StmCall" then -- StmCall Exp
    check_varindex_in_exp(stm[1], "call", func_id)
    check_exp(stm[1], func_name, func_id)
  else
    error("expecting a statement, but got a " .. stm.tag)
  end
end

count_block = function (block)
  if block.tag ~= "StmBlock" then
    error("expecting a block, but got a " .. block.tag)
  end
  for k,v in ipairs(block) do
    count_stm(v)
  end
end

check_block = function (block, func_name, func_id)
  if block.tag ~= "StmBlock" then
    error("expecting a block, but got a " .. block.tag)
  end
  for k,v in ipairs(block) do
    check_stm(v, func_name, func_id)
  end
end

local function count (ast)
  number_of_functions = 0
  anonymousf = 0
  globalf = 0
  localf = 0
  count_block(ast)
  print("number_of_functions", number_of_functions)
  print("anonymous", anonymousf)
  print("global", globalf)
  print("local", localf)
  if number_of_functions ~= anonymousf + globalf + localf then
    error("number of functions does not match")
  end
end

local function init_result ()
  result = {}
  result.number_of_functions = -1
  new_func_def("main")
end

local function count_recon ()
  result.anonymousf = 0
  result.globalf = 0
  result.localf = 0
  result.ret_nil_se = 0
  result.ret_false_se = 0
  result.use_type = 0
  result.use_type_recon = 0
  result.type_id = 0
  result.type_other = 0
  result.use_setmetatable = 0
  result.use_getmetatable = 0
  result.number_of_methods = 0
  result.table_field = 0
  result.empty_construct = 0
  result.only_static = 0
  result.only_dynamic = 0
  result.static_and_dynamic = 0
  result.varindex = 0
  result.varindex_literal = 0
  result.varindex_non_literal = 0
  result.old_varindex = 0
  result.varindex_function_call = 0
  result.varindex_somewhere_else = 0
  result.number_of_constructs = 0
  result.ret = result[0].ret
  for i=0,result.number_of_functions do
    if result[i].func_type == "A" then
      result.anonymousf = result.anonymousf + 1
    elseif result[i].func_type == "G" then
      result.globalf = result.globalf + 1
    elseif result[i].func_type == "L" then
      result.localf = result.localf + 1
    end
    result.ret_nil_se = result.ret_nil_se + result[i].ret_nil_se
    result.ret_false_se = result.ret_false_se + result[i].ret_false_se
    if result[i].use_type > 0 then
      result.use_type = result.use_type + 1
      result.use_type_recon = result.use_type_recon + result[i].use_type
      result.type_id = result.type_id + result[i].type_id
      result.type_other = result.type_other + result[i].type_other
    end
    result.use_setmetatable = result.use_setmetatable + result[i].use_setmetatable
    result.use_getmetatable = result.use_getmetatable + result[i].use_getmetatable
    result.number_of_methods = result.number_of_methods + result[i].is_method
    result.table_field = result.table_field + result[i].table_field
    result.number_of_constructs = result.number_of_constructs + result[i].number_of_constructs
    result.empty_construct = result.empty_construct + result[i].empty_construct
    result.only_static = result.only_static + result[i].only_static
    result.only_dynamic = result.only_dynamic + result[i].only_dynamic
    result.static_and_dynamic = result.static_and_dynamic + result[i].static_and_dynamic
    result.varindex = result.varindex + result[i].varindex
    result.varindex_literal = result.varindex_literal + result[i].varindex_literal
    result.varindex_non_literal = result.varindex_non_literal + result[i].varindex_non_literal
    result.old_varindex = result.old_varindex + result[i].old_varindex
    result.varindex_function_call = result.varindex_function_call + result[i].varindex_function_call
    result.varindex_somewhere_else = result.varindex_somewhere_else + result[i].varindex_somewhere_else
  end
  local sum = result.anonymousf + result.globalf + result.localf
  if result.number_of_functions ~= sum then
    error("number of functions does not match")
  end
  sum = result.type_id + result.type_other
  if result.use_type_recon ~= sum then
    error("number of the use of type does not match")
  end
  sum = result.empty_construct + result.only_static + result.only_dynamic + result.static_and_dynamic
  if result.number_of_constructs ~= sum then
    error("number of table constructs does not match")
  end
  sum = result.varindex_literal + result.varindex_non_literal
  if result.varindex ~= sum then
    error("number of varindex usage does not match")
  end
end

local function luac_recon (filename)
  local sum = 0
  local CMD = "luac -l '%s' | grep 'slots,' | cut -d',' -f6 | cut -d' ' -f2"
  local LUAC = string.format(CMD, filename)
  for i in io.popen(LUAC):lines() do
    sum = sum + i
  end
  os.remove("luac.out")
  if result.number_of_functions ~= sum then
    error("number of functions does not match with luac")
  end
end

local function check (ast)
  init_result()
  check_block(ast, "main", 0)
  if #ast > 0 and ast[#ast].tag == "StmRet" then
    result[0].ret = #ast[#ast][1]
  end
  count_recon()
end

function statistics.generate (filename)
  assert(type(filename) == "string")
  local ast,errormsg = parser.parse(filename)
  if not ast then
    error(errormsg)
  end
  check(ast)
  luac_recon(filename)
  return result
end

function statistics.print_header ()
  io.write("package,src_file,")
  io.write("func_id,func_type,")
  io.write("use_type,type_id,type_other,")
  io.write("is_method,")
  io.write("table_field,")
  io.write("ret_nil_se,ret_false_se,")
  io.write("use_setmetatable,use_getmetatable,")
  io.write("number_of_constructs,")
  io.write("empty_construct,only_static,only_dynamic,static_and_dynamic,")
  io.write("varindex,")
  io.write("varindex_literal,varindex_non_literal,")
  io.write("\n")
end

function statistics.print_result (filename, result)
  local t,i = {},0
  local package,src_file

  for w in string.gmatch(filename, "[^/]*/([^/]+)") do
    table.insert(t, w)
    if w == "unpack" then i = #t end
  end

  if i > 0 then
    package = t[i + 2]
    src_file = t[#t]
  else
    package = "no package"
    src_file = filename
  end

  for i=0,result.number_of_functions do
    io.write(string.format("%s,%s,", package, src_file))
    io.write(string.format("%d,%s,", i, result[i].func_type))
    io.write(string.format("%d,%d,%d,", result[i].use_type, result[i].type_id, result[i].type_other))
    io.write(string.format("%d,", result[i].is_method))
    io.write(string.format("%d,", result[i].table_field))
    io.write(string.format("%d,%d,", result[i].ret_nil_se, result[i].ret_false_se))
    io.write(string.format("%d,%d,", result[i].use_setmetatable, result[i].use_getmetatable))
    io.write(string.format("%d,", result[i].number_of_constructs))
    io.write(string.format("%d,", result[i].empty_construct))
    io.write(string.format("%d,", result[i].only_static))
    io.write(string.format("%d,", result[i].only_dynamic))
    io.write(string.format("%d,", result[i].static_and_dynamic))
    io.write(string.format("%d,", result[i].varindex))
    io.write(string.format("%d,", result[i].varindex_literal))
    io.write(string.format("%d,", result[i].varindex_non_literal))
    io.write("\n")
  end
end

function statistics.log_result (filename, result)
  -- fix values to be printed in the old style log
  if result[0].use_type > 0 then
    result.use_type = result.use_type - 1
  end
  if result[0].use_setmetatable > 0 then
    result.use_setmetatable = result.use_setmetatable - 1
  end
  if result[0].use_getmetatable > 0 then
    result.use_getmetatable = result.use_getmetatable - 1
  end
  if result[0].ret_nil_se > 0 then
    result.ret_nil_se = result.ret_nil_se - 1
  end
  if result[0].ret_false_se > 0 then
    result.ret_false_se = result.ret_false_se - 1
  end
  if result.ret then result.ret = true end
  print(filename)
  print("number_of_functions", result.number_of_functions)
  print("anonymous", result.anonymousf)
  print("global", result.globalf)
  print("local", result.localf)
  print("ret_nil_se", result.ret_nil_se)
  print("ret_false_se", result.ret_false_se)
  print("use_type", result.use_type)
  print("use_setmetatable", result.use_setmetatable)
  print("use_getmetatable", result.use_getmetatable)
  print("number_of_methods", result.number_of_methods)
  print("table_field", result.table_field)
  print("number_of_constructs", result.number_of_constructs)
  print("empty_construct", result.empty_construct)
  print("only_static", result.only_static)
  print("only_dynamic", result.only_dynamic)
  print("static_and_dynamic", result.static_and_dynamic)
  print("varindex", result.varindex)
  print("varindex_literal", result.varindex_literal)
  print("varindex_non_literal", result.varindex_non_literal)
  print("old_varindex", result.old_varindex)
  print("varindex_function_call", result.varindex_function_call)
  print("varindex_somewhere_else", result.varindex_somewhere_else)
  print("ret", result.ret)
end

return statistics
