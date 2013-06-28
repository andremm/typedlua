
local statistics = {}

local parser = require "parser"
local pp = require "pp"

local number_of_functions = 0
local anonymousf = 0
local globalf = 0
local localf = 0
local result = {}

local check_block, check_stm
local check_exp, check_var
local check_explist, check_varlist
local check_fieldlist

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
  result[n].varindex_literal_fc = 0
  result[n].varindex_non_literal = 0
  result[n].varindex_non_literal_fc = 0
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

check_explist = function (explist, func_name, func_id)
  for k,v in ipairs(explist) do
    check_exp(v, func_name, func_id)
  end
end

check_varlist = function (varlist, func_name, func_id)
  for k,v in ipairs(varlist) do
    check_var(v, func_name, func_id)
  end
end

local function is_literal (tag)
  if tag == "ExpNil" or
     tag == "ExpFalse" or
     tag == "ExpTrue" or
     tag == "ExpNum" or
     tag == "ExpStr" or
     tag == "ExpFunction" then
    return true
  end
  return false
end

check_var = function (var, func_name, func_id)
  if var.tag == "VarID" then -- VarID ID
  elseif var.tag == "VarIndex" then -- VarIndex Exp Exp
    -- statistics of the use of table indexing
    result[func_id].varindex = result[func_id].varindex + 1
    local tag = var[2].tag
   if is_literal(var[2].tag) then
      result[func_id].varindex_literal = result[func_id].varindex_literal + 1
    else
      result[func_id].varindex_non_literal = result[func_id].varindex_non_literal + 1
    end
    check_exp(var[1], func_name, func_id)
    check_exp(var[2], func_name, func_id)
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
    -- statistics of the use of table indexing
    result[func_id].varindex = result[func_id].varindex + 1
    result[func_id].varindex_literal = result[func_id].varindex_literal + 1
    result[func_id].varindex_literal_fc = result[func_id].varindex_literal_fc + 1
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
      -- statistics of the use of module
      elseif fname == "module" and func_id == 0 then
        result.calling_module = true
      end
    end
    -- statistics of the use of table indexing
    if exp[1].tag == "ExpVar" and exp[1][1].tag == "VarIndex" then
      if is_literal(exp[1][1][2].tag) then
        result[func_id].varindex_literal_fc = result[func_id].varindex_literal_fc + 1
      else
        result[func_id].varindex_non_literal_fc = result[func_id].varindex_non_literal_fc + 1
      end
    end
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
    check_exp(exp[1], func_name, func_id)
    check_exp(exp[2], func_name, func_id)
  elseif exp.tag == "ExpNot" or -- ExpNot Exp
         exp.tag == "ExpMinus" or -- ExpMinus Exp
         exp.tag == "ExpLen" then -- ExpLen Exp
    check_exp(exp[1], func_name, func_id)
  else
    error("expecting an expression, but got a " .. exp.tag)
  end
end

check_stm = function (stm, func_name, func_id)
  if stm.tag == "StmBlock" then -- StmBlock [Stm]
    check_block(stm, func_name, func_id)
  elseif stm.tag == "StmIfElse" then -- StmIfElse Exp Stm Stm
    check_exp(stm[1], func_name, func_id)
    check_stm(stm[2], func_name, func_id)
    check_stm(stm[3], func_name, func_id)
  elseif stm.tag == "StmWhile" then -- StmWhile Exp Stm
    check_exp(stm[1], func_name, func_id)
    check_stm(stm[2], func_name, func_id)
  elseif stm.tag == "StmForNum" then -- StmForNum ID Exp Exp Exp Stm
    check_exp(stm[2], func_name, func_id)
    check_exp(stm[3], func_name, func_id)
    check_exp(stm[4], func_name, func_id)
    check_stm(stm[5], func_name, func_id)
  elseif stm.tag == "StmForGen" then -- StmForGen [ID] [Exp] Stm
    check_explist(stm[2], func_name, func_id)
    check_stm(stm[3], func_name, func_id)
  elseif stm.tag == "StmRepeat" then -- StmRepeat Stm Exp
    check_stm(stm[1], func_name, func_id)
    check_exp(stm[2], func_name, func_id)
  elseif stm.tag == "StmFunction" then -- StmFunction FuncName ParList Stm
    new_func_def("global")
    local id = result.number_of_functions
    if stm[1].tag == "Method" then
      -- statistics of the use of method definition
      result[id].is_method = 1
      -- statistics of the use of function declaration as table field
      result[id].table_field = 1
    elseif stm[1].tag == "Function" then
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
    check_varlist(stm[1], func_name, func_id)
    check_explist(stm[2], func_name, func_id)
  elseif stm.tag == "StmLocalVar" then -- StmLocalVar [ID] [Exp]
    check_explist(stm[2], func_name, func_id)
  elseif stm.tag == "StmRet" then -- StmRet [Exp]
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
    check_exp(stm[1], func_name, func_id)
  else
    error("expecting a statement, but got a " .. stm.tag)
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

local function init_result ()
  result = {}
  result.number_of_functions = -1
  new_func_def("main")
end

local function result_recon ()
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
  result.varindex_literal_fc = 0
  result.varindex_non_literal = 0
  result.varindex_non_literal_fc = 0
  result.number_of_constructs = 0
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
    result.varindex_literal_fc = result.varindex_literal_fc + result[i].varindex_literal_fc
    result.varindex_non_literal = result.varindex_non_literal + result[i].varindex_non_literal
    result.varindex_non_literal_fc = result.varindex_non_literal_fc + result[i].varindex_non_literal_fc
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
    result.returning_module = true
  end
  result_recon()
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
  io.write("varindex_literal,varindex_literal_fc,")
  io.write("varindex_non_literal,varindex_non_literal_fc,")
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
    io.write(string.format("%d,", result[i].varindex_literal_fc))
    io.write(string.format("%d,", result[i].varindex_non_literal))
    io.write(string.format("%d,", result[i].varindex_non_literal_fc))
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
  print("varindex_literal_fc", result.varindex_literal_fc)
  print("varindex_non_literal", result.varindex_non_literal)
  print("varindex_non_literal_fc", result.varindex_non_literal_fc)
  print("returning_module", result.returning_module)
  print("calling_module", result.calling_module)
end

function statistics.init_merge ()
  local merge = {}
  merge.number_of_files = 0
  merge.number_of_functions = 0
  merge.anonymousf = 0
  merge.globalf = 0
  merge.localf = 0
  merge.ret_nil_se = 0
  merge.ret_false_se = 0
  merge.use_type = 0
  merge.type_id = 0
  merge.type_other = 0
  merge.use_setmetatable = 0
  merge.use_getmetatable = 0
  merge.number_of_methods = 0
  merge.table_field = 0
  merge.number_of_constructs = 0
  merge.empty_construct = 0
  merge.only_static = 0
  merge.only_dynamic = 0
  merge.static_and_dynamic = 0
  merge.varindex = 0
  merge.varindex_literal = 0
  merge.varindex_literal_fc = 0
  merge.varindex_non_literal = 0
  merge.varindex_non_literal_fc = 0
  merge.returning_module = 0
  merge.calling_module = 0
  merge.module_and_return = 0
  merge.plain_script = 0
  return merge
end

function statistics.merge (result, merge)
  merge.number_of_files = merge.number_of_files + 1
  merge.number_of_functions = merge.number_of_functions + result.number_of_functions
  merge.anonymousf = merge.anonymousf + result.anonymousf
  merge.globalf = merge.globalf + result.globalf
  merge.localf = merge.localf + result.localf
  merge.ret_nil_se = merge.ret_nil_se + result.ret_nil_se
  merge.ret_false_se = merge.ret_false_se + result.ret_false_se
  merge.use_type = merge.use_type + result.use_type
  if result.use_setmetatable > 0 then
    merge.use_setmetatable = merge.use_setmetatable + 1
  end
  if result.use_getmetatable > 0 then
    merge.use_getmetatable = merge.use_getmetatable + 1
  end
  merge.number_of_methods = merge.number_of_methods + result.number_of_methods
  merge.table_field = merge.table_field + result.table_field
  merge.number_of_constructs = merge.number_of_constructs + result.number_of_constructs
  merge.empty_construct = merge.empty_construct + result.empty_construct
  merge.only_static = merge.only_static + result.only_static
  merge.only_dynamic = merge.only_dynamic + result.only_dynamic
  merge.static_and_dynamic = merge.static_and_dynamic + result.static_and_dynamic
  merge.varindex = merge.varindex + result.varindex
  merge.varindex_literal = merge.varindex_literal + result.varindex_literal
  merge.varindex_literal_fc = merge.varindex_literal_fc + result.varindex_literal_fc
  merge.varindex_non_literal = merge.varindex_non_literal + result.varindex_non_literal
  merge.varindex_non_literal_fc = merge.varindex_non_literal_fc + result.varindex_non_literal_fc
  if result.returning_module and result.calling_module then
    merge.module_and_return = merge.module_and_return + 1
  elseif result.returning_module then
    merge.returning_module = merge.returning_module + 1
  elseif result.calling_module then
    merge.calling_module = merge.calling_module + 1
  else
    merge.plain_script = merge.plain_script + 1
  end
end

function statistics.log_merge (merge)
  print("MERGED RESULTS")
  print("number_of_files", merge.number_of_files)
  print("number_of_functions", merge.number_of_functions)
  print("anonymous", merge.anonymousf)
  print("global", merge.globalf)
  print("local", merge.localf)
  print("ret_nil_se", merge.ret_nil_se)
  print("ret_false_se", merge.ret_false_se)
  print("use_type", merge.use_type)
  print("use_setmetatable", merge.use_setmetatable)
  print("use_getmetatable", merge.use_getmetatable)
  print("number_of_methods", merge.number_of_methods)
  print("table_field", merge.table_field)
  print("number_of_constructs", merge.number_of_constructs)
  print("empty_construct", merge.empty_construct)
  print("only_static", merge.only_static)
  print("only_dynamic", merge.only_dynamic)
  print("static_and_dynamic", merge.static_and_dynamic)
  print("varindex", merge.varindex)
  print("varindex_literal", merge.varindex_literal)
  print("varindex_literal_fc", merge.varindex_literal_fc)
  print("varindex_non_literal", merge.varindex_non_literal)
  print("varindex_non_literal_fc", merge.varindex_non_literal_fc)
  print("returning_module", merge.returning_module)
  print("calling_module", merge.calling_module)
  print("module_and_return", merge.module_and_return)
  print("plain_script", merge.plain_script)
end

return statistics
