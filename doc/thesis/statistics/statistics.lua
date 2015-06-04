
local statistics = {}

local parser = require "parser"

local number_of_functions = 0
local anonymousf = 0
local globalf = 0
local localf = 0
local result = {}

local check_block, check_stm
local check_exp, check_var
local check_explist, check_varlist
local check_fieldlist

local metamethods = {
  __add = true,
  __call = true,
  __concat = true,
  __div = true,
  __eq = true,
  __gc = true,
  __index = true,
  __ipairs = true,
  __le = true,
  __len = true,
  __lt = true,
  __metatable = true,
  __mod = true,
  __mode = true,
  __mul = true,
  __newindex = true,
  __pairs = true,
  __pow = true,
  __sub = true,
  __tostring = true,
  __unm = true,
}

local function new_func_def (func_type, is_vararg)
  result.number_of_functions = result.number_of_functions + 1
  local n = result.number_of_functions
  result[n] = {}
  result[n].is_vararg = is_vararg and 1 or 0
  result[n].use_type = 0
  result[n].type_count = 0
  result[n].type_id = 0
  result[n].type_other = 0
  result[n].is_method = 0
  result[n].method_colon = 0
  result[n].method_self = 0
  result[n].method_this = 0
  result[n].table_field = 0
  result[n].ret_nil_se = 0
  result[n].ret_false_se = 0
  result[n].ret_mult_values = 0
  result[n].use_setmetatable = 0
  result[n].use_getmetatable = 0
  result[n].use_ipairs = 0
  result[n].ipairs_count = 0
  result[n].use_pairs = 0
  result[n].pairs_count = 0
  result[n].use_len = 0
  result[n].len_count = 0
  result[n].number_of_constructs = 0
  result[n].empty_construct = 0
  result[n].only_static = 0
  result[n].only_dynamic = 0
  result[n].static_and_dynamic = 0
  result[n].table_list = 0
  result[n].table_record = 0
  result[n].table_list_and_record = 0
  result[n].table_boolean = 0
  result[n].table_vararg = 0
  result[n].varindex = 0
  result[n].varindex_read = 0
  result[n].varindex_write = 0
  result[n].varindex_read_literal = 0
  result[n].varindex_read_boolean = 0
  result[n].varindex_read_number = 0
  result[n].varindex_read_string = 0
  result[n].varindex_read_non_literal = 0
  result[n].varindex_read_literal_fc = 0
  result[n].varindex_read_literal_mc = 0
  result[n].varindex_read_non_literal_fc = 0
  result[n].varindex_write_literal = 0
  result[n].varindex_write_boolean = 0
  result[n].varindex_write_number = 0
  result[n].varindex_write_string = 0
  result[n].varindex_write_non_literal = 0
  for k, v in pairs(metamethods) do
    result[n][k] = 0
  end
  result[n].assignments = 0
  result[n].simple_assignments = 0
  result[n].multiple_assignments = 0
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

function check_fieldlist (fieldlist, func_name, func_id)
  local id = func_id
  local only_static,only_dynamic = true,true
  local list,record,boolean = false,false,false
  -- statistics of the use of table constructs
  if #fieldlist[1] == 0 and #fieldlist[2] == 0 then
    result[func_id].empty_construct = result[func_id].empty_construct + 1
  elseif #fieldlist[1] > 0 and #fieldlist[2] == 0 then
    result[func_id].only_static = result[func_id].only_static + 1
    result[func_id].table_list = result[func_id].table_list + 1
    if #fieldlist[1] == 1 and fieldlist[1][1][1].tag == "ExpDots" then
      result[func_id].table_vararg = result[func_id].table_vararg + 1
    end
  elseif #fieldlist[1] == 0 and #fieldlist[2] > 0 then
    for k,v in ipairs(fieldlist[2]) do
      if v[1].tag == "ExpNum" then
        only_dynamic = false
        list = true
      elseif v[1].tag == "ExpStr" then
        only_dynamic = false
        record = true
        local m = v[1][1]
        if metamethods[m] then result[func_id][m] = result[func_id][m] + 1 end
      elseif v[1].tag == "ExpTrue" or
             v[1].tag == "ExpFalse" then
        only_dynamic = false
        list = false
        record = false
        boolean = true
      else
        only_static = false
      end
    end
    if only_static then
      result[func_id].only_static = result[func_id].only_static + 1
      if list and record then
        result[func_id].table_list_and_record = result[func_id].table_list_and_record + 1
      elseif list and not record then
        result[func_id].table_list = result[func_id].table_list + 1
      elseif record and not list then
        result[func_id].table_record = result[func_id].table_record + 1
      elseif boolean then
        result[func_id].table_boolean = result[func_id].table_boolean + 1
      end
    elseif only_dynamic then
      result[func_id].only_dynamic = result[func_id].only_dynamic + 1
    else
      result[func_id].static_and_dynamic = result[func_id].static_and_dynamic + 1
    end
  elseif #fieldlist[1] > 0 and #fieldlist[2] > 0 then
    only_dynamic = false
    list = true
    for k,v in ipairs(fieldlist[2]) do
      if v[1].tag == "ExpNum" then
        only_dynamic = false
        list = true
      elseif v[1].tag == "ExpStr" then
        only_dynamic = false
        record = true
        local m = v[1][1]
        if metamethods[m] then result[func_id][m] = result[func_id][m] + 1 end
      elseif v[1].tag == "ExpTrue" or
             v[1].tag == "ExpFalse" then
        only_dynamic = false
        list = false
        record = false
        boolean = true
      else
        only_static = false
      end
    end
    if only_static then
      result[func_id].only_static = result[func_id].only_static + 1
      if list and record then
        result[func_id].table_list_and_record = result[func_id].table_list_and_record + 1
      elseif list and not record then
        result[func_id].table_list = result[func_id].table_list + 1
      elseif record and not list then
        result[func_id].table_record = result[func_id].table_record + 1
      elseif boolean then
        result[func_id].table_boolean = result[func_id].table_boolean + 1
      end
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

function check_explist (explist, func_name, func_id)
  for k,v in ipairs(explist) do
    check_exp(v, func_name, func_id)
  end
end

function check_varlist (varlist, func_name, func_id)
  for k,v in ipairs(varlist) do
    check_var(v, func_name, func_id, "write")
  end
end

local function is_literal (tag)
  if tag == "ExpNil" or
     tag == "ExpFalse" or
     tag == "ExpTrue" or
     tag == "ExpNum" or
     tag == "ExpStr" then
    return true
  end
  return false
end

function check_var (var, func_name, func_id, read_or_write)
  if var.tag == "VarID" then -- VarID Name
  elseif var.tag == "VarIndex" then -- VarIndex Exp Exp
    -- statistics of the use of table indexing
    result[func_id].varindex = result[func_id].varindex + 1
    local tag = var[2].tag
    if read_or_write == "read" then
      result[func_id].varindex_read = result[func_id].varindex_read + 1
      if is_literal(tag) then
        result[func_id].varindex_read_literal = result[func_id].varindex_read_literal + 1
       if tag == "ExpTrue" or tag == "ExpFalse" then
          result[func_id].varindex_read_boolean = result[func_id].varindex_read_boolean + 1
       elseif tag == "ExpNum" then
          result[func_id].varindex_read_number = result[func_id].varindex_read_number + 1
        elseif tag == "ExpStr" then
          result[func_id].varindex_read_string = result[func_id].varindex_read_string + 1
        end
      else
        result[func_id].varindex_read_non_literal = result[func_id].varindex_read_non_literal + 1
      end
    elseif read_or_write == "write" then
      result[func_id].varindex_write = result[func_id].varindex_write + 1
      if is_literal(tag) then
        result[func_id].varindex_write_literal = result[func_id].varindex_write_literal + 1
       if tag == "ExpTrue" or tag == "ExpFalse" then
          result[func_id].varindex_write_boolean = result[func_id].varindex_write_boolean + 1
       elseif tag == "ExpNum" then
          result[func_id].varindex_write_number = result[func_id].varindex_write_number + 1
        elseif tag == "ExpStr" then
          result[func_id].varindex_write_string = result[func_id].varindex_write_string + 1
          local m = var[2][1]
          if metamethods[m] then result[func_id][m] = result[func_id][m] + 1 end
        end
      else
        result[func_id].varindex_write_non_literal = result[func_id].varindex_write_non_literal + 1
      end
    end
    check_exp(var[1], func_name, func_id)
    check_exp(var[2], func_name, func_id)
  end
end

function check_exp (exp, func_name, func_id)
  if exp.tag == "ExpNil" or
     exp.tag == "ExpFalse" or
     exp.tag == "ExpTrue" or
     exp.tag == "ExpDots" or
     exp.tag == "ExpNum" or -- ExpNum Double
     exp.tag == "ExpStr" then -- ExpStr String
  elseif exp.tag == "ExpVar" then -- ExpVar Var
    check_var(exp[1], func_name, func_id, "read")
  elseif exp.tag == "ExpFunction" then -- ExpFunction [Name] Stm
    new_func_def("anonymous", exp[1].is_vararg)
    local id = result.number_of_functions
    if #exp[1] > 0 then
      if exp[1][1] == "self" then
        result[id].is_method = 1
        result[id].method_self = 1
      elseif exp[1][1] == "this" then
        result[id].is_method = 1
        result[id].method_this = 1
      end
    end
    check_stm(exp[2], func_name, result.number_of_functions)
  elseif exp.tag == "ExpTableConstructor" then -- ExpTableConstructor FieldList
    result[func_id].number_of_constructs = result[func_id].number_of_constructs + 1
    check_fieldlist(exp[1], func_name, func_id)
  elseif exp.tag == "ExpMethodCall" then -- ExpMethodCall Exp [Exp]
    -- statistics of the use of table indexing
    result[func_id].varindex = result[func_id].varindex + 1
    result[func_id].varindex_read = result[func_id].varindex_read + 1
    result[func_id].varindex_read_literal = result[func_id].varindex_read_literal + 1
    result[func_id].varindex_read_string = result[func_id].varindex_read_string + 1
    result[func_id].varindex_read_literal_mc = result[func_id].varindex_read_literal_mc + 1
    check_exp(exp[1][1][1], func_name, func_id)
    check_explist(exp[2], func_name, func_id)
  elseif exp.tag == "ExpFunctionCall" then -- ExpFunctionCall Exp [Exp]
    if exp[1].tag == "ExpVar" and exp[1][1].tag == "VarID" then
      -- statistics of the use of type
      local fname = exp[1][1][1]
      if fname == "type" then
        result[func_id].use_type = result[func_id].use_type + 1
        result[func_id].type_count = result[func_id].type_count + 1
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
      -- statistics of the use of ipairs
      elseif fname == "ipairs" then
        result[func_id].ipairs_count = result[func_id].ipairs_count + 1
      -- statistics of the use of pairs
      elseif fname == "pairs" then
        result[func_id].pairs_count = result[func_id].pairs_count + 1
      -- statistics of the use of module
      elseif fname == "module" and func_id == 0 then
        result.calling_module = true
      end
    end
    -- statistics of the use of table indexing
    if exp[1].tag == "ExpVar" and exp[1][1].tag == "VarIndex" then
      if is_literal(exp[1][1][2].tag) then
        result[func_id].varindex_read_literal_fc = result[func_id].varindex_read_literal_fc + 1
      else
        result[func_id].varindex_read_non_literal_fc = result[func_id].varindex_read_non_literal_fc + 1
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

function check_stm (stm, func_name, func_id)
  if stm.tag == "StmBlock" then -- StmBlock [Stm]
    check_block(stm, func_name, func_id)
  elseif stm.tag == "StmIfElse" then -- StmIfElse Exp Stm Stm
    check_exp(stm[1], func_name, func_id)
    check_stm(stm[2], func_name, func_id)
    check_stm(stm[3], func_name, func_id)
  elseif stm.tag == "StmWhile" then -- StmWhile Exp Stm
    check_exp(stm[1], func_name, func_id)
    check_stm(stm[2], func_name, func_id)
  elseif stm.tag == "StmForNum" then -- StmForNum Name Exp Exp Exp Stm
    check_exp(stm[2], func_name, func_id)
    check_exp(stm[3], func_name, func_id)
    check_exp(stm[4], func_name, func_id)
    check_stm(stm[5], func_name, func_id)
    if stm[3].tag == "ExpLen" then
      result[func_id].len_count = result[func_id].len_count + 1
    end
  elseif stm.tag == "StmForGen" then -- StmForGen [Name] [Exp] Stm
    check_explist(stm[2], func_name, func_id)
    check_stm(stm[3], func_name, func_id)
  elseif stm.tag == "StmRepeat" then -- StmRepeat Stm Exp
    check_stm(stm[1], func_name, func_id)
    check_exp(stm[2], func_name, func_id)
  elseif stm.tag == "StmFunction" then -- StmFunction FuncName [Name] Stm
    new_func_def("global", stm[2].is_vararg)
    local id = result.number_of_functions
    local len = #stm[1]
    if stm[1].tag == "Method" then
      -- statistics of the use of method definition
      result[id].is_method = 1
      result[id].method_colon = 1
      -- statistics of the use of function declaration as table field
      result[id].table_field = 1
      local read = len - 2
      result[id].varindex = result[id].varindex + read + 1
      result[id].varindex_read = result[id].varindex_read + read
      result[id].varindex_read_literal = result[id].varindex_read_literal + read
      result[id].varindex_read_string = result[id].varindex_read_string + read
      result[id].varindex_write = result[id].varindex_write + 1
      result[id].varindex_write_literal = result[id].varindex_write_literal + 1
      result[id].varindex_write_string = result[id].varindex_write_string + 1
      local m = stm[1][len]
      if metamethods[m] then result[id][m] = result[id][m] + 1 end
    elseif stm[1].tag == "Function" then
      -- statistics of the use of method definition
      if #stm[2] > 0 then
        if stm[2][1] == "self" then
          result[id].is_method = 1
          result[id].method_self = 1
          result[id].table_field = 1
        elseif stm[2][1] == "this" then
          result[id].is_method = 1
          result[id].method_this = 1
          result[id].table_field = 1
        end
     end
      -- statistics of the use of function declaration as table field
      if len > 1  then
        result[id].table_field = 1
        local read = len - 2
        result[id].varindex = result[id].varindex + read + 1
        result[id].varindex_read = result[id].varindex_read + read
        result[id].varindex_read_literal = result[id].varindex_read_literal + read
        result[id].varindex_read_string = result[id].varindex_read_string + read
        result[id].varindex_write = result[id].varindex_write + 1
        result[id].varindex_write_literal = result[id].varindex_write_literal + 1
        result[id].varindex_write_string = result[id].varindex_write_string + 1
        local m = stm[1][len]
        if metamethods[m] then result[id][m] = result[id][m] + 1 end
      end
    end
    check_stm(stm[3], func_name, result.number_of_functions)
  elseif stm.tag == "StmLocalFunction" then -- StmLocalFunction Name [Name] Stm
    new_func_def("local", stm[2].is_vararg)
    check_stm(stm[3], func_name, result.number_of_functions)
  elseif stm.tag == "StmLabel" or -- StmLabel Name
         stm.tag == "StmGoTo" or -- StmGoTo Name
         stm.tag == "StmBreak" then
  elseif stm.tag == "StmAssign" then -- StmAssign [Var] [Exp]
    result[func_id].assignments = result[func_id].assignments + 1
    if #stm[1] == 1 then
      result[func_id].simple_assignments = result[func_id].simple_assignments + 1
    else
      result[func_id].multiple_assignments = result[func_id].multiple_assignments + 1
    end
    check_varlist(stm[1], func_name, func_id)
    check_explist(stm[2], func_name, func_id)
  elseif stm.tag == "StmLocalVar" then -- StmLocalVar [Name] [Exp]
    check_explist(stm[2], func_name, func_id)
  elseif stm.tag == "StmRet" then -- StmRet [Exp]
    -- statistics of the use of return
    local explist_size = #stm[1]
    if explist_size > 1 then
      result[func_id].ret_mult_values = 1
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

function check_block (block, func_name, func_id)
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
  result.is_vararg = 0
  result.ret_nil_se = 0
  result.ret_false_se = 0
  result.ret_mult_values = 0
  result.use_type = 0
  result.type_count = 0
  result.use_type_recon = 0
  result.type_id = 0
  result.type_other = 0
  result.use_setmetatable = 0
  result.use_getmetatable = 0
  result.use_ipairs = 0
  result.ipairs_count = 0
  result.use_pairs = 0
  result.pairs_count = 0
  result.use_len = 0
  result.len_count = 0
  result.number_of_methods = 0
  result.method_colon = 0
  result.method_self = 0
  result.method_this = 0
  result.table_field = 0
  result.empty_construct = 0
  result.only_static = 0
  result.only_dynamic = 0
  result.static_and_dynamic = 0
  result.table_list = 0
  result.table_record = 0
  result.table_list_and_record = 0
  result.table_boolean = 0
  result.table_vararg = 0
  result.varindex = 0
  result.varindex_read = 0
  result.varindex_write = 0
  result.varindex_read_literal = 0
  result.varindex_read_boolean = 0
  result.varindex_read_number = 0
  result.varindex_read_string = 0
  result.varindex_read_non_literal = 0
  result.varindex_read_literal_fc = 0
  result.varindex_read_literal_mc = 0
  result.varindex_read_non_literal_fc = 0
  result.varindex_write_literal = 0
  result.varindex_write_boolean = 0
  result.varindex_write_number = 0
  result.varindex_write_string = 0
  result.varindex_write_non_literal = 0
  for k, v in pairs(metamethods) do
    result[k] = 0
  end
  result.number_of_constructs = 0
  result.assignments = 0
  result.simple_assignments = 0
  result.multiple_assignments = 0
  for i=0,result.number_of_functions do
    if result[i].func_type == "A" then
      result.anonymousf = result.anonymousf + 1
    elseif result[i].func_type == "G" then
      result.globalf = result.globalf + 1
    elseif result[i].func_type == "L" then
      result.localf = result.localf + 1
    end
    result.is_vararg = result.is_vararg + result[i].is_vararg
    result.ret_nil_se = result.ret_nil_se + result[i].ret_nil_se
    result.ret_false_se = result.ret_false_se + result[i].ret_false_se
    result.ret_mult_values = result.ret_mult_values + result[i].ret_mult_values
    if result[i].use_type > 0 then
      result.use_type = result.use_type + 1
      result.type_count = result.type_count + result[i].type_count
      result.use_type_recon = result.use_type_recon + result[i].use_type
      result.type_id = result.type_id + result[i].type_id
      result.type_other = result.type_other + result[i].type_other
    end
    result.use_setmetatable = result.use_setmetatable + result[i].use_setmetatable
    result.use_getmetatable = result.use_getmetatable + result[i].use_getmetatable
    if result[i].ipairs_count > 0 then
      result.use_ipairs = 1
      result.ipairs_count = result.ipairs_count + result[i].ipairs_count
    end
    if result[i].pairs_count > 0 then
      result.use_pairs = 1
      result.pairs_count = result.pairs_count + result[i].pairs_count
    end
    if result[i].len_count > 0 then
      result.use_len = 1
      result.len_count = result.len_count + result[i].len_count
    end
    result.number_of_methods = result.number_of_methods + result[i].is_method
    result.method_colon = result.method_colon + result[i].method_colon
    result.method_self = result.method_self + result[i].method_self
    result.method_this = result.method_this + result[i].method_this
    result.table_field = result.table_field + result[i].table_field
    result.number_of_constructs = result.number_of_constructs + result[i].number_of_constructs
    result.empty_construct = result.empty_construct + result[i].empty_construct
    result.only_static = result.only_static + result[i].only_static
    result.only_dynamic = result.only_dynamic + result[i].only_dynamic
    result.static_and_dynamic = result.static_and_dynamic + result[i].static_and_dynamic
    result.table_list = result.table_list + result[i].table_list
    result.table_record = result.table_record + result[i].table_record
    result.table_list_and_record = result.table_list_and_record + result[i].table_list_and_record
    result.table_boolean = result.table_boolean + result[i].table_boolean
    result.table_vararg = result.table_vararg + result[i].table_vararg
    result.varindex = result.varindex + result[i].varindex
    result.varindex_read = result.varindex_read + result[i].varindex_read
    result.varindex_write = result.varindex_write + result[i].varindex_write
    result.varindex_read_literal = result.varindex_read_literal + result[i].varindex_read_literal
    result.varindex_read_boolean = result.varindex_read_boolean + result[i].varindex_read_boolean
    result.varindex_read_number = result.varindex_read_number + result[i].varindex_read_number
    result.varindex_read_string = result.varindex_read_string + result[i].varindex_read_string
    result.varindex_read_non_literal = result.varindex_read_non_literal + result[i].varindex_read_non_literal
    result.varindex_read_literal_fc = result.varindex_read_literal_fc + result[i].varindex_read_literal_fc
    result.varindex_read_literal_mc = result.varindex_read_literal_mc + result[i].varindex_read_literal_mc
    result.varindex_read_non_literal_fc = result.varindex_read_non_literal_fc + result[i].varindex_read_non_literal_fc
    result.varindex_write_literal = result.varindex_write_literal + result[i].varindex_write_literal
    result.varindex_write_boolean = result.varindex_write_boolean + result[i].varindex_write_boolean
    result.varindex_write_number = result.varindex_write_number + result[i].varindex_write_number
    result.varindex_write_string = result.varindex_write_string + result[i].varindex_write_string
    result.varindex_write_non_literal = result.varindex_write_non_literal + result[i].varindex_write_non_literal
    for k, v in pairs(metamethods) do
      result[k] = result[k] + result[i][k]
    end
    result.assignments = result.assignments + result[i].assignments
    result.simple_assignments = result.simple_assignments + result[i].simple_assignments
    result.multiple_assignments = result.multiple_assignments + result[i].multiple_assignments
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
  sum = result.varindex_read + result.varindex_write
  if result.varindex ~= sum then
    error("number of varindex usage does not match")
  end
  sum = result.simple_assignments + result.multiple_assignments
  if result.assignments ~= sum then
    error("number of assignments does not match")
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
  local ast,errormsg = parser.parse_from_file(filename)
  if not ast then
    error(errormsg)
  end
  check(ast)
  luac_recon(filename)
  return result
end

function statistics.log_result (filename, result)
  -- fix values to be printed in the old style log
  if result[0].use_type > 0 then
    result.use_type = result.use_type - 1
  end
  if result[0].ret_nil_se > 0 then
    result.ret_nil_se = result.ret_nil_se - 1
  end
  if result[0].ret_false_se > 0 then
    result.ret_false_se = result.ret_false_se - 1
  end
  if result[0].ret_mult_values > 0 then
    result.ret_mult_values = result.ret_mult_values - 1
  end
  if result.ret then result.ret = true end
  print(filename)
  print("number_of_functions", result.number_of_functions)
  print("anonymous", result.anonymousf)
  print("global", result.globalf)
  print("local", result.localf)
  print("is_vararg", result.is_vararg)
  print("ret_nil_se", result.ret_nil_se)
  print("ret_false_se", result.ret_false_se)
  print("ret_mult_values", result.ret_mult_values)
  print("use_type", result.use_type)
  print("type_count", result.type_count)
  print("use_setmetatable", result.use_setmetatable)
  print("use_getmetatable", result.use_getmetatable)
  print("use_ipairs", result.use_ipairs)
  print("ipairs_count", result.ipairs_count)
  print("use_pairs", result.use_pairs)
  print("pairs_count", result.pairs_count)
  print("use_len", result.use_len)
  print("len_count", result.len_count)
  print("number_of_methods", result.number_of_methods)
  print("method_colon", result.method_colon)
  print("method_self", result.method_self)
  print("method_this", result.method_this)
  print("table_field", result.table_field)
  print("number_of_constructs", result.number_of_constructs)
  print("empty_construct", result.empty_construct)
  print("only_static", result.only_static)
  print("only_dynamic", result.only_dynamic)
  print("static_and_dynamic", result.static_and_dynamic)
  print("table_list", result.table_list)
  print("table_record", result.table_record)
  print("table_list_and_record", result.table_list_and_record)
  print("table_boolean", result.table_boolean)
  print("table_vararg", result.table_vararg)
  print("varindex", result.varindex)
  print("varindex_read", result.varindex_read)
  print("varindex_write", result.varindex_write)
  print("varindex_read_literal", result.varindex_read_literal)
  print("varindex_read_boolean", result.varindex_read_boolean)
  print("varindex_read_number", result.varindex_read_number)
  print("varindex_read_string", result.varindex_read_string)
  print("varindex_read_non_literal", result.varindex_read_non_literal)
  print("varindex_read_literal_fc", result.varindex_read_literal_fc)
  print("varindex_read_literal_mc", result.varindex_read_literal_mc)
  print("varindex_read_non_literal_fc", result.varindex_read_non_literal_fc)
  print("varindex_write_literal", result.varindex_write_literal)
  print("varindex_write_boolean", result.varindex_write_boolean)
  print("varindex_write_number", result.varindex_write_number)
  print("varindex_write_string", result.varindex_write_string)
  print("varindex_write_non_literal", result.varindex_write_non_literal)
  for k, v in pairs(metamethods) do
    print(k, result[k])
  end
  print("returning_module", result.returning_module)
  print("calling_module", result.calling_module)
  print("assignments", result.assignments)
  print("simple_assignments", result.simple_assignments)
  print("multiple_assignments", result.multiple_assignments)
end

function statistics.init_merge ()
  local merge = {}
  merge.number_of_files = 0
  merge.number_of_projects = 0
  merge.project = {}
  merge.number_of_functions = 0
  merge.anonymousf = 0
  merge.globalf = 0
  merge.localf = 0
  merge.is_vararg = 0
  merge.ret_nil_se = 0
  merge.ret_false_se = 0
  merge.ret_mult_values = 0
  merge.use_type = 0
  merge.type_count = 0
  merge.type_id = 0
  merge.type_other = 0
  merge.use_setmetatable = 0
  merge.prj_setmetatable = 0
  merge.use_getmetatable = 0
  merge.prj_getmetatable = 0
  merge.use_ipairs = 0
  merge.ipairs_count = 0
  merge.use_pairs = 0
  merge.pairs_count = 0
  merge.use_len = 0
  merge.len_count = 0
  merge.number_of_methods = 0
  merge.prj_meth = 0
  merge.prj_colon = 0
  merge.prj_self = 0
  merge.prj_set_meth = 0
  merge.prj_set_colon = 0
  merge.prj_set_self = 0
  merge.method_colon = 0
  merge.method_self = 0
  merge.method_this = 0
  merge.table_field = 0
  merge.number_of_constructs = 0
  merge.empty_construct = 0
  merge.only_static = 0
  merge.only_dynamic = 0
  merge.static_and_dynamic = 0
  merge.table_list = 0
  merge.table_record = 0
  merge.table_list_and_record = 0
  merge.table_boolean = 0
  merge.table_vararg = 0
  merge.varindex = 0
  merge.varindex_read = 0
  merge.varindex_write = 0
  merge.varindex_read_literal = 0
  merge.varindex_read_boolean = 0
  merge.varindex_read_number = 0
  merge.varindex_read_string = 0
  merge.varindex_read_non_literal = 0
  merge.varindex_read_literal_fc = 0
  merge.varindex_read_literal_mc = 0
  merge.varindex_read_non_literal_fc = 0
  merge.varindex_write_literal = 0
  merge.varindex_write_boolean = 0
  merge.varindex_write_number = 0
  merge.varindex_write_string = 0
  merge.varindex_write_non_literal = 0
  for k, v in pairs(metamethods) do
    merge[k] = 0
  end
  merge.returning_module = 0
  merge.calling_module = 0
  merge.module_and_return = 0
  merge.plain_script = 0
  merge.assignments = 0
  merge.simple_assignments = 0
  merge.multiple_assignments = 0
  return merge
end

function statistics.merge (result, merge, project)
  merge.number_of_files = merge.number_of_files + 1
  if not merge.project[project] then
    merge.project[project] = {}
    merge.number_of_projects = merge.number_of_projects + 1
  end
  merge.number_of_functions = merge.number_of_functions + result.number_of_functions
  merge.anonymousf = merge.anonymousf + result.anonymousf
  merge.globalf = merge.globalf + result.globalf
  merge.localf = merge.localf + result.localf
  merge.is_vararg = merge.is_vararg + result.is_vararg
  merge.ret_nil_se = merge.ret_nil_se + result.ret_nil_se
  merge.ret_false_se = merge.ret_false_se + result.ret_false_se
  merge.ret_mult_values = merge.ret_mult_values + result.ret_mult_values
  merge.use_type = merge.use_type + result.use_type
  merge.type_count = merge.type_count + result.type_count
  if result.use_setmetatable > 0 then
    merge.use_setmetatable = merge.use_setmetatable + 1
    if not merge.project[project].use_setmetatable then
      merge.project[project].use_setmetatable = true
      merge.prj_setmetatable = merge.prj_setmetatable + 1
    end
  end
  if result.use_getmetatable > 0 then
    merge.use_getmetatable = merge.use_getmetatable + 1
    if not merge.project[project].use_getmetatable then
      merge.project[project].use_getmetatable = true
      merge.prj_getmetatable = merge.prj_getmetatable + 1
    end
  end
  if result.use_ipairs > 0 then
    merge.use_ipairs = merge.use_ipairs + 1
  end
  if result.use_pairs > 0 then
    merge.use_pairs = merge.use_pairs + 1
  end
  if result.use_len > 0 then
    merge.use_len = merge.use_len + 1
  end
  merge.ipairs_count = merge.ipairs_count + result.ipairs_count
  merge.pairs_count = merge.pairs_count + result.pairs_count
  merge.len_count = merge.len_count + result.len_count
  merge.number_of_methods = merge.number_of_methods + result.number_of_methods
  merge.method_colon = merge.method_colon + result.method_colon
  if result.method_colon > 0 then
    if not merge.project[project].use_colon then
      merge.project[project].use_colon = true
      merge.prj_colon = merge.prj_colon + 1
      if merge.project[project].use_self and
         not merge.project[project].use_meth then
        merge.project[project].use_meth = true
        merge.prj_meth = merge.prj_meth + 1
      end
    end
    if merge.project[project].use_setmetatable and
       not merge.project[project].use_set_colon then
      merge.project[project].use_set_colon = true
      merge.prj_set_colon = merge.prj_set_colon + 1
      if merge.project[project].use_set_self and
         not merge.project[project].use_set_meth then
        merge.project[project].use_set_meth = true
        merge.prj_set_meth = merge.prj_set_meth + 1
      end
    end
  end
  merge.method_self = merge.method_self + result.method_self
  if result.method_self > 0 then
    if not merge.project[project].use_self then
      merge.project[project].use_self = true
      merge.prj_self = merge.prj_self + 1
      if merge.project[project].use_colon and
         not merge.project[project].use_meth then
        merge.project[project].use_meth = true
        merge.prj_meth = merge.prj_meth + 1
      end
    end
    if merge.project[project].use_setmetatable and
       not merge.project[project].use_set_self then
      merge.project[project].use_set_self = true
      merge.prj_set_self = merge.prj_set_self + 1
      if merge.project[project].use_set_colon and
         not merge.project[project].use_set_meth then
        merge.project[project].use_set_meth = true
        merge.prj_set_meth = merge.prj_set_meth + 1
      end
    end
  end
  merge.method_this = merge.method_this + result.method_this
  merge.table_field = merge.table_field + result.table_field
  merge.number_of_constructs = merge.number_of_constructs + result.number_of_constructs
  merge.empty_construct = merge.empty_construct + result.empty_construct
  merge.only_static = merge.only_static + result.only_static
  merge.only_dynamic = merge.only_dynamic + result.only_dynamic
  merge.static_and_dynamic = merge.static_and_dynamic + result.static_and_dynamic
  merge.table_list = merge.table_list + result.table_list
  merge.table_record = merge.table_record + result.table_record
  merge.table_list_and_record = merge.table_list_and_record + result.table_list_and_record
  merge.table_boolean = merge.table_boolean + result.table_boolean
  merge.table_vararg = merge.table_vararg + result.table_vararg
  merge.varindex = merge.varindex + result.varindex
  merge.varindex_read = merge.varindex_read + result.varindex_read
  merge.varindex_write = merge.varindex_write + result.varindex_write
  merge.varindex_read_literal = merge.varindex_read_literal + result.varindex_read_literal
  merge.varindex_read_boolean = merge.varindex_read_boolean + result.varindex_read_boolean
  merge.varindex_read_number = merge.varindex_read_number + result.varindex_read_number
  merge.varindex_read_string = merge.varindex_read_string + result.varindex_read_string
  merge.varindex_read_non_literal = merge.varindex_read_non_literal + result.varindex_read_non_literal
  merge.varindex_read_literal_fc = merge.varindex_read_literal_fc + result.varindex_read_literal_fc
  merge.varindex_read_literal_mc = merge.varindex_read_literal_mc + result.varindex_read_literal_mc
  merge.varindex_read_non_literal_fc = merge.varindex_read_non_literal_fc + result.varindex_read_non_literal_fc
  merge.varindex_write_literal = merge.varindex_write_literal + result.varindex_write_literal
  merge.varindex_write_boolean = merge.varindex_write_boolean + result.varindex_write_boolean
  merge.varindex_write_number = merge.varindex_write_number + result.varindex_write_number
  merge.varindex_write_string = merge.varindex_write_string + result.varindex_write_string
  merge.varindex_write_non_literal = merge.varindex_write_non_literal + result.varindex_write_non_literal
  for k, v in pairs(metamethods) do
    if result[k] > 0 then
      if not merge.project[project][k] then
        merge.project[project][k] = true
        merge[k] = merge[k] + 1
      end
    end
  end
  merge.assignments = merge.assignments + result.assignments
  merge.simple_assignments = merge.simple_assignments + result.simple_assignments
  merge.multiple_assignments = merge.multiple_assignments + result.multiple_assignments
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
  print("number_of_projects", merge.number_of_projects)
  print("number_of_functions", merge.number_of_functions)
  print("anonymous", merge.anonymousf)
  print("global", merge.globalf)
  print("local", merge.localf)
  print("is_vararg", merge.is_vararg)
  print("ret_nil_se", merge.ret_nil_se)
  print("ret_false_se", merge.ret_false_se)
  print("ret_mult_values", merge.ret_mult_values)
  print("use_type", merge.use_type)
  print("type_count", merge.type_count)
  print("use_setmetatable", merge.use_setmetatable)
  print("prj_setmetatable", merge.prj_setmetatable)
  print("use_getmetatable", merge.use_getmetatable)
  print("prj_getmetatable", merge.prj_getmetatable)
  print("use_ipairs", merge.use_ipairs)
  print("ipairs_count", merge.ipairs_count)
  print("use_pairs", merge.use_pairs)
  print("pairs_count", merge.pairs_count)
  print("use_len", merge.use_len)
  print("len_count", merge.len_count)
  print("number_of_methods", merge.number_of_methods)
  print("prj_meth", (merge.prj_colon - merge.prj_meth) +
                    (merge.prj_self - merge.prj_meth) +
                     merge.prj_meth)
  print("prj_colon", merge.prj_colon - merge.prj_meth)
  print("prj_self", merge.prj_self - merge.prj_meth)
  print("prj_set_meth", (merge.prj_set_colon - merge.prj_set_meth) +
                        (merge.prj_set_self - merge.prj_set_meth) +
                         merge.prj_set_meth)
  print("prj_set_colon", merge.prj_set_colon - merge.prj_set_meth)
  print("prj_set_self", merge.prj_set_self - merge.prj_set_meth)
  print("method_colon", merge.method_colon)
  print("method_self", merge.method_self)
  print("method_this", merge.method_this)
  print("table_field", merge.table_field)
  print("number_of_constructs", merge.number_of_constructs)
  print("empty_construct", merge.empty_construct)
  print("only_static", merge.only_static)
  print("only_dynamic", merge.only_dynamic)
  print("static_and_dynamic", merge.static_and_dynamic)
  print("table_list", merge.table_list)
  print("table_record", merge.table_record)
  print("table_list_and_record", merge.table_list_and_record)
  print("table_boolean", merge.table_boolean)
  print("table_vararg", merge.table_vararg)
  print("varindex", merge.varindex)
  print("varindex_read", merge.varindex_read)
  print("varindex_write", merge.varindex_write)
  print("varindex_read_literal", merge.varindex_read_literal)
  print("varindex_read_boolean", merge.varindex_read_boolean)
  print("varindex_read_number", merge.varindex_read_number)
  print("varindex_read_string", merge.varindex_read_string)
  print("varindex_read_non_literal", merge.varindex_read_non_literal)
  print("varindex_read_literal_fc", merge.varindex_read_literal_fc)
  print("varindex_read_literal_mc", merge.varindex_read_literal_mc)
  print("varindex_read_non_literal_fc", merge.varindex_read_non_literal_fc)
  print("varindex_write_literal", merge.varindex_write_literal)
  print("varindex_write_boolean", merge.varindex_write_boolean)
  print("varindex_write_number", merge.varindex_write_number)
  print("varindex_write_string", merge.varindex_write_string)
  print("varindex_write_non_literal", merge.varindex_write_non_literal)
  for k, v in pairs(metamethods) do
    print(k, merge[k])
  end
  print("returning_module", merge.returning_module)
  print("calling_module", merge.calling_module)
  print("module_and_return", merge.module_and_return)
  print("plain_script", merge.plain_script)
  print("assignments", merge.assignments)
  print("simple_assignments", merge.simple_assignments)
  print("multiple_assignments", merge.multiple_assignments)
end

local function p (x, total)
  return string.format("%.0f%%", ((100 * x) / total))
end

function statistics.print_merge (merge)
  local prj_set_meth = (merge.prj_set_colon - merge.prj_set_meth) + (merge.prj_set_self - merge.prj_set_meth) + merge.prj_set_meth
  local prj_set_colon = merge.prj_set_colon - merge.prj_set_meth
  local prj_set_self = merge.prj_set_self - merge.prj_set_meth
  print("---")
  print("number of files", merge.number_of_files)
  print("number of projects", merge.number_of_projects)
  print("---")
  print("table constructor", merge.number_of_constructs)
  print("record", p(merge.table_record, merge.number_of_constructs))
  print("list", p(merge.table_list, merge.number_of_constructs))
  print("record + list", p(merge.table_list_and_record, merge.number_of_constructs))
  print("boolean", p(merge.table_boolean, merge.number_of_constructs))
  print("only literal", p(merge.only_static, merge.number_of_constructs))
  print("empty", p(merge.empty_construct, merge.number_of_constructs))
  print("only static", p(merge.only_dynamic, merge.number_of_constructs))
  print("mixes", p(merge.static_and_dynamic, merge.number_of_constructs))
  print("---")
  print("indexing", merge.varindex)
  print("read", p(merge.varindex_read, merge.varindex))
  print("write", p(merge.varindex_write, merge.varindex))
  print("read string", p(merge.varindex_read_string, merge.varindex_read))
  print("read number", p(merge.varindex_read_number, merge.varindex_read))
  print("read boolean", p(merge.varindex_read_boolean, merge.varindex_read))
  print("read literal", p(merge.varindex_read_literal, merge.varindex_read))
  print("read non literal", p(merge.varindex_read_non_literal, merge.varindex_read))
  print("read function call", p(merge.varindex_read_literal_fc + merge.varindex_read_literal_mc + merge.varindex_read_non_literal_fc, merge.varindex_read))
  print("literal function call", p(merge.varindex_read_literal_fc, merge.varindex_read))
  print("literal method call", p(merge.varindex_read_literal_mc, merge.varindex_read))
  print("non literall call", p(merge.varindex_read_non_literal_fc, merge.varindex_read))
  print("write string", p(merge.varindex_write_string, merge.varindex_write))
  print("write number", p(merge.varindex_write_number, merge.varindex_write))
  print("write boolean", p(merge.varindex_write_boolean, merge.varindex_write))
  print("write literal", p(merge.varindex_write_literal, merge.varindex_write))
  print("write non literal", p(merge.varindex_write_non_literal, merge.varindex_write))
  print("---")
  print("pairs", p(merge.use_pairs, merge.number_of_files))
  print("ipairs", p(merge.use_ipairs, merge.number_of_files))
  print("len", p(merge.use_len, merge.number_of_files))
  print("---")
  print("function declarations", merge.number_of_functions)
  print("---")
  print("type", p(merge.use_type, merge.number_of_functions))
  print("---")
  print("multiple", p(merge.ret_mult_values, merge.number_of_functions))
  print("nil + se", p(merge.ret_nil_se, merge.number_of_functions))
  print("false + se", p(merge.ret_false_se, merge.number_of_functions))
  print("---")
  print("method declarations", p(merge.number_of_methods, merge.number_of_functions))
  print("colon", p(merge.method_colon, merge.number_of_functions))
  print("self", p(merge.method_self, merge.number_of_functions))
  print("setmetatable", p(merge.prj_setmetatable, merge.number_of_projects))
  print("getmetatable", p(merge.prj_getmetatable, merge.number_of_projects))
  print("setmetatable + methods", p(prj_set_meth, merge.number_of_projects))
  print("colon", p(prj_set_colon, merge.number_of_projects))
  print("self", p(prj_set_self, merge.number_of_projects))
  print("both", p(prj_set_meth - (prj_set_colon + prj_set_self), merge.number_of_projects))
  print("---")
  for k, v in pairs(metamethods) do
    print(k, p(merge[k], merge.number_of_projects))
  end
  print("---")
  print("return module", p(merge.returning_module, merge.number_of_files))
  print("call module", p(merge.calling_module, merge.number_of_files))
  print("both", p(merge.module_and_return, merge.number_of_files))
  print("modules", p(merge.returning_module + merge.calling_module + merge.module_and_return, merge.number_of_files))
  print("scripts", p(merge.number_of_files - (merge.returning_module + merge.calling_module + merge.module_and_return), merge.number_of_files))
  print("---")
  print("variadic functions", p(merge.is_vararg, merge.number_of_functions))
  print("variadic lists", p(merge.table_vararg, merge.table_list), p(merge.table_vararg, merge.number_of_constructs))
  print("---")
end


return statistics
