
local module_list = {
  "lsl.base",
  "lsl.coroutine",
  "lsl.package",
  "lsl.string",
  "lsl.utf8",
  "lsl.table",
  "lsl.math",
  "lsl.io",
  "lsl.os",
  "md5.md5",
  "socket.socket",
  "socket.ftp",
  "socket.http",
  "socket.smtp",
  "socket.mime",
  "socket.ltn12",
  "socket.url",
  "lfs.lfs",
  "httpdigest.httpdigest",
  "typical.typical",
  "mod11.mod11",
  "tlc.tlast",
  "tlc.tltype",
  "tlc.tlst",
  "tlc.tllexer",
  "tlc.tlparser",
  "tlc.tldparser",
  "tlc.tlchecker",
  "tlc.tlcode",
}

local library_list = {
  "lsl",
  "md5",
  "socket",
  "lfs",
  "httpdigest",
  "typical",
  "mod11",
  "tlc",
}

local library_name = {
  lsl = "Lua Standard Library",
  md5 = "MD5",
  socket = "LuaSocket",
  lfs = "LuaFileSystem",
  httpdigest = "HTTP Digest",
  typical = "Typical",
  mod11 = "Modulo 11",
  tlc = "Typed Lua Compiler",
}

local cat = { "easy", "poly", "over", "hard" }

local function p (x, total)
  return tonumber(string.format("%.0f", (100 * x) / total))
end

local function pc (list, total)
  local easy = p(list[1], total)
  local poly = p(list[2], total)
  local over = p(list[3], total)
  local hard = p(list[4], total)
  local recon = easy + poly + over + hard
  return easy, poly, over, hard, recon
end

local function rows (m)
  local r = 0
  for i, v in ipairs(module_list) do
    if string.find(v, m) then
      r = r + 1
    end
  end
  return r
end

local result_by_module = {}
local total_by_module = {}

local result_by_library = {}
local total_by_library = {}

local function load_data ()
  package.path = "./?.lua"
  for i, m in ipairs(module_list) do
    local mod = require(m)
    result_by_module[i] = { 0, 0, 0, 0 }
    total_by_module[i] = 0
    for k, c in pairs(mod) do
      result_by_module[i][c] = result_by_module[i][c] + 1
      total_by_module[i] = total_by_module[i] + 1
    end
    local l = string.match(m, "(%w+)[.]%w+")
    if not result_by_library[l] then
      result_by_library[l] = { 0, 0, 0, 0 }
      total_by_library[l] = 0
    end
    for j, v in ipairs(result_by_module[i]) do
      result_by_library[l][j] = result_by_library[l][j] + v
      total_by_library[l] = total_by_library[l] + v
    end
  end
end

local function log_by_library ()
  for i, k in ipairs(library_list) do
    print(k)
    local l = result_by_library[k]
    for j, v in ipairs(l) do
      print(cat[j], v)
    end
    print("total", total_by_library[k], p(l[1], total_by_library[k]))
    print("---")
  end
end

local function log_by_module ()
  for i, m in ipairs(result_by_module) do
    print(module_list[i])
    for j, v in ipairs(m) do
      print(cat[j], v)
    end
    print("total", total_by_module[i], p(m[1], total_by_module[i]))
    print("---")
  end
end

local function log_total ()
  local modules, members, easy = 0, 0, 0
  for i, m in ipairs(result_by_module) do
    modules = modules + 1
    members = members + total_by_module[i]
    easy = easy + m[1]
  end
  local total = p(easy, members)
  print(modules, members, total)
end

local function table_by_library ()
  print("\\begin{table}[!ht]")
  print("\\begin{center}")
  print("\\begin{tabular}{|l|c|c|c|c|c|c|}")
  print("\\cline{2-5}")
  print("\\multicolumn{1}{c}{} & \\multicolumn{4}{|c|}{percentage of members} & \\multicolumn{1}{c}{} \\\\")
  print("\\hline")
  print("\\textbf{Case study} & \\textbf{easy} & \\textbf{poly} & \\textbf{over} & \\textbf{hard} & \\textbf{\\# members} \\\\")
  print("\\hline")
  for i, k in ipairs(library_list) do
    local name = library_name[k]
    local lib = result_by_library[k]
    local total = total_by_library[k]
    local easy, poly, over, hard, recon = pc(lib, total)
    io.write(string.format("%s", name))
    io.write(string.format(" & %d\\%%", easy))
    io.write(string.format(" & %d\\%%", poly))
    io.write(string.format(" & %d\\%%", over))
    io.write(string.format(" & %d\\%%", hard))
    io.write(string.format(" & %d \\\\ %% %d%%", total, recon))
    io.write("\n\\hline\n")
  end
  print("\\end{tabular}")
  print("\\end{center}")
  print("\\caption{Evaluation results for each case study}")
  print("\\label{tab:evalbycase}")
  print("\\end{table}")
end

local function table_by_module ()
  local t = {}
  for i, m in ipairs(result_by_module) do
    local l = string.match(module_list[i], "(%w+)[.]%w+")
    if not t[l] then
      print("---")
      print("\\begin{table}[!ht]")
      print("\\begin{center}")
      print("\\begin{tabular}{|c|c|c|c|c|c|}")
      print("\\cline{2-5}")
      print("\\multicolumn{1}{c}{} & \\multicolumn{4}{|c|}{percentage of members} & \\multicolumn{1}{c}{} \\\\")
      print("\\hline")
      print("\\textbf{Module} & \\textbf{easy} & \\textbf{poly} & \\textbf{over} & \\textbf{hard} & \\textbf{\\# members} \\\\")
      print("\\hline")
      local r = rows(l .. "[.]")
      t[l] = i - 1 + r
    end
    local module_name = string.match(module_list[i], "%w+[.](%w+)")
    local total = total_by_module[i]
    local easy, poly, over, hard, recon = pc(m, total)
    io.write(string.format("%s", module_name))
    io.write(string.format(" & %d\\%%", easy))
    io.write(string.format(" & %d\\%%", poly))
    io.write(string.format(" & %d\\%%", over))
    io.write(string.format(" & %d\\%%", hard))
    io.write(string.format(" & %d \\\\ %% %d%%", total, recon))
    io.write("\n\\hline\n")
    if i == t[l] then
      print("\\end{tabular}")
      print("\\end{center}")
      print(string.format("\\caption{Evaluation results for %s}", library_name[l]))
      print(string.format("\\label{tab:eval%s}", l))
      print("\\end{table}")
    end
  end
end

local usage = [[usage: lua eval.lua <option>
Available options are:
-ll	generate log by library
-lm	generate log by module
-tl	generate table by library
-tm	generate table by module]]

if #arg ~= 1 then
  print(usage)
  os.exit(1)
end

load_data()

local opt = arg[1]

if opt == "-ll" then
  log_by_library()
elseif opt == "-lm" then
  log_by_module()
elseif opt == "-tl" then
  table_by_library()
elseif opt == "-tm" then
  table_by_module()
elseif opt == "-total" then
  log_total()
else
  print(usage)
  os.exit(1)
end

os.exit(0)
