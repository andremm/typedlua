
local terms = {
  "test",
  "other test",
}

local def = {
  ["test"] = "a test.",
}

table.sort(terms)

io.write("\\begin{description}\n")
for k, v in ipairs(terms) do
  io.write(string.format("\\item[%s] %s\n", v, def[v] or "TODO"))
end
io.write("\\end{description}\n")
