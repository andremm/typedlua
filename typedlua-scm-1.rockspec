package = "typedlua"
version = "scm-1"
source = {
  url = "git://github.com/andremm/typedlua.git"
}
description = {
  summary = "An Optional Type System for Lua",
  detailed = [[
    Typed Lua is a typed superset of Lua that compiles to plain Lua.
    It provides optional type annotations, compile-time type checking, and
    class-based object oriented programming through the definition of classes,
    interfaces, and modules.
  ]],
  homepage = "https://github.com/andremm/typedlua",
  license = "MIT"
}
dependencies = {
  "lua >= 5.1",
  "lpeg >= 0.12"
}
build = {
  type = "builtin",
  modules = {
    ["typedlua.tlast"] = "typedlua/tlast.lua",
    ["typedlua.tlchecker"] = "typedlua/tlchecker.lua",
    ["typedlua.tlcode"] = "typedlua/tlcode.lua",
    ["typedlua.tllexer"] = "typedlua/tllexer.lua",
    ["typedlua.tlparser"] = "typedlua/tlparser.lua",
    ["typedlua.tlst"] = "typedlua/tlst.lua",
    ["typedlua.tltype"] = "typedlua/tltype.lua",
    ["typedlua.tldparser"] = "typedlua/tldparser.lua"
  },
  install = {
    bin = { "tlc" },
    lua = { ["typedlua.lsl"] = "typedlua/lsl.tld" }
  }
}
