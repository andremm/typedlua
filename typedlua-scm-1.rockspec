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
    ["typedlua.tlfilter"] = "typedlua/tlfilter.lua",
    ["typedlua.tllexer"] = "typedlua/tllexer.lua",
    ["typedlua.tlparser"] = "typedlua/tlparser.lua",
    ["typedlua.tlst"] = "typedlua/tlst.lua",
    ["typedlua.tltype"] = "typedlua/tltype.lua",
    ["typedlua.tldparser"] = "typedlua/tldparser.lua"
  },
  install = {
    bin = { "tlc" },
    lua = {
      ["typedlua.lsl51.base"] = "typedlua/lsl51/base.tld",
      ["typedlua.lsl51.coroutine"] = "typedlua/lsl51/coroutine.tld",
      ["typedlua.lsl51.package"] = "typedlua/lsl51/package.tld",
      ["typedlua.lsl51.string"] = "typedlua/lsl51/string.tld",
      ["typedlua.lsl51.table"] = "typedlua/lsl51/table.tld",
      ["typedlua.lsl51.math"] = "typedlua/lsl51/math.tld",
      ["typedlua.lsl51.io"] = "typedlua/lsl51/io.tld",
      ["typedlua.lsl51.os"] = "typedlua/lsl51/os.tld",
      ["typedlua.lsl51.debug"] = "typedlua/lsl51/debug.tld",
      ["typedlua.lsl52.base"] = "typedlua/lsl52/base.tld",
      ["typedlua.lsl52.coroutine"] = "typedlua/lsl52/coroutine.tld",
      ["typedlua.lsl52.package"] = "typedlua/lsl52/package.tld",
      ["typedlua.lsl52.string"] = "typedlua/lsl52/string.tld",
      ["typedlua.lsl52.table"] = "typedlua/lsl52/table.tld",
      ["typedlua.lsl52.math"] = "typedlua/lsl52/math.tld",
      ["typedlua.lsl52.bit32"] = "typedlua/lsl52/bit32.tld",
      ["typedlua.lsl52.io"] = "typedlua/lsl52/io.tld",
      ["typedlua.lsl52.os"] = "typedlua/lsl52/os.tld",
      ["typedlua.lsl52.debug"] = "typedlua/lsl52/debug.tld",
      ["typedlua.lsl53.base"] = "typedlua/lsl53/base.tld",
      ["typedlua.lsl53.coroutine"] = "typedlua/lsl53/coroutine.tld",
      ["typedlua.lsl53.package"] = "typedlua/lsl53/package.tld",
      ["typedlua.lsl53.string"] = "typedlua/lsl53/string.tld",
      ["typedlua.lsl53.utf8"] = "typedlua/lsl53/utf8.tld",
      ["typedlua.lsl53.table"] = "typedlua/lsl53/table.tld",
      ["typedlua.lsl53.math"] = "typedlua/lsl53/math.tld",
      ["typedlua.lsl53.io"] = "typedlua/lsl53/io.tld",
      ["typedlua.lsl53.os"] = "typedlua/lsl53/os.tld",
      ["typedlua.lsl53.debug"] = "typedlua/lsl53/debug.tld"
    }
  }
}
