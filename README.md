# Typed Lua
[![Build Status](https://travis-ci.org/andremm/typedlua.svg?branch=master)](https://travis-ci.org/andremm/typedlua)

Typed Lua is a typed superset of Lua that compiles to plain Lua.
It provides optional type annotations, compile-time type checking, and
class-based object oriented programming through the definition of classes,
interfaces, and modules.

# Requirements for running the compiler

1. Lua >= 5.1
1. [LPeg](http://www.inf.puc-rio.br/%7Eroberto/lpeg/) >= 0.12

# Install

Typed Lua must be installed in a standard location; [LuaRocks](http://luarocks.org) will do this, and will also install the LPeg dependency automatically.

        $ [install luarocks]
        $ luarocks install typedlua-scm-1.rockspec

# Usage

        $ tlc [options] [filename]

# Compiler options

        -h       print this help
        -d name  dump the AST (after typechecking) to file 'name'
        -o name  output to file 'name' (default is 'tlc.lua')
        -c       ansi colors on
        -p       print the AST in Metalua format (before typechecking)
        -s       strict mode on
        -v       print current version
        -w       warnings on (consistent-subtyping, unused locals)

# Runtime module loader

Typed Lua also provides a runtime loader to use Typed Lua modules on any
standard Lua virtual machine:

```lua
require "typedlua"
package.path = "./?.tl;" .. package.path

-- It will load my_module.tl if it exists:
local my_module = require "my_module"
local my_file = loadfile "my_file.tl"
```

The loader functions use the file extension (chunk name extension in the cases
of load and loadstring) to decide whether it contains Typed Lua (.tl) or plain
Lua. If you want to force loading the chunks as Typed Lua you can use the
functions provided by the typedlua module (these work like their standard Lua
counterparts):

```lua
local typedlua = require "typedlua"
typedlua.loadstring(string, chunkname)
typedlua.load(func, chunkname)
typedlua.loadfile(filename)
typedlua.dofile(filename)
```

# License

Released under the MIT License (MIT)

Copyright (c) 2013 Andre Murbach Maidl

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
