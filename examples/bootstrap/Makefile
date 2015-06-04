TLC=tlc

all: tllexer.lua tlast.lua tltype.lua tlst.lua tlparser.lua tldparser.lua tlchecker.lua tlcode.lua

tllexer.lua: tllexer.tl
	$(TLC) -o tllexer.lua tllexer.tl

tlast.lua: tlast.tl
	$(TLC) -o tlast.lua tlast.tl

tltype.lua: tltype.tl
	$(TLC) -o tltype.lua tltype.tl

tlst.lua: tlst.tl
	$(TLC) -o tlst.lua tlst.tl

tlparser.lua: tlparser.tl
	$(TLC) -o tlparser.lua tlparser.tl

tldparser.lua: tldparser.tl
	$(TLC) -o tldparser.lua tldparser.tl

tlchecker.lua: tlchecker.tl
	$(TLC) -o tlchecker.lua tlchecker.tl

tlcode.lua: tlcode.tl
	$(TLC) -o tlcode.lua tlcode.tl

clean:
	rm -f tllexer.lua tlast.lua tltype.lua tlst.lua tlparser.lua tldparser.lua tlchecker.lua tlcode.lua
