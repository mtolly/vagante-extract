.PHONY: default win mac

default:
	@echo "Usage:"
	@echo "  make win"
	@echo "  make mac"

win:
	rm -rf win
	mkdir win
	cp app-readme.txt win/readme.txt
	stack install
	cp $(shell stack exec which vagante-extract) win
	strip win/vagante-extract.exe

mac:
	rm -rf mac
	mkdir mac
	osacompile -o mac/drop-onto-me.app drop-onto-me.applescript
	cp app-readme.txt mac/readme.txt
	stack install
	cp $(shell stack exec which vagante-extract) mac/drop-onto-me.app/Contents/MacOS/
	strip mac/drop-onto-me.app/Contents/MacOS/vagante-extract
