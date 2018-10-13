.PHONY: default win mac

default:
	@echo "Usage:"
	@echo "  make win"
	@echo "  make mac"

mac:
	rm -rf mac
	mkdir mac
	osacompile -o mac/drop-onto-me.app drop-onto-me.applescript
	cp app-readme.txt mac/readme.txt
	stack install
	cp $(shell stack exec which vagante-extract) mac
