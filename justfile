build:
	lazbuild --build-all --build-mode=Release simpleMenu.lpr

dbgBuild:
	lazbuild --build-all --build-mode=Debug simpleMenu.lpr	

# README.md: README.mds
#	mdpreproc < README.mds > README.md

install: build # README.md
	cp -v simpleMenu ~/bin/
