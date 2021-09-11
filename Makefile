all:
	idris2 --install Uri.ipkg
	

tests:
	idris2 --build test/Tests.ipkg 
	cd test && ./build/exec/runtests .