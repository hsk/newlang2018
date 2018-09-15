all:
	time swipl main.pl src.mc
	gcc a.s -static lib/lib.c -o a; ./a
	time swipl main.pl -O1 src.mc
	gcc a.s -static lib/lib.c -o a; ./a
	time swipl main.pl -O2 src.mc
	gcc a.s -static lib/lib.c -o a; ./a
test:
	swipl tests/test_emit.pl
	swipl tests/test_graphRegAlloc.pl
	swipl tests/test_genCode.pl
	make clean
	@echo "test ok"
clean:
	rm -rf a a.s a.txt
