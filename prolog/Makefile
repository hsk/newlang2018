all:
	swipl main.pl src.mc
	swipl main.pl -O1 src.mc
test:
	swipl tests/test_emit.pl
	swipl tests/test_graphRegAlloc.pl
	swipl tests/test_genCode.pl
	make clean
	@echo "test ok"
clean:
	rm -rf a a.s a.txt
