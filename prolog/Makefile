all:
	swipl main.pl
test:
	swipl tests/test_utils.pl
	swipl tests/test_emit.pl
	swipl tests/test_memAlloc.pl
	swipl tests/test_graphRegAlloc.pl
	swipl tests/test_genCode.pl
	make clean
	@echo "test ok"
clean:
	rm -rf a a.s
