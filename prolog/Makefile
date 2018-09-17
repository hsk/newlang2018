all: mcc
	time -p swipl main.pl src.mc
	time -p swipl main.pl -O1 src.mc
	time -p swipl main.pl -O2 src.mc

	time -p ./mcc src.mc
	time -p ./mcc -O1 src.mc
	time -p ./mcc -O2 src.mc
	./mcc src.mc
	time -p gcc a.s -static lib/lib.c -o a; ./a
	./mcc -O1 src.mc
	time -p gcc a.s -static lib/lib.c -o a; ./a
	./mcc -O2 src.mc
	time -p gcc a.s -static lib/lib.c -o a; ./a
mcc: *.pl
	swipl -g main -o mcc -c main.pl

test: mcc
	swipl tests/test_genAmd64.pl
	swipl tests/test_graphRegAlloc.pl
	swipl tests/test_genCode.pl

	time -p ./mcc tests/b.mc
	gcc a.s -static lib/lib.c -o a; ./a
	time -p ./mcc -O1 tests/b.mc
	gcc a.s -static lib/lib.c -o a; ./a
	time -p ./mcc -O2 tests/b.mc
	gcc a.s -static lib/lib.c -o a; ./a
	@echo "test ok"
clean:
	rm -rf a.out a a.s a.txt mcc
