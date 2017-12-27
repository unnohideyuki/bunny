execs = bin/bunnyc

samples = sample/test-20161108

temps = src/Lexer.x src/Lexer.hs src/Parser.hs
objs = src/Lexer.o src/Parser.o src/Absyn.o src/Types.o src/Symbol.o \
       src/Typing.o src/Semant.o src/PreDefined.o \
       src/Core.o src/Desugar.o src/TrCore.o src/STG.o src/TrSTG.o \
       src/CodeGen.o src/DictPass.o \
       src/CompilerOpts.o src/DDumpAssump.o src/DDumpCore.o
all: $(samples) $(execs)

OPTIONS = -Wall -XFlexibleContexts

%.o:	%.hs
	ghc $(OPTIONS) -isrc $<

src/Lexer.o:	src/Lexer.hs

src/Lexer.hs:	src/Lexer.x
	alex src/Lexer.x

src/Lexer.x:	src/Lexer.x.source src/unisets.txt
	cpp -w -E -P -Isrc $< $@

src/Parser.hs:	src/Parser.y
	happy $<

bin/bunnyc:	app/Main.hs $(objs)
	mkdir -p bin
	ghc $(OPTIONS) -o $@ -isrc $<

clean:	
	rm -f src/*.hi src/*.o sample/*.o $(samples) $(temps) $(objs) $(execs)

rt:	
	cd ../brt; make

check:	bin/bunnyc rt
	cd test; ./run-test.sh

sample/test-20161108:	sample/test-20161108.hs $(objs)
	ghc $(OPTIONS) -isrc $<


