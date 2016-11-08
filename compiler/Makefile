execs = bin/bunnyc

samples = sample/test-20161108

temps = src/Lexer.x src/Lexer.hs src/Parser.hs
objs = src/Lexer.o src/Parser.o src/Absyn.o src/Types.o src/Symbol.o \
       src/Typing.o src/Semant.o src/PreDefined.o \
       src/Core.o src/Desugar.o src/TrCore.o src/STG.o src/TrSTG.o \
       src/CodeGen.o src/DictPass.o
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

src/Parser.o:	src/Parser.hs
src/Absyn.o:	src/Absyn.hs
src/Types.o:	src/Types.hs
src/Symbol.o:	src/Symbol.hs
src/Typing.o:	src/Typing.hs
src/Semant.o:	src/Semant.hs
src/PreDefined.o:	src/PreDefined.hs
src/Core.o:	src/Core.hs
src/Desugar.o:	src/Desugar.hs

bin/bunnyc:	src/compiler-main.hs $(objs)
	mkdir -p bin
	ghc $(OPTIONS) -o $@ -isrc $<

clean:	
	rm -f src/*.hi src/*.o sample/*.o $(samples) $(temps) $(objs) $(execs)

check:
	cd test; ./run-test.sh

sample/test-20161108:	sample/test-20161108.hs $(objs)
	ghc $(OPTIONS) -isrc $<


