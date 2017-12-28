execs = bin/bunnyc

temps = src/Lexer.x src/Lexer.hs src/Parser.hs
objs = src/Lexer.o src/Parser.o \
       src/Absyn.o src/Symbol.o src/Types.o \
       src/Typing.o src/Semant.o src/PreDefined.o \
       src/Core.o src/Desugar.o src/TrCore.o src/STG.o src/TrSTG.o \
       src/CodeGen.o src/DictPass.o \
       src/CompilerOpts.o src/DDumpAssump.o src/DDumpCore.o
all: $(execs)


OPTIONS = -Wall -XFlexibleContexts

src/Lexer.hs:	src/Lexer.x
	alex src/Lexer.x

src/Lexer.x:	src/Lexer.x.source src/unisets.txt
	cpp -w -E -P -Isrc $< $@

src/Parser.hs:	src/Parser.y
	happy $<

%.o:	%.hs
	mkdir -p log
	ghc $(OPTIONS) -isrc $< |& tee log/$(notdir $(basename $<)).log

bin/bunnyc:	app/Main.hs $(objs)
	mkdir -p bin
	ghc $(OPTIONS) -o $@ -isrc $<

clean:	
	rm -f src/*.hi src/*.o $(temps) $(objs) $(execs)
	rm -f log/*

rt:	
	cd ../brt; make

check:	bin/bunnyc rt
	cd test; ./run-test.sh

