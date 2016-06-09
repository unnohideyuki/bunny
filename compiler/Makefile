samples = sample/parser_sample sample/drive_semant sample/parse2core \
	sample/compiler0 \
	sample/drive_semant2

temps = src/Lexer.x src/Lexer.hs src/Parser.hs
objs = src/Lexer.o src/Parser.o src/Absyn.o src/Types.o src/Symbol.o \
       src/Typing.o src/Semant.o src/PreDefined.o \
       src/Core.o src/Desugar.o src/TrCore.o src/STG.o src/TrSTG.o \
       src/CodeGen.o
all: $(samples)

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

sample/parser_sample:	sample/parser_sample.hs $(objs)
	ghc $(OPTIONS) -isrc $<

sample/drive_semant:	sample/drive_semant.hs $(objs)
	ghc $(OPTIONS) -isrc $<

sample/drive_semant2:	sample/drive_semant2.hs $(objs)
	ghc $(OPTIONS) -isrc $<

sample/parse2core:	sample/parse2core.hs $(objs)
	ghc $(OPTIONS) -isrc $<

sample/compiler0:	sample/compiler0.hs $(objs)
	ghc $(OPTIONS) -isrc $<

clean:	
	rm -f src/*.hi src/*.o sample/*.o $(samples) $(temps) $(objs)
