samples = sample/parser_sample

temps = src/Lexer.x src/Lexer.hs src/Parser.hs
objs = src/Lexer.o src/Parser.o src/Absyn.o src/Types.o src/Symbol.o \
       src/Typing.o
all: $(samples)

%.o:	%.hs
	ghc -Wall -isrc $<

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

sample/parser_sample:	sample/parser_sample.hs $(objs)
	ghc -Wall -isrc $<

clean:	
	rm -f src/*.hi src/*.o sample/*.o $(samples) $(temps) $(objs)
