samples = sample/parser_sample

all: src/Lexer.o src/Parser.o $(samples)

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

sample/parser_sample:	sample/parser_sample.hs src/Parser.o src/Absyn.o
	ghc -Wall -isrc $<

clean:	
	rm -f src/Lexer.x src/Lexer.hs src/Parser.hs src/*.hi src/*.o sample/*.o $(samples)
