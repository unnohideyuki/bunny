src/Lexer.hs:	src/Lexer.x
	alex src/Lexer.x

src/Lexer.x:	src/Lexer.x.source src/unisets.txt
	cpp -w -E -P -Isrc $< $@

clean:	
	rm -f src/Lexer.x src/Lexer.hs src/*.hi src/*.o
