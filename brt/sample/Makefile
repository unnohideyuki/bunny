all:	Sample1 Sample2 Sample7 Sample8

brtsyn:
	javac -cp "../src" ../src/jp/ne/sakura/uhideyuki/brt/brtsyn/*.java

runtime:
	javac -cp "../src" ../src/jp/ne/sakura/uhideyuki/brt/runtime/*.java

Sample1:	brtsyn runtime
	cd sample1; javac -cp "../../src;." Sample1.java

run1:
	cd sample1; java -ea -cp "../../src;." Sample1

Sample2:	brtsyn runtime
	cd sample2; javac -cp "../../src;." Sample2.java

run2:
	cd sample2; java -ea -cp "../../src;." Sample2

Sample7:	brtsyn runtime
	cd sample7; javac -cp "../../src;." Sample7.java

run7:
	cd sample7; java -ea -cp "../../src;." Sample7

Sample8:	brtsyn runtime
	cd sample8; javac -cp "../../src;." Sample8.java

run8:
	cd sample8; java -ea -cp "../../src;." Sample8

