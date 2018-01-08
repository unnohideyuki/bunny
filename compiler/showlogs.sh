#!/bin/bash

tlines=0
twarns=0
thints=0

sp32=`echo -e -n "\t\t\t\t" | expand`

for f in src/*.hs app/Main.hs; do
    if [ $f != src/Lexer.hs -a $f != src/Parser.hs -a $f != src/unicateg.hs ]; then
	bn=`basename $f`
	# file name
	name=`echo -n -e "$f$sp32" | cut -c 1-32`
	echo -n -e "$name"

	# lines
	n=`wc -l $f | cut -d " " -f 1`
	echo -n -e "$n lines\t"
	tlines=`expr $tlines \+ $n`
	
	# warnings
	w=`cat log/* | grep $bn | grep "warning:" | wc -l | cut -d " " -f 1`
	echo -n -e "$w warnings\t"
	twarns=`expr $twarns \+ $w`

	# hints
	h=`hlint -XHaskell2010 $f | tail -1 | cut -d " " -f 1 | sed s/No/0/`
	thints=`expr $thints \+ $h`
	if [ $h -gt 1 ];then
	    echo "$h hints"
	else
	    echo "$h hint"
	fi
    fi
done

echo "----"
echo -e "$sp32$tlines lines\t$twarns warnings\t$thints hints"
