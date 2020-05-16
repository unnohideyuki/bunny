#!/bin/bash

para=$1
num=$2

i=0
j=0
nng=0

mkdir -p results
mkdir -p jout

errors="errors_${para}_${num}.txt"
rm -f $errors
touch $errors

function dotest(){
  f=$1
  bname=`basename $f .hs`

  ./test-compile.sh $f >/dev/null 2>/dev/null
  s=$?
  if [ -f err/$bname ];then
      if [ $s -eq 0 ] ;then
	  echo -n "C" # should not be compiled but done.
	  echo "$f: compile" >> errors.txt
	  return 1
      else
	  echo -n "."
	  return 0
      fi
  else
      if [ $s -ne 0 ] ;then
	  echo -n "c" # compile error
	  echo "$f: compile" >> errors.txt
	  return 1
      fi
  fi

  ./jcompile jout/$bname/Sample.java
  if [ $? -ne 0 ];then
    echo -n "j" # Java compilation failed
    echo "$f: java compile" >> errors.txt
    return 1
  fi

  ./run jout/$bname/Sample.java > results/$bname.txt 2> /dev/null
  if [ $? -ne 0 ];then
    echo -n "r" # Running $bname failed (or abend).
    echo "$f: abort" >> errors.txt
    return 1
  fi
  # cat results/$bname.txt

  diff results/$bname.txt expected
  if [ $? -ne 0 ];then
    echo -n "U" # Unexpected result: $bname"
    echo "$f: result check" >> errors.txt
    return 1
  fi

  echo -n "."
  return 0
}

for f in `ls -r samples/*.hs`; do
    if [ `expr $i % $para` -eq $num ]; then
      dotest $f
      nng=`expr $nng \+ $?`
      j=`expr $j \+ 1`
    fi
    i=`expr $i \+ 1`
done

total="total_${para}_${num}.txt"
echo $j > $total

nerrors="nerr_${para}_${num}.txt"
echo $nng > $nerrors

exit $nng
