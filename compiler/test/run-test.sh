#!/bin/bash

i=0
nng=0

mkdir -p results
mkdir -p jout

rm -f errors.txt
touch errors.txt

## Compilie lib/Prelude
rm -rf jout/Prelude
mkdir jout/Prelude
../bin/bunnyc -d jout/Prelude --xno-implicit-prelude ../lib/Prelude.hs
if [ $? -ne 0 ];then
    exit 1
fi

function dotest(){
  f=$1
  bname=`basename $f .hs`

  echo "----"
  echo "Test: $f"
  ./test-compile.sh $f
  s=$?
  if [ -f err/$bname ];then
      if [ $s -eq 0 ] ;then
	  echo "Compiling $f failed (e)"
	  echo "$f: compile" >> errors.txt
	  return 1
      else
	  return 0
      fi
  else
      if [ $s -ne 0 ] ;then
	  echo "Compiling $f failed"
	  echo "$f: compile" >> errors.txt
	  return 1
      fi
  fi

  ./jcompile jout/$bname/Sample.java
  if [ $? -ne 0 ];then
    echo "Java Compiling $bname failed."
    echo "$f: java compile" >> errors.txt
    return 1
  fi

  ./run jout/$bname/Sample.java > results/$bname.txt 2> /dev/null
  if [ $? -ne 0 ];then
      if [ ! -f err/$bname.rt ]; then
	  echo "Running $bname failed (or abend)."
	  echo "$f: abort" >> errors.txt
	  return 1
      fi
  else
      if [ -f err/$bname.rt ]; then
	  echo -n "R" # shuold abend but didn't
	  echo "$f: must abend" >> $errors
	  return 1
      fi
  fi
  cat results/$bname.txt

  if [ ! -f err/$bname.rt ]; then
      diff results/$bname.txt expected
      if [ $? -ne 0 ];then
	  echo "Unexpected result: $bname"
	  echo "$f: result check" >> errors.txt
	  return 1
      fi
  fi

  return 0
}

for f in `ls -r samples/*.hs`; do
    dotest $f
    nng=`expr $nng \+ $?`
    i=`expr $i \+ 1`
done

if [ $nng -eq 0 ]; then
    echo "==================="
    echo "Finished $i tests."
    echo "==================="
else
    echo "======================="
    echo " $nng/$i tests failed."
    echo "======================="
fi

exit $nng



