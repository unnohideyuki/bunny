#!/bin/sh

i=0
nng=0

mkdir -p results
mkdir -p jout

rm -f errors.txt
touch errors.txt

function dotest(){
  f=$1
    
  echo "----"
  echo "Test: $f"
  ./test-compile.sh $f
  if [ $? -ne 0 ];then
      echo "Compiling $f failed"
      echo "$f: compile" >> errors.txt
      return 1
  fi

  bname=`basename $f .hs`

  ./jcompile jout/$bname/Sample.java
  if [ $? -ne 0 ];then
    echo "Java Compiling $bname failed."
    echo "$f: java compile" >> errors.txt
    return 1
  fi

  ./run jout/$bname/Sample.java > results/$bname.txt 2> /dev/null
  if [ $? -ne 0 ];then
    echo "Running $bname failed (or abend)."
    echo "$f: abort" >> errors.txt
    return 1
  fi
  cat results/$bname.txt

  diff results/$bname.txt expected
  if [ $? -ne 0 ];then
    echo "Unexpected result: $bname"
    echo "$f: result check" >> errors.txt
    return 1
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



