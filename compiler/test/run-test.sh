#!/bin/sh

i=0

mkdir -p results

for f in `ls samples/*.hs`; do
  echo "----"
  echo "Test: $f"
  ./test-compile.sh $f
  if [ $? -ne 0 ];then
    echo "Compiling $f failed"
    exit 1
  fi

  bname=`basename $f .hs`

  ./jcompile jout/$bname/Sample.java
  if [ $? -ne 0 ];then
    echo "Java Compiling $bname failed."
    exit 1
  fi

  ./run jout/$bname/Sample.java > results/$bname.txt 2> /dev/null
  if [ $? -ne 0 ];then
    echo "Running $bname failed (or abend)."
    exit 1
  fi
  cat results/$bname.txt

  diff results/$bname.txt expected
  if [ $? -ne 0 ];then
    echo "Unexpected result: $bname"
    exit 1
  fi

  i=`expr $i \+ 1`
done

echo "==================="
echo "Finished $i tests."
echo "==================="


