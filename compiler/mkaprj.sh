#!/bin/bash
# usage: nkaprj.sh source_file

s=$1
bname=`basename $s .hs`

joutdir=jout/$bname
prjdir=prjs/$bname

# compile
./tcompile.sh $s
rm $joutdir/Sample.java

# copy project skelton
if [ ! -f AndroidProjectPrototype/local.properties ]; then
    echo "Please make your local.properties."
    exit 1
fi

rm -rf $prjdir
mkdir -p prjs
cp -r AndroidProjectPrototype $prjdir

cp -r ../brt/src/jp $prjdir/app/src/main/java/
mv $prjdir/app/src/main/java/jp/ne/sakura/uhideyuki/brt/runtime/IOWrapper.java2 $prjdir/app/src/main/java/jp/ne/sakura/uhideyuki/brt/runtime/IOWrapper.java 

tmpfile=/tmp/tmp.$$
mv $prjdir/app/src/main/res/values/strings.xml $tmpfile
cat $tmpfile | sed -e "s/##APP_NAME##/$bname/;" > $prjdir/app/src/main/res/values/strings.xml

odir=$prjdir/app/src/main/java/com/example/runtimeproto/jout
mkdir -p odir
for f in $joutdir/*.java; do
    of=`basename $f`
    echo "package com.example.runtimeproto.jout;" > $odir/$of
    cat $f >> $odir/$of
done

