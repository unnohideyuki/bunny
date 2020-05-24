#!/bin/bash 

source_file=$1
f=`basename $1 .hs`

rm -rf jout/$f
mkdir jout/$f

cat <<EOF > jout/$f/Sample.java
import jp.ne.sakura.uhideyuki.brt.brtsyn.*;
import jp.ne.sakura.uhideyuki.brt.runtime.*;

public class Sample {
    public static void main(String[] args){
      RT.eval(Main.mkmain());
    }
}
EOF

## ../bin/bunnyc -d jout/$f --xno-implicit-prelude ../lib/Prelude.hs
cp jout/Prelude/* jout/$f

if [ $? -ne 0 ];then
    exit 1
else
    ../bin/bunnyc -d jout/$f --xlibrary-path ../lib $source_file
fi

