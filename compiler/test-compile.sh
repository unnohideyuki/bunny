#!/bin/bash 
# usage (eg.) : compile0 sample0

source_file=$1
shift

f=`basename $source_file .hs`

mkdir -p jout/$f

echo "source file: $source_file"
echo "dst dir: $jout/$f"


cat <<EOF > jout/$f/Sample.java
import jp.ne.sakura.uhideyuki.brt.brtsyn.*;
import jp.ne.sakura.uhideyuki.brt.runtime.*;

public class Sample {
    public static void main(String[] args){
      RT.eval(Main.mkmain());
    }
}
EOF


bin/bunnyc -d jout/$f --xno-implicit-prelude -v lib/Prelude.hs

if [ $? -ne 0 ];then
    exit 1
else
    bin/bunnyc -d jout/$f --xlibrary-path ./lib -v $* $source_file
fi
