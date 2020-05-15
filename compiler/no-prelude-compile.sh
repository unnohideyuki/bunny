#!/bin/bash 
# usage (eg.) : $0 prog.hs [options]

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
      RT.eval(Prelude.mkmain());
    }
}
EOF


if [ $? -ne 0 ];then
    exit 1
else
    bin/bunnyc -d jout/$f --xlibrary-path ./lib -v --xno-implicit-prelude $* $source_file
fi
