#!/bin/bash -v
# usage (eg.) : compile0 sample0

source_file=$1

f=`basename $1 .hs`

mkdir -p jout/$f

cat <<EOF > jout/$f/Sample.java
import jp.ne.sakura.uhideyuki.brt.brtsyn.*;
import jp.ne.sakura.uhideyuki.brt.runtime.*;

public class Sample {
    public static void main(String[] args){
      RT.eval(Main.mkmain());
    }
}
EOF


bin/bunnyc -d jout/$f --xno-implicit-prelude lib/Prelude.hs

bin/bunnyc -d jout/$f --xlibrary-path ./lib $source_file
