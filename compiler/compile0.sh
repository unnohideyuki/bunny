#!/bin/bash -v
# usage (eg.) : compile0 sample0

f=$1

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

sample/compiler0 < testcases/$f.hs > jout/$f/Main.java

