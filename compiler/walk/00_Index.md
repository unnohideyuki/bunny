Code Walk Through 01
====================

開始： 2017-12-30


はじめに
--------

辞書渡し形式 (DictPass) が、実体化せずに TGen をそのままつかっていたのを直し、
コンパイラの警告や Hlint によるヒントにも一通り対応したので、
これからしばらくは、Code Walk Through を通じて現状コードの理解、問題整理をしたい。

読んでいるうちに、コードの微修正、改良をしたくなると思うので、専用のブランチ
cwalk1 を用意。分岐する直前のコミットは以下：

````
commit 195c0f82dfef0a55644375f9f0c38bcd1f05d8c5 (origin/master, origin/HEAD, master)
Author: UNNO Hideyuki <unno.hideyuki@nifty.com>
Date:   Sat Dec 30 13:23:20 2017 +0900

    CodeGen.hs: 2 hints
````

Code Walkthroug 中は、動作を変えるような変更はせずに、要変更点を記録していくだけに
とどめることとする。

作業開始前の logsummarize 結果は以下のとおり：

````
$ ./logsummarize.sh
src/Absyn.hs                    151 lines       0 warnings      0 hint
src/CodeGen.hs                  469 lines       0 warnings      2 hints
src/CompilerOpts.hs             91 lines        0 warnings      0 hint
src/Core.hs                     44 lines        0 warnings      0 hint
src/DDumpAssump.hs              15 lines        0 warnings      0 hint
src/DDumpCore.hs                15 lines        0 warnings      0 hint
src/Desugar.hs                  18 lines        0 warnings      0 hint
src/DictPass.hs                 221 lines       0 warnings      0 hint
src/NameMangle.hs               27 lines        0 warnings      0 hint
src/Pattern.hs                  121 lines       0 warnings      0 hint
src/PreDefined.hs               182 lines       0 warnings      0 hint
src/Semant.hs                   904 lines       0 warnings      0 hint
src/STG.hs                      78 lines        0 warnings      0 hint
src/Symbol.hs                   39 lines        0 warnings      0 hint
src/TrCore.hs                   257 lines       0 warnings      1 hint
src/TrSTG.hs                    38 lines        0 warnings      0 hint
src/Types.hs                    47 lines        0 warnings      0 hint
src/Typing.hs                   644 lines       0 warnings      0 hint
----
                                3361 lines      0 warnings      3 hints
````

当然、テストはすべて通っている状態をキープする。

````
===================
Finished 37 tests.
===================
````






