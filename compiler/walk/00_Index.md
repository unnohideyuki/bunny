Code Walk Through 01
====================

�J�n�F 2017-12-30


�͂��߂�
--------

�����n���`�� (DictPass) ���A���̉������� TGen �����̂܂܂����Ă����̂𒼂��A
�R���p�C���̌x���� Hlint �ɂ��q���g�ɂ���ʂ�Ή������̂ŁA
���ꂩ�炵�΂炭�́ACode Walk Through ��ʂ��Č���R�[�h�̗����A��萮�����������B

�ǂ�ł��邤���ɁA�R�[�h�̔��C���A���ǂ��������Ȃ�Ǝv���̂ŁA��p�̃u�����`
cwalk1 ��p�ӁB���򂷂钼�O�̃R�~�b�g�͈ȉ��F

````
commit 195c0f82dfef0a55644375f9f0c38bcd1f05d8c5 (origin/master, origin/HEAD, master)
Author: UNNO Hideyuki <unno.hideyuki@nifty.com>
Date:   Sat Dec 30 13:23:20 2017 +0900

    CodeGen.hs: 2 hints
````

Code Walkthroug ���́A�����ς���悤�ȕύX�͂����ɁA�v�ύX�_���L�^���Ă���������
�Ƃǂ߂邱�ƂƂ���B

��ƊJ�n�O�� logsummarize ���ʂ͈ȉ��̂Ƃ���F

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

���R�A�e�X�g�͂��ׂĒʂ��Ă����Ԃ��L�[�v����B

````
===================
Finished 37 tests.
===================
````






