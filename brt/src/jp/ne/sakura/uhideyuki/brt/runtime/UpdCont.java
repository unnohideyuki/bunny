package jp.ne.sakura.uhideyuki.brt.runtime;

import jp.ne.sakura.uhideyuki.brt.brtsyn.*;

public class UpdCont extends Cont {
    public Expr x;
    public UpdCont(Expr t) { assert t.isVar(); x = t; }
}
