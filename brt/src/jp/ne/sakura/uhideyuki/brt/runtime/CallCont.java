package jp.ne.sakura.uhideyuki.brt.runtime;

import jp.ne.sakura.uhideyuki.brt.brtsyn.*;

public class CallCont extends Cont {
    public AtomExpr[] args;
    public CallCont(AtomExpr[] as){ args = as; }
}
