package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class PapObj extends HeapObj {
    public Expr f;
    public AtomExpr[] args;
    public PapObj(Expr g, AtomExpr[] as){ f = g; args = as; }
}
