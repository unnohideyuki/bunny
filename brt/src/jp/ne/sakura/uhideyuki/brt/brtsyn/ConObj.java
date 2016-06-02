package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class ConObj extends HeapObj {
    public Cotr cotr;
    public AtomExpr[] args;
    public ConObj(Cotr c, AtomExpr[] as){ cotr = c; args = as; }
}
