package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class AtomExpr extends Expr {
    public Atom a;
    public AtomExpr(Atom b){ a = b; }

    public String inspect(){
	return "AtomExpr(a=" + a.inspect() + ")";
    }
}
