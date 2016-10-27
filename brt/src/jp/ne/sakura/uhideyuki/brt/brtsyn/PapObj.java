package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class PapObj extends HeapObj {
    public Expr f;
    public AtomExpr[] args;
    public PapObj(Expr g, AtomExpr[] as){ f = g; args = as; }
    public String inspect(){
	String s1 = "PapObj(f=" + f.inspect() + ", ";
	String s2 = "args=[";
	for (int i = 0; i < args.length; i++){
	    if (i > 0){ s2 = s2 + ", "; }
	    s2 = s2 + args[i].inspect();
	}
	s2 = s2 + "])";
	return s1 + s2;
    }
}
