package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class CaseExpr extends Expr {
    public Expr scrut;
    public Alt[] alts;
    public CaseExpr(Expr e, Alt[] as){ scrut=e; alts=as; }

    public String inspect(){
	String s1 = "CaseExpr(scrut=" + scrut.inspect() + ", ";
	String s2 = "alts=[";
	for (int i = 0; i < alts.length; i++){
	    if (i > 0){ s2 = s2 + ", "; }
	    s2 = s2 + alts[i].inspect();
	}
	s2 = s2 + "])";
	return s1 + s2;
    }
}
