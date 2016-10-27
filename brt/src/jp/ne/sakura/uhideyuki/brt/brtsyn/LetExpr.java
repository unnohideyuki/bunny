package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class LetExpr extends Expr {
    public Expr[] es;
    public LambdaForm lambda;
    public LetExpr(Expr[] xs, LambdaForm lam){ es = xs; lambda = lam; }
    public void setEs(Expr [] xs){ es = xs; }

    public String inspect(){
	String s1 = "LetExpr(lambda=" + lambda.toString() + ", ";
	String s2 = "es=[";
	for (int i = 0; i < es.length; i++){
	    if (i > 0){ s2 = s2 + ", "; }
	    s2 = s2 + es[i].inspect();
	}
	s2 = s2 + "])}";
	return s1 + s2;
    }
}
