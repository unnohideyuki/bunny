package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class FunAppExpr extends Expr {
    public Expr f;
    public AtomExpr[] args;
    public int arity;
    public FunAppExpr(Expr g, AtomExpr[] as, int n){
	assert n < 0 || as.length == n;

	if (g.isFunObj() || g.isThunk() || g.isPapObj()){
	    f = g;
	} else {
	    f = new AtomExpr(new Var(new Thunk(g)));
	}

	args = as;
	arity = n;
    }
}
