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

    public String inspect(){
	String s1 = "FunAppExpr(arity=" + String.valueOf(arity) + ", ";
	String s2 = "f=" + f.inspect() + ", ";

	String s3 = "args = [";
	for (int i = 0; i < args.length; i++){
	    if (i > 0){
		s3 = s3 + ", ";
	    }
	    s3 = s3 + args[i].inspect();
	}
	s3 = s3 + "])";

	return s1 + s2 + s3;
    }
}
