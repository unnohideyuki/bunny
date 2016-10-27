package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class PrimOpExpr extends Expr {
    public AtomExpr[] args;
    public LambdaForm lambda;

    public String inspect(){
	String s1 = "PrimOpExpr(lambda=" + lambda.toString() + ", ";

	String s2 = "args = [";
	for (int i = 0; i < args.length; i++){
	    if (i > 0){
		s2 = s2 + ", ";
	    }
	    s2 = s2 + args[i].inspect();
	}
	s2 = s2 + "])";

	return s1 + s2;
    }
}
