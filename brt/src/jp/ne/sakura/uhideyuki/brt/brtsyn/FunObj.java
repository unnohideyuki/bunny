package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class FunObj extends HeapObj {
    public int arity;
    public LambdaForm lambda;
    public FunObj(int a, LambdaForm lam){ arity = a; lambda = lam; }
    public String inspect(){
	return "FunObj(arity=" + String.valueOf(arity) + ", lambda="
	    + lambda.toString() + ")";
    }
}
