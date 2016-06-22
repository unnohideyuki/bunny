package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class LetExpr extends Expr {
    public Expr[] es;
    public LambdaForm lambda;
    public LetExpr(Expr[] xs, LambdaForm lam){ es = xs; lambda = lam; }
    public void setEs(Expr [] xs){ es = xs; }
}
