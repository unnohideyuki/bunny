package jp.ne.sakura.uhideyuki.brt.brtsyn;

public interface LambdaForm {
    public int arity();
    public Expr call(AtomExpr[] args);
}
