package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class CaseExpr extends Expr {
    public Expr scrut;
    public Alt[] alts;
    public CaseExpr(Expr e, Alt[] as){ scrut=e; alts=as; }
}
