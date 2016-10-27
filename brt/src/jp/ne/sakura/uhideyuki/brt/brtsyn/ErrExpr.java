package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class ErrExpr extends Expr {
    public String message;
    public ErrExpr(String m){ message = m; }
    public String inspect(){ return "ErrExpr(message=\"" + message + "\")"; }
}
