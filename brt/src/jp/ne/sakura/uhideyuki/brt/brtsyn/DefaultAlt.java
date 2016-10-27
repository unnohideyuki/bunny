package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class DefaultAlt extends Alt {
    public Expr e;
    public DefaultAlt(Expr expr){ e = expr; }
    public String inspect(){
	return "DefaultAlt(e=" + e.inspect() + ")";
    }
}
