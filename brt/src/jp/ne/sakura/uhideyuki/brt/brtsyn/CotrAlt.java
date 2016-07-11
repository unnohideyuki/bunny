package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class CotrAlt extends Alt {
    public Cotr cotr;
    public Expr e;
    public CotrAlt(String name, Expr expr){
	cotr = new Cotr(name);
	e = expr;
    }
}
