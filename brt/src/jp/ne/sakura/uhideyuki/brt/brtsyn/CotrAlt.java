package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class CotrAlt extends Alt {
    public Cotr cotr;
    public Expr e;
    public CotrAlt(String name, Expr expr){
	cotr = new Cotr(name);
	e = expr;
    }
    public String inspect(){
	return "CotrAlt(cotr=" + cotr.inspect() + ", "
	    + "e=" + e.inspect() + ")";
    }
}
