package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class Thunk extends HeapObj {
    public Expr e;
    public Thunk(Expr x){ e = x; }
    public String inspect(){
	return "Thunk(e=" + e.inspect() + ")";
    }
}
