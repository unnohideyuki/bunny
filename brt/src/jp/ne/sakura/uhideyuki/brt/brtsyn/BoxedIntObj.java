package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class BoxedIntObj extends HeapObj {
    public Integer value;
    public BoxedIntObj(LitInt x){ value = x.value; }
    public String inspect(){
	return String.valueOf(value);
    }
}
