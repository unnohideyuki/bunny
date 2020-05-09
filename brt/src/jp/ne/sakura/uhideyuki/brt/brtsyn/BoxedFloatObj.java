package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class BoxedFloatObj extends HeapObj {
    public float value;
    public BoxedFloatObj(LitFloat x){ value = x.value; }
    public String inspect(){
	return String.valueOf(value) + " :: Float";
    }
}
