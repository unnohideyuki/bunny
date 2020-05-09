package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class BoxedDoubleObj extends HeapObj {
    public double value;
    public BoxedDoubleObj(LitDouble x){ value = x.value; }
    public String inspect(){
	return String.valueOf(value) + " :: Double";
    }
}
