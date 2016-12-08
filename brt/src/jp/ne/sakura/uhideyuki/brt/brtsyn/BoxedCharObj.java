package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class BoxedCharObj extends HeapObj {
    public char value;
    public BoxedCharObj(LitChar x){ value = x.value; }
    public String inspect(){
	return "'" + value + "'";
    }
}
