package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class BoxedCharObj extends HeapObj {
    public int value;
    public BoxedCharObj(LitChar x){ value = x.value; }
    public String inspect(){
	return "'" + value + "' :: Char";
    }
}
