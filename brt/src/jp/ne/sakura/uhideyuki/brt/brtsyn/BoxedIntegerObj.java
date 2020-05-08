package jp.ne.sakura.uhideyuki.brt.brtsyn;
import java.math.BigInteger;

public class BoxedIntegerObj extends HeapObj {
    public BigInteger value;
    public BoxedIntegerObj(LitInteger x){ value = x.value; }
    public String inspect(){
	return value.toString() + " :: Integer";
    }
}
