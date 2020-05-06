package jp.ne.sakura.uhideyuki.brt.brtsyn;
import java.math.BigInteger;

public class LitInteger extends Literal { 
    public BigInteger value; 
    public LitInteger(String s){ value = new BigInteger(s); }
    public LitInteger(BigInteger v){ value = v; }
    public Boolean equals(Literal x){ return value.compareTo(((LitInteger)x).value) == 0; }

    public String inspect(){
	return "LitInteger(value='" + value.toString() + "')";
    }
}
