package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class LitInt extends Literal { 
    public long value; 
    public LitInt(long x){ value = x; }
    public Boolean equals(Literal x){ return value == ((LitInt)x).value; }

    public String inspect(){
	return "LitInt(value='" + String.valueOf(value) + "')";
    }
}
