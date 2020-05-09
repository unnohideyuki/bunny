package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class LitFloat extends Literal { 
    public float value;
    public LitFloat(float x){ value = x; }
    public Boolean equals(Literal x){ return value == ((LitFloat)x).value; }

    public String inspect(){
	return "LitFloat(value='" + String.valueOf(value) + "')";
    }
}
