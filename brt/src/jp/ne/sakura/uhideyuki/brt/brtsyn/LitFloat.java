package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class LitFloat extends Literal { 
    public Double value; 
    public Boolean equals(Literal x){ return value == ((LitFloat)x).value; }

    public String inspect(){
	return "LitFloat(value='" + String.valueOf(value) + "')";
    }
}
