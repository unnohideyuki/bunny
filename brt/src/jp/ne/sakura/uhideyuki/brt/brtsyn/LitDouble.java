package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class LitDouble extends Literal { 
    public double value; 
    public LitDouble(double x){ value = x; }
    public Boolean equals(Literal x){ return value == ((LitDouble)x).value; }

    public String inspect(){
	return "LitDouble(value='" + String.valueOf(value) + "')";
    }
}
