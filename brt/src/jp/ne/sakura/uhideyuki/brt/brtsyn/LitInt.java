package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class LitInt extends Literal { 
    public int value; 
    public Boolean equals(Literal x){ return value == ((LitInt)x).value; }
}
