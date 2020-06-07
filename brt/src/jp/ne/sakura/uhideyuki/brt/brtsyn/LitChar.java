package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class LitChar extends Literal {
    public int value; // Unicode codepoint
    public LitChar(int c){ value = c; }
    public Boolean equals(Literal x){ return value == ((LitChar)x).value; }

    public String inspect(){
	return "LitChar(value='" + String.valueOf(value) + "')";
    }
}

