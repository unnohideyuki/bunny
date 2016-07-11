package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class LitChar extends Literal {
    public char value;
    public LitChar(char c){ value = c; }
    public Boolean equals(Literal x){ return value == ((LitChar)x).value; }
}

