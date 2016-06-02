package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class Cotr {
    public String ident;
    public Boolean equals(Cotr c){ return ident.equals(c.ident); }
    public Cotr(String s){ ident = s; }
}
