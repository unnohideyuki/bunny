package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class Dict extends Atom {
    public Dictionary d;
    public Dict(Dictionary x){ d = x; }
    public String inspect(){ return "Dict(d=" + d.toString() + ")"; }
}
