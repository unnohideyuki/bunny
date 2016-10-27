package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class Dict extends Atom {
    public Object obj;
    public Dict(Object x){ obj = x; }
    public String inspect(){ return "Dict(obj=" + obj.toString() + ")"; }
}
