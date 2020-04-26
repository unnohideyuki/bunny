package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class CompositDict extends Atom {
    public AtomExpr d;
    public AtomExpr[] ds;
    public CompositDict(AtomExpr dict, AtomExpr[] dicts){ d = dict; ds = dicts; }

    public String inspect(){
	String s1 = "CompositDict(d=" + d.inspect() + ", ";
	String s2 = "ds=[";
	for (int i = 0; i < ds.length; i++){
	    if (i > 0){ s2 = s2 + ", "; }
	    s2 = s2 + ds[i].inspect();
	}
	s2 = s2 + "])";
	return s1 + s2;
    }
}
