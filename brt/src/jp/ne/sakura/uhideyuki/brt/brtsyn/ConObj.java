package jp.ne.sakura.uhideyuki.brt.brtsyn;

public class ConObj extends HeapObj {
    public Cotr cotr;
    public AtomExpr[] args;
    public ConObj(Cotr c, AtomExpr[] as){ cotr = c; args = as; }

    public String inspect(){
	String s1 = "ConObj(cotr=" + cotr.inspect() + ", ";
	String s2 = "args=[";
	for(int i = 0; i < args.length; i++){
	    if (i > 0){ s2 = s2 + ", "; }
	    s2 = s2 + args[i].inspect();
	}
	s2 = s2 + "])";
	return s1 + s2;
    }
}
