import jp.ne.sakura.uhideyuki.brt.brtsyn.*;
import jp.ne.sakura.uhideyuki.brt.runtime.*;

/** Sample1 -- A sample of STG evaludation, corresponding to:

  main = let
    s = "Hello, Let Expression!"
   in putStrLn s

 */
public class Sample2 {
    public static void main(String[] args){
	System.out.println("main");
	RT.eval(Main.mkmain());
    }
}

class Main {
    public static Expr mkmain(){
	Expr[] es = {RTLib.fromJString("Hello, Let Expression!")};
	return new LetExpr(es, lammain);
    }

    static BDmain lammain = new BDmain();

    static class BDmain implements LambdaForm {
	public BDmain(){ System.out.println("BD"); }
	public int arity(){ return 1; }
	public Expr call(AtomExpr[] args){
	    return RTLib.app(RTLib.putStrLn, args[0]);
	}
    }
}


