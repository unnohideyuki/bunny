import jp.ne.sakura.uhideyuki.brt.brtsyn.*;
import jp.ne.sakura.uhideyuki.brt.runtime.*;

/** Sample1 -- A sample of STG evaludation, corresponding to:

  main = let
    s = "Hello, Let Expression!"
   in putStrLn s

 */
public class Sample2 {
    public static void main(String[] args){
	Expr[] es = {RTLib.fromJString("Hello, Let Expression!")};
	Expr e = new LetExpr(es, Codes.body);
	RT.eval(e);
    }
}

class Codes {
    public static MainBody body = new MainBody();
}

class MainBody implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	return RTLib.app(RTLib.putStrLn, args[0]);
    }
}
