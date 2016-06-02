import jp.ne.sakura.uhideyuki.brt.brtsyn.*;
import jp.ne.sakura.uhideyuki.brt.runtime.*;

/** Sample8 -- A sample of STG evaludation, corresponding to:

  main = let
    s = "Hello, Let Expression!"
    e = putStrLn s
   in e

  This should be converted as follows (Closure Conversion):

  main = let
    s = "Hello, Let Expression!"
    e s = putStrLn s
   in e s
    


 */
public class Sample8 {
    public static void main(String[] args){
	Expr s = RTLib.fromJString("Hello, Free Variables again!"); 
	Expr e = Codes.func_e;
	Expr[] es = {s, e};
	Expr expr = new LetExpr(es, Codes.body);
	RT.eval(expr);
    }
}

class Codes {
    public static MainBody body = new MainBody();

    private static Expr mkExpr(HeapObj obj){
	return new AtomExpr(new Var(obj));
    }

    private static Expr mkExpr(char c){
	return new AtomExpr(new LitChar(c));
    }

    private static Expr mkFun(LambdaForm lam){
	return mkExpr(new FunObj(lam.arity(), lam));
    }

    public static Expr func_e = mkFun(new EBody());
}

// \s e -> e s
class MainBody implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	return RTLib.app(args[1], args[0]);
    }
}

// e s = putStrLn s
class EBody implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	return RTLib.app(RTLib.putStrLn, args[0]);
    }
}
