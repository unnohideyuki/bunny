import jp.ne.sakura.uhideyuki.brt.brtsyn.*;
import jp.ne.sakura.uhideyuki.brt.runtime.*;

/** Sample7 -- A sample of STG evaludation, corresponding to:

  main = let
    f = putStrLn
    e = f "Hello, Let Expression!"
   in e

  This should be converted as follows (Closure Conversion):

  main = let
    f = putStrLn
    e f = f "Hello, Let Expression!"
   in e f


 */
public class Sample7 {
    public static void main(String[] args){
	Expr f = RTLib.putStrLn;
	Expr e = Codes.func_e;
	Expr[] es = {f, e};
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

// \f e -> e f
class MainBody implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	return RTLib.app(args[1], args[0]);
    }
}

// e f = f "Hello, Free Variable!"
class EBody implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	return RTLib.app(args[0], RTLib.fromJString("Hello, Free Variable!"));
    }
}
