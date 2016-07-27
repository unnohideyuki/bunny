package jp.ne.sakura.uhideyuki.brt.runtime;

import jp.ne.sakura.uhideyuki.brt.brtsyn.*;

public class Prim {
    private static Expr mkExpr(HeapObj obj){
	return new AtomExpr(new Var(obj));
    }

    public static Expr mkputStrLn(){
	return RTLib.putStrLn;
    }

    public static Expr mkTrue(){
	return mkExpr(new ConObj(new Cotr("Prim.True"), new AtomExpr[0]));
    }

    public static Expr mkFalse(){
	return mkExpr(new ConObj(new Cotr("Prim.False"), new AtomExpr[0]));
    }

    public static Expr mkneErr(){
	return new ErrExpr("Error: Non-exhaustive patterns.");
    }

    public static Expr mkshow(){
	return RTLib.mkFun(new ShowFunc());
    }

    public static Expr show$Int(AtomExpr x){
	LitInt l = (LitInt) x.a;
	return RTLib.fromJString(l.value.toString());
    }

    // for Monad IO
    // (>>=) = Prim.bindIO
    public static class BindIOFunc implements LambdaForm {
	public int arity(){ return 2; }
	public Expr call(AtomExpr[] args){
	    Expr t0 = RT.eval(args[0]);
	    return RTLib.app(args[1], t0);
	}
    }
    public static Expr mkbindIO(){
	return RTLib.mkFun(new BindIOFunc());
    }

    // return = Prim.retIO
    public static class RetIOFunc implements LambdaForm {
	public int arity(){ return 1; }
	public Expr call(AtomExpr[] args){
	    assert args.length == arity();
	    return new AtomExpr(new Var(new ConObj(new Cotr("Main.IO"), args)));
	}
    }
    public static Expr mkretIO(){
	return RTLib.mkFun(new RetIOFunc());
    }

    // fail s = Prim.failIO s
    public static Expr mkfailIO(){
	assert(false); // todo: to be implemented.
	return null;
    }
}

class ShowFunc implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	AtomExpr x = args[0];

	if (x.a instanceof LitInt){
	    return Prim.show$Int(x);
	} else {
	    return new ErrExpr("unsupported show");
	}
    }
}



