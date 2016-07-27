import jp.ne.sakura.uhideyuki.brt.brtsyn.*;
import jp.ne.sakura.uhideyuki.brt.runtime.*;

public class Main {
    public static Expr mkmain(){
      Expr t0a = Main.mk_62__62_();
      Expr t0b = (Expr) new AtomExpr(new Dict( new DictMonadIO()));
      Expr t0 = RTLib.app(t0a, t0b); // (>>=) $MonadIO
      Expr t1 = Prim.mkputStrLn();
      Expr t2 = RTLib.fromJString("Hello! First contact with ...");
      Expr t3 = RTLib.app(t1, t2);
      Expr t4 = RTLib.app(t0, t3);
      Expr t5 = Prim.mkputStrLn();
      Expr t6 = RTLib.fromJString("an overloaded function!");
      Expr t7 = RTLib.app(t5, t6);
      Expr t8 = RTLib.app(t4, t7);
      return t8;
    }

    // Overloaded functions for Monad.
    // Main.(>>) :: $Monad -> m a -> (a -> m b) -> m b
    public static Expr mk_62__62_(){
	Expr t0 = RTLib.mkFun(new BindLAM());
	return t0;
    }

    static class BindLAM implements LambdaForm {
	public int arity(){ return 1; }
	public Expr call(AtomExpr[] args){
	    DictMonad d = (DictMonad) RTLib.extrDict(args[0]);
	    return d.mk_62__62_();
	}
    }
}

// Note: TODO: DictMonad and DictMonadIO should be public.

// class Monad declaration generates:
//   - abstract class DictMonad
//   - overloaded method definitions
abstract class DictMonad {
    abstract public Expr mk_62__62_();
}

// instance Monad IO declaration generates a dictionary instance.
class DictMonadIO extends DictMonad {
    // (>>=) = Prim.bindIO
    public Expr mk_62__62__61_(){
	return Prim.mkbindIO();
    }

    // f >> g = f >>= \_ -> g
    public Expr mk_62__62_(){
	Expr t0 = RTLib.mkFun(new LAM1());
	return t0;
    }

    class LAM1 implements LambdaForm {
	public int arity(){ return 2; }
	public Expr call (AtomExpr[] args){
	    Expr t0 = RTLib.mkFun(new LAMx(args[1]));
	    return RTLib.app(mk_62__62__61_(), args[0], t0);
	}
    }

    class LAMx implements LambdaForm {
	public int arity(){ return 1; }
	public Expr call (AtomExpr[] args){
	    return e;
	}
	public Expr e;
	public LAMx(Expr expr){ e = expr; }
    }
}


