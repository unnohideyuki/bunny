import jp.ne.sakura.uhideyuki.brt.brtsyn.*;
import jp.ne.sakura.uhideyuki.brt.runtime.*;

public class Main {
    public static Expr mkmain(){
      Expr t0 = Main.mk_62__62_();
      Expr t1 = Prim.mkputStrLn();
      Expr t2 = RTLib.fromJString("Hello!");
      Expr t3 = RTLib.app(t1, t2);
      Expr t4 = RTLib.app(t0, t3);
      Expr t5 = Prim.mkputStrLn();
      Expr t6 = RTLib.fromJString("World!");
      Expr t7 = RTLib.app(t5, t6);
      Expr t8 = RTLib.app(t4, t7);
      return t8;
    }

    // Prototype of Prim.bindIO
    public static class LAM0 implements LambdaForm {
	public int arity(){ return 2; }
	public Expr call (AtomExpr[] args){
	    Expr t0 = RT.eval(args[0]);
	    return RTLib.app(args[1], t0);
	}
    }

    // >>=
    public static Expr mk_62__62__61_(){
	Expr t0 = RTLib.mkFun(new LAM0());
	return t0;
    }

    public static class LAM1 implements LambdaForm {
	public int arity(){ return 2; }
	public Expr call (AtomExpr[] args){
	    Expr t0 = RTLib.mkFun(new LAMx(args[1]));
	    return RTLib.app(mk_62__62__61_(), args[0], t0);
	}
    }

    static class LAMx implements LambdaForm {
	public int arity(){ return 1; }
	public Expr call (AtomExpr[] args){
	    return e;
	}
	public Expr e;
	public LAMx(Expr expr){ e = expr; }
    }

    // f >> g = f >>= \_ -> g
    public static Expr mk_62__62_(){
	Expr t0 = RTLib.mkFun(new LAM1());
	return t0;
    }
}


