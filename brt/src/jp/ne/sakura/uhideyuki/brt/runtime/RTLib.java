package jp.ne.sakura.uhideyuki.brt.runtime;

import jp.ne.sakura.uhideyuki.brt.brtsyn.*;

import java.util.Arrays;

class ConsFunc implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == 2;
	return new AtomExpr(new Var(new ConObj(new Cotr("Prim.:"), args)));
    }
}

class PutStrLnFunc implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	String t = RTLib.toJString(RT.eval(args[0]));
	System.out.println(t);
	return RTLib.app(Prim.mkretIO(), RTLib.unit);
    }
}

public class RTLib {
    private static Expr mkExpr(HeapObj obj){
	return new AtomExpr(new Var(obj));
    }

    private static Expr mkExpr(char c){
	return new AtomExpr(new LitChar(c));
    }

    public static Expr mkFun(LambdaForm lam){
	return mkExpr(new FunObj(lam.arity(), lam));
    }

    public static Expr cons = mkFun(new ConsFunc());

    public static boolean isList(Expr e){
	if (e instanceof AtomExpr){
	    Atom a = ((AtomExpr)e).a;
	    if (a instanceof Var && ((Var)a).obj instanceof ConObj){
		Cotr c = ((ConObj)((Var)a).obj).cotr;
		return c.ident == "Prim.:";
	    }
	}
	return false;
    }

    public static Expr head(Expr e){
	assert(isList(e));
	Atom a = ((AtomExpr)e).a;
	ConObj obj = (ConObj)((Var)a).obj;
	Expr r = RT.eval(obj.args[0]);
	return r;
    }

    public static Expr tail(Expr e){
	assert(isList(e));
	Atom a = ((AtomExpr)e).a;
	ConObj obj = (ConObj)((Var)a).obj;
	Expr r = RT.eval(obj.args[1]);
	return r;
    }

    public static String toJString(Expr x){
	StringBuilder t = new StringBuilder();

	while(isList(x)){
	    AtomExpr e = (AtomExpr) head(x);
	    assert(((AtomExpr)e).a instanceof Var);
	    assert(((Var)((AtomExpr)e).a).obj instanceof BoxedCharObj);
	    BoxedCharObj c = (BoxedCharObj)((Var)((AtomExpr)e).a).obj;
	    t.append(c.value);
	    x = tail(x);
	}

	return t.toString();
    }

    public static Expr unit =
	mkExpr(new ConObj(new Cotr("Prim.()"), new AtomExpr[0]));

    public static Expr nil =
	mkExpr(new ConObj(new Cotr("Prim.[]"), new AtomExpr[0]));

    public static Expr app(Expr f, Expr a1, Expr a2){
	assert a1 instanceof AtomExpr;
	assert a2 instanceof AtomExpr;
	AtomExpr[] args = {(AtomExpr) a1, (AtomExpr) a2};
	return mkExpr(new Thunk(new FunAppExpr(f, args, -1)));
    }
	       
    public static Expr app(Expr f, Expr a){
	assert a instanceof AtomExpr;
	AtomExpr[] args = {(AtomExpr) a};
	assert (f.isFunObj() || f.isThunk() || f.isPapObj());
	return mkExpr(new Thunk(new FunAppExpr(f, args, -1)));
    }

    // mkApp -- a variation of app(f, a) that ensures f and a are AtomExprs.
    // TODO: This version is really necessary?
    //       Can the code generator ensure it?
    // 2016-10-13: Fun or Thunk or Pap check is moved to FunAppExpr constructor
    public static Expr mkApp(Expr f, Expr a){
	if (!(a instanceof AtomExpr)){
	    a = new AtomExpr(new Var(new Thunk(a)));
	}

	AtomExpr[] args = {(AtomExpr) a};
	return mkExpr(new Thunk(new FunAppExpr(f, args, -1)));
    }

    public static Expr mkApp(Expr f, Expr[] as){
	AtomExpr[] args = new AtomExpr[as.length];

	for(int i = 0; i < as.length; i++){
	    if (as[i] instanceof AtomExpr){
		args[i] = (AtomExpr) as[i];
	    } else {
		args[i] = new AtomExpr(new Var(new Thunk(as[i])));
	    }
	}

	return mkExpr(new Thunk(new FunAppExpr(f, args, -1)));
    }
	       
    private static Expr fromJCharArray(char[] s){
	if (s.length == 0){
	    return nil;
	} else if (s.length == 1){
	    // return app(cons, mkExpr(s[0]), nil);
	    AtomExpr[] args = {(AtomExpr) mkExpr(s[0]), (AtomExpr) nil};
	    return new AtomExpr(new Var(new ConObj(new Cotr("Prim.:"), args)));
	}
	
	char[] t = Arrays.copyOfRange(s, 1, s.length);
	// return app(cons, mkExpr(s[0]), fromJCharArray(t));
	AtomExpr[] args = {(AtomExpr) mkExpr(s[0]), 
			   (AtomExpr) fromJCharArray(t)};
	return new AtomExpr(new Var(new ConObj(new Cotr("Prim.:"), args)));
    }
	       
    public static Expr fromJString(String s){
	return fromJCharArray(s.toCharArray());
    }

    public static Expr putStrLn = mkFun(new PutStrLnFunc());

    public static Expr fromChar(char c){ return mkExpr(c); }

    public static Expr fromInteger(Integer i){
	return new AtomExpr(new LitInt(i));
    }

    public static Dictionary extrDict(AtomExpr e){
	if (!(e.a instanceof Dict)){
	    System.err.println("extrDict Error: " + e.inspect());
	}
	return ((Dict) e.a).d;
    }
}
