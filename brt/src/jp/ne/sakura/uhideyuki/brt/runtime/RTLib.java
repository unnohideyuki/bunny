package jp.ne.sakura.uhideyuki.brt.runtime;

import jp.ne.sakura.uhideyuki.brt.brtsyn.*;

import java.util.Arrays;


class ConsFunc implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == 2;
	return new AtomExpr(new Var(new ConObj(new Cotr("Prelude.:"), args)));
    }
}

class SeqFunc implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr t = RT.eval(args[0]);
	return args[1];
    }
}

class PutCharFunc implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	AtomExpr e = (AtomExpr) RT.eval(args[0]);
	assert(((AtomExpr)e).a instanceof Var);
	assert(((Var)e.a).obj instanceof BoxedCharObj);
	BoxedCharObj c = (BoxedCharObj)((Var) e.a).obj;
	IOWrapper.print(Character.toChars(c.value));
	return RTLib.app(Prim.mkretIO(), RTLib.unit);
    }
}

class PutStrLnFunc implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	String t = RTLib.toJString(RT.eval(args[0]));
	IOWrapper.println(t);
	return RTLib.app(Prim.mkretIO(), RTLib.unit);
    }
}

class ErrorFunc implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	String t = RTLib.toJString(RT.eval(args[0]));
	IOWrapper.errorPrintln("error: " + t);
	System.exit(1);
	return null;
    }
}

public class RTLib {
    private static Expr mkExpr(HeapObj obj){
	return new AtomExpr(new Var(obj));
    }

    public static Expr mkFun(LambdaForm lam){
	return mkExpr(new FunObj(lam.arity(), lam));
    }

    private static Expr mkLitChar(int c){
	return new AtomExpr(new LitChar(c));
    }

    public static Expr cons = mkFun(new ConsFunc());

    public static boolean isList(Expr e){
	if (e instanceof AtomExpr){
	    Atom a = ((AtomExpr)e).a;
	    if (a instanceof Var && ((Var)a).obj instanceof ConObj){
		Cotr c = ((ConObj)((Var)a).obj).cotr;
		return c.ident == "Prelude.:";
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

	    if (! (((Var)((AtomExpr)e).a).obj instanceof BoxedCharObj)){
		IOWrapper.errorPrintln("Not a BoxedCharObj:" + e.inspect());
	    }
	    assert(((Var)((AtomExpr)e).a).obj instanceof BoxedCharObj);

	    BoxedCharObj c = (BoxedCharObj)((Var)((AtomExpr)e).a).obj;
	    t.append(Character.toChars(c.value));
	    x = tail(x);
	}

	return t.toString();
    }

    public static Expr unit =
	mkExpr(new ConObj(new Cotr("Prelude.()"), new AtomExpr[0]));

    public static Expr nil =
	mkExpr(new ConObj(new Cotr("Prelude.[]"), new AtomExpr[0]));

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
	       
    private static Expr fromJCharArray(int[] s){
	if (s.length == 0){
	    return nil;
	} else if (s.length == 1){
	    // return app(cons, mkExpr(s[0]), nil);
	    AtomExpr[] args = {(AtomExpr) mkLitChar(s[0]), (AtomExpr) nil};
	    return new AtomExpr(new Var(new ConObj(new Cotr("Prelude.:"), args)));
	}
	
	int[] t = Arrays.copyOfRange(s, 1, s.length);
	// return app(cons, mkExpr(s[0]), fromJCharArray(t));
	AtomExpr[] args = {(AtomExpr) mkLitChar(s[0]), 
			   (AtomExpr) fromJCharArray(t)};
	return new AtomExpr(new Var(new ConObj(new Cotr("Prelude.:"), args)));
    }
	       
    public static Expr fromJString(String s){
	int[] charArray = s.codePoints().toArray();
	return fromJCharArray(charArray);
    }

    public static Expr seq = mkFun(new SeqFunc());
    
    public static Expr putChar = mkFun(new PutCharFunc());
    
    public static Expr putStrLn = mkFun(new PutStrLnFunc());

    public static Expr fromChar(int c){ return mkLitChar(c); }

    public static Expr mkLitInteger(String s){
	return new AtomExpr(new LitInteger(s));
    }

    public static Dictionary extrDict(AtomExpr e){
	if (!(e.a instanceof Dict)){
	    IOWrapper.errorPrintln("extrDict Error: " + e.inspect());
	}
	return ((Dict) e.a).d;
    }

    public static Expr error = mkFun(new ErrorFunc());

}
