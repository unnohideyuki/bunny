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

    public static Expr show$Char(AtomExpr x){
	LitChar c = (LitChar) x.a;
	return RTLib.fromJString("'" + c.value + "'");
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

    public static Expr mkcharLt(){
	return RTLib.mkFun(new CharLt());
    }

    public static Expr mkcharLe(){
	return RTLib.mkFun(new CharLe());
    }

    public static Expr mkcharGe(){
	return RTLib.mkFun(new CharGe());
    }

    public static Expr mkcharGt(){
	return RTLib.mkFun(new CharGt());
    }

    public static Expr mkcharEq(){
	return RTLib.mkFun(new CharEq());
    }

    public static Expr mkintegerLt(){
	return RTLib.mkFun(new IntegerLt());
    }

    public static Expr mkintegerLe(){
	return RTLib.mkFun(new IntegerLe());
    }

    public static Expr mkintegerGe(){
	return RTLib.mkFun(new IntegerGe());
    }

    public static Expr mkintegerGt(){
	return RTLib.mkFun(new IntegerGt());
    }

    public static Expr mkintegerEq(){
	return RTLib.mkFun(new IntegerEq());
    }
}

class ShowFunc implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = args[0];
	if (!x.isLitOrValue()){
	    x = RT.eval(x);
	}

	if (x instanceof AtomExpr && ((AtomExpr)x).a instanceof LitInt){
	    return Prim.show$Int((AtomExpr)x);
	} else if (x instanceof AtomExpr && ((AtomExpr)x).a instanceof LitChar){
	    return Prim.show$Char((AtomExpr)x);
	} else if (isList(x)){
	    Expr h = head(x);
	    Expr t = tail(x);

	    if (h instanceof AtomExpr && ((AtomExpr)h).a instanceof LitChar){
		return showstring((AtomExpr)x);
	    } else {
		return showlist((AtomExpr)x);
	    }
	} else {
	    System.out.println(x);
	    if (x instanceof AtomExpr){
		System.out.println(((AtomExpr)x).a);
	    }
	    /*return new ErrExpr("unsupported show");*/
	    return RTLib.fromJString("warn: unsupported show");
	}
    }

    boolean isList(Expr e){
	if (e instanceof AtomExpr){
	    Atom a = ((AtomExpr)e).a;
	    if (a instanceof Var && ((Var)a).obj instanceof ConObj){
		Cotr c = ((ConObj)((Var)a).obj).cotr;
		return c.ident == "Prim.:";
	    }
	}
	return false;
    }

    Expr head(Expr e){
	assert(isList(e));
	Atom a = ((AtomExpr)e).a;
	ConObj obj = (ConObj)((Var)a).obj;
	Expr r = obj.args[0];
	if (!r.isLitOrValue()){
	    r = RT.eval(r);
	}
	return r;
    }

    Expr tail(Expr e){
	assert(isList(e));
	Atom a = ((AtomExpr)e).a;
	ConObj obj = (ConObj)((Var)a).obj;
	Expr r = obj.args[1];
	if (!r.isLitOrValue()){
	    r = RT.eval(r);
	}
	return r;
    }

    Expr showstring(Expr x){
	assert(isList(x));
	String s = "\"";

	while(isList(x)){
	    AtomExpr e = (AtomExpr) head(x);
	    LitChar c = (LitChar) e.a;
	    s += c.value;
	    x = tail(x);
	}

	s += "\"";

	return RTLib.fromJString(s);
    }

    /* todo: only for [Int] */
    Expr showlist(Expr x){
	assert(isList(x));
	String s = "[";
	int i = 0;

	while(isList(x)){
	    if (i++ > 0){
		s += ",";
	    }

	    AtomExpr e = (AtomExpr) head(x);
	    assert(e.a instanceof LitInt);
	    LitInt l = (LitInt) e.a;
	    s += l.value.toString();
	    x = tail(x);
	}

	s += "]";

	return RTLib.fromJString(s);
    }
}

class CharLt implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();

	assert args[0].a instanceof LitChar;
	assert args[1].a instanceof LitChar;

	LitChar cl = (LitChar) args[0].a;
	LitChar cr = (LitChar) args[1].a;
	
	if (cl.value < cr.value){
	    return Prim.mkTrue();
	} else {
	    return Prim.mkFalse();
	}
    }
}

class CharLe implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();

	assert args[0].a instanceof LitChar;
	assert args[1].a instanceof LitChar;

	LitChar cl = (LitChar) args[0].a;
	LitChar cr = (LitChar) args[1].a;
	
	if (cl.value <= cr.value){
	    return Prim.mkTrue();
	} else {
	    return Prim.mkFalse();
	}
    }
}

class CharGe implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();

	assert args[0].a instanceof LitChar;
	assert args[1].a instanceof LitChar;

	LitChar cl = (LitChar) args[0].a;
	LitChar cr = (LitChar) args[1].a;
	
	if (cl.value >= cr.value){
	    return Prim.mkTrue();
	} else {
	    return Prim.mkFalse();
	}
    }
}

class CharGt implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();

	assert args[0].a instanceof LitChar;
	assert args[1].a instanceof LitChar;

	LitChar cl = (LitChar) args[0].a;
	LitChar cr = (LitChar) args[1].a;
	
	if (cl.value > cr.value){
	    return Prim.mkTrue();
	} else {
	    return Prim.mkFalse();
	}
    }
}

class CharEq implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();

	assert args[0].a instanceof LitChar;
	assert args[1].a instanceof LitChar;

	LitChar cl = (LitChar) args[0].a;
	LitChar cr = (LitChar) args[1].a;
	
	if (cl.value == cr.value){
	    return Prim.mkTrue();
	} else {
	    return Prim.mkFalse();
	}
    }
}

class IntegerLt implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();

	assert args[0].a instanceof LitInt;
	assert args[1].a instanceof LitInt;

	LitInt il = (LitInt) args[0].a;
	LitInt ir = (LitInt) args[1].a;
	
	if (il.value < ir.value){
	    return Prim.mkTrue();
	} else {
	    return Prim.mkFalse();
	}
    }
}

class IntegerLe implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();

	assert args[0].a instanceof LitInt;
	assert args[1].a instanceof LitInt;

	LitInt il = (LitInt) args[0].a;
	LitInt ir = (LitInt) args[1].a;
	
	if (il.value <= ir.value){
	    return Prim.mkTrue();
	} else {
	    return Prim.mkFalse();
	}
    }
}

class IntegerGe implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();

	assert args[0].a instanceof LitInt;
	assert args[1].a instanceof LitInt;

	LitInt il = (LitInt) args[0].a;
	LitInt ir = (LitInt) args[1].a;
	
	if (il.value >= ir.value){
	    return Prim.mkTrue();
	} else {
	    return Prim.mkFalse();
	}
    }
}

class IntegerGt implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();

	assert args[0].a instanceof LitInt;
	assert args[1].a instanceof LitInt;

	LitInt il = (LitInt) args[0].a;
	LitInt ir = (LitInt) args[1].a;
	
	if (il.value > ir.value){
	    return Prim.mkTrue();
	} else {
	    return Prim.mkFalse();
	}
    }
}

class IntegerEq implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();

	assert args[0].a instanceof LitInt;
	assert args[1].a instanceof LitInt;

	LitInt il = (LitInt) args[0].a;
	LitInt ir = (LitInt) args[1].a;
	
	if (il.value == ir.value){
	    return Prim.mkTrue();
	} else {
	    return Prim.mkFalse();
	}
    }
}

