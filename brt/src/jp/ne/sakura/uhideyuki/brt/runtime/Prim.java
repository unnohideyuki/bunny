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

    public static Expr show$Int(BoxedIntObj x){
	return RTLib.fromJString(x.value.toString());
    }

    public static Expr show$Char(BoxedCharObj c){
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

    public static Expr mkintegerAdd(){
	return RTLib.mkFun(new IntegerAdd());
    }

    public static Expr mkintegerMul(){
	return RTLib.mkFun(new IntegerMul());
    }

    public static Expr mkintLt(){
	return mkintegerLt();
    }

    public static Expr mkintLe(){
	return mkintegerLe();
    }

    public static Expr mkintGe(){
	return mkintegerGe();
    }

    public static Expr mkintGt(){
	return mkintegerGt();
    }

    public static Expr mkintEq(){
	return mkintegerEq();
    }

    public static Expr mkintAdd(){
	return mkintegerAdd();
    }

    public static Expr mkintMul(){
	return mkintegerMul();
    }

    // (,) = Prim.(,)
    public static class PairFunc implements LambdaForm {
	public int arity(){ return 2;}
	public Expr call(AtomExpr[] args){
	    assert args.length == arity();
	    return new AtomExpr(new Var(new ConObj(new Cotr("Prim.(,)"),
						   args)));
	}
    }

    public static Expr mk_40__44__41_(){
	return RTLib.mkFun(new PairFunc());
    }
}

class ShowFunc implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);

	if (x instanceof AtomExpr && 
	    ((AtomExpr)x).a instanceof Var &&
	    ((Var)((AtomExpr)x).a).obj instanceof BoxedIntObj){
	    return Prim.show$Int((BoxedIntObj) ((Var)((AtomExpr)x).a).obj);
	} else if (x instanceof AtomExpr && 
	    ((AtomExpr)x).a instanceof Var &&
	    ((Var)((AtomExpr)x).a).obj instanceof BoxedCharObj){
	    return Prim.show$Char((BoxedCharObj) ((Var)((AtomExpr)x).a).obj);

	} else if (isList(x)){
	    Expr h = head(x);
	    Expr t = tail(x);

	    if (h instanceof AtomExpr && 
		((AtomExpr)h).a instanceof Var &&
		((Var)((AtomExpr)h).a).obj instanceof BoxedCharObj){
		return showstring((AtomExpr)x);
	    } else {
		return showlist((AtomExpr)x);
	    }

	} else if (isTuple2(x)){
	    String s = "(";

	    AtomExpr a = (AtomExpr) fst(x);
	    AtomExpr b = (AtomExpr) snd(x);

	    s += elemToString(a);
	    s += ",";
	    s += elemToString(b);
	    s += ")";

	    return RTLib.fromJString(s);

	} else if (x instanceof AtomExpr && 
	    ((AtomExpr)x).a instanceof Var &&
		   ((Var)((AtomExpr)x).a).obj instanceof ConObj){
	    ConObj co = (ConObj)((Var)((AtomExpr)x).a).obj;
	    String ident = co.cotr.ident;

	    String r = null;
	    if (ident == "Prim.True") {
		r = "True";
	    } else if (ident == "Prim.False") {
		r = "False";
	    } else {
		String[] t = ident.split("\\.", 0);
		r = t[t.length - 1];
		LambdaForm f = new ShowFunc();
		for (int i = 0; i < co.args.length; i++){
		    r += " ";
		    AtomExpr[] a = {co.args[i]};
		    r += RTLib.toJString(f.call(a));
		}
	    }
	    return RTLib.fromJString(r);
	} else {
	    System.out.println(x);
	    if (x instanceof AtomExpr){
		System.out.println(((AtomExpr)x).a);
	    }
	    /*return new ErrExpr("unsupported show");*/
	    return RTLib.fromJString("warn: unsupported show");
	}
    }

    boolean isList(Expr e){ return RTLib.isList(e); }
    Expr head(Expr e){ return RTLib.head(e); }
    Expr tail(Expr e){ return RTLib.tail(e); }

    boolean isTuple2(Expr e){
	if (e instanceof AtomExpr){
	    Atom a = ((AtomExpr)e).a;
	    if (a instanceof Var && ((Var)a).obj instanceof ConObj){
		Cotr c = ((ConObj)((Var)a).obj).cotr;
		return c.ident == "Prim.(,)";
	    }
	}
	return false;
    }

    Expr fst(Expr e){
	assert(isTuple2(e));
	Atom a = ((AtomExpr)e).a;
	ConObj obj = (ConObj)((Var)a).obj;
	Expr r = RT.eval(obj.args[0]);
	return r;
    }

    Expr snd(Expr e){
	assert(isTuple2(e));
	Atom a = ((AtomExpr)e).a;
	ConObj obj = (ConObj)((Var)a).obj;
	Expr r = RT.eval(obj.args[1]);
	return r;
    }

    Expr showstring(Expr x){
	assert(isList(x));
	String s = "\"" + RTLib.toJString(x) + "\"";
	return RTLib.fromJString(s);
    }

    Expr showlist(Expr x){
	assert(isList(x));
	String s = "[";
	int i = 0;

	while(isList(x)){
	    if (i++ > 0){
		s += ",";
	    }

	    AtomExpr e = (AtomExpr) head(x);
	    s += elemToString(e);
	    x = tail(x);
	}

	s += "]";

	return RTLib.fromJString(s);
    }

    // todo; make it more general, only for Int now.
    String elemToString(AtomExpr x){
	assert(((AtomExpr)x).a instanceof Var);
	assert(((Var)((AtomExpr)x).a).obj instanceof BoxedIntObj);
	BoxedIntObj t = (BoxedIntObj)((Var)((AtomExpr)x).a).obj;
	return t.value.toString();
    }
}

class CharLt implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();

	Expr lhs = RT.eval(args[0]);
	Expr rhs = RT.eval(args[1]);

	assert lhs.isBoxedChar();
	assert rhs.isBoxedChar();

	BoxedCharObj cl =
	    (BoxedCharObj)((Var)((AtomExpr)lhs).a).obj;
	BoxedCharObj cr =
	    (BoxedCharObj)((Var)((AtomExpr)rhs).a).obj;
	
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

	Expr lhs = RT.eval(args[0]);
	Expr rhs = RT.eval(args[1]);

	assert lhs.isBoxedChar();
	assert rhs.isBoxedChar();

	BoxedCharObj cl =
	    (BoxedCharObj)((Var)((AtomExpr)lhs).a).obj;
	BoxedCharObj cr =
	    (BoxedCharObj)((Var)((AtomExpr)rhs).a).obj;
	
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

	Expr lhs = RT.eval(args[0]);
	Expr rhs = RT.eval(args[1]);

	assert lhs.isBoxedChar();
	assert rhs.isBoxedChar();

	BoxedCharObj cl =
	    (BoxedCharObj)((Var)((AtomExpr)lhs).a).obj;
	BoxedCharObj cr =
	    (BoxedCharObj)((Var)((AtomExpr)rhs).a).obj;
	
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

	Expr lhs = RT.eval(args[0]);
	Expr rhs = RT.eval(args[1]);

	assert lhs.isBoxedChar();
	assert rhs.isBoxedChar();

	BoxedCharObj cl =
	    (BoxedCharObj)((Var)((AtomExpr)lhs).a).obj;
	BoxedCharObj cr =
	    (BoxedCharObj)((Var)((AtomExpr)rhs).a).obj;

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

	Expr lhs = RT.eval(args[0]);
	Expr rhs = RT.eval(args[1]);

	assert lhs.isBoxedChar();
	assert rhs.isBoxedChar();

	BoxedCharObj cl =
	    (BoxedCharObj)((Var)((AtomExpr)lhs).a).obj;
	BoxedCharObj cr =
	    (BoxedCharObj)((Var)((AtomExpr)rhs).a).obj;

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

	Expr lhs = RT.eval(args[0]);
	Expr rhs = RT.eval(args[1]);

	assert lhs.isBoxedInt();
	assert rhs.isBoxedInt();

	BoxedIntObj il =
	    (BoxedIntObj)((Var)((AtomExpr)lhs).a).obj;
	BoxedIntObj ir =
	    (BoxedIntObj)((Var)((AtomExpr)rhs).a).obj;
	
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

	Expr lhs = RT.eval(args[0]);
	Expr rhs = RT.eval(args[1]);

	assert lhs.isBoxedInt();
	assert rhs.isBoxedInt();

	BoxedIntObj il =
	    (BoxedIntObj)((Var)((AtomExpr)lhs).a).obj;
	BoxedIntObj ir =
	    (BoxedIntObj)((Var)((AtomExpr)rhs).a).obj;

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

	Expr lhs = RT.eval(args[0]);
	Expr rhs = RT.eval(args[1]);

	assert lhs.isBoxedInt();
	assert rhs.isBoxedInt();

	BoxedIntObj il =
	    (BoxedIntObj)((Var)((AtomExpr)lhs).a).obj;
	BoxedIntObj ir =
	    (BoxedIntObj)((Var)((AtomExpr)rhs).a).obj;

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

	Expr lhs = RT.eval(args[0]);
	Expr rhs = RT.eval(args[1]);

	assert lhs.isBoxedInt();
	assert rhs.isBoxedInt();

	BoxedIntObj il =
	    (BoxedIntObj)((Var)((AtomExpr)lhs).a).obj;
	BoxedIntObj ir =
	    (BoxedIntObj)((Var)((AtomExpr)rhs).a).obj;
	
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

	Expr lhs = RT.eval(args[0]);
	Expr rhs = RT.eval(args[1]);

	assert lhs.isBoxedInt();
	assert rhs.isBoxedInt();

	BoxedIntObj il =
	    (BoxedIntObj)((Var)((AtomExpr)lhs).a).obj;
	BoxedIntObj ir =
	    (BoxedIntObj)((Var)((AtomExpr)rhs).a).obj;

	if (il.value == ir.value){
	    return Prim.mkTrue();
	} else {
	    return Prim.mkFalse();
	}
    }
}

class IntegerAdd implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();

	Expr lhs = RT.eval(args[0]);
	Expr rhs = RT.eval(args[1]);

	assert lhs.isBoxedInt();
	assert rhs.isBoxedInt();

	BoxedIntObj il =
	    (BoxedIntObj)((Var)((AtomExpr)lhs).a).obj;
	BoxedIntObj ir =
	    (BoxedIntObj)((Var)((AtomExpr)rhs).a).obj;

	LitInt r = new LitInt(il.value + ir.value);

	return new AtomExpr(r);
    }
}

class IntegerMul implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();

	Expr lhs = RT.eval(args[0]);
	Expr rhs = RT.eval(args[1]);

	assert lhs.isBoxedInt();
	assert rhs.isBoxedInt();

	BoxedIntObj il =
	    (BoxedIntObj)((Var)((AtomExpr)lhs).a).obj;
	BoxedIntObj ir =
	    (BoxedIntObj)((Var)((AtomExpr)rhs).a).obj;

	LitInt r = new LitInt(il.value * ir.value);

	return new AtomExpr(r);
    }
}
