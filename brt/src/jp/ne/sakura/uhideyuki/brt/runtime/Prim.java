package jp.ne.sakura.uhideyuki.brt.runtime;

import jp.ne.sakura.uhideyuki.brt.brtsyn.*;
import java.io.*;

public class Prim {
    private static Expr mkExpr(HeapObj obj){
	return new AtomExpr(new Var(obj));
    }

    public static Expr mkerror(){
	return RTLib.error;
    }

    public static Expr mkseq(){
	return RTLib.seq;
    }
    
    public static Expr mkputStrLn(){
	return RTLib.putStrLn;
    }

    public static Expr mkgetChar(){
	int c = 0;
	try {
	    c = System.in.read();
	}
	catch (IOException e){
	    // todo: handling the exception
	}
	return new AtomExpr(new LitChar((char) c));
    }

    public static Expr mkTrue(){
	return mkExpr(new ConObj(new Cotr("Prelude.True"), new AtomExpr[0]));
    }

    public static Expr mkFalse(){
	return mkExpr(new ConObj(new Cotr("Prelude.False"), new AtomExpr[0]));
    }

    public static Expr mk_40__41_(){
	return mkExpr(new ConObj(new Cotr("Prim.()"), new AtomExpr[0]));
    }

    public static Expr mkFAIL(){
	return new ErrExpr("Error: Non-exhaustive patterns.");
    }

    public static Expr mkintegerShow(){
	return RTLib.mkFun(new IntegerShowFunc());
    }

    public static Expr mkintShow(){
	return RTLib.mkFun(new IntShowFunc());
    }

    // for Monad IO
    // (>>=) = Prim.bindIO
    public static class BindIOFunc implements LambdaForm {
	public int arity(){ return 2; }
	public Expr call(AtomExpr[] args){
	    Expr t0 = (AtomExpr) RT.eval(args[0]);
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
	    return (Expr) args[0];
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

    public static Expr mkcharLe(){
	return RTLib.mkFun(new CharLe());
    }

    public static Expr mkcharEq(){
	return RTLib.mkFun(new CharEq());
    }

    public static Expr mkintToChar(){
	return RTLib.mkFun(new IntToChar());
    }
    
    public static Expr mkcharToInt(){
	return RTLib.mkFun(new CharToInt());
    }
    
    public static Expr mkintegerLe(){
	return RTLib.mkFun(new IntegerLe());
    }

    public static Expr mkintegerEq(){
	return RTLib.mkFun(new IntegerEq());
    }

    public static Expr mkintegerAdd(){
	return RTLib.mkFun(new IntegerAdd());
    }

    public static Expr mkintegerSub(){
	return RTLib.mkFun(new IntegerSub());
    }

    public static Expr mkintegerMul(){
	return RTLib.mkFun(new IntegerMul());
    }

    public static Expr mkintegerQuotRem(){
	return RTLib.mkFun(new IntegerQuotRem());
    }

    public static Expr mkintLe(){
	return RTLib.mkFun(new IntLe());
    }

    public static Expr mkintEq(){
	return RTLib.mkFun(new IntEq());
    }

    public static Expr mkintAdd(){
	return RTLib.mkFun(new IntAdd());
    }

    public static Expr mkintSub(){
	return RTLib.mkFun(new IntSub());
    }

    public static Expr mkintMul(){
	return RTLib.mkFun(new IntMul());
    }

    public static Expr mkintQuotRem(){
	return RTLib.mkFun(new IntQuotRem());
    }

    public static Expr mkintFromInteger(){
	return RTLib.mkFun(new IntFromInteger());
    }

    public static Expr mkintegerFromInt(){
	return RTLib.mkFun(new IntegerFromInt());
    }

    public static Expr mkdoubleLe(){
	return RTLib.mkFun(new DoubleLe());
    }

    public static Expr mkdoubleEq(){
	return RTLib.mkFun(new DoubleEq());
    }

    public static Expr mkdoubleAdd(){
	return RTLib.mkFun(new DoubleAdd());
    }

    public static Expr mkdoubleSub(){
	return RTLib.mkFun(new DoubleSub());
    }

    public static Expr mkdoubleMul(){
	return RTLib.mkFun(new DoubleMul());
    }

    public static Expr mkdoubleDiv(){
	return RTLib.mkFun(new DoubleDiv());
    }

    public static Expr mkdoubleSignum(){
	return RTLib.mkFun(new DoubleSignum());
    }

    public static Expr mkdoubleFromInteger(){
	return RTLib.mkFun(new DoubleFromInteger());
    }

    public static Expr mkdoubleShow(){
	return RTLib.mkFun(new DoubleShow());
    }

    public static Expr mkfloatLe(){
	return RTLib.mkFun(new FloatLe());
    }

    public static Expr mkfloatEq(){
	return RTLib.mkFun(new FloatEq());
    }

    public static Expr mkfloatAdd(){
	return RTLib.mkFun(new FloatAdd());
    }

    public static Expr mkfloatSub(){
	return RTLib.mkFun(new FloatSub());
    }

    public static Expr mkfloatMul(){
	return RTLib.mkFun(new FloatMul());
    }

    public static Expr mkfloatDiv(){
	return RTLib.mkFun(new FloatDiv());
    }

    public static Expr mkfloatSignum(){
	return RTLib.mkFun(new FloatSignum());
    }

    public static Expr mkfloatFromInteger(){
	return RTLib.mkFun(new FloatFromInteger());
    }

    public static Expr mkfloatShow(){
	return RTLib.mkFun(new FloatShow());
    }

    // (,) = Prelude.(,)
    public static class PairFunc implements LambdaForm {
	public int arity(){ return 2;}
	public Expr call(AtomExpr[] args){
	    assert args.length == arity();
	    return new AtomExpr(new Var(new ConObj(new Cotr("Prelude.(,)"),
						   args)));
	}
    }

    public static Expr mk_40__44__41_(){
	return RTLib.mkFun(new PairFunc());
    }
}

class IntegerShowFunc implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);

	if (x instanceof AtomExpr && 
	    ((AtomExpr)x).a instanceof Var &&
	    ((Var)((AtomExpr)x).a).obj instanceof BoxedIntegerObj){
	    return show$Integer((BoxedIntegerObj) ((Var)((AtomExpr)x).a).obj);
	} else {
	    return new ErrExpr("IntegerShowFunc: must not occur");
	}
    }
    
    private static Expr show$Integer(BoxedIntegerObj x){
	return RTLib.fromJString(x.value.toString());
    }
}

class IntShowFunc implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);

	if (x instanceof AtomExpr && 
	    ((AtomExpr)x).a instanceof Var &&
	    ((Var)((AtomExpr)x).a).obj instanceof BoxedIntObj){
	    return show$Int((BoxedIntObj) ((Var)((AtomExpr)x).a).obj);
	} else {
	    return new ErrExpr("IntShowFunc: must not occur");
	}
    }
    
    private static Expr show$Int(BoxedIntObj x){
	return RTLib.fromJString(String.valueOf(x.value));
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

class IntToChar implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();

	Expr x = RT.eval(args[0]);
	assert x.isBoxedInt();

	BoxedIntObj i = (BoxedIntObj)((Var)((AtomExpr)x).a).obj;
	return new AtomExpr(new LitChar((int)i.value));
    }
}

class CharToInt implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();

	Expr x = RT.eval(args[0]);
	assert x.isBoxedChar();

	BoxedCharObj c = (BoxedCharObj)((Var)((AtomExpr)x).a).obj;
	return new AtomExpr(new LitInt(c.value));
    }
}

class IntegerLe implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();

	Expr lhs = RT.eval(args[0]);
	Expr rhs = RT.eval(args[1]);

	assert lhs.isBoxedInteger();
	assert rhs.isBoxedInteger();

	BoxedIntegerObj il =
	    (BoxedIntegerObj)((Var)((AtomExpr)lhs).a).obj;
	BoxedIntegerObj ir =
	    (BoxedIntegerObj)((Var)((AtomExpr)rhs).a).obj;

	if (il.value.compareTo(ir.value) <= 0){
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

	assert lhs.isBoxedInteger();
	assert rhs.isBoxedInteger();

	BoxedIntegerObj il =
	    (BoxedIntegerObj)((Var)((AtomExpr)lhs).a).obj;
	BoxedIntegerObj ir =
	    (BoxedIntegerObj)((Var)((AtomExpr)rhs).a).obj;

	if (il.value.compareTo(ir.value) == 0){
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

	assert lhs.isBoxedInteger();
	assert rhs.isBoxedInteger();

	BoxedIntegerObj il =
	    (BoxedIntegerObj)((Var)((AtomExpr)lhs).a).obj;
	BoxedIntegerObj ir =
	    (BoxedIntegerObj)((Var)((AtomExpr)rhs).a).obj;

	LitInteger r = new LitInteger(il.value.add(ir.value));

	return new AtomExpr(r);
    }
}

class IntegerSub implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();

	Expr lhs = RT.eval(args[0]);
	Expr rhs = RT.eval(args[1]);

	assert lhs.isBoxedInteger();
	assert rhs.isBoxedInteger();

	BoxedIntegerObj il =
	    (BoxedIntegerObj)((Var)((AtomExpr)lhs).a).obj;
	BoxedIntegerObj ir =
	    (BoxedIntegerObj)((Var)((AtomExpr)rhs).a).obj;

	LitInteger r = new LitInteger(il.value.subtract(ir.value));

	return new AtomExpr(r);
    }
}

class IntegerMul implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();

	Expr lhs = RT.eval(args[0]);
	Expr rhs = RT.eval(args[1]);

	assert lhs.isBoxedInteger();
	assert rhs.isBoxedInteger();

	BoxedIntegerObj il =
	    (BoxedIntegerObj)((Var)((AtomExpr)lhs).a).obj;
	BoxedIntegerObj ir =
	    (BoxedIntegerObj)((Var)((AtomExpr)rhs).a).obj;

	LitInteger r = new LitInteger(il.value.multiply(ir.value));

	return new AtomExpr(r);
    }
}

class IntegerQuotRem implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();

	Expr lhs = RT.eval(args[0]);
	Expr rhs = RT.eval(args[1]);

	assert lhs.isBoxedInteger();
	assert rhs.isBoxedInteger();

	BoxedIntegerObj il =
	    (BoxedIntegerObj)((Var)((AtomExpr)lhs).a).obj;
	BoxedIntegerObj ir =
	    (BoxedIntegerObj)((Var)((AtomExpr)rhs).a).obj;

	LitInteger q = new LitInteger(il.value.divide(ir.value));
	LitInteger r = new LitInteger(il.value.remainder(ir.value));

	AtomExpr[] a = {new AtomExpr(q), new AtomExpr(r)};
	return new AtomExpr(new Var(new ConObj(new Cotr("Prelude.(,)"), a)));
    }
}

class IntLe implements LambdaForm {
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

class IntEq implements LambdaForm {
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

class IntAdd implements LambdaForm {
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

class IntSub implements LambdaForm {
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

	LitInt r = new LitInt(il.value - ir.value);

	return new AtomExpr(r);
    }
}

class IntMul implements LambdaForm {
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

class IntQuotRem implements LambdaForm {
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

	LitInt q = new LitInt(il.value / ir.value);
	LitInt r = new LitInt(il.value % ir.value);

	AtomExpr[] a = {new AtomExpr(q), new AtomExpr(r)};
	return new AtomExpr(new Var(new ConObj(new Cotr("Prelude.(,)"), a)));
    }
}

class IntFromInteger implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedInteger();

	BoxedIntegerObj ix =
	    (BoxedIntegerObj)((Var)((AtomExpr)x).a).obj;

	return new AtomExpr(new LitInt(ix.value.longValue()));
    }
}

class IntegerFromInt implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedInt();

	BoxedIntObj ix =
	    (BoxedIntObj)((Var)((AtomExpr)x).a).obj;

	return new AtomExpr(new LitInteger(ix.value));
    }
}

class DoubleLe implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();

	Expr lhs = RT.eval(args[0]);
	Expr rhs = RT.eval(args[1]);

	assert lhs.isBoxedDouble();
	assert rhs.isBoxedDouble();

	BoxedDoubleObj il =
	    (BoxedDoubleObj)((Var)((AtomExpr)lhs).a).obj;
	BoxedDoubleObj ir =
	    (BoxedDoubleObj)((Var)((AtomExpr)rhs).a).obj;

	if (il.value <= ir.value){
	    return Prim.mkTrue();
	} else {
	    return Prim.mkFalse();
	}
    }
}

class DoubleEq implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();

	Expr lhs = RT.eval(args[0]);
	Expr rhs = RT.eval(args[1]);

	assert lhs.isBoxedDouble();
	assert rhs.isBoxedDouble();

	BoxedDoubleObj il =
	    (BoxedDoubleObj)((Var)((AtomExpr)lhs).a).obj;
	BoxedDoubleObj ir =
	    (BoxedDoubleObj)((Var)((AtomExpr)rhs).a).obj;

	if (il.value == ir.value){
	    return Prim.mkTrue();
	} else {
	    return Prim.mkFalse();
	}
    }
}

class DoubleAdd implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();

	Expr lhs = RT.eval(args[0]);
	Expr rhs = RT.eval(args[1]);

	assert lhs.isBoxedDouble();
	assert rhs.isBoxedDouble();

	BoxedDoubleObj il =
	    (BoxedDoubleObj)((Var)((AtomExpr)lhs).a).obj;
	BoxedDoubleObj ir =
	    (BoxedDoubleObj)((Var)((AtomExpr)rhs).a).obj;

	LitDouble r = new LitDouble(il.value + ir.value);

	return new AtomExpr(r);
    }
}

class DoubleSub implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();

	Expr lhs = RT.eval(args[0]);
	Expr rhs = RT.eval(args[1]);

	assert lhs.isBoxedDouble();
	assert rhs.isBoxedDouble();

	BoxedDoubleObj il =
	    (BoxedDoubleObj)((Var)((AtomExpr)lhs).a).obj;
	BoxedDoubleObj ir =
	    (BoxedDoubleObj)((Var)((AtomExpr)rhs).a).obj;

	LitDouble r = new LitDouble(il.value - ir.value);

	return new AtomExpr(r);
    }
}

class DoubleMul implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();

	Expr lhs = RT.eval(args[0]);
	Expr rhs = RT.eval(args[1]);

	assert lhs.isBoxedDouble();
	assert rhs.isBoxedDouble();

	BoxedDoubleObj il =
	    (BoxedDoubleObj)((Var)((AtomExpr)lhs).a).obj;
	BoxedDoubleObj ir =
	    (BoxedDoubleObj)((Var)((AtomExpr)rhs).a).obj;

	LitDouble r = new LitDouble(il.value * ir.value);

	return new AtomExpr(r);
    }
}

class DoubleDiv implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();

	Expr lhs = RT.eval(args[0]);
	Expr rhs = RT.eval(args[1]);

	assert lhs.isBoxedDouble();
	assert rhs.isBoxedDouble();

	BoxedDoubleObj il =
	    (BoxedDoubleObj)((Var)((AtomExpr)lhs).a).obj;
	BoxedDoubleObj ir =
	    (BoxedDoubleObj)((Var)((AtomExpr)rhs).a).obj;

	LitDouble r = new LitDouble(il.value / ir.value);

	return new AtomExpr(r);
    }
}

class DoubleSignum implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedDouble();

	BoxedDoubleObj ix =
	    (BoxedDoubleObj)((Var)((AtomExpr)x).a).obj;

	if (ix.value > 0){
	    return new AtomExpr(new LitDouble(1.0));
	} else if (ix.value == 0) {
	    return new AtomExpr(new LitDouble(0.0));
	} else {
	    return new AtomExpr(new LitDouble(-1.0));
	}
    }
}

class DoubleFromInteger implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedInteger();

	BoxedIntegerObj ix =
	    (BoxedIntegerObj)((Var)((AtomExpr)x).a).obj;

	return new AtomExpr(new LitDouble(ix.value.doubleValue()));
    }
}

class DoubleShow implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);

	if (x instanceof AtomExpr && 
	    ((AtomExpr)x).a instanceof Var &&
	    ((Var)((AtomExpr)x).a).obj instanceof BoxedDoubleObj){
	    return show$Double((BoxedDoubleObj) ((Var)((AtomExpr)x).a).obj);
	} else {
	    return new ErrExpr("DoubleShow: must not occur");
	}
    }
    
    private static Expr show$Double(BoxedDoubleObj x){
	return RTLib.fromJString(String.valueOf(x.value).replace("E","e"));
    }
}

class FloatLe implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();

	Expr lhs = RT.eval(args[0]);
	Expr rhs = RT.eval(args[1]);

	assert lhs.isBoxedFloat();
	assert rhs.isBoxedFloat();

	BoxedFloatObj il =
	    (BoxedFloatObj)((Var)((AtomExpr)lhs).a).obj;
	BoxedFloatObj ir =
	    (BoxedFloatObj)((Var)((AtomExpr)rhs).a).obj;

	if (il.value <= ir.value){
	    return Prim.mkTrue();
	} else {
	    return Prim.mkFalse();
	}
    }
}

class FloatEq implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();

	Expr lhs = RT.eval(args[0]);
	Expr rhs = RT.eval(args[1]);

	assert lhs.isBoxedFloat();
	assert rhs.isBoxedFloat();

	BoxedFloatObj il =
	    (BoxedFloatObj)((Var)((AtomExpr)lhs).a).obj;
	BoxedFloatObj ir =
	    (BoxedFloatObj)((Var)((AtomExpr)rhs).a).obj;

	if (il.value == ir.value){
	    return Prim.mkTrue();
	} else {
	    return Prim.mkFalse();
	}
    }
}

class FloatAdd implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();

	Expr lhs = RT.eval(args[0]);
	Expr rhs = RT.eval(args[1]);

	assert lhs.isBoxedFloat();
	assert rhs.isBoxedFloat();

	BoxedFloatObj il =
	    (BoxedFloatObj)((Var)((AtomExpr)lhs).a).obj;
	BoxedFloatObj ir =
	    (BoxedFloatObj)((Var)((AtomExpr)rhs).a).obj;

	LitFloat r = new LitFloat(il.value + ir.value);

	return new AtomExpr(r);
    }
}

class FloatSub implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();

	Expr lhs = RT.eval(args[0]);
	Expr rhs = RT.eval(args[1]);

	assert lhs.isBoxedFloat();
	assert rhs.isBoxedFloat();

	BoxedFloatObj il =
	    (BoxedFloatObj)((Var)((AtomExpr)lhs).a).obj;
	BoxedFloatObj ir =
	    (BoxedFloatObj)((Var)((AtomExpr)rhs).a).obj;

	LitFloat r = new LitFloat(il.value - ir.value);

	return new AtomExpr(r);
    }
}

class FloatMul implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();

	Expr lhs = RT.eval(args[0]);
	Expr rhs = RT.eval(args[1]);

	assert lhs.isBoxedFloat();
	assert rhs.isBoxedFloat();

	BoxedFloatObj il =
	    (BoxedFloatObj)((Var)((AtomExpr)lhs).a).obj;
	BoxedFloatObj ir =
	    (BoxedFloatObj)((Var)((AtomExpr)rhs).a).obj;

	LitFloat r = new LitFloat(il.value * ir.value);

	return new AtomExpr(r);
    }
}

class FloatDiv implements LambdaForm {
    public int arity(){ return 2; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();

	Expr lhs = RT.eval(args[0]);
	Expr rhs = RT.eval(args[1]);

	assert lhs.isBoxedFloat();
	assert rhs.isBoxedFloat();

	BoxedFloatObj il =
	    (BoxedFloatObj)((Var)((AtomExpr)lhs).a).obj;
	BoxedFloatObj ir =
	    (BoxedFloatObj)((Var)((AtomExpr)rhs).a).obj;

	LitFloat r = new LitFloat(il.value / ir.value);

	return new AtomExpr(r);
    }
}

class FloatSignum implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedFloat();

	BoxedFloatObj ix =
	    (BoxedFloatObj)((Var)((AtomExpr)x).a).obj;

	if (ix.value > 0){
	    return new AtomExpr(new LitFloat(1.0f));
	} else if (ix.value == 0) {
	    return new AtomExpr(new LitFloat(0.0f));
	} else {
	    return new AtomExpr(new LitFloat(-1.0f));
	}
    }
}

class FloatFromInteger implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedInteger();

	BoxedIntegerObj ix =
	    (BoxedIntegerObj)((Var)((AtomExpr)x).a).obj;

	return new AtomExpr(new LitFloat(ix.value.floatValue()));
    }
}

class FloatShow implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);

	if (x instanceof AtomExpr && 
	    ((AtomExpr)x).a instanceof Var &&
	    ((Var)((AtomExpr)x).a).obj instanceof BoxedFloatObj){
	    return show$Float((BoxedFloatObj) ((Var)((AtomExpr)x).a).obj);
	} else {
	    return new ErrExpr("FloatShow: must not occur");
	}
    }
    
    private static Expr show$Float(BoxedFloatObj x){
	return RTLib.fromJString(String.valueOf(x.value).replace("E","e"));
    }
}
