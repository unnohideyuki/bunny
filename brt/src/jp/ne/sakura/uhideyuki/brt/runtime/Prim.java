package jp.ne.sakura.uhideyuki.brt.runtime;

import jp.ne.sakura.uhideyuki.brt.brtsyn.*;
import java.io.*;
import java.util.regex.*;
import java.math.BigInteger;
import java.lang.Math;

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

    public static Expr mkdoubleToRational(){
	return RTLib.mkFun(new DoubleToRational());
    }

    public static Expr mkRational(double x){
	String hs = Double.toHexString(x);
	BigInteger d = BigInteger.ZERO;
	BigInteger n = BigInteger.ONE;
	
	if (hs == "Infinity") {
	    d = BigInteger.valueOf(16).pow(256);
	} else if (hs == "-Infinity"){
	    d = BigInteger.valueOf(16).pow(256).negate();
	} else if (hs == "NaN"){
	    d = BigInteger.valueOf(16).pow(255).multiply(BigInteger.valueOf(-24));
	} else {
	    Pattern pat = Pattern.compile("-?0x(\\d+)\\.([0-9a-f]+)p(-?\\d+)");
	    Matcher m = pat.matcher(hs);
	    if (m.find()){
		boolean isNeg = (hs.charAt(0) == '\u002D');
		BigInteger d0 = new BigInteger(m.group(1) + m.group(2), 16);

		int len = m.group(2).length();
		int e0 = Integer.parseInt(m.group(3));
		int e = e0 - len*4;

		BigInteger n0;
		if (e < 0){
		    n0 = BigInteger.valueOf(1L << (-e));
		} else {
		    n0 = BigInteger.ONE;
		    d0 = d0.multiply(BigInteger.valueOf(1L << e));
		}

		BigInteger g = d0.gcd(n0);
		d = d0.divide(g);
		n = n0.divide(g);
		if (isNeg){ d = d.negate(); }
	    } else {
		System.err.println("Prim.mkRational: must not occur");
		return Prim.mkerror();
	    }
	}

	AtomExpr ad = new AtomExpr(new LitInteger(d));
	AtomExpr an = new AtomExpr(new LitInteger(n));
	AtomExpr[] args = {ad, an};
	return mkExpr(new ConObj(new Cotr("Prelude.:%"), args));
    }

    public static Expr mkdoubleFromRational(){
	return RTLib.mkFun(new DoubleFromRational());
    }

    public static double fromRational(BigInteger d0, BigInteger n0){
	if (d0 == BigInteger.valueOf(16).pow(256)){
	    return (1/0);
	} else if (d0 == BigInteger.valueOf(16).pow(256).negate()){
	    return (-1/0);
	} else if (d0 == BigInteger.valueOf(16).pow(255).multiply(BigInteger.valueOf(-24))){
	    return (0/0);
	} else {
	    double d = d0.doubleValue();
	    double n = n0.doubleValue();
	    return (d/n);
	}
    }

    public static Expr mkdoubleExp(){
	return RTLib.mkFun(new DoubleExp());
    }
    
    public static Expr mkdoubleLog(){
	return RTLib.mkFun(new DoubleLog());
    }
    
    public static Expr mkdoubleSin(){
	return RTLib.mkFun(new DoubleSin());
    }
    
    public static Expr mkdoubleCos(){
	return RTLib.mkFun(new DoubleCos());
    }
    
    public static Expr mkdoubleSinh(){
	return RTLib.mkFun(new DoubleSinh());
    }
    
    public static Expr mkdoubleCosh(){
	return RTLib.mkFun(new DoubleCosh());
    }
    
    public static Expr mkdoubleAsin(){
	return RTLib.mkFun(new DoubleAsin());
    }
    
    public static Expr mkdoubleAcos(){
	return RTLib.mkFun(new DoubleAcos());
    }
    
    public static Expr mkdoubleAtan(){
	return RTLib.mkFun(new DoubleAtan());
    }
    
    public static Expr mkdoubleAsinh(){
	return RTLib.mkFun(new DoubleAsinh());
    }
    
    public static Expr mkdoubleAcosh(){
	return RTLib.mkFun(new DoubleAcosh());
    }
    
    public static Expr mkdoubleAtanh(){
	return RTLib.mkFun(new DoubleAtanh());
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

    public static Expr mkfloatToRational(){
	return RTLib.mkFun(new FloatToRational());
    }

    public static Expr mkfloatFromRational(){
	return RTLib.mkFun(new FloatFromRational());
    }

    public static Expr mkfloatExp(){
	return RTLib.mkFun(new FloatExp());
    }
    
    public static Expr mkfloatLog(){
	return RTLib.mkFun(new FloatLog());
    }
    
    public static Expr mkfloatSin(){
	return RTLib.mkFun(new FloatSin());
    }
    
    public static Expr mkfloatCos(){
	return RTLib.mkFun(new FloatCos());
    }
    
    public static Expr mkfloatSinh(){
	return RTLib.mkFun(new FloatSinh());
    }
    
    public static Expr mkfloatCosh(){
	return RTLib.mkFun(new FloatCosh());
    }
    
    public static Expr mkfloatAsin(){
	return RTLib.mkFun(new FloatAsin());
    }
    
    public static Expr mkfloatAcos(){
	return RTLib.mkFun(new FloatAcos());
    }
    
    public static Expr mkfloatAtan(){
	return RTLib.mkFun(new FloatAtan());
    }
    
    public static Expr mkfloatAsinh(){
	return RTLib.mkFun(new FloatAsinh());
    }
    
    public static Expr mkfloatAcosh(){
	return RTLib.mkFun(new FloatAcosh());
    }
    
    public static Expr mkfloatAtanh(){
	return RTLib.mkFun(new FloatAtanh());
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

class DoubleToRational implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedDouble();
	BoxedDoubleObj ix = (BoxedDoubleObj)((Var)((AtomExpr)x).a).obj;
	return Prim.mkRational(ix.value);
    }
}

class DoubleFromRational implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	ConObj obj = (ConObj)((Var)((AtomExpr)x).a).obj;
	Cotr cotr = obj.cotr;
	assert cotr.ident == "Prelude.:%";
	Expr y = RT.eval(obj.args[0]);
	Expr z = RT.eval(obj.args[1]);
	BoxedIntegerObj id = (BoxedIntegerObj)((Var)((AtomExpr)y).a).obj;
	BoxedIntegerObj in = (BoxedIntegerObj)((Var)((AtomExpr)z).a).obj;
	double r = Prim.fromRational(id.value, in.value);
	return new AtomExpr(new LitDouble(r));
    }
}

class DoubleExp implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedDouble();
	BoxedDoubleObj ix = (BoxedDoubleObj)((Var)((AtomExpr)x).a).obj;
	double a = ix.value;
	double r = Math.exp(a);
	return new AtomExpr(new LitDouble(r));
    }
}

class DoubleLog implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedDouble();
	BoxedDoubleObj ix = (BoxedDoubleObj)((Var)((AtomExpr)x).a).obj;
	double a = ix.value;
	double r = Math.log(a);
	return new AtomExpr(new LitDouble(r));
    }
}

class DoubleSin implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedDouble();
	BoxedDoubleObj ix = (BoxedDoubleObj)((Var)((AtomExpr)x).a).obj;
	double a = ix.value;
	double r = Math.sin(a);
	return new AtomExpr(new LitDouble(r));
    }
}

class DoubleCos implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedDouble();
	BoxedDoubleObj ix = (BoxedDoubleObj)((Var)((AtomExpr)x).a).obj;
	double a = ix.value;
	double r = Math.cos(a);
	return new AtomExpr(new LitDouble(r));
    }
}

class DoubleSinh implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedDouble();
	BoxedDoubleObj ix = (BoxedDoubleObj)((Var)((AtomExpr)x).a).obj;
	double a = ix.value;
	double r = Math.sinh(a);
	return new AtomExpr(new LitDouble(r));
    }
}

class DoubleCosh implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedDouble();
	BoxedDoubleObj ix = (BoxedDoubleObj)((Var)((AtomExpr)x).a).obj;
	double a = ix.value;
	double r = Math.cosh(a);
	return new AtomExpr(new LitDouble(r));
    }
}

class DoubleAsin implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedDouble();
	BoxedDoubleObj ix = (BoxedDoubleObj)((Var)((AtomExpr)x).a).obj;
	double a = ix.value;
	double r = Math.asin(a);
	return new AtomExpr(new LitDouble(r));
    }
}

class DoubleAcos implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedDouble();
	BoxedDoubleObj ix = (BoxedDoubleObj)((Var)((AtomExpr)x).a).obj;
	double a = ix.value;
	double r = Math.acos(a);
	return new AtomExpr(new LitDouble(r));
    }
}

class DoubleAtan implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedDouble();
	BoxedDoubleObj ix = (BoxedDoubleObj)((Var)((AtomExpr)x).a).obj;
	double a = ix.value;
	double r = Math.atan(a);
	return new AtomExpr(new LitDouble(r));
    }
}

class DoubleAsinh implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedDouble();
	BoxedDoubleObj ix = (BoxedDoubleObj)((Var)((AtomExpr)x).a).obj;
	double a = ix.value;
	double r = Math.log(a + Math.sqrt(a*a + 1.0));
	return new AtomExpr(new LitDouble(r));
    }
}

class DoubleAcosh implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedDouble();
	BoxedDoubleObj ix = (BoxedDoubleObj)((Var)((AtomExpr)x).a).obj;
	double a = ix.value;
	double r = Math.log(a + Math.sqrt(a*a - 1.0));
	return new AtomExpr(new LitDouble(r));
    }
}

class DoubleAtanh implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedDouble();
	BoxedDoubleObj ix = (BoxedDoubleObj)((Var)((AtomExpr)x).a).obj;
	double a = ix.value;
	double r = Math.log((1.0 + a)/(1.0-a)) / 2.0;
	return new AtomExpr(new LitDouble(r));
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

class FloatToRational implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedFloat();
	BoxedFloatObj ix = (BoxedFloatObj)((Var)((AtomExpr)x).a).obj;
	return Prim.mkRational((double)ix.value);
    }
}

class FloatFromRational implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	ConObj obj = (ConObj)((Var)((AtomExpr)x).a).obj;
	Cotr cotr = obj.cotr;
	assert cotr.ident == "Prelude.:%";
	Expr y = RT.eval(obj.args[0]);
	Expr z = RT.eval(obj.args[1]);
	BoxedIntegerObj id = (BoxedIntegerObj)((Var)((AtomExpr)y).a).obj;
	BoxedIntegerObj in = (BoxedIntegerObj)((Var)((AtomExpr)z).a).obj;
	float r = (float) Prim.fromRational(id.value, in.value);
	return new AtomExpr(new LitFloat(r));
    }
}

class FloatExp implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedFloat();
	BoxedFloatObj ix = (BoxedFloatObj)((Var)((AtomExpr)x).a).obj;
	float a = ix.value;
	float r = (float) Math.exp((double) a);
	return new AtomExpr(new LitFloat(r));
    }
}

class FloatLog implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedFloat();
	BoxedFloatObj ix = (BoxedFloatObj)((Var)((AtomExpr)x).a).obj;
	float a = ix.value;
	float r = (float) Math.log((double) a);
	return new AtomExpr(new LitFloat(r));
    }
}

class FloatSin implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedFloat();
	BoxedFloatObj ix = (BoxedFloatObj)((Var)((AtomExpr)x).a).obj;
	float a = ix.value;
	float r = (float) Math.sin((double) a);
	return new AtomExpr(new LitFloat(r));
    }
}

class FloatCos implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedFloat();
	BoxedFloatObj ix = (BoxedFloatObj)((Var)((AtomExpr)x).a).obj;
	float a = ix.value;
	float r = (float) Math.cos((double) a);
	return new AtomExpr(new LitFloat(r));
    }
}

class FloatSinh implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedFloat();
	BoxedFloatObj ix = (BoxedFloatObj)((Var)((AtomExpr)x).a).obj;
	float a = ix.value;
	float r = (float) Math.sinh((double) a);
	return new AtomExpr(new LitFloat(r));
    }
}

class FloatCosh implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedFloat();
	BoxedFloatObj ix = (BoxedFloatObj)((Var)((AtomExpr)x).a).obj;
	float a = ix.value;
	float r = (float) Math.cosh((double) a);
	return new AtomExpr(new LitFloat(r));
    }
}

class FloatAsin implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedFloat();
	BoxedFloatObj ix = (BoxedFloatObj)((Var)((AtomExpr)x).a).obj;
	float a = ix.value;
	float r = (float) Math.asin((double) a);
	return new AtomExpr(new LitFloat(r));
    }
}

class FloatAcos implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedFloat();
	BoxedFloatObj ix = (BoxedFloatObj)((Var)((AtomExpr)x).a).obj;
	float a = ix.value;
	float r = (float) Math.acos((double) a);
	return new AtomExpr(new LitFloat(r));
    }
}

class FloatAtan implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedFloat();
	BoxedFloatObj ix = (BoxedFloatObj)((Var)((AtomExpr)x).a).obj;
	float a = ix.value;
	float r = (float) Math.atan((double) a);
	return new AtomExpr(new LitFloat(r));
    }
}

class FloatAsinh implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedFloat();
	BoxedFloatObj ix = (BoxedFloatObj)((Var)((AtomExpr)x).a).obj;
	double a = (double) ix.value;
	float r = (float) Math.log(a + Math.sqrt(a*a + 1.0));
	return new AtomExpr(new LitFloat(r));
    }
}

class FloatAcosh implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedFloat();
	BoxedFloatObj ix = (BoxedFloatObj)((Var)((AtomExpr)x).a).obj;
	double a = (double) ix.value;
	float r = (float) Math.log(a + Math.sqrt(a*a - 1.0));
	return new AtomExpr(new LitFloat(r));
    }
}

class FloatAtanh implements LambdaForm {
    public int arity(){ return 1; }
    public Expr call(AtomExpr[] args){
	assert args.length == arity();
	Expr x = RT.eval(args[0]);
	assert x.isBoxedFloat();
	BoxedFloatObj ix = (BoxedFloatObj)((Var)((AtomExpr)x).a).obj;
	double a = (double) ix.value;
	float r = (float) (Math.log((1.0 + a)/(1.0-a)) / 2.0);
	return new AtomExpr(new LitFloat(r));
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
