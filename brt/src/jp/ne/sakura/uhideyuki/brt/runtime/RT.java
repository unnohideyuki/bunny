package jp.ne.sakura.uhideyuki.brt.runtime;

import jp.ne.sakura.uhideyuki.brt.brtsyn.*;

import java.util.Stack;
import java.util.Arrays;

class EvalApply {
    private Stack<Cont> s;
    private Expr code;

    public EvalApply(){
	s = new Stack<Cont>();
    }

    public Expr eval(Expr e){
	code = e;
	while (runStep()){}
	return code;
    }

    static final NullCont nullcont = new NullCont();

    private Cont peek(){
	if (s.empty()){
	    return nullcont;
	} else {
	    return s.peek();
	}
    }

    public Boolean runStep(){
	/**** trace ****/
	System.err.println("runStep:" + code.inspect());


	if (code instanceof ErrExpr){
	    ErrExpr e = (ErrExpr) code;
	    System.out.println(e.message);
	    System.exit(-1);
	} if (code instanceof LetExpr){
	    evalLet();
	} else if (code instanceof CaseExpr){
	    evalCase(); // CASECON, CASEANY or CASE
	} else if (code.isLitOrValue() && (peek() instanceof CaseCont)){
	    evalRet();
	} else if (code.isThunk()){
	    evalThunk();
	} else if (code.isLitOrValue() && (peek() instanceof UpdCont)){
	    evalUpdate();
	} else if (code.isKnownCall()){
	    evalKnownCall();
	} else if (code instanceof PrimOpExpr){
	    evalPrimOp();
	} else if (code instanceof FunAppExpr){
	    Expr f = ((FunAppExpr)code).f;
	    assert f != null;
	    if (f.isFunObj()){
		evalFun(); // EXACT, CALLK or PAP2
	    } else if (f.isThunk()){
		evalTCall();
	    } else {
		assert f.isPapObj();
		evalPCall();
	    }
	} else if (peek() instanceof CallCont){
	    evalRetFun();
	} else {
	    if (!((code.isValue() || code.isLiteral()) && s.empty())){
		System.out.println(code.inspect());
	    }
	    assert (code.isValue() || code.isLiteral()) && s.empty();
	    return false;
	}

	return true;
    }

    private Expr mkVarExpr(HeapObj obj){
	Var v = new Var(obj);
	AtomExpr a = new AtomExpr(v);
	return a;
    }

    private void evalLet(){
	LetExpr e = (LetExpr) code;
	AtomExpr[] args = new AtomExpr[e.es.length];

	for (int i = 0; i < e.es.length; i++){
	    args[i] = (AtomExpr) mkVarExpr(new Thunk(e.es[i]));
	}

	code = e.lambda.call(args);
    }

    private void evalCase(){
	if (CaseCon() || CaseAny()){
	    return;
	}
	CaseExpr e = (CaseExpr) code;
	CaseCont c = new CaseCont(e.alts);
	s.push(c);
	code = e.scrut;
    }

    private Boolean CaseCon(){
	CaseExpr e = (CaseExpr) code;
	Expr scrut = e.scrut;
	Alt[] alts = e.alts;

	if (scrut.isConObj()){
	    ConObj cobj = (ConObj) ((Var)((AtomExpr) scrut).a).obj;
	    Cotr cotr = cobj.cotr;
	    CotrAlt calt = null;

	    for (int i = 0; i < alts.length; i++){
		Alt alt = alts[i];
		if (alt instanceof CotrAlt){
		    CotrAlt t = (CotrAlt) alt;
		    if (cotr.equals(t.cotr)){
			calt = t;
			break;
		    }
		}
	    }

	    if (calt != null){
		AtomExpr[] args = cobj.args;
		if (args.length > 0){
		    code = new FunAppExpr(calt.e, args, -1);
		} else {
		    code = calt.e;
		}
		return true;
	    }
	}

	return false;
    }

    private DefaultAlt getDefaultAlt(Alt[] alts){
	for (int i = 0; i < alts.length; i++){
	    Alt alt = alts[i];
	    if (alt instanceof DefaultAlt){
		return (DefaultAlt) alt;
	    }
	}
	return null;
    }

    private Boolean CaseAny(){
	CaseExpr e = (CaseExpr) code;
	Expr scrut = e.scrut;
	Alt[] alts = e.alts;

	if (scrut.isLitOrValue()){
	    DefaultAlt dalt = getDefaultAlt(alts);
	    assert dalt != null;
	    AtomExpr[] a = new AtomExpr[1];
	    a[0] = (AtomExpr) scrut; // is is safe when scrut.isLitOrValue()
	    code = new FunAppExpr(dalt.e, a, -1);
	    return true;
	}
	return false;
    }

    private void evalRet(){
	CaseCont cont = (CaseCont) s.pop();
	CaseExpr e = new CaseExpr(code, cont.alts);
	code = e;
    }

    private void evalThunk(){
	UpdCont upd = new UpdCont(code);
	Thunk t = (Thunk) ((Var)((AtomExpr) code).a).obj;
	code = t.e;
	s.push(upd);
    }

    private void evalUpdate(){
	UpdCont upd = (UpdCont) s.pop();
	Expr x = upd.x;
	assert x.isThunk();
	Var v = (Var)((AtomExpr)x).a ;
	v.obj = ((Var)((AtomExpr)code).a).obj;
	assert x.isValue();
    }

    private void evalKnownCall(){
	FunAppExpr e = (FunAppExpr) code;
	assert e.arity == e.args.length;

	Expr f = e.f;
	assert f.isValue();
	HeapObj obj = ((Var)((AtomExpr)f).a).obj;

	assert obj instanceof FunObj;
	FunObj fobj = (FunObj) obj;
	assert fobj.arity == e.arity;

	code = fobj.lambda.call(e.args);
    }

    private void evalPrimOp(){
	PrimOpExpr e = (PrimOpExpr) code;
	assert e.lambda.arity() == e.args.length;
	code = e.lambda.call(e.args);
    }

    private void evalFun(){
	FunAppExpr e = (FunAppExpr) code;
	FunObj fobj = (FunObj) e.f.getObj();

	if (e.args.length == fobj.arity){ // EXACT
	    code = fobj.lambda.call(e.args);
	} else if (e.args.length > fobj.arity){ // CALLK
	    AtomExpr[] args1 = Arrays.copyOfRange(e.args, 0, fobj.arity);
	    code = fobj.lambda.call(args1);

	    AtomExpr[] args2 =
		Arrays.copyOfRange(e.args, fobj.arity, e.args.length);
	    s.push(new CallCont(args2));
	} else { // PAP2
	    PapObj pap = new PapObj(e.f, e.args);
	    code = new AtomExpr(new Var(pap));
	}
    }

    private void evalTCall(){
	FunAppExpr e = (FunAppExpr) code;
	code = e.f;
	s.push(new CallCont(e.args));
    }

    private AtomExpr[] catArgs(AtomExpr[] a1, AtomExpr[] a2){
	AtomExpr[] r = new AtomExpr[a1.length + a2.length];
        System.arraycopy(a1, 0, r, 0, a1.length);
        System.arraycopy(a2, 0, r, a1.length, a2.length);
	return r;
    }

    private void evalPCall(){
	FunAppExpr e = (FunAppExpr) code;
	PapObj pap = (PapObj) e.f.getObj();
	Expr g = pap.f;
	AtomExpr[] args = catArgs(pap.args, e.args);
	code = new FunAppExpr(g, args, -1);
    }

    private void evalRetFun(){
	Expr f = code;
	if (!(f.isFunObj() || f.isPapObj())){
	    System.out.println(f.inspect());
	}
	assert f.isFunObj() || f.isPapObj();
	CallCont c = (CallCont) s.pop();
	code = new FunAppExpr(f, c.args, -1);
    }
}

public class RT {
    private static EvalApply rt = new EvalApply();
    public static Expr eval(Expr e){
	return rt.eval(e);
    }
}
