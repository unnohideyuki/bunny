package jp.ne.sakura.uhideyuki.brt.brtsyn;

public abstract class Expr {
    public Boolean isVar(){
	Boolean r = (this instanceof AtomExpr) && 
	    (((AtomExpr)this).a instanceof Var);
	return r;
    }

    public Boolean isLiteral(){
	Boolean r = ((this instanceof AtomExpr) &&
		     (((AtomExpr)this).a instanceof Literal));
	return r;
    }

    public Boolean isBoxedInt(){
	Boolean r = (this.isVar() &&
		     ((Var)((AtomExpr)this).a).obj instanceof BoxedIntObj);
	return r;
    }

    public Boolean isBoxedInteger(){
	Boolean r = (this.isVar() &&
		     ((Var)((AtomExpr)this).a).obj instanceof BoxedIntegerObj);
	return r;
    }

    public Boolean isBoxedChar(){
	Boolean r = (this.isVar() &&
		     ((Var)((AtomExpr)this).a).obj instanceof BoxedCharObj);
	return r;
    }

    public Boolean isBoxedValue(){
	return isBoxedInt() || isBoxedChar();
    }

    public Boolean isValue(){
	if (this.isVar()){
	    HeapObj obj = ((Var)((AtomExpr)this).a).obj;
	    Boolean r = (!(obj instanceof Thunk));
	    return r;
	}
	return isBoxedValue();
    }

    /*
    public Boolean isLitOrValue(){
	return (this.isLiteral() || this.isValue());
    }
    */

    public HeapObj getObj(){
	assert this.isVar();
	return ((Var)((AtomExpr)this).a).obj;
    }

    public Boolean isThunk(){
	return isVar() && getObj() instanceof Thunk;
    }

    public Boolean isConObj(){
	return isVar() && getObj() instanceof ConObj;
    }

    public Boolean isFunObj(){
	return isVar() && getObj() instanceof FunObj;
    }

    public Boolean isPapObj(){
	return isVar() && getObj() instanceof PapObj;
    }

    public Boolean isKnownCall(){
	Boolean r = ((this instanceof FunAppExpr) &&
		     (((FunAppExpr)this).arity > 0));
	return r;
    }

    public abstract String inspect();
}

