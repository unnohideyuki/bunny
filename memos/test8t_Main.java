import jp.ne.sakura.uhideyuki.brt.brtsyn.*;
import jp.ne.sakura.uhideyuki.brt.runtime.*;

public class Main {
    public static class LAM0 implements LambdaForm {
     public int arity(){ return 1; }
     public Expr call(AtomExpr[] args){
      Expr t0 = args[0];
      Expr t1 = Prim.mkFalse();
      Alt t2 = new CotrAlt("Prim.True", t1);
      Expr t3 = Prim.mkTrue();
      Alt t4 = new CotrAlt("Prim.False", t3);
      Alt[] t5 = {t2,t4};
      Expr t6 = new CaseExpr(t0, t5);
      return t6;
     }
    }

    public static Expr mknot(){
      Expr t0 = RTLib.mkFun(new LAM0());
      return t0;
    }


    public static class OL_43_ implements LambdaForm {
     public int arity(){ return 1; }
     public Expr call(AtomExpr[] args){
      Dict_36_Main_46_Num d = (Dict_36_Main_46_Num) RTLib.extrDict(args[0]);
      Expr t0 = d.mk_43_();
      return t0;
     }
    }

    public static Expr mk_43_(){
      Expr t0 = RTLib.mkFun(new OL_43_());
      return t0;
    }


    public static class OL_62_ implements LambdaForm {
     public int arity(){ return 1; }
     public Expr call(AtomExpr[] args){
      Dict_36_Main_46_Ord d = (Dict_36_Main_46_Ord) RTLib.extrDict(args[0]);
      Expr t0 = d.mk_62_();
      return t0;
     }
    }

    public static Expr mk_62_(){
      Expr t0 = RTLib.mkFun(new OL_62_());
      return t0;
    }


    public static class OL_62__61_ implements LambdaForm {
     public int arity(){ return 1; }
     public Expr call(AtomExpr[] args){
      Dict_36_Main_46_Ord d = (Dict_36_Main_46_Ord) RTLib.extrDict(args[0]);
      Expr t0 = d.mk_62__61_();
      return t0;
     }
    }

    public static Expr mk_62__61_(){
      Expr t0 = RTLib.mkFun(new OL_62__61_());
      return t0;
    }


    public static class OL_60__61_ implements LambdaForm {
     public int arity(){ return 1; }
     public Expr call(AtomExpr[] args){
      Dict_36_Main_46_Ord d = (Dict_36_Main_46_Ord) RTLib.extrDict(args[0]);
      Expr t0 = d.mk_60__61_();
      return t0;
     }
    }

    public static Expr mk_60__61_(){
      Expr t0 = RTLib.mkFun(new OL_60__61_());
      return t0;
    }


    public static class OL_60_ implements LambdaForm {
     public int arity(){ return 1; }
     public Expr call(AtomExpr[] args){
      Dict_36_Main_46_Ord d = (Dict_36_Main_46_Ord) RTLib.extrDict(args[0]);
      Expr t0 = d.mk_60_();
      return t0;
     }
    }

    public static Expr mk_60_(){
      Expr t0 = RTLib.mkFun(new OL_60_());
      return t0;
    }


    public static class OL_47__61_ implements LambdaForm {
     public int arity(){ return 1; }
     public Expr call(AtomExpr[] args){
      Dict_36_Main_46_Eq d = (Dict_36_Main_46_Eq) RTLib.extrDict(args[0]);
      Expr t0 = d.mk_47__61_();
      return t0;
     }
    }

    public static Expr mk_47__61_(){
      Expr t0 = RTLib.mkFun(new OL_47__61_());
      return t0;
    }


    public static class OL_61__61_ implements LambdaForm {
     public int arity(){ return 1; }
     public Expr call(AtomExpr[] args){
      Dict_36_Main_46_Eq d = (Dict_36_Main_46_Eq) RTLib.extrDict(args[0]);
      Expr t0 = d.mk_61__61_();
      return t0;
     }
    }

    public static Expr mk_61__61_(){
      Expr t0 = RTLib.mkFun(new OL_61__61_());
      return t0;
    }


    public static Expr mkmain(){
      Expr t0 = Prim.mkputStrLn();
      Expr t1 = Main.mkf();
      Expr t2 = (Expr) new AtomExpr(new Dict(new Dict_36_Main_46_Integer_64_Main_46_Ord()));
      Expr t3 = RTLib.app(t1, t2);
      Expr t4 = RTLib.fromInteger(5);
      Expr t5 = RTLib.mkApp(t3, t4);
      Expr t6 = RTLib.fromInteger(10);
      Expr t7 = RTLib.mkApp(t5, t6);
      Expr t8 = RTLib.mkApp(t0, t7);
      return t8;
    }


    public static class LAM11 implements LambdaForm {
     public int arity(){ return 1; }
     public Expr call(AtomExpr[] args){
      Expr t0 = args[0];
      Expr t1 = RTLib.fromJString("True");
      Alt t2 = new CotrAlt("Prim.True", t1);
      Expr t3 = RTLib.fromJString("False");
      Alt t4 = new CotrAlt("Prim.False", t3);
      Alt[] t5 = {t2,t4};
      Expr t6 = new CaseExpr(t0, t5);
      return t6;
     }
    }

    public static class LAM12 implements LambdaForm {
     public int arity(){ return 3; }
     public Expr call(AtomExpr[] args){
      Expr t0 = args[1];
      Expr t1 = RTLib.mkApp(Main.mk_62_(), args[0]); /* Main.f dict */
      Expr t2 = args[2];
      Expr t3 = RTLib.mkApp(t1, t2);
      Expr t4 = args[3];
      Expr t5 = RTLib.mkApp(t3, t4);
      Expr t6 = RTLib.mkApp(t0, t5);
      return t6;
     }
    }

    /* Body of the Main.f */
    public static class LAM10 implements LambdaForm {
     public int arity(){ return 3; }
     public Expr call(AtomExpr[] args){
      Expr t0 = RTLib.mkFun(new LAM11());
      Expr t1 = new LetExpr(null, new LAM12());
      /* Expr[] t2 = {t0,args[0],args[1]}; */
      Expr[] t2 = {args[0], t0,args[1],args[2]};
      ((LetExpr)t1).setEs(t2);
      return t1;
     }
    }

    public static Expr mkf(){
      Expr t0 = RTLib.mkFun(new LAM10());
      return t0;
    }


    public static class LAM14 implements LambdaForm {
     public int arity(){ return 2; }
     public Expr call(AtomExpr[] args){
      Expr t0 = Main.mknot();
      Expr t1 = Main.mkI_37_Char_46__61__61_();
      Expr t2 = args[0];
      Expr t3 = RTLib.mkApp(t1, t2);
      Expr t4 = args[1];
      Expr t5 = RTLib.mkApp(t3, t4);
      Expr t6 = RTLib.mkApp(t0, t5);
      return t6;
     }
    }

    public static Expr mkI_37_Char_46__47__61_(){
      Expr t0 = RTLib.mkFun(new LAM14());
      return t0;
    }


    public static Expr mkI_37_Char_46__60_(){
      Expr t0 = Prim.mkcharLt();
      return t0;
    }


    public static Expr mkI_37_Char_46__60__61_(){
      Expr t0 = Prim.mkcharLe();
      return t0;
    }


    public static Expr mkI_37_Char_46__62__61_(){
      Expr t0 = Prim.mkcharGe();
      return t0;
    }


    public static Expr mkI_37_Char_46__62_(){
      Expr t0 = Prim.mkcharGt();
      return t0;
    }


    public static Expr mkI_37_Char_46__61__61_(){
      Expr t0 = Prim.mkcharEq();
      return t0;
    }


    public static Expr mkI_37_Integer_46__60_(){
      Expr t0 = Prim.mkintegerLt();
      return t0;
    }


    public static Expr mkI_37_Integer_46__60__61_(){
      Expr t0 = Prim.mkintegerLe();
      return t0;
    }


    public static Expr mkI_37_Integer_46__62__61_(){
      Expr t0 = Prim.mkintegerGe();
      return t0;
    }


    public static Expr mkI_37_Integer_46__62_(){
      Expr t0 = Prim.mkintegerGt();
      return t0;
    }


    public static Expr mkI_37_Integer_46__43_(){
      Expr t0 = Prim.mkintegerEq();
      return t0;
    }


}
