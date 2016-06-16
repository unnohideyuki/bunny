import jp.ne.sakura.uhideyuki.brt.brtsyn.*;
import jp.ne.sakura.uhideyuki.brt.runtime.*;

public class Sample {
    public static void main(String[] args){
      RT.eval(Main.main);
    }
}

class Main {
    public static Expr mkmain(){
      Expr e1 = RTLib.putStrLn;
      Expr e2 = RTLib.fromJString("Hello, world!");
      return RTLib.app(e1, e2);
    }

    public static Expr main = mkmain();
}
