import jp.ne.sakura.uhideyuki.brt.brtsyn.*;
import jp.ne.sakura.uhideyuki.brt.runtime.*;

/** Sample1 -- A sample of STG evaludation, corresponding to:

  main = putStrLn "Hello, BRT!"

 */
public class Sample1 {
    public static void main(String[] args){
	Expr s = RTLib.fromJString("Hello, BRT!");
	Expr e = RTLib.app(RTLib.putStrLn, s);
	RT.eval(e);
    }
}
