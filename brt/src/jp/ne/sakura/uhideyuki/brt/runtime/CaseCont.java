package jp.ne.sakura.uhideyuki.brt.runtime;

import jp.ne.sakura.uhideyuki.brt.brtsyn.*;

public class CaseCont extends Cont {
    public Alt[] alts;
    public CaseCont(Alt[] as){ alts = as; }
}
