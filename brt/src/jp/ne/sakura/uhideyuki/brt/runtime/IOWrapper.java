package jp.ne.sakura.uhideyuki.brt.runtime;

import java.io.*;

public class IOWrapper {
    public static char getchar(){
	int c = 0;
	try {
	    c = System.in.read();
	}
	catch (IOException e){
	    // todo: handling the exception
	}
	return (char) c;
    }

    public static void println(String s){
	System.out.println(s);
    }

    public static void errorPrintln(String s){
	System.err.println(s);
    }

}
