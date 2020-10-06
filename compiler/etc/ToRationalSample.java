import java.io.*;
import java.util.regex.*;
import java.math.BigInteger;

public class ToRationalSample {
    public static void main(String[] args) throws IOException {
	try (BufferedReader br = new BufferedReader(new InputStreamReader(System.in))) {
	    String s = br.readLine();
	    double x = Double.parseDouble(s);

	    /* hs will be one of the following:
	       - "Infinity", "-Infinity" or "NaN"
	       - /-?\d\.\[0-9a-f]+p-?d+/
	    */
	    String hs = Double.toHexString(x);

	    if (hs == "Infinity") {
		BigInteger d = BigInteger.valueOf(16).pow(256);
		System.out.println(d.toString() + " % 1");
	    } else if (hs == "-Infinity"){
		BigInteger d = BigInteger.valueOf(16).pow(256).negate();
		System.out.println(d.toString() + " % 1");
	    } else if (hs == "NaN"){
		BigInteger d = BigInteger.valueOf(16).pow(255).multiply(BigInteger.valueOf(-24));
		System.out.println(d.toString() + " % 1");
	    } else {
		Pattern pat = Pattern.compile("(-?)0x(\\d+)\\.([0-9a-f]+)p(-?\\d+)");
		Matcher m = pat.matcher(hs);
		if (m.find()){
		    System.out.println(hs);
		    System.out.println(m.group(1));
		    System.out.println(m.group(2));
		    System.out.println(m.group(3));
		    System.out.println(m.group(4));

		    boolean isNeg = m.group(1) == "-";
		    BigInteger d0 = new BigInteger(m.group(2) + m.group(3), 16);

		    int len = m.group(3).length();
		    int e0 = Integer.parseInt(m.group(4));
		    int e = e0 - len*4;

		    BigInteger n0;
		    if (e < 0){
			n0 = BigInteger.valueOf(1L << (-e));
		    } else {
			n0 = BigInteger.ONE;
			d0 = d0.multiply(BigInteger.valueOf(1L << e));
		    }

		    BigInteger g = d0.gcd(n0);
		    BigInteger d = d0.divide(g);
		    BigInteger n = n0.divide(g);

		    if (isNeg){ d = d.negate(); }
		    System.out.println(d.toString() + " % " + n.toString());
		} else {
		    System.out.println("not found");
		}
	    }

	}
    }
}
