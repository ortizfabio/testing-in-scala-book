package examples.scalacheck;

import java.util.StringTokenizer;

public class StringUtils {
  public static String truncate(String s, int n) {
    if(s.length() <= n) return s;
    else return s.substring(0, n) + "...";
  }

  public static String[] tokenize(
    String s, char delim
  ) {
    String delimStr = new Character(delim).toString();
    StringTokenizer st = new StringTokenizer(
      s, delimStr);
    String[] tokens = new String[st.countTokens()];
    int i = 0;
    while(st.hasMoreTokens()) {
      tokens[i] = st.nextToken();
      i++;
    }
    return tokens;
  }

  public static boolean contains(
    String s, String subString
  ) {
    return s.indexOf(subString) != -1;
  }
}