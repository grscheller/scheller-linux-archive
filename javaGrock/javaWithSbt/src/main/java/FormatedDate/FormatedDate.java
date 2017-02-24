import java.util.Date;
import java.util.Locale;
import java.text.DateFormat;

/**
 *  The standard java hello world program.
 *
 *    Start off with something we know will compile.
 *
 */
class FormatedDate {

  public static void main(String[] args) {
    Date now = new Date();
    DateFormat df =
        DateFormat.getDateInstance(DateFormat.LONG, Locale.FRANCE);
    System.out.println(df.format(now));
  }

}
