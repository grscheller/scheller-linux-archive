/**
 *  A class to test if multiple threads of the underlying
 *  JVM can update adjacent elements of a byte array.
 *
 *  Ideally, separately executing threads, not interacting
 *  with each other's values, should not need synchronization
 *  to ensure sequential consistency.  Unfornuately, if the
 *  underlying processor can only interact on a word basis,
 *  synchronization will be needed.
 *
 *  Most of this code is lifted verbatum from the Java
 *  language Specification, Java SE 8 Edition.
 */
public class WordTearing extends Thread {
  static final int LENGTH = 8;
  static final int ITERS = 1000000;
  static byte[] mutableByteArray = new byte[LENGTH];
  static Thread[] threads = new Thread[LENGTH];

  final int id;

  /**
   *  Constructor
   */
  WordTearing(int theByte) {
    id = theByte;
  }

  public void run() {
    byte v = 0;
    for (int i = 0; i < ITERS; i++) {
      byte v2 = mutableByteArray[id];
      if (v != v2) {
        System.err.println("Word-Tearing found: " +
          "mutableByteArray[" + id + "] = "+ v2 +
          ", should be " + v);
        return;
      }
      v++;
      mutableByteArray[id] = v;
    }
  }

  /**
   * Eight Threads working indepently on each byte of a word.
   *
   * The threads should not get in each others way unless
   * the underlying java implementation can only work on 
   * whole words at a time.
   */
  public static void main(String[] args) {
    for (int i = 0; i < LENGTH; ++i)
      (threads[i] = new WordTearing(i)).start();
  }
}
