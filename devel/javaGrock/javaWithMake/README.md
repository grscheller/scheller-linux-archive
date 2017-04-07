Wrote versions of these a while back when I was getting
frustrated using Netbeans as my java IDE.  Wanted
to see if GNU Make was a viable build tool for java.
What here is just a rudimentary start.  I might come
back some day to continue when I better understand the
java ecosystem.  I wish to get this to the point of
providing a Make based build skeleton that could be
compatible with an an IDE.  In the age of SBT, probably
won't happen.

To build:
  $ make
  javac  Carl.java
  javac  Church.java
  javac  IsPrime.java

To run:
  $ java Carl
  Sum of 1 to 100 is 5050

  $ java Carl 36
  Sum of 1 to 36 is 666

  $ java Church foo bar
  foobar

  $ java IsPrime 2 721692259 721692261
  2 is prime.
  721692259 is prime.
  721692261 is not prime.
