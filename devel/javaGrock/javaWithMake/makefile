JAVAC = javac
# JFLAGS = -g
JFLAGS = 

.SUFFIXES: .java .class
.java.class:
	$(JAVAC) $(JFLAGS) $*.java

SOURCES = Carl.java

default: classes

classes: $(SOURCES:.java=.class)

clean:
	rm -f *.class

