JAVAC = javac
JFLAGS = -d classes

classes/%.class: %.java
	$(JAVAC) $(JFLAGS) $< 

SOURCES = Carl.java IsPrime.java

default: classfiles

classfiles: $(addprefix classes/,$(SOURCES:.java=.class))

clean:
	rm -fr classes

.PHONY: clean classfiles
