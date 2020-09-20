JAVAC = javac
JFLAGS = -d classes
JAR = jar

classes/%.class: %.java
	@mkdir -p classes
	$(JAVAC) $(JFLAGS) $< 

%.jar: classes/%.class
	$(JAR) cfe $@ $* -C classes $*.class

SOURCES = Carl.java IsPrime.java

default: classfiles

classfiles: $(addprefix classes/,$(SOURCES:.java=.class))
	
jarfiles: $(SOURCES:.java=.jar)

jar: classfiles jarfiles
	$(JAR) cf javaWithMake.jar -C classes .

clean:
	rm -fr classes

clobber: clean
	rm -f $(SOURCES:.java=.jar) javaWithMake.jar

.PHONY: clean clobber classfiles jarfiles
