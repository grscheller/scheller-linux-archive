# Python Notes

## Python is Object Oriented in the Smalltalk sense

* Python embraces classic OOP
  * C++ and Java actually are Class Oriented Programming COP
    * classes & functions are compiler directives
      * exist only at compile time
      * templates for objects
      * are not "first class" data types
    * code sharing via via "private" & "protected" inheritance
    * type hierarchies via "public" inheritance
    * variables are areas in computer memory
    * is an object 1 thing or 2 things for 2 threads of control?
  * Smalltalk emphasized message passing between stateful objects
    * functions are first class citizens
    * emphasized datastructures that pass messages between themselves
      * methods are functions tightly associated with the data
      * methods are the interfaces between objects
    * everything is an object, functions & classes "first class" objects
    * variables are references to objects, not areas in computer memory
      * different "labels" can refer to the same object
      * the same "label" can be reassigned to another object
    * classes in Smalltalk can thought of
      * as groupings of objects
      * defining polymorphic relationships between objects
  * C++
    * had to be backward compatible to C
      * hence syntax very inflexible
      * Dennis Richie said
        * if he did C over again declarations would be postfix
      * Bjarne Stroustrup said
        * "I consider the C declarator syntax an experiment that failed."
    * in early days C++ implemented as a pre-compiler to C
      * again creating backward capatibility issues
    * OOP model heavily influenced by the "Gang of Four"
    * in modern times more template driven than OOP driven
    * compiler sits in your class with a shotgun to enforce encapsulation
  * Java
    * syntax was made to be familiar to C++ programmers
    * until recently Java forced Gang-of-Four OOP down your throat
    * 
  * Python
    * OOP model more closely follows Smalltalk
    * encapsulation more a "gentleman's agreement"
      * a gentleman does not pry into private affairs of others
      * if you do, it is on you

### Everything is an object in Python

* Everything is a object, even a classes and functions.
  * once created any object
    * can have attributes attached to it (hence OOP not COP)
    * assigned to a variable (slap a "label" on it)
  * classes are objects
    * the "class" of a class is referred to as its metaclass
    * classes like any other object
      * can have attributes attached to it
  * functions are objects
    * can be assigned to references
    * can be function & method parameters and return values
      * methods are just functions associated with a object
      * classes can have class level methods & attributes called
    * function parameters are "passed by value"
      * but all variables are references, hence cheap to pass
      * function calls
        calls associate local references object the parameter references

### Python is garbage collected

* reference counted
* "stop the world" garbage collection
  * can cause unpredictable lags
  * circular references detection addressed but not perfectly

## Typing in Python

### Python is "untyped"
* At runtime everything is "duck typed"
  * if it quacks like a duck, and walks like a duck, it is a duck
  * if object has all the invoked methods & attributes then all is good
  * if object doesn't then runtime error
* byte code compiler does not check anything beyond syntax
  * but not until (first) runtime
  * class & function byte code is cached to disk
* byte code interpreter 
* classes and modules are not templates, they are executable code
* to avoid production runtime startup lag in Python applications
  * don't define objects other than very simple argument parsing in apps
  * you want runtime errors to happen in module test suites
  * runtime errors should relate to problems in the external world

### Python type annotations

#### Variance

* if a formal type parameter defines a type for data that is
  * returned by a method, it can be covariant
  * a parameter to a method, after its initial construction, it can be contravariant
  * both a method parameter & method return type, it must be invariant.
    * not necessarily the same method
    * err on the safe side, make formal type parameters invariant.

## Python Exceptions Handling

### Python exceptions hierarchy

```
BaseException
+---SystemExit
+---KeyboardInterrupt
+---GeneratorExit
+---Exception
    +---StopIteration
    +---StopAsyncIteration
    +---ArithmeticError
    |   +---FloatingPointError
    |   +---OverflowError
    |   +---ZeroDivisionError
    +---AssertionError
    +---AttributeError
    +---BufferError
    +---EOFError
    +---ImportError
    +---LookupError
    |   +---IndexError
    |   +---KeyError
    +---MemoryError
    +---NameError
    |   +---UnboundLocalError
    +---OSError
    |   +---BlockingIOError
    |   +---ChildProcessError
    |   +---ConnectionError
    |   |   +---BrokenPipeError
    |   |   +---ConnectionAbortedError
    |   |   +---ConnectionRefusedError
    |   |   +---ConnectionResetError
    |   +---FileExistsError
    |   +---FileNotFoundError
    |   +---InterruptedError
    |   +---IsADirectoryError
    |   +---NotADirectoryError
    |   +---PermissionError
    |   +---ProcessLookupError
    |   +---TimeoutError
    +---ReferenceError
    +---RuntimeError
    |   +---NotImplementedError
    |   +---RecursionError
    +---SyntaxError
    |   +---IndentationError
    |       +---TabError
    +---SystemError
    +---TypeError
    +---ValueError
    |   +---UnicodeError
    |       +---UnicodeDecodeError
    |       +---UnicodeEncodeError
    |       +---UnicodeTranslateError
    +---Warning
        +---DeprecationWarning
        +---PendingDeprecationWarning
        +---RuntimeWarning
        +---SyntaxWarning
        +---UserWarning
        +---FutureWarning
        +---ImportWarning
        +---UnicodeWarning
        +---BytesWarning
        +---ResourceWarning
```

