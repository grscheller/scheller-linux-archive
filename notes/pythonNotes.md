# Python Notes

## Python is Object Oriented in the Smalltalk sense

- Python embraces classic OOP
  - C++ and Java actually are Class Oriented Programming COP
    - classes & functions are compiler directives
      - exist only at compile time
      - templates for objects
      - are not "first class" data types
    - code sharing via via "private" & "protected" inheritance
    - type hierarchies via "public" inheritance
    - variables are areas in computer memory
    - is an object 1 thing or 2 things for 2 threads of control?
  - Smalltalk emphasized message passing between stateful objects
    - functions are first class citizens
    - emphasized datastructures that pass messages between themselves
      - methods are functions tightly associated with the data
      - methods are the interfaces between objects
    - everything is an object, functions & classes "first class" objects
    - variables are references to objects, not areas in computer memory
      - different "labels" can refer to the same object
      - the same "label" can be reassigned to another object
    - classes in Smalltalk can thought of
      - as groupings of objects
      - defining polymorphic relationships between objects
  - C++
    - had to be backward compatible to C
      - hence syntax very inflexible
      - Dennis Richie said
        - if he did C over again declarations would be postfix
      - Bjarne Stroustrup said
        - "I consider the C declarator syntax an experiment that failed."
    - in early days C++ implemented as a pre-compiler to C
      - again creating backward capatibility issues
    - OOP model heavily influenced by the "Gang of Four"
    - in modern times more template driven than OOP driven
    - compiler sits in your class with a shotgun to enforce encapsulation
  - Java
    - syntax was made to be familiar to C++ programmers
    - until recently Java forced Gang-of-Four OOP down your throat
    - functions are single method classes
  - Python
    - OOP model more closely follows Smalltalk
    - encapsulation more a "gentleman's agreement"
      - a gentleman does not pry into private affairs of others
      - if you do, it is on you
    - methods are functions associated with an object

### Everything is an object in Python

- Everything is a object, even classes and functions.
  - once created any object
    - can have attributes attached to it (hence OOP not COP)
    - assigned to a variable as in
      - slapping a "label" on it (a reference)
      - not copying a "value" to it (memory location)
  - classes are objects
    - the "class" of a class is referred to as its metaclass
    - classes like any other object
      - can have attributes attached to it
      - classes can have class level methods & attributes called
        - everything is an object, hence can be treated as such
        - if a object fails to find a method or attribute
          - a class method or attribute is searched for
          - assigning to such an attribute
            - creates one at the object level
            - shadowing the one from the class level
  - functions are objects
    - can be assigned to references
    - can be function & method parameters and return values
      - methods are just functions associated with a object
    - function arguments are passed-by-value
      - but all variables are references, hence cheap to pass
      - function calls associate local references to object the parameter references
  - built-ins are C code that acts like a class or function
    - some times you can derive a class from a builtin (int, tuple)
    - sometimes you can't (boolean)
    - some builtins pretend to be something they are not
      - like an int being an immutable singleton
        - where `a = a + 42`
          - does not change the value of an int
          - it reassigns the label a to a new int object
          - but computer really do change the values stored in registers
      - like int a subtype of float which is a subtype of complex
        - a disease going all the way back to Fortran
          - `x: float = 1.0` NOT internally same as `i: int = 1`
          - `c: complex = 3.0 + 0.0j` NOT the same as `x: float = 3.0`

### Python is garbage collected

- reference counted
- "stop the world" garbage collection
  - can cause unpredictable lags
  - circular references detection addressed but not perfectly
    - consider weakrefs

## Typing in Python

### Python is "untyped"
- At runtime everything is "duck typed"
  - if it quacks like a duck, and walks like a duck, it is a duck
  - if object has all the invoked methods & attributes then all is good
  - if object doesn't then runtime error
- byte code compiler does not check anything beyond syntax
  - but not until (first) runtime
  - class & function byte code is cached to disk
- byte code interpreter 
- classes and modules are not templates, they are executable code
- to avoid production runtime startup lag in Python applications
  - don't define objects other than very simple argument parsing in apps
  - you want runtime errors to happen in module test suites
  - runtime errors should relate to problems in the external world

### Python type annotations

#### Variance

- if a formal type parameter defines a type for data that is
  - returned by a method, it can be covariant
  - a parameter to a method, after its initial construction, it can be contravariant
  - both a method parameter & method return type, it must be invariant
    - not necessarily the same method

##### TypeVars

- must use TypeVars to denote variance, otherwise invariant
  - not sure how "smart" mypy is to deduce variance
  - better to be explicit
- contravariance and covariance are associated with classes
  - not functions or methods
  - TypeVars carrying variance info
    - should not be used for functions
    - should not be used with methods unless defined at the class level
- examples
  - invariant: `A = TypeVar('A')`
  - covariant: `B_co = TypeVar('B_co', Covariant=True)`
  - contravariant: `C_con = TypeVar('C_con', Contravariant=True)`
  - invariant: `def foo[A, B](a: A, f: Callable[[A], B]) -> B:`
    - A and B are both invariant

##### Variance rule-of-thumb

- is the generic type used only as a method argument type for your class?
  - if so, probably contravariant
- is it used only as a method return type for your class?
  - if so, probably covariant
- is it used as an attribute type for your class, and those attributes are immutable?
  - if so, probably covariant
- in all other cases, probably invariant.

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

### Detailed descriptions

- **BaseException** implementation detail, never use
- **Exception**
  - all built-in, non-system exiting exceptions
  - all user-defined exception should be derived from this
- **TypeError**
  - raised when operation or function is applied to wrong type
- **ValueError**
  - raised by operation or function of correct type but inappropriate value
  - and situation not described by a more precise exception
- **ArithmeticError**
  - **OverflowError**
  - **ZeroDivisionError**
  - **FloatingPointError** (currently not used in std libraries)
- **BufferError**
  - used by bytes, bytearray
  - used by extension types like array.array
    - efficient arrays of numeric values
- **LookupError**
   - **IndexError**
   - **KeyError**
  - also used by codecs.lookup()
- **AssertionError**
  - for "dot" lookups
    - use TypeError instead if object does not support attributes
    - \_\_getattributr\_\_() is called and returns ref or raises AttributeError
      - if AttributeError raised and \_\_getattr\_\_() defined
      - then \_\_getattr\_\_() used as a fallback
- **EOFError**
  - raised when input() function hits end-of-file without reading any data
    - io.IOBase.read() and io.IOBase.readline() just return empty string
- **GeneratorExit**
  - inherits from BaseException (technically not an error)
  - raised when a generator or coroutine is closed
- **ImportError**
  - **ModuleNotFoundError**
- **IndexError**
  - raises TypeError if index is not an integer
- **KeyError**
  - for dict, raised whenkey not there
- **KeyboardInterrupt**
  - inherits from BaseException (so not caught by Exception)
  - best practice is to end program fast and safely
- **MemoryError**
  - out-of-memory
  - iffy, if caught maybe objects can be deleted? 
- **NameError**
  - local or global name not found
- **NotImplementedError**
  - derived from RuntimeError
  - not to be confused with the NotImplemented builtin constant
- **OSError**
  - a lot of moving parts and subclassed exceptions
- **PythonFinalizationError**
  - derived from RuntimeError
  - raised when operation is blocked during interpreter shutdown
  - added in 3.13
- **RecursionError**
  - derived from RuntimeError
  - added in 3.5
- **ReferenceError**
  - when weakref.proxy is used to access an attribute after GC
  - see weakref module
- **RuntimeError**
  - raised when error does not fall in any other category
  - associated value is a string
- **StopIteration**
  - triggered by next() builtin or iterator's __next__() method
  - raised when a generator or coroutine returns
  - single attribute is value
    - given at exception creation, default in None
  - if directly or indirectly raised within generator (coroutine?) code
  - then converted to RuntimeError
- **StopAsyncIteration**
  - raised by the __anext__() method of an asynchronous iteator
- **SyntaxError**
  - raised by parser
    - can occur in
      - import statements
      - compile(), exec() or eval() built-in functions
      - or reading script from stdin (like in a REPL)
  - single attribute is a tuple containing location information
  - **IndentationError**
    - **TabError**
- **SystemError**
  - internal Python interpreter error
- **SystemExit**
  - inherits from BaseException (so not caught by Exception)
  - raised by sys.exit() function
  - attribute code = exit status | string to print | None
- **UnicodeError**
  - **UnicodeEncodeError**
  - **UnicodeDecodeError**
  - **UnicodeTranslateError**

### Programming Exceptions encountered in the wild

- **UnboundLocalError**
  - a subclass of **NameError**
