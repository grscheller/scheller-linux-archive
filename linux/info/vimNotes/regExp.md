## Regular Expressions
Regular expressions, also called RegExps, are used in
in Unix/Posix like environments to match textual patterns.
They are not to be confused with shell globbing pattern matching.

### History (TL;DR):
The concept of regular expressions arose in the 1950s from
the work of mathematitian/logician Stephen Kleen in his study of
[regular languages](https://en.wikipedia.org/wiki/Regular_language).
A regular language, also called a Type-3 grammar, is at the
bottom of what is called the 
[Chomsky Hierarchy](https://en.wikipedia.org/wiki/Chomsky_hierarchy).
Regular languages have the property that they can be recongnized
by a 
[Finite State Machine](https://en.wikipedia.org/wiki/Finite-state_machine).
This property has nice computational space and time implications when using
RegExps to search for text patterns.

Before he invented Unix, computer engineer Ken Thompson built Kleen's
notation for regular expresions into the editor QED.  The editor used a RegExp
to construct a finite state machine, in machine code, to search for the
text patterns represented by the RegExp.  This was an early example
of just-in-time complication!  He later added this capability to the
Unix text editor ed.

From ed, vi inherrited them.  They are used in many Unix based
untilities; lex, sed, AWK, Emacs, Perl, Vim, and Ruby have built in
regular expression support.  Many computer languages, like Python and
C/C++, have standard library support for them.  Note that many of these
"regular expression" implementations contain features that can not be
described in the sense of the formal language theory concept of a regular
grammar. 

### Unix Regular Expressions:

### Simple Examples:

### Using Regular Expressions in Vim:

