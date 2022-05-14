# Fish Shell Factoids

The Fish Shell is in the Ksh/Bash post Bourne Shell family of shells.
Fish came out around 2005 and does what some of the other shells
should have done back then, thrown off the shackles of Bourn Shell
backwards compatibility.  As a result, Fish is not POSIX compatible.

## Input pipeline as if typed

These are equivalent,

```
   [fish]
   $ echo hello (echo boobar fuzzbaz|sed 's/bar/bus/') world
   hello boobus fuzzbaz world

   [bash]
   $ echo hello $(echo boobar fuzzbaz|sed 's/bar/bus/') world
   hello boobus fuzzbaz world
```

## Read pipeline as if from a File

These are equivalenr

```
   [fish]
   $ diff (printf 'a b\nc d e\nx y\n'|psub) (printf 'a b\nc f e\nx y\n'|psub)
   2c2
   < c d e
   ---
   > c f e

   [bash]
   $ diff <(printf 'a b\nc d e\nx y\n') <(printf 'a b\nc f e\nx y\n')
   2c2
   < c d e
   ---
   > c f e
```

psub takes the following options:

| Option      | Long Option       | Description                                       |
|:-----------:|:----------------- |:------------------------------------------------- |
| `-f`        | `--file`          | use regular file, allows seeking, but is slower   |
| `-F`        | `--fifo`          | use fifo, faster but buffering problems can occur |
| `-s SUFFIX` | `--suffix=SUFFIX` | append SUFFIX like .c onto backing file or fifo   |

While bash uses the /dev/fd/ device files, psub is a fish function that returns
the name of a file or fifo it creates and later cleans up.  Using an actual file
is the default.  Fifos on Linux have a 64KiB capacity, which can be a problem
for nonblocking writing.

## Sourcing from a Pipeline

The fish source command can read from stdin,

```
   [fish]
   $ echo echo happy feet | source
   happy feet

   [bash]
   $ source <(echo echo happy feet)
   happy feet
```

Here is a more practical example, import the abbriviations
from another system,

```
   [fish]
   $ ssh some_host abbr -s | source
```

This assumes your login shell is fish on the other system and
ssh is configured correctly.

Unlike other shells, sourced files can take arguments like shell scripts,

```
   $ echo 'set foo hello
     echo $foo $argv[2]
     ' | source - goat world frog
   hello world

   $ echo 'set foo hello
     echo $foo $argv[2]
     ' > junk

   $ source junk earth mars saturn
   hello mars
```

## Read Trick since Pipelines Don't Use Subshells

You can use this Ksh pattern to asign values to shell variables
in the current shell.

```
   $ echo foobar buzzbaz | sed 's/bar/bus/' | read Foo Buzz
   $ echo $Foo $Buzz
   foobus buzzbaz
```

With no variables, read simply sends output to stdout, not some
default variable.

```
   $ echo boobar fuzzbaz|sed 's/bar/bus/' | read
   boobus fuzzbaz
```

Read is useful when reading lines of data

```
   $ echo 'aa bb cc
   dd ee
   ff gg hh ii' | while read a b c
       printf 'a - %s  b = %s  c = %s\n' $a $b $c
   end
   a - aa  b = bb  c = cc
   a - dd  b = ee  c =
   a - ff  b = gg  c = hh ii

   $ echo "a = $a"
   a =
```

Avoid shadowing outer scope variables with an an inner scope
read.  Not only is it bad programming practice, I found it can
cause Fish to behave a bit screwy.  Note that the variale aa does
not retain any of its while loop values.  This would be true even
if it was set in global or universal scope.  If you really want
to do this,

```
   $ set -g aa AA

   $ begin
         set -l aa
         read aa
         echo $aa
     end
   read> DDD
   DDD

   $ echo $aa
   AA
```

Fish is a bit more flexible with whitespace than other shell.  Note
that the shell variable xx has not been set before the while loop.

```
   $ echo 'aa
     bb
     cc' |
         while read xx
             echo $xx
         end
     aa
     bb
     cc

     /home/grs
     $ set -S xx
     $xx: set in global scope, unexported, with 0 elements
```

## Argument Parsing

Todo: see type psub, man argparse, man fish_opt

## Periodically update fish completions

Update command completions based on manpages.

```
   $ fish_update_completions
```

  Todo: see man fish_update_completions

