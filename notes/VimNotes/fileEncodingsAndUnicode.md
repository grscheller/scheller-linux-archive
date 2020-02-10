# Encodings and Unicode
Vim is capable of using different character encodings when
editing and saving files.  I find UTF-8 the most useful
encoding.

##  Character Encodings:
Vim is capable of working with different character encodings.

| Command                  | Description                                  |
|:------------------------ |:-------------------------------------------- |
| :set encoding            | list current character encoding              |
| :set encoding=utf-8      | set current character encoding vim displays  |
| :set fileencoding        | list current encoding in which to save files |
| :set encoding=latin1     | edit using Latin-1 character encoding        |
| :set fileencoding=latin1 | save files with Latin-1 encoding             |
| :set fileencodings       | list encodings to try when loading a file    |

The two main ones are UTF-8 for UNIX, MAC-OS, and HTML/Web and UTF-16
for Microsoft Windows.  Windows these days uses a mix of UTF-16, UTF-8,
and legacy encodings.

On my linux systems I put
```
   set encoding=utf-8
   set fileencoding=utf-8
```
in my `~/.vim/vimrc` file.  Now a days vim automatically figures out
whether the file being editted in UTF-8 or UTF-16LE with `\r\n` line
endings.

(TL;DR): GIT, for text files, does the conversion to the correct format
depending on the system you checkout to.  In the olde day, we use to
FTP files in text mode to convert format between different OS's.

## Vim Digraphs:
Following the recommendations of
[RFC-1345](https://tools.ietf.org/html/rfc1345),
vim allows users to enter characters within whatever encodings they
are using via 2 character "diagraph" sequences.  While in _Insert Mode_,
type `<ctrl-k> followed by a two character sequence.

|  Command    `   | Description                                   |
|:---------------:|:--------------------------------------------- |
|   `:dig`        | list diagraphs available for current encoding |
|  `<ctrl-k>a^`   | enter the character â                         |
|  `<ctrl-k>o:`   | enter the character ö                         |
|  `<ctrl-k>i'`   | enter the character í                         |
|  `<ctrl-k>l\*`  | enter the character λ                         |

The choice of characters you can enter this way depends on the
encoding you are using.

## Using Unicode Code Points:
In modern Unix terminal emulators and Libre Office, input and display
of Unicode code points just works.  Terminals are fixed width font
beasts, but Libre Office handles the variable width code points just fine.

While in _Insert Mode_, code points can be entered into vim either from
the terminal or by typing into gvim.

|  Command                     | Description            |
|:----------------------------:|:---------------------- |
|  `<ctrl-shift-u>u03b2<ret>`  | enter the character β  |
|  `<ctrl-shift-u>u3bb<ret>`   | enter the character λ  |

