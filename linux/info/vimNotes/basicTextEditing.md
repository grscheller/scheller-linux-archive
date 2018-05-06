## Basic text editing by example
This should be enough to enable you to be productive with vim.
For many years, this is basically all I knew.  Vim, like EMACS,
is a very power editor.  These examples barely scratch the surface
of what it can do.

(TL;DR) Vim is actually a Turing complete language.  It can be turned
into a complete IDE or Unix Shell.

I think with a few weeks of practice, the material covered here
can be internalized.  Eventually, these commands become part of
one's "muscle memory."

1. Vim has 4 modes:
   * _Normal Mode_
   * _Insert Mode_
   * _Command Mode_
   * _Visual Mode_ (not in original vi)

2. Cursor movement in _Normal Mode_:
   ```
      h,i,j,k   move cursor one character (also arrow keys)
      w, W      move forward to beginning next word
      b, B      move back to beginning of previous word
      e, E      move forward to end of word
      $         move to end of line
      ^         move to first non-whitespace character on line 
      0         move to first character on line
      %         move to the matching (, ) or [, ] or {, }
      (         move to beginning of sentence
      )         move to beginning of next sentence
      {         move up a paragraph
      }         move down paragraph
      G         move to last line in file
      gg        move to first line in file
      f<char>   move forward to next <char> on current line
      ;         move forward to target of last f command
      ,         move backward to target of last f command
      3f<char>  move forward to 3rd occurrence of <char> on line
      /RegExp<ret>  forward search for regular expression pattern
      ?RegExp<ret>  backward search for regular expression pattern
      /<ret>    search forward for last pattern
      ?<ret>    search backward for last pattern
      n         search forward or backward for last pattern
   ```

3. Interacting with the buffer in _Normal Mode_:
   ```
      yy    yank line to buffer (copy)
      dd    delete line and put in buffer (cut)
      5dd   delete 5 lines and put in buffer
      x     delete character under cursor to buffer
      p     paste buffer contents "after"
      P     paste buffer contents "before"
   ```
   What "before" or "after" mean depends on what is
   in the buffer.  `y` and `d` can be used with all
   the cursor positioning commands in section 2
   ```
      d$    delete to end of line and put in buffer
      3yw   yank three words to buffer, starting at cursor
      d2fz  delete from cursor to 2nd z on current line
      2y3w  ends up yanking 6 words
   ```
   You can use named buffers to store text.
   ```
      'adw  delete word and put in buffer a
      'A2yy yank 2 lines and append to buffer a
      'sd$  delete to end of line and put in buffer "s"
      'sp   paste contents of buffer "s" after cursor
      'aP   paste contents of buffer "a" before cursor
   ```
   One use case for named buffers is copying multiple items
   from multiple files and paste them into later files.

4. Commands to insert or manipulate text:
   These commands take vim from _Normal Mode_ to _Insert Mode_.
   To return to _Normal Mode_, type `<esc>`.
   ```
      i    insert text at cursor
      a    insert text after cursor
      I    insert text at beginning of line after initial white space
      0i   insert text beginning of line
      A    insert text at end of line
      o    open new line after current line to insert text
      O    open new line before current line to insert text
      3cw  change next three words
      c3w  change next three words
      c$   change text to end of line
      5cc  change next 5 lines
      5C   change next 5 lines
      c^   change text before cursor, excluding initial white space
      s    delete current character and enter Normal Mode
      ~    change case of current char, advance one char, return to Command Mode
      r    change current char to next char typed, stay in Command Mode
   ```
   While in _Insert Mode_, the file can be navigated through with the arrow keys.

5. Undo/redo commands:
   ```
      u         undo previous edit
      <ctrl-r>  redo edit undone
   ```
   These can be used to linearly undo and redo edits,
   like the arrow buttons in a web browser.
   Navigating with the arrow keys while in _Insert Mode_
   will result in multiple entries in the undo/redo buffers.

6. _Command Mode_ (line editor) commands:

   Vim is an open source version of the Unix editor vi,
   which is a CLI visual version of the Berkeley Unix
   line editor ex, which itself is a re-implementation of
   the AT&T Unix line editor ed.
   On really old terminals, essentially line printers with
   keyboards, the descendants of teletypes, you edited files
   one line at a time.

   _Command Mode_ commands developed from the original
   line editing commands.

   Use the `:` command to enter _Command Mode_.  The
   cursor jumps down to the bottom of the terminal window
   and prompts you with `: `.
   ```
      :w       Write to disk file you are editing
      :w file  Write to file, unlike MS Word, you're still editing orig file
      :q       Quit editing, vim will warn you if you have unsaved changes
      :wq      Write to disk, then quit
      :q!      Quit without saving unsaved changes
      :n       Move to next file given on command line
      :wn      Write to disk and move on to next file to edit
      :42      Move cursor to beginning of line 42
      :#       Give line number of current line cursor is on
      :s/foo/bar/  Substitute first instance of foo with bar on current line
      :s/foo/bar/g  Substitute all instances of foo with bar on current line
      :17,42s/foo/bar/g  Substitute all foo with bar, lines 17 to 42
   ```
   While in _Command Mode_, the up & down arrow keys cycles through previous
   _Command Mode_ commands.  The left & right arrow keys help you re-edit the
   line.  To return to _Normal Mode_, without issuing a command, press <esc>.

7. Repeating commands in _Normal Mode_:
   ```
      .  repeat the last command
   ```
   The command is the last _Normal Mode_ command, not _Command Mode_
   command, that changed text,

   This is frequently used in conjunction with the `n` command.

8. Dealing with whitespace characters:
   Tell vim to indicate where line endings and tabs are,
   ```
      :set list
   ```
   to return to displaying tabs and line endings normally,
   ```
      :set nolist
   ```
   This is a wonderful feature to get rid of tabs and trailing whitespace.

9. Introduction to _Visual Mode_:
   This mode allows you to select region of text by visually highlighting
   regions that can then be modified.

   To enter _Visual Mode_ from _Normal Mode_
   * type v for character based
   * type V for line based
   * type <ctrl-v> for block visual mode
   
   Highlight text via either the <hjkl-keys> or the arrow keys.
   Once selected, you can issue either _Normal Mode_ or 
   _Command Mode_ commands on that highlighted region.  To punt
   out of _Visual Mode_, hit <esc>.

