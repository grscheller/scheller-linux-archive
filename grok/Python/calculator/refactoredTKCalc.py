#!/usr/bin/env python

# Lightly modified from: https://www.simplifiedpython.net/python-calculator with
# the help of the pyright LSP server.  The Arch system Python already had tkinter
# module, but the tk and tcl Pacman packages also needed to be installed. Note, 
# the example being followed used old style Tk, not the newer ttk version.
#
# I don't think Tk has working closurses, hence the use of default parameters below
# in lambda functions.
#
# Best if run in floating window.

import tkinter as tk
import tkinter.font as tkfont

# Based on Kanagawa colorscheme
class Color(object):
    winterBlue    = '#252535'
    dragonBlack0  = '#0D0C0C'
    fujiWhite     = '#DCD7BA'
    oldWhite      = '#C8c093'
    lotusViolet3  = '#C9CBD1'

# Refactored calculator app I found on internet
class myCalc(tk.Frame):
 
    def _mk_frame(self, side):
        frame = tk.Frame(
            self,
            borderwidth=4,
            bd=2,
            bg=Color.lotusViolet3,
        )
        frame.pack(
            side=side,
            expand=tk.YES,
            fill=tk.BOTH
        )
        return frame
    
    def _mk_button(self, parent, side, text, command):
        button = tk.Button(
            parent,
            text=text,
            command=command,
            bg=Color.lotusViolet3,
            fg=Color.dragonBlack0,
            activebackground=Color.oldWhite,
            highlightcolor=Color.lotusViolet3,
            highlightbackground=Color.lotusViolet3,
            highlightthickness=2
        )
        button.pack(
            side=side,
            expand=tk.YES,
            fill=tk.BOTH
        )
        return button
    
    def _mk_entry(self, side, display):
        entry = tk.Entry(
            self, 
            relief=tk.RIDGE,
            textvariable=display,
            justify='right',
            bd=20,
            fg=Color.fujiWhite,
            bg=Color.winterBlue,
            highlightcolor=Color.lotusViolet3,
            highlightbackground=Color.lotusViolet3
        ).pack(
            side=side,
            expand=tk.YES,
            fill=tk.BOTH
        )
        return entry

    def __init__(self):
        tk.Frame.__init__(self)

        # Configure tk.Frame parent class
        self.option_add('*Font', tkfont.Font(family='arial', size=16, weight='bold'))
        self.pack(expand=tk.YES, fill=tk.BOTH)
 
        # Construct display
        display = tk.StringVar()
        self._mk_entry(tk.TOP, display)
 
        # Construct clear button
        clear_frame = self._mk_frame(tk.TOP)
        self._mk_button(
            clear_frame,
            tk.LEFT,
            'C',
            lambda disp=display, _='C': disp.set('')
        )
 
        # Construct calculator data entry buttons
        for rowButtons in ('789/', '456*', '123-', '0.+'):
            button_frame = self._mk_frame(tk.TOP)
            for rowButton in rowButtons:
                self._mk_button(
                    button_frame,
                    tk.LEFT,
                    rowButton,
                    lambda disp=display, rb=rowButton: disp.set(disp.get() + rb)
                )
 
        # Construct equals button
        eq_frame = self._mk_frame(tk.TOP)
        eq_button = self._mk_button(eq_frame , tk.LEFT, '=', None)
        eq_button.bind('<ButtonRelease-1>', lambda _, _self=self, disp=display: _self._calc(disp), '+')
 
    def _calc(self, display):
        try:
            display.set(eval(display.get()))
        except:
            display.set("ERROR")

myCalc().mainloop()
