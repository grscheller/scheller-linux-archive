#!/usr/bin/env python

# Using the old style Tk libraries, not the newer ttk version.
#
# I suspect Tk does not have working closurses, hence the
# use of default parameters the lambda functions below.
#
# Best if run in floating window.

import tkinter as tk
import tkinter.font as tkfont

# Based on Kanagawa colorscheme
class Color(object):
    winterBlue   = '#252535'
    dragonBlack0 = '#0D0C0C'
    fujiWhite    = '#DCD7BA'
    lotusYellow  = '#77713F'
    lotusViolet3 = '#C9CBD1'
    winterGreen  = '#2B3328'

# Refactored calculator app I found on internet
class myCalc(tk.Frame):
 
    def _mk_frame(self, side):
        frame = tk.Frame(
            self,
            borderwidth = 4,
            bg = Color.winterGreen,
        )
        frame.pack(
            side = side,
            expand = tk.YES,
            fill = tk.BOTH
        )
        return frame
    
    def _mk_display(self, side, display):
        entry = tk.Entry(
            self, 
            relief = tk.RIDGE,
            textvariable = display,
            borderwidth = 10,
            width = 21,
            justify = 'right',
            fg = Color.lotusViolet3,
            bg = Color.winterBlue,
            highlightcolor = Color.winterGreen,
            highlightbackground = Color.winterGreen
        ).pack(
            side = side,
            expand = tk.YES,
            fill = tk.BOTH
        )
        return entry

    def _mk_button(self, parent, side, text, command):
        button = tk.Button(
            parent,
            text = text,
            command = command,
            bg = Color.winterGreen,
            fg = Color.lotusViolet3,
            activebackground = Color.lotusYellow,
            highlightcolor = Color.winterGreen,
            highlightbackground = Color.winterGreen,
            highlightthickness = 2
        )
        button.pack(
            side = side,
            expand = tk.YES,
            fill = tk.BOTH
        )
        return button
    
    def __init__(self):
        ## Configure tk.Frame parent class
        tk.Frame.__init__(self)
        self.option_add('*Font', tkfont.Font(family = 'arial', size = 16, weight = 'bold'))
        self.pack(expand = tk.YES, fill = tk.BOTH)
 
        ## Construct display
        display = tk.StringVar()
        self._mk_display(tk.TOP, display)
 
        ## Construct top row
        top_frame = self._mk_frame(tk.TOP)

        # Construct clear button
        self._mk_button(
            top_frame,
            tk.LEFT,
            'C',
            lambda disp=display, _='C': disp.set('')
        )
 
        ## Construct calculator data entry buttons
        for rowButtons in ('789/', '456*', '123-', '0.+'):
            button_frame = self._mk_frame(tk.TOP)
            for rowButton in rowButtons:
                self._mk_button(
                    button_frame,
                    tk.LEFT,
                    rowButton,
                    lambda disp=display, rb=rowButton: disp.set(disp.get() + rb)
                )
 
        ## Construct bottom row
        bottom_frame = self._mk_frame(tk.TOP)

        # Construct equals button
        eq_button = self._mk_button(bottom_frame , tk.LEFT, ' = ', None)
        eq_button.bind(
            '<ButtonRelease-1>',
            (lambda _, _self=self, disp=display: _self._calc(disp)),
            '+'
        )
 
    def _calc(self, display):
        try:
            display.set(eval(display.get()))
        except:
            display.set("ERROR")

myCalc().mainloop()
