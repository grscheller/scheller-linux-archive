#!/usr/bin/env python

# Lightly modified from: https://www.simplifiedpython.net/python-calculator/ with
# the help of the pyright LSP server.  The Arch system Python had had tkinter module
# but the tk and tcl Pacman packages also needed to be installed.

import tkinter

YES = tkinter.YES
BOTH = tkinter.BOTH
LEFT = tkinter.LEFT
TOP = tkinter.TOP
RIDGE = tkinter.RIDGE

Frame = tkinter.Frame
Button = tkinter.Button
StringVar = tkinter.StringVar
Entry = tkinter.Entry
 
def iCalc(source, side):
    storeObj = Frame(source, borderwidth=4, bd=4, bg="powder blue")
    storeObj.pack(side=side, expand = YES, fill = BOTH)
    return storeObj
 
def button(source, side, text, command):
    storeObj = Button(source, text=text, command=command)
    storeObj.pack(side=side, expand = YES, fill= BOTH)
    return storeObj
 
class app(Frame):
    def __init__(self):
        Frame.__init__(self)
        self.option_add('*Font', 'arial 20 bold')
        self.pack(expand = YES, fill = BOTH)
 
        display = StringVar()
        Entry(self, relief=RIDGE, textvariable=display, justify='right',
              bd=30, bg="powder blue").pack(side=TOP, expand=YES, fill = BOTH)
 
        for clearButton in (["C"]):
            erase = iCalc(self, TOP)
            for ichar in clearButton:
                button(erase, LEFT, ichar, lambda storeObj=display, _=ichar: storeObj.set(''))
 
        for numButton in ("789/", "456*", "123-", "0.+"):
         FunctionNum = iCalc(self, TOP)
         for iEquals in numButton:
            button(FunctionNum, LEFT, iEquals,
                   lambda storeObj=display, q=iEquals: storeObj.set(storeObj.get() + q))
 
        EqualButton = iCalc(self, TOP)
        for iEquals in "=":
            if iEquals == '=':
                btniEquals = button(EqualButton, LEFT, iEquals, None)
                btniEquals.bind('<ButtonRelease-1>',
                                lambda _,s=self, storeObj=display: s.calc(storeObj),
                                '+')
            else:
                btniEquals = button(EqualButton, LEFT, iEquals,
                                    lambda storeObj=display, s=' %s ' % iEquals:
                                        storeObj.set(storeObj.get() + s))
 
    def calc(self, display):
        try:
            display.set(eval(display.get()))
        except:
            display.set("ERROR")

if __name__=='__main__':
    app().mainloop()
