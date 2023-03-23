#!/usr/bin/env python
#
# Python TTK program to convert feet to meters
# (broken) code found here: https://tkdocs.com/tutorial/firstexample.html
# 

import tkinter as tk
from tkinter import ttk

def calculate(feet: tk.StringVar):
    try:
        value = float(feet.get())
        meters.set(str(int(0.3048*value*10000.0 + 0.5)/10000.0))
    except ValueError:
        pass

root = tk.Tk()
root.title("Feet to Meters")

mainFrame = ttk.Frame(root, padding="3 3 12 12")
mainFrame.grid(column=0, row=0, sticky="nwes")
root.columnconfigure(0, weight=1)
root.rowconfigure(0, weight=1)

feet=tk.StringVar()
feetEntry = ttk.Entry(mainFrame, width=7, textvariable=feet)
feetEntry.grid(column=2, row=1, sticky='we')

meters=tk.StringVar()
ttk.Label(mainFrame, textvariable=meters).grid(column=2, sticky='we')

ttk.Button(
    mainFrame,
    text='Calculate',
    command=calculate # <- why do we need this here?
  #  command=(lambda _, calc=calculate, ft=feet: calc(ft))
).grid(column=3, row=3, sticky='w')

ttk.Label(mainFrame, text='feet').grid(column=3, row=1, sticky='e')
