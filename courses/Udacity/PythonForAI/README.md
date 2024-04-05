# Notes

## 2024-04-04

Working on gaussian module in the ../03-gaussian/ directory.

Created ai4py Python virtual environment. Added matplotlib and this is
what pip brought in:

```
$ pip list
Package         Version
--------------- -----------
contourpy       1.2.1
cycler          0.12.1
fonttools       4.50.0
kiwisolver      1.4.5
matplotlib      3.8.4
numpy           1.26.4
packaging       24.0
pillow          10.3.0
pyparsing       3.1.2
python-dateutil 2.9.0.post0
six             1.16.0
```

Decided not to bring in jupyter notebook dependencies. I am wondering
how the matplotlib GUI will work.

Decided to use pytest instead of the standard libraries unittest module.
Brought in the following dependencies.

```
iniconfig       2.0.0
pluggy          1.4.0
pytest          8.1.1
```

## 2024-04-04

LSP not that helpful, first thought was to install mypy into virtual
environment, packages installed:

```
mypy              1.9.0
mypy-extensions   1.0.0
typing_extensions 4.10.0
```

Did not fix problem. The problem is with pylsp. LSP Logs say that it was
not found in the virtual environment. It is installed in the neovim
environment, maybe it is needed in the py4ai virtual environment.


