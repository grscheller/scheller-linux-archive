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

## 2024-04-07

The problem is that jedi-language-server was not installed into the
py4ai virtual environment.

I am having trouble finding good information/documentation on what
exactly needs to be installed into the neovim virtenv vs what needs to
be installed into the virtenv of the Python version for which I am
developing.

Have been working on my devel environment infrastructure so that I will
not have to maintain my python virtenvs by hand.

## 2024-04-09

While working on ve.fish I think I found an answer to how to use
matplotlib without Jupyter notebooks:

```
   https://matplotlib.org/stable/users/explain/figure/interactive.html
```

Why Fish and not Python? Same reason pyenv uses Bash. If something goes
south or doesn't upgrade nicely with Python, I don't want it to affect my
configuration tool that I am using to fix the problem.

## 2024-04-15:

Decided I needed a better tool to maintain my Python virtual
environments beyond my ragtag collection of fish abbreviations and
a rather crude ve.fish function to help keep track of all my venv's
(virtual environments). Got rid of the fish abbreviations and made
ve.fish a full-fledged tool.

Why not do the tool in Python? Same reason pyenv uses Bash, if my Python
environment is totally screwed up, I don't want my tool that I use to
fix the environment screwed up too. 

The ve function does not depend on pyenv, but will use it if it is the
path. This is a design feature if I have to deploy the tool to an
environment which lacked pyenv.

Thought it would take a day or two to develop, took about 6 days.

## 2024-04-16:

Sorting out LSP for Python. Corrected syntax errors in plugins/lsp/lsp.lua
in my Neovim configs. Changing LSP servers from jedi-language-server to 
python-language-server.

First let's see what is available.

```
   $ pip install python-lsp-server
```

| Providers   | Description                                  |
|:-----------:|:-------------------------------------------- |
| Rope        | completions and renaming                     |
| Pyflakes    | linter to detect various errors              |
| McCabe      | linter for complexity checking               |
| pycodestyle | linter for style checking                    |
| pydocstyle  | docstring linter & style checking (disabled) |
| autopep8    | code formatting                              |
| YAPF        | code formatting (preferred over autopep8)    |
| flake8      | error checking (default)                     |
| pylint      | code linting (default)                       |

Also, install the pylsp-mypy plugin for python-lsp-server as well as
mypy itself. Mypy is an optional static type checker for Python
that aims to combine the benefits of both dynamic typing and
static typing. 

```
   $ pip install mypy pylsp-mypy
```

There is also ruff and a LSP for it, ruff-lsp. Ruff An extremely
fast Python linter and code formatter, written in Rust.

Seems that python-lsp-server also as "plug-ins" for ruff and mypy!

## 2024-04-16:

The old "neovim" module was renamed to "pynvim". I was going crazy
figuring out what exactly pynvim was and if neovim plugin was still
needed.

Also note that the other LSP servers I use are all written in Lua.

## 2024-04-16:

I will use python-lsp-server with the following plugins & providers:

| Providers   | Plugin      | Description                                  |
|:----------- |:----------- |:-------------------------------------------- |
| mypy        | pylsp-mypy  | static type checker                          |
| ruff        | ruff-lsp    | linter & formatter (written in rust)         |
| rope        |             | completions and renaming                     |
| mccabe      |             | linter for complexity checking               |

Here is what I will configure ve.fish to install in neovim environment:

```
   $ pip install pynvim python-lsp-server pylsp-mypy ruff-lsp rope mccabe
```

I am assuming ruff and mypy are dependencies of ruff-lsp and pylsp-mypy
respectfully.

The next steps will be to make sure I configure python-lsp-server, mypy,
ruff and mccabe correctly. This may be split between my neovim configs,
pyproject.toml, and tool configuration files. From what I have read,
tools (providers) will use pyproject.toml over their own config files.

