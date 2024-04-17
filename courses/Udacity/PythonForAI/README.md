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

## 2024-04-17:

I am giving up trying to put all my LSP infrastructure in a single
Python venv. Some of this infrastructure seems to need access access to
the virtual environment that the code being developed will run in.

I have set ve.conf to manage these virtual environments:

* dev (3.11.8)
  * pynvim

* dev_next (3.12.8)
  * ipython
  * pynvim

* grs (3.11.8)
  * grscheller.circular-array
  * grscheller.datastructures
  * grscheller.boring-math
  * ipython
  * pynvim
  * python-lsp-server pylsp-mypy ruff-lsp rope mccabe

* pydev (3.11.8)
  * ipython pytest pdoc3 flit
  * pynvim
  * python-lsp-server pylsp-mypy ruff-lsp rope mccabe

* pydev_next (3.12.2)
  * ipython pytest
  * pynvim

* py4ai (3.11.8)
  * matplotlib jupyterlab
  * ipython pytest
  * pynvim
  * python-lsp-server pylsp-mypy ruff-lsp rope mccabe

The dev venv is for non-Python development. Any python tools like
ipython will default to the system versions. The inclusion of pynvim is
for python based tooling for other languages.

The grs venv is for developing Python code where I want to leverage
a number of PyPI projects like my grscheller libraries. It is geared
for developing general Python applications. More of a prototype for
future Python venv's.

The pydev is a minimal venv mostly for developing Python modules. I use
my pypi.fish (pypath.fish?) script to manage $PYTHONPATH. This is the
environment I use to develop my grscheller libraries themselves.

The py4ai is an environment I am using for a Python for an online series
of AI/Data Science courses I am taking.

## 2024-04-17:

I tested out the pydev venv and the LSP worked very well, mypy being
especially helpful. Mypy did follow `$PYTHONPATH` but referred me to
a section of the mypy docs entitled "How imports are found". I may want
to configure something called `mypy_path` or the `$MYPYPATH` environment
variable.

At this point, LSP is working. I want to do some minimal configuration
tweaking and get back to the course.

## 2024-04-17:

Do I need node anymore?

```
   $ sudo pacman -Rsc nodejs
   checking dependencies...
   :: jupyterlab optionally requires npm: to install extensions
   
   Package (12)                Old Version  Net Change
   
   acorn                       1:8.11.0-1    -0.51 MiB
   bash-language-server        5.1.2-1       -4.94 MiB
   libngtcp2                   1.4.0-1       -0.45 MiB
   node-gyp                    10.1.0-2      -6.84 MiB
   nodejs-nopt                 7.2.0-1       -0.03 MiB
   npm                         10.5.2-1      -7.60 MiB
   npm-check-updates           16.14.18-1   -17.48 MiB
   semver                      7.6.0-1       -0.12 MiB
   typescript                  5.4.5-1      -30.86 MiB
   typescript-language-server  4.3.3-1       -2.13 MiB
   yaml-language-server        1.14.0-1     -17.78 MiB
   nodejs                      21.7.2-1     -46.86 MiB
   
   Total Removed Size:  135.60 MiB
   
   :: Do you want to remove these packages? [Y/n] n
```

Looks like I do for jupyterlab as well as for other language servers I'd
like to have. I really just don't to use typescripty things for large
software projects.

So, I'll keep the Node.js infrastructure on Arch Linux.

Do I need the Nix package manager and its neovim integrations?
