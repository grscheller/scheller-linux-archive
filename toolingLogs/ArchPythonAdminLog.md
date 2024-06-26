# Notes taken while configuring Arch Linux for Python development.

## 2022-01-27:

I really need to better grok Python virtual environments.

```bash
   $ pacman -Ss virtualenv
   extra/python-virtualenv 20.11.0-1
       Virtual Python Environment builder
   community/python-pipenv 2022.1.8-2
       Sacred Marriage of Pipfile, Pip, & Virtualenv.
   community/python-pytest-virtualenv 1.7.0-8
       Virtualenv fixture for py.test
   community/python-selinux 0.2.1-7
       Pure-python selinux shim module for use in virtualenvs
   community/python-tox 3.24.5-1
       Python virtualenv management and testing tool
   community/python-virtualenv-clone 0.5.7-3
       A script for cloning a non-relocatable virtualenv.
   community/python-virtualenvwrapper 4.8.4-6
       Extensions to Ian Bicking's virtualenv tool
```

I had remembered pipenv but had forgotten its name. Totally
bootstrapping from userland, I would need the following

```bash
  $ pacman -Ss pipx
  community/python-pipx 0.16.4-2
      Install and Run Python Applications in Isolated Environments
```

but since I can install pipenv with pacman into the system Python
environment, and I don't intend to hop between different installed
Python versions, I think just installing pipenv package should be
enough.

Here is what I got installed on Arch,

```bash
  $ python --version
  Python 3.10.2

  $ pip --version
  pip 20.3.4 from /usr/lib/python3.10/site-packages/pip (python 3.10)

  $ pip3 --version
  pip 20.3.4 from /usr/lib/python3.10/site-packages/pip (python 3.10)

  $ pip3.10 --version
  pip 20.3.4 from /usr/lib/python3.10/site-packages/pip (python 3.10)
```

Let's see where Python will look for user stuff,

```bash
  $ python -m site --user-base
  /home/grs/.local

  $ python -m site --user-site
  /home/grs/.local/lib/python3.10/site-packages
```

Installing pipenv system wide.

```bash
   $ sudo pacman -Syu python-pipenv
   ...
   Packages (8) python-certifi-2021.10.8-3  python-filelock-3.4.0-3
     python-importlib-metadata-4.8.1-3  python-platformdirs-2.4.1-1
     python-virtualenv-20.11.0-1  python-virtualenv-clone-0.5.7-3
     python-zipp-3.7.0-1  python-pipenv-2022.1.8-2
```

Thought it would install Python Virtualenv as a dependency.

## 2022-01-27:

While I am at it, I might as well look into the capability of
installing and switching between multiple verions of Python.

```bash
   $ pacman -Ss pyenv
   community/pyenv 2.2.4-1
       Easily switch between multiple versions of Python

   $ pacman -Qlq pyenv|grep '^/usr/bin/..*'
   /usr/bin/pyenv
   /usr/bin/pyenv-install
   /usr/bin/pyenv-uninstall
   /usr/bin/python-build
```

These look to be Bash scripts. Does have fish, and zsh, support.

```fish
   $ pacman -Qlq pyenv|grep fish
   /usr/share/fish/
   /usr/share/fish/vendor_completions.d/
   /usr/share/fish/vendor_completions.d/pyenv.fish
```

## 2022-01-28:

There exists a pyenv extension called pyenv-virtualenv for working
with virtualenv with pyenv. Does not seem to be a package for
this in the regular Arch Repos.

The Github pyenv repo

* https://github.com/pyenv/pyenv-install

states "Of course, you can create virtualenv yourself, or
pyenv-virtualenv to automate the process." I am choosing
to manage my virtual environments myself with pipenv.

Another good pyenv resource is the website
[Pipenv: Python Dev Workflow for Humans](https://pipenv.pypa.io/en/latest/).

I intend to establish a coherent Python environment with the
system pyenv and manage the individual virtual environments
with that Python's pipenv.

## 2022-01-28:

Pipenv documentation resides here:
[https://pipenv.pypa.io/en/latest](https://pipenv.pypa.io/en/latest/)
on the Python Packaging Authority website.

Quoted material below is pulled verbatim from above documentation.

Pipenv "automatically creates and manages a virtualenv for your
projects, as well as adds/removes packages from your Pipfile as
you install/uninstall packages. It also generates the ever-important
Pipfile.lock, which is used to produce deterministic builds."

Pipfile vs. setup.py. Hopefully, I won't have to bother grokking
how to use setuptools directly.

```fish
   $ pacman -Ss python-setuptools\$
   extra/python-setuptools 1:59.1.1-1 [installed]
       Easily download, build, install, upgrade, and uninstall Python packages

   $ pactree -r python-setuptools
   python-setuptools
   ├─python-distro
   │ └─python-pip
   │   └─python-pipenv
   └─python-pip
```

## 2022-01-28:

Updated my fish and bash environments for pyenv. See my commit
dated Fri Jan 28 16:39:42 2022 -0700 on my
[dotfiles](https://github.com/grscheller/dotfiles) repo.

Seems to work on both Arch Linux and MacOS. Actually I didn't remember
installing pyenv on my iMac and could not test how startup scripts behaved
when pyenv not installed. Pyenv configured and installed on euler7.
Will test on gauss17 where I have not installed pyenv yet.

On euler7:

```fish
    # pyenv install 3.11-dev
    Cloning https://github.com/python/Python-3.11-dev
    Installing to /home/grs/.pyenv/versions/3.11-dev

    $ pyenv versions
    * system (set by /home/grs/.pyenv/version)
      3.11-dev

    $ pyenv version
    system (set by /home/grs/.pyenv/version)

    $ pyenv --version
    pyenv 2.2.4
```

Here is what one of the shims looks like

```fish
    $ cat .pyenv/shims/pip
    #!/usr/bin/env bash
    set -e
    [ -n "$PYENV_DEBUG" ] && set -x

    program="${0##*/}"

    export PYENV_ROOT="/home/grs/.pyenv"
    exec "/usr/share/pyenv/libexec/pyenv" exec "$program" "$@"
```

## 2022-01-29:

Installed pyenv on my various sufficiently POSIX systems.

### On euler7:

Bootstrapped pyenv config by creating empty ~/.pyenv
directory and relogged in. (Arch Linux)

### On sc31:

I seemed to have had already had pyenv installed via
brew and when I pushed my shell environment over, pyenv
configured itself. (MacOS)

I noticed that on iMac a lot of the base directory files
from the Pyenv Github repo got installed in ~/.pyenv/,
did brew clone the Github repo?

### On gauss17:

Bootstrapped pyenv via a terminal session,

```fish
      $ eval pyenv init - fish | source
```

This did exactly the same as on euler7,

```fish
       $ ls -A ~/.pyenv/*
       /home/grs/.pyenv/shims:

       /home/grs/.pyenv/versions:
```

Note from future self: I just use the pyenv from Arch Linux.

## 2022-01-29:

Why is ~/.pyenv/shims/ empty on gauss17? Only thing different I
really did was installed Python 3.11-dev.python -m tkinter

To list all versions of "things" that Pyenv can install

```fish
   $ pyenv install --list
```

Which one do I want to install? Hmmm...

### Aside:

Since I am doing a Django project, lets go with some older
Python version supported by whatever verson of Django is
installed by Pacman.

```fish
   $ pacman -Qs django
   local/python-django 3.2.10-1
       A high-level Python Web framework that encourages rapid
       development and clean design
```

Django release schedule,

|  Series | Release/Date  | End support      | End extended support |`
|:------- |:-------------:|:----------------:|:--------------------:|
| 2.2 LTS | 2.2.26        | December 2, 2019 | April 2022           |
| 3.2 LTS | 3.2.11        | December 2021    | April 2024           |
| 4.0     | 4.0.1         | August 2022      | April 2023           |
| 4.1     | August 2022   | April 2023       | December 2023        |
| 4.2 LTS | April 2023    | December 2023    | April 2026           |
| 5.0     | December 2023 | August 2024      | April 2025           |

* Django 3.2 supports Python 3.6, 3.7, 3.8, 3.9, and 3.10 (as of 3.2.9)
* Django 4.0 supports Python 3.8, 3.9, and 3.10

Also, Django website: https://www.djangoproject.com/ states

```
   "We highly recommend and only officially
   support the latest release of each series"
```

I going to see if I can build my project in three configurations

* Django 3.2 LTS using Python 3.8.12
* Django 4.0 using Python 3.10.2 (Update when 4.2 LTS available)
* System Django & Python, currently 3.2.10 and 3.10.2 respectively

The last one will "roll" with Arch Linux.

Ok, I will install Python 3.8.12 and 3.10.2 via pyenv.

```fish
    $ pyenv install 3.8.12
    $ pyenv install 3.10.2
    $ pyenv versions
    * system (set by /home/grs/.pyenv/version)
      3.10.2
      3.8.12
```

## 2022-01-31:

Will need to understand how to use virtualenv and how it interacts
with pyenv.

```fish
   $ digpath pip
   /home/grs/.pyenv/shims/pip
   /usr/bin/pip
```

We find pip's shim first, which should currently point to the
system's pip.

```fish
   $ pip --version
   pip 20.3.4 from /usr/lib/python3.10/site-packages/pip (python 3.10)

   $ /usr/bin/pip --version
   pip 20.3.4 from /usr/lib/python3.10/site-packages/pip (python 3.10)

   $ pyenv whence pip
   3.10.2
   3.8.12

   $ pyenv which pip
   /usr/bin/pip
```

Let's do the same for virtualenv, note: I have not installed the
pyenv-virtualenv plugin to pyenv since I want to use pipenv.

```fish
    $ digpath virtualenv
    /usr/bin/virtualenv

    $ pyenv whence virtualenv

    $ pyenv which virtualenv
    /usr/bin/virtualenv
```

As well as for pipenv.

```fish
    $ digpath pipenv
    /usr/bin/pipenv

    $ pyenv whence pipenv

    $ pyenv which pipenv
    /usr/bin/pipenv
```

Nobody home for pipenv and virtualenv for the pyenv locally installed
Python versions. I guess I will need to install pipenv, probably the
only thing for which I will use pip directly.

## 2022-01-31:

Here are two fairly recent blog posts which describe how to use
Pyenv and Pipenv together:

* [rootstrap blog](https//www.rootstrap.com/blog/how-to-manage-your-python-projects-with-pipenv-pyenv/)
* [hackernoon](https://hackernoon.com/reaching-python-development-nirvana-bb5692adf30c)

The Pipenv site also has information on Pyenv:

* [https://pipenv.pypa.io/en/latest](https://pipenv.pypa.io/en/latest/)
* [https://pipenv.pypa.io/en/latest/advanced](https://pipenv.pypa.io/en/latest/advanced/)

This is a big change from 5 months ago when I was going in circles with
Pipenv, Pipfile, and setup.py.

Note to self: learn distinction between Pipfile & Pipfile.lock vs. setup.py

The first two lock down a build while the later one puts constraints on
library dependencies. The first two replace the old requirements.txt
mechanism.

## 2022-02-14:

Look into HallerPatrick/py_lsp.nvim. Might be useful if it turns out
that LSP has trouble distinguishing the environment of the Python
running in nvim vs virtual environment of the code being edited.

I would like to keep my Python virtual environments as lean as possible
and not have to install pynvim and pipenv into every one of them.

See 2022-09-13

## 2022-02-15:

Beginning to understand that pipenv is not a pip replacement and pip
still has its use case.

Pipenv manages a software project with a virtual environment. Pip
manages an installed Python environment. Pipenv will use pip under
the hood to manage the virtual environment.

I still use pip to manage the Python environments I install with pyenv.

Here is an example workflow. In my Neovim init.lua configs I set

* vim.g.python3_host_prog = os.getenv("HOME") .. '/.pyenv/shims/python'

Install a version of Python (future self: WRONG!)

```fish
    $ pyenv install 3.9.7
    $ pyenv global 3.9.7
    $ PIP_REQUIRE_VIRTUALENV=false pip install pip --upgrade
    $ PIP_REQUIRE_VIRTUALENV=false pip install pipenv
    $ PIP_REQUIRE_VIRTUALENV=false pip install pynvim
    $ pyenv global system
```

Initialize a new project and associate virtual environment

```fish
    $ mkdir junk; cd junk
    $ pyenv local 3.9.7   # pipenv will do below without this
    $ pipenv --python 3.9.7
    Creating a virtualenv for this project...
    Pipfile: /home/grs/junk/Pipfile
    Using /home/grs/.pyenv/versions/3.9.7/bin/python3.9 (3.9.7) to create virtualenv...
    ⠸ Creating virtual environment...created virtual environment CPython3.9.7.final.0-64 in 146ms
      creator CPython3Posix(dest=/home/grs/.local/share/virtualenvs/junk-Jrpe9XOJ, clear=False, no_vcs_ignore=False, global=False)
      seeder FromAppData(download=False, pip=bundle, setuptools=bundle, wheel=bundle, via=copy, app_data_dir=/home/grs/.local/share/virtualenv)
        added seed packages: pip==21.3.1, setuptools==60.1.0, wheel==0.37.1
      activators BashActivator,CShellActivator,FishActivator,NushellActivator,PowerShellActivator,PythonActivator
```

and switch to it

```fish
    $ pipenv shell
    Launching subshell in virtual environment...
     source /home/grs/.local/share/virtualenvs/junk-Jrpe9XOJ/bin/activate.fish
    (junk)$ ls
    Pipfile
    (junk)$ cat Pipfile
```

```toml
    [source]]
    url = "https://pypi.org/simple"
    verify_ssl = true
    name = "pypi"

    [packages]

    [dev-packages]

    [requires]
    python_version = "3.9"
    (junk)$ python --version
    Python 3.9.7
```

I believe (need to verify) that nvim itself will still use the same
version of Python used in the virtual environment, but run outside it.

At least :checkhealth is OK. Also python run against the virtual
environment cannot import pynvim; while run against the underlying
3.9.7 environment can.

To cleanup when done,

```fish
    (junk)$ exit
    $ pipenv --rm
    Removing virtualenv (/home/grs/.local/share/virtualenvs/junk-Jrpe9XOJ)...
    $ cd ..
    $ rm -rf junk/
```

## 2022-09-13:

I almost forgot most of this! Thank God I wrote the above all down.

* To manage multiple python versions: pyenv
    * pyenv-virtualenv plugin to manage virtual environments (not used)
      * using pipenv in lieu of this
  To manage virtual environments: venv (not using directly)
    * come with Python 3.3+
    * recommended by pypa
    * by default, puts virtual environment in project subdirectory
    * when used directly, need pip to install packages
    * virtualenv manages virtual environments (not using)
      * original tool which venv deprecated
      * seems future goals are to be very cross OS and cross Python version compatible
      * popular with Python 2.7 clingers
    * virtualenvwrapper gives convenience Python programs to manage virtual environments (not using)
      * uses venv when it can
      * not sure how it handles multiple installed Python versions
        * does it need to be installed separately on every installed Python?
        * maybe could use pipenvwrapper?
  To install Python packages: pip (not usually used directly)
    * official package installer for Python
    * packages installed from the Python Package Index and other indexes
* To Manage your Python projects/packages/applications: pipenv
    * wrapper function for pip and venv
    * automatically creates and manages a virtualenv for your projects
    * adds/removes packages from your Pipfile as you install/uninstall Packages
    * generates Pipfile.lock file for deterministic builds
    * pipenvwrapper adds convinence shell function to POSIX like shells (not using)
      * not POSIX compatible shellscripting
      * bash, ksh, zsh lowest common denominator shell meta programming

For more information, see

* [https://pipenv.pypa.io/en/latest](https://pipenv.pypa.io/en/latest/)
* [https://pipenv.pypa.io/en/latest/install](https://pipenv.pypa.io/en/latest/install/)
* [https://pipenv.pypa.io/en/latest/advanced](https://pipenv.pypa.io/en/latest/advanced/)

## 2022-10-18:

I am thinking about use cases for pyenv and pipenv.

1. Maintain a personnel Python environment for my Linux user. Basically
   installing packages I write and the dependencies they require. Would
   be contained in its own virtual environment. The global default
   pipenv for my user. It would use the Arch system Python.
2. Development environments where I develop the packages I install. Try
   to make as minimal as possible.
3. Specialty environments where I compartmentalize apps whose
   dependencies are fragile or out of date. Web based stuff for example,
   where all the dependencies go down like turtles. May need to even
   use old versions of Python.
4. A sandbox for pypy3? Could I create a shared virtual environment with
   pypy3 and python?

## 2022-10-18:

To do a "health check" against your virtual environment,

```fish
   $ pipenv check
```

After editing Pipfile, may need to blow away your virtual environment.

```fish
   $ pipenv --rm
```

I did another `pipenv check` and pipenv created a new virtualenv. Not
sure how it would handle external dependencies since my project had
none. Pipfile does have infrastructure for private Python repos.

See this [blog post](https://medium.com/hackernoon/reaching-python-development-nirvana-bb5692adf30c).

## 2022-10-19:

After pulling scheller-linux-archive repo to gauss17, only problem was
that .python-version was not set to the system Python.

See
[https://github.com/neovim/nvim-lspconfig/issues/717](https://github.com/neovim/nvim-lspconfig/issues/717)
for some insight on how pyenv and pipenv interact.

Seems that pipenv will set `$VIRTUAL_ENV` and prepend the bin directory
of that virtual environment to your `$PATH` before the pyenv shim.

## 2022-10-19:

Will do a Neovim :checkhealth outside and within the pipenv virtual
environment.

**Outside:**

```vim
   ## nvim-lsp-installer report
     - OK: neovim version >= 0.7.0

   ## Python 3 provider (optional)
     - INFO: pyenv: Path: /usr/share/pyenv/libexec/pyenv
     - INFO: pyenv: Root: /home/grs/.pyenv
     - INFO: Using: g:python3_host_prog = "/home/grs/.pyenv/shims/python"
     - INFO: Executable: /home/grs/.pyenv/shims/python
     - INFO: Python version: 3.10.8
     - INFO: pynvim version: 0.4.3
     - OK: Latest pynvim is installed.

   ## Python virtualenv
     - OK: no $VIRTUAL_ENV
```

**Inside:**

```vim
   ## nvim-lsp-installer report
     - OK: neovim version >= 0.7.0

   ## Python 3 provider (optional)
     - INFO: pyenv: Path: /usr/share/pyenv/libexec/pyenv
     - INFO: pyenv: Root: /home/grs/.pyenv
     - INFO: Using: g:python3_host_prog = "/home/grs/.pyenv/shims/python"
     - INFO: Executable: /home/grs/.pyenv/shims/python
     - ERROR: Command error (job=5, exit code 1): `/home/grs/.pyenv/shims/python -c 'import sys; sys.path = [p for p in sys.path if p != ""]; import neovim; print(neovim.__file__)'` (in '/home/grs/devel/scheller-linux-archive/grok/Python/boring_math')
       stderr: Traceback (most recent call last):  File "<string>", line 1, in <module>ModuleNotFoundError: No module named 'neovim'
     - INFO: Python version: 3.10.8
     - INFO: pynvim version: unable to load neovim Python module
     - ERROR: pynvim is not installed.
       Error: unable to load neovim Python module
       - ADVICE:
         - Run in shell: /home/grs/.pyenv/shims/python -m pip install pynvim

   ## Python virtualenv
     - INFO: $VIRTUAL_ENV is set to: /home/grs/.local/share/virtualenvs/boring_math-7KsFA2pk
     - INFO: Python version: 3.10.8
     - OK: $VIRTUAL_ENV provides :!python.
```

Now, looks to be only a little bit broken:

1. Looks like pynvim needs to be installed into the virtualenv. Instead
   of manually using pip, we can install it thru the [dev-packages]
   section of Pipfile.
2. Can we also install pipenv into each virtual environment via the
   Pipefile too? Seems like a chicken and egg situation.
3. Why did pipenv even work with boring_math even when it was not
   installed into the virtual invironment??? I guess it only needs to
   be installed in the the base Python environment, no matter whether
   it is the system Python or a Python installed by pyenv! Pipenv
   does not know anything about pyenv.
4. Maybe I should not define python3_host_prog in my nvim configs?
5. Maybe I should not run pipenv commands within a pipenv shell?

Beginning to understand enough so that this blog is beginning to
makes sense:

* [murawski.blog/pyenv-pipenv.html](http://murawski.blog/pyenv-pipenv.html)

## 2022-10-20:

Once you use pyenv to set up which **VERSION** of Python to use,
**THAT** is the version which pipenv will know about. If python runs
with that version but NOT in a virtual environment, will pyright LSP
know to use the virtual environment? Not clear to me.

```fish
   $ pyenv versions
   * system (set by /home/grs/.pyenv/version)
     3.10.6
     3.10.7

   $ pipenv run python -m pip list
   Package    Version
   ---------- -------
   pip        22.2.1
   setuptools 63.2.0
   wheel      0.37.1
```

Just to be sure, pointing python3_host_prog to the pyenv shim, then run
nvim in the virtual environment.

```lua
   vim.g.python3_host_prog = os.getenv('HOME') .. '/.pyenv/shims/python'
```

Will need to install pynvim into each virtual environment. I am not sure
the pyright LSP server will use the correct virtual environment when
nvim itself is using the base pyenv python environment.

```fish
   $ pipenv install --dev pynvim
   Installing pynvim...
   Adding pynvim to Pipfile\'s [dev-packages]...
   ✔ Installation Succeeded
   Pipfile.lock (e4eef2) out of date, updating to (5d7155)...
   Locking [packages] dependencies...
   Locking [dev-packages] dependencies...
   Building requirements...
   Resolving dependencies...
   ✔ Success!
   Updated Pipfile.lock (5d7155)!
   Installing dependencies\'s from Pipfile.lock (5d7155)...
   To activate this project virtualenv, run pipenv shell.
   Alternatively, run a command inside the virtualenv with pipenv run.

   $ pipenv run python -m pip list
   Package    Version
   ---------- -----------
   greenlet   1.1.3.post0
   msgpack    1.0.4
   pip        22.2.1
   pynvim     0.4.3
   setuptools 63.2.0
   wheel      0.37.1
```

Now, nvim `:checkhealth` gives

```
   ## Python 3 provider (optional)
     - INFO: pyenv: Path: /usr/share/pyenv/libexec/pyenv
     - INFO: pyenv: Root: /home/grs/.pyenv
     - INFO: Using: g:python3_host_prog = "/home/grs/.pyenv/shims/python"
     - INFO: Executable: /home/grs/.pyenv/shims/python
     - INFO: Python version: 3.10.8
     - INFO: pynvim version: 0.4.3
     - OK: Latest pynvim is installed.

   ## Python virtualenv
     - INFO: $VIRTUAL_ENV is set to: /home/grs/.local/share/virtualenvs/boring_math-7KsFA2pk
     - INFO: Python version: 3.10.8
     - OK: $VIRTUAL_ENV provides :!python.
```

and Pipfile contains

```toml
   [[source]]
   url = "https://pypi.org/simple"
   verify_ssl = true
   name = "pypi"

   [packages]

   [dev-packages]
   pynvim = "*"

   [requires]
   python_version = "3.10"
```

## 2022-10-20:

Hopped back to euler7 to see effect of changes from earlier today. After
git pull,

```fish
   $ pipenv update
```

was not pulling in pynvim. I had to do a do a manual

```fish
   $ pipenv install pynvim
```

To get pynvim installed. This triggered the Pipfile.lock to be
regenerated. Later, when trying to reproduce the problem,

```fish
   $ pipenv update
   Running $ pipenv lock then $ pipenv sync.
   Locking [packages] dependencies...
   Building requirements...
   Resolving dependencies...
   ✔ Success!
   Locking [dev-packages] dependencies...
   Building requirements...
   Resolving dependencies...
   ✔ Success!
   Updated Pipfile.lock (16b8f2)!
   Installing dependencies from Pipfile.lock (16b8f2)...
   To activate this project\'s virtualenv, run pipenv shell.
   Alternatively, run a command inside the virtualenv with pipenv run.
   All dependencies are now up-to-date!

   [euler7: ~/devel/scheller-linux-archive/grok/Python/boring_math] (master|✚ =)
   $ pipenv run python -m pip list
   Package    Version
   ---------- -----------
   greenlet   1.1.3.post0
   msgpack    1.0.4
   pip        22.0.3
   pynvim     0.4.3
   setuptools 60.6.0
   wheel      0.37.1
```

But,

```fish
   $ git diff Pipfile | cat
   diff --git a/grok/Python/boring_math/Pipfile b/grok/Python/boring_math/Pipfile
   index 3382f0c..c728d8f 100644
   --- a/grok/Python/boring_math/Pipfile
   +++ b/grok/Python/boring_math/Pipfile
   @@ -4,6 +4,7 @@ verify_ssl = true
    name = "pypi"

    [packages]
   +pynvim = "*"

    [dev-packages]
    pynvim = "*"
```

Opps, my bad. Forgot the --dev

```fish
   $ pipenv uninstall pynvim
   Uninstalling pynvim...
   Found existing installation: pynvim 0.4.3
   Uninstalling pynvim-0.4.3:
     Successfully uninstalled pynvim-0.4.3

   $ pipenv install --dev pynvim
   Installing pynvim...
   Adding pynvim to Pipfile\'s [dev-packages]...
   ✔ Installation Succeeded
   Pipfile.lock (e4eef2) out of date, updating to (5d7155)...
   Locking [packages] dependencies...
   Locking [dev-packages] dependencies...
   Building requirements...
   Resolving dependencies...
   ✔ Success!
   Updated Pipfile.lock (5d7155)!
   Installing dependencies from Pipfile.lock (5d7155)...
   To activate this project\'s virtualenv, run pipenv shell.
   Alternatively, run a command inside the virtualenv with pipenv run.

   $ pipenv run python -m pip list
   Package    Version
   ---------- -----------
   greenlet   1.1.3.post0
   msgpack    1.0.4
   pip        22.0.3
   pynvim     0.4.3
   setuptools 60.6.0
   wheel      0.37.1

   $ git diff Pipfile | cat
```

Let's see if we can reproduce the virtual environment after blowing
it away.

```fish
   $ pipenv --rm
   Removing virtualenv (/home/grs/.local/share/virtualenvs/boring_math-7KsFA2pk)...

   $ cat Pipfile
```

```toml
   [[source]]
   url = "https://pypi.org/simple"
   verify_ssl = true
   name = "pypi"

   [packages]

   [dev-packages]
   pynvim = "*"

   [requires]
   python_version = "3.10"
```

```fish
   $ pipenv install
   Creating a virtualenv for this project...
   Pipfile: /home/grs/devel/scheller-linux-archive/grok/Python/boring_math/Pipfile
   Using /usr/bin/python3 (3.10.8) to create virtualenv...
   ⠧ Creating virtual environment...created virtual environment CPython3.10.8.final.0-64 in 348ms
     creator Venv(dest=/home/grs/.local/share/virtualenvs/boring_math-7KsFA2pk, clear=False, no_vcs_ignore=False, global=False, describe=CPython3Posix)
     seeder FromAppData(download=False, pip=bundle, setuptools=bundle, wheel=bundle, via=copy, app_data_dir=/home/grs/.local/share/virtualenv)
       added seed packages: pip==22.2.1, setuptools==63.2.0, wheel==0.37.1
     activators BashActivator,CShellActivator,FishActivator,NushellActivator,PowerShellActivator,PythonActivator

   ✔ Successfully created virtual environment!
   Virtualenv location: /home/grs/.local/share/virtualenvs/boring_math-7KsFA2pk
   Installing dependencies from Pipfile.lock (5d7155)...
   To activate this project\'s virtualenv, run pipenv shell.
   Alternatively, run a command inside the virtualenv with pipenv run.

   $ pipenv run python -m pip list
   Package    Version
   ---------- -------
   pip        22.2.1
   setuptools 63.2.0
   wheel      0.37.1

   $ pipenv install --dev
   Installing dependencies from Pipfile.lock (5d7155)...
   To activate this project\'s virtualenv, run pipenv shell.
   Alternatively, run a command inside the virtualenv with pipenv run.

   $ pipenv run python -m pip list
   Package    Version
   ---------- -----------
   greenlet   1.1.3.post0
   msgpack    1.0.4
   pip        22.2.1
   pynvim     0.4.3
   setuptools 63.2.0
   wheel      0.37.1

   $ git status
   On branch master
   Your branch is up to date with 'origin/master'.

   nothing to commit, working tree clean
```

Try some Neovim health checks,

First:

```fish
   $ pipenv run nvim bin/pythag3.py
   :healthcheck
```

```vim
   ## Python 3 provider (optional)
     - INFO: pyenv: Path: /usr/share/pyenv/libexec/pyenv
     - INFO: pyenv: Root: /home/grs/.pyenv
     - INFO: Using: g:python3_host_prog = "/home/grs/.pyenv/shims/python"
     - INFO: Executable: /home/grs/.pyenv/shims/python
     - INFO: Python version: 3.10.8
     - INFO: pynvim version: 0.4.3
     - OK: Latest pynvim is installed.

   ## Python virtualenv
     - INFO: $VIRTUAL_ENV is set to: /home/grs/.local/share/virtualenvs/boring_math-7KsFA2pk
     - INFO: Python version: 3.10.8
     - OK: $VIRTUAL_ENV provides :!python.
```

Second:

```fish
   $ nvim bin/pythag3.py
   :healthcheck
```

```vim
   ## Python 3 provider (optional)
     - INFO: pyenv: Path: /usr/share/pyenv/libexec/pyenv
     - INFO: pyenv: Root: /home/grs/.pyenv
     - INFO: Using: g:python3_host_prog = "/home/grs/.pyenv/shims/python"
     - INFO: Executable: /home/grs/.pyenv/shims/python
     - INFO: Python version: 3.10.8
     - INFO: pynvim version: 0.4.3
     - OK: Latest pynvim is installed.

   ## Python virtualenv
     - OK: no $VIRTUAL_ENV
```

Since the pyright LSP language server gets spawned from the nvim
environment, the first approach seems to me "more correct."

Also, run via either
```fish
   $ pipenv run python bin/pythag3.py 20 30
   (3, 4, 5)
   (5, 12, 13)
   (7, 24, 25)
   (8, 15, 17)
   (20, 21, 29)

   $ pipenv run bin/pythag3.py 50 42
   (3, 4, 5)
   (5, 12, 13)
   (7, 24, 25)
   (8, 15, 17)
   (9, 40, 41)
   (12, 35, 37)
   (20, 21, 29)
```

where I am using

```python
   #!/usr/bin/env python
```

as the shebang line.

## 2022-10-20:

Some pyenv factoids:

1. pyenv manages Python environments NOT Python virtual environments
2. pyenv a shell function
   * versions for bash, ksh, zsh, fish
   * pyenv is a shell function that calls /usr/bin/pyenv (written in Bash)
   * it modifies the existing shell instead of creating a new one
   * it calls into a vast Bash shell infrastructure
3. each python environment managed by pyenv needs its own pipenv
4. pyenv shell sub command sets and shows the shell specific Python version
   * "pyenv shell" does not create a new shell, it modifies current one
   * multiple versions of Python can be enabled with it
   * will use this to define python and pypy in same environment
5. "pyenv rehash" sub command needs to be called when Python versions installed

## 2023-08-06:

Pipx: Install and Run Python Applications in Isolated Environments

Pipenv, or managing a virtual environment manually, is a bit overkill
if all you want to do is just install an external Python program. Pipx
is not for installing Python libraries, te packages need entry points.

Pip will step on what Pacman manages.

Pipx can automatically creates a virtual environment, installs the
package, and add a link for it to ~/.local/bin

```fish
   $ pipx install pycowsay
     installed package pycowsay 0.0.0.1, installed using Python 3.11.3
     These apps are now globally available
       - pycowsay
   done!
  
   $ pycowsay I am a cow
  
     ----------
   < I am a cow >
     ----------
      \   ^__^
       \  (oo)\_______
          (__)\       )\/\
              ||----w |
              ||     ||
  
   $ digpath pycowsay
   /home/grs/.local/bin/pycowsay
  
   $ ls -l (digpath pycowsay)
   lrwxrwxrwx 1 grs grs 49 Aug  6 13:54 /home/grs/.local/bin/pycowsay ->
                    /home/grs/.local/pipx/venvs/pycowsay/bin/pycowsay*
```

When developing for pipx, make sure to include an entry_points section
in the setup.py file for the application"

```python
    setup(
        # other arguments here...
        entry_points={
            'console_scripts': [
                'foo = my_package.some_module:main_func',
                'bar = other_module:some_func',
            ],
            'gui_scripts': [
                'baz = my_package_gui:start_func',
            ]
        }
    )
```

## 2023-08-01:

Installed python pytest onto gauss17 & euler7.

```fish
   $ sudo pacman -Syu python-pytest
```

## 2023-08-24:

A lot of the following information, I got from this
[tech post](https://bernat.tech/posts/pep-517-and-python-packaging/).

There are 3 types of Python package distributions:

1. source tree
2. source distribution
3. wheels (no build operations done on the user's computer)

Only the second two types get uploaded to PyPI. The first one requires
you to feed GIT repo information to pip.

The `setup.py` process is old. The distutils package was created in 2000.
You wrote an imperative script to slog through every step of the install
process. At the time all distributions were source distributions. To
install a source distribution pip more or less did the following steps:

1. Discover the package.
2. Download the source distribution and extract the package.
3. Run `python setup.py install`

The package maintainer wrote the setup.py script. The package maintainer used

* $ python setup.py sdist    # to generate the distribution
* $ python setup.py upload   # to upload to a central repository (now deprecated!)

In 2013 the twine tool was invented to upload packages to PyPI.

In 2004 setuptools was created for Python 2.6. This Python utility was
built upon the distutils package. This helped with dependency management
and provided many other additional features like the ability to declare
and automatically install dependencies.

There were some problems:

* Lots of monkey copied setup.py files out in the wild.
* Pip ran the setuptools installed on the user's system, not necessarily
  the one the maintainer used to generate the package.
* Both sutuptools & distutils were handcuffed by backward compatibility.
* How to handle other build tools loke cpython?
* This whole historically driven, backward compatible, process became
  almost impossible to grok.

To address the problem, two PEP\'s created.

* PEP-517: A build system independent format for build trees (2015-2017)
* PEP-518: Specifying Minimum Build System Requirements for Python Projects (2016)

These PEPs allowed the creation of declarative packaging tools tuned
to the package's use case and unshackled from distutils and setuptools
cruft. Though PEP-517 was accepted in 2017, pip took a while to
implement it.

## 2023-08-24:

Moving forward.

I don't need a Python package management system backwardly compatible to
Python 2.4. If you want to use a version of Python before 3.4, God bless
you, use old deprecated packages and toolchains. Pay developers fairly
to update them. But please, don't bother those who want to live in the
middle of the 21st century. Nor insist volunteer open source maintainers
keep packages irrationally backward compatible.

Since I quite like the Python 3.10 Match-Case statement, it is unlikely
I will use any version of Python before 3.10 (unless you pay me well
with lots of money). See PEP-636 for why I am so excited about
structural pattern matching.

Instead of invoking the backend through setup.py, backends are invoked
through modules & functions. All packaging backends must provide an API
that implements two methods "build_wheel" and "build_sdist" at the very
minimum.

Since pip 19.0 (pip currently on 23.2.1 for Python 3.11) pip supports
PEP-517 when a project has a pyproject.toml file in its root directory.

For examples of package configurations for many different backends, see
Bernát Gábor's GitHub profile: https://github.com/gaborbernat.

## 2023-08-24:

Installed python Flit onto gauss17 & euler7.

```fish
    $ sudo pacman -Syu python-flit
```

When using pip, Flit requires Python 3 and therefore needs to be
installed using the Python 3 version of pip. Python 2 modules can be
distributed using Flit, but need to be importable on Python 3 without
errors.

## 2023-08-27:

As I continue moving forward and educating myself on how to maintain
PyPI packages...

Since so far I have only written pure Python packages, I will use the
Flit as my packaging backend for PyPI. I will be following the Flit
documentation located on [PyPA](https://flit.pypa.io) and I will use
Flit's GitHub repo itself as a guide to set up a Flit based
[repo](https://github.com/pypa/flit).

My first python package to push to PyPI will be
`grscheller.datastuctures`. Version numbers 0.X.Y will be used while data
structures are still a part of the `grscheller-python-libs` repo. Will use
1.0.X when I break it out to its own GitHub repo, and 1.1.0 for first
version pushed out tp PyPI.

### Why use Flit? From its PyPa website:

* flit init helps you set up the information Flit needs about your package.
* Sub packages are automatically included
  * you only need to specify the top-level package.
* Data files within a package directory are automatically included.
  * Missing data files have been a common packaging mistake with other tools.
* The version number is taken from your package’s `__version__` attribute.
  * Version numbers match what tools like pip see.
* The `flit publish` command uploads a package to PyPI.
  * You don’t need a separate tool to do this.

### Document the steps/decisions I have taken:

#### 01. Simplfied datastuctures/__init__.py

```python
        """Python datastructures which do not throw exceptions."""

        __version__ = "0.1.0"

        from .dqueue import *
        from .stack import *
```

This will be the only location where `__version__` will be recorded.

#### 02. Copy LICENSE file

From root of the `grscheller-python-libs` repo to the
`grscheller/datastructures/` directory.

#### 03. Put copyright notices at the beginning of stack.py & dqueue.py

```python
    # Copyright 2023 Geoffrey R. Scheller
    #
    # Licensed under the Apache License, Version 2.0 (the "License");
    # you may not use this file except in compliance with the License.
    # You may obtain a copy of the License at
    #
    #     http://www.apache.org/licenses/LICENSE-2.0
    #
    # Unless required by applicable law or agreed to in writing, software
    # distributed under the License is distributed on an "AS IS" BASIS,
    # WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    # See the License for the specific language governing permissions and
    # limitations under the License.
```

#### 04. Initialize rsscheller/datasstructures for use by Flit.

First do

```fish
    $ cd ~/devel/grscheller-python-libs/grscheller/datastructures
    $ flit init
    Module name: grscheller.datastructures
    Try again.
    Module name: datastructures
    Author: Geoffrey R. Scheller
    Author email: geoffrey@scheller.com
    Home page:
    Choose a license (see http://choosealicense.com/ for more info)
    1. MIT - simple and permissive
    2. Apache - explicitly grants patent rights
    3. GPL - ensures that code based on this is shared with the same terms
    4. Skip - choose a license later
    Enter 1-4: 2

    Written pyproject.toml; edit that file to add optional extra info.
```

Which created the following pyproject.toml file:

```toml
   [build-system]
   requires = ["flit_core >=3.2,<4"]
   build-backend = "flit_core.buildapi"

   [project]
   name = "datastructures"
   authors = [{name = "Geoffrey R. Scheller", email = "geoffrey@scheller.com"}]
   license = {file = "LICENSE"}
   classifiers = ["License :: OSI Approved :: Apache Software License"]
   dynamic = ["version", "description"]
```

Based on what the Flit does for its own GitHub repo, I added

```toml
   [project.urls]
   Documentation = "https://github.com/grscheller/datastructures/README.md"
   Source = "https://github.com/grscheller/datastructures"
   Changelog = "https://grscheller/grscheller/datastructures/CHANGELOG.md"
```

## 2023-08-27:

Next steps are to get grscheller registered with PyPI and figure out how
to associate `datastructures` with the name `grscheller.datastructures`.

#### 05. Went to PyPI.org and set up a PyPI account with two-factor authentication.

Using same two-factor authentication service used for DevSecOps at work.

user name: grscheller

When I click "Your Projects" I get:

```
   You have not uploaded any projects to PyPI, yet. To learn how
   to get started, visit the Python Packaging User Guide.

   https://packaging.python.org/
```

#### 06. Create GitHub Repo: https://github.com/grscheller/datastructures

Created on GitHub with just an Apache 2.0 boilerplate license.

```fish
   $ cd ~/devel/grscheller-python-libs-submodules/
   $ mkdir grscheller
   $ cd grscheller
```
Copied over datastructure package from grscheller-python-libs the repo.
Added .pytest_cache/ to .gitignore file.

```fish
   $ set PYTHONPATH /home/grs/devel/grscheller-python-libs-submodules
   $ cd
   $ pytest --pyargs grscheller.datastructures
   ======================================= test session starts ========================================
   platform linux -- Python 3.11.3, pytest-7.4.0, pluggy-1.2.0
   rootdir: /home/grs
   collected 8 items

   devel/grscheller-python-libs-submodules/grscheller/datastructures/tests/test_dqueue.py ...    [ 37%]
   devel/grscheller-python-libs-submodules/grscheller/datastructures/tests/test_stack.py .....   [100%]

   ======================================== 8 passed in 0.02s =========================================
   $ cd -
   $ pwd
   /home/grs/devel/grscheller-python-libs-submodules/grscheller/datastructures
```

Committed changes to datastructures repo and pushed up to GitHub.

#### 07. Created a Git submodule for this effort

Remove datastructures from grscheller-python-libs and add back as a GIT
submodule. Updated PYTHONPATH in fish to

```fish
   $ set PYTHONPATH /home/grs/devel/grscheller-python-libs-submodules /home/grs/devel/grscheller-python-libs
```

Might want a write a shell script to juggle $PYTHONPATH

Removed datastructures from grscheller-python-libs repo and pushed
change to GitHub.

```
   TODO: Consider just pushing boring_math and func_tools to their own GIT
         repos and doing away with grscheller-python-libs???
```

Hold off adding datastructures back as a submodule. Also will probably
just drop `func_tools` and move `boring_math` to their own repos.

## 2023-08-29:

Abandoned idea of submodules. Will push boring-math to its own repo.
Will drop func_tools in lieu of just using itertools and functools from
the Python standard libraries.

#### 08. Before pushing grscheller.datastructures to PyPI

First set things up to install the package with pip from GitHub.

Create a git tag for the 0.2.0.0 version on datastructures:

```fish
   $ git tag -a v0.2.0.0 -m "First version grscheller.datastructures released on GitHub"
   $ git push origin v0.2.0.0
```

Source code for this tag is now downloadable from GitHub web frontend in
either zip or tar.gz format.

Not sure how pip will handle the grscheller part of the package name.
Will I need to put a grscheller directory in the repo for this to work?
Other packages just use an unqualified package name to have pip install
it, see

* https://github.com/NiklasTiede/TinyHTTPie
* https://the-coding-lab.com/2021/8-publishing-at-pypi/

for a setuptools/twine example which only has pip use an unqualified
name. I doubt "datastructures" would be available and wouldn't want to
use such a generic name without namespacing it. If only unqualified
names can be used, I'd have to come up with a unique name expressing
its purpose. Maybe non-burping-red-drangon-datastructures?

No sense worrying until it fails.

Before continuing with step 09, try installing package with pip via GitHub.

First see if pip is working correctly.

```fish
      $ set PIP_REQUIRE_VIRTUALENV false
      $ pip install pip-install-test
      error: externally-managed-environment

      × This environment is externally managed
      ╰─> To install Python packages system-wide, try 'pacman -S
          python-xyz', where xyz is the package you are trying to
          install.

          If you wish to install a non-Arch-packaged Python package,
          create a virtual environment using 'python -m venv path/to/venv'.
          Then use path/to/venv/bin/python and path/to/venv/bin/pip.

          If you wish to install a non-Arch packaged Python application,
          it may be easiest to use 'pipx install xyz', which will manage a
          virtual environment for you. Make sure you have python-pipx
          installed via pacman.

      note: If you believe this is a mistake, please contact your Python installation or OS distribution provider. You can override this, at the risk of breaking your Python installation or OS, by passing --break-system-packages.
      hint: See PEP 668 for the detailed specification.
```

OK, Arch does not want me to molest its Python configuration outside of
Pacman. I'll just continue using PYTHONPATH when using datastructures
with the system Python. To test pip, I'll need to create a virtual
environment.

```fish
    $ set PIP_REQUIRE_VIRTUALENV true
    $ pip install pip-install-test
    ERROR: Could not find an activated virtualenv (required).
```

Should I just use venv or pyenv? Well, might have well leverage my
investment in pyenv and reacquaint myself with it.

On euler7,

```fish
   $ pyenv versions
   * system (set by /home/grs/.local/share/pyenv/version)
     3.8.12
     3.9.7
     3.10.2

   $ pyenv uninstall 3.8.12
   $ pyenv uninstall 3.8.12 3.9.7 3.10.2
   pyenv: remove /home/grs/.local/share/pyenv/versions/3.8.12? [y|N] y
   pyenv: 3.8.12 uninstalled
   pyenv: remove /home/grs/.local/share/pyenv/versions/3.9.7? [y|N] y
   pyenv: 3.9.7 uninstalled
   pyenv: remove /home/grs/.local/share/pyenv/versions/3.10.2? [y|N] y
   pyenv: 3.10.2 uninstalled

   $ pyenv versions
   * system (set by /home/grs/.local/share/pyenv/version)
```

These versions were for web frameworks which force web-kiddies to use
old versions of Python. At least they were not too crufty.

I'll install the same version of Python Arch uses.

```fish
   $ python --version
   Python 3.11.5

   $ pyenv install 3.11.5
   Downloading Python-3.11.5.tar.xz...
   -> https://www.python.org/ftp/python/3.11.5/Python-3.11.5.tar.xz
   Installing Python-3.11.5...

   BUILD FAILED (Arch Linux using python-build 20180424)

   Inspect or clean up the working tree at /tmp/python-build.20230829131800.93756
   Results logged to /tmp/python-build.20230829131800.93756.log

   Last 10 log lines:
   Traceback (most recent call last):
     File "<frozen runpy>", line 189, in _run_module_as_main
     File "<frozen runpy>", line 148, in _get_module_details
     File "<frozen runpy>", line 112, in _get_module_details
     File "/tmp/python-build.20230829131800.93756/Python-3.11.5/Lib/ensurepip/__init__.py", line 4, in <module>
       import subprocess
     File "/tmp/python-build.20230829131800.93756/Python-3.11.5/Lib/subprocess.py", line 104, in <module>
       from _posixsubprocess import fork_exec as _fork_exec
   ModuleNotFoundError: No module named '_posixsubprocess'
   make: *** [Makefile:1880: install] Error 1
```

* same error when I try installing Python 3.10.13
* did work for Python 3.8.18
* fails for Python 3.9.0, different error
* fails for 3.10.1. Not interested in using any version before this one

Seems that pyenv moved where it stores its stuff

```
   ~/.pyenv/shims -> /home/grs/.local/share/pyenv/shims
```

OK, wasted enough time with pyenv. I'll just use the system python and
a virtual environment.

Create a Python virtual environment with system python.

```fish
      $ cd ~/devel
      $ python -m venv venv_system
      $ ls venv_system/
      bin/  include/  lib/  lib64@  pyvenv.cfg
      $ ls venv_system/bin
      $ ls venv_system/bin/
      activate      activate.fish  pip*   pip3.11*  python3@
      activate.csh  Activate.ps1   pip3*  python@   python3.11@
```

Now lets activate it.

```fish
      $ source venv_system/bin/activate.fish
      (venv_system)$
```

Foe clarity, I will suppress the (venv_system) part of the prompt below.

```fish
      $ string join \n $PATH
      /home/grs/devel/venv_system/bin
      /home/grs/.local/share/pyenv/shims
      /usr/lib/jvm/java-17-openjdk/bin
      ...

      $ file venv_system/bin/python
      venv_system/bin/python: symbolic link to /usr/bin/python

      $ python --version
      Python 3.11.5
```

Now test to see if pip now works.

```fish
      $ pip install pip-install-test
      Collecting pip-install-test
        Downloading pip_install_test-0.5-py3-none-any.whl (1.7 kB)
      Installing collected packages: pip-install-test
      Successfully installed pip-install-test-0.5

      $ python
      Python 3.11.5 (main, Aug 28 2023, 20:02:58) [GCC 13.2.1 20230801] on linux
      Type "help", "copyright", "credits" or "license" for more information.
      >>> import pip_install_test
      Good job!  You installed a pip module.

      Now get back to work!
      >>> exit()

      $ pip uninstall pip-install-test
      ...

      $ pypi
      Set $PYTHONPATH to /home/grs/devel/pypi:/home/grs/devel/grscheller-python-libs
      $ pytest --pyargs grscheller.datastructures
      ================================== test session starts ===================================
      platform linux -- Python 3.11.5, pytest-7.4.0, pluggy-1.2.0
      rootdir: /home/grs/devel/junk
      collected 8 items

      tests/test_dqueue.py ...                                                           [ 37%]
      tests/test_stack.py .....                                                          [100%]

      =================================== 8 passed in 0.01s ====================================
      $ pypi
      Removed $PYTHONPATH

      $ pytest --pyargs grscheller.datastructures
      ================================== test session starts ===================================
      platform linux -- Python 3.11.5, pytest-7.4.0, pluggy-1.2.0
      rootdir: /home/grs/devel/junk
      collected 0 items

      ================================= no tests ran in 0.00s ==================================
      ERROR: module or package not found: grscheller.datastructures (missing __init__.py?
```

Good, ready to see how well grscheller.datafiles installs from its GitHub repo.

#### 09. Install from GitHub

Try installing datastructures package fromm its GitHub repo into
a Python Virtual environment.

```fish
   $ pip install git+https://github.com/grscheller/datastructures@v0.2.0.0
   Collecting git+https://github.com/grscheller/datastructures@v0.2.0.0
     Cloning https://github.com/grscheller/datastructures (to revision v0.2.0.0) to /tmp/pip-req-build-l1y94lvt
     Running command git clone --filter=blob:none --quiet https://github.com/grscheller/datastructures /tmp/pip-req-build-l1y94lvt
     Running command git checkout -q 3a6bd777768ee515855cd15b4f5dc6e3f454c512
     Resolved https://github.com/grscheller/datastructures to commit 3a6bd777768ee515855cd15b4f5dc6e3f454c512
     Installing build dependencies ... done
     Getting requirements to build wheel ... error
     error: subprocess-exited-with-error

     × Getting requirements to build wheel did not run successfully.
     │ exit code: 1
     ╰─> [15 lines of output]
         Traceback (most recent call last):
           File "/home/grs/devel/venv_system/lib/python3.11/site-packages/pip/_vendor/pyproject_hooks/_in_process/_in_process.py", line 353, in <module>
             main()
           File "/home/grs/devel/venv_system/lib/python3.11/site-packages/pip/_vendor/pyproject_hooks/_in_process/_in_process.py", line 335, in main
             json_out['return_val'] = hook(**hook_input['kwargs'])
                                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
           File "/home/grs/devel/venv_system/lib/python3.11/site-packages/pip/_vendor/pyproject_hooks/_in_process/_in_process.py", line 118, in get_requires_for_build_wheel
             return hook(config_settings)
                    ^^^^^^^^^^^^^^^^^^^^^
           File "/tmp/pip-build-env-0huuhuq1/overlay/lib/python3.11/site-packages/flit_core/buildapi.py", line 31, in get_requires_for_build_wheel
             module = Module(info.module, Path.cwd())
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
           File "/tmp/pip-build-env-0huuhuq1/overlay/lib/python3.11/site-packages/flit_core/common.py", line 59, in __init__
             raise ValueError("No file/folder found for module {}".format(name))
         ValueError: No file/folder found for module datastructures
         [end of output]

     note: This error originates from a subprocess, and is likely not a problem with pip.
   error: subprocess-exited-with-error

   × Getting requirements to build wheel did not run successfully.
   │ exit code: 1
   ╰─> See above for output.

   note: This error originates from a subprocess, and is likely not a problem with pip.
```

Progress! Failed getting requirements to build the "wheel", but a sort
of expected it to. Says more configuration is needed in order to build
the "wheel." Didn't they use to call something like this an egg?

It caught on that flit was used as the build tool. Need to figure out if
I need to do something with FLIT or with the package to configure it so
that this wheel thingy can be built.

So far, clone the repo it works. Pip clones the repo and knows the FLIT
is the backend, but something (FLIT? pip?) does not know how to get the
requirements to build the wheel.

## 2023-08-30:

Can use multilevel package names like geoffrey.datastructures, so long as the
outer one is an empty directory.
See [this Flit issue](https://github.com/pypa/flit/issues/370) from 2020
that prompted the dev to add the feature.

```
  Note: This enhancement only tries to support PEP 420 'implicit' namespace
        packages, where the namespace package (the outer one) doesn't have
        an __init__.py file. This choice totally makes sense, grscheller
        by itself is not a package, just a "pseudo" namespace.

        The package name is `grscheller.datastructures` with an actual
        period in the name. Don't confuded the PyPI package name with
        Python module names. (future me 2024-04-19)
```

Was able to install from GitHub with pip, From the virtual environment.

#### 10. Try installing datastructures package from its GitHub repo into a Python

```fish
   $ pip install git+https://github.com/grscheller/datastructures@v0.2.0.2
   Collecting git+https://github.com/grscheller/datastructures@v0.2.0.2
     Cloning https://github.com/grscheller/datastructures (to revision v0.2.0.2) to /tmp/pip-req-build-h27n05qo
     Running command git clone --filter=blob:none --quiet https://github.com/grscheller/datastructures /tmp/pip-req-build-h27n05qo
     Running command git checkout -q 38a74ca88ce2874c79b0f8242b2dcd5d0af30386
     Resolved https://github.com/grscheller/datastructures to commit 38a74ca88ce2874c79b0f8242b2dcd5d0af30386
     Installing build dependencies ... done
     Getting requirements to build wheel ... done
     Preparing metadata (pyproject.toml) ... done
   Building wheels for collected packages: grscheller.datastructures
     Building wheel for grscheller.datastructures (pyproject.toml) ... done
     Created wheel for grscheller.datastructures: filename=grscheller_datastructures-0.2.0.2-py2.py3-none-any.whl size=11091 sha256=3b554425975a9987a2f8dd19e23e7a0ac7cc6cdf6348bf094e2a19d96399897f
     Stored in directory: /tmp/pip-ephem-wheel-cache-hutqkexz/wheels/48/b4/cc/040205d6a165dbbb6539f14da4e8fb4561644adcfc8020b188
   Successfully built grscheller.datastructures
   Installing collected packages: grscheller.datastructures
   Successfully installed grscheller.datastructures-0.2.0.2
```

Let's see if it really worked.

```fish
   $ ipython
   /usr/lib/python3.11/site-packages/IPython/core/interactiveshell.py:889: UserWarning:
      Attempting to work in a virtualenv. If you encounter problems, please install IPython inside the virtualenv.
```

```python
   Python 3.11.5 (main, Aug 28 2023, 20:02:58) [GCC 13.2.1 20230801]
   Type 'copyright', 'credits' or 'license' for more information
   IPython 8.14.0 -- An enhanced Interactive Python. Type '?' for help.

   In [1]: from grscheller.datastructures import *

   In [2]: foo = Stack()

   In [3]: foo.push(1).push(2).push(3)
   Out[3]: [ 3 -> 2 -> 1 -> None ]

   In [4]: foo.pop()
   Out[4]: 3

   In [5]: foo.pop()
   Out[5]: 2

   In [6]:
   Do you really want to exit ([y]/n)?
```

```fish
   $ echo $PYTHONPATH

   $ pytest --pyargs grscheller.datastructures
   ================================== test session starts ===================================
   platform linux -- Python 3.11.5, pytest-7.4.0, pluggy-1.2.0
   rootdir: /home/grs/devel
   collected 0 items

   ================================= no tests ran in 0.00s ==================================
   ERROR: module or package not found: grscheller.datastructures (missing __init__.py?)
```

So, grscheller.datastructures installed into virtual environment and
works but test suite fails to rum.

```fish
    $ pip list
    Package                   Version
    ------------------------- -------
    grscheller.datastructures 0.2.0.2
    pip                       23.2.1
    setuptools                65.5.0
    (venv_system)
    [grs@euler7: ~/devel]
    $ digpath pip
    /home/grs/devel/venv_system/bin/pip
    /home/grs/.local/share/pyenv/shims/pip
    /usr/bin/pip
  ```

From what I've seen online, it would be possible to get the test suite
to work, but the examples I found required some setup.py hacking.

Also, does it make sense providing end users access to test suites?
I will take the lazy way out and only provide access to test suites from
the cloned development environment.

## 2023-09-02:

Updated pyproject.toml for grscheller.datastructures

```toml
    [build-system]
    requires = ["flit_core>=3.4,<4"]
    build-backend = "flit_core.buildapi"

    [project]
    name = "grscheller.datastructures"
    authors = [
        { name = "Geoffrey R. Scheller", email = "geoffrey@scheller.com" },
    ]
    license = { file = "LICENSE" }
    readme = "README.md"
    requires-python = ">=3.10"
    classifiers = [
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: Apache Software License",
        "Operating System :: OS Independent",
        "Development Status :: 3 - Alpha"
    ]
    dependencies = ["pytest >=7.4"]
    dynamic = ["version", "description"]

    [project.urls]
    Documentation = "https://github.com/grscheller/datastructures/README.md"
    Source = "https://github.com/grscheller/datastructures"
    Changelog = "https://github.com/grscheller/datastructures/CHANGELOG.md"
```

Removed grscheller.datastructures, pytest and pytest dependencies. Then ran

```fish
    (venv_system)$ pip list|cat
    Package    Version
    ---------- -------
    pip        23.2.1
    setuptools 65.5.0

    (venv_system)$ pip install git+https://github.com/grscheller/datastructures@v0.2.1.0
    Collecting git+https://github.com/grscheller/datastructures@v0.2.1.0
      Cloning https://github.com/grscheller/datastructures (to revision v0.2.1.0) to /tmp/pip-req-build-u1eid79a
      Running command git clone --filter=blob:none --quiet https://github.com/grscheller/datastructures /tmp/pip-req-build-u1eid79a
      Running command git checkout -q 9ed95cd8a78083c28fd5210656566a0c8906605e
      Resolved https://github.com/grscheller/datastructures to commit 9ed95cd8a78083c28fd5210656566a0c8906605e
      Installing build dependencies ... done
      Getting requirements to build wheel ... done
      Preparing metadata (pyproject.toml) ... done
    Collecting pytest>=7.4 (from grscheller.datastructures==0.2.1.0)
      Obtaining dependency information for pytest>=7.4 from https://files.pythonhosted.org/packages/78/af/1a79db43409ea8569a8a91d0a87db4445c7de4cefcf6141e9a5c77dda2d6/pytest-7.4.1-py3-none-any.whl.metadata
      Using cached pytest-7.4.1-py3-none-any.whl.metadata (7.9 kB)
    Collecting iniconfig (from pytest>=7.4->grscheller.datastructures==0.2.1.0)
      Using cached iniconfig-2.0.0-py3-none-any.whl (5.9 kB)
    Collecting packaging (from pytest>=7.4->grscheller.datastructures==0.2.1.0)
      Using cached packaging-23.1-py3-none-any.whl (48 kB)
    Collecting pluggy<2.0,>=0.12 (from pytest>=7.4->grscheller.datastructures==0.2.1.0)
      Obtaining dependency information for pluggy<2.0,>=0.12 from https://files.pythonhosted.org/packages/05/b8/42ed91898d4784546c5f06c60506400548db3f7a4b3fb441cba4e5c17952/pluggy-1.3.0-py3-none-any.whl.metadata
      Using cached pluggy-1.3.0-py3-none-any.whl.metadata (4.3 kB)
    Using cached pytest-7.4.1-py3-none-any.whl (324 kB)
    Using cached pluggy-1.3.0-py3-none-any.whl (18 kB)
    Building wheels for collected packages: grscheller.datastructures
      Building wheel for grscheller.datastructures (pyproject.toml) ... done
      Created wheel for grscheller.datastructures: filename=grscheller_datastructures-0.2.1.0-py3-none-any.whl size=12306 sha256=eac39b6fc58babe8869a9bee9e9780a9ec61d8faa5b38b4636bf0a565d5ff2f6
      Stored in directory: /tmp/pip-ephem-wheel-cache-i2i2sw_k/wheels/9e/66/bb/00a48db1ab49744500cf3577804f7b7992f140c89068813740
    Successfully built grscheller.datastructures
    Installing collected packages: pluggy, packaging, iniconfig, pytest, grscheller.datastructures
    Successfully installed grscheller.datastructures-0.2.1.0 iniconfig-2.0.0 packaging-23.1 pluggy-1.3.0 pytest-7.4.1

    (venv_system)$ pip list|cat
    Package                   Version
    ------------------------- -------
    grscheller.datastructures 0.2.1.0
    iniconfig                 2.0.0
    packaging                 23.1
    pip                       23.2.1
    pluggy                    1.3.0
    pytest                    7.4.1
    setuptools                65.5.0

    (venv_system)$ pytest --pyargs grscheller.datastructures
    ============================================== test session starts ==============================================
    platform linux -- Python 3.11.5, pytest-7.4.1, pluggy-1.3.0
    rootdir: /home/grs/devel/venv_system
    collected 8 items

    lib/python3.11/site-packages/grscheller/datastructures/tests/test_dqueue.py ...                                                                                       [ 37%]
    lib/python3.11/site-packages/grscheller/datastructures/tests/test_stack.py .....                                                                                      [100%]

    =============================================== 8 passed in 0.04s ===============================================
```

## 2023-09-02:

  Attempts to upload to PyPI with token are failing.

#### 11. Upload grscheller.datastructures to PyPI using tokens

```fish
   $ FLIT_USERNAME=__token__ FLIT_PASSWORD=007834 flit publish
   Found 11 files tracked in git                                             I-flit.sdist
   Built sdist: dist/grscheller_datastructures-0.2.1.0.tar.gz           I-flit_core.sdist
   Copying package file(s) from /tmp/tmpddfkxgtv/grscheller_datastructures-0.2.1.0/grscheller/datastructures  I-flit_core.wheel
   Writing metadata files                                               I-flit_core.wheel
   Writing the record of files                                          I-flit_core.wheel
   Built wheel: dist/grscheller_datastructures-0.2.1.0-py3-none-any.whl I-flit_core.wheel
   Using repository at https://upload.pypi.org/legacy/                      I-flit.upload
   Uploading dist/grscheller_datastructures-0.2.1.0-py3-none-any.whl...     I-flit.upload
   Traceback (most recent call last):
     File "/usr/bin/flit", line 8, in <module>
       sys.exit(main())
                ^^^^^^
     File "/usr/lib/python3.11/site-packages/flit/__init__.py", line 200, in main
       main(args.ini_file, repository, args.pypirc, formats=set(args.format or []),
     File "/usr/lib/python3.11/site-packages/flit/upload.py", line 277, in main
       do_upload(built.wheel.file, built.wheel.builder.metadata, pypirc_path, repo_name)
     File "/usr/lib/python3.11/site-packages/flit/upload.py", line 252, in do_upload
       upload_file(file, metadata, repo)
     File "/usr/lib/python3.11/site-packages/flit/upload.py", line 245, in upload_file
       resp.raise_for_status()
     File "/usr/lib/python3.11/site-packages/requests/models.py", line 1021, in raise_for_status
       raise HTTPError(http_error_msg, response=self)
   requests.exceptions.HTTPError: 403 Client Error: Invalid or non-existent authentication information. See https://pypi.org/help/#invalid-auth for more information. for url: https://upload.pypi.org/legacy/
```

#### 12. Upload grscheller.datastructures to PyPI using username and password.

```fish
   $ FLIT_USERNAME=grscheller FLIT_PASSWORD='letmein123' flit publish
   Found 11 files tracked in git                                             I-flit.sdist
   Built sdist: dist/grscheller_datastructures-0.2.1.0.tar.gz           I-flit_core.sdist
   Copying package file(s) from /tmp/tmpxyua6ys6/grscheller_datastructures-0.2.1.0/grscheller/datastructures  I-flit_core.wheel
   Writing metadata files                                               I-flit_core.wheel
   Writing the record of files                                          I-flit_core.wheel
   Built wheel: dist/grscheller_datastructures-0.2.1.0-py3-none-any.whl I-flit_core.wheel
   Using repository at https://upload.pypi.org/legacy/                      I-flit.upload
   Uploading dist/grscheller_datastructures-0.2.1.0-py3-none-any.whl...     I-flit.upload
   Traceback (most recent call last):
     File "/usr/bin/flit", line 8, in <module>
       sys.exit(main())
                ^^^^^^
     File "/usr/lib/python3.11/site-packages/flit/__init__.py", line 200, in main
       main(args.ini_file, repository, args.pypirc, formats=set(args.format or []),
     File "/usr/lib/python3.11/site-packages/flit/upload.py", line 277, in main
       do_upload(built.wheel.file, built.wheel.builder.metadata, pypirc_path, repo_name)
     File "/usr/lib/python3.11/site-packages/flit/upload.py", line 252, in do_upload
       upload_file(file, metadata, repo)
     File "/usr/lib/python3.11/site-packages/flit/upload.py", line 245, in upload_file
       resp.raise_for_status()
     File "/usr/lib/python3.11/site-packages/requests/models.py", line 1021, in raise_for_status
       raise HTTPError(http_error_msg, response=self)
   requests.exceptions.HTTPError: 401 Client Error: User grscheller has two factor auth enabled, an API Token or Trusted Publisher must be used to upload in place of password. for url: https://upload.pypi.org/legacy/
```

Well, ... at least I have a valid account.

I am able to log into https://pypi.org/manage/account/ which requires my
username, password, and my Google Authenticator token. So authentication does
work with Google Authenticator when logging into PyPI.

One thing is bothering me, when I tell FLIT to use __token__ as the username
and the current token value as the password, what is "Two Factor" about that?

From https://flit.pypa.io/en/stable/upload.html there seems to be
a configuration file called ~/.pypirc which flit and twine will create if it
does not exist.

```fish
   $ cat ~/.pypirc
   [pypi]
   username = __token__
```

This does not look like the example given in the PyPI upload.html document above.

```toml
   [distutils]
   index-servers =
      pypi
      testpypi

   [pypi]
   repository = https://upload.pypi.org/legacy/
   username = sirrobin   # Replace with your PyPI username

   [testpypi]
   repository = https://test.pypi.org/legacy/
   username = sirrobin   # Replace with your TestPyPI username
```

Let's fix this

```toml
   [distutils]
   index-servers =
      pypi
      testpypi

   [pypi]
   repository = https://upload.pypi.org/legacy/
   username = grscheller

   [testpypi]
   repository = https://test.pypi.org/legacy/
   username = grscheller
```

and set

```fish
   $ chmod 600 ~/.pypirc
```

Let's try again

```fish
   $ FLIT_USERNAME='__token__' FLIT_PASSWORD=441880 flit publish
   Found 11 files tracked in git                                             I-flit.sdist
   Built sdist: dist/grscheller_datastructures-0.2.1.0.tar.gz           I-flit_core.sdist
   Copying package file(s) from /tmp/tmpi7m73i9s/grscheller_datastructures-0.2.1.0/grscheller/datastructures  I-flit_core.wheel
   Writing metadata files                                               I-flit_core.wheel
   Writing the record of files                                          I-flit_core.wheel
   Built wheel: dist/grscheller_datastructures-0.2.1.0-py3-none-any.whl I-flit_core.wheel
   Using repository at https://upload.pypi.org/legacy/                      I-flit.upload
   Uploading dist/grscheller_datastructures-0.2.1.0-py3-none-any.whl...     I-flit.upload
   Traceback (most recent call last):
     File "/usr/bin/flit", line 8, in <module>
       sys.exit(main())
                ^^^^^^
     File "/usr/lib/python3.11/site-packages/flit/__init__.py", line 200, in main
       main(args.ini_file, repository, args.pypirc, formats=set(args.format or []),
     File "/usr/lib/python3.11/site-packages/flit/upload.py", line 277, in main
       do_upload(built.wheel.file, built.wheel.builder.metadata, pypirc_path, repo_name)
     File "/usr/lib/python3.11/site-packages/flit/upload.py", line 252, in do_upload
       upload_file(file, metadata, repo)
     File "/usr/lib/python3.11/site-packages/flit/upload.py", line 245, in upload_file
       resp.raise_for_status()
     File "/usr/lib/python3.11/site-packages/requests/models.py", line 1021, in raise_for_status
       raise HTTPError(http_error_msg, response=self)
   requests.exceptions.HTTPError: 403 Client Error: Invalid or non-existent authentication information. See https://pypi.org/help/#invalid-auth for more information. for url: https://upload.pypi.org/legacy/
```

Let's install and use twine

```fish
   $ python -m twine upload --repository pypi dist/*
   Uploading distributions to https://upload.pypi.org/legacy/
   WARNING  Error getting password from keyring
            Traceback (most recent call last):
              File "/usr/lib/python3.11/site-packages/twine/auth.py", line 74, in
            get_password_from_keyring
                return cast(str, keyring.get_password(system, username))
                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
              File "/usr/lib/python3.11/site-packages/keyring/core.py", line 56, in
            get_password
                return get_keyring().get_password(service_name, username)
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
              File "/usr/lib/python3.11/site-packages/keyring/backends/fail.py", line 28,
            in get_password
                raise NoKeyringError(msg)
            keyring.errors.NoKeyringError: No recommended backend was available. Install
            a recommended 3rd party backend package; or, install the keyrings.alt package
            if you want to use the non-recommended backends. See
            https://pypi.org/project/keyring for details.
   Enter your password:
   Uploading grscheller_datastructures-0.2.1.0-py3-none-any.whl
   100% ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 17.9/17.9 kB • 00:00 • 19.6 MB/s
   WARNING  Error during upload. Retry with the --verbose option for more details.
   ERROR    HTTPError: 403 Forbidden from https://upload.pypi.org/legacy/
            Invalid or non-existent authentication information. See
            https://pypi.org/help/#invalid-auth for more information.
```

Similar problem. Let's try without environment variables. First edit ~/.pypirc

```toml
    [distutils]
    index-servers = pypi

    [pypi]
    repository = https://upload.pypi.org/legacy/
    username = __token__
```

Try again

```fish
   $ flit publish
   Found 11 files tracked in git                                                                                                                                    I-flit.sdist
   Built sdist: dist/grscheller_datastructures-0.2.1.0.tar.gz                                                                                                  I-flit_core.sdist
   Copying package file(s) from /tmp/tmpcmjp5neo/grscheller_datastructures-0.2.1.0/grscheller/datastructures                                                   I-flit_core.wheel
   Writing metadata files                                                                                                                                      I-flit_core.wheel
   Writing the record of files                                                                                                                                 I-flit_core.wheel
   Built wheel: dist/grscheller_datastructures-0.2.1.0-py3-none-any.whl                                                                                        I-flit_core.wheel
   Using repository at https://upload.pypi.org/legacy/                                                                                                             I-flit.upload
   Could not get password from keyring (No recommended backend was available.
                                        Install a recommended 3rd party backend package; or,
                                        install the keyrings.alt package if you want to use the non-recommended backends.
                                        See https://pypi.org/project/keyring for details.)  W-flit.upload
   Server  : https://upload.pypi.org/legacy/
   Username: __token__
   Password:
   Could not store password in keyring (No recommended backend was available.
      Install a recommended 3rd party backend package; or,
      install the keyrings.alt package if you want to use the non-recommended backends.
      See https://pypi.org/project/keyring for details.)  W-flit.upload
   Uploading dist/grscheller_datastructures-0.2.1.0-py3-none-any.whl...                                                                                            I-flit.upload
   Traceback (most recent call last):
     File "/usr/bin/flit", line 8, in <module>
       sys.exit(main())
                ^^^^^^
     File "/usr/lib/python3.11/site-packages/flit/__init__.py", line 200, in main
       main(args.ini_file, repository, args.pypirc, formats=set(args.format or []),
     File "/usr/lib/python3.11/site-packages/flit/upload.py", line 277, in main
       do_upload(built.wheel.file, built.wheel.builder.metadata, pypirc_path, repo_name)
     File "/usr/lib/python3.11/site-packages/flit/upload.py", line 252, in do_upload
       upload_file(file, metadata, repo)
     File "/usr/lib/python3.11/site-packages/flit/upload.py", line 245, in upload_file
       resp.raise_for_status()
     File "/usr/lib/python3.11/site-packages/requests/models.py", line 1021, in raise_for_status
       raise HTTPError(http_error_msg, response=self)
   requests.exceptions.HTTPError: 403 Client Error: Invalid or non-existent authentication information.
      See https://pypi.org/help/#invalid-auth for more information. for url: https://upload.pypi.org/legacy/
```

    See something:

```
   Could not store password in keyring (No recommended backend was available.
   Install a recommended 3rd party backend package; or, install the keyrings.alt
   package if you want to use the non-recommended backends.
   See https://pypi.org/project/keyring for details.)  W-flit.upload
```

I wish I better understood keyring infrastructure, at least it is not as bad as pam.

```fish
   $ pacman -Ss '^python.*keyring'
   extra/python-keyring 24.2.0-1 [installed]
       Store and access your passwords safely
       :q
   extra/python-keyrings-alt 1:5.0.0-1
       Alternate keyring implementations
```

## 2023-09-03:

Tried the alternate keyring, same result. Went back to default.

I see a variety of examples out there in the wild. One of which
I haven't tried is

```toml
    [pypi]
      username = __token__
      password = pypi-XcvB...aLd6
```

What the hell is this "pypi-" thingy???

Did I miss something important when I set up access to the PyPI repo?
Would explain the seemingly lack of two factor authentication.

See: [https://pypi.org/help/#apitoken](https://pypi.org/help/#apitoken)

I seem to have not recorded the API token I generated.

Redid the entire two factor authentication process. At no time was
I given any sort of number that I could append pypi- to. Well now Google
Authenticator has two timed codes associated with PyPI, but only the
second one now works to log me into PyPI.

There something called API tokens. I may have previously confused these
with physical keys like Yubi-keys. Let's try generating one. From PyPI
account management page: https://pypi.org/manage/account/

* click "Add API token"
* token created
* copied to clip board (starts with pypi-)
* added as password to ~/.pypirc

Let's try uploading it via flit

```fish
   $ flit publish
   Found 11 files tracked in git                                             I-flit.sdist
   Built sdist: dist/grscheller_datastructures-0.2.1.0.tar.gz           I-flit_core.sdist
   Copying package file(s) from /tmp/tmpio0bgscd/grscheller_datastructures-0.2.1.0/grscheller/datastructures  I-flit_core.wheel
   Writing metadata files                                               I-flit_core.wheel
   Writing the record of files                                          I-flit_core.wheel
   Built wheel: dist/grscheller_datastructures-0.2.1.0-py3-none-any.whl I-flit_core.wheel
   Using repository at https://upload.pypi.org/legacy/                      I-flit.upload
   Uploading dist/grscheller_datastructures-0.2.1.0-py3-none-any.whl...     I-flit.upload
   Package is at https://pypi.org/project/grscheller.datastructures/        I-flit.upload
   Using repository at https://upload.pypi.org/legacy/                      I-flit.upload
   Uploading dist/grscheller_datastructures-0.2.1.0.tar.gz...               I-flit.upload
   Package is at https://pypi.org/project/grscheller.datastructures/        I-flit.upload
```

Curiously, did not ask for a 6 digit authentication token. Not sure
where the two factor authentication is fitting in.

Any case, publishing to PyPI worked. I forgot to update the version
number to 1.0.0.0.

Lets see if I can install it from PyPI.

```fish
   $ pip uninstall grscheller.datastructures iniconfig packaging pluggy pytest

   $ pip list
   Package    Version
   ---------- -------
   pip        23.2.1
   setuptools 65.5.0

   $ pip install grscheller.datastructures
   Collecting grscheller.datastructures
     Obtaining dependency information for grscheller.datastructures from https://files.pythonhosted.org/packages/02/40/fc952fb2b4e2a73f7ccd33f3cb2c9ed5b2f0f48d6e347d23c7e0214a7da1/grscheller_datastructures-0.2.1.0-py3-none-any.whl.metadata
     Downloading grscheller_datastructures-0.2.1.0-py3-none-any.whl.metadata (2.5 kB)
   Collecting pytest>=7.4 (from grscheller.datastructures)
     Obtaining dependency information for pytest>=7.4 from https://files.pythonhosted.org/packages/78/af/1a79db43409ea8569a8a91d0a87db4445c7de4cefcf6141e9a5c77dda2d6/pytest-7.4.1-py3-none-any.whl.metadata
     Using cached pytest-7.4.1-py3-none-any.whl.metadata (7.9 kB)
   Collecting iniconfig (from pytest>=7.4->grscheller.datastructures)
     Using cached iniconfig-2.0.0-py3-none-any.whl (5.9 kB)
   Collecting packaging (from pytest>=7.4->grscheller.datastructures)
     Using cached packaging-23.1-py3-none-any.whl (48 kB)
   Collecting pluggy<2.0,>=0.12 (from pytest>=7.4->grscheller.datastructures)
     Obtaining dependency information for pluggy<2.0,>=0.12 from https://files.pythonhosted.org/packages/05/b8/42ed91898d4784546c5f06c60506400548db3f7a4b3fb441cba4e5c17952/pluggy-1.3.0-py3-none-any.whl.metadata
     Using cached pluggy-1.3.0-py3-none-any.whl.metadata (4.3 kB)
   Downloading grscheller_datastructures-0.2.1.0-py3-none-any.whl (12 kB)
   Using cached pytest-7.4.1-py3-none-any.whl (324 kB)
   Using cached pluggy-1.3.0-py3-none-any.whl (18 kB)
   Installing collected packages: pluggy, packaging, iniconfig, pytest, grscheller.datastructures
   Successfully installed grscheller.datastructures-0.2.1.0 iniconfig-2.0.0 packaging-23.1 pluggy-1.3.0 pytest-7.4.1

   $ pip list
   Package                   Version
   ------------------------- -------
   grscheller.datastructures 0.2.1.0
   iniconfig                 2.0.0
   packaging                 23.1
   pip                       23.2.1
   pluggy                    1.3.0
   pytest                    7.4.1
   setuptools                65.5.0
```

Success!

## 2023-09-09:

Was able to install grscheller.datastructures from GitHub with tests.

```fish
   $ pip list
   Package    Version
   ---------- -------
   pip        23.2.1
   setuptools 65.5.0

   $ pip install "grscheller.datastructures[test] @ git+https://git@github.com/grscheller/datastructures@v0.3.1.0"
   Collecting grscheller.datastructures[test]@ git+https://git@github.com/grscheller/datastructures@v0.3.1.0
     Cloning https://****@github.com/grscheller/datastructures (to revision v0.3.1.0) to /tmp/pip-install-yoiytp9x/grscheller-datastructures_eae8ab25c9f641d2a4982464e35466cd
     Running command git clone --filter=blob:none --quiet 'https://****@github.com/grscheller/datastructures' /tmp/pip-install-yoiytp9x/grscheller-datastructures_eae8ab25c9f641d2a4982464e35466cd
     Running command git checkout -q 78b6bbe6fc52e37d3bb2d2cb1538130219df9c90
     Resolved https://****@github.com/grscheller/datastructures to commit 78b6bbe6fc52e37d3bb2d2cb1538130219df9c90
     Installing build dependencies ... done
     Getting requirements to build wheel ... done
     Preparing metadata (pyproject.toml) ... done
   Collecting pytest>=7.4 (from grscheller.datastructures[test]@ git+https://git@github.com/grscheller/datastructures@v0.3.1.0)
     Obtaining dependency information for pytest>=7.4 from https://files.pythonhosted.org/packages/df/d0/e192c4275aecabf74faa1aacd75ef700091913236ec78b1a98f62a2412ee/pytest-7.4.2-py3-none-any.whl.metadata
     Downloading pytest-7.4.2-py3-none-any.whl.metadata (7.9 kB)
   Collecting iniconfig (from pytest>=7.4->grscheller.datastructures[test]@ git+https://git@github.com/grscheller/datastructures@v0.3.1.0)
     Using cached iniconfig-2.0.0-py3-none-any.whl (5.9 kB)
   Collecting packaging (from pytest>=7.4->grscheller.datastructures[test]@ git+https://git@github.com/grscheller/datastructures@v0.3.1.0)
     Using cached packaging-23.1-py3-none-any.whl (48 kB)
   Collecting pluggy<2.0,>=0.12 (from pytest>=7.4->grscheller.datastructures[test]@ git+https://git@github.com/grscheller/datastructures@v0.3.1.0)
     Obtaining dependency information for pluggy<2.0,>=0.12 from https://files.pythonhosted.org/packages/05/b8/42ed91898d4784546c5f06c60506400548db3f7a4b3fb441cba4e5c17952/pluggy-1.3.0-py3-none-any.whl.metadata
     Using cached pluggy-1.3.0-py3-none-any.whl.metadata (4.3 kB)
   Downloading pytest-7.4.2-py3-none-any.whl (324 kB)
      ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 324.5/324.5 kB 2.3 MB/s eta 0:00:00
   Using cached pluggy-1.3.0-py3-none-any.whl (18 kB)
   Building wheels for collected packages: grscheller.datastructures
     Building wheel for grscheller.datastructures (pyproject.toml) ... done
     Created wheel for grscheller.datastructures: filename=grscheller_datastructures-0.3.0.2-py3-none-any.whl size=13784 sha256=c07c8e14121efdc45f3250b9a6f7f9b4e2974d41f4a8b3f670df449b5e1187e3
     Stored in directory: /tmp/pip-ephem-wheel-cache-88d89h2g/wheels/e5/81/05/67d80536540da437b0d116e85716211a4d2ff766fbde92969b
   Successfully built grscheller.datastructures
   Installing collected packages: pluggy, packaging, iniconfig, grscheller.datastructures, pytest
   Successfully installed grscheller.datastructures-0.3.0.2 iniconfig-2.0.0 packaging-23.1 pluggy-1.3.0 pytest-7.4.2

   $ pip list
   Package                   Version
   ------------------------- -------
   grscheller.datastructures 0.3.0.2
   iniconfig                 2.0.0
   packaging                 23.1
   pip                       23.2.1
   pluggy                    1.3.0
   pytest                    7.4.2
   setuptools                65.5.0
```

## 2023-09-24:

  Pushed another release of grscheller.datastructures to PyPI. Decided
  that the notes above a bit to unwieldy.

  First make sure $PYTHONPATH is NOT set and go to repo.

```fish
   $ echo $PYTHONPATH
   $ cd ~/devel/pypy/datastructures
```

Updated PyPI version to 0.5.2.1

```fish
   $ nvim ./src/grscheller/datastructures/__init__.py
```

Tag and push to GitHub,

```fish
    $ git add .
    $ git commit
    $ git push
    $ git tag -a v0.5.2.1 -m "geared datastructures to FP"
    $ git push origin v0.5.2.1
  ```

Activate Python Virtual environment and clean up,

```
   $ source /home/grs/devel/venv_system/bin/activate.fish
   $ pip uninstall -y (pip list|tail +3|fields 1|grep -Ev "(pip|setuptools)"
   $ pip list
   Package                   Version
   ------------------------- -------
   pip                       23.2.1
   setuptools                65.5.0
```

Publish package to PyPI

```fish
   $ flit publish
```

Go to home directory and install package with test suite.

```fish
   $ cd
   $ pip install grscheller.datastructures[test]
   $ pip list
   Package                   Version
   ------------------------- -------
   grscheller.datastructures 0.5.2.1
   iniconfig                 2.0.0
   packaging                 23.1
   pip                       23.2.1
   pluggy                    1.3.0
   pytest                    7.4.2
   setuptools                65.5.0
```

Tested package out with ipython, made sure I was using the right
version of pytest, then ran pytest tests.

```fish
   $ ipython

   $ digpath pytest
   /home/grs/devel/venv_system/bin/pytest
   /usr/bin/pytest

   $ pytest --pyargs grscheller.datastructures
   ============================================== test session starts ==============================================
   platform linux -- Python 3.11.5, pytest-7.4.2, pluggy-1.3.0
   rootdir: /home/grs
   collected 17 items

   devel/venv_system/lib/python3.11/site-packages/grscheller/datastructures/tests/dqueue_test.py .....        [ 29%]
   devel/venv_system/lib/python3.11/site-packages/grscheller/datastructures/tests/maybe_test.py .....         [ 58%]
   devel/venv_system/lib/python3.11/site-packages/grscheller/datastructures/tests/stack_test.py .......       [100%]

   =============================================== 17 passed in 0.03s ==============================================
```

Now get out of the virtual environment.

```fish
   $ deactivate
```

That's all folks. Actually I used a few of my fish abbrs & a fish
function for the above:

| abbr  | Description                                                              |
|:-----:|------------------------------------------------------------------------- |
| pl    | pip list                                                                 |
| ve    | source /home/grs/devel/venv_system/bin/activate.fish                     |
| dp    | like which, but does not stop at the first file it finds                 |
| veclr | pip uninstall -y (pip list|tail +3|fields 1|grep -Ev "(pip|setuptools)") |

While in the virtual environment, I used the version of flit that came
with Arch: /usr/bin/flit which is a #!/usr/bin/python script. Probably
not the cleanest thing to do, but it worked.

Probably should have done the flit step outside the virtual environment,
or installed flit into the virtual environment.

## 2023-10-26:

Decided to use pdoc3 to generate documentation for my
grscheller.datastuctures PyPI package.

Does not come in the arch repos, will install it into my venv_system
virtual environment.

```fish
  $ pip install pdoc3
```

This installed MarkupSafe-2.1.3 mako-1.2.4 markdown-3.5 pdoc3-0.10.0

Note that this virtual environment does not include the datastructures
package itself. What I did was to set `$PYTHONPATH` to
`/home/grs/devel/pypi/datastructures/src` where it found
`grscheller/datastructures` directories.

## 2023-10-27:

Based on my work flow, I have determined I need two Python virtual
environments for now. Both will leverage the system Python.

Pacman installed programs will continue using the base system Python
infrastructure. I will try to minimalism this environment.

I will create a virtual environment for my daily Python use. It will
contain the latest PyPI released versions of my grscheller python
packages as well as a lot of other bells and whistles. Eventually this
will be the environment my fish configuration will put me in when I log
in.

Finally I will create a minimal Python virtual environment for the
purpose of developing my grscheller PyPI packages. It will not have any
grscheller packages installed. I will configure `$PYTHONPATH` using my
fish pypi function (later renamed pypath). It will have pytest, pdoc3
and ipython installed. Probably not much else. (Later installed flit
here too.)

## 2023-10-27:

First some cleanup. On gauss17 there is a lot of cruft I naively
installed thinking "that might be nice to use someday" or web frameworks
I tried out once.

In one terminal:

```fish
    $ for pp in (pacman -Qs python-|grep -E 'local'|sed 's/^local\///'|sed 's/ .*$//')
          pactree -r $pp
          echo
      end|less
```

In another terminal:

```fish
    $ pacman -Ss ...
    $ pacman -Qs ...
    $ pactree -r ...
    $ sudo pacman -Rsc ...
```

## 2023-10-27:

Now I will redo my virtual environment infrastructure.

```fish
    $ rm -rf ~/devel/venv_system
    $ mkdir ~/devel/python_envs
    $ cd ~/devel/python_envs
    $ python -m venv grs
    $ python -m venv pypi
    $ ls *
    grs:
    bin/ include/ lib/ lib64@ pyvenv.cfg

    pypi:
    bin/ include/ lib/ lib64@ pyvenv.cfg
```

Here are the fish abbreviations I will use manage the above

```fish
    abbr -a vg source ~/devel/python_envs/grs/bin/activate.fish
    abbr -a vp source ~/devel/python_envs/pypi/bin/activate.fish
    abbr -a -- vclr 'pip uninstall -y (pip list|tail +3|fields 1|grep -Ev "(pip|setuptools)")'
    abbr -a vd deactivate
    abbr -a pl pip list|cat
    abbr -a -- ipy ipython --TerminalInteractiveShell.editing_mode=vi
  ```

Here is the Python environment variables my fish configuration sets up

```bash
    PIP_REQUIRE_VIRTUALENV=true
    PYENV_SHELL=fish
    PYENV_ROOT=/home/grs/.local/share/pyenv
```

After I get everything working, I'll uninstall pyenv. Will reinstall it
at such time I need to use multiple versions of Python.

Time to fix my fish environment and reboot.

## 2023-10-17:

Decided to uninstall pyenv. Easy, I only conditionally configure it in
my fish shell if it was installed.

This is what I will initially install into the virtual environments:

* grs (PyPI names):
  * fonttools
  * grscheller.datastuctures
  * flit
  * ipython
  * pydantic

* pypi (PyPI names):
  * flit
  * ipython
  * pytest

System python toplevel dependenciew (after my cleanup):

* system python (pacman names):
  * cppcheck (uses python-pygments)
  * grapejuice
  * lutris
  * python-pipx
  * python-pynvim
  * yamllint
  * yt-dlp

With dependencies:

* grs:    35 packages
* pypi:   33 packages
* system: 68 packages (before cleanup this number was huge)

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

Also note that most of the other LSP servers I use are all written
in Lua.

## 2024-04-16:

I will use python-lsp-server with the following plugins & providers:

| Providers   | Plugin     | Description                                  |
|:----------- |:---------- |:-------------------------------------------- |
| mypy        | pylsp-mypy | static type checker                          |
| ruff        | ruff-lsp   | linter & formatter (written in rust)         |
| rope        |            | completions and renaming                     |
| mccabe      |            | linter for complexity checking               |

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
Python venv. Some of this infrastructure seems to need access to
the same venv that the code being developed runs in.

I have set ve.conf to manage these virtual environments (updated 2024-04-19):

* dev (3.11.8)
  * pynvim

* dev_next (3.12.2)
  * pynvim

* grs (3.11.8)
  * grscheller.circular-array
  * grscheller.datastructures
  * grscheller.boring-math
  * ipython pdoc3 pytest flit
  * pynvim
  * python-lsp-server pylsp-mypy ruff-lsp rope mccabe

* py4ai (3.11.8)
  * matplotlib jupyterlab
  * ipython pdoc3 pytest
  * pynvim
  * python-lsp-server pylsp-mypy ruff-lsp rope mccabe

* pydev (3.11.8)
  * ipython pdoc3 pytest flit
  * pynvim
  * python-lsp-server pylsp-mypy ruff-lsp rope mccabe

* pydev_next (3.12.2)
  * ipython pdoc3 pytest
  * pynvim

The dev venv is for non-Python development. Any python tools like
ipython will default to the system versions. The inclusion of pynvim is
for python based tooling for other languages.

The grs venv is for developing Python code where I want to leverage
a number of PyPI projects like my grscheller libraries. It is geared
for developing general Python applications. More of a prototype for
future Python venv's.

The pydev is a minimal venv mostly for developing Python modules. I use
my pypath.fish script to manage $PYTHONPATH. This is the
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

## 2024-04-19:

Converted this file from text to markdown. Will fo "markdown feedback"
later this evening.

