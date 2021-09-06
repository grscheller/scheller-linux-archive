# Packaging Python Projects

I started out following
[this tutorial](https://packaging.python.org/tutorials/packaging-projects/)
from PyPA's `packaging.python.org` website.  Unfortunately, the tutorial
is out of date.  Using setuptools directly seems deprecated.  Pipenv seems
to be the replacement.

From [PyPI's Pipenv](https://pypi.org/project/pipenv/) website,
Pipenv "automatically creates and manages a virtualenv for your projects,
as well as adds/removes packages from your Pipfile as you install/uninstall
packages. It also generates the ever-important Pipfile.lock, which is used
to produce deterministic builds."

[PyPA's Pipenv](https://pipenv.pypa.io/en/latest/) website claims
"Pipenv is a production-ready tool that aims to bring the best of all
packaging worlds to the Python world. It harnesses Pipfile, pip, and
virtualenv into one single command."

Therefore, I am replacing the above PyPA tutorial with the
[Pipenv Guide](https://realpython.com/pipenv-guide/) by Alexander VanTol
from the [Real Python](https://realpython.com/) website.

## Installing Pipenv

### Arch Linux

On Arch, the system Python is probably kept more up to date than anything
I would manual manage.

```
   $ sudo pacman -S python-pipenv
```

Similarly for other relatively up to date Linux distributions, use
the corresponding package management tool.

### MacOS (Darwin on iMac)

The system Python on MacOS is a joke.  I installed the Anaconda Python
[Individual Edition](https://www.anaconda.com/products/individual)
distribution.

```
   Todo:
```

### Poor man's install

As a last resort, use [Pipx](https://pypi.org/project/pipx/) to
install Pipenv into your home directory.  Especially if you are
an unempowered user with admin privileges.

```
   pip install --user pipx
   pipx install pipenv
```

I have not personally tested this.

## Pipenv Introduction

Pipenv replaces Pip in your development workflow.  The
`requirements.txt` file from Setuptools is replaced with
the `Pipfile` file.  The file `Pipfile.lock` is used to
enable deterministic builds.

Pipenv uses pip and virtualenv under the hood and provides
a single command line interface for their functionality.

