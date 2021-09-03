# Packaging Python Projects

I started out following
[this tutorial](https://packaging.python.org/tutorials/packaging-projects/)
from PyPA's `packaging.python.org` website.  Unfortunately, this tutorial
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

## Directory Structure (needs updating)

```
   packaging_tutorial/
   |
   |-- LICENSE
   |-- pyproject.toml
   |-- README.md
   |-- setup.cfg
   |-- src/
   |   |
   |   |-- example_package/
   |       |-- __init__.py
   |       |-- example.py
   |
   |-- tests/
```

## Create pyproject.toml (needs updating)

The [pyproject.toml](pyproject.toml) configuration file tells tools
like pip and build what is required to build this project.

* `build-system.requires` lists packages needed to _build_ the project
  * These include `setuptools` and `wheel`
  * These tools not needed when package is _installed_
* `build-system.build-backend` is Python object used to perform the build
  * `setuptools.build_meta` in this case
  * Other possible build objects include `poetry` and `flit`

## Configurating Metadata (needs updating)

Two types of mata data:

* Static metadata - configured via `setup.cfg`
  * prefered way
  * guaranteed to be the same every time
* Dynamic metadata - configured via `setup.py`
  * use only when absolutely necessary
  * needed for extension modules or extensions to setuptools
  * run as Python code
