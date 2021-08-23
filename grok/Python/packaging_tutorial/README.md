# Packaging Python Projects

Following
[this tutorial](https://packaging.python.org/tutorials/packaging-projects/)
from `packaging.python.org`.

To keep things simple, not necessarily easy, I will manually
manage a Python Virtual Environment for this project. I think
in the long term my daily driver will be Pipenv.

## Directory Structure

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

## Create pyproject.toml

The [pyproject.toml](pyproject.toml) configuration file tells tools
like pip and build what is required to build this project.

* `build-system.requires` lists packages needed to _build_ the project
  * These include `setuptools` and `wheel`
  * These tools not needed when package is _installed_
* `build-system.build-backend` is Python object used to perform the build
  * `setuptools.build_meta` in this case
  * Other possible build objects include `poetry` and `flit`

## Configurating Metadata

Two types of mata data:

* Static metadata - configured via `setup.cfg`
  * prefered way
  * guaranteed to be the same every time
* Dynamic metadata - configured via `setup.py`
  * use only when absolutely necessary
  * needed for extension modules or extensions to setuptools
  * run as Python code
