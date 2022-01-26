# Python-Django Unchained

Simple example of Python Django project.  Chose PostgreSQL
over MySQL only because the former is what is used at work.

## Installing Django with PostgreSQL

From the [Django Arch Wiki](https://wiki.archlinux.org/title/Django)
page.

```
   $ sudo pacman -Syu python-psycopg2 python-django
   $ python -m django --version
   3.2.10
   $ python --version
   Python 3.10.2
```

* django 3.2 -> python 3.6, 3.7, 3.8, 3.9, 3.10
* django 4.0 -> python 3.8, 3.9, 3.10

## Starting a Django project

To start a Django project,

```
   $ django-admin startproject unchained
```

I will begin by following the 
[official Django](https://docs.djangoproject.com/en/3.2/intro/tutorial01/)
tutorial.  Once I get a skeleton running, I am not sure how I will morph
it to take avantage of such a great name, `unchained`.

## Notes

Need to find a better way to manage Python projects.

```
   $ export PYTHONPATH=lib:../lib
   $ export PATH=$PATH:bin
```

Actually, I will need to configure a Python virtual environment before
proceeding.
