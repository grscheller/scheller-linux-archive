# Python-Django Unchained

Simple example of Python Django project.  Chose PostgreSQL
over MySQL only because the former is what is used at work.

## Installing Django with PostgreSQL

From the [Django Arch Wiki](https://wiki.archlinux.org/title/Django)
page.

```
   $ sudo pacman -Syu python-psycopg2 python-django
```

## Starting a Django project

To start a Django project,

```
   $ django-admin startproject unchained
```

I will begin by following the the 
[official Django](https://docs.djangoproject.com/en/2.2/intro/tutorial01/)
tutorial.  Once I get a skeleton running, I am not sure how I will morph
it to take avantage of such a grat name, `unchained`.

## Notes

Need to find a better way to manage Python projects.

```
   $ export PYTHONPATH=lib:../lib
   $ export PATH=$PATH:bin
```

Actually, I will need to configure a Python virtual environment before
proceeding.
