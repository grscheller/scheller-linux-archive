# Sphinx concepts

## Info Field Lists (:role:)

These are the most common in docstrings and use the `:name:` syntax:

### Parameters & Return Values

| Info Field Parameter | Description                           |
|:--------------------:|:------------------------------------- |
| `:param name:`       | describes a parameter                 |
| `:type name:`        | type of a parameter                   |
| `:returns:`          | describes the return value            |
| `:rtype:`            | return type                           |
| `:yields:`           | describes yielded values (generators) |
| `:ytype:`            | type of yielded value                 |


### Exceptions

| Exception          | Description                  |
|:------------------:|:---------------------------- |
| `:raises ExcType:` | document a raised exception  |
| `:except ExcType:` | document a raised exception  |


### Variables

| Variable      | Description                                |
|:-------------:|:------------------------------------------ |
| `:var name:`  | module scoped variable (outside any class) |
| `:ivar name:` | instance variable (set on self)            |
| `:cvar name:` | class variable - a ClassVar (set on cls)   |


## Directives (.. directive::)

Used as block-level notes within extended docstring descriptions:

### Admonition

| Admonition Type               | Description                      |
|:----------------------------- |:-------------------------------- |
| `.. admonition:: some text`   | Make your own label              |
| `.. note::`                   | general informational note       |
| `.. warning::`                | warning to the reader            |
| `.. danger::`                 | critical warning                 |
| `.. important::`              | important notice                 |
| `.. tip::`                    | helpful tip or suggestion        |
| `.. hint::`                   | a hint                           |
| `.. attention::`              | attention-grabbing notice        |
| `.. caution::`                | cautionary note                  |
| `.. seealso::`                | links to related content         |
| `.. deprecated:: version`     | marks deprecation with version   |
| `.. versionadded:: version`   | notes when the feature was added |
| `.. versionchanged:: version` | notes when behavior changed      |


### Code & Literal Directives

| Code & Literal Directives  | Description                                    |
|:--------------------------:|:---------------------------------------------- |
| `.. code-block:: language` | syntax-highlighted code                        |
| `.. literalinclude:: path` | include an external file                       |
| `.. testcode::`            | doctest code block (used with doctest builder) |
| `.. testoutput::`          | expected output for a .. testcode:: block      |
| `.. doctest::`             | interactive Python session example             |


### Math

| Math Directive           | Description            |
|:------------------------:|:---------------------- |
| `.. math::`              | block-level LaTeX math |


### Images & Figures

| Embedded            | Description                          |
|:------------------- |:------------------------------------ |
| `.. image:: path`   | embed an image                       |
| `.. figure:: path`  | embed a figure with optional caption |


### Structural Directives

| Structural Directives | Description                                                       |
|:---------------------:|:----------------------------------------------------------------- |
| `.. rubric:: title`   | informal heading (doesn't appear in TOC)                          |
| `.. toctree::`        | table of contents tree (rare in docstrings, common in .rst files) |
| `.. only:: condition` | conditional content inclusion                                     |
| `.. include:: path`   | include another file                                              |

## Cross-References

Used inline within docstring text:

Cross-Reference Roles (` :domain:role:\target`` `)

### Roles

#### Inline Math

| Role                     | Description            |
|:------------------------ |:---------------------- |
| ` :math:\\expression`` ` | inline LaTeX math      |


#### Inline Code & UI

| Role                               | Description                                     |
|:---------------------------------- |:----------------------------------------------- |
| ` :code:`expression` `             | inline code (equivalent to backtick literal)    |
| ` :samp:`text {variable}` `        | sample text with a user-substituted placeholder |
| ` :file:`path/to/{name}` `         | file path with a user-substituted placeholder   |
| ` :command:`cmd` `                 | OS-level command                                |
| ` :option:`-v` `                   | command-line option                             |
| ` :kbd:`Ctrl+C` `                  | keyboard input                                  |
| ` :menuselection:`File --> Save` ` | GUI menu path                                   |
| ` :guilabel:`OK` `                 | GUI label or button                             |


#### Standards & Glossary.

| Role                                       | Description                          |
|:------------------------------------------ |:------------------------------------ |
| ` :abbr:`HTML (HyperText Markup Language)` | abbreviation with expansion on hover |
| ` :acronym:`NATO` `                        | acronym                              |
| ` :term:`term` `                           | links to a glossary entry            |
| ` :pep:`8` `                               | link to a Python PEP                 |
| ` :rfc:`2119` `                            | link to an IETF RFC                  |


### Domains

Sphinx introduced the domain system to extend its cross-referencing
and indexing capabilities across different subject areas. A domain
is essentially a named collection of:

Directives for describing objects (e.g. .. py:function::, .. c:type::)
Roles for cross-referencing those objects (e.g. :py:func:, :c:type:)
An index of the described objects

Note that the `:py:` prefix is actually optional when documenting
Python, since py is Sphinx's default domain. For all other domains,
the prefix is required. The domain must also be active — either
built-in (C, C++, JS, rST, std) or registered via an extension
in conf.py.

One can set a default domain for an entire document via

```
    .. default-domain:: py`.

```

#### Python domain

| Python Domain | Return Value      |
|:-------------:|:----------------- |
| `:py:mod:`    | module            |
| `:py:func:`   | function          |
| `:py:class:`  | class             |
| `:py:meth:`   | method            |
| `:py:attr:`   | attribute         |
| `:py:exc:`    | exception         |
| `:py:obj:`    | any Python object |
| `:py:const:`  | constant          |
| `:py:data:`   | module-level data |


#### C Domain (:c:)

| C Domain         | Return Value                          |
|:----------------:|:------------------------------------- |
| `:c:func:`       | C function                            |
| `:c:member:`     | C struct or union member              |
| `:c:macro:`      | C macro                               |
| `:c:struct:`     | C struct                              |
| `:c:union:`      | C union                               |
| `:c:enum:`       | C enum                                |
| `:c:enumerator:` | C enum value                          |
| `:c:type:`       | C type (typedef or struct/union/enum) |
| `:c:var:`        | C variable                            |
| `:c:expr:`       | inline C expression (renders as code) |


#### C++ Domain (:cpp:)

| C++ Domain         |  Return Value                   |
|:------------------:|:------------------------------- |
| `:cpp:func:`       | function or method              |
| `:cpp:member:`     | member variable                 |
| `:cpp:var:`        | member variable                 |
| `:cpp:type:`       | type, typedef, or template type |
| `:cpp:class:`      | class or struct                 |
| `:cpp:struct:`     | struct                          |
| `:cpp:union:`      | union                           |
| `:cpp:enum:`       | scoped or unscoped enum         |
| `:cpp:enumerator:` | enum value                      |
| `:cpp:concept:`    | C++20 concept                   |
| `:cpp:expr:`       | inline C++ expression           |


#### JavaScript Domain (:js:)

| JavaScript Domain  |  Return Value        |
|:------------------:|:-------------------- |
| `:js:func:`        | function             |
| `:js:meth:`        | method               |
| `:js:class:`       | class                |
| `:js:data:`        | variable or constant |
| `:js:attr:`        | attribute            |


#### reStructuredText Domain (:rst:)

| Domain       |  Return Value      |
|:------------:|:------------------ |
| `:rst:role:` | an rST inline role |
| `:rst:dir:`  | an rST directive   |


#### Standard Domain (:std:)

The default domain, active when no domain prefix is specified:

| Domain            |  Return Value                   |
|:-----------------:|:------------------------------- |
| `:std:doc:`       | a document in the project       |
| `:std:ref:`       | a labeled section or figure     |
| `:std:label:`     | a cross-reference label         |
| `:std:term:`      | a glossary term                 |
| `:std:option:`    | a command-line option           |
| `:std:envvar:`    | an environment variable         |
| `:std:cmdoption:` | a program's command-line option |
| `:std:program:`   | a program name                  |


#### Other domains

| Domain  |  Return Value                   |
|:-------:|:------------------------------- |
| `:doc:` | link to another document        |
| `:ref:` | cross-reference to a label      |
| `:any:` | resolves to any matching target |
