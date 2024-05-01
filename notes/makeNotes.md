# GNU Make

This document is intended to be a concise reference with examples
and advice on makefile best practices.  It is not to be considered
a make tutorial.  For more complete documentation see the official
[GNU Make documentation](https://www.gnu.org/software/make/).

## Automtic variables

Automtic variables are set by make after a rule is matched.

| Automatic Variable | Description                                         |
|:------------------:|:--------------------------------------------------- |
|        `$@`        | Filename representing the target                    |
|        `$<`        | Filename of the first prerequisite                  |
|        `$?`        | All prerequisites which are newer than target       |
|        `$^`        | All prerequisites, duplicates removed               |
|        `$+`        | All prerequisites, duplicates not removed           |
|        `$*`        | Target with suffix removed                          |
|        `$%`        | Filename element of an archive member specification |

* Use of `$*` is discouraged outside of pattern rules.

## Special Built-in Target Names

Names that when used as targets, change the behavior of Make.

| Built-in Target        | Effect on Prereqs when targets & other side effects |
|:---------------------- |:--------------------------------------------------- |
|`.PHONY`                | Not real filesystem files, just recipes for tasks   |
|`.SUFFIXES`             | Prereqs are list of suffixes for rest of makefile   |
|`.DEFAULT`              | Gives last resort recipe for prerequisites          |
|`.PRECIOUS`             | Prereqs, when targets, not deleted if make killed   |
|`.INTEMEDIATE`          | Targets of prereqs treated as intermediate files    |
|`.SECONDARY`            | Same as .INTERMEDIATE but never automaically delete |
|`.SECONDEXPANSION`      | Expand again after all included makefiles read in   |
|`.DELETE_ON_ERROR`      | If recipe fails, delete target if it has changed    |
|`.IGNORE`               | Ignore errors when executing recipes                |
|`.LOW_RESOLUTION_TIME`  | For recipes which don't preserve timestamps well    |
|`.SILENT`               | Don't print recipe before execution                 |
|`.EXPORT_ALL_VARIABLES` | Export variables to child processes                 |
|`.NOTPARALLEL`          | Causes current invocation of make to serialize      |
|`.ONESHELL`             | All tabbed recipe lines executed in one shell       |
|`.POSIX`                | Run make in POSIX compliant mode                    |

Notes on Built-in Targets:

* `.PHONY`: an optimization and avoids conflict with real files of same name
* `.INTERMEDIATE`: with no prerequisites has no effect
  * intermediates are automatically deleted when no longer needed
* `.SECONDARY`: with no prereqs causes all targets to be secondary
* `.SECONDARYEXPANSION`: has no prereqs, applies to targets defined after it
  * can use automatic variables on second expansion
  * non trivial examples found [here][1]
* `.IGNORE`: any recipe given with the .IGNORE target itself is ignored
* `.LOW_RESOLUTION_TIME`: example low res time recipe is `cp -p $< $@`
* `.SILENT`: GNU hippies say best practice is to not use this feature
* `.EXPORT_ALL_VARIABLES`: no prerequisits
  * another anti-pattern to avoid
  * `make -e` is similar, has caused me many hours of painful decovolution
* `.NOTPARALLEL`: no prerequisites
  * usually used with recursive make builds
  * avoids race conditions between daughter make invocations
* `.ONESHELL`: no prerequisites
  * bad idea, global change is all or nothing
  * see [Automating Tasks](#automating-tasks) below to write oneliners
* `.POSIX`: no prerequisites
  * GNU Make extensions are still available
  * use POSIX behavior when it differs from GNU behavior
  * POSIX shells get past `-e` option

[1]: https://www.gnu.org/software/make/manual/html_node/Secondary-Expansion.html

## Comments in makefiles

Comments begin with a #

```make
    # Comment ends at end of the line

    # My prefered way
    # to have a multi-line
    # comment.

    # Another way \
    to have a multi-line \
    comment.

    # Slightly better way \
      for a multi-line \
      comment.
```

## Automating Tasks

Make rules can automate tasks.  Only lines which begins the
POSIX shell action, the ones with the "for" and "rm", needs to starts with
a "hard"`<tab>`.  We are just escaping the final `<new-line>'s` and
writing a "one-liner".

```make
    timeit: $(BINARIES)
    for bb in $(BINARIES);\
    do\
        echo -ne "\n$$bb: ";\
        time ./$$bb;\
    done

    clean:
    rm -f $(BINARIES)

    .PHONY: timeit clean
```

Notes on automating tasks:

* `$$` prevents make variable expansion and passes the shell a single `$`
* Using `./` for start of path to executable
* Also, I don't put `.` in my `$PATH`
