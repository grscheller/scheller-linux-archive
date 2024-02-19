# GIT Notes

These are the notes I update as I learn more about GIT source code
revision control. Though not a beginner tutorial, I try to
make these notes a useful resource for beginners as well as myself.

#### Git vs GitHub & GitLab

Don't conflate GIT with the GitLab or GitHub hosting services. There is
no such thing as a "*pull request*" or "*forking*" in GIT. These are
GitHub & GitLab constructs. GIT is design to `clone` repositories and
`push` & `pull` changes between those repos that share past history.

#### Decentralization

GIT is decentralized. Unlike version control tools like SCCS, CVS, or
Subversion there is no *central* or *blessed* repository, unless you
just consider one as such. Even then the blessed repo is still just
a social construct, GIT does not care.

GIT branches are just light weight user controlled labels, not large
monolithic directory structures in a centrally maintained software
repository.

In the 1990's, as a contractor for AT&T Microelectonics, I was the
SCCS administrator for AT&T Microelectronics Process Control
Manufacturing Execution System which tracked and controlled products
through multiple semiconductor manufacturing production lines. Process
Control, a misnormer, Production Cotrol would have been a better name,
controlled production based on customer orders and inventory. If it had
existed, GIT would have been much simpler to configure, maintain and use
than SCCS.

#### Git user interfaces

You do not know GIT until you can use its CLI inerface. GIT's CLI
interface was designed to implement user version control workflows.
Typically I find that most GIT GUI clients, web interfaces and IDEs are
designed to adapt the user to some particular workflow, typically that
of the tool designer. Without a knowledge of GIT's CLI iterface, it is
much harded to know what to look for in a particular GIT GUI client's
menus, keyboard shortcuts, and help utility. GUI and web clients come
and go, GIT's CLI interface will be with us for decades, if not
centuries.

#### Resources I used to learn GIT:

[ProGIT Book](http://www.git-scm.com/book/en/v2)

[Atlassian - Setting up a repo](https://www.atlassian.com/git/tutorials/setting-up-a-repository)

[Git 101](https://www.garron.me/en/articles/git-101-basics-introduction-basic-use-commands.html)

[Stack Overflow](https://stackoverflow.com/?tab=active)

---

## Getting help

Most Linux distributions have a good GIT tutorial in the man pages.

```bash
   $ man gittutorial
```

To get a comprehensive overview of git, use

```bash
   $ man git
```

and to get information on individual commands

```bash
   $ man git-add
   $ man git-info
```

or using the --help option in git

```bash
   $ git --help
   $ git clone --help
```

Three "patterns" to best leverage the above help commands

```bash
   $ git help verb
   $ git verb --help
   $ man git-verb
```

Also see [GIT help files](https://support.github.com/) on the web.

---

## GIT config command

### Five places git stores config info

1. **/etc/gitconfig**
2. **/usr/local/gitconfig**
3. **~/.config/git/config**
4. **~/.gitconfig**
5. **.git/config at root of repo**

Each one overrides the ones above it.

The `git config` command (without --global)

* updates the repo's '.git/config' file,
* or complains if not in a GIT repository

The `git config --global` command will

* update #4 if it exists
* otherwise creates and updates #4 if #3 does not exist
* otherwise it updates #3

I usually keep #3 under git control and #4 not. That way
`git config --global` configurations don't get clobbered when
I update my config files with my dotfile installation script.

Aside: The `git maintenance` command will create #4 even if #3 exists.

### Configuring GIT

#### Set up your identity

```bash
   $ git config --global user.name 'John Doe'
   $ git config --global user.email john.doe.2@us.af.mil
```

If you want to override these settings for a specific project,
run the above commands without the --global option when in
that project directory structure.

#### To customize editor

```
   $ git config --global core.editor nvim
```

#### To list the settings set so far

```bash
   $ git config --list
```

#### for a specific setting

```bash
   $ git config user.name
```

#### What currently is configured for this project's repo

```bash
   $ cd ~/devel/scheller-linux-archive
   $ git config --list --show-origin --show-scope
   global  file:/home/grs/.config/git/config       user.name=grscheller
   global  file:/home/grs/.config/git/config       user.email=geoffrey@scheller.com
   global  file:/home/grs/.config/git/config       user.signingkey=/home/grs/.ssh/id_ed25519_grscheller.pub
   global  file:/home/grs/.config/git/config       core.editor=nvim
   global  file:/home/grs/.config/git/config       core.fsmonitor=true
   global  file:/home/grs/.config/git/config       core.pager=less
   global  file:/home/grs/.config/git/config       color.pager=yes
   global  file:/home/grs/.config/git/config       pull.rebase=false
   global  file:/home/grs/.config/git/config       init.defaultbranch=main
   global  file:/home/grs/.config/git/config       submodules.recurse=true
   global  file:/home/grs/.config/git/config       diff.submodule=log
   global  file:/home/grs/.config/git/config       rerere.enabled=true
   global  file:/home/grs/.config/git/config       gpg.format=ssh
   global  file:/home/grs/.gitconfig       maintenance.repo=/home/grs/devel/scheller-linux-archive
   global  file:/home/grs/.gitconfig       maintenance.repo=/home/grs/devel/dotfiles
   global  file:/home/grs/.gitconfig       maintenance.repo=/home/grs/devel/dotfiles-submodules/fish
   global  file:/home/grs/.gitconfig       maintenance.repo=/home/grs/devel/dotfiles-submodules/nvim
   global  file:/home/grs/.gitconfig       maintenance.repo=/home/grs/devel/dotfiles-submodules/home
   global  file:/home/grs/.gitconfig       maintenance.repo=/home/grs/devel/dotfiles-submodules/sway-env
   global  file:/home/grs/.gitconfig       maintenance.repo=/home/grs/devel/grok-typescript
   global  file:/home/grs/.gitconfig       maintenance.repo=/home/grs/devel/fpinScala3Stdlib
   global  file:/home/grs/.gitconfig       maintenance.repo=/home/grs/devel/neovim-notes
   global  file:/home/grs/.gitconfig       maintenance.repo=/home/grs/devel/pypi/boring-math
   global  file:/home/grs/.gitconfig       maintenance.repo=/home/grs/devel/pypi/datastructures
   global  file:/home/grs/.gitconfig       maintenance.repo=/home/grs/devel/pypi/circular-array
   global  file:/home/grs/.gitconfig       maintenance.repo=/home/grs/devel/web
   local   file:.git/config        core.repositoryformatversion=0
   local   file:.git/config        core.filemode=true
   local   file:.git/config        core.bare=false
   local   file:.git/config        core.logallrefupdates=true
   local   file:.git/config        remote.origin.url=git@github.com:grscheller/scheller-linux-archive
   local   file:.git/config        remote.origin.fetch=+refs/heads/*:refs/remotes/origin/*
   local   file:.git/config        branch.master.remote=origin
   local   file:.git/config        branch.master.merge=refs/heads/master
   local   file:.git/config        branch.add-license-1.remote=origin
   local   file:.git/config        branch.add-license-1.merge=refs/heads/add-license-1
   local   file:.git/config        remote.beowulf.url=https://github.com/grsBeowulf/scheller-linux-archive
   local   file:.git/config        remote.beowulf.fetch=+refs/heads/*:refs/remotes/beowulf/*
   local   file:.git/config        maintenance.auto=false
   local   file:.git/config        maintenance.strategy=incremental
```

---

## GIT clone and init commands

Setting up your own GIT repository to work with.

### Starting from scratch

Initializing a repository in an existing directory

```bash
   $ cd /path/to/an/existing/directory
   $ git init
```

This creates a .git subdirectory (a GIT repository skeleton).

Add files to this repository:

```bash
   $ git add *.c *.py
   $ git add LICENSE README.txt
   $ git commit -m 'initial project version'
```

### Cloning existing repositories:

To clone an existing GIT repository

```bash
   $ git clone git@github.com:grscheller/grok-typescript
   $ git clone https://github.com/grscheller/grok-typescript learn-ts
   $ git clone grs@us.navo.hpc.mil:proj/grsHome.git
   $ git clone ~/devel/myRepo ~/junk/myRepoCopy
```

This is not just a snapshot from the repo of the project, it clones the
entire repository, complete with all change control files, containing
all past versions of the project. The second version changes the name of
the directory containing the GIT repo from grok-typescript to learn-ts.
GIT does not care about the name of the directory it gets cloned into.
For the last version, the "upstream repo" or `origin` was set to
"/home/grs/devel/myRepo" with no knowledge of the upstream's `origin`,
if it even exists.

### Using GitHub or GitLab

Go to github.com or gitlab.com and create yourself an empty GIT repo for
your project, using their webtools. Then clone the empty repo as shown
in the above system. Both services provide useful boiler plate for
various types of projects and software licenses.

### Setting up a local shared repository

I found doing this useful when I had two contractors working the same
code base but didn't have access to each other's GIT repos. The only
thing they share is ssh access to the same out-of-date CentOS Linux
system where I had no root access.

First I had to ask the system admin "nicely" to have the contractors and
myself put in the same linux secondary group with access to some common
filesystem real estate. At least this admin knew what a "secondary
group" was.

```bash
   $ su -
   $(root) groupadd repo --users grs dude1 dude2
   $(root) mkdir /share/repos
   $(root) chown grs:repo /share/repos
   $(root) chmod 2770 /share/repos
```

Then using my login

```bash
   $ umask 0007
   $ls -ld /share/repos
   drwxrws---. 7 grs repos 4096 Dec 12 09:17 /share/repos
   $ cd /share/repos
   $ git init --bare OurProject.git
```

This initializes an empty GIT repository, but without the outer working
directory. Shared repositories should always be created with the
`--bare` flag. Think `--bare` as marking the repository as a storage
facility as opposed to a development environment.

Now connect up our empty naked repository to somewhere where some prior
GIT based development work has been done, but neither contractor has
access:

```bash
   $ cd OurProject.git
   $ git remote add upstream /home/grs/devel/someProject
   $ git fetch upstream
   $ git fetch --tags upstream
```

This will bring in all the branches and tags. Next from the someProject
upstream repo

```bash
   $ cd /home/grs/devel/someProject
   $ git remote add downstream /share/repos/ourProject.git
```

Make sure your umask is set to `0007` so that all the files in
ourProject.git get created with the necessary group rwx permissions for
group access. If not, GIT will function, but with subtly bizzare
behavior. The SGID bit is set on the directory so that all files will be
in the repo group, not a user's default groups.

Both contractors can clone this shared repo and collaborate by pulling
and pushing to it. I can pull from the shared repo and send upstream
from my repo.

Changes I would change if I had to do this again:

* refuse to help unless I had administrator privaledges
* run an actual git server either locally or somewhere else
* have the contractors digitally sign their commits

If you are starting a brand new project, setting up the shared repo is
a bit easier. Create a shared, empty, bare repository as above, clone
it, add the initial files to the clone, and push it back to the shared
repo.

```bash
   $ git clone /share/repos/ourProject.git
   $ cp -R /path/to/some/initial/files/* ourProject
   $ cd ourProject
   $ git add *
   $ git commit
   $ git push
```

### Renaming the directory a local GIT repo is in

To "rename" the directory that a GIT repository is in, first note that
that git does not care about the name of the working directory. The
client just needs to know where to point.

On the server:

```bash
   $ mv PAT.git SDT.git
```

On the client:

```bash
   $ git remote rm origin
   $ git remote add origin grs@us.mhpcc.hpc.mil:projects/SDT.git
```

Note: you may need to tell a branch what its upstream now is

```bash
   $ git checkout master
   $ git branch -u origin/master
   $ git fetch origin
```

### Cloning from another working repo.

Let's say I have a working repo in ~/Devel/SDT

```bash
   $ cd ~/Devel/SDT
```

Make sure we are in a stable state

```bash
   $ git status
   On branch sdt_devel_branch
   nothing to commit, working tree clean
```

See what branches are here

```bash
   $ git branch
     sdt_production_branch
   * sdt_devel_branch
     master
```

Let's clone the repo

```bash
   $ cd ../..
   $ mkdir temp
   $ cd temp
   $ git clone ../Devel/SDT
   Initialized empty GIT repository in /home/grs/temp/SDT/.git/
```

Let's see what we got

```bash
   $ cd SDT
   $ git branch
   * sdt_devel_branch
   $ git remote -v
   origin    /home/grs/temp/../Devel/SDT (fetch)
   origin    /home/grs/temp/../Devel/SDT (push)
```

We only picked up the currently active branch. I will sometimes do this
to have a quick and dirty snapshot of a working copy of the software.
Also, I can 'rm -rf .git' and either tar ball or burn to DVD what I want
to give someone. Just make sure you do this in ~/temp/SDT and not in
~/Devel/SDT!!! I learned this the hard way.

---

## The fetch command

The fetch command updates local copies of remote branches.

```bash
   $ git fetch origin
```

will update information of local copies of all remote branches.

```bash
   $ git fetch --all
```

will do this with all remote repositories your local repo knows about.

**Note:** 'git fetch' commands will not create local branches to track
remote branches. You must do a 'git checkout' for each new branch you
want to track. The 'git pull --all' command will only pull from branches
you currently track.

---

## Overiding global configurations when cloning a repo:

To overiding global configurations when cloning a repo

```bash
   $ git clone --config user.email=me@myotheremail.com \
       --config http.sslcainfo=/home/geoff/.ssh/Cert_dropbox_wont_have.crt \
       --config http.verify=true \
       https://aur.archlinux.org/dropbox.git
```

You can use `-c` instead of `--config`.

---

## Listing the commit comments made in a repo

To list the commit comments made for the current "checked out" version
of the repo you are currently in, do

```bash
   $ git log
```

for a given director in the repo, do

```bash
   $ git log grok/Haskell
```

For a given file, do

```bash
   $ git log adminLogs/gauss17ArchLinuxAdmin.log
```

---

## GIT Basics:

Here is the "lifecycle" of a file in a GIT workspace

```
   Untracked        Unmodified      Modified         Staged
       |                |              |                |
       |⁃⁃⁃add file⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃>|                |
       |                |⁃⁃edit file⁃⁃>|                |
       |                |              |⁃⁃stage file⁃⁃⁃>|
       |<⁃⁃remove file⁃⁃|              |                |
       |                |<⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃commit file⁃⁃⁃|
       |                |              |                |
```

and here is the "lifecycle" of a GIT workflow

```
   remote    tracked    local       index    workspace
     |          |         |           |          |
     |⁃⁃pull⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃>|
     |  ⁝       |         |           |          |
     |  ⁝⁃⁃⁃⁃⁃⁃>|         |           |<⁃⁃⁃add⁃⁃⁃|
     |  ⁝       |         |           |          |
     |  ⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃>|           |          |
     |          |         |           |          |
     |⁃⁃fetch⁃⁃>|         |<⁃⁃commit⁃⁃|          |
     |          |         |           |          |
     |<⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃⁃     |           |          |
     |          |   ⁝     |           |          |
     |          |<⁃⁃push⁃⁃|           |          |
     |          |         |           |          |
```

### Checking status of files:

To checking status of the files in your repo

```bash
   $ git status
   $ git status --short
```

**Note:** Git only tracks files. not directories.

### Begin tracking files

To begin tracking a file

```bash
   $ git add myfile.c    # myfile.c is now staged
   $ git add directory   # all files staged recursively
```

Think of add as meaning "add this content to the next commit" rather
than "add this file to the project." GIT stages the file as it was when
'git add' command was issued. If you modify a file after it was staged,
you have to add it again to pick up the latest changes.

### Unstaging files

To unstage a file

```bash
   $ git restore --staged some_file
```
This s a clear improvement over how this was done in older versions of
GIT as shown below.

```bash
   $ git reset HEAD some_file
```

where HEAD is a special ref that points to the commit that currently is
checked out. Using the git-restore command makes it more clear as to
what is being done. The git-reset command seems to be taking advantage
of GIT implementation details and is opaque to what is really being
done.

To discard changes not alrady staged

```bash
   $ git checkout some_file
```

---

## Ignoring files:

Use a .gitignore file to make GIT ignore files.

Example .gitinore file:

```gitignore
   # ignore all .a files
   *.a
   # but do track lib.a, dispite ignoring all .a files above
   !lib.a
   # only ignore the root TODO file, not subdir/TODO
   /TODO
   # ignore all files in the build/ directory
   build/
   # ignore doc/notes.txt, but not doc/server/arch.txt
   doc/*.txt
   # ignore all .txt files in the doc/ directory tree
   doc/**/*.txt
```

* Blank lines and lines starting with # are ignored
* Extended shell globbing patterns work
* End patterns with a forward slash (/) to specify a directory
* Negate a pattern by starting it with an exclamation point (!)
* Two asterisks match nested directories

---

## Moving and removing files.

### Removing files

To remove files from a branch

```bash
   $ git rm file1 file2 dir1/
   $ git commit
```

If there are untracked changes to the files, the files will remain in
the workspace as untracted files. GIT does not track directories so
dir1/ will remain too if there are untracted files in it.

You may need to do

```bash
   $ rm file1 file2
   $ rm -r dir1/
```

to actually get rid of them from the working directory. Sometimes one
run into empty directories when files are removed upstream.

Lets say you want to remove lots of files. For example, lets get rid of
all the subversion directories (since we are using GIT).

```bash
   $ find . -depth -name '.svn' -exec rm -rf '{}' \;
```

best practice is to then use

```bash
   $ git add --update
   $ git commit
```

The `--update` or `-u` option only matches files in the index rather
than the working tree. This removes as well as modifies index entries to
match the working tree, but adds no new files.

This will save you from having to do a lot of tricky bash shell
scripting like

```bash
   $ git rm $(git status | grep delete | awk '{print $3}')
   $ git commit
```

which will also work.

To updating major changes, such as a vendor upgrade, with file additions
and removals, in a brute force sort of way, use

```bash
   $ git add --all
   $ git commit
```

The `--all` or `-A` option is like `--update` except that it also will
match against files in the working tree.

For better fidelity, especially when dealing with directory structure
changes, one might want to use `git mv` commands.

### Moving files

To move files

```bash
   $ git mv path/to/filename new/path/to/new_filename
   $ git commit
```

GIT will move and stage the original version. If you move a file with
unstaged changes, The changes made to the file will be unstaged changes
within the working directory.

Usual Unix mv command semantics apply. To move a bunch of files to a new
directory

```bash
   $ mkdir new_dir
   $ git mv file1 file2 file3 new_dir
   $ git commit
```

To remove an empty directory.

GIT does not track directories, only files. You can't use "git
rm" to remove a directory and push change.

A good habit is to periodically use

```bash
   $ git clean -dn
```

to see what needs cleaning up. Either manually clean up or use

```bash
   $ git clean -df
```

The `git clean` command takes the following options

| Option    | Description                                           |
|:---------:|:----------------------------------------------------- |
|  `-d`     | remove untracked directories too                      |
|  `-n`     | dry run, just show what to remove                     |
|  `-f`     | force                                                 |
|  `-i`     | interactive mode                                      |
|  `-x`     | don't use ignore rules from .gitignore files          |
|  `-X`     | remove only files ignored by git                      |
|  `-e pat` | add pattern `pat` to ignored patterns from .gitignore |

either -n, -f, or -i must be selected.

---

## Creating revisionist history via git rebase:

Rewriting your history, don't do this if you have already shared it!!!

The 'git rebase' command comes in handy when you need to reorder
commits, change commit messages, squash commits together.

Reasonable reasons for squashing commits might be

* enforcing a policy of never pushing non-working history to a
  software release branch
* to more effectively use `git bisect` to find a bug
  * helpful to endusers not familiar with the code base who spot a bug

Hiding your *dirty laundry* so that everyone will think you write
perfect code the first time is never a good reason to rebase. 

### Redo the last 4 commits:

```bash
   $ git rebase -i HEAD~4
```

       Drops you into an editor session:

```
   pick 8c4a6a5 Commit message four commits ago.
   pick 4a3f436 Commit message three commits ago.
   pick 949e05d Commit message two commits ago.
   pick 8ae51b6 Commit message on last commit.
   ...
```

And gives you the following choices to edit into above:

```
   p, pick = use commit
   r, reword = use commit, but edit the commit message
   e, edit = use commit, but stop for amending
   s, squash = use commit, but meld into previous commit
   f, fixup = like 'squash', but discard this commit's log message
   x, exec = run command (the rest of the line) using shell
   d, drop = remove commit
```

In the case of a merge conflict, GIT drops you back to a shell. Fix the
conflict, and

```bash
   $ git add ...
   $ git rebase --continue   # or use "git rebase --abort"
```

### Amending commits

If you have not pushed your changes upstream, you can use the
'git commit --amend' to update the last commit. GitHub will
refuse a push with such an amended commit if a previous version
was already pushed. You will need to do a `git merge` with the
upstream version to make your HEAD pushable.

### Amending pushed commits

You just pushed to GitHub changes you very deeply regret. GitHub
rejects your amended changes.

You don't want to be a *bad boy* and cause grief to others by doing a

```bash
   $ git push --force   # DON'T Do THIS!!!
```

The above could swallow work done by others!

There is a newer option to the `git push` command that may save you.

```bash
   git push --force-with-lease
```

When this option is used, GIT will let you force commit this change as
long as it will not overwrite any work on the remote branch when more
commits were added to the remote branch.

You did not have to do a messy merge! But you may still be a *bad boy*.
If someone had fetched your changes they will not be able to push back
until *they* did a messy merge. Also, until they fully merge, they will
still have access to your *dirty little secret* and could very well
put it back!

There is a lot more to this command and could be useful in rebasing
situations. This option can take a ref as an optional parameter.
This option allows you to say that you expect the history you are
updating is what you rebased and want to replace. If the remote ref
still points at the commit you specified, you can be sure that no other
people did anything to the ref. It is like taking a "lease" on the ref
without explicitly locking it, and the remote ref is updated only if the
"lease" is still valid.

--force-with-lease alone, without specifying the details, will protect
all remote refs that are going to be updated by requiring their current
value to be the same as the remote-tracking branch we have for them.

---

## GIT branch:

Note that all branches really are just pointers to commits.

#### Create new branches

To create a new branch (does not checkout the branch)

```bash
   $ git branch myBranch
```

This branch is based on the current branch checked out.

To create a new local empty branch

```bash
   $ git checkout --orphan newBranch
```

This will create a new branch without any commit. The first commit will
start a new history without any ancestry. The `--orphan` is a useful
option when you want to create a directory structure and files
resembling the ones from the current branch.

#### List available branches

List all available local branches:

```bash
   $ git branch
```

To also see the remote tracked branches:

```bash
   $ git branch -a
     Anansi
     astrolog-java
     bar-mph
     silverdb
     silver_grs_oneToolbar
   * master
     oneToolbar
     remotes/origin/Anansi
     remotes/origin/HEAD -> origin/master
     remotes/origin/astrolog-java
     remotes/origin/bar-mph
     remotes/origin/silverdb
     remotes/origin/silverdb_grs_oneToolbar
     remotes/origin/master
```

#### View local branches relative to current branch

To view merged local GIT branches relative to current branch

```bash
   $ git branch --merged
```

and unmerged local GIT branches

```bash
   $ git branch --no-merged
```

#### Delete a local branch

To delete a local branch safely, git will prevent you if you have
unmerged changes.

```bash
   $ git branch -d myBranch
```

To force delete a specified branch and permanently throw away all
unmerged changes

```bash
   $ git branch -D myBranch
```

#### Delete a remote branch

To delete a remote branch or tag

```bash
   $ git push origin --delete myBranchToDelete
```

Prior to git version 7.0 but will still work

```bash
   $ git push origin :myBranchToDelete
```

#### Deleting remote references

To delete references to remote branches that no longer exist on their
remote repos.

```bash
   $ git fetch -p
   $ git fetch -p some_remote_repo
   $ git fetch -p origin
```

#### Renaming branches

To rename a local Branch

```bash
   $ git branch -m oldname newname
```

To rename the current branch to new name

```bash
   $ git branch -m myNewName
```

#### Deleting tracking banches

To delete an existing (local) remote tracking branch

```bash
   $ git branch -d -r origin/macos
```

#### Tracking info

To figuring out what exactly your branches are actually tracking

```bash
   $ git branch -vv
   car-mhf         b819996 ,origin/car-mhf, Fixed awkward language in a comment to make more clear.
   goldsdb         41d7446 [origin/goldsdb] The initial View Table now contains 1 empty row.
   master          28f7355 [origin/master] Up dates Resources/DTIC_GIT_Notes.txt with notes on:
   * scheller-master 2c7336f [scheller/master: ahead 1] An updater to the root README.md file.

   $ git remote -v
   origin  https://geoffrey.scheller@repos.vdl.afrl.af.mil/git/astrodynamics/astrodynamics.git (fetch)
   origin  https://geoffrey.scheller@repos.vdl.afrl.af.mil/git/astrodynamics/astrodynamics.git (push)
   scheller    ../../../scheller-linux-archive/ (fetch)
   scheller    ../../../scheller-linux-archive/ (push)
```

---

## GIT checkout:

The git checkout command lets you navigate between branches. Checking
out a branch updates the files in the working directory to match the
version stored in that branch, and it tells GIT to record all new
commits on that branch. Think of it as a way to select which line of
development you’re working on.

### Checking out branches

```bash
   $ git checkout existingBranch
```

Can also create a new branch at checkout

```bash
   $ git checkout -b newBranch
```

or base it on another existing branch instead of the current one.

```bash
   $ git checkout -b newBranch anotherBranch
```

You can work on multiple branches in a single repository by switching
between them with `git checkout` or `git switch`.

Check out a branch you don't have locally from origin (the repo
you are tracking from, usually the one you first cloned from).

```bash
   $ git fetch
   $ git checkout some_new_remote_branch
```

If you are tracking several remote branches, you may need to be a bit
more specific:

```bash
   $ git checkout -b some_branch remote-name/some_branch
```

Lets say we have two remotes each with a branch with same name

```bash
   $ git branch -r
   origin/copperDB
   origin/dmc_run
   origin/master
   origin/spaceRad
   scheller/master
```

and we are already tracking origin/master.

```bash
   $ git branch
     copperDB
     dmc_run
   * master
```

How do we checkout scheller/master? We give it another name

```bash
   $ git checkout -b scheller-master --track scheller/master
```

Now we have

```bash
   $ git branch
     copperDB
     dmc_run
     master
   * scheller-master
```

---

## GIT merge:

The git merge command is used to merge another branch into your current
branch. If merge successful without conflicts, you are done. If not,
changes are merged into your working directory with GIT putting comments
in the code. In this case, you still need to resolve the conflicts and
do a git commit.

### Merging in another branch

To merge a specific branch into the current branch

```bash
   $ git merge someBranch
```

**Note:** The branch you are merging into is the currently checked out
branch. If you are in a "detached HEAD" state, you will need to create
a branch to merge someBranch into.

Edit any conflicts, `git add` changes, `commit` changes if needed.

### Three-way merge

Here is an example of the workflow for a 3-way merge. Start a new
feature on its own new branch

```bash
   $ git checkout -b new-feature master
```
edit some files

```bash
   $ git add file1 file2 file3
   $ git commit -m 'Starting new feature X'
```

edit some more files

```bash
   $ git add file2 file4
   $ git commit -m 'Finish new feature X'
```

Parallel development on the master branch

```bash
   $ git checkout master
```
edit some files, file3 changes somewhat orthogonal to feature X changes

```bash
   $ git add file3 file5
   $ git commit -m 'Make some super-stable changes to master'
```

Merge in the new-feature branch

```bash
   $ git merge new-feature
   $ git commit
```

Delete the new-feature branch once things are safely tested and merged,
no sense keeping old cruft around. Best practices is to only push
to origin only stable changes.

```bash
   $ git branch -d new-feature
```

### Periodically incorporate changes from master:

       Start a new feature

```bash
   $ git checkout -b long-term-feature master
```

       Do some development for a while ...

       Switch to master and sync up with origin/master

```bash
   $ git checkout master
   $ git pull origin master
```

       Switch back to development branch and merge in changes from master

```bash
   $ git checkout long-term-feature
   $ git merge master
   <edit any conflicts, 'git add' changes, commit>
```

       Continue long-term development ...

---

## Syncing repositories:

### Making connections

To list remote connections.

```bash
   $ git remote
   origin
```

Use -v for more info.

```
   $ git remote -v
   origin grs@us.navo.hpc.mil:proj/PAT (fetch)
   origin grs@us.navo.hpc.mil:proj/PAT (push)
```

The name, this case origin, can be used as a short cut to the other
repository in git commands. The name origin is the default name given to
the reprository you clone from, otherwise there is nothing special about
it.

To add a remote connection

```bash
   $ git remote add ethel emurtz@devel.desilu.com:repo/iHateFred
```

To remove a connection

```bash
   $ git remote rm fred
```

To rename connection

```bash
   $ git remote rename ricky lucy
```

Developers need to pull upstream commits to their local repository and
push local commits to other repositories. Having connections to other
individual developers makes it possible to collaborate outside of the
main (or blessed) repository. This can be very useful for a small teams
working on a large subproject.

### Have existing branch track a remote branch

To make an existing branch track a remote branch, as of GIT 1.8.0

```bash
   $ git checkout bar
   $ git branch -u upstream/foo
```

Now your local branch bar is tracking the branch foo on the remote repo
upstream.

If you are on a branch other than bar,

```bash
   $ git branch -u upstream/foo bar
```

if you like longer options, in 1.8.0+ you can do

```bash
   $ git branch --set-upstream-to=upstream/foo bar
```

To keep things simple, best practices is to rename upstream to origin
or origin-foo and rename your local branch bar to foo.

### Use GIT fetch to *sneak a peak*

Use `fetch` to import commits into the tracking branch for the remote
repository. This gives you a chance to review changes before integrating
them into the local copy of the project.

```bash
   $ git fetch origin
   $ git diff origin
```

or with more fidelity

```bash
   $ git switch someBranch
   $ git fetch origin someBranch
   $ git diff origin/someBranch
```

Use `git branch` to view local branches and use `git branch -r` for
remote branches. Inspect these branches with the ususal git chechout and
git log commands. Be aware that checking out a tracking branch will
leave you in a "detached head" state.

---

## Push and Pull

Use `git push` to merge local changes into remote branches located on
remote repos.

Use `git pull` to merge in remote changes. This command syncs the local
tracking branch with the remote branch. Then it merges the local
tracking branch into the local branch. The `git pull` command is
shorthand for `git fetch` followed by `git merge FETCH_HEAD`

Remember, pushing and pulling are between different repositories.
Merging happend between branches on the same repository. Both `git push`
and `get pull` use `git merge` behind the scenes.

### GIT push and pull

```bash
   $ git pull origin someBranch
```

To pull from the branch you are tracking with the current branch, use

```bash
   $ git pull
```

**Warning!!!**

```bash
   $ git pull origin someBranch
```

will pull in from the remote "someBranch" even if you are on the local
"someOtherBranch".

Best practice is:

```bash
   $ git fetch origin  # So local repo knows of remote changes.
   $ git status        # So we are sure we are where we think.
                         # This will also give other potentially
                         # useful info to keep yourself from
                         # shooting yourself in foot.
   $ git pull
```

Now, `git status` may tell us we can't just do a fast foreward. In
that case, create a new local branch tracking the remote branch. That
way you have two places to morph before doing a `git merge`.

The command

```bash
   $ git pull
```

which actually is the same as

```bash
   $ git fetch
   $ git merge FETCH_HEAD
```

which accmplishes same thing as doing

```bash
   $ git fetch origin         # Sync local copy of master
                              # with master on origin.
   $ git merge origin/master    # Merge local copy from last
                                # fetch from origin into your
                                # local version of master.
```

or, assuming master tracks origin/master

```bash
   $ git fetch
   $ git merge origin
```

What you are actually merging into your working directory is
a local copy of the remote repo. You are "tracking" the local
copy. If pull requires a password, then the fetch will too, but
not the merge. The `git fetch` is syncing your local copy with what
is on the remote.

More explicitly,

```bash
   git checkout master         # or whatever branch
   git fetch origin            # fetch all tracked upstream changes
   git merge origin/master     # local copy of upstream branch
```

To push local changes elsewhere

```bash
   $ git push origin someBranch
   $ git push origin --all
   $ git push origin --tags
```

The commands (with nothing else)

```bash
   $ git push
   $ git push origin
```

should be avoided on older versions of GIT. On git 1.9.5, it will
push all tracked branches to their remotes. In later versions of
git, it will just push whatever branch you are currently on.

To remove a remote branch or tag, do

```bash
   git push --delete Remote_Name Branch_or_Tag_Name
```

On older versions of GIT

```bash
   $ git push Remote_Name :Branch_Name
```

note the space before the colen, similar to renaming a branch,
you are pushing "nothing" into BranchName.

To push a tag,

```bash
   $ git push <RemoteName> <TagName>
```

to push all tags

```bash
   $ git push <RemoteName> --tag
```

### GIT push to GITHUB

Cloning from GITHUB sets the URL to origin as

```bash
   $ git config remote.origin.url
   https://github.com/grscheller/scheller-linux-environment
```

On Arch Linux 'git push' prompts for username and password and
everything works fine. On CentOS 6.8 (git version 1.7.1) I get the
error:

```bash
   $ git push
   error: The requested URL returned error: 403 Forbidden while accessing https://github.com/grscheller/scheller-linux-environment/info/refs
   fatal: HTTP request failed
```

To fix this, set the URL to

```bash
   $ git config remote.origin.url https://grscheller@github.com/grscheller/scheller-linux-environment.git
```

Not recommended, but setting it to
https://grscheller:MYPASSWORD@github.com/grscheller/scheller-linux-environment.git
would allow you to not have to type your password.

I think the issue is because of the really olde version of git
that CentOS 6.8 uses, but it might also be related to firewall
proxy issues.

#### Interesting factoid:

When I changed the name of a repo from
scheller-linux-environment to scheller-linux-archive,
above value for remote.origin.url still worked, but
changing the repo name part of it to something random
like, like scheller-linux-foofoo, failed.

### Using SSH with GITHUB:

From the GITHUB website, go to `Settings -> SSH and GPG keys`

Paste contents of ~/.ssh/id_rsa to the SSH text box. Leave
off system name at end. No newlines.

Next, from the repo, tell git to use ssh protocal

```bash
   $ git remote set-url origin git@github.com:grscheller/scheller-linux-archive
```

       Now,

```bash
   $ git remote -v
   origin     git@github.com:grscheller/scheller-linux-archive (fetch)
   origin     git@github.com:grscheller/scheller-linux-archive (push)
```

       Able to push to GITHUB without the password.

       I created a shell alias to restart the ssh-keyserver if it should
       ever get hung.

```bash
   $ alias addkey
   alias addkey='eval $(ssh-agent) && ssh-add'
```

---

## Tagging:

#### List available tags.

To list all current tags in alphabetical order

```bash
   $ git tag
```

To refine the search

```bash
   $ git tag -l 'v1.8.*'
   $ git tag -l '*foo*'
```

#### Creating Tags.

There are two types of tags, lightweight and annotated. A lightweight
tag is like a branch that does not change, just a pointer to a specific
commit. Annotated tags are stored as full objects in GIT database. They
have the tagger's name, e-mail address, date, and can be signed with GNU
Privacy Guard.

To create an annotated tag, use -a

```bash
   $ git tag -a v1.0 -m 'Original DVS code gotten from David Steller'
```

#### Pushing tags

By default, git push does not push tags. To explicitly push a tag

```bash
   $ git push origin v1.0
```

To push all available tags, use the --tags option on git push

```bash
   $ git push origin --tags
```

### Checking out tags.

To create a new branch at a specific tag, say v2.2.0

```bash
   $ git checkout -b version2-2 v2.2.0
```

#### Deleting tags (local and remote).

To delete local tag:

```bash
   $ git tag -d tag_to_delete
```

To delete off of a remote repo:

```bash
   $ git push origin :refs/tags/tag_to_delete
```

#### Show info about a tag

Use the show cmd

```bash
   $ git show tag_name
```

---

## Examining previous versions of a file:

### GIT show

Use 'git show' to view a previous version of file

```bash
   $ git show REVISION:path/to/file
```

for example

```bash
   $ git show HEAD~3:PAT_Files/mfiles/FilterValuesPanel.m
```

will send the third version back of the file to a pager.

```bash
   $ git show HEAD~1:pat.m > junk
```

will dump it all to a file called junk.

```bash
   $ git show HEAD@{2022-03-30}:./config/nvim/lua/grs/init.lua
```

will show the version of the file as of that date.

```bash
   $ git show HEAD@{2022-03-30}:./config/nvim/lua/grs|cat
   tree HEAD@{2022-03-30}:./config/nvim/lua/grs

   Colorscheme.lua
   Completions.lua
   DevEnv.lua
   Options.lua
   Packer.lua
   Telescope.lua
   TextEdit.lua
   Treesitter.lua
   WhichKey.lua
   init.lua
```

which lists the files in that directory as of that date.

```bash
   $ git show HEAD@{2022-06-4}
```

will list the commit messages and diffs for that day.

### GIT log

Use 'git log' to view the revision history a of file

```bash
   $ git log -p --follow config/nvim/init.lua
```

The `-p` tells git to show all patch information, the `--follow` to
follow the history even in the event that the file name was changed.

---

## GIT Revision History Tools:

### GIT log

#### To show commit comments for history

```bash
   $ git log
```

#### To shows history of both commits A and B (a Union)

```bash
   $ git log A B
```

#### To include diff info with commit comments

```bash
   $ git log -p
```

#### First line summaries of commit messages

```bash
   $ git log --oneline
```

To get the most out of git log, remember the
seven rules of a great GIT commit message:

* Separate subject from body with a blank line
* Limit the subject line to 50 characters
* Capitalize the subject line
* Do not end the subject line with a period
* Use the imperative mood in the subject line
* Wrap the body at 72 characters
* Use the body to explain what and why vs. how

Above taken from this
[blog post](https://chris.beams.io/posts/git-commit/).

### GIT diff

Let's say we have three branches, main, feature1 and feature2.

First, lets see how to compare two specific files in two specific
branches against each other. Output will be similar to the Unix diff
command.

```bash
   $ git diff feature1:file1 feature2:file2
```

The line between the files which are the same will be colored white, red
lines will be the items in file1 not in file2, green lines will be for
items in file2 not in file1. Left red, Right green. The red lines will
begin with a `-`, the green lines a `+`.

Compare two branches. If you leave off the `:`, git usually guesses
correctly what you mean. I like to be non-ambiguous.

```bash
   $ git diff main: feature1:
```

Comparing past version with what is in the working directory

```bash
   $ git diff HEAD~2:DVS.m DVS.m
```

Note that 2 revisions ago need not necessarily be on the same branch!

To comparing a file between branches

```bash
   $ git diff new_feature:DVS.m master:DVS.m
```

If given only one argument, compare with the working directory.

### GIT Ranges

The range `A..B` will be resolved to show each commit individually from
`A` to `B`. Files can be specified via syntax `commit:path/to/file`.
Pass it a directory, it will show info of the last commit changing in
that directory.

#### git range

The range `A...B` means every commit reachable by `A` or `B` but not both.

* for `git log` will show commits it makes when used with divergent branches
* for `git diff` it is syntactic sugar for `git diff $(git merge-base A B) B`
* for `git show` will show commit info for each single commit in that range

#### git merge-base

Find the best common ancestor for a three way merge.

#### Find all the "roots" of a repo,

```bash
   $ git rev-list --max-parents=0 HEAD
   4a0c3cff3b6d192effeb44bc2d8429c5e9f85825
   6e886a1b01a10f39b53bf8b90dba4c73625f4353
```

#### Finding the first commit a file appeared

```bash
   $ git log --oneline -- Actor.scala | tail -n 1
   d71adde Implemented book's nonblocking fpinscala.parallelism package.
   Blocking version now package fpinscala.parallelism.javaFutures.
```

Then search for equivalent of "d71adde" in a git log.

---

## Using Neovim as GIT Pager:

You need to turn color off in GIT and let nvim do colorization.

```bash
   $ git config --global color.pager no
   $ git config --global core.pager 'nvim -R'
```

The `-R` means read-only.

---

## GIT Stash:

Store away current state of working directory and the index and goes
back to a clean working directory:

```bash
   $ git stash push
```

or just

```bash
   $ git stash
```

Drop the last stash entry from the list of stash entries:

```bash
   $ git drop
```

Pop changes back into working directory, perhaps on a subsequent commit

```bash
   $ git stash pop
```

This may fail due to conflicts due to changes being applied at the
commit that was HEAD at the time of the stash. Resolve the conflict
and use git drop.

Remove all stash entries:

```bash
   $ git stash clear
```

  Note, entries will be subject to pruning, so there may be no way to
  recover these changes.

  Typical workflow when following a remote repo:

```bash
   $ git stash
   $ git pull
   $ git stash pop
```

---

## TODO:

**TODO:** Merge the following in the following

To create a temporary branch and push it upstream.

```bash
   $ git branch tmpWork
   $ git stash
   $ git checkout tmpWork
   $ git stash apply             # stash apply ???
   $ git add .
   $ git commit
   $ git push -u origin tmpWork
   $ git status
   On branch tmpwork
   Your branch is up to date with 'origin/tmpWork'.

   nothing to commit, working tree clean
```

**TODO:** Explain the difference between `git checkout branch`
and `git switch branch`. The first is a bit overloaded.

**TODO:** Add info on `git blame`

**TODO:** Add info on `rerere`

**TODO:** Add info on `git maintenance`

**TODO:** Add info on `git diff --cache`

**TODO:** Look into git-rev-list and git-show-branch

**TODO:** Give examples og Git Ranges in use

**TODO:** Update GitHub access
