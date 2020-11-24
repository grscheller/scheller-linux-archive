# Node.js

Node.js® is a JavaScript runtime built on
Chrome's V8 JavaScript engine.

* [Installation](#installation)
* [npm](#npm)
* [Simple Example](#simple-example)

## Installation

### Install on Arch

Available in community repo.

```
   $ sudo pacman -Syu nodejs npm npm-check-updates
```

### Install on OpenBSD

Available in ports collection at
/usr/ports/lang/node, or via the package manager

```
   $ pkg_add node
```

### Install on Windows

Download the Windows installer from nodejs.org,
or alternatively via Chocolatey

```
   $ cinst nodejs
```

For a full install with npm

```
   $ cinst nodejs.install
```

## npm

Originally short for Node Package Manager, npm
is a package manager for JavaScript and is the
default package manager for the JavaScript
runtime environment Node.js. It consists of a
command line client, also called npm, and an
online database of public and paid-for private
packages, called the npm registry. The registry
is accessed via the client, and the available
packages can be browsed and searched via the npm
website. The package manager and registry are
maintained by npm, Inc. a subsidiary of GitHub.

npm is written entirely in JavaScript and thus
is cross platform.

## Simple Example

The Script, [app.js](app.js), will generates a
webpage webpage running off port 3000i which
simply says "Hello World".

```
   $ node app.js
   Server running at http://192.168.1.22:3000/
   ^C
```

Replace the IP given in app.js with the one of the
machine you run the web service on, or just
`localhost`.  Using localhost will only make the
webpage available to browsers running on the machine
node is running.
