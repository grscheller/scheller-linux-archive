# Jupyter Lab

I think the best use case for this technology is for visualization,
presentation, and as an IDE. Primarily I am interested in the first two.

Its debugger reminds me of the Matlab debugger. Debugging is only
supported with certain "kernels" where the
[ipykernel](https://github.com/ipython/ipykernel) is one and gets
installed by default.

Jupyter kernels allow you to use Jupyter interfaces and tools with
multiple programming languages.

#### Links

* [Jupyter project page](https://jupyter.org/)
* [JupyterLab documentation](https://jupyterlab.readthedocs.io/en/latest/user/index.html)
* [Jupyter GitHub page](https://github.com/jupyter)

## How to install

Can be done with just pip and a virtual environment.

* goto where you want the virtual environment
* create the virtual environment
* change to the the (jupyter_learn)
* see what is there
* update base environment
* see what is there
* install JupyterLab
* see what got installed

```fish
   $ cd ~/devel/python_envs
   $ python -m venv jupyter_learn
   $ ve jupyter_learn
   $ pip list
   Package    Version
   ---------- -------
   pip        24.0
   setuptools 65.5.0
   $ pip install --upgrade pip setuptools
   $ pip list
   Package    Version
   ---------- -------
   pip        24.0
   setuptools 69.1.1
   $ pip install jupyterlab
   $ pip list
   Package                   Version
   ------------------------- ---------------
   anyio                     4.3.0
   argon2-cffi               23.1.0
   argon2-cffi-bindings      21.2.0
   arrow                     1.3.0
   asttokens                 2.4.1
   async-lru                 2.0.4
   attrs                     23.2.0
   Babel                     2.14.0
   beautifulsoup4            4.12.3
   bleach                    6.1.0
   certifi                   2024.2.2
   cffi                      1.16.0
   charset-normalizer        3.3.2
   comm                      0.2.1
   debugpy                   1.8.1
   decorator                 5.1.1
   defusedxml                0.7.1
   executing                 2.0.1
   fastjsonschema            2.19.1
   fqdn                      1.5.1
   h11                       0.14.0
   httpcore                  1.0.4
   httpx                     0.27.0
   idna                      3.6
   ipykernel                 6.29.3
   ipython                   8.22.2
   isoduration               20.11.0
   jedi                      0.19.1
   Jinja2                    3.1.3
   json5                     0.9.22
   jsonpointer               2.4
   jsonschema                4.21.1
   jsonschema-specifications 2023.12.1
   jupyter_client            8.6.0
   jupyter_core              5.7.1
   jupyter-events            0.9.0
   jupyter-lsp               2.2.4
   jupyter_server            2.13.0
   jupyter_server_terminals  0.5.2
   jupyterlab                4.1.4
   jupyterlab_pygments       0.3.0
   jupyterlab_server         2.25.4
   MarkupSafe                2.1.5
   matplotlib-inline         0.1.6
   mistune                   3.0.2
   nbclient                  0.9.0
   nbconvert                 7.16.2
   nbformat                  5.9.2
   nest-asyncio              1.6.0
   notebook_shim             0.2.4
   overrides                 7.7.0
   packaging                 24.0
   pandocfilters             1.5.1
   parso                     0.8.3
   pexpect                   4.9.0
   pip                       24.0
   platformdirs              4.2.0
   prometheus_client         0.20.0
   prompt-toolkit            3.0.43
   psutil                    5.9.8
   ptyprocess                0.7.0
   pure-eval                 0.2.2
   pycparser                 2.21
   Pygments                  2.17.2
   python-dateutil           2.9.0.post0
   python-json-logger        2.0.7
   PyYAML                    6.0.1
   pyzmq                     25.1.2
   referencing               0.33.0
   requests                  2.31.0
   rfc3339-validator         0.1.4
   rfc3986-validator         0.1.1
   rpds-py                   0.18.0
   Send2Trash                1.8.2
   setuptools                69.1.1
   six                       1.16.0
   sniffio                   1.3.1
   soupsieve                 2.5
   stack-data                0.6.3
   terminado                 0.18.0
   tinycss2                  1.2.1
   tornado                   6.4
   traitlets                 5.14.1
   types-python-dateutil     2.8.19.20240311
   uri-template              1.3.0
   urllib3                   2.2.1
   wcwidth                   0.2.13
   webcolors                 1.13
   webencodings              0.5.1
   websocket-client          1.7.0
```

## Commandline options

```fish
   $ jupyter --version
   Selected Jupyter core packages...
   IPython          : 8.22.2
   ipykernel        : 6.29.3
   ipywidgets       : not installed
   jupyter_client   : 8.6.0
   jupyter_core     : 5.7.1
   jupyter_server   : 2.13.0
   jupyterlab       : 4.1.4
   nbclient         : 0.9.0
   nbconvert        : 7.16.2
   nbformat         : 5.9.2
   notebook         : not installed
   qtconsole        : not installed
   traitlets        : 5.14.1

   $ jupyter --paths
   config:
       /home/grs/devel/python_envs/jupyter_learn/etc/jupyter
       /home/grs/.jupyter
       /usr/local/etc/jupyter
       /etc/jupyter
   data:
       /home/grs/devel/python_envs/jupyter_learn/share/jupyter
       /home/grs/.local/share/jupyter
       /usr/local/share/jupyter
       /usr/share/jupyter
   runtime:
       /home/grs/.local/share/jupyter/runtime

   $ jupyter --config-dir
   /home/grs/.jupyter

   $ jupyter --data-dir
   /home/grs/.local/share/jupyter

   $ jupyter --runtime-dir
   /home/grs/.local/share/jupyter/runtime

   $ jupyter
   usage: jupyter [-h] [--version] [--config-dir] [--data-dir] [--runtime-dir]
                  [--paths] [--json] [--debug]
                  [subcommand]

   Jupyter: Interactive Computing

   positional arguments:
     subcommand     the subcommand to launch

   options:
     -h, --help     show this help message and exit
     --version      show the versions of core jupyter packages and exit
     --config-dir   show Jupyter config dir
     --data-dir     show Jupyter data dir
     --runtime-dir  show Jupyter runtime dir
     --paths        show all Jupyter paths. Add --json for machine-readable format.
     --json         output paths as machine-readable json
     --debug        output debug information about paths

   Available subcommands: dejavu events execute kernel kernelspec lab labextension
   labhub migrate nbconvert run server troubleshoot trust

   Please specify a subcommand or one of the optional arguments.
```

## How to use

### First attempt

The documentation doesn't say where to point your browser.

```fish
   $ jupyter run
   0.00s - Debugger warning: It seems that frozen modules are being used, which may
   0.00s - make the debugger miss breakpoints. Please pass -Xfrozen_modules=off
   0.00s - to python to disable frozen modules.
   0.00s - Note: Debugging will proceed. Set PYDEVD_DISABLE_FILE_VALIDATION=1 to disable this validation.
   ^D
   [IPKernelApp] WARNING | Parent appears to have exited, shutting down.
```

I was hoping the server would give some sort of hint.

### Second attempt

This seems to have worked.

```fish
   $ jupyter lab
   [ServerApp] jupyter_lsp | extension was successfully linked.
   [ServerApp] jupyter_server_terminals | extension was successfully linked.
   [ServerApp] jupyterlab | extension was successfully linked.
   [ServerApp] Writing Jupyter server cookie secret to /home/grs/.local/share/jupyter/runtime/jupyter_cookie_secret
   [ServerApp] notebook_shim | extension was successfully linked.
   [ServerApp] notebook_shim | extension was successfully loaded.
   [ServerApp] jupyter_lsp | extension was successfully loaded.
   [ServerApp] jupyter_server_terminals | extension was successfully loaded.
   [LabApp] JupyterLab extension loaded from /home/grs/devel/python_envs/jupyter_learn/lib/python3.11/site-packages/jupyterlab
   [LabApp] JupyterLab application directory is /home/grs/devel/python_envs/jupyter_learn/share/jupyter/lab
   [LabApp] Extension Manager is 'pypi'.
   [ServerApp] jupyterlab | extension was successfully loaded.
   [ServerApp] Serving notebooks from local directory: /home/grs/devel/notes/scheller-linux-archive
   [ServerApp] Jupyter Server 2.13.0 is running at:
   [ServerApp] http://localhost:8888/lab?token=46bb4dac0b3b35532e19a299a490d69986f439a294925043
   [ServerApp]     http://127.0.0.1:8888/lab?token=46bb4dac0b3b35532e19a299a490d69986f439a294925043
   [ServerApp] Use Control-C to stop this server and shut down all kernels (twice to skip confirmation).
   [ServerApp]

       To access the server, open this file in a browser:
           file:///home/grs/.local/share/jupyter/runtime/jpserver-27242-open.html
       Or copy and paste one of these URLs:
           http://localhost:8888/lab?token=46bb4dac0b3b35532e19a299a490d69986f439a294925043
           http://127.0.0.1:8888/lab?token=46bb4dac0b3b35532e19a299a490d69986f439a294925043
   [ServerApp] Skipped non-installed server(s): dockerfile-language-server-nodejs, javascript-typescript-langserver, jedi-language-server, julia-language-server, pyright, python-language-server, python-lsp-server, r-languageserver, sql-language-server, texlab, unified-language-server, vscode-css-languageserver-bin, vscode-html-languageserver-bin, vscode-json-languageserver-bin
   [LabApp] Build is up to date
   [ServerApp] Creating new notebook in
   [ServerApp] Writing notebook-signing key to /home/grs/.local/share/jupyter/notebook_secret
   [ServerApp] Kernel started: 079ef574-f3df-4e5c-ae93-953b29769c1f
   0.00s - Debugger warning: It seems that frozen modules are being used, which may
   0.00s - make the debugger miss breakpoints. Please pass -Xfrozen_modules=off
   0.00s - to python to disable frozen modules.
   0.00s - Note: Debugging will proceed. Set PYDEVD_DISABLE_FILE_VALIDATION=1 to disable this validation.
   [ServerApp] Connecting to kernel 079ef574-f3df-4e5c-ae93-953b29769c1f.
   [ServerApp] Connecting to kernel 079ef574-f3df-4e5c-ae93-953b29769c1f.
   [ServerApp] Connecting to kernel 079ef574-f3df-4e5c-ae93-953b29769c1f.
   [ServerApp] Saving file at /Untitled.ipynb
   [ServerApp] Saving file at /Untitled.ipynb
   [ServerApp] New terminal with automatic name: 1
   [ServerApp] Kernel shutdown: 079ef574-f3df-4e5c-ae93-953b29769c1f
   [ServerApp] EOF on FD 21; stopping reading
   [ServerApp] Terminal 1 closed
   [ServerApp] Shutting down on /api/shutdown request.
   [ServerApp] Shutting down 4 extensions
```

I have suppressed the timestamps in the above.

Seems to have hijacked an open Firefox browser, opened a new tab with
the URL: `http://localhost:8888/lab` without using the above token.

Defaults to a horribly bright white theme. Choice between two bright
white themes. Was able to to configure settings to give dark version of
the default theme which seems very readable. Took a bit of hunting
around to find the settings to do this.

On the same machine, I am able to shutdown or logout the session from
the browser interface. If I log out, can log back in with the token by
going to `http://localhost:8888/login`. Connection is refused if I go to
euler7 and do `http://gauss17:8888/login`, I don't even have a chance to
enter a token.

I am on the right track. I wish to run the server off a more powerful
computer from the console and access it with a less powerful one over
the network. Also want to look into the OpenGL client.

Factoid, after shutting everything down, I was left in the directory I
ran the server and found the following:

```
   $ file Untitled.ipynb
   Untitled.ipynb: JSON text data
   $ file .ipynb_checkpoints/Untitled-checkpoint.ipynb
   .ipynb_checkpoints/Untitled-checkpoint.ipynb: JSON text data
```

So Jupyter notebooks are just JSON files!
