### MYS2 General info
Seems to be a fork of Cygwin/MSYS/MinGW.  While Cygwin tries
to create a POSIX compliant programming environment.  MSYS2
is geared to create native Windows applications.
* Three distint environments MSYS, MINGWIN64, MINGW32
  * MSYS builds against a minimal POSIX compatibility layer
    * Default PATH:
      ```
         /usr/local/bin
         /usr/bin:/bin
         /opt/bin
         /c/Windows/System32
         /c/Windows
         /c/Windows/System32/Wbem
         /c/Windows/System32/WindowsPowerShell/v1.0/
      ```
  * MINGW64 builds native 64-bit Windows applications
    * Default PATH:
      ```
         /mingw64/bin
         /usr/local/bin
         /usr/bin:/bin
         /c/Windows/System32
         /c/Windows
         /c/Windows/System32/Wbem
         /c/Windows/System32/WindowsPowerShell/v1.0/
      ```
  * MINGW32 builds native 32-bit Windows applications
    * Default PATH:
      ```
         /mingw32/bin
         /usr/local/bin
         /usr/bin:/bin
         /c/Windows/System32
         /c/Windows
         /c/Windows/System32/Wbem
         /c/Windows/System32/WindowsPowerShell/v1.0/
      ```
* Uses Pacman as its package manager
  * Packages for each environments
    ```
       $ pacman -Ss curl
       mingw32/mingw-w64-i686-curl 7.64.0-2
           Command line tool and library for transferring data with URLs. (mingw-w64)
       mingw32/mingw-w64-i686-flickcurl 1.26-2
           Flickcurl is a C library for the Flickr API (mingw-w64)
       mingw64/mingw-w64-x86_64-curl 7.64.0-2
           Command line tool and library for transferring data with URLs. (mingw-w64)
       mingw64/mingw-w64-x86_64-flickcurl 1.26-2
           Flickcurl is a C library for the Flickr API (mingw-w64)
       msys/curl 7.64.0-2 (base) [installed]
           Multi-protocol file transfer utility
       msys/libcurl 7.64.0-2 (libraries) [installed]
           Multi-protocol file transfer library (runtime)
       msys/libcurl-devel 7.64.0-2 (development)
           Libcurl headers and librarie
    ```
  * To install into an environment
    ```
       $ pacman -S[yu] [<msys|mingw64|mingw32>/]]<package-name>
    ```
  * Actually the package name alone encodes the environment
* Git-for-Windows 
  * thinly disguised MSYS2 environment
  * GIT client built against MingW64
