# Revision history for pythag-triples

## 0.1.0.0 -- 2024-05-28

* Redoing from scratch - [from Stackoverflow](https://stackoverflow.com/questions/9300286/how-do-i-build-a-simple-project-with-cabal)
  * cabal init
    * cabal update (559 days old - suggested by cabal configured)
  * cabal configure
  * cabal build

* Built with no errors

```fish
    $ ls -l (digpath 'pytha*')
    lrwxrwxrwx 1 grs grs 140 Nov 15  2022
    /home/grs/.cabal/bin/pythagTriples -> \
        ../store/ghc-9.0.2/pythag-triples-0.7.0.0-e-pythagTriples-8786af3fcdf1644642c18ac9691c599f02105c7aaab58147e2873cd102380a92/bin/pythagTriples

    $ cabal install
    Wrote tarball sdist to
    /home/grs/devel/scheller-linux-archive/grok/Haskell/pythag-triples/dist-newstyle/sdist/pythag-triples-0.1.0.0.tar.gz
    Resolving dependencies...
    Build profile: -w ghc-9.2.8 -O1
    In order, the following will be built (use -v for more details):
    - pythag-triples-0.1.0.0 (exe:pythag-triples) (requires build)
    Starting     pythag-triples-0.1.0.0 (exe:pythag-triples)
    Building     pythag-triples-0.1.0.0 (exe:pythag-triples)
    Installing   pythag-triples-0.1.0.0 (exe:pythag-triples)
    Completed    pythag-triples-0.1.0.0 (exe:pythag-triples)
    Symlinking 'pythag-triples' to '/home/grs/.cabal/bin/pythag-triples'

    $ ls -l (digpath 'pytha*')
    lrwxrwxrwx 1 grs grs 142 May 28 16:21 /home/grs/.cabal/bin/pythag-triples -> \
        ../store/ghc-9.2.8/pythag-triples-0.1.0.0-e-pythag-triples-b56e37c3f15035c859d1943ce6635285388c6714ea2cd1e9c1d86727364e140b/bin/pythag-triples
    lrwxrwxrwx 1 grs grs 140 Nov 15  2022 /home/grs/.cabal/bin/pythagTriples -> \
        ../store/ghc-9.0.2/pythag-triples-0.7.0.0-e-pythagTriples-8786af3fcdf1644642c18ac9691c599f02105c7aaab58147e2873cd102380a92/bin/pythagTriples

    $ pythag-triples 10
    Hello, Haskell!
    $ pythagTriples 10
    pythagTriples: error while loading shared libraries: libHSsplit-0.2.3.5-L1bLVmvZ58j6aPvvR7cSgS-ghc9.0.2.so: cannot open shared object file: No such file or directory
```
