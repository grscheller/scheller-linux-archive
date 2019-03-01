## Deep Learning with Python using PyTorch and Anaconda
To be consistent with what I am doing professionally, I am
using Anaconda instead of the Arch Linux Python.
* Initially Following the
  [PyTorch](https://pytorch.org/)
  website.
* Other useful information is on the
  [GitHub PyTourch](https://github.com/pytorch/pytorch)
  website.
* PyTorch has a unique way of building neural networks: using and replaying a tape recorder. 
* See post 2018-02-14 entries in 
  [ArchLinuxAdminLogs](../../../notes/ArchLinuxAdminLogs/ArchLinuxAdmin.log)
  for the Anaconda setup.

### Installing non-CUDA version on CentOS 7:
```
   $ conda create --name pytorch -c pytorch pytorch-cpu torchvision-cpu
   Collecting package metadata: done
   Solving environment: done

   ## Package Plan ##

     environment location: /home/schelleg/opt/anaconda3/envs/pytorch

     added / updated specs:
       - pytorch-cpu
       - torchvision-cpu

   The following packages will be downloaded:

       package                    |            build
       ---------------------------|-----------------
       openssl-1.1.1b             |       h7b6447c_0         4.0 MB
       pytorch-cpu-1.0.1          |      py3.7_cpu_2        26.8 MB  pytorch
       torchvision-cpu-0.2.1      |             py_2          37 KB  pytorch
       ------------------------------------------------------------
                                              Total:        30.8 MB

   The following NEW packages will be INSTALLED:

     blas               pkgs/main/linux-64::blas-1.0-mkl
     ca-certificates    pkgs/main/linux-64::ca-certificates-2019.1.23-0
     certifi            pkgs/main/linux-64::certifi-2018.11.29-py37_0
     cffi               pkgs/main/linux-64::cffi-1.12.1-py37h2e261b9_0
     freetype           pkgs/main/linux-64::freetype-2.9.1-h8a8886c_1
     intel-openmp       pkgs/main/linux-64::intel-openmp-2019.1-144
     jpeg               pkgs/main/linux-64::jpeg-9b-h024ee3a_2
     libedit            pkgs/main/linux-64::libedit-3.1.20181209-hc058e9b_0
     libffi             pkgs/main/linux-64::libffi-3.2.1-hd88cf55_4
     libgcc-ng          pkgs/main/linux-64::libgcc-ng-8.2.0-hdf63c60_1
     libgfortran-ng     pkgs/main/linux-64::libgfortran-ng-7.3.0-hdf63c60_0
     libpng             pkgs/main/linux-64::libpng-1.6.36-hbc83047_0
     libstdcxx-ng       pkgs/main/linux-64::libstdcxx-ng-8.2.0-hdf63c60_1
     libtiff            pkgs/main/linux-64::libtiff-4.0.10-h2733197_2
     mkl                pkgs/main/linux-64::mkl-2019.1-144
     mkl_fft            pkgs/main/linux-64::mkl_fft-1.0.10-py37ha843d7b_0
     mkl_random         pkgs/main/linux-64::mkl_random-1.0.2-py37hd81dba3_0
     ncurses            pkgs/main/linux-64::ncurses-6.1-he6710b0_1
     ninja              pkgs/main/linux-64::ninja-1.8.2-py37h6bb024c_1
     numpy              pkgs/main/linux-64::numpy-1.15.4-py37h7e9f1db_0
     numpy-base         pkgs/main/linux-64::numpy-base-1.15.4-py37hde5b4d6_0
     olefile            pkgs/main/linux-64::olefile-0.46-py37_0
     openssl            pkgs/main/linux-64::openssl-1.1.1b-h7b6447c_0
     pillow             pkgs/main/linux-64::pillow-5.4.1-py37h34e0f95_0
     pip                pkgs/main/linux-64::pip-19.0.3-py37_0
     pycparser          pkgs/main/linux-64::pycparser-2.19-py37_0
     python             pkgs/main/linux-64::python-3.7.2-h0371630_0
     pytorch-cpu        pytorch/linux-64::pytorch-cpu-1.0.1-py3.7_cpu_2
     readline           pkgs/main/linux-64::readline-7.0-h7b6447c_5
     setuptools         pkgs/main/linux-64::setuptools-40.8.0-py37_0
     six                pkgs/main/linux-64::six-1.12.0-py37_0
     sqlite             pkgs/main/linux-64::sqlite-3.26.0-h7b6447c_0
     tk                 pkgs/main/linux-64::tk-8.6.8-hbc83047_0
     torchvision-cpu    pytorch/noarch::torchvision-cpu-0.2.1-py_2
     wheel              pkgs/main/linux-64::wheel-0.33.1-py37_0
     xz                 pkgs/main/linux-64::xz-5.2.4-h14c3975_4
     zlib               pkgs/main/linux-64::zlib-1.2.11-h7b6447c_3
     zstd               pkgs/main/linux-64::zstd-1.3.7-h0b5b093_0

   Proceed ([y]/n)? y

   Downloading and Extracting Packages
   pytorch-cpu-1.0.1    | 26.8 MB   | ##################################### | 100% 
   openssl-1.1.1b       | 4.0 MB    | ##################################### | 100% 
   torchvision-cpu-0.2. | 37 KB     | ##################################### | 100% 
   Preparing transaction: done
   Verifying transaction: done
   Executing transaction: done
   #
   # To activate this environment, use
   #
   #     $ conda activate pytorch
   #
   # To deactivate an active environment, use
   #
   #     $ conda deactivate
```

### Installing on CUDA enabled Arch Linux:
```
   $ conda create --name pytorch -c pytorch pytorch torchvision
   Collecting package metadata: done
   Solving environment: done

   ==> WARNING: A newer version of conda exists. <==
     current version: 4.6.3
     latest version: 4.6.7

   Please update conda by running

    $ conda update -n base -c defaults conda

    ## Package Plan ##

      environment location: /home/geoff/opt/anaconda3/envs/pytorch

      added / updated specs:
        - pytorch
        - torchvision

    The following packages will be downloaded:

        package                    |            build
        ---------------------------|-----------------
        cffi-1.12.1                |   py37h2e261b9_0         220 KB
        cudatoolkit-10.0.130       |                0       380.0 MB
        mkl_fft-1.0.10             |   py37ha843d7b_0         169 KB
        openssl-1.1.1b             |       h7b6447c_0         4.0 MB
        pip-19.0.3                 |           py37_0         1.8 MB
        pytorch-1.0.1              |py3.7_cuda10.0.130_cudnn7.4.2_2       375.4 MB  pytorch
        setuptools-40.8.0          |           py37_0         643 KB
        torchvision-0.2.1          |             py_2          37 KB  pytorch
        wheel-0.33.1               |           py37_0          39 KB
        ------------------------------------------------------------
                                               Total:       762.3 MB

    The following NEW packages will be INSTALLED:

      blas               pkgs/main/linux-64::blas-1.0-mkl
      ca-certificates    pkgs/main/linux-64::ca-certificates-2019.1.23-0
      certifi            pkgs/main/linux-64::certifi-2018.11.29-py37_0
      cffi               pkgs/main/linux-64::cffi-1.12.1-py37h2e261b9_0
      cudatoolkit        pkgs/main/linux-64::cudatoolkit-10.0.130-0
      freetype           pkgs/main/linux-64::freetype-2.9.1-h8a8886c_1
      intel-openmp       pkgs/main/linux-64::intel-openmp-2019.1-144
      jpeg               pkgs/main/linux-64::jpeg-9b-h024ee3a_2
      libedit            pkgs/main/linux-64::libedit-3.1.20181209-hc058e9b_0
      libffi             pkgs/main/linux-64::libffi-3.2.1-hd88cf55_4
      libgcc-ng          pkgs/main/linux-64::libgcc-ng-8.2.0-hdf63c60_1
      libgfortran-ng     pkgs/main/linux-64::libgfortran-ng-7.3.0-hdf63c60_0
      libpng             pkgs/main/linux-64::libpng-1.6.36-hbc83047_0
      libstdcxx-ng       pkgs/main/linux-64::libstdcxx-ng-8.2.0-hdf63c60_1
      libtiff            pkgs/main/linux-64::libtiff-4.0.10-h2733197_2
      mkl                pkgs/main/linux-64::mkl-2019.1-144
      mkl_fft            pkgs/main/linux-64::mkl_fft-1.0.10-py37ha843d7b_0
      mkl_random         pkgs/main/linux-64::mkl_random-1.0.2-py37hd81dba3_0
      ncurses            pkgs/main/linux-64::ncurses-6.1-he6710b0_1
      ninja              pkgs/main/linux-64::ninja-1.8.2-py37h6bb024c_1
      numpy              pkgs/main/linux-64::numpy-1.15.4-py37h7e9f1db_0
      numpy-base         pkgs/main/linux-64::numpy-base-1.15.4-py37hde5b4d6_0
      olefile            pkgs/main/linux-64::olefile-0.46-py37_0
      openssl            pkgs/main/linux-64::openssl-1.1.1b-h7b6447c_0
      pillow             pkgs/main/linux-64::pillow-5.4.1-py37h34e0f95_0
      pip                pkgs/main/linux-64::pip-19.0.3-py37_0
      pycparser          pkgs/main/linux-64::pycparser-2.19-py37_0
      python             pkgs/main/linux-64::python-3.7.2-h0371630_0
      pytorch            pytorch/linux-64::pytorch-1.0.1-py3.7_cuda10.0.130_cudnn7.4.2_2
      readline           pkgs/main/linux-64::readline-7.0-h7b6447c_5
      setuptools         pkgs/main/linux-64::setuptools-40.8.0-py37_0
      six                pkgs/main/linux-64::six-1.12.0-py37_0
      sqlite             pkgs/main/linux-64::sqlite-3.26.0-h7b6447c_0
      tk                 pkgs/main/linux-64::tk-8.6.8-hbc83047_0
      torchvision        pytorch/noarch::torchvision-0.2.1-py_2
      wheel              pkgs/main/linux-64::wheel-0.33.1-py37_0
      xz                 pkgs/main/linux-64::xz-5.2.4-h14c3975_4
      zlib               pkgs/main/linux-64::zlib-1.2.11-h7b6447c_3
      zstd               pkgs/main/linux-64::zstd-1.3.7-h0b5b093_0

    Proceed ([y]/n)? y

    Downloading and Extracting Psource /cygdrive/c/Users/scotch/Anaconda3/etc/profile.d/conda.shackages
    pytorch-1.0.1        | 375.4 MB  | ##################################### | 100% 
    pip-19.0.3           | 1.8 MB    | ##################################### | 100% 
    mkl_fft-1.0.10       | 169 KB    | ##################################### | 100% 
    cudatoolkit-10.0.130 | 380.0 MB  | ##################################### | 100% 
    setuptools-40.8.0    | 643 KB    | ##################################### | 100% 
    openssl-1.1.1b       | 4.0 MB    | ##################################### | 100% 
    cffi-1.12.1          | 220 KB    | ##################################### | 100% 
    wheel-0.33.1         | 39 KB     | ##################################### | 100% 
    torchvision-0.2.1    | 37 KB     | ##################################### | 100% 
    Preparing transaction: done
    Verifying transaction: done
    Executing transaction: done
    #
    # To activate this environment, use
    #
    #     $ conda activate pytorch
    #
    # To deactivate an active environment, use
    #
    #     $ conda deactivate
```

### Immediately upgrading what I just installed on Arch Linux:
```
   $ conda activate pytorch
   (pytorch)$ conda update --all
   Collecting package metadata: done
   Solving environment: done

   ## Package Plan ##

     environment location: /home/geoff/opt/anaconda3/envs/pytorch

   The following packages will be downloaded:

       package                    source /cygdrive/c/Users/scotch/Anaconda3/etc/profile.d/conda.sh|            build
       ---------------------------|-----------------
       cudnn-7.3.1                |       cuda10.0_0       385.1 MB
       pytorch-1.0.1              |cuda100py37he554f03_0       344.6 MB
       ------------------------------------------------------------

   The following NEW packages will be INSTALLED:

     cudnn              pkgs/main/linux-64::cudnn-7.3.1-cuda10.0_0

   The following packages will be SUPERSEDED by a higher-priority channel:

     pytorch            pytorch::pytorch-1.0.1-py3.7_cuda10.0~ --> pkgs/main::pytorch-1.0.1-cuda100py37he554f03_0

   Proceed ([y]/n)? y

   Downloading and Extracting Packages(base) C:\Users\scotch>conda update conda
Solving environment: done

## Package Plan ##

  environment location: C:\Users\scotch\Anaconda3

  added / updated specs:
    - conda


The following packages will be downloaded:

    package                    |            build
    ---------------------------|-----------------
    conda-4.6.7                |           py37_0         1.7 MB

The following packages will be UPDATED:

    conda: 4.5.12-py37_0 --> 4.6.7-py37_0

Proceed ([y]/n)?


Downloading and Extracting Packages
conda-4.6.7          | 1.7 MB    | ############################################################################ | 100%
Preparing transaction: done
Verifying transaction: done
Executing transaction: done

(base) C:\Users\scotch>conda env list
# conda environments:
#
base                  *  C:\Users\scotch\Anaconda3
play1                    C:\Users\scotch\Anaconda3\envs\play1


(base) C:\Users\scotch>
   pytorch-1.0.1        | 344.6 MB  | #################################### | 100% 
   cudnn-7.3.1          | 385.1 MB  | #################################### | 100% 
   Preparing transaction: done
   Verifying transaction: done
   Executing transaction: done
```
Seems to have installed cudnn-7.3.source /cygdrive/c/Users/scotch/Anaconda3/etc/profile.d/conda.sh1 but is this consistent with line #167 above?
Lets try again,
```
   (pytorch)$ conda update --all
   Collecting package metadata: done
   Solving environment: done

   # All requested packages already installed.
```
Now try again while indicating the channel too,
```
   (pytorch)$ conda update -c pytorch --all
   Collecting package metadata: done
   Solving environment: done

   ## Package Plan ##

     environment location: /home/geoff/opt/anaconda3/envs/pytorch

   The following packages will be downloaded:

       package                    |            build
       ---------------------------source /cygdrive/c/Users/scotch/Anaconda3/etc/profile.d/conda.sh|-----------------
       torchvision-0.2.2          |             py_2          44 KB  pytorch
       tqdm-4.31.1                |             py_0          47 KB
       ------------------------------------------------------------
                                              Total:          91 KB

   The following NEW packages will be INSTALLED:

     tqdm               pkgs/main/noarch::tqdm-4.31.1-py_0

   The following packages will be UPDATED:

     pytorch            pkgs/main::pytorch-1.0.1-cuda100py37h~ --> pytorch::pytorch-1.0.1-py3.7_cuda10.0.130_cudnn7.4.2_2
     torchvision                                    0.2.1-py_2 --> 0.2.2-py_2

   Proceed ([y]/n)? 

   Downloading and Extracting Packasource /cygdrive/c/Users/scotch/Anaconda3/etc/profile.d/conda.shges
   torchvision-0.2.2    | 44 KB     | ############################################################ | 100% 
   tqdm-4.31.1          | 47 KB     | ############################################################ | 100% 
   Preparing transaction: done
   Verifying transaction: done
   Executing transaction: done
```
My guess is that this time the version of pytorch built with cudnn 7.4.2 is used.

### Installing on CUDA enabled Windows 10-64bit:
Using the "GUI" installer, choose not to put Python on Path.  Also installed
the Visual Studio VSCode.  Free  as in "Anaconda partner with MS."

Installed to C:\Users\scotch\Anaconda3

Updated end of my Cygwin `.bashrc` to be
```
   ## Configure Anaconda3 Python Distribution
   if [[ -d ~/opt/anaconda3 ]]
   then
       source ~/opt/anaconda3/etc/profile.d/conda.sh
   elif [[ -d /cygdrive/c/Users/scotch/Anaconda3 ]]
   then
       source /cygdrive/c/Users/scotch/Anaconda3/etc/profile.d/conda.sh
   fi
```
Now lets see if conda works in Cygwin environment,
```
   $ conda create --name play1
   Solving environment: ...working... done

   CondaHTTPError: HTTP 000 CONNECTION FAILED for url <https://repo.anaconda.com/pkgs/pro/noarch/repodata.json.bz2>
   Elapsed: -

   An HTTP error occurred when trying to retrieve this URL.
   HTTP errors are often intermittent, and a simple retry will get you on your way.

   If your current network has https://www.anaconda.com blocked, please file
   a support request with your network engineering team.

   SSLError(MaxRetryError('HTTPSConnectionPool(host=\'repo.anaconda.com\', port=443): Max retries exceeded with url: /pkgs/pro/noarch/repodata.json.bz2 (Caused by SSLError("Can\'t connect to HTTPS URL because the SSL module is not available."))'))
```

OK, lets see if Conda GUI environment suffers from the same problem,
launched `ANACONDA NAVIGATOR` from taskbar menu.
* Horrible bright white with light gray lettering (can't read)
* Able to launch a cmd.exe window
```
   (base) C:\Users\scotch>conda create --name play1
   Solving environment: done

   ==> WARNING: A newer version of conda exists. <==
     current version: 4.5.12
     latest version: 4.6.7

   Please update conda by running

       $ conda update -n base -c defaults conda

   ## Package Plan ##

      environment location: C:\Users\scotch\Anaconda3\envs\play1

      Proceed ([y]/n)?

      Preparing transaction: done
      Verifying transaction: done
      Executing transaction: done
```
Let's see if we can create a virtual environment,
```
   (base) C:\Users\scotch>conda activate play1

   (play1) C:\Users\scotch>
```(play1) C:\Users\scotch>deactivate

   (base) C:\Users\scotch>
```
Upgrade conda,
``
   (base) C:\Users\scotch>conda update conda
   Solving environment: done

   ## Package Plan ##

     environment location: C:\Users\scotch\Anaconda3

     added / updated specs:
       - conda

   The following packages will be downloaded:

       package                    |            build
       ---------------------------|-----------------
       conda-4.6.7                |           py37_0         1.7 MB

   The following packages will be UPDATED:

       conda: 4.5.12-py37_0 --> 4.6.7-py37_0

   Proceed ([y]/n)?

   Downloading and Extracting Packages
   conda-4.6.7          | 1.7 MB    | ############################ | 100%
   Preparing transaction: done
   Verifying transaction: done
   Executing transaction: done

   (base) C:\Users\scotch>conda env list
   # conda environments:
   #
   base                  *  C:\Users\scotch\Anaconda3
   play1                    C:\Users\scotch\Anaconda3\envs\play1


   (base) C:\Users\scotch>
```
