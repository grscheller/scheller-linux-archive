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

   Proceed ([y]/n)? 

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
    
```
