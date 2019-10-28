## Resubmission
This is a resubmission. In this version I have:

* Explained acronym in the description text

* Replaced \dontrun{} with \donttest{}: 
    the examples take more than 10 sec to run 

* Added examples for main functions.

* Explained the results for main functions.


## Test environments
* local Windows 10 install, R 3.6.1
* local Windows 10 install, R-devel
* Ubuntu Xenial 16.04 (on travis-ci), R 3.6.1
* win-builder (oldrelease and release)
* Fedora Linux, R-devel, clang, gfortran (on Rhub)
* Debian Linux, R-devel, GCC ASAN/UBSAN  (on Rhub)
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (on Rhub)
* Ubuntu Linux 16.04 LTS, R-release, GCC (on Rhub)

## R CMD check results
There were no ERRORs, or WARNINGs.

There was 2 NOTE:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Yangzhuoran Yang <Fin.Yang@monash.edu>'
  New submission
  Possibly mis-spelled words in DESCRIPTION:
    CRSP (25:62)

  CRSP is short for Center for Research in Security Prices and is commonly used.

* checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    'examples_i386' 'examples_x64' 'ycevo-Ex_i386.Rout'
    'ycevo-Ex_x64.Rout'
    
  Only appears on Windows R-devel version. 
  Looks like the development version run examples for i386 and x64 without cleaning afterwards, 
  which means it is onlt a problem in checking.
  Does not affect functionality and stability of the package.

## Downstream dependencies

There are currently no downstream dependencies for this package.
This is the first submission of the package.