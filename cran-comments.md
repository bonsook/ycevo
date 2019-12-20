## Resubmission

This is a resubmission. In this version I have:

* Added references describing the methods in the description field of the DESCRIPTION file.

* Added another vignette.

* Added more documentation to explain what the functions return.

In previous versions I have:

* Explained acronym in the description text

* Replaced \dontrun{} with \donttest{}: 
    the examples take more than 10 sec to run 

* Added examples for main functions.

* Explained the results for main functions.


## Test environments
* local Windows 10 install, R 3.6.2
* Ubuntu Xenial 16.04 (on travis-ci), R 3.6.1
* Ubuntu Xenial 16.04 (on travis-ci), R 3.6.2
* Ubuntu Xenial 16.04 (on travis-ci), R 3.5.3
* Ubuntu Xenial 16.04 (on travis-ci), R Under development (unstable) (2019-12-19 r77605)
* win-builder (devel, oldrelease and release)
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (on Rhub)



## R CMD check results
There were no ERRORs, or WARNINGs.

There was 2 NOTE:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Yangzhuoran Yang <Fin.Yang@monash.edu>'
  New submission
  Possibly mis-spelled words in DESCRIPTION:
    CRSP (27:6)
    Koo (28:50)
    Vecchia (28:62)

  CRSP is explained in the description and Koo and Vecchia are names.

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