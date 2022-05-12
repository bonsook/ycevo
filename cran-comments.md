This is the first submission of the package.

## Test environments
* local Windows 10 install, R 4.2.0
* maxOS-latest (release) x86_64-apple-darwin17.0 (64-bit) (GitHub actions), R 4.2.0
* windows-latest (release) x86_64-w64-mingw32 (64-bit) (GitHub actions), R 4.2.0
* ubuntu-latest (devel) x86_64-pc-linux-gnu (64-bit) (GitHub actions), R Under development (unstable) (2022-05-10 r82335) 
* ubuntu-latest (release) x86_64-pc-linux-gnu (64-bit) (GitHub actions), R 4.2.0
* ubuntu-latest (oldrel-1) x86_64-pc-linux-gnu (64-bit) (GitHub actions), R 4.1.3
* win-builder (devel, oldrelease and release)
* Windows Server 2022, R-devel, 64 bit (on Rhub)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (on Rhub)
*	Fedora Linux, R-devel, clang, gfortran (on Rhub)

## R CMD check results
There were no ERRORs, or WARNINGs.

There were 2 NOTEs:

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Yangzhuoran Fin Yang <yangyangzhuoran@gmail.com>’
New submission
Possibly mis-spelled words in DESCRIPTION:
  CRSP (26:79)
  Koo (25:5)
  Vecchia (25:17)

CRSP is explained in the description and Koo and Vecchia are names.

* checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

Only appears on Windows R-devel version. 
From [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can be ignored.

## Downstream dependencies

There are currently no downstream dependencies for this package.
This is the first submission of the package.