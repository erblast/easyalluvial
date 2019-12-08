
# Update 0.2.2

removed leading space from ORCID in DESCRIPTION

c(ORCID = " 0000-0003-4585-7799") --> c(ORCID = "0000-0003-4585-7799")


## Test Environments
* local x86_64-w64-mingw32/x64 (64-bit) R 3.6.1
* Appveyor x86_64-w64-mingw32/x64 (64-bit) R 3.6.1
* Travis CI Ubuntu 16.04.6 LTS x86_64-pc-linux-gnu (64-bit) R 3.6.1
* Travis CI Ubuntu 16.04.6 LTS x86_64-pc-linux-gnu (64-bit) R-devel
* Rhub Fedora Linux, R-devel, clang, gfortran --> PREPERROR (see below)
* Rhub Ubuntu Linux 16.04 LTS, R-release, GCC
* Rhub Windows Server 2008 R2 SP1, R-devel, 32/64 bit

coverage -> 96 %


## Test Results

No errors or notes except for:
**PREPERROR**
ERROR: dependency ‘ModelMetrics’ is not available for package ‘caret’,   

## Comments

`caret`, `covr`, `vdiffr` are pretty common packages cannot say why they are not available on these linux systems.

# This package does not have any reverse dependencies