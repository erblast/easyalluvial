
# Update 0.3.0

vdiffr and other packages moved to suggests and use was made conditional

## Test Environments
* local macOS 10.14.6 R 4.0.3
* local Win10x64 R 4.0.0
* Appveyor x86_64-w64-mingw32/x64 (64-bit) R 4.0.0
* Travis CI Ubuntu 16.04.6 LTS x86_64-pc-linux-gnu (64-bit) R 4.0.0
* Travis CI Ubuntu 16.04.6 LTS x86_64-pc-linux-gnu (64-bit) R-devel
* Rhub Fedora Linux, R-devel, clang, gfortran --> ERROR (see below)
* Rhub Ubuntu Linux 16.04 LTS, R-release, GCC --> ERROR (see below)
* Rhub Windows Server 2008 R2 SP1, R-devel, 32/64 bit --> ERROR (see below)
* WinBuilder R 4.0.0
* WinBuilder R devel

coverage -> 96 %


## Test Results

No errors or notes except for:

ERROR: dependency 'utf8' is not available for package 'pillar' on Rhub Windows
ERROR: Packages suggested but not available: 'covr', 'vdiffr', 'pkgdown' on Rhub Ubuntu Linux
ERROR: Package suggested but not available: ‘vdiffr’ on Rhub Fedora Linux

## Reverse Dependencies

This package has one reverse dependency 'parcats'
- which is also maintained by me
- travis CI installs dev versions of both packages and checks each of them
- all check are passing
- https://travis-ci.org/github/erblast/parcats/jobs/684018260
- https://travis-ci.org/github/erblast/easyalluvial/jobs/684015855