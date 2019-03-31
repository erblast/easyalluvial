
# Update 0.2.0

## Test Environments
* local x86_64-w64-mingw32/x64 (64-bit) R 3.5.3
* local x86_64-apple-darwin15.6.0 R 3.5.3
* TravisCI - Ubuntu 14.04.5 LTS R 3.5.2 
* RHub Debian Linux, R-devel, GCC
* RHub Debian Linux, R-devel, GCC ASAN/UBSAN
* RHub Ubuntu Linux 16.04 LTS, R-release, GCC
* RHub Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* RHub macOS 10.11 El Capitan, R-release (experimental)
* RHub Ubuntu Linux 16.04 LTS, R-devel with rchk --> PREPERROR (see below)
* RHub Fedora Linux, R-devel, clang, gfortran --> PREPERROR (see below)
* RHub Windows Server 2008 R2 SP1, R-devel, 32/64 bit
coverage -> 96 %


## Test Results

No errors or notes except for:
**PREPERROR**
ERROR: dependency ‘ModelMetrics’ is not available for package ‘caret’,   
ERROR: dependency ‘freetypeharfbuzz’ is not available for package ‘vdiffr’  

# This package does not have any reverse dependencies