# Resubmission 0.3.0 - 3

Got one Note from automated CRAN check that tests are too slow.
Overall checktime 12 min > 10 min

Disabled a few more tests on CRAN

# Resubmission 0.3.0 - 2

Got one Note from automated CRAN check that tests are too slow.
Disabled some none-essential longer running tests for CRAN

# Resubmission 0.3.0 - 1

For some reasons 'caret' did not install on the Windows Server which ran
the automated CRAN checks.

Since 'caret' is only in Suggests, the examples will now only execute if 'caret'
is installed.

# Update 0.3.0

vdiffr and other packages moved to suggests and usage was made conditional

## Test Environments
* local macOS 10.14.6 R 4.0.3
* Appveyor x86_64-w64-mingw32/x64 (64-bit) R 4.0.3
* github actions macos-latest R 4.0.3
* Rhub Fedora Linux, R-devel, clang, gfortran
* Rhub Ubuntu Linux 16.04 LTS, R-release, GCC --> NOTE
* Rhub Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* WinBuilder R 4.0.3
* WinBuilder R devel

coverage -> 96 %


## Test Results

No errors or notes except for:
5 Examples with CPU or elapsed time > 5s
This server seem to be a bit slower than the others.
The examples are already pretty minimal and have been around for a while. 
I would prefer to keep them and not put them into \dontrun{}. 

## Reverse Dependencies

This package has one reverse dependency 'parcats'
- which is also maintained by me
- all checks are passing