
# Update 0.2.0

## Test Environments
* local Win10x64 R 3.5.3

# Update to ensure `dplyr 0.8.0.` compatibility 

## Test environments
* local Win10x64 R 3.5.2
* local MacOS 10.13.6 R 3.5.2
* TravisCI - Ubuntu 14.04.5 LTS R 3.5.2 
* Win Builder R-devel (2019-01-09 r75961)
* Win Builder R-release R 3.5.2
* RHub Builder Debian Linux, R-devel, GCC
* RHub Builder Fedora Linux, R-devel, clang, gfortran
* Rhub Builder Ubuntu Linux 16.04 LTS, R-release, GCC
* coverage -> 97%

## Test Results

No errors or notes except for:

**Installation errors for new suggested dependency `vidffr`**
* RHub Builder Debian Linux, R-devel, GCC
* RHub Builder Fedora Linux, R-devel, clang, gfortran
--> since `vdiffr` is a tidyverse package I trust this not to be an issue in the future

**1 Note**
Rhub Builder Ubuntu Linux 16.04 LTS, R-release, GCC

```
Author field differs from that derived from Authors@R
  Author:    ‘Bjoern Koneswarakantha [aut, cre] (<https://orcid.org/ 0000-0003-4585-7799>)’
  Authors@R: ‘Bjoern Koneswarakantha [aut, cre] ( 0000-0003-4585-7799)’
```
--> Authors@R: person( "Bjoern", "Koneswarakantha", role = c("aut","cre"), email = "datistics@gmail.com", comment = c(ORCID = " 0000-0003-4585-7799") )
has not been changed since the latest submission to CRAN and is compliant with the format required [in CRAN checklist](ftp://cran.r-project.org/pub/R/web/packages/submission_checklist.html)

# This package does not have any reverse dependencies