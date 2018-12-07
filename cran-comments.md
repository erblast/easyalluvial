## Test environments
* local Win10x64 R 3.5.1
* local MacOS 10.13.6 R 3.5.1
* TravisCI - Ubuntu 14.04.5 LTS R 3.5.1 
* Win Builder R-devel (2018-11-16 r75619)
* Win Builder R-release R 3.5.1
* coverage -> 97%

**There were no ERRORs, WARNINGs or NOTEs**

# Resubmission

*Examples generating plots were wrapped in  `\dontrun{if(interactive()){...}`*
The `if(interactive())` was not necessary, however examples took a bit too long to execute, so they were wrapped in `\dontrun{}`. Adressed this now by allowing the first plot to render and then keeping the `\dontrun{}` wrapper for the plots that follow.


*No citation of method in description field*
Citation was added.
