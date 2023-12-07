# Resubmission after being archived on CRAN

All suggested dependencies were already controlled for, meaning there was a 
check in place that would raise an error with a message to install the missing
package. Those are limited to functions most users will not use.

All unit tests would skip if suggested packages were missing.

One example missed a check for a suggested package installation which was added:

-  check for vip pkg installation before executing example of alluvial_model_response_parsnip

## Test Environments
* local macOS M1 R 4.3.2
* github actions macos-latest R 4.3.2
* WinBuilder R 4.3.2
* WinBuilder R devel

coverage -> 96 %


## Test Results

Maintainer: ‘Bjoern Koneswarakantha <datistics@gmail.com>’

New submission

Package was archived on CRAN

## Reverse Dependencies

no reverse dependencies