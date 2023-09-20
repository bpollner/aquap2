<!-- badges: start -->

<!-- [![R-CMD-check](https://github.com/bpollner/uniset/workflows/R-CMD-check/badge.svg)](https://github.com/bpollner/uniset/actions) [![codecov](https://codecov.io/gh/bpollner/uniset/branch/master/graph/badge.svg?token=QK8GPB9XLM)](https://app.codecov.io/gh/bpollner/uniset?branch=master) [![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/uniset)](https://cran.r-project.org/package=uniset) [![metacran downloads](https://cranlogs.r-pkg.org/badges/grand-total/uniset)](https://cran.r-project.org/package=uniset) <img src="man/figures/logo.png" align="right" height="139"/> -->

<!-- badges: end -->

# aquap2

Package `aquap2` provides methods and tools for multivariate data analysis in R, with an additional focus on specialized Aquaphotomics procedures and methods.


## Summary
- Write XXX
- something XXX
- nice XXX

## Installation

### via CRAN
Install release version from CRAN via

``` r
install.packages("aquap2") 
```
### Or download from github:

To install the R-package "aquap2", do a github-install via devtools, in that way all dependencies should get installed as well.

If you work on a Windows-machine, first download and install [R-tools](https://cran.r-project.org/bin/windows/Rtools/). (Re-start the R-session.)

In the further steps, pay attention to the output of the console.

If not already done, install the package "devtools" via
```
install.packages("devtools")
```
Use devtools to install a package dependency not available on CRAN:
```
library(devtools)
install_github(repo="rwehrens/ChemometricsWithR", ref="master")
```
Install aquap2 from this repository:
```
install_github(repo="bpollner/aquap2", ref="latestPublic")
```
You can now launch aquap2 and perform (only required at first use) the required setup
```
library(aquap2)
updateSettings()
updateSettings()
```

All required code in a single block:
```
install.packages("devtools")
library(devtools)
install_github(repo="rwehrens/ChemometricsWithR", ref="master")
install_github(repo="bpollner/aquap2", ref="latestPublic")
library(aquap2)
updateSettings()
updateSettings()
```