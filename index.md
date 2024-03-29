<!-- badges: start -->

<!-- [![R-CMD-check](https://github.com/bpollner/uniset/workflows/R-CMD-check/badge.svg)](https://github.com/bpollner/uniset/actions) [![codecov](https://codecov.io/gh/bpollner/uniset/branch/master/graph/badge.svg?token=QK8GPB9XLM)](https://app.codecov.io/gh/bpollner/uniset?branch=master) [![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/uniset)](https://cran.r-project.org/package=uniset) [![metacran downloads](https://cranlogs.r-pkg.org/badges/grand-total/uniset)](https://cran.r-project.org/package=uniset) <img src="man/figures/logo.png" align="right" height="139"/> -->

<!-- badges: end -->

# aquap2

Package `aquap2` provides methods and tools for multivariate data analysis in R, with an additional focus on specialized Aquaphotomics procedures and methods.

## Summary
aquap2 can be useful in three distinct areas in NIRS:
* **Experiment Design**: Outline the class-structure and generate a truly randomized sample list.
* **Data Acquisition**: While recording spectra, the previously generated sample list can be used, thus eliminating errors in assigning classes.
* **Data Analysis**: aquap2 provides an array of standard chemometric procedures as well as some specialized methods used in Aquaphotomics.


## Installation
If you work on a Windows-machine, you might first have to download and install [R-tools](https://cran.r-project.org/bin/windows/Rtools/). (Re-start the R-session.)

If not already done, install the package "devtools" via
```
install.packages("devtools")
```
Install release version of `aquap2` from CRAN via
``` r
install.packages("aquap2") 
```
or download from github:
``` r
library(devtools)
install_github(repo="bpollner/aquap2", ref="latestPublic")
```
You can now launch aquap2 and perform (only required at first use) the required setup
```
library(aquap2)
ap2_settings_setup()
```
This adds an entry to the `.Renviron` file and generates the corresponding folder containing the `aquap2_settings.R` file.
(You might have to restart R.)
