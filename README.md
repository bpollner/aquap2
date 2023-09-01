# aquap2 - Multivariate Data Analysis Tools for R including Aquaphotomics Methods
Authors: Bernhard Pollner & Zoltan Kovacs

[![DOI](https://zenodo.org/badge/30932899.svg)](https://zenodo.org/badge/latestdoi/30932899)

## Getting Started
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

## Course Material
### Japan 2018
The repository [aquap2_courseMaterial](https://github.com/bpollner/aquap2_courseMaterial) contains a demo dataset with some exploratory and quantitative data analysis steps designed to give a first impression of some of the functions of aquap2.
This is the same demo-dataset as handed out at [The 3rd Aquaphotomics Symposium](http://conference.aquaphotomics.com/) in Japan in December 2018.

### Rome 2023
The course material for the Summer School on Aquaphotomics in the course of the [The 3rd European Aquaphotomics Symposium](https://www.3aec.sisnir.org/) (September 2023, Rome) can be found [here](https://github.com/bpollner/aquap2_course_Rome2023).

---

## Important
Please note that the package in its current state is in several parts still considered to be beta:
* From the cross-validated classifier methods, only "fda" and "lda" have undergone a certain amount of testing, the other methods (nnet etc.) have been built in, but still require testing and the implementation of optimization options
