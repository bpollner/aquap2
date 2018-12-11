# aquap2 - Multivariate Data Analysis Tools for R including Aquaphotomics Methods
Authors: Bernhard Pollner & Zoltan Kovacs

## Getting Started
To install the R-package "aquap2", do a simple github-install via devtools, in that way all dependencies should get installed as well.

If not already done, first install the package "devtools" via
```
install.packages("devtools")
```

Then use devtools to install aquap2 from this repository:
```
library(devtools)
install_github(repo="bpollner/aquap2", ref="latestPublic")
```
You can now launch aquap2 and perform (only required at first use) the required setup
```
library(aquap2)
updateSettings()
```

## Course Material
The repository [aquap2_courseMaterial](https://github.com/bpollner/aquap2_courseMaterial) contains a demo dataset with some exploratory and quantitative data analysis steps designed to give a first impression of some of the functions of aquap2.
This is the same demo-dataset as handed out at [The 3rd Aquaphotomics Symposium](http://conference.aquaphotomics.com/) in Japan in December 2018.


## Note
Please note that the package in its current state is in several parts still considered to be beta:
* From the cross-validated classifier methods, only "fda" and "lda" have undergone a certain amount of testing, the other methods (nnet etc.) have been built in, but still require testing and the implementation of optimization options
