# Version 0.4.5
## New:
* Started with testing

## Improved:

## Bugfix:




# Version 0.4.4
## New:
* In Aquagrams, bootR is now adjusted to the actual dataset.

## Improved: 
* Aquagram	

## Bugfix:
* Various small bugfixes



# Version 0.4.2
## New:
* Create new variables via combining pre-existent values or by calculating a new value using an arbitrary expression.

## Improved: 
* When merging datasets, it is now possible to also resample the wavelengths if necessary.	

## Bugfix:
* Various small bugfixes (e.g. the show-method for aquap_data)



# Version 0.4.1
## New:
* Added merging of datasets.	

## Improved:

## Bugfix:
* Fixed a bug that caused the path for the settings home folder in the .Renviron file to be wrong on Windwos machines.
	



# Version 0.3.2
## New:
	
## Improved:
	
## Bugfix:
* Fixed "stringsAsFactors" problem in creating the header data-frame (due to the update to R 4.x)
	



# Version 0.3.1
## New:
* option in the settings to use also virtual cores in parallel computing
	
## Improved:
* classic aquagram is now impossible to bootstrap (as that does not make any sense)
	
## Bugfix:
* fixed the parallel calculation of aquagrams on windows systems




# Version 0.2.0
## New:
* Added classifiers.
* Function to select and possibly average groups of wavelengths (function siWlg).




#Version 0.1.9
## New:
* Values on the X-Axis can be others than wavelengths; the denominator can be specified in the metadata.
	
##Bugfix:
* Repaired custom color vectors in Aquagrams.
* Other small bugfixes.



# Version 0.1.8
## New:	
* Added possibility to plot Aquagrams linear instead of circular. Factory-default is linear; 
* parameter "aqg_plottingType" in the settings file.
* Added output of table indicating significant differences between groups and the corresponding values.
* Added some more Aquagram specific options to the settings file.
* Resampling of data to new wavelengths (core data manipulation function).
* Added possibility to average spectra by any class-variable grouping (function "do_avg").
	
## Improved:
* If exact temperature is present in temperature calibration data, "loess" is not used any more.
* In the "diff" modes of the Aquagram, the input for "aqg.minus" is now vectorized, i.e. you can supply 
a character vector in "aqg.minus" for corresponding subtractions in the character vector in "aqg.vars".
		
## Bugfix:
* Classic Aquagram can now be produced without having a temperature calibration file.
* Outlier-detection using the package rrcovHD should now work on all systems.
* Now correct detection of range (precise wavelength independent) in universal AUC table.
* Various other small bugfixes.
