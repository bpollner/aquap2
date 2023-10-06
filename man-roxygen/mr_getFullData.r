#' @title *** Get / Import Spectral Data *** 
#' @description If everyting is left at the defaults, the function first tries 
#' to load an R-object containing previously imported spectral data. If this was 
#' not found, it tries to import spectral data from a file in the rawdata-folder, 
#' fuses (if slType is not NULL) these data together with the class-header 
#' provided in the sampleLists/sl_in folder and saves the resulting dataset.
#' It is also possible to use a user-defined custom function to import data from 
#' a file in any format, containing the NIR-spectra as well as all the class- 
#' and numerical  variables. In the latter case it is still possible to fuse 
#' additional variables provided in a file in sampleLists/sl_in with the imported 
#' data.
#' @details From the metadata, provided in the first argument, the experiment 
#' name is extracted, and (if 'ttl' is TRUE) first the dataset-file having this 
#' name is looked for in the 'R-data' folder and, if there, is being loaded.
#' If the file could not be found (or if 'ttl' is FALSE) the spectral file having 
#' the same name as the experiment name (plus its specific ending) is imported 
#' from the rawdata-folder. The sample list (what is used to create the header) 
#' must be in the sampleLists/sl_in folder and must be named with the experiment 
#' name, followed by a "-in" and then the file extension. To be recognized as 
#' such, the standard columns have to be named with the standard column names 
#' as defined in the settings.r file. (see \code{\link{printStdColnames}})
#' If you use a custom function and provide all the class- and numerical variables 
#' together with the spectral data, set argument 'slType' to NULL.
#' If you import from a .pir file and have all the class- and numerical variables 
#' inside the .pir file, set argument 'slType'to NULL.
#' If the dataset is the result of the fusion of other datasets 
#' \code{\link{mergeDatasets}}, the slot 'mergeInfo' will contain further information.
#' @section Note: The strict regime with the filenames (see Details) seems maybe 
#' at first at bit complicated, but it proved to be good practise to ensure a 
#' strict and conscious handling of the files.
#' @param md List. The object with the metadat of the experiment. 
#' The default is to get the metadata file via  \code{\link{getmd}}.
#' @param filetype Character. The type of the spectral raw data file. 
#' If a value other than "def" is provided, this is overriding the 
#' value of "filetype" in the metadata file. Possible values are:
#' \itemize{
#' \item \code{def}: Gets the default value from the setings.r file. 
#'      (variable 'imp_specFileType')
#' \item \code{vision_NSAS.da}: Import from the .da file generated by the 
#' Vision-software from a Foss-XDS spectroscope.
#' \item \code{tabDelim.txt}: Import any tab delimited text file that contains only 
#' the NIR spectra and *no* additional columns like e.g. time, temperature etc, 
#' and that has 1 character in front of the wavelengths in the column names of 
#' the NIR spectra.
#' \item \code{Pirouette.pir}: Import spectra *and* any class- or numerical variable 
#' directly from a .pir file. Those column-names in the .pir  file that match the 
#' standard-column names (\code{\link{printStdColnames}}) as defined in the 
#' settings.r file will be assigned to those columns automatically.
#' \item \code{custom@@yourFile.R}: It is possible to use a custom import function 
#' to import **any** type of spectra. Use \code{custom@@yourFile.R}, 
#' with \code{yourFile.R} being an .R-file located in the path 
#' specified in the .Renviron file. Please refer to \code{\link{custom_import}} 
#' for further information.
#' \item \code{xls}: Import raw spectra from a xlsx file. Please see the section
#' \code{Import from xlsx} below for further details.
#' }
#' @param naString Character. What to use as 'NA'. Applies only when 'filetype' 
#' is \code{tabDelim.txt}.
#' @param slType Character. The type of sample-list file in the sampleLists/
#' sl_in folder. Possible values are:
#' \itemize{
#'    \item \code{def}: Gets the value from the metadata file (variable 
#' 			\code{sampleListType}.)
#'          (variable 'imp_sampleListType')
#'    \item \code{NULL}: By providing 'NULL' to the argument \code{slType} you 
#' 			indicate  hat no sample-list should be imported to create the header. 
#' 			This would  be the case if you use a custom-function to import your 
#' 			spectral data and all the necessary class- and numerical variables 
#' 			are already defined in the same file that holds the spectral data. 
#' 			Please refer to \code{\link{custom_import}} for further information 
#' 			on the requirements for this custom import function.
#'          A custom function can be used to import spectral data and at the same 
#'          time import additional variables from a sample-list file by providing 
#'          one of the characters listed below.
#'    \item \code{xls}: an Excel file ending in '.xlsx'
#' }
#' @param trhLog If data from temperatur and rel.humidity logger should be 
#' imported and aligned to a timestamp in the dataset. Possible values are:
#' \itemize{
#'  \item \code{def}: Gets the value from the variable \code{tempHumLog} from the 
#' 		metadata file.        
#'  \item \code{FALSE}: No data from a logger-file will be imported.
#'  \item \code{ESPEC}: Import data from a tab. delim .txt file generated by an 
#'  	'ESPEC' logger. (This is included for historical reasons.)
#'  \item \code{custom@@yourFile.R}  You can provide your own import-function for 
#'		 importing data from any logger, with \code{yourFile.R} being a .R-file 
#' 		located in the settings-home folder as specified in the .Renviron 
#' 		file. Please refer to \code{\link{custom_TRH}} for further information.
#' }
#' @param multiplyRows Character or Logical. If the rows in the sample list 
#' should be multiplied by the number of consecutive scans as specified 
#' in the variable \code{nrConScans}) in the metadata of the experiment. 
#' \itemize{
#' 	 \item \code{def}: If the argument \code{multiplyRows} in the function 
#' 		\code{getFullData} is left at \code{def}, the value (\code{TRUE} or 
#' 		\code{FALSE} or \code{auto}) from the variable \code{multiplyRows} from 
#' 		the **metadata file** is used. 
#'  \item \code{auto}: Checks if there is an error column in the sample list. 
#' 		If no error column and no column for consecutive scans or only the 
#' 		error column is present, the rows in the sample list will be multiplied
#' 		by the numer of consecutive scans as given in the metadata. The values 
#' 		in the error column (if any) will be used to correct the number of 
#' 		consecutive scans for each respective sample. If no error column, but 
#' 		a column for consecutive scans is present in the sample list, it will 
#' 		**not** be multiplied. 
#'  \item \code{FALSE}: The sample list will be left as it is. In that case 
#' 		it is the users responsibility to provide a sample list with the rows 
#' 		correctly multiplied to match the number of consecutive scans in the 
#' 		dataset. 
#'  \item \code{TRUE}: For multiplying every row in the sample list by the number 
#' 		of consecutive scans as specified in \code{nrConScans} in the metadata 
#' 		of the experiment. If values are given in the error column in the sample
#' 		list, the consecutive scans for each sample will be corrected by this 
#' 		number. 
#' }
#' Please also refer to \code{\link{exportSampleList}} and the explanation to the 
#' argument \code{multiplyRows} therein. 
#' @param ttl Logical, 'try to load'. If a possibly existing r-data file should be 
#' loaded. From the provided metadata (argument 'md') the experiment name is 
#' extracted, and if a file having the same name as the experiment name is found 
#' in folder 'R-data' it is loaded. If there is no such file, the spectra and class 
#' variables are imported from raw-data, and the whole dataset is safed if 
#' argument 'stf' is TRUE.
#' In other words, providing 'FALSE' to argument 'ttl' always imports the spectra 
#' from the raw-data.
#' @param stf Logical, 'save to file'. If the final dataset should be saved to 
#' the 'R-data' folder after import from the raw-data file. Defaults to 'TRUE'.
#' @param dol Detect outliers. If outliers should be detected using the flags 
#' provided by \code{\link[rrcovHD]{RSimca}}. If left at the default "def", the 
#' value from the settings.r file will be used (parameter \code{imp_flagOutliers}.
#' If \code{dol} evaluates to TRUE, an additional column flagging the outliers 
#' as detected in the scope of the complete dataset will be added to the dataset.
#' @param sh Character length one. Manual path to settings home. Can and should 
#' be left at the default \code{NULL}.
#' @section Import from xlsx: 
#' For the raw spectra to be imported from a xlsx file, a few prerequisites have
#' to be fulfilled. It is recommended to look at the file structure of
#' xlsx files generated via \code{\link{export_ap2_ToXlsx}} and use that as a 
#' template.
#' \itemize{
#' \item At least two worksheets are required to be in the xlsx file: One contains 
#' the data, the other some metadata describing the data.
#' \item Data Worksheet: The worksheet containing the data can either contain 
#' only NIR spectra, or class and numerical variables (what is called the 
#' 'header') **and** NIR spectra. (Compare \code{\link{export_ap2_ToXlsx}}).
#' The data worksheet´s name should either end in \code{_data}, or it has to be 
#' the first worksheet.
#' \item _meta Worksheet: The name of the worksheet containing the metadata must end
#' in \code{_meta}. There can only be one worksheet ending in  \code{_meta} in the
#' file. In this worksheet, there has to be one row with three columns. The names 
#' of the columns have to be \code{ncol_header}, \code{rownamesAsFirstColumn} 
#' and \code{ncpwl}.
#' \item First column in _meta: Provide an integer denoting the number of columns
#' in the header. Provide \code{0} (zero) if the data only contain NIR spectra.
#' \item Second column in _meta: Logical, denotes whether there are rownames in 
#' the data. Set to \code{TRUE} or \code{FALSE}.
#' \item Third column in _meta: Provide an integer denoting the number of 
#' characters in front of the wavelength-number. Set to \code{0} (zero) if there
#' are no characters in front of the wavelengths. 
#' \item Timestamps: Should there be timestamps in the xlsx file, their column 
#' name has to be \code{Timestamp}, and the format has to be \code{POSIXct} in 
#' order to be recognized correctly. If these requirements can not be met, it is 
#' advised to write a custom import function to import from xlsx files. Please 
#' see \code{\link{custom_import}} for further information.
#' } 
#' If there are class and numerical variables present in the xlsx file **and** 
#' variables from a sample list are imported as well (so \code{slType} is **not** 
#' \code{NULL}), the sample list must contain a column denoting the 
#' sample number. In this case, the sample number and the number of consecutive 
#' scans get imported from the sample list file, and it will result in an error 
#' to have those variables in the xlsx file as well.
#' Generally, it is not possible to have two variables with the same name.
#' Please look at the files generated via \code{\link{export_ap2_ToXlsx}} as a 
#' reference.
#' @seealso \code{\link{readSpectra}}, \code{\link{readHeader}}, 
#' \code{\link{aquap_data-methods}}
#' @return An object of class 'aquap_data' containing a data frame and six slots:
#' \itemize{
#' \item dataframe Consists of 'header', 'colRep' and 'NIR'.
#' \item metadata A list with the metadata of the experiment
#' \item anproc Possibly a list with an analysis procedure
#' \item mergeInfo Possibly an object of class 'aquap_mergeLabels' 
#' \item calcVarInfo Possibly a list containing information on calculated variables.
#' (\code{\link{generateMergeLabels}}), if the dataset is the result of merging 
#' other datsets.
#'  \item ncpwl Numeric length one, the number of characters before the wavelength 
#'  in the column names of the NIR spectra.
#' \item version A length one character noting the version of the dataset.
#' }
#' @examples
#' \dontrun{
#'  md <- getmd()
#'  fd <- getFullData(md)
#'  fd <- getFullData() # the same as above
#'  fd <- gfd(getmd(expName="OtherName")) # to override the experiment name specified in 
#'  # the metadata.r file and load the dataset called 'Foo' instead. (see ?getmd)
#'  fd <- gfd(md=getmd("foo.r")) # loads metadata from file 'foo.r'
#'  fd <- getFullData(filetype="custom@@myFunc.r", slType="xls")
#'  # This would use a custom function to read in the raw spectra, and read in 
#'  # the class- and numerical variables from an Excel file.
#'  ## 
#'  md <- getmd()
#'  md$meta$expName <- "bar"
#'  fd <- getFullData(md) # load a rawdata-file called "bar"
#' }
#' @family Core functions
