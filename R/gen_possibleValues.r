## regarding Aquagram
pv_AquagramModes <- c("classic", "classic-diff", "sfc", "sfc-diff", "aucs", "aucs-diff", "aucs.tn", "aucs.tn-diff", "aucs.tn.dce", "aucs.tn.dce-diff", "aucs.dce",  "aucs.dce-diff")



## PCA plotting
pv_pca_what <- c("both", "scores", "loadings")


## SIMCA 
pv_simca_calc_sampling <- c("random", "interleaved")
pv_warningIcDistRange <- 12

## PLSR calculation & plotting
pv_plsr_crossvalidation <- c("CV", "LOO")
pv_plsr_what <- c("both", "errors", "regression")
pv_plsr_levelLimitMsg <- " (nc.level.lim.) "

## Noise
pv_noiseAddModes <- c("sdNorm", "sdUnif", "extrema", "static")


## Aquagram plotting
pv_fsa_fss <- c("both", "only")


## Classification
pv_classificationFuncs_XDA <- c("lda", "qda", "fda", "mclustda")
pv_nonDAClassifiers <- c("rndforest", "svm", "nnet")
pv_allClassificationFuncs <- c(pv_classificationFuncs_XDA, pv_nonDAClassifiers)



## regarding cube
pv_what_classifPlots <- c("da", "rnf", "svm", "nnet")
pv_what_subPlots <- c("all", "pca", "sim", "pls", "aqg", pv_what_classifPlots)
pv_what_models <- c("pca", "simca", "plsr", "aquagr") # what possible values for arguments when retrieving the models using getcm



## General Plotting
pv_legendPosition <- c("auto", "topleft", "topright", "bottomright", "bottomleft")


############
## complete possible values for modifying via ...
pv_modifyUCL <- c("spl.var", "spl.wl")
pv_modifyDPT <- c("spl.do.csAvg", "spl.csAvg.raw", "spl.do.noise", "spl.noise.raw", "spl.do.exOut", "spl.exOut.raw", "spl.exOut.var", "dpt.pre", "dpt.post")
pv_modifyPCA <- c("do.pca", "pca.colorBy", "pca.elci", "pca.elcolorBy", "pca.what", "pca.sc", "pca.sc.pairs", "pca.lo")
pv_modifySIMCA <- c("do.sim", "sim.vars", "sim.K")
pv_modifyPLSR <- c("do.pls", "pls.regOn", "pls.ncomp", "pls.valid", "pls.exOut", "pls.colorBy", "pls.what", "pls.rdp")
pv_modifyAquagram_calc <- c("do.aqg", "aqg.vars", "aqg.nrCorr", "aqg.spectra", "aqg.minus", "aqg.mod", "aqg.TCalib", "aqg.Texp", "aqg.bootCI", "aqg.R", "aqg.smoothN", "aqg.selWls", "aqg.msc", "aqg.reference")
pv_modifyAquagram_plot <- c("aqg.fsa", "aqg.fss", "aqg.ccol", "aqg.clt", "aqg.pplot", "aqg.plines", "aqg.disc")
pv_modifyClassifiers <- c("do.da", "da.type", "da.classOn", "do.rnf", "rnf.classOn", "do.svm", "svm.classOn", "do.nnet", "nnet.classOn")
pv_modifyClassif2 <- c("da.testCV", "da.percTest", "da.cvBootCutoff", "da.cvBootFactor", "da.valid", "rnf.testCV", " rnf.percTest", "rnf.cvBootCutoff", "rnf.cvBootFactor", "rnf.valid", "svm.testCV", "svm.percTest", "svm.cvBootCutoff", " vm.cvBootFactor", "svm.valid", "nnet.testCV", "nnet.percTest", "nnet.cvBootCutoff", "nnet.cvBootFactor", "nnet.valid")
pv_modifyClassif3 <- c("da.pcaRed", "da.pcaNComp", "svm.pcaRed", "svm.pcaNComp", "rnf.pcaRed", "rnf.pcaNComp", "nnet.pcaRed", "nnet.pcaNComp")
pv_modifyGenPlot <- c("pg.where", "pg.main", "pg.sub", "pg.fns")	
additionalElements <- c("aps")
pv_tripleDotsMod <- c(pv_modifyUCL, pv_modifyDPT, pv_modifyPCA, pv_modifySIMCA, pv_modifyPLSR, pv_modifyAquagram_calc, pv_modifyAquagram_plot, pv_modifyClassifiers, pv_modifyClassif2, pv_modifyClassif3, pv_modifyGenPlot, additionalElements)
##############

## import, filetype etc.
pv_filetypes <- c("vision_NSAS.da", "tabDelim.txt", "Pirouette.pir", "xlsx")


## Data pre-treatment moduls
pv_dptModules <- c("sgol", "snv", "msc", "emsc", "osc", "deTr", "gsd")
pv_extraMods <- "extraMods"

## the version of the dataset
pv_versionDataset <- "0.1.4"   # change this only!! if the structure of the dataset has been changed !! XXX

pv_initialTempCalibFilename <- "TempCalib"
pv_YcolumnNameSampleTemp <- "smpTemp"

pv_suffixForTemplates <- "_TEMPLATE"

#########################################################
## Globals
pv_noiseDistPrefix <- "nd_"
