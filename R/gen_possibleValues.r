## regarding Aquagram
pv_AquagramModes <- c("classic", "classic-diff", "sfc", "sfc-diff", "aucs", "aucs-diff", "aucs.tn", "aucs.tn-diff", "aucs.tn.dce", "aucs.tn.dce-diff", "aucs.dce",  "aucs.dce-diff")



## regarding cube
pv_what_subPlots <- c("all", "pca", "sim", "pls", "aqg")
pv_what_models <- c("pca", "sim", "pls") # what possible values for arguments when retrieving the models using getCubeModel

## PCA plotting
pv_pca_what <- c("both", "scores", "loadings")


## SIMCA calculation
pv_simca_calc_sampling <- c("random", "interleaved")


## Aquagram plotting
pv_fsa_fss <- c("both", "only")

############
## complete possible values for modifying via ...
pv_modifyUCL <- c("spl.var", "spl.wl")
pv_modifyDPT <- c("spl.do.csAvg", "spl.csAvg.raw", "spl.do.noise", "spl.noise.raw", "spl.do.exOut", "spl.exOut.raw", "spl.exOut.var", "dpt.pre", "dpt.post")
pv_modifyPCA<-c("do.pca", "pca.colorBy", "pca.elci", "pca.elcolorBy", "pca.what", "pca.sc", "pca.sc.pairs", "pca.lo")
pv_modifySIMCA<-c("do.sim", "sim.vars", "sim.K")
pv_modifyPLSR<-c("do.pls", "pls.regOn", "pls.ncomp", "pls.valid", "pls.colorBy")
pv_modifyAquagram<-c("do.aqg", "aqg.vars", "aqg.nrCorr", "aqg.spectra", "aqg.minus", "aqg.mod", "aqg.TCalib", "aqg.Texp", "aqg.bootCI", "aqg.R", "aqg.smoothN", "aqg.selWls", "aqg.msc", "aqg.reference", "aqg.fsa", "aqg.fss", "aqg.ccol", "aqg.clt", "aqg.pplot", "aqg.plines", "aqg.disc")
pv_modifyGenPlot<-c("pg.where", "pg.main", "pg.sub", "pg.fns")	
pv_tripleDotsMod <- c(pv_modifyUCL, pv_modifyDPT, pv_modifyPCA, pv_modifySIMCA, pv_modifyPLSR, pv_modifyAquagram, pv_modifyGenPlot)
##############

## import, filetype etc.
pv_filetypes <- c("vision_NSAS.da", "tabDelim.txt", "Pirouette.pir")


## Data pre-treatment moduls
pv_dptModules <- c("smo", "snv", "msc", "osc", "1der", "2der", "deTr")
