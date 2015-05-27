## PLot data and SAMRL model fits 
## @knitr readAndScore

if(.Platform$OS.type == "unix") {
  wd <- file.path("opt","source","RPmodels","H_and_H_LB4L","SAMRL")
  
}   else if (.Platform$OS.type == "windows") {
  wd <- file.path(Sys.getenv("USERPROFILE"),"source","RPmodels","H_and_H_LB4L","SAMRL")
}
setwd(wd)

library(dplyr)
library(ggplot2)
library(reshape2)
library(R.matlab)


if (!file.exists('SAMRL_agg_results.Rdata')) {
  Matlab$startServer()
  matlab <- Matlab()
  isOpen <- open(matlab)
  [aggchi, aggpars, aggfits] = fitAgg('S1',.15,'S2',.4,'R',3,'R_cor',6,'O1',1.2,'O2',2,'showfigs','Off');  
  evaluate(matlab, "[aggchi, aggpars, aggfits] = fitAgg('S1',.15,'S2',.4,'R',3,'R_cor',6,'O1',1.2,'O2',2,'showfigs','Off');")
  evaluate(matlab, "export(aggfits, 'file','aggfits.csv', 'Delimiter', ',');")
  aggfits <- read.csv('aggfits.csv',header=T,na.strings = "")
  file.remove('aggfits.csv')
  aggchi <- getVariable(matlab, "aggchi")
  aggchi <- aggchi[[1]]
  aggparams <-  getVariable(matlab, "aggpars")
  aggparams<-as.data.frame(aggparams[[1]])

  save(aggchi,aggparams,aggfits,file = 'SAMRL_agg_results.Rdata')
  close(matlab)
} else {  
  load('SAMRL_agg_results.Rdata')
}

aggfits$timepoint <- factor(aggfits$timepoint, levels = c(1,2,3), labels = c("Practice Test", "Immediate Test", "Delayed Test"))
aggfits$other_type <- factor(aggfits$other_type, levels = c('T', 'C',NA_character_), exclude = NULL)
levels(aggfits$other_type) <- list("New Cue" = c('C','T'), "Same Cue" = c(NA_character_))

aggfits <- melt(aggfits, id.vars = c("group","timepoint", "practice", "other_type"), 
                measure.vars = c("acc","pred_acc","acc_plus","pred_acc_plus","acc_neg","pred_acc_neg"),
                value.name = 'pc')