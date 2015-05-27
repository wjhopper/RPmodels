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

## @knitr fitsubs
if (!file.exists('SAMRL_sub_prob_params_results.Rdata') {
  Matlab$startServer()
  matlab <- Matlab()
  isOpen <- open(matlab)
  evaluate(matlab, "[sub_ll, sub_params, subfits] = fitSub_noRI('S1x1',0.1304,'S1x2', .15/(1.2 +.15),'S2x2', .4/(1.2 +.4),'R',(3/3+1),'R_cor',(6/6+1),'k',10,'p',1, 'showfigs','Off'); ")
  evaluate(matlab, "export(subfits, 'file','subfits.csv', 'Delimiter', ',');")
  subfits <- read.csv('subfits.csv',header=T,na.strings = "")
  file.remove('subfits.csv')
  sub_ll <- getVariable(matlab, "sub_ll")
  sub_ll <- sub_ll[[1]]
  sub_params <-  getVariable(matlab, "sub_params")
  sub_params<-as.data.frame(sub_params[[1]])
  colnames(sub_params) <- c("O","R","Rcor","S1","S2")
  save(aggchi,aggparams,sub_ll, sub_params,file = 'SAMRL_sub_prob_params_results.Rdata')
  close(matlab)
} else {  
  load('SAMRL_sub_prob_params_results.Rdata')
}

subfits$timepoint <- factor(subfits$timepoint, levels = c(1,2,3), labels = c("Practice Test", "Immediate Test", "Delayed Test"))
subfits$other_type <- factor(subfits$other_type, levels = c('T', 'C',NA_character_), exclude = NULL)
levels(subfits$other_type) <- list("New Cue" = 'T', "Same Cue" = c(NA_character_),`NA` = 'C')
subfits <- melt(subfits, id.vars = c("subject","group","timepoint", "practice", "other_type","chain"), 
                measure.vars = c("acc","pred_acc","acc_plus","pred_acc_plus","acc_neg","pred_acc_neg"),
                value.name = 'pc')
subfits$chain[subfits$chain==4] =1 

del = unique(subfits$subject[subfits$group=='delay'])
imm = unique(subfits$subject[subfits$group=='immediate'])
all = unique(subfits$subject)
a = which(all %in% del)
b = which(all %in% imm)
sub_params_imm <- sub_params[b,]
sub_params_del <- sub_params[a,]
sub_ll_imm <- sub_ll[b]
sub_ll_del <- sub_ll[a]
subplots_imm <- vector(mode='list', length = length(unique(subfits$subject[subfits$group=='immediate'])))
subplots_del <- vector(mode='list', length = length(unique(subfits$subject[subfits$group=='delay'])))

source("plotSAMRL.R")

## @knitr fitsubs
if (!file.exists('SAMRL_sub_prob_params_results.Rdata') {
  Matlab$startServer()
  matlab <- Matlab()
  isOpen <- open(matlab)
  evaluate(matlab, "[sub_ll, sub_params, subfits] = fitSub_noRI('S1x1',0.1304,'S1x2', .15/(1.2 +.15),'S2x2', .4/(1.2 +.4),'R',(3/3+1),'R_cor',(6/6+1),'k',10,'p',1, 'showfigs','Off'); ")
  evaluate(matlab, "export(subfits, 'file','subfits.csv', 'Delimiter', ',');")
  subfits <- read.csv('subfits.csv',header=T,na.strings = "")
  file.remove('subfits.csv')
  sub_ll <- getVariable(matlab, "sub_ll")
  sub_ll <- sub_ll[[1]]
  sub_params <-  getVariable(matlab, "sub_params")
  sub_params<-as.data.frame(sub_params[[1]])
  colnames(sub_params) <- c("O","R","Rcor","S1","S2")
  save(aggchi,aggparams,sub_ll, sub_params,file = 'SAMRL_sub_prob_params_results.Rdata')
  close(matlab)
} else {  
  load('SAMRL_sub_prob_params_results.Rdata')
}

subfits$timepoint <- factor(subfits$timepoint, levels = c(1,2,3), labels = c("Practice Test", "Immediate Test", "Delayed Test"))
subfits$other_type <- factor(subfits$other_type, levels = c('T', 'C',NA_character_), exclude = NULL)
levels(subfits$other_type) <- list("New Cue" = 'T', "Same Cue" = c(NA_character_),`NA` = 'C')
subfits <- melt(subfits, id.vars = c("subject","group","timepoint", "practice", "other_type","chain"), 
                measure.vars = c("acc","pred_acc","acc_plus","pred_acc_plus","acc_neg","pred_acc_neg"),
                value.name = 'pc')
subfits$chain[subfits$chain==4] =1 

del = unique(subfits$subject[subfits$group=='delay'])
imm = unique(subfits$subject[subfits$group=='immediate'])
all = unique(subfits$subject)
a = which(all %in% del)
b = which(all %in% imm)
sub_params_imm <- sub_params[b,]
sub_params_del <- sub_params[a,]
sub_ll_imm <- sub_ll[b]
sub_ll_del <- sub_ll[a]
subplots_imm <- vector(mode='list', length = length(unique(subfits$subject[subfits$group=='immediate'])))
subplots_del <- vector(mode='list', length = length(unique(subfits$subject[subfits$group=='delay'])))

source("plotSAMRL.R")