## PLot data and SAMRL model fits 

model <- list(fname = 'SAMRL_sub_results.Rdata', modelstring = "fitSub('S1',.15,'S2',.4,'R',3,'R_cor',6,'O1',1.2, 'showfigs','Off');" )
# model <- list(fname = 'SAMRL_sub_noRI_results.Rdata', modelstring = "fitSub('S1',.15,'S2',.4,'R',3,'R_cor',6,'O1',1.2, 'showfigs','Off','RI',false);" )
# model <- list(fname = 'SAMRL_sub_P_results.Rdata', modelstring = "fitSub('pS1x1',.15/(1 +.15),'pS1x2', .15/(1.6 +.15),'pS2x2', .4/(1.2 +.4),'pR',3/(3+1),'pRcor',6/(6+1),'free_params',{'pS1x1','pS1x2','pS2x2','pR','pRcor'}, 'showfigs','Off');")
# model <- list(fname = 'SAMRL_sub_P_noRI_results.Rdata', modelstring = "fitSub('pS1x1',.15/(1 +.15),'pS1x2', .15/(1.6 +.15),'pS2x2', .4/(1.2 +.4),'pR',(3/3+1),'pRcor',(6/6+1),'free_params',{'pS1x1','pS1x2','pS2x2','pR','pRcor'}, 'showfigs','Off','RI',false);")

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
library(boot)
source('plotUtils.R')

## Fit the data ####
if (!file.exists(model[[1]])) {
  results <- fit(model[[1]], model[[2]])
} else {
  load(model[[1]])
}

## prep the data #####
subfits <- results$fits
sub_params <- as.data.frame(results$params)
if (all(sub_params[1,] <=1) && all(sub_params[1,] >= 0 )) {
  colnames(sub_params) <- c("R","Rcor","pS1x1","pS1x2","pS2x2")
} else {
  colnames(sub_params) <- c("O","pR","pRcor","S1","S2")
}

sub_ll <- results$err
subfits$timepoint <- factor(subfits$timepoint, levels = c(1,2,3), labels = c("Practice Test", "Immediate Test", "Delayed Test"))
subfits$other_type <- factor(subfits$other_type, levels = c('T', 'C',NA_character_), exclude = NULL)
levels(subfits$other_type) <- list("New Cue" = 'T', "Same Cue" = c(NA_character_),`NA` = 'C')
subfits <- melt(subfits, id.vars = c("subject","group","timepoint", "practice", "other_type","chain"), 
                measure.vars = c("acc","pred_acc","acc_plus","pred_acc_plus","acc_neg","pred_acc_neg"),
                value.name = 'pc')
subfits$chain[subfits$chain==4] =1 
subfits$LO <- log(subfits$pc/(1-subfits$pc))
subfits$LO[is.nan(subfits$LO) | !is.finite(subfits$LO)] <- NA

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


## plot the data for each sub ####
## @knitr subplots
for (j in c('immediate','delay')) {
  k=1
  for ( i in unique(subfits$subject[subfits$group==j])) { #unique(subfits$subject[subfits$group==j]))
    df1  <- filter(subfits,subject ==i, !is.na(pc), group==j, variable %in% c("acc","pred_acc"))
    df2 <- filter(subfits, subject ==i,!is.na(pc), group==j, variable %in% c("acc_plus","pred_acc_plus","acc_neg","pred_acc_neg")) #%>%
    df2$other_type <- factor(df2$other_type)
    df2$variable <- factor(df2$variable,levels=c("acc_plus","acc_neg","pred_acc_plus","pred_acc_neg"))
    plist <- plot(df1,df2)
    plist[['p1']] <- plist[['p1']] + ggtitle(paste('Subject',i, 'Final Test Accuracy'))
    plist[['p2']] <- plist[['p2']] + ggtitle(paste('Subject',i, 'Conditional Accuracy'))
    if (j =='immediate') {
      subplots_imm[[k]] <- plist
    } else { 
      subplots_del[[k]] <- plist
    }
    k=k+1
  }
}

## Plot the averages across subs ####
## @knitr means
sf_means1 <- subfits %>% filter(variable %in% c('acc','pred_acc'), !is.nan(pc)) %>%
  group_by(group,timepoint,practice,other_type,chain,variable) %>%
  summarise(M = mean(pc),
            MLO = mean(LO,na.rm=TRUE),
            semLO = sd(LO,na.rm=TRUE)/sqrt(n()),
            upper = MLO + semLO,
            lower = MLO - semLO)
sf_means1 <- mutate_each(sf_means1, funs(inv.logit), MLO,semLO,upper,lower )
sf_means2 <- subfits %>% filter(variable %in% c("acc_plus","pred_acc_plus","acc_neg","pred_acc_neg"), !is.nan(pc)) %>%
  group_by(group,timepoint,practice,other_type,chain,variable) %>%
  summarise(cM =mean(pc),
            MLO = mean(LO,na.rm=TRUE),
            semLO = sd(LO,na.rm=TRUE)/sqrt(n()),
            upper = MLO + semLO,
            lower = MLO - semLO)
sf_means2 <- mutate_each(sf_means2, funs(inv.logit), MLO,semLO,upper,lower )
sf_means2$other_type <- factor(sf_means2$other_type)
sf_means2$variable <- factor(sf_means2$variable,levels=c("acc_plus","acc_neg","pred_acc_plus","pred_acc_neg"))
p1 <- ggplot(data=sf_means1,aes(x=timepoint,y=M, shape = variable,color = as.factor(chain), ymax=.85,ymin=.15)) +
  geom_point(size =3.5) +
  #  geom_line(data=sf_means1[sf_means1$timepoint %in% c("Delayed Test", "Practice Test"),],  aes(group=interaction(chain,variable))) + 
  geom_line(data=sf_means1[sf_means1$timepoint %in% c("Delayed Test", "Immediate Test"),],  aes(group=interaction(chain,variable))) +
  geom_line(data=sf_means1[sf_means1$timepoint %in% c("Practice Test", "Immediate Test"),],  aes(group=interaction(group,chain,variable))) +   
  geom_errorbar(aes(ymin = lower, ymax = upper),width=.1) + 
  scale_x_discrete("Test (Nested within Group)",expand=c(0,.25)) + 
  scale_color_discrete("Condition",labels = c("Practice Test",
                                              "Test Practice with Cue 1,\nFinal Test with Unpracticed Cue 2",
                                              "No Practice with Cue 1,\nFinal Test with Cue 1",
                                              "Study Practice with Cue 1,\nFinal Test with Cue 1",
                                              "Test Practice with Cue 1,\nFinal Test with Cue 1")) + 
  scale_shape_manual("Type", values = c(19,4), labels=c("Observed.","SAM-RL")) + 
  ylab("Test Accuracy") + 
  mytheme + theme(legend.key.height=unit(3,"line")) +
  ggtitle(paste('Averaged Data and Model Predictions'))

p2 <- ggplot(mapping= aes(x=other_type,y=cM,fill=variable)) +
  geom_bar(data=sf_means2[sf_means2$variable %in% c("acc_plus","acc_neg"),], 
           position='dodge',stat="identity") +
  geom_point(data=sf_means2[sf_means2$variable %in% c("pred_acc_plus","pred_acc_neg"),],
             position= position_dodge(width=.9),shape = 4,size = 3.5) +
  geom_errorbar(data=sf_means2[sf_means2$variable %in% c("acc_plus","acc_neg"),],
                aes(ymin = lower, ymax = upper),position = position_dodge(width = .9),width=.2) +
  geom_errorbar(data=sf_means2[sf_means2$variable %in% c("pred_acc_plus","pred_acc_neg"),],
                aes(ymin = lower, ymax = upper),position = position_dodge(width = .9),width=.2) +
  scale_x_discrete("Retreival Cue Used", limits = c("Same Cue","New Cue")) + 
  scale_fill_brewer("Practice\nAccuracy", 
                    limits=c('acc_neg','acc_plus'), 
                    labels=c("Incorrect","Correct"),
                    palette="Set1") + 
  facet_grid(.~timepoint) + 
  scale_y_continuous("Accuracy", limits = c(0,1)) +
  mytheme +
  ggtitle(paste('Conditional Accuracy'))

sf_par_means_imm <- colMeans(sub_params_imm)
sf_par_means_del <- colMeans(sub_params_del)

sf_ll_means_imm <- mean(sub_ll_imm)
sf_ll_means_del <- mean(sub_ll_del)


