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
source('plotUtils.R')

## Fit the data ####
if (!file.exists('SAMRL_sub_results.Rdata')) {
  results <- fit('SAMRL_sub_results.Rdata', "fitSub('S1',.15,'S2',.4,'R',3,'R_cor',6,'O1',1.2, 'showfigs','Off');")
} else {
  load('SAMRL_sub_results.Rdata')
}

## prep the data #####
subfits <- results$fits
sub_params <- as.data.frame(results$params)
colnames(sub_params) <- c("O","R","Rcor","S1","S2")
sub_ll <- results$err
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
  summarise(M = mean(pc)) 
sf_means2 <- subfits %>% filter(variable %in% c("acc_plus","pred_acc_plus","acc_neg","pred_acc_neg"), !is.nan(pc)) %>%
  group_by(group,timepoint,practice,other_type,chain,variable) %>%
  summarise(cM =mean(pc)) 
sf_means2$other_type <- factor(sf_means2$other_type)
sf_means2$variable <- factor(sf_means2$variable,levels=c("acc_plus","acc_neg","pred_acc_plus","pred_acc_neg"))
p1 <- ggplot(data=sf_means1,aes(x=timepoint,y=M, shape = variable,color = as.factor(chain), ymax=.85,ymin=.15)) +
  geom_point(size =3.5) +
  #  geom_line(data=sf_means1[sf_means1$timepoint %in% c("Delayed Test", "Practice Test"),],  aes(group=interaction(chain,variable))) + 
  geom_line(data=sf_means1[sf_means1$timepoint %in% c("Delayed Test", "Immediate Test"),],  aes(group=interaction(chain,variable))) +
  geom_line(data=sf_means1[sf_means1$timepoint %in% c("Practice Test", "Immediate Test"),],  aes(group=interaction(group,chain,variable))) +   
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
  geom_bar(data=sf_means2[sf_means2$variable %in% c("acc_plus","acc_neg"),], position='dodge',stat="identity") +
  geom_point(data=sf_means2[sf_means2$variable %in% c("pred_acc_plus","pred_acc_neg"),], position= position_dodge(width=.9), stat="identity",shape = 4,size = 3.5) +
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

sf_chi_means_imm <- mean(sub_ll_imm)
sf_chi_means_del <- mean(sub_ll_del)


