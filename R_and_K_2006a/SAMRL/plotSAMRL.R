library(reshape2)
library(ggplot2)
library(dplyr)

## Get fits 
## @knitr get_fits

# mytheme and pres_theme are from .Rprofile

## Set wd, depending on platform
if(.Platform$OS.type == "unix") {
  root <- Sys.getenv("HOME")
  wd <- file.path("opt","source","RPmodels","R_and_K_2006a","SAMRL")
}   else if (.Platform$OS.type == "windows") {
  root <- Sys.getenv("USERPROFILE")
  wd <- file.path(Sys.getenv("USERPROFILE"),"source","RPmodels","R_and_K_2006a","SAMRL")
}
setwd(wd)
source('SAMRL.R')

g2 <- function(obs,pred,N) {
  Lc <- obs*(log(pred)) + ((1-obs)*log(1-pred))
  Lu <- obs*(log(obs)) + ((1-obs)*log(1-obs))
  err <- -sum(2*N*(Lc-Lu))
  return(err)
}

fits <- function(params = NULL, one_shot = 'mixed',RI=TRUE) {
  point_fits <-  melt(SAMRL(params, t=c(1,params$O2,params$O7), one_shot= one_shot,RI=RI,predict=FALSE),
                      id =c("time","one_shot"),variable.name="class",value.name="acc")
  point_fits[,c("type","RI")] <- list('model',RI)
  point_fits <- point_fits[!(point_fits$time > 1 & point_fits$class=='OneStudy'),]
  point_fits$time <- factor(point_fits$time, labels = c("Immediate", "Two Days","Seven Days"))
  return(point_fits)
#   df$time <point- factor(df$time,labels= c('5 Minutes','2 Days', '7 Days'))
}

forgetting <- function(params = NULL) {
  preds_same_c <- SAMRL(params, one_shot='never')
  preds_same_c <- melt(preds_same_c , id =c("time","one_shot"),variable.name="class",value.name="acc")
  preds_diff_c <- SAMRL(params,  one_shot='always')[,c('time','Test',"one_shot")]
  preds_diff_c <- melt(preds_diff_c , id =c("time","one_shot"),variable.name="class",value.name="acc")
  preds_mixed_c <- SAMRL(params, one_shot='mixed')[,c('time','Test',"one_shot")]
  preds_mixed_c <- melt(preds_mixed_c , id =c("time","one_shot"),variable.name="class",value.name="acc")
  preds <- rbind(preds_same_c,preds_diff_c,preds_mixed_c)
  preds$one_shot <- factor(preds$one_shot,levels = c('never', 'always','mixed'))  # Generate fits for noRI model   
  return(preds)
}

params<-read.csv('params.csv',header=T)
params=cbind(params, list(k=rep(500, nrow(params))),list(nItems=rep(30, nrow(params))))
RI_oneshot<-params[1,]
RI_no_one_shot<-params[2,]
noRI_one_shot<-params[3,]
noRI_no_one_shot <-params[4,]

# observed data from Roediger and Karpicke, 2006a, Experiment 1
# obs=c(.70,.81,.75,.54,.68,.42,.56)
data<-data.frame(acc=rep(c(.70,.81,.75,.54,.68,.42,.56),nrow(params)),
                 time=factor(round(c(1,1,1,params[1,'O2'],params[1,'O2'],params[1,'O7'],params[1,'O7'],
                              1,1,1,params[2,'O2'],params[2,'O2'],params[2,'O7'],params[2,'O7'],
                              1,1,1,params[3,'O2'],params[3,'O2'],params[3,'O7'],params[3,'O7'], 
                              1,1,1,params[4,'O2'],params[4,'O2'],params[4,'O7'],params[4,'O7']),
                            3)),
#                  group=rep(c('test','study','test','study','test','study','test'),nrow(params)),
                 # Test_Type=rep(c("Practice Test", "Final Test","Final Test","Final Test","Final Test","Final Test","Final Test"),nrow(params)),
                 class=rep(c("OneStudy", "TwoStudy","Test","TwoStudy","Test","TwoStudy","Test"),nrow(params)),
                 paramset=rep(c(1,2,3,4),each=7)
#                  type = 'real'
                )
levels(data$time) <- list('Immediate' = 1, "Two Days" = c("2.392","3.047","3.367","3.375"), "Seven Days" = c("3.419","4.594","5.426","6.035"))
# Generate fits for RI,one shot model 
m1_RI_one_shot <- data.frame(paramset=1,arrange(fits(params = RI_oneshot),time))
# data <- rbind(data,m1_RI_one_shot)
err1 <- g2(obs = data$acc[1:7], pred = m1_RI_one_shot$acc, N=120)
m1_RI_one_shot_fcurve <- forgetting(params = RI_oneshot)

# Generate fits for RI, no one shot model
m2_RI_no_one_shot <- data.frame(paramset=2,arrange(fits(params = RI_no_one_shot,one_shot='never'),time))
# data <- rbind(data,m2_RI_no_one_shot)
err2 <- g2(obs = data$acc[1:7], pred = m2_RI_no_one_shot$acc, N=120)
m2_RI_no_one_shot_fcurve <-  forgetting(RI_no_one_shot)
  
# Generate fits for no RI,one shot model
m3_no_RI_one_shot <- data.frame(paramset=3,arrange(fits(params = noRI_one_shot,RI=FALSE),time))
# data <- rbind(data,m3_no_RI_one_shot)
err3 <- g2(obs = data$acc[1:7], pred = m3_no_RI_one_shot$acc, N=120)
m3_no_RI_one_shot_fcurve <- forgetting(noRI_one_shot)

# Generate fits for no RI, no one shot model
m4_no_RI_no_one_shot<- data.frame(paramset=4,arrange(fits(params = noRI_no_one_shot,one_shot='never',RI=FALSE),time))
# data <- rbind(data,m4_no_RI_no_one_shot)
err4 <- g2(obs = data$acc[1:7], pred = m4_no_RI_no_one_shot$acc, N=120)
m4_no_RI_no_one_shot_fcurve <- forgetting(noRI_no_one_shot)

preds <- rbind( m1_RI_one_shot, m2_RI_no_one_shot, m3_no_RI_one_shot, m4_no_RI_no_one_shot)
# Generate fits for noRI model 
# point_fits <- SAMRL(params[1,], t=c(1,params[1,'O2'],params[1,'O7']), one_shot='mixed',predict=FALSE)
# melted_fits <- melt(point_fits, id =c("time","one_shot"),variable.name="class",value.name="acc")
# df <- data[data$paramset==1,c('acc','time','class')]
# df$type <- 'real'
# melted_fits$one_shot <- NULL
# melted_fits$type <- 'model'
# melted_fits <- melted_fits[!(melted_fits$time > 1 & melted_fits$class=='OneStudy'),]
# df <- rbind(melted_fits,df)
# # df$time[df$time==1] <- '5 Minutes'
# # df$time[df$time == '2.9828'] <- '2 Days'
# # df$time[df$time=='4.499'] <- '7 Days'
# df$time <- factor(df$time,labels= c('5 Minutes','2 Days', '7 Days'))

# Calculate g^2 error statistic
# err <- g2(obs = data$acc[1:7], pred = data$acc[data$paramset==4 & data.type=='model'], N=120)
# obs <- df$acc[8:14]
# pred = df[df$type=='model','acc']
# Lc <- obs*(log(pred)) + ((1-obs)*log(1-pred))
# Lu <- obs*(log(obs)) + ((1-obs)*log(1-obs))
# err <- -sum(2*120*(Lc-Lu))

# preds_same_c <- SAMRL(params[1,], one_shot='never')
# preds_same_c <- melt(preds_same_c , id =c("time","one_shot"),variable.name="class",value.name="acc")
# preds_diff_c <- SAMRL(params[1,],  one_shot='always')[,c('time','Test',"one_shot")]
# preds_diff_c <- melt(preds_diff_c , id =c("time","one_shot"),variable.name="class",value.name="acc")
# preds_mixed_c <- SAMRL(params[1,], one_shot='mixed')[,c('time','Test',"one_shot")]
# preds_mixed_c <- melt(preds_mixed_c , id =c("time","one_shot"),variable.name="class",value.name="acc")
# preds <- rbind(preds_same_c,preds_diff_c,preds_mixed_c)
# preds$one_shot <- factor(preds$one_shot,levels = c('never', 'always','mixed'))


# # Generate fits for no RI model 
# point_fits_noRI <- SAMRL(params[4,], t=c(1,params[4,'O2'],params[4,'O7']), one_shot='mixed',predict=FALSE, RI=FALSE)
# melted_fits_noRI <- melt(point_fits_noRI, id =c("time","one_shot"),variable.name="class",value.name="acc")
# df_noRI <- data[data$paramset==4,c('acc','time','class')]
# df_noRI$type <- 'real'
# melted_fits_noRI$one_shot <- NULL
# melted_fits_noRI$type <- 'model'
# melted_fits_noRI <- melted_fits_noRI[!(melted_fits_noRI$time > 1 & melted_fits_noRI$class=='OneStudy'),]
# df_noRI <- rbind(melted_fits_noRI,df_noRI)
# # df_noRI$time[df_noRI$time==1] <- '5 Minutes'
# # df_noRI$time[df_noRI$time == '2.9828'] <- '2 Days'
# # df_noRI$time[df_noRI$time=='4.499'] <- '7 Days'
# df_noRI$time <- factor(df_noRI$time,labels= c('5 Minutes','2 Days', '7 Days'))
# 
# # Calculate g^2 error statistic
# pred <- df_noRI[df_noRI$type=='model','acc']
# obs <- df_noRI$acc[8:14]
# Lc <- obs*(log(pred)) + ((1-obs)*log(1-pred))
# Lu <- obs*(log(obs)) + ((1-obs)*log(1-obs))
# err <- -sum(2*120*(Lc-Lu))
# 
# preds_same_c_noRI <- SAMRL(params[4,], one_shot='never',RI=FALSE)
# preds_same_c_noRI <- melt(preds_same_c_noRI , id =c("time","one_shot"),variable.name="class",value.name="acc")
# preds_diff_c_noRI <- SAMRL(params[4,],  one_shot='always',RI=FALSE)[,c('time','Test',"one_shot")]
# preds_diff_c_noRI <- melt(preds_diff_c_noRI , id =c("time","one_shot"),variable.name="class",value.name="acc")
# preds_mixed_c_noRI <- SAMRL(params[4,], one_shot='mixed',RI=FALSE)[,c('time','Test',"one_shot")]
# preds_mixed_c_noRI<- melt(preds_mixed_c_noRI , id =c("time","one_shot"),variable.name="class",value.name="acc")
# preds_noRI <- rbind(preds_same_c_noRI,preds_diff_c_noRI,preds_mixed_c_noRI)
# preds_noRI$one_shot <- factor(preds_noRI$one_shot,levels = c('never', 'always','mixed'))


## Fits only #####
## @knitr fits_only
p1 <- ggplot(data = data, aes(x=time, y=acc, colour=class)) +
      geom_point(size=5) + 
      geom_point(data = preds,size=5,shape = 4) + 
      facet_grid(RI~one_shot) + 
      scale_color_discrete(name = "Conditions",
                       labels = c("Practice Test", "Final Test (S)", "Final Test (T)")) +
      scale_shape_manual(name="Source", breaks = c('model','real'), c('Model Fits', 'Obs. Data'), values = c(4,1)) + 
      scale_x_discrete("Retention Interval") + ylab("Memory Accuracy") + 
      ggtitle('SAM-RL vs. Roediger & Karpicke 2006b Data') +
      mytheme

p1_pres <- p1+pres_theme

## always new context preds #####
## @knitr one_new_context
p2 <- ggplot(preds[preds$one_shot=="never",],
             aes(time,acc,colour=class)) + 
      geom_line(size=1.75,aes(linetype=one_shot)) + 
      geom_point(aes(x=time, y=acc),color="black",size=5,
                 data=data[data$paramset==1,]) +
      geom_point(aes(x=time, y=acc, color=class),size=3.5,
             data=data[data$paramset==1,]) +
      xlab("Retention Interval (Days) / Interference (O)") + ylab("Memory Accuracy") + 
      ggtitle('SAM-RL Predictions vs. Roediger & Karpicke 2006b Data') +
      scale_color_discrete(name = "Conditions",
                           labels = c("Practice Test", "Final Test (S)", "Final Test (T)")) + 
      scale_linetype_manual("Final Test\nContext", 
                            values = c(3,2,1),
                            breaks = c('never','always','mixed'), 
                            labels = c("Always New Context","Always Old Context", "Switch Context")) +
      mytheme + 
      guides(colour = guide_legend(order = 1), 
         linetype = guide_legend(order = 2))

p2_pres <- p2+pres_theme

## always old context preds #####
## @knitr one_old_context
p3 <- p2  %+% preds[preds$one_shot %in% c("always", "never"),]   
p3_pres <- p3+pres_theme

## switch context preds #####
## @knitr switch_context
p4 <- p3 %+% preds
p4_pres <- p4+pres_theme 


p5 <- p1 %+% df_noRI
p6 <- p2 %+% preds_noRI[preds_noRI$one_shot=="never",]
p6$layers[[2]] <- geom_point(aes(x=time, y=acc),color="black",size=5, data=data[data$paramset==4,])
p6$layers[[3]] <- geom_point(aes(x=time, y=acc, color=class),size=3.5,data=data[data$paramset==4,])
p7 <- p6 %+% preds_noRI[preds$one_shot %in% c("always", "never"),] 
p8 <- p7 %+% preds_noRI


