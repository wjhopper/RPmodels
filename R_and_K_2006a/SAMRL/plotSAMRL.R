## Get fits 
## @knitr env
library(reshape2)
library(ggplot2)
library(dplyr)

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
}

forgetting <- function(params = NULL,RI= TRUE) {
  preds_same_c <- SAMRL(params, t = seq(1,params$O7+1, .1), one_shot='never',RI=RI)
  preds_same_c <- melt(preds_same_c , id =c("time","one_shot"),variable.name="class",value.name="acc")
  preds_diff_c <- SAMRL(params,  t = seq(1,params$O7+1,.1), one_shot='always',RI=RI)[,c('time','Test',"one_shot")]
  preds_diff_c <- melt(preds_diff_c , id =c("time","one_shot"),variable.name="class",value.name="acc")
  preds_mixed_c <- SAMRL(params,t = seq(1,params$O7+1,.1), one_shot='mixed',RI=RI)[,c('time','Test',"one_shot")]
  preds_mixed_c <- melt(preds_mixed_c , id =c("time","one_shot"),variable.name="class",value.name="acc")
  preds <- rbind(preds_same_c,preds_diff_c,preds_mixed_c)
  preds$one_shot <- factor(preds$one_shot,levels = c('never', 'always','mixed'))  # Generate fits for noRI model   
  return(preds)
}

forgetting_plots <- function(d,data) {
  p1 <- ggplot(d[d$one_shot=="never",], mapping = aes(time,acc,colour=class)) + 
    geom_line(size=1.75,aes(linetype=one_shot)) + 
    geom_point(aes(x=time, y=acc),color="black",size=5, data = data) +
    geom_point(aes(x=time, y=acc, color=class),size=3.5,data = data) +
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
  

  p2 <- p1  %+% d[d$one_shot %in% c("always", "never"),]   
  p3 <- p2 %+% d
  return(list(p1,p2,p3)) 
}

RI_names <- list("FALSE" = 'No Rec. Interference', "TRUE" = "Rec. Interference")
one_shot_names <- list('mixed' = 'Recovery Dependence', 'never' = 'No Recovery Dependence')
plot_labeller <- function(variable,value){
  if (variable=='RI') {
    return(RI_names[as.character(value)])
  } else {
    return(one_shot_names[value])
  }
}

## @knitr readIn
params<-read.csv('params.csv',header=T)
params=cbind(params, list(k=rep(500, nrow(params))),list(nItems=rep(30, nrow(params))))
RI_oneshot<-params[1,]
RI_no_one_shot<-params[2,]
noRI_one_shot<-params[3,]
noRI_no_one_shot <-params[4,]

# observed data from Roediger and Karpicke, 2006a, Experiment 1
# obs=c(.70,.81,.75,.54,.68,.42,.56)
data<-data.frame(acc=rep(c(.70,.81,.75,.54,.68,.42,.56),nrow(params)),
                 time=round(c(1,1,1,params[1,'O2'],params[1,'O2'],params[1,'O7'],params[1,'O7'],
                              1,1,1,params[2,'O2'],params[2,'O2'],params[2,'O7'],params[2,'O7'],
                              1,1,1,params[3,'O2'],params[3,'O2'],params[3,'O7'],params[3,'O7'], 
                              1,1,1,params[4,'O2'],params[4,'O2'],params[4,'O7'],params[4,'O7']),
                            3),
                 class=rep(c("OneStudy", "TwoStudy","Test","TwoStudy","Test","TwoStudy","Test"),nrow(params)),
                 paramset=rep(c(1,2,3,4),each=7),
                 type="real"
                )
data$time_factor <- factor(data$time)
levels(data$time_factor) <- list('Immediate' = 1, "Two Days" = c("2.392","3.047","3.367","3.375"), "Seven Days" = c("3.419","4.594","5.426","6.035"))

## @knitr modelfits

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
m3_no_RI_one_shot_fcurve <- forgetting(noRI_one_shot,RI=FALSE)

# Generate fits for no RI, no one shot model
m4_no_RI_no_one_shot<- data.frame(paramset=4,arrange(fits(params = noRI_no_one_shot,one_shot='never',RI=FALSE),time))
# data <- rbind(data,m4_no_RI_no_one_shot)
err4 <- g2(obs = data$acc[1:7], pred = m4_no_RI_no_one_shot$acc, N=120)
m4_no_RI_no_one_shot_fcurve <- forgetting(noRI_no_one_shot,RI=FALSE)

preds <- rbind( m1_RI_one_shot, m2_RI_no_one_shot, m3_no_RI_one_shot, m4_no_RI_no_one_shot)

## @knitr modelplots
p1 <- ggplot(data = data, mapping = aes(x=time_factor, y=acc, colour=class,shape=type)) +
      geom_point(color = "black",size=5) +
      geom_point(size=3.5) + 
      geom_point(data = preds,size=6,mapping = aes(x=time, y=acc, colour=class)) + 
      facet_grid(RI~one_shot,labeller=plot_labeller) + 
      scale_color_discrete(name = "Conditions",
                       labels = c("Practice Test", "Final Test (T)", "Final Test (S)")) +
      scale_shape_manual(name="", breaks = c('model','real'), c('Model Fits', 'Obs. Data'), values = c(4,19)) + 
      scale_x_discrete("Retention Interval") + ylab("Memory Accuracy") + 
      ggtitle('SAM-RL vs. Roediger & Karpicke 2006b Data') +
      mytheme
# Could potentially not do a facet and map the symbol type to the model 
p1_pres <- p1+pres_theme

m1_RI_one_shot_fplots <- forgetting_plots(m1_RI_one_shot_fcurve, data[data$paramset==1,])
m2_RI_no_one_shot_fplots <- forgetting_plots(m2_RI_no_one_shot_fcurve,data[data$paramset==2,])
m3_no_RI_one_shot_fplots  <- forgetting_plots(m3_no_RI_one_shot_fcurve ,data[data$paramset==3,])
m4_no_RI_no_one_shot_plots <- forgetting_plots(m4_no_RI_no_one_shot_fcurve,data[data$paramset==4,])


