library(reshape2)
library(ggplot2)

# Rows 1 and 2 will do the CEMS poster data 

## Get fits 
## @knitr get_fits
mytheme <- theme(axis.title.x=element_text(size=rel(2),vjust=-.65),
                 axis.title.y=element_text(size=rel(2),vjust=1.5),
                 axis.title.x=element_text(size=rel(2)),
                 axis.title.y=element_text(size=rel(2)),
                 axis.ticks = element_line(size=rel(2)),
                 axis.text = element_text(size=rel(1.25)),
                 legend.text = element_text(size=16),
                 legend.title = element_text(size=20),
                 plot.title = element_text(size=24))
source('SAMRI.R')
params<-read.csv(file.path(getwd(),'params.csv'),header=T)
params=cbind(params, list(k=rep(500, nrow(params))),list(nItems=rep(30, nrow(params))))
good<-unlist(params[1,])
bad<-unlist(params[2,])
badbutgood<-unlist(params[3,])

# observed data from Roediger and Karpicke, 2006a, Experiment 1
obs=c(.70,.81,.75,.54,.68,.42,.56)
data<-data.frame(acc=rep(obs,nrow(params)),
                 time=c(1,1,1,params[1,'O2'],params[1,'O2'],params[1,'O7'],params[1,'O7'],
                        1,1,1,params[2,'O2'],params[2,'O2'],params[2,'O7'],params[2,'O7'],
                        1,1,1,params[3,'O2'],params[3,'O2'],params[3,'O7'],params[3,'O7']),
                 group=rep(c('test','study','test','study','test','study','test'),nrow(params)),
                 Test_Type=rep(c("Practice Test", "Final Test","Final Test","Final Test","Final Test","Final Test","Final Test"),nrow(params)),
                 class=rep(c("OneStudy", "TwoStudy","Test","TwoStudy","Test","TwoStudy","Test"),nrow(params)),
                 paramset=rep(c(1,2,3),each=length(obs))
                )
# Generate fits
point_fits <- SAMRI(params[1,], t=c(1,params[1,'O2'],params[1,'O7']), one_shot='mixed',predict=FALSE)
melted_fits <- melt(point_fits, id =c("time","one_shot"),variable.name="class",value.name="acc")
df <- data[data$paramset==1,c('acc','time','class')]
df$type <- 'real'
melted_fits$one_shot <- NULL
melted_fits$type <- 'model'
melted_fits <- melted_fits[!(melted_fits$time > 1 & melted_fits$class=='OneStudy'),]
df <- rbind(melted_fits,df)
df$time[df$time==1] <- '5 Minutes'
df$time[df$time == '2.9828'] <- '2 Days'
df$time[df$time=='4.499'] <- '7 Days'
df$time <- factor(df$time,levels= c('5 Minutes','2 Days', '7 Days'))

preds_same_c <- SAMRI(params[1,], one_shot='never')
preds_same_c <- melt(preds_same_c , id =c("time","one_shot"),variable.name="class",value.name="acc")
preds_diff_c <- SAMRI(params[1,],  one_shot='always')[,c('time','Test',"one_shot")]
preds_diff_c <- melt(preds_diff_c , id =c("time","one_shot"),variable.name="class",value.name="acc")
preds_mixed_c <- SAMRI(params[1,], one_shot='mixed')[,c('time','Test',"one_shot")]
preds_mixed_c <- melt(preds_mixed_c , id =c("time","one_shot"),variable.name="class",value.name="acc")
preds <- rbind(preds_same_c,preds_diff_c,preds_mixed_c)
preds$one_shot <- factor(preds$one_shot,levels = c('never', 'always','mixed'))
# preds2 <- SAMRI(params[3,], t=seq(1,5,.1))
# comb_preds= rbind(preds_same_c,preds_diff_c,preds_mixed_c)
# melted_preds = melt(comb_preds, id =c("time","one_shot"),variable.name="class",value.name="acc")



## Fits only #####
## @knitr fits_only
p1 <- ggplot(data = df, aes(x=time, y=acc, shape = type, colour=class)) +
      geom_point(size=5) + 
      geom_point(size=5) + 
      scale_color_discrete(name = "Conditions",
                       labels = c("Practice Test", "Final Test (S)", "Final Test (T)")) +
      scale_shape_manual(name="Source", breaks = c('model','real'), c('Model Fits', 'Obs. Data'), values = c(4,1)) + 
      scale_x_discrete("Retention Interval") + ylab("Memory Accuracy") + 
      ggtitle('SAM-RL vs. Observed Data') +
      mytheme

## always new context preds #####
## @knitr one_new_context
p2 <- ggplot(preds[preds$one_shot=="never",],
             aes(time,acc,colour=class)) + 
      geom_line(size=1.25,aes(linetype=one_shot)) + 
      geom_point(aes(x=time, y=acc),color="black",size=4,
                 data=data[data$paramset==1,]) +
      geom_point(aes(x=time, y=acc, color=class),size=2.5,
             data=data[data$paramset==1,]) +
      xlab("Retention Interval (Days) / Interference (O)") + ylab("Memory Accuracy") + 
      ggtitle('SAM-RL Predictions vs. Observed Data') +
      scale_color_discrete(name = "Conditions",
                           labels = c("Practice Test", "Final Test (S)", "Final Test (T)")) + 
      
      scale_linetype_manual("Final Test\nContext", 
                        values = 3,
                        breaks = c('never'), 
                        labels = c("Always New Context")) + 
      mytheme + 
      guides(colour = guide_legend(order = 1), 
         linetype = guide_legend(order = 2))

## always old context preds #####
## @knitr one_old_context
p3 <- ggplot(preds[preds$one_shot %in% c("always", "never"),],
             aes(time,acc,colour=class)) + 
    geom_path(size=1.25,aes(linetype = one_shot)) + 
    geom_point(aes(x=time, y=acc),color="black",size=4,
              data=data[data$paramset==1,]) +
    geom_point(aes(x=time, y=acc, color=class),size=2.5,
              data=data[data$paramset==1,]) +
    xlab("Retention Interval (Days) / Interference (O)") + ylab("Memory Accuracy") + 
    ggtitle('SAM-RL Predictions vs. Observed Data') +
    scale_color_discrete(name = "Conditions",
                       labels = c("Practice Test", "Final Test (S)", "Final Test (T)")) + 
    scale_linetype_discrete("Final Test\nContext", 
                          breaks = c('never','always','mixed'), 
                          labels = c("Always New Context","Always Practice Context", "Switch Context")) +
    mytheme +
    guides(colour = guide_legend(order = 1), 
         linetype = guide_legend(order = 2))
      

## switch context preds #####
## @knitr switch_context
p4 <- ggplot(preds, aes(time,acc,colour=class)) + 
  geom_path(size=1.25,aes(linetype = one_shot)) + 
  geom_point(aes(x=time, y=acc),color="black",size=4,
             data=data[data$paramset==1,]) +
  geom_point(aes(x=time, y=acc, color=class),size=2.5,
             data=data[data$paramset==1,]) +
  xlab("Retention Interval (Days) / Interference (O)") + ylab("Memory Accuracy") + 
  ggtitle('SAM-RL Predictions vs. Observed Data') +
  scale_color_discrete(name = "Conditions",
                       labels = c("Practice Test", "Final Test (S)", "Final Test (T)")) +
  scale_linetype_discrete("Final Test\nContext", 
                          breaks = c('never','always','mixed'), 
                          labels = c("Always New Context","Always Practice Context", "Switch Context")) +
  mytheme +
  guides(colour = guide_legend(order = 1), 
         linetype = guide_legend(order = 2))

