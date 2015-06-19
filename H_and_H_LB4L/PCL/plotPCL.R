plotPCL <- function(model="std") {
  library(reshape2)
  library(ggplot2)
  library(dplyr)
  library(boot)
  
  if(.Platform$OS.type == "unix") {
    root <- Sys.getenv("HOME")
    wd <- file.path("opt","source","RPmodels","H_and_H_LB4L","PCL")
  }   else if (.Platform$OS.type == "windows") {
    root <- Sys.getenv("USERPROFILE")
    wd <- file.path(Sys.getenv("USERPROFILE"),"source","RPmodels","H_and_H_LB4L","PCL")
  }
  setwd(wd)
  source('PCL.R')
  if (!file.exists(paste(model,"results.Rdata",sep="_"))) {
    source('fitPCL.R')
    m <- fitPCL(model=model)
  } else {
    load(paste(model,"results.Rdata",sep="_"))
    m <- tosave
    rm(tosave)
  }
  m$plots <- vector(mode='list',length=length(unique(m$data$subject)))
  params <- lapply(lapply(m$results, optimx:::coef.optimx),c)
  params <- lapply(params, setNames, colnames(optimx:::coef.optimx(m$results[[1]])))
  k=1
  tmp <- vector(mode='list',length(unique(m$data$subject)))
  for (i in unique(m$data$subject)) {
    tmp[[k]] <-m$fcn(free=params[[k]],fixed=m$fix,data=m$data[m$data$subject==i,])
    k=k+1
  }
  data <- do.call(rbind,tmp) #lapply(params,m$fcn, fixed=m$fix, data=m$data))
  data$chain <- factor(data$chain)
  tmp <- melt(data, id.vars = c("subject","group","timepoint", "practice", "other_type","chain","n","nplus","nneg"), 
       measure.vars = c("acc","pred_acc","acc_plus","pred_acc_plus","acc_neg","pred_acc_neg"),
       value.name = 'pc')
  df1  <- filter(tmp,variable %in% c("acc","pred_acc"))
  df1$chain[df1$chain==4] <- 1
  df2 <- filter(tmp,variable %in% c("acc_plus","pred_acc_plus","acc_neg","pred_acc_neg")) #%>%
  df2$other_type <- factor(df2$other_type,exclude="C")
  df2$variable <- factor(df2$variable,levels=c("acc_plus","acc_neg","pred_acc_plus","pred_acc_neg"))

  
  if (length(unique(data$timepoint[data$subject == unique(data$subject)[1]]))>2) {
    m$plots <- vector(mode='list',length=1)
    p1 <- ggplot(df1,aes(x=timepoint,y=pc,color = chain,shape=variable)) + 
      geom_point(size =3) +
      scale_x_discrete("Test (Nested within Group)",labels = c("Practice Test","Immediate Final Test","Delayed Final Test"),expand=c(0,.25)) + 
      scale_color_discrete("Condition",
                           labels = c("Test Practice with Cue 1,\nFinal Test with Unpracticed Cue 2",
                                      "No Practice with Cue 1,\nFinal Test with Cue 1",
                                      "Study Practice with Cue 1,\nFinal Test with Cue 1",
                                      "Test Practice with Cue 1,\nFinal Test with Cue 1")) + 
      scale_shape_manual("Type", values=c(19,4),labels=c("Observed.","PCL")) + 
      scale_y_continuous("Test Accuracy",limits= c(0,1)) + 
      mytheme + theme(legend.key.height=unit(2,"line")) + 
      ggtitle(paste('Averaged Data and Model Predictions'))  
    p2 <-  ggplot(mapping= aes(x=other_type,y=pc,fill=variable,ymax=1)) +
      geom_bar(data=df2[df2$variable %in% c("acc_plus","acc_neg"),], position='dodge',stat="identity") +
      geom_point(data=df2[df2$variable %in% c("pred_acc_plus","pred_acc_neg"),], position= position_dodge(width=.9), stat="identity",shape = 4,size = 3.5) +
      facet_grid(.~group) + 
      scale_x_discrete("Retreival Cue Used",labels = c("Same Cue","New Cue")) + 
      scale_fill_brewer("Practice\nAccuracy", 
                        limits=c('acc_neg','acc_plus'), 
                        labels=c("Incorrect","Correct"),
                        palette="Set1") + 
      scale_y_continuous("Accuracy", limits = c(0,1)) +
      mytheme
    m$plots[[1]][c("acc","cond_acc")] <- list(p1,p2)
    return(m)
  } else {
    m$plots <- vector(mode='list',length=length(unique(m$data$subject))+1)
    m$aggPlots  <- list(acc =NULL,cond_acc = NULL)
    data[,c("pred_nplus","pred_nneg")] <- NA 
    data$pred_nplus[!is.na(data$pred_acc_plus)] <- data$n[!is.na(data$pred_acc) & data$timepoint ==1] * data$pred_acc[!is.na(data$pred_acc_plus)] # * data$pred_acc_plus[!is.na(data$pred_acc_plus)]
    data$pred_nneg[!is.na(data$pred_acc_neg)] <- data$n[!is.na(data$pred_acc) & data$timepoint ==1] * (1-data$pred_acc[!is.na(data$pred_acc_neg)]) # * data$pred_acc_neg[!is.na(data$pred_acc_neg)]
    k=1
    for (i in unique(m$data$subject)) {
      p1 <- ggplot(df1[df1$subject==i,],aes(x=timepoint,y=pc,color = chain,shape=variable)) + 
        geom_point(size =3) +
        scale_x_discrete("Test (Nested within Group)",labels = c("Practice Test","Final Test"),expand=c(0,.25)) + 
        scale_color_discrete("Condition",
                             labels = c("Test Practice with Cue 1,\nFinal Test with Unpracticed Cue 2",
                                        "No Practice with Cue 1,\nFinal Test with Cue 1",
                                        "Study Practice with Cue 1,\nFinal Test with Cue 1",
                                        "Test Practice with Cue 1,\nFinal Test with Cue 1")) + 
        scale_shape_manual("Type", values=c(19,4),labels=c("Observed.","PCL")) + 
        scale_y_continuous("Test Accuracy",limits= c(0,1)) + 
        mytheme + theme(legend.key.height=unit(2,"line")) + 
        ggtitle(paste('Averaged Data and Model Predictions'))  
      p2 <-  ggplot(mapping= aes(x=other_type,y=pc,fill=variable,ymax=1)) +
        geom_bar(data=df2[df2$subject==i & df2$variable %in% c("acc_plus","acc_neg"),], position='dodge',stat="identity") +
        geom_point(data=df2[df2$subject==i & df2$variable %in% c("pred_acc_plus","pred_acc_neg"),], position= position_dodge(width=.9), stat="identity",shape = 4,size = 3.5) + 
        scale_x_discrete("Retreival Cue Used",labels = c("Same Cue","New Cue")) + 
        scale_fill_brewer("Practice\nAccuracy", 
                          limits=c('acc_neg','acc_plus'), 
                          labels=c("Incorrect","Correct"),
                          palette="Set1") + 
        scale_y_continuous("Accuracy", limits = c(0,1)) +
        mytheme
      m$plots[[k]][c("acc","cond_acc")] <- list(p1,p2)#  [c("acc","cond_acc")]]<- list(p1,p2)
      k=k+1

    }
#     testfnc <- function(x,y ) { 
#       x
#       y
#       }
#   data <- melt(data,id.vars = c("group","chain","timepoint","n"),variable.name = 'type',value.name="pc")
    tmp <- data %>% group_by(group,timepoint,practice,other_type,chain) %>% 
      summarise(acc = mean(acc),
                pred_acc = mean(pred_acc),
                acc_plus = sum(acc_plus * nplus,na.rm=TRUE)/sum(nplus,na.rm=TRUE),
                nplus=sum(nplus),
                pred_acc_plus =sum(pred_acc_plus * pred_nplus,na.rm=TRUE)/sum(pred_nplus,na.rm=TRUE),
                pred_nplus = sum(pred_nplus),
                acc_neg = sum(acc_neg * nneg,na.rm=TRUE)/sum(nneg,na.rm=TRUE),
                nneg=sum(nneg),
                pred_acc_neg =sum(pred_acc_neg * pred_nneg,na.rm=TRUE)/sum(pred_nneg,na.rm=TRUE),
                pred_nneg= sum(pred_nneg))
    tmp <- melt(tmp, id.vars = c("group","timepoint", "practice", "other_type","chain","nplus","nneg"), 
                measure.vars = c("acc","pred_acc","acc_plus","pred_acc_plus","acc_neg","pred_acc_neg"),
                value.name = 'pc')
    df1  <- filter(tmp,variable %in% c("acc","pred_acc"))
    df1$chain[df1$chain==4] <- 1
    df2 <- filter(tmp,variable %in% c("acc_plus","pred_acc_plus","acc_neg","pred_acc_neg")) #%>%
    df2$other_type <- factor(df2$other_type,exclude="C")
    df2$variable <- factor(df2$variable,levels=c("acc_plus","acc_neg","pred_acc_plus","pred_acc_neg"))   
    p1agg <- ggplot(df1,aes(x=timepoint,y=pc,color = chain,shape=variable)) + 
      geom_point(size =3) +
      scale_x_discrete("Test (Nested within Group)",labels = c("Practice Test","Immediate Final Test","Delayed Final Test"),expand=c(0,.25)) + 
      scale_color_discrete("Condition",
                           labels = c("Test Practice with Cue 1,\nFinal Test with Unpracticed Cue 2",
                                      "No Practice with Cue 1,\nFinal Test with Cue 1",
                                      "Study Practice with Cue 1,\nFinal Test with Cue 1",
                                      "Test Practice with Cue 1,\nFinal Test with Cue 1")) + 
      scale_shape_manual("Type", values=c(19,4),labels=c("Observed.","PCL")) + 
      scale_y_continuous("Test Accuracy",limits= c(0,1)) + 
      mytheme + theme(legend.key.height=unit(2,"line")) + 
      ggtitle(paste('Averaged Data and Model Predictions'))  
    p2agg <-  ggplot(mapping= aes(x=other_type,y=pc,fill=variable,ymax=1)) +
      geom_bar(data=df2[df2$variable %in% c("acc_plus","acc_neg"),], position='dodge',stat="identity") +
      geom_point(data=df2[df2$variable %in% c("pred_acc_plus","pred_acc_neg"),], position= position_dodge(width=.9), stat="identity",shape = 4,size = 3.5) +
      facet_grid(.~group) + 
      scale_x_discrete("Retreival Cue Used",labels = c("Same Cue","New Cue")) + 
      scale_fill_brewer("Practice\nAccuracy", 
                        limits=c('acc_neg','acc_plus'), 
                        labels=c("Incorrect","Correct"),
                        palette="Set1") + 
      scale_y_continuous("Accuracy", limits = c(0,1)) +
      mytheme    
    m$plots[[k]][c("accAgg","condAccAgg")] <- list(p1agg,p2agg)#  [c("acc","cond_acc")]]<- list(p1,p2)
    return(m)
  }  
    
}

# plotSS <- function(df1,df2) { 
#   p1 <- ggplot(data=df1,aes(x=timepoint,y=pc, shape = variable,color = factor(chain),group=interaction(chain,variable))) +
#     geom_point(size =3.5) +
#     geom_line() + 
#     scale_x_discrete("Test (Nested within Group)",expand=c(0,.25)) + 
#     scale_color_discrete("Condition", breaks = c(1,2,3,5),
#                          labels = c("Test Practice with Cue 1,\nFinal Test with Unpracticed Cue 2",
#                                     "No Practice with Cue 1,\nFinal Test with Cue 1",
#                                     "Study Practice with Cue 1,\nFinal Test with Cue 1",
#                                     "Test Practice with Cue 1,\nFinal Test with Cue 1")) + 
#     scale_shape_manual("Type",labels=c("Observed","SAM-RL"),values=c(19,4)) + 
#     scale_y_continuous("Test Accuracy",limits= c(0,1)) + 
#     mytheme + theme(legend.key.height=unit(3,"line"))
#   
#   p2 <- ggplot(mapping= aes(x=other_type,y=pc,fill=variable,ymax=1)) +
#     geom_bar(data=df2[df2$variable %in% c("acc_plus","acc_neg"),], position='dodge',stat="identity") +
#     geom_point(data=df2[df2$variable %in% c("pred_acc_plus","pred_acc_neg"),], position= position_dodge(width=.9), stat="identity",shape = 4,size = 3.5) +
#     scale_x_discrete("Retreival Cue Used", limits = c("Same Cue","New Cue")) + 
#     scale_fill_brewer("Practice\nAccuracy", 
#                       limits=c('acc_neg','acc_plus'), 
#                       labels=c("Incorrect","Correct"),
#                       palette="Set1") + 
#     scale_y_continuous("Accuracy", limits = c(0,1)) +
#     mytheme
#   
#   return(list(p1 = p1,p2=p2))
# }
# 
# plotAgg <- function(df1,df2,y = c('M','cM'),bars=FALSE) {
#   
#   p1 <- ggplot(data=df1,aes_string(x="timepoint",y=y[1], shape = "variable",color = "chain", ymax=.85,ymin=.15)) +
#     geom_point(size =3.5) +
#     #    geom_line(data=df1[df1$timepoint %in% c("Delayed Test", "Practice Test"),],  aes(group=interaction(chain,variable))) + 
#     geom_line(data=df1[df1$timepoint %in% c("Delayed Test", "Immediate Test"),],  aes(group=interaction(chain,variable))) +
#     geom_line(data=df1[df1$timepoint %in% c("Practice Test", "Immediate Test"),],  aes(group=interaction(group,chain,variable))) +   
#     scale_x_discrete("Test (Nested within Group)",expand=c(0,.25)) + 
#     scale_color_discrete("Condition",
#                          labels = c("Test Practice with Cue 1,\nFinal Test with Unpracticed Cue 2",
#                                     "No Practice with Cue 1,\nFinal Test with Cue 1",
#                                     "Study Practice with Cue 1,\nFinal Test with Cue 1",
#                                     "Test Practice with Cue 1,\nFinal Test with Cue 1")) + 
#     scale_shape_manual("Type", values = c(19,4), labels=c("Observed.","SAM-RL")) + 
#     ylab("Test Accuracy") + 
#     mytheme + theme(legend.key.height=unit(3,"line")) +
#     ggtitle(paste('Averaged Data and Model Predictions'))  
#   
#   if (bars) {
#     p1 <- p1 + geom_errorbar(aes(ymin = lower, ymax = upper),width=.075)
#   }
#   
#   p2 <- ggplot(mapping= aes_string(x="other_type",y=y[2],fill="variable")) +
#     geom_bar(data=df2[df2$variable %in% c("acc_plus","acc_neg"),], 
#              position='dodge',stat="identity") +
#     geom_point(data=df2[df2$variable %in% c("pred_acc_plus","pred_acc_neg"),],
#                position= position_dodge(width=.9),shape = 4,size = 3.5) +
#     facet_grid(.~timepoint) +   
#     scale_x_discrete("Retreival Cue Used", limits = c("Same Cue","New Cue")) + 
#     scale_fill_brewer("Practice\nAccuracy", 
#                       limits=c('acc_neg','acc_plus'), 
#                       labels=c("Incorrect","Correct"),
#                       palette="Set1") + 
#     scale_y_continuous("Accuracy", limits = c(0,1)) +
#     mytheme +
#     ggtitle(paste('Conditional Accuracy'))
#   
#   if (bars) {
#     p2 <- p2 + geom_errorbar(data=df2[df2$variable %in% c("acc_plus","acc_neg"),],
#                              aes(ymin = lower, ymax = upper),position = position_dodge(width = .9),width=.2) +
#       geom_errorbar(data=df2[df2$variable %in% c("pred_acc_plus","pred_acc_neg"),],
#                     aes(ymin = lower, ymax = upper),position = position_dodge(width = .9),width=.2)
#   }
#   
#   return(list(p1 = p1,p2=p2))
# }