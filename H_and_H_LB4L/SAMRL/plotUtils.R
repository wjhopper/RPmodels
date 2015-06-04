# plotting and fitting utilts

fit <- function(fileString, matlabString) { 
  setOption(matlab, "readResult/interval", 10); # Default is 1 second
  setOption(matlab, "readResult/maxTries", 30*(60/10)); # ~30 minutes  
  Matlab$startServer()
  matlab <- Matlab()
  isOpen <- open(matlab)
  tryCatch(evaluate(matlab, paste("[err, params, fits] = ",matlabString)),
           error = function(x) {close(matlab);stop(paste("Could not fit model", matlabString))})
#   evaluate(matlab, "names = fits.Properties.VarNames(:);")
#   evaluate(matlab, "fitscell = dataset2cell(fits);")
#   cnames <- unlist(getVariable(matlab,"names"))
#   fits <- getVariable(matlab,"fitscell")
  tryCatch(evaluate(matlab, "export(fits, 'file','fits.csv', 'Delimiter', ',');"),
           error = function(x) {close(matlab); stop(paste("Could not export results", matlabString))})
  fits <- read.csv('fits.csv',header=T,na.strings = "")
  file.remove('fits.csv')
  err <- getVariable(matlab, "err")
  err <- err[[1]]
  params <-  getVariable(matlab, "params")
  params<-as.data.frame(params[[1]])
  results<- list(err=err,params=params,fits=fits)
  save(results,file = fileString)
  close(matlab)
  return(results)
}

plot <- function(df1,df2) { 
  p1 <- ggplot(data=df1,aes(x=timepoint,y=pc, shape = variable,color = factor(chain),group=interaction(chain,variable))) +
    geom_point(size =3.5) +
    geom_line() + 
    scale_x_discrete("Test (Nested within Group)",expand=c(0,.25)) + 
    scale_color_discrete("Condition", breaks = c(1,2,3,5),
                         labels = c("Test Practice with Cue 1,\nFinal Test with Unpracticed Cue 2",
                                    "No Practice with Cue 1,\nFinal Test with Cue 1",
                                    "Study Practice with Cue 1,\nFinal Test with Cue 1",
                                    "Test Practice with Cue 1,\nFinal Test with Cue 1")) + 
    scale_shape_manual("Type",labels=c("Observed","SAM-RL"),values=c(19,4)) + 
    scale_y_continuous("Test Accuracy",limits= c(0,1)) + 
    mytheme + theme(legend.key.height=unit(3,"line"))
  
  p2 <- ggplot(mapping= aes(x=other_type,y=pc,fill=variable,ymax=1)) +
    geom_bar(data=df2[df2$variable %in% c("acc_plus","acc_neg"),], position='dodge',stat="identity") +
    geom_point(data=df2[df2$variable %in% c("pred_acc_plus","pred_acc_neg"),], position= position_dodge(width=.9), stat="identity",shape = 4,size = 3.5) +
    scale_x_discrete("Retreival Cue Used", limits = c("Same Cue","New Cue")) + 
    scale_fill_brewer("Practice\nAccuracy", 
                      limits=c('acc_neg','acc_plus'), 
                      labels=c("Incorrect","Correct"),
                      palette="Set1") + 
    scale_y_continuous("Accuracy", limits = c(0,1)) +
    mytheme
  
  return(list(p1 = p1,p2=p2))
}

plotMeans <- function(df1,df2,y = c('M','cM'),bars=FALSE) {
  
  p1 <- ggplot(data=df1,aes_string(x="timepoint",y=y[1], shape = "variable",color = "chain", ymax=.85,ymin=.15)) +
    geom_point(size =3.5) +
    #    geom_line(data=df1[df1$timepoint %in% c("Delayed Test", "Practice Test"),],  aes(group=interaction(chain,variable))) + 
    geom_line(data=df1[df1$timepoint %in% c("Delayed Test", "Immediate Test"),],  aes(group=interaction(chain,variable))) +
    geom_line(data=df1[df1$timepoint %in% c("Practice Test", "Immediate Test"),],  aes(group=interaction(group,chain,variable))) +   
    scale_x_discrete("Test (Nested within Group)",expand=c(0,.25)) + 
    scale_color_discrete("Condition",
                         labels = c("Test Practice with Cue 1,\nFinal Test with Unpracticed Cue 2",
                                    "No Practice with Cue 1,\nFinal Test with Cue 1",
                                    "Study Practice with Cue 1,\nFinal Test with Cue 1",
                                    "Test Practice with Cue 1,\nFinal Test with Cue 1")) + 
    scale_shape_manual("Type", values = c(19,4), labels=c("Observed.","SAM-RL")) + 
    ylab("Test Accuracy") + 
    mytheme + theme(legend.key.height=unit(3,"line")) +
    ggtitle(paste('Averaged Data and Model Predictions'))  
  
  if (bars) {
    p1 <- p1 + geom_errorbar(aes(ymin = lower, ymax = upper),width=.075)
  }
  
  p2 <- ggplot(mapping= aes_string(x="other_type",y=y[2],fill="variable")) +
    geom_bar(data=df2[df2$variable %in% c("acc_plus","acc_neg"),], 
             position='dodge',stat="identity") +
    geom_point(data=df2[df2$variable %in% c("pred_acc_plus","pred_acc_neg"),],
               position= position_dodge(width=.9),shape = 4,size = 3.5) +
    facet_grid(.~timepoint) +   
    scale_x_discrete("Retreival Cue Used", limits = c("Same Cue","New Cue")) + 
    scale_fill_brewer("Practice\nAccuracy", 
                      limits=c('acc_neg','acc_plus'), 
                      labels=c("Incorrect","Correct"),
                      palette="Set1") + 
    scale_y_continuous("Accuracy", limits = c(0,1)) +
    mytheme +
    ggtitle(paste('Conditional Accuracy'))

  if (bars) {
    p2 <- p2 + geom_errorbar(data=df2[df2$variable %in% c("acc_plus","acc_neg"),],
                                 aes(ymin = lower, ymax = upper),position = position_dodge(width = .9),width=.2) +
      geom_errorbar(data=df2[df2$variable %in% c("pred_acc_plus","pred_acc_neg"),],
                    aes(ymin = lower, ymax = upper),position = position_dodge(width = .9),width=.2)
  }
  
    return(list(p1 = p1,p2=p2))
}