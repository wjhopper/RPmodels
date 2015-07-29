plotPCL <- function(model="std") {
  
  library(reshape2)
  library(ggplot2)
  library(dplyr)
  library(boot)
  
  if(.Platform$OS.type == "unix") {
    root <- Sys.getenv("HOME")
    wd <- file.path("/opt","source","RPmodels","H_and_H_LB4L","PCL")
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
  }
  
  params <- lapply(lapply(m$results, optimx:::coef.optimx),c)
  params <- lapply(params, setNames, colnames(optimx:::coef.optimx(m$results[[1]])))
  
  k=1
  tmp <- vector(mode='list',length(unique(m$data$subject)))
  for (i in unique(m$data$subject)) {
    tmp[[k]] <-m$fcn(free=params[[k]],fixed=m$fix,data=m$data[m$data$subject==i,])
    k=k+1
  }
  
  data <- do.call(rbind,tmp)
  data[, c("subject","group", "practice", "other_type","chain")] <- 
    lapply(data[,c("subject","group", "practice", "other_type","chain")],factor,exclude=NULL)
  IVdata <- melt(data, id.vars = c("subject","group", "practice", "other_type","chain"), 
       measure.vars = c("prac_acc","pred_prac_acc","final_acc","pred_final_acc"),
       value.name = 'pc') %>% 
    mutate(type = factor(ifelse(grepl("pred",variable,fixed=TRUE),'model','real')),
           timepoint = factor(ifelse(grepl("prac",variable,fixed=TRUE),1, ifelse(grepl("delay",group,fixed=TRUE),3,2))),
           grouping = factor(paste(type,strSort(as.character(interaction(practice,other_type)),'.'),sep='.')))
  conAcc <- melt(data, id.vars = c("subject","group", "practice", "other_type","chain"), 
              measure.vars = c("acc_plus","pred_acc_plus","acc_neg","pred_acc_neg"),
              value.name = 'pc')  %>% 
    mutate(type = factor(ifelse(grepl("pred",variable,fixed=TRUE),'model','real')),
           prac_acc = factor(ifelse(grepl("plus",variable,fixed=TRUE),1,0)),
           timepoint = factor(ifelse(grepl("prac",variable,fixed=TRUE),1, ifelse(grepl("delay",group,fixed=TRUE),3,2))),
           grouping = factor(paste(type,strSort(as.character(interaction(practice,other_type)),'.'),sep='.')))
  conN <- melt(data, id.vars = c("subject","group", "practice", "other_type","chain"), 
                 measure.vars = c("nplus","nneg"),
                 value.name = 'n') %>% 
    mutate(prac_acc = factor(ifelse(grepl("plus",variable,fixed=TRUE),1,0)))
  conAcc <-left_join(conAcc,select(conN,subject, group, practice, other_type, chain,prac_acc,n))

  if (length(unique(data$timepoint[data$subject == unique(data$subject)[1]]))>2) {
    IVplot_grouped <- ggplot(filter(IVdata_grouped,is.finite(pc)) %>% 
                               mutate(group_imm_to_del = replace(group,which(group=='delay' & timepoint ==3),'immediate'),
                                      group_del_to_imm = replace(group,which(group=='immediate' & timepoint ==2),'delay'),
                                      grouping1 =  factor(paste(type,group_imm_to_del,
                                                                strSort(as.character(interaction(practice,other_type)),'.'), 
                                                                sep='.')),
                                      grouping2 =  factor(paste(type,group_del_to_imm,
                                                                strSort(as.character(interaction(practice,other_type)),'.'),
                                                                sep='.'))),
                             aes(x=timepoint, shape = type, y=pc,ymax=.85,ymin=.15)) +
      geom_line(aes(group = grouping1,linetype=grouping1),size=.75) +
      geom_line(aes(group = grouping2,linetype=grouping2),size=.75) +
      geom_point(aes(color=practice),size=3) +
      scale_linetype_manual("Final Test Cue",
                            values=c("model.delay.C.NA" =1 ,"model.delay.C.T"=2, 
                                     "model.delay.NA.S"=1, "model.delay.NA.T"=1,
                                     "model.immediate.C.NA"=1,"model.immediate.C.T"=2,
                                     "model.immediate.NA.S"=1, "model.immediate.NA.T"=1,
                                     "real.delay.C.NA"=1,"real.delay.C.T"=2,
                                     "real.delay.NA.S"=1, "real.delay.NA.T"=1,
                                     "real.immediate.C.NA"=1,"real.immediate.C.T"=2,
                                     "real.immediate.NA.S"=1, "real.immediate.NA.T"=1),
                            limits=c("model.delay.NA.T","model.delay.C.T"),
                            labels=c("Practiced Cue","Unpracticed Cues")) +
      scale_x_discrete("Group",expand=c(0,.25),labels=c("Immediate","Delay")) + 
      scale_color_discrete("Practice Method",labels = c("No Practice", "Restudy Practice","Cued Recall Practice")) + 
      ylab("Final Test Accuracy") + 
      mytheme +
      ggtitle('Final Cued Recall Accuracy By Practice Type and Cue Type')
    
    conPlot_grouped <-  ggplot(filter(conAcc_grouped,type=='real'),
                               aes(x=other_type, y= pc,fill=prac_acc,ymax=1)) + 
      geom_bar(position='dodge',stat="identity") +
      geom_point(shape= 19, size = 3, position = position_dodge(width=0.9),
                 data = filter(conAcc_grouped,type=='model')) +
      # label n observations in each cell
      geom_text(aes(y=-.025,label =n, group=prac_acc),
                position = position_dodge(width=0.9)) +
      facet_grid(. ~ group, labeller=function(...) { return(c("Immediate","Delayed"))}) +
      scale_fill_brewer("Practice\nAccuracy", 
                        breaks=c(0,1), 
                        labels=c("Incorrect", "Correct"),
                        palette="Set1") +  
      scale_x_discrete("Final Test Cue", limits=c(NA,'T'),labels=c("Practiced","Unpracticed")) +
      scale_y_continuous("Final Test Accuracy",expand=c(0,.025)) +
      mytheme + 
      ggtitle('Conditional Final Test Accuracy')
    m$plots <- list(accAgg = IVplot_grouped, condAccAgg = conPlot_grouped)
    
  } else {
    k=1
    m$plots <- vector(mode='list',length=length(unique(m$data$subject)))
    for (i in unique(m$data$subject)) {
      IVplot <- ggplot(data= filter(IVdata, subject==i,is.finite(pc)),
                       aes(x=timepoint,y=pc,linetype = grouping, shape =type, group = grouping)) +
        geom_point(aes(color=practice),size=3) +    
        geom_line(size=.75) + 
        scale_x_discrete("Test",expand=c(0,.25),labels=c("Practice","Final")) + 
        scale_color_discrete("Practice",labels= c("No Practice", "Study Practice","Test practice")) +
        scale_shape_discrete("Type", labels=c("PCL","Obs. Data")) +
        scale_linetype_manual("Final Test Cue",values=c("model.C.NA" = 1,"model.C.T" =2, "model.NA.S" =1, 
                                                        "model.NA.T" =1, "real.C.NA" = 1,"real.C.T"  =2,
                                                        "real.NA.S" = 1, "real.NA.T"=1),
                              limits=c("model.NA.T","model.C.T"),labels=c("Practiced Cue","Unpracticed Cues")) +
        scale_y_continuous("Final Test Accuracy",limit=0:1) + 
        mytheme + 
        ggtitle('Test Accuracy')
      
      conPlot <- ggplot(filter(conAcc,subject ==i,type=='real'),
                   aes(x=other_type, y= pc,fill=prac_acc,ymax=1)) + 
        geom_bar(position='dodge',stat="identity") +
        geom_point(shape= 19, size = 3, position = position_dodge(width=0.9),
                   data = filter(conAcc,subject ==i,type=='model')) +
        # label n observations in each cell
        geom_text(aes(y=-.025,label =n, group=prac_acc),
                  position = position_dodge(width=0.9)) +
        scale_fill_brewer("Practice\nAccuracy", 
                          breaks=c(0,1), 
                          labels=c("Incorrect", "Correct"),
                          palette="Set1") +  
        scale_x_discrete("Final Test Cue", limits=c(NA,'T'),labels=c("Practiced","Unpracticed")) +
        scale_y_continuous("Final Test Accuracy",expand=c(0,.025)) +
        mytheme + 
        ggtitle('Conditional Final Test Accuracy')
      
      m$plots[[k]] <- list(acc= IVplot, cond_acc = conPlot)
      k=k+1
  }
    IVdata_grouped <- IVdata %>% group_by(group,practice,other_type,chain, type, timepoint, grouping,variable) %>% 
      summarise(pc= mean(pc)) %>% ungroup() %>%
      mutate(grouping = paste(group,grouping,sep='.'))

    conAcc_grouped <- conAcc %>% group_by(group,practice,other_type,chain, type, timepoint, grouping,prac_acc,variable) %>% 
      summarise(pc= weighted.mean(pc,w = n),
                n = sum(n))  %>% ungroup() %>%
      mutate(grouping = paste(group,grouping,sep='.'))
    conAcc_grouped$group <- factor(conAcc_grouped$group, levels = c("immediate","delay"))
    
    IVplot_grouped <- ggplot(filter(IVdata_grouped,is.finite(pc)) %>% 
             mutate(group_imm_to_del = replace(group,which(group=='delay' & timepoint ==3),'immediate'),
                    group_del_to_imm = replace(group,which(group=='immediate' & timepoint ==2),'delay'),
                    grouping1 =  factor(paste(type,group_imm_to_del,
                                              strSort(as.character(interaction(practice,other_type)),'.'), 
                                              sep='.')),
                    grouping2 =  factor(paste(type,group_del_to_imm,
                                              strSort(as.character(interaction(practice,other_type)),'.'),
                                              sep='.'))),
           aes(x=timepoint, shape = type, y=pc,ymax=.85,ymin=.15)) +
      geom_line(aes(group = grouping1,linetype=grouping1),size=.75) +
      geom_line(aes(group = grouping2,linetype=grouping2),size=.75) +
      geom_point(aes(color=practice),size=3) +
      scale_linetype_manual("Final Test Cue",
                            values=c("model.delay.C.NA" =1 ,"model.delay.C.T"=2, 
                                     "model.delay.NA.S"=1, "model.delay.NA.T"=1,
                                     "model.immediate.C.NA"=1,"model.immediate.C.T"=2,
                                     "model.immediate.NA.S"=1, "model.immediate.NA.T"=1,
                                     "real.delay.C.NA"=1,"real.delay.C.T"=2,
                                     "real.delay.NA.S"=1, "real.delay.NA.T"=1,
                                     "real.immediate.C.NA"=1,"real.immediate.C.T"=2,
                                     "real.immediate.NA.S"=1, "real.immediate.NA.T"=1),
                            limits=c("model.delay.NA.T","model.delay.C.T"),
                            labels=c("Practiced Cue","Unpracticed Cues")) +
      scale_x_discrete("Group",expand=c(0,.25),labels=c("Immediate","Delay")) + 
      scale_color_discrete("Practice Method",labels = c("No Practice", "Restudy Practice","Cued Recall Practice")) + 
      ylab("Final Test Accuracy") + 
      mytheme +
      ggtitle('Cued Recall Accuracy')
    
    conPlot_grouped <-  ggplot(filter(conAcc_grouped,type=='real'),
                     aes(x=other_type, y= pc,fill=prac_acc,ymax=1)) + 
      geom_bar(position='dodge',stat="identity") +
      geom_point(shape= 19, size = 3, position = position_dodge(width=0.9),
                 data = filter(conAcc_grouped,type=='model')) +
      # label n observations in each cell
      geom_text(aes(y=-.025,label =n, group=prac_acc),
                position = position_dodge(width=0.9)) +
      facet_grid(. ~ group, labeller=function(...) { return(c("Immediate","Delayed"))}) +
      scale_fill_brewer("Practice\nAccuracy", 
                        breaks=c(0,1), 
                        labels=c("Incorrect", "Correct"),
                        palette="Set1") +  
      scale_x_discrete("Final Test Cue", limits=c(NA,'T'),labels=c("Practiced","Unpracticed")) +
      scale_y_continuous("Final Test Accuracy",expand=c(0,.025)) +
      mytheme + 
      ggtitle('Conditional Final Test Accuracy')
    
    m$plots[[k]] <- list(accAgg = IVplot_grouped, condAccAgg = conPlot_grouped)
    
    save(m,file = paste(model,"results.Rdata",sep="_"))
  }  
  return(m)
}
