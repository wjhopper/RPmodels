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
  m$plots <- vector(mode='list',length=length(unique(m$data$subject)))
  
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
           timepoint = factor(ifelse(grepl("prac",variable,fixed=TRUE),1, 
                                     ifelse(grepl("delay",group,fixed=TRUE),3,2))),
           chain = replace(chain,chain==4,1),
           xaxis = interaction(chain,timepoint))
  conAcc <- melt(data, id.vars = c("subject","group", "practice", "other_type","chain"), 
              measure.vars = c("acc_plus","pred_acc_plus","acc_neg","pred_acc_neg"),
              value.name = 'pc')  %>% 
    mutate(type = factor(ifelse(grepl("pred",variable,fixed=TRUE),'model','real')),
           prac_acc = factor(ifelse(grepl("plus",variable,fixed=TRUE),1,0)),
           timepoint = factor(ifelse(grepl("prac",variable,fixed=TRUE),1, 
                                     ifelse(grepl("delay",group,fixed=TRUE),3,2))))
  conN <- melt(data, id.vars = c("subject","group", "practice", "other_type","chain"), 
                 measure.vars = c("nplus","nneg"),
                 value.name = 'n') %>% 
    mutate(prac_acc = factor(ifelse(grepl("plus",variable,fixed=TRUE),1,0)))
  conAcc <-left_join(conAcc,select(conN,subject, group, practice, other_type, chain,prac_acc,n))
  
  jointAcc <- melt(data, id.vars = c("subject","group", "practice", "other_type","chain"),
                   measure.vars = c("prac_and_final","prac_and_not_final","not_prac_and_final",
                                    "not_prac_and_not_final","pred_prac_and_final",
                                    "pred_prac_and_not_final","pred_not_prac_and_final",
                                    "pred_not_prac_and_not_final"),
                   value.name = 'pc') %>% 
    mutate(type = factor(ifelse(grepl("pred",variable,fixed=TRUE),'model','real')),
           prac_acc = factor(ifelse(grepl("not_prac",variable,fixed=TRUE),0,1),labels = c("inc","cor")),
           final_acc = factor(ifelse(grepl("not_final",variable,fixed=TRUE),0,1),labels = c("inc","cor")),
           xaxis = interaction(chain,prac_acc,final_acc))
  
  if (length(unique(IVdata$timepoint[IVdata$subject == unique(IVdata$subject)[1]]))==2) {
    k=1
    for (i in unique(m$data$subject)) {
      IVplot <- ggplot(data= filter(IVdata, subject==i, is.finite(pc)),
                       aes(x=timepoint,y=pc,color=chain, shape =type, group =interaction(chain,type))) +
        geom_point(size=3) +    
        geom_line(size=.75) + 
        scale_x_discrete("Test",expand=c(0,.25),labels=c("Practice","Final")) + 
        scale_color_discrete("Condition",labels = c("2 Cues, 1=Tested & 2=Unpracticed",
                                                    "1 Cue, No Practice",
                                                    "1 Cue, Restudied",
                                                    "1 Cue, Tested")) + 
        scale_shape_discrete("Type", labels=c("real" = "Obs. Data","model" = "PCL Model")) +
        scale_y_continuous("Final Test Accuracy",limit=0:1) + 
        mytheme + 
        theme(legend.key.height=unit(2,"line")) + 
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
      
      jointPlot <- ggplot(data = rbind(filter(IVdata, subject==i,timepoint != 1,chain %in% 2:3) %>%
                                         select(type,xaxis,pc),
                                       filter(jointAcc, subject==i,chain %in% c(1,5)) %>%
                                         select(type,xaxis,pc)) %>%
                            mutate(cues = factor(ifelse(grepl("1.",xaxis),2,1))),
                          aes(x=xaxis, y= pc, color = type)) + 
        geom_point(size= 3) +
        scale_x_discrete(labels = c("2.2" = "Control","3.2" = "Restudy",
                                    "2.3" = "Control","3.3" = "Restudy",
                                    "1.cor.cor" ="2 Cue: Cor. & Cor.",
                                    "1.cor.inc"  = "2 Cues: Cor. & Inc.",
                                    "1.inc.cor"  = "2 Cue: Inc. & Cor.",
                                    "1.inc.inc" = "2 Cue: Inc. & Inc.",
                                    "5.cor.cor"  = "1 Cue: Cor & Cor", 
                                    "5.cor.inc" = "1 Cue: Cor. & Inc.", 
                                    "5.inc.cor"  = "1 Cue: Inc. & Cor.",
                                    "5.inc.inc"  = "1 Cue: Inc. & Inc."),
                         name = "Condition") +
        scale_color_discrete("Type", labels=c("real" = "Obs. Data","model" = "PCL Model")) + 
        scale_y_continuous("Proportion",limit=0:1) + 
        mytheme + theme(axis.text.x = element_text(size=rel(1))) +
        ggtitle("All Condtions")
      
      m$plots[[k]] <- list(acc= IVplot, cond_acc = conPlot, joint_acc = jointPlot)
      k=k+1
    }
    
    IVdata_grouped <- IVdata %>% group_by(group,practice,other_type,chain, type, timepoint, xaxis,variable) %>% 
      summarise(pc= mean(pc)) %>% ungroup() %>%
      mutate(chain = replace(chain,chain==4,1))
    
    conAcc_grouped <- conAcc %>% group_by(group,practice,other_type,chain, type, timepoint, prac_acc,variable) %>% 
      summarise(pc= mean(pc,na.rm=TRUE),
                n = sum(n,na.rm=TRUE))  %>% ungroup()
    
    conAcc_grouped$group <- factor(conAcc_grouped$group, levels = c("immediate","delay"))
    
    jointAcc_grouped <- jointAcc  %>% group_by(group,practice,other_type,chain, type, prac_acc, final_acc, xaxis, variable) %>%
      summarise(pc = mean(pc,na.rm=TRUE))  %>% ungroup()
  } else {
    k=1
    IVdata_grouped <- IVdata 
    conAcc_grouped <- conAcc
    conAcc_grouped$group <- factor(conAcc_grouped$group, levels = c("immediate","delay"))
    jointAcc_grouped <- jointAcc 
  }
 
 
  IVplot_grouped <- ggplot(filter(IVdata_grouped,is.finite(pc)) %>% 
                     mutate(group_del_to_imm = replace(group,which(group=='delay' & timepoint ==3),'immediate'),
                            group_imm_to_del = replace(group,which(group=='immediate' & timepoint ==2),'delay'),
                            grouping1 = interaction(group_del_to_imm,chain,type),
                            grouping2 = interaction(group_imm_to_del,chain,type)),
                   aes(x=timepoint, shape = type, color=chain, y=pc,ymax=.85,ymin=.15)) +
    geom_point(size=3) +
    geom_line(aes(group = grouping1),size=.75) +
    scale_color_discrete("Condition",labels = c("2 Cues, 1=Tested & 2=Unpracticed",
                                                "1 Cue, No Practice",
                                                "1 Cue, Restudied",
                                                "1 Cue, Tested")) + 
    scale_shape_discrete("Type", labels=c("real" = "Obs. Data","model" = "PCL Model")) + 
    scale_x_discrete("Group",expand=c(0,.25),labels=c("Practice","Immediate","Delay")) + 
    ylab("Final Test Accuracy") + 
    mytheme +  theme(legend.key.height=unit(2,"line")) + 
    ggtitle('Cued Recall Accuracy')
  
  conPlot_grouped <-  ggplot(filter(conAcc_grouped, is.finite(pc), type=='real'),
                             aes(x=other_type, y= pc,fill=prac_acc,ymax=1)) + 
    geom_bar(position='dodge',stat="identity") +
    geom_point(shape= 19, size = 3, position = position_dodge(width=0.9),
               data = filter(conAcc_grouped,type=='model')) +
    # label n observations in each cell
    geom_text(aes(y=-.025,label =n, group=prac_acc),
              position = position_dodge(width=0.9)) +
    facet_grid(. ~ group,labeller=function(...) { return(c("Immediate","Delayed"))}) +
    scale_fill_brewer("Practice\nAccuracy", 
                      breaks=c(0,1), 
                      labels=c("Incorrect", "Correct"),
                      palette="Set1") +  
    scale_x_discrete("Final Test Cue", limits=c(NA,'T'),labels=c("Practiced","Unpracticed")) +
    scale_y_continuous("Final Test Accuracy",expand=c(0,.025)) +
    mytheme + 
    ggtitle('Conditional Final Test Accuracy')
  
  jointPlot_grouped <- ggplot(data = rbind(filter(IVdata_grouped, timepoint != 1,chain %in% 2:3) %>%
                                           select(group,type,xaxis,pc),
                                         filter(jointAcc_grouped, chain %in% c(1,5)) %>%
                                           select(group,type,xaxis,pc)) %>%
                              mutate(xaxis = replace(xaxis,xaxis=="2.3", "2.2"),
                                     xaxis = replace(xaxis,xaxis=="3.3", "3.2"),
                                     cues = factor(ifelse(grepl("1.",xaxis),2,1))),
                            aes(x=xaxis, y= pc, color = type)) + 
    geom_point(size= 3) +
    facet_grid(group~. ) + #,labeller=function(...) { return(c("Immediate","Delayed"))}) + 
    scale_x_discrete(labels = c("2.2" = "Control","3.2" = "Restudy",
                                "1.cor.cor" ="2 Cue: Cor. & Cor.",
                                "1.cor.inc"  = "2 Cues: Cor. & Inc.",
                                "1.inc.cor"  = "2 Cue: Inc. & Cor.",
                                "1.inc.inc" = "2 Cue: Inc. & Inc.",
                                "5.cor.cor"  = "1 Cue: Cor & Cor", 
                                "5.cor.inc" = "1 Cue: Cor. & Inc.", 
                                "5.inc.cor"  = "1 Cue: Inc. & Cor.",
                                "5.inc.inc"  = "1 Cue: Inc. & Inc."),
                     name = "Condition") +
    scale_color_discrete("Type", labels=c("real" = "Obs. Data","model" = "PCL Model")) + 
    scale_y_continuous("Proportion",limit=0:1) + 
    mytheme + theme(axis.text.x = element_text(size=rel(1))) +
    ggtitle("All Condtions")
  
  m$plots[[k]] <- list(accAgg = IVplot_grouped, condAccAgg = conPlot_grouped,
                       jointAccAgg = jointPlot_grouped)
  save(m,file = paste(model,"results.Rdata",sep="_"))
    
  return(m)
}
