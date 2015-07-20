plotPCL <- function(model="ss_std") {
  library(reshape2)
  library(ggplot2)
  library(dplyr)
  library(boot)
  library(Matrix)
  if(.Platform$OS.type == "unix") {
    root <- Sys.getenv("HOME")
    wd <- file.path("/opt","source","RPmodels","H_and_H_CFR","PCL")
  }   else if (.Platform$OS.type == "windows") {
    root <- Sys.getenv("USERPROFILE")
    wd <- file.path(Sys.getenv("USERPROFILE"),"source","RPmodels","H_and_H_CFR","PCL")
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
  if (any(class(m$results[[1]])=="optimx")) {
    params <- lapply(lapply(m$results, optimx:::coef.optimx),c)
    params <- lapply(params, setNames, colnames(optimx:::coef.optimx(m$results[[1]])))
    tmp <- vector(mode='list',length(unique(m$data$subject)))
    k=1
    for (i in unique(m$data$sub_num)) {
      tmp[[k]] <-m$fcn(free=params[[k]],fixed=m$fix,data=m$data[m$data$sub_num==i,])
      k=k+1
    }
  } else if (class(m$results[[1]]) =='list') {
    tmp <- lapply(m$results,`[[`,2)
  }
  preds <- do.call(rbind,lapply(tmp,`[[`,2)) 
  
  # munge the data into shape
  names(m$data) <- c("subject",names(m$data)[-1])
#   levels(preds$class) <- list(np = "prac", sp = "restudy", tp = "test")
  m$data$order[m$data$order==16] <- 14 # workaround for now
  m$data[,c("subject","class","order")] <- lapply(m$data[,c("subject","class","order")],factor)
  preds[,c("subject","class","order")] <- lapply(preds[,c("subject","class","order")],factor)
  
  if (length(unique(preds$subject))>1) {
    median_data <- m$data %>% group_by(subject,class,order) %>% 
      summarise(acc = sum(score)/4,
                medianRT = median(RT),
                medianCRT = median(CRT))
    median_preds <- preds %>% group_by(subject,class,order) %>% 
      summarise(pred_acc = mean(pred_acc),
                pred_medianRT = median(pred_RT),
                pred_medianCRT = median(pred_CRT))
    sub_data <- left_join(median_preds,median_data)
    sub_data$acc[is.na(sub_data$acc)] <- 0 # set na's for accuracy for zeros
    # data <- melt(data,id.vars=c("subject","class","order"),factorsAsStrings = TRUE)
    k <- 1
    for (i in unique(sub_data$subject)) {
      rtPlot <- ggplot(data =sub_data[sub_data$subject==i,],aes(x=order)) +
        geom_point(aes(y=medianRT,shape="data")) +
        geom_line(aes(y=medianRT,group=class)) +
        geom_point(aes(y=pred_medianRT,shape="model")) +
        geom_line(aes(y=pred_medianRT,group=class)) + 
        facet_grid(. ~ class) +
        scale_x_discrete("Output Item") + 
        scale_y_continuous("Median First Press RT") +
        scale_shape_manual("",values=c("data"=1,"model"=2),labels=c("Data","PCL")) + 
        mytheme + 
        ggtitle("Data vs. PCL: Median RT")
      
      accPlot <- ggplot(data =sub_data[sub_data$subject==i,],aes(x=order)) +
        geom_point(aes(y=acc,shape="data")) +
        geom_line(aes(y=acc,group=class)) +
        geom_point(aes(y=pred_acc,shape="model")) +
        geom_line(aes(y=pred_acc,group=class)) + 
        facet_grid(. ~ class) +
        scale_x_discrete("Output Item") + 
        scale_y_continuous("Accuracy") +
        scale_shape_manual("",values=c("data"=1,"model"=2),labels=c("Data","PCL")) + 
        mytheme + 
        ggtitle("Data vs. PCL: Accuracy")
    
      m$plots[[k]] <- list(RT=rtPlot,Acc = accPlot)
      k=k+1
    }
    aggData <- m$data %>% group_by(class,order) %>% 
      summarise(acc = sum(score)/(length(unique(subject))*4),
                medianRT = median(RT),
                medianCRT = median(CRT))
    aggData$acc[is.na(aggData$acc)] <- 0 
    aggData$type <- "data"
    aggPreds <- preds %>% group_by(class,order) %>% 
      summarise(acc = mean(pred_acc),
                medianRT = median(pred_RT),
                medianCRT = median(pred_CRT))
    aggPreds$type <- "preds"
    aggData <- rbind(aggData,aggPreds)
    aggData$type <- factor(aggData$type)
    aggRTPlot <- ggplot(data =aggData,aes(x=order,y=medianRT,shape=type)) +
      geom_point() +
      geom_line(aes(group=interaction(class,type))) +
      facet_grid(. ~ class) +
      scale_x_discrete("Output Item") + 
      scale_y_continuous("Median First Press RT") +
      # scale_shape_manual() + 
      mytheme + 
      ggtitle("Data vs. PCL: Agg. Median RT")
    
    aggAccPlot <- ggplot(data =aggData,aes(x=order,y=acc,shape=type)) +
      geom_point() +
      geom_line(aes(group=interaction(class,type))) +
      facet_grid(. ~ class) +
      scale_x_discrete("Output Item") + 
      scale_y_continuous("Accuracy") +
      # scale_shape_manual("",labels=c("Data","PCL")) + 
      mytheme + 
      ggtitle("Data vs. PCL: Agg. Median RT")
    m$plots[k] <- list(list(aggRT=aggRTPlot,aggAcc = aggAccPlot))
  }
  save(m,file = paste(model,"results.Rdata",sep="_"))
}
