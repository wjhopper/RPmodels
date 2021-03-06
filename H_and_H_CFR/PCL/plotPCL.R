plotPCL <- function(model="ss_std",plotListOnly = TRUE) {
  library(reshape2)
  library(ggplot2)
  library(dplyr)

  classNames <- list(
    'np'="No Practice",
    'sp'="Study Practice",
    'tp'="Test Practice"
  )  
  
  classLabeller <- function(variable,value) {        
    if (variable=='class') {
      return(classNames[value])
    } else {
      return(value)
    }
  }
  
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
  m$plots <- vector(mode='list',length=length(unique(m$data$sub_num)))
  if (any(class(m$results[[1]])=="optimx")) {
    params <- lapply(lapply(m$results, optimx:::coef.optimx),c)
    params <- lapply(params, setNames, colnames(optimx:::coef.optimx(m$results[[1]])))
    tmp <- vector(mode='list',length(unique(m$data$sub_num)))
    k=1
    for (i in unique(m$data$sub_num)) {
      tmp[[k]] <-m$fcn(free=params[[k]],fixed=m$fix,data=m$data[m$data$sub_num==i,])
      k=k+1
    }
  } else if (class(m$results[[1]]) =='list') {
    tmp <- lapply(m$results,`[[`,2)
  }
  preds <- do.call(rbind,lapply(tmp,`[[`,2))
  dist <- do.call(rbind,lapply(tmp,`[[`,3))
  dist$sub_num <- rep(unique(m$data$sub_num),each= nrow(dist)/length(unique(m$data$sub_num)))
  # munge the data into shape
  m$data[,c("sub_num","class","order")] <- lapply(m$data[,c("sub_num","class","order")],factor)
  preds[,c("sub_num","class","order")] <- lapply(preds[,c("sub_num","class","order")],factor)
  
  if (length(unique(preds$sub_num))>1) {
    median_data <- m$data %>% group_by(sub_num,class,order) %>% 
      summarise(acc = sum(score)/4,
                medianRT = median(RT))
    median_preds <- preds %>% group_by(sub_num,class,order) %>% 
      summarise(pred_acc = mean(pred_acc),
                pred_medianRT = median(pred_RT,na.rm=TRUE))
    sub_data <- left_join(median_preds,median_data)
    sub_data$acc[is.na(sub_data$acc)] <- 0 # set na's for accuracy for zeros

    k <- 1
    for (i in unique(sub_data$sub_num)) {
      rtPlot <- ggplot(data =sub_data[sub_data$sub_num==i,],aes(x=order)) +
        geom_point(aes(y=medianRT,shape="data")) +
        geom_line(aes(y=medianRT,group=class)) +
        geom_point(aes(y=pred_medianRT,shape="model")) +
        geom_line(aes(y=pred_medianRT,group=class)) + 
        facet_grid(. ~ class,labeller = classLabeller) +
        scale_x_discrete("Output Item") + 
        scale_y_continuous("Median First Press RT") +
        scale_shape_manual("",values=c("data"=1,"model"=2),labels=c("Obs. Data","PCL Model")) + 
        mytheme + 
        ggtitle("Data vs. PCL: Median RT")
      
      accPlot <- ggplot(data =sub_data[sub_data$sub_num==i,],aes(x=order)) +
        geom_point(aes(y=acc,shape="data")) +
        geom_line(aes(y=acc,group=class)) +
        geom_point(aes(y=pred_acc,shape="model")) +
        geom_line(aes(y=pred_acc,group=class)) + 
        facet_grid(. ~ class,labeller = classLabeller) +
        scale_x_discrete("Output Item") + 
        scale_y_continuous("Accuracy") +
        scale_shape_manual("",values=c("data"=1,"model"=2),labels=c("Data","PCL")) + 
        mytheme + 
        ggtitle("Data vs. PCL: Accuracy")
      
      densityPlot <- ggplot(data =dist[dist$sub_num==i,],aes(x=RTrounded,y=RTdist)) + 
        geom_line(size=.75) +
        facet_grid(class~order) +
        geom_point(data = m$data[m$data$sub_num==i,],
                   mapping=aes(x=RTrounded),y=0,color="blue") + 
        scale_x_continuous(limits=c(0,max(m$data$RTrounded[m$data$sub_num==i]+5))) + 
        mytheme + theme(axis.text.x = element_text(size=rel(1))) + 
        ggtitle("Model Density")
    
      m$plots[[k]] <- list(RT=rtPlot,Acc = accPlot, density = densityPlot)
      k=k+1
    }
    aggData <- m$data %>% group_by(class,order) %>% 
      summarise(acc = sum(score)/(length(unique(sub_num))*4),
                err = sqrt(sum((score-acc)^2)/length(unique(sub_num))*4)/sqrt(length(unique(sub_num))*4),
                medianRT = median(RT),
                mad = median(abs(RT-medianRT),na.rm=TRUE)) %>%
      mutate(acc = replace(acc,is.na(acc),0),
             type = factor("data"))
    aggPreds <- preds %>% group_by(class,order) %>% 
      summarise(acc = mean(pred_acc),
                err = sd(pred_acc)/sqrt(n()),
                medianRT = median(pred_RT,na.rm=TRUE),
                mad = median(abs(pred_RT-medianRT),na.rm=TRUE)) %>%
      mutate(type = factor("model"))
    aggData <- rbind(aggData,aggPreds)
    
    aggDist <- dist %>% group_by(class,order,RTrounded) %>%
      summarise(RTdist = mean(RTdist))

    aggRTPlot <- ggplot(data =aggData,aes(x=order,y=medianRT,shape=type)) +
      geom_point() +
      geom_line(aes(group=interaction(class,type), linetype=type)) +
      geom_errorbar(aes(ymax=medianRT+mad, ymin=medianRT-mad),width=.15) +
      facet_grid(. ~ class,labeller = classLabeller) +
      scale_x_discrete("Output Item") + 
      scale_y_continuous("Median First Press RT") +
      scale_shape_manual("",values=c("data"=1,"model"=2),labels=c("Obs. Data","PCL Model")) + 
      scale_linetype_discrete(guide=FALSE) + 
      mytheme + 
      ggtitle("Data vs. PCL: Agg. Median RT")
    
    aggAccPlot <- ggplot(data =aggData,aes(x=order,y=acc,shape=type)) +
      geom_point() +
      geom_line(aes(group=interaction(class,type), linetype=type)) +
      facet_grid(. ~ class,labeller = classLabeller) +
      scale_x_discrete("Output Item") + 
      scale_y_continuous("Accuracy") +
      scale_shape_manual("",values=c("data"=1,"model"=2),labels=c("Obs. Data","PCL Model")) + 
      scale_linetype_discrete(guide=FALSE) + 
      mytheme + 
      ggtitle("Data vs. PCL: Accuracy")
    
    aggDensityPlot <- ggplot(data =aggDist,aes(x=RTrounded,y=RTdist)) + 
      geom_line(size=.75) +
      facet_grid(class~order) +
      geom_point(data = m$data,
                 mapping=aes(x=RTrounded),y=0,color="blue") + 
      scale_x_continuous(limits=c(0,max(m$data$RTrounded+2.5))) +
      mytheme + theme(axis.text.x = element_text(size=rel(1))) + 
      ggtitle("Average Model Density")
    m$plots[k] <- list(list(aggRT=aggRTPlot,aggAcc = aggAccPlot, aggDensity = aggDensityPlot))
  }
  
  # restore to typing when originally read in
  m$data$order <- as.numeric(levels(m$data$order))[m$data$order]
  
  # save the model object which has had plots added to it
  save(m,file = paste(model,"results.Rdata",sep="_"))
  
  if (plotListOnly) {
    return(m$plots)
  } else {
    return(m)
  }

}


empDensityPlot <- function(data=NULL) {
  dataFull <-expand.grid(class=levels(data$class), order= levels(factor(data$order)),
                         time = seq(.1,90,.1),density=NA)
  for (i in unique(dataFull$class)) {
    for(j in unique(dataFull$order)) {
      D <- density(data$CRT[data$class==i & data$order==j],bw=1,n=900,from=.1,to=90)$y
      D <-D/sum(D)
      D<-(sum(data$class==i & data$order==j)/(34*4)) * D
      dataFull$density[dataFull$class==i & dataFull$order==j] <- D
    }
  }
  dataFull <- dataFull %>% group_by(class) %>% 
    mutate(densityNormalized = density/sum(density))
  
  densityPlot <- ggplot(dataFull,aes(x=time,y=densityNormalized,color=class))+
    geom_line() +
    facet_grid(.~order) + 
    scale_x_continuous(limits=c(0,max(data$CRT+5))) + 
    scale_color_discrete("Condition",labels=c("No Practice", "Study", "Test"))
  return(densityPlot)
}