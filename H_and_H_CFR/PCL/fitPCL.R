fitPCL <- function(model=1,inpar = FALSE,...,debugLevel = 0) {
  library(optimx)
  library(dplyr)
  library(reshape2)
  is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1]) 
  needed <- list("foreach","doParallel")

  if (inpar) {
    packs <- all(sapply(needed,is.installed))
    if (all(packs)) {
      library(foreach)
      library(doParallel)
      cl <- makeCluster(detectCores()-1)
      registerDoParallel(cl)
      #     library(doRNG)
      #     registerDoRNG(456)
    } else {
      message("Packages foreach and doParallel not found to do parallel processing, falling back")
      inpar = FALSE
    }
  } 
  ## Set wd, depending on platform
  if (any(c(is.null(sys.call(-1)), if (!is.null(sys.call(-1))){!agrepl(as.character((sys.call(-1))),"plotPCL")}else{TRUE}))) {
    if(.Platform$OS.type == "unix") {
      root <- Sys.getenv("HOME")
      wd <- file.path("/opt","source","RPmodels","H_and_H_CFR","PCL")
    }   else if (.Platform$OS.type == "windows") {
      root <- Sys.getenv("USERPROFILE")
      wd <- file.path(Sys.getenv("USERPROFILE"),"source","RPmodels","H_and_H_CFR","PCL")
    }
    setwd(wd)
  }
  source('PCL.R')
  
  if (debugLevel[1]>0){
#     trace(PCL,browser,debugLevel[2])
    setBreakpoint('PCL.R', debugLevel[2], envir = fitPCL)
  }
  
  SS_data <- cbind(read.csv(file.path('..','CFRssRaw.csv')),pred_acc = NA,
                   pred_acc_plus= NA,pred_acc_neg= NA) %>% 
    group_by(sub_num) %>% 
    mutate(abs_list =rep(1:12,as.vector(table(list,phase)[,2:1][table(list,phase)[,2:1]>0]))) %>% 
    group_by(sub_num, abs_list)   %>% 
    filter(score == 1 ) %>%
    mutate(CRT = cumsum(RT),
           RTrounded = round(RT,1),
           order = 1:n(),
           Nrecalled = sum(score)) %>% 
    arrange(desc(phase),practice) # dont need to arrange by subject since the data frame is grouped by it
  # SS_data[,c('sub_num','class')] <- lapply(SS_data[,c('sub_num','class')],factor)
  
  models <- list('ss_std' = list(fcn = PCL, free= c(ER=.53,LR=.1,TR =.05, FR=.05),
                                 fix= c(theta=.5,nFeat=100,nSim=1000,nList=15,lambda=.2,Tmin=2,Tmax=10,Time=90), 
                                 yoke = NULL,data=SS_data,
                                 low = -Inf, up = Inf, results = vector(mode="list",length=length(unique(SS_data$sub_num)))),
                 'ss_timeFree' = list(fcn = PCL,free= c(ER=.53,LR=.1,TR =.05, FR=.05,Tmin=2, Tmax=10, lambda=.25), 
                                      fixed = c(theta=.5,nFeat=100,nSim=1000,nList=15,Time=90),
                                      yoke = NULL, data=SS_data,
                                      low = -Inf, up = Inf),
                 'ss_timeFree_noForgetting' = list(fcn = PCL,free= c(ER=.53,LR=.1,TR =.05, Tmin=2, Tmax=10, lambda=.25), 
                                      fixed = c(FR=0, theta=.5,nFeat=100,nSim=1000,nList=15,Time=90),
                                      yoke = NULL, data=SS_data,
                                      low = -Inf, up = Inf),
                 'ss_lambdaFree' = list(fcn = PCL,free= c(ER=.53,LR=.1,TR =.05, FR=.05, lambda=.25), 
                                      fixed = c(theta=.5,nFeat=100,nSim=1000,nList=15,Tmin=2, Tmax=10,Time=90),
                                      yoke = NULL, data=SS_data,
                                      low = -Inf, up = Inf))
  
  for (i in model) {
    reqParams <- c(names(formals(models[[i]]$fcn)$free), names(formals(models[[i]]$fcn)$fix))
    reqParams <- reqParams[!reqParams %in%  c("","Tmin","Tmax","lambda")]
    givenParams <- names(c(models[[i]]$free,models[[i]]$fix))
    if (!all( reqParams[reqParams != ""]  %in% givenParams)) {
      stop(paste(reqParams[!reqParams %in% givenParams], " not specified in model,check model input list"))
    }
  }
  
  if (inpar) {
    #begin parallel method
    
    cat(paste('[',Sys.time(),']',"INIT parlog"), file=file.path(wd,"parlog.txt"),
        sep='\n',append = TRUE)
    clusterExport(cl,c("recallNoTime","recallTime","LL", "SSE","RTdis"))
    
    results <- foreach(m=models[model],export="wd") %:%
      foreach(j =unique(m$data$sub_num),.verbose=T,.packages=c("dplyr","optimx","reshape2","KernSmooth")) %dopar% {
        sink(file.path(wd,"parlog.txt"), append=TRUE)
        cat(paste("Fitting subject", j,"\n"))
        iterim <- optimx(par=m$free[c("ER","LR")], fn = m$fcn, method = "Nelder-Mead",
                         control = list(maxit = 1000), lower=m$low, upper=m$up,
                         fixed=c(m$fix,m$free['TR']), data=m$data[m$data$sub_num ==j,], fittingAcc=TRUE)
        newStart <- c(unlist(iterim[,c("ER","LR")]), m$free[!(names(m$free) %in% c("ER","LR"))])
        optimx(par=newStart, fn = m$fcn, method = "Nelder-Mead", control = list(maxit = 1000),
               lower=m$low, upper=m$up, 
               fixed=m$fix, data=m$data[m$data$sub_num ==j,], fittingRT=TRUE)
      }
    
    stopCluster(cl)
    cat(paste('[',Sys.time(),']',"Fitting Completed, Goodbye"), 
        file=file.path(wd,"parlog.txt"),sep='\n',append = TRUE)    
    
    for (i in 1:length(results)){
      m <- model[i]
      models[[m]]$results <- results[[i]]
    }
    # End parallel method 
    
  } else {
    k=1
    for (m in models[model]) {
      m$results = vector(mode="list",length=length(unique(m$sub_num)))
      for (j in unique(m$data$sub_num)) {
        message(paste("Fitting subject", j))
        iterim <- optimx(par=m$free[c("ER","LR")], fn = m$fcn, method = "Nelder-Mead",
                         control = list(maxit = 1000), lower=m$low, upper=m$up,
                         fixed=c(m$fix,m$free['TR']), data=m$data[m$data$sub_num ==j,], fittingAcc=TRUE)
        newStart <- c(unlist(iterim[,c("ER","LR")]), m$free[!(names(m$free) %in% c("ER","LR"))])
        fit <- optimx(par=newStart, fn = m$fcn, method = "Nelder-Mead", control = list(maxit = 1000),
                    lower=m$low, upper=m$up,
                    fixed=m$fix, data=m$data[m$data$sub_num ==j,], fittingRT=TRUE)
        m$results[[k]] <- fit
        k=k+1
      }
    }
  }

  k=1
  for (m in models[model]) {
    save(m,file=paste(names(models[model[k]]),"_results.Rdata",sep=''))
    k=k+1
  }
  if (debugLevel[1]>0){
    untrace(PCL)
  }
  return(models[model])
}
