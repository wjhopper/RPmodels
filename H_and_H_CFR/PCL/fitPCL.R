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
  
  SS_data <- cbind(read.csv(file.path('..','CFRss.csv')),pred_acc = NA,pred_acc_plus= NA,pred_acc_neg= NA)
  SS_data <- filter(SS_data,!is.na(order))
  SS_data$RTrounded <- round(SS_data$RT,1)
  
  models <- list('ss_std' = list(fcn = PCL, free= c(ER=.53,LR=.1,TR =.05, FR=.05),
                                 fix= c(theta=.5,nFeat=100,nSim=1000,nList=15,lambda=.2,Tmin=2,Tmax=10,Time=90), 
                                 yoke = NULL,data=SS_data,
                                 low = -Inf, up = Inf, results = vector(mode="list",length=length(unique(SS_data$sub_num)))),
                 'ss_yoke1' = list(fcn = PCL, free= c(ER=.53,LR=.1,TR =.05, FR=.05),
                                 fix= c(theta=.5,nFeat=100,nSim=1000,nList=15,lambda=.2,Tmin=2,Tmax=10,Time=90),
                                 yoke = NULL,data=SS_data,
                                 low = -Inf, up = Inf, results = vector(mode="list",length=length(unique(SS_data$sub_num)))),
                 'ss_yoke2' = list(fcn = PCL, free= c(ER=.53,LR=.1,TR =.05, FR=.05),
                                 fix= c(theta=.5,nFeat=100,nSim=1000,nList=15,lambda=.2,Tmin=2,Tmax=10,Time=90),
                                 yoke = NULL,data=SS_data,
                                 low = -Inf, up = Inf, results = vector(mode="list",length=length(unique(SS_data$sub_num)))),
                 'ss_yoke3' = list(fcn = PCL, free= c(ER=.53,LR=.1,TR =.05, FR=.05),
                                 fix= c(theta=.5,nFeat=100,nSim=1000,nList=15,lambda=.2,Tmin=2,Tmax=10,Time=90),
                                 yoke = NULL,data=SS_data,
                                 low = -Inf, up = Inf, results = vector(mode="list",length=length(unique(SS_data$sub_num)))))
  for (i in model) {
    reqParams <- c(names(formals(models[[i]]$fcn)$free), names(formals(models[[i]]$fcn)$fix))
    reqParams <- reqParams[!reqParams %in%  c("","Tmin","Tmax","lambda")]
    givenParams <- names(c(models[[i]]$free,models[[i]]$fix))
    if (!all( reqParams[reqParams != ""]  %in% givenParams)) {
      stop(paste(reqParams[!reqParams %in% givenParams], " not specified in model,check model input list"))
    }
  }
  
  if (inpar) {
    writeLines(paste('[',Sys.time(),']',"INIT parlog"), con=file.path(wd,"parlog.txt"),sep='\n')
    clusterExport(cl,c("recall","LL","RTdis"))
    results <- foreach(m=models[model],export="wd") %:%
      foreach(j =unique(m$data$sub_num),.verbose=T,.packages=c("dplyr","optimx","reshape2")) %dopar% {
        sink(file.path(wd,"parlog.txt"), append=TRUE)
        cat(paste("Fitting subject", j,"\n"))
        optimx(par=m$free, fn = m$fcn, method = "Nelder-Mead",lower=m$low, upper=m$up,
               fixed=m$fix, data=m$data[m$data$sub_num ==j,], fitting=TRUE)
      }
    for (i in 1:length(results)){
      m <- model[i]
      models[[m]]$results <- results[[i]]
    }
    stopCluster(cl)
    cat(paste('[',Sys.time(),']',"Finished Fitting, Goodbye"))
  } else {
    k=1
    for (i in model) {
      for (j in unique(models[[i]]$data$sub_num)) {
        message(paste("Fitting subject", j))
#         res <- models[[i]]$fcn(free=models[[i]]$free,fixed=models[[i]]$fix,data=models[[i]]$data[models[[i]]$data$sub_num ==j,], fitting=FALSE)
        a <- optimx(par=models[[i]]$free, fn = models[[i]]$fcn, method = "Nelder-Mead",lower=models[[i]]$low, upper=models[[i]]$up,
                    fixed=models[[i]]$fix, data=models[[i]]$data[models[[i]]$data$sub_num ==j,], fitting=TRUE)
        models[[i]]$results[[k]] <- a
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
