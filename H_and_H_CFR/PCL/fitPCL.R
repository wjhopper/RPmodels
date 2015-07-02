fitPCL <- function(model=1,...,debugLevel = 0) {
  library(optimx)
  library(Matrix)
  is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1]) 
  needed <- list("foreach","doParallel","doRNG")
  if (all(sapply(needed,is.installed))) {
    library(foreach)
    library(doParallel)
    cl <- makeCluster(detectCores()-1)
    registerDoParallel(cl)
    inpar = TRUE
  } else {
    inpar = FALSE
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
  SS_data <- cbind(read.csv(file.path('..','CFRss.csv')),pred_acc = NA,pred_acc_plus= NA,pred_acc_neg= NA)
  
  models <- list('ss_std' = list(fcn = PCL, free= c(ER=.53,LR=.1,TR =.05, FR=.05),
                                 fix= c(theta=.5,nFeat=100,nSim=1000,nList=15,Tmin=2,Tmax=10,Time=90), 
                                 yoke = NULL,data=SS_data,
                                 low = -Inf, up = Inf, results = vector(mode="list",length=length(unique(SS_data$subject)))),
                 'ss_yoke1' = list(fcn = PCL, free= c(ER=.53,LR=.1,TR =.05, FR=.05),
                                 fix= c(theta=.5,nFeat=100,nSim=1000,nList=15,Tmin=2,Tmax=10,Time=90),
                                 yoke = NULL,data=SS_data,
                                 low = -Inf, up = Inf, results = vector(mode="list",length=length(unique(SS_data$subject)))),
                 'ss_yoke2' = list(fcn = PCL, free= c(ER=.53,LR=.1,TR =.05, FR=.05),
                                 fix= c(theta=.5,nFeat=100,nSim=1000,nList=15,Tmin=2,Tmax=10,Time=90),
                                 yoke = NULL,data=SS_data,
                                 low = -Inf, up = Inf, results = vector(mode="list",length=length(unique(SS_data$subject)))),
                 'ss_yoke3' = list(fcn = PCL, free= c(ER=.53,LR=.1,TR =.05, FR=.05),
                                 fix= c(theta=.5,nFeat=100,nSim=1000,nList=15,Tmin=2,Tmax=10,Time=90),
                                 yoke = NULL,data=SS_data,
                                 low = -Inf, up = Inf, results = vector(mode="list",length=length(unique(SS_data$subject)))))
  for (i in model) {
    reqParams <- c(names(formals(models[[i]]$fcn)$free), names(formals(models[[i]]$fcn)$fix))
    reqParams <- reqParams[!reqParams %in%  c("","Tmin","Tmax","lambda")]
    givenParams <- names(c(models[[i]]$free,models[[i]]$fix))
    if (!all( reqParams[reqParams != ""]  %in% givenParams)) {
      stop(paste(reqParams[!reqParams %in% givenParams], " not specified in model,check model input list"))
    }
  }
  
  if (debugLevel[1] == 2) {
    setBreakpoint('PCL.R', line=debugLevel[2])
  }
  k=1
  if (debugLevel[1] == 0 || debugLevel[1]==2 ) {
    if (inpar) {
      clusterExport(cl,c("recallNoTime","recallTime","LL","g2"))
      #       .export=ls(envir=globalenv())
      results <- foreach(m=models[model]) %:%
        foreach(j =unique(m$data$subject),.verbose=T,.packages=c("optimx","Matrix")) %dopar% {
          #           m$fcn(m$free,fixed=m$fix,data=m$data[m$data$subject ==j,], fitting=TRUE)
          optimx(par=m$free, fn = m$fcn, method = "Nelder-Mead",lower=m$low, upper=m$up,
                 fixed=m$fix, data=m$data[m$data$subject ==j,], fitting=TRUE)
        }
      for (i in 1:length(results)){
        m <- model[i]
        models[[m]]$results <- results[[i]]
      }
    } else {
      for (i in model) {
        for (j in unique(models[[i]]$data$subject)) {
          a <- optimx(par=models[[i]]$free, fn = models[[i]]$fcn, method = "Nelder-Mead",lower=models[[i]]$low, upper=models[[i]]$up,
                      fixed=models[[i]]$fix, data=models[[i]]$data[models[[i]]$data$subject ==j,], fitting=TRUE)
          models[[i]]$results[[k]] <- a
          k=k+1
        }
      }
    }
  } else if (debugLevel[1] == 1) {
    for (i in model) {
      for (j in unique(models[[i]]$data$subject)) {
        message(paste("Fitting Subject", j, ": model", names(models[i])))
        res <- models[[i]]$fcn(free=models[[i]]$free,fixed=models[[i]]$fix,data=models[[i]]$data[models[[i]]$data$subject ==j,], fitting=FALSE, cluster=cl)
        models[[i]]$results[[k]] <- res
        k=k+1
      }
    }
  }
  k=1
  for (m in models[model]) {
    save(m,file=paste(names(models[model[k]]),"_results.Rdata",sep=''))
    k=k+1
  }
  if (exists('cl')) {
    stopCluster(cl)
  }
  return(models[model])
}