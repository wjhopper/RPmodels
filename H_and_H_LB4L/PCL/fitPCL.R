fitPCL <- function(model=1,...,debugLevel = 0,) {
  library(optimx)
  library(Matrix)
  is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1]) 
  if (is.installed('parallel')) {
    library(parallel)
    cl <- makeCluster(detectCores()-1)
  } else {
    cl <- NULL
  }
  ## Set wd, depending on platform
  if (any(c(is.null(sys.call(-1)), if (!is.null(sys.call(-1))){!agrepl(as.character((sys.call(-1))),"plotPCL")}else{TRUE}))) {
    if(.Platform$OS.type == "unix") {
      root <- Sys.getenv("HOME")
      wd <- file.path("/opt","source","RPmodels","H_and_H_LB4L","PCL")
    }   else if (.Platform$OS.type == "windows") {
      root <- Sys.getenv("USERPROFILE")
      wd <- file.path(Sys.getenv("USERPROFILE"),"source","RPmodels","H_and_H_LB4L","PCL")
    }
    setwd(wd)
  }
  source('PCL.R')
  data <- cbind(read.csv(file.path('..','group_means.csv')),pred_acc = NA,pred_acc_plus= NA,pred_acc_neg= NA,subject=9999)
  SS_data <- cbind(read.csv(file.path('..','cond_means_by_ss.csv')),pred_acc = NA,pred_acc_plus= NA,pred_acc_neg= NA)
  
  models <- list('std' = list(fcn = PCL, free= c(ER=.58,LR=.07,TR =.1, F1=.1,F2=.1),
                                   fix= c(Tmin= .25, Tmax=20, lambda=.5,theta=.5,nFeat=100,nSim=1000,nList=15,Time=10),data=data,
                                   low = -Inf, up = Inf, results = vector(mode="list",length=length(unique(data$subject)))), 
                 'std_ss' = list(fcn = PCLss, free= c(ER=.58,LR=.07,TR =.1, F1=.1),
                                   fix= c(Tmin= .25, Tmax=20, lambda=.5,theta=.5,nFeat=100,nSim=1000,nList=15,Time=10),data=SS_data,
                                   low = -Inf, up = Inf, results = vector(mode="list",length=length(unique(SS_data$subject)))),
                 'minFree_ss' = list(fcn = PCLss, free= c(ER=.58,LR=.07,TR =.1, F1=.1,Tmin= .25),
                                 fix= c(Tmax=20, lambda=.5,theta=.5,nFeat=100,nSim=1000,nList=15,Time=10),data=SS_data,
                                 low = -Inf, up = Inf, results = vector(mode="list",length=length(unique(SS_data$subject)))))
  
  for (i in model) {
    reqParams <- c(names(formals(models[[i]]$fcn)$free), names(formals(models[[i]]$fcn)$fix))   
    reqParams <- reqParams[reqParams != ""]
    givenParams <- names(c(models[[i]]$free,models[[i]]$fix))
    if (!all( reqParams[reqParams != ""]  %in% givenParams)) {
      stop(paste(reqParams[!reqParams %in% givenParams], " not specified in model,check model input list"))
    }    
    if (debugLevel[1] == 2) {
      setBreakpoint('PCL.R', line=debugLevel[2])
    }
    k=1
    for (j in unique(models[[i]]$data$subject)){
      if (debugLevel[1] == 0 || debugLevel[1]==2 ) {
        message(paste("Fitting Subject", j, ": model", names(models[i])))
        a <- optimx(par=models[[i]]$free, fn = models[[i]]$fcn, method = "Nelder-Mead",lower=models[[i]]$low, upper=models[[i]]$up,
                    fixed=models[[i]]$fix, data=models[[i]]$data[models[[i]]$data$subject ==j,], fitting=TRUE, cluster=cl)
        models[[i]]$results[[k]] <- a
      } else if (debugLevel[1] == 1) {
      res <- models[[i]]$fcn(free=models[[i]]$free,fixed=models[[i]]$fix,data=models[[i]]$data[models[[i]]$data$subject ==j,], fitting=FALSE, cluster=cl)
      models[[i]]$results[[k]] <- res
      }
      k=k+1
    }
    tosave <- models[[i]]
    save(tosave,file=paste(names(models[i]),"_results.Rdata",sep=''))
    
  }
  if (exists('cl')) {
    stopCluster(cl)
  }
  return(models[model])
  
}