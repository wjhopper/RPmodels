fitPCL <- function(model=1,parforce=FALSE,...,debugLevel = 0) {
  library(optimx)
  is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1]) 
  needed <- list("foreach","doParallel")
  packs <- all(sapply(needed,is.installed))
  if (all(packs) && parforce) {
    library(foreach)
    library(doParallel)
    cl <- makeCluster(detectCores()-1)
    registerDoParallel(cl)
#     library(doRNG)
#     registerDoRNG(456)
    inpar = TRUE
    } else if (!all(packs) && parforce) {
    message("Packages foreach and doParallel not found to do parallel processing, falling back")
    inpar = FALSE
    } else {
    inpar = FALSE
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
  if (debugLevel[1] == 2) {
    setBreakpoint('PCL.R', line=debugLevel[2])
  }
  source('PCL.R')
  data <- cbind(read.csv(file.path('..','group_means.csv')),pred_acc = NA,pred_acc_plus= NA,pred_acc_neg= NA,subject=9999)
  SS_data <- cbind(read.csv(file.path('..','cond_means_by_ss.csv')),pred_acc = NA,pred_acc_plus= NA,pred_acc_neg= NA)
  
  models <- list('std' = list(fcn = PCL, free= c(ER=.58,LR=.07,TR =.4, F1=.1,F2=.1),
                                   fix= c(theta=.5,nFeat=100,nSim=1000,nList=15,Time=10),data=data,
                                   low = -Inf, up = Inf, results = vector(mode="list",length=length(unique(data$subject)))), 
                 'std_ss' = list(fcn = PCLss, free= c(ER=.53,LR=.07,TR =.4, F1=.1),
                                   fix= c(theta=.5,nFeat=100,nSim=1000,nList=15,Time=10),data=SS_data,
                                   low = -Inf, up = Inf, results = vector(mode="list",length=length(unique(SS_data$subject)))))
  for (i in model) {
    reqParams <- c(names(formals(models[[i]]$fcn)$free), names(formals(models[[i]]$fcn)$fix))
    reqParams <- reqParams[!reqParams %in%  c("","Tmin","Tmax","lambda")]
    givenParams <- names(c(models[[i]]$free,models[[i]]$fix))
    if (!all( reqParams[reqParams != ""]  %in% givenParams)) {
      stop(paste(reqParams[!reqParams %in% givenParams], " not specified in model,check model input list"))
    }
  }
    

  k=1
  if (debugLevel[1] == 0 || debugLevel[1]==2 ) {
    if (inpar) {
      clusterExport(cl,c("recallNoTime","recallTime","LL","g2"))
      results <- foreach(m=models[model]) %:%
        foreach(j =unique(m$data$subject),.verbose=T,.packages=c("optimx")) %dopar% {
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
            message(paste("Fitting subject", j))
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
        res <- models[[i]]$fcn(free=models[[i]]$free,fixed=models[[i]]$fix,data=models[[i]]$data[models[[i]]$data$subject ==j,], fitting=FALSE)
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
  if (inpar) {
    stopCluster(cl)
  }
  return(models[model])
}
