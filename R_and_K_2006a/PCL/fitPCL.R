fitPCL <- function(model=1, ...) {
  
  ## @knitr env
  library(optimx)
  is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])   
  if (is.installed('parallel')) {
    library(parallel)
    cl <- makeCluster(detectCores()-1)
  }
  ## Set wd, depending on platform
  if (!is.null(sys.call(-1)) && as.character((sys.call(-1)))!="plotPCL"){
    if(.Platform$OS.type == "unix") {
      root <- Sys.getenv("HOME")
      wd <- file.path("opt","source","RPmodels","R_and_K_2006a","PCL")
    }   else if (.Platform$OS.type == "windows") {
      root <- Sys.getenv("USERPROFILE")
      wd <- file.path(Sys.getenv("USERPROFILE"),"source","RPmodels","R_and_K_2006a","PCL")
    }
    setwd(wd)
    source('PCL.R')    
  }
  data <- read.csv(file.path('..','group_means.csv'))
  
  models <- list('standard' = list(fcn = PCL, free =c(ER= 0.5873,LR= 0.0840,TR = 0.0843, F1=  0.1286, F2=0.0608, Tmin= 15.5763),
                                   fix= c(Tmax=60, lambda=.5,theta=.5,nFeat=100,nSim=1000,nList=30,Time=420),data=data,
                                   low = -Inf, up = Inf))
  for (i in model) {
    reqParams <- c(names(formals(models[[i]]$fcn)$free), names(formals(models[[i]]$fcn)$fix))   
    reqParams <- reqParams[reqParams != ""]
    givenParams <- names(c(models[[i]]$free,models[[i]]$fix))
    if (!all( reqParams[reqParams != ""]  %in% givenParams)) {
      stop(paste(reqParams[!reqParams %in% givenParams], " not specified in model,check model input list"))
    }
    a <- optimx(par=models[[i]]$free, fn = models[[i]]$fcn, method = "Nelder-Mead",lower=models[[i]]$low, upper=models[[i]]$up,
                fixed=models[[i]]$fix, data=models[[i]]$data, fitting=TRUE,parallel = cl)
    models[[i]]$result <- a
  }
  
  return(models[[models]])

}
