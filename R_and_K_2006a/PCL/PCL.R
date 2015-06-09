PCL <- function(model=1, ...) {
  
  ## @knitr env
  library(optimx)
  
  models <- list('standard' = list(fcn = fitPCL, free =c(ER= 0.5873,LR= 0.0840,TR = 0.0843, F1=  0.1286, F2=0.0608, Tmin= 15.5763),
                                   fix= c(Tmax=60, lambda=.5,theta=.5,nFeat=100,nSim=1000,nList=30,Time=420),data=data,
                                   low = -Inf, up = Inf))

  sys.calls()[[sys.nframe()-1]]
  ## Set wd, depending on platform
  if (as.character((sys.call(-1)))!="plotPCL"){
    if(.Platform$OS.type == "unix") {
      root <- Sys.getenv("HOME")
      wd <- file.path("opt","source","RPmodels","R_and_K_2006a","PCL")
    }   else if (.Platform$OS.type == "windows") {
      root <- Sys.getenv("USERPROFILE")
      wd <- file.path(Sys.getenv("USERPROFILE"),"source","RPmodels","R_and_K_2006a","PCL")
    }
    setwd(wd)
  }
  source('fitPCL.R')
  data <- read.csv(file.path('..','group_means.csv'))

  for (i in model) {
    a <- optimx(par=models[[i]]$free, fn = models[[i]]$fcn, method = "Nelder-Mead",lower=models[[i]]$low, upper=models[[i]]$up,
                fixed=models[[i]]$fix, data=models[[i]]$data, fitting=TRUE)
    models[[i]]$result <- a
  }
  
  return(models[[models]])

}
