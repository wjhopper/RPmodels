plotPCL <- function(model=1, ...) {
  library(reshape2)
  library(ggplot2)
  
  if(.Platform$OS.type == "unix") {
    root <- Sys.getenv("HOME")
    wd <- file.path("opt","source","RPmodels","R_and_K_2006a","PCL")
  }   else if (.Platform$OS.type == "windows") {
    root <- Sys.getenv("USERPROFILE")
    wd <- file.path(Sys.getenv("USERPROFILE"),"source","RPmodels","R_and_K_2006a","PCL")
  }
  setwd(wd)
  source('PCL.R')
  if (!file.exists('models.Rdata')) {
#     source('fitPCL.R')
    models <- fitPCL(model)
  } else {
    load('models.Rdata')
  }
  data <- PCL(free=optimx:::coef.optimx(models[[model]]$results)[1,], fixed=models[[model]]$fix, data=models[[model]]$data)$data
  data <- melt(data,id.vars = c("group","chain","timepoint","n"),variable.name = 'type',value.name="pc")
  p1 <- ggplot(data = data, mapping = aes(x=timepoint, y=pc, colour=group,shape=type)) +
    geom_point(color = "black",size=5) +
    geom_point(size=3.5) + 
    scale_color_discrete(name = "Conditions",
                         labels = c("Test Practice", "Study Practice")) +
    scale_shape_manual(name="", labels= c('Obs. Data','Model Fits'), values = c(19,17)) + 
    scale_x_discrete("Retention Interval", labels = c("Practice Test", "Immediate Final Test", "2 Day Delay","7 Day Delay")) + ylab("Memory Accuracy") + 
    ggtitle('PCL vs. Roediger & Karpicke 2006b Data') +
    mytheme
  return(list(plot=p1,models=models))
}