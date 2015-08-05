
## @knitr envSetup 
library(gridExtra)
library(xtable)
library(ggplot2)
library(dplyr)

opts_chunk$set(echo = FALSE,fig.width=16,fig.height=7,cache=FALSE, 
               warning=F, message=FALSE, fig.align='center')
if(.Platform$OS.type == "unix") {
 root <- Sys.getenv("HOME")
 wd <- file.path("/opt","source","RPmodels","H_and_H_CFR","PCL")
}   else if (.Platform$OS.type == "windows") {
 root <- Sys.getenv("USERPROFILE")
 wd <- file.path(Sys.getenv("USERPROFILE"),"source","RPmodels","H_and_H_CFR","PCL")
}
setwd(wd)


## @knitr modelInfo
modelInfo <- function(m) {
print(xtable(t(as.matrix(m$free)), digits=3,caption = paste("Starting Free Parameters")),
      type = "html", include.rownames=FALSE, caption.placement="top")
print(xtable(t(as.matrix(m$fix)), digits=3,caption = paste("Fixed Parameters")),
      type = "html", include.rownames=FALSE, caption.placement="top")
}

## @knitr subjectResults
subjectResults <- function(m) {
  sList <- unique(m$data$sub_num)
  for (i in 1:(length(m$plots)-1))  {
    cat('<h4 class="subid"> Subject',sList[i],'Results </h4>')
    grid.arrange(m$plots[[i]]$Acc, m$plots[[i]]$RT,ncol=1,nrow=2)
    print(xtable(data.frame(Subject = sList[i],m$results[[i]]),
                 digits=3, caption = paste("Best Parameters")),
        type = "html", include.rownames=FALSE, caption.placement="top")
    print(m$plots[[i]]$density)
  }	       
}

## @knitr subjectAverages 
subjectAverages <- function(m) {
  grid.arrange(m$plots[[length(m$plots)]]$aggAcc,m$plots[[length(m$plots)]]$aggRT,ncol=1,nrow=2)
  avg_res <- do.call(rbind,m$results) %>% summarise_each(funs(mean))
  print(xtable(as.matrix(avg_res[1:which(names(avg_res)=='value')]), digits=3,
                 caption = paste("Average Parameters")),
        type = "html", include.rownames=FALSE, caption.placement="top")
  print(m$plots[[length(m$plots)]]$aggDensity)
  
}