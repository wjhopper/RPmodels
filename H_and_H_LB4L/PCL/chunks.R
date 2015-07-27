
## @knitr envSetup 
library(gridExtra)
library(xtable)

strSort <- function (x, splitter) {
  sapply(lapply(strsplit(x, splitter, fixed = TRUE), sort), 
         paste, collapse = ".")
}

opts_chunk$set(echo = FALSE,fig.width=16,fig.height=7,cache=FALSE, 
               warning=F, message=FALSE, fig.align='center')
if(.Platform$OS.type == "unix") {
  root <- Sys.getenv("HOME")
  wd <- file.path("/opt","source","RPmodels","H_and_H_LB4L","PCL")
  source(file.path("/opt","source","RPmodels",".Rprofile"))
}   else if (.Platform$OS.type == "windows") {
  root <- Sys.getenv("USERPROFILE")
  wd <- file.path(Sys.getenv("USERPROFILE"),"source","RPmodels","H_and_H_LB4L","PCL")
  source(file.path(Sys.getenv("USERPROFILE"),"source","RPmodels", ".Rprofile"))
  
}
setwd(wd)
source('plotPCL.R')

## @knitr modelInfo
modelInfo <- function(m) {
print(xtable(t(as.matrix(m$free)), digits=3,caption = paste("Starting Free Parameters")),
      type = "html", include.rownames=FALSE, caption.placement="top")
print(xtable(t(as.matrix(m$fix)), digits=3,caption = paste("Fixed Parameters")),
      type = "html", include.rownames=FALSE, caption.placement="top")
}

## @knitr subjectResults
subjectResults <- function(m) {
  sList <- unique(m$data$subject)
  for (i in 1:(length(m$plots)-1))  {
    cat('<h4 class="subid"> Subject',sList[i],'Results </h4>')
    grid.arrange(m$plots[[i]]$acc, m$plots[[i]]$cond_acc,ncol=2,nrow=1)
    print(xtable(data.frame(Subject = sList[i],Group = m$data$group[m$data$subject==sList[i]][1],m$results[[i]]),
                 digits=3, caption = paste("Best Parameters")),
          type = "html", include.rownames=FALSE, caption.placement="top")
  }
}

## @knitr subjectAverages 
subjectAverages <- function(m) {
  grid.arrange(m$plots[[length(m$plots)]]$accAgg,m$plots[[length(m$plots)]]$condAccAgg,ncol=2,nrow=1)
  r <- m$results[[length(m$results)]]
  print(xtable(as.matrix(r[1:which(names(r)=='value')]), digits=3,
               caption = paste("Average Parameters")),
        type = "html", include.rownames=FALSE, caption.placement="top")
}