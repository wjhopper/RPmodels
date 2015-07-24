local({r <- getOption("repos")
      r["CRAN"] <- "http://cran.revolutionanalytics.com"
      options(repos=r)})

options(stringsAsFactors=FALSE)
options(max.print=100)
options(scipen=10)
options(prompt="> ")
options(continue="... +  ")
options(width = 80)
options(contrasts = c("contr.helmert", "contr.poly"))
q <- function (save="no", ...) {
  quit(save=save, ...)
}


.env <- new.env()

.env$unfactor <- function(df){
  id <- sapply(df, is.factor)
  df[id] <- lapply(df[id], as.character)
  df
}

.env$refactor <- function(df){
  id <- sapply(df, is.factor)
  df[id] <- lapply(df[id], as.character)
  df
} 

.env$is.installed <- function(mypkg) {is.element(mypkg, installed.packages()[,1]) }

.env$strSort <- function(x,splitter) {
  sapply(lapply(strsplit(x,splitter,fixed = TRUE), sort), paste, collapse=".")
}

.env$getMyPath <- function() {
#initial.options <- commandArgs(trailingOnly = FALSE)
#file.arg.name <- "--file="
#script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
#script.basename <- dirname(script.name)
#return(script.basename)

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
PATH <- dirname(frame_files[[length(frame_files)]])
return(PATH)


#whereFrom=sys.calls()[[1]]
#print(whereFrom)
## This should be an expression that looks something like
## source("pathname/myfilename.R")
#whereFrom=as.character(whereFrom[2]) # get the pathname/filename
#print(whereFrom)
#whereFrom=paste(getwd(),whereFrom,sep="/") # prefix it with the current working directory
#print(whereFrom)
#pathnameIndex=gregexpr(".*/",whereFrom) # we want the string up to the final '/'
#print(pathnameIndex)
#pathnameLength=attr(pathnameIndex[[1]],"match.length")
#print(pathnameLength)
#whereFrom=substr(whereFrom,1,pathnameLength-1)
#print(whereFrom) # or "setwd(whereFrom)" to set the working directory
}
library(ggplot2, quietly=TRUE)
library(grid, quietly=TRUE)
.env$mytheme <- theme_grey() + theme(axis.title.x=element_text(size=rel(2),vjust=-.65),
                                     axis.title.y=element_text(size=rel(2),vjust=1.5),
                                     legend.title = element_text(size=rel(1.6)),
                                     legend.text = element_text(size=rel(1.35)),
                                     axis.text.x = element_text(size=rel(2)),
                                     axis.text.y = element_text(size=rel(2)),
                                     axis.ticks.x = element_blank(),
                                     strip.text = element_text(size=rel(1.25)),
                                     plot.title = element_text(size=20))                       

.env$pres_theme <- theme_grey() + theme(axis.title.x=element_text(size=rel(3.5),vjust=-.15),
                                     axis.title.y=element_text(size=rel(3.5),vjust=1.25),
                                     legend.title = element_text(size=rel(3.5)),
                                     legend.text = element_text(size=rel(3)),
                                     axis.text.x = element_text(size=rel(3.5)),
                                     axis.text.y = element_text(size=rel(3.5)),
                                     axis.ticks.x = element_blank(),
                                     plot.title = element_text(size=36),
                                     strip.text = element_text(size=rel(3.5)),                                     
                                     legend.key.height=unit(5,"line"),
                                     legend.key.width=unit(5,"line")
                                     )
attach(.env)

message("\n*** Successfully loaded .Rprofile ***\n")
