recall <- function(mem=NULL,thresh=NULL,Tmin=NULL,Tmax=NULL,Time=NULL,lambda=NULL) {

  totalRT <- matrix(0,nrow=nrow(mem),ncol=ncol(mem))
  a <- apply(mem,1,sort,decreasing = TRUE,index.return=TRUE)
  inds <- do.call("rbind", lapply(a, "[[", 2))
  RT=Tmin + (Tmax-Tmin)*exp(-lambda*abs(mem-thresh))  
  for (i in 1:nrow(mem)) {
    totalRT[i,inds[i,]] <- cumsum(RT[i,inds[i,]])
  }
  recalled <- (totalRT < Time) & (mem > thresh)
  return(recalled)
}

g2 <- function(obs,pred,N) {
  Lc <- obs*(log(pred)) + ((1-obs)*log(1-pred))
  Lu <- obs*(log(obs)) + ((1-obs)*log(1-obs))
  err <- -sum(2*N*(Lc-Lu))
#   print(pred)
#   print(err)
  return(err)
}

fitPCL <- function(free= c(ER=.58,LR=.07,TR =.1, F1=.1,F2=.1, Tmin=15), fixed = c(Tmax=60, lambda=.5,theta=.5,nFeat=100,nSim=1000,nList=30,Time=420),
                   data=NULL, fitting=FALSE) {
  p <- c(free,fixed)
#   print(free)
  pnames <- c("ER","LR","TR","F1","F2","Tmin","Tmax","lambda","theta","nFeat","nSim","nList", "Time")
  if (!all(pnames %in% names(p))) {
     stop(paste(pnames[!pnames %in% names(p)], " not specified in model,check model input list"))
  }
  
  if (any(p[c("ER","LR","TR","F1","F2","lambda","theta")] > 1) || any(p[c("ER","LR","TR","F1","F2","lambda","theta")] < 0)) {
    err <- 100000
    return(err)
  }
  #practice test
  init_mem <- matrix(rbinom(p['nSim']*p['nList'],p['nFeat'], p['ER']),nrow=p['nSim'],ncol=p['nList'])
  init_thresh <- matrix(rbinom(p['nSim']*p['nList'],p['nFeat'], p['theta']),nrow=p['nSim'],ncol=p['nList'])
  prac <- recall(init_mem,init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  
  #restudy practice
  #immediate
  restudyImmStrengths <- init_mem + matrix(rbinom(p['nSim']*p['nList'],p['nFeat']-init_mem, p['LR']),nrow=p['nSim'],ncol=p['nList'])
  restudyImmAcc<-recall(restudyImmStrengths, init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  #2 days
  restudyTwoStrengths <- restudyImmStrengths - matrix(rbinom(p['nSim']*p['nList'],restudyImmStrengths, p['F1']),nrow=p['nSim'],ncol=p['nList'])
  restudyTwoAcc <- recall(restudyTwoStrengths, init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  #seven days
  restudySevenStrengths <- restudyTwoStrengths - matrix(rbinom(p['nSim']*p['nList'],restudyTwoStrengths, p['F2']),nrow=p['nSim'],ncol=p['nList'])
  restudySevenAcc <- recall(restudySevenStrengths, init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  
  #test practice
  #immediate
  testImmStrengths <- init_mem
  testImmThresh <- init_thresh
  testImmStrengths[prac==TRUE] <- init_mem[prac==TRUE] + matrix(rbinom(p['nSim']*p['nList'],p['nFeat']-init_mem, p['LR']),nrow=p['nSim'],ncol=p['nList'])[prac==TRUE]
  testImmThresh[prac==TRUE] <- init_thresh[prac==TRUE] - matrix(rbinom(p['nSim']*p['nList'],p['nFeat']-init_thresh, p['TR']),nrow=p['nSim'],ncol=p['nList'])[prac==TRUE]
  testImmAcc <- recall(testImmStrengths, testImmThresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  # 2 days 
  testTwoStrengths <- testImmStrengths - matrix(rbinom(p['nSim']*p['nList'],testImmStrengths, p['F1']),nrow=p['nSim'],ncol=p['nList'])
  testTwoAcc <- recall(testTwoStrengths, testImmThresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  # 7 days
  testSevenStrengths <- testTwoStrengths - matrix(rbinom(p['nSim']*p['nList'],testTwoStrengths, p['F2']),nrow=p['nSim'],ncol=p['nList'])
  testSevenAcc <- recall(testSevenStrengths, testImmThresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  
  #baseline
  # 2 days 
  controlTwoStrengths <- init_mem - matrix(rbinom(p['nSim']*p['nList'], init_mem, p['F1']),nrow=p['nSim'],ncol=p['nList'])
  controlTwoAcc <- recall(controlTwoStrengths, init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  # 7 Days 
  controlSevenStrengths <- controlTwoStrengths - matrix(rbinom(p['nSim']*p['nList'], controlTwoStrengths, p['F1']),nrow=p['nSim'],ncol=p['nList'])
  controlSevenAcc <- recall(controlSevenStrengths,init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  
  data$predAcc<-NA
  data$predAcc[data$chain==1 & data$timepoint > 1] = c(mean(restudyImmAcc), mean(restudyTwoAcc), mean(restudySevenAcc))
  data$predAcc[data$chain==2 ] = c(mean(prac), mean(testImmAcc), mean(testTwoAcc), mean(testSevenAcc))
  
  err <- g2(obs=data$acc[!is.na(data$acc)],pred=data$predAcc[!is.na(data$predAcc)],N=120)
  if (fitting) {
    return(err)
  } else {
    return(list(data = data, err=err))
  }
  
}



