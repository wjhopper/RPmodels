recall <- function(mem,thresh,Tmin=NULL,Tmax=NULL,Time=NULL,lambda=NULL) {

    for (i in 1:nrow(mem)) {
    a <- sort.int(mem[i,],decreasing = TRUE,index.return=TRUE)
    mem[i,] <- a[[1]]
    thresh[i,] <- thresh[i,a[[2]]]
    }
  RT<- Tmin + (Tmax-Tmin)*exp(-lambda*abs(mem-thresh))
  CRT <- t(apply(RT,1,cumsum))
  # Time <- 90 
  recalled <- (CRT < Time) & (mem >= thresh) # only above threshold and shorter than alloted time are recoverable
  return(list(Acc=recalled,RT=RT,CRT=CRT))
}

RTdis <- function(RT,Time) {
  
  RTdis=matrix(0,nrow=10*Time,ncol=ncol(RT));  #for each rank ordering recall position (Nlist), the KS density at each 1/10 second of recall test period (TestTime)
  nas <- is.na(RT)
    for (n in 1:p['nList']) {
      RTs<- RT[!nas[,i],i]
      D <- density(RTs,bw=1,n=900,from=.1,to=90)$y
      D <- D/sum(D)
      RTdis[,i] <- (length(RTs)/nrow(RT))*D
    }
  RTdis <- RTdis/sum(RTdis)
}


LL <- function(obs,pred,N) {

  return(err)
}

PCL <- function(free= c(ER=.53,LR=.1,TR =.05, FR=.05), fixed = c(Tmin=2, Tmax=10, lambda=.2,theta=.5,nFeat=100,nSim=1000,nList=15,Time=NULL),
                yoke = NULL, data=NULL, fitting=FALSE) {
  
  p <- c(free,fixed)
  
  if (any(p[names(p) %in% c("ER","LR","TR","F1","F2","theta")] > 1) || any(names(p) %in% c("ER","LR","TR","F1","F2","lambda","theta") < 0)) {
    err <- 100000
    return(err)
  }
  set.seed(456)
  mxn <-  p['nSim']*p['nList'] #dimensions precalculation 
  
  #practice test
#   init_mem <- matrix(rbinom(mxn,p['nFeat'], p['ER']),nrow=p['nSim'],ncol=p['nList'])
#   init_thresh <- matrix(rbinom(mxn,p['nFeat'], p['theta']),nrow=p['nSim'],ncol=p['nList'])
  init_mem <- as.matrix(read.csv('mem.csv'))
  init_thresh <- as.matrix(read.csv('theta.csv'))
  prac <- recall(init_mem,init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  
  # study practice
  restudyStrengths <- init_mem + matrix(rbinom(mxn,p['nFeat']-init_mem, p['LR']),nrow=p['nSim'],ncol=p['nList'])
  restudyStrengths  <- restudyStrengths - matrix(rbinom(mxn,restudyStrengths, p['LR']),nrow=p['nSim'],ncol=p['nList'])
  restudyAcc<-recall(restudyStrengths, init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])  

  # test practice
  #copy strengths and thresholds from practice test 
  testStrengths <- init_mem 
  testThresh <- init_thresh
  testStrengths[prac==TRUE] <- init_mem[prac==TRUE] + matrix(rbinom(mxn,p['nFeat']-init_mem, p['LR']),nrow=p['nSim'],ncol=p['nList'])[prac==TRUE]
  testThresh[prac==TRUE] <- init_thresh[prac==TRUE] - matrix(rbinom(mxn,init_thresh, p['TR']),nrow=p['nSim'],ncol=p['nList'])[prac==TRUE]
  testStrengths <- testStrengths - matrix(rbinom(p['nSim']*p['nList'],testStrengths, p['F1']),nrow=p['nSim'],ncol=p['nList'])
  testAcc <- recall(testStrengths, testThresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])  
  testAccPlus <- testAcc[prac==TRUE]
  testAccNeg <- testAcc[prac==FALSE]
}