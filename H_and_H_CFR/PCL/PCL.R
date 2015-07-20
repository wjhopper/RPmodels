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
  return(list(Acc=recalled,RTrounded=RT,CRT=CRT))
}

RTdis <- function(RT,Time) {
  
  RTdis=matrix(0,nrow=10*Time,ncol=ncol(RT))  #for each rank ordering recall position (Nlist), the KS density at each 1/10 second of recall test period (TestTime)
  nas <- is.na(RT)
  for (n in 1:ncol(RT)) {
    RTs<- RT[!nas[,n],n]
    D <- density(RTs,bw=1,n=900,from=.1,to=90)
    height <- D$y
    height <- height/sum(height)
    RTdis[,n] <- (length(RTs)/nrow(RT))*height
  }
  RTdist <- melt(RTdis/sum(RTdis),varnames=c("RTrounded","order"),value.name = "RTdist")
  RTdist$RTrounded <- RTdist$RTrounded/10
  return(RTdist)
}

LL <- function(obs,pred) {
  sum_frame <- inner_join(obs,pred) %>% group_by(class,order) %>% 
    summarise(likelihood=sum(RTdist))
  sum_frame$likelihood[sum_frame$likelihood == 0] <- (0.5)*.Machine$double.xmin
  err <- -sum(log(sum_frame$likelihood))
#   if (is.infinite(err)) {#((0.5)*.Machine$double.xmax)is.na(err) || is.infinite(err)) {
#     print(err)
#   }
  return(err)
}

PCL <- function(free= c(ER=.53,LR=.1,TR =.05, FR=.05), fixed = c(Tmin=2, Tmax=10, lambda=.2,theta=.5,nFeat=100,nSim=1000,nList=15,Time=90),
                yoke = NULL, data=NULL, fitting=FALSE) {
  
  p <- c(free,fixed)
  
  if (any(p[names(p) %in% c("ER","LR","TR","FR","theta")] > 1) || any(p[names(p) %in% c("ER","LR","TR","FR","lambda","theta")] < 0)) {
    err <- 100000
    return(err)
  }
  set.seed(456)
  mxn <-  p['nSim']*p['nList'] #dimensions precalculation 
  
  #practice test
  init_mem <- matrix(rbinom(mxn,p['nFeat'], p['ER']),nrow=p['nSim'],ncol=p['nList'])
  init_thresh <- matrix(rbinom(mxn,p['nFeat'], p['theta']),nrow=p['nSim'],ncol=p['nList'])
#   init_mem <- as.matrix(read.csv('mem.csv'))
#   init_thresh <- as.matrix(read.csv('theta.csv'))
  prac <- recall(init_mem,init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  prac$dist <- RTdis(prac$RT,p['Time']) 
  
  # study practice
  restudyStrengths <- init_mem + rbinom(mxn,p['nFeat']-init_mem, p['LR'])
  restudyStrengths  <- restudyStrengths - rbinom(mxn,restudyStrengths, p['FR'])
  restudy<-recall(restudyStrengths, init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])  
  restudy$dist <- RTdis(restudy$RT,p['Time']) 
  
  # test practice
  testStrengths <- init_mem #copy strengths and thresholds from practice test 
  testThresh <- init_thresh 
  #imm
  testStrengths[prac$Acc==TRUE] <- init_mem[prac$Acc==TRUE] + rbinom(sum(prac$Acc==TRUE),p['nFeat']-init_mem[prac$Acc==TRUE], p['LR'])
  testThresh[prac$Acc==TRUE] <- init_thresh[prac$Acc==TRUE] - rbinom(sum(prac$Acc==TRUE),init_thresh[prac$Acc==TRUE], p['TR'])
  testStrengths <- testStrengths -rbinom(mxn,testStrengths, p['FR'])
  test <- recall(testStrengths, testThresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  test$dist <- RTdis(test$RT,p['Time']) 
  
  dist <- data.frame(class = rep(c('np','sp','tp'),each=nrow(prac$dist)),
                      rbind(prac$dist, restudy$dist, test$dist))
  err <- LL(obs=data[,c("class","order","RTrounded")],pred = dist)
  if (fitting) {
    return(err)
  } else {
    acc <- melt(rbind(prac$Acc,restudy$Acc,test$Acc),
                varnames=c("class","order"),value.name = "pred_acc")
    RT <- melt(do.call(rbind,list(np=prac$RT, sp = restudy$RT, tp = test$RT)),
               varnames=c("class","order"),value.name = "pred_RT")
    CRT <- melt(do.call(rbind,list(np=prac$CRT, sp = restudy$CRT, tp = test$CRT)),
                varnames=c("class","order"),value.name = "pred_CRT")
    preds <- data.frame(subject = data$sub_num[1],Reduce(inner_join,list(acc,RT,CRT)))
    preds$class <- rep(rep(c("np","sp","tp"),each = nrow(prac$Acc)),ncol(prac$Acc))
    return(list(err=err,preds=preds,dist = dist))
  }
}