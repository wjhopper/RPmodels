recallTime <- function(mem=NULL,thresh=NULL,Tmin=NULL,Tmax=NULL,Time=NULL,lambda=NULL) {
  RT=Tmin + (Tmax-Tmin)*exp(-lambda*abs(mem-thresh))  
  recalled <- (RT < Time) & (mem > thresh)
  return(recalled)
}

recallNoTime <- function(mem=NULL,thresh=NULL) {
  recalled <- mem > thresh
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

LL <- function(obs,pred,N) {
  obs = obs * N
  ll = (obs*log(pred))+((N-obs)*log(1-pred))
  err=-sum(ll[!is.nan(ll)]);          
  return(err)
}

PCL <- function(free= c(ER=.58,LR=.07,TR =.1, F1=.1,F2=.1), fixed = c(Tmin=1, Tmax=10, lambda=.5,theta=.5,nFeat=100,nSim=1000,nList=15,Time=NULL),
                   data=NULL, fitting=FALSE, cluster = NULL) {
  
  p <- c(free,fixed)

  if (any(p[names(p) %in% c("ER","LR","TR","F1","F2","theta")] > 1) || any(names(p) %in% c("ER","LR","TR","F1","F2","lambda","theta") < 0)) {
    err <- 100000
    return(err)
  } 
  if (any(is.na(c(p["lambda"],p["Tmin"],p["Tmax"],p['Time'])))) {
    recall <- recallNoTime
  } else{
    recall <- recallTime
  }
  mxn <-  p['nSim']*p['nList'] #dimensions precalculation 
  
  #practice test
  init_mem <- matrix(rbinom(mxn,p['nFeat'], p['ER']),nrow=p['nSim'],ncol=p['nList'])
  init_thresh <- matrix(rbinom(mxn,p['nFeat'], p['theta']),nrow=p['nSim'],ncol=p['nList'])
  prac <- recall(init_mem,init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'], parallel=cluster)
  
  #control no practice
  #imm
  controlImmStrengths <- init_mem - matrix(rbinom(mxn, init_mem, p['F1']),nrow=p['nSim'],ncol=p['nList'])
  controlImmAcc <- recall(controlImmStrengths, init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'], parallel=cluster)  
  #del
  controlDelStrengths <- controlImmStrengths - matrix(rbinom(mxn, controlImmStrengths, p['F2']),nrow=p['nSim'],ncol=p['nList'])
  controlDelAcc <- recall(controlDelStrengths,init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'], parallel=cluster)
  
  # study practice
  #imm
  restudyStrengths <- init_mem + matrix(rbinom(mxn,p['nFeat']-init_mem, p['LR']),nrow=p['nSim'],ncol=p['nList'])
  restudyImmStrengths  <- restudyStrengths - matrix(rbinom(mxn,restudyStrengths, p['LR']),nrow=p['nSim'],ncol=p['nList'])
  restudyImmAcc<-recall(restudyImmStrengths, init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'], parallel=cluster)  
  #del
  restudyDelStrengths <- restudyImmStrengths - matrix(rbinom(mxn,restudyImmStrengths, p['F2']),nrow=p['nSim'],ncol=p['nList'])
  restudyDelAcc <- recall(restudyDelStrengths, init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'], parallel=cluster)
  
  # test practice
  #copy strengths and thresholds from practice test 
  testStrengths <- init_mem 
  testThresh <- init_thresh
  #imm
  testStrengths[prac==TRUE] <- init_mem[prac==TRUE] + matrix(rbinom(mxn,p['nFeat']-init_mem, p['LR']),nrow=p['nSim'],ncol=p['nList'])[prac==TRUE]
  testThresh[prac==TRUE] <- init_thresh[prac==TRUE] - matrix(rbinom(mxn,p['nFeat']-init_thresh, p['TR']),nrow=p['nSim'],ncol=p['nList'])[prac==TRUE]
  testImmStrengths <- testStrengths - matrix(rbinom(p['nSim']*p['nList'],testStrengths, p['F1']),nrow=p['nSim'],ncol=p['nList'])
  testImmAcc <- recall(testImmStrengths, testThresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'], parallel=cluster)  
  testImmAccPlus <- testImmAcc[prac==TRUE]
  testImmAccNeg <- testImmAcc[prac==FALSE]
  #del
  testDelStrengths <- testStrengths - matrix(rbinom(mxn,testStrengths, p['F2']),nrow=p['nSim'],ncol=p['nList'])
  testDelAcc <- recall(testDelStrengths, testThresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'], parallel=cluster)
  testDelAccPlus <- testDelAcc[prac==TRUE]
  testDelAccNeg <- testDelAcc[prac==FALSE]
  
  #no practice, other cue test practice
  #imm
  testOCImmStrengths <- controlImmStrengths
  testOCImmThresh <- testThresh
  testOCImmAcc <- recall(testOCImmStrengths, testOCImmThresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'], parallel=cluster)  
  testOCImmAccPlus <- testOCImmAcc[prac==TRUE]
  testOCImmAccNeg <- testOCImmAcc[prac==FALSE]
  #del
  testOCDelStrengths <-controlDelStrengths
  testOCDelAcc <- recall(testOCDelStrengths, testOCImmThresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'], parallel=cluster)
  testOCDelAccPlus <- testOCDelAcc[prac==TRUE]
  testOCDelAccNeg <- testOCDelAcc[prac==FALSE]

  avgsImm <- lapply(list(prac=prac, chain1 = testOCImmAcc, chain1plus =  testOCImmAccPlus, chain1neg = testOCImmAccNeg,
                         chain2 = controlImmAcc, chain3= restudyImmAcc,
                         chain5 = testImmAcc, chain5plus= testImmAccPlus,chain5neg = testImmAccNeg), mean)
  avgsDel<- lapply(list(chain1 = testOCDelAcc, chain1plus =  testOCImmAccPlus, chain1neg = testOCImmAccNeg,
                        chain2 = controlDelAcc, chain3= restudyDelAcc,
                        chain5 = testDelAcc, chain5plus= testDelAccPlus,chain5neg = testDelAccNeg ), mean)
  data$pred_acc[!is.na(data$acc) & data$timepoint ==1] <- avgsImm$prac
  data[data$chain==1 & data$timepoint >1,c("pred_acc", "pred_acc_plus","pred_acc_neg")] <- c(avgsImm$chain1, avgsImm$chain1plus,avgsImm$chain1neg, 
                                                                                             avgsDel$chain1,avgsDel$chain1plus,avgsDel$chain1plus)
  data$pred_acc[data$chain==2 & data$timepoint >1] <- c(avgsImm$chain2, avgsDel$chain2)
  data$pred_acc[data$chain==3 & data$timepoint >1] <- c(avgsImm$chain3, avgsDel$chain4)
  data[data$chain==5 & data$timepoint >1,c("pred_acc", "pred_acc_plus","pred_acc_neg")] <-c(avgsImm$chain5, avgsImm$chain5plus,avgsImm$chain5neg,
                                                                                            avgsDel$chain5,avgsDel$chain5plus,avgsDel$chain5plus)
  err <- g2(obs=data$acc[!is.na(data$acc)],pred=data$pred_acc[!is.na(data$acc)],N=15*length(data$acc[!is.na(data$acc)]))
  if (fitting) {
    return(err)
  } else {
    return(data)
  }
}

PCLss <- function(free= c(ER=.58,LR=.07,TR =.1, F1=.1), fixed = c(Tmin=1, Tmax=10, lambda=.5,theta=.5,nFeat=100,nSim=1000,nList=NULL,Time=NULL),
                          data=NULL, fitting=FALSE,cluster= NULL) {
  
  p <- c(free,fixed)

  if (any(p[names(p) %in% c("ER","LR","TR","F1","theta")] > 1) || any(names(p) %in% c("ER","LR","TR","F1","lambda","theta") < 0)) {
    err <- 100000
    return(err)
  } 
  
  if (any(is.na(c(p["lambda"],p["Tmin"],p["Tmax"],p['Time'])))) {
    recall <- recallNoTime
  } else{
    recall <- recallTime
  }
  
  mxn <-  p['nSim']*p['nList'] #dimensions precalculation 
  
  #practice test
  init_mem <- matrix(rbinom(mxn,p['nFeat'], p['ER']),nrow=p['nSim'],ncol=p['nList'])
  init_thresh <- matrix(rbinom(mxn,p['nFeat'], p['theta']),nrow=p['nSim'],ncol=p['nList'])
  prac <- recall(init_mem,init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'], parallel=cluster)
  
  #control no practice
  controlImmStrengths <- init_mem - matrix(rbinom(mxn, init_mem, p['F1']),nrow=p['nSim'],ncol=p['nList'])
  controlImmAcc <- recall(controlImmStrengths, init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'], parallel=cluster)  

  # study practice
  #imm
  restudyStrengths <- init_mem + matrix(rbinom(mxn,p['nFeat']-init_mem, p['LR']),nrow=p['nSim'],ncol=p['nList'])
  restudyImmStrengths  <- restudyStrengths - matrix(rbinom(mxn,restudyStrengths, p['LR']),nrow=p['nSim'],ncol=p['nList'])
  restudyImmAcc<-recall(restudyImmStrengths, init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'], parallel=cluster)  
  
  # test practice
  #copy strengths and thresholds from practice test 
  testStrengths <- init_mem 
  testThresh <- init_thresh
  testImmAccPlus <- Matrix(0,nrow=p['nSim'],ncol=p['nList'], sparse = TRUE) # careful, is of class Matrix, not matrix
  testImmAccNeg <- Matrix(0,nrow=p['nSim'],ncol=p['nList'], sparse = TRUE)
  #imm
  testStrengths[prac==TRUE] <- init_mem[prac==TRUE] + matrix(rbinom(mxn,p['nFeat']-init_mem, p['LR']),nrow=p['nSim'],ncol=p['nList'])[prac==TRUE]
  testThresh[prac==TRUE] <- init_thresh[prac==TRUE] - matrix(rbinom(mxn,p['nFeat']-init_thresh, p['TR']),nrow=p['nSim'],ncol=p['nList'])[prac==TRUE]
  testImmStrengths <- testStrengths - matrix(rbinom(p['nSim']*p['nList'],testStrengths, p['F1']),nrow=p['nSim'],ncol=p['nList'])
  testImmAcc <- recall(testImmStrengths, testThresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'], parallel=cluster)  
  testImmAccPlus[prac==TRUE] <- testImmAcc[prac==TRUE]
  testImmAccNeg[prac==FALSE] <- testImmAcc[prac==FALSE]
  
  #no practice, other cue test practice
  #imm
  testOCImmStrengths <- controlImmStrengths
  testOCImmThresh <- testThresh
  testOCImmAccPlus <- Matrix(0,nrow=p['nSim'],ncol=p['nList'], sparse = TRUE) # careful, is of class Matrix, not matrix
  testOCImmAccNeg <- Matrix(0,nrow=p['nSim'],ncol=p['nList'], sparse = TRUE)
  testOCImmAcc<- recall(testOCImmStrengths, testOCImmThresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'], parallel=cluster)  
  testOCImmAccPlus[prac==TRUE] <- testOCImmAcc[prac==TRUE] 
  testOCImmAccNeg[prac==FALSE] <- testOCImmAcc[prac==FALSE]
  
  #compute predictions
  avgs <- lapply(list(prac=prac, chain1plus = testOCImmAccPlus,chain1neg = testOCImmAccNeg, chain2 = controlImmAcc, chain3= restudyImmAcc,
                      chain5plus = testImmAccPlus,chain5neg = testImmAccNeg), mean)
  avgs$chain1 <- (avgs$chain1plus*avgs$prac)+(avgs$chain1neg*(1-avgs$prac))
  avgs$chain5 <- (avgs$chain5plus*avgs$prac)+(avgs$chain5neg*(1-avgs$prac))
  # Fill in data frame with preds
  data$pred_acc[!is.na(data$acc) & data$timepoint==1]  <- avgs$prac
  data[data$chain==1 & data$timepoint!=1, c("pred_acc", "pred_acc_plus","pred_acc_neg")] <- avgs[c("chain1", "chain1plus","chain1neg")]
  data$pred_acc[data$chain==2 & data$timepoint!=1] <- avgs$chain2
  data$pred_acc[data$chain==3 & data$timepoint!=1] <- avgs$chain3
  data[data$chain==5  & data$timepoint!=1, c("pred_acc", "pred_acc_plus","pred_acc_neg")] <- avgs[c("chain5", "chain5plus","chain5neg")]
  preds <- c(data$pred_acc[!is.na(data$pred_acc) & data$timepoint ==1],
             data$pred_acc[!is.na(data$pred_acc) & !data$chain %in% c(1,5) & data$timepoint !=1], 
             data$pred_acc_plus[!is.na(data$pred_acc_plus)], 
             data$pred_acc_neg[!is.na(data$pred_acc_neg)])
  obs <- c(data$acc[!is.na(data$acc) & data$timepoint ==1],
           data$acc[!is.na(data$acc) & !data$chain %in% c(1,5) & data$timepoint !=1], 
           data$acc_plus[!is.na(data$acc_plus)], 
           data$pred_acc_neg[!is.na(data$acc_neg)])
  err <- LL(obs=obs,pred=preds,N=c(rep(p['nList'], length(preds) - 4), data$nplus[!(is.na(data$nplus))], data$nneg[!(is.na(data$nneg))]))
  if (fitting) {
    return(err)
  } else {
    return(data)
  }  
}