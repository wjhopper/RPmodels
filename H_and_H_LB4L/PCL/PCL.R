recallTime <- function(mem=NULL,thresh=NULL,Tmin=NULL,Tmax=NULL,Time=NULL,lambda=NULL) {
  RT=Tmin + (Tmax-Tmin)*exp(-lambda*abs(mem-thresh))  
  recalled <- (RT < Time) & (mem > thresh)
  return(recalled)
}

recallNoTime <- function(mem=NULL,thresh=NULL,...) {
  recalled <- mem > thresh
  return(recalled)
} 

g2 <- function(obs,pred,N) {
  pred[pred==0] <- 0.001 # & !is.nan(N[pred==0])]# 1/(2*N[pred==0])
  pred[pred==1] <- 0.999
  Lc <- obs*(log(pred)) + ((1-obs)*log(1-pred))
  Lu <- obs*(log(obs)) + ((1-obs)*log(1-obs))
  err <- -sum(2*N*(Lc-Lu))
  #   print(pred)
  #   print(err)
  return(err)
}

LL <- function(obs,pred,N) {
  obs = obs * N
  pred[pred==0] <- 0.001 # & !is.nan(N[pred==0])]# 1/(2*N[pred==0])
  pred[pred==1] <- 0.999
  ll = (obs*log(pred))+((N-obs)*log(1-pred))
  err=-sum(ll[!is.nan(ll)])        
  return(err)
}

PCL <- function(free= c(ER=.58,LR=.07,TR =.4, F1=.1,F2=.1), fixed = c(Tmin=1, Tmax=10, lambda=.5,theta=.5,nFeat=100,nSim=1000,nList=15,Time=10),
                   data=NULL, fitting=FALSE) {
  
  p <- c(free,fixed)

  if (any(p[names(p) %in% c("ER","LR","TR","F1","F2","theta")] > 1) || any(p[names(p) %in% c("ER","LR","TR","F1","F2","lambda","theta")] < 0)) {
    err <- 100000
    return(err)
  }
  set.seed(456)
  if (any(is.na(c(p["lambda"],p["Tmin"],p["Tmax"],p['Time'])))) {
    recall <- recallNoTime
  } else{
    recall <- recallTime
  }
  mxn <-  p['nSim']*p['nList'] #dimensions precalculation 
  
  #practice test
  init_mem_C1 <- matrix(rbinom(mxn,p['nFeat'], p['ER']),nrow=p['nSim'],ncol=p['nList'])
  init_mem_C2 <- matrix(rbinom(mxn,p['nFeat'], p['ER']),nrow=p['nSim'],ncol=p['nList'])
  init_thresh <- matrix(rbinom(mxn,p['nFeat'], p['theta']),nrow=p['nSim'],ncol=p['nList'])
  prac <- recall(init_mem_C1,init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  
  
  #control no practice
  #imm
  controlImmStrengths <- init_mem_C1 - rbinom(mxn, init_mem_C1, p['F1'])
  controlImmAcc <- recall(controlImmStrengths, init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  #del
  controlDelStrengths <- init_mem_C1 - rbinom(mxn, init_mem_C1, p['F2'])
  controlDelAcc <- recall(controlDelStrengths,init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  
  # study practice
  #imm
  restudyStrengths <- init_mem_C1 + rbinom(mxn,p['nFeat']-init_mem_C1, p['LR'])
  restudyImmStrengths  <- restudyStrengths - rbinom(mxn,restudyStrengths, p['F1'])
  restudyImmAcc<-recall(restudyImmStrengths, init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])  
  #del
  restudyDelStrengths <- restudyStrengths - rbinom(mxn,restudyStrengths, p['F2'])
  restudyDelAcc <- recall(restudyDelStrengths, init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  
  # test practice
  testStrengths <- init_mem_C1 #copy strengths and thresholds from practice test 
  testThresh <- init_thresh 
  #imm
  testStrengths[prac==TRUE] <- init_mem_C1[prac==TRUE] + rbinom(sum(prac==TRUE),p['nFeat']-init_mem_C1[prac==TRUE], p['LR'])
  testThresh[prac==TRUE] <- init_thresh[prac==TRUE] - rbinom(sum(prac==TRUE),init_thresh[prac==TRUE], p['TR'])
  testImmStrengths <- testStrengths -rbinom(mxn,testStrengths, p['F1'])
  testImmAcc <- recall(testImmStrengths, testThresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  #del
  testDelStrengths <- testStrengths - rbinom(mxn,testStrengths, p['F2'])
  testDelAcc <- recall(testDelStrengths, testThresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  
  
  #no practice, other cue test practice
  #imm
  testOCImmStrengths <- init_mem_C2 - rbinom(mxn, init_mem_C2, p['F1'])
  testOCImmAcc<- recall(testOCImmStrengths, testThresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  #del
  testOCDelStrengths <- init_mem_C2 - rbinom(mxn, init_mem_C2, p['F2'])
  testOCDelAcc<- recall(testOCImmStrengths, testThresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])

  
  avgsImm <- lapply(list(prac=prac, chain1 = testOCImmAcc, chain1plus =  testOCImmAcc[prac==TRUE], chain1neg = testOCImmAcc[prac==FALSE],
                         chain2 = controlImmAcc, chain3= restudyImmAcc,
                         chain5 = testImmAcc, chain5plus= testImmAcc[prac==TRUE],chain5neg = testImmAcc[prac==FALSE]),
                    mean)
  avgsDel<- lapply(list(chain1 = testOCDelAcc, chain1plus =  testOCDelAcc[prac==TRUE], chain1neg = testOCDelAcc[prac==FALSE],
                        chain2 = controlDelAcc, chain3= restudyDelAcc,
                        chain5 = testDelAcc, chain5plus= testDelAcc[prac==TRUE],chain5neg = testDelAcc[prac==FALSE]),
                   mean)
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

PCLss <- function(free= c(ER=.58,LR=.07,TR =.4, F1=.1), fixed = c(Tmin=1, Tmax=10, lambda=.5,theta=.5,nFeat=100,nSim=1000,nList=15,Time=10),
                          data=NULL, fitting=FALSE) {
  
  p <- c(free,fixed)

  if (any(p[names(p) %in% c("ER","LR","TR","F1","theta")] > 1) || any(p[names(p) %in% c("ER","LR","TR","F1","lambda","theta")] < 0)) {
    err <- 100000
    return(err)
  } 
  set.seed(456)
  if (any(is.na(c(p["lambda"],p["Tmin"],p["Tmax"],p['Time'])))) {
    recall <- recallNoTime
  } else{
    recall <- recallTime
  }
  
  mxn <-  p['nSim']*p['nList'] #dimensions precalculation 
  
  #practice test
  init_mem_C1 <- matrix(rbinom(mxn,p['nFeat'], p['ER']),nrow=p['nSim'],ncol=p['nList'])
  init_mem_C2 <- matrix(rbinom(mxn,p['nFeat'], p['ER']),nrow=p['nSim'],ncol=p['nList'])
  init_thresh <- matrix(rbinom(mxn,p['nFeat'], p['theta']),nrow=p['nSim'],ncol=p['nList'])
  prac <- recall(init_mem_C1,init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  
  
  #control no practice
  controlImmStrengths <- init_mem_C1 - rbinom(mxn, init_mem_C1, p['F1'])
  controlImmAcc <- recall(controlImmStrengths, init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])  

  # study practice
  #imm
  restudyStrengths <- init_mem_C1 + rbinom(mxn,p['nFeat']-init_mem_C1, p['LR'])
  restudyImmStrengths  <- restudyStrengths - rbinom(mxn,restudyStrengths, p['F1'])
  restudyImmAcc<-recall(restudyImmStrengths, init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])  
  
  # test practice
  testStrengths <- init_mem_C1 #copy strengths and thresholds from practice test 
  testThresh <- init_thresh 
  #imm
  testStrengths[prac==TRUE] <- init_mem_C1[prac==TRUE] + rbinom(sum(prac==TRUE),p['nFeat']-init_mem_C1[prac==TRUE], p['LR'])
  testThresh[prac==TRUE] <- init_thresh[prac==TRUE] - rbinom(sum(prac==TRUE),init_thresh[prac==TRUE], p['TR'])
  testImmStrengths <- testStrengths -rbinom(mxn,testStrengths, p['F1'])
  testImmAcc <- recall(testImmStrengths, testThresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])  
  
  #no practice, other cue test practice
  #imm
  testOCImmStrengths <- init_mem_C2 - rbinom(mxn, init_mem_C2, p['F1'])
  testOCImmAcc<- recall(testOCImmStrengths, testThresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])  
  #compute predictions
  avgs <- lapply(list(prac=prac,
                      chain1 = testOCImmAcc, chain1plus = testOCImmAcc[prac==TRUE],chain1neg = testOCImmAcc[prac==FALSE], 
                      chain2 = controlImmAcc, chain3= restudyImmAcc,
                      chain5 = testImmAcc, chain5plus = testImmAcc[prac==TRUE],chain5neg = testImmAcc[prac==FALSE]),
                 mean)
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
           data$acc_neg[!is.na(data$acc_neg)])
  err <- LL(obs=obs,pred=preds,N=c(rep(p['nList'], length(preds) - 4), data$nplus[!(is.na(data$nplus))], data$nneg[!(is.na(data$nneg))]))
  if (fitting) {
    return(err)
  } else {
    return(data)
  }  
}