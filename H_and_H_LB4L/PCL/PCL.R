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

PCL <- function(free= c(ER=.58,LR=.07,TR =.1, F1=.1,F2=.1), fixed = c(Tmin=1, Tmax=10, lambda=.5,theta=.5,nFeat=100,nSim=1000,nList=15,Time=10),
                   data=NULL, fitting=FALSE) {
  
  p <- c(free,fixed)

  if (any(p[names(p) %in% c("ER","LR","TR","F1","F2","theta")] > 1) || any(names(p) %in% c("ER","LR","TR","F1","F2","lambda","theta") < 0)) {
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
  prac_C1 <- recall(init_mem_C1,init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  prac_C2 <- recall(init_mem_C2,init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  
  #control no practice
  #imm
  controlImmStrengths <- init_mem_C1 - matrix(rbinom(mxn, init_mem_C1, p['F1']),nrow=p['nSim'],ncol=p['nList'])
  controlImmAcc <- recall(controlImmStrengths, init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])  
  #del
  controlDelStrengths <- controlImmStrengths - matrix(rbinom(mxn, controlImmStrengths, p['F2']),nrow=p['nSim'],ncol=p['nList'])
  controlDelAcc <- recall(controlDelStrengths,init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  
  # study practice
  #imm
  restudyStrengths <- init_mem_C1 + matrix(rbinom(mxn,p['nFeat']-init_mem_C1, p['LR']),nrow=p['nSim'],ncol=p['nList'])
  restudyImmStrengths  <- restudyStrengths - matrix(rbinom(mxn,restudyStrengths, p['LR']),nrow=p['nSim'],ncol=p['nList'])
  restudyImmAcc<-recall(restudyImmStrengths, init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])  
  #del
  restudyDelStrengths <- restudyImmStrengths - matrix(rbinom(mxn,restudyImmStrengths, p['F2']),nrow=p['nSim'],ncol=p['nList'])
  restudyDelAcc <- recall(restudyDelStrengths, init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  
  # test practice
  #copy strengths and thresholds from practice test 
  testStrengths <- init_mem_C1 
  testThresh <- init_thresh
  #imm
  testStrengths[prac_C1==TRUE] <- init_mem_C1[prac_C1==TRUE] + matrix(rbinom(mxn,p['nFeat']-init_mem_C1, p['LR']),nrow=p['nSim'],ncol=p['nList'])[prac_C1==TRUE]
  testThresh[prac_C1==TRUE] <- init_thresh[prac_C1==TRUE] - matrix(rbinom(mxn,init_thresh, p['TR']),nrow=p['nSim'],ncol=p['nList'])[prac_C1==TRUE]
  testImmStrengths <- testStrengths - matrix(rbinom(p['nSim']*p['nList'],testStrengths, p['F1']),nrow=p['nSim'],ncol=p['nList'])
  testImmAcc <- recall(testImmStrengths, testThresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])  
  testImmAccPlus <- testImmAcc[prac_C1==TRUE]
  testImmAccNeg <- testImmAcc[prac_C1==FALSE]
  #del
  testDelStrengths <- testStrengths - matrix(rbinom(mxn,testStrengths, p['F2']),nrow=p['nSim'],ncol=p['nList'])
  testDelAcc <- recall(testDelStrengths, testThresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  testDelAccPlus <- testDelAcc[prac_C1==TRUE]
  testDelAccNeg <- testDelAcc[prac_C1==FALSE]
  
  #no practice, other cue test practice
  #imm
  testOCImmStrengths <- init_mem_C2 - matrix(rbinom(mxn, init_mem_C2, p['F1']),nrow=p['nSim'],ncol=p['nList'])
  testOCImmThresh <- testThresh
  testOCImmAcc <- recall(testOCImmStrengths, testOCImmThresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])  
  testOCImmAccPlus <- testOCImmAcc[prac_C1==TRUE]
  testOCImmAccNeg <- testOCImmAcc[prac_C1==FALSE]
  #del
  testOCDelStrengths <-controlDelStrengths
  testOCDelAcc <- recall(testOCDelStrengths, testOCImmThresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  testOCDelAccPlus <- testOCDelAcc[prac_C1==TRUE]
  testOCDelAccNeg <- testOCDelAcc[prac_C1==FALSE]

  avgsImm <- lapply(list(prac_C1=prac_C1, chain1 = testOCImmAcc, chain1plus =  testOCImmAccPlus, chain1neg = testOCImmAccNeg,
                         chain2 = controlImmAcc, chain3= restudyImmAcc,
                         chain5 = testImmAcc, chain5plus= testImmAccPlus,chain5neg = testImmAccNeg), mean)
  avgsDel<- lapply(list(chain1 = testOCDelAcc, chain1plus =  testOCImmAccPlus, chain1neg = testOCImmAccNeg,
                        chain2 = controlDelAcc, chain3= restudyDelAcc,
                        chain5 = testDelAcc, chain5plus= testDelAccPlus,chain5neg = testDelAccNeg ), mean)
  data$pred_acc[!is.na(data$acc) & data$timepoint ==1] <- avgsImm$prac_C1
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

PCLss <- function(free= c(ER=.58,LR=.07,TR =.1, F1=.1), fixed = c(Tmin=1, Tmax=10, lambda=.5,theta=.5,nFeat=100,nSim=1000,nList=15,Time=10),
                          data=NULL, fitting=FALSE) {
  
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
  init_mem_C1 <- matrix(rbinom(mxn,p['nFeat'], p['ER']),nrow=p['nSim'],ncol=p['nList'])
  init_mem_C2 <- matrix(rbinom(mxn,p['nFeat'], p['ER']),nrow=p['nSim'],ncol=p['nList'])
  init_thresh <- matrix(rbinom(mxn,p['nFeat'], p['theta']),nrow=p['nSim'],ncol=p['nList'])
  prac_C1 <- recall(init_mem_C1,init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  prac_C2 <- recall(init_mem_C2,init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  
  #control no practice
  controlImmStrengths <- init_mem_C1 - rbinom(mxn, init_mem_C1, p['F1'])
  controlImmAcc <- recall(controlImmStrengths, init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])  

  # study practice
  #imm
  restudyStrengths <- init_mem_C1 + rbinom(mxn,p['nFeat']-init_mem_C1, p['LR'])
  restudyImmStrengths  <- restudyStrengths - rbinom(mxn,restudyStrengths, p['LR'])
  restudyImmAcc<-recall(restudyImmStrengths, init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])  
  
  # test practice
  testStrengths <- init_mem_C1 #copy strengths and thresholds from practice test 
  testThresh <- init_thresh 
  testImmAccPlus <- Matrix(0,nrow=p['nSim'],ncol=p['nList'], sparse = TRUE) # careful, is of class Matrix, not matrix
  testImmAccNeg <- testImmAccPlus
  #imm
  testStrengths[prac_C1==TRUE] <- init_mem_C1[prac_C1==TRUE] + rbinom(sum(prac_C1==TRUE),p['nFeat']-init_mem_C1[prac_C1==TRUE], p['LR'])
  testThresh[prac_C1==TRUE] <- init_thresh[prac_C1==TRUE] - rbinom(sum(prac_C1==TRUE),init_thresh[prac_C1==TRUE], p['TR'])
  testImmStrengths <- testStrengths -rbinom(mxn,testStrengths, p['F1'])
  testImmAcc <- recall(testImmStrengths, testThresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])  
  testImmAccPlus[prac_C1==TRUE] <- testImmAcc[prac_C1==TRUE]
  testImmAccNeg[prac_C1==FALSE] <- testImmAcc[prac_C1==FALSE]
  
  #no practice, other cue test practice
  testOCImmStrengths <- init_mem_C2 - rbinom(mxn, init_mem_C2, p['F1'])
  testOCImmAccPlus <- Matrix(0,nrow=p['nSim'],ncol=p['nList'], sparse = TRUE) # careful, is of class Matrix, not matrix
  testOCImmAccNeg <-testOCImmAccPlus
  #imm
  testOCImmAcc<- recall(testOCImmStrengths, testThresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])  
  testOCImmAccPlus[prac_C2==TRUE] <- testOCImmAcc[prac_C2==TRUE] 
  testOCImmAccNeg[prac_C2==FALSE] <- testOCImmAcc[prac_C2==FALSE]
  
  #compute predictions
  avgs <- lapply(list(prac_C1=prac_C1, chain1plus = testOCImmAccPlus,chain1neg = testOCImmAccNeg, chain2 = controlImmAcc, chain3= restudyImmAcc,
                      chain5plus = testImmAccPlus,chain5neg = testImmAccNeg), mean)
  avgs$chain1 <- (avgs$chain1plus*avgs$prac_C1)+(avgs$chain1neg*(1-avgs$prac_C1))
  avgs$chain5 <- (avgs$chain5plus*avgs$prac_C1)+(avgs$chain5neg*(1-avgs$prac_C1))
  # Fill in data frame with preds
  data$pred_acc[!is.na(data$acc) & data$timepoint==1]  <- avgs$prac_C1
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