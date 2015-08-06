recallTime <- function(mem=NULL,thresh=NULL,space=NULL,Tmin=NULL,Tmax=NULL,Time=NULL,lambda=NULL) {
  RT=Tmin + (Tmax-Tmin)*exp(-lambda*abs(mem-thresh))  
  recalled <- (RT < Time) & (mem > thresh)
  spaced <- as.logical(rbinom(recalled,1,space))
  recalled[recalled & spaced] <- FALSE
  return(recalled)
}

recallNoTime <- function(mem=NULL,thresh=NULL,space=NULL,...) {
  recalled <- mem > thresh
  spaced <- as.logical(rbinom(recalled,1,space))
  recalled[recalled & spaced] <- FALSE
  return(recalled)
} 

binomialg2 <- function(obs,pred,N) {
  Lc <- obs*(log(pred)) + ((1-obs)*log(1-pred))
  Lu <- obs*(log(obs)) + ((1-obs)*log(1-obs))
  err <- -sum(2*N*(Lc-Lu))
  return(err)
}

multinomialg2 <- function(obs,pred,N) {
  Lc <- obs*(log(pred)) 
  Lu <- obs*(log(obs)) 
  err <- -sum(2*N*(Lc-Lu))
  return(err)
}

binomialLL <- function(obs,pred,N) {
  obs <- obs * N
  ll <-  obs[obs!=0]*log(pred[obs!=0]) + ((N-obs)[obs!=0])*log(1-pred[obs!=0])
  err <- -sum(ll)
  return(err)
}

multinomialLL <- function(obs,pred,N){
  obs = obs * N
  ll <- obs[obs!=0]*log(pred[obs!=0])
  err <- -sum(ll)
  return(err)
}
PCL <- function(free= c(ER=.58,LR=.07,TR =.4, F1=.1,F2=.1,space=.03),
                fixed = c(theta=.5, nFeat=100, nSim=1000, nList=15, Tmin=NA, Tmax=NA, lambda=NA, Time=NA),
                data=cbind(read.csv(file.path('..','allData_by_groups.csv')),
                                pred_prac_acc = NA, pred_final_acc = NA,
                                pred_acc_plus= NA,pred_acc_neg= NA,
                                pred_prac_and_final=NA,pred_prac_and_not_final=NA,
                                pred_not_prac_and_final=NA,pred_not_prac_and_not_final=NA),
                fitting=FALSE) {
  
  p <- c(free,fixed)

  if (any(p[names(p) %in% c("ER","LR","TR","F1","F2","theta")] > 1, na.rm=T) || 
      any(p[names(p) %in% c("ER","LR","TR","F1","F2","lambda","theta")] < 0, na.rm=T)) {
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
  prac <- recall(init_mem_C1,init_thresh, p['space'], p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  
  
  #control no practice
  #imm
  controlImmStrengths <- init_mem_C1 - rbinom(mxn, init_mem_C1, p['F1'])
  controlImmAcc <- recall(controlImmStrengths, init_thresh, p['space'], p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  #del
  controlDelStrengths <- init_mem_C1 - rbinom(mxn, init_mem_C1, p['F2'])
  controlDelAcc <- recall(controlDelStrengths,init_thresh, p['space'], p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  
  # study practice
  #imm
  restudyStrengths <- init_mem_C1 + rbinom(mxn,p['nFeat']-init_mem_C1, p['LR'])
  restudyImmStrengths  <- restudyStrengths - rbinom(mxn,restudyStrengths, p['F1'])
  restudyImmAcc<-recall(restudyImmStrengths, init_thresh, p['space'], p['Tmin'], p['Tmax'], p['Time'],p['lambda'])  
  #del
  restudyDelStrengths <- restudyStrengths - rbinom(mxn,restudyStrengths, p['F2'])
  restudyDelAcc <- recall(restudyDelStrengths, init_thresh, p['space'], p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  
  # test practice
  testStrengths <- init_mem_C1 #copy strengths and thresholds from practice test 
  testThresh <- init_thresh 
  #imm
  testStrengths[prac] <- init_mem_C1[prac] + rbinom(sum(prac),p['nFeat']-init_mem_C1[prac], p['LR'])
  testThresh[prac] <- init_thresh[prac] - rbinom(sum(prac),init_thresh[prac], p['TR'])
  testImmStrengths <- testStrengths -rbinom(mxn,testStrengths, p['F1'])
  testImmAcc <- recall(testImmStrengths, testThresh, p['space'], p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  #del
  testDelStrengths <- testStrengths - rbinom(mxn,testStrengths, p['F2'])
  testDelAcc <- recall(testDelStrengths, testThresh, p['space'], p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  
  
  #no practice, other cue test practice
  #imm
  testOCImmStrengths <- init_mem_C2 - rbinom(mxn, init_mem_C2, p['F1'])
  testOCImmAcc<- recall(testOCImmStrengths, testThresh, p['space'], p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  #del
  testOCDelStrengths <- init_mem_C2 - rbinom(mxn, init_mem_C2, p['F2'])
  testOCDelAcc<- recall(testOCDelStrengths, testThresh, p['space'], p['Tmin'], p['Tmax'], p['Time'],p['lambda'])

  
  avgsImm <- lapply(list(prac=prac, chain1 = testOCImmAcc, chain1plus =  testOCImmAcc[prac], chain1neg = testOCImmAcc[!prac],
                         chain1_prac_final = (prac & testOCImmAcc),
                         chain1_prac_not_final = (prac & !testOCImmAcc),
                         chain1_not_prac_final = (!prac & testOCImmAcc),
                         chain1_not_prac_not_final = (!prac & !testOCImmAcc),
                         chain2 = controlImmAcc, chain3= restudyImmAcc,
                         chain5 = testImmAcc, chain5plus= testImmAcc[prac],chain5neg = testImmAcc[!prac],
                         chain5_prac_final = (prac & testImmAcc),
                         chain5_prac_not_final = (prac & !testImmAcc),
                         chain5_not_prac_final = (!prac & testImmAcc),
                         chain5_not_prac_not_final = (!prac & !testImmAcc)
                         ),
                    mean)
  avgsDel<- lapply(list(prac=prac,chain1 = testOCDelAcc, chain1plus =  testOCDelAcc[prac], chain1neg = testOCDelAcc[!prac],
                        chain1_prac_final = (prac & testOCDelAcc),
                        chain1_prac_not_final = (prac & !testOCDelAcc),
                        chain1_not_prac_final = (!prac & testOCDelAcc),
                        chain1_not_prac_not_final = (!prac & !testOCDelAcc),
                        chain2 = controlDelAcc, chain3= restudyDelAcc,
                        chain5 = testDelAcc, chain5plus= testDelAcc[prac],chain5neg = testDelAcc[!prac],
                        chain5_prac_final = (prac & testDelAcc),
                        chain5_prac_not_final = (prac & !testDelAcc),
                        chain5_not_prac_final = (!prac & testDelAcc),
                        chain5_not_prac_not_final = (!prac & !testDelAcc)),
                   mean)
  insertCols <- c("pred_final_acc", "pred_acc_plus","pred_acc_neg","pred_prac_and_final",
                  "pred_prac_and_not_final","pred_not_prac_and_final","pred_not_prac_and_not_final")
  
  #Immediate Predictions
  immRows <- data$group=="immediate"  
  data$pred_prac_acc[!is.na(data$prac_acc) & immRows] <- avgsImm$prac
  data[data$chain==1 & immRows, insertCols] <- avgsImm[c("chain1", "chain1plus","chain1neg",
                                                     "chain1_prac_final","chain1_prac_not_final",
                                                     "chain1_not_prac_final","chain1_not_prac_not_final")]
  data$pred_final_acc[data$chain==2 & immRows] <- avgsImm$chain2
  data$pred_final_acc[data$chain==3 & immRows] <- avgsImm$chain3
  data[data$chain==5 & immRows, insertCols] <- avgsImm[c("chain5", "chain5plus","chain5neg",
                                                       "chain5_prac_final","chain5_prac_not_final",
                                                       "chain5_not_prac_final","chain5_not_prac_not_final")]
  #Delay Predictions
  delRows <-data$group=="delay"  
  data$pred_prac_acc[!is.na(data$prac_acc) & delRows] <- avgsDel$prac
  data[data$chain==1 & delRows, insertCols] <- avgsDel[c("chain1", "chain1plus","chain1neg",
                                                          "chain1_prac_final","chain1_prac_not_final",
                                                          "chain1_not_prac_final","chain1_not_prac_not_final")]
  data$pred_final_acc[data$chain==2 & delRows] <- avgsDel$chain2
  data$pred_final_acc[data$chain==3 & delRows] <- avgsDel$chain3
  data[data$chain==5 & delRows, insertCols] <- avgsDel[c("chain5", "chain5plus","chain5neg",
                                                          "chain5_prac_final","chain5_prac_not_final",
                                                          "chain5_not_prac_final","chain5_not_prac_not_final")]
  
  preds <- c(data$pred_final_acc[data$practice %in% c('C','S') & is.na(data$other_type)], # control and study practice
             unlist(t(data[!is.na(data$pred_acc_plus),tail(insertCols,4)]), use.names = FALSE)) # items with test practice 
  obs <- c(data$final_acc[data$practice %in% c('C','S') & is.na(data$other_type)], # control and study practice
           unlist(t(data[!is.na(data$pred_acc_plus),gsub("pred_","",tail(insertCols,4))]), use.names = FALSE))# items with test practice 
  N <- c(data$n[data$practice %in% c('C','S') & is.na(data$other_type)], 
         data$n[!is.na(data$pred_acc_plus)])
  err <- binomialLL(obs=obs[1:4],pred=preds[1:4],N=15) + 
    multinomialLL(obs=obs[5:20],pred=preds[5:20],N=15)
  
    if (fitting) {
    return(err)
  } else {
    return(data)
  }
}

PCLss <- function(free= c(ER=.53,LR=.15,TR =.1, F1=.1,space=.03), 
                  fixed = c(theta=.5,nFeat=100,nSim=1000,nList=15,Tmin=NA, Tmax=NA, lambda=NA,Time=NA),
                  data=cbind(subset(read.csv(file.path('..','allData_by_subjects.csv')),subject==89),
                             pred_prac_acc = NA, pred_final_acc = NA,
                             pred_acc_plus= NA,pred_acc_neg= NA,
                             pred_prac_and_final=NA,pred_prac_and_not_final=NA,
                             pred_not_prac_and_final=NA,pred_not_prac_and_not_final=NA),
                  fitting=FALSE) {
  
  p <- c(free,fixed)

  if (any(p[names(p) %in% c("ER","LR","TR","F1","theta","space")] > 1,na.rm = TRUE) || 
      any(p[names(p) %in% c("ER","LR","TR","F1","lambda","theta","space")] < 0,na.rm = TRUE)) {
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
  prac <- recall(init_mem_C1,init_thresh, p['space'], p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  
  
  #control no practice
  controlStrengths <- init_mem_C1 - rbinom(mxn, init_mem_C1, p['F1'])
  controlAcc <- recall(controlStrengths, init_thresh, p['space'], p['Tmin'], p['Tmax'], p['Time'],p['lambda'])  

  # study practice
  #imm
  restudyStrengths <- init_mem_C1 + rbinom(mxn,p['nFeat']-init_mem_C1, p['LR'])
  restudyStrengths  <- restudyStrengths - rbinom(mxn,restudyStrengths, p['F1'])
  restudyAcc<-recall(restudyStrengths, init_thresh, p['space'], p['Tmin'], p['Tmax'], p['Time'],p['lambda'])  
  
  # test practice
  testStrengths <- init_mem_C1 #copy strengths and thresholds from practice test 
  testThresh <- init_thresh 
  #imm
  testStrengths[prac] <- init_mem_C1[prac] + rbinom(sum(prac),p['nFeat']-init_mem_C1[prac], p['LR'])
  testThresh[prac] <- init_thresh[prac] - rbinom(sum(prac),init_thresh[prac], p['TR'])
  testStrengths <- testStrengths -rbinom(mxn,testStrengths, p['F1'])
  testAcc <- recall(testStrengths, testThresh,p['space'],p['Tmin'], p['Tmax'], p['Time'],p['lambda'])  
  
  #no practice, other cue test practice
  #imm
  testOCStrengths <- init_mem_C2 - rbinom(mxn, init_mem_C2, p['F1'])
  testOCAcc<- recall(testOCStrengths, testThresh, p['space'], p['Tmin'], p['Tmax'], p['Time'],p['lambda'])  
  
  #compute predictions
  avgs <- lapply(list(prac=prac,
                      chain1 = testOCAcc, chain1plus = testOCAcc[prac],chain1neg = testOCAcc[!prac], 
                      chain1_prac_final = (prac & testOCAcc),
                      chain1_prac_not_final = (prac & !testOCAcc),
                      chain1_not_prac_final = (!prac & testOCAcc),
                      chain1_not_prac_not_final = (!prac & !testOCAcc),
                      chain2 = controlAcc, chain3= restudyAcc,
                      chain5 = testAcc, chain5plus = testAcc[prac],chain5neg = testAcc[!prac],
                      chain5_prac_final = (prac & testAcc),
                      chain5_prac_not_final = (prac & !testAcc),
                      chain5_not_prac_final = (!prac & testAcc),
                      chain5_not_prac_not_final = (!prac & !testAcc)),
                 mean)
  
  # Fill in data frame with preds
  data$pred_prac_acc[!is.na(data$prac_acc)]  <- avgs$prac
  insertCols <- c("pred_final_acc", "pred_acc_plus","pred_acc_neg","pred_prac_and_final",
                 "pred_prac_and_not_final","pred_not_prac_and_final","pred_not_prac_and_not_final")
  data[data$chain==1, insertCols] <- avgs[c("chain1", "chain1plus","chain1neg",
                                            "chain1_prac_final","chain1_prac_not_final",
                                            "chain1_not_prac_final","chain1_not_prac_not_final")]
  data$pred_final_acc[data$chain==2] <- avgs$chain2
  data$pred_final_acc[data$chain==3] <- avgs$chain3
  data[data$chain==5, insertCols] <- avgs[c("chain5", "chain5plus","chain5neg",
                                            "chain5_prac_final","chain5_prac_not_final",
                                            "chain5_not_prac_final","chain5_not_prac_not_final")]
  
  preds <- c(data$pred_final_acc[data$practice %in% c('C','S') & is.na(data$other_type)], # control and study practice
             unlist(t(data[!is.na(data$pred_acc_plus),tail(insertCols,4)]), use.names = FALSE)) # items with test practice 
  obs <- c(data$final_acc[data$practice %in% c('C','S') & is.na(data$other_type)], # control and study practice
           unlist(t(data[!is.na(data$pred_acc_plus),gsub("pred_","",tail(insertCols,4))]), use.names = FALSE))# items with test practice 
  N <- c(data$n[data$practice %in% c('C','S') & is.na(data$other_type)], 
         data$n[!is.na(data$pred_acc_plus)])
  err <- binomialLL(obs=obs[1:2],pred=preds[1:2],N=N[1:2]) + 
    multinomialLL(obs=obs[3:10],pred=preds[3:10],N=rep(N[3:4],each=4))
  if (fitting) {
    return(err)
  } else {
    return(data)
  }  
}