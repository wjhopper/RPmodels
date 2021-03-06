recallTime <- function(mem,thresh,Tmin=NULL,Tmax=NULL,Time=NULL,lambda=NULL) {

  RT <- serialOrder <- matrix(NA, nrow = nrow(mem), ncol = ncol(mem))
  recalled <- matrix(FALSE, nrow = nrow(RT), ncol = ncol(RT))
  for (i in 1:nrow(RT)) {
    ord <- order(mem[i,],decreasing=TRUE)
    reverseOrd <- order(ord)
    CRT <- cumsum(Tmin + (Tmax-Tmin)*exp(-lambda*abs(mem[i,]-thresh[i,]))[ord])
    acc <- CRT < Time & (mem[i,ord]  >= thresh[i,ord])
    if (any(acc)) {
      RT[i,ord[acc]] <- c(CRT[acc][1],diff(CRT[acc]))
    } 
    recalled[i,] <- acc[reverseOrd]
    serialOrder[i,] <- ord
  }
  
  return(list(Acc=recalled,RTrounded=RT,order=serialOrder))
  # tests
  # max(mem[i,])==mem[i,ord[1]] & min(mem[i,])==mem[i,ord[15]]
  # mem[i,ord][reverseOrd] == mem[i,]
  # all((mem[i,] >= thresh[i,])==recalled[i,])
  # all(!is.na(RT[i,]) ==recalled[i,] & is.na(RT[i,]) == !recalled[i,])
  
  
}

recallNoTime <- function(mem,thresh, ...) {
  recalled <- mem >= thresh
  serialOrder <- t(apply(mem,1, order, decreasing = TRUE))
  return(list(Acc=recalled, order=serialOrder))
}

RTdis <- function(RT = NULL, order = NULL, Time= NULL) {
  
  # for each rank ordering recall position (Nlist), 
  # the KS density at each 1/10 second of recall test period (TestTime)
  
  RTdist=matrix(0,nrow=10*Time,ncol=ncol(RT)) 
  RT <-t(sapply(seq(nrow(RT)),
                function(x) c(RT[x, order[x,][!is.na(RT[x,order[x,]])]], 
                              RT[x, order[x,][is.na(RT[x,order[x,]])]])
         ))
  for (n in 1:ncol(RT)) {
    RTs<- RT[!is.na(RT[,n]),n]
    if (length(RTs)>2) {
      D <- bkde(RTs,bandwidth=1,gridsize=900,range.x=c(.1,90))
#       D <- density(RTs,bw=1,n=900,from=.1,to=90)
      height <- D$y/sum(D$y)
      RTdist[,n] <- (length(RTs)/nrow(RT))*height
    }
  }
  RTdist <- melt(RTdist/sum(RTdist),varnames=c("RTrounded","order"),value.name = "RTdist")
  RTdist$RTrounded <- RTdist$RTrounded/10
  return(RTdist)
}

LL <- function(obs,pred) {
  likelihoods <- inner_join(obs,pred)
#   likelihoods$RTdist[likelihoods$RTdist == 0] <- (0.5)*.Machine$double.xmin
  err <- -sum(log(likelihoods$RTdist))
  return(err)
}

SSE <- function(obs,pred) {
  obs <- obs %>% group_by(class,order) %>% 
    summarise(acc = sum(score)/4)
  data <- pred %>% group_by(class,order) %>% 
    summarise(pred_acc = mean(pred_acc)) %>% 
   left_join(obs)
  data$acc[is.na(data$acc)] <- 0 # set na's for accuracy for zeros
  err <- sum((data$pred_acc-data$acc)^2)
  return(err)
}

PCL <- function(free= c(ER=.53,LR=.3,TR =.3, FR=.1,Tmin=2, Tmax=10, lambda=.8), 
                fixed = c(theta=.5,nFeat=100,nSim=1000,nList=15,Time=90),
                yoke = NULL, data=cbind(read.csv(file.path('..','CFRss.csv')),
                                        pred_acc = NA,pred_acc_plus= NA,pred_acc_neg= NA),
                fittingRT=FALSE, fittingAcc=FALSE) {
  p <- c(free,fixed)
  
  if (any(p[names(p) %in% c("ER","LR","TR","FR","theta")] > 1) || any(p[names(p) %in% c("ER","LR","TR","FR","lambda","theta")] < 0)) {
    err <- 100000
    return(err)
  }
  
  set.seed(456)
  if (any(is.na(c(p["lambda"],p["Tmin"],p["Tmax"],p['Time'])))) {
    useTime =FALSE
    recall <- recallNoTime
  } else{
    useTime <- TRUE
    recall <- recallTime
  }
  mxn <-  p['nSim']*p['nList'] #dimensions precalculation 
  
  #practice test
  init_mem <- matrix(rbinom(mxn,p['nFeat'], p['ER']),nrow=p['nSim'],ncol=p['nList'])
  init_thresh <- matrix(rbinom(mxn,p['nFeat'], p['theta']),nrow=p['nSim'],ncol=p['nList'])
#   init_mem <- as.matrix(read.csv('mem.csv'))
#   init_thresh <- as.matrix(read.csv('theta.csv'))
  prac <- recall(init_mem,init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])
  
  # study practice
  restudyStrengths <- init_mem + rbinom(mxn,p['nFeat']-init_mem, p['LR'])
  restudyStrengths  <- restudyStrengths - rbinom(mxn,restudyStrengths, p['FR'])
  restudy<-recall(restudyStrengths, init_thresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])  
  
  # test practice
  testStrengths <- init_mem #copy strengths and thresholds from practice test 
  testThresh <- init_thresh 
  testStrengths[prac$Acc] <- init_mem[prac$Acc] + rbinom(sum(prac$Acc),p['nFeat']-init_mem[prac$Acc], p['LR'])
  testThresh[prac$Acc] <- init_thresh[prac$Acc] - rbinom(sum(prac$Acc),init_thresh[prac$Acc], p['TR'])
  testStrengths <- testStrengths -rbinom(mxn,testStrengths, p['FR'])
  test <- recall(testStrengths, testThresh, p['Tmin'], p['Tmax'], p['Time'],p['lambda'])

  if (useTime) {
    dist <- data.frame(class = rep(c('np','sp','tp'),each=13500), # 40500 = 15 items * 900 points 
                        rbind(RTdis(prac$RT, prac$order, p['Time']),
                              RTdis(restudy$RT, restudy$order, p['Time']),
                              RTdis(test$RT, test$order, p['Time']),
                              row.names = NULL))
  } else {
    acc <-rbind(prac$Acc,restudy$Acc,test$Acc)
    order <- rbind(prac$order,restudy$order,test$order)
    acc <-t(sapply(seq(nrow(acc)),
                  function(x) c(acc[x, order[x,][!is.na(acc[x,order[x,]])]], 
                                acc[x, order[x,][is.na(acc[x,order[x,]])]])
    )) %>%
      melt(varnames=c("class","order"),value.name = "pred_acc") %>% 
      mutate(class = rep(rep(c("np","sp","tp"),each = nrow(prac$Acc)),ncol(prac$Acc)))
  }
  
  if (fittingRT) {
    err <- LL(obs=data[,c("class","order","RTrounded")],pred = dist)
    return(err)
  } else if (fittingAcc) {
        err <- SSE(obs=data, pred  = acc )
    return(err)
  } else { 
    err <- LL(obs=data[,c("class","order","RTrounded")],pred = dist)
    RT <-rbind(prac$RT,restudy$RT,test$RT)
    order <- rbind(prac$order,restudy$order,test$order)
    RT <-t(sapply(seq(nrow(RT)),
                  function(x) c(RT[x, order[x,][!is.na(RT[x,order[x,]])]], 
                                RT[x, order[x,][is.na(RT[x,order[x,]])]])
    ))
    acc <-melt(!is.na(RT), varnames=c("class","order"),value.name = "pred_acc")
    RT <- melt(RT, varnames=c("class","order"),value.name = "pred_RT")
    preds <- data.frame(sub_num = data$sub_num[1],left_join(acc,RT))
    preds$class <- rep(rep(c("np","sp","tp"),each = nrow(prac$Acc)),ncol(prac$Acc))
    
    if (useTime) {
      return(list(err=err,preds=preds,dist = dist))
    } else {
      return(list(err=err,preds=preds))
    }
  }
}