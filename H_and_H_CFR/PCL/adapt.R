recall <- function(mem,thetas) {
  a <- apply(mem,1,sort.int,decreasing = TRUE,index.return=TRUE, method='quick') #sampling goes in decreasing memory strength order (not random)
  inds <- do.call("rbind", lapply(a, "[[", 2))
    for (i in 1:nrow(mem)) {
        totalRT[i,inds[i,]] <- cumsum(RT[i,inds[i,]])
  }
  
  RT=Tmin + (Tmax-Tmin)*exp(-lambda*abs(mem-thresh))  #time to recovery (or fail to recover) each memory
  
  recalled <- (RT < Time) & (mem > thresh) # only above threshold and shorter than alloted time are recoverable

  for (s in 1:nrow(mem)) {
    RTsim=RT[s,]
    memsim=mem[s,]
    thetasim=thetas[s,]
  
  [val,ind]=sort(memsim,decreasing = TRUE,index.return=TRUE) # rank based on number of features encoded
  
  RankedTotRT=cumsum(RTsim(ind));  % find cumulative latency based on ranking, regardless of recoverability
  TotRT=RankedTotRT(ind);             % convert back to unordered list
  TotRT(memsim<thetasim)=TestTime; % if unrecoverable, set time to end of test period
  TotRT(TotRT>TestTime)=TestTime; % if total time greater than test period, set equal to end of test period
  
  a=sort(TotRT,'ascend');  % new sorting based on actual recall RTs (only recoverable within alloted time)
  
  recalled(s,:)=TotRT<TestTime;  % used for accuracy across all simulations
  
  RankedTotRTs(s,:)=TotRT(ind);  % used for latency across all simulations (in order of recall)
  
  }
}

RTdis=InterRecallDist(RankedTotRTs);  % calculate latency distributions
end

RTdis <- function(RankedTotRTs) {

RTdis=zeros(10*TestTime,Nlist);  % for each rank ordering recall position (Nlist), the KS density at each 1/10 second of recall test period (TestTime)

times=[.1:.1:TestTime]
        
      for (n in 1:p['nList']) {
          if n==1
              RTs=RankedTotRTs(RankedTotRTs(:,n)<TestTime,n);
          else
              RTs=RankedTotRTs(RankedTotRTs(:,n)<TestTime,n)-RankedTotRTs(RankedTotRTs(:,n)<TestTime,n-1);
          end
          Nobs=size(RTs,1);
          if Nobs>0
              RTdis(:,n)=ksdensity(RTs,times,'bandwidth',1);
          end
          RTdis(:,n)=RTdis(:,n)./sum(RTdis(:,n));  % normalize so that cumulative distribution adds up to 1.0
          RTdis(:,n)=(Nobs/Nsim).*RTdis(:,n);   % reduce distribution by number of recalls at that ranked list position
          
      }
      
      RTdis=RTdis./sum(sum(RTdis));

}


LL <- function(obs,pred,N) {

  return(err)
}

PCL <- function(free= c(ER=.58,LR=.07,TR =.1, F1=.1,F2=.1), fixed = c(Tmin=1, Tmax=10, lambda=.5,theta=.5,nFeat=100,nSim=1000,nList=15,Time=NULL),
                data=NULL, fitting=FALSE) {
  
  p <- c(free,fixed)
  
  if (any(p[names(p) %in% c("ER","LR","TR","F1","F2","theta")] > 1) || any(names(p) %in% c("ER","LR","TR","F1","F2","lambda","theta") < 0)) {
    err <- 100000
    return(err)
  }
  set.seed(456)
  mxn <-  p['nSim']*p['nList'] #dimensions precalculation 
  
  #practice test
  init_mem <- matrix(rbinom(mxn,p['nFeat'], p['ER']),nrow=p['nSim'],ncol=p['nList'])
  init_thresh <- matrix(rbinom(mxn,p['nFeat'], p['theta']),nrow=p['nSim'],ncol=p['nList'])
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