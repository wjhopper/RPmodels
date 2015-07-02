function pattern_complete_latency

    % learning parameters

    Learn = .53; % proportion of features initially learned
    Relearn = .1; % proportion additional learned with study or test success
    Forget = .05;   % proportional forgetting at final test 
    ThetaReduce = .05; % proportional theta reduction for test success (recovery learning)
    
    % latency parameters
    
    Tmax = 10;  % at threshold, time to fail to recover
    Tmin = 2;  % time to recover(or not) when far from threshold (either direction)
    lambda = .2;  % exponential change in moving away from threshold
    
    % fixed values
    
    Theta = .5; % because parameters can scale, fix at .5
    Nfeat=100;  % number of total features per item
    Nsim = 1000;  % number of simulations to average
    Nlist = 15;   % items per study list
    TestTime = 90; % 1.5 minutes

    InitialMemory = zeros(Nsim,Nlist);
    InitialThetas = zeros(Nsim,Nlist);

    % both learning and setting of thetas for each memory are binomial random
%     InitialMemory = increment(InitialMemory,Learn);
%     InitialThetas = increment(InitialThetas,Theta);
    InitialMemory = double(dataset('File','mem.csv','Delimiter',','));
    InitialThetas = double(dataset('File','theta.csv','Delimiter',','));

    % practice test performance
    [Baseline,BaselineRTs] = recall(InitialMemory,InitialThetas); 
    
    % after practice test
    TestMemory = InitialMemory;
    TestMemory(Baseline==1) = increment(TestMemory(Baseline==1),Relearn); % learn more features for recalled items
    TestThetas = InitialThetas;
    TestThetas(Baseline==1) = decrement(TestThetas(Baseline==1),ThetaReduce);  % decrease thetas for recalled items
    TestMemory = decrement(TestMemory,Forget); % remove features with delay (context change matches less well)
    [AfterTest,AfterTestRTs] = recall(TestMemory,TestThetas);
    
    % after re-study
    StudyMemory = increment(InitialMemory,Relearn);  % learn more features for re-studied items
    StudyMemory = decrement(StudyMemory,Forget);    % remove features with delay (context change matches less well)
    [AfterStudy,AfterStudyRTs] = recall(StudyMemory,InitialThetas); 
    
    
    mean(mean(Baseline))
    figure(1);
    for n=1:Nlist
        subplot(3,Nlist,n);
        plot(BaselineRTs(:,n));
        axis([0 200 0 .003]);
    end
    
    mean(mean(AfterTest))
    figure(1);
    for n=1:Nlist
        subplot(3,Nlist,Nlist+n);
        plot(AfterTestRTs(:,n));
        axis([0 200 0 .003]);
    end
    
    mean(mean(AfterStudy))
    figure(1);
    for n=1:Nlist
        subplot(3,Nlist,2*Nlist+n);
        plot(AfterStudyRTs(:,n));
        axis([0 200 0 .003]);
    end
    
    function mem=increment(mem,rate)
        mem = mem + binornd(Nfeat-mem,rate);
    end

    function mem=decrement(mem,rate)
        mem = mem - binornd(mem,rate);
    end

    function [recalled,RTdis]=recall(mem,thetas)
        [val,ind]=sort(mem,2,'descend'); % sampling goes in decreasing memory strength order (not random)
        % because thetas are random, decreasing strength does not gaurantee rank ordering of recoverability
        
        Recoverable=mem>thetas;  % only above threshold memories are recoverable   
        
        RT=Tmin + (Tmax-Tmin).*exp(-lambda.*abs(mem-thetas)); % time to recovery (or fail to recover) each memory
        
        for s=1:Nsim
            RTsim=RT(s,:);
            memsim=mem(s,:);
            thetasim=thetas(s,:);
            
            [val,ind]=sort(memsim,'descend'); % rank based on number of features encoded
            
            RankedTotRT=cumsum(RTsim(ind));  % find cumulative latency based on ranking, regardless of recoverability
            TotRT=RankedTotRT(ind);             % convert back to unordered list
            TotRT(memsim<thetasim)=TestTime; % if unrecoverable, set time to end of test period
            TotRT(TotRT>TestTime)=TestTime; % if total time greater than test period, set equal to end of test period
            
            [val,ind]=sort(TotRT,'ascend');  % new sorting based on actual recall RTs (only recoverable within alloted time)
            
            recalled(s,:)=TotRT<TestTime;  % used for accuracy across all simulations
            
            RankedTotRTs(s,:)=TotRT(ind);  % used for latency across all simulations (in order of recall)
                   
        end
        
        RTdis=InterRecallDist(RankedTotRTs);  % calculate latency distributions
    end

    function RTdis=InterRecallDist(RankedTotRTs)
       
        RTdis=zeros(10*TestTime,Nlist);  % for each rank ordering recall position (Nlist), the KS density at each 1/10 second of recall test period (TestTime)
        
        times=[.1:.1:TestTime]';
        
        for n=1:Nlist
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
            
        end
        
        RTdis=RTdis./sum(sum(RTdis));
    end
end
