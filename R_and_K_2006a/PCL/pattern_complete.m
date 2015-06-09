function err = pattern_complete(pars)

% Features are activated, or not, by retrieval cues. This is modeled with
% the binomial distribution. Re-study and successfull recall both result in
% additional features becoming activated. However, successfull recall also
% lowers the threshold (theta) number of features that arel,  necessary to
% support pattern completion (aka recovery). In this modethere is no
% magic change between immediate and delay in terms previously failed
% recoveries. Instead, similar to the bifurcation model, the role of delay
% is to decrease the number of features that are activated so as to place
% memories closer to their thresholds. However, because testing decreases
% these thresholds, previously recalled memories are initially more resisant to this form
% of forgetting. This model *might* be able to handle the cue change data
% by having test practice with the other cue decrease the theta of the
% target memory but not result in any additional learning of features (no
% increase in features acivated by cue uses at final test). Also, this
% model can be applied directly to the latency data and can even model the
% very linear cumulative latency curve after testing (because all
% previously recalled memories are now far above threshold, their
% individual recovery times are all at the minimum)

% in summary, this model is a marriage between the bifurcation
% model and SAM. The key insight is that the threshold in the bifurcation
% model is the threshold necessary for recovery (pattern completion).
% Furthermore, it's not that test practice makes the memories super strong
% as compared to restudy, but rather that memories are more readily
% recovered (lower threshold). This directly corresponds to the connection
% strength between features (which supports recovery) as compared to the
% connection strength between retrieval cues and memories.

% one final note. Unlike the bifurcation model, this model can produce
% immediate inreases in performance following test practice. This occurs
% because a decrease in theta for previously recalled memories means that
% these memories can be recovered more quickly the next time. Because they
% are recalled more quickly, there is more time to work down the list of
% memories (which are sampled in terms of memory strength, somewhat akin to
% the Luce choice rule). In this manner, it was not necessary to assume
% that more time was spent attempting to recall in the final test of the
% R&K experiment.

    % learning parameters

%     Learn = .58; % proportion of features initially learned
%     Relearn = .07; % proportion additional learned with study or test success
%     Forget1 = .1;   % proportional forgetting after 2 days 
%     Forget2 = .1;   % proportional additional forgetting for next 5 days
%     ThetaReduce = .1; % proportional theta reduction for test success (recovery learning)
    if any(pars <0) || any(pars(1:4) >1)
        err=100000
    else
        Learn = pars(1); % proportion of features initially learned
        Relearn = pars(2); % proportion additional learned with study or test success
        Forget1 = pars(3);   % proportional forgetting after 2 days 
        Forget2 = pars(4);   % proportional additional forgetting for next 5 days
        ThetaReduce = pars(5); % proportional theta reduction for test success (recovery learning)

        % latency parameters

        % these can affect accuracy if time runs out
        % however, not enough constraint to have all of these be free when only
        % modeling accuracy
        % for R&K data, letting Tmin be free should be sufficient

        Tmax = 60;  % at threshold, time to fail to recover
    %     Tmin = 15;  % time to recover(or not) when far from threshold (either direction)
        Tmin = pars(6);  % time to recover(or not) when far from threshold (either direction
        lambda = .5;  % exponential change in moving away from threshold

        % fixed values

        Theta = .5; % because parameters can scale, fix at .5
        Nfeat=100;  % number of total features per item
        Nsim = 1000;  % number of simulations to average
        Nlist = 30;   % items per study list
        TestTime = 420; % 7 minutes

        InitialMemory = zeros(Nsim,Nlist);
        InitialThetas = zeros(Nsim,Nlist);

        % both learning and setting of thetas for each memory are binomial random
        InitialMemory = increment(InitialMemory,Learn);
        InitialThetas = increment(InitialThetas,Theta);
        Baseline1 = recall(InitialMemory,InitialThetas); % practice test performance

    %     testmemories = double(dataset('File','mem.csv','Delimiter',','));
    %     testthresh = double(dataset('File','thresh.csv','Delimiter',','));
    %     Baselinetest = recall(testmemories,testthresh); % practice test performance
        % after re-study
        StudyMemory = increment(InitialMemory,Relearn);  % learn more features for re-studied items
        AfterStudy1 = recall(StudyMemory,InitialThetas);
        StudyMemory = decrement(StudyMemory,Forget1);    % remove features with delay (context change matches less well)
        AfterStudy2 = recall(StudyMemory,InitialThetas);  
        StudyMemory = decrement(StudyMemory,Forget2);    % remove aditional features with additional delay
        AfterStudy3 = recall(StudyMemory,InitialThetas);  

        % after practice test
        TestMemory = InitialMemory;
        TestMemory(Baseline1==1) = increment(TestMemory(Baseline1==1),Relearn); % learn more features for recalled items
        TestThetas = InitialThetas;
        TestThetas(Baseline1==1) = decrement(TestThetas(Baseline1==1),ThetaReduce);  % decrease thetas for recalled items
        AfterTest1 = recall(TestMemory,TestThetas);
        TestMemory = decrement(TestMemory,Forget1); % remove features with delay (context change matches less well)
        AfterTest2 = recall(TestMemory,TestThetas);
        TestMemory = decrement(TestMemory,Forget2); % remove aditional features with additional delay
        AfterTest3 = recall(TestMemory,TestThetas);

        % baseline performance (aside from test practice)
        FinalMemory = InitialMemory;
        FinalMemory = decrement(FinalMemory,Forget1);
        Baseline2 = recall(FinalMemory,InitialThetas);
        FinalMemory = decrement(FinalMemory,Forget2);
        Baseline3 = recall(FinalMemory,InitialThetas);

        results=[mean(mean(Baseline1)) mean(mean(Baseline2))  mean(mean(Baseline3));
                 mean(mean(AfterStudy1)) mean(mean(AfterStudy2)) mean(mean(AfterStudy3));
                 mean(mean(AfterTest1)) mean(mean(AfterTest2)) mean(mean(AfterTest3))];
        obs = [.7 .81 .75 .54 .68 .42 .56];
        pred = results([1 2 3 5 6  8 9]);
        Lu=(obs.*log(obs))+((1-obs).*log(1-obs));
        Lc=(obs.*log(pred))+((1-obs).*log(1-pred));
        err=-sum((2*120*(Lc-Lu)));
    end
    %     plot(results'); 
    %     axis([1 3 0 1]);

        function mem=increment(mem,rate)
            mem = mem + binornd(Nfeat-mem,rate);
        end

        function mem=decrement(mem,rate)
            mem = mem - binornd(mem,rate);
        end

        function recalled=recall(mem,thetas)
             TotRT = zeros(Nsim,Nlist);          
            [~,ind]=sort(mem,2,'descend'); % sampling goes in decreasing memory strength order (not random)
            % because thetas are random, decreasing strength does not gaurantee rank ordering of recoverability

            Recoverable=mem>thetas;  % only above threshold memories are recoverable   
            RT=Tmin + (Tmax-Tmin).*exp(-lambda.*abs(mem-thetas)); % time to recovery (or fail to recover) each memory

            for s=1:Nsim    
    %             ListRT=cumsum(RT(s,ind(s,:)));
    %             TotRT(s,ind(s,:))=ListRT;            % cumulative recall latency
                TotRT(s,ind(s,:)) = cumsum(RT(s,ind(s,:)));
            end
            BeforeDeadline=TotRT<TestTime;  % only memories with RTs less than deadline can be recalled
            recalled = BeforeDeadline.*Recoverable;  % recalled memories are recoverable and before deadline
        end


end
