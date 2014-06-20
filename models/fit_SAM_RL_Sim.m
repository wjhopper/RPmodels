function  err = fit_SAM_RL_Sim(params)
%   Detailed explanation goes here
% data=[acc on recall practice, acc on 5 min test after study, acc on 5 min
% test after recall practice, acc on 2 day test after study, acc on 2 day
% test after recall practice, acc on 1 week test after study, acc on 1 week
% test after recall practice]

data=[.70,.81,.75,.54,.68,.42,.56];
k=500;
nItems=30;
nSub=10000;
O_imm=1;
S1=params(1);
S2=params(2);
rho=params(3);
R1=params(4);
R_correct=params(5);
O2=params(6);
O7=params(7);

%% Practice Test!!!
sampled=sample(S1,rho,O_imm,nSub,nItems,k,'study');
recalled=recover(R1,rho,O_imm,sampled);       
practice_test=mean(mean(recalled(:,1:end-1),2));


%% Immediate Test: Restudied 
sampled=sample((S2-S1),rho,O_imm,nSub,nItems,k,'restudy',sampled);
recalled=recover(R1,rho,O_imm,sampled); 
restudied_imm_test=mean(mean(recalled(:,1:end-1,2)));

%% Immediate Test: Tested 
sampled=sample(S1,rho,O_imm,nSub,nItems,k,'test');
recalled=recover(R1,rho,O_imm,sampled); 
tested_imm_test=mean(mean(recalled(:,1:end-1,2)));


%% 2-Day Delay: Restudied
sampled=sample((S2-S1),rho,O_imm,nSub,nItems,k,'restudy',sampled);
recalled=recover(R1,rho,O_imm,sampled); 
restudied_imm_test=mean(mean(recalled(:,1:end-1,2)));restudied_2_day_test=mean(mean(recalled(:,1:end-1,2)));

%% 2-Day delay: Tested
tested_2_day=[ nan(size(recalled,1),size(recalled,2))  repmat(O_two,1000,1)] ; % initialize 
tested_2_day(recalled(:,1:end-1)==1)=normrnd(S_two,rho,1,numel(tested_2_day(recalled==1)));
tested_2_day(recalled(:,1:end-1)==0)=normrnd(S_one,rho,1,numel(tested_2_day(recalled==0)));
tested_2_day(tested_2_day<0)=0;
tested_2_day(:,end+1)=sum(tested_2_day,2); % sum of within-experiment item strengths and interference parameter (i.e. denominator of luce choice sampling rule)
tested_2_day(:,1:(end-1))=tested_2_day(:,1:(end-1))./repmat((tested_2_day(:,end)),1,size(tested_2_day(:,1:(end-1)),2)); % probability of sampling each item on an attempt
tested_2_day_samples=mnrnd(k,tested_2_day(:,1:end-1)); %500 attempts at sampling items for list, returns the # of time each item was sampled
recalled=zeros(size(tested_2_day_samples,1),size(tested_2_day_samples,2)); % initialize 

for i = 1:numel(tested_2_day_samples) % each item, for each 'participant'......
    
    if (tested_2_day_samples(i) > 0) && (sample_fails ~= 1) % if an item was sampled at least once
        recovery = binornd(1,R_one/(R_one + O_two),1,tested_2_day_samples(i)); % attempt-level results of n recovery trials
        [~,first_fail] = min(recovery); 
        [~, first_hit] = max(recovery);
        if first_hit > first_fail; % only if success comes before failure can you ever recall!
            recalled(i)=1;
        end
    end 

end                
tested_2_day_test=mean(mean(recalled(:,1:end-1,2)));

%% 7-day delay: Restudied
restudy_7_day=[normrnd(S_two,rho,1000,nItems) repmat(O_seven,1000,1)];% 30 Item Strengths after initial restudy, 1000 subjects, and interference
restudy_7_day(restudy_7_day<0)=0; % make 0 the minimum strength
restudy_7_day(:,end+1)=sum(restudy_7_day,2); % sum of within-experiment item strengths and interference parameter (i.e. denominator of luce choice sampling rule)
restudy_7_day(:,1:(end-1))=restudy_7_day(:,1:(end-1))./repmat((restudy_7_day(:,end)),1,size(restudy_7_day(:,1:(end-1)),2)); % probability of sampling each item on an attempt
restudy_7_day_samples=mnrnd(k,restudy_7_day(:,1:end-1)); %500 attempts at sampling items for list, returns the # of time each item was sampled
recalled=zeros(size(restudy_7_day_samples,1),size(restudy_7_day_samples,2)); % initialize 
pack; % optimize memory

for i = 1:numel(restudy_7_day_samples) % each item, for each 'participant'......
    
    if restudy_7_day_samples(i) > 0 % if an item was sampled at least once
        recovery = binornd(1,R_one/(R_one + O_seven),1,restudy_7_day_samples(i)); % attempt-level results of n recovery trials
        [~,first_fail] = min(recovery); 
        [~, first_hit] = max(recovery);
        if first_hit > first_fail; % only if success comes before failure can you ever recall!
            recalled(i)=1;
        end
    end 

end                
restudied_7_day_test=mean(mean(recalled,2));

%% 7-day delay: Tested






%% Check predictions
pred_tot=[practice_test restudied_imm_test tested_imm_test ...
    restudied_2_day_test tested_2_day_test restudied_7_day_test tested_7_day_test];

Lu=(data.*log(data))+((1-data).*log(1-data));
Lc=(data.*log(pred_tot))+((1-data).*log(1-pred_tot));
err=-sum((2*120*(Lc(1:7)-Lu(1:7))));
if (S_one <=0 || S_two <= 0 || R_one <= 0 || R_correct <= 0 || O_two <= 0.01 || O_seven <= 0.01)
    err=1000000;
end

if S_two < S_one;
   err=1000000;
end 

if R_correct < R_one;
   err=1000000;
end 

figure(1);
hold off
plot(data);
hold on
plot(pred_tot,'r');





