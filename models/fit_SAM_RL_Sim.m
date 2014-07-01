function  err = fit_SAM_RL_Sim(params,data,design,k,nSub,nItems)
%   Detailed explanation goes here
rng(10); % seed the rng with a constant, so results converge.
S1=params(1);
S2=params(2);
rho=params(3);
R1=params(4); 
R_correct=params(5);
O_imm=1;        
O2=params(6);          
O7=params(7);

s_strengths = strengths([S1 S2],rho, [O_imm, O2,O7],'sampling', nSub, nItems);
r_strengths = strengths([R1 R_correct],rho, [O_imm, O2,O7],'recovery', nSub, nItems);
pred=recall(s_strengths,r_strengths,design,k);


%% Check predictions
Lu=(data.*log(data))+((1-data).*log(1-data));
Lc=(data.*log(pred))+((1-data).*log(1-pred));
err=-sum((2*120*(Lc(1:7)-Lu(1:7))));
if (S1 <=0 || S2 <= 0 || R1 <= 0 || R_correct <= 0 || O2 <= O_imm || O7 <= O_imm)
    err=1000000;
end

if S2 < S1;
   err=1000000;
end 

if R_correct < R1;
   err=1000000;
end 

h=figure(1);
set(h,'Position', [100, 100, 800, 500]);
hold off
plot([0,1,2,7], data(1:4), 'b--', [0,1,2,7], data([1 5:7]),'b');
hold on
plot([0,1,2,7], pred(1:4),'r--', [0,1,2,7], pred([1 5:7]),'r');
legend('Study (obs)','Test (obs)','Study (SAM)', 'Test (SAM)','Location','NortheastOutside');




