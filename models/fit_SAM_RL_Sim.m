function  err = fit_SAM_RL_Sim(params,data,design,k,nSub,nItems)
%   Detailed explanation goes here

S1=params(1);
S2=params(2);
rho=params(3);
R1=params(4); 
R_correct=params(5);
O_imm=1;        
O2=params(6);          
O7=params(7);

strengths = sample_strengths([S1 S2],rho, [O_imm, O2,O7], nSub, nItems);
r_strengths = recovery_strengths([R1 R_correct],rho, [O_imm, O2,O7], nSub, nItems);
pred=recall(strengths,r_strengths,design,k);



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





