% SAM Testing Effect Simulation
% Free Parameters
S1=2; % mean of sampling strength distribution after initial study
S2=4.1; % mean of sampling strength distribution after study practice & correct recall
rho=.25;   % scaling paramter to get SD of sampling distribution
R1=4; % Recovery Strength after initial study
R_correct=99; % Recovery Strength after correct recall
O2=2; % Extralist interference at 2 day test
O7=4; % Extralist interference at 7 day test
k=500;
nItems=3;
nSub=10;
design[];
% data=[acc on recall practice, acc on 5 min test after study, acc on 5 min
% test after recall practice, acc on 2 day test after study, acc on 2 day
% test after recall practice, acc on 1 week test after study, acc on 1 week
% test after recall practice] 
data=[ .70, .81, .75, .54, .68, .42, .56];
params=[S1 S2 rho R1 R_correct O2 O7];
[fitted_params, chisquare, ~ , info]=fminsearch(@(x) fit_SAM_RL_Sim(x,data,design,k,nItems,nSub),params,optimset('MaxFunEvals',2500));
