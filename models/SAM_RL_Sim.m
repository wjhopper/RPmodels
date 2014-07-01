% SAM Testing Effect Simulation
% Free Parameters
S1=.004743; % mean of sampling strength distribution after initial study
S2=.021532; % mean of sampling strength distribution after study practice & correct recall
rho=0;   % scaling paramter to get SD of sampling distribution
R1=4.036; % Recovery Strength after initial study
R_correct=20.81; % Recovery Strength after correct recall
O2=2.9828; % Extralist interference at 2 day test
O7=4.499; % Extralist interference at 7 day test
k=500;
nItems=30;
nSub=1000;
design={'S','T';'S','S'};
[rn,cn]=find(strcmp(design,'T'));
if any(cn==1)
    error('design:dimensions','You shouldn''t have a test before you study!');
end
% data=[acc on recall practice, acc on 5 min test after study,  acc on 2 day test after study,
% acc on 1 week test after study, acc on 5 min test after recall practice,
% acc on 2 day test after recall practice, acc on 1 week test after recall practice] 
data=[ .70, .81, .54, .42, .75, .68, .56];
params=[S1 S2 rho R1 R_correct O2 O7];
%[fitted_params, chisquare, ~ , info]=fminsearch(@(x) fit_SAM_RL_Sim(x,data,design,k,nSub,nItems),params,optimset('MaxFunEvals',2500));
chisquare=fit_SAM_RL_Sim(params,data,design,k,nSub,nItems);