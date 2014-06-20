% SAM Testing Effect Simulation
% Free Parameters
S1=2; % mean of sampling strength distribution after initial study
S2=4; % mean of sampling strength distribution after study practice & correct recall
rho=.25;   % scaling paramter to get SD of sampling distribution
R1=4; % Recovery Strength after initial study
R_correct=99; % Recovery Strength after correct recall
O2=2; % Extralist interference at 2 day test
O7=4; % Extralist interference at 7 day test

params=[S1 S2 rho R1 R_correct O2 O7];
[fitted_params, chisquare, ~ , info]=fminsearch(@fit_SAM_RL_Sim,params,optimset('MaxFunEvals',2500));
