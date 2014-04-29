% SAM Testing Modeling
% Parameters
S_one=1; % Sampling Strength after initial study
S_two=2; % Sampling Strength after round of study practice
S_correct=S_two; % Sampling Strength after correct recall in test practice - same as after study practice
S_incorrect=S_one; % Sampling strength after incorrect recall in test practice - same as after initial study i.e. no sampling learning
R_one=1; % Recovery Strength after initial study
R_two=2; % Recovery Strength after study practice - same as after initial study i.e. no recovery learning
R_correct=3; % Recovery Strength after correct recall
R_incorrect=R_one; % Recovery Strength after incorrect recall
O_imm=1; % Extralist interference at test practice and at 5 minute final test - fixed
O_two=5; % Extralist interference at 2 day test
O_seven=10; % Extralist interference at 7 day test

params=[S_one S_two R_one R_two R_correct O_two O_seven];
[fitted_params, chisquare, ~ , info]=fminsearch(@fit_SAM,params,optimset('MaxFunEvals',1500));
%fit_SAM(params);
