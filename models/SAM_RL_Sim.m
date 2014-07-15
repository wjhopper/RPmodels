function SAM_RL_Sim(fit,plotting)
% SAM Testing Effect Simulation
switch nargin
    case 0
        fit='fit';
        plotting='final';
    case 1
        plotting = 'final';
end    
global pred

% Free Parameters54
S1=.004743; % mean of sampling strength distribution after initial study
S2=.021532; % mean of sampling strength distribution after study practice & correct recall
rho=0;   % scaling paramter to get SD of sampling distribution
R1=4.036; % Recovery Strength after initial study
R_correct=20.81; % Recovery Strength after correct recall
O2=2.9828; % Extralist interference at 2 day test
O7=4.499; % Extralist interference at 7 day test
%save('starting_params.mat');
% Fixed Parameters 
k=500;
nItems=30;
nSub=1000;
design={'S','T';'S','S'};
[~,cn]=find(strcmp(design,'T'));
if any(cn==1)
    error('design:dimensions','You shouldn''t have a test before you study!');
end


% data=[acc on 5 min test after study,  acc on 2 day test after study,  acc on 1 week test after study, 
%       acc on recall practice,  acc on 5 min test after recall practice,
%       acc on 2 day test after recall practice, acc on 1 week test after recall practice] 
data=[ .81, .54, .42, .70,  .75, .68, .56];
params=[S1 S2 rho R1 R_correct O2 O7];

one_shot=1;
% Set plotting behavior
if strcmpi(plotting,'iter')  
    fmin_opts=optimset('MaxFunEvals',2500,'OutputFcn', @iter_output);
    plotting=1;
elseif strcmp(plotting,'final')
    fmin_opts=optimset('MaxFunEvals',2500,'OutputFcn', @final_output);
    plotting=1;
elseif strcmp(plotting,'off')
    fmin_opts=optimset('MaxFunEvals',2500);
    plotting=0;
else    
    error('SAM_RL_Sim:input_args', 'Invalid input argument. ''plotting'' must be equal to ''iter'', ''final'', of ''off''')
end

% set simulation to find best fitting params, or to use starting params and
% quit
if strcmpi(fit,'fit')
    [fitted_params, chisquare, ~ , info]=fminsearch(@(x) fit_SAM_RL_Sim(x,data,design,k,nSub,nItems,one_shot),params,fmin_opts) %#ok<NASGU,NOPRT,*ASGLU>
elseif strcmp(fit,'check')
    chisquare=fit_SAM_RL_Sim(params,data,design,k,nSub,nItems,one_shot,plotting) %#ok<NASGU,NOPRT>
else
    error('SAM_RL_Sim:input_args', 'Invalid input argument. ''fit'' must be equal to ''fit'' or ''check''')
end 

% fminsearch's ouput functions
function stop = iter_output(~,~,state)
    stop = false;
    if any(strcmp(state,{'iter','done'}))
            h=figure(1);
            set(h,'Position', [100, 100, 800, 500]);
            hold off
            plot([0,1,2,7], data(1:4), 'b--', [0,1,2,7], data([1 5:7]),'b');
            hold on
            plot([0,1,2,7], pred(1:4),'r--', [0,1,2,7], pred([1 5:7]),'r');
            legend('Study (obs)','Test (obs)','Study (SAM)', 'Test (SAM)','Location','NortheastOutside');
    end
end

function stop = final_output(~,~,state)
    stop = false;
    if strcmp(state,'done')
            h=figure(1);
            set(h,'Position', [100, 100, 800, 500]);
            hold off
            plot([0,1,2,7], data(1:4), 'b--', [0,1,2,7], data([1 5:7]),'b');
            hold on
            plot([0,1,2,7], pred(1:4),'r--', [0,1,2,7], pred([1 5:7]),'r');
            legend('Study (obs)','Test (obs)','Study (SAM)', 'Test (SAM)','Location','NortheastOutside');
    end
end
end