function  err = fit_SAM_RL_Sim(params,data,design,fix_params,free_params,one_shot,plotting,fit)
%   Detailed explanation goes here
rng(10); % seed the rng with a constant, so results converge.
% change values in free_params table with the updated ones from fminseach, and
% cat them together with the fix_params
param_list=[[free_params num2cell(params')]; fix_params(:,1:2)];
% Find all the S parameter values, and put them in a low-high sorted vector
find_S=param_list(~cellfun(@isempty,regexp(param_list(:,1),'S\d')),2);
S_params=sort(cell2mat(find_S))';
% Find all the R parameter values, and put them in a low-high sorted vector
find_R=param_list(~cellfun(@isempty,regexp(param_list(:,1),'R\w*')),2);
R_params=sort(cell2mat(find_R))';
% Find all the O parameter values, and put them in a low-high sorted vector
find_O=param_list(~cellfun(@isempty,regexp(param_list(:,1),'O\d')),2);
O_imm=1; 
O_params=[O_imm sort(cell2mat(find_O))'];
% Find the nItems parameter value
find_nItems=param_list(strcmp(param_list(:,1),'nItems'),2);
nItems=sort(cell2mat(find_nItems))';
% if sim
% Find the rho parameter value
find_rho=param_list(strcmp(param_list(:,1),'rho'),2);
rho=sort(cell2mat(find_rho))';
% Find the nSubs parameter value
find_nSubs=param_list(strcmp(param_list(:,1),'nSubs'),2);
nSubs=sort(cell2mat(find_nSubs))';
% end
% Find the k parameter value
find_k=param_list(strcmp(param_list(:,1),'k'),2);
k=sort(cell2mat(find_k))';
if any(strcmp('rho',free_params(:,1)))
    rho_txt='Rho free: ';
else
    rho_txt='Rho fixed @ ';
end

if strcmp(one_shot,'on')
   one_shot_ctrl=true;
else
   one_shot_ctrl=false;
end


s_strengths = strengths(S_params, rho, O_params, nSubs, nItems);
r_strengths = strengths(R_params, rho, O_params, nSubs, nItems);
pred=recall(s_strengths,r_strengths,design,k,one_shot_ctrl);


%% Check predictions
Lu=(data.*log(data))+((1-data).*log(1-data));
Lc=(data.*log(pred))+((1-data).*log(1-pred));
err=-sum((2*120*(Lc(1:7)-Lu(1:7))));
if ( any(S_params < 0)|| any(R_params < 0) || any(diff(O_params) < 0) || any(diff(S_params) < 0) || any(diff(R_params) < 0))
    err=1000000;
end

if  any(strcmp(plotting,{'iter','final'}))
    h=figure(1);
    set(h,'Position', [100,50, 1200, 700],'Name','SAM-RL Plots', 'NumberTitle', 'off');
    hold off
    if strcmp(fit,'check') 
            plot([0,1,2,7], data([4 1:3]), 'b--', [0,1,2,7], data(4:7),'b');
            hold on
            plot([0,1,2,7], pred([4, 1:3]),'r--', [0,1,2,7], pred(4:7),'r');
            legend('Study (obs)','Test (obs)','Study (SAM)', 'Test (SAM)','Location','Northeast');
            text('position',[.25,min([data pred])+.02], ...
                 'string', char(['\chi^2{ = }' num2str(err)] ,[ 'One Shot: ' one_shot], [rho_txt num2str(rho)]), ...
                 'FontWeight','bold');
    elseif strcmp(fit,'fit')
        subplot(6,6,[1:3 7:9 13:15], 'Position',[.03 .52 .45 .46])  
            plot([0,1,2,7], data([4 1:3]), 'b--', [0,1,2,7], data(4:7),'b');
            hold on
            plot([0,1,2,7], pred([4, 1:3]),'r--', [0,1,2,7], pred(4:7),'r');
            legend('Study (obs)','Test (obs)','Study (SAM)', 'Test (SAM)','Location','Northeast');
            text('position',[.25,min([data pred])+.02], ...
                 'string', char(['\chi^2{ = }' num2str(err)] ,[ 'One Shot: ' one_shot], [rho_txt num2str(rho)]), ...
                 'FontWeight','bold');
    end
end


end


