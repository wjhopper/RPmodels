function  err = fit_SAM_RL_Sim(params,data,design,fix_params,free_params,one_shot,plotting,varargin)
%   Detailed explanation goes here
rng(10); % seed the rng with a constant, so results converge.

S1=params(strcmp(free_params(:,1),'S1'));
S2=params(strcmp(free_params(:,1),'S2'));
R1=params(strcmp(free_params(:,1),'R'));
R_correct=params(strcmp(free_params(:,1),'Rcor'));
O2=params(strcmp(free_params(:,1),'O1'));
O7=params(strcmp(free_params(:,1),'O2'));
if any(strcmp('rho',free_params(:,1)))
    rho=params(strcmp(free_params(:,1),'rho'));
    rho_txt='Rho free: ';
else
    rho=fix_params{strcmp(fix_params(:,1),'rho'),2};
    rho_txt='Rho fixed @ ';
end
k=fix_params{strcmp(fix_params(:,1),'k'),2};
nSub=fix_params{strcmp(fix_params(:,1),'nSubs'),2};
nItems=fix_params{strcmp(fix_params(:,1),'nItems'),2};
O_imm=1;   
if strcmp(one_shot,'on')
   one_shot_ctrl=true;
else
   one_shot_ctrl=false;
end

s_strengths = strengths([S1 S2],rho, [O_imm, O2,O7], nSub, nItems);
r_strengths = strengths([R1 R_correct],rho, [O_imm, O2,O7], nSub, nItems);
pred=recall(s_strengths,r_strengths,design,k,one_shot_ctrl);


%% Check predictions
Lu=(data.*log(data))+((1-data).*log(1-data));
Lc=(data.*log(pred))+((1-data).*log(1-pred));
err=-sum((2*120*(Lc(1:7)-Lu(1:7))));
if (S1 <=0 || S2 <= 0 || R1 <= 0 || R_correct <= 0 || O2 <= O_imm || O7 <= O_imm || S2 < S1 || R_correct < R1)
    err=1000000;
end

if  any(strcmp(plotting,{'iter','final'}))
    h=figure(1);
    set(h,'Position', [100,50, 1200, 700],'Name','SAM-RL Plots');
    hold off
    if strcmp(plotting,'final') && ~strcmp(varargin{1},'fit');
            plot([0,1,2,7], data([4 1:3]), 'b--', [0,1,2,7], data(4:7),'b');
            hold on
            plot([0,1,2,7], pred([4, 1:3]),'r--', [0,1,2,7], pred(4:7),'r');
            legend('Study (obs)','Test (obs)','Study (SAM)', 'Test (SAM)','Location','NortheastOutside');
            text('position',[.25,min([data pred])+.02], ...
                 'string', char(['\chi^2{ = }' num2str(err)] ,[ 'One Shot: ' one_shot], [rho_txt num2str(rho)]), ...
                 'FontWeight','bold');
    elseif strcmp(plotting,'iter')
%         param_hist=varargin{1};
%         fval_hist=varargin{2};
        subplot(6,6,[1:3 7:9 13:15], 'Position',[.03 .52 .45 .46])  
            plot([0,1,2,7], data([4 1:3]), 'b--', [0,1,2,7], data(4:7),'b');
            hold on
            plot([0,1,2,7], pred([4, 1:3]),'r--', [0,1,2,7], pred(4:7),'r');
            legend('Study (obs)','Test (obs)','Study (SAM)', 'Test (SAM)','Location','Northeast');
            text('position',[.25,min([data pred])+.02], ...
                 'string', char(['\chi^2{ = }' num2str(err)] ,[ 'One Shot: ' one_shot], [rho_txt num2str(rho)]), ...
                 'FontWeight','bold');
%         subplot(6,6,[4:6 10:12 16:18],'Position',[.54 .52 .45 .46])
%             if ~isempty(param_hist)
%             hold on
%             for i=1:(size(param_hist,2)-1)
%                 plot(param_hist(:,end),param_hist(:,i))
%             end
%             hold off
%             end
%         subplot(6,6,(19:36),'Position',[.03 .03 .96 .45])
%             if ~isempty(fval_hist)
%             plot(fval_hist(:,1),fval_hist(:,2));
%             end
    end
end


end


