function  err = fit_SAM_RL_Sim(params,data,design,fix_params,free_params,one_shot,plotting)
%   Detailed explanation goes here
rng(10); % seed the rng with a constant, so results converge.

S1=params(strcmp(free_params(:,1),'S1'));
S2=params(strcmp(free_params(:,1),'S2'));
R1=params(strcmp(free_params(:,1),'R'));
R_correct=params(strcmp(free_params(:,1),'Rcor'));
O2=params(strcmp(free_params(:,1),'O1'));
O7=params(strcmp(free_params(:,1),'O2'));
if strcmp('rho_param',free_params(:,1))
    rho=params(strcmp(free_params(:,1),'rho'));
else
    rho=fix_params{strcmp(fix_params(:,1),'rho'),2};
end
k=fix_params{strcmp(fix_params(:,1),'k'),2};
nSub=fix_params{strcmp(fix_params(:,1),'nSubs'),2};
nItems=fix_params{strcmp(fix_params(:,1),'nItems'),2};
O_imm=1;   

if strcmp(one_shot,'on')
   one_shot=true;
else
   one_shot=false;
end

s_strengths = strengths([S1 S2],rho, [O_imm, O2,O7], nSub, nItems);
r_strengths = strengths([R1 R_correct],rho, [O_imm, O2,O7], nSub, nItems);
pred=recall(s_strengths,r_strengths,design,k,one_shot);


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

if  strcmp(plotting,'iter')
    h=figure(1);
    set(h,'Position', [100, 100, 800, 500]);
    hold off
    plot([0,1,2,7], data([4 1:3]), 'b--', [0,1,2,7], data(4:7),'b');
    hold on
    plot([0,1,2,7], pred([4, 1:3]),'r--', [0,1,2,7], pred(4:7),'r');
    legend('Study (obs)','Test (obs)','Study (SAM)', 'Test (SAM)','Location','NortheastOutside');
    text('position',[.25,min([data pred])+.02],'string', ['\chi^2{ = }' num2str(err)],'FontWeight','bold');
end


end


