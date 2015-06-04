function wrapper(set)
% wrapper for SAMRL fitting
% use to hold onto some good parameter settings to use as starting parameters
%#ok<*NASGU,*ASGLU>
if nargin == 0
    set = 2;
end
cnames  = {'O','R','Rcor','S1','S2','LL'}

switch set 
    case 1
        % Dave's parameters - just check
        [aggchi, aggpars, aggfits] = fitAgg('S1',.15,'S2',.4,'R',3,'R_cor',6,'O1',1.2,'O2',2,'k',10, 'fitting',false);  %#ok<*NASGU,*ASGLU>
        [sub_ll, sub_params, subfits] = fitSub('S1',.15 ,'S2',.4,'R',3,'R_cor',6,'O1',1.2,'k',10, 'fitting',false);     
        [sub_ll_P, sub_params_P, subfits_P] = fitSub('pS1x1',.15/(1+.15),'pS1x2',  .15/(1.2 +.15),'pS2x2',.4/(1.2 +.4),'pR',3/(3+1),'pRcor',6/(6+1), ...
                                                     'free_params',{'pS1x1','pS1x2','pS2x2','pR','pRcor'},'fitting',false);                   
                                                                                      
    case 2
         % Fit dave's parameters
        cnames  = {'O','R','Rcor','S1','S2','LL'};
        [aggchi, aggpars, aggfits] = fitAgg('S1',.15,'S2',.4,'R',3,'R_cor',6,'O1',1.2,'O2',2,'k',10,'p',1, 'showfigs','Off'); 
        [sub_ll, sub_params, subfits] = fitSub('S1',.15,'S2',.4,'R',3,'R_cor',6,'O1',1.2,'k',10,'p',1, 'showfigs', 'Off');    
        [sub_ll_P, sub_params_P, subfits_P] = fitSub('pS1x1',.15/(1 +.15),'pS1x2', .15/(1.2 +.15),'pS2x2', .4/(1.2 +.4),'pR',3/(3+1),'pRcor',6/(6+1), ...
                                                      'free_params',{'pS1x1','pS1x2','pS2x2','pR','pRcor'}, 'showfigs','Off');
        export(subfits,'file','subfits.csv','Delimiter',',')
        export(mat2dataset([sub_params sub_ll], 'VarNames', cnames),'file','sub_params.csv','Delimiter',',')
        export(subfits_P,'file','subfits_P.csv','Delimiter',',')
        export(mat2dataset([sub_params_P sub_ll_P], 'VarNames', cnames),'file','sub_params_P.csv','Delimiter',',')
    case 3
         % Fit dave's parameters without recovery interference        
        cnames  = {'pR','pRcor','pS1x1','pS1x2','pS2x2','LL'};         
        [aggchi, aggpars, aggfits] = fitAgg('S1',.15,'S2',.4,'R',3,'R_cor',6,'O1',1.2,'O2',2,'showfigs','Off','RI',false); 
        [sub_ll_noRI, sub_params_noRI, subfits_noRI] = fitSub('S1',.15,'S2',.4,'R',3,'R_cor',6,'O1',1.2,'showfigs','Off','RI',false);    
        [sub_ll_P_noRI, sub_params_P_noRI, subfits_P_noRI] = fitSub('pS1x1',.15/(1 +.15),'pS1x2', .15/(1.2 +.15),'pS2x2', .4/(1.2 +.4),'pR',3/(3+1),'pRcor',6/(6+1), ...
                                                                    'free_params',{'pS1x1','pS1x2','pS2x2','pR','pRcor'}, 'showfigs','Off','RI',false);
        export(subfits_noRI,'file','subfits_noRI.csv','Delimiter',',')
        export(mat2dataset([sub_params_noRI sub_ll_noRI], 'VarNames', cnames),'file','sub_params_noRI.csv','Delimiter',',')
        export(subfits_noRI,'file','subfits_P_noRI.csv','Delimiter',',')
        export(mat2dataset([sub_params_P_noRI sub_ll_P_noRI], 'VarNames', cnames),'file','sub_params_P_noRI.csv','Delimiter',',')
    case 4
        % Go for it from random
        [chi, pars, data] = SAMRL;
    case 5 % try out things
        [aggchi, aggpars, aggfits, subfits, sub_ll,sub_params] = SAMRL('S1',0.8443,'S2',60.7880,'R',5.7985,'R_cor',6.3558,'O1', 1.2,'O2', 2.3265,'k',10,'p',1, ...
                                                                        'fix_params', {'rho','nSubs','k','p'}, ...
                                                                        'free_params',{'S1','S2','R','R_cor','O1','O2'},...
                                                                        'fitting',true,'showfigs','On','range',37);  %#ok<*NASGU,*ASGLU>        
end
