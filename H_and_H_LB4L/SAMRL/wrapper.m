function wrapper(set)
% wrapper for SAMRL fitting
% use to hold onto some good parameter settings to use as starting parameters
%#ok<*NASGU,*ASGLU>
if nargin == 0
    set = 2;
end
switch set 
    case 1
        % Dave's parameters - just check
        [aggchi, aggpars, aggfits] = fitAgg('S1',.15,'S2',.4,'R',3,'R_cor',6,'O1',1.2,'O2',2,'k',10,'p',1, ...
                                            'fitting',false);  %#ok<*NASGU,*ASGLU>
        [sub_ll, sub_params, subfits] = fitSub('S1',.15 ,'S2',.4,'R',3,'R_cor',6,'O1',1.2,'k',10,'p',1, ...
                                               'fitting',false);     
        [sub_ll_noRI, sub_params_noRI, subfits_noRI] = fitSub_noRI('S1x1',.15/(1+.15),'S1x2', 0.1111,'S2x2',0.25,'R',.75,'R_cor',0.8571,'k',10,'p',1, ...
                                                                    'fitting',false);                   
                                                                                      
    case 2
         % Fit dave's parameters                      
        [aggchi, aggpars, aggfits] = fitAgg('S1',.15,'S2',.4,'R',3,'R_cor',6,'O1',1.2,'O2',2,'k',10,'p',1, ...
                                            'showfigs','Off'); 
        [sub_ll, sub_params, subfits] = fitSub('S1',.15,'S2',.4,'R',3,'R_cor',6,'O1',1.2,'k',10,'p',1, ...
                                               'showfigs','Off');    
        [sub_ll_noRI, sub_params_noRI, subfits_noRI] = fitSub_noRI('S1x1',0.1304,'S1x2', .15/(1.2 +.15),'S2x2', .4/(1.2 +.4),'R',(3/3+1),'R_cor',(6/6+1),'k',10,'p',1, ...
                                                                   'showfigs','Off');                                                   
    case 3
        % Go for it from random
        [chi, pars, data] = SAMRL;
    case 4 % try out things
        [aggchi, aggpars, aggfits, subfits, sub_ll,sub_params] = SAMRL('S1',0.8443,'S2',60.7880,'R',5.7985,'R_cor',6.3558,'O1', 1.2,'O2', 2.3265,'k',10,'p',1, ...
                                                                        'fix_params', {'rho','nSubs','k','p'}, ...
                                                                        'free_params',{'S1','S2','R','R_cor','O1','O2'},...
                                                                        'fitting',true,'showfigs','On','range',37);  %#ok<*NASGU,*ASGLU>        
end
