% wrapper for SAMRL fitting

% These parameters get you in the ballpark
[chisquare, best_params, data] = SAMRL('S1',0.0054, 'S2', 0.0234, 'R',   4.4833, 'R_cor', 4.4833, 'O1', 1.1092, ...
                                       'O2',   2.0788, 'p',  0.8807,'k' , 250, 'one_shot', 2, 'fitting',true);
[chisquare, best_params, data] = SAMRL('S1', 0.1, 'S2', 2, 'R',   2.9714, 'R_cor', 4.3165, 'O1', 1.0801, ...
                                       'O2',   2.0788, 'p',  .5,'k' , 250, 'one_shot', 2, 'fitting',true);                                   
[chisquare, best_params, data] = SAMRL;
% Dave's parameters 
[chi, pars, data] = SAMRL('S1',.15,'S2',.4,'R',3,'R_cor',6,'O1',1.2,'O2',2,'k',10,'p',1, ...
                          'fix_params', {'rho','nSubs','k','p'},'free_params',{'S1','S2','R','R_cor','O1','O2'},'fitting',false)
[psampling, precovery, acc] = predict(.05, 5,1,100,.75) % good prac acc parameters
[psampling, precovery, acc] = predict([.05 .2], [5 5], 2.5, 100,[2*(.75*(1-.75)) .75^2])