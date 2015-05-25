function wrapper(set)
% wrapper for SAMRL fitting
% use to hold onto some good parameter settings to use as starting parameters
%#ok<*NASGU,*ASGLU>
if nargin == 0
    set = 2;
end
switch set 
    case 1
        % Dave's parameters 
        [aggchi, aggpars, aggfits, subfits, sub_ll,sub_params] = SAMRL('S1',.15,'S2',.4,'R',3,'R_cor',6,'O1',1.2,'O2',2,'k',10,'p',1, ...
                                                                        'fix_params', {'rho','nSubs','k','p'}, ...
                                                                        'free_params',{'S1','S2','R','R_cor','O1','O2'},...
                                                                        'fitting',false);  %#ok<*NASGU,*ASGLU>
    case 2
         % Fit dave's parameters                      
        [aggchi, aggpars, aggfits, subfits, sub_ll,sub_params] = SAMRL('S1',.15,'S2',.4,'R',3,'R_cor',6,'O1',1.2,'O2',2,'k',10,'p',1, 'fix_params', {'rho','nSubs','k','p'},'free_params',{'S1','S2','R','R_cor','O1','O2'},'showfigs','Off','range', 37); 
    case 3
        % Go for it from random
        [chi, pars, data] = SAMRL; 
    case 4 % try out things
        [aggchi, aggpars, aggfits, subfits, sub_ll,sub_params] = SAMRL('S1',0.8443,'S2',60.7880,'R',5.7985,'R_cor',6.3558,'O1', 1.2,'O2', 2.3265,'k',10,'p',1, ...
                                                                        'fix_params', {'rho','nSubs','k','p'}, ...
                                                                        'free_params',{'S1','S2','R','R_cor','O1','O2'},...
                                                                        'fitting',true,'showfigs','On','range',37);  %#ok<*NASGU,*ASGLU>        
end
