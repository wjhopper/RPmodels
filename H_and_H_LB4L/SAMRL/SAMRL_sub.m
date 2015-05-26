function [  err, data ] = SAMRL_sub(params,data,fix_params,free_params,one_shot, RI)
 
    utils = getUtils;
    [S_params, R_params, O_params, k, p, rho] = utils.findNamedPars(params, fix_params, free_params);
    
    if all(params) <=1 && all(params) >=0 
        [S_params, R_params, O_params] = utils.transform(S_params(1), S_params(2), S_params(3), R_params(1), R_params(2));
    end
    if ( any(S_params <= 0)|| any(R_params <= 0) || any(diff(R_params) < 0) || any(diff(S_params) < 0) || any(diff(O_params) < 0) || (p > 1 || p <=0))
        err=1000000;
    else 
        if RI
            O_ind = 2;
        else
            O_ind = 1;
        end        
        [ s_chain, c_chain, t_chain, ot_chain] = utils.defChains(data);
        if any(strcmp('rho',free_params(:,1))) || any(strcmp('rho',fix_params(:,1))) && rho ~= 0 
            
        else
            % Predict practice test accuracy
            prac = p .* sample(S_params(1), O_params(1), k) .* recover(R_params(1),O_params(1)); 

            % predict restudy accuracy
            restudy = sum([ p^2 .* sample(S_params(2), O_params(2), k) .* recover(R_params(1),O_params(O_ind)) ; ...
                          (2*p*(1-p)) .*  sample(S_params(1), O_params(2), k) .* recover(R_params(1),O_params(O_ind))]);

            % predict no practice accuracy

            control = p .* sample(S_params(1), O_params(2:end), k) .* recover(R_params(1),O_params(O_ind));
           

            % predict test practice same cue accuracy
            weight = [prac 1-prac]';
            if one_shot > 1 && all(data.timepoint(data.timepoint >1)) <= one_shot
                penalty = p * (1-  sample(S_params(1), O_params(1), k))/weight(2,1);
            else
                penalty = 1; % full strengths
            end
            same_conditionals = [sample(S_params(2), O_params(2), k) .* recover(R_params(2),O_params(O_ind)); ...
                                 penalty .*sample(S_params(1), O_params(2), k) .* recover(R_params(1),O_params(O_ind))];
            test_same = sum(weight .* same_conditionals);

            % predict test practice using other cue accuracy
            new_conditionals = [sample(S_params(1), O_params(2), k) .* recover(R_params(2),O_params(O_ind)); ...
                                sample(S_params(1), O_params(2), k) .* recover(R_params(1),O_params(O_ind))];    
            test_diff = sum(weight .* new_conditionals);

            data.pred_acc(data.timepoint == 1 & ~isnan(data.acc)) = prac; 
            data.pred_acc(data.timepoint > 1 & data.chain == c_chain) = control;        
            data.pred_acc(data.timepoint > 1 & data.chain == s_chain) = restudy;  
            data.pred_acc(data.timepoint > 1 & data.chain == t_chain) = test_same;
            data.pred_acc(data.timepoint > 1 & data.chain == ot_chain) = test_diff;
            data.pred_acc_plus(data.timepoint > 1 & data.chain==t_chain) = same_conditionals(1,1);
            data.pred_acc_neg(data.timepoint > 1 & data.chain==t_chain) = same_conditionals(2,1); 
            data.pred_acc_plus(data.timepoint > 1 & data.chain==ot_chain) = new_conditionals(1,1);
            data.pred_acc_neg(data.timepoint > 1 & data.chain==ot_chain) = new_conditionals(2,1);     

            obs = [data.acc(~isnan(data.acc) & data.timepoint == 1) ; data.acc(ismember(data.chain, [2,3]) & data.timepoint > 1); ...
                   data.acc_plus(ismember(data.chain, [1 5]) & data.timepoint > 1); data.acc_neg(ismember(data.chain, [1 5]) & data.timepoint > 1)];
            pred = [[ prac; prac] ; control; restudy ; new_conditionals(1,1) ; same_conditionals(1,1) ;  new_conditionals(2,1) ; same_conditionals(2,1)]; 
            pred(isnan(obs)) = [];        
            obs(isnan(obs)) = [];            
            err=-sum((obs.*log(pred))+((1-obs).*log(1-pred)));              
        end
    end
end


function psampling = sample(S,O,k)
    psampling = 1-((1-(S./(S+O))).^k);
end

function precovery = recover(R,O)
    precovery = R./(R+O);
end

