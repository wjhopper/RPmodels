function [ err, data] = SAMRL_agg(params,data,fix_params,free_params,one_shot,RI)

    utils = getUtils;
    [S_params, R_params, O_params, k, p, rho, nSubs] = utils.findNamedPars(params, fix_params, free_params);
    
    if ( any(S_params <= 0)|| any(R_params <= 0) || any(diff(R_params) < 0) || any(diff(S_params) < 0) || any(diff(O_params) < 0) || (p > 1 || p <=0))
        err=1000000;
    else 
        if RI
            O_ind = 2:3;
        else
            O_ind = 1;
        end
        [ s_chain, c_chain, t_chain, ot_chain] = utils.defChains(data);
        if any(strcmp('rho',free_params(:,1))) || any(strcmp('rho',fix_params(:,1))) && rho ~= 0 
%         s_strengths = strengths(S_params, rho, O_params, nSubs, nItems);
%         r_strengths = strengths(R_params, rho, O_params, nSubs, nItems);
%         pred=recall(s_strengths,r_strengths,design,k,one_shot_ctrl);
        else
        % Predict practice test accuracy
            prac = p .* sample(S_params(1), O_params(1), k) .* recover(R_params(1),O_params(1)); 
            data.pred_acc(data.timepoint == 1 & ~isnan(data.acc)) = prac;
            
            % predict restudy accuracy            
            restudy = sum([ p^2 .* sample(S_params(2), O_params(2:end), k) .* recover(R_params(1),O_params(O_ind)) ; ...
                              (2*p*(1-p)) .*  sample(S_params(1), O_params(2:end), k) .* recover(R_params(1),O_params(O_ind))]);
            data.pred_acc(data.timepoint > 1 & data.chain == s_chain) = restudy(data.timepoint(data.timepoint > 1 & data.chain == s_chain) -1);       

            % predict no practice accuracy 
            control = p .* sample(S_params(1), O_params(2:end), k) .* recover(R_params(1),O_params(O_ind));
            data.pred_acc(data.timepoint > 1 & data.chain == c_chain) = control(data.timepoint(data.timepoint > 1 & data.chain == c_chain) -1);

            % predict test practice same cue accuracy
            weight = data.pred_acc(data.chain==t_chain & data.timepoint ==1);
            weight = [weight 1-weight]';
            clms = data.timepoint(data.timepoint > 1 & data.chain == t_chain) -1;
            if one_shot > 1 && all(data.timepoint(data.timepoint >1)) <= one_shot
                penalty = p * (1-  sample(S_params(1), O_params(1), k))/weight(2,1);
            else
                penalty = 1; % full strengths
            end
            same_conditionals = [sample(S_params(2), O_params(2:end), k) .* recover(R_params(2),O_params(O_ind)); ...
                                [penalty ones(length(O_params(2:end))-1)].*sample(S_params(1), O_params(2:end), k) .* recover(R_params(1),O_params(O_ind))];
            data.pred_acc_plus(data.chain==5 & data.timepoint > 1) = same_conditionals(1,clms);
            data.pred_acc_neg(data.chain==5 & data.timepoint > 1) = same_conditionals(2, clms);                            
            test_same = sum(weight .* same_conditionals(:,clms));
            data.pred_acc(data.timepoint > 1 & data.chain == t_chain) = test_same;

            % predict test practice using other cue accuracy
            weight = data.pred_acc(strcmp('T',data.practice) & strcmp('C',data.other_type) & data.timepoint ==1);
            weight = [weight 1-weight]';
            clms = data.timepoint(data.timepoint > 1 & data.chain == ot_chain) -1;
            new_conditionals = [sample(S_params(1), O_params(2:end), k) .* recover(R_params(2),O_params(O_ind)); ...
                                sample(S_params(1), O_params(2:end), k) .* recover(R_params(1),O_params(O_ind))];
            data.pred_acc_plus(data.chain==1 & data.timepoint > 1) = new_conditionals(1,clms);
            data.pred_acc_neg(data.chain==1 & data.timepoint > 1) = new_conditionals(2,clms);        
            test_diff = sum(weight .* new_conditionals(:,clms));
            data.pred_acc(data.timepoint > 1 & data.chain == ot_chain) = test_diff;

            obs = [data.acc(~isnan(data.acc) & data.timepoint == 1) ; data.pred_acc(data.timepoint > 1 & data.chain == c_chain); ...
                   data.acc(data.timepoint > 1 & data.chain == s_chain);  data.acc(data.timepoint > 1 & data.chain == t_chain); ...
                   data.acc(data.timepoint > 1 & data.chain == ot_chain); ...
                   data.acc_plus(data.chain==5 & data.timepoint > 1); data.acc_plus(data.chain==1 & data.timepoint > 1);... 
                   data.acc_neg(data.chain==5 & data.timepoint > 1); data.acc_neg(data.chain==1 & data.timepoint > 1) ];
            pred = [data.pred_acc(~isnan(data.acc) & data.timepoint == 1) ; data.pred_acc(data.timepoint > 1 & data.chain == c_chain); ...
                   data.pred_acc(data.timepoint > 1 & data.chain == s_chain);  data.pred_acc(data.timepoint > 1 & data.chain == t_chain); ...
                   data.pred_acc(data.timepoint > 1 & data.chain == ot_chain); ...
                   data.pred_acc_plus(data.chain==5 & data.timepoint > 1); data.pred_acc_neg(data.chain==5 & data.timepoint > 1);... 
                   data.pred_acc_plus(data.chain==1 & data.timepoint > 1); data.pred_acc_neg(data.chain==1 & data.timepoint > 1) ];  
               
            Lu=(obs.*log(obs))+((1-obs).*log(1-obs));
            Lc=(obs.*log(pred))+((1-obs).*log(1-pred));
            err=-sum(2*nSubs*(Lc(1:(end-4))-Lu(1:(end-4))));    
        end
    end
end

function psampling = sample(S,O,k)
    psampling = 1-((1-(S./(S+O))).^k);
end

function precovery = recover(R,O)
    precovery = R./(R+O);
end

