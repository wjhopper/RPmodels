function  [err, data] = fitSAMRL(params,data,fix_params,free_params,one_shot)
%   Detailed explanation goes here

    rng(10); % seed the rng with a constant, so results converge.
    % change values in free_params table with the updated ones from fminseach, and
    % cat them together with the fix_params
    param_list=[[free_params num2cell(params')]; fix_params(:,1:2)];
    % Find all the S parameter values, and put them in a low-high sorted vector
    find_S=param_list(~cellfun(@isempty,regexp(param_list(:,1),'S\d')),2);
    S_params=cell2mat(find_S)';
    % Find all the R parameter values, and put them in a low-high sorted vector
    find_R=param_list(~cellfun(@isempty,regexp(param_list(:,1),'R\w*')),2);
    R_params=cell2mat(find_R)';
    % Find all the O parameter values, and put them in a low-high sorted vector
    find_O=param_list(~cellfun(@isempty,regexp(param_list(:,1),'O\d')),2);
    O_imm=1; 
    O_params=[O_imm cell2mat(find_O)'];
    O_params = [O_params repmat(O_params(end),1,max(data.timepoint)-length(O_params))];
    % Find the nItems parameter value
    % find_nItems=param_list(strcmp(param_list(:,1),'nItems'),2);
    % nItems=sort(cell2mat(find_nItems))';
    % Find the rho parameter value
    find_rho=param_list(strcmp(param_list(:,1),'rho'),2);
    rho=sort(cell2mat(find_rho))';
    % Find the nSubs parameter value
    find_nSub=param_list(strcmp(param_list(:,1),'nSubs'),2);
    nSubs=sort(cell2mat(find_nSub))';
    % end
    % Find the k parameter value
    find_k=param_list(strcmp(param_list(:,1),'k'),2);
    k=sort(cell2mat(find_k))';
    % Find the p parameter value
    find_p=param_list(strcmp(param_list(:,1),'p'),2);
    p=sort(cell2mat(find_p))';

    if ( any(S_params < 0)|| any(R_params < 0) || param_list{strcmp('R',param_list),2} > param_list{strcmp('R_cor',param_list),2} || ...
          param_list{strcmp('S1',param_list),2} > param_list{strcmp('S2',param_list),2}  ||  param_list{strcmp('O1',param_list),2} > param_list{strcmp('O2',param_list),2} || p > 1 || ...
          any(O_params(2:end) < 1))
        err=1000000;
    else 
        data.pred_acc = nan(length(data),1);
        data.pred_cond_plus = nan(length(data),1);
        data.pred_cond_neg = nan(length(data),1);
%         data.S_cor = nan(length(data),1);
%         data.S_inc = nan(length(data),1);
%         data.R_cor = nan(length(data),1);
%         data.R_inc = nan(length(data),1);
%         data.O = nan(length(data),1);

        if any(strcmp('rho',free_params(:,1))) || any(strcmp('rho',fix_params(:,1))) && rho ~= 0 %#ok<BDSCI>
            pred=SAMRL_Sim; 
        elseif (any(strcmp('rho',fix_params(:,1))) && rho == 0 )|| isempty(rho)%#ok<BDSCI>
             SAMRLfast;  
        end

    %% Check predictions
        % obs = [ data.acc(~isnan(data.acc)) data.cond_plus(~isnan( data.cond_plus)) data.cond_neg(~isnan( data.cond_neg))];
        % pred = [ data.pred_acc(~isnan(data.pred_acc)) data.pred_cond_plus(~isnan(data.data.pred_cond_plus)) ...
        %          data.pred_cond_neg(~isnan(data.data.pred_cond_neg))];
        obs = data.acc(~isnan(data.acc));
        pred =  data.pred_acc(~isnan(data.acc));
        Lu=(obs.*log(obs))+((1-obs).*log(1-obs));
        Lc=(obs.*log(pred))+((1-obs).*log(1-pred));
        err=-sum((2*nSubs*(Lc(1:end)-Lu(1:end))));        
    end

%     function pred = SAMRL_Sim
%         s_strengths = strengths(S_params, rho, O_params, nSubs, nItems);
%         r_strengths = strengths(R_params, rho, O_params, nSubs, nItems);
%         pred=recall(s_strengths,r_strengths,design,k,one_shot_ctrl);
%     end

    function SAMRLfast
        s_chain = unique(data.chain(strcmp('S',data.method) & data.timepoint ==1));
        t_chain = unique(data.chain(strcmp('T',data.method) & strcmp('',data.other_type) & ~isnan(data.acc) & data.timepoint ==1));
        c_chain = unique(data.chain(strcmp('C',data.method) & strcmp('',data.other_type) & data.timepoint ==1));
        ot_chain = unique(data.chain(strcmp('T',data.other_type) & data.timepoint ==1));
        % Predict practice test accuracy
        prac = p .* sample(S_params(1), O_params(1), k) .* recover(R_params(1),O_params(1)); 
        data.pred_acc(data.timepoint == 1 & ~isnan(data.acc)) = prac;
        
        % predict restudy accuracy
        restudy = sum([ p^2 .* sample(S_params(2), O_params(2:3), k) .* recover(R_params(1),O_params(2:3)) ; ...
                      (2*p*(1-p)) .*  sample(S_params(1), O_params(2:3), k) .* recover(R_params(1),O_params(2:3))]);
        data.pred_acc(data.timepoint > 1 & data.chain == s_chain) = restudy;       
                  
        % predict no practice accuracy
        control = p .* sample(S_params(1), O_params(2:3), k) .* recover(R_params(1),O_params(2:3));
        data.pred_acc(data.timepoint > 1 & data.chain == c_chain) = control;
        
        % predict test practice same cue accuracy
        weight = data.pred_acc(data.chain==t_chain & data.timepoint ==1);
        weight = [weight 1-weight]';
        if one_shot > 1
            weight(2,1) = p * (1-  sample(S_params(1), O_params(1), k));
        end
        test_same = sum(weight .* [sample(S_params(2), O_params(2:3), k) .* recover(R_params(2),O_params(2:3)); ...
                                   sample(S_params(1), O_params(2:3), k) .* recover(R_params(1),O_params(2:3))]);
        data.pred_acc(data.timepoint > 1 & data.chain == t_chain) = test_same;
        
        % predict test practice using other cue accuracy
        weight = data.pred_acc(strcmp('T',data.method) & strcmp('C',data.other_type) & data.timepoint ==1);
        weight = [weight 1-weight]';
        test_diff = sum(weight .* [sample(S_params(1), O_params(2:3), k) .* recover(R_params(2),O_params(2:3)); ...
                                   sample(S_params(1), O_params(2:3), k) .* recover(R_params(1),O_params(2:3))]);
        data.pred_acc(data.timepoint > 1 & data.chain == ot_chain) = test_diff;
        
    end
        
    function   SAMRL
        data.pred_acc(data.timepoint == 1) = p .* sample(S_params(1), O_params(1), k) .* recover(R_params(1),O_params(1)); 
        for i = unique(data.group)'
            group_tps = unique(data.timepoint(strcmp(i,data.group) )); %#ok<*CCAT>
            for j=(group_tps)'
                un_tp = find(unique(data.timepoint(strcmp(i,data.group)))==j, 1);
                datarows = find(data.timepoint==j & strcmp(data.group,i)); %#ok<CCAT>
                O = O_params(j);

                if  un_tp == min(data.timepoint)
                    % if this is the minimmum time point, assume everything equal, only study up to this point 
                    S = repmat(S_params(1) , length(datarows),2/size(S_params(1),2));
                    R = repmat(R_params(1), length(datarows), 2/size(R_params(1),2));                     
                    weights = repmat([p 0 0 0 ], length(datarows),1);  

                else  % Bookeeping on conditions, performance, etc           
                 % Get all perfomance history for this groups items
                    grp_hist = data(ismember(data.group,{'all',i{:}}),:);
                 % Find this groups last time point before this round
                    last_tp = max(grp_hist.timepoint(grp_hist.timepoint ~= max(grp_hist.timepoint)));
                 % keep a pointer to the rows for the most recent time points around 
                    oldrows = find(data.timepoint== last_tp & ismember(data.group,{'all',i{:}}));
                 % Get parameter strengths 
                    S =  double(data(oldrows, {'S_cor','S_inc'}));
                    R =  double(data(oldrows, {'R_cor','R_inc'}));               
                 % Find the positions of the different item conditions
                    trows = strcmp('T',data.method(oldrows));  
                    srows = strcmp('S',data.method(oldrows));
                    crows = strcmp('C',data.method(oldrows)) & ismember(data.other_type(oldrows),{'S','C',''});
                    ot_trows = strcmp('C',data.method(oldrows)) & strcmp('T',data.other_type(oldrows));   

                    % Calculate what weights to put on the combinations
                    % of sampling/recovery strength probabilities 
                    weights = zeros(length(datarows),4);
                    weights(trows,1:2) = [data.pred_acc(oldrows(trows)) (1-data.pred_acc(oldrows(trows)))]; 
                    weights(srows,[2,4]) = [ 2*(p*(1-p)) p^2 ]; 
                    weights(crows,2) = p; 
                    weights(ot_trows,2:3) =  [(1-data.pred_acc(oldrows(strcmp('T',data.method(oldrows)) & strcmp('C',data.other_type(oldrows))))) ...
                                              data.pred_acc(oldrows(strcmp('T',data.method(oldrows)) & strcmp('C',data.other_type(oldrows))))];
                end
                % Actually making preds
                sampling  = (S./(S + O)); 
                psampling = 1-((1-sampling).^k);
                recovery = R./(R + O);

                % [ (Shigh * Rhigh) (Slow * Rlow) (Slow * Rhigh) (Shigh * Rlow) ]  
                preds = [psampling.*recovery fliplr(psampling).*recovery]; 

                if j <= one_shot && j > min(data.timepoint)
                    % penalty is the probability of NOT sampling
                    % aka the percentage of misses that occur in the recovery stage during practice test is the limiting factor on recall
                    psampling = S_params(1)/(S_params(1) + O_params(1));
                    penalty = (1-psampling).^k;                  
                    weights(((trows | ot_trows) & data.cues(datarows)==1),2) = p*penalty; %p*((1-psampling).^k);
                end     

                data.pred_acc(datarows) = sum(preds .* weights, 2);

                if j > min(data.timepoint)
                    data(datarows(ot_trows & ~isnan(data.acc(datarows))),{'pred_cond_neg','pred_cond_plus'}) = mat2dataset(preds(ot_trows,2:3).*[p 1]);
                    data(datarows(trows & ~isnan(data.acc(datarows))),{'pred_cond_plus','pred_cond_neg'}) = ...
                        mat2dataset(preds(trows(~isnan(data.acc(datarows))),1:2).*[1 p]);
                end

                if  un_tp < length(S_params) % find(group_tps ==j)
                    data(datarows,{'S_inc','S_cor'}) = mat2dataset(repmat(S_params(un_tp:un_tp+1),length(datarows),1));   
                end

                if un_tp  < length(R_params)
                    data(datarows, {'R_inc','R_cor'}) = mat2dataset(repmat(R_params(un_tp:un_tp+1),length(datarows),1));                        
                end
                data.O(datarows) = O;
            end
        end          
    end

    function psampling = sample(S,O,k)
        psampling = 1-((1-(S./(S+O))).^k);
    end

    function precovery = recover(R,O)
        precovery = R./(R+O);
    end

end


