function  [err, data] = fit_SAM_RL(params,data,fix_params,free_params,one_shot, RI)
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
    O_params = [O_params repmat(O_params(end),1,max(data.timepoint)-length(O_params) - length(unique(data.timepoint(strcmp('all',data.group)))))];
    % Find the rho parameter value
    find_rho=param_list(strcmp(param_list(:,1),'rho'),2);
    rho=cell2mat(find_rho)';
    % Find the nSubs parameter value
    find_nSub=param_list(strcmp(param_list(:,1),'nSubs'),2);
    nSubs=sort(cell2mat(find_nSub))';
    % Find the k parameter value
    find_k=param_list(strcmp(param_list(:,1),'k'),2);
    k=sort(cell2mat(find_k))';
    % Find the nItems parameter value
    find_nItems=param_list(strcmp(param_list(:,1),'nItems'),2);
    nItems=sort(cell2mat(find_nItems))';

    if ( any(S_params < 0)|| any(R_params < 0) || any(O_params <1)|| param_list{strcmp('R',param_list),2} > param_list{strcmp('R_cor',param_list),2} || ...
          param_list{strcmp('S1',param_list),2} > param_list{strcmp('S2',param_list),2}  ||  param_list{strcmp('O1',param_list),2} > param_list{strcmp('O2',param_list),2})
        err=1000000;
    else 
        if RI
            O_ind = 1:3;
        else
            O_ind = 1;
        end        
        data.pred_acc = nan(length(data),1);
        data.S_cor = nan(length(data),1);
        data.S_inc = nan(length(data),1);
        data.R_cor = nan(length(data),1);
        data.R_inc = nan(length(data),1);
        data.O = nan(length(data),1);

        if any(strcmp('rho',free_params(:,1))) || (any(strcmp('rho',fix_params(:,1))) && all(rho) ~= 0)  
            pred=SAMRL_Sim; 
            data.pred_acc(data.timepoint > 1 & data.chain ==1) = pred(1:3);
            data.pred_acc(data.chain ==2) = pred(4:7);        
        elseif (any(strcmp('rho',fix_params(:,1))) && all(rho) == 0 ) ||  isempty(rho)
            SAMRL;     
        end

        %% Check predictions

        obs = data.acc(~isnan(data.acc));
        pred = data.pred_acc(~isnan(data.pred_acc));
        Lu=(obs.*log(obs))+((1-obs).*log(1-obs));
        Lc=(obs.*log(pred))+((1-obs).*log(1-pred));
        err=-sum((2*nSubs*(Lc-Lu)));
    end

        function pred = SAMRL_Sim
            s_strengths = strengths(S_params, rho, O_params, nSubs, nItems);
            r_strengths = strengths(R_params, rho, O_params, nSubs, nItems);
            pred=recall(s_strengths,r_strengths,k,one_shot);
            data.pred_acc(data.timepoint > 1 & data.chain ==1) = pred(1:3);
            data.pred_acc(data.chain ==2) = pred(4:7);
        end

        function  SAMRL
            %Studied Items
            prac = (1-((1-(S_params(1)./( (S_params(1)*30)+ O_params(1) ))) .^k)).*(R_params(1)./(R_params(1)+O_params(1)));
            study_acc=(1-((1-(S_params(2)./( (S_params(2)*30)+ O_params ))) .^k)).*(R_params(1)./(R_params(1)+O_params(O_ind)));
            %Tested Items
            p_sample_test_correct = S_params(2)./( (S_params(2)*prac*nItems) + (S_params(1)*(1-prac)*nItems) + O_params );                            
            p_sample_test_incorrect = S_params(1)./( ( S_params(2)*prac*nItems) + (S_params(1)*(1-prac)*nItems) + O_params );
            p_recall_test_correct = (1-((1-p_sample_test_correct).^k)).*(R_params(2)./(R_params(2)+O_params(O_ind)));
            p_recall_test_incorrect_imm=(1-((1-p_sample_test_incorrect)).^k).*(R_params(1)./(R_params(1)+O_params(O_ind)));
            scaler = repmat((1-prac),1,3);
            scaler(one_shot) = (1-( 1-((1-(S_params(1)/( (S_params(1)*30)+ O_params(1) ))) .^k)) );
            test_acc=(p_recall_test_correct*prac) + (p_recall_test_incorrect_imm.*scaler);
            data.pred_acc(data.chain==1 & strcmp('T',data.method)) = study_acc;
            data.pred_acc (data.chain==2 & strcmp('T',data.method)) = [prac test_acc];       
        end



end


