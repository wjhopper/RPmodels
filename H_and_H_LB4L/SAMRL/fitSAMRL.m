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
        data.pred_acc_plus = nan(length(data),1);
        data.pred_acc_neg = nan(length(data),1);
%         data.S_cor = nan(length(data),1);
%         data.S_inc = nan(length(data),1);
%         data.R_cor = nan(length(data),1);
%         data.R_inc = nan(length(data),1);
%         data.O = nan(length(data),1);

        if any(strcmp('rho',free_params(:,1))) || any(strcmp('rho',fix_params(:,1))) && rho ~= 0 %#ok<BDSCI>
%             pred=SAMRL_Sim; 
        elseif (any(strcmp('rho',fix_params(:,1))) && rho == 0 )|| isempty(rho)%#ok<BDSCI>
            SAMRL;
%              SAMRL_DEH;
        end

    %% Check predictions
        if any(strcmp('subject',get(data, 'VarNames')))
            obs = [data.acc(~isnan(data.acc) & data.timepoint == 1) ; data.acc(ismember(data.chain, [2,3]) & data.timepoint > 1); ...
                    data.acc_plus(ismember(data.chain, [1 5]) & data.timepoint > 1); data.acc_neg(ismember(data.chain, [1 5]) & data.timepoint > 1)];
            pred = [data.pred_acc(~isnan(data.acc) & data.timepoint == 1) ; data.pred_acc(ismember(data.chain, [2,3]) & data.timepoint > 1); ...
                    data.pred_acc_plus(ismember(data.chain, [1 5]) & data.timepoint > 1); data.pred_acc_neg(ismember(data.chain, [1 5]) & data.timepoint > 1)];        
        else 
            obs = data.acc(~isnan(data.acc));
            pred = data.pred_acc(~isnan(data.acc));        
        end
        Lu=(obs.*log(obs))+((1-obs).*log(1-obs));
        Lc=(obs.*log(pred))+((1-obs).*log(1-pred));
        err=-sum((2*nSubs*(Lc(1:end)-Lu(1:end))));        
    end

%     function pred = SAMRL_Sim
%         s_strengths = strengths(S_params, rho, O_params, nSubs, nItems);
%         r_strengths = strengths(R_params, rho, O_params, nSubs, nItems);
%         pred=recall(s_strengths,r_strengths,design,k,one_shot_ctrl);
%     end

    function SAMRL
        s_chain = unique(data.chain(strcmp('S',data.practice) & data.timepoint ==1));
        t_chain = unique(data.chain(strcmp('T',data.practice) & strcmp('',data.other_type) & ~isnan(data.acc) & data.timepoint ==1));
        c_chain = unique(data.chain(strcmp('C',data.practice) & strcmp('',data.other_type) & data.timepoint ==1));
        ot_chain = unique(data.chain(strcmp('T',data.other_type) & data.timepoint ==1));
        
        % Predict practice test accuracy
        prac = p .* sample(S_params(1), O_params(1), k) .* recover(R_params(1),O_params(1)); 
        data.pred_acc(data.timepoint == 1 & ~isnan(data.acc)) = prac;
        
        % predict restudy accuracy
        restudy = sum([ p^2 .* sample(S_params(2), O_params(2:3), k) .* recover(R_params(1),O_params(2:3)) ; ...
                      (2*p*(1-p)) .*  sample(S_params(1), O_params(2:3), k) .* recover(R_params(1),O_params(2:3))]);
        data.pred_acc(data.timepoint > 1 & data.chain == s_chain) = restudy(data.timepoint(data.timepoint > 1 & data.chain == s_chain) -1);       
                  
        % predict no practice accuracy
        control = p .* sample(S_params(1), O_params(2:3), k) .* recover(R_params(1),O_params(2:3));
        data.pred_acc(data.timepoint > 1 & data.chain == c_chain) = control(data.timepoint(data.timepoint > 1 & data.chain == c_chain) -1);
        
        % predict test practice same cue accuracy
        weight = data.pred_acc(data.chain==t_chain & data.timepoint ==1);
        weight = [weight 1-weight]';
        clms = data.timepoint(data.timepoint > 1 & data.chain == t_chain) -1;
        if one_shot > 1
            penalty = p * ((1-  sample(S_params(1), O_params(1), k))/(weight(2,1)));
        end
        same_conditionals = [sample(S_params(2), O_params(2:3), k) .* recover(R_params(2),O_params(2:3)); ...
                             [penalty 1].*sample(S_params(1), O_params(2:3), k) .* recover(R_params(1),O_params(2:3))];
        data.pred_acc_plus(data.chain==5 & data.timepoint > 1) = same_conditionals(1,clms);
        data.pred_acc_neg(data.chain==5 & data.timepoint > 1) = same_conditionals(2, clms);                            
        test_same = sum(weight .* same_conditionals(:,clms));
        data.pred_acc(data.timepoint > 1 & data.chain == t_chain) = test_same;
        
        % predict test practice using other cue accuracy
        weight = data.pred_acc(strcmp('T',data.practice) & strcmp('C',data.other_type) & data.timepoint ==1);
        weight = [weight 1-weight]';
        clms = data.timepoint(data.timepoint > 1 & data.chain == ot_chain) -1;
        new_conditionals = [sample(S_params(1), O_params(2:3), k) .* recover(R_params(2),O_params(2:3)); ...
                            sample(S_params(1), O_params(2:3), k) .* recover(R_params(1),O_params(2:3))];
        data.pred_acc_plus(data.chain==1 & data.timepoint > 1) = new_conditionals(1,clms);
        data.pred_acc_neg(data.chain==1 & data.timepoint > 1) = new_conditionals(2,clms);        
        test_diff = sum(weight .* new_conditionals(:,clms));
        data.pred_acc(data.timepoint > 1 & data.chain == ot_chain) = test_diff;
        
    end

    function psampling = sample(S,O,k)
        psampling = 1-((1-(S./(S+O))).^k);
    end

    function precovery = recover(R,O)
        precovery = R./(R+O);
    end
        
    function SAMRL_DEH %#ok<DEFNU>

        k=10;

        O1=1.2; 
        O2=2;    
        S1=.15;  
        S2=.4;   
        R1=3;    
        R2=6;   

        figure(1);

        % test practice
        T=samp(S1,1)*recov(R1,1);
        subplot(2,1,1);
        hold off
        plot(.8,T,'+k');
        hold on
        plot(.8,.55,'og');
        plot(.8,.57,'og');
        plot(.8,.52,'og');
        plot(.8,.52,'og');

        % baseline
        base(1)=samp(S1,O1)*recov(R1,O1);
        base(2)=samp(S1,O2)*recov(R1,O2);
        plot(base,'-k');
        plot([.44 .28],'ok');

        % restudy
        restudy(1)=samp(S2,O1) * recov(R1,O1);
        restudy(2)=samp(S2,O2) * recov(R1,O2);
        plot(restudy,'-b');
        plot([.81 .53],'ob');

        % same cue test practice
        same_test_cor(1)=samp(S2,O1)*recov(R2,O1); % conditioned on correct test practice
        same_test_inc(1)=((1-samp(S1,1))/(1-T))*samp(S1,O1)*recov(R1,O1); % conditioned on incorect test practice. Calculate proportion of incorrect items that were never sampled
        same_test(1)=T*same_test_cor(1)+(1-T)*same_test_inc(1);

        same_test_cor(2)=samp(S2,O2)*recov(R2,O2); % conditioned on correct test practice
        same_test_inc(2)=samp(S1,O2)*recov(R1,O2); % conditioned on incorrect test practice
        same_test(2)=T*same_test_cor(2)+(1-T)*same_test_inc(2);
        plot(same_test,'-r');
        plot([.56 .46],'or');


        % other cue test practice
        other_test_cor(1)=samp(S1,O1)*recov(R2,O1); % conditioned on correct test practice
        other_test_inc(1)=samp(S1,O1)*recov(R1,O1); % conditioned on incorrect test practice
        other_test(1)=T*other_test_cor(1)+(1-T)*other_test_inc(1);

        other_test_cor(2)=samp(S1,O2)*recov(R2,O2); % conditioned on correct test practice
        other_test_inc(2)=samp(S1,O2)*recov(R1,O2); % conditioned on incorrect test practice
        other_test(2)=T*other_test_cor(2)+(1-T)*other_test_inc(2);
        plot(other_test,'--r');
        plot([.55 .32],'or');


        axis([.5 2.25 0 1]);

        % plot conditionals for same cue and other cue conditions

        subplot(2,1,2);
        hold off
        plot(same_test_cor,'-r');
        hold on
        plot(other_test_cor,'--r');
        plot(same_test_inc,'-k');
        plot(other_test_inc,'--k');


        axis([.5 2.25 0 1]);

            function prob=samp(S,O)
                prob=(1-(1-(S/(S+O)))^k);
            end

            function prob=recov(R,O)
                prob=R/(R+O);
            end
    end




end


