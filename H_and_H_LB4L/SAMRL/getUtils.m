function [utils] = getUtils

    utils.cleanAndAdd = @cleanAndAdd;
    utils.parse_params = @parse_params;
    utils.drawResults = @drawResults;
    utils.findNamedPars = @findNamedPars;
    utils.defChains = @defChains;    
    utils.transform = @transform;
end

function [S_params, R_params, O_params, k, p, rho, nSubs] = findNamedPars(params, fix_params, free_params)

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
%     O_params = [O_params repmat(O_params(end),1,max(data.timepoint)-length(O_params))];
    % Find the rho parameter value
    find_rho=param_list(strcmp(param_list(:,1),'rho'),2);
    rho=sort(cell2mat(find_rho))';
    % Find the nSubs parameter value
    find_nSub=param_list(strcmp(param_list(:,1),'nSubs'),2);
    nSubs=sort(cell2mat(find_nSub))';
    % Find the k parameter value
    find_k=param_list(strcmp(param_list(:,1),'k'),2);
    k=sort(cell2mat(find_k))';
    % Find the p parameter value
    find_p=param_list(strcmp(param_list(:,1),'p'),2);
    p=sort(cell2mat(find_p))';
    % Find the nItems parameter value
%     find_nItems=param_list(strcmp(param_list(:,1),'nItems'),2);
%     nItems=sort(cell2mat(find_nItems))';    
end

function [S_params, R_params, O_params] = transform(S1x1, S1x2, S2x2, R, R_cor)
    O_params(1) = 1;
    S_params(1)=S1x1/(1-S1x1);
    O_params(2)=((S1x1/(1-S1x1))/S1x2) - (S1x1/(1-S1x1));
    S_params(2)=(S2x2*O_params(2))/(1-S2x2); 
    R_params(1)=R/(1-R);
    R_params(2)=R_cor/(1-R_cor); % change to O=1 for final recovery
end

 function df = cleanAndAdd(df)
    df = replaceWithMissing(df,'Strings','NA');
    df.cues = ones(length(df),1);
    df.cues(ismember(df.timepoint,[2,3]) & strcmp(df.other_type,'C')) = 2;
    df.pred_acc = nan(length(df),1);
    df.pred_acc_plus = nan(length(df),1);
    df.pred_acc_neg = nan(length(df),1);    
    df.acc_plus(df.nplus==0) = nan(sum(df.nplus==0),1);
    df.acc_neg(df.nneg==0) = nan(sum(df.nneg==0),1);
 end
 
 function [fix_params, free_params, param_vec, fix_param_array ] = parse_params(ip)
    [fix_params, inda] = intersect(ip.Parameters, ip.Results.fix_params);
    [free_params, indb] = intersect(ip.Parameters, ip.Results.free_params);
    tmp = struct2cell(ip.Results);
    param_vec  = [tmp{indb}];
    fix_param_array = [fix_params', num2cell([tmp{inda}]')];
 end
 
 function [ s_chain, c_chain, t_chain, ot_chain] = defChains(data)
    s_chain = unique(data.chain(strcmp('S',data.practice) & data.timepoint ==1));
    t_chain = unique(data.chain(strcmp('T',data.practice) & strcmp('',data.other_type) & ~isnan(data.acc) & data.timepoint ==1));
    c_chain = unique(data.chain(strcmp('C',data.practice) & strcmp('',data.other_type) & data.timepoint ==1));
    ot_chain = unique(data.chain(strcmp('T',data.other_type) & data.timepoint ==1));
 end
 
 function h = drawResults(data,chisquare, oneshotval, p, show,name)
        h = figure('Visible',show);
        subplot(2,1,1);
        if numel(intersect({'immediate';'delay'},data.group)) ==2
            symbol = '-';
        else
            symbol = 'x';
        end    
        hold on     
            xlim([min(data.timepoint)-.25 max(data.timepoint)+.25])
            title(name)
            xlabel('timepoint')
            % Chain 1 = C-T, Chain 2 = C, Chain 3 = S, Chain 4 = T-C, Chain 5 = T
            plot(data.timepoint(strcmp('immediate',data.group) & data.timepoint==1),data.acc(strcmp('immediate',data.group)& data.timepoint==1), 'og')
            plot(data.timepoint(strcmp('delay',data.group)& data.timepoint==1), data.acc(strcmp('delay',data.group)& data.timepoint==1),'og')
            plot(data.timepoint(ismember(data.timepoint, [2 3]) & data.chain==1),data.acc(ismember(data.timepoint, [2 3]) & data.chain==1), 'or')
            plot(data.timepoint(ismember(data.timepoint, [2 3]) & data.chain==2),data.acc(ismember(data.timepoint, [2 3]) & data.chain==2), 'om')
            plot(data.timepoint(ismember(data.timepoint, [2 3]) & data.chain==3),data.acc(ismember(data.timepoint, [2 3]) & data.chain==3), 'ok')
            plot(data.timepoint(ismember(data.timepoint, [2 3]) & data.chain==5),data.acc(ismember(data.timepoint, [2 3]) & data.chain==5), 'ob')
            plot(data.timepoint(strcmp('immediate',data.group) & data.timepoint==1),data.pred_acc(strcmp('immediate',data.group)& data.timepoint==1), 'xg')
            plot(data.timepoint(strcmp('delay',data.group)& data.timepoint==1), data.pred_acc(strcmp('delay',data.group)& data.timepoint==1),'xg')
            plot(data.timepoint(ismember(data.timepoint, [2 3]) & data.chain==1),data.pred_acc(ismember(data.timepoint, [2 3]) & data.chain==1), [symbol, 'r'])
            plot(data.timepoint(ismember(data.timepoint, [2 3]) & data.chain==2),data.pred_acc(ismember(data.timepoint, [2 3]) & data.chain==2), [symbol, 'm'])
            plot(data.timepoint(ismember(data.timepoint, [2 3]) & data.chain==3),data.pred_acc(ismember(data.timepoint, [2 3]) & data.chain==3), [symbol, 'k'])
            plot(data.timepoint(ismember(data.timepoint, [2 3]) & data.chain==5),data.pred_acc(ismember(data.timepoint, [2 3]) & data.chain==5), [symbol, 'b'])
            text('position',[min(data.timepoint),min([data.acc data.pred_acc])+.02], ...
             'string', char(['err = ' num2str(chisquare)] ,[ 'One Shot: ' num2str(oneshotval)]), ...
             'FontWeight','bold');
        hold off 
        subplot(2,1,2);
        hold on
            title('Conditional Accuracy')
            xlabel('Cue (1=Same,2=New)')
            xlim([.75 2.25])
            plot(2,data.acc_plus(ismember(data.timepoint, [2 3]) & data.chain==1), '+r')
            plot(1,data.acc_plus(ismember(data.timepoint, [2 3]) & data.chain==5), '+b')
            plot(2,data.acc_neg(ismember(data.timepoint, [2 3]) & data.chain==1), 'or')
            plot(1,data.acc_neg(ismember(data.timepoint, [2 3]) & data.chain==5), 'ob')

            plot(2,data.pred_acc_plus(ismember(data.timepoint, [2 3]) & data.chain==1), [symbol, 'r'])
            plot(1,data.pred_acc_plus(ismember(data.timepoint, [2 3]) & data.chain==5), [symbol, 'b'])   
            plot(2,data.pred_acc_neg(ismember(data.timepoint, [2 3]) & data.chain==1), [symbol, 'r'])
            plot(1,data.pred_acc_neg(ismember(data.timepoint, [2 3]) & data.chain==5), [symbol, 'b'])        
        hold off
        saveas(h,fullfile(p.root_dir,'SAMRL',name),'png')
 end 