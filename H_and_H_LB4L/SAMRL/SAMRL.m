 function [chisquare, final_params, agg_data, varargout] = SAMRL(varargin)

    ip = inputParser;
    ip.KeepUnmatched = true;
    ip.addParamValue('S1', unifrnd(.1,.2), @isnumeric)
    ip.addParamValue('S2', unifrnd(.5,1), @isnumeric)
    ip.addParamValue('R', unifrnd(4,6), @isnumeric)
    ip.addParamValue('R_cor', unifrnd(20,30), @isnumeric)
    ip.addParamValue('O1', unifrnd(1,2), @isnumeric)
    ip.addParamValue('O2', unifrnd(2,3), @isnumeric)
    ip.addParamValue('rho',0, @isnumeric)
    ip.addParamValue('R_var',0, @isnumeric)
    ip.addParamValue('k', 5, @isnumeric)
    ip.addParamValue('p', .8, @isnumeric)
    ip.addParamValue('nSubs',63, @isnumeric)
    ip.addParamValue('free_params', {'S1','S2','R','R_cor','O1','O2','p'})
    ip.addParamValue('fix_params', {'rho','nSubs','k'})
    ip.addParamValue('fitting',true, @islogical)
    ip.addParamValue('one_shot',2, @isnumeric)
    ip.addParamValue('showfigs','On', @ischar)
    ip.addParamValue('range',1:100,@isnumeric)
    parse(ip,varargin{:}); 

    [p.file_dir , ~, ~]  = fileparts(mfilename('fullpath'));
    parts = strsplit(p.file_dir,'\');
    p.root_dir = fullfile(parts{1:end-1});
    path(path,genpath(p.file_dir));   

    agg_data = dataset('file',fullfile(p.root_dir,'group_means.csv'), ...
        'format','%s%s%s%f%f%f%f%f%f%f%f%f', 'TreatAsEmpty','NA','Delimiter',',','ReadVarNames','on');
    agg_data = cleanAndAdd(agg_data);
    
    subdata = dataset('file',fullfile(p.root_dir,'cond_means_by_ss.csv'), ...
        'format','%f%s%s%s%f%f%f%f%f%f%f%f%f%f', 'TreatAsEmpty','NA','Delimiter',',','ReadVarNames','on');
    subdata = cleanAndAdd(subdata);
    subdata = subdata(ismember(subdata.subject,ip.Results.range),:);

    [~, free_params, param_vec, fix_param_array ] = parse_params(ip);
        
    % Fit aggregate Means
%     if ip.Results.fitting
%         fmin_opts=optimset('MaxFunEvals',2500);  
%         best_chisquare = 10000;
%         for i = 1:1
%             [fitted_params, chisquare] = fminsearch(@(x) ...
%                 fitSAMRL(x,agg_data, fix_param_array, free_params', ip.Results.one_shot), param_vec, fmin_opts);
%             if chisquare < best_chisquare
%                 best_chisquare = chisquare;
%                 final_params = fitted_params;
%             end
%         end
%         [chisquare,agg_data]=fitSAMRL(final_params, agg_data,  fix_param_array, free_params', ip.Results.one_shot);
%     else
%         [chisquare,agg_data]=fitSAMRL(param_vec, agg_data,  fix_param_array, free_params', ip.Results.one_shot);
%         final_params= param_vec;
%     end
%     h = drawresults(agg_data,chisquare,ip.Results.one_shot, p, 'On','Means');
%     close(h);

    % Fit Subject Means
    start_free_params = free_params;
    start_param_vec = param_vec;
    ll = zeros(length(unique(subdata.subject)),1);
    pars = zeros(length(unique(subdata.subject)),5);
    k=1;
    for i = unique(subdata.subject)'
        if all(strcmp('immediate', subdata.group(subdata.subject ==i)))
            free_params = start_free_params(~strcmp('O2',start_free_params));
            param_vec = start_param_vec(~strcmp('O2',start_free_params));
        else
            free_params = start_free_params(~strcmp('O1',start_free_params));
            param_vec = start_param_vec(~strcmp('O1',start_free_params));
        end
        if ip.Results.fitting
            fmin_opts=optimset('MaxFunEvals',3500,'MaxIter',3500);  
            best_chisquare = 10000;
            for j = 1:1
                [fitted_params, chisquare] = fminsearch(@(x) ...
                    fitSAMRL(x,subdata(subdata.subject ==i,:), fix_param_array, free_params', ip.Results.one_shot), param_vec, fmin_opts);
                if chisquare < best_chisquare
                    best_chisquare = chisquare;
                    final_params = fitted_params;
%                     a = cellfun(@(x) find(strcmp(x,varargin)), free_params, 'Unif', 0 )
%                     b = [[a{:}] [a{:}] + 1]
%                     varargin(b)  = []   
%                     parse(ip,varargin{:}); 
%                     [~, free_params, param_vec, fix_param_array ] = parse_params(ip);

                end
            end
            [chisquare,data]=fitSAMRL(final_params,subdata(subdata.subject ==i,:),  fix_param_array, free_params', ip.Results.one_shot);
        else
            [chisquare,data]=fitSAMRL(param_vec, subdata(subdata.subject ==i,:),  fix_param_array, free_params', ip.Results.one_shot);
            final_params= param_vec;
        end
        subdata(subdata.subject ==i,{'pred_acc','pred_acc_plus','pred_acc_neg'}) = data(:,{'pred_acc','pred_acc_plus','pred_acc_neg'});
        ll(k) = chisquare;
        pars(k,:) = final_params;
        h = drawresults(data,chisquare,ip.Results.one_shot, p, ip.Results.showfigs,['Subject' num2str(i)]); 
        close(h);
        k=k+1;
    end
    varargout{1} = subdata;
    varargout{2} = ll;
    varargout{3} = pars;
    
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
 
 function h = drawresults(data,chisquare, oneshotval, p, show,name)
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