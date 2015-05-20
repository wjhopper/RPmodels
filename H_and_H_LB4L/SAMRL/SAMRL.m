 function [chisquare, final_params, data, varargout] = SAMRL(varargin)

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
    ip.addParamValue('k', 10, @isnumeric)
    ip.addParamValue('p', .8, @isnumeric)
    ip.addParamValue('nSubs',63, @isnumeric)
    ip.addParamValue('free_params', {'S1','S2','R','R_cor','O1','O2','p'})
    ip.addParamValue('fix_params', {'rho','nSubs','k'})
    ip.addParamValue('fitting',true, @islogical)
    ip.addParamValue('one_shot',2, @isnumeric)
    parse(ip,varargin{:}); 

    [p.file_dir , ~, ~]  = fileparts(mfilename('fullpath'));
    parts = strsplit(p.file_dir,'\');
    p.root_dir = fullfile(parts{1:end-1});
    path(path,genpath(p.file_dir));    
    
    agg_data = dataset('file',fullfile(p.root_dir,'group_means.csv'), ...
        'format','%s%s%s%f%f%f%f%f%f%f', 'TreatAsEmpty','NA','Delimiter',',','ReadVarNames','on');
    agg_data = replaceWithMissing(agg_data,'Strings','NA');
    agg_data.cues = ones(length(agg_data),1);
    agg_data.cues(ismember(agg_data.timepoint,[2,3]) & strcmp(agg_data.other_type,'C')) = 2;
    
    subdata = dataset('file',fullfile(p.root_dir,'cond_means_by_ss.csv'), ...
        'format','%f%s%s%s%f%f%f%f%f%f%f%f%f%f', 'TreatAsEmpty','NA','Delimiter',',','ReadVarNames','on');
    subdata.cues = ones(length(subdata),1);
    subdata.cues(ismember(subdata.timepoint,[2,3]) & strcmp(subdata.other_type,'C')) = 2;
    [fix_params, inda] = intersect(ip.Parameters, ip.Results.fix_params);
    [free_params, indb] = intersect(ip.Parameters, ip.Results.free_params);
    tmp = struct2cell(ip.Results);
    param_vec  = [tmp{indb}];
    fix_param_array = [fix_params', num2cell([tmp{inda}]')];

%     if ip.Results.fitting
%         fmin_opts=optimset('MaxFunEvals',2500);  
%         best_chisquare = 10000;
%         for i = 1:100
%             [fitted_params, chisquare] = fminsearch(@(x) ...
%                 fitSAMRL(x,agg_data, fix_param_array, free_params', ip.Results.one_shot), param_vec, fmin_opts);
%             if chisquare < best_chisquare
%                 best_chisquare = chisquare;
%                 final_params = fitted_params;
%                 param_vec = fitted_params;
%             end
%         end
%         [chisquare,data]=fitSAMRL(final_params, agg_data,  fix_param_array, free_params', ip.Results.one_shot);
%     else
%         [chisquare,data]=fitSAMRL(param_vec, agg_data,  fix_param_array, free_params', ip.Results.one_shot);
%         final_params= param_vec;
%     end
%     drawresults('On','means')
%     
    for i = unique(subdata.subject)'
        if ip.Results.fitting
            fmin_opts=optimset('MaxFunEvals',2500);  
            best_chisquare = 10000;
            for j = 1:1
                [fitted_params, chisquare] = fminsearch(@(x) ...
                    fitSAMRL(x,subdata(subdata.subject ==i,:), fix_param_array, free_params', ip.Results.one_shot), param_vec, fmin_opts);
                if chisquare < best_chisquare
                    best_chisquare = chisquare;
                    final_params = fitted_params;
                    param_vec = fitted_params;
                end
            end
            [chisquare,data]=fitSAMRL(final_params,subdata(subdata.subject ==i,:),  fix_param_array, free_params', ip.Results.one_shot);
        else
            [chisquare,data]=fitSAMRL(param_vec, subdata(subdata.subject ==i,:),  fix_param_array, free_params', ip.Results.one_shot);
            final_params= param_vec;
        end
        drawresults('Off',['Subject' num2str(i)]); 
    end
         
        
     function drawresults(show,name)
        h = figure('Visible',show);
        subplot(2,1,1);
        if numel(intersect({'immediate';'delay'},data.group)) ==2
            symbol = '-';
        else
            symbol = 'x';
        end    
        hold on     
            xlim([min(data.timepoint)-.25 max(data.timepoint)+.25])
            title('Condition Means')
            xlabel('timepoint')
            % Chain 1 = C-T, Chain 2 = C, Chain 3 = S, Chain 4 = T-C, Chain 5 = T
            plot(data.timepoint(strcmp('immediate',data.group) & data.timepoint==1),data.acc(strcmp('immediate',data.group)& data.timepoint==1), 'or')
            plot(data.timepoint(strcmp('delay',data.group)& data.timepoint==1), data.acc(strcmp('delay',data.group)& data.timepoint==1),'ob')
            plot(data.timepoint(ismember(data.timepoint, [2 3]) & data.chain==1),data.acc(ismember(data.timepoint, [2 3]) & data.chain==1), 'or')
            plot(data.timepoint(ismember(data.timepoint, [2 3]) & data.chain==2),data.acc(ismember(data.timepoint, [2 3]) & data.chain==2), 'ob')
            plot(data.timepoint(ismember(data.timepoint, [2 3]) & data.chain==3),data.acc(ismember(data.timepoint, [2 3]) & data.chain==3), 'ok')
            plot(data.timepoint(ismember(data.timepoint, [2 3]) & data.chain==5),data.acc(ismember(data.timepoint, [2 3]) & data.chain==5), 'og')
            plot(data.timepoint(strcmp('immediate',data.group) & data.timepoint==1),data.pred_acc(strcmp('immediate',data.group)& data.timepoint==1), 'xr')
            plot(data.timepoint(strcmp('delay',data.group)& data.timepoint==1), data.pred_acc(strcmp('delay',data.group)& data.timepoint==1),'xb')
            plot(data.timepoint(ismember(data.timepoint, [2 3]) & data.chain==1),data.pred_acc(ismember(data.timepoint, [2 3]) & data.chain==1), [symbol, 'r'])
            plot(data.timepoint(ismember(data.timepoint, [2 3]) & data.chain==2),data.pred_acc(ismember(data.timepoint, [2 3]) & data.chain==2), [symbol, 'b'])
            plot(data.timepoint(ismember(data.timepoint, [2 3]) & data.chain==3),data.pred_acc(ismember(data.timepoint, [2 3]) & data.chain==3), [symbol, 'k'])
            plot(data.timepoint(ismember(data.timepoint, [2 3]) & data.chain==5),data.pred_acc(ismember(data.timepoint, [2 3]) & data.chain==5), [symbol, 'g'])
            text('position',[min(data.timepoint),min([data.acc data.pred_acc])+.02], ...
             'string', char(['\chi^2{ = }' num2str(chisquare)] ,[ 'One Shot: ' num2str(ip.Results.one_shot)]), ...
             'FontWeight','bold');
        hold off 
        subplot(2,1,2);
        hold on
            title('Conditional Accuracy')
            xlabel('timepoint')
            xlim([min(data.timepoint(data.timepoint > 1))-.25 max(data.timepoint(data.timepoint > 1))+.25])
            plot(data.timepoint(ismember(data.timepoint, [2 3]) & data.chain==1),data.acc_plus(ismember(data.timepoint, [2 3]) & data.chain==1), '+r')
            plot(data.timepoint(ismember(data.timepoint, [2 3]) & data.chain==5),data.acc_plus(ismember(data.timepoint, [2 3]) & data.chain==5), '+g')
            plot(data.timepoint(ismember(data.timepoint, [2 3]) & data.chain==1),data.acc_neg(ismember(data.timepoint, [2 3]) & data.chain==1), 'or')
            plot(data.timepoint(ismember(data.timepoint, [2 3]) & data.chain==5),data.acc_neg(ismember(data.timepoint, [2 3]) & data.chain==5), 'og')

            plot(data.timepoint(ismember(data.timepoint, [2 3]) & data.chain==1),data.pred_acc_plus(ismember(data.timepoint, [2 3]) & data.chain==1), [symbol, 'r'])
            plot(data.timepoint(ismember(data.timepoint, [2 3]) & data.chain==5),data.pred_acc_plus(ismember(data.timepoint, [2 3]) & data.chain==5), [symbol, 'g'])   
            plot(data.timepoint(ismember(data.timepoint, [2 3]) & data.chain==1),data.pred_acc_neg(ismember(data.timepoint, [2 3]) & data.chain==1), [symbol, 'r'])
            plot(data.timepoint(ismember(data.timepoint, [2 3]) & data.chain==5),data.pred_acc_neg(ismember(data.timepoint, [2 3]) & data.chain==5), [symbol, 'g'])        
        hold off
        saveas(h,fullfile(p.root_dir,name),'jpg')
        close(h)
     end
end