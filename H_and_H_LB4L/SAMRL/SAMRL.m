 function [chisquare, final_params, data, varargout] = SAMRL(varargin)

    ip = inputParser;
    ip.KeepUnmatched = true;
    ip.addParamValue('S1', unifrnd(.01,.05), @isnumeric)
    ip.addParamValue('S2', unifrnd(.1,1), @isnumeric)
    ip.addParamValue('R', unifrnd(4,6), @isnumeric)
    ip.addParamValue('R_cor', unifrnd(20,30), @isnumeric)
    ip.addParamValue('O1', unifrnd(2,4), @isnumeric)
    ip.addParamValue('O2', unifrnd(5,7), @isnumeric)
    ip.addParamValue('rho',0, @isnumeric)
    ip.addParamValue('R_var',0, @isnumeric)
    ip.addParamValue('k', 400, @isnumeric)
    ip.addParamValue('p', .7, @isnumeric)
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
    rawdata = dataset('file',fullfile(p.root_dir,'group_means.csv'), ...
        'format','%s%s%s%f%f%f%f%f%f%f', 'TreatAsEmpty','NA','Delimiter',',','ReadVarNames','on');
    rawdata = replaceWithMissing(rawdata,'Strings','NA');
    rawdata.cues = ones(length(rawdata),1);
    rawdata.cues(ismember(rawdata.timepoint,[2,3]) & strcmp(rawdata.other_type,'C')) = 2;

    [fix_params, inda] = intersect(ip.Parameters, ip.Results.fix_params);
    [free_params, indb] = intersect(ip.Parameters, ip.Results.free_params);
    tmp = struct2cell(ip.Results);
    param_vec  = [tmp{indb}];
    fix_param_array = [fix_params', num2cell([tmp{inda}]')];

    if ip.Results.fitting
        fmin_opts=optimset('MaxFunEvals',2500);      
        [fitted_params, ~, ~ , info] = fminsearch(@(x) ...
            fitSAMRL(x,rawdata, fix_param_array, free_params', ip.Results.one_shot), param_vec, fmin_opts);
        [chisquare,data]=fitSAMRL(fitted_params, rawdata,  fix_param_array, free_params', ip.Results.one_shot);
        final_params = fitted_params;
        varargout{1} = info;

    else
        [chisquare,data]=fitSAMRL(param_vec, rawdata,  fix_param_array, free_params', ip.Results.one_shot);
        final_params= param_vec;
    end
    
    figure;
    hold on     
        xlim([min(data.timepoint)-.25 max(data.timepoint)+.25])
        title('all group means')
        xlabel('timepoint')
        plot(data.timepoint(strcmp('immediate',data.group)),data.acc(strcmp('immediate',data.group)), 'ro')
        plot(data.timepoint(strcmp('delay',data.group)), data.acc(strcmp('delay',data.group)),'bo')
        plot(data.timepoint(strcmp('immediate',data.group)),data.pred_acc(strcmp('immediate',data.group)), 'rx')
        plot(data.timepoint(strcmp('delay',data.group)), data.pred_acc(strcmp('delay',data.group)),'bx')
        text('position',[min(data.timepoint),min([data.acc data.pred_acc])+.02], ...
         'string', char(['\chi^2{ = }' num2str(chisquare)] ,[ 'One Shot: ' num2str(ip.Results.one_shot)]), ...
         'FontWeight','bold');
    hold off 
    
%     figure;
%     hold on
%         xlim([-.25 1.25])
%         title('immediate group conditional means')
%         xlabel('practice accuracy')
%         plot([0,1], double(data(strcmp('immediate',data.group) & data.timepoint > 1 &  data.chain == 1,{'cond_neg','cond_plus'})), 'bo')
%         plot([0,1], double(data(strcmp('immediate',data.group) & data.timepoint > 1 &  data.chain == 5,{'cond_neg','cond_plus'})), 'ro')
%         plot([0,1], double(data(strcmp('immediate',data.group) & data.timepoint > 1 &  data.chain == 1,{'pred_cond_neg','pred_cond_plus'})), 'bx')
%         plot([0,1], double(data(strcmp('immediate',data.group) & data.timepoint > 1 &  data.chain == 5,{'pred_cond_neg','pred_cond_plus'})), 'rx')
%     hold off
%     
%     figure;
%     hold on
%         xlim([-.25 1.25])
%         title('practice accuracy')
%         plot([0,1], double(data(strcmp('delay',data.group) & data.timepoint > 1 &  data.chain == 1,{'cond_neg','cond_plus'})), 'bo')
%         plot([0,1], double(data(strcmp('delay',data.group) & data.timepoint > 1 &  data.chain == 5,{'cond_neg','cond_plus'})), 'ro')
%         plot([0,1], double(data(strcmp('delay',data.group) & data.timepoint > 1 &  data.chain == 1,{'pred_cond_neg','pred_cond_plus'})), 'bx')
%         plot([0,1], double(data(strcmp('delay',data.group) & data.timepoint > 1 &  data.chain == 5,{'pred_cond_neg','pred_cond_plus'})), 'rx')
%     hold off
end