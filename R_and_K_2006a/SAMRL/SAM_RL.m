function  [chisquare, final_params, data, varargout]  = SAM_RL(varargin)
    % SAMRL and the Testing Effect
    % Examples:
    %   SAM_RL('nSubs',1000,'free_params',{'S1','S2','R','R_cor','O1','O2','rho'}, 'fix_params',{'nSubs','nItems','k'})
    %
    %   Will use the simulation method, allowing for variability in sampling
    %   strength (rho = item sd) with 1000 simulated subjects
    
    ip = inputParser;
    ip.KeepUnmatched = true;
    ip.addParamValue('S1', 0.0047429, @isnumeric) % mean of sampling strength distribution after initial study
    ip.addParamValue('S2', 0.021532, @isnumeric)  % mean increase in sampling strength distribution after study practice & correct recall
    ip.addParamValue('R', 4.0636, @isnumeric) % Recovery Strength after initial study
    ip.addParamValue('R_cor', 20.81, @isnumeric) % Increase in recovery strength after correct recall
    ip.addParamValue('O1', 2.9828, @isnumeric) % Extralist interference at 2 day test
    ip.addParamValue('O2', 4.499, @isnumeric) % Extralist interference at 7 day test
    ip.addParamValue('rho',0, @isnumeric)  %  SD of sampling strength distribution
    ip.addParamValue('R_var',0, @isnumeric) 
    ip.addParamValue('k', 500, @isnumeric) % number of search cycles
    ip.addParamValue('nSubs',120, @isnumeric)
    ip.addParamValue('nItems',30, @isnumeric)
    ip.addParamValue('free_params', {'S1','S2','R','R_cor','O1','O2'})
    ip.addParamValue('fix_params', {'rho','nSubs','nItems','k'})
    ip.addParamValue('fitting',true, @islogical)
    ip.addParamValue('one_shot',1, @isnumeric)
        
    parse(ip,varargin{:}); 
    [p.file_dir , ~, ~]  = fileparts(mfilename('fullpath'));
    parts = strsplit(p.file_dir,'\');
    p.root_dir = fullfile(parts{1:end-1});
    path(path,genpath(p.file_dir));
    rawdata = dataset('file',fullfile(p.root_dir,'group_means.csv'), ...
        'format','%s%s%s%f%f%f%f%f', 'TreatAsEmpty','NA','Delimiter',',','ReadVarNames','on');
    rawdata = replaceWithMissing(rawdata,'Strings','NA');
    rawdata.cues = ones(length(rawdata),1);
    rawdata.cues(ismember(rawdata.timepoint,[2,3]) & strcmp(rawdata.other_type,'C')) = 2;

    [fix_params, inda] = intersect(ip.Parameters, ip.Results.fix_params);
    [free_params, indb] = intersect(ip.Parameters, ip.Results.free_params);
    tmp = struct2cell(ip.Results);
    param_vec  = [tmp{indb}];
    fix_param_array = [fix_params', num2cell([tmp{inda}]')];

    if ip.Results.fitting
        fmin_opts=optimset('MaxFunEvals',2000,'PlotFcn',@optimplotfval);      
        [fitted_params, ~, ~ , info] = fminsearch(@(x) ...
            fit_SAM_RL(x,rawdata, fix_param_array, free_params', ip.Results.one_shot), param_vec, fmin_opts);
        [chisquare,data]=fit_SAM_RL(fitted_params, rawdata,  fix_param_array, free_params', ip.Results.one_shot) ;
         final_params= param_vec;
         varargout{1} = info;
    else
        [chisquare,data]=fit_SAM_RL(param_vec, rawdata,  fix_param_array, free_params', ip.Results.one_shot) ;
        final_params= fitted_params;
    end
    figure;
    hold on     
        plot(data.timepoint(data.chain == 1),data.acc(data.chain == 1), 'ro')
        plot(data.timepoint(data.chain == 2), data.acc(data.chain == 2),'bo')
        plot(data.timepoint(data.chain == 1),data.pred_acc(data.chain == 1), 'rx')
        plot(data.timepoint(data.chain == 2), data.pred_acc(data.chain == 2),'bx')
        xlim([min(data.timepoint)-.25 max(data.timepoint)+.25])
        text('position',[min(data.timepoint),min([data.acc data.pred_acc])+.02], ...
         'string', char(['\chi^2{ = }' num2str(chisquare)] ,[ 'One Shot: ' num2str(ip.Results.one_shot)], ...
         [ 'rho: ' num2str(final_params(strcmp('rho','free_params')))]),'FontWeight','bold');
    hold off
       
end
