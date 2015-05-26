function [ chisquare, final_params, agg_data ] = fitAgg(varargin)

    rng(10);
    utils = getUtils;
    
    ip = inputParser;
    ip.KeepUnmatched = true;
    ip.addParamValue('S1', unifrnd(.1,.5), @isnumeric)
    ip.addParamValue('S2', unifrnd(.5,1), @isnumeric)
    ip.addParamValue('R', unifrnd(2,5), @isnumeric)
    ip.addParamValue('R_cor', unifrnd(5,10), @isnumeric)
    ip.addParamValue('O1', unifrnd(2,3), @isnumeric)
    ip.addParamValue('O2', unifrnd(3,4), @isnumeric)
    ip.addParamValue('rho',0, @isnumeric)
    ip.addParamValue('R_var',0, @isnumeric)
    ip.addParamValue('k', 10, @isnumeric)
    ip.addParamValue('p', .8, @isnumeric)
    ip.addParamValue('nSubs',63, @isnumeric)
    ip.addParamValue('free_params', {'S1','S2','R','R_cor','O1','O2'})
    ip.addParamValue('fix_params', {'rho','nSubs','k','p'})
    ip.addParamValue('fitting',true, @islogical)
    ip.addParamValue('one_shot',2, @isnumeric)
    ip.addParamValue('RI',true, @isnumeric)    
    ip.addParamValue('showfigs','On', @ischar)
    parse(ip,varargin{:}); 

    [p.file_dir , ~, ~]  = fileparts(mfilename('fullpath'));
    parts = strsplit(p.file_dir,'\');
    p.root_dir = fullfile(parts{1:end-1});
    path(path,genpath(p.file_dir));   

    agg_data = dataset('file',fullfile(p.root_dir,'group_means.csv'), ...
        'format','%s%s%s%f%f%f%f%f%f%f%f', 'TreatAsEmpty','NA','Delimiter',',','ReadVarNames','on');
    agg_data = utils.cleanAndAdd(agg_data);

    [~, free_params, param_vec, fix_param_array ] = utils.parse_params(ip);
        
    % Fit aggregate Means
    if ip.Results.fitting
        fmin_opts=optimset('MaxFunEvals',2500);  
        best_chisquare = 10000;
        for i = 1:1
            [fitted_params, chisquare] = fminsearch(@(x) ...
                SAMRL_agg(x,agg_data, fix_param_array, free_params', ip.Results.one_shot,ip.Results.RI), param_vec, fmin_opts);
            if chisquare < best_chisquare
                best_chisquare = chisquare;
                final_params = fitted_params;
            end
        end
        [chisquare,agg_data]=SAMRL_agg(final_params, agg_data,  fix_param_array, free_params', ip.Results.one_shot,ip.Results.RI);
    else
        [chisquare,agg_data]=SAMRL_agg(param_vec, agg_data,  fix_param_array, free_params', ip.Results.one_shot,ip.Results.RI);
        final_params= param_vec;
    end
    h = utils.drawResults(agg_data,chisquare,ip.Results.one_shot, p, 'On','Means');
    
    if ip.Results.fitting
        fmin_opts=optimset('MaxFunEvals',2500);  
        best_chisquare = 10000;
        for i = 1:1
            [fitted_params, chisquare] = fminsearch(@(x) ...
                SAMRL_DEH(x,agg_data, fix_param_array, free_params'), param_vec, fmin_opts);
            if chisquare < best_chisquare
                best_chisquare = chisquare;
                final_params = fitted_params;
            end
        end
        [chisquare,agg_dataDEH]=SAMRL_DEH(final_params, agg_data,  fix_param_array, free_params'); %#ok<*NASGU>
    else
        [chisquare,agg_dataDEH]=SAMRL_DEH(param_vec, agg_data,  fix_param_array, free_params'); 
        final_params= param_vec;
    end    
    
    close(h);

end

