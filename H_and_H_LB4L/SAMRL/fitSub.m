function [ ll, pars, subdata] = fitSub(varargin)
    
    utils = getUtils;
    rng(10);
    ip = inputParser;
    ip.KeepUnmatched = true;
    ip.addParamValue('S1', unifrnd(.1,.5), @isnumeric)
    ip.addParamValue('S2', unifrnd(.5,1), @isnumeric)
    ip.addParamValue('R', unifrnd(2,5), @isnumeric)
    ip.addParamValue('R_cor', unifrnd(5,10), @isnumeric)
    ip.addParamValue('O1', unifrnd(2,4), @isnumeric)
    ip.addParamValue('pS1x1', unifrnd(.15,.5), @isnumeric)
    ip.addParamValue('pS1x2', unifrnd(.0,.15), @isnumeric)
    ip.addParamValue('pS2x2', unifrnd(.5,1), @isnumeric)       
    ip.addParamValue('pR', unifrnd(.5,.7), @isnumeric)
    ip.addParamValue('pRcor', unifrnd(.7,.9), @isnumeric)     
    ip.addParamValue('rho',0, @isnumeric)
    ip.addParamValue('R_var',0, @isnumeric)
    ip.addParamValue('k', 10, @isnumeric)
    ip.addParamValue('p', 1, @isnumeric)
    ip.addParamValue('free_params', {'S1','S2','R','R_cor','O1'} )
    ip.addParamValue('fix_params', {'rho','nSubs','k','p'})
    ip.addParamValue('fitting',true, @islogical)
    ip.addParamValue('one_shot',2, @isnumeric)
    ip.addParamValue('RI',true, @islogical)    
    ip.addParamValue('showfigs','On', @ischar)
    ip.addParamValue('range',1:100,@isnumeric)
    parse(ip,varargin{:}); 

    [p.file_dir , ~, ~]  = fileparts(mfilename('fullpath'));
    parts = strsplit(p.file_dir,'\');
    p.root_dir = fullfile(parts{1:end-1});
    path(path,genpath(p.file_dir));
    
    [~, free_params, param_vec, fix_param_array ] = utils.parse_params(ip);

    subdata = dataset('file',fullfile(p.root_dir,'cond_means_by_ss.csv'), ...
                      'format','%f%s%s%s%f%f%f%f%f%f%f%f', 'TreatAsEmpty','NA','Delimiter',',','ReadVarNames','on');
    subdata = utils.cleanAndAdd(subdata);
    subdata = subdata(ismember(subdata.subject,ip.Results.range),:);

    ll = zeros(length(unique(subdata.subject)),1);
    pars = zeros(length(unique(subdata.subject)),5);
    k=1;
    starting = param_vec;
    for i = unique(subdata.subject)'
        if ip.Results.fitting
            param_vec = starting;
            fmin_opts=optimset('MaxFunEvals',3500,'MaxIter',3500);  
            best_chisquare = 10000;
            for j = 1:15
                [fitted_params, chisquare] = fminsearch(@(x) ...
                    SAMRL_sub(x,subdata(subdata.subject ==i,:), fix_param_array, free_params', ip.Results.one_shot,ip.Results.RI), param_vec, fmin_opts);
                if chisquare < best_chisquare
                    best_chisquare = chisquare;
                    final_params = fitted_params;
                end
                a = cellfun(@(x) find(strcmp(x,varargin)), free_params, 'Unif', 0 );
                b = [[a{:}] [a{:}] + 1];
                varargin(b)  = []   ;
                parse(ip,varargin{:}); 
                [~, free_params, param_vec, fix_param_array ] = utils.parse_params(ip);
            end
            [chisquare,data]=SAMRL_sub(final_params,subdata(subdata.subject ==i,:),  fix_param_array, free_params', ip.Results.one_shot,ip.Results.RI);
        else
            [chisquare,data]=SAMRL_sub(param_vec, subdata(subdata.subject ==i,:),  fix_param_array, free_params', ip.Results.one_shot,ip.Results.RI);
            final_params= param_vec;
        end
        subdata(subdata.subject ==i,{'pred_acc','pred_acc_plus','pred_acc_neg'}) = data(:,{'pred_acc','pred_acc_plus','pred_acc_neg'});
        ll(k) = chisquare;
        pars(k,:) = final_params;
        h = utils.drawResults(data,chisquare,ip.Results.one_shot, p, ip.Results.showfigs,['Subject' num2str(i)]); 
        close(h);
        k=k+1;
    end 
 end



