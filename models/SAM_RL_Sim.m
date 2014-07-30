function SAM_RL_Sim(varargin)
% SAM Testing Effect Simulation
% NEEDS A REAL DESCRIPTION BECAUSE ARGUMENT LIST IS CRITICAL!!!!!!!!
[stack,~]=dbstack; % Find caller!!!

if strcmp(stack(1,1).name,mfilename) && size(stack,1)== 1
    if nargin > 0 
        error('SAM_RL:calling', ['If calling SAM_RL_Sim directly, don''t pass in arguments.\n' ...
                                'Data and parameters are hard coded, edit them directly in the file!'])
    end
    
    % Format for Setting Params: 1x3 Cell Array, use specific data types
    % $NAME_param = {Name' (String), Value (num), fix (1)/free (0)}
    % In variable name, replace $NAME with the name used in the cell array.

    % Free Parameters
    S1_param={'S1',.004743,0}; %#ok<*NASGU> % mean of sampling strength distribution after initial study
    S2_param={'S2',.021532,0}; % mean of sampling strength distribution after study practice & correct recall
    rho_param={'rho',0,1};   % scaling paramter to get SD of sampling distribution
    R_param={'R',4.036,0}; % Recovery Strength after initial study
    Rcor_param = {'Rcor',20.81,0}; % Recovery Strength after correct recall
    O1_param={'O1',2.9828,0}; % Extralist interference at 2 day test
    O2_param={'O2',4.499,0}; % Extralist interference at 7 day test
    k_param={'k',500,1};
    nItems_param={'nItems',30,1};
    nSub_param={'nSub',1000,1};
    z=[ who('-rexexp','*_param')'; repmat({','},1,numel(who('-rexexp','*_param'))) ];
    z=horzcat(z{1:end-1});
    param_list=eval(['vertcat(',z,')']); clear z;
    free_params=param_list([param_list{:,3}]==0,1:2); 
    fix_params=param_list([param_list{:,3}]==1,1:2);
    
    design={'S','T';'S','S'}; 
    [~,cn]=find(strcmp(design,'T'));
    if any(cn==1)
        error('design:dimensions','You shouldn''t have a test before you study!');
    end
    
    % data=[acc on 5 min test after study,  acc on 2 day test after study,  acc on 1 week test after study, 
    %       acc on recall practice,  acc on 5 min test after recall practice,
    %       acc on 2 day test after recall practice, acc on 1 week test after recall practice] 
    data=[ .81, .54, .42, .70,  .75, .68, .56];

    one_shot='on';
    plotting='iter';
    fit='check';
elseif strcmp(stack(2,1).name,'run_button_Callback') && size(stack,1) > 1
    % Make Params Array
    hObject=varargin{1};
    handles=varargin{2};
    h=findobj('-regexp','Tag','\w*_param');
    p=strrep(get(h,'Tag'),'_param','');
    v=cellfun(@str2double,get(h,'String'),'uni',false);
    f=[ {1,1,1}'; get(findobj('-regexp','Tag','\w*_fix'),'Value') ];
    param_list={p{:} ;v{:} ;f{:}}';
    free_params=param_list([param_list{:,3}]==0,1:2); 
    fix_params=param_list([param_list{:,3}]==1,1:2);
    % Get Design
    [design]=gui('make_data',handles,0);
    %Get Data
    data = get(handles.data_table,'Data');
    data=reshape(data',1,[]);
    data=data(cellfun(@isnumeric,data));
    data=[data{:}];
    if isempty(data);
        errordlg('Need to fill in data')
        set(handles.data_table,'BackgroundColor','r');
    end
    % Get Plotting
    plotting=strrep(get(get(handles.plots_panel,'SelectedObject'),'Tag'),'plots_',''); 
    % Get One Shot
    one_shot=get(get(handles.one_shot,'SelectedObject'),'Tag');
    % Get Run command
    fit=get(get(handles.fit_panel,'SelectedObject'),'Tag');
end

param_hist=[];
fval_hist=[];

if strcmpi(fit,'fit') 
    fmin_opts=optimset('MaxFunEvals',2500,'OutputFcn', @history);      
    if strcmpi(plotting,'iter')    
        [fitted_params, chisquare, ~ , info]=fminsearch(@(x) fit_SAM_RL_Sim(x,data,design,fix_params,free_params,one_shot,plotting,fit),[free_params{:,2}],fmin_opts) %#ok<NASGU,NOPRT,*ASGLU>
    elseif strcmpi(plotting,'final')
        [fitted_params, chisquare, ~ , info]=fminsearch(@(x) fit_SAM_RL_Sim(x,data,design,fix_params,free_params,one_shot,'off',fit),[free_params{:,2}],fmin_opts) %#ok<NASGU,NOPRT,*ASGLU>
        chisquare=fit_SAM_RL_Sim(fitted_params,data,design,fix_params,free_params,one_shot,plotting,fit) %#ok<NASGU,NOPRT>
        history_plot_final([],[],'done')        
    else
        [fitted_params, chisquare, ~ , info]=fminsearch(@(x) fit_SAM_RL_Sim(x,data,design,fix_params,free_params,one_shot,'off',fit),[free_params{:,2}],fmin_opts) %#ok<NASGU,NOPRT,*ASGLU>
    end    
elseif strcmp(fit,'check')
    chisquare=fit_SAM_RL_Sim([free_params{:,2}],data,design,fix_params,free_params,one_shot,plotting,fit) %#ok<NASGU,NOPRT>
end


    % fminsearch's ouput functions
    function [stop]= history(x,optimvals,state)
        stop = false;
        if any(strcmp(state, {'iter','done'}))
                param_hist =[param_hist; ([x optimvals.iteration]) ];
                fval_hist= [ fval_hist;  ([optimvals.fval optimvals.iteration ])];
                if findobj('type','figure','name','SAM-RL Plots');
                    subplot(6,6,[4:6 10:12 16:18],'Position',[.54 .52 .45 .46])
                        hold on
                        for i=1:(size(param_hist,2)-1)
                            plot(param_hist(:,end),param_hist(:,i))
                        end
                        hold off
                    subplot(6,6,(19:36),'Position',[.03 .03 .96 .45])
                        plot(fval_hist(:,2), fval_hist(:,1));
                end
        end
    end

end
