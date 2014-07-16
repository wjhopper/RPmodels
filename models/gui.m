function varargout = gui(varargin)
% GUI MATLAB code for gui.fig
%      GUI, by itself, creates a new GUI or raises the existing
%      singleton*.
%
%      H = GUI returns the handle to a new GUI or the handle to
%      the existing singleton*.
%
%      GUI('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in GUI.M with the given input arguments.
%
%      GUI('Property','Value',...) creates a new GUI or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before gui_OpeningFcn gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to gui_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run_button (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help gui

% Last Modified by GUIDE v2.5 15-Jul-2014 16:45:19

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @gui_OpeningFcn, ...
                   'gui_OutputFcn',  @gui_OutputFcn, ...
                   'gui_LayoutFcn',  [] , ...
                   'gui_Callback',   []);
if nargin && ischar(varargin{1})
    gui_State.gui_Callback = str2func(varargin{1});
end

if nargout
    [varargout{1:nargout}] = gui_mainfcn(gui_State, varargin{:});
else
    gui_mainfcn(gui_State, varargin{:});
end
% End initialization code - DO NOT EDIT


    % --- Executes just before gui is made visible.
    function gui_OpeningFcn(hObject, eventdata, handles, varargin)
    % This function has no output args, see OutputFcn.
    % hObject    handle to figure
    % eventdata  reserved - to be defined in a future version of MATLAB
    % handles    structure with handles and user data (see GUIDATA)
    % varargin   command line arguments to gui (see VARARGIN)

    if exist([pwd,'\starting_params.mat'],'file')
       p=open([pwd,'\starting_params.mat']);
       params=[fieldnames(p) struct2cell(p)];
       for i=1:size(params,1)
           if isa(params{i,2},'double');
              if length(params{i,2})> 1
                  val=num2str(params{i,2});
                  val= regexprep(val,'[^\w'']','');
                  sep=repmat(',',1,length(val));
                  val =reshape(reshape([val(:),sep(:)],2*size(val,1),[])',1,[]); 
                  val(end)=[];
                  set_cmd = strcat('set(handles.', params{i,1}, ',''String'',''', val,''')');
              else
                 set_cmd = strcat('set(handles.', params{i,1}, ',''String'', [', num2str(params{i,2}),'])');
              end
           end
           eval(set_cmd);
       end    
       make_data(handles,1);
      set(handles.data_table,'Data',p.data_table);
      assignin('base','defaults',p);
    else
     set(handles.data_table,'Data',cell(''));
    end

    % Choose default command line output for gui
    handles.output = hObject;

    % Update handles structure
    guidata(hObject, handles);

    % UIWAIT makes gui wait for user response (see UIRESUME)
    % uiwait(handles.figure1);

    function varargout = gui_OutputFcn(hObject, eventdata, handles) 
        % varargout  cell array for returning output args (see VARARGOUT);
        % hObject    handle to figure
        % eventdata  reserved - to be defined in a future version of MATLAB
        % handles    structure with handles and user data (see GUIDATA)

        % Get default command line output from handles structure
        varargout{1} = handles.output;
  

    function rho_param_Callback(hObject, eventdata, handles)
        valid=validate(hObject,handles,1);
        if ~valid
            uicontrol(hObject);
        end
  

    function rho_param_CreateFcn(hObject, eventdata, handles)
            if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
                set(hObject,'BackgroundColor','white');
            end
  

    function k_param_Callback(hObject, eventdata, handles)
        valid=validate(hObject,handles,1);
        if ~valid
           uicontrol(hObject)
        end
  

    function k_param_CreateFcn(hObject, eventdata, handles)
        if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
            set(hObject,'BackgroundColor','white');
        end
  

    function nSubs_param_Callback(hObject, eventdata, handles)
        valid=validate(hObject,handles,1);
        if ~valid
           uicontrol(hObject)
        end


    function nSubs_param_CreateFcn(hObject, eventdata, handles)

        if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
            set(hObject,'BackgroundColor','white');
        end


    function nItems_param_Callback(hObject, eventdata, handles)
        valid=validate(hObject,handles,1);
        if ~valid
           uicontrol(hObject)
        end


    function nItems_param_CreateFcn(hObject, eventdata, handles)
        if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
            set(hObject,'BackgroundColor','white');
        end


    function nAttempts_Callback(hObject, eventdata, handles)
        [valid, nAttempts] =validate(hObject,handles,1);
        if valid && nAttempts ~= evalin('base','defaults.nAttempts')
            prac=(1:nAttempts)-1;
            prac=num2str(prac(:))';
            sep=repmat(',',1,length(prac));
            tests =reshape(reshape([prac(:),sep(:)],2*size(prac,1),[])',1,[]); 
            tests(end)=[];
            set(handles.nPractice_Tests,'String',tests);
            make_data(handles,1)
        else
            uicontrol(hObject)
        end

    function nAttempts_CreateFcn(hObject, eventdata, handles)
        if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
            set(hObject,'BackgroundColor','white');
        end

    function nPractice_Tests_Callback(hObject, eventdata, handles)
        [valid, nPractice_tests] =validate(hObject,handles);
        if valid && any(nPractice_tests ~= evalin('base','defaults.nPractice_Tests'))
            if max(nPractice_tests) < str2double(get(handles.nAttempts,'String'))
                set(handles.nAttempts,'String',num2str(max(nPractice_tests)+1));
            end    
        make_data(handles,1)
        else
            uicontrol(hObject)
        end


    function nPractice_Tests_CreateFcn(hObject, eventdata, handles)
        if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
            set(hObject,'BackgroundColor','white');
        end


    function run_button_Callback(hObject, eventdata, handles,varargin)
        h=findobj('-regexp','Tag','\w*_param');
        p=get(h,'Tag');
        v=cellfun(@str2double,get(h,'String'),'uni',false);
        f=[ {1,1,1}'; get(findobj('-regexp','Tag','\w*_fix'),'Value') ];
        param_list={p{:} ;v{:} ;f{:}}';
        free_params=param_list([param_list{:,3}]==0,1:2);
        fix_params=param_list([param_list{:,3}]==1,1:2);

        design=make_data(handles,0);
        data = get(handles.data_table,'Data');
        data=reshape(data',1,[]);
        data=data(cellfun(@isnumeric,data));
        data=[data{:}];

        plotting=strrep(get(get(handles.plots_panel,'SelectedObject'),'Tag'),'plots_','');
        one_shot=get(get(handles.one_shot,'SelectedObject'),'Tag');
        fit=get(get(handles.fit_panel,'SelectedObject'),'Tag');

        if strcmpi(fit,'fit')
            fmin_opts=optimset('MaxFunEvals',2500,'OutputFcn', @output);
            [fitted_params, chisquare, ~ , info]=fminsearch(@(x) fit_SAM_RL_Sim(x,data,design,fix_params,free_params,one_shot,plotting),[free_params{:,2}],fmin_opts) %#ok<NASGU,NOPRT,*ASGLU>
            % save best fitting params in default struct!!

            fit_SAM_RL_Sim(fitted_params,data,design,fix_params,free_params,one_shot,'iter') 
        elseif strcmp(fit,'check') && any(strcmp(plotting,{'iter','final'}))
            chisquare=fit_SAM_RL_Sim([free_params{:,2}],data,design,fix_params,free_params,one_shot,'iter') %#ok<NASGU,NOPRT>

        end

        %quit
   
    % fminsearch's ouput functions
    function stop = output(x,~,state)
        stop = false;
        switch state
            case 'iter'
                x;
            case 'done'
        end


    
    function [valid,value] =validate(hObject,handles,len)
        valid=false;
        x=str2double(strsplit(get(hObject,'String'),'\s*,\s*','DelimiterType','RegularExpression'));
        % 
        % handles_cell=[fieldnames(handles) struct2cell(handles)];
        % x = handles_cell{[handles_cell{:,2}]'==hObject,1}

        if any(isnan(x)) || any(~isreal(x)) || (exist('len','var') && ~isequal(length(x),len))
            set(handles.run_button,'String','Bad Params')
            set(handles.run_button,'Enable','off')
            set(handles.run_button,'BackgroundColor','r')
            set(hObject,'BackgroundColor','r')
            value=NaN;
        else    
            set(handles.run_button,'String','Run Model')
            set(handles.run_button,'Enable','on')
            set(handles.run_button,'BackgroundColor','g')
            set(hObject,'BackgroundColor','w')
            valid=true;
            value=x;
        end


    function nFinal_tests_Callback(hObject, eventdata, handles)

    [valid, nPractice_tests] =validate(hObject,handles);
        if valid && nPractice_tests ~= evalin('base','defaults.nFinal_tests')
        make_data(handles,1)
        else
            uicontrol(hObject)
        end


    % --- Executes during object creation, after setting all properties.
    function nFinal_tests_CreateFcn(hObject, eventdata, handles)
        if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
            set(hObject,'BackgroundColor','white');
        end


    % --- Executes when entered data in editable cell(s) in data_table.
    function data_table_CellEditCallback(hObject, eventdata, handles)
        % hObject    handle to data_table (see GCBO)
        % eventdata  structure with the following fields (see UITABLE)
        %	Indices: row and column indices of the cell(s) edited
        %	PreviousData: previous data for the cell(s) edited
        %	EditData: string(s) entered by the user
        %	NewData: EditData or its converted form set on the Data property. Empty if Data was not changed
        %	Error: error string when failed to convert EditData to appropriate value for Data
        % handles    structure with handles and user data (see GUIDATA)
              % Transform subscipts of selected cell into idx
              data=get(hObject,'Data');      
              if eventdata.PreviousData=='S'
                  data{eventdata.Indices(1),eventdata.Indices(2)}=eventdata.PreviousData;
                  set(hObject,'Data',data);
              end

    function S2_param_Callback(hObject, eventdata, handles)
        [valid, S2]=validate(hObject,handles,1);
        if ~valid
           uicontrol(hObject)
        end

        if S2 < str2double(get(handles.S1_param,'String'));
            set(hObject,'String',get(handles.S1_param,'String'))
        end

    function S2_param_CreateFcn(hObject, eventdata, handles)
        if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
            set(hObject,'BackgroundColor','white');
        end

    function S1_param_Callback(hObject, eventdata, handles)
        [valid, S1]=validate(hObject,handles,1);
        if ~valid
           uicontrol(hObject)
        end

        if S1 > str2double(get(handles.S2_param,'String'));
            set(handles.S2_param,'String',num2str(S1))
        end


    function S1_param_CreateFcn(hObject, eventdata, handles)
        if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
            set(hObject,'BackgroundColor','white');
        end


    function S_add_Callback(hObject, eventdata, handles)

    function R_add_Callback(hObject, eventdata, handles)


    function Rcor_param_Callback(hObject, eventdata, handles)
    [valid, Rcor]=validate(hObject,handles,1);
        if ~valid
           uicontrol(hObject)
        end

        if Rcor < str2double(get(handles.R_param,'String'));
            set(hObject,'String',get(handles.Rcor_param,'String'))
        end

    % --- Executes during object creation, after setting all properties.
    function Rcor_param_CreateFcn(hObject, eventdata, handles)
        if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
            set(hObject,'BackgroundColor','white');
        end


    function R_param_Callback(hObject, eventdata, handles)
        [valid, R]=validate(hObject,handles,1);
        if ~valid
           uicontrol(hObject)
        end

        if R > str2double(get(handles.Rcor_param,'String'));
            set(handles.Rcor_param,'String',num2str(R))
        end

    function R_param_CreateFcn(hObject, eventdata, handles)
        if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
            set(hObject,'BackgroundColor','white');
        end
 


    function O_add_Callback(hObject, eventdata, handles)


    function O2_param_Callback(hObject, eventdata, handles)
        [valid, O2]=validate(hObject,handles,1);
        if ~valid
           uicontrol(hObject)
        end

        if O2 < str2double(get(handles.O1_param,'String'));
            set(hObject,'String',get(handles.O1_param,'String'))
        end


    function O2_param_CreateFcn(hObject, eventdata, handles)
        if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
            set(hObject,'BackgroundColor','white');
        end


    function O1_param_Callback(hObject, eventdata, handles)
        [valid, O1]=validate(hObject,handles,1);
        if ~valid
           uicontrol(hObject)
        end

        if 01 > str2double(get(handles.O2_param,'String'));
            set(handles.O2_param,'String',num2str(O1))
        end


    function O1_param_CreateFcn(hObject, eventdata, handles)
         if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
            set(hObject,'BackgroundColor','white');
         end

    function [design] = make_data(handles,fill)
        [v1 , nAttempts] = validate(handles.nAttempts,handles,1);
        [v2 , nPractice_Tests]  = validate(handles.nPractice_Tests,handles);
        [v3 , nFinal_tests] = validate(handles.nFinal_tests,handles,1);
        if v1 && v2 && v3
            nStudy_Practice=((max(nPractice_Tests))-nPractice_Tests);
            design=num2cell([nStudy_Practice'  nPractice_Tests']);
            design=cellfun(@(x) num2str(repmat(x,1,x)),design,'uni',false);
            design(:,2)=regexprep(design(:,2),'\d*','T');
            design(:,1)=regexprep(design(:,1),'\d*','S');
            design= regexprep(design,'[^\w'']','');
        if fill==1
            data=repmat(char('x'),sum(2.^(nAttempts-1)),nAttempts-1);
            y=1;
            for i=1:size(design,1)
                x=unique(perms([design{i,:}]),'rows');
                data((y):(y+size(x,1)-1),:)=x;
                y=find(data=='x',1);
            end
            data=[ repmat('S',size(data,1),1) data repmat('T',size(data,1),nFinal_tests) ];
            data=reshape(cellstr(data(:)),size(data,1),size(data,2));
            set(handles.data_table,'Data',data);
            set(handles.data_table,'ColumnName',[repmat({'Practice'},1,nAttempts),repmat({'Final'},1,nFinal_tests)]);
            rows=1:size(data,1);
            set(handles.data_table,'RowName',cellstr([repmat('Cond. ',size(data,1),1) (num2str(rows(:))) ])' );
        end
        design(cellfun(@(x) strcmp(x,''),design)) =[];
        design=[{'S','S'}' design(:)];
        end


    function fit_panel_SelectionChangeFcn(hObject, eventdata, handles)
        % hObject    handle to the selected object in uipanel1 
        % eventdata  structure with the following fields (see UIBUTTONGROUP)
        %	EventName: string 'SelectionChanged' (read only)
        %	OldValue: handle of the previously selected object or empty
        %	NewValue: handle of the currently selected object
        % handles    structure with handles and user data (see GUIDATA)
        switch get(eventdata.NewValue,'Tag') % Get Tag of selected object.
            case 'radiobutton1'
                display('Radio button 1');
            case 'radiobutton2'
                display('Radio button 2');
            case 'togglebutton1'
                display('Toggle button 1');
            case 'togglebutton2'
                display('Toggle button 2');
        end

