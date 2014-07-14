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

% Last Modified by GUIDE v2.5 15-Jul-2014 00:33:17

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
data=cell(['']);
if exist([pwd,'\starting_params.mat'],'file')
   p = open([pwd,'\starting_params.mat']);
end


% Choose default command line output for gui
handles.output = hObject;

% Update handles structure
guidata(hObject, handles);

% UIWAIT makes gui wait for user response (see UIRESUME)
% uiwait(handles.figure1);


% --- Outputs from this function are returned to the command line.
function varargout = gui_OutputFcn(hObject, eventdata, handles) 
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output;


function rho_param_Callback(hObject, eventdata, handles)

rho= str2double(get(handles.rho_param,'String'));
validate(rho,handles,1);

% --- Executes during object creation, after setting all properties.
function rho_param_CreateFcn(hObject, eventdata, handles)
% hObject    handle to rho_param (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function k_param_Callback(hObject, eventdata, handles)
valid=validate(hObject,handles,1);
if ~valid
   uicontrol(hObject)
end


% --- Executes during object creation, after setting all properties.
function k_param_CreateFcn(hObject, eventdata, handles)
% hObject    handle to k_param (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function nSubs_param_Callback(hObject, eventdata, handles)
valid=validate(hObject,handles,1);
if ~valid
   uicontrol(hObject)
end
% --- Executes during object creation, after setting all properties.
function nSubs_param_CreateFcn(hObject, eventdata, handles)
% hObject    handle to nSubs_param (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function nItems_param_Callback(hObject, eventdata, handles)
valid=validate(hObject,handles,1);
if ~valid
   uicontrol(hObject)
end


% --- Executes during object creation, after setting all properties.
function nItems_param_CreateFcn(hObject, eventdata, handles)
% hObject    handle to nItems_param (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

function nAttempts_Callback(hObject, eventdata, handles)

[valid, nPractice_tests] =validate(hObject,handles,1);
if valid
    prac=(1:nPractice_tests)-1;
    prac=num2str(prac(:))';
    sep=repmat(',',1,length(prac));
    tests =reshape(reshape([prac(:),sep(:)],2*size(prac,1),[])',1,[]); 
    tests(end)=[];
    set(handles.nPractice_Tests,'String',tests);
    make_data(handles)
else
    uicontrol(hObject)
end

% --- Executes during object creation, after setting all properties.
function nAttempts_CreateFcn(hObject, eventdata, handles)
% hObject    handle to nAttempts (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function nPractice_Tests_Callback(hObject, eventdata, handles)
% nPractice_Tests= str2double(strsplit(get(handles.nPractice_Tests,'String'),'\s*,\s*','DelimiterType','RegularExpression'));
[valid, nPractice_tests] =validate(hObject,handles);
if valid
    if max(nPractice_tests) < str2double(get(handles.nAttempts,'String'))
        set(handles.nAttempts,'String',num2str(max(nPractice_tests)+1));
    end    
make_data(handles)
else
    uicontrol(hObject)
end

% --- Executes during object creation, after setting all properties.
function nPractice_Tests_CreateFcn(hObject, eventdata, handles)
% hObject    handle to nPractice_Tests (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

% --------------------------------------------------------------------
function fit_panel_ButtonDownFcn(hObject, eventdata, handles)
% hObject    handle to fit_panel (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% --- Executes when selected object is changed in fit_panel.

function fit_panel_SelectionChangeFcn(hObject, eventdata, handles)
% hObject    handle to the selected object in fit_panel 
% eventdata  structure with the following fields (see UIBUTTONGROUP)
%	EventName: string 'SelectionChanged' (read only)
%	OldValue: handle of the previously selected object or empty if none was selected
%	NewValue: handle of the currently selected object
% handles    structure with handles and user data (see GUIDATA)
button=get(eventdata.NewValue,'Tag');
switch button
    case fit_fmin
    fit='fit';
    case check
    fit='check';
end    

% --- Executes on button press in run_button.
function run_button_Callback(hObject, eventdata, handles,fit,S_params,R_params,O_params,data,design,k,nSub,nItems,varargin)
% hObject    handle to run_button (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

h=findobj('-regexp','Tag','\w*_param');
p=get(h,'Tag');
v=get(h,'String');
h=findobj('-regexp','Tag','\w*_fix');
f=get(h,'Value');
params={p v f};


if strcmpi(fit,'fit')
    [fitted_params, chisquare, ~ , info]=fminsearch(@(x) fit_SAM_RL_Sim(x,data,design,k,nSub,nItems),params,fmin_opts) %#ok<NASGU,NOPRT,*ASGLU>
elseif strcmp(fit,'check')
    chisquare=fit_SAM_RL_Sim(params,data,design,k,nSub,nItems,plotting) %#ok<NASGU,NOPRT>
end 
%quit

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
% if ~valid
%     error('gui:validinput','Invalid input %d for %s',x,get(hobject

function nFinal_tests_Callback(hObject, eventdata, handles)

[valid, nPractice_tests] =validate(hObject,handles);
if valid
make_data(handles)
else
    uicontrol(hObject)
end

% --- Executes during object creation, after setting all properties.
function nFinal_tests_CreateFcn(hObject, eventdata, handles)
% hObject    handle to nFinal_tests (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
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


% --- Executes on button press in S2_fix.
function S2_fix_Callback(hObject, eventdata, handles)
% hObject    handle to S2_fix (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of S2_fix



function S2_param_Callback(hObject, eventdata, handles)
% hObject    handle to S2_param (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of S2_param as text
%        str2double(get(hObject,'String')) returns contents of S2_param as a double


% --- Executes during object creation, after setting all properties.
function S2_param_CreateFcn(hObject, eventdata, handles)
% hObject    handle to S2_param (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function S1_param_Callback(hObject, eventdata, handles)
% hObject    handle to S1_param (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of S1_param as text
%        str2double(get(hObject,'String')) returns contents of S1_param as a double


% --- Executes during object creation, after setting all properties.
function S1_param_CreateFcn(hObject, eventdata, handles)
% hObject    handle to S1_param (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in S1_fix.
function S1_fix_Callback(hObject, eventdata, handles)
% hObject    handle to S1_fix (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of S1_fix


% --- Executes on button press in S_add.
function S_add_Callback(hObject, eventdata, handles)
% hObject    handle to S_add (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --- Executes on button press in R_add.
function R_add_Callback(hObject, eventdata, handles)
% hObject    handle to R_add (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --- Executes on button press in Rcor_fix.
function Rcor_fix_Callback(hObject, eventdata, handles)
% hObject    handle to Rcor_fix (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of Rcor_fix



function Rcor_param_Callback(hObject, eventdata, handles)
% hObject    handle to Rcor_param (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of Rcor_param as text
%        str2double(get(hObject,'String')) returns contents of Rcor_param as a double


% --- Executes during object creation, after setting all properties.
function Rcor_param_CreateFcn(hObject, eventdata, handles)
% hObject    handle to Rcor_param (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function R_param_Callback(hObject, eventdata, handles)
% hObject    handle to R_param (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of R_param as text
%        str2double(get(hObject,'String')) returns contents of R_param as a double


% --- Executes during object creation, after setting all properties.
function R_param_CreateFcn(hObject, eventdata, handles)
% hObject    handle to R_param (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in R_fix.
function R_fix_Callback(hObject, eventdata, handles)
% hObject    handle to R_fix (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of R_fix


% --- Executes on button press in O_add.
function O_add_Callback(hObject, eventdata, handles)
% hObject    handle to O_add (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --- Executes on button press in O2_fix.
function O2_fix_Callback(hObject, eventdata, handles)
% hObject    handle to O2_fix (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of O2_fix



function O2_param_Callback(hObject, eventdata, handles)
[valid, O2]=validate(hObject,handles,1);
if ~valid
   uicontrol(hObject)
end

if 02 < str2double(get(handles.O1_param,'String'));
    set(hObject,'String',get(handles.O1_param,'String'))
end

% --- Executes during object creation, after setting all properties.
function O2_param_CreateFcn(hObject, eventdata, handles)
% hObject    handle to O2_param (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
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
% --- Executes during object creation, after setting all properties.
function O1_param_CreateFcn(hObject, eventdata, handles)
% hObject    handle to O1_param (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in O1_fix.
function O1_fix_Callback(hObject, eventdata, handles)
% hObject    handle to O1_fix (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of O1_fix
function make_data(handles)
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

% --- Executes on button press in rho_fix.
function rho_fix_Callback(hObject, eventdata, handles)
% hObject    handle to rho_fix (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of rho_fix