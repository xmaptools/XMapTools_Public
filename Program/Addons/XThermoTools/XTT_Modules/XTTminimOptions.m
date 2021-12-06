function varargout = XTTminimOptions(varargin)
%XTTMINIMOPTIONS M-file for XTTminimOptions.fig
%      XTTMINIMOPTIONS, by itself, creates a new XTTMINIMOPTIONS or raises the existing
%      singleton*.
%
%      H = XTTMINIMOPTIONS returns the handle to a new XTTMINIMOPTIONS or the handle to
%      the existing singleton*.
%
%      XTTMINIMOPTIONS('Property','Value',...) creates a new XTTMINIMOPTIONS using the
%      given property value pairs. Unrecognized properties are passed via
%      varargin to XTTminimOptions_OpeningFcn.  This calling syntax produces a
%      warning when there is an existing singleton*.
%
%      XTTMINIMOPTIONS('CALLBACK') and XTTMINIMOPTIONS('CALLBACK',hObject,...) call the
%      local function named CALLBACK in XTTMINIMOPTIONS.M with the given input
%      arguments.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help XTTminimOptions

% Last Modified by GUIDE v2.5 15-Jul-2020 15:17:59

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @XTTminimOptions_OpeningFcn, ...
                   'gui_OutputFcn',  @XTTminimOptions_OutputFcn, ...
                   'gui_LayoutFcn',  [], ...
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


% --- Executes just before XTTminimOptions is made visible.
function XTTminimOptions_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   unrecognized PropertyName/PropertyValue pairs from the
%            command line (see VARARGIN)

% Load MinimOptions.mat

Where = which('MinimOptions.mat');

load(Where);

handles.MinimOptions = MinimOptions;
handles.Where = Where;

% Position
Unit = get(hObject,'Unit');
set(hObject,'Unit','normalized');
Position = get(hObject,'Position');
NewPosition = Position;
NewPosition(1) = (1-Position(3))/2;
NewPosition(2) = (1-Position(4))/2;
set(hObject,'Position',NewPosition);
set(hObject,'Unit',Unit);


guidata(hObject, handles);
UPDATE_displayFromMinimOptions(hObject, eventdata, handles);


% Choose default command line output for XTTminimOptions
handles.output = hObject;

% Update handles structure
guidata(hObject, handles);

return



% --- Outputs from this function are returned to the command line.
function varargout = XTTminimOptions_OutputFcn(hObject, eventdata, handles)
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output;


% --- Executes when user attempts to close figure1.
function figure1_CloseRequestFcn(hObject, eventdata, handles)
% hObject    handle to figure1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: delete(hObject) closes the figure
delete(hObject);



function UPDATE_displayFromMinimOptions(hObject, eventdata, handles);
%

MinimOptions = handles.MinimOptions;

% Use the following alternative model
set(handles.CheckAutoMethod,'Value',MinimOptions.Weights.Use)

set(handles.ListMethMinim,'String',MinimOptions.Weights.ListAutoWts,'Value',MinimOptions.Weights.Selected);

set(handles.FieldWt1,'String',num2str(MinimOptions.Weights.Values(MinimOptions.Weights.Selected(1),1)));
set(handles.FieldWt2,'String',num2str(MinimOptions.Weights.Values(MinimOptions.Weights.Selected(1),2)));
set(handles.FieldWt3,'String',num2str(MinimOptions.Weights.Values(MinimOptions.Weights.Selected(1),3)));

if get(handles.CheckAutoMethod,'Value')
    set(handles.ListMethMinim,'Enable','on')
    set(handles.FieldWt1,'Enable','on')
    set(handles.FieldWt2,'Enable','on')
    set(handles.FieldWt3,'Enable','on')
    set(handles.pushbutton9,'Enable','on')
    set(handles.ButtonRestoreDef,'Enable','on')
else
    set(handles.ListMethMinim,'Enable','off')
    set(handles.FieldWt1,'Enable','off')
    set(handles.FieldWt2,'Enable','off')
    set(handles.FieldWt3,'Enable','off')
    set(handles.pushbutton9,'Enable','off')
    set(handles.ButtonRestoreDef,'Enable','off')
end



set(handles.ListMethOptim,'String',MinimOptions.Search.MethList);

set(handles.FieldSymplexNbSteps,'String',num2str(MinimOptions.Search.Symplex.NbSteps))

switch MinimOptions.Search.Symplex.FirstOpt
    case 1
        set(handles.RadioOpt1_Prem,'Value',1);
        set(handles.RadioOpt1_Bing,'Value',0);
    case 2
        set(handles.RadioOpt1_Bing,'Value',1);
        set(handles.RadioOpt1_Prem,'Value',0);
end

set(handles.NbPerm1,'String',MinimOptions.Uncertainty.NbPerm1);
set(handles.NbPerm2,'String',MinimOptions.Uncertainty.NbPerm2);

if isequal(MinimOptions.TestMode,1)
    set(handles.SelectTestMode,'Value',1);
else
    set(handles.SelectTestMode,'Value',0);
end

return





% --- Executes on button press in Default.
function Default_Callback(hObject, eventdata, handles)
% hObject    handle to Default (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --- Executes on button press in Apply.
function Apply_Callback(hObject, eventdata, handles)
% 

MinimOptions = handles.MinimOptions;
Where = handles.Where;

disp(' ')
disp('  >> Update MinimOptions.mat')

save(Where,'MinimOptions');

disp('  >> Done (new file saved)')
disp(' ')

close(gcf)
return

% --- Executes on selection change in ListMethOptim.
function ListMethOptim_Callback(hObject, eventdata, handles)
% hObject    handle to ListMethOptim (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = cellstr(get(hObject,'String')) returns ListMethOptim contents as cell array
%        contents{get(hObject,'Value')} returns selected item from ListMethOptim


% --- Executes during object creation, after setting all properties.
function ListMethOptim_CreateFcn(hObject, eventdata, handles)
% hObject    handle to ListMethOptim (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in CheckAutoMethod.
function CheckAutoMethod_Callback(hObject, eventdata, handles)
% 
handles.MinimOptions.Weights.Use = get(hObject,'Value');
guidata(hObject, handles);
UPDATE_displayFromMinimOptions(hObject, eventdata, handles);
return


function FieldWt1_Callback(hObject, eventdata, handles)
% hObject    handle to FieldWt1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of FieldWt1 as text
%        str2double(get(hObject,'String')) returns contents of FieldWt1 as a double


% --- Executes during object creation, after setting all properties.
function FieldWt1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to FieldWt1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function FieldWt2_Callback(hObject, eventdata, handles)
% hObject    handle to FieldWt2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of FieldWt2 as text
%        str2double(get(hObject,'String')) returns contents of FieldWt2 as a double


% --- Executes during object creation, after setting all properties.
function FieldWt2_CreateFcn(hObject, eventdata, handles)
% hObject    handle to FieldWt2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function FieldWt3_Callback(hObject, eventdata, handles)
% hObject    handle to FieldWt3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of FieldWt3 as text
%        str2double(get(hObject,'String')) returns contents of FieldWt3 as a double


% --- Executes during object creation, after setting all properties.
function FieldWt3_CreateFcn(hObject, eventdata, handles)
% hObject    handle to FieldWt3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in pushbutton9.
function pushbutton9_Callback(hObject, eventdata, handles)
%

w(1) = str2num(get(handles.FieldWt1,'String'));
w(2) = str2num(get(handles.FieldWt2,'String'));
w(3) = str2num(get(handles.FieldWt3,'String'));

w = w/sum(w);

set(handles.FieldWt1,'String',num2str(w(1)));
set(handles.FieldWt2,'String',num2str(w(2)));
set(handles.FieldWt3,'String',num2str(w(3)));

MinimOptions = handles.MinimOptions;

MinimOptions.Weights.ListAutoWts{end+1} = ['Additional configuration: ',num2str(sprintf('%.2f',w(1))),'/',num2str(sprintf('%.2f',w(2))),'/',num2str(sprintf('%.2f',w(3)))]
MinimOptions.Weights.Values(end+1,:) = w;
MinimOptions.Weights.Selected = length(MinimOptions.Weights.ListAutoWts);

handles.MinimOptions = MinimOptions;
guidata(hObject, handles);
UPDATE_displayFromMinimOptions(hObject, eventdata, handles);

return




% --- Executes on selection change in ListMethMinim.
function ListMethMinim_Callback(hObject, eventdata, handles)
%
handles.MinimOptions.Weights.Selected = get(hObject,'Value');
guidata(hObject, handles);
UPDATE_displayFromMinimOptions(hObject, eventdata, handles);
return

% --- Executes during object creation, after setting all properties.
function ListMethMinim_CreateFcn(hObject, eventdata, handles)
% hObject    handle to ListMethMinim (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in RadioOpt1_Prem.
function RadioOpt1_Prem_Callback(hObject, eventdata, handles)
% 
handles.MinimOptions.Search.Symplex.FirstOpt = 1;
guidata(hObject, handles);
UPDATE_displayFromMinimOptions(hObject, eventdata, handles);
return

% --- Executes on button press in RadioOpt1_Bing.
function RadioOpt1_Bing_Callback(hObject, eventdata, handles)
% 
handles.MinimOptions.Search.Symplex.FirstOpt = 2;
guidata(hObject, handles);
UPDATE_displayFromMinimOptions(hObject, eventdata, handles);
return


function FieldSymplexNbSteps_Callback(hObject, eventdata, handles)
% 
handles.MinimOptions.Search.Symplex.NbSteps = num2str(get(hObject,'String'));
guidata(hObject, handles);
UPDATE_displayFromMinimOptions(hObject, eventdata, handles);
return


% --- Executes during object creation, after setting all properties.
function FieldSymplexNbSteps_CreateFcn(hObject, eventdata, handles)
% hObject    handle to FieldSymplexNbSteps (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function NbPerm1_Callback(hObject, eventdata, handles)
handles.MinimOptions.Uncertainty.NbPerm1 = num2str(get(hObject,'String'));
guidata(hObject, handles);
UPDATE_displayFromMinimOptions(hObject, eventdata, handles);
return

% --- Executes during object creation, after setting all properties.
function NbPerm1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to NbPerm1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function NbPerm2_Callback(hObject, eventdata, handles)
handles.MinimOptions.Uncertainty.NbPerm2 = num2str(get(hObject,'String'));
guidata(hObject, handles);
UPDATE_displayFromMinimOptions(hObject, eventdata, handles);
return

% --- Executes during object creation, after setting all properties.
function NbPerm2_CreateFcn(hObject, eventdata, handles)
% hObject    handle to NbPerm2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in SelectTestMode.
function SelectTestMode_Callback(hObject, eventdata, handles)

MinimOptions = handles.MinimOptions;

if isequal(get(handles.SelectTestMode,'Value'),1)
    MinimOptions.TestMode = 1;
else
    MinimOptions.TestMode = 0;
end

handles.MinimOptions = MinimOptions;
guidata(hObject, handles);
UPDATE_displayFromMinimOptions(hObject, eventdata, handles);
return


% --- Executes on button press in ButtonRestoreDef.
function ButtonRestoreDef_Callback(hObject, eventdata, handles)
%
MinimOptions = handles.MinimOptions;

ButtonName = questdlg({'This action will delete all your additional configurations','Continue?'},'XThermoTools','Yes');
switch ButtonName,
    case 'Yes',
        MinimOptions.Weights.ListAutoWts = MinimOptions.Weights.ListAutoWts(1:5);
        MinimOptions.Weights.Values = MinimOptions.Weights.Values(1:5,:);
        MinimOptions.Weights.Selected = 1;
        
        handles.MinimOptions = MinimOptions;
        guidata(hObject, handles);
        UPDATE_displayFromMinimOptions(hObject, eventdata, handles);
end % switch

return
