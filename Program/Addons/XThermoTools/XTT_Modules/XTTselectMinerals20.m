function varargout = XTTselectMinerals20(varargin)
% XTTSELECTMINERALS20 MATLAB code for XTTselectMinerals20.fig
%      XTTSELECTMINERALS20, by itself, creates a new XTTSELECTMINERALS20 or raises the existing
%      singleton*.
%
%      H = XTTSELECTMINERALS20 returns the handle to a new XTTSELECTMINERALS20 or the handle to
%      the existing singleton*.
%
%      XTTSELECTMINERALS20('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in XTTSELECTMINERALS20.M with the given input arguments.
%
%      XTTSELECTMINERALS20('Property','Value',...) creates a new XTTSELECTMINERALS20 or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before XTTselectMinerals20_OpeningFcn gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to XTTselectMinerals20_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help XTTselectMinerals20

% Last Modified by GUIDE v2.5 04-Jul-2019 11:52:26

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @XTTselectMinerals20_OpeningFcn, ...
                   'gui_OutputFcn',  @XTTselectMinerals20_OutputFcn, ...
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


% --- Executes just before XTTselectMinerals20 is made visible.
function XTTselectMinerals20_OpeningFcn(hObject, eventdata, handles, varargin)

set(gcf,'Name',['XThermoTools']);

handles.ListMin = varargin{1};
handles.Selected = varargin{2};
handles.Available = varargin{3};

Message = varargin{end};
set(handles.text1,'String',Message);

guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles);


% Center this window
if length(varargin) > 4
    GuiXTT = varargin{4};
    % Find position of XTTselectMinerals20
    PositionXTT = get(GuiXTT,'Position');
    Unit = get(hObject,'Unit');
    set(hObject,'Unit','pixels');
    Position = get(hObject,'Position');
    NewPosition = Position;
    NewPosition(1) = PositionXTT(1)+PositionXTT(3)-Position(3);
    NewPosition(2) = PositionXTT(2)+PositionXTT(4)-Position(4);
    set(hObject,'Position',NewPosition);
    set(hObject,'Unit',Unit);
else
    
    Unit = get(hObject,'Unit');
    set(hObject,'Unit','normalized');
    Position = get(hObject,'Position');
    NewPosition = Position;
    NewPosition(1) = (1-Position(3))/2;
    NewPosition(2) = (1-Position(4))/2;
    set(hObject,'Position',NewPosition);
    set(hObject,'Unit',Unit);
end

handles.gcf = gcf;


% Choose default command line output for XTTselectMinerals20
handles.output1 = handles.Selected;

% Update handles structure
guidata(hObject, handles);

% UIWAIT makes XTTselectMinerals20 wait for user response (see UIRESUME)
uiwait(handles.XTTselectMinerals20);


% --- Outputs from this function are returned to the command line.
function varargout = XTTselectMinerals20_OutputFcn(hObject, eventdata, handles) 
% Get default command line output from handles structure
varargout{1} = handles.output1;
delete(hObject);
return



% --- Executes on button press in pushbutton1.
function pushbutton1_Callback(hObject, eventdata, handles)

ListMin = handles.ListMin;
Selected = handles.Selected;

for i=1:length(Selected)
    eval(['Selected(i) = get(handles.togglebutton',char(num2str(i)),',','''Value'');'])
end

handles.output1 = Selected;
guidata(hObject, handles);

close(handles.gcf);
return


function UpdateDisplay(hObject, eventdata, handles)

ListMin = handles.ListMin;
Selected = handles.Selected;
Available = handles.Available;

for i=1:20
    if i <= length(handles.ListMin)
        
        eval(['set(handles.togglebutton',char(num2str(i)),',','''Visible''',',','''on'',','''String'',''',char(ListMin{i}),''');']);
        
        if Selected(i)
            eval(['set(handles.togglebutton',char(num2str(i)),',','''Value'',1);'])
        else
            eval(['set(handles.togglebutton',char(num2str(i)),',','''Value'',0);'])
        end
        
        if ~Available(i)
            eval(['set(handles.togglebutton',char(num2str(i)),',','''Enable''',',','''off''',');'])
        end
        
    else        
        eval(['set(handles.togglebutton',char(num2str(i)),',','''Visible''',',','''off''',');']);

    end
end

drawnow
guidata(hObject, handles);

return






% --- Executes on button press in togglebutton1.
function togglebutton1_Callback(hObject, eventdata, handles)







% --- Executes on button press in togglebutton2.
function togglebutton2_Callback(hObject, eventdata, handles)
% hObject    handle to togglebutton2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of togglebutton2


% --- Executes on button press in togglebutton3.
function togglebutton3_Callback(hObject, eventdata, handles)
% hObject    handle to togglebutton3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of togglebutton3


% --- Executes on button press in togglebutton4.
function togglebutton4_Callback(hObject, eventdata, handles)
% hObject    handle to togglebutton4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of togglebutton4


% --- Executes on button press in togglebutton5.
function togglebutton5_Callback(hObject, eventdata, handles)
% hObject    handle to togglebutton5 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of togglebutton5


% --- Executes on button press in togglebutton6.
function togglebutton6_Callback(hObject, eventdata, handles)
% hObject    handle to togglebutton6 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of togglebutton6


% --- Executes on button press in togglebutton7.
function togglebutton7_Callback(hObject, eventdata, handles)
% hObject    handle to togglebutton7 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of togglebutton7


% --- Executes on button press in togglebutton8.
function togglebutton8_Callback(hObject, eventdata, handles)
% hObject    handle to togglebutton8 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of togglebutton8


% --- Executes on button press in togglebutton9.
function togglebutton9_Callback(hObject, eventdata, handles)
% hObject    handle to togglebutton9 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of togglebutton9


% --- Executes on button press in togglebutton10.
function togglebutton10_Callback(hObject, eventdata, handles)
% hObject    handle to togglebutton10 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of togglebutton10


% --- Executes on button press in togglebutton11.
function togglebutton11_Callback(hObject, eventdata, handles)
% hObject    handle to togglebutton11 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of togglebutton11


% --- Executes on button press in togglebutton12.
function togglebutton12_Callback(hObject, eventdata, handles)
% hObject    handle to togglebutton12 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of togglebutton12


% --- Executes on button press in togglebutton13.
function togglebutton13_Callback(hObject, eventdata, handles)
% hObject    handle to togglebutton13 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of togglebutton13


% --- Executes on button press in togglebutton14.
function togglebutton14_Callback(hObject, eventdata, handles)
% hObject    handle to togglebutton14 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of togglebutton14


% --- Executes on button press in togglebutton15.
function togglebutton15_Callback(hObject, eventdata, handles)
% hObject    handle to togglebutton15 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of togglebutton15


% --- Executes on button press in togglebutton16.
function togglebutton16_Callback(hObject, eventdata, handles)
% hObject    handle to togglebutton16 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of togglebutton16


% --- Executes on button press in togglebutton17.
function togglebutton17_Callback(hObject, eventdata, handles)
% hObject    handle to togglebutton17 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of togglebutton17


% --- Executes on button press in togglebutton18.
function togglebutton18_Callback(hObject, eventdata, handles)
% hObject    handle to togglebutton18 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of togglebutton18


% --- Executes on button press in togglebutton19.
function togglebutton19_Callback(hObject, eventdata, handles)
% hObject    handle to togglebutton19 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of togglebutton19


% --- Executes on button press in togglebutton20.
function togglebutton20_Callback(hObject, eventdata, handles)
% hObject    handle to togglebutton20 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of togglebutton20


% --- Executes when user attempts to close XTTselectMinerals20.
function XTTselectMinerals20_CloseRequestFcn(hObject, eventdata, handles)
if isequal(get(hObject, 'waitstatus'),'waiting')
    uiresume(hObject);
else
    delete(hObject);
end
return


% --- Executes on button press in SelectALL.
function SelectALL_Callback(hObject, eventdata, handles)
%
Selected = handles.Selected;
Available = handles.Available;

IdxAvailable = find(Available);
Selected(IdxAvailable) = ones(size(IdxAvailable));


handles.Selected = Selected;
guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles);
return



% --- Executes on button press in SelectNONE.
function SelectNONE_Callback(hObject, eventdata, handles)
%
Selected = handles.Selected;
Available = handles.Available;

IdxAvailable = find(Available);
Selected(IdxAvailable) = zeros(size(IdxAvailable));


handles.Selected = Selected;
guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles);
return

