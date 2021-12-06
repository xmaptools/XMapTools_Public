function varargout = XTTsetElMap(varargin)
% XTTSETELMAP MATLAB code for XTTsetElMap.fig
%      XTTSETELMAP, by itself, creates a new XTTSETELMAP or raises the existing
%      singleton*.
%
%      H = XTTSETELMAP returns the handle to a new XTTSETELMAP or the handle to
%      the existing singleton*.
%
%      XTTSETELMAP('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in XTTSETELMAP.M with the given input arguments.
%
%      XTTSETELMAP('Property','Value',...) creates a new XTTSETELMAP or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before XTTsetElMap_OpeningFcn gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to XTTsetElMap_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help XTTsetElMap

% Last Modified by GUIDE v2.5 18-Jul-2013 14:54:12

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @XTTsetElMap_OpeningFcn, ...
                   'gui_OutputFcn',  @XTTsetElMap_OutputFcn, ...
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


% --- Executes just before XTTsetElMap is made visible.
function XTTsetElMap_OpeningFcn(hObject, eventdata, handles, varargin)
%


ListMap = varargin{1};   %{'SIO2','AL2O3','FEO','FE2O3','K2O'};
DefElCodeDisp = varargin{2};  %{'MG#','MGO.(FEO+MGO)','MGO.(FEO+MGO)';'FE#','FEO./(FEO+MGO)','FEO./(FEO+MGO)'};
DefDisplay = varargin{3};  %{'1','1','SIO2'};
GuiXTT = varargin{4}; 

handles.DefDisplay = DefDisplay;

% 1/2/3 (selected)
% Num(1-2) or 0 (3)
% Code(1-2-3)

handles.ListMap = ListMap;
handles.DefElCodeDisp = DefElCodeDisp;

set(handles.popupmenu1,'String',ListMap);
for i=1:size(DefElCodeDisp,1)
    ListElCodeDisp{i} = DefElCodeDisp{i,2};
end
set(handles.popupmenu2,'String',ListElCodeDisp);
set(handles.edit1,'String',DefDisplay{3});

switch str2num(DefDisplay{1})
    case 1
        set(handles.popupmenu1,'Value',str2num(DefDisplay{2}));
    case 2
        set(handles.popupmenu2,'Value',str2num(DefDisplay{2})); 
end

drawnow

guidata(hObject, handles);
handles = WereWeAre(hObject, eventdata, handles);

guidata(hObject, handles);
handles.output1 = handles.Code4XThermoTools;
handles.output2 = handles.DefDisplay;

% Find position of XThermoTools
PositionXTT = get(GuiXTT,'Position');
Unit = get(hObject,'Unit');
set(hObject,'Unit','pixels');
Position = get(hObject,'Position');
NewPosition = Position;
NewPosition(1) = PositionXTT(1)+PositionXTT(3)-Position(3);
NewPosition(2) = PositionXTT(2)+PositionXTT(4)-Position(4);
set(hObject,'Position',NewPosition);
set(hObject,'Unit',Unit);

%keyboard

handles.gcf = gcf;

% Choose default command line output for XTTsetElMap
handles.output = hObject;

% Update handles structure
guidata(hObject, handles);

% UIWAIT makes ModSelectElem wait for user response (see UIRESUME)
uiwait(hObject);
return


function [handles] = WereWeAre(hObject, eventdata, handles)

DefDisplay = handles.DefDisplay;

%set(handles.checkbox1,'Enable','off');
%set(handles.checkbox2,'Enable','off');
%set(handles.checkbox3,'Enable','off');

switch str2num(DefDisplay{1})
    case 1
        set(handles.checkbox1,'Value',1);
        set(handles.checkbox2,'Value',0);
        set(handles.checkbox3,'Value',0);
        
        handles.Code4XThermoTools = handles.ListMap{get(handles.popupmenu1,'Value')};
        
        DefDisplay{2} = num2str(get(handles.popupmenu1,'Value'));
        DefDisplay{3} = handles.Code4XThermoTools;
        
    case 2
        set(handles.checkbox1,'Value',0);
        set(handles.checkbox2,'Value',1);
        set(handles.checkbox3,'Value',0);
        
        handles.Code4XThermoTools = handles.DefElCodeDisp{get(handles.popupmenu2,'Value'),2};
        
        DefDisplay{2} = num2str(get(handles.popupmenu2,'Value'));
        DefDisplay{3} = handles.Code4XThermoTools;
        
    case 3
        set(handles.checkbox1,'Value',0);
        set(handles.checkbox2,'Value',0);
        set(handles.checkbox3,'Value',1);
        
        handles.Code4XThermoTools = get(handles.edit1,'String');
        DefDisplay{2} = 0;
        DefDisplay{3} = handles.Code4XThermoTools;
end


handles.DefDisplay = DefDisplay;
drawnow
% Update handles structure
guidata(hObject, handles);
return


% --- Outputs from this function are returned to the command line.
function varargout = XTTsetElMap_OutputFcn(hObject, eventdata, handles) 
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output1;
varargout{2} = handles.output2;
delete(hObject);
return


% --- Executes on button press in checkbox1.
function checkbox1_Callback(hObject, eventdata, handles)
handles.DefDisplay{1} = '1'; 
WereWeAre(hObject, eventdata, handles);
return

% --- Executes on selection change in popupmenu1.
function popupmenu1_Callback(hObject, eventdata, handles)
handles.DefDisplay{1} = '1'; 
[handles] = WereWeAre(hObject, eventdata, handles);
ButtonOK_Callback(hObject, eventdata, handles)
return

% --- Executes during object creation, after setting all properties.
function popupmenu1_CreateFcn(hObject, eventdata, handles)
% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in checkbox2.
function checkbox2_Callback(hObject, eventdata, handles)
handles.DefDisplay{1} = '2'; 
WereWeAre(hObject, eventdata, handles);
return


% --- Executes on selection change in popupmenu2.
function popupmenu2_Callback(hObject, eventdata, handles)
handles.DefDisplay{1} = '2'; 
WereWeAre(hObject, eventdata, handles);
return


% --- Executes during object creation, after setting all properties.
function popupmenu2_CreateFcn(hObject, eventdata, handles)
% hObject    handle to popupmenu2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in checkbox3.
function checkbox3_Callback(hObject, eventdata, handles)
handles.DefDisplay{1} = '3'; 
WereWeAre(hObject, eventdata, handles);
return



function edit1_Callback(hObject, eventdata, handles)
handles.DefDisplay{1} = '3'; 
WereWeAre(hObject, eventdata, handles);
return


% --- Executes during object creation, after setting all properties.
function edit1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes when user attempts to close figure1.
function figure1_CloseRequestFcn(hObject, eventdata, handles)
if isequal(get(hObject, 'waitstatus'),'waiting')
    uiresume(hObject);
else
    delete(hObject);
end
return


function ButtonOK_Callback(hObject, eventdata, handles)
Code4XThermoTools = handles.Code4XThermoTools;

DefDisplay = handles.DefDisplay;

handles.output1 = Code4XThermoTools;
handles.output2 = DefDisplay;

guidata(hObject, handles);

close(handles.gcf);
return
