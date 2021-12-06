function varargout = XTTselectElem(varargin)
% XTTSELECTELEM MATLAB code for XTTselectElem.fig
%      XTTSELECTELEM, by itself, creates a new XTTSELECTELEM or raises the existing
%      singleton*.
%
%      H = XTTSELECTELEM returns the handle to a new XTTSELECTELEM or the handle to
%      the existing singleton*.
%
%      XTTSELECTELEM('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in XTTSELECTELEM.M with the given input arguments.
%
%      XTTSELECTELEM('Property','Value',...) creates a new XTTSELECTELEM or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before XTTselectElem_OpeningFcn gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to XTTselectElem_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help XTTselectElem

% Last Modified by GUIDE v2.5 04-Jul-2013 15:59:58

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @XTTselectElem_OpeningFcn, ...
                   'gui_OutputFcn',  @XTTselectElem_OutputFcn, ...
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


% --- Executes just before XTTselectElem is made visible.
function XTTselectElem_OpeningFcn(hObject, eventdata, handles, varargin)


ElData = varargin{1};
handles.ElData = ElData;
guidata(hObject, handles);

%First setting:

for i=1:35   % size of the figure
    if i <= length(ElData.selected)
        eval(['set(handles.EL',num2str(i),',''String'',char(ElData.listElem{i}))']);
        eval(['set(handles.EL',num2str(i),',''Value'',ElData.selected(i))']);
    
    else
        eval(['set(handles.EL',num2str(i),',''Visible'',''off'')']);
    end
end


functionUpdateElData(handles);

%keyboard

% Center this window
Unit = get(hObject,'Unit');
set(hObject,'Unit','normalized');
Position = get(hObject,'Position');
NewPosition = Position;
NewPosition(1) = (1-Position(3))/2;
NewPosition(2) = (1-Position(4))/2;
set(hObject,'Position',NewPosition);
set(hObject,'Unit',Unit);

%keyboard


handles.gcf = gcf;

handles.output = [0];

% Update handles structure
guidata(hObject, handles);

% UIWAIT makes ModSelectElem wait for user response (see UIRESUME)
uiwait(hObject);
return



function [] = functionUpdateElData(handles)

ElData = handles.ElData;

StrChar = '';

for i=1:length(ElData.selected)
    eval(['Etat = get(handles.EL',num2str(i),',''Value'');']);
    if Etat
        StrChar = [StrChar,' - ',char(ElData.listElem{i})];
    end
end

StrChar = StrChar(4:end);

set(handles.chemicalSystem,'String',StrChar);
    
return


% --- Outputs from this function are returned to the command line.
function varargout = XTTselectElem_OutputFcn(hObject, eventdata, handles) 
%uiresume(handles.figure1)

varargout{1} = handles.output;

delete(hObject)
return


function figure1_WindowButtonDownFcn(hObject, eventdata, handles)
return


function figure1_CloseRequestFcn(hObject, eventdata, handles)
if isequal(get(hObject, 'waitstatus'),'waiting')
    uiresume(hObject);
else
    delete(hObject);
end
return


% --- Executes on button press in ButtonOK.
function ButtonOK_Callback(hObject, eventdata, handles)

ElData = handles.ElData;

for i=1:length(ElData.selected)
    eval(['Etat(i) = get(handles.EL',num2str(i),',''Value'');']);
end

handles.output = Etat;
guidata(hObject, handles);


%h = ModSelecElem.m
close(handles.gcf);
return


% --- Executes on button press in pushbutton1.
function pushbutton1_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL1.
function EL1_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL2.
function EL2_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL3.
function EL3_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL4.
function EL4_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL5.
function EL5_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL6.
function EL6_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL7.
function EL7_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL8.
function EL8_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL9.
function EL9_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL10.
function EL10_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL11.
function EL11_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL12.
function EL12_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL13.
function EL13_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL14.
function EL14_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL15.
function EL15_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL16.
function EL16_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL17.
function EL17_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL18.
function EL18_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL19.
function EL19_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL20.
function EL20_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL21.
function EL21_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL22.
function EL22_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL23.
function EL23_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL24.
function EL24_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL25.
function EL25_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL26.
function EL26_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL27.
function EL27_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL28.
function EL28_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL29.
function EL29_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL30.
function EL30_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL31.
function EL31_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL32.
function EL32_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL33.
function EL33_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL34.
function EL34_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return


% --- Executes on button press in EL35.
function EL35_Callback(hObject, eventdata, handles)
functionUpdateElData(handles);
return
