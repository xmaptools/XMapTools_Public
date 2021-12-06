function varargout = XTTdisplayCompositions(varargin)
% XTTDISPLAYCOMPOSITIONS MATLAB code for XTTdisplayCompositions.fig
%      XTTDISPLAYCOMPOSITIONS, by itself, creates a new XTTDISPLAYCOMPOSITIONS or raises the existing
%      singleton*.
%
%      H = XTTDISPLAYCOMPOSITIONS returns the handle to a new XTTDISPLAYCOMPOSITIONS or the handle to
%      the existing singleton*.
%
%      XTTDISPLAYCOMPOSITIONS('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in XTTDISPLAYCOMPOSITIONS.M with the given input arguments.
%
%      XTTDISPLAYCOMPOSITIONS('Property','Value',...) creates a new XTTDISPLAYCOMPOSITIONS or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before XTTdisplayCompositions_OpeningFcn gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to XTTdisplayCompositions_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help XTTdisplayCompositions

% Last Modified by GUIDE v2.5 30-Jul-2013 14:40:36

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @XTTdisplayCompositions_OpeningFcn, ...
                   'gui_OutputFcn',  @XTTdisplayCompositions_OutputFcn, ...
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


% --- Executes just before XTTdisplayCompositions is made visible.
function XTTdisplayCompositions_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to XTTdisplayCompositions (see VARARGIN)

% Choose default command line output for XTTdisplayCompositions
handles.output = hObject;

% Update handles structure
guidata(hObject, handles);

% UIWAIT makes XTTdisplayCompositions wait for user response (see UIRESUME)
% uiwait(handles.figure1);


% --- Outputs from this function are returned to the command line.
function varargout = XTTdisplayCompositions_OutputFcn(hObject, eventdata, handles) 
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output;


% --- Executes on button press in ButtonApplyAndExit.
function ButtonApplyAndExit_Callback(hObject, eventdata, handles)
% hObject    handle to ButtonApplyAndExit (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --- Executes on selection change in PopUpMinerals.
function PopUpMinerals_Callback(hObject, eventdata, handles)
% hObject    handle to PopUpMinerals (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = cellstr(get(hObject,'String')) returns PopUpMinerals contents as cell array
%        contents{get(hObject,'Value')} returns selected item from PopUpMinerals


% --- Executes during object creation, after setting all properties.
function PopUpMinerals_CreateFcn(hObject, eventdata, handles)
% hObject    handle to PopUpMinerals (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on selection change in PopUpAnalysis.
function PopUpAnalysis_Callback(hObject, eventdata, handles)
keyboard 


% --- Executes during object creation, after setting all properties.
function PopUpAnalysis_CreateFcn(hObject, eventdata, handles)
% hObject    handle to PopUpAnalysis (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end
