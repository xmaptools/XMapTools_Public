function varargout = XTTselectCompInAntidote(varargin)
% XTTSELECTCOMPINANTIDOTE MATLAB code for XTTselectCompInAntidote.fig
%      XTTSELECTCOMPINANTIDOTE, by itself, creates a new XTTSELECTCOMPINANTIDOTE or raises the existing
%      singleton*.
%
%      H = XTTSELECTCOMPINANTIDOTE returns the handle to a new XTTSELECTCOMPINANTIDOTE or the handle to
%      the existing singleton*.
%
%      XTTSELECTCOMPINANTIDOTE('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in XTTSELECTCOMPINANTIDOTE.M with the given input arguments.
%
%      XTTSELECTCOMPINANTIDOTE('Property','Value',...) creates a new XTTSELECTCOMPINANTIDOTE or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before XTTselectCompInAntidote_OpeningFcn gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to XTTselectCompInAntidote_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help XTTselectCompInAntidote

% Last Modified by GUIDE v2.5 08-Mar-2020 08:03:08

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @XTTselectCompInAntidote_OpeningFcn, ...
                   'gui_OutputFcn',  @XTTselectCompInAntidote_OutputFcn, ...
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


% --- Executes just before XTTselectCompInAntidote is made visible.
function XTTselectCompInAntidote_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
%

WorkVariXMap = varargin{1};
BinPhaseDef = varargin{2};

Compt = 0;
for i = 1:length(BinPhaseDef)
    if BinPhaseDef(i).IsPhaseIn
        Compt = Compt+1;
        Selection(Compt).name = BinPhaseDef(i).name;
        
        Selection(Compt).Where = find(ismember(WorkVariXMap.Names,BinPhaseDef(i).DBMinName));
        
        Selection(Compt).Idx = i;
        Selection(Compt).nbEl = length(BinPhaseDef(i).ListVariAntidote);
        
        Selection(Compt).IsSel = 1;
        Selection(Compt).IsElSel = ones(1,Selection(Compt).nbEl);
        
        Selection(Compt).ListVariAntidote = BinPhaseDef(i).ListVariAntidote; 
    end
end

handles.WorkVariXMap = WorkVariXMap;
handles.BinPhaseDef = BinPhaseDef;
handles.Selection = Selection;

NbMinMax = 8;
NbElMax = 8;

for i = 1:NbMinMax
    eval(['set(handles.SelPhase',num2str(i),',''Visible'',''off'');']);
    for j = 1:NbElMax
        eval(['set(handles.P',num2str(i),'El',num2str(j),',''Visible'',''off'');']);
        eval(['set(handles.DispP',num2str(i),'El',num2str(j),',''Visible'',''off'');']);
    end
end

handles = InitiateButtons(handles);

% Center this window
Unit = get(hObject,'Unit');
set(hObject,'Unit','normalized');
Position = get(hObject,'Position');
NewPosition = Position;
NewPosition(1) = (1-Position(3))/2;
NewPosition(2) = (1-Position(4))/2;
set(hObject,'Position',NewPosition);
set(hObject,'Unit',Unit);

handles.gcf = gcf;

% Choose default command line output for XTTselectCompInAntidote
handles.output = [0];

% Update handles structure
guidata(hObject, handles);

% UIWAIT makes XTTselectCompInAntidote wait for user response (see UIRESUME)
uiwait(handles.figure1);
return


% --- Executes when user attempts to close figure1.
function figure1_CloseRequestFcn(hObject, eventdata, handles)
if isequal(get(hObject, 'waitstatus'),'waiting')
    uiresume(hObject);
else
    delete(hObject);
end
return


% --- Executes on button press in Apply.
function Apply_Callback(hObject, eventdata, handles)
%

handles = UpdateSelection(handles);

BinPhaseDefOut = handles.BinPhaseDef;
Selection = handles.Selection;

for i = 1:length(Selection)
    Where = Selection(i).Where; 
    if  isequal(Selection(i).IsSel,1)
        IdxSel = find(Selection(i).IsElSel);
        BinPhaseDefOut(Where).ListVariAntidote = BinPhaseDefOut(Where).ListVariAntidote(IdxSel);
    else
        BinPhaseDefOut(Where).IsPhaseIn = 0;
    end
end

handles.output = BinPhaseDefOut;
guidata(hObject, handles);

close(handles.gcf);
return



function handles = UpdateSelection(handles)
%

Selection = handles.Selection;

for i = 1:length(Selection)
    eval(['Value =  get(handles.SelPhase',num2str(i),',''Value'');']);
    
    for j = 1:Selection(i).nbEl
        eval(['Selection(i).IsElSel(j) = get(handles.P',num2str(i),'El',num2str(j),',''Value'');']);
    end
    
    if Value
        Selection(i).IsSel = 1;
    else
        Selection(i).IsSel = 0;
    end
    
end

handles.Selection = Selection;
handles = InitiateButtons(handles);

return


function handles = InitiateButtons(handles)
%

BinPhaseDef = handles.BinPhaseDef;
Selection = handles.Selection;
WorkVariXMap = handles.WorkVariXMap;

for i = 1:length(Selection)
    
    eval(['set(handles.SelPhase',num2str(i),',''Visible'',''on'');']);
    eval(['set(handles.SelPhase',num2str(i),',''String'',Selection(i).name);']);
    
    if isequal(Selection(i).IsSel,1)
        eval(['set(handles.SelPhase',num2str(i),',''Value'',1);']);
    else
        eval(['set(handles.SelPhase',num2str(i),',''Value'',0);']);
    end
    
    for j = 1:Selection(i).nbEl
        
        ElName = BinPhaseDef(Selection(i).Idx).ListVariAntidote{j};
        WhereEl = find(ismember(WorkVariXMap.Els,ElName));
        Conc = WorkVariXMap.COMP(Selection(i).Where,WhereEl);
        
        if isequal(Selection(i).IsSel,1)
            eval(['set(handles.DispP',num2str(i),'El',num2str(j),',''Visible'',''on'',''String'',Conc);']);
            eval(['set(handles.P',num2str(i),'El',num2str(j),',''Visible'',''on'',''Enable'',''on'');']);
        else
            eval(['set(handles.DispP',num2str(i),'El',num2str(j),',''Visible'',''off'');']);
            eval(['set(handles.P',num2str(i),'El',num2str(j),',''Visible'',''off'');']);
        end
        
        if ~length(Conc)
            Selection(i).IsElSel(j) = 0;
            eval(['set(handles.P',num2str(i),'El',num2str(j),',''Enable'',''off'');']);
        end
        
        if isequal(Selection(i).IsElSel(j),1) 
            eval(['set(handles.P',num2str(i),'El',num2str(j),',''String'',ElName,''Value'',1);']);
        else
            eval(['set(handles.P',num2str(i),'El',num2str(j),',''String'',ElName,''Value'',0);']);
        end
        
    end
end



%keyboard




return






% --- Outputs from this function are returned to the command line.
function varargout = XTTselectCompInAntidote_OutputFcn(hObject, eventdata, handles) 
%
varargout{1} = handles.output;

delete(hObject)
return


% --- Executes on button press in P1El1.
function P1El1_Callback(hObject, eventdata, handles)
% hObject    handle to P1El1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P1El1


% --- Executes on button press in P1El2.
function P1El2_Callback(hObject, eventdata, handles)
% hObject    handle to P1El2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P1El2


% --- Executes on button press in P1El3.
function P1El3_Callback(hObject, eventdata, handles)
% hObject    handle to P1El3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P1El3


% --- Executes on button press in P1El4.
function P1El4_Callback(hObject, eventdata, handles)
% hObject    handle to P1El4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P1El4


% --- Executes on button press in SelPhase1.
function SelPhase1_Callback(hObject, eventdata, handles)
%
handles = UpdateSelection(handles);
return

% --- Executes on button press in P1El5.
function P1El5_Callback(hObject, eventdata, handles)
% hObject    handle to P1El5 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P1El5


% --- Executes on button press in P1El6.
function P1El6_Callback(hObject, eventdata, handles)
% hObject    handle to P1El6 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P1El6


% --- Executes on button press in P1El7.
function P1El7_Callback(hObject, eventdata, handles)
% hObject    handle to P1El7 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P1El7


% --- Executes on button press in P1El8.
function P1El8_Callback(hObject, eventdata, handles)
% hObject    handle to P1El8 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P1El8


% --- Executes on button press in P2El1.
function P2El1_Callback(hObject, eventdata, handles)
% hObject    handle to P2El1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P2El1


% --- Executes on button press in P2El2.
function P2El2_Callback(hObject, eventdata, handles)
% hObject    handle to P2El2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P2El2


% --- Executes on button press in P2El3.
function P2El3_Callback(hObject, eventdata, handles)
% hObject    handle to P2El3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P2El3


% --- Executes on button press in P2El4.
function P2El4_Callback(hObject, eventdata, handles)
% hObject    handle to P2El4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P2El4


% --- Executes on button press in SelPhase2.
function SelPhase2_Callback(hObject, eventdata, handles)
%
handles = UpdateSelection(handles);
return

% --- Executes on button press in P2El5.
function P2El5_Callback(hObject, eventdata, handles)
% hObject    handle to P2El5 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P2El5


% --- Executes on button press in P2El6.
function P2El6_Callback(hObject, eventdata, handles)
% hObject    handle to P2El6 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P2El6


% --- Executes on button press in P2El7.
function P2El7_Callback(hObject, eventdata, handles)
% hObject    handle to P2El7 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P2El7


% --- Executes on button press in P2El8.
function P2El8_Callback(hObject, eventdata, handles)
% hObject    handle to P2El8 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P2El8


% --- Executes on button press in P3El1.
function P3El1_Callback(hObject, eventdata, handles)
% hObject    handle to P3El1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P3El1


% --- Executes on button press in P3El2.
function P3El2_Callback(hObject, eventdata, handles)
% hObject    handle to P3El2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P3El2


% --- Executes on button press in P3El3.
function P3El3_Callback(hObject, eventdata, handles)
% hObject    handle to P3El3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P3El3


% --- Executes on button press in P3El4.
function P3El4_Callback(hObject, eventdata, handles)
% hObject    handle to P3El4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P3El4


% --- Executes on button press in SelPhase3.
function SelPhase3_Callback(hObject, eventdata, handles)
%
handles = UpdateSelection(handles);
return

% --- Executes on button press in P3El5.
function P3El5_Callback(hObject, eventdata, handles)
% hObject    handle to P3El5 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P3El5


% --- Executes on button press in P3El6.
function P3El6_Callback(hObject, eventdata, handles)
% hObject    handle to P3El6 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P3El6


% --- Executes on button press in P3El7.
function P3El7_Callback(hObject, eventdata, handles)
% hObject    handle to P3El7 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P3El7


% --- Executes on button press in P3El8.
function P3El8_Callback(hObject, eventdata, handles)
% hObject    handle to P3El8 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P3El8


% --- Executes on button press in P4El1.
function P4El1_Callback(hObject, eventdata, handles)
% hObject    handle to P4El1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P4El1


% --- Executes on button press in P4El2.
function P4El2_Callback(hObject, eventdata, handles)
% hObject    handle to P4El2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P4El2


% --- Executes on button press in P4El3.
function P4El3_Callback(hObject, eventdata, handles)
% hObject    handle to P4El3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P4El3


% --- Executes on button press in P4El4.
function P4El4_Callback(hObject, eventdata, handles)
% hObject    handle to P4El4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P4El4


% --- Executes on button press in SelPhase4.
function SelPhase4_Callback(hObject, eventdata, handles)
%
handles = UpdateSelection(handles);
return

% --- Executes on button press in P4El5.
function P4El5_Callback(hObject, eventdata, handles)
% hObject    handle to P4El5 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P4El5


% --- Executes on button press in P4El6.
function P4El6_Callback(hObject, eventdata, handles)
% hObject    handle to P4El6 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P4El6


% --- Executes on button press in P4El7.
function P4El7_Callback(hObject, eventdata, handles)
% hObject    handle to P4El7 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P4El7


% --- Executes on button press in P4El8.
function P4El8_Callback(hObject, eventdata, handles)
% hObject    handle to P4El8 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P4El8


% --- Executes on button press in P5El1.
function P5El1_Callback(hObject, eventdata, handles)
% hObject    handle to P5El1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P5El1


% --- Executes on button press in P5El2.
function P5El2_Callback(hObject, eventdata, handles)
% hObject    handle to P5El2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P5El2


% --- Executes on button press in P5El3.
function P5El3_Callback(hObject, eventdata, handles)
% hObject    handle to P5El3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P5El3


% --- Executes on button press in P5El4.
function P5El4_Callback(hObject, eventdata, handles)
% hObject    handle to P5El4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P5El4


% --- Executes on button press in SelPhase5.
function SelPhase5_Callback(hObject, eventdata, handles)
%
handles = UpdateSelection(handles);
return


% --- Executes on button press in P5El5.
function P5El5_Callback(hObject, eventdata, handles)
% hObject    handle to P5El5 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P5El5


% --- Executes on button press in P5El6.
function P5El6_Callback(hObject, eventdata, handles)
% hObject    handle to P5El6 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P5El6


% --- Executes on button press in P5El7.
function P5El7_Callback(hObject, eventdata, handles)
% hObject    handle to P5El7 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P5El7


% --- Executes on button press in P5El8.
function P5El8_Callback(hObject, eventdata, handles)
% hObject    handle to P5El8 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P5El8


% --- Executes on button press in P6El1.
function P6El1_Callback(hObject, eventdata, handles)
% hObject    handle to P6El1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P6El1


% --- Executes on button press in P6El2.
function P6El2_Callback(hObject, eventdata, handles)
% hObject    handle to P6El2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P6El2


% --- Executes on button press in P6El3.
function P6El3_Callback(hObject, eventdata, handles)
% hObject    handle to P6El3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P6El3


% --- Executes on button press in P6El4.
function P6El4_Callback(hObject, eventdata, handles)
% hObject    handle to P6El4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P6El4


% --- Executes on button press in SelPhase6.
function SelPhase6_Callback(hObject, eventdata, handles)
%
handles = UpdateSelection(handles);
return


% --- Executes on button press in P6El5.
function P6El5_Callback(hObject, eventdata, handles)
% hObject    handle to P6El5 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P6El5


% --- Executes on button press in P6El6.
function P6El6_Callback(hObject, eventdata, handles)
% hObject    handle to P6El6 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P6El6


% --- Executes on button press in P6El7.
function P6El7_Callback(hObject, eventdata, handles)
% hObject    handle to P6El7 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P6El7


% --- Executes on button press in P6El8.
function P6El8_Callback(hObject, eventdata, handles)
% hObject    handle to P6El8 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P6El8


% --- Executes on button press in P7El1.
function P7El1_Callback(hObject, eventdata, handles)
% hObject    handle to P7El1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P7El1


% --- Executes on button press in P7El2.
function P7El2_Callback(hObject, eventdata, handles)
% hObject    handle to P7El2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P7El2


% --- Executes on button press in P7El3.
function P7El3_Callback(hObject, eventdata, handles)
% hObject    handle to P7El3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P7El3


% --- Executes on button press in P7El4.
function P7El4_Callback(hObject, eventdata, handles)
% hObject    handle to P7El4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P7El4


% --- Executes on button press in SelPhase7.
function SelPhase7_Callback(hObject, eventdata, handles)
%
handles = UpdateSelection(handles);
return



% --- Executes on button press in P7El5.
function P7El5_Callback(hObject, eventdata, handles)
% hObject    handle to P7El5 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P7El5


% --- Executes on button press in P7El6.
function P7El6_Callback(hObject, eventdata, handles)
% hObject    handle to P7El6 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P7El6


% --- Executes on button press in P7El7.
function P7El7_Callback(hObject, eventdata, handles)
% hObject    handle to P7El7 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P7El7


% --- Executes on button press in P7El8.
function P7El8_Callback(hObject, eventdata, handles)
% hObject    handle to P7El8 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P7El8


% --- Executes on button press in P8El1.
function P8El1_Callback(hObject, eventdata, handles)
% hObject    handle to P8El1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P8El1


% --- Executes on button press in P8El2.
function P8El2_Callback(hObject, eventdata, handles)
% hObject    handle to P8El2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P8El2


% --- Executes on button press in P8El3.
function P8El3_Callback(hObject, eventdata, handles)
% hObject    handle to P8El3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P8El3


% --- Executes on button press in P8El4.
function P8El4_Callback(hObject, eventdata, handles)
% hObject    handle to P8El4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P8El4


% --- Executes on button press in SelPhase8.
function SelPhase8_Callback(hObject, eventdata, handles)
%
handles = UpdateSelection(handles);
return

% --- Executes on button press in P8El5.
function P8El5_Callback(hObject, eventdata, handles)
% hObject    handle to P8El5 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P8El5


% --- Executes on button press in P8El6.
function P8El6_Callback(hObject, eventdata, handles)
% hObject    handle to P8El6 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P8El6


% --- Executes on button press in P8El7.
function P8El7_Callback(hObject, eventdata, handles)
% hObject    handle to P8El7 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P8El7


% --- Executes on button press in P8El8.
function P8El8_Callback(hObject, eventdata, handles)
% hObject    handle to P8El8 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of P8El8



