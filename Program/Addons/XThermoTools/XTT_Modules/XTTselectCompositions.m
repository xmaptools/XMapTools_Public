function varargout = XTTselectCompositions(varargin)
% XTTSELECTCOMPOSITIONS MATLAB code for XTTselectCompositions.fig
%      XTTSELECTCOMPOSITIONS, by itself, creates a new XTTSELECTCOMPOSITIONS or raises the existing
%      singleton*.
%
%      H = XTTSELECTCOMPOSITIONS returns the handle to a new XTTSELECTCOMPOSITIONS or the handle to
%      the existing singleton*.
%
%      XTTSELECTCOMPOSITIONS('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in XTTSELECTCOMPOSITIONS.M with the given input arguments.
%
%      XTTSELECTCOMPOSITIONS('Property','Value',...) creates a new XTTSELECTCOMPOSITIONS or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before XTTselectCompositions_OpeningFcn gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to XTTselectCompositions_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help XTTselectCompositions

% Last Modified by GUIDE v2.5 26-Jul-2013 11:10:37

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @XTTselectCompositions_OpeningFcn, ...
                   'gui_OutputFcn',  @XTTselectCompositions_OutputFcn, ...
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


% --- Executes just before XTTselectCompositions is made visible.
function XTTselectCompositions_OpeningFcn(hObject, eventdata, handles, varargin)


Computation = varargin{1};
MapWindow = varargin{2};
ComputationCode = varargin{3};


ComptPhase = 0;
ComptEM = 0;
TheNamesSelected = [];
if length(Computation.SSmodelsComp)                    % SS models are used
    
    for i=1:length(Computation.SSmodelsComp.Models)
        ComptPhase = ComptPhase + 1;
        TheNamesSelected{ComptPhase} = Computation.SSmodelsComp.Models(i).Name;
        for j=1:length(Computation.SSmodelsComp.Models(i).ListEM)
            ComptEM = ComptEM+1;
            TheListEM{ComptEM} = Computation.SSmodelsComp.Models(i).ListEM{j};
        end
    end
else
    TheListEM = {'Empty'};
end

if ~isequal(length(TheListEM),length(Computation.ThDataComp))
    % There is Pure phases
    for i=1:length(Computation.ThDataComp)
        TheName = Computation.ThDataComp(i).name;
        if ismember(TheName,TheListEM)
            Computation.ThDataComp(i).SolidSolution=1;
        else
            ComptPhase = ComptPhase+1;
            TheNamesSelected{ComptPhase} = TheName;
            Computation.ThDataComp(i).SolidSolution=0;
        end
    end
end

set(handles.PopUpSelected,'String',TheNamesSelected);
set(handles.PopUpMinerals,'String',MapWindow.Mask.ListMin);

Computation.TheNamesSelected = TheNamesSelected;

% Read ComputationCode... 
handles.Computation = Computation;
handles.MapWindow = MapWindow;
handles.ComputationCode = ComputationCode;

guidata(hObject, handles);

UpdateDisplay(hObject, eventdata, handles);




% Choose default command line output for XTTselectCompositions
handles.output1 = handles.Computation;
handles.output2 = handles.ComputationCode;

handles.gcf = gcf;

% Update handles structure
guidata(hObject, handles);

% UIWAIT makes XTTselectCompositions wait for user response (see UIRESUME)
uiwait(handles.figure1);


% --- Outputs from this function are returned to the command line.
function varargout = XTTselectCompositions_OutputFcn(hObject, eventdata, handles) 

varargout{1} = handles.output1;
varargout{2} = handles.output2;

keyboard

delete(hObject);




function [] = UpdateDisplay(hObject, eventdata, handles)

Computation = handles.Computation;
MapWindow = handles.MapWindow;
ComputationCode = handles.ComputationCode;


% [1] update the menus
TheRowSelected = get(handles.PopUpSelected,'Value');
TheType = ComputationCode(TheRowSelected,2);



switch TheType
    case 2                                                 % Solid solution
        set(handles.PopUpCompositions,'visible','on','value',ComputationCode(TheRowSelected,3));
        set(handles.ButtonLoad,'visible','on');
        set(handles.PopUpMinerals,'visible','on','value',ComputationCode(TheRowSelected,4));
        
        if ComputationCode(TheRowSelected,3) == 1 || ComputationCode(TheRowSelected,3) == 3 
            set(handles.TextNumber,'visible','on','enable','on','String',char(num2str(ComputationCode(TheRowSelected,5))));
        else
            set(handles.TextNumber,'visible','on','enable','inactive','String',char(num2str(ComputationCode(TheRowSelected,5))));
        end
        set(handles.TextActivity,'enable','inactive','String',char(num2str(ComputationCode(TheRowSelected,6))));
        
        
    case 3                                                 % Pure substance
        set(handles.PopUpCompositions,'visible','off');
        set(handles.ButtonLoad,'visible','off');
        set(handles.PopUpMinerals,'visible','off');
        
        set(handles.TextNumber,'visible','off','enable','inactive','String',char(num2str(ComputationCode(TheRowSelected,5))));
        set(handles.TextActivity,'enable','on','String',char(num2str(ComputationCode(TheRowSelected,6))));
 
end


% [2] update the UiTable
Table4Display = cell(size(ComputationCode));
TheNamesSelected = get(handles.PopUpSelected,'String');
TheMethods = get(handles.PopUpCompositions,'String');
TheMinerals = get(handles.PopUpMinerals,'String');

for i=1:length(ComputationCode(:,1));
    TheType = ComputationCode(i,2);
    switch TheType
        case 2   
            Table4Display{i,1} = TheNamesSelected{i};
            Table4Display{i,2} = 'SS';
            Table4Display{i,3} = TheMethods{ComputationCode(i,3)};
            Table4Display{i,4} = TheMinerals{ComputationCode(i,4)};
            Table4Display{i,5} = num2str(ComputationCode(i,5));
            Table4Display{i,6} = num2str(ComputationCode(i,6));
        
        case 3
            Table4Display{i,1} = TheNamesSelected{i};
            Table4Display{i,2} = 'PS';
            Table4Display{i,3} = '';
            Table4Display{i,4} = '';
            Table4Display{i,5} = num2str(ComputationCode(i,5));
            Table4Display{i,6} = num2str(ComputationCode(i,6));
    end
end

% Display selected raw in red

for i=1:length(ComputationCode(:,1));
    if i == TheRowSelected
        for j = 1:size(Table4Display,2)
            Table4Display(i,j) = strcat('<html><span style="color: #FF0000; font-weight: bold;">', ...
                Table4Display(i,j),'</span></html>');
        end
    else
        for j = 1:size(Table4Display,2)
            Table4Display(i,j) = strcat('<html><span style="color: #00000; font-weight: normal;">', ...
                Table4Display(i,j),'</span></html>');
        end
        
    end
    
    

end

set(handles.uitable1,'Data',Table4Display);

set(handles.TextCombinations,'String',['... corresponding to ',char(num2str(prod(ComputationCode(:,5)),'%g')),' combinations'])

return 



% --- Executes on button press in ButtonApply.
function ButtonApply_Callback(hObject, eventdata, handles)
handles.output1 = handles.Computation;
handles.output2 = handles.ComputationCode;


guidata(hObject, handles);
close(handles.gcf);

return

% --- Executes on selection change in PopUpSelected.
function PopUpSelected_Callback(hObject, eventdata, handles)


UpdateDisplay(hObject, eventdata, handles);
return


% --- Executes during object creation, after setting all properties.
function PopUpSelected_CreateFcn(hObject, eventdata, handles)
% hObject    handle to PopUpSelected (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on selection change in PopUpCompositions.
function PopUpCompositions_Callback(hObject, eventdata, handles)
TheValue = get(gco,'Value');
TheRowSelected = get(handles.PopUpSelected,'Value');

if TheValue == 2
    handles.ComputationCode(TheRowSelected,5) = length(find(handles.MapWindow.Mask.Data == get(handles.PopUpMinerals,'Value')));
end

if TheValue == 1
    handles.ComputationCode(TheRowSelected,5) = 1;
end

if TheValue == 3
    handles.ComputationCode(TheRowSelected,5) = 50;
end

if TheValue == 4
    handles.ComputationCode(TheRowSelected,5) = 1;
end

if TheValue == 5
    handles.ComputationCode(TheRowSelected,5) = 1;
end

handles.ComputationCode(TheRowSelected,3) = TheValue;

guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles);
return


% --- Executes during object creation, after setting all properties.
function PopUpCompositions_CreateFcn(hObject, eventdata, handles)
% hObject    handle to PopUpCompositions (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in ButtonLoad.
function ButtonLoad_Callback(hObject, eventdata, handles)
% hObject    handle to ButtonLoad (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)



function TextNumber_Callback(hObject, eventdata, handles)
TheValue = str2num(get(gco,'String'));
TheRowSelected = get(handles.PopUpSelected,'Value');

handles.ComputationCode(TheRowSelected,5) = TheValue;

guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles);
return


% --- Executes during object creation, after setting all properties.
function TextNumber_CreateFcn(hObject, eventdata, handles)
% hObject    handle to TextNumber (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function TextActivity_Callback(hObject, eventdata, handles)
TheValue = str2num(get(gco,'String'));
TheRowSelected = get(handles.PopUpSelected,'Value');

handles.ComputationCode(TheRowSelected,6) = TheValue;

guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles);
return


% --- Executes during object creation, after setting all properties.
function TextActivity_CreateFcn(hObject, eventdata, handles)
% hObject    handle to TextActivity (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on selection change in PopUpMinerals.
function PopUpMinerals_Callback(hObject, eventdata, handles)
TheValue = get(gco,'Value');
TheRowSelected = get(handles.PopUpSelected,'Value');

handles.ComputationCode(TheRowSelected,4) = TheValue;

if get(handles.PopUpCompositions,'Value') == 2
    handles.ComputationCode(TheRowSelected,5) = length(find(handles.MapWindow.Mask.Data == get(handles.PopUpMinerals,'Value')));
end


guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles);
return


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


% --- Executes when user attempts to close figure1.
function figure1_CloseRequestFcn(hObject, eventdata, handles)
if isequal(get(hObject, 'waitstatus'),'waiting')
    uiresume(hObject);
else
    delete(hObject);
end
return
