function varargout = XTTselectFluidsGasesBuffers(varargin)
% XTTSELECTFLUIDSGASESBUFFERS MATLAB code for XTTselectFluidsGasesBuffers.fig
%      XTTSELECTFLUIDSGASESBUFFERS, by itself, creates a new XTTSELECTFLUIDSGASESBUFFERS or raises the existing
%      singleton*.
%
%      H = XTTSELECTFLUIDSGASESBUFFERS returns the handle to a new XTTSELECTFLUIDSGASESBUFFERS or the handle to
%      the existing singleton*.
%
%      XTTSELECTFLUIDSGASESBUFFERS('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in XTTSELECTFLUIDSGASESBUFFERS.M with the given input arguments.
%
%      XTTSELECTFLUIDSGASESBUFFERS('Property','Value',...) creates a new XTTSELECTFLUIDSGASESBUFFERS or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before XTTselectFluidsGasesBuffers_OpeningFcn gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to XTTselectFluidsGasesBuffers_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help XTTselectFluidsGasesBuffers

% Last Modified by GUIDE v2.5 04-Mar-2019 10:19:01

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @XTTselectFluidsGasesBuffers_OpeningFcn, ...
                   'gui_OutputFcn',  @XTTselectFluidsGasesBuffers_OutputFcn, ...
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


% --- Executes just before XTTselectFluidsGasesBuffers is made visible.
function XTTselectFluidsGasesBuffers_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to XTTselectFluidsGasesBuffers (see VARARGIN)


BinGfDef = varargin{1}; 
handles.BinGfDef = BinGfDef;



% A FEW SETTINGS FOR DISPLAY:

NbSpec = length(BinGfDef.Fluids.Spec);
for i=1:NbSpec
    SpecNames{i} = BinGfDef.Fluids.Spec(i).Name;
end

% #1
set(handles.POP_Fs_Min1,'String',{'None',SpecNames{1}});
if BinGfDef.Fluids.Spec(1).IsActive
    set(handles.POP_Fs_Min1,'Value',2);
else
    set(handles.POP_Fs_Min1,'Value',1);
end

if NbSpec>1
    set(handles.POP_Fs_Min2,'String',{'None',SpecNames{2}});
    if BinGfDef.Fluids.Spec(2).IsActive
        set(handles.POP_Fs_Min2,'Value',2);
    else
        set(handles.POP_Fs_Min2,'Value',1);
    end
else
    set(handles.POP_Fs_Min2,'String','None');
end

if NbSpec>2
    set(handles.POP_Fs_Min3,'String',{'None',SpecNames{3}});
    if BinGfDef.Fluids.Spec(3).IsActive
        set(handles.POP_Fs_Min3,'Value',2);
    else
        set(handles.POP_Fs_Min3,'Value',1);
    end
else
    set(handles.POP_Fs_Min3,'String','None');
end

if NbSpec>3
    set(handles.POP_Fs_Min4,'String',{'None',SpecNames{4}});
    if BinGfDef.Fluids.Spec(3).IsActive
        set(handles.POP_Fs_Min3,'Value',2);
    else
        set(handles.POP_Fs_Min3,'Value',1);
    end
else
    set(handles.POP_Fs_Min4,'String','None');
end

set(handles.POP_Ms1,'String',{'None',BinGfDef.Melt.Name});



handles.NbSpec = NbSpec;
handles.SpecNames = SpecNames;


% -- Update Menus -- 
guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles);

Unit = get(hObject,'Unit');
set(hObject,'Unit','normalized');
Position = get(hObject,'Position');
NewPosition = Position;
NewPosition(1) = (1-Position(3))/2;
NewPosition(2) = (1-Position(4))/2;
set(hObject,'Position',NewPosition);
set(hObject,'Unit',Unit);

handles.gcf = gcf;
handles.output = BinGfDef;
guidata(hObject, handles);

% UIWAIT makes ModSelectElem wait for user response (see UIRESUME)
uiwait(hObject);
return


% --- Outputs from this function are returned to the command line.
function varargout = XTTselectFluidsGasesBuffers_OutputFcn(hObject, eventdata, handles) 
%

varargout{1} = handles.output;
delete(hObject);
return

% --- Executes when user attempts to close figure1.
function figure1_CloseRequestFcn(hObject, eventdata, handles)
%
if isequal(get(hObject, 'waitstatus'),'waiting')
    uiresume(hObject);
else
    delete(hObject);
end
return


% --- Executes on button press in APPLY.
function APPLY_Callback(hObject, eventdata, handles)
%

BinGfDef = handles.BinGfDef;

handles.output = BinGfDef;
guidata(hObject, handles);


%h = ModSelecElem.m
close(handles.gcf);
return


function UpdateDisplay(hObject, eventdata, handles)
%

BinGfDef = handles.BinGfDef;

if BinGfDef.Fluids.Activate
    set(handles.Activate_FLUID,'Value',1);
    set(handles.Optimize_NFluid,'Value',BinGfDef.Fluids.Optimize);
    
    Compt = 0;
    for i = 1:length(BinGfDef.Fluids.Spec)
        eval(['set(handles.POP_Fs_Min',num2str(i),',''Enable'',''on'');']);

        if BinGfDef.Fluids.Spec(i).IsActive
            eval(['set(handles.POP_Fs_Min',num2str(i),',''Value'',2);']);
        else
            eval(['set(handles.POP_Fs_Min',num2str(i),',''Value'',1);']);
            
            % Not active - hide the fields
            eval(['set(handles.POP_Fs_Lower',num2str(i),',''Enable'',''off'');']);
            eval(['set(handles.POP_Fs_Upper',num2str(i),',''Enable'',''off'');']);
        end
        
        if BinGfDef.Fluids.Spec(i).IsActive && ~get(handles.Optimize_NFluid,'Value')
            eval(['set(handles.POP_Fs_N',num2str(i),',''Enable'',''on'',''String'',num2str(BinGfDef.Fluids.Spec(',num2str(i),').Bulk));']);
            
            eval(['set(handles.POP_Fs_Lower',num2str(i),',''Enable'',''off'');']);
            eval(['set(handles.POP_Fs_Upper',num2str(i),',''Enable'',''off'');']);
            
            Compt = Compt+1;
            if get(handles.Optimize_NFluid,'Value')
                eval(['set(handles.POP_Fs_Lower',num2str(i),',''String'',num2str(BinGfDef.Fluids.Spec(',num2str(i),').Lower));']);
                eval(['set(handles.POP_Fs_Upper',num2str(i),',''String'',num2str(BinGfDef.Fluids.Spec(',num2str(i),').Upper));']);
                %BinGfDef.Fluids.Optimize = 1;
                
            else
                %BinGfDef.Fluids.Optimize = 0;
            end
            
        else
            if BinGfDef.Fluids.Spec(i).IsActive && get(handles.Optimize_NFluid,'Value')
                set(handles.Optimize_NFluid,'Enable','on')
                
                eval(['set(handles.POP_Fs_Lower',num2str(i),',''Enable'',''on'');']);
                eval(['set(handles.POP_Fs_Upper',num2str(i),',''Enable'',''on'');']);
                
                eval(['set(handles.POP_Fs_Lower',num2str(i),',''String'',num2str(BinGfDef.Fluids.Spec(',num2str(i),').Lower));']);
                eval(['set(handles.POP_Fs_Upper',num2str(i),',''String'',num2str(BinGfDef.Fluids.Spec(',num2str(i),').Upper));']);
            end
            
            eval(['set(handles.POP_Fs_N',num2str(i),',''Enable'',''off'');']);
        end
    end
    
    if length(BinGfDef.Fluids.Spec) < 4
        for i = length(BinGfDef.Fluids.Spec)+1:4
            eval(['set(handles.POP_Fs_Min',num2str(i),',''Enable'',''off'');']);
            eval(['set(handles.POP_Fs_N',num2str(i),',''Enable'',''off'');']);
            eval(['set(handles.POP_Fs_Lower',num2str(i),',''Enable'',''off'');']);
            eval(['set(handles.POP_Fs_Upper',num2str(i),',''Enable'',''off'');']);
        end
    end
    
    if Compt
        set(handles.Optimize_NFluid,'Enable','On');
    end
    
    set(handles.POP_Fs_Min2,'Enable','on');
    
    %set(handles.POP_Fs_N1,'Enable','on')
    
else
    
    set(handles.Optimize_NFluid,'Enable','off');
    
    set(handles.POP_Fs_Min1,'Enable','off');
    set(handles.POP_Fs_Min2,'Enable','off');
    set(handles.POP_Fs_Min3,'Enable','off');
    set(handles.POP_Fs_Min4,'Enable','off');
    
    set(handles.POP_Fs_N1,'Enable','off');
    set(handles.POP_Fs_N2,'Enable','off');
    set(handles.POP_Fs_N3,'Enable','off');
    set(handles.POP_Fs_N4,'Enable','off');
    
    set(handles.POP_Fs_Lower1,'Enable','off');
    set(handles.POP_Fs_Lower2,'Enable','off');
    set(handles.POP_Fs_Lower3,'Enable','off');
    set(handles.POP_Fs_Lower4,'Enable','off');
    
    set(handles.POP_Fs_Upper1,'Enable','off');
    set(handles.POP_Fs_Upper2,'Enable','off');
    set(handles.POP_Fs_Upper3,'Enable','off');
    set(handles.POP_Fs_Upper4,'Enable','off');
    
end


if BinGfDef.Melt.Activate
    set(handles.Activate_MELT,'Value',1);

    set(handles.POP_Ms1,'Value',2);
    set(handles.POP_Ms1,'Enable','inactive');
    
    set(handles.Include_MELTinSOL,'Enable','on');
    
    if BinGfDef.Melt.Include
        set(handles.Include_MELTinSOL,'Value',1);
       
    end
    
else
    
    set(handles.POP_Ms1,'Value',1);
    
    set(handles.POP_Ms1,'Enable','off');
    set(handles.Include_MELTinSOL,'Enable','off');
    
end



if BinGfDef.Oxygen.ActivateExtraO
    set(handles.Optimize_NO,'Enable','on');
    set(handles.Activate_EXTO,'Value',1);
    
    if BinGfDef.Oxygen.OptimizeExtraO
        set(handles.Optimize_NO,'value',1);
        set(handles.POP_O_N1,'Enable','off');
        set(handles.POP_O_Lower1,'Enable','on');
        set(handles.POP_O_Upper1,'Enable','on');
        set(handles.POP_O_Lower1,'String',BinGfDef.Oxygen.ExtraO.Lower);
        set(handles.POP_O_Upper1,'String',BinGfDef.Oxygen.ExtraO.Upper);
    else
        set(handles.Optimize_NO,'value',0);
        set(handles.POP_O_N1,'Enable','on');
        set(handles.POP_O_N1,'String',BinGfDef.Oxygen.ExtraO.Bulk);
        set(handles.POP_O_Lower1,'Enable','off');
        set(handles.POP_O_Upper1,'Enable','off');
    end
    
else
    
    set(handles.POP_O_N1,'Enable','off');
    set(handles.POP_O_Lower1,'Enable','off');
    set(handles.POP_O_Upper1,'Enable','off');
    
    set(handles.Optimize_NO,'Enable','off');

end


if BinGfDef.Oxygen.ActivateBuffer
    
    
    
else
    
    set(handles.POP_O_Buffer,'Enable','off');
    
end




%keyboard


return



% ACTIVATE

% --- Executes on button press in Activate_FLUID.
function Activate_FLUID_Callback(hObject, eventdata, handles)
% 
if get(gco,'Value')
    handles.BinGfDef.Fluids.Activate = 1;
else
    handles.BinGfDef.Fluids.Activate = 0;
end
guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles)
return

% --- Executes on button press in Activate_MELT.
function Activate_MELT_Callback(hObject, eventdata, handles)
%
if get(gco,'Value')
    handles.BinGfDef.Melt.Activate = 1;
else
    handles.BinGfDef.Melt.Activate = 0;
end
guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles)
return

% --- Executes on button press in Activate_EXTO.
function Activate_EXTO_Callback(hObject, eventdata, handles)
%
if get(gco,'Value')
    handles.BinGfDef.Oxygen.ActivateExtraO = 1;
else
    handles.BinGfDef.Oxygen.ActivateExtraO = 0;
end
guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles)
return

% --- Executes on button press in Activate_OBUFFER.
function Activate_OBUFFER_Callback(hObject, eventdata, handles)
%
if get(gco,'Value')
    handles.BinGfDef.Oxygen.ActivateBuffer = 1;
else
    handles.BinGfDef.Oxygen.ActivateBuffer = 0;
end
guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles)
return






% --- Executes on selection change in POP_O_Buffer.
function POP_O_Buffer_Callback(hObject, eventdata, handles)
% hObject    handle to POP_O_Buffer (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = cellstr(get(hObject,'String')) returns POP_O_Buffer contents as cell array
%        contents{get(hObject,'Value')} returns selected item from POP_O_Buffer


% --- Executes during object creation, after setting all properties.
function POP_O_Buffer_CreateFcn(hObject, eventdata, handles)
% hObject    handle to POP_O_Buffer (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end








% --- Executes on button press in Optimize_NO.
function Optimize_NO_Callback(hObject, eventdata, handles)
% 
Value = get(handles.Optimize_NO,'Value'); 
if Value
    handles.BinGfDef.Oxygen.OptimizeExtraO = 1;
else
    handles.BinGfDef.Oxygen.OptimizeExtraO = 0;
end
guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles);
return


function POP_O_Lower1_Callback(hObject, eventdata, handles)
%
Value = str2num(get(handles.POP_O_Lower1,'String'));
handles.BinGfDef.Oxygen.ExtraO.Lower = Value;
guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles);
return

% --- Executes during object creation, after setting all properties.
function POP_O_Lower1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to POP_O_Lower1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


function POP_O_Upper1_Callback(hObject, eventdata, handles)
% 
Value = str2num(get(handles.POP_O_Upper1,'String'));
handles.BinGfDef.Oxygen.ExtraO.Upper = Value;
guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles);
return

% --- Executes during object creation, after setting all properties.
function POP_O_Upper1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to POP_O_Upper1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


function POP_O_N1_Callback(hObject, eventdata, handles)
% 
Value = str2num(get(handles.POP_O_N1,'String'));

if Value < 0.0001
    Value = 0.0001;
    set(hObject,'String',num2str(Value));
end
    
handles.BinGfDef.Oxygen.ExtraO.Bulk = Value;
guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles);
return


% --- Executes during object creation, after setting all properties.
function POP_O_N1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to POP_O_N1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end





% --- Executes on selection change in POP_Ms1.
function POP_Ms1_Callback(hObject, eventdata, handles)
% hObject    handle to POP_Ms1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = cellstr(get(hObject,'String')) returns POP_Ms1 contents as cell array
%        contents{get(hObject,'Value')} returns selected item from POP_Ms1


% --- Executes during object creation, after setting all properties.
function POP_Ms1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to POP_Ms1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end





function POP_Fs_Lower1_Callback(hObject, eventdata, handles)
%
Value = str2num(get(handles.POP_Fs_Lower1,'String'));

if Value < 0.0001
    Value = 0.0001;
    set(hObject,'String',num2str(Value));
end
    
handles.BinGfDef.Fluids.Spec(1).Lower = Value;
guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles);
return

% --- Executes during object creation, after setting all properties.
function POP_Fs_Lower1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to POP_Fs_Lower1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function POP_Fs_Lower2_Callback(hObject, eventdata, handles)
%
Value = str2num(get(handles.POP_Fs_Lower2,'String')); 

if Value < 0.0001
    Value = 0.0001;
    set(hObject,'String',num2str(Value));
end
    
handles.BinGfDef.Fluids.Spec(2).Lower = Value;
guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles);
return

% --- Executes during object creation, after setting all properties.
function POP_Fs_Lower2_CreateFcn(hObject, eventdata, handles)
% hObject    handle to POP_Fs_Lower2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


function POP_Fs_Lower3_Callback(hObject, eventdata, handles)
%
Value = str2num(get(handles.POP_Fs_Lower3,'String')); 

if Value < 0.0001
    Value = 0.0001;
    set(hObject,'String',num2str(Value));
end
    
handles.BinGfDef.Fluids.Spec(3).Lower = Value;
guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles);
return

% --- Executes during object creation, after setting all properties.
function POP_Fs_Lower3_CreateFcn(hObject, eventdata, handles)
% hObject    handle to POP_Fs_Lower3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


function POP_Fs_Lower4_Callback(hObject, eventdata, handles)
%
Value = str2num(get(handles.POP_Fs_Lower4,'String')); 

if Value < 0.0001
    Value = 0.0001;
    set(hObject,'String',num2str(Value));
end
    
handles.BinGfDef.Fluids.Spec(4).Lower = Value;
guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles);
return

% --- Executes during object creation, after setting all properties.
function POP_Fs_Lower4_CreateFcn(hObject, eventdata, handles)
% hObject    handle to POP_Fs_Lower4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end






% --- Executes on selection change in POP_Fs_Min1.
function POP_Fs_Min1_Callback(hObject, eventdata, handles)
%
Value = get(handles.POP_Fs_Min1,'Value'); 
if Value > 1
    handles.BinGfDef.Fluids.Spec(1).IsActive = 1;
else
    handles.BinGfDef.Fluids.Spec(1).IsActive = 0;
end
guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles);
return

% --- Executes during object creation, after setting all properties.
function POP_Fs_Min1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to POP_Fs_Min1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on selection change in POP_Fs_Min2.
function POP_Fs_Min2_Callback(hObject, eventdata, handles)
%
Value = get(handles.POP_Fs_Min2,'Value');
if Value > 1
    handles.BinGfDef.Fluids.Spec(2).IsActive = 1;
else
    handles.BinGfDef.Fluids.Spec(2).IsActive = 0;
end
guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles);
return

% --- Executes during object creation, after setting all properties.
function POP_Fs_Min2_CreateFcn(hObject, eventdata, handles)
% hObject    handle to POP_Fs_Min2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on selection change in POP_Fs_Min3.
function POP_Fs_Min3_Callback(hObject, eventdata, handles)
%
Value = get(handles.POP_Fs_Min3,'Value');
if Value > 1
    handles.BinGfDef.Fluids.Spec(3).IsActive = 1;
else
    handles.BinGfDef.Fluids.Spec(3).IsActive = 0;
end
guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles);
return


% --- Executes during object creation, after setting all properties.
function POP_Fs_Min3_CreateFcn(hObject, eventdata, handles)
% hObject    handle to POP_Fs_Min3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on selection change in POP_Fs_Min4.
function POP_Fs_Min4_Callback(hObject, eventdata, handles)
%
Value = get(handles.POP_Fs_Min4,'Value');
if Value > 1
    handles.BinGfDef.Fluids.Spec(4).IsActive = 1;
else
    handles.BinGfDef.Fluids.Spec(4).IsActive = 0;
end
guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles);
return


% --- Executes during object creation, after setting all properties.
function POP_Fs_Min4_CreateFcn(hObject, eventdata, handles)
% hObject    handle to POP_Fs_Min4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in Optimize_NFluid.
function Optimize_NFluid_Callback(hObject, eventdata, handles)
%
handles.BinGfDef.Fluids.Optimize = get(handles.Optimize_NFluid,'Value');
guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles)
return








function POP_Fs_Upper1_Callback(hObject, eventdata, handles)
%
Value = str2num(get(handles.POP_Fs_Upper1,'String')); 
handles.BinGfDef.Fluids.Spec(1).Upper = Value;
guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles);
return

% --- Executes during object creation, after setting all properties.
function POP_Fs_Upper1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to POP_Fs_Upper1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


function POP_Fs_Upper2_Callback(hObject, eventdata, handles)
%
Value = str2num(get(handles.POP_Fs_Upper2,'String')); 
handles.BinGfDef.Fluids.Spec(2).Upper = Value;
guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles);
return

% --- Executes during object creation, after setting all properties.
function POP_Fs_Upper2_CreateFcn(hObject, eventdata, handles)
% hObject    handle to POP_Fs_Upper2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


function POP_Fs_Upper3_Callback(hObject, eventdata, handles)
%
Value = str2num(get(handles.POP_Fs_Upper3,'String')); 
handles.BinGfDef.Fluids.Spec(3).Upper = Value;
guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles);
return

% --- Executes during object creation, after setting all properties.
function POP_Fs_Upper3_CreateFcn(hObject, eventdata, handles)
% hObject    handle to POP_Fs_Upper3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


function POP_Fs_Upper4_Callback(hObject, eventdata, handles)
%
Value = str2num(get(handles.POP_Fs_Upper4,'String')); 
handles.BinGfDef.Fluids.Spec(4).Upper = Value;
guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles);
return

% --- Executes during object creation, after setting all properties.
function POP_Fs_Upper4_CreateFcn(hObject, eventdata, handles)
% hObject    handle to POP_Fs_Upper4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end





function POP_Fs_N1_Callback(hObject, eventdata, handles)
%
Value = str2num(get(handles.POP_Fs_N1,'String'));

if Value < 0.0001
    Value = 0.0001;
    set(hObject,'String',num2str(Value));
end
    

handles.BinGfDef.Fluids.Spec(1).Bulk = Value;
guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles);
return

% --- Executes during object creation, after setting all properties.
function POP_Fs_N1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to POP_Fs_N1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


function POP_Fs_N2_Callback(hObject, eventdata, handles)
% 
Value = str2num(get(handles.POP_Fs_N2,'String')); 

if Value < 0.0001
    Value = 0.0001;
    set(hObject,'String',num2str(Value));
end
    
handles.BinGfDef.Fluids.Spec(2).Bulk = Value;
guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles);
return

% --- Executes during object creation, after setting all properties.
function POP_Fs_N2_CreateFcn(hObject, eventdata, handles)
% hObject    handle to POP_Fs_N2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


function POP_Fs_N3_Callback(hObject, eventdata, handles)
%
Value = str2num(get(handles.POP_Fs_N3,'String')); 

if Value < 0.0001
    Value = 0.0001;
    set(hObject,'String',num2str(Value));
end
    

handles.BinGfDef.Fluids.Spec(3).Bulk = Value;
guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles);
return

% --- Executes during object creation, after setting all properties.
function POP_Fs_N3_CreateFcn(hObject, eventdata, handles)
% hObject    handle to POP_Fs_N3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


function POP_Fs_N4_Callback(hObject, eventdata, handles)
% 
Value = str2num(get(handles.POP_Fs_N4,'String')); 

if Value < 0.0001
    Value = 0.0001;
    set(hObject,'String',num2str(Value));
end
    
handles.BinGfDef.Fluids.Spec(4).Bulk = Value;
guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles);
return

% --- Executes during object creation, after setting all properties.
function POP_Fs_N4_CreateFcn(hObject, eventdata, handles)
% hObject    handle to POP_Fs_N4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end




% --- Executes on button press in Include_MELTinSOL.
function Include_MELTinSOL_Callback(hObject, eventdata, handles)
% 
Value = get(handles.Include_MELTinSOL,'Value'); 
handles.BinGfDef.Melt.Include = Value;
guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles);
return
