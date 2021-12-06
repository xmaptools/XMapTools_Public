function varargout = XTTsetOptionXTT(varargin)
% XTTSETOPTIONXTT MATLAB code for XTTsetOptionXTT.fig
%      XTTSETOPTIONXTT, by itself, creates a new XTTSETOPTIONXTT or raises the existing
%      singleton*.
%
%      H = XTTSETOPTIONXTT returns the handle to a new XTTSETOPTIONXTT or the handle to
%      the existing singleton*.
%
%      XTTSETOPTIONXTT('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in XTTSETOPTIONXTT.M with the given input arguments.
%
%      XTTSETOPTIONXTT('Property','Value',...) creates a new XTTSETOPTIONXTT or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before XTTsetOptionXTT_OpeningFcn gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to XTTsetOptionXTT_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help XTTsetOptionXTT

% Last Modified by GUIDE v2.5 21-Mar-2019 12:04:08

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @XTTsetOptionXTT_OpeningFcn, ...
                   'gui_OutputFcn',  @XTTsetOptionXTT_OutputFcn, ...
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


% --- Executes just before XTTsetOptionXTT is made visible.
function XTTsetOptionXTT_OpeningFcn(hObject, eventdata, handles, varargin)
%



% Read existing file
File = [cd,'/OptionsXTT.txt'];

if exist(File);
    Valid = 1;
    fid = fopen(File);
    while 1
        TheL = fgetl(fid);

        if isequal(TheL,-1);
            break
        end

        if length(TheL)
            if isequal(TheL(1),'>')
                TheStr = strread(TheL(2:end),'%s');
                switch str2double(TheStr{1})
                    case 1
                        OptString{1} = fgetl(fid);
                        OptString{2} = fgetl(fid);
                    case 2
                        OptString{3} = fgetl(fid);
                        OptString{4} = fgetl(fid);
                    case 3
                        OptString{5} = fgetl(fid);
                        OptString{6} = fgetl(fid);
                        OptString{7} = fgetl(fid);
                        OptString{8} = fgetl(fid);
                    case 4
                        OptString{9} = fgetl(fid);
                        OptString{10} = fgetl(fid);

                end
            end
        end

    end
    fclose(fid);
    
    set(handles.Bulk1,'String',OptString{1});
    set(handles.Bulk2,'String',OptString{2});
    
    set(handles.Bingo_T,'String',OptString{3});
    set(handles.Bingo_P,'String',OptString{4});
    
    TVal = strread(OptString{9},'%s','delimiter',':');
    set(handles.Tmin,'String',TVal{1});
    set(handles.Tinc,'String',TVal{2});
    set(handles.Tmax,'String',TVal{3});

    PVal = strread(OptString{10},'%s','delimiter',':');
    set(handles.Pmin,'String',PVal{1});
    set(handles.Pinc,'String',PVal{2});
    set(handles.Pmax,'String',PVal{3});
    
    set(handles.Antidote_NbPerm,'String',OptString{5});
    set(handles.Antidote_dPx,'String',OptString{6});
    set(handles.Antidote_Tol,'String',OptString{8});
    
    PTVal = strread(OptString{7},'%s','delimiter',',');
    set(handles.Antidote_dT,'String',PTVal{1});
    set(handles.Antidote_dP,'String',PTVal{2});
    
end


% Initialization:

handles = GenerateOptionsXTT(hObject, eventdata, handles);

% Figure position
Unit = get(hObject,'Unit');
set(hObject,'Unit','normalized');
Position = get(hObject,'Position');
NewPosition = Position;
NewPosition(1) = (1-Position(3))/2;
NewPosition(2) = (1-Position(4))/2;
set(hObject,'Position',NewPosition);
set(hObject,'Unit',Unit);

handles.gcf = gcf;
handles.output = 0;

% Update handles structure
guidata(hObject, handles);

uiwait(hObject);return



% --- Outputs from this function are returned to the command line.
function varargout = XTTsetOptionXTT_OutputFcn(hObject, eventdata, handles) 
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure

varargout = {};  % changed 03.2019 to fix an issue on Windows

return


function figure1_WindowButtonDownFcn(hObject, eventdata, handles)
    
return


function figure1_CloseRequestFcn(hObject, eventdata, handles)
if isequal(get(hObject, 'waitstatus'),'waiting')
    uiresume(hObject);
    delete(hObject);
else
    delete(hObject);
end
return

% Function that read and generate handles.OptionsXTT
function handles = GenerateOptionsXTT(hObject, eventdata, handles)

handles.OptionsXTT.Bulk1 = get(handles.Bulk1,'String');
handles.OptionsXTT.Bulk2 = get(handles.Bulk2,'String');

handles.OptionsXTT.Bingo_T = str2num(get(handles.Bingo_T,'String'));
handles.OptionsXTT.Bingo_P = str2num(get(handles.Bingo_P,'String'));

handles.OptionsXTT.Tmin = str2num(get(handles.Tmin,'String'));
handles.OptionsXTT.Tinc = str2num(get(handles.Tinc,'String'));
handles.OptionsXTT.Tmax = str2num(get(handles.Tmax,'String'));

handles.OptionsXTT.Pmin = str2num(get(handles.Pmin,'String'));
handles.OptionsXTT.Pinc = str2num(get(handles.Pinc,'String'));
handles.OptionsXTT.Pmax = str2num(get(handles.Pmax,'String'));

handles.OptionsXTT.Antidote_NbPerm = str2num(get(handles.Antidote_NbPerm,'String'));
handles.OptionsXTT.Antidote_dPx = str2num(get(handles.Antidote_dPx,'String'));
handles.OptionsXTT.Antidote_dT = str2num(get(handles.Antidote_dT,'String'));
handles.OptionsXTT.Antidote_dP = str2num(get(handles.Antidote_dP,'String'));
handles.OptionsXTT.Antidote_Tol = str2num(get(handles.Antidote_Tol,'String'));

% Check PT point
if handles.OptionsXTT.Bingo_T > handles.OptionsXTT.Tmin && handles.OptionsXTT.Bingo_T < handles.OptionsXTT.Tmax && handles.OptionsXTT.Bingo_P > handles.OptionsXTT.Pmin && handles.OptionsXTT.Bingo_P < handles.OptionsXTT.Pmax
    set(handles.Optimize,'Enable','on')
else
    set(handles.Optimize,'Enable','off')
end

% Plot
axes(handles.axes1), cla, hold on

Ti = [handles.OptionsXTT.Tmin:handles.OptionsXTT.Tinc:handles.OptionsXTT.Tmax];
Pi = [handles.OptionsXTT.Pmin:handles.OptionsXTT.Pinc:handles.OptionsXTT.Pmax];

plot(repmat([Ti(1),Ti(end)],length(Pi),1)',[Pi',Pi']','-k');
plot([Ti',Ti']',repmat([Pi(1),Pi(end)],length(Ti),1)','-k');

plot(handles.OptionsXTT.Bingo_T,handles.OptionsXTT.Bingo_P,'or','Markersize',10,'MarkerfaceColor','r','MarkerEdgeColor','k')

xlabel('Temperature (degree C)')
ylabel('Pressure (bar)')

guidata(hObject, handles);
return




% --- Executes on button press in Optimize.
function Apply_Callback(hObject, eventdata, handles)
%

File = [cd,'/OptionsXTT.txt'];

% Check if the file already exist
DoesItExist = exist(File);

if ~isequal(DoesItExist,0)
    % we delete this file
    delete(File);
end
    
fid = fopen(File,'w');

fprintf(fid,'%s\n%s\n','----','## Option file for XThermoTools generated by Bingo-Antidote ##');
fprintf(fid,'%s\n\n',['Generated: ',datestr(now)]);

fprintf(fid,'%s\n%s\n','! Bulk options','>1 ');
fprintf(fid,'%s\n%s\n\n',handles.OptionsXTT.Bulk1,handles.OptionsXTT.Bulk2);

fprintf(fid,'%s\n%s\n','! Bingo default P-T conditions','>2');
fprintf(fid,'%s\n%s\n\n',num2str(handles.OptionsXTT.Bingo_T),num2str(handles.OptionsXTT.Bingo_P));

fprintf(fid,'%s\n%s\n','! Antidote default parameters','>3');
fprintf(fid,'%s\n%s\n%s\n%s\n\n',num2str(handles.OptionsXTT.Antidote_NbPerm),num2str(handles.OptionsXTT.Antidote_dPx),[num2str(handles.OptionsXTT.Antidote_dT),',',num2str(handles.OptionsXTT.Antidote_dP)],num2str(handles.OptionsXTT.Antidote_Tol));

fprintf(fid,'%s\n%s\n','! P-T window','>4');
fprintf(fid,'%s\n%s\n\n',[num2str(handles.OptionsXTT.Tmin),':',num2str(handles.OptionsXTT.Tinc),':',num2str(handles.OptionsXTT.Tmax)],[num2str(handles.OptionsXTT.Pmin),':',num2str(handles.OptionsXTT.Pinc),':',num2str(handles.OptionsXTT.Pmax)]);

fclose(fid);

close(handles.gcf);
return

function Bulk1_Callback(hObject, eventdata, handles)
%
GenerateOptionsXTT(hObject, eventdata, handles);
return

% --- Executes during object creation, after setting all properties.
function Bulk1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to Bulk1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function Bulk2_Callback(hObject, eventdata, handles)
%
GenerateOptionsXTT(hObject, eventdata, handles);
return

% --- Executes during object creation, after setting all properties.
function Bulk2_CreateFcn(hObject, eventdata, handles)
% hObject    handle to Bulk2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function Bingo_T_Callback(hObject, eventdata, handles)
%
GenerateOptionsXTT(hObject, eventdata, handles);
return

% --- Executes during object creation, after setting all properties.
function Bingo_T_CreateFcn(hObject, eventdata, handles)
% hObject    handle to Bingo_T (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function Bingo_P_Callback(hObject, eventdata, handles)
%
GenerateOptionsXTT(hObject, eventdata, handles);
return


% --- Executes during object creation, after setting all properties.
function Bingo_P_CreateFcn(hObject, eventdata, handles)
% hObject    handle to Bingo_P (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function Antidote_NbPerm_Callback(hObject, eventdata, handles)
%
GenerateOptionsXTT(hObject, eventdata, handles);
return


% --- Executes during object creation, after setting all properties.
function Antidote_NbPerm_CreateFcn(hObject, eventdata, handles)
% hObject    handle to Antidote_NbPerm (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function Antidote_Tol_Callback(hObject, eventdata, handles)
%
GenerateOptionsXTT(hObject, eventdata, handles);
return

% --- Executes during object creation, after setting all properties.
function Antidote_Tol_CreateFcn(hObject, eventdata, handles)
% hObject    handle to Antidote_Tol (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function Antidote_dPx_Callback(hObject, eventdata, handles)
%
GenerateOptionsXTT(hObject, eventdata, handles);
return

% --- Executes during object creation, after setting all properties.
function Antidote_dPx_CreateFcn(hObject, eventdata, handles)
% hObject    handle to Antidote_dPx (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function Antidote_dP_Callback(hObject, eventdata, handles)
%
GenerateOptionsXTT(hObject, eventdata, handles);
return


% --- Executes during object creation, after setting all properties.
function Antidote_dP_CreateFcn(hObject, eventdata, handles)
% hObject    handle to Antidote_dP (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function Antidote_dT_Callback(hObject, eventdata, handles)
%
GenerateOptionsXTT(hObject, eventdata, handles);
return


% --- Executes during object creation, after setting all properties.
function Antidote_dT_CreateFcn(hObject, eventdata, handles)
% hObject    handle to Antidote_dT (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function Tmin_Callback(hObject, eventdata, handles)
%
GenerateOptionsXTT(hObject, eventdata, handles);
return


% --- Executes during object creation, after setting all properties.
function Tmin_CreateFcn(hObject, eventdata, handles)
% hObject    handle to Tmin (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function Pmin_Callback(hObject, eventdata, handles)
%
GenerateOptionsXTT(hObject, eventdata, handles);
return



% --- Executes during object creation, after setting all properties.
function Pmin_CreateFcn(hObject, eventdata, handles)
% hObject    handle to Pmin (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function Tinc_Callback(hObject, eventdata, handles)
%
GenerateOptionsXTT(hObject, eventdata, handles);
return


% --- Executes during object creation, after setting all properties.
function Tinc_CreateFcn(hObject, eventdata, handles)
% hObject    handle to Tinc (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function Pinc_Callback(hObject, eventdata, handles)
%
GenerateOptionsXTT(hObject, eventdata, handles);
return


% --- Executes during object creation, after setting all properties.
function Pinc_CreateFcn(hObject, eventdata, handles)
% hObject    handle to Pinc (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function Tmax_Callback(hObject, eventdata, handles)
%
GenerateOptionsXTT(hObject, eventdata, handles);
return


% --- Executes during object creation, after setting all properties.
function Tmax_CreateFcn(hObject, eventdata, handles)
% hObject    handle to Tmax (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function Pmax_Callback(hObject, eventdata, handles)
%
GenerateOptionsXTT(hObject, eventdata, handles);
return


% --- Executes during object creation, after setting all properties.
function Pmax_CreateFcn(hObject, eventdata, handles)
% hObject    handle to Pmax (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in Optimize.
function Optimize_Callback(hObject, eventdata, handles)
%
Ti = [handles.OptionsXTT.Tmin:handles.OptionsXTT.Tinc:handles.OptionsXTT.Tmax];

NbSteps = length(Ti);
OptimalPsteps = (handles.OptionsXTT.Pmax-handles.OptionsXTT.Pmin)/(NbSteps-1);

set(handles.Pinc,'String',num2str(floor(OptimalPsteps)));

guidata(hObject, handles);
GenerateOptionsXTT(hObject, eventdata, handles);
return
