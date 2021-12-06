function varargout = XTTselectAssemblage(varargin)
% XTTSELECTASSEMBLAGE MATLAB code for XTTselectAssemblage.fig
%      XTTSELECTASSEMBLAGE, by itself, creates a new XTTSELECTASSEMBLAGE or raises the existing
%      singleton*.
%
%      H = XTTSELECTASSEMBLAGE returns the handle to a new XTTSELECTASSEMBLAGE or the handle to
%      the existing singleton*.
%
%      XTTSELECTASSEMBLAGE('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in XTTSELECTASSEMBLAGE.M with the given input arguments.
%
%      XTTSELECTASSEMBLAGE('Property','Value',...) creates a new XTTSELECTASSEMBLAGE or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before XTTselectAssemblage_OpeningFcn gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to XTTselectAssemblage_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help XTTselectAssemblage

% Last Modified by GUIDE v2.5 10-Jul-2013 16:41:50

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @XTTselectAssemblage_OpeningFcn, ...
                   'gui_OutputFcn',  @XTTselectAssemblage_OutputFcn, ...
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


% --- Executes just before XTTselectAssemblage is made visible.
function XTTselectAssemblage_OpeningFcn(hObject, eventdata, handles, varargin)

% -- varargin --

ElData = varargin{1};
ThData = varargin{2};
SSmodels = varargin{3};

handles.ElData = ElData;
handles.ThData = ThData;
handles.SSmodels = SSmodels;

guidata(hObject, handles);

% -- Update Filenames --
ElDataFilename = strread(ElData.Filename,'%s','delimiter','/');
SSmodelsFilename = strread(SSmodels.Filename,'%s','delimiter','/');

set(handles.ThDataFile,'String',ElDataFilename{end});
set(handles.FileSSData,'String',SSmodelsFilename{end});

StrChar = '';
for i=1:length(ElData.selected)
    if ElData.selected(i)
        StrChar = [StrChar,' - ',char(ElData.listElem{i})];
    end
end
StrChar = StrChar(4:end);

set(handles.FileElemList,'String',StrChar);


% -- update Uitable1 and Uitable2 and elemList --
LesElemsSel = ElData.listElem(find(ElData.selected));
 

% -- find the minerals for the chemical system -- 
SelectedThData = zeros(length(ThData),1);

for i=1:length(ThData)
    LesComp = ThData(i).compMat; % number of elements
    LesCompSyst = ThData(i).compMat.*ElData.selected; % number of elements
    
    if isequal(LesComp,LesCompSyst)
        SelectedThData(i) = 1;
    end
end

ThDataShort = ThData(find(SelectedThData));
handles.ThDataShort = ThDataShort;

for i=1:length(ThDataShort)
    ListMinShort{i} = ThDataShort(i).name;
end

handles.ListMinShort = ListMinShort;


% -- Find the SS models for thc chemical system

% *** General rule: We cannot select only the SS models for which all the
% End-members are in the selected minerals. This is to restrictive. But we
% can not use SS models with only one EM. The rule is that we need two or
% more end-members to have the SS model available. The other end-members
% cannot be selected. ***

ComptMin = 0;
LastMin = 0;

for i=1:length(SSmodels.ListMin)
    % 1) One Mineral (such as FELDSPARS)
    NbSubModels = length(SSmodels.Min(i).ListModels);
    
    for j=1:NbSubModels
        
        TheModel = SSmodels.Min(i).Models(j);
        EMOk = ismember(TheModel.ListEM,ListMinShort);
        if length(find(EMOk)) >= 2                      % number of matches
            if LastMin ~= i
                ComptMin = ComptMin+1;                          % New Min ?
                SSmodelsShort.ListMin{ComptMin} = SSmodels.ListMin{i};
                ComptModels = 1;
                LastMin = 1;
            else
                ComptModels = ComptModels+1;
            end
            SSmodelsShort.Min(ComptMin).ListModels{ComptModels} = SSmodels.Min(i).ListModels{j};
            
            OldModel = SSmodels.Min(i).Models(j);
            NewModel = OldModel;
            NewModel.ListEMok = NewModel.ListEM;
            
            for k=1:length(EMOk)
                TheName = NewModel.ListEM{k};
                if EMOk(k)
                    NewModel.ListEMok{k} = [TheName,'_s'];
                else
                    NewModel.ListEMok{k} = [TheName,'_n'];
                end
            end
            
            SSmodelsShort.Min(ComptMin).Models(ComptModels) = NewModel;
            
        end

    end

end

handles.SSmodelsShort = SSmodelsShort;


% -- Assemblage Code --

% ** (1) the ref of the line; (2) type 1_not sel 2_SS 3_pure phase 
% 4_fluid 5_gas (3) SS Mineral (4) SS model LIST 0 default (5) EM display
% (6 Saved DISPLAY)


% AssembageCode is defined in the opening function of XThermoTools.

AssemblageCode = varargin{4};
handles.AssemblageCode = AssemblageCode;

NewLign = length(AssemblageCode(:,1));
set(handles.MenuRef,'String',1:NewLign,'Value',1);


% Output Default
handles.output1 = '';
handles.output2 = '';
handles.output3 = '';
handles.output4 = AssemblageCode;



% -- Update Menus -- 
guidata(hObject, handles);
UpdateMenu(hObject, eventdata, handles);
 

handles.gcf = gcf;
handles.output = 0;
% Update handles structure
guidata(hObject, handles);

% UIWAIT makes ModSelectElem wait for user response (see UIRESUME)
uiwait(hObject);
return



% --- Outputs from this function are returned to the command line.
function varargout = XTTselectAssemblage_OutputFcn(hObject, eventdata, handles) 
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

keyboard

% Get default command line output from handles structure
varargout{1} = handles.output1;
varargout{2} = handles.output2;
varargout{3} = handles.output3;
varargout{4} = handles.output4;

delete(hObject);


function figure1_WindowButtonDownFcn(hObject, eventdata, handles)
    
return


function figure1_CloseRequestFcn(hObject, eventdata, handles)
if isequal(get(hObject, 'waitstatus'),'waiting')
    uiresume(hObject);
else
    delete(hObject);
end
return




function [] = UpdateDisplay(hObject, eventdata, handles)

AssemblageCode = handles.AssemblageCode;
LastLign = length(AssemblageCode(:,1));

AssemblageDisplay = cell(1,24);    % size of the display table

Compt= 0;
if LastLign > 1
    % Create the variable here
    for i=1:LastLign
        if isequal(AssemblageCode(i,end),2)
            Compt = Compt+1;
            
            LesTypes = get(handles.MenuType,'String');
            LeType = AssemblageCode(i,2);
            
            AssemblageDisplay{Compt,1} = char(LesTypes{LeType});

            switch LeType
                case 2
                    LesModels = handles.SSmodelsShort.Min(AssemblageCode(i,3)).ListModels;
                    LeModel = AssemblageCode(i,4);
                    AssemblageDisplay{Compt,2} = char(LesModels{LeModel});
                    
                    NameMin = handles.SSmodelsShort.Min(AssemblageCode(i,3)).Models(AssemblageCode(i,4)).ListEMok; 
                    ComptMin = 0;
                    for j=1:length(NameMin)
                        TheNameMin = NameMin{j};
                        if isequal(TheNameMin(end),'s')
                            ComptMin = ComptMin+1;
                            AssemblageDisplay{Compt,2+ComptMin} = char(TheNameMin);
                        end
                    end
                    
                    
                case 3
                    LesMins = handles.ListMinShort;
                    LeMin = AssemblageCode(i,3);
                    AssemblageDisplay{Compt,2} = '...';
                    AssemblageDisplay{Compt,3} = [char(LesMins{LeMin}),'_p'];
            end
        end
    end
end
    

% display the variable into the tables
set(handles.Uitable1,'Data',AssemblageDisplay(:,1:2));
set(handles.Uitable2,'Data',AssemblageDisplay(:,3:end));

return


function [] = UpdateMenu(hObject, eventdata, handles)
AssemblageCode = handles.AssemblageCode;
LastLign = length(AssemblageCode(:,1));
LaLign = get(handles.MenuRef,'Value');

if isequal(LastLign,LaLign)
    set(handles.MenuUpdate,'String','Add','visible','on');
    set(handles.MenuDelete,'Visible','off'); % tjr, cette ligne n'est pas encore ajoutée...
else
    set(handles.MenuUpdate,'Visible','off'); % update automatique... 
    set(handles.MenuDelete,'Visible','on');
end

switch AssemblageCode(LaLign,2)
    
    case 1
        % Empty line
        set(handles.MenuType,'Value',1);
        
        set(handles.MenuMineral,'String',handles.SSmodelsShort.ListMin,'Value',1,'visible','off');
        set(handles.MenuModel,'String',handles.SSmodelsShort.Min(1).ListModels,'Value',1,'visible','off');
        set(handles.MenuEndMembers,'String',handles.SSmodelsShort.Min(1).Models(1).ListEMok,'Value',1,'visible','off');
        set(handles.MenuSel,'visible','off');
        set(handles.MenuUpdate,'enable','off');
        set(handles.MenuDelete,'enable','off');
        
    case 2
        % SS
         set(handles.MenuType,'Value',2);

        set(handles.MenuMineral,'String',handles.SSmodelsShort.ListMin,'Value',AssemblageCode(LaLign,3),'visible','on');
        set(handles.MenuModel,'String',handles.SSmodelsShort.Min(AssemblageCode(LaLign,3)).ListModels,'Value',AssemblageCode(LaLign,4),'visible','on');
        set(handles.MenuEndMembers,'String',handles.SSmodelsShort.Min(AssemblageCode(LaLign,3)).Models(AssemblageCode(LaLign,4)).ListEMok,'Value',AssemblageCode(LaLign,5),'visible','on');
        
        NameMin = handles.SSmodelsShort.Min(AssemblageCode(LaLign,3)).Models(AssemblageCode(LaLign,4)).ListEMok{AssemblageCode(LaLign,5)};
        if isequal(NameMin(end),'s')
            set(handles.MenuSel,'visible','on','value',1);
        elseif isequal(NameMin(end),'d')
            set(handles.MenuSel,'visible','on','value',0);
        end
        
        if isequal(NameMin(end),'n')
            set(handles.MenuSel,'visible','off');
        end

        set(handles.MenuUpdate,'enable','on');
        set(handles.MenuDelete,'enable','on');
        
        
    case 3
        % Pure phase        
        set(handles.MenuType,'Value',3);
        
        set(handles.MenuMineral,'String',handles.ListMinShort,'Value',AssemblageCode(LaLign,3),'visible','on');
        
        set(handles.MenuModel,'String','','Value',1,'visible','off');
        set(handles.MenuEndMembers,'String','','Value',1,'visible','off');
        
        set(handles.MenuSel,'visible','off');
        set(handles.MenuUpdate,'enable','on');
        set(handles.MenuDelete,'enable','on');
        
end


guidata(hObject, handles);
UpdateDisplay(hObject, eventdata, handles);
return



% --- Executes on button press in New.
function New_Callback(hObject, eventdata, handles)
set(handles.MenuRef,'Value',1,'String',1);
AssemblageCode = ones(1,6);                                      % 6 fields
handles.AssemblageCode = AssemblageCode;

% -- Update Menus -- 
handles.output = hObject;
UpdateMenu(hObject, eventdata, handles);


% --- Executes on button press in Load.
function Load_Callback(hObject, eventdata, handles)


% [1] First read the backup and check the compatibility

[filename, pathname, filterindex] = uigetfile({'*.txt','TXT-files (*.txt)'}, 'Pick a file', 'XTT_Assemblages/*.txt');
if ~filterindex
    return
end

OldAssemblageCode = handles.AssemblageCode;

ElData = handles.ElData;
ThData = handles.ThData;
SSmodels = handles.SSmodels;

ThDataShort = handles.ThDataShort;
SSmodelsShort = handles.SSmodelsShort;

ListMinShort = handles.ListMinShort;

fid = fopen([pathname,filename],'r');

lalign = fgetl(fid);
lalign = fgetl(fid);
lalign = fgetl(fid);

% XTT_ThermoData file
lalign = fgetl(fid);
LaLignSep = strread(lalign,'%s');
if ~isequal(LaLignSep{2},ElData.Filename)
    disp('The ThermoData file is not the same !!!');
    fclose(fid);
    return
end

% XTT_SSData file
lalign = fgetl(fid);
LaLignSep = strread(lalign,'%s');
if ~isequal(LaLignSep{2},SSmodels.Filename)
    disp('The SSModels file is not the same !!!');
    fclose(fid);
    return
end

% ELEM system
ElementsString = '';
for i=1:length(ElData.selected)
    if ElData.selected(i)
        ElementsString = [ElementsString,char(ElData.listElem{i}),'-'];
    end
end
ElementsString = ElementsString(1:end-1);    
lalign = fgetl(fid);
LaLignSep = strread(lalign,'%s');
if ~isequal(LaLignSep{2},ElementsString)
    disp('The Chemical system is not the same !!!');
    fclose(fid);
    return
end


% AssemblageCode
lalign = fgetl(fid);
Compt = 0;
while 1
    lalign = fgetl(fid);
    if isequal(lalign(1),'>')
        break
    end
    Compt = Compt+1;
    NewAssemblageCode(Compt,:) = strread(lalign,'%f');
end


ComptLine = 0;
ComptEM = 0;
while 1
    lalign = fgetl(fid);
  
    if isequal(lalign,-1)  ||  numel(lalign) == 0
        %disp('end')
        break
    end
    
    if isequal(lalign(4),'1')
        
    elseif isequal(lalign(4),'2')
        ComptLine = ComptLine + 1;
        ComptEM = 0;
        
        TheLineStr = strread(lalign,'%s');
        TheNameMin{ComptLine} = TheLineStr{3};
        TheModelMin = TheLineStr{4};
        
        if length(SSmodelsShort.Min) >= NewAssemblageCode(ComptLine,3)
            if ~isequal(char(TheNameMin{ComptLine}),char(SSmodelsShort.ListMin(NewAssemblageCode(ComptLine,3))))
                disp(['Problem with the Mineral ',char(SSmodelsShort.ListMin(NewAssemblageCode(ComptLine,3))),': unrecognized ',char(TheNameMin{ComptLine})])
                fclose(fid);
                return
                
            end
        end
        
        if length(SSmodelsShort.Min(NewAssemblageCode(ComptLine,3)).Models.Name) >= NewAssemblageCode(ComptLine,4)
            if ~isequal(char(TheModelMin),char(SSmodelsShort.Min(NewAssemblageCode(ComptLine,3)).Models(NewAssemblageCode(ComptLine,4)).Name))
                disp(['Problem with the SS Model ',char(SSmodelsShort.Min(NewAssemblageCode(ComptLine,3)).Models(NewAssemblageCode(ComptLine,4)).Name),': unrecognized ',char(TheModelMin)])
                fclose(fid);
                return
                
            end
        end
        
        %SSmodelsShort.Min(NewAssemblageCode(ComptLine,3)).Models.Name;

        
        
    elseif isequal(lalign(4),'3')
        ComptLine = ComptLine + 1;
        ComptEM = 0;
        
        TheLineStr = strread(lalign,'%s');
        TheNameMinPhase = TheLineStr{3};
        
        TheNameMinPhase = TheNameMinPhase(1:end-2);
        
        if ~ismember(TheNameMinPhase,ListMinShort)
            disp(['The phase ',char(TheNameMinPhase),' is not in the database'])
            fclose(fid);
            return
        end
        
        
    else
        ComptEM = ComptEM + 1;
        
        if length(SSmodelsShort.Min) >= NewAssemblageCode(ComptLine,3)
            if length(SSmodelsShort.Min(NewAssemblageCode(ComptLine,3)).Models) >= NewAssemblageCode(ComptLine,4)
                if length(SSmodelsShort.Min(NewAssemblageCode(ComptLine,3)).Models(NewAssemblageCode(ComptLine,4)).ListEMok) >= ComptEM
                    TheNameDB = SSmodelsShort.Min(NewAssemblageCode(ComptLine,3)).Models(NewAssemblageCode(ComptLine,4)).ListEMok{ComptEM};
                else
                    TheNameDB = '';
                end
            else
                TheNameDB = '';
            end
        else
            TheNameDB = '';
        end
        
        if isequal(TheNameDB,'')
            disp(['Problem with the EM list of ',char(TheNameMin{ComptLine}),': unrecognized ',char(strread(lalign,'%s'))])
            fclose(fid);
            return
        end
        
        TheNameNew = char(strread(lalign,'%s'));
        
        TheNameDB = TheNameDB(1:end-2);
        TheNameNew = TheNameNew(1:end-2);
        
        if ~isequal(TheNameDB,TheNameNew)
            disp(['Problem with the EM list of ',char(TheNameMin{ComptLine}),': difference between ',char(TheNameDB),' (DB) and ',char(TheNameNew),' (BACKUP)'])
            fclose(fid);
            return
        end
    end
    
end



fclose(fid);


% [2] Delete the displayed data
New_Callback(hObject, eventdata, handles);

% [3] write the new variables (we just need to apply the new SS names and
% the new AssemblageCode variable)

handles.AssemblageCode = NewAssemblageCode;

NewLign = length(NewAssemblageCode(:,1));
set(handles.MenuRef,'String',1:NewLign,'Value',1);

% -- Update Menus -- 
handles.output = hObject;
UpdateMenu(hObject, eventdata, handles);

return




% --- Executes on button press in Save.
function Save_Callback(hObject, eventdata, handles)

if isequal(get(handles.MenuUpdate,'Visible'),'on')
    warndlg('The selected row is not added to the assemblage table','WARNING')
    return
end


if ~isequal(exist('XTT_Assemblages'),7)
    mkdir('XTT_Assemblages');
end

[filename, pathname, filterindex] = uiputfile({'*.txt','TXT-files (*.txt)'}, 'Save as', 'XTT_Assemblages/*.txt');

if ~filterindex
    return
end
AssemblageCode = handles.AssemblageCode;

ElData = handles.ElData;
ThData = handles.ThData;
SSmodels = handles.SSmodels;

ThDataShort = handles.ThDataShort;
SSmodelsShort = handles.SSmodelsShort;

fid = fopen([pathname,filename],'w');


fprintf(fid,'- - - - - - - - - - - - - - - - - - - - - - - - - - - - - \n');
fprintf(fid,['  XThermoTools Assemblage file (',char(datestr(now)),')\n']);
fprintf(fid,'- - - - - - - - - - - - - - - - - - - - - - - - - - - - - \n');

fprintf(fid,['>\t',char(ElData.Filename),'\n']);
fprintf(fid,['>\t',char(SSmodels.Filename),'\n']);

ElementsString = '';
for i=1:length(ElData.selected)
    if ElData.selected(i)
        ElementsString = [ElementsString,char(ElData.listElem{i}),'-'];
    end
end
ElementsString = ElementsString(1:end-1);          

fprintf(fid,['>\t',char(ElementsString),'\n']);

fprintf(fid,['>\t','Assemblage Matrix','\n']);
fprintf(fid,'\t%f\t%f\t%f\t%f\t%f\t%f\n',AssemblageCode');

fprintf(fid,['>\t','Assemblage Details','\n']);


for i=1:length(AssemblageCode(:,1))
    if isequal(AssemblageCode(i,end),2)
        TheType = AssemblageCode(i,2);
        switch TheType
            case 3
                fprintf(fid,['\t',char(num2str(i)),'\t',char(num2str(TheType)),'\t',[char(ThDataShort(AssemblageCode(i,3)).name),'_p'],'\n']);

            case 2
                TheModels = SSmodelsShort.Min(AssemblageCode(i,3)).ListModels;
                if length(char(SSmodelsShort.ListMin{AssemblageCode(i,3)})) < 4
                    fprintf(fid,['\t',char(num2str(i)),'\t',char(num2str(TheType)),'\t',char(SSmodelsShort.ListMin{AssemblageCode(i,3)}),'\t\t\t\t',char(TheModels{AssemblageCode(i,4)}),'\n']);
                elseif length(char(SSmodelsShort.ListMin{AssemblageCode(i,3)})) < 8
                    fprintf(fid,['\t',char(num2str(i)),'\t',char(num2str(TheType)),'\t',char(SSmodelsShort.ListMin{AssemblageCode(i,3)}),'\t\t\t',char(TheModels{AssemblageCode(i,4)}),'\n']);
                else
                    fprintf(fid,['\t',char(num2str(i)),'\t',char(num2str(TheType)),'\t',char(SSmodelsShort.ListMin{AssemblageCode(i,3)}),'\t\t',char(TheModels{AssemblageCode(i,4)}),'\n']);
                end
                
                for j=1:length(SSmodelsShort.Min(AssemblageCode(i,3)).Models(AssemblageCode(i,4)).ListEMok)
                    TheMinName = SSmodelsShort.Min(AssemblageCode(i,3)).Models(AssemblageCode(i,4)).ListEMok{j};
                    fprintf(fid,['\t','\t\t',char(num2str(TheMinName)),'\n']);
                end
        end
    end
end


fclose(fid);







return


% --- Executes on selection change in MenuRef.
function MenuRef_Callback(hObject, eventdata, handles)
UpdateMenu(hObject, eventdata, handles);
return

% --- Executes on selection change in MenuType.
function MenuType_Callback(hObject, eventdata, handles)
LaLign = get(handles.MenuRef,'Value');
LeNewMod = get(gco,'Value');

handles.AssemblageCode(LaLign,2:end-1) = [LeNewMod,1,1,1];

guidata(hObject, handles);
UpdateMenu(hObject, eventdata, handles)
return


% --- Executes on selection change in MenuModel.
function MenuModel_Callback(hObject, eventdata, handles)
LaLign = get(handles.MenuRef,'Value');

if handles.AssemblageCode(LaLign,2) == 2
    % SS
    handles.AssemblageCode(LaLign,4:end-1) = [get(gco,'Value'),1];
end

guidata(hObject, handles);
UpdateMenu(hObject, eventdata, handles)
return


% --- Executes on selection change in MenuMineral.
function MenuMineral_Callback(hObject, eventdata, handles)
LaLign = get(handles.MenuRef,'Value');

if isequal(handles.AssemblageCode(LaLign,2),2)
    % SS
    handles.AssemblageCode(LaLign,3:end-1) = [get(gco,'Value'),1,1];

else
    % Pure Phase
    handles.AssemblageCode(LaLign,3) = [get(gco,'Value')];
end

guidata(hObject, handles);
UpdateMenu(hObject, eventdata, handles)
return


% --- Executes on selection change in MenuEndMembers.
function MenuEndMembers_Callback(hObject, eventdata, handles)
LaLign = get(handles.MenuRef,'Value');

if handles.AssemblageCode(LaLign,2) == 2
    % SS
    handles.AssemblageCode(LaLign,5) = get(gco,'Value');
end

guidata(hObject, handles);
UpdateMenu(hObject, eventdata, handles)
return


% --- Executes on button press in togglebutton1.
function togglebutton1_Callback(hObject, eventdata, handles)

return


% --- Executes on button press in MenuSel.
function MenuSel_Callback(hObject, eventdata, handles)
LaLign = get(handles.MenuRef,'Value');
AssemblageCode = handles.AssemblageCode;

ListEMok = handles.SSmodelsShort.Min(AssemblageCode(LaLign,3)).Models(AssemblageCode(LaLign,4)).ListEMok;

NewName = ListEMok{AssemblageCode(LaLign,5)};

if get(gco,'Value')
    NewName(end) = 's';
else
    NewName(end) = 'd';
end

ListEMok{AssemblageCode(LaLign,5)} = NewName;
handles.SSmodelsShort.Min(AssemblageCode(LaLign,3)).Models(AssemblageCode(LaLign,4)).ListEMok = ListEMok;


guidata(hObject, handles);
UpdateMenu(hObject, eventdata, handles)
return


% CREATE A NEW LINE
function MenuUpdate_Callback(hObject, eventdata, handles)
LaLign = get(handles.MenuRef,'Value');
AssemblageCode = handles.AssemblageCode;
AssemblageCode(LaLign,end) = 2;                                %for display

NewLign = length(AssemblageCode(:,1))+1;
set(handles.MenuRef,'String',1:NewLign,'Value',NewLign);

AssemblageCode(NewLign,:) = ones(1,6);
AssemblageCode(NewLign,1) = NewLign;



handles.AssemblageCode = AssemblageCode;

guidata(hObject, handles);
UpdateMenu(hObject, eventdata, handles)
return


% --- Executes on button press in MenuDelete.
function MenuDelete_Callback(hObject, eventdata, handles)
AssemblageCode = handles.AssemblageCode;
TheLigne = get(handles.MenuRef,'Value');

NewAssemblageCode = zeros(size(AssemblageCode,1)-1,size(AssemblageCode,2));

if TheLigne ==1
    NewAssemblageCode = AssemblageCode(2:end,:);
elseif TheLigne == length(AssemblageCode)
    NewAssemblageCode = AssemblageCode(1:end-1,:);
else
    NewAssemblageCode(1:TheLigne-1,:) = AssemblageCode(1:TheLigne-1,:);
    NewAssemblageCode(TheLigne:end,:) = AssemblageCode(TheLigne+1:end,:);
end

NewLign = length(NewAssemblageCode(:,1));
set(handles.MenuRef,'String',1:NewLign,'Value',NewLign);

NewAssemblageCode(:,1) = [1:NewLign];


handles.AssemblageCode = NewAssemblageCode;
% -- Update Menus -- 
handles.output = hObject;
UpdateMenu(hObject, eventdata, handles);

return






% --- Executes on button press in Done.
function Done_Callback(hObject, eventdata, handles)

AssemblageCode = handles.AssemblageCode;

if length(AssemblageCode(:,1)) == 1
    return
end

SSmodels = handles.SSmodelsShort;



% [1] Create SSmodelsComp

ComptModel = 0;
ComptEM = 0;
OK = 0;
for i=1:length(AssemblageCode(:,1))
    switch AssemblageCode(i,2)
        case 2
            OK = 1;
            ComptModel = ComptModel+1;

            SSmodelsComp.Models(ComptModel) = SSmodels.Min(AssemblageCode(i,3)).Models(AssemblageCode(i,4));
            
            SSmodelsComp.ListModel{ComptModel} = SSmodelsComp.Models(ComptModel).Name;
            
            for j=1:length(SSmodelsComp.Models(ComptModel).ListEMok)
                TheName = SSmodelsComp.Models(ComptModel).ListEMok{j};
                if TheName(end) == 's';
                    ComptEM = ComptEM+1;
                    SSmodelsComp.ListEM{ComptEM} = TheName(1:end-2);
                end
            end
    end
end


ThData = handles.ThDataShort;
ListMin = handles.ListMinShort;

% [2] Create ThDataComp

ComptSubst = 0;
for i=1:length(AssemblageCode(:,1))
    switch AssemblageCode(i,2)
        case 3
            ComptSubst = ComptSubst + 1;
            ThDataComp(ComptSubst) = ThData(AssemblageCode(i,3));
    end
end


if OK
    for i=1:length(SSmodelsComp.ListEM)
        [Oui,Ou] = ismember(SSmodelsComp.ListEM{i},ListMin);
        ComptSubst = ComptSubst + 1;
        ThDataComp(ComptSubst) = ThData(Ou);
    end
else
    SSmodelsComp = '';
end
    

MinCompoMat = zeros(length(ThDataComp),length(handles.ElData.selected));
for i=1:length(ThDataComp)
    MinCompoMat(i,:) = ThDataComp(i).compMat;
end






handles.output1 = SSmodelsComp;
handles.output2 = ThDataComp;
handles.output3 = MinCompoMat;
handles.output4 = AssemblageCode;
guidata(hObject, handles);

close(handles.gcf);



return











% --- Executes during object creation, after setting all properties.
function MenuRef_CreateFcn(hObject, eventdata, handles)
% hObject    handle to MenuRef (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

% --- Executes during object creation, after setting all properties.
function MenuMineral_CreateFcn(hObject, eventdata, handles)
% hObject    handle to MenuMineral (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

% --- Executes during object creation, after setting all properties.
function MenuEndMembers_CreateFcn(hObject, eventdata, handles)
% hObject    handle to MenuEndMembers (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

% --- Executes during object creation, after setting all properties.
function MenuType_CreateFcn(hObject, eventdata, handles)
% hObject    handle to MenuType (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

% --- Executes during object creation, after setting all properties.
function MenuModel_CreateFcn(hObject, eventdata, handles)
% hObject    handle to MenuModel (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end
