function varargout = VER_XThermoTools_804(varargin)
% VER_XTHERMOTOOLS_804 MATLAB code for VER_XThermoTools_804.fig
%      VER_XTHERMOTOOLS_804, by itself, creates a new VER_XTHERMOTOOLS_804 or raises the existing
%      singleton*.
%
%      H = VER_XTHERMOTOOLS_804 returns the handle to a new VER_XTHERMOTOOLS_804 or the handle to
%      the existing singleton*.
%
%      VER_XTHERMOTOOLS_804('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in VER_XTHERMOTOOLS_804.M with the given input arguments.
%
%      VER_XTHERMOTOOLS_804('Property','Value',...) creates a new VER_XTHERMOTOOLS_804 or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before VER_XThermoTools_804_OpeningFcn gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to VER_XThermoTools_804_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help VER_XThermoTools_804

% Last Modified by GUIDE v2.5 24-Jul-2019 09:55:42

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @VER_XThermoTools_804_OpeningFcn, ...
                   'gui_OutputFcn',  @VER_XThermoTools_804_OutputFcn, ...
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



% #########################################################################
%       OPENING FUNCTION
function VER_XThermoTools_804_OpeningFcn(hObject, eventdata, handles, varargin)


disp(' ');
disp('____________________________________________________________________')
disp(' ');


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fortran libraries for Theriakd
setenv('DYLD_LIBRARY_PATH', '/usr/local/bin')
setenv('GFORTRAN_STDIN_UNIT', '5') 
setenv('GFORTRAN_STDOUT_UNIT', '6') 
setenv('GFORTRAN_STDERR_UNIT', '0')

% New 2.1.1
if ispc
    %set(gcf,'Position',[5,1.85,261,49]);
    handles.FileDirCode = 'file:/';
else
    set(gcf,'Position',[9.6667 0.7500 197.5000 55.7500]);
    handles.FileDirCode = 'file://';
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% Version
fid = fopen('XTT_Local_version.txt');
localVersion =fgetl(fid);
fclose(fid); 

localStr = strread(localVersion,'%s','delimiter','-');

Version = localStr{2};  
ReleaseDate = localStr{3};               
ProgramState = localStr{4};

set(handles.title2,'String',['Release ',char(Version),'-',char(ProgramState)]);
set(gcf,'Name',['XThermoTools ',char(Version)]);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% Config 
fid = fopen('XTT_Config.txt');
LocBase = char(fgetl(fid)); % main directory of xmap tools
TheriakBase = char(fgetl(fid)); % main directory of xmap tools
fclose(fid);
handles.LocBase = LocBase;
handles.TheriakBase = TheriakBase;

axes(handles.LOGO);
img = imread([LocBase,'/XTT_Dev/logo/logo_xtt_final.png'],'BackgroundColor',[0.9412    0.9412    0.9412]);
image(img); 
axis image
set(gca,'visible','off');

axes(handles.LOGO2);
img = imread([LocBase,'/XTT_Dev/logo/logo_BingoAntidote_final.png'],'BackgroundColor',[0.9412    0.9412    0.9412]);
image(img);
axis image
set(gca,'visible','off');

handles.BingoAntidoteLogo = img;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% DCheck database Version
handles = CheckDatabaseSTATE(handles);
%keyboard



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% Copy Theriak.ini for WINDOWS users
if ispc
    Check = exist([TheriakBase,'/theriak.ini']);
    
    if ~Check
        uiwait(msgbox({'Oups... ', ...
            ' ', ...
            'Dear PC user, XThermoTools did not find your file theriak.ini in the Theriak directory you specified during the setup.', ...
            ' ', ...
            'The program will continue but it seems that there is a serious issue with your setup. Maybe try to re-install XThermoTools'}, ...
            'XThermoTools','error','modal'));
    else
        [SUCCESS,MESSAGE,MESSAGEID] = copyfile([TheriakBase,'/theriak.ini'],[cd,'/theriak.ini'],'f');
    end
    
end


%XimshowX([LocBase,'/XTT_Dev/logo/logo_xtt_final.png']);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% Add paths
FunctionPath = strcat(LocBase,'/XTT_Functions');
addpath(FunctionPath);
FunctionPath = strcat(LocBase,'/XTT_Core');
addpath(FunctionPath);
ModulesPath = strcat(LocBase,'/XTT_Modules');
addpath(ModulesPath);
ModulesPath = strcat(LocBase,'/XTT_Dev');
addpath(ModulesPath);
%ModulesPath = strcat(LocBase,'/ThermoData');
%addpath(ModulesPath);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% Load Default Data
% (1) XTT_Def_ElDisp.txt
fid = fopen('XTT_Def_ElDisp.txt'); Compt = 0;
while 1
    lalign = fgetl(fid);
    if isequal(lalign,'') || isequal(lalign,-1)
        break
    end
    if ~isequal(lalign(1),'!')
        Compt = Compt+1;
        Temp = strread(lalign,'%s');
        DefElCodeDisp{Compt,1} = Temp{1};
        DefElCodeDisp{Compt,2} = Temp{2};
        DefElCodeDisp{Compt,3} = Temp{3};
    end
end
handles.DefElCodeDisp = DefElCodeDisp;

fid = fopen('XTT_Def_OxideName.txt'); Compt = 0;
while 1
    lalign = fgetl(fid);
    if isequal(lalign,'') || isequal(lalign,-1)
        break
    end
    if ~isequal(lalign(1),'!')
        Compt = Compt+1;
        Temp = strread(lalign,'%s');
        DefOxideName{Compt,1} = Temp{1};
        DefOxideName{Compt,2} = Temp{2};
        DefOxideName{Compt,3} = str2num(Temp{3});
        DefOxideName{Compt,4} = str2num(Temp{4});
        DefOxideName{Compt,5} = str2num(Temp{5});
    end
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BINGO initialization
%              Create the variable BingoDefault
BingoDefault = BingoInitialization(hObject, eventdata, handles);

% This path has been checked before to open XThermoTools...
if ispc
   BingoDefault.Theriak.Path = [handles.TheriakBase,'\theriak']; 
else
    BingoDefault.Theriak.Path = [handles.TheriakBase,'/theriak'];
end

handles.BingoDefault = BingoDefault;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% Create the variable BinBulk(i) .Type 
%                
handles.BinBulk(1).Type = 0;   % Bulk Composition
handles.BinBulk(1).Name = 'Bulk composition';
handles.BinBulk(1).CompositionOriginal = BingoDefault.Theriak.InputBulk;
handles.BinBulk(1).CompositionModified = BingoDefault.Theriak.InputBulk;
handles.BinBulk(1).CompositionIterative = BingoDefault.Theriak.InputBulk;
handles.BinBulk(1).CompoDisp = 1;
handles.BinBulk(1).DomainXrefYref = [0 0];
%handles.BinBulk(1).SelectedPhases = 0;

for i=1:length(handles.BinBulk)
    ListNamesBulk{i} = handles.BinBulk(i).Name;
end
set(handles.BinPopUpBulkCompoList,'String',ListNamesBulk);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% Create the variable SF (Structural formula) .DefOxide 
%                                             .DefElem 
%                                             .ListOxide
%                                             .ListElem
Compt = 0;
PreviousElem = '';

for i=1:size(DefOxideName,1)
    TheElem = DefOxideName{i,1};
    if ~isequal(TheElem,PreviousElem)
        Compt = Compt+1;
        PreviousElem = TheElem;
        
        SF.ListElem{Compt} = TheElem;
        SF.DefElem(Compt).Name = TheElem;
        
    end
    SF.DefOxide(Compt).NameOxi = DefOxideName{i,2};
    SF.DefOxide(Compt).IndiceOxi = i;
    SF.DefOxide(Compt).Elem = TheElem;
    SF.DefOxide(Compt).IndiceElem = Compt;
    SF.DefOxide(Compt).MolarMass = DefOxideName{i,3};
    SF.DefOxide(Compt).NbAtoms = DefOxideName{i,4};
    SF.DefOxide(Compt).NbOxygen = DefOxideName{i,5};
    
    SF.ListOxide{i} = DefOxideName{i,2};
    
    ListElemRepet{i} = TheElem;
    
end

for i=1:length(SF.DefElem)
    TheSum = find(ismember(ListElemRepet,SF.DefElem(i).Name));
    SF.DefElem(i).Sum = TheSum;
end

handles.SF = SF;
handles.Compositions = [];


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% Create the variable MemoryReports 
% 
handles.MemoryReports.List = {'no result available so far ...'};
handles.MemoryReports.Nb = 0;
handles.MemoryReports.NbMax = 5;         % use this variable to set the max 
                                                 % number of reports stored
handles.MemoryReports.Data = [];


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% Update Antidote Menu
[AntidoteMenu] = Recipe_MenuDef(1);
set(handles.BinPopUpListeAntidotes,'String',AntidoteMenu);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% Display
handles.rotateFig = 0;

if length(varargin)
    handles.WereWeAre = 1;
    
    % Import Data from XMapTools
    TheQuanti = varargin{1};
    TheMaskFile = varargin{2};
    
    % Correct the maskfile for the pixels used...
    SumOxide = zeros(size(TheQuanti.elem(1).quanti));
    for i = 1:length(TheQuanti.elem)
        SumOxide = SumOxide + TheQuanti.elem(i).quanti;
    end
    RejectedPixels = find(~SumOxide(:));
    RejectedPixelsTotal = RejectedPixels;
    NbPixelsTotal = size(SumOxide,1)*size(SumOxide,2);
    
    MaksPx = TheMaskFile.Mask(RejectedPixels);
    TheMaskFile.Mask(RejectedPixels) = zeros(size(RejectedPixels));
    
    
    % --- (1) handles.MapWindow.Mask: 
    handles.MapWindow.Mask.Data = TheMaskFile.Mask; 
    handles.MapWindow.Mask.DensityMap = TheMaskFile.DensityMap;
    
    for i=2:length(TheMaskFile.NameMinerals)
        ListMin{i-1} = TheMaskFile.NameMinerals{i};
    end
    
    handles.MapWindow.Mask.ListMin = ListMin;
    handles.MapWindow.Mask.Selected = ones(1,length(ListMin)); % For display
    
    handles.MapWindow.Mask.DispListMinSelected4Bulk = ListMin;
    handles.MapWindow.Mask.AvailableFromXMap = ones(1,length(ListMin)); % (0) not available / (1) available
    handles.MapWindow.Mask.Selected4Bulk = ones(1,length(ListMin));
    
    % Update the selected and available MAPS: 
    
    for i=1:length(ListMin)
        TotalPixels = length(find(TheMaskFile.Mask == i));
        RejectedPixels = length(find(MaksPx == i));
        
        if TotalPixels > 0 && TotalPixels > RejectedPixels
            handles.MapWindow.Mask.AvailableFromXMap(i) = 1;
            handles.MapWindow.Mask.Selected4Bulk(i) = 1;
            %handles.MapWindow.Mask.DispListMinSelected4Bulk{i} = char(char(handles.MapWindow.Mask.ListMin{i}));
        else
            handles.MapWindow.Mask.AvailableFromXMap(i) = 0;
            handles.MapWindow.Mask.Selected4Bulk(i) = 0;
            handles.MapWindow.Mask.Selected(i) = 0;
            %handles.MapWindow.Mask.DispListMinSelected4Bulk{i} = ['*NA* ',char(char(handles.MapWindow.Mask.ListMin{i}))];
        end
        
    end
    
    guidata(hObject, handles);
    UpdateSelectedPhases4Bulk(hObject, eventdata, handles);

    for i = 1:length(handles.MapWindow.Mask.Selected4Bulk)
        if isequal(handles.MapWindow.Mask.Selected4Bulk(i),1)
            handles.MapWindow.Mask.Selected(i) = 1;
        else
            handles.MapWindow.Mask.Selected(i) = 0;
        end
    end
    
    set(handles.DispPhases4BulkList,'String',[num2str(sum(handles.MapWindow.Mask.Selected4Bulk)),' phase(s) selected (',num2str(sum(handles.MapWindow.Mask.AvailableFromXMap)),'/',num2str(length(handles.MapWindow.Mask.Selected4Bulk)),' are available)']);
 
    handles.MapWindow.Mask.DispListMinSelected4Bulk = get(handles.BinPopUpBulkPhases4BulkList,'String');

    % --- (2) handles.MapWindow.Maps:
    handles.MapWindow.Maps.ListMaps = TheQuanti.listname;
    
    for i=1:length(TheQuanti.elem)
        Data(i).ValuesOx = TheQuanti.elem(i).quanti;
    end
    
    handles.MapWindow.Maps.Data = Data;
    handles.MapWindow.ElCode4Display = char(handles.MapWindow.Maps.ListMaps{1});  % We display the first element
    
    handles.MapWindow.DefMapDisplay = {'1','1',char(handles.MapWindow.Maps.ListMaps{1})};
    
    guidata(hObject, handles);
    DisplayMapWindow(hObject, eventdata, handles);

    
else
    handles.WereWeAre = 0;
    % We can load a backup
    
end

disp(' ');
disp(['----------------- IMPORTED DATA FROM XMAPTOOLS -----------------'])
disp(' ');

fprintf('\t%s\t%.0f%s%.0f\t%s%.2f%s\n','-> rejected pixels:',length(RejectedPixelsTotal),'/',NbPixelsTotal,' (',length(RejectedPixelsTotal)/NbPixelsTotal*100,'%)')

if length(RejectedPixels)
    for i=1:TheMaskFile.Nb
        NbPxOut = length(find(MaksPx(:) == i));
        if NbPxOut
            fprintf('\t\t%s\t%.0f%s%.0f\t%s%.2f%s\n',char(TheMaskFile.NameMinerals{i+1}),NbPxOut,'/',NbPixelsTotal,' (',NbPxOut/NbPixelsTotal*100,'%)')
        end
    end
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Create the variable BinPhaseDef 
%                                 
% 

for i=1:length(handles.MapWindow.Mask.ListMin)
    BinPhaseDef(i).name = handles.MapWindow.Mask.ListMin{i};
    BinPhaseDef(i).IsPhaseIn = 0;
    BinPhaseDef(i).OxBasis = [];
    BinPhaseDef(i).ListVariAntidote = [];
    BinPhaseDef(i).ListWeightAntidote = [];
    BinPhaseDef(i).DBName = '';
    BinPhaseDef(i).DBMinName = '';
    BinPhaseDef(i).PhaseVolProp = [];
    BinPhaseDef(i).NbGrps = 0;
    BinPhaseDef(i).SelGrps = 0;
    BinPhaseDef(i).VolGrps = [];
    BinPhaseDef(i).Grps(1).IsGrp = 0;
    BinPhaseDef(i).Grps(1).Name = '';
    BinPhaseDef(i).Grps(1).XrefYref = [];
    BinPhaseDef(i).Grps(1).OxideCompositions = [];
    BinPhaseDef(i).Grps(1).SigmaOxide = [];
    BinPhaseDef(i).Grps(1).OxideLabels = {};
    BinPhaseDef(i).Grps(1).SFCompositions = [];
    BinPhaseDef(i).Grps(1).SigmaSF = [];
    BinPhaseDef(i).Grps(1).SFLabels = {};
end
handles.BinPhaseDef = BinPhaseDef;


set(handles.BinPopUpPhaseList,'String',handles.MapWindow.Mask.ListMin);
 
set(handles.ButtonInvTool,'Value',0);
set(handles.UiPanelInvTool,'Visible','off');
set(handles.ButtonForTool,'Value',0);
set(handles.UiPanelForTool,'Visible','off');

set(handles.ButtonCombTool,'Value',1);
set(handles.UiPanelCombTool,'Visible','on');

set(handles.SWOT,'string','','visible','off');

set(handles.InvReacWaitBar3,'Position',[14.167 24.167 0.0001 0.9]);


% Update the display

FileName = 'OptionsXTT.txt';
[OptString,Valid] = ReadOptionXTT(FileName);

set(handles.BinTextBinTC,'String',OptString{3});
set(handles.BinTextBinP,'String',OptString{4});

set(handles.BinTextAnt1,'String',OptString{5});
set(handles.BinTextAnt2,'String',OptString{6});
set(handles.BinTextAnt3,'String',OptString{7});
set(handles.BinTextAnt4,'String',OptString{8});

set(handles.BinTextBinTCminmax,'String',OptString{9});
set(handles.BinTextBinPminmax,'String',OptString{10});

disp(' ');
if Valid
    fprintf('\t%s\t\t%s\n','-> options:','from OptionsXTT.txt');
else
    fprintf('\t%s\t\t%s\n','-> options:','default (OptionsXTT.txt not found)');
end
for i=1:length(OptString)
    fprintf('\t\t%s\t\t%s\n',['opt',num2str(i)],char(OptString{i}));
end


% Bingo-Antidote: 
handles = BinPopUpProgram_Callback(hObject, eventdata, handles);
handles = BinPopUpDatabase_Callback(hObject, eventdata, handles);

set(handles.BinTextBulkCompo,'String',char(handles.BinBulk(1).CompositionModified));

axes(handles.axes2)
xlabel('Variable 1'), ylabel('Variable 2')

% Initialize the display workspace
InitPanelDisp(1,handles);

% Default Values... 
handles.AssemblageCode = ones(1,6);

guidata(hObject, handles);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Adjust the display
%                                 
% 
UnitBefore = get(gcf,'Units');
set(gcf,'Units','pixels');

%disp('_ After changing to pixels')
%disp([get(gcf,'Units'),' ',num2str(get(gcf,'Position'))])

FigPosition = get(gcf,'Position');
ScreenSize = get(0,'ScreenSize');

Ratio = 0.6121;
ProportionScreen = 0.85;
RatioScreen = ScreenSize(4)/ScreenSize(3);

if RatioScreen < Ratio
    FigHeight = ScreenSize(4)*ProportionScreen;
    FigWidth = FigHeight/Ratio;
else
    FigWidth = ScreenSize(3)*ProportionScreen;
    FigHeight = FigWidth*Ratio;
end
FigX = (ScreenSize(3)-FigWidth)/2;
FigY = (ScreenSize(4)-FigHeight)/3;

set(gcf,'Position',[FigX,FigY,FigWidth,FigHeight]) 
drawnow 

% RUN wereweare functions
BinWereWeAre(hObject, eventdata, handles);

set(gcf, 'WindowButtonMotionFcn', @mouseMove);

handles.output = hObject;
guidata(hObject, handles);



disp(' '); disp(' ');


% Not used anymore ____________________________________________


return


% --- Outputs from this function are returned to the command line.
function varargout = VER_XThermoTools_804_OutputFcn(hObject, eventdata, handles) 
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output;
return


% --- Executes when user attempts to close figure1.
function figure1_CloseRequestFcn(hObject, eventdata, handles)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% Remove paths
LocBase = handles.LocBase;

% Removepath:
FunctionPath = strcat(LocBase,'/XTT_Functions');
rmpath(FunctionPath);
FunctionPath = strcat(LocBase,'/XTT_Core');
rmpath(FunctionPath);
ModulesPath = strcat(LocBase,'/XTT_Modules');
rmpath(ModulesPath);
ModulesPath = strcat(LocBase,'/XTT_Dev');
rmpath(ModulesPath);



delete(hObject);
return


% DATABASE CHECK AND UPDATE

function handles = CheckDatabaseSTATE(handles)
%

fid = fopen([handles.LocBase,'/XTT_Databases/Database_Local_Version.txt'],'r'); 
DB_Local =fgetl(fid); 
fclose(fid);

[State,DB_Online,DB_Local] = CheckDBUpdates(DB_Local);

if State
    set(handles.VersionDB,'String','Update available ...','ForegroundColor',[1,0,0]);
    set(handles.Menu_UpdateDB,'Label','Databases: Download & Update','Enable','On')
else
    set(handles.VersionDB,'String',['VER_',DB_Local],'ForegroundColor',[0,0,1]);
    set(handles.Menu_UpdateDB,'Label','Databases: No Update Available','Enable','Off')
end

function [State,Online,Local] = CheckDBUpdates(DB_Local)
%

LocalStr = strread(DB_Local,'%s','delimiter','-');
Local = LocalStr{2};
Online = '';

State = 0; % No update or no internet

[DB_Online,flag] = urlread('http://www.xmaptools.com/FTP_addons/XThermoTools/Database_version.php');

if flag && length(DB_Online) 
    
    OnlineStr = strread(DB_Online,'%s','delimiter','-');
    
    Online = OnlineStr{2};
    
    OnlineNum = strread(Online,'%f','delimiter','.');
    LocalNum = strread(Local,'%f','delimiter','.');
    
    ServerDB = (OnlineNum(1)+OnlineNum(2)*0.01);
    LocalDB = (LocalNum(1)+LocalNum(2)*0.01);

    if ServerDB > LocalDB
        State = 1;
    end
end


return

function Menu_UpdateDB_Callback(hObject, eventdata, handles)
%

[onlinePackage,flag] = urlread('http://www.xmaptools.com/FTP_addons/XThermoTools/Database_package.php');

if flag && length(onlinePackage) 
    LocBase = handles.LocBase;
    
    WhereWeAre = cd;
    h = waitbar(0,'Database Update - Please wait...');
    waitbar(0.1,h);
    
    cd(LocBase);
    waitbar(0.25,h);
    
    % Copy the DATABASE directory
    [SUCCESS,MESSAGE,MESSAGEID] = copyfile([LocBase,'/XTT_Databases'],[LocBase,'/XTT_Databases_TEMP'],'f');
    waitbar(0.4,h);
    
    rmdir([LocBase,'/XTT_Databases'],'s');
    waitbar(0.6,h);
    
    WebAdress = ['http://www.xmaptools.com/FTP_addons/XThermoTools/',char(onlinePackage)];
    unzip(WebAdress);
    
    if isdir('__MACOSX')
        [status,msg,msgID] = rmdir('__MACOSX', 's');
    end
    waitbar(0.8,h);
    
    rmdir([LocBase,'/XTT_Databases_TEMP'],'s');
    waitbar(0.9,h);
    
    cd(WhereWeAre);
    waitbar(1,h);
    
    % Update is done:
    close(h);
    
    % Check and update display...
    handles = CheckDatabaseSTATE(handles);
    guidata(hObject, handles);
else
    errordlg('The update server cannot be reached!','XThermoTools');
    return
end
return




% -------------------------------------------------------------------------
%                         M O D E    D I S P L A Y
% -------------------------------------------------------------------------


% #########################################################################
%       DISPLAY MAP WINDOW (1.1.1)
function [TheMap4Display] = DisplayMapWindow(hObject, eventdata, handles)



MapWindow = handles.MapWindow;


% Check that the element is available
TheElement4Display = MapWindow.ElCode4Display;
TheElement4DisplayClean = TheElement4Display;

TheLetters = isstrprop(TheElement4DisplayClean,'alpha')+isstrprop(TheElement4DisplayClean,'digit');
for i=1:length(TheLetters)
    if ~TheLetters(i)
        TheElement4DisplayClean(i) = ' ';
    end
end
TheElementsInCode = strread(TheElement4DisplayClean,'%s');

[Ok,Ou] = ismember(TheElementsInCode,MapWindow.Maps.ListMaps);

% We change Ok for the Digits number which are not tested (see after). 
for i=1:length(Ok)
    if ~Ok(i)
        ThisOne = TheElementsInCode{i};
        TheDigits = isstrprop(ThisOne,'digit');
        
        if sum(TheDigits) == length(TheDigits)
            Ok(i) = 1;
        end
    end
end

if sum(Ok) ~= length(Ok)
    warndlg({'Element display Error. No corresponding map for the following elements:',char(TheElementsInCode{find(abs(Ok-1))})},'Warning');
    return
end
    

% Prepare the element variables run ElCode4Display
for i = 1:length(MapWindow.Maps.ListMaps)
    eval([char(MapWindow.Maps.ListMaps{i}),' = MapWindow.Maps.Data(i).ValuesOx;']);
end

eval(['TheElementalMap = ',char(MapWindow.ElCode4Display),';']);

% Clear the element variables;
for i = 1:length(MapWindow.Maps.ListMaps)
    eval(['clear ',char(MapWindow.Maps.ListMaps{i})]);
end


% Read TheMask;
TheMask = zeros(size(TheElementalMap));
for i=1:length(MapWindow.Mask.Selected)
    if MapWindow.Mask.Selected(i)
        TheMask(find(MapWindow.Mask.Data == i)) = ones(1,length(find(MapWindow.Mask.Data == i)));
    end
end

TheMap4Display = TheElementalMap .* TheMask;

WereIsNan = find(isnan(TheMap4Display));
TheMap4Display(WereIsNan) = zeros(size(WereIsNan));

% Display
cla(handles.axes1)
axes(handles.axes1)

imagesc(XimrotateX(TheMap4Display,handles.rotateFig)), axis image, colorbar vertical
if ~length(find(TheMap4Display))
    caxis([0,1]);
else
    caxis([min(TheMap4Display(find(TheMap4Display))),max(TheMap4Display(find(TheMap4Display)))])
end
colormap([0,0,0;RdYlBu(64)])
set(handles.axes1,'xtick',[], 'ytick',[]); 

set(handles.TextElDisp,'String',TheElement4Display);

set(gcf, 'WindowButtonMotionFcn', @mouseMove);

ActivateZoomFigure1(hObject, eventdata, handles);

Compt = 0; 
LesNameMin4Disp = '';

for i=1:length(MapWindow.Mask.Selected)
    if MapWindow.Mask.Selected(i)
        Compt = Compt+1;
        LesNameMin4Disp{Compt} = MapWindow.Mask.ListMin{i};
    end
end
set(handles.TextMinDisp,'String',LesNameMin4Disp);

return


% #########################################################################
%       MODE FUNCTION (INVTOOL, FORTOOL, COMBTOOL) (1.1.1)
function [] = XTTModeFunction(hObject, eventdata, handles)
Mode = handles.WindowMode;

switch Mode
    case 1
        set(handles.ButtonInvTool,'Value',1);
        set(handles.UiPanelInvTool,'Visible','on');
        
        set(handles.ButtonForTool,'Value',0);
        set(handles.UiPanelForTool,'Visible','off');
        
        set(handles.ButtonCombTool,'Value',0);
        set(handles.UiPanelCombTool,'Visible','off');
        
    case 2
        
        set(handles.ButtonInvTool,'Value',0);
        set(handles.UiPanelInvTool,'Visible','off');
        
        set(handles.ButtonForTool,'Value',1);
        set(handles.UiPanelForTool,'Visible','on');
        
        set(handles.ButtonCombTool,'Value',0);
        set(handles.UiPanelCombTool,'Visible','off');
        
    case 3
        
        set(handles.ButtonInvTool,'Value',0);
        set(handles.UiPanelInvTool,'Visible','off');
        
        set(handles.ButtonForTool,'Value',0);
        set(handles.UiPanelForTool,'Visible','off');
        
        set(handles.ButtonCombTool,'Value',1);
        set(handles.UiPanelCombTool,'Visible','on');
        
end

return


% #########################################################################
%       INVTOOL BUTTON (1.1.1)
function ButtonInvTool_Callback(hObject, eventdata, handles)
handles.WindowMode = 1;
guidata(hObject, handles);
XTTModeFunction(hObject, eventdata, handles)


% We start from nothing...
handles.InvWere(1).WeAre = [0,0];          % [ThermoDataSet,ThermoSSmodels]
handles.InvWere(2).WeAre = [0];                            % [ChemicalSyst]
handles.InvWere(3).WeAre = [0];                           % [InvAssemblage]
handles.InvWere(4).WeAre = [0];                               % [Reactions]
handles.InvWere(5).WeAre = [0];                            % [Compositions]

guidata(hObject, handles);
InvWereWeAre(hObject, eventdata, handles);

return


% #########################################################################
%       FORTOOL BUTTON (1.1.1)
function ButtonForTool_Callback(hObject, eventdata, handles)
handles.WindowMode = 2;
guidata(hObject, handles);
XTTModeFunction(hObject, eventdata, handles)
return


% #########################################################################
%       COMBTOOL BUTTON (1.1.1)
function ButtonCombTool_Callback(hObject, eventdata, handles)
handles.WindowMode = 3;
guidata(hObject, handles);
XTTModeFunction(hObject, eventdata, handles)
return


% #########################################################################
%       CLEAN DIRECTORY BUTTON (1.2.2)
function BinButtonCleanDir_Callback(hObject, eventdata, handles)

FileName = {'XBIN','OUT','theriak.last','clean','coplot','domino.last', ...
    'explot.last','gitter','guzzler.last','table'};

for i=1:length(FileName)
    if exist([cd,'/',char(FileName{i})])
        delete([cd,'/',char(FileName{i})]);
    end    
end
msgbox('Cleanning done!','XThermoTools','help');

return




% -------------------------------------------------------------------------
%                           F I G U R E  (1)
% -------------------------------------------------------------------------


% #########################################################################
%       ACTIVATE ZOOM (WITHOUT LOGO) (1.3.1)
function ActivateZoomFigure1(hObject, eventdata, handles)
%

axes(handles.axes1);
zoom on
% zoom reset

axes(handles.LOGO);
h1 = zoom;
setAllowAxesZoom(h1,gca,0);

axes(handles.LOGO2);
h2 = zoom;
setAllowAxesZoom(h2,gca,0);

axes(handles.axes_PLOT1);
h3 = zoom;
setAllowAxesZoom(h3,gca,0);

axes(handles.axes_PLOT2);
h4 = zoom;
setAllowAxesZoom(h4,gca,0);

return


% #########################################################################
%       BUTTON SELECT ELEMENTS FOR DISPLAY MAP (1.1.1)
function ButtonElSet_Callback(hObject, eventdata, handles)

ListMap = handles.MapWindow.Maps.ListMaps;   %{'SIO2','AL2O3','FEO','FE2O3','K2O'};
DefElCodeDisp = handles.DefElCodeDisp;  %{'MG#','MGO.(FEO+MGO)','MGO.(FEO+MGO)';'FE#','FEO./(FEO+MGO)','FEO./(FEO+MGO)'};
DefMapDisplay = handles.MapWindow.DefMapDisplay;  %{'1','1','SIO2'};

[handles.MapWindow.ElCode4Display,handles.MapWindow.DefMapDisplay] = XTTsetElMap(ListMap,DefElCodeDisp,DefMapDisplay,gcf);

set(handles.ButtonAuto1,'String','Auto');

guidata(hObject, handles);
DisplayMapWindow(hObject, eventdata, handles);

return


% #########################################################################
%       BUTTON AUTO-CONTRAST (1.1.1)
function ButtonAuto1_Callback(hObject, eventdata, handles)

% On bosse sur le premier Child qui est forcement la carte... 

lesInd = get(handles.axes1,'child');
for i=1:length(lesInd)
    if length(get(lesInd(i),'type')) == 5 % image 
        AADonnees = get(lesInd(i),'CData');
        break
    else
        AADonnees = [];
    end
end

Triee = sort(AADonnees(:));
for i=1:length(Triee)
	if Triee(i) > 0 && ~isnan(Triee(i)) % defined
    	break 
    end
end

NTriee = Triee(i:end);

Val = round(length(NTriee) * 0.065); % voir avec une detection du pic.


TheButtonName = get(handles.ButtonAuto1,'String');

switch TheButtonName
    
    case 'Auto'
        set(handles.ButtonAuto1,'String','Reset');
        
        
        axes(handles.axes1)
        if Val > 1
            if NTriee(Val) < NTriee(length(NTriee)-Val)
                % V1.4.2 sans caxis
                set(handles.axes1,'CLim',[NTriee(Val),NTriee(length(NTriee)-Val)])
                %caxis([NTriee(Val),NTriee(length(NTriee)-Val)]);
            end
        else
            return % no posibility to update (size  = 0)
        end
        
        
    case 'Reset'
        set(handles.ButtonAuto1,'String','Auto');

        set(handles.axes1,'CLim',[min(AADonnees(:)),max(AADonnees(:))]);
end


guidata(hObject,handles);
return


% #########################################################################
%       BUTTON SELECT PHASES FIGURE 1 (1.1.1)
function ButtonMinSet_Callback(hObject, eventdata, handles)

[Selected] = XTTselectMinerals20(handles.MapWindow.Mask.ListMin,handles.MapWindow.Mask.Selected,handles.MapWindow.Mask.Selected4Bulk,gcf,'Select phases to PLOT in the map');

handles.MapWindow.Mask.Selected = Selected;

guidata(hObject, handles);
DisplayMapWindow(hObject, eventdata, handles);
return


% #########################################################################
%       BUTTON ADJUST MIN FIGURE 1 (1.1.1)
function ButtonAdjust_Callback(hObject, eventdata, handles)


axes(handles.axes1)
a = get(handles.axes1,'CLim');

TheCMin = a(1);
TheCMax = a(2);


[OutPutValues] = inputdlg({'CMin','CMax'}','Input',[1;1],{num2str(TheCMin),num2str(TheCMax)});

NewCMin = str2num(OutPutValues{1});
NewCMax = str2num(OutPutValues{2});

axes(handles.axes1)
if NewCMin <= NewCMax
    set(handles.axes1,'CLim',[NewCMin,NewCMax]);
    
    set(handles.ButtonAuto1,'String','Auto');
    
end



return


% #########################################################################
%       EXPORT FIGURE 1 (1.1.1)
function ButtonExport1_Callback(hObject, eventdata, handles)
% La maintenant la grande question est comment rcuprer la colorbar
axes(handles.axes1)
CMap = colormap;

lesInd = get(handles.axes1,'child');

CLim = get(handles.axes1,'CLim');
YDir = get(handles.axes1,'YDir');

figure, 
hold on

% On trace d'abord les images...
for i=1:length(lesInd)
    leType = get(lesInd(i),'Type');
    if length(leType) == 5
        if leType == 'image';
            imagesc(get(lesInd(i),'CData')), axis image
        end
    end
    
end


% ensuite les lignes
for i=1:length(lesInd)
    leType = get(lesInd(i),'Type');
    if length(leType) == 4
        if leType == 'line';
            plot(get(lesInd(i),'XData'),get(lesInd(i),'YData'),'Marker',get(lesInd(i),'Marker'),'Color',get(lesInd(i),'Color'),'LineStyle',get(lesInd(i),'LineStyle'),'LineWidth',get(lesInd(i),'LineWidth'), ...
                'MarkerEdgeColor',get(lesInd(i),'MarkerEdgeColor'),'MarkerFaceColor',get(lesInd(i),'MarkerFaceColor'),'Markersize',get(lesInd(i),'MarkerSize')) % prpopriets ici
        end
    end
    
end

% puis les textes
for i=1:length(lesInd)
    leType = get(lesInd(i),'Type');
    if length(leType) == 4
        if leType == 'text'
            LaPosition = get(lesInd(i),'Position');
            LeTxt = text(LaPosition(1),LaPosition(2),get(lesInd(i),'String'));
            set(LeTxt,'Color',get(lesInd(i),'Color'),'BackgroundColor',get(lesInd(i),'BackgroundColor'), ...
                'FontName',get(lesInd(i),'FontName'),'FontSize',get(lesInd(i),'FontSize'));
        end
    end
end



set(gca,'CLim',CLim);
set(gca,'YDir',YDir);
set(gca,'xtick',[], 'ytick',[]);
set(gca,'box','on')
set(gca,'LineStyleOrder','-')
set(gca,'LineWidth',0.5)

colormap(CMap)
colorbar horizontal
box on

return


% #########################################################################
%       CLEAN FIGURES 1 (1.2.1)
function ButtonClean_Callback(hObject, eventdata, handles)
%
lesInd = get(handles.axes1,'child');
for i=1:length(lesInd)
    leType = get(lesInd(i),'Type');
    if ~isequal(leType,'image');
        delete(lesInd(i));
    end   
end
axes(handles.axes1);
set(gcf, 'WindowButtonMotionFcn', @mouseMove);

ActivateZoomFigure1(hObject, eventdata, handles);


CleanAxes2(handles);
return


function CleanAxes2(handles)
%

axes(handles.axes2), cla
eval(['TCi = [',get(handles.BinTextBinTCminmax,'String'),'];']);
eval(['Pi = [',get(handles.BinTextBinPminmax,'String'),'];']);

axis([TCi(1) TCi(end) Pi(1) Pi(end)]);
xlabel('Temperature');
ylabel('Pressure');
drawnow
return

% from XMapTools...  


% #########################################################################
%     ROTATE FIGURE (1.1.1)
function ButtonRotate_Callback(hObject, eventdata, handles)

previousRotate = handles.rotateFig;
    
handles.rotateFig = handles.rotateFig + 90;
if handles.rotateFig >= 360
    handles.rotateFig = 0;
end 

guidata(hObject,handles);

DisplayMapWindow(hObject, eventdata, handles);

ActivateZoomFigure1(hObject, eventdata, handles);

return





% Adapted from the XMapTools function (30.01.2016)
function mouseMove(hObject, eventdata) 
handles = guidata(hObject);

laSize = size(XimrotateX(handles.MapWindow.Maps(1).Data(1).ValuesOx,handles.rotateFig));

lesX = [0 laSize(2)]; %get(handles.axes1,'XLim');
lesY = [0 laSize(1)]; %get(handles.axes1,'YLim');

xLength = lesX(2)-lesX(1);
yLength = lesY(2)-lesY(1);

C = get (handles.axes1, 'CurrentPoint');

lesInd = get(handles.axes1,'child');

% On extrait l'image...
for i=1:length(lesInd)
    leType = get(lesInd(i),'Type');
    if length(leType) == 5
        if leType == 'image';
            Data = get(lesInd(i),'CData');
        end
    end   
end

if C(1,1) >= 0 && C(1,1) <= lesX(2) && C(1,2) >= 0 && C(1,2) <= lesY(2)

    set(gcf,'pointer','crosshair');

    switch handles.rotateFig

        case 0

            set(handles.LiveCoordX, 'string', round(C(1,1))); %abscisse
            set(handles.LiveCoordY, 'string', round(C(1,2))); %ordonn?e

        case 90
            set(handles.LiveCoordX, 'string', round(yLength - C(1,2))); %abscisse
            set(handles.LiveCoordY, 'string', round(C(1,1))); %ordonn?e

        case 180
            set(handles.LiveCoordX, 'string', round(xLength - C(1,1))); %abscisse
            set(handles.LiveCoordY, 'string', round(yLength - C(1,2))); %ordonn?e

        case 270
            set(handles.LiveCoordX, 'string', round(C(1,2))); %abscisse
            set(handles.LiveCoordY, 'string', round(xLength - C(1,1))); %ordonn?e            

    end

    % no rotation here because we read the map !!!

    TheX = round(C(1,2));
    TheY = round(C(1,1));

    if ~TheX, TheX = 1; end   % This is to fix a border error
    if ~TheY, TheY = 1; end

    TheZ = Data(TheX,TheY);
    set(handles.LiveCoordZ, 'string', num2str(TheZ));


else
    set(gcf,'pointer','arrow');
    set(handles.LiveCoordX, 'string', '...'); %abscisse
    set(handles.LiveCoordY, 'string', '...'); %ordonn?e
    set(handles.LiveCoordZ, 'string', '...'); %value
end

    
return



% #########################################################################
%       XkmeansX function  (V1.6.2)
function [idx, C, sumD, D] = XkmeansX(X, k, varargin)
%
% k-means for VER_XMapTools_750
% P. Lanari (Sept 2012)
% 
%
%

if nargin < 2
    error('At least two input arguments required.');
end

% n points in p dimensional space
[n, p] = size(X);
Xsort = []; Xord = [];

pnames = {   'distance'  'start' 'replicates' 'maxiter' 'emptyaction' 'display'};
dflts =  {'sqeuclidean' 'sample'          []       100        'error'  'notify'};
[errmsg,distance,start,reps,maxit,emptyact,display] ...
                       = statgetargs(pnames, dflts, varargin{:});
error(errmsg);

if ischar(distance)
    distNames = {'sqeuclidean','cityblock','cosine','correlation','hamming'};
    i = strmatch(lower(distance), distNames);
    if length(i) > 1
        error(sprintf('Ambiguous ''distance'' parameter value:  %s.', distance));
    elseif isempty(i)
        error(sprintf('Unknown ''distance'' parameter value:  %s.', distance));
    end
    distance = distNames{i};
    switch distance 
    case 'cityblock'
        [Xsort,Xord] = sort(X,1);
    case 'cosine'
        Xnorm = sqrt(sum(X.^2, 2));
        if any(min(Xnorm) <= eps * max(Xnorm))
            error(['Some points have small relative magnitudes, making them ', ...
                   'effectively zero.\nEither remove those points, or choose a ', ...
                   'distance other than ''cosine''.'], []);
        end
        X = X ./ Xnorm(:,ones(1,p));
    case 'correlation'
        X = X - repmat(mean(X,2),1,p);
        Xnorm = sqrt(sum(X.^2, 2));
        if any(min(Xnorm) <= eps * max(Xnorm))
            error(['Some points have small relative standard deviations, making them ', ...
                   'effectively constant.\nEither remove those points, or choose a ', ...
                   'distance other than ''correlation''.'], []);
        end
        X = X ./ Xnorm(:,ones(1,p));
    case 'hamming'
        if ~all(ismember(X(:),[0 1]))
            error('Non-binary data cannot be clustered using Hamming distance.');
        end
    end
else
    error('The ''distance'' parameter value must be a string.');
end

if ischar(start)
    startNames = {'uniform','sample','cluster'};
    i = strmatch(lower(start), startNames);
    if length(i) > 1
        error(sprintf('Ambiguous ''start'' parameter value:  %s.', start));
    elseif isempty(i)
        error(sprintf('Unknown ''start'' parameter value:  %s.', start));
    elseif isempty(k)
        error('You must specify the number of clusters, K.');
    end
    start = startNames{i};
    if strcmp(start, 'uniform')
        if strcmp(distance, 'hamming')
            error('Hamming distance cannot be initialized with uniform random values.');
        end
        Xmins = min(X,1);
        Xmaxs = max(X,1);
    end
elseif isnumeric(start)
    CC = start;
    start = 'numeric';
    if isempty(k)
        k = size(CC,1);
    elseif k ~= size(CC,1);
        error('The ''start'' matrix must have K rows.');
    elseif size(CC,2) ~= p
        error('The ''start'' matrix must have the same number of columns as X.');
    end
    if isempty(reps)
        reps = size(CC,3);
    elseif reps ~= size(CC,3);
        error('The third dimension of the ''start'' array must match the ''replicates'' parameter value.');
    end
    
    % Need to center explicit starting points for 'correlation'. (Re)normalization
    % for 'cosine'/'correlation' is done at each iteration.
    if isequal(distance, 'correlation')
        CC = CC - repmat(mean(CC,2),[1,p,1]);
    end
else
    error('The ''start'' parameter value must be a string or a numeric matrix or array.');
end

if ischar(emptyact)
    emptyactNames = {'error','drop','singleton'};
    i = strmatch(lower(emptyact), emptyactNames);
    if length(i) > 1
        error(sprintf('Ambiguous ''emptyaction'' parameter value:  %s.', emptyact));
    elseif isempty(i)
        error(sprintf('Unknown ''emptyaction'' parameter value:  %s.', emptyact));
    end
    emptyact = emptyactNames{i};
else
    error('The ''emptyaction'' parameter value must be a string.');
end

if ischar(display)
    i = strmatch(lower(display), strvcat('off','notify','final','iter'));
    if length(i) > 1
        error(sprintf('Ambiguous ''display'' parameter value:  %s.', display));
    elseif isempty(i)
        error(sprintf('Unknown ''display'' parameter value:  %s.', display));
    end
    display = i-1;
else
    error('The ''display'' parameter value must be a string.');
end

if k == 1
    error('The number of clusters must be greater than 1.');
elseif n < k
    error('X must have more rows than the number of clusters.');
end

% Assume one replicate
if isempty(reps)
    reps = 1;
end

%
% Done with input argument processing, begin clustering
%

dispfmt = '%6d\t%6d\t%8d\t%12g';
D = repmat(NaN,n,k);   % point-to-cluster distances
Del = repmat(NaN,n,k); % reassignment criterion
m = zeros(k,1);

totsumDBest = Inf;
for rep = 1:reps
    switch start
    case 'uniform'
        C = unifrnd(Xmins(ones(k,1),:), Xmaxs(ones(k,1),:));
        % For 'cosine' and 'correlation', these are uniform inside a subset
        % of the unit hypersphere.  Still need to center them for
        % 'correlation'.  (Re)normalization for 'cosine'/'correlation' is
        % done at each iteration.
        if isequal(distance, 'correlation')
            C = C - repmat(mean(C,2),1,p);
        end
    case 'sample'
        C = double(X(randsample(n,k),:)); % X may be logical
    case 'cluster'
        Xsubset = X(randsample(n,floor(.1*n)),:);
        [dum, C] = kmeans(Xsubset, k, varargin{:}, 'start','sample', 'replicates',1);
    case 'numeric'
        C = CC(:,:,rep);
    end    
    changed = 1:k; % everything is newly assigned
    idx = zeros(n,1);
    totsumD = Inf;
    
    if display > 2 % 'iter'
        disp(sprintf('  iter\t phase\t     num\t         sum'));
    end
    
    %
    % Begin phase one:  batch reassignments
    %
    
    converged = false;
    iter = 0;
    while true
        % Compute the distance from every point to each cluster centroid
        D(:,changed) = distfun(X, C(changed,:), distance, iter);
        
        % Compute the total sum of distances for the current configuration.
        % Can't do it first time through, there's no configuration yet.
        if iter > 0
            totsumD = sum(D((idx-1)*n + (1:n)'));
            % Test for a cycle: if objective is not decreased, back out
            % the last step and move on to the single update phase
            if prevtotsumD <= totsumD
                idx = previdx;
                [C(changed,:), m(changed)] = gcentroids(X, idx, changed, distance, Xsort, Xord);
                iter = iter - 1;
                break;
            end
            if display > 2 % 'iter'
                disp(sprintf(dispfmt,iter,1,length(moved),totsumD));
            end
            if iter >= maxit, break; end
        end

        % Determine closest cluster for each point and reassign points to clusters
        previdx = idx;
        prevtotsumD = totsumD;
        [d, nidx] = min(D, [], 2);

        if iter == 0
            % Every point moved, every cluster will need an update
            moved = 1:n;
            idx = nidx;
            changed = 1:k;
        else
            % Determine which points moved
            moved = find(nidx ~= previdx);
            if length(moved) > 0
                % Resolve ties in favor of not moving
                moved = moved(D((previdx(moved)-1)*n + moved) > d(moved));
            end
            if length(moved) == 0
                break;
            end
            idx(moved) = nidx(moved);

            % Find clusters that gained or lost members
            changed = unique([idx(moved); previdx(moved)])';
        end

        % Calculate the new cluster centroids and counts.
        [C(changed,:), m(changed)] = gcentroids(X, idx, changed, distance, Xsort, Xord);
        iter = iter + 1;
        
        % Deal with clusters that have just lost all their members
        empties = changed(m(changed) == 0);
        if ~isempty(empties)
            switch emptyact
            case 'error'
                error(sprintf('Empty cluster created at iteration %d.',iter));
            case 'drop'
                % Remove the empty cluster from any further processing
                D(:,empties) = NaN;
                changed = changed(m(changed) > 0);
                if display > 0
                    warning(sprintf('Empty cluster created at iteration %d.',iter));
                end
            case 'singleton'
                if display > 0
                    warning(sprintf('Empty cluster created at iteration %d.',iter));
                end
                
                for i = empties
                    % Find the point furthest away from its current cluster.
                    % Take that point out of its cluster and use it to create
                    % a new singleton cluster to replace the empty one.
                    [dlarge, lonely] = max(d);
                    from = idx(lonely); % taking from this cluster
                    C(i,:) = X(lonely,:);
                    m(i) = 1;
                    idx(lonely) = i;
                    d(lonely) = 0;
                    
                    % Update clusters from which points are taken
                    [C(from,:), m(from)] = gcentroids(X, idx, from, distance, Xsort, Xord);
                    changed = unique([changed from]);
                end
            end
        end
    end % phase one

    % Initialize some cluster information prior to phase two
    switch distance
    case 'cityblock'
        Xmid = zeros([k,p,2]);
        for i = 1:k
            if m(i) > 0
                % Separate out sorted coords for points in i'th cluster,
                % and save values above and below median, component-wise
                Xsorted = reshape(Xsort(idx(Xord)==i), m(i), p);
                nn = floor(.5*m(i));
                if mod(m(i),2) == 0
                    Xmid(i,:,1:2) = Xsorted([nn, nn+1],:)';
                elseif m(i) > 1
                    Xmid(i,:,1:2) = Xsorted([nn, nn+2],:)';
                else
                    Xmid(i,:,1:2) = Xsorted([1, 1],:)';
                end
            end
        end
    case 'hamming'
        Xsum = zeros(k,p);
        for i = 1:k
            if m(i) > 0
                % Sum coords for points in i'th cluster, component-wise
                Xsum(i,:) = sum(X(idx==i,:), 1);
            end
        end
    end
    
    %
    % Begin phase two:  single reassignments
    %
    changed = find(m' > 0);
    lastmoved = 0;
    nummoved = 0;
    iter1 = iter;
    while iter < maxit
        % Calculate distances to each cluster from each point, and the
        % potential change in total sum of errors for adding or removing
        % each point from each cluster.  Clusters that have not changed
        % membership need not be updated.
        %
        % Singleton clusters are a special case for the sum of dists
        % calculation.  Removing their only point is never best, so the
        % reassignment criterion had better guarantee that a singleton
        % point will stay in its own cluster.  Happily, we get
        % Del(i,idx(i)) == 0 automatically for them.
		switch distance
		case 'sqeuclidean'
            for i = changed
                mbrs = (idx == i);
                sgn = 1 - 2*mbrs; % -1 for members, 1 for nonmembers
                if m(i) == 1
                    sgn(mbrs) = 0; % prevent divide-by-zero for singleton mbrs
                end
                Del(:,i) = (m(i) ./ (m(i) + sgn)) .* sum((X - C(repmat(i,n,1),:)).^2, 2);
            end
        case 'cityblock'
            for i = changed
                if mod(m(i),2) == 0 % this will never catch singleton clusters
                    ldist = Xmid(repmat(i,n,1),:,1) - X;
                    rdist = X - Xmid(repmat(i,n,1),:,2);
                    mbrs = (idx == i);
                    sgn = repmat(1-2*mbrs, 1, p); % -1 for members, 1 for nonmembers
                    Del(:,i) = sum(max(0, max(sgn.*rdist, sgn.*ldist)), 2);
                else
                    Del(:,i) = sum(abs(X - C(repmat(i,n,1),:)), 2);
                end
            end
        case {'cosine','correlation'}
            % The points are normalized, centroids are not, so normalize them
            normC(changed) = sqrt(sum(C(changed,:).^2, 2));
            if any(normC < eps) % small relative to unit-length data points
                error(sprintf('Zero cluster centroid created at iteration %d.',iter));
            end
            % This can be done without a loop, but the loop saves memory allocations
            for i = changed
                XCi = X * C(i,:)';
                mbrs = (idx == i);
                sgn = 1 - 2*mbrs; % -1 for members, 1 for nonmembers
                Del(:,i) = 1 + sgn .*...
                      (m(i).*normC(i) - sqrt((m(i).*normC(i)).^2 + 2.*sgn.*m(i).*XCi + 1));
            end
        case 'hamming'
            for i = changed
                if mod(m(i),2) == 0 % this will never catch singleton clusters
                    % coords with an unequal number of 0s and 1s have a
                    % different contribution than coords with an equal
                    % number
                    unequal01 = find(2*Xsum(i,:) ~= m(i));
                    numequal01 = p - length(unequal01);
                    mbrs = (idx == i);
                    Di = abs(X(:,unequal01) - C(repmat(i,n,1),unequal01));
                    Del(:,i) = (sum(Di, 2) + mbrs*numequal01) / p;
                else
                    Del(:,i) = sum(abs(X - C(repmat(i,n,1),:)), 2) / p;
                end
            end
		end

        % Determine best possible move, if any, for each point.  Next we
        % will pick one from those that actually did move.
        previdx = idx;
        prevtotsumD = totsumD;
        [minDel, nidx] = min(Del, [], 2);
        moved = find(previdx ~= nidx);
        if length(moved) > 0
            % Resolve ties in favor of not moving
            moved = moved(Del((previdx(moved)-1)*n + moved) > minDel(moved));
        end
        if length(moved) == 0
            % Count an iteration if phase 2 did nothing at all, or if we're
            % in the middle of a pass through all the points
            if (iter - iter1) == 0 | nummoved > 0
                iter = iter + 1;
                if display > 2 % 'iter'
                    disp(sprintf(dispfmt,iter,2,nummoved,totsumD));
                end
            end
            converged = true;
            break;
        end
        
        % Pick the next move in cyclic order
        moved = mod(min(mod(moved - lastmoved - 1, n) + lastmoved), n) + 1;
        
        % If we've gone once through all the points, that's an iteration
        if moved <= lastmoved
            iter = iter + 1;
            if display > 2 % 'iter'
                disp(sprintf(dispfmt,iter,2,nummoved,totsumD));
            end
            if iter >= maxit, break; end
            nummoved = 0;
        end
        nummoved = nummoved + 1;
        lastmoved = moved;
        
        oidx = idx(moved);
        nidx = nidx(moved);
        totsumD = totsumD + Del(moved,nidx) - Del(moved,oidx);
        
        % Update the cluster index vector, and rhe old and new cluster
        % counts and centroids
        idx(moved) = nidx;
        m(nidx) = m(nidx) + 1;
        m(oidx) = m(oidx) - 1;
        switch distance
        case 'sqeuclidean'
            C(nidx,:) = C(nidx,:) + (X(moved,:) - C(nidx,:)) / m(nidx);
            C(oidx,:) = C(oidx,:) - (X(moved,:) - C(oidx,:)) / m(oidx);
        case 'cityblock'
            for i = [oidx nidx]
                % Separate out sorted coords for points in each cluster.
                % New centroid is the coord median, save values above and
                % below median.  All done component-wise.
                Xsorted = reshape(Xsort(idx(Xord)==i), m(i), p);
                nn = floor(.5*m(i));
                if mod(m(i),2) == 0
                    C(i,:) = .5 * (Xsorted(nn,:) + Xsorted(nn+1,:));
                    Xmid(i,:,1:2) = Xsorted([nn, nn+1],:)';
                else
                    C(i,:) = Xsorted(nn+1,:);
                    if m(i) > 1
                        Xmid(i,:,1:2) = Xsorted([nn, nn+2],:)';
                    else
                        Xmid(i,:,1:2) = Xsorted([1, 1],:)';
                    end
                end
            end
        case {'cosine','correlation'}
            C(nidx,:) = C(nidx,:) + (X(moved,:) - C(nidx,:)) / m(nidx);
            C(oidx,:) = C(oidx,:) - (X(moved,:) - C(oidx,:)) / m(oidx);
        case 'hamming'
            % Update summed coords for points in each cluster.  New
            % centroid is the coord median.  All done component-wise.
            Xsum(nidx,:) = Xsum(nidx,:) + X(moved,:);
            Xsum(oidx,:) = Xsum(oidx,:) - X(moved,:);
            C(nidx,:) = .5*sign(2*Xsum(nidx,:) - m(nidx)) + .5;
            C(oidx,:) = .5*sign(2*Xsum(oidx,:) - m(oidx)) + .5;
        end
        changed = sort([oidx nidx]);
    end % phase two
    
    if (~converged) & (display > 0)
        % Commented by pierre for this program !!!
        %warning(sprintf('Failed to converge in %d iterations.', maxit));
    end

    % Calculate cluster-wise sums of distances
    nonempties = find(m(:)'>0);
    D(:,nonempties) = distfun(X, C(nonempties,:), distance, iter);
    d = D((idx-1)*n + (1:n)');
    sumD = zeros(k,1);
    for i = 1:k
        sumD(i) = sum(d(idx == i));
    end
    if display > 1 % 'final' or 'iter'
        disp(sprintf('%d iterations, total sum of distances = %g',iter,totsumD));
    end

    % Save the best solution so far
    if totsumD < totsumDBest
        totsumDBest = totsumD;
        idxBest = idx;
        Cbest = C;
        sumDBest = sumD;
        if nargout > 3
            Dbest = D;
        end
    end
end

% Return the best solution
idx = idxBest;
C = Cbest;
sumD = sumDBest;
if nargout > 3
    D = Dbest;
end

function D = distfun(X, C, dist, iter)
%DISTFUN Calculate point to cluster centroid distances.
[n,p] = size(X);
D = zeros(n,size(C,1));
clusts = 1:size(C,1);

switch dist
case 'sqeuclidean'
    for i = clusts
        D(:,i) = sum((X - C(repmat(i,n,1),:)).^2, 2);
    end
case 'cityblock'
    for i = clusts
        D(:,i) = sum(abs(X - C(repmat(i,n,1),:)), 2);
    end
case {'cosine','correlation'}
    % The points are normalized, centroids are not, so normalize them
    normC = sqrt(sum(C.^2, 2));
    if any(normC < eps) % small relative to unit-length data points
        error(sprintf('Zero cluster centroid created at iteration %d.',iter));
    end
    % This can be done without a loop, but the loop saves memory allocations
    for i = clusts
        D(:,i) = 1 - (X * C(i,:)') ./ normC(i);
    end
case 'hamming'
    for i = clusts
        D(:,i) = sum(abs(X - C(repmat(i,n,1),:)), 2) / p;
    end
end

function [centroids, counts] = gcentroids(X, index, clusts, dist, Xsort, Xord)
%GCENTROIDS Centroids and counts stratified by group.
[n,p] = size(X);
num = length(clusts);
centroids = repmat(NaN, [num p]);
counts = zeros(num,1);
for i = 1:num
    members = find(index == clusts(i));
    if length(members) > 0
        counts(i) = length(members);
        switch dist
        case 'sqeuclidean'
            centroids(i,:) = sum(X(members,:),1) / counts(i);
        case 'cityblock'
            % Separate out sorted coords for points in i'th cluster,
            % and use to compute a fast median, component-wise
            Xsorted = reshape(Xsort(index(Xord)==clusts(i)), counts(i), p);
            nn = floor(.5*counts(i));
            if mod(counts(i),2) == 0
                centroids(i,:) = .5 * (Xsorted(nn,:) + Xsorted(nn+1,:));
            else
                centroids(i,:) = Xsorted(nn+1,:);
            end
        case {'cosine','correlation'}
            centroids(i,:) = sum(X(members,:),1) / counts(i); % unnormalized
        case 'hamming'
            % Compute a fast median for binary data, component-wise
            centroids(i,:) = .5*sign(2*sum(X(members,:), 1) - counts(i)) + .5;
        end
    end
end

function [emsg,varargout]=statgetargs(pnames,dflts,varargin)
%STATGETARGS Process parameter name/value pairs for statistics functions
%   [EMSG,A,B,...]=STATGETARGS(PNAMES,DFLTS,'NAME1',VAL1,'NAME2',VAL2,...)
%   accepts a cell array PNAMES of valid parameter names, a cell array
%   DFLTS of default values for the parameters named in PNAMES, and
%   additional parameter name/value pairs.  Returns parameter values A,B,...
%   in the same order as the names in PNAMES.  Outputs corresponding to
%   entries in PNAMES that are not specified in the name/value pairs are
%   set to the corresponding value from DFLTS.  If nargout is equal to
%   length(PNAMES)+1, then unrecognized name/value pairs are an error.  If
%   nargout is equal to length(PNAMES)+2, then all unrecognized name/value
%   pairs are returned in a single cell array following any other outputs.
%
%   EMSG is empty if the arguments are valid, or the text of an error message
%   if an error occurs.  STATGETARGS does not actually throw any errors, but
%   rather returns an error message so that the caller may throw the error.
%   Outputs will be partially processed after an error occurs.
%
%   This utility is used by some Statistics Toolbox functions to process
%   name/value pair arguments.
%
%   Example:
%       pnames = {'color' 'linestyle', 'linewidth'}
%       dflts  = {    'r'         '_'          '1'}
%       varargin = {{'linew' 2 'nonesuch' [1 2 3] 'linestyle' ':'}
%       [emsg,c,ls,lw] = statgetargs(pnames,dflts,varargin{:})    % error
%       [emsg,c,ls,lw,ur] = statgetargs(pnames,dflts,varargin{:}) % ok

%   Copyright 1993-2002 The MathWorks, Inc. 
%   $Revision: 1.4 $  $Date: 2002/02/04 19:25:45 $ 

% We always create (nparams+1) outputs:
%    one for emsg
%    nparams varargs for values corresponding to names in pnames
% If they ask for one more (nargout == nparams+2), it's for unrecognized
% names/values

% Initialize some variables
emsg = '';
nparams = length(pnames);
varargout = dflts;
unrecog = {};
nargs = length(varargin);

% Must have name/value pairs
if mod(nargs,2)~=0
    emsg = sprintf('Wrong number of arguments.');
else
    % Process name/value pairs
    for j=1:2:nargs
        pname = varargin{j};
        if ~ischar(pname)
            emsg = sprintf('Parameter name must be text.');
            break;
        end
        i = strmatch(lower(pname),pnames);
        if isempty(i)
            % if they've asked to get back unrecognized names/values, add this
            % one to the list
            if nargout > nparams+1
                unrecog((end+1):(end+2)) = {varargin{j} varargin{j+1}};
                
                % otherwise, it's an error
            else
                emsg = sprintf('Invalid parameter name:  %s.',pname);
                break;
            end
        elseif length(i)>1
            emsg = sprintf('Ambiguous parameter name:  %s.',pname);
            break;
        else
            varargout{i} = varargin{j+1};
        end
    end
end

varargout{nparams+1} = unrecog;



% #########################################################################
%    DEBUG Mode (1.1.1)
function ButtonDebug_Callback(hObject, eventdata, handles)

RunSWOT(0,handles);

disp(' ')
disp('- XThermoTools - ')
disp('Debug mode in the command windows')
disp('All variables are saved into the global variable handles')
disp(' ')

keyboard





% -------------------------------------------------------------------------
%                           F I G U R E  (2)
% -------------------------------------------------------------------------


% #########################################################################
%       AXES 2 >> TYPE MENU (1.1.1)
function PopUpAxes2Type_Callback(hObject, eventdata, handles)

switch get(handles.PopUpAxes2Type,'Value')
    
    case 1 % Scatter (binary)
        set(handles.TextPlot2V1,'Visible','on','enable','on');
        set(handles.PopUpV1Type1,'Visible','on','enable','on','Value',2);
        set(handles.PopUpV1Type2,'Visible','on','enable','off');
        set(handles.PopUpV1Type3,'Visible','on','enable','off');
        
        set(handles.TextPlot2V2,'Visible','on','enable','on');
        set(handles.PopUpV2Type1,'Visible','on','enable','on','Value',1);
        set(handles.PopUpV2Type2,'Visible','on','enable','off');
        set(handles.PopUpV2Type3,'Visible','on','enable','off');
        
    case 2 % Histogram
        set(handles.TextPlot2V1,'Visible','on','enable','on');
        set(handles.PopUpV1Type1,'Visible','on','enable','on','Value',2);
        set(handles.PopUpV1Type2,'Visible','on','enable','off');
        set(handles.PopUpV1Type3,'Visible','off');
        
        set(handles.TextPlot2V2,'Visible','off');
        set(handles.PopUpV2Type1,'Visible','off');
        set(handles.PopUpV2Type2,'Visible','off');
        set(handles.PopUpV2Type3,'Visible','off');
        
    case 3 % Map
        set(handles.TextPlot2V1,'Visible','on','enable','on');
        set(handles.PopUpV1Type1,'Visible','on','enable','on','Value',2);
        set(handles.PopUpV1Type2,'Visible','off');
        set(handles.PopUpV1Type3,'Visible','off');
        
        set(handles.TextPlot2V2,'Visible','off');
        set(handles.PopUpV2Type1,'Visible','off');
        set(handles.PopUpV2Type2,'Visible','off');
        set(handles.PopUpV2Type3,'Visible','off');
        
    case 4  % EquiPlot
        set(handles.TextPlot2V1,'Visible','on');
        set(handles.PopUpV1Type1,'Visible','on','enable','off','Value',2);
        set(handles.PopUpV1Type2,'Visible','off');
        set(handles.PopUpV1Type3,'Visible','off');
        
        set(handles.TextPlot2V2,'Visible','on');
        set(handles.PopUpV2Type1,'Visible','on','enable','off','Value',1);
        set(handles.PopUpV2Type2,'Visible','off');
        set(handles.PopUpV2Type3,'Visible','off');
        
    case 5 % None
        set(handles.TextPlot2V1,'Visible','off');
        set(handles.PopUpV1Type1,'Visible','off');
        set(handles.PopUpV1Type2,'Visible','off');
        set(handles.PopUpV1Type3,'Visible','off');
        
        set(handles.TextPlot2V2,'Visible','off');
        set(handles.PopUpV2Type1,'Visible','off');
        set(handles.PopUpV2Type2,'Visible','off');
        set(handles.PopUpV2Type3,'Visible','off');

end









% -------------------------------------------------------------------------
%
%                      ##### ##### #   # ##### #####  
%                      #   #   #   #   # #     #   #   
%                      #####   #   ##  # #  ## #   #     
%                      #   #   #   # # # #   # #   #    
%                      #####  #### #  ## ##### #####
%
% -------------------------------------------------------------------------

% goto onestou

% #########################################################################
%       BIN - Were Are We? (1.2.1)
function BinWereWeAre(hObject, eventdata, handles)
%

% PL March 2019:
%
% buttons replaced by menus (and not deleted yet because the
% callbackfunctions are still in there... To be fixed one day if we get
% read of the buttons (not sure, icons can be good as well).

set(handles.BinButtonDeleteShape,'Visible','off');
set(handles.BinButtonAddShape,'Visible','off');
set(handles.BinButtonLoadShape,'Visible','off');
set(handles.BinButtonSaveShape,'Visible','off');
set(handles.BinButtonFluidGases,'Visible','off');   
set(handles.BinButtonAddGroupOfPhase,'Visible','off');
set(handles.BinButtonImportGrp,'Visible','off');
set(handles.BinButtonSaveGrp,'Visible','off');
set(handles.BinButtonSyncPhase,'Visible','off');
set(handles.BinButtonSyncManualPhase,'Visible','off');
set(handles.BinButtonCalcPhase,'Visible','off');

% PART 1 : CHEMICAL SYSTEM
Part1 = 0;

if get(handles.BinPopUpProgram,'Value') > 1
    set(handles.BinPopUpDatabase,'Enable','on');
    %set(handles.BinTextChemicalSyst,'Enable','inactive');
    
    if length(get(handles.BinTextChemicalSyst,'String'));
        Part1 = 1;
        set(handles.BinCheckBoxPart1,'Value',1);
    else
        set(handles.BinCheckBoxPart1,'Value',0);
    end
    
else
    set(handles.BinPopUpDatabase,'Enable','on');
    %set(handles.BinTextChemicalSyst,'Enable','off');
end


% PART 2 : BULK COMPOSITION
%    -> Update the list available in BinPopUpBulkCompoList
Part2 = 0;
if Part1
    
    set(handles.Menu_Bulk,'Enable','on');
    
    BinBulk = handles.BinBulk;

    %    -> Update display options
    SelectedCompo = get(handles.BinPopUpBulkCompoList,'Value');
%     if isequal(BinBulk(SelectedCompo).Type,0)
%         % Bulk rock composition
%         %set(handles.BinCheckBoxPart2,'String','Input composition (BULK ROCK COMPOSITION)');
%         set(handles.BinCheckBoxPart2,'Value',1); 
% 
%     else
%         % Local bulk composition
%         %set(handles.BinCheckBoxPart2,'String','Input composition (LOCAL BULK COMPOSITION)');
%         set(handles.BinCheckBoxPart2,'Value',1);
% 
%     end
    
    Items = get(handles.BinPopUpBulkCompoList,'String');
    if length(Items) > 1 && SelectedCompo > 1
        %set(handles.BinButtonDeleteShape,'Enable','on');
        set(handles.Menu_BulkDelete,'Enable','on');
    else
        %set(handles.BinButtonDeleteShape,'Enable','off');
        set(handles.Menu_BulkDelete,'Enable','off');
    end
    
    % Update handles structure
    guidata(hObject, handles);
    
    % UPDATE DISPLAY
    set(handles.BinCheckBoxPart2,'Value',1);
    
    set(handles.BinTextBulkCompo,'Enable','on');
    set(handles.BinPopUpBulkCompoList,'Enable','on');
    %set(handles.BinButtonAddShape,'Enable','on');
    %set(handles.BinButtonLoadShape,'Enable','on');
    %set(handles.BinButtonSaveShape,'Enable','on');
    
    set(handles.Menu_BulkAdd,'Enable','on');
    set(handles.Menu_BulkLoadConfig,'Enable','on');
    set(handles.Menu_BulkSaveConfig,'Enable','on');
    
    set(handles.BinPopUpBulkPhases4BulkList,'Enable','on');
    set(handles.BinButtonPhasesSelected4Bulk,'Enable','on');

    if iscell(get(handles.BinPopUpBulkCompoList,'String')) && length(get(handles.BinPopUpBulkCompoList,'String')) > 1 && SelectedCompo > 1
        Part2 = 1; 
    end
    
else
    set(handles.Menu_Bulk,'Enable','off');
    
    set(handles.BinCheckBoxPart2,'Value',0);
    
    set(handles.BinTextBulkCompo,'Enable','off');
    set(handles.BinPopUpBulkCompoList,'Enable','off');
    %set(handles.BinButtonAddShape,'Enable','off');
    %set(handles.BinButtonDeleteShape,'Enable','off');
    %set(handles.BinButtonLoadShape,'Enable','off');
    %set(handles.BinButtonSaveShape,'Enable','off');
    
    set(handles.Menu_BulkAdd,'Enable','off');
    set(handles.Menu_BulkLoadConfig,'Enable','off');
    set(handles.Menu_BulkSaveConfig,'Enable','off');
    set(handles.Menu_BulkDelete,'Enable','off');
    
    set(handles.BinPopUpBulkPhases4BulkList,'Enable','off');
    set(handles.BinButtonPhasesSelected4Bulk,'Enable','off');
end
    

% PART 2b: Fluids
Part2b = 0;
if Part2
    set(handles.Menu_PhasesFMB,'Enable','on');
    set(handles.Menu_OtherElements,'Enable','on');
    %set(handles.BinButtonFluidGases,'Enable','on');
    if isfield(handles,'BinGfDef')
        Part2b = 1;
    end
else
    set(handles.Menu_PhasesFMB,'Enable','off');
    set(handles.Menu_OtherElements,'Enable','off');
    %set(handles.BinButtonFluidGases,'Enable','off');
end

% PART 3: PHASES
Part3 = 0;
if Part2  % Actually we don't need part 2b for BINGO as bulk is saved)
    
    set(handles.Menu_Phases,'Enable','on');
    
    set(handles.BinPopUpPhaseList,'Enable','on');
    set(handles.BinCheckBoxIsPhaseIn,'Enable','on');
    set(handles.BinButtonSyncPhase,'Enable','on');
    set(handles.BinTextPhaseTheriakName,'Enable','on');

    %set(handles.BinButtonLoadGrp,'Enable','on');
    %set(handles.BinButtonImportGrp,'Enable','on');
     
    SelectedPhase = get(handles.BinPopUpPhaseList,'Value');
    BinPhaseDef = handles.BinPhaseDef;

    if BinPhaseDef(SelectedPhase).IsPhaseIn
        set(handles.BinCheckBoxIsPhaseIn,'Enable','on');
        
        set(handles.Menu_PhasesAddGrp,'Enable','on');
        
        %set(handles.BinButtonAddGroupOfPhase,'Enable','on');
        set(handles.BinButtonSyncManualPhase,'Enable','on');
        
        FieldBinPopUpGrpOfPhase = get(handles.BinPopUpGrpOfPhase,'String');

        if ~length(FieldBinPopUpGrpOfPhase{1})
            set(handles.BinPopUpGrpOfPhase,'Enable','off');
            
            set(handles.Menu_PhasesCALC,'Enable','off');
            
            set(handles.BinButtonCalcPhase,'Enable','off');
            set(handles.BinTextPhaseTheriakVolProp,'Enable','off');
        else
            set(handles.BinPopUpGrpOfPhase,'Enable','on');
            
            set(handles.Menu_PhasesCALC,'Enable','on');
            
            set(handles.BinButtonCalcPhase,'Enable','on');
            set(handles.BinTextPhaseTheriakVolProp,'Enable','on');
        end
        
    else
        set(handles.BinCheckBoxIsPhaseIn,'Enable','off');
        
        set(handles.Menu_PhasesAddGrp,'Enable','off');
        set(handles.Menu_PhasesDeleteGrp,'Enable','off');
        set(handles.Menu_PhasesCALC,'Enable','off');
        
        set(handles.BinPopUpGrpOfPhase,'Enable','off');
        %set(handles.BinButtonAddGroupOfPhase,'Enable','off');
        set(handles.BinButtonSyncManualPhase,'Enable','off');
        
        set(handles.BinButtonCalcPhase,'Enable','off');
        set(handles.BinTextPhaseTheriakVolProp,'Enable','off');
    end

    Ok = [];
    Compt = 0;
    for i=1:length(BinPhaseDef)
        if BinPhaseDef(i).IsPhaseIn
        	Compt = Compt+1;
            
            % I do not remember what the hell I thought when I created this
            % It seems to be quite stupid, but it works out well.
            
            FieldBinPopUpGrpOfPhase = get(handles.BinPopUpGrpOfPhase,'String');

            if length(FieldBinPopUpGrpOfPhase{1}) && length(BinPhaseDef(i).PhaseVolProp)
                Ok(Compt) = 1;
            else
                Ok(Compt) = 0;
            end
        end
    end
    
    if length(Ok) && isequal(length(Ok),sum(Ok)) 
        Part3 = 1;
        set(handles.BinCheckBoxPart3,'Value',1);
    else
        set(handles.BinCheckBoxPart3,'Value',0);
    end
else
    set(handles.Menu_Phases,'Enable','off');
    
    set(handles.BinPopUpPhaseList,'Enable','off');
    set(handles.BinCheckBoxIsPhaseIn,'Enable','off');
    set(handles.BinButtonSyncPhase,'Enable','off');
    set(handles.BinTextPhaseTheriakName,'Enable','off');
    set(handles.BinPopUpGrpOfPhase,'Enable','off');
    %set(handles.BinButtonAddGroupOfPhase,'Enable','off');
    set(handles.BinButtonSyncManualPhase,'Enable','off');
    set(handles.BinButtonCalcPhase,'Enable','off');
    set(handles.BinTextPhaseTheriakVolProp,'Enable','off');
    
    %set(handles.BinButtonLoadGrp,'Enable','off')
    set(handles. BinButtonImportGrp,'Enable','off');
    
    set(handles.Menu_PhasesAddGrp,'Enable','off');
    set(handles.Menu_PhasesDeleteGrp,'Enable','off');
end

% SPECIAL: Update the menu if a solid is displayed
if isequal(get(handles.BinPopUpPhaseList,'Enable'),'on')
    BinPhaseDef = handles.BinPhaseDef;
    ListPhase = get(handles.BinPopUpPhaseList,'String');
    SelectedPhase = get(handles.BinPopUpPhaseList,'Value');
    if ~BinPhaseDef(SelectedPhase).IsPhaseIn
        set(handles.Menu_PhasesSyncSolid,'Label',['Synchronize ',char(ListPhase{SelectedPhase})],'Enable','On');
        set(handles.Menu_PhasesDesync,'Enable','off');
        set(handles.Menu_PhasesManualSync,'Enable','off');
    else
        set(handles.Menu_PhasesSyncSolid,'Label',['Resynchronize ',char(ListPhase{SelectedPhase})],'Enable','On');
        set(handles.Menu_PhasesDesync,'Enable','on');
        set(handles.Menu_PhasesManualSync,'Enable','on');
    end
else
    set(handles.Menu_PhasesSyncSolid,'Label','Synchronize Solid','Enable','Off');
    set(handles.Menu_PhasesDesync,'Enable','off');
    set(handles.Menu_PhasesManualSync,'Enable','off');
end


% PART 4: BINGO-ANTIDOTE
Part4 = 0;

if Part3
    
    
    set(handles.BinCheckBoxPart4,'Value',1);
    set(handles.BinTextBinTC,'Enable','on');
    set(handles.BinTextBinP,'Enable','on');
    set(handles.BinButtonRunBingo,'Enable','on');
    
    if isfield(handles,'BinGfDef')   % Otherwise some recipes will not work
        set(handles.BinCheckBoxPart5,'Value',1);
        set(handles.BinPopUpListeAntidotes,'Enable','on');
        set(handles.BinButtonRunAntidote,'Enable','on');
        set(handles.BinTextBinTCminmax,'Enable','on');
        set(handles.BinTextBinPminmax,'Enable','on');
    end
    
    %set(handles.BinButtonSaveGrp,'Enable','on');
    
else
    
    
    set(handles.BinCheckBoxPart4,'Value',0);
    set(handles.BinTextBinTC,'Enable','off');
    set(handles.BinTextBinP,'Enable','off');
    set(handles.BinButtonRunBingo,'Enable','off');
    
    set(handles.BinTextBinRes1,'String','...');
    set(handles.BinTextBinRes2,'String','...');
    set(handles.BinTextBinRes3,'String','...');
    set(handles.BinTextBinRes4,'String','...');
    
    set(handles.BinCheckBoxPart5,'Value',0);
    set(handles.BinPopUpListeAntidotes,'Enable','off');
    set(handles.BinButtonRunAntidote,'Enable','off');
    set(handles.BinTextBinTCminmax,'Enable','off');
    set(handles.BinTextBinPminmax,'Enable','off');
    
    %set(handles.BinButtonSaveGrp,'Enable','off');
    
    set(handles.BinTextAnt1,'Enable','off');
    set(handles.BinTextAnt2,'Enable','off');
    set(handles.BinTextAnt3,'Enable','off');
    set(handles.BinTextAnt4,'Enable','off');
    
    set(handles.BinPopUpVarPhaseList,'Enable','off');
end

return



% #########################################################################
%       BIN - Initialization of BINGO (1.2.2)
function [BingoDefault] = BingoInitialization(hObject, eventdata, handles)
%

BingoDefault.SelectedProgram = 0;
BingoDefault.SelectedDatabase = 0;

BingoDefault.Theriak.Path = '';                          % Not defined here
BingoDefault.Theriak.Database(1).Label = '';
BingoDefault.Theriak.Database(1).File1 = '';
BingoDefault.Theriak.Database(1).Version = '';
BingoDefault.Theriak.Database(1).Comment = '';
BingoDefault.Theriak.Database(1).MinTrans = '';
BingoDefault.Theriak.Database(1).PathForDB = '';
BingoDefault.Theriak.InputBulk = [];

BingoDefault.PerpleX.Path = '';
BingoDefault.PerpleX.Database(1).Label = '';
BingoDefault.PerpleX.Database(1).File1 = '';
BingoDefault.PerpleX.Database(1).File2 = '';
BingoDefault.PerpleX.Database(1).MinTrans = '';
BingoDefault.PerpleX.InputBulk = [];


LocBase = handles.LocBase;
ThePathForDB = [LocBase,'/XTT_Databases/'];
fid = fopen([ThePathForDB,'Database_Definitions.txt']);

Compt = 0;
while 1
    TheLine = fgetl(fid);
    
    if isequal(TheLine,-1)
        break
    end
    
    if isequal(TheLine,'###')
        
        Compt = Compt+1;
        
        % FileName:
        TheStr = strread(fgetl(fid),'%s');
        BingoDefault.Theriak.Database(Compt).Label = TheStr{2};
        BingoDefault.Theriak.Database(Compt).File1 = TheStr{2};
        
        % DefinitionFile:
        TheStr = strread(fgetl(fid),'%s');
        BingoDefault.Theriak.Database(Compt).MinTrans = TheStr{2};
        
        % Version:
        TheStr = strread(fgetl(fid),'%s');
        BingoDefault.Theriak.Database(Compt).Version = TheStr{2};
        
        % Comment:
        TheLine = fgetl(fid);
        BingoDefault.Theriak.Database(Compt).Comment = TheLine(11:end);

        % Path for DB
        BingoDefault.Theriak.Database(Compt).PathForDB = ThePathForDB;
    end
end

fclose(fid);


% fid = fopen('XTT_ConnectionConfig.txt');
% while 1
%     LaLign = fgetl(fid);
%     if isequal(LaLign,-1)
%         break
%     end
%     if length(LaLign)
%         if isequal(LaLign(1),'>')
%             TheRef = str2double(LaLign(2:4));
% 
%             switch TheRef
%                 
%                 case 1      % Default Thermodynamic dataset
%                     TheL = fgetl(fid);
%                     Compt=0;
%                     while length(TheL)
%                         Compt = Compt+1;
%                         %keyboard
%                         TheLStr = strread(TheL,'%s');
%                         BingoDefault.Theriak.Database(Compt).Label = TheLStr{1};
%                         BingoDefault.Theriak.Database(Compt).File1 = TheLStr{2};
%                         BingoDefault.Theriak.Database(Compt).File2 = TheLStr{3};
%                         BingoDefault.Theriak.Database(Compt).MinTrans = TheLStr{4};
%                         
%                         TheL = fgetl(fid);
%                     end  
%                     
%                 case 2      % Default Input Compositions
%                    BingoDefault.Theriak.InputBulk = fgetl(fid);
%                    
%             end
%         end
%     end
% end
% fclose(fid);


% READ XTT_Min_ files

for i=1:length(BingoDefault.Theriak.Database)
    
    [DefMin,DefGf] = ReadMinTrans(BingoDefault.Theriak.Database(i).MinTrans,BingoDefault.Theriak.Database(i).PathForDB); 
    
    BingoDefault.Theriak.Database(i).DefMin = DefMin;
    BingoDefault.Theriak.Database(i).DefGf = DefGf;
end

return


function [DefMin,DefGf] = ReadMinTrans(MinTransFile,Path4DB)
% 
DefMin = [];
DefGf = [];

fid = fopen([Path4DB,MinTransFile]); Compt = 0;
while 1
    lalign = fgetl(fid);
    if isequal(lalign,-1)
        break
    end
    
    if length(lalign) > 1
        if isequal(lalign(1:2),'>1')
            Compt = 0;
            while 1
                lalign = fgetl(fid);
                if isequal(lalign,'')
                    break
                end
                Compt = Compt+1;
                Temp = strread(lalign,'%s');
                DefMin{Compt,1} = Temp{1};
                DefMin{Compt,2} = str2num(Temp{2});
                DefMin{Compt,3} = Temp{3};
                DefMin{Compt,4} = strread(Temp{4}(2:end-1),'%s','delimiter',',')';
                %DefMin{Compt,5} = str2double(strread(Temp{5}(2:end-1),'%s','delimiter',','))';
            end
        end
        
    end
    
    if length(lalign) > 1
        if isequal(lalign(1:2),'>2')
            Compt = 0;
            while 1
                lalign = fgetl(fid);
                if isequal(lalign,'')
                    break
                end
                Compt = Compt+1;
                Temp = strread(lalign,'%s');
                DefGf{Compt,1} = Temp{1};
                DefGf{Compt,2} = str2num(Temp{2});
                DefGf{Compt,3} = Temp{3};
                DefGf{Compt,4} = strread(Temp{4}(2:end-1),'%s','delimiter',',')';
                %DefGf{Compt,5} = str2double(strread(Temp{5}(2:end-1),'%s','delimiter',','))';
            end
        end
    end
end

return


% #########################################################################
%       BIN - Activate SWOT-MOBIL (1.2.1)
function [] = RunSWOT(State,handles)

if State == 1
    iconsFolder = fullfile(handles.LocBase,'/XTT_Dev/img/');
    iconUrl = strrep([handles.FileDirCode, iconsFolder 'SWOT_32.gif'],'\','/');

    str = ['<html><img src="' iconUrl '"/></html>'];
    
    set(handles.SWOT,'string',str,'visible','on');
    
else
    set(handles.SWOT,'string','','visible','off');
end
drawnow
return


% #########################################################################
%       BIN - Setting menu for PROGRAM (1.2.2)
function handles = BinPopUpProgram_Callback(hObject, eventdata, handles)
% 

InitPanelDisp(1,handles);

Selected = get(handles.BinPopUpProgram,'Value');

switch Selected
    case 2
        % Theriak
        for i = 1:length(handles.BingoDefault.Theriak.Database)
            ListName{i} = handles.BingoDefault.Theriak.Database(i).File1;
        end
        
        handles.BingoDefault.SelectedProgram = 1;
        handles.BingoDefault.SelectedDatabase = 1;
        
        set(handles.BinPopUpDatabase,'String',ListName,'Value',1);
        
    case 3
        
        h = helpdlg({'Sorry but Perple_X is not yet compatible!'},'Help Dialog');

        set(handles.BinPopUpProgram,'Value',1);
               
%         set(handles.BinPopUpDatabase,'String','');
%         
%         handles.BingoDefault.SelectedProgram = 2;
%         handles.BingoDefault.SelectedDatabase = 0;
        
    case 4
        
        h = helpdlg({' ','You must be joking!',' ',...
                     'It is not reasonable to use Thermocalc here because it would take forever.', ...
                     'Save your time and give Theriak-Domino a try.',' '},'Help Dialog');

        set(handles.BinPopUpProgram,'Value',1);
        
end

guidata(hObject, handles);
BinWereWeAre(hObject, eventdata, handles);
return


% #########################################################################
%       BIN - Setting menu for DATABASE (1.2.2)
function handles = BinPopUpDatabase_Callback(hObject, eventdata, handles)
%
InitPanelDisp(1,handles);

handles.BingoDefault.SelectedDatabase = get(handles.BinPopUpDatabase,'Value');
guidata(hObject, handles);

if get(handles.BinCheckBoxPart3,'Value')
    
    % We don't ask anymore and we check everything...
    
    BinPhaseDef = handles.BinPhaseDef;
    [BinPhaseDef] = UpdateBinPhaseDef(BinPhaseDef,handles);
    handles.BinPhaseDef = BinPhaseDef;
    
%     DoWeCalc = questdlg('Would you like to reload the phase definitions?','Warning database changed','Yes');
% 
%     switch DoWeCalc
%         case 'Yes'
%             BinButtonLoadGrp_Callback(hObject, eventdata, handles);
%     end

    guidata(hObject, handles);
end

% Check if the database is available...
WorkingDir = cd;

Databases = get(handles.BinPopUpDatabase,'String');
SelectedDatabase = get(handles.BinPopUpDatabase,'Value');
NameDatabase = Databases{SelectedDatabase};

if ~isequal(exist([WorkingDir,'/',NameDatabase]),2)
    DoWeCalc = questdlg({['The database ',NameDatabase,' does not exist in the working directory'],'Would you like BA to copy this database?'},'Warning database is missing','Yes');
    switch DoWeCalc
        case 'Yes'
            CopyDB2WorkingDirectory_Callback(hObject, eventdata, handles);
            
    end
    
end
return


% #########################################################################
%       BIN - Set the compositional space (1.2.1)
function BinButtonChemicalSyst1_Callback(hObject, eventdata, handles)
% 

InitPanelDisp(1,handles);

MapWindow = handles.MapWindow;
SF = handles.SF;

TheElementMaps = MapWindow.Maps.ListMaps;

for i=1:length(TheElementMaps)
    
    [IsElemInSF,WhereOxideInSF] = ismember(TheElementMaps{i},SF.ListOxide);

    if ~IsElemInSF
        warndlg(['The Element ',char(TheElementMaps{i}),' is not available in XThermoTools ...'],'Warning');
        return
    end
    
    BinElData.listElem{i} = SF.DefOxide(WhereOxideInSF).Elem;
    BinElData.listOxides{i} = TheElementMaps{i};
    BinElData.selected(i) = 1;
    BinElData.MolarMass(i) = SF.DefOxide(WhereOxideInSF).MolarMass;
    BinElData.NbAtoms(i) = SF.DefOxide(WhereOxideInSF).NbAtoms;
    BinElData.NbOxygen(i) = SF.DefOxide(WhereOxideInSF).NbOxygen;
end    

if isfield(handles,'BinElData')
    BinElData.selected = handles.BinElData.selected;
end

[Selected] = XTTselectElem(BinElData);

if isequal(Selected,0)
    guidata(hObject, handles);
    InvWereWeAre(hObject, eventdata, handles)
    return
end

BinElData.selected = Selected;
handles.BinElData = BinElData;

ListString = '';
for i=1:length(BinElData.selected)
    if BinElData.selected(i)
        ListString = [ListString,'-',char(BinElData.listElem{i})];
    end
end
ListString = ListString(2:end);

set(handles.BinTextChemicalSyst,'String',ListString);

guidata(hObject, handles);
BinWereWeAre(hObject, eventdata, handles)
return


% #########################################################################
%       BIN - Function to selected / unselect phases (1.3.4)
function BinButtonPhasesSelected4Bulk_Callback(hObject, eventdata, handles)
% 
SelectedOri = handles.MapWindow.Mask.Selected4Bulk;

[Selected] = XTTselectMinerals20(handles.MapWindow.Mask.ListMin,handles.MapWindow.Mask.Selected4Bulk,handles.MapWindow.Mask.AvailableFromXMap,'Select phases for BULK');

handles.MapWindow.Mask.Selected4Bulk = Selected;

guidata(hObject, handles);
UpdateSelectedPhases4Bulk(hObject, eventdata, handles);

for i = 1:length(handles.MapWindow.Mask.Selected4Bulk)
    if isequal(handles.MapWindow.Mask.Selected4Bulk(i),1)
        handles.MapWindow.Mask.Selected(i) = 1;
    else
        handles.MapWindow.Mask.Selected(i) = 0;
    end
end

set(handles.DispPhases4BulkList,'String',[num2str(sum(handles.MapWindow.Mask.Selected4Bulk)),' phase(s) selected (',num2str(sum(handles.MapWindow.Mask.AvailableFromXMap)),'/',num2str(length(handles.MapWindow.Mask.Selected4Bulk)),' are available)']);

guidata(hObject, handles);
DisplayMapWindow(hObject, eventdata, handles);

if ~isequal(SelectedOri,Selected)
    BinBulk = handles.BinBulk;

    for i=2:length(BinBulk)
        % Calculate the bulk and update the corresponding fields
        [BinBulk(i)] = BinUpdateBulkComposition(BinBulk(i),i,handles);
    end

    handles.BinBulk = BinBulk;
    guidata(hObject, handles);

    BinPopUpBulkCompoList_Callback(hObject, eventdata, handles);
end


return


% #########################################################################
%       BIN - Function to update the menu of selected phases (1.3.4)
function UpdateSelectedPhases4Bulk(hObject, eventdata, handles)

ListMin = handles.MapWindow.Mask.ListMin;

for i=1:length(ListMin)
    
    MinName = char(char(handles.MapWindow.Mask.ListMin{i}));
    MaxName = 20;
    
    SpacesToPrint = ' ';
    if length(MinName) < MaxName;
        for j = length(MinName):MaxName 
            SpacesToPrint = [SpacesToPrint,' '];
        end
    end 
   
    if isequal(handles.MapWindow.Mask.Selected4Bulk(i),1)
        handles.MapWindow.Mask.DispListMinSelected4Bulk{i} = char(char(handles.MapWindow.Mask.ListMin{i}));
    else
        if isequal(handles.MapWindow.Mask.AvailableFromXMap(i),0)
            handles.MapWindow.Mask.DispListMinSelected4Bulk{i} = [char(MinName),SpacesToPrint,'## Not available ## '];
        else
            handles.MapWindow.Mask.DispListMinSelected4Bulk{i} = [char(MinName),SpacesToPrint,'## REJECTED ## '];
        end
    end
    %disp(handles.MapWindow.Mask.DispListMinSelected4Bulk{i});
end


set(handles.BinPopUpBulkPhases4BulkList,'String',handles.MapWindow.Mask.DispListMinSelected4Bulk);
drawnow

guidata(hObject, handles);
return







% BULK COMPOSITION

% #########################################################################
%       BIN - Text for bulk composition display (1.2.1)
function BinTextBulkCompo_Callback(hObject, eventdata, handles)
%

BinBulk = handles.BinBulk;
SelectedCompo = get(handles.BinPopUpBulkCompoList,'Value');

CompoNew = get(hObject,'String');
CompoOriginal = BinBulk(SelectedCompo).CompositionOriginal;

if length(CompoNew) > 2
    % Do we switch to modified composition?
    if isequal(CompoOriginal,CompoNew)
        BinBulk(SelectedCompo).CompoDisp = 1;
    else
        BinBulk(SelectedCompo).CompoDisp = 2;
        BinBulk(SelectedCompo).CompositionModified = CompoNew;
    end
    
else
    % reset
    BinBulk(SelectedCompo).CompoDisp = 1;
end


handles.BinBulk = BinBulk;
guidata(hObject, handles);

BinPopUpBulkCompoList_Callback(hObject, eventdata, handles)


%BinWereWeAre(hObject, eventdata, handles)
return


% #########################################################################
%       BIN - Menu for Bulk Compositions (1.2.1)
function BinPopUpBulkCompoList_Callback(hObject, eventdata, handles)
%

InitPanelDisp(1,handles);

BinBulk = handles.BinBulk;
SelectedCompo = get(handles.BinPopUpBulkCompoList,'Value');

ButtonClean_Callback(hObject, eventdata, handles);
% If there is the shape: Plot the shape
if isequal(BinBulk(SelectedCompo).Type,1)
    PlotDomainShape(BinBulk(SelectedCompo),handles);
end

% Update the composition display

switch handles.BinBulk(SelectedCompo).CompoDisp 
    case 1         % original
        
        set(handles.BinTextBulkCompo,'String',handles.BinBulk(SelectedCompo).CompositionOriginal);
        set(handles.BinTextBulkCompo,'ForegroundColor',[0,0,0]);
        
    case 2         % modified
        
        set(handles.BinTextBulkCompo,'String',handles.BinBulk(SelectedCompo).CompositionModified);
        set(handles.BinTextBulkCompo,'ForegroundColor',[1,0,0]);
        
    case 3         % iterative bulk
        
        set(handles.BinTextBulkCompo,'String',handles.BinBulk(SelectedCompo).CompositionIterative);
        set(handles.BinTextBulkCompo,'ForegroundColor',[0,0,1]);
        
end

% I have tried to comment that: If we change the bulk we don't loose the
% phase proportions... and can directly call bingo again
% PL 11.03.2019

% if isequal(get(handles.BinCheckBoxPart4,'Value'),1)
%     % clean the PhaseVolProp
%     BinPhaseDef = handles.BinPhaseDef;
%     for i=1:length(BinPhaseDef)
%         BinPhaseDef(i).PhaseVolProp = [];
%         % Here we do not clean the volume of the groups...
%     end
%     handles.BinPhaseDef = BinPhaseDef;
%     guidata(hObject, handles);
%     set(handles.BinTextPhaseTheriakVolProp,'String','');
% end

% PL 23.05.19
% I commented this one that deactivates the zoom when we have to select a
% group and we are already zoomed in. 

%ActivateZoomFigure1(hObject, eventdata, handles);

BinWereWeAre(hObject, eventdata, handles);
return


% #########################################################################
%       BIN - Button to Plot domain Shape (1.2.1)
function PlotDomainShape(SelectedBinBulk,handles)
%
axes(handles.axes1), hold on

Xref = SelectedBinBulk.DomainXrefYref(:,1);
Yref = SelectedBinBulk.DomainXrefYref(:,2);

[XFig,YFig] = CoordinatesFromRef(Xref,Yref,handles);

for i=1:length(XFig)-1
    plot([round(XFig(i)),round(XFig(i+1))],[round(YFig(i)),round(YFig(i+1))],'-ok','MarkerFaceColor','w','MarkerEdgeColor','k')
end
plot([round(XFig(end)),round(XFig(1))],[round(YFig(end)),round(YFig(1))],'-ok','MarkerFaceColor','w','MarkerEdgeColor','k')
 
drawnow
return


% #########################################################################
%       BIN - Button to DELETE a BULK (1.5)
function BinButtonDeleteShape_Callback(hObject, eventdata, handles)

Selected =get(handles.BinPopUpBulkCompoList,'Value');

if Selected > 1 % We should not delete the bulk (used probably as reference later on)
    BinBulk = handles.BinBulk;

    Compt = 0;
    for i = 1:length(BinBulk)
        if ~isequal(i,Selected)
            Compt = Compt+1;
            BinBulkNew(Compt) = BinBulk(i);
        end
    end
    
    handles.BinBulk = BinBulkNew;
    guidata(hObject, handles);
    
    BinBulk = handles.BinBulk;
    
    %update
    for i=1:length(BinBulk)
        ListNamesBulk{i} = BinBulk(i).Name;
    end
    set(handles.BinPopUpBulkCompoList,'String',ListNamesBulk,'Value',Selected-1);  % Selected-1 is always defined as Selected > 2
    
    set(gcf, 'WindowButtonMotionFcn', @mouseMove);
    
    ActivateZoomFigure1(hObject, eventdata, handles);
    
    BinPopUpBulkCompoList_Callback(hObject, eventdata, handles)
    
    drawnow
    
end
return


% #########################################################################
%       BIN - Button to ADD a new domain and calculate the bulk composition (1.2.1)
function BinButtonAddShape_Callback(hObject, eventdata, handles)
%

InitPanelDisp(1,handles);

ButtonClean_Callback(hObject, eventdata, handles);

% Select a new domain
axes(handles.axes1), hold on
zoom off

XLimsMap = get(handles.axes1,'Xlim');
YLimsMap = get(handles.axes1,'Ylim');
XTolMap = 0*(XLimsMap(2) - XLimsMap(1));
YTolMap = 0*(YLimsMap(2) - YLimsMap(1));

Click=0;
ComptResult=0;
while Click <= 2
	[X,Y,Click] = XTTginputXTT(1,handles);
    if Click < 2
        if X < XLimsMap(1)-XTolMap || X > XLimsMap(2)+XTolMap || Y < YLimsMap(1)-XTolMap || Y > YLimsMap(2)+XTolMap
            warndlg({'Aborded...','Do not click outside the map while selecting an area!'},'Bingo-Antidote')
            return
        end
    end
    [XFig,YFig] = CoordinatesFromRef(X,Y,handles);
    %keyboard
    if Click < 2
    	ComptResult = ComptResult+1;
        h(ComptResult,1) = X;
        h(ComptResult,2) = Y;
        hPlot(ComptResult,1) = XFig;             % for display (Fig coordinate system)
        hPlot(ComptResult,2) = YFig;
        
        if ComptResult >= 2
            plot([round(hPlot(ComptResult-1,1)),round(hPlot(ComptResult,1))],[round(hPlot(ComptResult-1,2)),round(hPlot(ComptResult,2))],'-ok','MarkerFaceColor','w','MarkerEdgeColor','k')
        else
            plot(round(hPlot(ComptResult,1)),round(hPlot(ComptResult,2)),'ow','MarkerFaceColor','w','MarkerEdgeColor','k')
        end
    end 
end 
if isempty(h) || isequal(ComptResult,2)
    return
end

plot([round(hPlot(end,1)),round(hPlot(1,1))],[round(hPlot(end,2)),round(hPlot(1,2))],'-ok','MarkerFaceColor','w','MarkerEdgeColor','k')


% Add the domain to the list ...
BinBulk = handles.BinBulk;

WhereItGoes = length(BinBulk)+1;

TempBinBulk.Type = 1;
TempBinBulk.Name = ['LB_',num2str(WhereItGoes)];
TempBinBulk.CompositionOriginal = 'Not yet available';
TempBinBulk.CompositionModified = 'Not yet available';
TempBinBulk.CompositionIterative = 'Not yet available';
TempBinBulk.CompoDisp = 1;
TempBinBulk.DomainXrefYref = h;
%TempBinBulk.SelectedPhases = 0;  % it seems that this is not used...

% Calculate the bulk and update the corresponding fields
[TempBinBulk] = BinUpdateBulkComposition(TempBinBulk,WhereItGoes,handles);

BinBulk(WhereItGoes) = TempBinBulk;

% Update the menu
for i=1:length(BinBulk)
    ListNamesBulk{i} = BinBulk(i).Name;
end
set(handles.BinPopUpBulkCompoList,'String',ListNamesBulk,'Value',WhereItGoes);

handles.BinBulk = BinBulk;
guidata(hObject, handles);

set(gcf, 'WindowButtonMotionFcn', @mouseMove);

ActivateZoomFigure1(hObject, eventdata, handles);

BinPopUpBulkCompoList_Callback(hObject, eventdata, handles)

drawnow

% New July 2019 - Opens options everytime a bulk is defined...
BinButtonFluidGases_Callback(hObject, eventdata, handles);

return


% #########################################################################
%       BIN - Calculate the Local Bulk composition (1.2.1)
function [TempBinBulk] = BinUpdateBulkComposition(TempBinBulk,LBref,handles)
%

MapWindow = handles.MapWindow;
SF = handles.SF;
BinElData = handles.BinElData;

DensityMap = MapWindow.Mask.DensityMap;

% (1) Get the shape of the selected pixels
h = TempBinBulk.DomainXrefYref;
[LinS,ColS] = size(DensityMap);
MaskSel = Xpoly2maskX(h(:,1),h(:,2),LinS,ColS);

% (2) Remove the pixels without composition 
%     and remove the pixels of the unselected phases  (PL 14.06.17)
OxideSum=zeros(size(DensityMap));
PxSel4Bulk = zeros(size(DensityMap));
ListElem = MapWindow.Maps.ListMaps;
for i=1:length(ListElem)
    OxideSum = OxideSum + MapWindow.Maps.Data(i).ValuesOx;
end

for i = 1:length(handles.MapWindow.Mask.Selected4Bulk)
    if handles.MapWindow.Mask.Selected4Bulk(i)
        TheSpecMaskPixels = find(MapWindow.Mask.Data == i);
        PxSel4Bulk(TheSpecMaskPixels) = ones(size(TheSpecMaskPixels));
    end
end

MaskSelFinal = zeros(size(DensityMap));
PixelsSelected = find(OxideSum(:) > 0 & MaskSel(:) > 0 & PxSel4Bulk(:) > 0 & DensityMap(:)>0);
MaskSelFinal(PixelsSelected) = ones(size(PixelsSelected));

% Density correction for all maps
AverageDensity = mean(DensityMap(PixelsSelected));
%disp(num2str(AverageDensity));

for i=1:length(ListElem)
    TheMap = MapWindow.Maps.Data(i).ValuesOx;
    DCM(i).map = (TheMap.*DensityMap.*MaskSelFinal)./AverageDensity; 
end

% Atom weight for the selected elements
Concat = '0   ';
SelEl = find(BinElData.selected);
NbSelEl = length(SelEl);

FileName = 'OptionsXTT.txt';
[OptString,Valid] = ReadOptionXTT(FileName);

OptString1 = OptString{1};
OptString2 = OptString{2};

for i=1:NbSelEl
    ElCode = BinElData.listElem(SelEl(i));

    OxideValue = mean(DCM(SelEl(i)).map(PixelsSelected));
    AtomW = OxideValue/BinElData.MolarMass(SelEl(i))*BinElData.NbAtoms(SelEl(i));
    
    Concat = [Concat,char(ElCode),'(',num2str(AtomW),')'];
    
    Oxide2Print(i) = OxideValue;
    OxideName2Print{i} = char(BinElData.listOxides(SelEl(i)));
    
    %disp([char(ElCode),' ',char(num2str(OxideValue)),' ',char(num2str(AtomW))])
end 
Concat = [Concat,char(OptString1),'   ',char(OptString2),'LB_',num2str(LBref)];

TempBinBulk.CompositionOriginal = Concat;
TempBinBulk.CompositionModified = Concat;
TempBinBulk.CompositionIterative = Concat;

disp(' ')
disp(['----------------- NEW BULK COMPOSITION (',['LB_',num2str(LBref)],') -----------------'])

for i=1:length(Oxide2Print)
    fprintf('%s\t',char(OxideName2Print{i}));
end
fprintf('\n');
for i=1:length(Oxide2Print)
    fprintf('%.3f\t',Oxide2Print(i));
end
fprintf('\n\n');

return


% #########################################################################
%       BIN - Button to SAVE the domain shapes (1.2.1)
function BinButtonSaveShape_Callback(hObject, eventdata, handles)
%
BinBulk = handles.BinBulk;

[Success,Message,MessageID] = mkdir('Bingo');

cd Bingo

Databases = get(handles.BinPopUpDatabase,'String');
NameDatabase = Databases{get(handles.BinPopUpDatabase,'Value')};
filename = ['BIN_Bulk.xtt'];

if isequal(exist(filename,'file'),2)
    ButtonName = questdlg(['Would you like to update ',filename,'?'],'Save Compositions','Yes');  
    switch ButtonName
        case 'Yes'
            Question = 0;
            Directory = filename;
            pathname = [cd,'/'];
        otherwise
            Question = 1;
    end         
else
    Question = 1;
end

if Question
    [Directory, pathname] = uiputfile({'*.xtt', 'XThermoTools File (*.xtt)'}, 'Save the compositions as',filename);
end
cd ..

if Directory
    fid = fopen(strcat(pathname,Directory),'w');
    
    for i=1:length(BinBulk)
        fprintf(fid,'%s%.0f\t%.0f\n','#',i,BinBulk(i).Type);
        fprintf(fid,'%s\n',BinBulk(i).Name);
        fprintf(fid,'%s\n',BinBulk(i).CompositionOriginal);
        fprintf(fid,'%s\n',BinBulk(i).CompositionModified);
        fprintf(fid,'%s\n',BinBulk(i).CompositionIterative);
        fprintf(fid,'%.0f\n',BinBulk(i).CompoDisp);
        
        fprintf(fid,'%.0f\n',size(BinBulk(i).DomainXrefYref,1));
        fprintf(fid,'%.4f\t%.4f\n',BinBulk(i).DomainXrefYref');
    end
    fclose(fid);
end


return


% #########################################################################
%       BIN - Button to LOAD the domain shapes (1.2.1)
function BinButtonLoadShape_Callback(hObject, eventdata, handles)
%

InitPanelDisp(1,handles);

[Success,Message,MessageID] = mkdir('Bingo');

cd Bingo

Databases = get(handles.BinPopUpDatabase,'String');
NameDatabase = Databases{get(handles.BinPopUpDatabase,'Value')};
filename = ['BIN_Bulk.xtt'];

if isequal(exist(filename,'file'),2)
    ButtonName = questdlg(['Would you like to use ',filename,'?'],'Load Compositions','Yes');  
    switch ButtonName
        case 'Yes'
            Question = 0;
        otherwise
            Question = 1;
    end         
else
    Question = 1;
end

if Question
    [filename, pathname] = uigetfile({'*.xtt', 'XThermoTools File (*.xtt)'},'Pick a file');

    if ~filename
        return
    end

    fid = fopen([pathname,filename],'r');
else
    fid = fopen([filename],'r');
end


Compt = 0;
while 1
    % New block
    Compt=Compt+1;
    TheL = fgetl(fid);
    
    if isequal(TheL,'') || isequal(TheL,-1)
        break
    end
    
    TheStr = strread(TheL,'%s');
    BinBulk(Compt).Type = str2num(TheStr{2});
    
    TheL = fgetl(fid);
    BinBulk(Compt).Name = TheL;
    
    TheL = fgetl(fid);
    BinBulk(Compt).CompositionOriginal = TheL;
    
    TheL = fgetl(fid);
    BinBulk(Compt).CompositionModified = TheL;
    
    TheL = fgetl(fid);
    BinBulk(Compt).CompositionIterative = TheL;
    
    TheL = fgetl(fid);
    BinBulk(Compt).CompoDisp = str2num(TheL);
    
    TheL = fgetl(fid);
    NbCoordinates = str2num(TheL);
    
    for i=1:NbCoordinates
        TheL = fgetl(fid);
        BinBulk(Compt).DomainXrefYref(i,:) = strread(TheL,'%f')';
    end

end    
fclose(fid);    
cd ..


% ButtonName = questdlg('Would you like to recalculate the local compositions','Load Compositions','Yes');  
% switch ButtonName
%     case 'Yes'
%         Question = 1;
%     otherwise
%         Question = 0;
% end
% 
% if Question
for i=2:length(BinBulk)
    % Calculate the bulk and update the corresponding fields
    [BinBulk(i)] = BinUpdateBulkComposition(BinBulk(i),i,handles);
end
% end


% Update the menu
for i=1:length(BinBulk)
    ListNamesBulk{i} = BinBulk(i).Name;
end
set(handles.BinPopUpBulkCompoList,'String',ListNamesBulk,'Value',i);

handles.BinBulk = BinBulk;
guidata(hObject, handles);

BinPopUpBulkCompoList_Callback(hObject, eventdata, handles);

% New July 2019 - Opens options everytime a bulk is defined...
BinButtonFluidGases_Callback(hObject, eventdata, handles);

return


% #########################################################################
%       BIN - Button to synchronize the gases and fluids (1.2.1)
function BinButtonFluidGases_Callback(hObject, eventdata, handles)
% 

if ~isfield(handles,'BinGfDef') 

    % INITIALIZATION:
    PreLoad = 0;
    SelectedCompo = get(handles.BinPopUpBulkCompoList,'Value');
    if isequal(handles.BinBulk(SelectedCompo).CompoDisp,3)
        % This means that the bulk H was controlled by the Module
        PreLoad = 1;
        DispBulk = handles.BinBulk(SelectedCompo).CompositionIterative;
        WhereH = find(ismember(DispBulk,'H'));
        DispBulkCut = DispBulk(WhereH+2:end);
        Part1 = strread(DispBulkCut,'%s','delimiter',')');
        H_def = str2num(Part1{1});
    end

    iDB = handles.BingoDefault.SelectedDatabase;
    switch handles.BingoDefault.SelectedProgram
        case 1
            DefGf = handles.BingoDefault.Theriak.Database(iDB).DefGf;
        case 2
            DefGf = handles.BingoDefault.PerpleX.Database(iDB).DefGf;
    end

    DBases = get(handles.BinPopUpDatabase,'String');

    if PreLoad
        BinGfDef.Fluids.Activate = 1;
    else
        BinGfDef.Fluids.Activate = 0;
    end
    BinGfDef.Fluids.Optimize = 0;
    BinGfDef.Fluids.Spec(1).IsActive = '';
    BinGfDef.Fluids.Spec(1).Name = '';
    BinGfDef.Fluids.Spec(1).DBMinName = '';
    BinGfDef.Fluids.Spec(1).DBName = '';
    BinGfDef.Fluids.Spec(1).ListVariAntidote = '';
    BinGfDef.Fluids.Spec(1).ListWeightAntidote = 0;
    BinGfDef.Fluids.Spec(1).Bulk = 0.5;
    BinGfDef.Fluids.Spec(1).Lower = 0.0001;
    BinGfDef.Fluids.Spec(1).Upper = 1;

    BinGfDef.Melt.Activate = 0;
    BinGfDef.Melt.Include = 0;
    BinGfDef.Melt.Name = '';                  % we can have only one melt model
    BinGfDef.Melt.DBName = '';

    BinGfDef.Oxygen.ActivateExtraO = 0;
    BinGfDef.Oxygen.OptimizeExtraO = 0;
    BinGfDef.Oxygen.ExtraO.Bulk = 0;
    BinGfDef.Oxygen.ExtraO.Lower = 0.0001;
    BinGfDef.Oxygen.ExtraO.Upper = 1;

    BinGfDef.Oxygen.ActivateBuffer = 0;
    BinGfDef.Oxygen.Buffer.Name = '';                  
    BinGfDef.Oxygen.Buffer.DBName = '';


    ComptFSpec = 0;
    ComptBuffer = 0;
    
    for i = 1:size(DefGf,1)
        
        Name = DefGf{i,1};
        NameS = strread(Name,'%s','delimiter','_');
        
        if iscell(NameS)
            if isequal(NameS{1},'Fluid')
                ComptFSpec = ComptFSpec +1;
                
                BinGfDef.Fluids.Spec(ComptFSpec).IsActive = 0;
                BinGfDef.Fluids.Spec(ComptFSpec).Name = DefGf{i,1};
                BinGfDef.Fluids.Spec(ComptFSpec).DBMinName = DefGf{i,3};
                BinGfDef.Fluids.Spec(ComptFSpec).DBName = DBases{get(handles.BinPopUpDatabase,'Value')};
                BinGfDef.Fluids.Spec(ComptFSpec).ListVariAntidote = DefGf{i,4};
                BinGfDef.Fluids.Spec(ComptFSpec).ListWeightAntidote = ''; %DefGf{i,5};
                BinGfDef.Fluids.Spec(ComptFSpec).Bulk = 0;
                BinGfDef.Fluids.Spec(ComptFSpec).Lower = 0.0001;
                BinGfDef.Fluids.Spec(ComptFSpec).Upper = 1;
                
                
            elseif isequal(NameS{1},'Buffer')

                ComptBuffer = ComptBuffer+1;
                
                BinGfDef.Oxygen.ActivateBuffer = 0;
                BinGfDef.Oxygen.Buffer.Name = DefGf{i,1};                  
                BinGfDef.Oxygen.Buffer.DBName = DefGf{i,3};    
    
            end
        end
        
        if isequal(Name,'Melt')
            
            BinGfDef.Melt.Activate = 0;
            BinGfDef.Melt.Include = 0;
            BinGfDef.Melt.Name = DefGf{i,1};       
            BinGfDef.Melt.DBName = DefGf{i,3};
            
            % Option to compare the composition to be added later on!
            
        end
        
        % New PL july 2019 to start with H: 
        BinGfDef.Fluids.Activate = 1;
        BinGfDef.Fluids.Spec(1).IsActive = 1;
        BinGfDef.Fluids.Spec(1).Bulk = 1;
        
    end
    
    if PreLoad
        % Here we add the PreLoad read from the bulk...  (H ONLY)
        BinGfDef.Fluids.Spec(1).IsActive = 1;
        BinGfDef.Fluids.Spec(1).Bulk = H_def;
    end
    
else
    BinGfDef = handles.BinGfDef;
end

%keyboard

[BinGfDef] = XTTselectFluidsGasesBuffers(BinGfDef); 


% DO we need to update the bulk?
BinBulk = handles.BinBulk;
SelectedCompo = get(handles.BinPopUpBulkCompoList,'Value');

[Bulk,BinBulk] = SuperFastBulkUpdate(BinGfDef,BinBulk,SelectedCompo);


% Update the Composition
BinBulk(SelectedCompo).CompositionIterative = Bulk;

handles.BinBulk = BinBulk;
handles.BinGfDef = BinGfDef;
guidata(hObject, handles);

BinPopUpBulkCompoList_Callback(hObject, eventdata, handles)
BinWereWeAre(hObject, eventdata, handles);
return


% This one is for testing (should not be used very soon)
function [Bulk,BinBulk] = SuperFast_H_Update(BinBulk,HVal)

% Always use the iterative there...
SelectedCompo = 1;

BinBulk(SelectedCompo).CompoDisp = 3;  % We take the iterative bulk!
BulkOri = BinBulk(SelectedCompo).CompositionIterative;

% Update H:
[S,IdxS] = ismember('H',BulkOri);

Part1 = BulkOri(1:IdxS+1);
while 1
    IdxS = IdxS + 1;
    if isequal(BulkOri(IdxS),')')
        break
    end
end
Part2 = BulkOri(IdxS:end);

Bulk = [Part1,num2str(HVal),Part2];
BinBulk(SelectedCompo).CompositionIterative = Bulk;

return


% #########################################################################
%       BIN - Menu to DISPLAY the phases (1.2.1)
function BinPopUpPhaseList_Callback(hObject, eventdata, handles)
%

InitPanelDisp(1,handles)

SelectedPhase = get(handles.BinPopUpPhaseList,'Value');
BinPhaseDef = handles.BinPhaseDef;

if BinPhaseDef(SelectedPhase).IsPhaseIn
    set(handles.BinCheckBoxIsPhaseIn,'Value',1);
    set(handles.BinTextPhaseTheriakName,'String',BinPhaseDef(SelectedPhase).DBMinName);
    
    set(handles.Menu_PhasesAddGrp,'Enable','on');
else
    set(handles.BinCheckBoxIsPhaseIn,'Value',0);
    set(handles.BinTextPhaseTheriakName,'String','');
    set(handles.BinTextPhaseTheriakVolProp,'String','');
    
    set(handles.Menu_PhasesAddGrp,'Enable','off');
end


if BinPhaseDef(SelectedPhase).SelGrps
    TheName = BinPhaseDef(SelectedPhase).Grps(BinPhaseDef(SelectedPhase).SelGrps).Name;
else
    TheName = BinPhaseDef(SelectedPhase).Grps(1).Name;
end

set(handles.Menu_PhasesDeleteGrp,'Enable','off');

% PL UPDATE: to BinPhaseDef(SelectedPhase).NbGrps
if BinPhaseDef(SelectedPhase).NbGrps %length(TheName)
    % Update the display and Grp list
    UpdateDisplayGrpList(BinPhaseDef,SelectedPhase,handles);
    
    BinPopUpGrpOfPhase_Callback(hObject, eventdata, handles);
    if length(BinPhaseDef(SelectedPhase).PhaseVolProp)
        set(handles.BinTextPhaseTheriakVolProp,'String',num2str(BinPhaseDef(SelectedPhase).PhaseVolProp));
    end
else
    % Clean and plot the selected shape
    BinPopUpBulkCompoList_Callback(hObject, eventdata, handles);
    set(handles.BinTextPhaseTheriakVolProp,'String','');
end
   
guidata(hObject, handles);
BinWereWeAre(hObject, eventdata, handles)
return


% #########################################################################
%       BIN - Function to UPDATE the Group menu (1.2.1)
function UpdateDisplayGrpList(BinPhaseDef,SelectedPhase,handles)
%
for i=1:BinPhaseDef(SelectedPhase).NbGrps
    ListNameGrp{i} = BinPhaseDef(SelectedPhase).Grps(i).Name;
end

if i > 1
    set(handles.Menu_PhasesDeleteGrp,'Enable','on');
else
    set(handles.Menu_PhasesDeleteGrp,'Enable','off');
end

set(handles.BinPopUpGrpOfPhase,'String',ListNameGrp,'Value',BinPhaseDef(SelectedPhase).SelGrps);
return


% #########################################################################
%       BIN - CheckBox to deactivate the selected phase (1.2.1)
function BinCheckBoxIsPhaseIn_Callback(hObject, eventdata, handles)
%

InitPanelDisp(1,handles);

SelectedPhase = get(handles.BinPopUpPhaseList,'Value');
BinPhaseDef = handles.BinPhaseDef;

BinPhaseDef(SelectedPhase).IsPhaseIn = 0;

%PL UPDATE   - this should work ...
BinPhaseDef(SelectedPhase).NbGrps = 0;
BinPhaseDef = FctResetGrps(BinPhaseDef,SelectedPhase);

BinPhaseDef(SelectedPhase).SelGrps = 0;
BinPhaseDef(SelectedPhase).VolGrps = [];

for i=1:length(BinPhaseDef)
    BinPhaseDef(i).PhaseVolProp = [];
    %BinPhaseDef(i).NbGrps = 0;
    
    % PL UPDATE   - this should work ...
    %BinPhaseDef(i).NbGrps = 0;
    %BinPhaseDef = FctResetGrps(BinPhaseDef,i);
     
    %BinPhaseDef(i).SelGrps = 0;
    %BinPhaseDef(i).VolGrps = [];
end

handles.BinPhaseDef = BinPhaseDef;

guidata(hObject, handles);
BinPopUpPhaseList_Callback(hObject, eventdata, handles)
return


% #########################################################################
%       BIN - Button to synchronize the selected phase (1.2.1)
function BinButtonSyncPhase_Callback(hObject, eventdata, handles)
%
InitPanelDisp(1,handles);

BinPhaseDef = handles.BinPhaseDef;

SelectedPhase = get(handles.BinPopUpPhaseList,'Value');
ListPhases = get(handles.BinPopUpPhaseList,'String');

iDB = handles.BingoDefault.SelectedDatabase;
switch handles.BingoDefault.SelectedProgram
    case 1
        DefMin = handles.BingoDefault.Theriak.Database(iDB).DefMin;
    case 2
        DefMin = handles.BingoDefault.PerpleX.Database(iDB).DefMin;
end

for i=1:length(DefMin(:,1))
    ListRefMiner{i} = DefMin{i,1};
end
ListRefMiner = ListRefMiner';

NamePhaseForSearch = upper(ListPhases{SelectedPhase});

[YesIs,WereIsPhase] = ismember(NamePhaseForSearch,ListRefMiner);

if ~YesIs
    [WereIsPhase,OK] = listdlg('ListString',ListRefMiner,'SelectionMode','single');
    if ~OK
        return
    end
end

BinPhaseDef(SelectedPhase).IsPhaseIn = 1;
BinPhaseDef(SelectedPhase).OxBasis = DefMin{WereIsPhase,2};
BinPhaseDef(SelectedPhase).ListVariAntidote = DefMin{WereIsPhase,4};
BinPhaseDef(SelectedPhase).ListWeightAntidote = [];     % DefMin{WereIsPhase,5};   PL ** 03.03.19 
DBases = get(handles.BinPopUpDatabase,'String');
BinPhaseDef(SelectedPhase).DBName = DBases{get(handles.BinPopUpDatabase,'Value')};
BinPhaseDef(SelectedPhase).DBMinName = DefMin{WereIsPhase,3};
BinPhaseDef(SelectedPhase).PhaseVolProp = [];

BinPhaseDef(SelectedPhase).NbGrps = 0;
BinPhaseDef = FctResetGrps(BinPhaseDef,SelectedPhase);

BinPhaseDef(SelectedPhase).SelGrps = 0;
BinPhaseDef(SelectedPhase).VolGrps = [];

set(handles.BinPopUpGrpOfPhase,'String',{''},'Value',1);
% Clean and plot the selected shape
BinPopUpBulkCompoList_Callback(hObject, eventdata, handles);

handles.BinPhaseDef = BinPhaseDef;

%BinPhaseDef(SelectedPhase)

guidata(hObject, handles);
BinPopUpPhaseList_Callback(hObject, eventdata, handles)
return



% #########################################################################
%       BIN - Button to MANUALY synchronize the selected phase (1.2.1)
function BinButtonSyncManualPhase_Callback(hObject, eventdata, handles)
% 
InitPanelDisp(1,handles);

BinPhaseDef = handles.BinPhaseDef;

SelectedPhase = get(handles.BinPopUpPhaseList,'Value');
ListPhases = get(handles.BinPopUpPhaseList,'String');

iDB = handles.BingoDefault.SelectedDatabase;
switch handles.BingoDefault.SelectedProgram
    case 1
        DefMin = handles.BingoDefault.Theriak.Database(iDB).DefMin;
    case 2
        DefMin = handles.BingoDefault.PerpleX.Database(iDB).DefMin;
end

for i=1:length(DefMin(:,1))
    ListRefMiner{i} = DefMin{i,1};
end
ListRefMiner = ListRefMiner';

NamePhaseForSearch = upper(ListPhases{SelectedPhase});

[YesIs,WereIsPhase] = ismember(NamePhaseForSearch,ListRefMiner);
 
InitialValue = zeros(size(ListRefMiner));
if YesIs
    InitialValue = WereIsPhase;
else
    InitialValue = 1;
end


[WereIsPhase,OK] = listdlg('ListString',ListRefMiner,'SelectionMode','single','InitialValue',InitialValue);
if ~OK
    return
end

BinPhaseDef(SelectedPhase).IsPhaseIn = 1;
BinPhaseDef(SelectedPhase).OxBasis = DefMin{WereIsPhase,2};
BinPhaseDef(SelectedPhase).ListVariAntidote = DefMin{WereIsPhase,4};
BinPhaseDef(SelectedPhase).ListWeightAntidote = []; %DefMin{WereIsPhase,5};
DBases = get(handles.BinPopUpDatabase,'String');
BinPhaseDef(SelectedPhase).DBName = DBases{get(handles.BinPopUpDatabase,'Value')};
BinPhaseDef(SelectedPhase).DBMinName = DefMin{WereIsPhase,3};
BinPhaseDef(SelectedPhase).PhaseVolProp = [];

BinPhaseDef(SelectedPhase).NbGrps = 0;
BinPhaseDef = FctResetGrps(BinPhaseDef,SelectedPhase);

BinPhaseDef(SelectedPhase).SelGrps = 0;
BinPhaseDef(SelectedPhase).VolGrps = [];

set(handles.BinPopUpGrpOfPhase,'String',{''},'Value',1);
% Clean and plot the selected shape
BinPopUpBulkCompoList_Callback(hObject, eventdata, handles);

handles.BinPhaseDef = BinPhaseDef;

%BinPhaseDef(SelectedPhase)

guidata(hObject, handles);
BinPopUpPhaseList_Callback(hObject, eventdata, handles)
return


% #########################################################################
%       BIN - RESET groups variable (1.2.1)
function [BinPhaseDef] = FctResetGrps(BinPhaseDef,SelectedPhase)

%for i=1:length(BinPhaseDef(SelectedPhase).Grps)
BinPhaseDef(SelectedPhase).Grps(1).IsGrp = 0;
BinPhaseDef(SelectedPhase).Grps(1).Name = '';
BinPhaseDef(SelectedPhase).Grps(1).XrefYref = [];

BinPhaseDef(SelectedPhase).Grps(1).OxideCompositions = [];
BinPhaseDef(SelectedPhase).Grps(1).SigmaOxide = [];
BinPhaseDef(SelectedPhase).Grps(1).OxideLabels = [];
BinPhaseDef(SelectedPhase).Grps(1).SFCompositions =[];
BinPhaseDef(SelectedPhase).Grps(1).SigmaSF =[];
BinPhaseDef(SelectedPhase).Grps(1).SFLabels =[];

BinPhaseDef(SelectedPhase).Grps = BinPhaseDef(SelectedPhase).Grps(1);
%end

return


% #########################################################################
%       BIN - Menu of phase groups (1.2.1)
function BinPopUpGrpOfPhase_Callback(hObject, eventdata, handles)
%

InitPanelDisp(1,handles);

% Clean and plot the selected shape
% This comes from BinPopUpBulkCompoList_Callback(hObject, eventdata, handles)
% but we do not want to use that one that clean the vol fractions
% -------------------------------------------------------------
BinBulk = handles.BinBulk;
SelectedCompo = get(handles.BinPopUpBulkCompoList,'Value');

ButtonClean_Callback(hObject, eventdata, handles);
% If there is the shape: Plot the shape
if isequal(BinBulk(SelectedCompo).Type,1)
    PlotDomainShape(BinBulk(SelectedCompo),handles);
end
% -------------------------------------------------------------

BinPhaseDef = handles.BinPhaseDef;
SelectedPhase = get(handles.BinPopUpPhaseList,'Value');
iGrp = get(handles.BinPopUpGrpOfPhase,'Value');

handles.BinPhaseDef(SelectedPhase).SelGrps = iGrp; 
Xref = BinPhaseDef(SelectedPhase).Grps(iGrp).XrefYref(:,1);
Yref = BinPhaseDef(SelectedPhase).Grps(iGrp).XrefYref(:,2);

[XFig,YFig] = CoordinatesFromRef(Xref,Yref,handles);

if size(Xref,1) > 1
    for i=1:length(XFig)-1
        plot([round(XFig(i)),round(XFig(i+1))],[round(YFig(i)),round(YFig(i+1))],'-om','MarkerFaceColor','w','MarkerEdgeColor','m')
    end
    plot([round(XFig(end)),round(XFig(1))],[round(YFig(end)),round(YFig(1))],'-om','MarkerFaceColor','w','MarkerEdgeColor','m')
else
    plot([round(XFig(end)),round(XFig(1))],[round(YFig(end)),round(YFig(1))],'-om','MarkerFaceColor','w','MarkerEdgeColor','m')
end

guidata(hObject, handles);
return


% #########################################################################
%       BIN - button to ADD a phase groups (1.2.1)
function BinButtonAddGroupOfPhase_Callback(hObject, eventdata, handles)
ManageChemicalGroups(1,hObject, eventdata, handles);
return


% #########################################################################
%       BIN - Function to manage the groups (add/delete) (1.5)
function ManageChemicalGroups(Mode,hObject, eventdata, handles)
%
% --------------------------------------------------------
%   Mode    Description
% --------------------------------------------------------
%   1       Add a new group
%   2       Replace a group          *** NOT AVAILABLE YET
%   3       Delete the group
% --------------------------------------------------------

BinPhaseDef = handles.BinPhaseDef;
SelectedPhase = get(handles.BinPopUpPhaseList,'Value');

% --------
% Update the display
InitPanelDisp(1,handles);
BinPopUpBulkCompoList_Callback(hObject, eventdata, handles);

% --------
% If we delete a group, we first update "BinPhaseDef"
if isequal(Mode,3)  
    
    Grp2Delete = BinPhaseDef(SelectedPhase).SelGrps;
    
    BinPhaseDef(SelectedPhase).NbGrps = BinPhaseDef(SelectedPhase).NbGrps - 1;
    BinPhaseDef(SelectedPhase).SelGrps = 1;
    
    OldGrps = BinPhaseDef(SelectedPhase).Grps;
    Compt = 0;
    for i = 1:length(OldGrps)
        if ~isequal(i,Grp2Delete)
            Compt = Compt+1;
            NewGrps(Compt) = OldGrps(i);
            NewGrps(Compt).Name = ['Grp_',num2str(Compt)];
        end
    end
    
    BinPhaseDef(SelectedPhase).Grps = NewGrps;
    NbGroups4Prop = Compt;
end


% --------
% If we add a group, we need to select a domain (mandatory)
if isequal(Mode,1) 
    
    axes(handles.axes1), hold on
    %zoom off
    
    XLimsMap = get(handles.axes1,'Xlim');
    YLimsMap = get(handles.axes1,'Ylim');
    
    Click=0;
    ComptResult=0;
    h=[];
    while Click <= 2
        [X,Y,Click] = XTTginputXTT(1,handles);
        if Click < 2
            if X < XLimsMap(1) || X > XLimsMap(2) || Y < YLimsMap(1) || Y > YLimsMap(2)
                warndlg({'Aborded...','Do not click outside the map while selecting an area!'},'Bingo-Antidote')
            return
            end
        end
        [XFig,YFig] = CoordinatesFromRef(X,Y,handles);
        
        if Click < 2 
            ComptResult = ComptResult+1;
            h(ComptResult,1) = X;
            h(ComptResult,2) = Y;
            hPlot(ComptResult,1) = XFig;             % for display (Fig coordinate system)
            hPlot(ComptResult,2) = YFig;
            
            if ComptResult >= 2
                plot([round(hPlot(ComptResult-1,1)),round(hPlot(ComptResult,1))],[round(hPlot(ComptResult-1,2)),round(hPlot(ComptResult,2))],'-om','MarkerFaceColor','w','MarkerEdgeColor','m')
            else
                plot(round(hPlot(ComptResult,1)),round(hPlot(ComptResult,2)),'ow','MarkerFaceColor','w','MarkerEdgeColor','m')
            end
        end
    end
    
    if isempty(h) || isequal(ComptResult,1) || isequal(ComptResult,2) 
        warndlg({'Aborded...','at least 3 points are required to define an area'},'Bingo-Antidote')
        return
    end
    
    if ComptResult>1;
        plot([round(hPlot(end,1)),round(hPlot(1,1))],[round(hPlot(end,2)),round(hPlot(1,2))],'-om','MarkerFaceColor','w','MarkerEdgeColor','m')
    end
    drawnow
    
    NbGrps = BinPhaseDef(SelectedPhase).NbGrps;
    iGrp = NbGrps + 1;
    
    BinPhaseDef(SelectedPhase).NbGrps = iGrp;
    NbGroups4Prop = iGrp;
    
end  
  
%set(gca,{'xlim','ylim'},ZoomValues);

% volume of groups to be updated later on...
BinPhaseDef(SelectedPhase).VolGrps = ones(1,NbGroups4Prop)*100/NbGroups4Prop; % to be updated later
 
% We update "BinPhaseDef"
if isequal(Mode,1) 
    BinPhaseDef(SelectedPhase).Grps(iGrp).IsGrp = 1;
    BinPhaseDef(SelectedPhase).Grps(iGrp).Name = ['Grp_',num2str(iGrp)];
    BinPhaseDef(SelectedPhase).Grps(iGrp).XrefYref = h;
    
    % --------------------------------------------------------
    % Import required variables
    BinElData = handles.BinElData;
    MapWindow = handles.MapWindow;
    
    SelectedMask = get(handles.BinPopUpPhaseList,'Value');
    MaskMap = MapWindow.Mask.Data;
    
    MaskLayer = zeros(size(MapWindow.Maps.Data(1).ValuesOx));
    WhichOnesPx = find(MaskMap(:) == SelectedMask);
    MaskLayer(WhichOnesPx) = ones(size(WhichOnesPx));
    
    OxideSum=zeros(size(MapWindow.Mask.DensityMap));
    ListElem = MapWindow.Maps.ListMaps;
    
    [nPixV, nPixH] = size(MapWindow.Maps.Data(1).ValuesOx);
    Chemic = zeros(nPixV*nPixH,length(ListElem));
    
    for i=1:length(ListElem)
        OxideSum = OxideSum + MapWindow.Maps.Data(i).ValuesOx;
        Chemic(:,i) = MapWindow.Maps.Data(i).ValuesOx(:).*MaskLayer(:);
    end
    
    % --------------------------------------------------------
    % Compositions (at the right format)
    [LinS,ColS] = size(MapWindow.Mask.DensityMap);
    TheMapCoord = reshape([1:LinS*ColS],LinS,ColS);
    
    switch size(h,1)
        case 1
            SelPixels = round(h(2)-1)*LinS+round(h(1));
            SelPixels = TheMapCoord(round(h(2)),round(h(1)));
            
            if ~isequal(MaskMap(SelPixels),SelectedMask)
                SelPixels = [];
            end
            
        otherwise
            MaskSel = Xpoly2maskX(h(:,1),h(:,2),LinS,ColS);
            
            SelPixels = find(MaskMap(:) == SelectedMask & MaskSel(:) == 1);
    end
    
    if isempty(SelPixels)
        warndlg(['The selection does not belong to ',char(MapWindow.Mask.ListMin(SelectedMask))],'Warning');
        return
    end
    
    SelectedMaps = find(BinElData.selected);
    
    disp(' ')
    disp(['Mineral: ',BinPhaseDef(SelectedPhase).name, ' || Grp: ',num2str(iGrp)])
    disp(' ')
    
    for i=1:length(SelectedMaps)
        
        % With outlier rejection                                (PL - 18.05.18)
        SelData = MapWindow.Maps.Data(SelectedMaps(i)).ValuesOx(SelPixels);
        
        NbPixels = length(SelData);
        Average = mean(SelData);
        Sigma6 = 6*std(SelData);
        Outliers = find(SelData > Average+Sigma6 | SelData < Average-Sigma6);
        NotOutliers = find(SelData < Average+Sigma6 | SelData > Average-Sigma6);
        
        NbOutliers = length(Outliers);
        
        if NbOutliers/NbPixels < 0.05 && NbOutliers/NbPixels  > 0
            disp(sprintf('\t%s\t%s',char(BinElData.listElem{SelectedMaps(i)}),[num2str(NbOutliers),'/',num2str(NbPixels),' (',num2str(NbOutliers/NbPixels*100),' %) of the pixel(s) rejected (6 sigma test, 5% cut-off threshold)']))
            SelData = SelData(NotOutliers);
        else
            disp(sprintf('\t%s\t%s',[char(BinElData.listElem{SelectedMaps(i)}),'*'],[num2str(NbOutliers),'/',num2str(NbPixels),' (',num2str(NbOutliers/NbPixels*100),' %) of the pixels are apparently outliers (6 sigma test, 5% cut-off threshold) - ALL PIXELS SELECTED']))
        end
        
        CompositionOxide(i) = mean(SelData);
        SigmaOxide(i) = std(SelData);
        OxideLabels{i} = BinElData.listOxides{SelectedMaps(i)};
        SFLabels{i} = BinElData.listElem{SelectedMaps(i)};
        MolarMass(i) = BinElData.MolarMass(SelectedMaps(i));
        NbAtoms(i) = BinElData.NbAtoms(SelectedMaps(i));
        NbOxygen(i) = BinElData.NbOxygen(SelectedMaps(i));
        
        if isnan(SigmaOxide(i))
            disp('This shouldn''t have happened, please contact pierre.lanari@geo.unibe.ch (ERR1628)')
            disp('type ''return'' to continue')
            keyboard
        end
    end
    
    [SFCompositions,SigmaSF] = BinStructForm(CompositionOxide,SigmaOxide,MolarMass,NbAtoms,NbOxygen,BinPhaseDef(SelectedPhase).OxBasis);
    
    % Check for NaN (zero concentrations in oxide)
    WhereNaN = find(isnan(SigmaSF));
    if length(WhereNaN)
        SigmaSF(WhereNaN) = zeros(size(WhereNaN));
    end
    
    disp(' ')
    
    h = sprintf('\t');
    for n1=1:length(OxideLabels)
        h = [h,sprintf('%s\t',char(OxideLabels{n1}))];
    end
    disp(h)
    h = sprintf('\t');
    for n1=1:length(CompositionOxide)
        h = [h,sprintf('%.2f\t',CompositionOxide(n1))];
    end
    disp(h)
    h = sprintf('\t');
    for n1=1:length(SigmaOxide)
        h = [h,sprintf('%.2f\t',SigmaOxide(n1))];
    end
    disp(h)
    disp(' ')
    h = sprintf('\t');
    for n1=1:length(SFLabels)
        h = [h,sprintf('%s\t',char(SFLabels{n1}))];
    end
    disp(h)
    h = sprintf('\t');
    for n1=1:length(SFCompositions)
        h = [h,sprintf('%.2f\t',SFCompositions(n1))];
    end
    disp(h)
    h = sprintf('\t');
    for n1=1:length(SigmaSF)
        h = [h,sprintf('%.2f\t',SigmaSF(n1))];
    end
    disp(h)
    
    BinPhaseDef(SelectedPhase).Grps(iGrp).OxideCompositions = CompositionOxide;
    BinPhaseDef(SelectedPhase).Grps(iGrp).SigmaOxide = SigmaOxide;
    BinPhaseDef(SelectedPhase).Grps(iGrp).OxideLabels = OxideLabels;
    BinPhaseDef(SelectedPhase).Grps(iGrp).SFCompositions = SFCompositions;
    BinPhaseDef(SelectedPhase).Grps(iGrp).SigmaSF = SigmaSF;
    BinPhaseDef(SelectedPhase).Grps(iGrp).SFLabels = SFLabels;
    %BinPhaseDef(SelectedPhase).Grps(iGrp).OxBasis = BinPhaseDef(SelectedPhase).OxBasis;
    
end
    
NbMasks = BinPhaseDef(SelectedPhase).NbGrps + 1; % We apparently add an other mask for the pixels that are not garnet...

if NbMasks > 2
    % we need to perform a quick classification
    for i=1:NbMasks-1
        ChemicRef(i,:) = BinPhaseDef(SelectedPhase).Grps(i).OxideCompositions;
    end
    ChemicRef(NbMasks,:) = zeros(size(ChemicRef(1,:)));
    
    RunSWOT(1,handles);
    Groups = XkmeansX(Chemic(:,SelectedMaps), NbMasks, 'start', ChemicRef,'MaxIter',0); 
    Groups = reshape(Groups, nPixV, nPixH);
    
    figure, imagesc(Groups), axis image, colorbar
    drawnow
    for i=1:NbMasks-1
        NbPixels(i) = length(find(Groups(:) == i));
    end
    
    Fractions = NbPixels./sum(NbPixels)*100;
    
    % Update...
    BinPhaseDef(SelectedPhase).VolGrps = Fractions;
    RunSWOT(0,handles);
end

% Always select the last group...
BinPhaseDef(SelectedPhase).SelGrps = length(BinPhaseDef(SelectedPhase).Grps);

% Check PL ---
%BinPhaseDef(SelectedPhase)
%if isequal(BinPhaseDef(SelectedPhase).SelGrps,0)
%    keyboard
%end

handles.BinPhaseDef = BinPhaseDef;
guidata(hObject, handles);

% This might still crash (but it seems to work out well)
UpdateDisplayGrpList(BinPhaseDef,SelectedPhase,handles);  
% --

BinPopUpGrpOfPhase_Callback(hObject, eventdata, handles);
BinWereWeAre(hObject, eventdata, handles);
return


% #########################################################################
%       BIN - STRUCTURAL FORMULA (1.2.1)
function [SFCompositions,SigmaSF] = BinStructForm(CompositionOxide,SigmaOxide,MolarMass,NbAtoms,NbOxygen,OxBasis)
%

AtomicPer = CompositionOxide./MolarMass.*NbAtoms;
TheSum = sum((AtomicPer .* NbOxygen) ./ NbAtoms);
RefOx = TheSum/OxBasis;

SFCompositions = AtomicPer / RefOx;


% Uncertainty propagation  - - - - - - - - - - - - - - - -  (PL - 28.05.18)
% Tested using a MC test (see below)

SigmaAtomicPer = AtomicPer.*(SigmaOxide./CompositionOxide);     % Tested OK

A = (AtomicPer .* NbOxygen) ./ NbAtoms;
SigmaA = A.*SigmaAtomicPer./AtomicPer;                          % Tested OK

% Check for NaN... 
WhereNaN = find(isnan(SigmaA));
if length(WhereNaN)
    SigmaA(WhereNaN) = zeros(size(WhereNaN)); 
end 

SigmaTheSum = sqrt(sum(SigmaA.^2));

SigmaRefOx = RefOx*(SigmaTheSum/TheSum);

SigmaSF = SFCompositions.*(sqrt((SigmaAtomicPer./AtomicPer).^2+(SigmaRefOx./RefOx)^2));


% Check for NaN (zero concentrations in oxide)
WhereNaN = find(isnan(SigmaSF));
if length(WhereNaN)
    SigmaSF(WhereNaN) = zeros(size(WhereNaN)); 
end 





% Monte Carlo TEST 
MCtest = 0; 
if isequal(MCtest,1)
    
    NbPerm = 10000;
    
    
    CompositionOxideMC = repmat(CompositionOxide,NbPerm,1) + randn(NbPerm,size(CompositionOxide,2)).*repmat(SigmaOxide,NbPerm,1);
    
    disp(num2str(CompositionOxide))
    disp(num2str(mean(CompositionOxideMC)))
    disp(' ')
    disp(num2str(SigmaOxide))
    disp(num2str(std(CompositionOxideMC)))
    disp(' - - - - - - ')
    
    AtomicPerMC = zeros(NbPerm,size(AtomicPer,2));
    TheSumMC = zeros(NbPerm,size(TheSum,2));
    RefOxMC = zeros(NbPerm,size(RefOx,2));
    SFCompositionsMC = zeros(NbPerm,size(SFCompositions,2));
        
    for i = 1:NbPerm
        
        AtomicPerMC(i,:) = CompositionOxideMC(i,:)./MolarMass.*NbAtoms;
        TheSumMC(i) = sum((AtomicPerMC(i,:) .* NbOxygen) ./ NbAtoms);
        RefOxMC(i) = TheSumMC(i)/OxBasis;
        
        SFCompositionsMC(i,:) = AtomicPerMC(i,:) / RefOxMC(i);
        
        %SFCompositionsMC(i,:) = BinStructForm(CompositionOxideMC(i,:),SigmaOxide,MolarMass,NbAtoms,NbOxygen,BinPhaseDef(SelectedPhase).OxBasis);  
    end
    
    disp(' ')
    disp(num2str(SFCompositions))
    disp(num2str(mean(SFCompositionsMC)))
    disp(' ')
    disp(num2str(SigmaSF))
    disp(num2str(std(SFCompositionsMC)))     
end

%keyboard

return


% #########################################################################
%       BIN - button to CALCULATE the phase proportions (1.2.1)
function BinButtonCalcPhase_Callback(hObject, eventdata, handles)
% 

% DoWeCalc = questdlg('All phases must have been defined and synchronized before extracting the phase proportions','What do we do?','Apply','Cancel','Apply');
%             
% switch DoWeCalc
%     case 'Cancel'
%         return
%         
%     case 'Apply'
        
MapWindow = handles.MapWindow;
BinBulk = handles.BinBulk;
SelBinBulk = get(handles.BinPopUpBulkCompoList,'Value');

% (1) Get the shape of the selected pixels
h = BinBulk(SelBinBulk).DomainXrefYref;
[LinS,ColS] = size(MapWindow.Mask.Data);
MaskSel = Xpoly2maskX(h(:,1),h(:,2),LinS,ColS);

BinPhaseDef = handles.BinPhaseDef;

TheSelMaskOk = MaskSel .* MapWindow.Mask.Data;

%figure, imagesc(MapWindow.Mask.Data), axis image, colorbar
%keyboard

Compt = 0;
for i=1:length(BinPhaseDef)
    if BinPhaseDef(i).IsPhaseIn
        Compt = Compt+1;
        PhaseSelected(Compt) = i;
        NbPixels(Compt) = length(find(TheSelMaskOk(:) == i));
    end
end

Proportions = NbPixels./sum(NbPixels)*100;

Compt = 0;
for i=1:length(BinPhaseDef)
    if BinPhaseDef(i).IsPhaseIn
        Compt = Compt+1;
        BinPhaseDef(i).PhaseVolProp = Proportions(Compt);
    end
end

handles.BinPhaseDef = BinPhaseDef;
guidata(hObject, handles);

BinPopUpPhaseList_Callback(hObject, eventdata, handles)

% end


return





% #########################################################################
%       BIN - Button to SAVE the Groups (1.2.1)
function BinButtonSaveGrp_Callback(hObject, eventdata, handles)
% 

BinPhaseDef = handles.BinPhaseDef; 
  
[Success,Message,MessageID] = mkdir('Bingo');

Databases = get(handles.BinPopUpDatabase,'String');
NameDatabase = Databases{get(handles.BinPopUpDatabase,'Value')};
filename = ['BIN_Phases_',NameDatabase,'_.xtt'];

cd Bingo
if isequal(exist(filename,'file'),2)
    ButtonName = questdlg(['Would you like to update ',filename,'?','Save Phase Definitions'],'Yes');  
    switch ButtonName
        case 'Yes'
            Question = 0;
            Directory = filename;
            pathname = [cd,'/'];
        otherwise
            Question = 1;
    end         
else
    Question = 1;
end

if Question
    [Directory, pathname] = uiputfile({'*.xtt', 'XThermoTools File (*.xtt)'}, 'Save the Phase Definitions as',filename);
end
cd ..

if Directory
    fid = fopen(strcat(pathname,Directory),'w');

    for i=1:length(BinPhaseDef)
        fprintf(fid,'%s%.0f\t%.0f\n','#',i,BinPhaseDef(i).IsPhaseIn);
        fprintf(fid,'%s\n',BinPhaseDef(i).name);
        
        if BinPhaseDef(i).IsPhaseIn
            fprintf(fid,'%f\n',BinPhaseDef(i).OxBasis);
            
            for n1=1:length(BinPhaseDef(i).ListVariAntidote)
                fprintf(fid,'%s\t',char(BinPhaseDef(i).ListVariAntidote{n1}));
            end
            fprintf(fid,'\n');

            for n1=1:length(BinPhaseDef(i).ListWeightAntidote)
                fprintf(fid,'%.4f\t',BinPhaseDef(i).ListWeightAntidote(n1));
            end
            fprintf(fid,'\n');
            
            fprintf(fid,'%s\n',BinPhaseDef(i).DBName); 
            fprintf(fid,'%s\n',BinPhaseDef(i).DBMinName);
            fprintf(fid,'%f\n',BinPhaseDef(i).PhaseVolProp);

            fprintf(fid,'%f\n',BinPhaseDef(i).NbGrps);
            
            fprintf(fid,'%f\n',BinPhaseDef(i).SelGrps); 
            
            for n1=1:length(BinPhaseDef(i).VolGrps)
                fprintf(fid,'%f\t',BinPhaseDef(i).VolGrps(n1));
            end
            fprintf(fid,'\n');
            
            for j=1:BinPhaseDef(i).NbGrps
                fprintf(fid,'%s%.0f\t%.0f\n','>',j,BinPhaseDef(i).Grps(j).IsGrp);
                
                fprintf(fid,'%s\n',BinPhaseDef(i).Grps(j).Name);

                for n1=1:2
                    for n2=1:size(BinPhaseDef(i).Grps(j).XrefYref,1)
                        fprintf(fid,'%.4f\t',BinPhaseDef(i).Grps(j).XrefYref(n2,n1));
                    end
                    fprintf(fid,'\n');
                end

                for n1=1:length(BinPhaseDef(i).Grps(j).OxideCompositions)
                    fprintf(fid,'%.4f\t',BinPhaseDef(i).Grps(j).OxideCompositions(n1));
                end
                fprintf(fid,'\n');
                
                for n1=1:length(BinPhaseDef(i).Grps(j).SigmaOxide)
                    fprintf(fid,'%.4f\t',BinPhaseDef(i).Grps(j).SigmaOxide(n1));
                end
                fprintf(fid,'\n');

                for n1=1:length(BinPhaseDef(i).Grps(j).OxideLabels)
                    fprintf(fid,'%s\t',BinPhaseDef(i).Grps(j).OxideLabels{n1});
                end
                fprintf(fid,'\n');

                for n1=1:length(BinPhaseDef(i).Grps(j).SFCompositions)
                    fprintf(fid,'%.4f\t',BinPhaseDef(i).Grps(j).SFCompositions(n1));
                end
                fprintf(fid,'\n');
                
                for n1=1:length(BinPhaseDef(i).Grps(j).SigmaSF)
                    fprintf(fid,'%.4f\t',BinPhaseDef(i).Grps(j).SigmaSF(n1));
                end
                fprintf(fid,'\n');

                for n1=1:length(BinPhaseDef(i).Grps(j).SFLabels)
                    fprintf(fid,'%s\t',BinPhaseDef(i).Grps(j).SFLabels{n1});
                end
                fprintf(fid,'\n');

                
            end
        end   
    end
    fclose(fid);
end

return


% #########################################################################
%       BIN - Button to LOAD the Groups (1.2.1)
function BinButtonLoadGrp_Callback(hObject, eventdata, handles)
% 
ImportLoadGrp(1, hObject, eventdata, handles);
return


% #########################################################################
%       BIN - Button to LOAD or IMPORT the Groups (1.3.1)
function BinButtonImportGrp_Callback(hObject, eventdata, handles)
% 
ImportLoadGrp(2, hObject, eventdata, handles);
return


% #########################################################################
%       BIN - Function to LOAD or IMPORT the Groups (1.3.1)
function ImportLoadGrp(Wtd, hObject, eventdata, handles)
%

% LOAD      (Wtd = 1)  % This does not exist anymore...
% IMPORT    (Wtd = 2)

InitPanelDisp(1,handles);

[Success,~,MessageID] = mkdir('Bingo');

Databases = get(handles.BinPopUpDatabase,'String');
NameDatabase = Databases{get(handles.BinPopUpDatabase,'Value')};
filename = ['BIN_Phases_',NameDatabase,'_.xtt'];

cd Bingo
if isequal(exist(filename,'file'),2)
    switch Wtd
    	case 1   % Not used in the new version
            ButtonName = questdlg(['Would you like to use ',filename,'?'],'Load Phase Definitions','Yes');  
            switch ButtonName
                case 'Yes'
                    Question = 0;
                    filename = filename';
                otherwise
                    Question = 1;
            end
            
        case 2
            ButtonName = questdlg(['Would you like to loead ',filename,'?'],'Load/Import Phase Definitions','Yes (load this file)','No (Import a file)','Cancel','Yes (load this file)');  
            switch ButtonName
                case 'Yes (load this file)'
                    Wtd = 1;
                    Question = 0;
                case 'No (Import a file)'
                    Question = 1;
                otherwise
                    cd ..
                    return
            end
    end
else
    Question = 1;
end

if Question
    [filename, pathname] = uigetfile({'*.xtt', 'XThermoTools File (*.xtt)'},'Pick a file');

    if ~filename
        cd ..
        return
    end

    fid = fopen([pathname,filename],'r');
else
    fid = fopen([filename],'r');
end


Compt = 0;
while 1
    % New block
    Compt=Compt+1;
    TheL = fgetl(fid);
    
    if isequal(TheL,'') || isequal(TheL,-1)
        break
    end
     
    TheL = strread(TheL,'%s');
    BinPhaseDef(Compt).IsPhaseIn = str2num(TheL{2});
    
    TheL = fgetl(fid);
    BinPhaseDef(Compt).name = TheL;
    
    if BinPhaseDef(Compt).IsPhaseIn
        TheL = fgetl(fid);
        BinPhaseDef(Compt).OxBasis = str2double(TheL);
        
        TheL = fgetl(fid);
        BinPhaseDef(Compt).ListVariAntidote = strread(TheL,'%s')';
        
        TheL = fgetl(fid);
        BinPhaseDef(Compt).ListWeightAntidote = str2double(strread(TheL,'%s'))';
        
        TheL = fgetl(fid);
        BinPhaseDef(Compt).DBName = TheL;
        
        TheL = fgetl(fid);
        BinPhaseDef(Compt).DBMinName = TheL;
        
        TheL = fgetl(fid);
        BinPhaseDef(Compt).PhaseVolProp = str2double(TheL); % or zero?
        
        TheL = fgetl(fid);
        BinPhaseDef(Compt).NbGrps = str2double(TheL);
        
        TheL = fgetl(fid);
        BinPhaseDef(Compt).SelGrps = str2double(TheL);
        
        TheL = fgetl(fid);
        if BinPhaseDef(Compt).NbGrps > 1
            BinPhaseDef(Compt).VolGrps = str2double(strread(TheL,'%s'));
        else
            BinPhaseDef(Compt).VolGrps = str2double(TheL);
        end
        
        for iG=1:BinPhaseDef(Compt).NbGrps
            
            TheL = fgetl(fid);
            TheL = strread(TheL,'%s');
            
            BinPhaseDef(Compt).Grps(iG).IsGrp = str2double(TheL{2});
            
            TheL = fgetl(fid);
            BinPhaseDef(Compt).Grps(iG).Name = TheL;
            
            TheL1 = fgetl(fid);
            TheL2 = fgetl(fid);
            
            BinPhaseDef(Compt).Grps(iG).XrefYref = [str2double(strread(TheL1,'%s')),str2double(strread(TheL2,'%s'))];

            TheL = fgetl(fid);
            BinPhaseDef(Compt).Grps(iG).OxideCompositions = str2double(strread(TheL,'%s'))';
            
            TheL = fgetl(fid);
            BinPhaseDef(Compt).Grps(iG).SigmaOxide = str2double(strread(TheL,'%s'))';
            
            TheL = fgetl(fid);
            BinPhaseDef(Compt).Grps(iG).OxideLabels = strread(TheL,'%s')';
            
            TheL = fgetl(fid);
            BinPhaseDef(Compt).Grps(iG).SFCompositions = str2double(strread(TheL,'%s'))';
            
            TheL = fgetl(fid);
            BinPhaseDef(Compt).Grps(iG).SigmaSF = str2double(strread(TheL,'%s'))';
            
            TheL = fgetl(fid);
            BinPhaseDef(Compt).Grps(iG).SFLabels = strread(TheL,'%s')';
        end
    else
        BinPhaseDef(Compt).OxBasis = [];
        BinPhaseDef(Compt).DBName = '';
        BinPhaseDef(Compt).DBMinName= '';
        BinPhaseDef(Compt).PhaseVolProp = [];
        BinPhaseDef(Compt).SelGrps = 0;
        BinPhaseDef(Compt).VolGrps = [];
        BinPhaseDef(Compt).Grps(1).IsGrp = 0;
        BinPhaseDef(Compt).Grps(1).Name = '';
        BinPhaseDef(Compt).Grps(1).XrefYref = []; 
        BinPhaseDef(Compt).Grps(1).OxideCompositions = [];
        BinPhaseDef(Compt).Grps(1).SigmaOxide = [];
        BinPhaseDef(Compt).Grps(1).OxideLabels = '';
        BinPhaseDef(Compt).Grps(1).SFCompositions = [];
        BinPhaseDef(Compt).Grps(1).SigmaSF = [];
        BinPhaseDef(Compt).Grps(1).SFLabels = '';
    end
end    
fclose(fid);    
cd ..



[BinPhaseDef] = UpdateBinPhaseDef(BinPhaseDef,handles);

% end

% Update the menu
for i=1:length(BinPhaseDef)
    ListNames{i} = BinPhaseDef(i).name;
end
set(handles.BinPopUpPhaseList,'String',ListNames,'Value',1);

if isequal(BinPhaseDef(1).Grps(1).IsGrp,1)
    for i=1:length(BinPhaseDef(1).Grps)
        if isequal(BinPhaseDef(1).Grps(i).IsGrp,1)
            GrpList{i} = BinPhaseDef(1).Grps(i).Name;
        end
    end
else
    GrpList = {''};
end

set(handles.BinPopUpGrpOfPhase,'String',GrpList,'Value',1);

handles.BinPhaseDef = BinPhaseDef;
guidata(hObject, handles);

% This should work (Don't update but clac again...)
%
%BinPopUpPhaseList_Callback(hObject, eventredata, handles);
BinButtonCalcPhase_Callback(hObject, eventdata, handles);
return




function [BinPhaseDef] = UpdateBinPhaseDef(BinPhaseDef,handles)
%

Databases = get(handles.BinPopUpDatabase,'String');
NameDatabase = Databases{get(handles.BinPopUpDatabase,'Value')};

BingoDefault = handles.BingoDefault;

iDB = BingoDefault.SelectedDatabase;

switch BingoDefault.SelectedProgram
    case 1
        [DefMin,DefGf] = ReadMinTrans(BingoDefault.Theriak.Database(BingoDefault.SelectedDatabase).MinTrans,BingoDefault.Theriak.Database(BingoDefault.SelectedDatabase).PathForDB);
        %DefMin = handles.BingoDefault.Theriak.Database(iDB).DefMin;
    case 2
        [DefMin,DefGf] = ReadMinTrans(BingoDefault.PerpleX.Database(BingoDefault.SelectedDatabase).MinTrans);
        %DefMin = handles.BingoDefault.PerpleX.Database(iDB).DefMin;
end

for i=1:length(DefMin(:,1))
    ListRefMiner{i} = DefMin{i,1};
end
ListRefMiner = ListRefMiner';

% switch Wtd
%     case {1,2}
for i=1:length(BinPhaseDef)
    
    if BinPhaseDef(i).IsPhaseIn
        
        [YesIs,WereIsPhase] = ismember(upper(BinPhaseDef(i).name),ListRefMiner);
        
        %keyboard
        
        if ~YesIs
            [WereIsPhase,OK] = listdlg('ListString',ListRefMiner,'SelectionMode','single','PromptString',{'Select the model for the phase: ',BinPhaseDef(i).name});
            if ~OK
                return
            end
        end
        
        BinPhaseDef(i).DBName = NameDatabase;
        
        BinPhaseDef(i).OxBasis = DefMin{WereIsPhase,2};
        BinPhaseDef(i).ListVariAntidote = DefMin{WereIsPhase,4};
        BinPhaseDef(i).ListWeightAntidote = []; %DefMin{WereIsPhase,5};   % PL ** 03.03.19
        BinPhaseDef(i).DBMinName = DefMin{WereIsPhase,3};
    end
    
end
% end 



% ButtonName = questdlg('Would you like to recalculate the compositions?','Load Compositions','Yes');  
% switch ButtonName
%     case 'Yes'
%         Question = 1;
%     otherwise
%         Question = 0;
% end

OldBinPhaseDef = BinPhaseDef;

% switch Question
%     case 1

% --------------------------------------------------------
% Import required variables
BinElData = handles.BinElData;
MapWindow = handles.MapWindow;

% Compositions (at the right format)
[LinS,ColS] = size(MapWindow.Mask.DensityMap);
TheMapCoord = reshape([1:LinS*ColS],LinS,ColS);

for iPhase = 1:length(BinPhaseDef)
    for iGrp = 1:length(BinPhaseDef(iPhase).Grps)
        
        if BinPhaseDef(iPhase).Grps(iGrp).IsGrp
            
            h = BinPhaseDef(iPhase).Grps(iGrp).XrefYref;
            
            SelectedMask = iPhase;
            MaskMap = MapWindow.Mask.Data;
            
            MaskLayer = zeros(size(MapWindow.Maps.Data(1).ValuesOx));
            WhichOnesPx = find(MaskMap(:) == SelectedMask);
            MaskLayer(WhichOnesPx) = ones(size(WhichOnesPx));
            
            OxideSum=zeros(size(MapWindow.Mask.DensityMap));
            ListElem = MapWindow.Maps.ListMaps;
            
            [nPixV, nPixH] = size(MapWindow.Maps.Data(1).ValuesOx);
            Chemic = zeros(nPixV*nPixH,length(ListElem));
            
            for i=1:length(ListElem)
                OxideSum = OxideSum + MapWindow.Maps.Data(i).ValuesOx;
                Chemic(:,i) = MapWindow.Maps.Data(i).ValuesOx(:).*MaskLayer(:);
            end
            
            switch size(h,1)
                case 1
                    SelPixels = round(h(2)-1)*LinS+round(h(1));
                    SelPixels = TheMapCoord(round(h(2)),round(h(1)));
                    
                    if ~isequal(MaskMap(SelPixels),SelectedMask)
                        SelPixels = [];
                    end
                    
                otherwise
                    MaskSel = Xpoly2maskX(h(:,1),h(:,2),LinS,ColS);
                    
                    SelPixels = find(MaskMap(:) == SelectedMask & MaskSel(:) == 1);
            end
            
            if isempty(SelPixels)
                keyboard
                warndlg(['The selection does not belong to ',char(MapWindow.Mask.ListMin(SelectedMask))],'Warning');
                return
            end
            
            SelectedMaps = find(BinElData.selected);
            
            % - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            % Below is the new version with uncertainties... (PL - 28.05.2017)
            
            SelectedPhase = iPhase;
            
            disp(' ')
            disp(['Mineral: ',BinPhaseDef(SelectedPhase).name, ' || Grp: ',num2str(iGrp)])
            disp(' ')
            
            for i=1:length(SelectedMaps)
                
                % With outlier rejection                                (PL - 18.05.18)
                SelData = MapWindow.Maps.Data(SelectedMaps(i)).ValuesOx(SelPixels);
                
                NbPixels = length(SelData);
                Average = mean(SelData);
                Sigma6 = 6*std(SelData);
                Outliers = find(SelData > Average+Sigma6 | SelData < Average-Sigma6);
                NotOutliers = find(SelData < Average+Sigma6 | SelData > Average-Sigma6);
                
                NbOutliers = length(Outliers);
                
                if NbOutliers/NbPixels < 0.05 && NbOutliers/NbPixels  > 0
                    disp(sprintf('\t%s\t%s',char(BinElData.listElem{SelectedMaps(i)}),[num2str(NbOutliers),'/',num2str(NbPixels),' (',num2str(NbOutliers/NbPixels*100),' %) of the pixel(s) rejected (6 sigma test, 5% cut-off threshold)']))
                    SelData = SelData(NotOutliers);
                else
                    disp(sprintf('\t%s\t%s',[char(BinElData.listElem{SelectedMaps(i)}),'*'],[num2str(NbOutliers),'/',num2str(NbPixels),' (',num2str(NbOutliers/NbPixels*100),' %) of the pixels are apparently outliers (6 sigma test, 5% cut-off threshold) - ALL PIXELS SELECTED']))
                end
                
                CompositionOxide(i) = mean(SelData);
                SigmaOxide(i) = std(SelData);
                OxideLabels{i} = BinElData.listOxides{SelectedMaps(i)};
                SFLabels{i} = BinElData.listElem{SelectedMaps(i)};
                MolarMass(i) = BinElData.MolarMass(SelectedMaps(i));
                NbAtoms(i) = BinElData.NbAtoms(SelectedMaps(i));
                NbOxygen(i) = BinElData.NbOxygen(SelectedMaps(i));
                
                if isnan(SigmaOxide(i))
                    disp('This shouldn''t have happened, please contact pierre.lanari@geo.unibe.ch (ERR1628)')
                    disp('type ''return'' to continue')
                    keyboard
                end
            end
            
            %                     for i=1:length(SelectedMaps)
            %                         CompositionOxide(i) = mean(MapWindow.Maps.Data(SelectedMaps(i)).ValuesOx(SelPixels));
            %                         OxideLabels{i} = BinElData.listOxides{SelectedMaps(i)};
            %                         SFLabels{i} = BinElData.listElem{SelectedMaps(i)};
            %                         MolarMass(i) = BinElData.MolarMass(SelectedMaps(i));
            %                         NbAtoms(i) = BinElData.NbAtoms(SelectedMaps(i));
            %                         NbOxygen(i) = BinElData.NbOxygen(SelectedMaps(i));
            %                     end
            
            
            [SFCompositions,SigmaSF] = BinStructForm(CompositionOxide,SigmaOxide,MolarMass,NbAtoms,NbOxygen,BinPhaseDef(SelectedPhase).OxBasis);
            
            disp(' ')
            
            h = sprintf('\t');
            for n1=1:length(OxideLabels)
                h = [h,sprintf('%s\t',char(OxideLabels{n1}))];
            end
            disp(h)
            h = sprintf('\t');
            for n1=1:length(CompositionOxide)
                h = [h,sprintf('%.2f\t',CompositionOxide(n1))];
            end
            disp(h)
            h = sprintf('\t');
            for n1=1:length(SigmaOxide)
                h = [h,sprintf('%.2f\t',SigmaOxide(n1))];
            end
            disp(h)
            disp(' ')
            h = sprintf('\t');
            for n1=1:length(SFLabels)
                h = [h,sprintf('%s\t',char(SFLabels{n1}))];
            end
            disp(h)
            h = sprintf('\t');
            for n1=1:length(SFCompositions)
                h = [h,sprintf('%.2f\t',SFCompositions(n1))];
            end
            disp(h)
            h = sprintf('\t');
            for n1=1:length(SigmaSF)
                h = [h,sprintf('%.2f\t',SigmaSF(n1))];
            end
            disp(h)
            
            BinPhaseDef(SelectedPhase).Grps(iGrp).OxideCompositions = CompositionOxide;
            BinPhaseDef(SelectedPhase).Grps(iGrp).SigmaOxide = SigmaOxide;
            BinPhaseDef(SelectedPhase).Grps(iGrp).OxideLabels = OxideLabels;
            BinPhaseDef(SelectedPhase).Grps(iGrp).SFCompositions = SFCompositions;
            BinPhaseDef(SelectedPhase).Grps(iGrp).SigmaSF = SigmaSF;
            BinPhaseDef(SelectedPhase).Grps(iGrp).SFLabels = SFLabels;
            
            %                     for i=1:length(SelectedMaps)
            %                         CompositionOxide(i) = mean(MapWindow.Maps.Data(SelectedMaps(i)).ValuesOx(SelPixels));
            %                         OxideLabels{i} = BinElData.listOxides{SelectedMaps(i)};
            %                         SFLabels{i} = BinElData.listElem{SelectedMaps(i)};
            %                         MolarMass(i) = BinElData.MolarMass(SelectedMaps(i));
            %                         NbAtoms(i) = BinElData.NbAtoms(SelectedMaps(i));
            %                         NbOxygen(i) = BinElData.NbOxygen(SelectedMaps(i));
            %                     end
            %
            %                     SFCompositions = BinStructForm(CompositionOxide,MolarMass,NbAtoms,NbOxygen,BinPhaseDef(iPhase).OxBasis);
            %
            %                     BinPhaseDef(iPhase).Grps(iGrp).OxideCompositions = CompositionOxide;
            %                     BinPhaseDef(iPhase).Grps(iGrp).OxideLabels = OxideLabels;
            %                     BinPhaseDef(iPhase).Grps(iGrp).SFCompositions = SFCompositions;
            %                     BinPhaseDef(iPhase).Grps(iGrp).SFLabels = SFLabels;
        end
        
    end
end

return


% 
function [] = OldVersion()
% % #########################################################################
% %       BIN - Quality: Assemblage
% function [Result,Link,Report] = Bingo_Qasm(WorkVariMod,WorkVariXMap,Report,DoWePrint,handles)
% % Bingo_Qasm is a function to estimate the Evaluation of the  
% % model for the stable assemblage.
% %
% % To be described later once the strategy is set.
% % 
% % Last change by Pierre Lanari (30.11.2015)
% %
% 
% PhasesTher = WorkVariMod.Names;
% PhasesXMap = WorkVariXMap.Names;
% 
% % Generate the variable LINK to be used later
% ComptPhase=0;
% Link.PhasesNames = {''};
% for i=1:length(PhasesTher)
%     if ~ismember(Link.PhasesNames,PhasesTher{i})
%         ComptPhase = ComptPhase+1;
%         Link.PhasesNames{ComptPhase} = PhasesTher{i};
%     end
% end
% for i=1:length(PhasesXMap)
%     if ~ismember(Link.PhasesNames,PhasesXMap{i})
%         ComptPhase = ComptPhase+1;
%         Link.PhasesNames{ComptPhase} = PhasesXMap{i};
%     end
% end
% 
% [Link.TherIsIn,Link.TherIndices] = ismember(Link.PhasesNames,PhasesTher);
% [Link.XMapIsIn,Link.XMapIndices] = ismember(Link.PhasesNames,PhasesXMap);
% 
% NbPhasesInvolved = length(Link.TherIsIn);
% NbMissingPhases = 2*NbPhasesInvolved-(length(find(Link.TherIsIn))+length(find(Link.XMapIsIn)));
% 
% NbPhasesMax = max([length(PhasesTher),length(PhasesXMap)]);
% NbMatches = length(find(ismember(PhasesTher,PhasesXMap)));
% 
% % Preliminary version 
% %Result = (NbPhasesInvolved-NbMissingPhases)/NbPhasesInvolved*100; % not so good
% %Result =  NbMatches/NbPhasesMax*100;% Eq (2.2)
% 
% % eduester: Search for <1% mineral phases
% %VolTherMatch(find(Link.TherIsIn)) = WorkVariMod.VolFrac(Link.TherIndices(find(Link.TherIndices)));
% %NbPhasesInvolved2=NbPhasesInvolved-sum(VolTherMatch(Link.TherIndices(find(Link.XMapIsIn==0)))<0.01);
% 
% % eduester: new equation
% %Result =  NbMatches/NbPhasesInvolved2*100;% Eq (2.1)
% 
% % lanari equation
% Result =  NbMatches/NbPhasesInvolved*100;% Eq (2.1)
% 
% if DoWePrint
%     disp(' ')
%     Code1 = '%s\t\t';
%     Code2 = '%s\t\t';
%     for i=1:length(Link.PhasesNames)
%         Len = length(Link.PhasesNames{i});
%         if Len < 8
%             for j=Len:8
%                 Link.PhasesNames{i}=[char(Link.PhasesNames{i}),' '];
%             end
%         end    
%         Code1=[Code1,'%s\t'];
%         Code2=[Code2,'%f\t'];
%     end
%     Code1(end) = 'n';
%     Code2(end) = 'n';
%     
%     fprintf('%s\n','##### Evaluation criterion (1) ASSEMBLAGE ##### ');
%     fprintf(Code1,'Phases:',Link.PhasesNames{:})
%     fprintf(Code2,'THER:',Link.TherIsIn);
%     fprintf(Code2,'XMAP:',Link.XMapIsIn);
%     fprintf('%s\n','  ');
%     fprintf('%s\n','-------------');
%     fprintf('%s\t%.0f\n','n =',length(PhasesTher));
%     fprintf('%s\t%.0f\n','m =',length(PhasesXMap));
%     fprintf('%s\t%.0f\n','l =',NbMatches);
%     fprintf('%s\t%.2f\n','Qass =',Result);  
%     fprintf('%s\n','-------------');
%     
%     Report.Phases = Link.PhasesNames;
%     Report.PhasesXMap = Link.XMapIsIn;
%     Report.PhasesTher = Link.TherIsIn;
%     
%     %keyboard
% end
% 
% 
% return
% 
% 
% % #########################################################################
% %       BIN - Quality: Modes
% function [Evaluation,Report] = Bingo_Qvol(WorkVariMod,WorkVariXMap,Link,Evaluation,Report,DoWePrint,handles)
% % ComputeVolumeAssemblage is a function to estimate the Evaluation of the  
% % model for the volume of stable phases.
% %
% % To be described later once the strategy is set.
% % 
% % Last change by Pierre Lanari (30.11.2015)
% %
% 
% VolTherMatch = zeros(size(Link.PhasesNames));
% VolXMapMatch = zeros(size(Link.PhasesNames));
% 
% VolTherMatch(find(Link.TherIsIn)) = WorkVariMod.VolFrac(Link.TherIndices(find(Link.TherIndices)));
% VolXMapMatch(find(Link.XMapIsIn)) = WorkVariXMap.VolFrac(Link.XMapIndices(find(Link.XMapIndices)));
% 
% VolTherMatchNorm = VolTherMatch ./ sum(VolTherMatch);
% VolXMapMatchNorm = VolXMapMatch ./ sum(VolXMapMatch);
% 
% % 2.1 Very simple comparison of volume fractions
% %Result_Diff = 100-sum(abs(VolTherMatch-VolXMapMatch)*100/2)
% 
% % eduester: new equation
% Result_Diff = 100.*sqrt(sum((VolTherMatch+VolXMapMatch)/2.*(1-abs(VolTherMatch-VolXMapMatch)./(VolTherMatch+VolXMapMatch)).^2));
% %keyboard
% E_Minim1 = sum(sqrt((VolTherMatch-VolXMapMatch).^2));
% 
% Evaluation.Volume = Result_Diff;
% 
% if DoWePrint
%     disp(' ')
%     Code1 = '%s\t\t';
%     Code2 = '%s\t\t';
%     for i=1:length(Link.PhasesNames)
%         Len = length(Link.PhasesNames{i});
%         if Len < 8
%             for j=Len:8
%                 Link.PhasesNames{i}=[char(Link.PhasesNames{i}),' '];
%             end
%         end    
%         Code1=[Code1,'%s\t'];
%         Code2=[Code2,'%f\t'];
%     end
%     Code1(end) = 'n';
%     Code2(end) = 'n'; 
%     
%     disp(' ')
%     fprintf('%s\n','##### Evaluation criterion (2) VOLUME FRACTIONS ##### ');
%     fprintf(Code1,'Phases:',Link.PhasesNames{:})
%     fprintf(Code2,'THER:',VolTherMatch);
%     fprintf(Code2,'XMAP:',VolXMapMatch);
%     fprintf(Code2,'abs(D):',abs(VolTherMatch-VolXMapMatch));
%     fprintf('%s\n','  ');
%     fprintf('%s\n','-------------');
%     fprintf('%s\t%.2f\n','Qvol =',Result_Diff);  
%     fprintf('%s\n','-------------');
%     
%     
%     % Replace the names
%     TheNamesPlot = Link.PhasesNames;
%     for i=1:length(TheNamesPlot)
%         TempStr = char(TheNamesPlot{i});
%         for j=1:length(TempStr)
%             if isequal(TempStr(j),'_')
%                 TempStr(j) = ' ';
%             end
%         end
%         if VolTherMatch(i) > 0
%             TheNamesPlotTher{i} = TempStr;
%         else
%             TheNamesPlotTher{i} = ' ';
%         end
%         
%         if VolXMapMatch(i) > 0
%             TheNamesPlotMap{i} = TempStr;
%         else
%             TheNamesPlotMap{i} = ' ';
%         end
%     end
%     
%     Report.TheNamesPlotMap = TheNamesPlotMap;
%     Report.TheNamesPlotTher = TheNamesPlotTher;
%     
%     Report.VolXMap = VolXMapMatch*100;
%     Report.VolTher = VolTherMatch*100;
%             
% end
% 
% return
% 
% 
% % #########################################################################
% %       BIN - Quality: Cmp
% function [Evaluation,Report] = Bingo_Qcmp(WorkVariMod,WorkVariXMap,Link,Evaluation,Report,DoWePrint,handles)
% % Bingo_Qcmp is a function to estimate the Evaluation of the  
% % model for the composition of stable phases.
% %
% % To be described later once the strategy is set.
% % 
% % Last change by Pierre Lanari (28.05.2018)
% %
% 
% % CutOffThreshold (apfu value)
% CutOffThreshold = 0.005;
% 
% NbElemsTher = WorkVariMod.NbEl;   
% ElemsListTher = WorkVariMod.Els;
% 
% NbElemsXMap = WorkVariXMap.NbEl;
% ElemsListXMap = WorkVariXMap.Els;
% 
% NbElems = NbElemsTher;
% ElemsList = ElemsListTher;
% 
% [Ok,WhereIsMem] = ismember(ElemsListTher,ElemsListXMap);
% 
% WorkVariXMap.COMPok = zeros(WorkVariXMap.NbPhases,NbElemsTher);
% WorkVariXMap.COMPok(:,find(Ok)) = WorkVariXMap.COMP(:,WhereIsMem(find(Ok)));
% 
% WorkVariXMap.UNCok = zeros(WorkVariXMap.NbPhases,NbElemsTher);
% WorkVariXMap.UNCok(:,find(Ok)) = WorkVariXMap.UNC(:,WhereIsMem(find(Ok)));
% 
% 
% if DoWePrint
%     disp(' ')
%     Code1 = '%s\t\t';
%     Code2 = '%s\t\t';
%     for i=1:NbElems-1    
%         Code1=[Code1,'%s\t\t'];
%         Code2=[Code2,'%f\t'];
%     end
%     Code1(end) = 'n';
%     Code2(end) = 'n';
%     
%     disp(' ')
%     fprintf('%s\n','##### Evaluation criterion (3) PHASE COMPOSITIONS ##### ');    
% end
% 
% 
% % Correction of O for H in the structural formula:
% WhereH = find(ismember(ElemsList,'H'));
% if WhereH
%     TheHValues = WorkVariMod.COMP(:,WhereH);
%     WorkVariMod.COMP(1:WorkVariMod.NbPhases,WhereH) = zeros(WorkVariMod.NbPhases,1);
% else
%     TheHValues = 0;
% end
% 
% WhereO = find(ismember(ElemsList,'O'));
% if WhereO
%     WorkVariMod.COMP(1:WorkVariMod.NbPhases,WhereO) = WorkVariMod.COMP(1:WorkVariMod.NbPhases,WhereO)-0.5*TheHValues;
% end
% 
% CompTherMatch = zeros(size(Link.PhasesNames,2),NbElems);
% CompXMapMatch = zeros(size(Link.PhasesNames,2),NbElems);
% CompXMapUnc1s = zeros(size(Link.PhasesNames,2),NbElems);
% 
% if size(WorkVariMod.COMP,1) < 2
%     keyboard
% end 
% 
% CompTherMatch(find(Link.TherIsIn),:) = WorkVariMod.COMP(Link.TherIndices(find(Link.TherIndices)),:);
% CompXMapMatch(find(Link.XMapIsIn),:) = WorkVariXMap.COMPok(Link.XMapIndices(find(Link.XMapIndices)),:);
% 
% CompXMapUnc1s(find(Link.XMapIsIn),:) = WorkVariXMap.UNCok(Link.XMapIndices(find(Link.XMapIndices)),:);
% 
% % Check if O(anhydrous) is the same in both      (PL 05.03.19)
% DoWeHaveZeroElem = 0;
% for i = 1:size(CompTherMatch,1)
%     if CompTherMatch(i,WhereO) > 0 && CompXMapMatch(i,WhereO) && ~isequal(CompTherMatch(i,WhereO),CompXMapMatch(i,WhereO))
%         CorrFactor = CompTherMatch(i,WhereO)/CompXMapMatch(i,WhereO);
%         if DoWePrint
%             fprintf('\n%s\t\t%s\n','*Warning*',['SF renormalized for phase: ',char(Link.PhasesNames{i}),' (O was XMap[',num2str(CompXMapMatch(i,WhereO)),'] ;Model[',num2str(CompTherMatch(i,WhereO)),'])']);
%         end
%         CompXMapMatch(i,:) = CompXMapMatch(i,:)*CorrFactor;
%         DoWeHaveZeroElem = DoWeHaveZeroElem+1;
%     end
% end
% if DoWeHaveZeroElem > 0 && DoWePrint
%     fprintf('\n');
% end
% % 3.2 WEIGHTED Cmp with UNC               (28 May 2018) ** Version 8 **  PL
% 
% 
% % Perspective Version for Lanari & Duesterhoeft (2019)
% %
% %   - Meth = 1          Erik2 using only UNC (no TOL) and an unweighted
% %                       mean method
% %                       I added an option not to have Unc1s < 0.01 apfu
% %
% %
% % ## Same potential issue as before: ##
% % Ok, the way this part is coded is not optimal...
% % The variable BinPhaseDef is not directly sent to this function, it is
% % taken from the handles. Of course it works fine, but if handles doesn't
% % come for any reason or future update, it will not work anymore.
% %
% % PL (28.05.2018) -
% %
% %
% % ## ##
% % I udpated this function to extract the compositions from
% % CompXMapMatch and CompTherMatch (see just above)
% % It is not so clear why we haven't done that since the begining..
% %
% % PL (05.03.2019)
% 
% 
% BinPhaseDef = handles.BinPhaseDef;
% for i=1:length(BinPhaseDef)
%     NameMinBinDef{i} = BinPhaseDef(i).DBMinName;
% end
% 
% for i=1:length(Link.PhasesNames)
%     NameRaw = Link.PhasesNames{i};
%     NameRawStr = strread(NameRaw,'%s');
%     if iscell(NameRawStr)
%         NamePhasesMatch{i} = NameRawStr{1};
%     else
%         NamePhasesMatch{i} = NameRaw;
%     end
% end
% 
% Qual = zeros(size(CompTherMatch,1),1);
% Missfit = zeros(size(CompTherMatch,1),1);
% weight = zeros(size(CompTherMatch,1),1);
% 
% %ElemsList = WorkVariMod.Els;
% 
% IsCompared = zeros(size(CompTherMatch,1),1);
% 
% for i=1:size(CompTherMatch,1)
%     
%     NamePhasesModel = NamePhasesMatch{i};
%     
%     NbAtXMap = sum(CompXMapMatch(i,2:end-1)); % I excluded oxygen (2:end) and E (end)
%     NbAtTher = sum(CompTherMatch(i,2:end-1)); % and E (end)
%     
%     if NbAtXMap > 0 && NbAtTher > 0
%         
%         IsCompared(i) = 1;
%         
%         % Extract the element list (to be compared) from BinPhaseDef
%         WhereInBinDef = find(ismember(NameMinBinDef,NamePhasesModel));
%         
%         ElsSel = BinPhaseDef(WhereInBinDef).ListVariAntidote;
%         %WeightSel = BinPhaseDef(WhereInBinDef).ListWeightAntidote;
%         
%         [Yes,WhereElemInList] = ismember(ElsSel,ElemsList);
%         %[Yes1,WhereElemInMod] = ismember(ElsSel,WorkVariMod.Els);
%         %[Yes2,WhereElemInXMap] = ismember(ElsSel,WorkVariXMap.Els);
%         
%         % PL 03.03.19
%         % Special case if the element is not defined in the bulk (zero for map and model)
%         % Otherwise the average is bad
%         KeepElement = find(WhereElemInList ~= 0);
%         SkipElement = find(WhereElemInList == 0);
%         %KeepElement = find(WhereElemInMod ~= 0 | WhereElemInXMap ~= 0);
%         %SkipElement = find(WhereElemInMod == 0 & WhereElemInXMap == 0);
%         
%         CompoMod = zeros(size(WhereElemInList));
%         CompoXMap = zeros(size(WhereElemInList));
%         Unc1s = zeros(size(WhereElemInList));
%         
%         CompoMod(find(WhereElemInList)) = CompTherMatch(i,WhereElemInList(find(WhereElemInList)));
%         CompoXMap(find(WhereElemInList)) = CompXMapMatch(i,WhereElemInList(find(WhereElemInList)));
%         Unc1s(find(WhereElemInList)) = CompXMapUnc1s(i,WhereElemInList(find(WhereElemInList)));
%         
%         %CompoMod(find(WhereElemInMod)) = WorkVariMod.COMP(Link.TherIndices(i),WhereElemInMod(find(WhereElemInMod)));
%         %CompoXMap(find(WhereElemInXMap)) = WorkVariXMap.COMP(Link.XMapIndices(i),WhereElemInXMap(find(WhereElemInXMap)));
%         %Unc1s(find(WhereElemInXMap)) = WorkVariXMap.UNC(Link.XMapIndices(i),WhereElemInXMap(find(WhereElemInXMap)));
%         
%         %if ~isequal(length(CompoMod),length(CompoXMap)) % ,length(WeightSel))  PL ** 03.03.19
%         %    disp('Oups something went wrong in Bingo_Qcmp')
%         %    keyboard
%         %end
%         
%         DIFFabs = abs(CompoMod-CompoXMap);
%         
%         % This is Meth 1
%         
%         Fac1 = 1;  % plateau at 1 sigma
%         Fac2 = 5;  % NOTE: the zero is at Fac1+Fac2 (6 sigma)!
%         
%         % Check for Unc < 0.01 and replace by 0.01                     (PL)
%         WhereTOOLOW = find(Unc1s > 0 & Unc1s <0.01);
%         if length(WhereTOOLOW)
%             Unc1s(WhereTOOLOW) = 0.01*ones(size(Unc1s(WhereTOOLOW)));
%         end
%         
%         DIFF2 = DIFFabs-(Unc1s/Fac1);
%         
%         WhereNEGZ = find(DIFF2<=0);
%         if length(WhereNEGZ)
%             DIFF2(WhereNEGZ) = zeros(size(WhereNEGZ));
%         end
%         WherePOSZ = find(DIFF2>Fac2*Unc1s);
%         if length(WherePOSZ)
%             DIFF2(WherePOSZ) = Fac2*Unc1s(WherePOSZ).*ones(size(WherePOSZ));
%         end
%         
%         
%         QUALsmall = 1*((Fac2*Unc1s-DIFF2)./(Fac2*Unc1s)).^(CompoMod+1);
%         
%         WhereNAN = find(isnan(QUALsmall));
%         if WhereNAN
%             % zero concentration in the map (must be zero
%             % quality)
%             QUALsmall(WhereNAN) = zeros(size(WhereNAN));
%         end
%         
%         Qual(i) = mean(QUALsmall(KeepElement)).*100;   % Changed PL 03.03.19 to add KeepElement
%         
%         % Display the element skipped! Addition by PL on 03.03.19:
%         ElsSel2 = ElsSel;
%         if length(SkipElement)
%             for j=1:length(SkipElement)
%                 ElsSel2{SkipElement(j)} = [char(ElsSel{SkipElement(j)}),'*'];
%             end
%         end
%         
%         if DoWePrint
%             fprintf('%s\n%s\n','-',char(Link.PhasesNames{i}));
%             fprintf(Code1,'Els:   ',ElemsList{1:end-1});
%             fprintf(Code2,'THER:  ',CompTherMatch(i,1:end-1));
%             fprintf(Code2,'XMAP:  ',CompXMapMatch(i,1:end-1));
%             fprintf(Code2,'abs(D):',abs(CompTherMatch(i,1:end-1)-CompXMapMatch(i,1:end-1)));
%             
%             disp(' ')
%             
%             CodeT1 = '%s\t\t';
%             CodeT2 = '%s\t\t';
%             for k=1:length(ElsSel2)
%                 CodeT1=[CodeT1,'%s\t\t'];
%                 CodeT2=[CodeT2,'%f\t'];
%             end
%             CodeT1(end) = 'n';
%             CodeT2(end) = 'n';
%             
%             fprintf(CodeT1,'Els:   ',ElsSel2{1:end})
%             %fprintf(CodeT2,'InTHER:',WhereElemInMod);         % TEMP
%             %fprintf(CodeT2,'InXMAP:',WhereElemInXMap);        % TEMP
%             fprintf(CodeT2,'THER:  ',CompoMod);
%             fprintf(CodeT2,'XMAP:  ',CompoXMap);
%             disp('_______')
%             fprintf(CodeT2,'UNC:   ',Unc1s);
%             fprintf(CodeT2,'DIFFab:',DIFFabs);
%             fprintf(CodeT2,'DIFF2: ',DIFF2);
%             disp('_______')
%             fprintf(CodeT2,'QUALs: ',QUALsmall);
%             %fprintf(CodeT2,'X(El): ',RelFract);
%             
%             disp(' ')
%             disp(['QUALITY = ',num2str(Qual(i)),' %'])
%             
%             disp(' ')
%         end
%         
%     else
%         
%         if DoWePrint
%             disp([' - - - Phase: ',char(NamePhasesModel),' has been skipped by BINGO - - - '])
%             disp(' ')
%         end
%         
%     end
% end
% 
% %E_Minim2 = sum(Missfit); % not used it seems
% 
% VolFracInModel = zeros(size(Qual));
% WherePhasesInModel = find(ismember(NamePhasesMatch,WorkVariMod.Names));
% 
% VolFracInModel(WherePhasesInModel) = WorkVariMod.VolFrac(WherePhasesInModel);
% 
% VolFracInModelCorr = zeros(size(VolFracInModel));
% WeTake = find(IsCompared);
% VolFracInModelCorr(WeTake) = VolFracInModel(WeTake)/sum(VolFracInModel(WeTake));
% 
% if sum(Qual)  > 0    
%     %Evaluation.Compositions = mean(Qual); % All of them 
%     %Evaluation.Compositions = mean(Qual(find(Qual))); % only the ones that matches. 
% 
%     Evaluation.Compositions = sum(Qual.*VolFracInModelCorr); % TEST PL (13.07.2018)
%     
%     %Evaluation.Compositions = sum(Qual.*VolFracInModel); % TEST PL (21.03.2019)
%     % I think that this is not a good option... 
%     
%     Evaluation.MinQ_TEMP = Qual;
%     Evaluation.MinQw_TEMP = Qual.*VolFracInModelCorr;
%     Evaluation.MinIsCompared_TEMP = IsCompared;
%     Evaluation.MinNames_TEMP = NamePhasesMatch;
%     
% else
%     Evaluation.Compositions = 0;
%     
%     Evaluation.MinQ_TEMP = 0;
%     Evaluation.MinQw_TEMP = 0;
%     Evaluation.MinIsCompared_TEMP = 0;
%     Evaluation.MinNames_TEMP = '';
% end
% 
% if DoWePrint
%     fprintf('\n%s\n%s\t\t','-------------','Phase');
%     
%     for i = 1:length(Evaluation.MinNames_TEMP)
%         NameMin = Evaluation.MinNames_TEMP{i};
%         if length(NameMin) > 6
%             fprintf('%s\t',NameMin(1:6));
%         else
%             fprintf('%s\t',NameMin);
%         end
%     end
%     fprintf('\n%s\t','Evaluated');
%     for i = 1:length(Evaluation.MinNames_TEMP)
%         if IsCompared(i)
%             fprintf('%s\t','Yes');
%         else
%             fprintf('%s\t','No');
%         end
%     end
%     fprintf('\n%s\t','Qual (%)');
%     for i = 1:length(Evaluation.MinNames_TEMP)
%         fprintf('%.2f\t',Qual(i));
%     end
%     fprintf('\n%s\t\t','v_norm');
%     for i = 1:length(Evaluation.MinNames_TEMP)
%         fprintf('%.2f\t',VolFracInModelCorr(i));
%     end
%     fprintf('\n%s\t\t','v');
%     for i = 1:length(Evaluation.MinNames_TEMP)
%         fprintf('%.2f\t',VolFracInModel(i));
%     end
%     fprintf('\n\n');
%     
%     % This is independent of the one selected below
%     fprintf('%s\t%.2f\n','Qcmp_vn =',sum(Qual.*VolFracInModelCorr)); 
%     fprintf('%s\t%.2f\n\n','Qcmp_v  =',sum(Qual.*VolFracInModel)); 
%     
%     fprintf('%s\t%.2f\n','Qcmp =',Evaluation.Compositions);  
%     fprintf('%s\n','-------------');
%     
%     Report.ElemsList = ElemsList;
%     Report.CmpXMap = CompXMapMatch;
%     Report.CmpTher = CompTherMatch;
% end
% 
% 
% 
% 
% % #########################################################################
% % Below the ancient Qcmp functions (not used anymore and not located at the
% % right location ... (PL 05.03.2019)
% 
% % - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
% % 2.1 Very simple Cmp comparison  (Version 1)
% if 0
%     Qual = zeros(size(CompTherMatch,1),1);
%     Missfit = zeros(size(CompTherMatch,1),1);
%     weight = zeros(size(CompTherMatch,1),1);
%     for i=1:size(CompTherMatch,1)
%         NbAtXMap = sum(CompXMapMatchCALC(i,2:end)); % I excluded oxygen (2:end)
%         NbAtTher = sum(CompTherMatch(i,2:end)); 
%         if NbAtXMap > 0 && NbAtTher > 0 
%             Diffs = sum(abs(CompTherMatch(i,:)-CompXMapMatchCALC(i,:)));
%             Qual(i) = (max([NbAtXMap,NbAtTher])-Diffs)/max([NbAtXMap,NbAtTher])*100;
%         end
% 
%         weight(i) = max([CompXMapMatchCALC(i,1),CompTherMatch(i,1)]);
%         Missfit(i) = sum(abs(CompTherMatch(i,:)-CompXMapMatchCALC(i,:)))*weight(i);
% 
%     end
% end
% 
% 
% 
% % - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
% % 2.2 Alternative Cmp comparison (PET2016 - Bern; 29 July 2016)(Version 2)
% if 0
%     Qual = zeros(size(CompTherMatch,1),1);
%     Missfit = zeros(size(CompTherMatch,1),1);
%     weight = zeros(size(CompTherMatch,1),1);
%     
%     for i=1:size(CompTherMatch,1)
%         NbAtXMap = sum(CompXMapMatchCALC(i,2:end-1)); % I excluded oxygen (2:end) and E (end)
%         NbAtTher = sum(CompTherMatch(i,2:end-1)); % and E (end)
%          
%         if NbAtXMap > 0 && NbAtTher > 0 
%             Diffs = abs(CompTherMatch(i,2:end-1)-CompXMapMatchCALC(i,2:end-1));
%             CompareRef = max([CompTherMatch(i,2:end-1);CompXMapMatchCALC(i,2:end-1)]);
%             
%             [MaxVal,MaxInd] = max(Diffs);
%             
%             QualAll = (max([NbAtXMap,NbAtTher])-Diffs)/max([NbAtXMap,NbAtTher]);
%             
%             if MaxVal
%                 QualLargest = (1 - (Diffs(MaxInd)./CompareRef(MaxInd)));
%             else
%                 QualLargest = 1;
%             end
% 
%             Qual(i) = mean(QualAll)*QualLargest*100;
%             
%             %Qual(i) = (max([NbAtXMap,NbAtTher])-Diffs)/max([NbAtXMap,NbAtTher])*100;
%         end
% 
%         weight(i) = max([CompXMapMatchCALC(i,1),CompTherMatch(i,1)]);
%         Missfit(i) = sum(abs(CompTherMatch(i,:)-CompXMapMatchCALC(i,:)))*weight(i);
% 
%     end
%     
% end
% 
% 
% 
% % - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
% % 2.3 Modified Alternative Cmp comparison (Bern; 18 August 2016) (Version 3)
% if 0
%     Qual = zeros(size(CompTherMatch,1),1);
%     Missfit = zeros(size(CompTherMatch,1),1);
%     weight = zeros(size(CompTherMatch,1),1);
%     
%     for i=1:size(CompTherMatch,1)
%         NbAtXMap = sum(CompXMapMatchCALC(i,2:end-1)); % I excluded oxygen (2:end) and E (end)
%         NbAtTher = sum(CompTherMatch(i,2:end-1)); % and E (end)
%          
%         if NbAtXMap > 0 && NbAtTher > 0 
%             Diffs = abs(CompTherMatch(i,2:end-1)-CompXMapMatchCALC(i,2:end-1));
%             CompareRef = max([CompTherMatch(i,2:end-1);CompXMapMatchCALC(i,2:end-1)]);
%             
%             [MaxVal,MaxInd] = max(Diffs);
%             
%             
%             [SortVal,SortInd] = sort(Diffs);
%             QualSort = (1 - (SortVal./CompareRef(SortInd)));
%             
%             WeUse = find(~isnan(QualSort));
%             
%             if length(WeUse)
%                 Qual(i) = mean(QualSort(WeUse))*100;
%             else
%                 Qual(i) = 100;
%             end
%             
% %             if MaxVal
% %                 QualLargest = (1 - (Diffs(MaxInd)./CompareRef(MaxInd)));
% %             else
% %                 QualLargest = 1;
% %             end
% % 
% %             Qual(i) = mean(QualAll)*QualLargest*100;
%             
%         end
% 
%         weight(i) = max([CompXMapMatchCALC(i,1),CompTherMatch(i,1)]);
%         Missfit(i) = sum(abs(CompTherMatch(i,:)-CompXMapMatchCALC(i,:)))*weight(i);
% 
%     end
%     
% end
% 
% 
% % - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
% % 2.3 Modified Alternative Cmp comparison (Kiel; 24 August 2016) (Version 4)
% 
% if 0
%     Qual = zeros(size(CompTherMatch,1),1);
%     Missfit = zeros(size(CompTherMatch,1),1);
%     weight = zeros(size(CompTherMatch,1),1);
%     
%     for i=1:size(CompTherMatch,1)
%         NbAtXMap = sum(CompXMapMatch(i,2:end-1)); % I excluded oxygen (2:end) and E (end)
%         NbAtTher = sum(CompTherMatch(i,2:end-1)); % and E (end)
%          
%         if NbAtXMap > 0 && NbAtTher > 0 
%             Diffs = abs(CompTherMatch(i,2:end-1)-CompXMapMatch(i,2:end-1));
%             CompareRef = max([CompTherMatch(i,2:end-1);CompXMapMatch(i,2:end-1)]);
%             
%             RefConcApfu = max([CompTherMatch(i,2:end-1);CompXMapMatch(i,2:end-1)]);
%             
%             SumDiff = sum(Diffs);
%             
%             Qij = zeros(size(Diffs));
%             Qwij = zeros(size(Diffs));
%             WhereCalc = find(Diffs+CompareRef > 0 & RefConcApfu > CutOffThreshold);
%             
%             Qij(WhereCalc) = 1-Diffs(WhereCalc)./CompareRef(WhereCalc);
%             Qwij(WhereCalc) = 1-Diffs(WhereCalc)./SumDiff;
%             
%             if find(Qij(WhereCalc).*Qwij(WhereCalc) > 0)
%                 Qual(i) = mean(Qij(WhereCalc).*Qwij(WhereCalc))*100;
%             else
%                 Qual(i) = 100;
%             end
%             
%         end
%         
%         weight(i) = max([CompXMapMatch(i,1),CompTherMatch(i,1)]);
%         Missfit(i) = sum(abs(CompTherMatch(i,:)-CompXMapMatch(i,:)))*weight(i);
% 
%     end
%     
% end
% 
% 
% 
% % - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
% % 2.4 Modified Alternative Cmp comparison (Kiel; 21 December 2016) (Version 6) eduester
% if 0
%     Qual = zeros(size(CompTherMatch,1),1);
%     Missfit = zeros(size(CompTherMatch,1),1);
%     weight = zeros(size(CompTherMatch,1),1);
%     
%     for i=1:size(CompTherMatch,1)
%         NbAtXMap = sum(CompXMapMatch(i,2:end-1)); % I excluded oxygen (2:end) and E (end)
%         NbAtTher = sum(CompTherMatch(i,2:end-1)); % and E (end)
%          
%         if NbAtXMap > 0 && NbAtTher > 0 
%             Diffs = abs(CompTherMatch(i,2:end-1)-CompXMapMatch(i,2:end-1));
%             CompareRef = max([CompTherMatch(i,2:end-1);CompXMapMatch(i,2:end-1)]);
%             
%             RefConcApfu = max([CompTherMatch(i,2:end-1);CompXMapMatch(i,2:end-1)]);
%             
%             SumDiff = sum(Diffs);
%             
%             Qij = zeros(size(Diffs));
%             Qwij = zeros(size(Diffs));
%             WhereCalc = find(Diffs+CompareRef > 0 & RefConcApfu > CutOffThreshold);
%             
%             Qij(WhereCalc) = 1-Diffs(WhereCalc)./CompareRef(WhereCalc);
%             Qwij(WhereCalc) = 1-Diffs(WhereCalc)./SumDiff;
%             
%             if find(Qij(WhereCalc).*Qwij(WhereCalc) > 0)
%                 %Qual(i) = mean(Qij(WhereCalc).*Qwij(WhereCalc))*100;
%                 Qual(i) = sum(Qij(WhereCalc).*(1./Qwij(WhereCalc)))/sum((1./Qwij(WhereCalc)))*100;
%             else
%                 Qual(i) = 100;
%             end
%             
%         end
%         
%         weight(i) = max([CompXMapMatch(i,1),CompTherMatch(i,1)]);
%         Missfit(i) = sum(abs(CompTherMatch(i,:)-CompXMapMatch(i,:)))*weight(i);
% 
%     end
%     
% end
% 
% 
% % - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
% % 3.1 WEIGHTED Cmp comparison (SWOT; 12 February 2018) ** Version 7 **  PL
% if 0
%     
%     % Ok, the way this part is coded is not optimal...
%     % The variable BinPhaseDef is not directly sent to this function, it is
%     % taken from the handles. Of course it works fine, but if handles doesn't
%     % come for any reason or future update, it will not work anymore. 
%     %
%     % Pierre Lanari (12.02.2018) - SWOT
%     
%     BinPhaseDef = handles.BinPhaseDef;
%     for i=1:length(BinPhaseDef)
%         NameMinBinDef{i} = BinPhaseDef(i).DBMinName;
%     end
%     
%     for i=1:length(Link.PhasesNames)
%          NameRaw = Link.PhasesNames{i};
%          NameRawStr = strread(NameRaw,'%s');
%          if iscell(NameRawStr)
%              NamePhasesMatch{i} = NameRawStr{1};
%          else
%              NamePhasesMatch{i} = NameRaw;
%          end
%     end
%     
%     Qual = zeros(size(CompTherMatch,1),1);
%     Missfit = zeros(size(CompTherMatch,1),1);
%     weight = zeros(size(CompTherMatch,1),1);
%     
%     ElemsList = WorkVariMod.Els;
%     
% %     keyboard
% %     CompTherMatch
% %     WorkVariMod.Names
%     
%     IsCompared = zeros(size(CompTherMatch,1),1);
% 
%     for i=1:size(CompTherMatch,1)
%         
%         NamePhasesModel = NamePhasesMatch{i};
%         
%         NbAtXMap = sum(CompXMapMatch(i,2:end-1)); % I excluded oxygen (2:end) and E (end)
%         NbAtTher = sum(CompTherMatch(i,2:end-1)); % and E (end)
%         
%         if NbAtXMap > 0 && NbAtTher > 0
%             
%             IsCompared(i) = 1;
%             
%             % Extract the parameters from BinPhaseDef
%             WhereInBinDef = find(ismember(NameMinBinDef,NamePhasesModel));
%             
%             ElsSel = BinPhaseDef(WhereInBinDef).ListVariAntidote;
%             WeightSel = BinPhaseDef(WhereInBinDef).ListWeightAntidote;
%             
%             [Yes1,WhereElemInMod] = ismember(ElsSel,WorkVariMod.Els);
%             [Yes2,WhereElemInXMap] = ismember(ElsSel,WorkVariXMap.Els);
%             
%             CompoMod = zeros(size(WhereElemInMod));
%             CompoXMap = zeros(size(WhereElemInMod));
%             
%             CompoMod(find(WhereElemInMod)) = WorkVariMod.COMP(Link.TherIndices(i),WhereElemInMod(find(WhereElemInMod)));
%             CompoXMap(find(WhereElemInXMap)) = WorkVariXMap.COMP(Link.XMapIndices(i),WhereElemInXMap(find(WhereElemInXMap)));
%             
%             
%             if ~isequal(length(CompoMod),length(CompoXMap),length(WeightSel))
%                 disp('Oups something went wrong in Bingo_Qcmp')
%                 keyboard
%             end
%             
%             TOL = WeightSel;
%             TOLw = WeightSel.*CompoXMap;
%             
%             DIFFabs = abs(CompoMod-CompoXMap);
%             
%             DIFF2 = DIFFabs-TOL;              
%             WhereNEGZ = find(DIFF2<=0);
%             
%             DIFF2(WhereNEGZ) = zeros(size(WhereNEGZ));
%             
%             QUALsmall = 100-DIFF2./TOLw.*10;
%             
%             WhereNEGZ = find(QUALsmall<=0);
%             WhereNEGN = find(isnan(QUALsmall));
%             
%             QUALsmall(WhereNEGZ) = zeros(size(WhereNEGZ));
%             QUALsmall(WhereNEGN) = 100*ones(size(WhereNEGN));
%             
%             RelFract = CompoMod/sum(CompoMod);
%             Qual(i) = sum(QUALsmall.*RelFract);
%             
%             if DoWePrint
%                 disp('-')
%                 disp(char(Link.PhasesNames{i}))
%                 
%                 fprintf(Code1,'Els:   ',ElemsList{1:end-1})
%                 fprintf(Code2,'THER:  ',CompTherMatch(i,1:end-1));
%                 fprintf(Code2,'XMAP:  ',CompXMapMatch(i,1:end-1));
%                 fprintf(Code2,'abs(D):',abs(CompTherMatch(i,1:end-1)-CompXMapMatch(i,1:end-1)));
%             
%                 disp(' ')
%                 
%                 CodeT1 = '%s\t\t';
%                 CodeT2 = '%s\t\t';
%                 for k=1:length(ElsSel)
%                     CodeT1=[CodeT1,'%s\t\t'];
%                     CodeT2=[CodeT2,'%f\t'];
%                 end
%                 CodeT1(end) = 'n';
%                 CodeT2(end) = 'n';
%                 
%                 fprintf(CodeT1,'Els:   ',ElsSel{1:end})
%                 %fprintf(CodeT2,'InTHER:',WhereElemInMod);         % TEMP
%                 %fprintf(CodeT2,'InXMAP:',WhereElemInXMap);        % TEMP
%                 fprintf(CodeT2,'THER:  ',CompoMod);
%                 fprintf(CodeT2,'XMAP:  ',CompoXMap);
%                 disp('_______')
%                 fprintf(CodeT2,'TOL:   ',TOL);
%                 fprintf(CodeT2,'TOLw:  ',TOLw);
%                 fprintf(CodeT2,'DIFFab:',DIFFabs);
%                 fprintf(CodeT2,'DIFF2: ',DIFF2);
%                 disp('_______')
%                 fprintf(CodeT2,'QUALs: ',QUALsmall);
%                 fprintf(CodeT2,'X(El): ',RelFract);
%                 
%                 disp(' ')
%                 disp(['QUALITY = ',num2str(Qual(i)),' %'])
%                 
%                 disp(' ')
%             end
%             
%         else
%             
%             if DoWePrint
%                 disp([' - - - Phase: ',char(NamePhasesModel),' has been skipped by BINGO - - - '])
%                 disp(' ')
%             end
%             
%         end
%     end
%      
% end   
% 
% 
% 
% return
% 
return


% goto run bingo

% #########################################################################
%       BIN - *** RUN BINGO ***
function BinButtonRunBingo_Callback(hObject, eventdata, handles)
% 
RunSWOT(1,handles);

Res_ClearReports_Callback(hObject, eventdata, handles);

% -------------------------------------------------------------------------
% DISPLAY 
disp(' ')
disp(' ')
disp('-----------------------------------------------------')
disp(['    >>> New BINGO Run: ',datestr(now),'  <<<'])
disp(' ')

% New (19.06.2019 - PL)
load('MinimOptions.mat');

% -------------------------------------------------------------------------
% (1) Create WORKVARI from the selected domain
[WorkVariXMap] = GenerateResXMap(handles.BinPhaseDef);


% -------------------------------------------------------------------------
% (2) SET initial variables for Theriak
[BinSet] = SetBin(handles);

D_Temp = get(handles.BinTextBinTC,'String');
D_Press = get(handles.BinTextBinP,'String');

CleanAxes2(handles);
plot(str2num(D_Temp),str2num(D_Press),'o','MarkerFaceColor','r','MarkerEdgeColor','k');
eval(['TCi = [',get(handles.BinTextBinTCminmax,'String'),'];']);
eval(['Pi = [',get(handles.BinTextBinPminmax,'String'),'];']);

axis([TCi(1) TCi(end) Pi(1) Pi(end)]);
xlabel('Temperature');
ylabel('Pressure');
drawnow 
%axis(AxisVal);

fprintf('%s\t\t%s\n','Bulk',BinSet.Bulk2Display)
fprintf('%s\t%s\n','Database',BinSet.Database);
fprintf('%s\t\t%.0f\n','P(bar)',str2num(D_Press));
fprintf('%s\t\t%.0f\n','T(C)',str2num(D_Temp));
disp(' ')

% -------------------------------------------------------------------------
% (3) Call Theriak
WorkVariMod = TheriakCall(BinSet,D_Temp,D_Press);

%WorkVariMod = TheriakdCall(InvMet,D_Temp,D_Press,handles);


% -------------------------------------------------------------------------
% (4) Call Bingo
DoWePrint = 1;

Report.Mode = 'BINGO';
Report.Tdeg = D_Temp;
Report.Pbar = D_Press;

[A1,V1,C1,TOTAL2,Report] = BingoCall(WorkVariMod,WorkVariXMap,Report,DoWePrint,handles);

TheDate = datestr(now);

% Update "Report" and plot
Report.QualityFactors = [A1,V1,C1,TOTAL2];
Report.Path = BinSet.Path;
Report.Database = BinSet.Database;
Report.Bulk2Display = BinSet.Bulk2Display;
Report.Info = {['## REPORT OUTPUT ##'], ...
    ['>> Date: ',TheDate], ...
    ['>> Program: BINGO'], ...
    ['>> Eq. conditions: ',num2str(str2num(D_Temp),'%.0f'),' C; ',num2str(str2num(D_Press)/1000,'%.1f'),' kbar'], ...
    ['>> Thermodynamic dataset: ',Report.Database], ...
    ['>> Local bulk composition: ',Report.Bulk2Display(2:end)]};

Report.Title = ['BINGO | ',num2str(str2num(D_Temp),'%.0f'),' C; ',num2str(str2num(D_Press)/1000,'%.1f'),' kbar | ',Report.Database,' | ',TheDate];

% WARNING: If you want to add new variables to the REPORT, update add them
% at the end and update the "Load" function for compatibility. 

handles = AddReport2Memory(Report,hObject, eventdata, handles);

if isequal(MinimOptions.TestMode,1)
    DefMin = handles.BingoDefault.Theriak.Database(get(handles.BinPopUpDatabase,'Value')).DefMin;
    WriteInputBingoStandalone(Report,DefMin);
end

guidata(hObject, handles);

disp(' '), disp(' ')
disp(['    >>> End BINGO Run: ',TheDate,'  <<<'])
disp('-----------------------------------------------------')
RunSWOT(0,handles);

% Initialize the LIVE workspace
InitPanelDisp(2,handles);

ActivateZoomFigure1(hObject, eventdata, handles);
return




function [] = WriteInputBingoStandalone(Report,DefMin)
%

disp(' '),disp(' ')
disp('##### TEST MODE (BINGO) #####') 

disp(' ')
disp(' >> Checking the folder [Bingo_Test]')

[Success,Message,MessageID] = mkdir('Bingo_Test');

disp(' >> Done')

disp(' ')
disp(' >> Generating [InputBingo.txt]')

fid = fopen([cd,'/Bingo_Test/InputBingo.txt'],'w');
fprintf(fid,'%.0f\n',length(Report.Phases));
fprintf(fid,'%.0f\n',length(Report.ElemsList));
CodeF = '';
for i = 1:length(Report.ElemsList)
    if i < length(Report.ElemsList)
        fprintf(fid,'%s\t',char(Report.ElemsList{i}));
    else
        fprintf(fid,'%s\n',char(Report.ElemsList{i}));
    end
    CodeF = [CodeF,'%f\t'];
end
CodeF(end) = 'n';
CodeF2 = '';
for i = 1:length(Report.Phases)
    fprintf(fid,CodeF,Report.CmpXMap(i,:));
    CodeF2 = [CodeF2,'%f\t'];
end
CodeF2(end) = 'n';
for i = 1:length(Report.Phases)
    if i < length(Report.Phases)
        fprintf(fid,'%s\t',char(Report.Phases{i}));
    else
        fprintf(fid,'%s\n',char(Report.Phases{i}));
    end
end
fprintf(fid,CodeF2,Report.VolXMap);
for i = 1:length(Report.Phases)
    if i < length(Report.Phases)
        fprintf(fid,'%s\t','A');
    else
        fprintf(fid,'%s\n','A');
    end
end

fclose(fid);

disp(' >> Done ')

disp(' ')
disp([' >> Copying [THERIN] and [',Report.Database,']'])
copyfile([cd,'/THERIN'],[cd,'/Bingo_Test/THERIN'],'f');
copyfile([cd,'/',Report.Database],[cd,'/Bingo_Test/',Report.Database],'f');
disp(' >> Done ')

disp(' ')
disp([' >> Saving [DefMin.mat] '])
save('Bingo_Test/DefMin.mat','DefMin');
disp(' >> Done ')
%keyboard

return









% Archives (to be deleted soon): 

function [WorkVariMod] = Core_ReadResTheriak3_v210319(OutputTheriakd,ListRefMiner)
% 
% New version 1.3.1 (22.05.17)
%
% -> The new version of theriak provides a table with structural formulas
% for both MAC and WINDOWS. I updated this function to directly read this
% table as in the originial Bingo-Antidote version.
%

TestInput = strread(OutputTheriakd,'%s','delimiter','\n');

% To display Theriak output: 
%OutputTheriakd

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% (1) Elements and test for error with the database ...
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

%
WhereElOrder = find(ismember(TestInput,'elements in stable phases:'))+3;

if isempty(WhereElOrder)
    OutputTheriakd
    error('ERROR IN READING THE THERIAK OUTPUT FOR ELEMENTS (see theriak output above); the table containing the elements in stable phases is not available');
    return
end

El1 = strread(char(TestInput(WhereElOrder)),'%s')';
if length(El1) == 10
    SecondRow = strread(char(TestInput(WhereElOrder+1)),'%s')';
    if length(SecondRow) < length(El1) % Could be a second row...
        El1 = [El1,SecondRow];
    end
end

WorkVariMod(1).Els = El1;
WorkVariMod(1).NbEl = length(El1);

if WorkVariMod(1).NbEl > 10
    Shift = 1;
else
    Shift = 0;
end


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% (2a) Elements in stable phases
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

WhereCompositions = find(ismember(TestInput,'elements per formula unit:'))+1;

Compt = 1; Indice = 1;
while 1
    ShiftRows = (Compt-1)*Shift;
    Temp = strread(char(TestInput(WhereCompositions+Compt+ShiftRows)),'%s')';
    if isempty(Temp)
        break          % OK
    end
    if Shift
        Temp2 = strread(char(TestInput(WhereCompositions+Compt+ShiftRows+1)),'%s')';
    end
    
    NbElem1 = length(str2double(Temp(2:end)));
    ASS_Indice2(Indice) = Compt;
    ASS_Names2{Indice} = Temp{1};
    ASS_COMP2(Indice,1:NbElem1) = str2double(Temp(2:end));
    if Shift
        ASS_COMP2(Indice,NbElem1+1:NbElem1+length(Temp2)) = str2double(Temp2);
    end
    Indice = Indice+1;    
        
    Compt = Compt+1;
end


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% (2b) Volume and densities of solids
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

WhereVol = find(ismember(TestInput,'volumes and densities of stable phases:'))+4;

Compt = 1; Indice = 1; Skip = 0;
while 1
    Temp = strread(char(TestInput(WhereVol+Compt+Skip)),'%s')';
    
    if ~isequal(length(Temp),11)
        break
    end

    FractPer = str2double(Temp(5));

    if FractPer > 0.0001
        VOL_VolCCM(Compt) = str2double(Temp(4));
        VOL_VolFrac(Compt) = FractPer/100;
        VOL_Dens(Compt) = str2double(Temp(11)); 

        VOL_Names{Compt} = Temp{1};

        Compt = Compt+1;
    else
        Skip = Skip+1;
    end
end


% Check for Gases     *** New 1.4.1 (PL - 10.02.2018) 
WhereGf = WhereVol+Compt+Skip+4;

Temp = strread(char(TestInput(WhereGf)),'%s')';

Compt2 = 1;
WhereGf = WhereGf + 1;

if isequal(char(Temp{1}),'gases') && isequal(char(Temp{2}),'and') && isequal(char(Temp{3}),'fluids')    
    while 1
        Temp = strread(char(TestInput(WhereGf+Compt2)),'%s')';

        if ~isequal(length(Temp),7)
            break
        end
        
        VOLgf_VolCCM(Compt2) = str2double(Temp(4));
        VOLgf_VolFrac(Compt2) = 0;
        VOLgf_Dens(Compt2) = str2double(Temp(7));
        
        VOLgf_Names{Compt2} = Temp{1};
        
        Compt2 = Compt2+1;        
    end
    
else
    VOLgf_VolCCM = [];
    VOLgf_VolFrac = [];
    VOLgf_Dens = [];
    VOLgf_Names = '';
end

% Check if a liquid phase should be added to the solids
DoWeUdpateVol = 0;
for i=1:length(VOLgf_VolCCM)
    TempName = VOLgf_Names{i};
    TempNameStr = strread(TempName,'%s','delimiter','_');
    if iscell(TempNameStr)
        NameGf = TempNameStr{1};
    else
        NameGf = TempName;
    end
    Ok = find(ismember(ListRefMiner,NameGf));
    
    if Ok
        DoWeUdpateVol = 1;
        %Compt = Compt +1;
        VOL_VolCCM(Compt) = VOLgf_VolCCM(i);
        VOL_VolFrac(Compt) = VOLgf_VolFrac(i);
        VOL_Dens(Compt) = VOLgf_Dens(i); 
    
        VOL_Names{Compt} = TempName;
    end
end

if DoWeUdpateVol
    VOL_VolFrac = VOL_VolCCM/sum(VOL_VolCCM);
end


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% (3) SYNCHRONIZATION (SOLIDS + FLUIDS)
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

%keyboard

for i=1:length(VOL_Names)
    
    [OK,WhereC] = ismember(VOL_Names{i},ASS_Names2);
    
    if ~OK
        OutputTheriakd
        disp(' ')
        disp(' ')
        disp(['IT SEEMS THAT THE COMPOSITION OF PHASE ',char(VOL_Names{i}),' IS NOT AVAILABLE']) 
        disp('please send the theriak output above with error code (ERR2048) to pierre.lanari@geo.unibe.ch  ...')
        keyboard
    end
    
    WorkVariMod(1).Indice(i) = i;
   	WorkVariMod(1).Names{i} = VOL_Names{i};
    WorkVariMod(1).COMP(i,:) = ASS_COMP2(WhereC,:);
    WorkVariMod(1).VolFrac(i) = VOL_VolFrac(i);
    WorkVariMod(1).Dens(i) = VOL_Dens(i);
end

WorkVariMod(1).NbPhases = length(WorkVariMod(1).Names);


% FLUID PL - 07.03.2019
if Compt2 > 1 % then there is at least one fluid phase
    WorkVariMod(1).FluidPhases = Compt2-1;
    WorkVariMod(1).FluidDens = VOLgf_Dens;
    
    for i= 1:Compt2-1
        [OK,WhereC] = ismember(VOLgf_Names{i},ASS_Names2);
        WorkVariMod(1).FluidNames{i} = VOLgf_Names{i};
        WorkVariMod(1).FluidCOMP(i,:) = ASS_COMP2(WhereC,:);
    end
else
    WorkVariMod(1).FluidPhases = 0;
    WorkVariMod(1).FluidDens = [];
    WorkVariMod(1).FluidCOMP = [];
end

% ------------------------------------------------------------------------
% Check Theriak Names and remove EM abbreviations
% ------------------------------------------------------------------------
for i=1:length(WorkVariMod(1).Names)
    TheNameFromTheriak = WorkVariMod(1).Names{i};
    if ~ismember(TheNameFromTheriak,ListRefMiner)
        WereD = ismember(TheNameFromTheriak,'_');
        %keyboard
        switch sum(WereD)
            case 1
                % we delete it...
                Where = find(WereD);
                WorkVariMod(1).Names{i} = TheNameFromTheriak(1:Where-1);
            case 2
                % we have to delete the first one (Compatibility with the
                % MELT model of DOUG
                Where = find(WereD);
                NameTemp = TheNameFromTheriak(1:Where(1)-1);
                if isequal(NameTemp,'LIQtc6')
                    WorkVariMod(1).Names{i} = NameTemp;
                else
                    % we delete the second one ...
                    WorkVariMod(1).Names{i} = TheNameFromTheriak(1:Where(2)-1);
                end
            case 3
                disp('Oups, too many underscores in this name contact Pierre Lanari')
                keyboard
        end
    end
end


% ------------------------------------------------------------------------
% Check for phase demixion (not identified in ListRefMiner). 
% ------------------------------------------------------------------------
% Note this is typically caused by flat G function in complex solid 
% solution models from Roger Powell (amphiboles).  
%
% in this case we select the phase with the higher volume fraction for
% comparison with the observation and rename the other phases.
%
%                                                  Pierre Lanari (24.10.16)

for i=1:length(WorkVariMod(1).Names)
    TheName = WorkVariMod(1).Names{i};
    Ind = find(ismember(WorkVariMod(1).Names,TheName));
    
    if length(Ind)>1
        Vols = WorkVariMod.VolFrac(Ind);
        [Val,IndSort] = sort(Vols,2,'descend');
        
        for j=2:length(IndSort)
            WorkVariMod(1).Names{Ind(IndSort(j))} = [WorkVariMod(1).Names{Ind(IndSort(j))},num2str(j)];
        end    
    end
end

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% (4) CHEMICAL POTENTIAL OF COMPONENTS
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

WhereChemCo = find(ismember(TestInput,'--------------------------------------------------------------------'))+1;

Compt = 0;
ComptOk = 0;
while 1
    Temp = strread(char(TestInput(WhereChemCo+Compt)),'%s')';
    
    if ~isequal(char(Temp{1}),'P')
        break
    end
    
    PName = char(Temp{3});
    if isequal(PName(1),'*')
        ComptOk = ComptOk+1;
        WorkVariMod.ChemComp{ComptOk} = PName(2:end);
        WorkVariMod.ChemPot(ComptOk) = -str2num(Temp{5});
    end
    Compt = Compt+1;
end


% WhereChemCo = find(ismember(TestInput,'chemical potentials of components:'))+3;
% 
% Temp = strread(char(TestInput(WhereChemCo)),'%s')';
% NbComponents = str2num(Temp{end});
% 
% for i = 1:NbComponents-1
%     Temp = strread(char(TestInput(WhereChemCo+11+NbComponents+i)),'%s')';
%     WorkVariMod.ChemComp{i} = char(Temp{1});
%     WorkVariMod.ChemPot(i) = -str2num(Temp{3});
% end


return


function [WorkVariMod] = Core_ReadResTheriak2_v220517(OutputTheriakd,ListRefMiner)
% 
% New version 1.2.4

TestInput = strread(OutputTheriakd,'%s','delimiter','\n');

% To display Theriak output: 
%OutputTheriakd

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% (1) Elements and test for error with the database ...
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

%
WhereElOrder = find(ismember(TestInput,'elements in stable phases:'))+3;

if isempty(WhereElOrder)
    OutputTheriakd
    error('ERROR WITH THE DATABASE (see theriak output above) ...');
    return
end

El1 = strread(char(TestInput(WhereElOrder)),'%s')';
if length(El1) == 10
    SecondRow = strread(char(TestInput(WhereElOrder+1)),'%s')';
    if length(SecondRow) < length(El1) % Could be a second row...
        El1 = [El1,SecondRow];
    end
end

WorkVariMod(1).Els = El1;
WorkVariMod(1).NbEl = length(El1);

if WorkVariMod(1).NbEl > 10
    Shift = 1;
else
    Shift = 0;
end


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% (2a) Elements in stable phases
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

WhereCompositions = WhereElOrder+Shift;
Compt = 1; Indice = 1;
while 1
    ShiftRows = (Compt-1)*Shift;
    Temp = strread(char(TestInput(WhereCompositions+Compt+ShiftRows)),'%s')';
    if isempty(Temp)
        break          % OK
    end
    if Shift
        Temp2 = strread(char(TestInput(WhereCompositions+Compt+ShiftRows+1)),'%s')';
    end
    
    NbElem1 = length(str2double(Temp(2:end)));
    ASS_Indice2(Indice) = Compt;
    ASS_Names2{Indice} = Temp{1};
    ASS_COMP2(Indice,1:NbElem1) = str2double(Temp(2:end));
    if Shift
        ASS_COMP2(Indice,NbElem1+1:NbElem1+length(Temp2)) = str2double(Temp2);
    end
    Indice = Indice+1;    
        
    Compt = Compt+1;
end


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% (2b) Volume and densities of solids
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

WhereVol = find(ismember(TestInput,'volumes and densities of stable phases:'))+4;

Compt = 1; Indice = 1;
while 1
    Temp = strread(char(TestInput(WhereVol+Compt)),'%s')';
    
    if ~isequal(length(Temp),11)
        break
    end
    
    VOL_VolFrac(Compt) = str2double(Temp(5))/100;
	VOL_Dens(Compt) = str2double(Temp(11)); 
    
    VOL_Names{Compt} = Temp{1};
    
    Compt = Compt+1;
end


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% (3) SYNCHRONIZATION (let's consider only the solids)
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

for i=1:length(VOL_Names)
    
    [OK,WhereC] = ismember(VOL_Names{i},ASS_Names2);
    
    if ~OK
        OutputTheriakd
        disp(' ')
        disp(' ')
        disp(['IT SEEMS THAT THE COMPOSITION OF PHASE ',char(VOL_Names{i}),' IS NOT AVAILABLE']) 
        disp('please send the theriak output above with error code (ERR2048) to pierre.lanari@geo.unibe.ch  ...')
        keyboard
    end
    
    WorkVariMod(1).Indice(i) = i;
   	WorkVariMod(1).Names{i} = VOL_Names{i};
    
    % Correction of O for H:
    WhereH = find(ismember(WorkVariMod.Els,'H'));
    WhereO = find(ismember(WorkVariMod.Els,'O'));

    TempCOMP = ASS_COMP2(WhereC,:);
    if length(WhereH) && length(WhereO)
        % Correction assuming H2O
        TempCOMP(WhereO) = TempCOMP(WhereO) - 0.5* TempCOMP(WhereH);
        TempCOMP(WhereH) = 0;
    end

    WorkVariMod(1).COMP(i,:) = TempCOMP;
    WorkVariMod(1).VolFrac(i) = VOL_VolFrac(i);
    WorkVariMod(1).Dens(i) = VOL_Dens(i);
end

WorkVariMod(1).NbPhases = length(WorkVariMod(1).Names);

% ------------------------------------------------------------------------
% Check Theriak Names and remove EM abbreviations
% ------------------------------------------------------------------------
for i=1:length(WorkVariMod(1).Names)
    TheNameFromTheriak = WorkVariMod(1).Names{i};
    if ~ismember(TheNameFromTheriak,ListRefMiner)
        WereD = ismember(TheNameFromTheriak,'_');
        switch sum(WereD)
            case 1
                % we delete it...
                Where = find(WereD);
                WorkVariMod(1).Names{i} = TheNameFromTheriak(1:Where-1);
            case 2
                % we delete the second one ...
                Where = find(WereD);
                WorkVariMod(1).Names{i} = TheNameFromTheriak(1:Where(2)-1);
            case 3
                keyboard
        end
    end
end
 

% ------------------------------------------------------------------------
% Check for phase demixion (not identified in ListRefMiner). 
% ------------------------------------------------------------------------
% Note this is typically caused by flat G function in complex solid 
% solution models from Roger Powell (amphiboles).  
%
% in this case we select the phase with the higher volume fraction for
% comparison with the observation and rename the other phases.
%
%                                                  Pierre Lanari (24.10.16)

for i=1:length(WorkVariMod(1).Names)
    TheName = WorkVariMod(1).Names{i};
    Ind = find(ismember(WorkVariMod(1).Names,TheName));
    
    if length(Ind)>1
        Vols = WorkVariMod.VolFrac(Ind);
        [Val,IndSort] = sort(Vols,2,'descend');
        
        for j=2:length(IndSort)
            WorkVariMod(1).Names{Ind(IndSort(j))} = [WorkVariMod(1).Names{Ind(IndSort(j))},num2str(j)];
        end    
    end
end

return


function [WorkVariMod] = Core_ReadResTheriak_NOTUSED(OutputTheriakd,ListRefMiner)
%

TestInput = strread(OutputTheriakd,'%s','delimiter','\n');

WhereElOrder = find(ismember(TestInput,'elements in stable phases:'))+3;
WorkVariMod(1).Els = strread(char(TestInput(WhereElOrder)),'%s')';

WorkVariMod(1).NbEl = length(WorkVariMod(1).Els);

WhereCompositions = find(ismember(TestInput,'elements per formula unit:'))+1;
Compt = 1; Indice = 1;
while 1
    Temp = strread(char(TestInput(WhereCompositions+Compt)),'%s')';
    if isempty(Temp)
        break
    end
    if ~isequal(Temp{1},'STEAM') && ~isequal(Temp{1},'OXYGEN') && ~isequal(Temp{1},'HYDROGEN') && ~isequal(Temp{1},'water.fluid')
        WorkVariMod(1).Indice(Indice) = Compt;
        WorkVariMod(1).Names{Indice} = Temp{1};
        WorkVariMod(1).COMP(Indice,:) = str2double(Temp(2:end));
        Indice = Indice+1;
    end
    Compt = Compt+1;
end

WorkVariMod(1).NbPhases = length(WorkVariMod(1).Names);

WhereVol = find(ismember(TestInput,'volumes and densities of stable phases:'))+4;

WorkVariMod(1).VolFrac = zeros(size(WorkVariMod(1).Names));
WorkVariMod(1).Dens = zeros(size(WorkVariMod(1).Names));
for i=1:WorkVariMod(1).NbPhases
	Temp = strread(char(TestInput(WhereVol+i)),'%s')';
    Names4Check{WorkVariMod(1).Indice(i)} = Temp{1};
    if length(Temp) < 11
        disp('Oups, something went wrong... [check Core_ReadResTheriak]')
        keyboard
    end
	WorkVariMod(1).VolFrac(WorkVariMod(1).Indice(i)) = str2double(Temp(5))/100;
	WorkVariMod(1).Dens(WorkVariMod(1).Indice(i)) = str2double(Temp(11)); 
end

if ~isequal(WorkVariMod(1).Names,Names4Check)
    disp('Oups, something went wrong... [check Core_ReadResTheriak]')
    keyboard
end

% Check Theriak Names and remove EM abbreviations
for i=1:length(WorkVariMod(1).Names)
    TheNameFromTheriak = WorkVariMod(1).Names{i};
    if ~ismember(TheNameFromTheriak,ListRefMiner)
        WereD = ismember(TheNameFromTheriak,'_');
        switch sum(WereD)
            case 1
                % we delete it...
                Where = find(WereD);
                WorkVariMod(1).Names{i} = TheNameFromTheriak(1:Where-1);
            case 2
                % we delete the second one ...
                Where = find(WereD);
                WorkVariMod(1).Names{i} = TheNameFromTheriak(1:Where(2)-1);
            case 3
                keyboard
        end
    end
end

return










% ### REPORTS ###



% #########################################################################
%       BIN - Report: Clear report list
function Res_ClearReports_Callback(hObject, eventdata, handles)
% 

handles.MemoryReports.List = {'no result available so far ...'};
handles.MemoryReports.Nb = 0;
handles.MemoryReports.Data = [];

axes(handles.axes2), cla
axes(handles.axes_PLOT1), cla
title('');
axes(handles.axes_PLOT2), cla
title('');

set(handles.ResPopUpListReport,'String',handles.MemoryReports.List,'Value',1);

set(handles.Disp_Obs1,'String','');
set(handles.Disp_Obs2,'String','');
set(handles.Disp_Obs3,'String','');
set(handles.Disp_Obs4,'String','');
set(handles.Disp_Obs5,'String','');

set(handles.Disp_Mod1,'String','');
set(handles.Disp_Mod2,'String','');
set(handles.Disp_Mod3,'String','');
set(handles.Disp_Mod4,'String','');
set(handles.Disp_Mod5,'String','');

set(handles.Disp_Res1,'String','');

set(handles.BinTextBinRes1,'String','...');
set(handles.BinTextBinRes2,'String','...');
set(handles.BinTextBinRes3,'String','...');
set(handles.BinTextBinRes4,'String','...');

guidata(hObject, handles);

% InitPanelDisp(1,handles);
return



% #########################################################################
%       BIN - Report: load
function Res_LoadReports_Callback(hObject, eventdata, handles)
% 

[Success,Message,MessageID] = mkdir('Bingo_Reports');

cd Bingo_Reports

[filename, pathname] = uigetfile('*.mat', 'Select a report');

cd ..

if filename
    ProjectPath = strcat(pathname,filename);
else
    return
end

load(ProjectPath);

% Here we shall add updates for compatibility...


handles = AddReport2Memory(TheReport, hObject, eventdata, handles);


return



% #########################################################################
%       BIN - Report: save the selected report(s)
function Res_SaveReports_Callback(hObject, eventdata, handles)
% 
MemoryReports = handles.MemoryReports;

SelectedReport = get(handles.ResPopUpListReport,'Value');
Report = MemoryReports.Data(SelectedReport).Report;
 
[Success,Message,MessageID] = mkdir('Bingo_Reports');

cd Bingo_Reports

ReportName = MemoryReports.List{SelectedReport};
Ok = find(~ismember(ReportName,' '));
ReportName = ReportName(Ok);
Ok = find(~ismember(ReportName,';'));
ReportName = ReportName(Ok);
Ok = find(~ismember(ReportName,'.'));
ReportName = ReportName(Ok);
Ok = find(~ismember(ReportName,'-'));
ReportName = ReportName(Ok);
Ok = find(~ismember(ReportName,':'));
ReportName = ReportName(Ok);

Ok = find(ismember(ReportName,'|'));
for i=1:length(Ok)
    ReportName(Ok(i)) = '_';
end

filename = [ReportName,'.mat'];

[Directory, pathname] = uiputfile({'*.mat', 'XThermoTools Report File (*.mat)'}, 'Save the selected report as',filename);

cd ..
 
TheReport = Report;

if Directory
    save(strcat(pathname,Directory),'TheReport');
    
    fid = fopen(strcat(pathname,Directory(1:end-4),'.xtt'),'w');
    
    fprintf(fid,'%s\n','#######################################');
    fprintf(fid,'%s\n','#      BINGO-ANTIDOTE REPORT FILE     #');
    fprintf(fid,'%s\n\n','#######################################');

    fprintf(fid,'%s\n\n',Report.Title);
    
    fprintf(fid,'%s\t%s\n\n','Program: ',Report.Mode);
    
    fprintf(fid,'%s\t%s\n','Dataset: ',Report.Database);
    fprintf(fid,'%s\t%s\n\n','LEB:     ',Report.Bulk2Display);
    
    fprintf(fid,'%s\t%s\n','TC:      ',Report.Tdeg);
    fprintf(fid,'%s\t%s\n','Pbar:    ',Report.Pbar);

    fprintf(fid,'%s\n','');
    
    fprintf(fid,'%s\t%.4f\t%s\n','Qass:    ',Report.QualityFactors(1),'/ 100');
    fprintf(fid,'%s\t%.4f\t%s\n','Qmod:    ',Report.QualityFactors(2),'/ 100');
    fprintf(fid,'%s\t%.4f\t%s\n','Qcmp:    ',Report.QualityFactors(3),'/ 100');
    fprintf(fid,'%s\t%.4f\t%s\n','Qtot:    ',Report.QualityFactors(4),'/ 100');
    
    fprintf(fid,'%s\n','');
    
    fprintf(fid,'%s\t','Phases:  ');
    for i=1:length(Report.Phases)
        fprintf(fid,'%s',Report.Phases{i});
        if i<length(Report.Phases)
            fprintf(fid,'\t');
        else
            fprintf(fid,'\n');
        end
    end

    fprintf(fid,'%s\t','IsInMap: ');
    for i=1:length(Report.PhasesXMap)
        fprintf(fid,'%.0f',Report.PhasesXMap(i));
        if i<length(Report.PhasesXMap)
            fprintf(fid,'\t');
        else
            fprintf(fid,'\n');
        end
    end
    
    fprintf(fid,'%s\t','IsInMod: ');
    for i=1:length(Report.PhasesTher)
        fprintf(fid,'%.0f',Report.PhasesTher(i));
        if i<length(Report.PhasesTher)
            fprintf(fid,'\t');
        else
            fprintf(fid,'\n');
        end
    end
    

    fprintf(fid,'%s\t','Vol_Map: ');
    for i=1:length(Report.VolXMap)
        fprintf(fid,'%.5f',Report.VolXMap(i));
        if i<length(Report.VolXMap)
            fprintf(fid,'\t');
        else
            fprintf(fid,'\n');
        end
    end
    
    fprintf(fid,'%s\t','Vol_Mod: ');
    for i=1:length(Report.VolTher)
        fprintf(fid,'%.5f',Report.VolTher(i));
        if i<length(Report.VolTher)
            fprintf(fid,'\t');
        else
            fprintf(fid,'\n');
        end
    end
    
    fprintf(fid,'%s\n\n','');
    
    fprintf(fid,'%s\n','>> MAP mineral compositions (in apfu)');
    
    for i=1:length(Report.ElemsList)
        fprintf(fid,'%s',Report.ElemsList{i});
        if i<length(Report.ElemsList)
            fprintf(fid,'\t');
        else
            fprintf(fid,'\n');
        end
    end
    
    for i=1:size(Report.CmpXMap,1)
        for j=1:size(Report.CmpXMap,2)
            if Report.CmpXMap(i,j)>10
                fprintf(fid,'%.4f',Report.CmpXMap(i,j));
            else
                fprintf(fid,'%.5f',Report.CmpXMap(i,j));
            end
            if j<size(Report.CmpXMap,2)
                fprintf(fid,'\t');
            else
                fprintf(fid,'\n');
            end
        end
    end
    
    fprintf(fid,'%s\n\n','');
    
    fprintf(fid,'%s\n','>> MODEL mineral compositions (in apfu)');
    
    for i=1:length(Report.ElemsList)
        fprintf(fid,'%s',Report.ElemsList{i});
        if i<length(Report.ElemsList)
            fprintf(fid,'\t');
        else
            fprintf(fid,'\n');
        end
    end
    
    for i=1:size(Report.CmpTher,1)
        for j=1:size(Report.CmpTher,2)
            if Report.CmpTher(i,j)>10
                fprintf(fid,'%.4f',Report.CmpTher(i,j));
            else
                fprintf(fid,'%.5f',Report.CmpTher(i,j));
            end
            if j<size(Report.CmpTher,2)
                fprintf(fid,'\t');
            else
                fprintf(fid,'\n');
            end
        end
    end
    
    fclose(fid);
    
    axes(handles.axes_PLOT1)
    TheColormap = colormap;
    
    Fig = figure(666);
    
    h1 = subplot(1,2,1);
    hp1 = pie(h1,Report.VolXMap+0.000000000001,Report.TheNamesPlotMap); 
    set(hp1(2:2:end),'FontSize',9);
    colormap(TheColormap)
    title('Observations (map)','FontSize',13);

    h2 = subplot(1,2,2);
    hp2 = pie(h2,Report.VolTher+0.00000000001,Report.TheNamesPlotTher);
    set(hp2(2:2:end),'FontSize',9);
    colormap(TheColormap)
    title('Model (Theriak)','FontSize',13);

    saveas(gcf,strcat(pathname,Directory(1:end-4),'.fig'),'fig');
    saveas(gcf,strcat(pathname,Directory(1:end-4),'.pdf'),'pdf');
    
    close(Fig);
    
    
    
end

return



% #########################################################################
%       BIN - Report: Add report to memory
function handles = AddReport2Memory(Report, hObject, eventdata, handles)
%

MaxRep = handles.MemoryReports.NbMax;
NbRep = handles.MemoryReports.Nb;

if isequal(NbRep,MaxRep)
    % Move the reports
    for i=MaxRep:-1:2
        handles.MemoryReports.List{i} = handles.MemoryReports.List{i-1};
        handles.MemoryReports.Data(i) = handles.MemoryReports.Data(i-1);    
    end
    
elseif NbRep > 0
    for i=NbRep+1:-1:2
        handles.MemoryReports.List{i} = handles.MemoryReports.List{i-1};
        handles.MemoryReports.Data(i) = handles.MemoryReports.Data(i-1);    
    end
    
    handles.MemoryReports.Nb = NbRep+1;
else
    handles.MemoryReports.Nb = NbRep+1;
end

% Add the new one
handles.MemoryReports.Data(1).Report = Report;
handles.MemoryReports.List{1} =  Report.Title; 
    
% handles.MemoryReports.List = {'no result available so far ...'};
% handles.MemoryReports.Nb = 0;
% handles.MemoryReports.NbMax = 5;         % use this variable to set the max 
%                                                  % number of reports stored
% handles.MemoryReports.Data = [];

set(handles.ResPopUpListReport,'String',handles.MemoryReports.List,'Value',1);

guidata(hObject, handles);

UpdateLiveDisplayPanel(hObject, eventdata, handles);

return



% #########################################################################
%       BIN - Report: Select menu to display a report
function ResPopUpListReport_Callback(hObject, eventdata, handles)
%
UpdateLiveDisplayPanel(hObject, eventdata, handles);
return



% #########################################################################
%       BIN - Report: UPDATE THE DISPLAY PANEL (LIVE)
function UpdateLiveDisplayPanel(hObject, eventdata, handles)
%

SelRep = get(handles.ResPopUpListReport,'Value');

if ~handles.MemoryReports.Nb
    return
end

Report = handles.MemoryReports.Data(SelRep).Report;

% Update the elements
set(handles.Res_AddCol1,'String',Report.ElemsList);
set(handles.Res_AddCol2,'String',Report.ElemsList);

AddEl1 = get(handles.Res_AddCol1,'Value');
AddEl2 = get(handles.Res_AddCol2,'Value');

if isequal(AddEl1,AddEl2) && isequal(AddEl1,1) && length(Report.ElemsList) > 2
    AddEl1 = 2;
    AddEl2 = 3;
    
    set(handles.Res_AddCol1,'Value',AddEl1);
    set(handles.Res_AddCol2,'Value',AddEl2);
else
    %AddEl1 = 1;
    %AddEl2 = 2;
    
    set(handles.Res_AddCol1,'Value',AddEl1);
    set(handles.Res_AddCol2,'Value',AddEl2);
end

set(handles.Disp_LabObs4,'String',Report.ElemsList{AddEl1})
set(handles.Disp_LabMod4,'String',Report.ElemsList{AddEl1})

set(handles.Disp_LabObs5,'String',Report.ElemsList{AddEl2})
set(handles.Disp_LabMod5,'String',Report.ElemsList{AddEl2})


% PLOT
axes(handles.axes_PLOT1)    

hp1 = pie(handles.axes_PLOT1,Report.VolXMap+0.000000000001,Report.TheNamesPlotMap); 
set(hp1(2:2:end),'FontSize',9);
title(handles.axes_PLOT1,'Observations (map)','FontSize',13);

% % Temporary save - - - - -
% h = figure(999);
% hp1 = pie(Report.VolXMap+0.000000000001,Report.TheNamesPlotMap); 
% set(hp1(2:2:end),'FontSize',9);
% title('Observations (map)','FontSize',13);
% saveas(h,'Last_Bingo_OBS.pdf');
% close(h);
% % - - - - - Temporary save 

axes(handles.axes_PLOT2)

hp2 = pie(handles.axes_PLOT2,Report.VolTher+0.00000000001,Report.TheNamesPlotTher);
set(hp2(2:2:end),'FontSize',9);
title(handles.axes_PLOT2,'Model (Theriak)','FontSize',13);

% % Temporary save - - - - -
% h = figure(999);
% hp2 = pie(Report.VolTher+0.00000000001,Report.TheNamesPlotTher)
% set(hp2(2:2:end),'FontSize',9);
% title('Model (Theriak)','FontSize',13);
% saveas(h,'Last_Bingo_MOD.pdf');
% close(h);
% % - - - - - Temporary save

Txtlength = 7;

for i=1:length(Report.Phases)
    
    TempName = Report.Phases{i};
    NamePhase = '       ';
    
    if length(TempName) > Txtlength
        NamePhase = TempName(1:Txtlength);
    else
        NamePhase(1:length(TempName)) = TempName;
    end
    
    NamesCorr{i} = NamePhase;
    PhasesXMap{i} = num2str(Report.PhasesXMap(i)',0);

    if Report.VolXMap(i) > 0
        VolXMap{i} = num2str(Report.VolXMap(i)','%.2f');
    else
        VolXMap{i} = ' ';
    end
    
    PhasesTher{i} = num2str(Report.PhasesTher(i)',0);
    
    if Report.VolTher(i) > 0
        VolTher{i} = num2str(Report.VolTher(i)','%.2f');
    else
        VolTher{i} = ' ';
    end
    
    % Composition 1
    
    if Report.VolXMap(i) > 0
        CmpXMap1{i} = num2str(Report.CmpXMap(i,AddEl1)','%.2f');
        CmpXMap2{i} = num2str(Report.CmpXMap(i,AddEl2)','%.2f');
    else
        CmpXMap1{i} = ' ';
        CmpXMap2{i} = ' ';
    end
    
    if Report.VolTher(i) > 0
    	CmpTher1{i} = num2str(Report.CmpTher(i,AddEl1)','%.2f');
        CmpTher2{i} = num2str(Report.CmpTher(i,AddEl2)','%.2f');
    else
        CmpTher1{i} = ' ';
        CmpTher2{i} = ' ';
    end
    
    
end

set(handles.Disp_Mod1,'String',NamesCorr);
set(handles.Disp_Mod2,'String',PhasesTher);
set(handles.Disp_Mod3,'String',VolTher);
set(handles.Disp_Mod4,'String',CmpTher1);
set(handles.Disp_Mod5,'String',CmpTher2);

set(handles.Disp_Obs1,'String',NamesCorr);
set(handles.Disp_Obs2,'String',PhasesXMap);
set(handles.Disp_Obs3,'String',VolXMap);
set(handles.Disp_Obs4,'String',CmpXMap1);
set(handles.Disp_Obs5,'String',CmpXMap2);

set(handles.Disp_Res1,'String',Report.Info);

set(handles.BinTextBinRes1,'String',[num2str(Report.QualityFactors(1),'%.0f'),' %']);
set(handles.BinTextBinRes2,'String',[num2str(Report.QualityFactors(2),'%.2f'),' %']);
set(handles.BinTextBinRes3,'String',[num2str(Report.QualityFactors(3),'%.2f'),' %']);

set(handles.BinTextBinRes4,'String',[num2str(Report.QualityFactors(4),'%.2f'),' %']);

ActivateZoomFigure1(hObject, eventdata, handles);

drawnow

return




% --- Executes on selection change in Res_AddCol2.
function Res_AddCol2_Callback(hObject, eventdata, handles)
%
UpdateLiveDisplayPanel(hObject, eventdata, handles);
return



% --- Executes on selection change in Res_AddCol1.
function Res_AddCol1_Callback(hObject, eventdata, handles)
%
UpdateLiveDisplayPanel(hObject, eventdata, handles);
return


% --- Executes on button press in PanButton_LIVE.
function PanButton_LIVE_Callback(hObject, eventdata, handles)
%

InitPanelDisp(3,handles); % LIVE
return

% --- Executes on button press in PanButton_MAP.
function PanButton_MAP_Callback(hObject, eventdata, handles)
%

InitPanelDisp(1,handles); % MAP

return


% --- Executes on button press in PanButton_RES.
function PanButton_RES_Callback(hObject, eventdata, handles)
%

InitPanelDisp(2,handles); % RESULTS

return



% -------------------------------------------------------------------------
%
%             #     #    # ##### ##### ####   ###  ##### #####
%            # #    # #  #   #     #   #   # #   #   #   #         
%           #####   #  # #   #     #   #   # #   #   #   ###      
%          #     #  #   ##   #     #   #   # #   #   #   #        
%         #       # #    #   #   ##### ####   ###    #   #####        
%
% -------------------------------------------------------------------------




% #########################################################################
%       ANT - Button to SELECT variables in ANTIDOTE
function BinPopUpVarPhaseList_Callback(hObject, eventdata, handles)
% 
Where = get(handles.BinPopUpVarPhaseList,'Value');
set(handles.BinPopUpPhaseList,'Value',Where);
BinPopUpPhaseList_Callback(hObject, eventdata, handles);
%BinWereWeAre(hObject, eventdata, handles);
return


% #########################################################################
%       ANT - Antidote recipes menu
function BinPopUpListeAntidotes_Callback(hObject, eventdata, handles)
% 

handles = Recipe_UpdateMenu(hObject, eventdata, handles);
BinPopUpVarPhaseList_Callback(hObject, eventdata, handles);

return

[WhereRecipe1,WhereRecipe2,WhereRecipe3,WhereRecipe4,WhereRecipe5,WhereRecipe6,WhereRecipe7,WhereRecipe8,WhereRecipe9,WhereRecipe10,WhereRecipe11,WhereRecipe12,WhereRecipe13,WhereRecipe14,WhereRecipe15,WhereRecipe16] = Antidote_RecipeDefinition(1);

WhatRecipe = get(handles.BinPopUpListeAntidotes,'Value');

BinPhaseDef = handles.BinPhaseDef;

set(handles.BinLabelAnt1,'String','Perm.');
set(handles.BinLabelAnt2,'String','dPx');
set(handles.BinTextAnt1,'String','100');
set(handles.BinTextAnt2,'String','40');

set(handles.push_loadSW,'Visible','off');
set(handles.check_textSW,'Visible','off');


if isequal(WhatRecipe,WhereRecipe4) || isequal(WhatRecipe,WhereRecipe5) || isequal(WhatRecipe,WhereRecipe6)
    % Update list and activate
    
    Compt=0;
    for i=1:length(BinPhaseDef)
        if BinPhaseDef(i).IsPhaseIn
            Compt = Compt+1;
            
            StrP = '';
            for E1 = 1:length(BinPhaseDef(i).ListVariAntidote);
                StrP = [StrP,char(BinPhaseDef(i).ListVariAntidote{E1}),'-'];%,'(',num2str(BinPhaseDef(i).ListWeightAntidote(E1)),')'];
            end
            StrP = StrP(1:end-1);
            ListNames{i} = [BinPhaseDef(i).DBMinName,'_',StrP];
        else
            ListNames{i} = '... not available ...';
        end
        
    end

    set(handles.BinPopUpVarPhaseList,'String',ListNames,'Enable','on','Value',get(handles.BinPopUpPhaseList,'Value'));
    BinPopUpVarPhaseList_Callback(hObject, eventdata, handles);
else
    set(handles.BinPopUpVarPhaseList,'String',{''},'Value',1,'Enable','off');
end

if isequal(WhatRecipe,4) || isequal(WhatRecipe,8)
    set(handles.BinTextAnt4,'Enable','on');
else
    set(handles.BinTextAnt4,'Enable','off');
end

if isequal(WhatRecipe,WhereRecipe7) 
    set(handles.BinTextAnt1,'Enable','on');
    set(handles.BinTextAnt2,'Enable','on');
    set(handles.BinTextAnt3,'Enable','off');
    
elseif isequal(WhatRecipe,WhereRecipe8)
    set(handles.BinTextAnt1,'Enable','on');
    set(handles.BinTextAnt2,'Enable','off');
    set(handles.BinTextAnt3,'Enable','on');
    
elseif isequal(WhatRecipe,WhereRecipe9)
    set(handles.BinTextAnt1,'Enable','on');
    set(handles.BinTextAnt2,'Enable','on');
    set(handles.BinTextAnt3,'Enable','on');
    
elseif isequal(WhatRecipe,WhereRecipe10)
    set(handles.BinTextAnt1,'Enable','off');
    set(handles.BinTextAnt2,'Enable','off');
    set(handles.BinTextAnt3,'Enable','off');
    
% elseif isequal(WhatRecipe,WhereRecipe11) %eduester19a floating window is now 14  
%     set(handles.BinTextAnt1,'Enable','on');
%     set(handles.BinTextAnt2,'Enable','on');
%     set(handles.BinTextAnt3,'Enable','off');
%     set(handles.BinLabelAnt1,'String','x_steps');
%     set(handles.BinLabelAnt2,'String','y_steps');
%     set(handles.BinTextAnt1,'String','3');
%     set(handles.BinTextAnt2,'String','3');
%     set(handles.check_textSW,'Visible','on');
%     set(handles.push_loadSW,'Visible','on');
       
 
elseif isequal(WhatRecipe,WhereRecipe11) %eduester19a scanning window is now 15  
    set(handles.BinTextAnt1,'Enable','on');
    set(handles.BinTextAnt2,'Enable','on');
    set(handles.BinTextAnt3,'Enable','off');
    set(handles.BinLabelAnt1,'String','x_steps');
    set(handles.BinLabelAnt2,'String','y_steps');
    set(handles.BinTextAnt1,'String','3');
    set(handles.BinTextAnt2,'String','3');
    set(handles.check_textSW,'Visible','on');
    set(handles.push_loadSW,'Visible','on');
   
elseif isequal(WhatRecipe,12) %eduester19a growing window is now 16
    set(handles.BinTextAnt1,'Enable','on');
    set(handles.BinTextAnt2,'Enable','off');
    set(handles.BinTextAnt3,'Enable','off');
    set(handles.BinLabelAnt1,'String','steps');
    set(handles.BinLabelAnt2,'String','y_steps');
    set(handles.BinTextAnt1,'String','10');
    %set(handles.BinTextAnt2,'String','3');
    set(handles.check_textSW,'Visible','on');
    set(handles.push_loadSW,'Visible','on');
else
    set(handles.BinTextAnt1,'Enable','off');
    set(handles.BinTextAnt2,'Enable','off');
    set(handles.BinTextAnt3,'Enable','off');
end

    

return


% #########################################################################
%       ANT - Call antidote options GuI
function BinButtonOptions_Callback(hObject, eventdata, handles)
%
XTTminimOptions();
return




















% goto antidote

% #########################################################################
%       ANT - RUN ANTIDOTE
function BinButtonRunAntidote_Callback(hObject, eventdata, handles)
%

% PL - 21.03.2019 
% [WhereRecipe1,WhereRecipe2,WhereRecipe3,WhereRecipe4,WhereRecipe5,WhereRecipe6,WhereRecipe7,WhereRecipe8,WhereRecipe9,WhereRecipe10,WhereRecipe11,WhereRecipe12,WhereRecipe13,WhereRecipe14,WhereRecipe15,WhereRecipe16] = Antidote_RecipeDefinition(1);

%
RunSWOT(1,handles);
AntidoteRecipes = get(handles.BinPopUpListeAntidotes,'String');
AntidoteMode = get(handles.BinPopUpListeAntidotes,'Value');

ButtonClean_Callback(hObject, eventdata, handles);

disp(' ')
disp(' ')
disp('- - - - - - - - - - - - - - - - - - - - - - - - - - -')
disp(['   >>> New ANTIDOTE job: ',datestr(now),'  <<<'])
disp(' ')

[WorkVariXMap] = GenerateResXMap(handles.BinPhaseDef);

% Load the options
load('MinimOptions.mat');

% Switch the panel to LIVE
InitPanelDisp(3,handles); % RESULTS

% Clear the live panel
FunctionClearLivePanel(handles);
drawnow


[RecipeIndex,RecipeFunctionDef] = Recipe_Indexing(1);

TheRecipeIndex = find(ismember(RecipeIndex,AntidoteMode));

if isempty(TheRecipeIndex)
    disp('No recipe selected ...')
    disp(' ')
    RunSWOT(0,handles);
    
    errordlg('You need to select a recipe to use Antidote','XThermoTools');
    return
end

TheRecipeFunctionName = RecipeFunctionDef{TheRecipeIndex};

eval(['[Output,Antidote_VARIABLES,handles] = ',TheRecipeFunctionName,'(AntidoteRecipes,AntidoteMode,WorkVariXMap,MinimOptions,handles);']);

% Error
if isequal(Output.Message,'Error')
    disp(' ')
    disp(' ANTIDOTE Job aborted ... (see message box) ');
    RunSWOT(0,handles);
    disp(' '),disp(' ')
    disp(['   >>> End ANTIDOTE job: ',datestr(now),'  <<<'])
    disp('- - - - - - - - - - - - - - - - - - - - - - - - - - -')
    disp(' ')
    return
end

% Important if the recipe changed the handles...
guidata(hObject, handles);

% Call Bingo?
if Output.WeCallBingo
    disp(' '), disp(' ')
    disp('>> ANTIDOTE calls BINGO ---->')
    BinButtonRunBingo_Callback(hObject, eventdata, handles);
end


% Save the last computation (Last_Antidote.mat)?
if Output.WeSaveWorkspace
    VariablesStruc = whos;       % this does not save the figures in MATLAB 2014b on
    Compt = 0;
    for i = 1:length(VariablesStruc)
        if ~isequal(VariablesStruc(i).name,'eventdata') && ~isequal(VariablesStruc(i).name,'hObject') && ~isequal(VariablesStruc(i).name,'handles')
            Compt = Compt+1;
            NewStruc(Compt).name = VariablesStruc(i).name;
        end
    end
    save('Last_Antidote.mat',NewStruc.name);
end



disp(' '),disp(' ')
disp(['   >>> End ANTIDOTE job: ',datestr(now),'  <<<'])
disp('- - - - - - - - - - - - - - - - - - - - - - - - - - -')
disp(' ')

RunSWOT(0,handles);
return







switch AntidoteMode
    
    case{1,5,9,13,18,22}
        
        
    %----------------------------------------------------------------------    
    %######################################################################
    case WhereRecipe1      
        
        
        
        
        
    %----------------------------------------------------------------------    
    %######################################################################    
    case WhereRecipe2   % MAPPING FUNCTION (fixed PT) 
        % goto recipe2
        
        

        
        
        
    %----------------------------------------------------------------------    
    %######################################################################  
    case {WhereRecipe3,WhereRecipe6}   % UNCERTAINTIES
        
        
        
        
        
    %----------------------------------------------------------------------    
    %######################################################################       
    case WhereRecipe4    % SINGLE PHASE

        
        
        
        
        
        
    %----------------------------------------------------------------------    
    %######################################################################
    case WhereRecipe5   % MAP SINGLE PHASE

   
        
        
        
    %----------------------------------------------------------------------
    %######################################################################    
    case {WhereRecipe7,WhereRecipe8,WhereRecipe9}     % UNCERTAINTY
        
        
        
        
        
        
    %----------------------------------------------------------------------
    %######################################################################         
    case WhereRecipe10 % Sliding window with Chemical Potentials (PL, 14.02.18, SWOT)
        
        
        ButtonClean_Callback(hObject, eventdata, handles);
        
        
        
        
    %----------------------------------------------------------------------
    %######################################################################         
    case WhereRecipe11 %eduester: Sliding window
        
        
        
        % <------------------------ end recipe 14 ------------------------>
                
        
        
    %----------------------------------------------------------------------
    %######################################################################  
    case WhereRecipe12  %   eduester: Growing window
        
        
        % <------------------------ end recipe 16 ------------------------>
        
     
    %----------------------------------------------------------------------    
    %######################################################################
    case {WhereRecipe13}    % Chemical Potential mapping  
        
        
        

        
        
        
    %----------------------------------------------------------------------    
    %######################################################################
    case {WhereRecipe14,WhereRecipe15,WhereRecipe16}    % Optimise H, O or C in a binary
        
        
        
        
        
        
end

VariablesStruc = whos;       % this does not save the figures in MATLAB 2014b on
Compt = 0;
for i = 1:length(VariablesStruc)
    if ~isequal(VariablesStruc(i).name,'eventdata') && ~isequal(VariablesStruc(i).name,'hObject') && ~isequal(VariablesStruc(i).name,'handles')
        Compt = Compt+1;
        NewStruc(Compt).name = VariablesStruc(i).name;
    end
end
save('Last_Antidote.mat',NewStruc.name);
%save Last_Antidote

disp(' '),disp(' ')
disp(['   >>> End ANTIDOTE job: ',datestr(now),'  <<<'])
disp('- - - - - - - - - - - - - - - - - - - - - - - - - - -')
disp(' ')

RunSWOT(0,handles);
return



        






% #########################################################################
%                                             (eduester - not tested by PL)
function uitable_sliding_CellSelectionCallback(hObject, eventdata, handles)
% hObject    handle to uitable_sliding (see GCBO)
% eventdata  structure with the following fields (see MATLAB.UI.CONTROL.TABLE)
%	Indices: row and column indices of the cell(s) currently selecteds
% handles    structure with handles and user data (see GUIDATA)



%-------------------------------------------
set(handles.axes_Live_PLOT1,'Visible','Off');
set(handles.axes_Live_PLOT2,'Visible','Off');
set(handles.axes_Live_PLOT3,'Visible','Off');
set(handles.axes_Live_PLOT4,'Visible','Off');
set(handles.axes_Live_PLOT5,'Visible','On');
axes(handles.axes_Live_PLOT5), cla


selected_cell =eventdata.Indices;
if isempty(selected_cell) 
    selected_cell = [1 1];
end
    
data_SW=get(handles.uitable_sliding,'Data');
indices=1:1:(size(data_SW,1));
txt=cell(size(data_SW,1),1);
txt2=txt;

nwindows=data_SW(:,1);
E4=data_SW(:,2);
A1=data_SW(:,3);
V1=data_SW(:,4);
C1=data_SW(:,5);
D_Temp=data_SW(:,6);
D_Press=data_SW(:,7);
xmin=data_SW(:,8);
ymin=data_SW(:,9);
xmax=data_SW(:,10);
ymax=data_SW(:,11);

ButtonClean_Callback(hObject, eventdata, handles);

if isempty(selected_cell) == 0
    selection=indices(unique(selected_cell(:,1)));
    for i=1:1:size(data_SW,1)
        txt{i}=['   #',num2str(nwindows(i)),' - ',num2str(round(E4(i),1)),'%'];
        txt2{i}=['  #',num2str(nwindows(i))];
    end
    % -------------------------------------------------------------------------
    % eduester
    axes(handles.axes_Live_PLOT5), hold on
    cla(handles.axes_Live_PLOT5)
    %cla reset
    plot(D_Temp,D_Press,'o','MarkerFaceColor','w','MarkerEdgeColor','k'), hold on
    plot(D_Temp(selection),D_Press(selection),'o','MarkerFaceColor','r','MarkerEdgeColor','k');
    if get(handles.check_textSW, 'Value') == 1
        text(D_Temp(selection),D_Press(selection),txt(selection));
    end
    eval(['TCi = [',get(handles.BinTextBinTCminmax,'String'),'];']);
    eval(['Pi = [',get(handles.BinTextBinPminmax,'String'),'];']);
    
    axis([TCi(1) TCi(end) Pi(1) Pi(end)]);
    xlabel('Temperature');
    ylabel('Pressure');
    drawnow
    %axis(AxisVal);
    hold off
      
    axes(handles.axes1), hold on
    plot(xmin(selection)+(xmax(selection)-xmin(selection))/2,ymin(selection)+(ymax(selection)-ymin(selection))/2,'ok','MarkerFaceColor','w','MarkerEdgeColor','k')
    %---------
    plot([xmin(max(selection)),xmax(max(selection))],[ymax(max(selection)),ymax(max(selection))],'-k','MarkerFaceColor','w','MarkerEdgeColor','k')
    plot([xmin(max(selection)),xmax(max(selection))],[ymin(max(selection)),ymin(max(selection))],'-k','MarkerFaceColor','w','MarkerEdgeColor','k')
    plot([xmin(max(selection)),xmin(max(selection))],[ymin(max(selection)),ymax(max(selection))],'-k','MarkerFaceColor','w','MarkerEdgeColor','k')
    plot([xmax(max(selection)),xmax(max(selection))],[ymin(max(selection)),ymax(max(selection))],'-k','MarkerFaceColor','w','MarkerEdgeColor','k')
    
    text(xmin(selection)+(xmax(selection)-xmin(selection))/2,ymin(selection)+(ymax(selection)-ymin(selection))/2, txt2(selection), 'Color', 'k');
    drawnow
% -------------------------------------------------------------------------
    if max(size(selection)) == 1
        % plot([xmin(selection),xmax(selection)],[ymax(selection),ymax(selection)],'-k','MarkerFaceColor','w','MarkerEdgeColor','k')
        % plot([xmin(selection),xmax(selection)],[ymin(selection),ymin(selection)],'-k','MarkerFaceColor','w','MarkerEdgeColor','k')
        % plot([xmin(selection),xmin(selection)],[ymin(selection),ymax(selection)],'-k','MarkerFaceColor','w','MarkerEdgeColor','k')
        % plot([xmax(selection),xmax(selection)],[ymin(selection),ymax(selection)],'-k','MarkerFaceColor','w','MarkerEdgeColor','k')
        
        %eduester
        % BinSet = SetBin(handles);
        % -------------------------------------------------------------------------
        % (1) SET initial variables for Theriak (BinSet)
        
        switch handles.BingoDefault.SelectedProgram
            case 1
                ProgPath = handles.BingoDefault.Theriak.Path;
                DefMin = handles.BingoDefault.Theriak.Database(get(handles.BinPopUpDatabase,'Value')).DefMin;
            case 2
                ProgPath = handles.BingoDefault.PerpleX.Path;
                DefMin = handles.BingoDefault.PerpleX.Database(get(handles.BinPopUpDatabase,'Value')).DefMin;
        end
        
        Databases = get(handles.BinPopUpDatabase,'String');
        TheSelDatabase = Databases{get(handles.BinPopUpDatabase,'Value')};
        
        %(2) -----------------------------------------------------------------------
        SelectedBinBulk = get(handles.BinPopUpBulkCompoList,'Value');
        TempBinBulk = handles.BinBulk(SelectedBinBulk) ;
        
        CoorSim=[xmin(selection) ymin(selection); xmax(selection) ymin(selection); xmax(selection) ymax(selection); xmin(selection) ymax(selection)];
        
        TempBinBulk.DomainXrefYref = CoorSim;
        [TempBinBulk] = BinUpdateBulkComposition(TempBinBulk,1,handles);
        Bulk = TempBinBulk.CompositionOriginal;
        set(handles.BinTextBulkCompo,'String',Bulk);
        set(handles.BinTextBulkCompo,'ForegroundColor',[0,0,0]);
        handles.BinBulk(SelectedBinBulk)=TempBinBulk;
        %new
        [ModPhaseDef] = BinButtonCalcPhaseFAST(handles);
        handles.BinPhaseDef = ModPhaseDef;
        
        [WorkVariXMap] = GenerateResXMap(handles.BinPhaseDef);
        % -------------------------------------------------------------------------
        % Options (to be integrated to the interface later)
        [BinSet] = SetBinFAST(ProgPath,DefMin,TheSelDatabase,Bulk,handles.BinGfDef);
        
        set(handles.BinTextBinTC,'String',num2str(D_Temp(selection)));
        set(handles.BinTextBinP,'String',num2str(D_Press(selection)));
        
        
    end
end

set(handles.axes1,'Visible','On');
Karte=gca;
fig=figure(33);
new_handle = copyobj(Karte,fig);
colormap(fig,[0,0,0;RdYlBu(64)]), colorbar
return


% #########################################################################
%       ANT - LOAD a scanning window (eduester - not tested by PL)
function push_loadSW_Callback(hObject, eventdata, handles)
%
[filename, pathname] = uigetfile({'SW_data*.txt', 'Sliding Window File (SW_data.txt)'},'Pick a file');

if ~filename
    return
end

SW_data=load([pathname,filename]);
set(handles.uitable_sliding,'Data',SW_data);
set(handles.uitable_sliding,'Visible','on');


return







% -------------------------------------------------------------------------
% -------------------------------------------------------------------------
% -------------------------------------------------------------------------

% BELOW ARE THE SUBTOUTINES NOT DIRECTLY USED BUT REQUIRED FOR THE GUI ...

% -------------------------------------------------------------------------
% -------------------------------------------------------------------------
% -------------------------------------------------------------------------


% --- Executes on button press in BinCheckBoxPart1.
function BinCheckBoxPart1_Callback(hObject, eventdata, handles)
% 
return

% --- Executes during object creation, after setting all properties.
function BinTextBulkCompo_CreateFcn(hObject, eventdata, handles)
% hObject    handle to BinTextBulkCompo (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in BinCheckBoxPart2.
function BinCheckBoxPart2_Callback(hObject, eventdata, handles)
%

% --- Executes during object creation, after setting all properties.
function BinPopUpBulkCompoList_CreateFcn(hObject, eventdata, handles)
% hObject    handle to BinPopUpBulkCompoList (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


function BinTextChemicalSyst_Callback(hObject, eventdata, handles)
%
return

% --- Executes during object creation, after setting all properties.
function BinTextChemicalSyst_CreateFcn(hObject, eventdata, handles)
% hObject    handle to BinTextChemicalSyst (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

% --- Executes during object creation, after setting all properties.
function BinPopUpGrpOfPhase_CreateFcn(hObject, eventdata, handles)
% hObject    handle to BinPopUpGrpOfPhase (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

% --- Executes during object creation, after setting all properties.
function BinTextPhaseTheriakName_CreateFcn(hObject, eventdata, handles)
% hObject    handle to BinTextPhaseTheriakName (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

function BinTextPhaseTheriakName_Callback(hObject, eventdata, handles)
% hObject    handle to BinTextPhaseTheriakName (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of BinTextPhaseTheriakName as text
%        str2double(get(hObject,'String')) returns contents of BinTextPhaseTheriakName as a double

% --- Executes during object creation, after setting all properties.
function BinPopUpPhaseList_CreateFcn(hObject, eventdata, handles)
% hObject    handle to BinPopUpPhaseList (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

% --- Executes on button press in BinCheckBoxPart3.
function BinCheckBoxPart3_Callback(hObject, eventdata, handles)
% hObject    handle to BinCheckBoxPart3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of BinCheckBoxPart3

% --- Executes during object creation, after setting all properties.
function BinTextPhaseTheriakVolProp_CreateFcn(hObject, eventdata, handles)
% hObject    handle to BinTextPhaseTheriakVolProp (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

function BinTextPhaseTheriakVolProp_Callback(hObject, eventdata, handles)
% hObject    handle to BinTextPhaseTheriakVolProp (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of BinTextPhaseTheriakVolProp as text
%        str2double(get(hObject,'String')) returns contents of BinTextPhaseTheriakVolProp as a double

% --- Executes on button press in BinCheckBoxPart4.
function BinCheckBoxPart4_Callback(hObject, eventdata, handles)
% hObject    handle to BinCheckBoxPart4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of BinCheckBoxPart4

% --- Executes during object creation, after setting all properties.
function BinTextBinTC_CreateFcn(hObject, eventdata, handles)
% hObject    handle to BinTextBinTC (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes during object creation, after setting all properties.
function BinTextBinP_CreateFcn(hObject, eventdata, handles)
% hObject    handle to BinTextBinP (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes during object creation, after setting all properties.
function BinTextBinRes1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to BinTextBinRes1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


function BinTextBinRes1_Callback(hObject, eventdata, handles)
% hObject    handle to BinTextBinRes1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of BinTextBinRes1 as text
%        str2double(get(hObject,'String')) returns contents of BinTextBinRes1 as a double


function BinTextBinRes2_Callback(hObject, eventdata, handles)
% hObject    handle to BinTextBinRes2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of BinTextBinRes2 as text
%        str2double(get(hObject,'String')) returns contents of BinTextBinRes2 as a double


% --- Executes during object creation, after setting all properties.
function BinTextBinRes2_CreateFcn(hObject, eventdata, handles)
% hObject    handle to BinTextBinRes2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function BinTextBinRes3_Callback(hObject, eventdata, handles)
% hObject    handle to BinTextBinRes3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of BinTextBinRes3 as text
%        str2double(get(hObject,'String')) returns contents of BinTextBinRes3 as a double


% --- Executes during object creation, after setting all properties.
function BinTextBinRes3_CreateFcn(hObject, eventdata, handles)
% hObject    handle to BinTextBinRes3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


function BinTextBinRes4_Callback(hObject, eventdata, handles)
% hObject    handle to BinTextBinRes4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of BinTextBinRes4 as text
%        str2double(get(hObject,'String')) returns contents of BinTextBinRes4 as a double


% --- Executes during object creation, after setting all properties.
function BinTextBinRes4_CreateFcn(hObject, eventdata, handles)
% hObject    handle to BinTextBinRes4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in BinCheckBoxPart5.
function BinCheckBoxPart5_Callback(hObject, eventdata, handles)
% hObject    handle to BinCheckBoxPart5 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of BinCheckBoxPart5


% --- Executes during object creation, after setting all properties.
function BinPopUpListeAntidotes_CreateFcn(hObject, eventdata, handles)
% hObject    handle to BinPopUpListeAntidotes (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


function BinTextBinTCminmax_Callback(hObject, eventdata, handles)
% hObject    handle to BinTextBinTCminmax (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of BinTextBinTCminmax as text
%        str2double(get(hObject,'String')) returns contents of BinTextBinTCminmax as a double


% --- Executes during object creation, after setting all properties.
function BinTextBinTCminmax_CreateFcn(hObject, eventdata, handles)
% hObject    handle to BinTextBinTCminmax (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function BinTextBinPminmax_Callback(hObject, eventdata, handles)
% hObject    handle to BinTextBinPminmax (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of BinTextBinPminmax as text
%        str2double(get(hObject,'String')) returns contents of BinTextBinPminmax as a double


% --- Executes during object creation, after setting all properties.
function BinTextBinPminmax_CreateFcn(hObject, eventdata, handles)
% hObject    handle to BinTextBinPminmax (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in SWOT.
function SWOT_Callback(hObject, eventdata, handles)
% hObject    handle to SWOT (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --- Executes during object creation, after setting all properties.
function BinPopUpProgram_CreateFcn(hObject, eventdata, handles)
% hObject    handle to BinPopUpProgram (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes during object creation, after setting all properties.
function BinPopUpDatabase_CreateFcn(hObject, eventdata, handles)
% hObject    handle to BinPopUpDatabase (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes during object creation, after setting all properties.
function BinPopUpVarPhaseList_CreateFcn(hObject, eventdata, handles)
% hObject    handle to BinPopUpVarPhaseList (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


function BinTextBinTC_Callback(hObject, eventdata, handles)
%


return


function BinTextBinP_Callback(hObject, eventdata, handles)
%


return



function BinTextAnt2_Callback(hObject, eventdata, handles)
% hObject    handle to BinTextAnt2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of BinTextAnt2 as text
%        str2double(get(hObject,'String')) returns contents of BinTextAnt2 as a double


% --- Executes during object creation, after setting all properties.
function BinTextAnt2_CreateFcn(hObject, eventdata, handles)
% hObject    handle to BinTextAnt2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function BinTextAnt3_Callback(hObject, eventdata, handles)
% hObject    handle to BinTextAnt3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of BinTextAnt3 as text
%        str2double(get(hObject,'String')) returns contents of BinTextAnt3 as a double


% --- Executes during object creation, after setting all properties.
function BinTextAnt3_CreateFcn(hObject, eventdata, handles)
% hObject    handle to BinTextAnt3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function BinTextAnt1_Callback(hObject, eventdata, handles)
% hObject    handle to BinTextAnt1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of BinTextAnt1 as text
%        str2double(get(hObject,'String')) returns contents of BinTextAnt1 as a double


% --- Executes during object creation, after setting all properties.
function BinTextAnt1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to BinTextAnt1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function BinTextAnt4_Callback(hObject, eventdata, handles)
% hObject    handle to BinTextAnt4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of BinTextAnt4 as text
%        str2double(get(hObject,'String')) returns contents of BinTextAnt4 as a double


% --- Executes during object creation, after setting all properties.
function BinTextAnt4_CreateFcn(hObject, eventdata, handles)
% hObject    handle to BinTextAnt4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes during object creation, after setting all properties.
function ResPopUpListReport_CreateFcn(hObject, eventdata, handles)
% hObject    handle to ResPopUpListReport (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in BINGOANTIDOTE.
function BINGOANTIDOTE_Callback(hObject, eventdata, handles)
% 

% --- Executes during object creation, after setting all properties.
function Res_AddCol2_CreateFcn(hObject, eventdata, handles)
% hObject    handle to Res_AddCol2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes during object creation, after setting all properties.
function Res_AddCol1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to Res_AddCol1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end










% -------------------------------------------------------------------------
%
%                 ##### #   # #   # ##### ##### ##### #
%                   #   ##  #  # #    #   #   # #   # #
%                   #   # # #  # #    #   #   # #   # #
%                   #   #  ##   #     #   #   # #   # #
%                 ##### #   #   #     #   ##### ##### #####
%
% -------------------------------------------------------------------------


% #########################################################################
%       INV - Were We are (1.1.1)
function InvWereWeAre(hObject, eventdata, handles)


% [1] Part 1 _ Thermodynamic data and models

if sum(handles.InvWere(1).WeAre) == 2
    set(handles.InvCheckBoxPart1,'Value',1);
    OnEst = 2;
else
    set(handles.InvCheckBoxPart1,'Value',0);
    OnEst = 1;
end


% On Est ???

if handles.InvWere(2).WeAre
    OnEst = 2;
end
if handles.InvWere(3).WeAre
    OnEst = 3;
end
if handles.InvWere(4).WeAre
    OnEst = 4;
end
if handles.InvWere(5).WeAre
    OnEst = 5;
end

% [2] Part 2 _ Chemical System

if OnEst >= 2
    
    set(handles.InvCheckBoxPart2,'enable','inactive');
    set(handles.InvTextChemicalSyst,'enable','inactive');
    set(handles.InvButtonChemicalSyst,'enable','on');
    
    if handles.InvWere(2).WeAre == 1
        set(handles.InvCheckBoxPart2,'Value',1);
        OnEst = 3;
    else
        set(handles.InvCheckBoxPart2,'Value',0);
    end
    
    
else
    set(handles.InvCheckBoxPart2,'enable','off');
    %set(handles.InvTextChemicalSyst,'enable','off');
    set(handles.InvButtonChemicalSyst,'enable','off');
end




% [3] Part 3 _ Assemblage

if OnEst >= 3
    
    % Desactivate the buttons of 1 (2 is done). 
    set(handles.InvButtonThermoDataSet,'enable','off');
    set(handles.InvButtonThermoSSmodels,'enable','off');
    
    set(handles.InvCheckBoxPart3,'enable','inactive');
    set(handles.InvTextAssemblageAll,'enable','on');
    set(handles.InvButtonAssemblage,'enable','on');
    
    set(handles.InvTextAssemblageAll,'backgroundColor',[0.2471,0.2471,0.2471])
    
    if handles.InvWere(3).WeAre == 1
        set(handles.InvCheckBoxPart3,'Value',1);
        OnEst = 4;
    else
        set(handles.InvCheckBoxPart3,'Value',0);
    end
    
    
else
    set(handles.InvCheckBoxPart3,'enable','off');
    set(handles.InvTextAssemblageAll,'enable','off');
    set(handles.InvButtonAssemblage,'enable','off');
    
    set(handles.InvTextAssemblageAll,'backgroundColor',[0.65,0.65,0.65])

end


% [4] Part 4 _ Reactions

if OnEst >= 4    
    set(handles.InvReacWaitBar1,'backgroundColor',[0,0,0])
    
    set(handles.InvCheckBoxPart4,'enable','inactive');
    set(handles.InvTextReactions,'enable','inactive');
    set(handles.InvButtonFindReactions,'enable','on');
    
    set(handles.InvButtonLoadReactions,'enable','on');
    
    set(handles.InvTextFileReactions,'enable','inactive');
    
    
    
    if handles.InvWere(4).WeAre == 1 
        set(handles.InvButtonChemicalSyst,'enable','off');
        %set(handles.InvButtonAssemblage,'enable','off');
            
        set(handles.InvCheckBoxPart4,'Value',1);
        OnEst = 5;
        
        set(handles.InvButtonPrintReactions,'enable','on');
        set(handles.InvButtonSaveReactions,'enable','on');
        
    else
        set(handles.InvCheckBoxPart4,'Value',0);
        
        set(handles.InvButtonPrintReactions,'enable','off');
        set(handles.InvButtonSaveReactions,'enable','off');
        
        %set(handles.InvTextFileReactions,'enable','off');
        
        set(handles.InvReacWaitBar3,'Position',[14.167 24.167 0.0001 0.9]);
        set(handles.InvTextReactions,'String','');
        
    end
    
else
    set(handles.InvCheckBoxPart4,'enable','off');
    %set(handles.InvTextReactions,'enable','off');
    set(handles.InvButtonFindReactions,'enable','off');
    
    set(handles.InvButtonLoadReactions,'enable','off');
    
    set(handles.InvButtonPrintReactions,'enable','off');
    set(handles.InvButtonSaveReactions,'enable','off');
    
    set(handles.InvTextFileReactions,'String','');
    
    %set(handles.InvTextFileReactions,'enable','off');
    
    set(handles.InvReacWaitBar1,'backgroundColor',[0.35,0.35,0.35])
    
end


if OnEst == 5
    
    set(handles.InvButtonSelectCompositions,'enable','on');
    
    if handles.InvWere(5).WeAre == 1
        set(handles.InvPopUpCompositions,'enable','on');
        set(handles.InvCheckBoxPart5,'enable','inactive','value',1);
        
        if isequal(length(handles.Compositions),length(handles.Computation.SSmodelsComp.ListModel))
            % means all the compositions have been selected / defined
            set(handles.InvButtonMultiSFcalcul,'enable','on');
            
            % OnEst == 6;
            
        else
            set(handles.InvButtonMultiSFcalcul,'enable','off');
        end
                
    else
        set(handles.InvPopUpCompositions,'enable','off');
        set(handles.InvCheckBoxPart5,'enable','inactive','value',0);
        set(handles.InvButtonMultiSFcalcul,'enable','off');
    end
    
else
    set(handles.InvCheckBoxPart5,'enable','off','value',0);
    set(handles.InvButtonSelectCompositions,'enable','off');
    set(handles.InvButtonMultiCompositions,'visible','off');
    set(handles.InvPopUpCompositions,'enable','off');
    
    set(handles.InvButtonMultiSFcalcul,'enable','off');
    
    
    
end

return



% #########################################################################
%       INV - Select DataSet file (1.1.1)
function InvButtonThermoDataSet_Callback(hObject, eventdata, handles)

LocBase = handles.LocBase;

[filename, pathname, filterindex] = uigetfile({'*.txt',  'TXT Files (*.txt)'}, ...  
    'Pick a ThermoData file',[LocBase,'/ThermoData/INVTOOL_ThermoData_JUN92.txt']);

if ~filterindex
    return
end

% Read the file
ThermoDataFile = [pathname,filename];
[ElData,ThData] = Function_ReadThermoData(ThermoDataFile);

handles.ElData = ElData;
handles.ThData = ThData;


handles.InvWere(1).WeAre(1) = 1;

set(handles.InvTextThermoDataSet,'String',char(filename),'UserData',[pathname,filename]);


guidata(hObject, handles);
InvWereWeAre(hObject, eventdata, handles)
return



% #########################################################################
%       INV - Select SS models file (1.1.1)
function InvButtonThermoSSmodels_Callback(hObject, eventdata, handles)

LocBase = handles.LocBase;

[filename, pathname, filterindex] = uigetfile({'*.txt',  'TXT Files (*.txt)'}, ...  
    'Pick a SS models file',[LocBase,'/ThermoData/INVTOOL_SSModels_JUN92.txt']);

if ~filterindex
    return
end

% Read the file
SSDataFile = [pathname,filename];
[SSmodels] = Function_ReadSSData(SSDataFile);

handles.SSmodels = SSmodels;


handles.InvWere(1).WeAre(2) = 1;

set(handles.InvTextThermoSSmodels,'String',char(filename),'UserData',[pathname,filename]);


guidata(hObject, handles);
InvWereWeAre(hObject, eventdata, handles)
return



% #########################################################################
%       INV - Set the Chemical system (1.1.1)
function InvButtonChemicalSyst_Callback(hObject, eventdata, handles)

ElData = handles.ElData;
keyboard
[Selected] = XTTselectElem(ElData);

if isequal(Selected,0)
    handles.InvWere(2).WeAre = 0;
    guidata(hObject, handles);
    InvWereWeAre(hObject, eventdata, handles)
    return
end

ElData.selected = Selected;
handles.ElData = ElData;

ListString = '';
for i=1:length(ElData.selected)
    if ElData.selected(i)
        ListString = [ListString,'-',char(ElData.listElem{i})];
    end
end
ListString = ListString(2:end);

set(handles.InvTextChemicalSyst,'String',ListString);

handles.InvWere(2).WeAre = 1;

guidata(hObject, handles);
InvWereWeAre(hObject, eventdata, handles)
return



% #########################################################################
%       INV - Set the assemblage (1.1.1)
function InvButtonAssemblage_Callback(hObject, eventdata, handles)

ElData = handles.ElData;
ThData = handles.ThData;
SSmodels = handles.SSmodels;

AssemblageCode = handles.AssemblageCode;

[Computation.SSmodelsComp,Computation.ThDataComp,Computation.MinCompoMat,AssemblageCode] ...
    = XTTselectAssemblage(ElData,ThData,SSmodels,AssemblageCode); 


if isequal(Computation.ThDataComp,'')    
    handles.InvWere(3).WeAre = 0;
    set(handles.InvTextAssemblageAll,'String','');
    guidata(hObject, handles);
    InvWereWeAre(hObject, eventdata, handles)
    return
end

set(handles.InvTextFileReactions,'String','');

Carac = 0;
Ligne = 1;
TheDisplay{1} = '';

for i=1:length(Computation.ThDataComp)
    TheName = Computation.ThDataComp(i).name;
    Carac = Carac + length(TheName);
    if Carac > 60
        Ligne = Ligne+1;
        TheDisplay{Ligne} = '';
        ThePreviousText = TheDisplay{Ligne-1};
        TheDisplay{Ligne-1} = ThePreviousText(1:end-3);
        Carac = 0;
    end
    TheDisplay{Ligne} = [char(TheDisplay{Ligne}),char(TheName),' + '];
end

ThePreviousText = TheDisplay{Ligne};
TheDisplay{Ligne} = ThePreviousText(1:end-3);

handles.Computation = Computation;

set(handles.InvTextAssemblageAll,'String',TheDisplay);

handles.InvWere(3).WeAre = 1;
handles.InvWere(4).WeAre = 0;
handles.InvWere(5).WeAre = 0;

% Create the default variable ComputationCode:
Labels = 1:size(AssemblageCode,1)-1;
ComputationCode = [Labels',zeros(size(AssemblageCode,1)-1,5)];
for i=1:size(AssemblageCode,1)-1                       % last line is empty
    ComputationCode(i,2) = AssemblageCode(i,2);
    switch ComputationCode(i,2)
        
        case 2
            ComputationCode(i,3:6) = [1,1,1,0];
            
        case 3
            ComputationCode(i,3:6) = [0,0,1,1];
            
    end
end

handles.ComputationCode = ComputationCode;

% We delete the reactions (if we change the assemblage). 
handles.Computation.Reac = [];
handles.Compositions = [];

handles.AssemblageCode = AssemblageCode;

guidata(hObject, handles);
InvWereWeAre(hObject, eventdata, handles)
return



% #########################################################################
%       INV - Find Reactions (1.1.1)
function InvButtonFindReactions_Callback(hObject, eventdata, handles)

set(handles.InvTextFileReactions,'String','... please wait');

ElData = handles.ElData;
Computation = handles.Computation;

% [1]  List of minerals  ------------ 
for i=1:length(Computation.ThDataComp)
    ListMineralFin{i} = Computation.ThDataComp(i).name;
end

% [2]  Composition Matrix  ------------ 
MatCompo = Computation.MinCompoMat;
LesInd = [1:length(MatCompo(:,1))];

% [3]  WaitBar initialization  ------------
for i=2:length(LesInd)
    TheNbReacTest(i) = size(nchoosek(LesInd,i),1);
end
NbTest = sum(TheNbReacTest);
 
TheLim = NbTest/20;
set(handles.InvReacWaitBar3,'Position',[14.167 24.167 0.0001 0.9]);
drawnow


% [4] Search all the reactions  ------------
lesCoefParReaction = zeros(300,length(LesInd));
lesMinParReaction = zeros(300,length(LesInd));

%tic
nbReactSauvees = 0;
TheTest = 0;
TheTestTot = 0;
for i=2:length(LesInd)
    
    TheCombiReac = nchoosek(LesInd,i);
    NbCombiReac = length(TheCombiReac(:,1));
    
    
    for j=1:NbCombiReac
        LaCombiATester = TheCombiReac(j,:);
        Mineralo = MatCompo(LaCombiATester,:)';
        
        % colorbar
        TheTest= TheTest+ 1;
        TheTestTot = TheTestTot+1;
        if TheTest > TheLim
            TheTest = 0;
            Percent = TheTestTot/NbTest;
            set(handles.InvReacWaitBar3,'Position',[14.167 24.167 13.80*Percent 0.9]);
            drawnow
        end
        
        %if i==7 && isequal(LaCombiATester,[2,4,5,6,8,9,10])
            %keyboard
        %end
        
        
        if rank(Mineralo) == i-1;

            [numCoefsReact, denCoefsReact] = rat(null(Mineralo,'r'));
            lcmDenom=denCoefsReact(1); 
            for iii=2:i
                lcmDenom=lcm(lcmDenom,denCoefsReact(iii)); 
            end
            CoefsReact = numCoefsReact ./ denCoefsReact * lcmDenom;
            
            if all(CoefsReact)
                nbReactSauvees = nbReactSauvees + 1;
                lesMinParReaction(nbReactSauvees,1:i) = LaCombiATester;
                lesCoefParReaction(nbReactSauvees,LaCombiATester) = CoefsReact';
                
                set(handles.InvTextReactions,'String',char(num2str(nbReactSauvees)));
                drawnow
                
            end
                        
        end        
    end
end
%toc 


% [5] Check if there is reaction or not  ------------
if ~nbReactSauvees
    set(handles.InvTextReactions,'String',['0 (0)']);
    drawnow
    warndlg(['No reactions found for this assemblage'],'Warning');
    return
end

lesCoefParReaction = lesCoefParReaction(1:nbReactSauvees,:);
lesMinParReaction = lesMinParReaction(1:nbReactSauvees,:);


% [6] Update Wait Bar  ------------
set(handles.InvReacWaitBar3,'Position',[14.167 24.167 13.80 0.9]);


% [7] Search the independent reactions  ------------
MatCompoClean = MatCompo(:,find(sum(MatCompo,1)));     % All the required elements (no more)
B0 = diag(ones(1,size(MatCompoClean,1)));

[A1,B1] = GaussJordanReduction(MatCompoClean,B0);

Compt = 0;
for i=1:size(A1,1)
    if isempty(find(A1(i,:)))
        Compt = Compt+1;
        TheLin(Compt) = i;
        
        [numCoefsReact, denCoefsReact] = rat(B1(i,:));
        lcmDenom=denCoefsReact(1); 
        for iii=2:length(numCoefsReact)
            lcmDenom=lcm(lcmDenom,denCoefsReact(iii)); 
        end
        mesCoef = numCoefsReact ./ denCoefsReact * lcmDenom;
        
        CoefReactIndep(Compt,:) = mesCoef;
        
    end
end

NbReacIndep = length(TheLin);

lesReactionsIndep = cell(size(B1(TheLin,:),1),1);
for iReact=1:NbReacIndep
    mesCoefStoe = CoefReactIndep(iReact,:);
    
    iMinDebut=1; 
    while ~mesCoefStoe(iMinDebut), iMinDebut=iMinDebut+1; end
    
    leCoef=mesCoefStoe(iMinDebut);    
    signe='-  '; 
    signe=signe(2+sign(leCoef));
    
    if abs(leCoef)==1
        maReaction = [signe,' ',ListMineralFin{iMinDebut}];
    else
        maReaction = [signe,' ',int2str(abs(leCoef)),' ',ListMineralFin{iMinDebut}];
    end
    for iMin=iMinDebut+1:length(LesInd)

        leCoef=mesCoefStoe(iMin);
        if leCoef
            signe='- +'; signe=signe(2+sign(leCoef));
            if abs(leCoef)==1
                maReaction = [maReaction,' ',signe,' ',ListMineralFin{iMin}];
            else
                maReaction = [maReaction,' ',signe,' ',int2str(abs(leCoef)),' ',ListMineralFin{iMin}];
            end
        end,
    end, 
    lesReactionsIndep{iReact} = maReaction;
    lesCoefsIndep(iReact,:) = mesCoefStoe;
end

%MatCompoClean = MatCompo(:,find(sum(MatCompo,1)));
%NbReacIndep = size(MatCompoClean,1) - size(MatCompoClean,2);    % Theoritical not always equal to 
                                                                 % the result of the Spear's method !!!!
set(handles.InvTextReactions,'String',[char(num2str(nbReactSauvees)),' (',char(num2str(NbReacIndep)),')']);
drawnow


% [8] Final treatement of all the reactions  ------------
lesReactions = cell(size(lesMinParReaction,1),1);
IndiceIndependent = zeros(size(lesMinParReaction,1),1);

for iReact=1:nbReactSauvees 
    mesCoefStoe=lesCoefParReaction(iReact,:);
    iMinDebut=1; 
    while ~mesCoefStoe(iMinDebut), iMinDebut=iMinDebut+1; end
    
    leCoef=mesCoefStoe(iMinDebut);    
    signe='-  '; 
    signe=signe(2+sign(leCoef));
    
    if abs(leCoef)==1
        maReaction = [signe,' ',ListMineralFin{iMinDebut}];
    else
        maReaction = [signe,' ',int2str(abs(leCoef)),' ',ListMineralFin{iMinDebut}];
    end
    for iMin=iMinDebut+1:length(LesInd)

        leCoef=mesCoefStoe(iMin);
        if leCoef
            signe='- +'; signe=signe(2+sign(leCoef));
            if abs(leCoef)==1
                maReaction = [maReaction,' ',signe,' ',ListMineralFin{iMin}];
            else
                maReaction = [maReaction,' ',signe,' ',int2str(abs(leCoef)),' ',ListMineralFin{iMin}];
            end
        end,
    end, 
    lesReactions{iReact} = maReaction;
    
    if ismember(maReaction,lesReactionsIndep)
        % So this reaction is in the independent set. 
        IndiceIndependent(iReact) = 1;
    end
end 


% [7] Final save in the variable Computation.Reac  ------------
for i=1:nbReactSauvees
    OuMin = find(lesMinParReaction(i,:));
    NbMin = length(OuMin);
    LesMin = ListMineralFin((lesMinParReaction(i,OuMin)));
    
    OuCoef = find(lesCoefParReaction(i,:));
    LesCoef = lesCoefParReaction(i,OuCoef);
    
    Computation.Reac(i).ref = i;
    Computation.Reac(i).name = char(lesReactions(i));
    Computation.Reac(i).minerals = LesMin;
    Computation.Reac(i).coef = LesCoef; 
    Computation.Reac(i).indep = IndiceIndependent(i);
end


% [8] Check if the independent reactions are in the list !!!
if sum(IndiceIndependent) ~= NbReacIndep
    Compt = i;
    for i = 1:length(lesReactionsIndep)
        maReactionIndep = lesReactionsIndep{i};
        if ~ismember(maReactionIndep,lesReactions)
            Compt = Compt+1;
            Computation.Reac(Compt).ref = Compt;
            Computation.Reac(Compt).name = char(maReactionIndep);
            
            ComptSize = 0;
            for j=1:length(lesCoefsIndep(i,:));
                if lesCoefsIndep(i,j)
                    ComptSize = ComptSize+1;
                    Computation.Reac(Compt).coef(ComptSize) = lesCoefsIndep(i,j);
                    Computation.Reac(Compt).minerals{ComptSize} = ListMineralFin{j};
                end
            end
            
            Computation.Reac(Compt).indep = 1;
            
        end
    end
    set(handles.InvTextReactions,'String',[char(num2str(length(Computation.Reac))),' (',char(num2str(NbReacIndep)),')']);
    drawnow
end


set(handles.InvTextFileReactions,'String','... not saved');
handles.InvWere(4).WeAre = 1;
handles.InvWere(5).WeAre = 0;

handles.Computation = Computation;
guidata(hObject, handles);
InvWereWeAre(hObject, eventdata, handles)
return


% #########################################################################
%       INV - GaussJordanReduction to find independent reactions (1.1.1)
function [A1,B1] = GaussJordanReduction(varargin)
% -----
%
%               [A,B] = GaussJordanReduction(A0,B0)
%
% This function produces the reduced row echelon form of B and the
% corresponding A matrix by preserving:
%
%                          A * x = B * y
%
% Modified by P. Lanari (24.07.13); Created by Marco Burn. 
%

A0 = varargin{1};
B0 = varargin{2};

if ~isequal(size(A0,1),size(B0,1)) 
    error('size(A0,1) and size(B0,1) should be the same')
end

A =A0;
B = B0;
A1 = A;
B1 = B;

msize = size(A0);
rows =  msize(1,1);
columns = msize(1,2);


for i = 1:rows 
        j=i;
        if j>columns 
            break;
        end
        
        while A(i,j) == 0
            i3 = i;
            while i3 <= rows &&  0 == A(i3, j) 
                i3 = i3 + 1;
            end
            
            
            if i3>rows
                break;

            end
            for j1 = 1:columns
                                 
                A1(i,j1) = A(i3, j1);
                B1(i,1:rows)= B(i3, 1:rows);
                A1(i3,j1) = A(i, j1);
                B1(i3,1:rows)= B(i, 1:rows);
                
            end
  
            A = A1;
            B = B1;
        end
    
            A = A1;
            B = B1;
            i2 = i+1;
            
            while i2 <= rows 
               if A(i2,j) ~= 0 
                  factor2 = -A(i2,j)/A(j,j);
                  if j>columns 
                    break;
                  end
                  for j2 = j:columns
                           A1(i2, j2) = A(i2, j2)+ factor2 * A(i,j2);
                           for x=1:rows
                                   B1(i2, x) = B(i2, x) + factor2 * B(i, x);
                           end    
                  end
                  A = A1;
                  B = B1;
               end
               i2=i2+1; 
            end
end
return



% #########################################################################
%       INV - Print Reactions (1.1.1)
function InvButtonPrintReactions_Callback(hObject, eventdata, handles)

disp(' ')
disp(' ')
disp(' - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -')

Computation = handles.Computation;

for i=1:length(Computation.Reac)
    if Computation.Reac(i).indep == 1
        disp(['** ',char(Computation.Reac(i).name)])
    else
        disp(['   ',char(Computation.Reac(i).name)])
    end
end

disp(' - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -')
disp(' ')
disp(' ')
return



% #########################################################################
%       INV - Save Reactions (1.1.1)
function InvButtonSaveReactions_Callback(hObject, eventdata, handles)

if ~isequal(exist('XTT_Reactions'),7)
    mkdir('XTT_Reactions');
end

[filename, pathname, filterindex] = uiputfile( ...
       {'*.txt','TXT-files (*.txt)'}, ...
        'Export reaction(s) as', 'XTT_Reactions/Untitled.txt');

    
if ~filename, return, end

Computation = handles.Computation;


fid = fopen([pathname,filename],'w');
fprintf(fid,'%s\n\n', 'Reaction(s) exported:');
for i = 1:length(Computation.Reac)
    if Computation.Reac(i).indep == 1
        fprintf(fid,'%s\n',['>** ',char(int2str(i))]);
    else
        fprintf(fid,'%s\n',['>.. ',char(int2str(i))]);
    end
    
    fprintf(fid,'%s\n',char(Computation.Reac(i).name));
    for j = 1:length(Computation.Reac(i).minerals) - 1
        fprintf(fid,'%s\t',char(Computation.Reac(i).minerals{j}));
    end
    fprintf(fid,'%s\n',char(Computation.Reac(i).minerals{end}));
    fprintf(fid,'%s\n\n',char(num2str(Computation.Reac(i).coef)));
end

fprintf(fid,'%s','END');
fclose(fid);

set(handles.InvTextFileReactions,'String',filename);


return



% #########################################################################
%       INV - Load Reactions (1.1.1)
function InvButtonLoadReactions_Callback(hObject, eventdata, handles)

[filename, pathname, filterindex] = uigetfile( ...
	{'*.txt','TXT-files (*.txt)'}, ...
	'Pick up reaction file', 'XTT_Reactions/Untitled.txt');

if ~filterindex
    return
end

Computation = handles.Computation;
Computation.Reac = [];

fid = fopen([pathname,filename]);
Compt = 0;
ComptIndep = 0;

while 1
    lalign = fgetl(fid);
    
    if length(lalign) > 1
        if strcmp('END',char(lalign))
            break
        end
        
        if lalign(1) == '>'
            % Alors on ajoute une reaction
            Compt = Compt+1;
            Computation.Reac(Compt).ref = str2num(lalign(5:end));
            
            if isequal(lalign(2:3),'**')
                Computation.Reac(Compt).indep = 1;
                ComptIndep = ComptIndep+1;
            else
                Computation.Reac(Compt).indep = 0;
            end
            
            Computation.Reac(Compt).name = char(fgetl(fid));
            Computation.Reac(Compt).minerals = strread(fgetl(fid),'%s')';
            Computation.Reac(Compt).coef = strread(fgetl(fid),'%f')';

        end
    end
end



% Check that all the minerals are in the database (and vice-versa)
% (0.45 sec for 150 reac)
for i=1:length(handles.Computation.ThDataComp)
    ListMinDatabase{i} = handles.Computation.ThDataComp(i).name;
end
ListMinDatabaseValid = zeros(size(ListMinDatabase));

for i=1:length(Computation.Reac)
    for j=1:length(Computation.Reac(i).minerals)
        
        TheMin = Computation.Reac(i).minerals(j);
        [Yess,Ou] = ismember(TheMin,ListMinDatabase);
        
        if ~Yess
            warndlg(['The mineral ',char(TheMin),' is not in the assemblage'],'Warning')
            return
        end
        
        ListMinDatabaseValid(Ou) = 1;
    end
end

if length(find(ListMinDatabaseValid == 0))
    TheWere = find(ListMinDatabaseValid == 0);
    TheMinAbs = '';
    for i=1:length(TheWere)
        TheMinAbs = [TheMinAbs,' ',char(ListMinDatabase{TheWere(i)})];
    end
    
    warndlg(['Bad assemblage for this reaction file (unsused: ',char(TheMinAbs),')'],'Warning')
    return
end


% Update menus
set(handles.InvReacWaitBar3,'Position',[14.167 24.167 13.80 0.9]);
set(handles.InvTextReactions,'String',[char(num2str(length(Computation.Reac))),' (',char(num2str(ComptIndep)),')']);
drawnow

set(handles.InvTextFileReactions,'String',filename);

handles.InvWere(4).WeAre = 1;

handles.Computation = Computation;
guidata(hObject, handles);
InvWereWeAre(hObject, eventdata, handles)
return



% #########################################################################
%       INV - Select Compositions (1.1.1)
function InvButtonSelectCompositions_Callback(hObject, eventdata, handles)

Computation = handles.Computation;
MapWindow = handles.MapWindow;
ComputationCode = handles.ComputationCode;

[Computation,ComputationCode] = XTTselectCompositions(Computation,MapWindow,ComputationCode);
 
handles.Computation = Computation;
handles.ComputationCode = ComputationCode;

if ~isequal(ComputationCode(2,4),ComputationCode(1,4))
    handles.InvWere(5).WeAre = 1;
    set(handles.InvPopUpCompositions,'String',Computation.TheNamesSelected);
    
    InvDisplayButtonSelectedComposition(hObject, eventdata, handles);
else
    handles.InvWere(5).WeAre = 0;
    disp('Error, minerals were probably not selected')
end


guidata(hObject, handles);
InvWereWeAre(hObject, eventdata, handles)
return



% #########################################################################
%       INV - PopUp Select Compositions (1.1.1)
function InvPopUpCompositions_Callback(hObject, eventdata, handles)
InvDisplayButtonSelectedComposition(hObject, eventdata, handles)
return



% #########################################################################
%       INV - Display button text where compositions are selected (1.1.1)
function InvDisplayButtonSelectedComposition(hObject, eventdata, handles)

Computation = handles.Computation;
MapWindow = handles.MapWindow;
ComputationCode = handles.ComputationCode;

TheSelected = get(handles.InvPopUpCompositions,'Value');
TheType = ComputationCode(TheSelected,2);

switch TheType
    case 2                                                 % solid solution
        set(handles.InvButtonMultiCompositions,'visible','on'); 
        TheSelectedMode = ComputationCode(TheSelected,3);
        
        switch TheSelectedMode
            case 1    % Map (Clicked)
                set(handles.InvButtonMultiCompositions,'Visible','on');
                set(handles.InvButtonMultiCompositions,'String','Select');
                
            case 2    % Map (All)
                set(handles.InvButtonMultiCompositions,'Visible','off');
                
            case 3    % Map (Random)
                set(handles.InvButtonMultiCompositions,'Visible','on');
                set(handles.InvButtonMultiCompositions,'String','Display');
                
            case 4    % Map (Fixed)
                set(handles.InvButtonMultiCompositions,'Visible','on');
                set(handles.InvButtonMultiCompositions,'String','Select');
                
            case 5    % Input (Fixed)
                set(handles.InvButtonMultiCompositions,'Visible','on');
                set(handles.InvButtonMultiCompositions,'String','Define');
                
            case 6    % Input (File)
                set(handles.InvButtonMultiCompositions,'Visible','on');
                set(handles.InvButtonMultiCompositions,'String','Load');
                
            case 7
                
        end
        
        
    case 3
        set(handles.InvButtonMultiCompositions,'visible','off');
        
        
end



drawnow
return


% #########################################################################
%       INV - Select compositions (1.1.1)
function InvButtonMultiCompositions_Callback(hObject, eventdata, handles)

Computation = handles.Computation;
MapWindow = handles.MapWindow;
ComputationCode = handles.ComputationCode;
SF = handles.SF;
Compositions = handles.Compositions;

TheSelected = get(handles.InvPopUpCompositions,'Value');
TheSelectedMode = ComputationCode(TheSelected,3);

switch TheSelectedMode
    case 1    % Map (Clicked)
         
        Clique = 1;
        Compt = 0;
        while Clique <= 2
            [Xref,Yref,Clique] = XTTginputXTT(1,handles);
            [X,Y] = CoordinatesFromRef(Xref,Yref,handles);
            
            if Clique <= 2
                Compt = Compt + 1;
                X = round(X);
                Y = round(Y);
                
                hold on,
                plot(X,Y,'o','MarkerEdgeColor','r','MarkerFaceColor','w','markersize',7)
                leTxt = text(X-1,Y,num2str(Compt));
                
                Xref = round(Xref);
                Yref = round(Yref);
                
                Selected(Compt,:) = [Yref,Xref,Y,X];
                
                
            else
                if Compt == 0
                    disp('no point selected')
                    return
                end

                % Check General Plot...
                break
            end
        end
          
        SelectedMode = 1;                                       % From Maps 
            
    
        
        %keyboard
        
        
        %warndlg(['Error, please select a composition of ',char(handles.MapWindow.Mask.ListMin{TheMaskValue}), ' ...'],'Warning')
        
    case 2    % Map (All)
        set(handles.InvButtonMultiCompositions,'Visible','off');

    case 3    % Map (Random)
        set(handles.InvButtonMultiCompositions,'Visible','on');
        set(handles.InvButtonMultiCompositions,'String','Display');

    case 4    % Map (Fixed)
        set(handles.InvButtonMultiCompositions,'Visible','on');
        set(handles.InvButtonMultiCompositions,'String','Select');

    case 5    % Input (Fixed)
        set(handles.InvButtonMultiCompositions,'Visible','on');
        set(handles.InvButtonMultiCompositions,'String','Define');

    case 6    % Input (File)
        set(handles.InvButtonMultiCompositions,'Visible','on');
        set(handles.InvButtonMultiCompositions,'String','Load');

    case 7

end


% Prepare the Data

switch SelectedMode 
    case 1                                                  % From the maps
        
        ListMaps = MapWindow.Maps.ListMaps;
        
        Were = find(ismember(SF.ListOxide,ListMaps));
        
        if ~isequal(length(ListMaps),length(Were))
        	warndlg(['Error with the names of the maps'],'Warning')
            return
        end
        
        NbAnalyses = size(Selected,1);
        
        Compositions(TheSelected).SSname = handles.Computation.SSmodelsComp.ListModel(TheSelected);
        Compositions(TheSelected).Type = 1;           % oxide without atoms
        Compositions(TheSelected).Nb = NbAnalyses;     % number of analyses
        
        Compositions(TheSelected).OxData = zeros(NbAnalyses,length(SF.ListOxide));
        
        for i=1:NbAnalyses
            for j=1:length(ListMaps)
                DataFormatShort(i,j) = MapWindow.Maps.Data(j).ValuesOx(Selected(i,1),Selected(i,2));
            end
            Compositions(TheSelected).OxData(i,Were) = DataFormatShort(i,:);
        end
        
        Compositions(TheSelected).DisplayOxDataShort = DataFormatShort;
        Compositions(TheSelected).DisplayOxiNameShort = ListMaps;
             
end

handles.Compositions = Compositions;


ActivateZoomFigure1(hObject, eventdata, handles);

guidata(hObject,handles);
InvWereWeAre(hObject, eventdata, handles)
return






% --- Executes on button press in InvButtonMultiSFcalcul.
function InvButtonMultiSFcalcul_Callback(hObject, eventdata, handles)




keyboard










% --- Executes during object creation, after setting all properties.
function PopUpAxes2Type_CreateFcn(hObject, eventdata, handles)
% hObject    handle to PopUpAxes2Type (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on selection change in PopUpV1Type1.
function PopUpV1Type1_Callback(hObject, eventdata, handles)
% hObject    handle to PopUpV1Type1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = cellstr(get(hObject,'String')) returns PopUpV1Type1 contents as cell array
%        contents{get(hObject,'Value')} returns selected item from PopUpV1Type1


% --- Executes during object creation, after setting all properties.
function PopUpV1Type1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to PopUpV1Type1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on selection change in PopUpV1Type2.
function PopUpV1Type2_Callback(hObject, eventdata, handles)
% hObject    handle to PopUpV1Type2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = cellstr(get(hObject,'String')) returns PopUpV1Type2 contents as cell array
%        contents{get(hObject,'Value')} returns selected item from PopUpV1Type2


% --- Executes during object creation, after setting all properties.
function PopUpV1Type2_CreateFcn(hObject, eventdata, handles)
% hObject    handle to PopUpV1Type2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on selection change in PopUpV1Type3.
function PopUpV1Type3_Callback(hObject, eventdata, handles)
% hObject    handle to PopUpV1Type3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = cellstr(get(hObject,'String')) returns PopUpV1Type3 contents as cell array
%        contents{get(hObject,'Value')} returns selected item from PopUpV1Type3


% --- Executes during object creation, after setting all properties.
function PopUpV1Type3_CreateFcn(hObject, eventdata, handles)
% hObject    handle to PopUpV1Type3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on selection change in PopUpV2Type1.
function PopUpV2Type1_Callback(hObject, eventdata, handles)
% hObject    handle to PopUpV2Type1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = cellstr(get(hObject,'String')) returns PopUpV2Type1 contents as cell array
%        contents{get(hObject,'Value')} returns selected item from PopUpV2Type1


% --- Executes during object creation, after setting all properties.
function PopUpV2Type1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to PopUpV2Type1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on selection change in PopUpV2Type2.
function PopUpV2Type2_Callback(hObject, eventdata, handles)
% hObject    handle to PopUpV2Type2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = cellstr(get(hObject,'String')) returns PopUpV2Type2 contents as cell array
%        contents{get(hObject,'Value')} returns selected item from PopUpV2Type2


% --- Executes during object creation, after setting all properties.
function PopUpV2Type2_CreateFcn(hObject, eventdata, handles)
% hObject    handle to PopUpV2Type2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on selection change in PopUpV2Type3.
function PopUpV2Type3_Callback(hObject, eventdata, handles)
% hObject    handle to PopUpV2Type3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = cellstr(get(hObject,'String')) returns PopUpV2Type3 contents as cell array
%        contents{get(hObject,'Value')} returns selected item from PopUpV2Type3


% --- Executes during object creation, after setting all properties.
function PopUpV2Type3_CreateFcn(hObject, eventdata, handles)
% hObject    handle to PopUpV2Type3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end











function InvTextThermoSSmodels_Callback(hObject, eventdata, handles)
% hObject    handle to InvTextThermoSSmodels (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of InvTextThermoSSmodels as text
%        str2double(get(hObject,'String')) returns contents of InvTextThermoSSmodels as a double


% --- Executes during object creation, after setting all properties.
function InvTextThermoSSmodels_CreateFcn(hObject, eventdata, handles)
% hObject    handle to InvTextThermoSSmodels (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function InvTextChemicalSyst_Callback(hObject, eventdata, handles)
% hObject    handle to InvTextChemicalSyst (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of InvTextChemicalSyst as text
%        str2double(get(hObject,'String')) returns contents of InvTextChemicalSyst as a double


% --- Executes during object creation, after setting all properties.
function InvTextChemicalSyst_CreateFcn(hObject, eventdata, handles)
% hObject    handle to InvTextChemicalSyst (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end











function InvTextAssemblagePure_Callback(hObject, eventdata, handles)
% hObject    handle to InvTextAssemblagePure (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of InvTextAssemblagePure as text
%        str2double(get(hObject,'String')) returns contents of InvTextAssemblagePure as a double


% --- Executes during object creation, after setting all properties.
function InvTextAssemblagePure_CreateFcn(hObject, eventdata, handles)
% hObject    handle to InvTextAssemblagePure (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function InvTextAssemblageEM_Callback(hObject, eventdata, handles)
% hObject    handle to InvTextAssemblageEM (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of InvTextAssemblageEM as text
%        str2double(get(hObject,'String')) returns contents of InvTextAssemblageEM as a double


% --- Executes during object creation, after setting all properties.
function InvTextAssemblageEM_CreateFcn(hObject, eventdata, handles)
% hObject    handle to InvTextAssemblageEM (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end








% -------------------------------------------------------------------------
%
%                 ##### ##### ##### ##### ##### ##### #
%                 #     #   # #   #   #   #   # #   # #
%                 ##### #   # #####   #   #   # #   # #
%                 #     #   # # #     #   #   # #   # #
%                 #     ##### #   #   #   ##### ##### #####
%
% -------------------------------------------------------------------------


% #########################################################################
%       FOR - Check the connection with Theriak
function [ForToolDefault] = ForToolsInitialization(hObject, eventdata, handles)


% (1) read the default file: XTT_TheriakConfig.txt

BingoDefault.TheriakPath

fid = fopen('XTT_ConnectionConfig.txt');
while 1
    LaLign = fgetl(fid);
    if isequal(LaLign,-1)
        break
    end
    if isequal(LaLign(1),'>')
        TheRef = str2double(LaLign(2:3));
        
        switch TheRef
            
            case 1      % Directory of Theriak
                
                ForToolDefault.Path = fgetl(fid);
            
            case 2      % Default Thermodynamic dataset
               ForToolDefault.ThDataFile = fgetl(fid);
               set(handles.ForTextThData,'String',ForToolDefault.ThDataFile);
               
            case 3      % Default Input Compositions
               ForToolDefault.InputComposition = fgetl(fid);
               set(handles.ForTextBulkCompo,'String',ForToolDefault.InputComposition);
        end
    end
end

guidata(hObject, handles);

% (2) First test of theriak
%     we will use the default bulk rock composition to calculate the
%     density at 550C and 17000 bar. 

disp(' ');
disp(' * * * FORTOOL modelling (TEMPORARY DISPLAY) * * *');
disp('---------------- ');
disp(['         Path: ',char(ForToolDefault.Path)]);
disp(['       ThData: ',char(ForToolDefault.ThDataFile)]);
if ~exist(ForToolDefault.ThDataFile,'file')
    disp(' ')
    disp(['-> ERROR: XThermoTools cannot found the ThDataSet file: ',char(ForToolDefault.ThDataFile)])
    disp(' ')
end
disp(['        Compo: ',char(ForToolDefault.InputComposition)]);
disp('---------------- ');
disp(['  Computation: Density for T=550C & P=17000 bar']);
disp('---------------- ');


ForToolDefault.In = 'in';
ForToolDefault.FileName = 'TemporaryTheriakd';

% (3) Create the file [ForToolDefault.FileName] with the input composition for
%     Theriakd
INFILE=char( '    400     4000',['0   ',char(get(handles.ForTextBulkCompo,'String'))]);
dlmwrite(ForToolDefault.FileName , INFILE,'delimiter','');

D_Temp = num2str(550);
D_Press = num2str(17000);

% (4) Create the file 'in'
Command = 'DENSOL'; % density of the rock
INFILE=char( ForToolDefault.ThDataFile, D_Temp,  D_Press , Command);
dlmwrite(ForToolDefault.In , INFILE,'delimiter','');

% (5) Run Theriakd and compute the density
tic
address = [ForToolDefault.Path ' ' ForToolDefault.In ' ' ForToolDefault.FileName];
[wum,yum]=system(address);
TheDensityCalc = str2double(yum);
T = toc;

% (6) Display the results
disp(['        Calls: 1']);
disp([' Elapsed time: ',num2str(T)]);
disp(['       Result: ',num2str(TheDensityCalc)]);
disp(' * * * * * * * * * * * * * * *');
disp(' ');

if TheDensityCalc > 0
    set(handles.ForTextTheriakStatut,'String','Connected with Theriak')
    set(handles.ForTextTheriakStatut,'ForegroundColor',[0,0,1]);
else
    set(handles.ForTextTheriakStatut,'String','Theriak not found')
end

return








% #########################################################################
%       FOR - Find My Garnet
function InvButtonMet1Run_Callback(hObject, eventdata, handles)

% Default variables for FORTOOL 
ForToolDefault = handles.ForToolDefault;
MapWindow = handles.MapWindow;

% CORRECTION A LA MAIN MAIS CA DEVRAIT LE FAIRE TOUT SEUL AVANT ?????????
ForToolDefault.ThDataFile= get(handles.ForTextThData,'String');

ListMaps = MapWindow.Maps.ListMaps;


% (1) Select a Garnet composition
axes(handles.axes1)
[Xref,Yref,Clique] = XTTginputXTT(1,handles);
[X,Y] = CoordinatesFromRef(Xref,Yref,handles);

X = round(X); Y = round(Y);


% (2) Compute the structural formula
FunctionOrder = {'SIO2','AL2O3','FEO','MNO','MGO','CAO','NA2O','K2O'};

[Yes,Where] = ismember(FunctionOrder,ListMaps);

TheCompo = zeros(length(Yes),1);

for i=1:length(Yes)
    if Yes(i)
        TheCompo(i) = MapWindow.Maps.Data(Where(i)).ValuesOx(Y,X);
    end
    
end

[XAlm,XGro,XPyr,XSpe] = EMpropGarnet(TheCompo);

ForToolDefault.GARlookFor = [XAlm,XGro,XPyr];

% (3) Find the garnet
options = optimset('fminsearch'); options=optimset(options,'TolX',100,'TolFun',100,'display','iter','MaxFunEvals',300,'MaxIter',100);
X0 = [500,20000]; % first P,T input

f = @FctMinimGarnetComp;
[Resultata, residudu] = fminsearch(f, X0,options,ForToolDefault);

[Residu,ModelCompos] = FctMinimGarnetComp(Resultata,ForToolDefault);


disp(' * * * * * * * RESULTS * * * * * * * ')
disp(num2str(Resultata))
disp(num2str(Residu))
disp(num2str(ModelCompos))
disp(' ')
        

return



% #########################################################################
%       FOR - MINIM Garnet
function [Residu,ModelCompos] = FctMinimGarnetComp(X0,ForToolDefault)

if X0(1) > 1000 || X0(1) < 100 || X0(2) < 100 || X0(2) > 40000
    Residu = abs(1e13*randn);
    ModelCompos = [0,0,0];
    return
end

D_Temp = num2str(X0(1));
D_Press = num2str(X0(2));

% (1) Check if garnet is stable ...
Command = '*MGARN'; % number of moles of garnet
INFILE=char( ForToolDefault.ThDataFile, D_Temp,  D_Press , Command);
dlmwrite(ForToolDefault.In , INFILE,'delimiter','');

address = [ForToolDefault.Path ' ' ForToolDefault.In ' ' ForToolDefault.FileName];
[wum,yum]=system(address); 
NbMolesGarnet = str2double(yum);

if NbMolesGarnet > 0
    % 1) XAlm
    Command = '*XGARNFE'; % number of moles of garnet
    INFILE=char( ForToolDefault.ThDataFile, D_Temp,  D_Press , Command);
    dlmwrite(ForToolDefault.In , INFILE,'delimiter','');
    
    address = [ForToolDefault.Path ' ' ForToolDefault.In ' ' ForToolDefault.FileName];
    [wum,yum]=system(address); 
    XMgGarnet = str2double(yum);
        
    % 2) XGro
    Command = '*XGARNCA'; % number of moles of garnet
    INFILE=char( ForToolDefault.ThDataFile, D_Temp,  D_Press , Command);
    dlmwrite(ForToolDefault.In , INFILE,'delimiter','');

    address = [ForToolDefault.Path ' ' ForToolDefault.In ' ' ForToolDefault.FileName];
    [wum,yum]=system(address); 
    XCaGarnet = str2double(yum);
    
    
    % 3) XPyr
    Command = '*XGARNMG'; % number of moles of garnet
    INFILE=char( ForToolDefault.ThDataFile, D_Temp,  D_Press , Command);
    dlmwrite(ForToolDefault.In , INFILE,'delimiter','');

    address = [ForToolDefault.Path ' ' ForToolDefault.In ' ' ForToolDefault.FileName];
    [wum,yum]=system(address); 
    XFeGarnet = str2double(yum);
    
    
    % 4) Compos
    XAlm = XMgGarnet/(XMgGarnet+XCaGarnet+XFeGarnet);
    XGro = XCaGarnet/(XMgGarnet+XCaGarnet+XFeGarnet);
    XPyr = XFeGarnet/(XMgGarnet+XCaGarnet+XFeGarnet);
    
    ModelCompos = [XAlm,XGro,XPyr];
    
    Residu = sum(abs(ForToolDefault.GARlookFor - ModelCompos));
    
else
    Residu = abs(1e13*randn);
    ModelCompos = [0,0,0];
    disp('Garnet not stable')
end
 
return


% #########################################################################
%       FOR - Structural formulas of GARNET
function [XAlm,XGro,XPyr,XSpe] = EMpropGarnet(Compo)

NbOx = 12; 
TravMat = []; % initialization required
    
% SiO2 / Al2O3 / FeO / Fe2O3 / MnO / MgO / CaO / Na2O / K2O 
Num = [1,2,1,2,1,1,1,2,2]; % Nombre de cations.
NumO= [2,3,1,3,1,1,1,1,1]; % Nombre d'Oxygenes.
Cst = [60.09,101.96,71.85,159.68,70.94,40.30,56.08, ...
    61.98,94.20]; % atomic mass

TravMat(1:3) = Compo(1:3); % Si02 Al2O3 FeO
TravMat(4) = 0; % Fe2O3
TravMat(5:9) = Compo(4:8); % MnO MgO CaO Na2O K2O

AtomicPer = TravMat./Cst.*Num;

TheSum = sum((AtomicPer .* NumO) ./ Num);
RefOx = TheSum/NbOx;

lesResults = AtomicPer / RefOx;

Si = lesResults(1);
Al = lesResults(2); 
Fe = lesResults(3)+ lesResults(4);
Mn = lesResults(5);
Mg = lesResults(6);
Ca = lesResults(7); 
Na = lesResults(8); 
K = lesResults(9);

XGro = Ca/(Ca+Fe+Mg+Mn);
XSpe = Mn/(Ca+Fe+Mg+Mn);
XPyr = Mg/(Ca+Fe+Mg+Mn);
XAlm = Fe/(Ca+Fe+Mg+Mn);

return





function ForTextThData_Callback(hObject, eventdata, handles)
% hObject    handle to ForTextThData (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of ForTextThData as text
%        str2double(get(hObject,'String')) returns contents of ForTextThData as a double


% --- Executes during object creation, after setting all properties.
function ForTextThData_CreateFcn(hObject, eventdata, handles)
% hObject    handle to ForTextThData (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function ForTextBulkCompo_Callback(hObject, eventdata, handles)
% hObject    handle to ForTextBulkCompo (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of ForTextBulkCompo as text
%        str2double(get(hObject,'String')) returns contents of ForTextBulkCompo as a double


% --- Executes during object creation, after setting all properties.
function ForTextBulkCompo_CreateFcn(hObject, eventdata, handles)
% hObject    handle to ForTextBulkCompo (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on selection change in ForPopUpMethod.
function ForPopUpMethod_Callback(hObject, eventdata, handles)
% hObject    handle to ForPopUpMethod (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = cellstr(get(hObject,'String')) returns ForPopUpMethod contents as cell array
%        contents{get(hObject,'Value')} returns selected item from ForPopUpMethod


% --- Executes during object creation, after setting all properties.
function ForPopUpMethod_CreateFcn(hObject, eventdata, handles)
% hObject    handle to ForPopUpMethod (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end
















% THESE FUNCTIONS DO NOT REQUIRED CODE (BUT WE NEED THESE FUNCTIONS)



% --- Executes on button press in InvCheckBoxPart1.
function InvCheckBoxPart1_Callback(hObject, eventdata, handles)
% hObject    handle to InvCheckBoxPart1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of InvCheckBoxPart1


% --- Executes on button press in InvCheckBoxPart2.
function InvCheckBoxPart2_Callback(hObject, eventdata, handles)
% hObject    handle to InvCheckBoxPart2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of InvCheckBoxPart2


% --- Executes on button press in InvCheckBoxPart3.
function InvCheckBoxPart3_Callback(hObject, eventdata, handles)
% hObject    handle to InvCheckBoxPart3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of InvCheckBoxPart3

function InvTextThermoDataSet_Callback(hObject, eventdata, handles)
% hObject    handle to InvTextThermoDataSet (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of InvTextThermoDataSet as text
%        str2double(get(hObject,'String')) returns contents of InvTextThermoDataSet as a double


% --- Executes during object creation, after setting all properties.
function InvTextThermoDataSet_CreateFcn(hObject, eventdata, handles)
% hObject    handle to InvTextThermoDataSet (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in InvCheckBoxPart4.
function InvCheckBoxPart4_Callback(hObject, eventdata, handles)
% hObject    handle to InvCheckBoxPart4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of InvCheckBoxPart4



function InvTextReactions_Callback(hObject, eventdata, handles)
% hObject    handle to InvTextReactions (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of InvTextReactions as text
%        str2double(get(hObject,'String')) returns contents of InvTextReactions as a double


% --- Executes during object creation, after setting all properties.
function InvTextReactions_CreateFcn(hObject, eventdata, handles)
% hObject    handle to InvTextReactions (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end















function InvTextFileReactions_Callback(hObject, eventdata, handles)
% hObject    handle to InvTextFileReactions (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of InvTextFileReactions as text
%        str2double(get(hObject,'String')) returns contents of InvTextFileReactions as a double


% --- Executes during object creation, after setting all properties.
function InvTextFileReactions_CreateFcn(hObject, eventdata, handles)
% hObject    handle to InvTextFileReactions (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


 


% --- Executes on button press in InvCheckBoxPart5.
function InvCheckBoxPart5_Callback(hObject, eventdata, handles)
% hObject    handle to InvCheckBoxPart5 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of InvCheckBoxPart5





% --- Executes during object creation, after setting all properties.
function InvPopUpCompositions_CreateFcn(hObject, eventdata, handles)
% hObject    handle to InvPopUpCompositions (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% -------------------------------------------------------------------------





% --- Executes on selection change in BinPopUpBulkPhases4BulkList.
function BinPopUpBulkPhases4BulkList_Callback(hObject, eventdata, handles)
% hObject    handle to BinPopUpBulkPhases4BulkList (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = cellstr(get(hObject,'String')) returns BinPopUpBulkPhases4BulkList contents as cell array
%        contents{get(hObject,'Value')} returns selected item from BinPopUpBulkPhases4BulkList


% --- Executes during object creation, after setting all properties.
function BinPopUpBulkPhases4BulkList_CreateFcn(hObject, eventdata, handles)
% hObject    handle to BinPopUpBulkPhases4BulkList (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: popupmenu controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in check_textSW.
function check_textSW_Callback(hObject, eventdata, handles)
% hObject    handle to check_textSW (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of check_textSW


% goto menu

% --------------------------------------------------------------------
function Menu_File_Callback(hObject, eventdata, handles)
% hObject    handle to Menu_File (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --------------------------------------------------------------------
function Menu_Initialize_Callback(hObject, eventdata, handles)

return

% --------------------------------------------------------------------
function Menu_CleanupDir_Callback(hObject, eventdata, handles)
BinButtonCleanDir_Callback(hObject, eventdata, handles);
return

% --------------------------------------------------------------------
function Menu_GenerateOptions_Callback(hObject, eventdata, handles)
XTTsetOptionXTT;

FileName = 'OptionsXTT.txt';
[OptString,Valid] = ReadOptionXTT(FileName);

set(handles.BinTextBinTC,'String',OptString{3});
set(handles.BinTextBinP,'String',OptString{4});

set(handles.BinTextAnt1,'String',OptString{5});
set(handles.BinTextAnt2,'String',OptString{6});
set(handles.BinTextAnt3,'String',OptString{7});
set(handles.BinTextAnt4,'String',OptString{8});

set(handles.BinTextBinTCminmax,'String',OptString{9});
set(handles.BinTextBinPminmax,'String',OptString{10});

disp(' ');
if Valid
    fprintf('\t%s\t\t%s\n','-> options:','from OptionsXTT.txt');
else
    fprintf('\t%s\t\t%s\n','-> options:','default (OptionsXTT.txt not found)');
end
for i=1:length(OptString)
    fprintf('\t\t%s\t\t%s\n',['opt',num2str(i)],char(OptString{i}));
end

guidata(hObject, handles);
BinWereWeAre(hObject, eventdata, handles);
return

% --------------------------------------------------------------------
function Menu_CheckTheriak_Callback(hObject, eventdata, handles)
% hObject    handle to Menu_CheckTheriak (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --------------------------------------------------------------------
function Menu_Bulk_Callback(hObject, eventdata, handles)
% hObject    handle to Menu_Bulk (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --------------------------------------------------------------------
function Menu_BulkAdd_Callback(hObject, eventdata, handles)
BinButtonAddShape_Callback(hObject, eventdata, handles);
return

% --------------------------------------------------------------------
function Menu_BulkDelete_Callback(hObject, eventdata, handles)
BinButtonDeleteShape_Callback(hObject, eventdata, handles);
return


% --------------------------------------------------------------------
function Menu_BulkSaveConfig_Callback(hObject, eventdata, handles)
BinButtonSaveShape_Callback(hObject, eventdata, handles);
return

% --------------------------------------------------------------------
function Menu_BulkLoadConfig_Callback(hObject, eventdata, handles)
BinButtonLoadShape_Callback(hObject, eventdata, handles);
return


% --------------------------------------------------------------------
function Menu_Phases_Callback(hObject, eventdata, handles)
% hObject    handle to Menu_Phases (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --------------------------------------------------------------------
function Menu_PhasesFMB_Callback(hObject, eventdata, handles)
BinButtonFluidGases_Callback(hObject, eventdata, handles);
return

% --------------------------------------------------------------------
function Menu_PhasesSyncSolid_Callback(hObject, eventdata, handles)
BinButtonSyncPhase_Callback(hObject, eventdata, handles);
return

% --------------------------------------------------------------------
function Menu_PhasesGroups_Callback(hObject, eventdata, handles)
% hObject    handle to Menu_PhasesGroups (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --------------------------------------------------------------------
function Menu_SolidsLoadConfig_Callback(hObject, eventdata, handles)
BinButtonImportGrp_Callback(hObject, eventdata, handles);
return

% --------------------------------------------------------------------
function Menu_SolidsSaveConfig_Callback(hObject, eventdata, handles)
BinButtonSaveGrp_Callback(hObject, eventdata, handles)
return

% --------------------------------------------------------------------
function Menu_PhasesAddGrp_Callback(hObject, eventdata, handles)
BinButtonAddGroupOfPhase_Callback(hObject, eventdata, handles);
return



% --------------------------------------------------------------------
function Menu_PhasesDeleteGrp_Callback(hObject, eventdata, handles)
ManageChemicalGroups(3,hObject, eventdata, handles);
return

% --------------------------------------------------------------------
function Menu_PhasesCALC_Callback(hObject, eventdata, handles)
BinButtonCalcPhase_Callback(hObject, eventdata, handles);
return


% --------------------------------------------------------------------
function Menu_BingoAntidote_Callback(hObject, eventdata, handles)
% hObject    handle to Menu_BingoAntidote (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --------------------------------------------------------------------
function Menu_CloseXTT_Callback(hObject, eventdata, handles)
%
delete(gcf);
return


% --------------------------------------------------------------------
function Menu_PhasesDesync_Callback(hObject, eventdata, handles)
BinCheckBoxIsPhaseIn_Callback(hObject, eventdata, handles)
return


% --------------------------------------------------------------------
function Menu_PhasesManualSync_Callback(hObject, eventdata, handles)
BinButtonSyncManualPhase_Callback(hObject, eventdata, handles)
return


% --------------------------------------------------------------------
function Menu_Help_Callback(hObject, eventdata, handles)
url = [handles.LocBase,'/Help.html'];
web(url)
return


% --------------------------------------------------------------------
function Menu_Preferences_Callback(hObject, eventdata, handles)
BinButtonOptions_Callback(hObject, eventdata, handles);
return

% --------------------------------------------------------------------
function Menu_About_Callback(hObject, eventdata, handles)
url = [handles.LocBase,'/About.txt'];
web(url)
return


% --------------------------------------------------------------------
function Menu_OtherElements_Callback(hObject, eventdata, handles)
% hObject    handle to Menu_OtherElements (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --------------------------------------------------------------------
function CopyDB2WorkingDirectory_Callback(hObject, eventdata, handles)
%
WorkingDir = cd;

Databases = get(handles.BinPopUpDatabase,'String');
SelectedDatabase = get(handles.BinPopUpDatabase,'Value');
NameDatabase = Databases{SelectedDatabase};

DBinfo = handles.BingoDefault.Theriak.Database(SelectedDatabase);
Path4DB = [DBinfo.PathForDB,DBinfo.Label];
[SUCCESS,MESSAGE,MESSAGEID] = copyfile(Path4DB,[WorkingDir,'/',NameDatabase],'f');

if SUCCESS
    h = msgbox({'Success'}, 'Help','Help');
else
    h = msgbox({'Oups something went wrong','the file cannot be copied'}, 'Error','error');
end
return;
