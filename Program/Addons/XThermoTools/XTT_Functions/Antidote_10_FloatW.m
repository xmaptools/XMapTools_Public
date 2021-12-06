function [Output,Antidote_VARIABLES,handles] = Antidote_10_FloatW(AntidoteRecipes,AntidoteMode,WorkVariXMap,MinimOptions,handles)
%
%


% Update the live display:
eval(['TCi = [',get(handles.BinTextBinTCminmax,'String'),'];']);
eval(['Pi = [',get(handles.BinTextBinPminmax,'String'),'];']);

LIMS = [TCi(1)-1,TCi(end)+1,Pi(1)-1,Pi(end)+1];


% -------------------------------------------------------------------------
% (1) SET initial variables for Theriak (BinSet) and update display

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

D_Temp = get(handles.BinTextBinTC,'String');
Temp = str2num(D_Temp);
D_Press = get(handles.BinTextBinP,'String');
Press = str2num(D_Press);

BinSet = SetBin(handles); % will be updated later on

fprintf('%s\t%s\n','Antidote',char(AntidoteRecipes(AntidoteMode)))
fprintf('%s\t%s\n','Database',BinSet.Database);
disp(' ')
fprintf('%s\t\t%.0f\n','Temp. P',Temp);
fprintf('%s\t%.0f\n','Temp. TC',Press);
disp(' ')


% NB: The printed bulk is not used here ...
%OriginalBulk = get(handles.BinTextBulkCompo,'String');

SelectedBinBulk = get(handles.BinPopUpBulkCompoList,'Value');
BinBulkOri = handles.BinBulk(SelectedBinBulk);

TempBinBulk = BinBulkOri;


% -------------------------------------------------------------------------
% Define the sliding window and define a path ...
InitPanelDisp(1,handles);

axes(handles.axes1), hold on

XLimsMap = get(handles.axes1,'Xlim');
YLimsMap = get(handles.axes1,'Ylim');

for i=1:2
    
    [a,b,clique] = XTTginputXTT(1,handles);
    if clique < 2
        
        if a < XLimsMap(1) || a > XLimsMap(2) || b < YLimsMap(1) || b > YLimsMap(2)
            warndlg({'Aborded...','Do not click outside the map!'},'Bingo-Antidote')
            Output.WeCallBingo = 0;
            Output.WeSaveWorkspace = 0;
            Output.Message = 'Error';
            
            Antidote_VARIABLES = [];
            return
        end
        
        h(i,1) = a;
        h(i,2) = b;
        
        [aPlot,bPlot] = CoordinatesFromRef(a,b,handles);
        
        % new (1.6.2)
        hPlot(i,1) = aPlot;
        hPlot(i,2) = bPlot;
        
        plot(floor(hPlot(i,1)),floor(hPlot(i,2)),'.w') % point
    else
        return
    end
end

XMin = min(h(:,1));
XMax = max(h(:,1));
YMin = min(h(:,2));
YMax = max(h(:,2));

[XMinPlot,YMinPlot] = CoordinatesFromRef(XMin,YMin,handles);
[XMaxPlot,YMaxPlot] = CoordinatesFromRef(XMax,YMax,handles);

plot([XMinPlot,XMaxPlot],[YMinPlot,YMinPlot],'-m','linewidth',2), plot([XMinPlot,XMaxPlot],[YMinPlot,YMinPlot],'-k','linewidth',1)
plot([XMinPlot,XMaxPlot],[YMaxPlot,YMaxPlot],'-m','linewidth',2), plot([XMinPlot,XMaxPlot],[YMaxPlot,YMaxPlot],'-k','linewidth',1)
plot([XMinPlot,XMinPlot],[YMinPlot,YMaxPlot],'-m','linewidth',2), plot([XMinPlot,XMinPlot],[YMinPlot,YMaxPlot],'-k','linewidth',1)
plot([XMaxPlot,XMaxPlot],[YMinPlot,YMaxPlot],'-m','linewidth',2), plot([XMaxPlot,XMaxPlot],[YMinPlot,YMaxPlot],'-k','linewidth',1)

XSemiL = (XMax-XMin)/2;
YSemiL = (YMax-YMin)/2;

XCenter = XMin+XSemiL;
YCenter = YMin+YSemiL;

[XCenterPlot,YCenterPlot] = CoordinatesFromRef(XCenter,YCenter,handles);

plot(XCenterPlot,YCenterPlot,'.w')


% Define the points where to go...
XSteps = XCenter;
YSteps = YCenter;

ComptIter = 0;
Compt = 1;
while 1
    [a,b,clique] = XTTginputXTT(1,handles);
    if clique < 2
        
        if a < XLimsMap(1) || a > XLimsMap(2) || b < YLimsMap(1) || b > YLimsMap(2)
            warndlg({'Aborded...','Do not click outside the map!'},'Bingo-Antidote')
            Output.WeCallBingo = 0;
            Output.WeSaveWorkspace = 0;
            Output.Message = 'Error';
            
            Antidote_VARIABLES = [];
            return
        end
        
        Compt = Compt+1;
        
        XSteps(Compt) = a;
        YSteps(Compt) = b;
        
        [XStepsPlot,YStepsPlot] = CoordinatesFromRef(XSteps,YSteps,handles);
        %keyboard
        plot(XStepsPlot(end),YStepsPlot(end),'.w') % point
        plot([XStepsPlot(end),XStepsPlot(end-1)],[YStepsPlot(end),YStepsPlot(end-1)],'-m','linewidth',2)
        
    else
        if Compt == 1
            Output.WeCallBingo = 0;
            Output.WeSaveWorkspace = 0;
            Output.Message = 'Error';
            
            Antidote_VARIABLES = [];
            return
        else
            break
        end
    end
end

NbSteps = str2num(char(inputdlg({'Steps within each segment'},'XMapTools',1,{'30'})));

if isempty(NbSteps)
    Output.WeCallBingo = 0;
    Output.WeSaveWorkspace = 0;
    Output.Message = 'Error';
    
    Antidote_VARIABLES = [];
    return
end

NbSegments = length(XStepsPlot)-1;

ComptRes = 0;
DistTrav = 0;

% Create ProData to store all the information along the profile
ProData.Qfactors.Q1 = zeros(1,NbSegments*NbSteps);
ProData.Qfactors.Q2 = zeros(1,NbSegments*NbSteps);
ProData.Qfactors.Q3 = zeros(1,NbSegments*NbSteps);
ProData.Qfactors.Qt = zeros(1,NbSegments*NbSteps);

ProData.MinProp.ListMin = '';
ProData.MinProp.ProMatrix = zeros(1,NbSegments*NbSteps);

ProData.MinPropXray.ListMin = '';
ProData.MinPropXray.ProMatrix = zeros(1,NbSegments*NbSteps);

ProData.MinComp.ListMin = '';
ProData.MinComp.MinQ = zeros(1,NbSegments*NbSteps);
ProData.MinComp.MinQw = zeros(1,NbSegments*NbSteps);

ProData.ChemPot.ListChemPot = '';
ProData.ChemPot.ChemPotMatrix = zeros(1,NbSegments*NbSteps);


tic
for iSeg = 1:NbSegments
    
    XCenter = XSteps(iSeg);
    XEnd = XSteps(iSeg+1);
    YCenter = YSteps(iSeg);
    YEnd = YSteps(iSeg+1);
    
    % dX,dY
    dX = (max([XCenter,XEnd])-min([XCenter,XEnd]))/(NbSteps-1);
    if XCenter > XEnd
        dX = -dX;
    end
    
    dY = (max([YCenter,YEnd])-min([YCenter,YEnd]))/(NbSteps-1);
    if YCenter > YEnd
        dY = -dY;
    end
    
    XTemp = XCenter;
    YTemp = YCenter;
    
    if isequal(iSeg,1)
        TheMinInc = 1;
    else
        TheMinInc = 2;
    end
    
    for i=TheMinInc:NbSteps
        
        if i>1
            XTemp = XTemp+dX;
            YTemp = YTemp+dY;
        end
        
        XCoord = [XTemp-XSemiL,XTemp+XSemiL,XTemp+XSemiL,XTemp-XSemiL];
        YCoord = [YTemp-YSemiL,YTemp-YSemiL,YTemp+YSemiL,YTemp+YSemiL];
        
        [XTempPlot,YTempPlot] = CoordinatesFromRef(XTemp,YTemp,handles);
        [XCoordPlot,YCoordPlot] = CoordinatesFromRef(XCoord,YCoord,handles);
        
        %plot(XTempPlot,YTempPlot,'.k')
        plot(XCoordPlot,YCoordPlot,'.k')
        drawnow
        
        disp(' '), disp(' ')
        disp([' -->  LOOP: ',num2str(NbSteps*(iSeg-1)+i),'/',num2str(NbSegments*NbSteps),''])
        disp(' ')
        
        % Now we need to update the bulk composition
        CoorSim = [XCoord',YCoord'];
        TempBinBulk.DomainXrefYref = CoorSim;
        
        [TempBinBulk] = BinUpdateBulkComposition(TempBinBulk,1,handles);
        
        [Bulk,TempBinBulk] = SuperFastBulkUpdate(handles.BinGfDef,TempBinBulk,1);
        
        disp(Bulk);
        
        [BinSet] = SetBinFAST(ProgPath,DefMin,TheSelDatabase,Bulk,handles.BinGfDef);
        
        WorkVariMod = TheriakCall(BinSet,D_Temp,D_Press);
        
        %guidata(hObject, handles);   % not needed
        [BinPhaseDef] = BinButtonCalcPhaseFASTlocal(TempBinBulk, handles);
        [WorkVariXMap] = GenerateResXMap(BinPhaseDef);
        
        % Here we
        
        [Emin,Evaluation] = Opti_EpsiMinimCalc(WorkVariMod,WorkVariXMap,MinimOptions,1,handles);
        
        ComptIter = ComptIter+1;
        
        ProData = UpdateProData(ProData,NbSteps*(iSeg-1)+i,Evaluation,Emin,WorkVariMod,WorkVariXMap);
        
    end
end

% plot the results
FinalPlotProData(ProData,NbSegments*NbSteps);

ht1=toc;
disp(' ')
fprintf('%s\t%.4f\n','CPU time',ht1);




Output.WeCallBingo = 0;
Output.WeSaveWorkspace = 0;
Output.Message = 'Success';

Antidote_VARIABLES = [];

return
