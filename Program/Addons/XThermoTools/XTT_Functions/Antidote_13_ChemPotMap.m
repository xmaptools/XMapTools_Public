function [Output,Antidote_VARIABLES,handles] = Antidote_13_ChemPotMap(AntidoteRecipes,AntidoteMode,WorkVariXMap,MinimOptions,handles)
%
%

InitPanelDisp(1,handles);

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

% Parameters for the map:
MapWindow = handles.MapWindow;
DensityMap = MapWindow.Mask.DensityMap;

MapSize = size(DensityMap);


%ImputParam = inputdlg({'dX (pixels)','Step (pixels)'},'Mapping parameters',1,{'250','250'});


dX = str2num(get(handles.BinTextAnt2,'String')); 
dY = dX;
Step = dX;


% %Below is the version for SWOT 2019 - works for squared maps:
% Ratio = MapSize(1)/MapSize(2);
% dX = str2num(ImputParam{1});
% %Overlap = str2num(ImputParam{2})/100;
% 
% %dX = (MapSize(2)+MapSize(2)*Overlap)/str2num(ImputParam{1});
% dY = dX*Ratio;
% 
% %Step = dX-(Overlap*dX);      % in pixel
% Step = str2num(ImputParam{2})

if Step < 0
    return
end

Xi = [floor((dX-1)/2):Step:MapSize(2) - floor((dX-1)/2)];
Yi = [floor((dY-1)/2):Step:MapSize(1) - floor((dY-1)/2)];

XYData.Qfactors.Q1 = zeros(1,length(Yi)*length(Xi));
XYData.Qfactors.Q2 = zeros(1,length(Yi)*length(Xi));
XYData.Qfactors.Q3 = zeros(1,length(Yi)*length(Xi));
XYData.Qfactors.Qt = zeros(1,length(Yi)*length(Xi));

XYData.Bulk.ListEl = '';
XYData.Bulk.BulkEl = zeros(1,length(Yi)*length(Xi));

XYData.ChemPot.ListChemPot = '';
XYData.ChemPot.ChemPotMatrix = zeros(1,length(Yi)*length(Xi));

XYData.AsmData.ListMin = '';
XYData.AsmData.IsStable = zeros(length(Yi),length(Xi),20);   % 3D matrix with z=phase
XYData.AsmData.VolFrac = zeros(length(Yi),length(Xi),20);   % 3D matrix with z=phase

Compt = 0;
for iX = 1:length(Xi)
    for iY = 1:length(Yi)
        
        Compt = Compt+1;
        disp(' '), disp(' ')
        disp([' -->  LOOP: ',num2str(Compt),'/',num2str(length(Xi)*length(Yi)),''])
        disp(' ')
        
        XVals = [Xi(iX)-floor((dX-1)/2),Xi(iX)+floor((dX-1)/2),Xi(iX)+floor((dX-1)/2),Xi(iX)-floor((dX-1)/2)];
        YVals = [Yi(iY)+floor((dY-1)/2),Yi(iY)+floor((dY-1)/2),Yi(iY)-floor((dY-1)/2),Yi(iY)-floor((dY-1)/2)];
        
        axes(handles.axes1)
        hold on, plot(XVals,YVals,'-k')
        drawnow
        
        TempBinBulk.DomainXrefYref = [XVals',YVals'];
        
        [TempBinBulk] = BinUpdateBulkComposition(TempBinBulk,1,handles);
        
        [Bulk,TempBinBulk] = SuperFastBulkUpdate(handles.BinGfDef,TempBinBulk,1);
        
        disp(Bulk);
        
        [BinSet] = SetBinFAST(ProgPath,DefMin,TheSelDatabase,Bulk,handles.BinGfDef);
        
        WorkVariMod = TheriakCall(BinSet,D_Temp,D_Press);
        
        %guidata(hObject, handles);   % not needed
        [BinPhaseDef] = BinButtonCalcPhaseFASTlocal(TempBinBulk, handles);
        [WorkVariXMap] = GenerateResXMap(BinPhaseDef);
                
        [Emin,Evaluation] = Opti_EpsiMinimCalc(WorkVariMod,WorkVariXMap,MinimOptions,1,handles);
        
        XYData = UpdateXYData(XYData,Compt,Evaluation,Emin,WorkVariMod,WorkVariXMap,Bulk,iX,iY);
        
        % goto present
        
    end
end

DateStr = char(datestr(now));
DateStr(find(DateStr == ' ')) = '_';
ProjectName = ['Antidote_Recipe13_',DateStr];

ProjectName = CleanProjectName(ProjectName);

[Success,Message,MessageID] = mkdir('Antidote');
cd Antidote
[Success,Message,MessageID] = mkdir(ProjectName);
cd ..

Directory = ['Antidote/',ProjectName,'/'];

for i = 1:length(XYData.AsmData.ListMin)
    h = figure;
    imagesc(Xi,Yi,XYData.AsmData.IsStable(:,:,i)), colormap([1,0,0;0,0,1])
    axis([0 MapSize(2) 0 MapSize(1)]);
    title(['Stability (blue) of ',char(XYData.AsmData.ListMin{i}),' at ',D_Temp,'/',D_Press,]),
    %saveas(h,[Directory,'Stability_',char(XYData.AsmData.ListMin{i}),'.pdf'],'pdf');
    saveas(h,[Directory,'Stability_',char(XYData.AsmData.ListMin{i}),'.fig'],'fig');
    close(h)
    
    h = figure;
    imagesc(Xi,Yi,XYData.AsmData.VolFrac(:,:,i)), colorbar, colormap([0,0,0;RdYlBu(64)])
    axis([0 MapSize(2) 0 MapSize(1)]);
    title(['Modelled mode of ',char(XYData.AsmData.ListMin{i}),' at ',D_Temp,'/',D_Press,]),
    %saveas(h,[Directory,'Stability_',char(XYData.AsmData.ListMin{i}),'.pdf'],'pdf');
    saveas(h,[Directory,'Mode_',char(XYData.AsmData.ListMin{i}),'.fig'],'fig');
    close(h)
    
end

for i = 1:length(XYData.ChemPot.ListChemPot)
    
    h = figure;
    Map2Plot = reshape(XYData.ChemPot.ChemPotMatrix(i,:)/1000,length(Yi),length(Xi));
    imagesc(Xi,Yi,Map2Plot), colorbar,
    %colormap(flip(hot(128)))
    colormap(hot(128))
    axis([0 MapSize(2) 0 MapSize(1)]);
    title(['mu',char(XYData.ChemPot.ListChemPot{i}),' (in KJ.mol^-^1) at ',D_Temp,'/',D_Press,]),
    %saveas(h,[Directory,'mu_',char(XYData.ChemPot.ListChemPot{i}),'_Map.pdf'],'pdf');
    saveas(h,[Directory,'mu_',char(XYData.ChemPot.ListChemPot{i}),'_Map.fig'],'fig');
    close(h)
    
    
    h = figure; hold on
    [DX,DY] = gradient(-Map2Plot);
    contour(Xi,Yi,Map2Plot)
    quiver(Xi,Yi,DX,DY,1)
    set(gca,'Ydir','reverse');
    colormap(hot(128))
    axis([0 MapSize(2) 0 MapSize(1)]);
    %saveas(h,[Directory,'mu_',char(XYData.ChemPot.ListChemPot{i}),'_Quiver.pdf'],'pdf');
    saveas(h,[Directory,'mu_',char(XYData.ChemPot.ListChemPot{i}),'_Quiver.fig'],'fig');
    close(h)
end




Output.WeCallBingo = 0;
Output.WeSaveWorkspace = 0;
Output.Message = 'Success';

Antidote_VARIABLES = [];

return


function [XYData2] = UpdateXYData(XYData2,iPos,Evaluation,Emin,WorkVariMod,WorkVariXMap,Bulk,iX,iY)

XYData = XYData2;

XYData.Qfactors.Q1(1,iPos) = Evaluation.assemblage;
XYData.Qfactors.Q2(1,iPos) = Evaluation.Volume;
XYData.Qfactors.Q3(1,iPos) = Evaluation.Compositions;
XYData.Qfactors.Qt(1,iPos) = Emin;

[Elem,Vals,Extras4Bulks] = ExtractElemValsFromBulk(Bulk);

[Is,Ind] = ismember(Elem,XYData.Bulk.ListEl);

for i = 1:length(Is)
    
    if Is(i)
       Where = Ind(i); 
    else
        % Create a new dimensions
        Where = length(XYData.Bulk.ListEl)+1;
        XYData.Bulk.ListEl{Where} = char(Elem{i});
    end
    
    XYData.Bulk.BulkEl(Where,iPos) = Vals(i);
    % WorkVariMod.Dens(i);
end


for i = 1:length(WorkVariMod.ChemComp)
    Ind3 = find(ismember(XYData.ChemPot.ListChemPot,WorkVariMod.ChemComp{i}));
    
    if Ind3
        Where3 = Ind3;
    else
        Where3 = length(XYData.ChemPot.ListChemPot)+1;
        XYData.ChemPot.ListChemPot{Where3} = char(WorkVariMod.ChemComp{i});
    end
    
    XYData.ChemPot.ChemPotMatrix(Where3,iPos) = WorkVariMod.ChemPot(i);
    
end

StablePhases = WorkVariMod.Names;
for i = 1:length(StablePhases)
    Where = find(ismember(XYData.AsmData.ListMin,StablePhases{i}));
    if ~length(Where)
        Where = length(XYData.AsmData.ListMin)+1;
        XYData.AsmData.ListMin{end+1} = StablePhases{i};
    end
    XYData.AsmData.IsStable(iY,iX,Where) = 1;
    XYData.AsmData.VolFrac(iY,iX,Where) = WorkVariMod.VolFrac(i);
end


XYData2 = XYData;
return





