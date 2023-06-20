function [Output,Antidote_VARIABLES] = Antidote_13_ChemPot(WorkVariXMap,MinimOptions,app)
%
%
%

ROI_DeleteROI(app);

% Load MinimOptions to enable TEST mode
load('MinimOptions.mat');

Tmin = app.TminEditField.Value;
Tmax = app.TmaxEditField.Value;
Pmin = app.PminEditField.Value;
Pmax = app.PmaxEditField.Value;
Res = app.AntidoteGridresolutionEditField.Value;

Temp = app.BingoTemperatureEditField.Value;
D_Temp = num2str(Temp);

Press = app.BingoPressureEditField.Value;
D_Press = num2str(Press*1e4);

[BinSet] = SetBin(app);

SelectedBinBulk = app.ROITree.SelectedNodes.NodeData;
BinBulkOri = app.BinBulk(SelectedBinBulk);
TempBinBulk = BinBulkOri;

ProgPath = BinSet.Path;
TheSelDatabase = BinSet.Database;
iDB = find(ismember(app.DatabaseListBox.Items,TheSelDatabase));
DefMin = app.BingoDefault.Theriak.Database(iDB).DefMin;

app.Report_Antidote{end+1} = ['Antidote: Recipe [13] - Chemical potential mapping (fixed P–T)'];
app.Report_Antidote{end+1} = '';
% app.Report_Antidote{end+1} = ['Bulk: ',BinSet.Bulk2Display,''];
app.Report_Antidote{end+1} = ['Database: ',BinSet.Database,''];
app.Report_Antidote{end+1} = '';
app.Text_Report_Antidote.Value = app.Report_Antidote;

% Generate the grid
MapSize = size(app.MapData.CData(1).Map);

dX = app.AntidoteROIparamNbStepsEditField.Value;
dY = dX;

Step = dX;

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

ROI = drawrectangle(app.UIAxes,'Position',[0,1,1,1],'Color',[1,0,0.7137],'InteractionsAllowed','none');

tic
Compt = 0;
for iX = 1:length(Xi)
    for iY = 1:length(Yi)
        
        Compt = Compt+1;
        app.Report_Antidote{end+1} = [' -->  LOOP: ',num2str(Compt),'/',num2str(length(Xi)*length(Yi))];
        
        XVals = [Xi(iX)-floor((dX-1)/2),Xi(iX)+floor((dX-1)/2),Xi(iX)+floor((dX-1)/2),Xi(iX)-floor((dX-1)/2)];
        YVals = [Yi(iY)+floor((dY-1)/2),Yi(iY)+floor((dY-1)/2),Yi(iY)-floor((dY-1)/2),Yi(iY)-floor((dY-1)/2)];
        
        ROI.Position = [XVals(end),YVals(end),dX,dX];
        TempBinBulk.DomainXrefYref = ROI.Vertices;
        drawnow
        
        [TempBinBulk] = BinUpdateBulkComposition(app,TempBinBulk,SelectedBinBulk);
        
        [Bulk,TempBinBulk] = SuperFastBulkUpdate(app.BinGfDef,TempBinBulk,1);
        
        [BinSet] = SetBinFAST(ProgPath,DefMin,TheSelDatabase,Bulk,app.BinGfDef);
        
        app.Report_Antidote{end+1} = ['      BULK: ',BinSet.Bulk2Display];
        
        WorkVariMod = TheriakCall(BinSet,D_Temp,D_Press);
        
        [BinPhaseDef] = BinButtonCalcPhaseFASTlocal(TempBinBulk, app);
        [WorkVariXMap] = GenerateResXMap(BinPhaseDef);
        
        [Emin,Evaluation] = Opti_EpsiMinimCalc(WorkVariMod,WorkVariXMap,MinimOptions,0,app);
        
        XYData = UpdateXYData(XYData,Compt,Evaluation,Emin,WorkVariMod,WorkVariXMap,Bulk,iX,iY);
        
        app.Text_Report_Antidote.Value = app.Report_Antidote;
        pause(0.1)
        scroll(app.Text_Report_Antidote,'bottom');
        
    end
end

ROI_DeleteROI(app);

app.Report_Antidote{end+1} = [' '];
app.Report_Antidote{end+1} = ['Saving results...'];
app.Report_Antidote{end+1} = [' '];
app.Text_Report_Antidote.Value = app.Report_Antidote;
pause(0.1)
scroll(app.Text_Report_Antidote,'bottom');

% plot the results
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
    saveas(h,[Directory,'Stability_',char(XYData.AsmData.ListMin{i}),'.pdf'],'pdf');
    saveas(h,[Directory,'Stability_',char(XYData.AsmData.ListMin{i}),'.fig'],'fig');
    close(h)
    
    h = figure;
    imagesc(Xi,Yi,XYData.AsmData.VolFrac(:,:,i)), colorbar, colormap([0,0,0;RdYlBu(64)])
    axis([0 MapSize(2) 0 MapSize(1)]);
    title(['Modelled mode of ',char(XYData.AsmData.ListMin{i}),' at ',D_Temp,'/',D_Press,]),
    saveas(h,[Directory,'Stability_',char(XYData.AsmData.ListMin{i}),'.pdf'],'pdf');
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
    saveas(h,[Directory,'mu_',char(XYData.ChemPot.ListChemPot{i}),'_Map.pdf'],'pdf');
    saveas(h,[Directory,'mu_',char(XYData.ChemPot.ListChemPot{i}),'_Map.fig'],'fig');
    close(h)
    
    
    h = figure; hold on
    [DX,DY] = gradient(-Map2Plot);
    contour(Xi,Yi,Map2Plot)
    quiver(Xi,Yi,DX,DY,1)
    set(gca,'Ydir','reverse');
    colormap(hot(128))
    axis([0 MapSize(2) 0 MapSize(1)]);
    saveas(h,[Directory,'mu_',char(XYData.ChemPot.ListChemPot{i}),'_Quiver.pdf'],'pdf');
    saveas(h,[Directory,'mu_',char(XYData.ChemPot.ListChemPot{i}),'_Quiver.fig'],'fig');
    close(h)
end

ht1 = toc;
app.Report_Antidote{end+1} = ['CPU time ',num2str(ht1),' s'];

app.Report_Antidote{end+1} = [' '];
app.Report_Antidote{end+1} = [' '];

app.Text_Report_Antidote.Value = app.Report_Antidote;

scroll(app.Text_Report_Antidote,'bottom');

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





