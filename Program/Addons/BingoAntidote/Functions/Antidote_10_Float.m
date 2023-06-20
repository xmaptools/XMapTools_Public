function [Output,Antidote_VARIABLES] = Antidote_10_Float(WorkVariXMap,MinimOptions,app)
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

Ti = [Tmin:(Tmax-Tmin)/(Res-1):Tmax];
Pi = [Pmin:(Pmax-Pmin)/(Res-1):Pmax];

LIMS = [Ti(1),Ti(end),Pi(1),Pi(end)];

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

app.Report_Antidote{end+1} = ['Antidote: Recipe [10] - Floating window (fixed P–T, variable bulk)'];
app.Report_Antidote{end+1} = '';
% app.Report_Antidote{end+1} = ['Bulk: ',BinSet.Bulk2Display,''];
app.Report_Antidote{end+1} = ['Database: ',BinSet.Database,''];
app.Report_Antidote{end+1} = '';
app.Text_Report_Antidote.Value = app.Report_Antidote;

% Generate the path from ROI objects
GenerateROIPath(app,'off')

NbSteps = size(app.TextureROI.InterpPositions,1);

% Create ProData to store all the information along the profile
ProData.Qfactors.Q1 = zeros(1,NbSteps);
ProData.Qfactors.Q2 = zeros(1,NbSteps);
ProData.Qfactors.Q3 = zeros(1,NbSteps);
ProData.Qfactors.Qt = zeros(1,NbSteps);

ProData.MinProp.ListMin = '';
ProData.MinProp.ProMatrix = zeros(1,NbSteps);

ProData.MinPropXray.ListMin = '';
ProData.MinPropXray.ProMatrix = zeros(1,NbSteps);

ProData.MinComp.ListMin = '';
ProData.MinComp.MinQ = zeros(1,NbSteps);
ProData.MinComp.MinQw = zeros(1,NbSteps);

ProData.ChemPot.ListChemPot = '';
ProData.ChemPot.ChemPotMatrix = zeros(1,NbSteps);

ROI = drawrectangle(app.UIAxes,'Position',app.TextureROI.InterpPositions(1,:),'Color',[1,0,0.7137],'InteractionsAllowed','none');

tic
for i = 1:NbSteps
    
    app.Report_Antidote{end+1} = [' -->  LOOP: ',num2str(i),'/',num2str(NbSteps)];
    
    % Now we need to update the bulk composition
    ROI.Position = app.TextureROI.InterpPositions(i,:);
    TempBinBulk.DomainXrefYref = ROI.Vertices;
    
    [TempBinBulk] = BinUpdateBulkComposition(app,TempBinBulk,SelectedBinBulk);
    
    [Bulk,TempBinBulk] = SuperFastBulkUpdate(app.BinGfDef,TempBinBulk,1);
    
    [BinSet] = SetBinFAST(ProgPath,DefMin,TheSelDatabase,Bulk,app.BinGfDef);
    
    app.Report_Antidote{end+1} = ['      BULK: ',BinSet.Bulk2Display];
    
    WorkVariMod = TheriakCall(BinSet,D_Temp,D_Press);
    
    [BinPhaseDef] = BinButtonCalcPhaseFASTlocal(TempBinBulk, app);
    [WorkVariXMap] = GenerateResXMap(BinPhaseDef);
    
    [Emin,Evaluation] = Opti_EpsiMinimCalc(WorkVariMod,WorkVariXMap,MinimOptions,0,app);
    
    ProData = UpdateProData(ProData,i,Evaluation,Emin,WorkVariMod,WorkVariXMap);
    
    app.Text_Report_Antidote.Value = app.Report_Antidote;
    pause(0.1)
    scroll(app.Text_Report_Antidote,'bottom');

end

app.Report_Antidote{end+1} = [' '];
app.Report_Antidote{end+1} = ['Saving results...'];
app.Report_Antidote{end+1} = [' '];
app.Text_Report_Antidote.Value = app.Report_Antidote;
pause(0.1)
scroll(app.Text_Report_Antidote,'bottom');

% plot the results
FinalPlotProData(ProData,NbSteps);

app.Report_Antidote{end+1} = [' '];

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


end



