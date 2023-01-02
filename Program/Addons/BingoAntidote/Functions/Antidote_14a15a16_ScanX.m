function [Output,Antidote_VARIABLES] = Antidote_14a15a16_ScanX(AntidoteMode,WorkVariXMap,MinimOptions,app)
%
%

if isequal(AntidoteMode,'H') && isequal(app.BinGfDef.Fluids.Spec(1).Optimize,0)
    uialert(app.BingoAntidote_GUI,'Select "Optimize N" for H in the LBC tab to use this recipe!','Error Antidote');
    Output.WeCallBingo = 0;
    Output.WeSaveWorkspace = 0;
    Output.Message = 'Error';

    Antidote_VARIABLES = [];
    return
end
if isequal(AntidoteMode,'C') && isequal(app.BinGfDef.Fluids.Spec(2).Optimize,0)
    uialert(app.BingoAntidote_GUI,'Select "Optimize N" for C in the LBC tab to use this recipe!','Error Antidote');
    Output.WeCallBingo = 0;
    Output.WeSaveWorkspace = 0;
    Output.Message = 'Error';

    Antidote_VARIABLES = [];
    return
end
if isequal(AntidoteMode,'O') && isequal(app.BinGfDef.Oxygen.OptimizeExtraO,0)
    uialert(app.BingoAntidote_GUI,'Select "Optimize O" in the LBC tab to use this recipe!','Error Antidote');
    Output.WeCallBingo = 0;
    Output.WeSaveWorkspace = 0;
    Output.Message = 'Error';

    Antidote_VARIABLES = [];
    return
end
    
% Extract min and max values (case-dependent)
BinGfDef = app.BinGfDef;

Min_H = 0;
Max_H = 0;
Min_C = 0;
Max_C = 0;

for i = 1:length(BinGfDef.Fluids.Spec)
    if BinGfDef.Fluids.Spec(i).IsActive
        if isequal(BinGfDef.Fluids.Spec(i).ListVariAntidote,{'H'})
            Min_H = BinGfDef.Fluids.Spec(i).Lower;
            Max_H = BinGfDef.Fluids.Spec(i).Upper;
        elseif isequal(BinGfDef.Fluids.Spec(i).ListVariAntidote,{'C'})
            Min_C = BinGfDef.Fluids.Spec(i).Lower;
            Max_C = BinGfDef.Fluids.Spec(i).Upper;
        end
    end
end

switch AntidoteMode
    case 'H'
        if ~(Max_H -Min_H) > 0
            uialert(app.BingoAntidote_GUI,'Max(H) must be greater than Min(H)!','Error Antidote');
            return
        else
            Min = Min_H;
            Max = Max_H;
            ElementB = 'H';
            app.Report_Antidote{end+1} = ['Antidote: Recipe [14] - Scanning H (fixed P-T)'];
        end
    case 'C'
        if ~(Max_C -Min_C) > 0
            uialert(app.BingoAntidote_GUI,'Max(C) must be greater than Min(C)!','Error Antidote');
            return
        else
            Min = Min_C;
            Max = Max_C;
            ElementB = 'C';
            app.Report_Antidote{end+1} = ['Antidote: Recipe [14] - Scanning C (fixed P-T)'];
        end
    case 'O'
        
         Min = app.OxMin_EditField.Value;
         Max = app.OxMax_EditField.Value;
        
        if ~(Max - Min) > 0
            uialert(app.BingoAntidote_GUI,'Max(O) must be greater than Min(O)!','Error Antidote')
            return
        end
        
        ElementB = 'O'; 
        app.Report_Antidote{end+1} = ['Antidote: Recipe [14] - Scanning O (fixed P-T)'];
end

[BinSet] = SetBin(app);

app.Report_Antidote{end+1} = ['Bulk: ',BinSet.Bulk2Display];
app.Report_Antidote{end+1} = ['Database: ',BinSet.Database];
app.Report_Antidote{end+1} = '';
app.Text_Report_Antidote.Value = app.Report_Antidote;


xlabel(app.UIAxes_LiveAntidote1,ElementB)
ylabel(app.UIAxes_LiveAntidote1,'Quality (%)')
title(app.UIAxes_LiveAntidote1,'Quality factors (%)')
axis(app.UIAxes_LiveAntidote1,[Min Max 0 100])
drawnow


% -------------------------------------------------------------------------
% (1) SET initial variables for Theriak (BinSet)

ProgPath = BinSet.Path;
DefMin = app.BingoDefault.Theriak.Database(find(ismember(app.DatabaseListBox.Items,app.DatabaseListBox.Value))).DefMin;
TheSelDatabase = app.DatabaseListBox.Value;

Temp = app.BingoTemperatureEditField.Value;
D_Temp = num2str(Temp);

Press = app.BingoPressureEditField.Value;
D_Press = num2str(Press * 1e4);

app.Report_Antidote{end+1} = ['Temperature: ',D_Temp,' (°C)'];
app.Report_Antidote{end+1} = ['Pressure: ',D_Press,' (GPa)'];
app.Report_Antidote{end+1} = ['Binary for ',ElementB,' in range: ',num2str(Min),'-',num2str(Max)];
app.Report_Antidote{end+1} = '';

app.Text_Report_Antidote.Value = app.Report_Antidote;

Name4SaveResults = ['Results_',num2str(Temp),'-',num2str(Press),'.txt'];

SelectedBinBulk = app.ROITree.SelectedNodes.NodeData;
BinBulkOri = app.BinBulk(SelectedBinBulk);
TempBinBulk = BinBulkOri;

% goto nb loop
NbSteps = 30;
Step = (Max-Min)/(NbSteps-1);

X_Vari = [Min:Step:Max];

% Create ProData2 to store all the information along the profile
ProData2.Qfactors.Q1 = zeros(1,NbSteps);
ProData2.Qfactors.Q2 = zeros(1,NbSteps);
ProData2.Qfactors.Q3 = zeros(1,NbSteps);
ProData2.Qfactors.Qt = zeros(1,NbSteps);

ProData2.MinProp.ListMin = '';
ProData2.MinProp.ProMatrix = zeros(1,NbSteps);

ProData2.MinPropXray.ListMin = '';
ProData2.MinPropXray.ProMatrix = zeros(1,NbSteps);

ProData2.MinComp.ListMin = '';
ProData2.MinComp.MinQ = zeros(1,NbSteps);
ProData2.MinComp.MinQw = zeros(1,NbSteps);

tic
for i = 1:length(X_Vari)
    
    app.Report_Antidote{end+1} = [' -->  LOOP: ',num2str(i),'/',num2str(NbSteps)];
    
    [Bulk,TempBinBulk] = SuperFast_X_Update(TempBinBulk,{ElementB},X_Vari(i));
    %[Bulk_TEST,TempBinBulk_TEST] = SuperFast_H_Update(TempBinBulk,X_Vari(i));
    
    app.Report_Antidote{end+1} = [Bulk];
    app.Text_Report_Antidote.Value = app.Report_Antidote;
    
    [BinSet] = SetBinFAST(ProgPath,DefMin,TheSelDatabase,Bulk,app.BinGfDef);
    
    WorkVariMod = TheriakCall(BinSet,D_Temp,D_Press);
    
    [BinPhaseDef] = BinButtonCalcPhaseFASTlocal(TempBinBulk, app);
    [WorkVariXMap] = GenerateResXMap(BinPhaseDef);
    
    [Emin,Evaluation] = Opti_EpsiMinimCalc(WorkVariMod,WorkVariXMap,MinimOptions,0,app);
    
    ProData2 = UpdateProData2(ProData2,i,Evaluation,Emin,WorkVariMod,WorkVariXMap);
    
    if i > 1
        cla(app.UIAxes_LiveAntidote1)
        plot(app.UIAxes_LiveAntidote1,repmat(X_Vari(1:i),4,1)',[ProData2.Qfactors.Q1(1:i);ProData2.Qfactors.Q2(1:i);ProData2.Qfactors.Q3(1:i);abs(ProData2.Qfactors.Qt(1:i))]','.-')
        xlabel(app.UIAxes_LiveAntidote1,[char(ElementB),' (mol)'])
        ylabel(app.UIAxes_LiveAntidote1,'Qfactors (%)')
        legend(app.UIAxes_LiveAntidote1,{'Qass','Qvol','Qcmp','Qtotal'},'Location','Best')
        title(app.UIAxes_LiveAntidote1,'Quality factors (%)')
        drawnow
        
        pause(0.1)
        scroll(app.Text_Report_Antidote,'bottom');
    end
    
end

% plot the results
FinalPlotProData2(ProData2,14,ElementB,X_Vari,Name4SaveResults);

ht1=toc;

app.Report_Antidote{end+1} = '';
app.Report_Antidote{end+1} = ['CPU time ',num2str(ht1)];
app.Report_Antidote{end+1} = '';

app.Text_Report_Antidote.Value = app.Report_Antidote;

Output.WeCallBingo = 0;
Output.WeSaveWorkspace = 0;
Output.Message = 'Success';

Antidote_VARIABLES = [];

return


