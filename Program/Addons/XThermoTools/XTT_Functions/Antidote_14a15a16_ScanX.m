function [Output,Antidote_VARIABLES,handles] = Antidote_14a15a16_ScanX(AntidoteRecipes,AntidoteMode,WorkVariXMap,MinimOptions,handles)
%
%
%

% Warning this function evaluates isequal(AntidoteMode,10) &
% isequal(AntidoteMode,11) as well as the following definitions:
WhereRecipe14 = 19;
WhereRecipe15 = 20;
WhereRecipe16 = 21;


% Check that we can run this recipe...
if ~handles.BinGfDef.Fluids.Optimize && ~handles.BinGfDef.Oxygen.OptimizeExtraO
    errordlg('The mode optimize "H" or "O" or "C" it not selected','Error Antidote')
    %RunSWOT(0,handles);
    Output.WeCallBingo = 0;
    Output.WeSaveWorkspace = 0;
    Output.Message = 'Error';

    Antidote_VARIABLES = [];
    return
end

% Extract min and max values (case-dependent)
BinGfDef = handles.BinGfDef;

Min_H = 0;
Max_H = 0;
Min_C = 0;
Max_C = 0;

for i = 1:length(BinGfDef.Fluids.Spec)
    if BinGfDef.Fluids.Spec(i).IsActive
        if isequal(BinGfDef.Fluids.Spec(i).ListVariAntidote,{'H'})
            Min_H = handles.BinGfDef.Fluids.Spec(i).Lower;
            Max_H = handles.BinGfDef.Fluids.Spec(i).Upper;
        elseif isequal(BinGfDef.Fluids.Spec(i).ListVariAntidote,{'C'})
            Min_C = handles.BinGfDef.Fluids.Spec(i).Lower;
            Max_C = handles.BinGfDef.Fluids.Spec(i).Upper;
        end
    end
end

switch AntidoteMode
    case WhereRecipe14
        if ~(Max_H -Min_H) > 0
            errordlg('Max(H) must be greater than Min(H)!','Error Antidote')
            RunSWOT(0,handles);
            return
        else
            Min = Min_H;
            Max = Max_H;
            ElementB = 'H';
            Recipe = 14;
        end
    case WhereRecipe15
        if ~(Max_C -Min_C) > 0
            errordlg('Max(C) must be greater than Min(C)!','Error Antidote')
            RunSWOT(0,handles);
            return
        else
            Min = Min_C;
            Max = Max_C;
            ElementB = 'C';
            Recipe = 15;
        end
    case WhereRecipe16
        
        Min = handles.BinGfDef.Oxygen.ExtraO.Lower;
        Max = handles.BinGfDef.Oxygen.ExtraO.Upper;
        
        if ~(Max - Min) > 0
            errordlg('Max(O) must be greater than Min(O)!','Error Antidote')
            RunSWOT(0,handles);
            return
        end
        
        ElementB = 'O';
        Recipe = 16;
        
end

UpdateLIVEWaitBar(1,0,handles);

set(handles.Live_Disp1_Text1,'Visible','On','String','>>');
set(handles.Live_Disp1_Text2,'Visible','On','String',['Scanning over ',char(ElementB),'']);
set(handles.Live_Disp1_Text3,'Visible','On','String','Computing ...','ForegroundColor',[1,0,0]);
drawnow

% Update the live display:
set(handles.axes_Live_PLOT5,'Visible','On');
axes(handles.axes_Live_PLOT5), cla, %colorbar,
xlabel([ElementB]), ylabel('Quality (%)')
title('Quality factors (%)')
axis([Min Max 0 100])
drawnow

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
fprintf('\t\t%s\t%s\t%s\n','El.','Min','Max');
fprintf('%s\t\t%s\t%.4f\t%.4f\n','Binary:',ElementB,Min,Max);
disp(' ')

Name4SaveResults = ['Results_',num2str(Temp),'-',num2str(Press),'.txt'];

% NB: The printed bulk is not used here ...
%OriginalBulk = get(handles.BinTextBulkCompo,'String');

SelectedBinBulk = get(handles.BinPopUpBulkCompoList,'Value');
BinBulkOri = handles.BinBulk(SelectedBinBulk);

TempBinBulk = BinBulkOri;

%Min = handles.BinGfDef.Fluids.Spec(1).Lower;
%Max = handles.BinGfDef.Fluids.Spec(1).Upper;

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
    
    disp(' '); disp(' '); disp(' '); disp(' ');
    disp([' -->  LOOP: ',num2str(i),'/',num2str(NbSteps),''])
    disp(' ')
    
    [Bulk,TempBinBulk] = SuperFast_X_Update(TempBinBulk,{ElementB},X_Vari(i));
    %[Bulk_TEST,TempBinBulk_TEST] = SuperFast_H_Update(TempBinBulk,X_Vari(i));
    
    disp(Bulk);
    %disp(Bulk_TEST);
    
    [BinSet] = SetBinFAST(ProgPath,DefMin,TheSelDatabase,Bulk,handles.BinGfDef);
    
    WorkVariMod = TheriakCall(BinSet,D_Temp,D_Press);
    
    %guidata(hObject, handles);   % not needed
    [BinPhaseDef] = BinButtonCalcPhaseFASTlocal(TempBinBulk, handles);
    [WorkVariXMap] = GenerateResXMap(BinPhaseDef);
    
    [Emin,Evaluation] = Opti_EpsiMinimCalc(WorkVariMod,WorkVariXMap,MinimOptions,1,handles);
    
    ProData2 = UpdateProData2(ProData2,i,Evaluation,Emin,WorkVariMod,WorkVariXMap);
    
    if i > 1
        axes(handles.axes_Live_PLOT5)
        cla
        plot(repmat(X_Vari(1:i),4,1)',[ProData2.Qfactors.Q1(1:i);ProData2.Qfactors.Q2(1:i);ProData2.Qfactors.Q3(1:i);abs(ProData2.Qfactors.Qt(1:i))]','.-')
        xlabel([char(ElementB),' (mol)'])
        ylabel('Qfactors (%)')
        legend({'Qass','Qvol','Qcmp','Qtotal'},'Location','Best')
        title('Quality factors (%)')
        drawnow
    end
    
    UpdateLIVEWaitBar(1,i/length(X_Vari),handles);
    drawnow
end

set(handles.Live_Disp1_Text3,'Visible','On','String','Done','ForegroundColor',[0,0,0]);
drawnow

UpdateLIVEWaitBar(1,1,handles);

set(handles.Live_Disp2_Text1,'Visible','On','String','>>');
set(handles.Live_Disp2_Text2,'Visible','On','String','Saving results');
set(handles.Live_Disp2_Text3,'Visible','On','String','Please wait ...','ForegroundColor',[1,0,0]);
drawnow

% plot the results
FinalPlotProData2(ProData2,Recipe,ElementB,X_Vari,Name4SaveResults);

set(handles.Live_Disp2_Text3,'Visible','On','String','Done','ForegroundColor',[0,0,0]);
drawnow

ht1=toc;
disp(' ')
fprintf('%s\t%.4f\n','CPU time',ht1);







Output.WeCallBingo = 0;
Output.WeSaveWorkspace = 0;
Output.Message = 'Success';

Antidote_VARIABLES = [];

return


