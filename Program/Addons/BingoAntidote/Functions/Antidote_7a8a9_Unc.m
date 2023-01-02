function [Output,Antidote_VARIABLES] = Antidote_7a8a9_Unc(Mode,WorkVariXMap,MinimOptions,app)
%
%
%

% Warning, the variable "Mode" can be: 
%   - Bulk          Recipe 7
%   - PT            Recipe 8
%   - Bulk+PT       Recipe 9

Tmin = app.TminEditField.Value;
Tmax = app.TmaxEditField.Value;
Pmin = app.PminEditField.Value;
Pmax = app.PmaxEditField.Value;

Tstep = app.TstepEditField.Value;
Pstep = app.PstepEditField.Value;

Ti = [Tmin:Tstep:Tmax];
Pi = [Pmin:Pstep:Pmax];

UncPos = app.AntidoteNbPerm1EditField_2.Value;
UncT = app.AntidoteNbPerm1EditField_3.Value;
UncP = app.AntidoteNbPerm1EditField_4.Value;

NbPerm = app.AntidoteNbPerm1EditField.Value;

LIMS = [Ti(1),Ti(end),Pi(1),Pi(end)];

ShapeMode = 1;
TPMode = 1;
if isequal(Mode,'Bulk')
    TPMode = 0;
elseif isequal(Mode,'PT')
    ShapeMode = 0;
end

Temp = app.BingoTemperatureEditField.Value;
D_Temp = num2str(Temp);

Press = app.BingoPressureEditField.Value;
D_Press = num2str(Press*1e4);

if ShapeMode && ~TPMode
    %
    app.TabGroup2.SelectedTab = app.MapTab;
else
    %
    
end

cla(app.UIAxes_LiveAntidote1,'reset')
plot(app.UIAxes_LiveAntidote1,Temp,Press,'ok');
xlabel(app.UIAxes_LiveAntidote1,'Temperature (°C)');
ylabel(app.UIAxes_LiveAntidote1,'Pressure (GPa)');
app.UIAxes_LiveAntidote1.XLim = [Tmin,Tmax];
app.UIAxes_LiveAntidote1.YLim = [Pmin,Pmax];
drawnow

[BinSet] = SetBin(app);

ProgPath = BinSet.Path;
TheSelDatabase = BinSet.Database;
iDB = find(ismember(app.DatabaseListBox.Items,TheSelDatabase));
DefMin = app.BingoDefault.Theriak.Database(iDB).DefMin;

% -------------------------------------------------------------------------
% (3) Perturbation of the shape
SelectedBinBulk = app.ROITree.SelectedNodes.NodeData;
BinBulkOri = app.BinBulk(SelectedBinBulk);

TempBinBulk = BinBulkOri;
CoorOri = BinBulkOri.DomainXrefYref;

% First call for reference ...
Bulk = BinBulkOri.CompositionOriginal;
[BinSet] = SetBinFAST(ProgPath,DefMin,TheSelDatabase,Bulk,app.BinGfDef);
WorkVariModRef = TheriakCall(BinSet,D_Temp,D_Press);

VolFrac = zeros(NbPerm,length(WorkVariModRef.Names));
COMP = zeros(NbPerm,length(WorkVariModRef.Names),length(WorkVariModRef.Els));

[Result,LinkRef] = Bingo_Qasm(WorkVariModRef,WorkVariXMap,0,0,'',app);

NbElemsTher = WorkVariModRef.NbEl;
ElemsListTher = WorkVariModRef.Els;

NbElemsXMap = WorkVariXMap.NbEl;
ElemsListXMap = WorkVariXMap.Els;

NbElems = NbElemsTher;
ElemsList = ElemsListTher;

[Ok,WhereIsMem] = ismember(ElemsListTher,ElemsListXMap);

WorkVariXMap.COMPok = zeros(WorkVariXMap.NbPhases,NbElemsTher);
WorkVariXMap.COMPok(:,find(Ok)) = WorkVariXMap.COMP(:,WhereIsMem(find(Ok)));

Tsimul = Temp+randn(1,NbPerm).*UncT;
Psimul = Press+randn(1,NbPerm).*UncP;

for i=1:NbPerm
    
    if ShapeMode
        CoorSim = CoorOri + randn(size(CoorOri)).*UncPos;
        TempBinBulk.DomainXrefYref = CoorSim;
        TempBinBulk = BinUpdateBulkComposition(app,TempBinBulk,1);
    end
    Bulk = TempBinBulk.CompositionOriginal;
    
    if NbPerm<=100
        if ShapeMode
            hold(app.UIAxes,'on');
            CoorOriForPlot = [CoorSim;CoorSim(1,:)];
            plot(app.UIAxes,CoorOriForPlot(:,1),CoorOriForPlot(:,2),'-k');
            drawnow
        end
        if TPMode
            hold(app.UIAxes_LiveAntidote1,'on');
            plot(app.UIAxes_LiveAntidote1,Tsimul(i),Psimul(i),'ok');
            drawnow
        end
    end
    
    if ShapeMode
        [BinSet] = SetBinFAST(ProgPath,DefMin,TheSelDatabase,Bulk,app.BinGfDef);
    end
    if TPMode
        D_Temp = num2str(Tsimul(i));
        D_Press = num2str(Psimul(i) * 1e4);
    end
    
    WorkVariMod = TheriakCall(BinSet,D_Temp,D_Press);
    
    if isequal(WorkVariModRef.Names,WorkVariMod.Names)
        VolFrac(i,:) = WorkVariMod.VolFrac;
        
        CompTherMatch = zeros(size(LinkRef.PhasesNames,2),NbElems);
        CompXMapMatch = zeros(size(LinkRef.PhasesNames,2),NbElems);
        
        CompTherMatch(find(LinkRef.TherIsIn),:) = WorkVariMod.COMP(LinkRef.TherIndices(find(LinkRef.TherIndices)),:);
        CompXMapMatch(find(LinkRef.XMapIsIn),:) = WorkVariXMap.COMPok(LinkRef.XMapIndices(find(LinkRef.XMapIndices)),:);
        
        %                 % Use one oxygen if the phase is not in the map
        %                 WhereAbsXMap = find(sum(CompXMapMatch,2) == 0);
        %                 if length(WhereAbsXMap)
        %                     CompXMapMatch(WhereAbsXMap,1) = ones(length(WhereAbsXMap));
        %                 end
        %
        %                 Where2Compare = find(CompTherMatch(:,1));
        %                 CorrOxygens = ones(size(CompTherMatch(:,1)));
        %                 CorrOxygens(Where2Compare) = CompXMapMatch(Where2Compare,1)./CompTherMatch(Where2Compare,1);
        %                 CompTherMatchDISP = CompTherMatch .* repmat(CorrOxygens,1,size(CompTherMatch,2));
        %
        %                 COMP(i,:,:) = CompTherMatchDISP;        % WorkVariMod.COMP;
        
        COMP(i,:,:) = WorkVariMod.COMP;
        
    else
        
        TempVol = zeros(size(WorkVariModRef.VolFrac));
        TempCOMP = zeros(size(WorkVariModRef.COMP));
        
        [Ok,Where] = ismember(WorkVariModRef.Names,WorkVariMod.Names);
        
        WhereNonZero = find(Ok);
        
        TempVol(WhereNonZero) = WorkVariMod.VolFrac(Where(WhereNonZero));
        TempCOMP(WhereNonZero,:) = WorkVariMod.COMP(Where(WhereNonZero),:);
        
        VolFrac(i,:) = TempVol;
        COMP(i,:,:) = TempCOMP;
        
    end
    
end

for i=1:length(WorkVariModRef.Names)
    app.Report_Antidote{end+1} = [' '];
    app.Report_Antidote{end+1} = [' '];
    
    Selected = find(VolFrac(:,i));
    
    app.Report_Antidote{end+1} = ['PHASE: ',WorkVariModRef.Names{i},' (selected: ',num2str(length(Selected)),'/',num2str(NbPerm),')'];
    app.Report_Antidote{end+1} = ['....: mean | std | % (at 1s)'];
    
    app.Report_Antidote{end+1} = ['VOLs: ',num2str(mean(VolFrac(Selected,i))),' | ',num2str(std(VolFrac(Selected,i))),' | ',num2str(std(VolFrac(Selected,i))/mean(VolFrac(Selected,i))*100)];
    app.Report_Antidote{end+1} = ['VOLu: ',num2str(mean(VolFrac(:,i))),' | ',num2str(std(VolFrac(:,i))),' | ',num2str(std(VolFrac(:,i))/mean(VolFrac(:,i))*100)];
    app.Report_Antidote{end+1} = ['....'];
    
    
    for j=1:length(WorkVariModRef.Els)
        if ~isequal(WorkVariModRef.Els{j},'O') && ~isequal(WorkVariModRef.Els{j},'H') && ~isequal(WorkVariModRef.Els{j},'E') && mean(COMP(:,i,j))
            app.Report_Antidote{end+1} = [char(WorkVariModRef.Els{j}),': ',num2str(mean(COMP(Selected,i,j))),' | ',num2str(std(COMP(Selected,i,j))),' | ',num2str(std(COMP(Selected,i,j))/(mean(COMP(Selected,i,j))+1e-19)*100)];
        end
    end
    
    figure, hist(VolFrac(Selected,i)), title([WorkVariModRef.Names{i},' VOL (',num2str(length(Selected)),'/',num2str(NbPerm),')'])
    
end
app.Report_Antidote{end+1} = [' '];
app.Report_Antidote{end+1} = [' '];

app.Text_Report_Antidote.Value = app.Report_Antidote;

Output.WeCallBingo = 0;
Output.WeSaveWorkspace = 0;
Output.Message = 'Success';

Antidote_VARIABLES = [];


end



