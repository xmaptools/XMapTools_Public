function [Output,Antidote_VARIABLES,handles] = Antidote_7a8a9_Unc(AntidoteRecipes,AntidoteMode,WorkVariXMap,MinimOptions,handles)
%
%
%


% Warning this function evaluates isequal(AntidoteMode,10) & isequal(AntidoteMode,11)


% Update the live display:
eval(['TCi = [',get(handles.BinTextBinTCminmax,'String'),'];']);
eval(['Pi = [',get(handles.BinTextBinPminmax,'String'),'];']);

LIMS = [TCi(1)-1,TCi(end)+1,Pi(1)-1,Pi(end)+1];

ShapeMode = 1;
TPMode = 1;
if isequal(AntidoteMode,10)
    TPMode = 0;
elseif isequal(AntidoteMode,11)
    ShapeMode = 0;
end

if ShapeMode && ~TPMode
    InitPanelDisp(1,handles); % RESULTS
else
    set(handles.axes_Live_PLOT5,'Visible','On');
    axes(handles.axes_Live_PLOT5), cla,
    xlabel('Temperature (C)'), ylabel('Pressure (Kbar)')
    axis(LIMS)
    drawnow
end


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


% -------------------------------------------------------------------------
% Options (to be integrated to the interface later)

NbPerm = str2num(get(handles.BinTextAnt1,'String'));
PerturbShape = str2num(get(handles.BinTextAnt2,'String'));
eval(['PerturbTP = [',get(handles.BinTextAnt3,'String'),'];']);

% -------------------------------------------------------------------------
% (2) T & P from Bingo

D_Temp = get(handles.BinTextBinTC,'String');
Temp = str2num(D_Temp);
D_Press = get(handles.BinTextBinP,'String');
Press = str2num(D_Press);

axes(handles.axes_Live_PLOT5), hold on
plot(Temp,Press,'or','MarkerEdgeColor','k','MarkerFaceColor','r');
axis(LIMS)
xlabel('Temperature');
ylabel('Pressure');

% NB: The printed bulk is not used here ...
%OriginalBulk = get(handles.BinTextBulkCompo,'String');

% -------------------------------------------------------------------------
% (3) Perturbation of the shape
SelectedBinBulk = get(handles.BinPopUpBulkCompoList,'Value');
BinBulkOri = handles.BinBulk(SelectedBinBulk);

TempBinBulk = BinBulkOri;
CoorOri = BinBulkOri.DomainXrefYref;

% First call for reference ...
Bulk = BinBulkOri.CompositionOriginal;
[BinSet] = SetBinFAST(ProgPath,DefMin,TheSelDatabase,Bulk,handles.BinGfDef);
WorkVariModRef = TheriakCall(BinSet,D_Temp,D_Press);

VolFrac = zeros(NbPerm,length(WorkVariModRef.Names));
COMP = zeros(NbPerm,length(WorkVariModRef.Names),length(WorkVariModRef.Els));

[Result,LinkRef] = Bingo_Qasm(WorkVariModRef,WorkVariXMap,0,0,handles);

NbElemsTher = WorkVariModRef.NbEl;
ElemsListTher = WorkVariModRef.Els;

NbElemsXMap = WorkVariXMap.NbEl;
ElemsListXMap = WorkVariXMap.Els;

NbElems = NbElemsTher;
ElemsList = ElemsListTher;

[Ok,WhereIsMem] = ismember(ElemsListTher,ElemsListXMap);

WorkVariXMap.COMPok = zeros(WorkVariXMap.NbPhases,NbElemsTher);
WorkVariXMap.COMPok(:,find(Ok)) = WorkVariXMap.COMP(:,WhereIsMem(find(Ok)));

Tsimul = Temp+randn(1,NbPerm).*PerturbTP(1);
Psimul = Press+randn(1,NbPerm).*PerturbTP(2);
%keyboard
for i=1:NbPerm
    
    if ShapeMode
        CoorSim = CoorOri + randn(size(CoorOri)).*PerturbShape;
        TempBinBulk.DomainXrefYref = CoorSim;
        [TempBinBulk] = BinUpdateBulkComposition(TempBinBulk,1,handles);
    end
    Bulk = TempBinBulk.CompositionOriginal;
    
    if NbPerm<=100
        if ShapeMode && ~TPMode
            axes(handles.axes1), hold on
            CoorOriForPlot = [CoorSim;CoorSim(1,:)];
            plot(CoorOriForPlot(:,1),CoorOriForPlot(:,2),'-k');
            %axis(ValuesAxis);
            drawnow
        end
        if TPMode
            axes(handles.axes_Live_PLOT5), hold on
            plot(Tsimul(i),Psimul(i),'ok');
            %plot(Temp,Press,'or','MarkerEdgeColor','k','MarkerFaceColor','r');
            axis(LIMS)
            xlabel('Temperature');
            ylabel('Pressure');
            
            drawnow
        end
    end
    
    if ShapeMode
        [BinSet] = SetBinFAST(ProgPath,DefMin,TheSelDatabase,Bulk,handles.BinGfDef);
    end
    if TPMode
        D_Temp = num2str(Tsimul(i));
        D_Press = num2str(Psimul(i));
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

% Display ....
for i=1:length(WorkVariModRef.Names)
    disp(' ')
    disp(' ')
    
    Selected = find(VolFrac(:,i));
    
    fprintf('%s\n',['PHASE: ',WorkVariModRef.Names{i},' (selected: ',num2str(length(Selected)),'/',num2str(NbPerm),')']);
    fprintf('%s\t%s\t%s\t%s\n',' ','mean','std','% (1o)');
    
    fprintf('%s\t%.4f\t%.4f\t%.4f\n','  VOLs',mean(VolFrac(Selected,i)),std(VolFrac(Selected,i)),std(VolFrac(Selected,i))/mean(VolFrac(Selected,i))*100);
    fprintf('%s\t%.4f\t%.4f\t%.4f\n','  VOLu',mean(VolFrac(:,i)),std(VolFrac(:,i)),std(VolFrac(:,i))/mean(VolFrac(:,i))*100);
    fprintf('%s\n','  ----');
    
    for j=1:length(WorkVariModRef.Els)
        if ~isequal(WorkVariModRef.Els{j},'O') && ~isequal(WorkVariModRef.Els{j},'H') && ~isequal(WorkVariModRef.Els{j},'E') && mean(COMP(:,i,j))
            fprintf('%s%s\t%.4f\t%.4f\t%.4f\n','  ',char(WorkVariModRef.Els{j}),mean(COMP(Selected,i,j)),std(COMP(Selected,i,j)),std(COMP(Selected,i,j))/(mean(COMP(Selected,i,j))+1e-19)*100);
        end
    end
    figure, hist(VolFrac(Selected,i)), title([WorkVariModRef.Names{i},' VOL (',num2str(length(Selected)),'/',num2str(NbPerm),')'])
end
disp(' ')






Output.WeCallBingo = 0;
Output.WeSaveWorkspace = 0;
Output.Message = 'Success';

Antidote_VARIABLES = [];


return



