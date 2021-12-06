function [Output,Antidote_VARIABLES,handles] = Antidote_3a6_PTunc(AntidoteRecipes,AntidoteMode,WorkVariXMap,MinimOptions,handles)
%
%
%


% Warning this function evaluate isequal(AntidoteMode,4)


% warning NOT CHECKED PL March 2019 ---


% Tolerance factor
UncStr = get(handles.BinTextAnt4,'String');
UncInt = strread(UncStr,'%f','delimiter',',');

if ~length(UncInt)
    return
end

eval(['TCi = [',get(handles.BinTextBinTCminmax,'String'),'];']);
eval(['Pi = [',get(handles.BinTextBinPminmax,'String'),'];']);

LIMS = [TCi(1),TCi(end),Pi(1),Pi(end)];

BinSet = SetBin(handles);

% variables for single phase computations:
BinPhaseDef = handles.BinPhaseDef(get(handles.BinPopUpVarPhaseList,'Value'));

D_Temp = get(handles.BinTextBinTC,'String');
Tinit = str2num(D_Temp);
D_Press = get(handles.BinTextBinP,'String');
Pinit = str2num(D_Press);

StepsT = length(TCi);
StepsP = length(Pi);

dT = round((TCi(end)-TCi(1))/max([StepsT,StepsP]));
dP = round((Pi(end)-Pi(1))/max([StepsT,StepsP]));

DeltaT = dT;
DeltaP = dP;

%keyboard

fprintf('%s\t%s\n','Antidote',char(AntidoteRecipes(AntidoteMode)))
fprintf('%s\t\t%s\n','Bulk',BinSet.Bulk2Display)
fprintf('%s\t%s\n','Database',BinSet.Database);
fprintf('%s\t%s\n','Loops',num2str(length(UncInt)));
disp(' ')
fprintf('%s\t\t%.0f\n','Temp. P',Tinit);
fprintf('%s\t%.0f\n','Temp. TC',Pinit);
disp(' ')

if MinimOptions.Weights.Use
    fprintf('%s\t%s\t%s\n','Equation:','Other',['[E4 = -(',num2str(MinimOptions.Weights.Values(MinimOptions.Weights.Selected,1)),'*E1 + ',num2str(MinimOptions.Weights.Values(MinimOptions.Weights.Selected,2)),'*E2 + ',num2str(MinimOptions.Weights.Values(MinimOptions.Weights.Selected,3)),'*E3)]']);
else
    fprintf('%s\t%s\t%s\n','Equation:','Classic','[E4 = -1/3*(E1 + (E1/100)*E2 + (E1/100)*(E2/100)*E3)]');
end


% PART 1 - AUTO-REFINEMENT as in GRTMOD (Lanari et al., 2017), EJM.

% Update the live display:
set(handles.axes_Live_PLOT5,'Visible','On');
axes(handles.axes_Live_PLOT5), cla,
xlabel('Temperature (C)'), ylabel('Pressure (Kbar)')
title(['P-T uncertainty (',UncStr,' %)'])
plot(Tinit,Pinit,'o','MarkerFaceColor','r','MarkerEdgeColor','k');
axis(LIMS);

drawnow

tic
for iUnc=1:length(UncInt)
    
    if length(UncInt) > 1
        disp(' '), disp(' ')
        disp([' -->  LOOP: ',num2str(iUnc),'/',num2str(length(UncInt)),' [Tol(%) = ',num2str(UncInt(iUnc)),']'])
        disp(' ')
    else
        disp(' ')
        fprintf('%s\t\t%.6f\n','Tol(%)',UncInt(iUnc));
        disp(' ')
    end
    
    disp(' ')
    disp(['##### Auto-refinement stage (',num2str(Tinit),' C & ',num2str(Pinit),' bar) #####']);
    disp(' ');
    
    
    set(handles.Live_Disp1_Text1,'Visible','On','String','>>');
    set(handles.Live_Disp1_Text2,'Visible','On','String','Auto-refinement stage');
    set(handles.Live_Disp1_Text3,'Visible','On','String','Computing ...','ForegroundColor',[1,0,0]);
    drawnow
    
    % Call a first Bingo to get E4 at Tinit and Pinit
    if isequal(AntidoteMode,4)
        [ResSolution,Evaluation] = OptiBingoPT([1,1],[Tinit,Pinit],LIMS,BinSet,WorkVariXMap,MinimOptions,handles);
    else
        [ResSolution] = OptiBingoPTSinglePhase([1,1],[Tinit,Pinit],LIMS,BinSet,WorkVariXMap,BinPhaseDef);
    end
    
    if ResSolution > 0
        ResError = ResSolution + ResSolution*UncInt(iUnc)/100;
    else
        ResError = ResSolution - ResSolution*UncInt(iUnc)/100;
    end
    
    Tfin=[];
    Pfin=[];
    ErrorsT=[];
    ErrorsP=[];
    ErrorsE4=[];
    
    Loops = 0;
    DirectionsToTest = [1,1,1,1,1,1,1,1];
    PositiveTests =    [0,0,0,0,0,0,0,0];
    
    while 1
        Loops = Loops + 1;
        NewCoordinates = ComptDirections2Coordinates(DirectionsToTest,dT,dP,Tinit,Pinit);
        TheNewRes = [];
        
        for i = 1:length(NewCoordinates)
            
            if NewCoordinates(i,1) < LIMS(1) || NewCoordinates(i,1) > LIMS(2) || ...
                    NewCoordinates(i,2) < LIMS(3) || NewCoordinates(i,2) > LIMS(4)
                
                % We are outside the P-T range and it makes no sense to keep
                % working... (see GRTMOD comment)
                NewCoordinates(i,1) = 0;
                NewCoordinates(i,2) = 0;
                DirectionsToTest(i) = 0;
            end
            
            
            if NewCoordinates(i,1) > 0
                
                if isequal(AntidoteMode,4)
                    [TheNewRes,Evaluation] = OptiBingoPT([1,1],[NewCoordinates(i,1),NewCoordinates(i,2)],LIMS,BinSet,WorkVariXMap,MinimOptions,handles);
                else
                    [TheNewRes] = OptiBingoPTSinglePhase([1,1],[NewCoordinates(i,1),NewCoordinates(i,2)],LIMS,BinSet,WorkVariXMap,BinPhaseDef);
                end
                %[TheNewRes,Evaluation] = OptiBingoPT([1,1],[NewCoordinates(i,1),NewCoordinates(i,2)],LIMS,BinSet,WorkVariXMap,handles);
                
                if TheNewRes > ResError 
                    DirectionsToTest(i) = 0;
                else
                    PositiveTests(i) = PositiveTests(i)+1;
                end
            end
            
        end
        
        TheExitTest = sum(DirectionsToTest);
        disp(['  --> Loop [',num2str(Loops),'] - (',num2str(TheExitTest),')'])
        
        if ~TheExitTest
            disp(' ');
            break
        end
        
        % Update DirectionsToTest...
        dT = dT + DeltaT;
        dP = dP + DeltaP;
        
    end
    
    FinalCoordinates = FinalDirections2Coordinates(PositiveTests,DeltaT,DeltaP,Tinit,Pinit);
    
    ErrorsT = FinalCoordinates(:,1);
    ErrorsP = FinalCoordinates(:,2);
    
    axes(handles.axes_Live_PLOT5), cla, hold on
    for i=1:length(ErrorsT)
        plot([ErrorsT(i),Tinit],[ErrorsP(i),Pinit],'-k');
    end
    xlabel('Temperature (C)'), ylabel('Pressure (Kbar)')
    title(['P-T uncertainty (',UncStr,' %)'])
    plot(Tinit,Pinit,'o','MarkerFaceColor','r','MarkerEdgeColor','k');
    axis(LIMS);
    
    set(handles.Live_Disp1_Text3,'Visible','On','String','Done ','ForegroundColor',[0,0,0]);
    drawnow
    
    % PART 2 - MONTE-CARLO simulation to refine the shape of the uncertainty enveloppe
    
    NbPerm = MinimOptions.Uncertainty.NbPerm1;
    NbPermRef = MinimOptions.Uncertainty.NbPerm2;
    
    if ischar(NbPerm)
        NbPerm = str2num(NbPerm);
    end
    if ischar(NbPermRef)
        NbPermRef = str2num(NbPermRef);
    end
    
    disp(' ')
    disp(['##### Uncertainty enveloppe stage (',num2str(Tinit),' C & ',num2str(Pinit),' bar) #####']);
    disp(' ');
    
    set(handles.Live_Disp2_Text1,'Visible','On','String','>>');
    set(handles.Live_Disp2_Text2,'Visible','On','String','Monte-Carlo simulation (1/3)');
    set(handles.Live_Disp2_Text3,'Visible','On','String','Computing ...','ForegroundColor',[1,0,0]);
    drawnow
    
    MaxTdiff = max(abs(Tinit-ErrorsT));
    if isequal(MaxTdiff,0)
        MaxTdiff = 25;
    end
    
    MaxPdiff = max(abs(Pinit-ErrorsP));
    if isequal(MaxPdiff,0)
        MaxPdiff = 1000;
    end
    
    % OPTIMIZATION 1:
    disp('  --> Optimization [1/3] '),
    Tinv = Tinit + randn(floor(NbPerm),1)*MaxTdiff/1.4;
    Pinv = Pinit + randn(floor(NbPerm),1)*MaxPdiff/1.4;
    
    % Add the points that are inside from the auto-refinement stage
    Tinv(end+1) = Tinit;
    Pinv(end+1) = Pinit;
    
    for i = 1:length(ErrorsT);
        if isequal(ErrorsT(i),Tinit) && isequal(ErrorsP(i),Pinit)
            % nothing to be done here.
        else
            Tinv(end+1) = ErrorsT(i) + randn(1)*0.01; % small difference in T to not have two identical points
            Pinv(end+1) = ErrorsP(i) + randn(1)*0.1;
        end
    end
    
    % Check for permuation outside the P-T range
    TPinside = find(Tinv > LIMS(1) & Tinv < LIMS(2) & Pinv > LIMS(3) & Pinv < LIMS(4));
    
    Tsimul(1:length(TPinside)) = Tinv(TPinside);
    Psimul(1:length(TPinside)) = Pinv(TPinside);
    
    axes(handles.axes_Live_PLOT5), hold on
    plot(Tsimul,Psimul,'.b');
    
    % Second simulation: 
    NbPermAugm = NbPerm*2;
    
    Tsimul(length(TPinside)+1:length(TPinside)+NbPermAugm) = LIMS(1) + (LIMS(2)-LIMS(1))*rand(NbPermAugm,1);
    Psimul(length(TPinside)+1:length(TPinside)+NbPermAugm) = LIMS(3) + (LIMS(4)-LIMS(3))*rand(NbPermAugm,1);
    
    disp(['      ... ',num2str(NbPerm),' + ',num2str(NbPermAugm),' = ',num2str(numel(Tsimul)),' minimizations']);
    
    axes(handles.axes_Live_PLOT5), hold on
    plot(Tsimul(length(TPinside)+1:length(TPinside)+NbPermAugm),Psimul(length(TPinside)+1:length(TPinside)+NbPermAugm),'.k');
    drawnow
    
    TheRes = zeros(size(Tsimul));
    
    UpdateLIVEWaitBar(2,0,handles);
    
    Compt = 0;
    for i=1:length(Tsimul)
        Compt = Compt+1;
        if Compt > 5
            Compt = 0;
            UpdateLIVEWaitBar(2,i/length(Tsimul),handles);
        end
        if isequal(AntidoteMode,4)
            [TheRes(i),Evaluation] = OptiBingoPT([1,1],[Tsimul(i),Psimul(i)],LIMS,BinSet,WorkVariXMap,MinimOptions,handles);
        else
            [TheRes(i)] = OptiBingoPTSinglePhase([1,1],[Tsimul(i),Psimul(i)],LIMS,BinSet,WorkVariXMap,BinPhaseDef);
        end
    end
    
    UpdateLIVEWaitBar(2,1,handles);
    
    SetOk = find(TheRes < ResError);
    PTfin = [Tsimul(SetOk)',Psimul(SetOk)'];
    
    PTcoord1 = ScatterContouringFct(PTfin);
    
    plot(PTcoord1(:,1),PTcoord1(:,2),'-b');
    
    
    % OPTIMIZATION 2:
    disp('  --> Optimization [2/3] ')
    
    set(handles.Live_Disp2_Text2,'Visible','On','String','Monte-Carlo simulation (2/3)');
    drawnow
    
    dT = (max(PTcoord1(:,1))- min(PTcoord1(:,1)))/3;
    dP = (max(PTcoord1(:,2))- min(PTcoord1(:,2)))/3;
    
    disp(['      ... ',num2str(NbPermRef),' * ',num2str(numel(PTcoord1)),' = ',num2str(NbPermRef*numel(PTcoord1)),' minimizations']);
    
    ComptInd = 1;
    for i=1:size(PTcoord1,1)
        
        Tinv2 = PTcoord1(i,1) + randn(NbPermRef,1)*dT;
        Pinv2 = PTcoord1(i,2) + randn(NbPermRef,1)*dP;
        
        % Check for permuation outside the P-T range
        TPinside2 = find(Tinv2 > LIMS(1) & Tinv2 < LIMS(2) & Pinv2 > LIMS(3) & Pinv2 < LIMS(4));
        
        Tsimul2(ComptInd:ComptInd+length(TPinside2)-1) = Tinv2(TPinside2);
        Psimul2(ComptInd:ComptInd+length(TPinside2)-1) = Pinv2(TPinside2);
        
        ComptInd = ComptInd+length(TPinside2)-1;
    end
    
    axes(handles.axes_Live_PLOT5), hold on
    plot(Tsimul2,Psimul2,'.g')
    
    TheRes = zeros(size(Tsimul2));
    
    UpdateLIVEWaitBar(2,0,handles);
    Compt = 0;
    for i=1:length(Tsimul2)
        Compt = Compt+1;
        if Compt > 5
            Compt = 0;
            UpdateLIVEWaitBar(2,i/length(Tsimul2),handles);
        end
        if isequal(AntidoteMode,4)
            [TheRes2(i),Evaluation] = OptiBingoPT([1,1],[Tsimul2(i),Psimul2(i)],LIMS,BinSet,WorkVariXMap,MinimOptions,handles);
        else
            [TheRes2(i)] = OptiBingoPTSinglePhase([1,1],[Tsimul2(i),Psimul2(i)],LIMS,BinSet,WorkVariXMap,BinPhaseDef);
        end
    end
    
    UpdateLIVEWaitBar(2,1,handles);
    
    SetOk = find(TheRes2 < ResError);
    PTfin2 = [Tsimul2(SetOk)',Psimul2(SetOk)'];
    
    PTcoord2 = ScatterContouringFct([PTfin;PTfin2]);
    
    axes(handles.axes_Live_PLOT5), hold on
    plot(PTcoord2(:,1),PTcoord2(:,2),'-g');
    
    
    % OPTIMIZATION 3:
    disp('  --> Optimization [3/3] ')
    
    dT = (max(PTcoord2(:,1))- min(PTcoord2(:,1)))/6;
    dP = (max(PTcoord2(:,2))- min(PTcoord2(:,2)))/6;
    
    disp(['      ... ',num2str(NbPermRef),' * ',num2str(numel(PTcoord2)),' = ',num2str(NbPermRef*numel(PTcoord2)),' minimizations']);
    disp(' ')
    
    set(handles.Live_Disp2_Text2,'Visible','On','String','Monte-Carlo simulation (3/3)');
    drawnow
    
    ComptInd = 1;
    for i=1:size(PTcoord2,1)
        
        Tinv3 = PTcoord2(i,1) + randn(NbPermRef,1)*dT;
        Pinv3 = PTcoord2(i,2) + randn(NbPermRef,1)*dP;
        
        % Check for permuation outside the P-T range
        TPinside3 = find(Tinv3 > LIMS(1) & Tinv3 < LIMS(2) & Pinv3 > LIMS(3) & Pinv3 < LIMS(4));
        
        Tsimul3(ComptInd:ComptInd+length(TPinside3)-1) = Tinv3(TPinside3);
        Psimul3(ComptInd:ComptInd+length(TPinside3)-1) = Pinv3(TPinside3);
        
        ComptInd = ComptInd+length(TPinside3)-1;
    end
    
    axes(handles.axes_Live_PLOT5), hold on
    plot(Tsimul3,Psimul3,'.r')
    
    TheRes = zeros(size(Tsimul3));
    
    UpdateLIVEWaitBar(2,0,handles);
    Compt = 0;
    for i=1:length(Tsimul3)
        Compt = Compt+1;
        if Compt > 5
            Compt = 0;
            UpdateLIVEWaitBar(2,i/length(Tsimul3),handles);
        end
        if isequal(AntidoteMode,4)
            [TheRes3(i),Evaluation] = OptiBingoPT([1,1],[Tsimul3(i),Psimul3(i)],LIMS,BinSet,WorkVariXMap,MinimOptions,handles);
        else
            [TheRes3(i)] = OptiBingoPTSinglePhase([1,1],[Tsimul3(i),Psimul3(i)],LIMS,BinSet,WorkVariXMap,BinPhaseDef);
        end
    end
    
    UpdateLIVEWaitBar(2,1,handles);
    
    SetOk = find(TheRes3 < ResError);
    PTfin3 = [Tsimul3(SetOk)',Psimul3(SetOk)'];
    
    PTcoord3 = ScatterContouringFct([PTfin;PTfin2;PTfin3]);
    
    axes(handles.axes_Live_PLOT5), hold on
    plot(PTcoord3(:,1),PTcoord3(:,2),'-r');
    
    Results(iUnc).PTcoord = PTcoord3;
    drawnow
    
end

set(handles.Live_Disp2_Text3,'Visible','On','String','Done','ForegroundColor',[0,0,0]);
drawnow

figure, cla, hold on
for i=1:length(ErrorsT)
    plot([ErrorsT(i),Tinit],[ErrorsP(i),Pinit],'-k');
end
plot(PTcoord3(:,1),PTcoord3(:,2),'-k');
plot(Tinit,Pinit,'o','MarkerFaceColor','r','MarkerEdgeColor','k');
axis(LIMS);
xlabel('Temperature (C)'), ylabel('Pressure (Kbar)');
title(['P-T uncertainty (',UncStr,' %)']);

%         if length(Results) > 1
%             figure, hold on
%             for i=1:length(Results)
%                 plot(Results(i).PTcoord(:,1),Results(i).PTcoord(:,2),'-k');
%             end
%         end

ht1=toc;
disp(' ')
fprintf('%s\t%.4f\n','CPU time',ht1);



Output.WeCallBingo = 0;
Output.WeSaveWorkspace = 0;
Output.Message = 'Success';

Antidote_VARIABLES = [];


return



% #########################################################################
%       ANT - Auto-refinement from GRTMOD
function [NewCoordinates] = ComptDirections2Coordinates(DirectionsToTest,dT,dP,Tinit,Pinit)
NewCoordinates(1,:) = DirectionsToTest(1) * [Tinit,Pinit+dP];
NewCoordinates(2,:) = DirectionsToTest(2) * [Tinit+dT,Pinit+dP];
NewCoordinates(3,:) = DirectionsToTest(3) * [Tinit+dT,Pinit];
NewCoordinates(4,:) = DirectionsToTest(4) * [Tinit+dT,Pinit-dP];
NewCoordinates(5,:) = DirectionsToTest(5) * [Tinit,Pinit-dP];
NewCoordinates(6,:) = DirectionsToTest(6) * [Tinit-dT,Pinit-dP];
NewCoordinates(7,:) = DirectionsToTest(7) * [Tinit-dT,Pinit];
NewCoordinates(8,:) = DirectionsToTest(8) * [Tinit-dT,Pinit+dP];
return


% #########################################################################
%       ANT - Auto-refinement from GRTMOD
function [FinalCoordinates] = FinalDirections2Coordinates(PositiveTests,DeltaT,DeltaP,Tinit,Pinit)
FinalCoordinates(1,:) = [Tinit,Pinit+PositiveTests(1)*DeltaP];
FinalCoordinates(2,:) = [Tinit+PositiveTests(2)*DeltaT,Pinit+PositiveTests(2)*DeltaP];
FinalCoordinates(3,:) = [Tinit+PositiveTests(3)*DeltaT,Pinit];
FinalCoordinates(4,:) = [Tinit+PositiveTests(4)*DeltaT,Pinit-PositiveTests(4)*DeltaP];
FinalCoordinates(5,:) = [Tinit,Pinit-PositiveTests(5)*DeltaP];
FinalCoordinates(6,:) = [Tinit-PositiveTests(6)*DeltaT,Pinit-PositiveTests(6)*DeltaP];
FinalCoordinates(7,:) = [Tinit-PositiveTests(7)*DeltaT,Pinit];
FinalCoordinates(8,:) = [Tinit-PositiveTests(8)*DeltaT,Pinit+PositiveTests(8)*DeltaP];
return


function [PTcoord] = ScatterContouringFct(PTfin)
%
%


WeDisp = 0;

if WeDisp
    plot(PTfin(:,1),PTfin(:,2),'.r'), hold on
end


% Starting point (lower-Temperature)
[ValTStart,start] = min(PTfin(:,1));
[ValTBreak1,break1] = max(PTfin(:,1));

PTi = PTfin(start,:);
PTcoord = PTi;

Compt = 1;
while ~isequal(PTi(1),ValTBreak1)
    Compt = Compt+1;
    
    WhereIn = find(PTfin(:,1) > PTi(1));
    
    % Calculate the slopes
    Slopes = (PTfin(WhereIn,2)-PTi(2))./(PTfin(WhereIn,1)-PTi(1));
    
    % Calculate the distances
    Distances = sqrt((PTfin(WhereIn,1)-PTi(1)).^2 + (PTfin(WhereIn,2)-PTi(2)).^2);
    
    [valS,SlopeInd] = max(Slopes);
    
%     [SortDist,SortInd] = sort(Distances);
%     TheX = PTfin(SortInd,1)-PTfin(SlopeInd,1);
% 
%     PossibleMatches = find(TheX == 0)-1;
    
    TheNewInd = WhereIn(SlopeInd);
%     for i=1:PossibleMatches
%         if  TheX(i) < 0
%             TheNewInd = SortInd(i);
%         end
%     end
      
    newPTi = PTfin(TheNewInd,:);
     
    if WeDisp
        plot([PTi(1),newPTi(1)],[PTi(2),newPTi(2)],'-');
        drawnow
    end
    
    PTi = newPTi;
    PTcoord(Compt,:) = PTi; 
end

while ~isequal(PTi(1),ValTStart)
    Compt = Compt+1;
    
    WhereIn = find(PTfin(:,1) < PTi(1));
    
    % Calculate the slopes
    Slopes = (PTi(2)-PTfin(WhereIn,2))./(PTi(1)-PTfin(WhereIn,1));
   
    [valS,SlopeInd] = max(Slopes);
 
    TheNewInd = WhereIn(SlopeInd);
      
    newPTi = PTfin(TheNewInd,:);
     
    if WeDisp
        plot([PTi(1),newPTi(1)],[PTi(2),newPTi(2)],'-');
        drawnow
    end
    
    PTi = newPTi;
    PTcoord(Compt,:) = PTi; 
end
 
    
return

