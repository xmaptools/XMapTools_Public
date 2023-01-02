function [Output,Antidote_VARIABLES] = Antidote_3a6_PTunc(Mode,WorkVariXMap,MinimOptions,app)
%
%
%

% warning NOT CHECKED PL March 2019 ---

Tmin = app.TminEditField.Value;
Tmax = app.TmaxEditField.Value;
Pmin = app.PminEditField.Value;
Pmax = app.PmaxEditField.Value;

Tstep = app.TstepEditField.Value;
Pstep = app.PstepEditField.Value;

Ti = [Tmin:Tstep:Tmax];
Pi = [Pmin:Pstep:Pmax];

UncInt = app.AntidoteToleranceEditField.Value;

NbPerm = app.AntidoteNbPerm1EditField.Value;
NbPermRef = app.AntidoteNbPerm2EditField.Value;

LIMS = [Ti(1),Ti(end),Pi(1),Pi(end)];

[BinSet] = SetBin(app);

switch Mode
    case {'cmp','Cmp'}
        BinPhaseDef = app.BinPhaseDef(app.SelectedPhaseOptiDropDown.Value);
end

Tinit = app.BingoTemperatureEditField.Value;
Pinit = app.BingoPressureEditField.Value;

StepsT = length(Ti);
StepsP = length(Pi);

DeltaT = round((Ti(end)-Ti(1))/max([StepsT,StepsP]));
DeltaP = (Pi(end)-Pi(1))/max([StepsT,StepsP]);

dT = DeltaT;
dP = DeltaP;

cla(app.UIAxes2,'reset');
plot(app.UIAxes2,Tinit,Pinit,'ok','MarkerFaceColor','k')
axis(app.UIAxes2,LIMS);
hold(app.UIAxes2,'on')
xlabel(app.UIAxes2,'Temperature (°C)'), ylabel(app.UIAxes2,'Pressure (GPa)')
drawnow

app.TabGroup2.SelectedTab = app.ResultsTab;


% variables for single phase computations:
% BinPhaseDef = handles.BinPhaseDef(get(handles.BinPopUpVarPhaseList,'Value'));

app.Report_Antidote{end+1} = ['Antidote: Recipe [3] - P–T uncertainty'];
app.Report_Antidote{end+1} = '';
app.Report_Antidote{end+1} = ['Bulk: ',BinSet.Bulk2Display];
app.Report_Antidote{end+1} = ['Database: ',BinSet.Database];
app.Report_Antidote{end+1} = '';
app.Report_Antidote{end+1} = ['Temp. P: ',num2str(Tinit)];
app.Report_Antidote{end+1} = ['Temp. TC: ',num2str(Pinit)];
app.Report_Antidote{end+1} = '';
if MinimOptions.Weights.Use
    app.Report_Antidote{end+1} = ['Equation: Other',['[E4 = -(',num2str(MinimOptions.Weights.Values(MinimOptions.Weights.Selected,1)),'*E1 + ',num2str(MinimOptions.Weights.Values(MinimOptions.Weights.Selected,2)),'*E2 + ',num2str(MinimOptions.Weights.Values(MinimOptions.Weights.Selected,3)),'*E3)]']];
    app.Report_Antidote{end+1} = '';
else
    app.Report_Antidote{end+1} = ['Equation: Classic [E4 = -1/3*(E1 + (E1/100)*E2 + (E1/100)*(E2/100)*E3)]'];
    app.Report_Antidote{end+1} = '';
end
    
app.Text_Report_Antidote.Value = app.Report_Antidote;

% PART 1 - AUTO-REFINEMENT as in GRTMOD (Lanari et al., 2017), EJM.
app.WaitBar.Message = ['Auto-refinement stage (',num2str(Tinit),' °C & ',num2str(Pinit),' GPa)'];

tic
for iUnc=1:length(UncInt)
    if length(UncInt) > 1
        app.Report_Antidote{end+1} = [' -->  LOOP: ',num2str(iUnc),'/',num2str(length(UncInt)),' [Tol(%) = ',num2str(UncInt(iUnc)),']'];
        app.Report_Antidote{end+1} = '';
    else
        app.Report_Antidote{end+1} = ['Tol(%): ',num2str(UncInt(iUnc))];
        app.Report_Antidote{end+1} = '';
    end
    
    app.Report_Antidote{end+1} = ['##### Auto-refinement stage (',num2str(Tinit),' C & ',num2str(Pinit),' GPa) #####'];
    app.Report_Antidote{end+1} = '';
    
    app.Text_Report_Antidote.Value = app.Report_Antidote;
    
    switch Mode
        case {'all','All'}
            [ResSolution,Evaluation] = OptiBingoPT([1,1],[Tinit,Pinit],LIMS,BinSet,WorkVariXMap,MinimOptions,app);
        case {'cmp','Cmp'}
            [ResSolution] = OptiBingoPTSinglePhase([1,1],[Tinit,Pinit],LIMS,BinSet,WorkVariXMap,BinPhaseDef,app);
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
                
                switch Mode
                    case {'all','All'}
                        [TheNewRes,Evaluation] = OptiBingoPT([1,1],[NewCoordinates(i,1),NewCoordinates(i,2)],LIMS,BinSet,WorkVariXMap,MinimOptions,app);
                    case {'cmp','Cmp'}
                        [TheNewRes] = OptiBingoPTSinglePhase([1,1],[NewCoordinates(i,1),NewCoordinates(i,2)],LIMS,BinSet,WorkVariXMap,BinPhaseDef,app);
                end
                
                if TheNewRes > ResError 
                    DirectionsToTest(i) = 0;
                else
                    PositiveTests(i) = PositiveTests(i)+1;
                end
            end
            
        end
        
        TheExitTest = sum(DirectionsToTest);
        
        app.Report_Antidote{end+1} = ['  --> Loop [',num2str(Loops),'] - (',num2str(TheExitTest),')'];
        app.Text_Report_Antidote.Value = app.Report_Antidote;
        
        if ~TheExitTest
            disp(' ');
            break
        end
        
        % Update DirectionsToTest...
        dT = dT + DeltaT;
        dP = dP + DeltaP;
        
        FinalCoordinates = FinalDirections2Coordinates(PositiveTests,DeltaT,DeltaP,Tinit,Pinit);
        
        ErrorsT = FinalCoordinates(:,1);
        ErrorsP = FinalCoordinates(:,2);
        
    end
    
    for i=1:length(ErrorsT)
        plot(app.UIAxes2,[ErrorsT(i),Tinit],[ErrorsP(i),Pinit],'-k');
    end
    
    % PART 2 - MONTE-CARLO simulation to refine the shape of the uncertainty enveloppe
    
    NbPerm = app.AntidoteNbPerm1EditField.Value;
    NbPermRef = app.AntidoteNbPerm2EditField.Value;
    
    app.Report_Antidote{end+1} = '';
    app.Report_Antidote{end+1} = ['##### Uncertainty enveloppe stage (',num2str(Tinit),' C & ',num2str(Pinit),' GPa) #####'];
    app.Report_Antidote{end+1} = '';
    app.Text_Report_Antidote.Value = app.Report_Antidote;
    
    MaxTdiff = max(abs(Tinit-ErrorsT));
    if isequal(MaxTdiff,0)
        MaxTdiff = 25;
    end
    
    MaxPdiff = max(abs(Pinit-ErrorsP));
    if isequal(MaxPdiff,0)
        MaxPdiff = 1000;
    end
    
    % OPTIMIZATION 1:
    %app.WaitBar.Message = 'Optimization [1/3]';
    app.Report_Antidote{end+1} = ['  --> Optimization [1/3] '];
    app.Text_Report_Antidote.Value = app.Report_Antidote;
    
    Tinv = Tinit + randn(floor(NbPerm),1)*MaxTdiff/1.4;
    Pinv = Pinit + randn(floor(NbPerm),1)*MaxPdiff/1.4;
    
    % Add the points that are inside from the auto-refinement stage
    Tinv(end+1) = Tinit;
    Pinv(end+1) = Pinit;
    
    for i = 1:length(ErrorsT)
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
    
    plot(app.UIAxes2,Tsimul,Psimul,'.b');
    drawnow
    
    % Second simulation: 
    NbPermAugm = NbPerm*2;
    
    Tsimul(length(TPinside)+1:length(TPinside)+NbPermAugm) = LIMS(1) + (LIMS(2)-LIMS(1))*rand(NbPermAugm,1);
    Psimul(length(TPinside)+1:length(TPinside)+NbPermAugm) = LIMS(3) + (LIMS(4)-LIMS(3))*rand(NbPermAugm,1);
    
    app.Report_Antidote{end+1} = ['      ... ',num2str(NbPerm),' + ',num2str(NbPermAugm),' = ',num2str(numel(Tsimul)),' minimizations'];
    app.Report_Antidote{end+1} = '';
    app.Text_Report_Antidote.Value = app.Report_Antidote;
    
    pause(0.1)
    scroll(app.Text_Report_Antidote,'bottom');
    
    plot(app.UIAxes2,Tsimul(length(TPinside)+1:length(TPinside)+NbPermAugm),Psimul(length(TPinside)+1:length(TPinside)+NbPermAugm),'.k');
    drawnow
    
    TheRes = zeros(size(Tsimul));
    
    Compt = 0;
    for i=1:length(Tsimul)
        Compt = Compt+1;
        if Compt > 5
            Compt = 0;
            app.WaitBar.Message = ['Optimization [1/3] – ',num2str(round(i/length(Tsimul)*100)),'% done'];
        end
        switch Mode
            case {'all','All'}
                [TheRes(i),Evaluation] = OptiBingoPT([1,1],[Tsimul(i),Psimul(i)],LIMS,BinSet,WorkVariXMap,MinimOptions,app);
            case {'cmp','Cmp'}
                [TheRes(i)] = OptiBingoPTSinglePhase([1,1],[Tsimul(i),Psimul(i)],LIMS,BinSet,WorkVariXMap,BinPhaseDef,app);
        end
    end
    
    SetOk = find(TheRes < ResError);
    PTfin = [Tsimul(SetOk)',Psimul(SetOk)'];
    
    PTcoord1 = ScatterContouringFct(PTfin);
    
    plot(app.UIAxes2,PTcoord1(:,1),PTcoord1(:,2),'-b');
    drawnow
    
    
    % OPTIMIZATION 2:
    %app.WaitBar.Message = 'Optimization [2/3]';
    app.Report_Antidote{end+1} = ['  --> Optimization [2/3] '];
    app.Text_Report_Antidote.Value = app.Report_Antidote;
    
    dT = (max(PTcoord1(:,1))- min(PTcoord1(:,1)))/3;
    dP = (max(PTcoord1(:,2))- min(PTcoord1(:,2)))/3;
    
    app.Report_Antidote{end+1} = ['      ... ',num2str(NbPermRef),' * ',num2str(numel(PTcoord1)),' = ',num2str(NbPermRef*numel(PTcoord1)),' minimizations'];
    app.Report_Antidote{end+1} = '';
    app.Text_Report_Antidote.Value = app.Report_Antidote;
    
    pause(0.1)
    scroll(app.Text_Report_Antidote,'bottom');
    
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
    
    plot(app.UIAxes2,Tsimul2,Psimul2,'.g')
    drawnow
    
    TheRes = zeros(size(Tsimul2));
    
    Compt = 0;
    for i=1:length(Tsimul2)
        Compt = Compt+1;
        if Compt > 5
            Compt = 0;
            app.WaitBar.Message = ['Optimization [2/3]  – ',num2str(round(i/length(Tsimul2)*100)),'% done'];
        end
        switch Mode
            case {'all','All'}
                [TheRes2(i),Evaluation] = OptiBingoPT([1,1],[Tsimul2(i),Psimul2(i)],LIMS,BinSet,WorkVariXMap,MinimOptions,app);
            case {'cmp','Cmp'}
                [TheRes2(i)] = OptiBingoPTSinglePhase([1,1],[Tsimul2(i),Psimul2(i)],LIMS,BinSet,WorkVariXMap,BinPhaseDef,app);
        end
    end
    
    SetOk = find(TheRes2 < ResError);
    PTfin2 = [Tsimul2(SetOk)',Psimul2(SetOk)'];
    
    PTcoord2 = ScatterContouringFct([PTfin;PTfin2]);
    
    plot(app.UIAxes2,PTcoord2(:,1),PTcoord2(:,2),'-g');
    drawnow
    
    
    % OPTIMIZATION 3:
    %app.WaitBar.Message = 'Optimization [3/3]';
    app.Report_Antidote{end+1} = ['  --> Optimization [3/3] '];
    app.Text_Report_Antidote.Value = app.Report_Antidote;
    
    dT = (max(PTcoord2(:,1))- min(PTcoord2(:,1)))/6;
    dP = (max(PTcoord2(:,2))- min(PTcoord2(:,2)))/6;
    
    app.Report_Antidote{end+1} = ['      ... ',num2str(NbPermRef),' * ',num2str(numel(PTcoord2)),' = ',num2str(NbPermRef*numel(PTcoord2)),' minimizations'];
    app.Report_Antidote{end+1} = '';
    app.Text_Report_Antidote.Value = app.Report_Antidote;
    
    pause(0.1)
    scroll(app.Text_Report_Antidote,'bottom');
    
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
    
    plot(app.UIAxes2,Tsimul3,Psimul3,'.r')
    drawnow
    
    TheRes3 = zeros(size(Tsimul3));
    
    Compt = 0;
    for i=1:length(Tsimul3)
        Compt = Compt+1;
        if Compt > 5
            Compt = 0;
            app.WaitBar.Message = ['Optimization [3/3] – ',num2str(round(i/length(Tsimul3)*100)),'% done'];
        end
        switch Mode
            case {'all','All'}
                [TheRes3(i),Evaluation] = OptiBingoPT([1,1],[Tsimul3(i),Psimul3(i)],LIMS,BinSet,WorkVariXMap,MinimOptions,app);
            case {'cmp','Cmp'}
                [TheRes3(i)] = OptiBingoPTSinglePhase([1,1],[Tsimul3(i),Psimul3(i)],LIMS,BinSet,WorkVariXMap,BinPhaseDef,app);
        end
    end
    
    
    SetOk = find(TheRes3 < ResError);
    PTfin3 = [Tsimul3(SetOk)',Psimul3(SetOk)'];
    
    PTcoord3 = ScatterContouringFct([PTfin;PTfin2;PTfin3]);
    
    plot(app.UIAxes2,PTcoord3(:,1),PTcoord3(:,2),'-r');
    
    Results(iUnc).PTcoord = PTcoord3;
    drawnow
    
end


figure, cla, hold on
for i=1:length(ErrorsT)
    plot([ErrorsT(i),Tinit],[ErrorsP(i),Pinit],'-k');
end
plot(PTcoord3(:,1),PTcoord3(:,2),'-k');
plot(Tinit,Pinit,'o','MarkerFaceColor','r','MarkerEdgeColor','k');
axis(LIMS);
xlabel('Temperature (C)'), ylabel('Pressure (Kbar)');
title(['P-T uncertainty (',num2str(UncInt(iUnc)),' %)']);


ht1=toc;

app.Report_Antidote{end+1} = ['CPU time: ',num2str(ht1), 's'];
app.Text_Report_Antidote.Value = app.Report_Antidote;

pause(0.1)
scroll(app.Text_Report_Antidote,'bottom');

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

