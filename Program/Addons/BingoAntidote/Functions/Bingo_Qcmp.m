function [Evaluation,Report] = Bingo_Qcmp(WorkVariMod,WorkVariXMap,Link,Evaluation,Report,DoWePrint,UpdateText2Disp,app)
% ComputeQualityCompositions is a function to estimate the Evaluation of the  
% model for the composition of stable phases.
%
% To be described later once the strategy is set.
% 
% Last change by Pierre Lanari (21.06.2019)
%

Evaluation.Compositions = 0;

NbElemsTher = WorkVariMod.NbEl;   
ElemsListTher = WorkVariMod.Els;

NbElemsXMap = WorkVariXMap.NbEl;
ElemsListXMap = WorkVariXMap.Els;

NbElems = NbElemsTher;
ElemsList = ElemsListTher;

[Ok,WhereIsMem] = ismember(ElemsListTher,ElemsListXMap);

WorkVariXMap.COMPok = zeros(WorkVariXMap.NbPhases,NbElemsTher);
WorkVariXMap.COMPok(:,find(Ok)) = WorkVariXMap.COMP(:,WhereIsMem(find(Ok)));

WorkVariXMap.UNCok = zeros(WorkVariXMap.NbPhases,NbElemsTher);
WorkVariXMap.UNCok(:,find(Ok)) = WorkVariXMap.UNC(:,WhereIsMem(find(Ok)));

if UpdateText2Disp
    app.Report_Bingo{end+1} = '##### Evaluation criterion (3) PHASE COMPOSITIONS ##### ';
    app.Report_Bingo{end+1} = ' ';
end

if DoWePrint
    disp(' ')
    Code1 = '%s\t\t';
    Code2 = '%s\t\t';
    for i=1:NbElems-1    
        Code1=[Code1,'%s\t\t'];
        Code2=[Code2,'%f\t'];
    end
    Code1(end) = 'n';
    Code2(end) = 'n';
    
    disp(' ')
    fprintf('%s\n','##### Evaluation criterion (3) PHASE COMPOSITIONS ##### ');    
end


% Correction of O for H in the structural formula:
WhereH = find(ismember(ElemsList,'H'));
if WhereH
    TheHValues = WorkVariMod.COMP(:,WhereH);
    WorkVariMod.COMP(1:WorkVariMod.NbPhases,WhereH) = zeros(WorkVariMod.NbPhases,1);
else
    TheHValues = 0;
end

WhereO = find(ismember(ElemsList,'O'));
if WhereO
    WorkVariMod.COMP(1:WorkVariMod.NbPhases,WhereO) = WorkVariMod.COMP(1:WorkVariMod.NbPhases,WhereO)-0.5*TheHValues;
end

CompTherMatch = zeros(size(Link.PhasesNames,2),NbElems);
CompXMapMatch = zeros(size(Link.PhasesNames,2),NbElems);
CompXMapUnc1s = zeros(size(Link.PhasesNames,2),NbElems);

if size(WorkVariMod.COMP,1) < 2
    %keyboard
end 

CompTherMatch(find(Link.TherIsIn),:) = WorkVariMod.COMP(Link.TherIndices(find(Link.TherIndices)),:);
CompXMapMatch(find(Link.XMapIsIn),:) = WorkVariXMap.COMPok(Link.XMapIndices(find(Link.XMapIndices)),:);

CompXMapUnc1s(find(Link.XMapIsIn),:) = WorkVariXMap.UNCok(Link.XMapIndices(find(Link.XMapIndices)),:);

% Check if O(anhydrous) is the same in both      (PL 05.03.19)
DoWeHaveZeroElem = 0;
for i = 1:size(CompTherMatch,1)
    if CompTherMatch(i,WhereO) > 0 && CompXMapMatch(i,WhereO) && ~isequal(CompTherMatch(i,WhereO),CompXMapMatch(i,WhereO))
        CorrFactor = CompTherMatch(i,WhereO)/CompXMapMatch(i,WhereO);
        if DoWePrint
            fprintf('\n%s\t\t%s\n','*Warning*',['SF renormalized for phase: ',char(Link.PhasesNames{i}),' (O was XMap[',num2str(CompXMapMatch(i,WhereO)),'] ;Model[',num2str(CompTherMatch(i,WhereO)),'])']);
        end
        CompXMapMatch(i,:) = CompXMapMatch(i,:)*CorrFactor;
        DoWeHaveZeroElem = DoWeHaveZeroElem+1;
    end
end
if DoWeHaveZeroElem > 0 && DoWePrint
    fprintf('\n');
end



% 3.2 WEIGHTED Cmp with UNC               (28 May 2018) ** Version 8 **  PL


% Perspective Version for Lanari & Duesterhoeft (2019)
%
%   - Meth = 1          Erik2 using only UNC (no TOL) and an unweighted
%                       mean method
%                       I added an option not to have Unc1s < 0.01 apfu
%
%
% ## Same potential issue as before: ##
% Ok, the way this part is coded is not optimal...
% The variable BinPhaseDef is not directly sent to this function, it is
% taken from the handles. Of course it works fine, but if handles doesn't
% come for any reason or future update, it will not work anymore.
%
% PL (28.05.2018) -
%
%
% ## ##
% I udpated this function to extract the compositions from
% CompXMapMatch and CompTherMatch (see just above)
% It is not so clear why we haven't done that since the begining..
%
% PL (05.03.2019)


BinPhaseDef = app.BinPhaseDef;
for i=1:length(BinPhaseDef)
    NameMinBinDef{i} = BinPhaseDef(i).DBMinName;
end

for i=1:length(Link.PhasesNames)
    NameRaw = Link.PhasesNames{i};
    NameRawStr = strread(NameRaw,'%s');
    if iscell(NameRawStr)
        NamePhasesMatch{i} = NameRawStr{1};
    else
        NamePhasesMatch{i} = NameRaw;
    end
end

Qual = zeros(size(CompTherMatch,1),1);
Missfit = zeros(size(CompTherMatch,1),1);
weight = zeros(size(CompTherMatch,1),1);

%ElemsList = WorkVariMod.Els;

IsCompared = zeros(size(CompTherMatch,1),1);

Compt4Save = 0;
for i=1:size(CompTherMatch,1)
    
    NamePhasesModel = NamePhasesMatch{i};
    
    NbAtXMap = sum(CompXMapMatch(i,2:end-1)); % I excluded oxygen (2:end) and E (end)
    NbAtTher = sum(CompTherMatch(i,2:end-1)); % and E (end)
    
    if NbAtXMap > 0 && NbAtTher > 0
        
        IsCompared(i) = 1;
        
        % Extract the element list (to be compared) from BinPhaseDef
        WhereInBinDef = find(ismember(NameMinBinDef,NamePhasesModel));
        
        ElsSel = BinPhaseDef(WhereInBinDef).ListVariAntidote;
        %WeightSel = BinPhaseDef(WhereInBinDef).ListWeightAntidote;
        
        [Yes,WhereElemInList] = ismember(ElsSel,ElemsList);
        %[Yes1,WhereElemInMod] = ismember(ElsSel,WorkVariMod.Els);
        %[Yes2,WhereElemInXMap] = ismember(ElsSel,WorkVariXMap.Els);
        
        % PL 03.03.19
        % Special case if the element is not defined in the bulk (zero for map and model)
        % Otherwise the average is bad
        KeepElement = find(WhereElemInList ~= 0);
        SkipElement = find(WhereElemInList == 0);
        %KeepElement = find(WhereElemInMod ~= 0 | WhereElemInXMap ~= 0);
        %SkipElement = find(WhereElemInMod == 0 & WhereElemInXMap == 0);
        
        CompoMod = zeros(size(WhereElemInList));
        CompoXMap = zeros(size(WhereElemInList));
        Unc1s = zeros(size(WhereElemInList));
        
        CompoMod(find(WhereElemInList)) = CompTherMatch(i,WhereElemInList(find(WhereElemInList)));
        CompoXMap(find(WhereElemInList)) = CompXMapMatch(i,WhereElemInList(find(WhereElemInList)));
        Unc1s(find(WhereElemInList)) = CompXMapUnc1s(i,WhereElemInList(find(WhereElemInList)));
        
        %CompoMod(find(WhereElemInMod)) = WorkVariMod.COMP(Link.TherIndices(i),WhereElemInMod(find(WhereElemInMod)));
        %CompoXMap(find(WhereElemInXMap)) = WorkVariXMap.COMP(Link.XMapIndices(i),WhereElemInXMap(find(WhereElemInXMap)));
        %Unc1s(find(WhereElemInXMap)) = WorkVariXMap.UNC(Link.XMapIndices(i),WhereElemInXMap(find(WhereElemInXMap)));
        
        %if ~isequal(length(CompoMod),length(CompoXMap)) % ,length(WeightSel))  PL ** 03.03.19
        %    disp('Oups something went wrong in ComputeQualityCompositions')
        %    keyboard
        %end
        
        
        % Eqation (3) in Duesterhoeft & Lanari (in prep)
        DIFFabs = abs(CompoMod-CompoXMap);  
        
        % Parameters for calculating Qcmp:
        Fac1 = 1;                    % plateau at 1 sigma for Qcmp of 100 %
        Fac2 = 5;               % NOTE: the zero is at Fac1+Fac2 (6 sigma)!
        
        % Check for Unc < 0.01 and replace by 0.01                     (PL)
        WhereTOOLOW = find(Unc1s > 0 & Unc1s <0.01);
        if length(WhereTOOLOW)
            Unc1s(WhereTOOLOW) = 0.01*ones(size(Unc1s(WhereTOOLOW)));
        end
        
        DIFF2 = DIFFabs-(Unc1s/Fac1);
        
        WhereNEGZ = find(DIFF2<=0);
        if length(WhereNEGZ)
            DIFF2(WhereNEGZ) = zeros(size(WhereNEGZ));
        end
        WherePOSZ = find(DIFF2>Fac2*Unc1s);
        if length(WherePOSZ)
            DIFF2(WherePOSZ) = Fac2*Unc1s(WherePOSZ).*ones(size(WherePOSZ));
        end
        
        
        % -------------------------------------------------------------------------
        % Qcmp_ij - Eqation (4) in Duesterhoeft & Lanari (in prep)
        QUALsmall = (1-DIFF2./(Fac2*Unc1s)).^(CompoMod+1);
        
        % -------------------------------------------------------------------------

        % Note: This equation was updated by PL (21.06.2019) to fit the MS
        % the old version was: %QUALsmall = 1*((Fac2*Unc1s-DIFF2)./(Fac2*Unc1s)).^(CompoMod+1);
        
        
        WhereNAN = find(isnan(QUALsmall));
        if WhereNAN
            % zero concentration in the map (must be zero
            % quality)
            QUALsmall(WhereNAN) = zeros(size(WhereNAN));
        end
        
        
        
        % -------------------------------------------------------------------------
        % Weighting - see Eqation (5) in Duesterhoeft & Lanari (in prep)     
        Qual(i) = mean(QUALsmall(KeepElement)).*100;
        % -------------------------------------------------------------------------
        
        
        % Display the element skipped! Addition by PL on 03.03.19:
        ElsSel2 = ElsSel;
        if length(SkipElement)
            for j=1:length(SkipElement)
                ElsSel2{SkipElement(j)} = [char(ElsSel{SkipElement(j)}),'*'];
            end
        end
        
        % Added by PL on 03.03.20
        Compt4Save = Compt4Save+1;
        Evaluation.MinComp_TEMP(Compt4Save).MinName = Link.PhasesNames{i};
        Evaluation.MinComp_TEMP(Compt4Save).MinEl = ElsSel2;
        Evaluation.MinComp_TEMP(Compt4Save).MinCompMod = CompoMod;
        Evaluation.MinComp_TEMP(Compt4Save).MinCompXMap = CompoXMap;
        Evaluation.MinComp_TEMP(Compt4Save).MinUnc1s = Unc1s;
        
        
        if UpdateText2Disp
            app.Report_Bingo{end+1} = ['> ',char(Link.PhasesNames{i})];
            
            CompObs = 'OBS: ';
            CompMod = 'MOD: ';
            for j = 1:length(ElemsList)
                if CompXMapMatch(i,j) > 0
                    Val = num2str(round(CompXMapMatch(i,j),2));
                    if length(Val) == 1
                        AddZeros = '.00';
                    elseif length(Val) == 2
                        AddZeros = '.0';
                    elseif length(Val) == 3
                        AddZeros = '.';
                    else
                        AddZeros = '';
                    end
                    CompObs = [CompObs,ElemsList{j},' = ',Val,AddZeros,' | '];
                else
                    CompObs = [CompObs,ElemsList{j},' = ','0.00',' | '];
                end
                if CompTherMatch(i,j) > 0
                    Val = num2str(round(CompTherMatch(i,j),2));
                    if length(Val) == 1
                        AddZeros = '.00';
                    elseif length(Val) == 2
                        AddZeros = '.0';
                    elseif length(Val) == 3
                        AddZeros = '.';
                    else
                        AddZeros = '';
                    end
                    CompMod = [CompMod,ElemsList{j},' = ',Val,AddZeros,' | '];
                else
                    CompMod = [CompMod,ElemsList{j},' = ','0.00',' | '];
                end
            end
            
            app.Report_Bingo{end+1} = CompObs;
            app.Report_Bingo{end+1} = CompMod;
            app.Report_Bingo{end+1} = ' ';
            
            CompObs = 'OBS: ';
            CompMod = 'MOD: ';
            CompSig = 'UNC: ';
            CompQUA = 'QUA: ';
            for j = 1:length(ElsSel2)
                if CompoXMap(j) > 0
                    Val = num2str(round(CompoXMap(j),2));
                    if length(Val) == 1
                        AddZeros = '.00';
                    elseif length(Val) == 2
                        AddZeros = '.0';
                    elseif length(Val) == 3
                        AddZeros = '.';
                    else
                        AddZeros = '';
                    end
                    CompObs = [CompObs,ElsSel2{j},' = ',Val,AddZeros,' | '];
                else
                    CompObs = [CompObs,ElsSel2{j},' = ','0.00',' | '];
                end
                if CompoMod(j) > 0
                    Val = num2str(round(CompoMod(j),2));
                    if length(Val) == 1
                        AddZeros = '.00';
                    elseif length(Val) == 2
                        AddZeros = '.0';
                    elseif length(Val) == 3
                        AddZeros = '.';
                    else
                        AddZeros = '';
                    end
                    CompMod = [CompMod,ElsSel2{j},' = ',Val,AddZeros,' | '];
                else
                    CompMod = [CompMod,ElsSel2{j},' = ','0.00',' | '];
                end
                if Unc1s(j) > 0
                    Val = num2str(round(Unc1s(j),2));
                    if length(Val) == 1
                        AddZeros = '.00';
                    elseif length(Val) == 2
                        AddZeros = '.0';
                    elseif length(Val) == 3
                        AddZeros = '.';
                    else
                        AddZeros = '';
                    end
                    CompSig = [CompSig,ElsSel2{j},' = ',Val,AddZeros,' | '];
                else
                    CompSig = [CompSig,ElsSel2{j},' = ','0.00',' | '];
                end
                if QUALsmall(j) > 0
                    Val = num2str(round(QUALsmall(j),2));
                    if length(Val) == 1
                        AddZeros = '.00';
                    elseif length(Val) == 2
                        AddZeros = '.0';
                    elseif length(Val) == 3
                        AddZeros = '.';
                    else
                        AddZeros = '';
                    end
                    CompQUA = [CompQUA,ElsSel2{j},' = ',Val,AddZeros,' | '];
                else
                    CompQUA = [CompQUA,ElsSel2{j},' = ','0.00',' | '];
                end
            end
            
            app.Report_Bingo{end+1} = CompObs;
            app.Report_Bingo{end+1} = CompSig;
            app.Report_Bingo{end+1} = CompMod;
            app.Report_Bingo{end+1} = CompQUA;
            app.Report_Bingo{end+1} = ' ';

            app.Report_Bingo{end+1} = ['Qcmp = ',num2str(Qual(i)),' %'];
            app.Report_Bingo{end+1} = ' ';

        end
        
        if DoWePrint
            fprintf('%s\n%s\n','-',char(Link.PhasesNames{i}));
            fprintf(Code1,'Els:   ',ElemsList{1:end-1});
            fprintf(Code2,'THER:  ',CompTherMatch(i,1:end-1));
            fprintf(Code2,'XMAP:  ',CompXMapMatch(i,1:end-1));
            fprintf(Code2,'abs(D):',abs(CompTherMatch(i,1:end-1)-CompXMapMatch(i,1:end-1)));
            
            disp(' ')
            
            CodeT1 = '%s\t\t';
            CodeT2 = '%s\t\t';
            for k=1:length(ElsSel2)
                CodeT1=[CodeT1,'%s\t\t'];
                CodeT2=[CodeT2,'%f\t'];
            end
            CodeT1(end) = 'n';
            CodeT2(end) = 'n';
            
            fprintf(CodeT1,'Els:   ',ElsSel2{1:end})
            %fprintf(CodeT2,'InTHER:',WhereElemInMod);         % TEMP
            %fprintf(CodeT2,'InXMAP:',WhereElemInXMap);        % TEMP
            fprintf(CodeT2,'THER:  ',CompoMod);
            fprintf(CodeT2,'XMAP:  ',CompoXMap);
            disp('_______')
            fprintf(CodeT2,'UNC:   ',Unc1s);
            fprintf(CodeT2,'DIFFab:',DIFFabs);
            fprintf(CodeT2,'DIFF2: ',DIFF2);
            disp('_______')
            fprintf(CodeT2,'QUALs: ',QUALsmall);
            %fprintf(CodeT2,'X(El): ',RelFract);
            
            disp(' ')
            disp(['Qcmp = ',num2str(Qual(i)),' %'])
            
            disp(' ')
        end
        
    else
        
        if UpdateText2Disp

            app.Report_Bingo{end+1} = ['> ',char(Link.PhasesNames{i})];
            app.Report_Bingo{end+1} = ' - - - Phase skipped by BINGO - - - ';
            app.Report_Bingo{end+1} = ' ';
        end
        
        if DoWePrint
            disp([' - - - Phase: ',char(NamePhasesModel),' has been skipped by BINGO - - - '])
            disp(' ')
        end
        
    end
end

%E_Minim2 = sum(Missfit); % not used it seems

VolFracInModel = zeros(size(Qual));
WherePhasesInModel = find(ismember(NamePhasesMatch,WorkVariMod.Names));

VolFracInModel(WherePhasesInModel) = WorkVariMod.VolFrac(WherePhasesInModel);

VolFracInModelCorr = zeros(size(VolFracInModel));
WeTake = find(IsCompared);
VolFracInModelCorr(WeTake) = VolFracInModel(WeTake)/sum(VolFracInModel(WeTake));

if sum(Qual)  > 0    

    % -------------------------------------------------------------------------
    % Qcmp - Eqation (5b) in Duesterhoeft & Lanari (in prep)
    Evaluation.Compositions = sum(Qual.*VolFracInModelCorr); 
    
    % -------------------------------------------------------------------------
    
    Evaluation.MinQ_TEMP = Qual;
    Evaluation.MinQw_TEMP = Qual.*VolFracInModelCorr;
    Evaluation.MinIsCompared_TEMP = IsCompared; 
    Evaluation.MinNames_TEMP = NamePhasesMatch;
        
else
    Evaluation.Compositions = 0;
    
    Evaluation.MinQ_TEMP = 0;
    Evaluation.MinQw_TEMP = 0;
    Evaluation.MinIsCompared_TEMP = 0;
    Evaluation.MinNames_TEMP = '';
    
    % Added by PL on 03.03.20
    Evaluation.MinEl_TEMP = '';
    Evaluation.MinCompMod_TEMP = 0;
    Evaluation.MinCompXMap_TEMP = 0;
    Evaluation.MinUnc1s_TEMP = 0;
end

if UpdateText2Disp
    app.Text_Report.Value = app.Report_Bingo;
end

if DoWePrint
    fprintf('\n%s\n%s\t\t','-------------','Phase');
    
    for i = 1:length(Evaluation.MinNames_TEMP)
        NameMin = Evaluation.MinNames_TEMP{i};
        if length(NameMin) > 6
            fprintf('%s\t',NameMin(1:6));
        else
            fprintf('%s\t',NameMin);
        end
    end
    fprintf('\n%s\t','Evaluated');
    for i = 1:length(Evaluation.MinNames_TEMP)
        if IsCompared(i)
            fprintf('%s\t','Yes');
        else
            fprintf('%s\t','No');
        end
    end
    fprintf('\n%s\t','Qual (%)');
    for i = 1:length(Evaluation.MinNames_TEMP)
        fprintf('%.2f\t',Qual(i));
    end
    fprintf('\n%s\t\t','v_norm');
    for i = 1:length(Evaluation.MinNames_TEMP)
        fprintf('%.2f\t',VolFracInModelCorr(i));
    end
    fprintf('\n%s\t\t','v');
    for i = 1:length(Evaluation.MinNames_TEMP)
        fprintf('%.2f\t',VolFracInModel(i));
    end
    fprintf('\n\n');
    
    % This is independent of the one selected below
    fprintf('%s\t%.2f\n','Qcmp_vn =',sum(Qual.*VolFracInModelCorr)); 
    fprintf('%s\t%.2f\n\n','Qcmp_v  =',sum(Qual.*VolFracInModel)); 
    
    fprintf('%s\t%.2f\n','Qcmp =',Evaluation.Compositions);  
    fprintf('%s\n','-------------');
    
    Report.ElemsList = ElemsList;
    Report.CmpXMap = CompXMapMatch;
    Report.CmpTher = CompTherMatch;
end



return


