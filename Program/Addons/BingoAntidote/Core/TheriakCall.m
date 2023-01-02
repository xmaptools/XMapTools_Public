function [WorkVariMod] = TheriakCall(BinSet,D_Temp,D_Press)
%
%

% Generate THERIN:
dlmwrite('THERIN',char( ['    ',char(D_Temp),'     ',char(D_Press)],BinSet.Bulk2Display),'delimiter','');

BinSet.Path = ['"',BinSet.Path,'"'];


[wum,yum]=system([BinSet.Path,'   XBIN   THERIN']);

    
[WorkVariMod] = Core_ReadResTheriak(yum,BinSet.ListRefMiner); 

return


function [WorkVariMod] = Core_ReadResTheriak(OutputTheriakd,ListRefMiner)
% 
% New version (21.03.19)
%
% -> This version reads the chemical potential from the Theriak's output
% and does not required to define the components in the database.
%

TestInput = strread(OutputTheriakd,'%s','delimiter','\n');

% To display Theriak output: 
%OutputTheriakd

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% (1) Elements and test for error with the database ...
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

%
WhereElOrder = find(ismember(TestInput,'elements in stable phases:'))+3;

if isempty(WhereElOrder)
    OutputTheriakd
    error('ERROR IN READING THE THERIAK OUTPUT FOR ELEMENTS (see theriak output above); the table containing the elements in stable phases is not available');
    return
end

El1 = strread(char(TestInput(WhereElOrder)),'%s')';
if length(El1) == 10
    SecondRow = strread(char(TestInput(WhereElOrder+1)),'%s')';
    if length(SecondRow) < length(El1) % Could be a second row...
        El1 = [El1,SecondRow];
    end
end

WorkVariMod(1).Els = El1;
WorkVariMod(1).NbEl = length(El1);

if WorkVariMod(1).NbEl > 10
    Shift = 1;
else
    Shift = 0;
end


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% (2a) Elements in stable phases
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

WhereCompositions = find(ismember(TestInput,'elements per formula unit:'))+1;

Compt = 1; Indice = 1;
while 1
    ShiftRows = (Compt-1)*Shift;
    Temp = strread(char(TestInput(WhereCompositions+Compt+ShiftRows)),'%s')';
    if isempty(Temp)
        break          % OK
    end
    if Shift
        Temp2 = strread(char(TestInput(WhereCompositions+Compt+ShiftRows+1)),'%s')';
    end
    
    NbElem1 = length(str2double(Temp(2:end)));
    ASS_Indice2(Indice) = Compt;
    ASS_Names2{Indice} = Temp{1};
    ASS_COMP2(Indice,1:NbElem1) = str2double(Temp(2:end));
    if Shift
        ASS_COMP2(Indice,NbElem1+1:NbElem1+length(Temp2)) = str2double(Temp2);
    end
    Indice = Indice+1;    
        
    Compt = Compt+1;
end


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% (2b) Volume and densities of solids
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

WhereVol = find(ismember(TestInput,'volumes and densities of stable phases:'))+4;

Compt = 1; Indice = 1; Skip = 0;
while 1
    Temp = strread(char(TestInput(WhereVol+Compt+Skip)),'%s')';
    
    if ~isequal(length(Temp),11)
        break
    end

    FractPer = str2double(Temp(5));

    if FractPer > 0.0001
        VOL_VolCCM(Compt) = str2double(Temp(4));
        VOL_VolFrac(Compt) = FractPer/100;
        VOL_Dens(Compt) = str2double(Temp(11)); 

        VOL_Names{Compt} = Temp{1};

        Compt = Compt+1;
    else
        Skip = Skip+1;
    end
end


% Check for Gases     *** New 1.4.1 (PL - 10.02.2018) 
WhereGf = WhereVol+Compt+Skip+4;

Temp = strread(char(TestInput(WhereGf)),'%s')';

Compt2 = 1;
WhereGf = WhereGf + 1;

if isequal(char(Temp{1}),'gases') && isequal(char(Temp{2}),'and') && isequal(char(Temp{3}),'fluids')    
    while 1
        Temp = strread(char(TestInput(WhereGf+Compt2)),'%s')';

        if ~isequal(length(Temp),7)
            break
        end
        
        VOLgf_VolCCM(Compt2) = str2double(Temp(4));
        VOLgf_VolFrac(Compt2) = 0;
        VOLgf_Dens(Compt2) = str2double(Temp(7));
        
        VOLgf_Names{Compt2} = Temp{1};
        
        Compt2 = Compt2+1;        
    end
    
else
    VOLgf_VolCCM = [];
    VOLgf_VolFrac = [];
    VOLgf_Dens = [];
    VOLgf_Names = '';
end

% Check if a liquid phase should be added to the solids
DoWeUdpateVol = 0;
for i=1:length(VOLgf_VolCCM)
    TempName = VOLgf_Names{i};
    TempNameStr = strread(TempName,'%s','delimiter','_');
    if iscell(TempNameStr)
        NameGf = TempNameStr{1};
    else
        NameGf = TempName;
    end
    Ok = find(ismember(ListRefMiner,NameGf));
    
    if Ok
        DoWeUdpateVol = 1;
        %Compt = Compt +1;
        VOL_VolCCM(Compt) = VOLgf_VolCCM(i);
        VOL_VolFrac(Compt) = VOLgf_VolFrac(i);
        VOL_Dens(Compt) = VOLgf_Dens(i); 
    
        VOL_Names{Compt} = TempName;
    end
end

if DoWeUdpateVol
    VOL_VolFrac = VOL_VolCCM/sum(VOL_VolCCM);
end


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% (3) SYNCHRONIZATION (SOLIDS + FLUIDS)
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

%keyboard

for i=1:length(VOL_Names)
    
    [OK,WhereC] = ismember(VOL_Names{i},ASS_Names2);
    
    if ~OK
        OutputTheriakd
        disp(' ')
        disp(' ')
        disp(['IT SEEMS THAT THE COMPOSITION OF PHASE ',char(VOL_Names{i}),' IS NOT AVAILABLE']) 
        disp('please send the theriak output above with error code (ERR2048) to pierre.lanari@geo.unibe.ch  ...')
        keyboard
    end
    
    WorkVariMod(1).Indice(i) = i;
   	WorkVariMod(1).Names{i} = VOL_Names{i};
    WorkVariMod(1).COMP(i,:) = ASS_COMP2(WhereC,:);
    WorkVariMod(1).VolFrac(i) = VOL_VolFrac(i);
    WorkVariMod(1).Dens(i) = VOL_Dens(i);
end

WorkVariMod(1).NbPhases = length(WorkVariMod(1).Names);

% FLUID PL - 07.03.2019
if Compt2 > 1 % then there is at least one fluid phase
    WorkVariMod(1).FluidPhases = Compt2-1;
    WorkVariMod(1).FluidDens = VOLgf_Dens;
    
    for i= 1:Compt2-1
        [OK,WhereC] = ismember(VOLgf_Names{i},ASS_Names2);
        
        % added (PL 25.07.2019):
        TheNameFromTheriak = VOLgf_Names{i};
        WereD = ismember(TheNameFromTheriak,'_');
        switch sum(WereD)
            case 1
                Where = find(WereD);
                WorkVariMod(1).FluidNames{i} = char(TheNameFromTheriak(1:Where-1));
                
            case 2
                % we have to delete the first one (Compatibility with the
                % MELT model of DOUG (considered as fluid)
                Where = find(WereD);
                NameTemp = TheNameFromTheriak(1:Where(1)-1);
                if isequal(NameTemp,'LIQtc6')
                    WorkVariMod(1).FluidNames{i} = NameTemp;
                else
                    % we delete the second one ...
                   WorkVariMod(1).FluidNames{i} = TheNameFromTheriak(1:Where(2)-1);
                end
                
            otherwise
                WorkVariMod(1).FluidNames{i} = VOLgf_Names{i};
        end
        
        WorkVariMod(1).FluidCOMP(i,:) = ASS_COMP2(WhereC,:);
    end
    
    % Check for melt (PL 25.07.2019)
    
    %keyboard
    
    % ---
    
else
    WorkVariMod(1).FluidPhases = 0;
    WorkVariMod(1).FluidDens = [];
    WorkVariMod(1).FluidCOMP = [];
end

% ------------------------------------------------------------------------
% Check Theriak Names and remove EM abbreviations
% ------------------------------------------------------------------------
for i=1:length(WorkVariMod(1).Names) 
    TheNameFromTheriak = WorkVariMod(1).Names{i};
    if ~ismember(TheNameFromTheriak,ListRefMiner)
        WereD = ismember(TheNameFromTheriak,'_');
        %keyboard
        switch sum(WereD)
            case 1
                % we delete it...
                Where = find(WereD);
                WorkVariMod(1).Names{i} = TheNameFromTheriak(1:Where-1);
            case 2
                % we have to delete the first one (Compatibility with the
                % MELT model of DOUG (considered as solid)
                Where = find(WereD);
                NameTemp = TheNameFromTheriak(1:Where(1)-1);
                if isequal(NameTemp,'LIQtc6')
                    WorkVariMod(1).Names{i} = NameTemp;
                else
                    % we delete the second one ...
                    WorkVariMod(1).Names{i} = TheNameFromTheriak(1:Where(2)-1);
                end
            case 3
                disp('Oups, too many underscores in this name contact Pierre Lanari')
                keyboard
        end
    end
end


% ------------------------------------------------------------------------
% Check for phase demixion (not identified in ListRefMiner). 
% ------------------------------------------------------------------------
% Note this is typically caused by flat G function in complex solid 
% solution models from Roger Powell (amphiboles).  
%
% in this case we select the phase with the higher volume fraction for
% comparison with the observation and rename the other phases.
%
%                                                  Pierre Lanari (24.10.16)

for i=1:length(WorkVariMod(1).Names)
    TheName = WorkVariMod(1).Names{i};
    Ind = find(ismember(WorkVariMod(1).Names,TheName));
    
    if length(Ind)>1
        Vols = WorkVariMod.VolFrac(Ind);
        [Val,IndSort] = sort(Vols,2,'descend');
        
        for j=2:length(IndSort)
            WorkVariMod(1).Names{Ind(IndSort(j))} = [WorkVariMod(1).Names{Ind(IndSort(j))},num2str(j)];
        end    
    end
end

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% (4) CHEMICAL POTENTIAL OF COMPONENTS
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

WhereChemCo = find(ismember(TestInput,'chemical potentials of components:'))+3;

Temp = strread(char(TestInput(WhereChemCo)),'%s')';
NbComponents = str2num(Temp{end});

Message = char(TestInput(WhereChemCo+7));

if isequal(Message,'oxydes probably buffered');
    WeStoreChemPot = 1;
else
    WeStoreChemPot = 0;
end

WhereChemRead = find(ismember(TestInput,'component   chem.pot.'))+2;

for i = 1:NbComponents
    Temp = strread(char(TestInput(WhereChemRead+i-1)),'%s')';
    % Sometimes there is n components and n-1 chemical potential displayed
    if ~isempty(Temp)
        Oxide = char(Temp{1});
        Oxide = upper(Oxide(2:end-1));
        WorkVariMod.ChemComp{i} = Oxide;
        if WeStoreChemPot
            WorkVariMod.ChemPot(i) = str2num(Temp{2});
        else
            WorkVariMod.ChemPot(i) = NaN;
        end
    end
end

% Below is the version that reads the chemical components defined in the
% database 

% WhereChemCo = find(ismember(TestInput,'--------------------------------------------------------------------'))+1;
% 
% Compt = 0;
% ComptOk = 0;
% while 1
%     Temp = strread(char(TestInput(WhereChemCo+Compt)),'%s')';
%     
%     if ~isequal(char(Temp{1}),'P')
%         break
%     end
%     
%     PName = char(Temp{3});
%     if isequal(PName(1),'*')
%         ComptOk = ComptOk+1;
%         WorkVariMod.ChemComp{ComptOk} = PName(2:end);
%         WorkVariMod.ChemPot(ComptOk) = -str2num(Temp{5});
%     end
%     Compt = Compt+1;
% end





return