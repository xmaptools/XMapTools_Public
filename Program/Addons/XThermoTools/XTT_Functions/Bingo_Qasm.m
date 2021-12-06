function [Result,Link,Report] = Bingo_Qasm(WorkVariMod,WorkVariXMap,Report,DoWePrint,handles)
% Bingo_Qasm is the function calculating Qasm
%
% To be described later once the strategy is set.
% 
% Last change by Pierre Lanari (21.06.2019)
%

PhasesTher = WorkVariMod.Names;
PhasesXMap = WorkVariXMap.Names;


% Generate the variable LINK to be used later on
ComptPhase=0;
Link.PhasesNames = {''};
for i=1:length(PhasesTher)
    if ~ismember(Link.PhasesNames,PhasesTher{i})
        ComptPhase = ComptPhase+1;
        Link.PhasesNames{ComptPhase} = PhasesTher{i};
    end
end
for i=1:length(PhasesXMap)
    if ~ismember(Link.PhasesNames,PhasesXMap{i})
        ComptPhase = ComptPhase+1;
        Link.PhasesNames{ComptPhase} = PhasesXMap{i};
    end
end

[Link.TherIsIn,Link.TherIndices] = ismember(Link.PhasesNames,PhasesTher);
[Link.XMapIsIn,Link.XMapIndices] = ismember(Link.PhasesNames,PhasesXMap);

NbPhasesInvolved = length(Link.TherIsIn);
NbMissingPhases = 2*NbPhasesInvolved-(length(find(Link.TherIsIn))+length(find(Link.XMapIsIn)));

NbPhasesMax = max([length(PhasesTher),length(PhasesXMap)]);
NbMatches = length(find(ismember(PhasesTher,PhasesXMap)));


% -------------------------------------------------------------------------
% Qasm - Eqation (1) in Duesterhoeft & Lanari (in prep)

Result =  NbMatches/NbPhasesInvolved*100; 

% -------------------------------------------------------------------------



if DoWePrint
    disp(' ')
    Code1 = '%s\t\t';
    Code2 = '%s\t\t';
    for i=1:length(Link.PhasesNames)
        Len = length(Link.PhasesNames{i});
        if Len < 8
            for j=Len:8
                Link.PhasesNames{i}=[char(Link.PhasesNames{i}),' '];
            end
        end    
        Code1=[Code1,'%s\t'];
        Code2=[Code2,'%f\t'];
    end
    Code1(end) = 'n';
    Code2(end) = 'n';
    
    fprintf('%s\n','##### Evaluation criterion (1) ASSEMBLAGE ##### ');
    fprintf(Code1,'Phases:',Link.PhasesNames{:})
    fprintf(Code2,'THER:',Link.TherIsIn);
    fprintf(Code2,'XMAP:',Link.XMapIsIn);
    fprintf('%s\n','  ');
    fprintf('%s\n','-------------');
    fprintf('%s\t%.0f\n','n =',length(PhasesTher));
    fprintf('%s\t%.0f\n','m =',length(PhasesXMap));
    fprintf('%s\t%.0f\n','l =',NbMatches);
    fprintf('%s\t%.2f\n','Qasm =',Result);  
    fprintf('%s\n','-------------');
    
    Report.Phases = Link.PhasesNames;
    Report.PhasesXMap = Link.XMapIsIn;
    Report.PhasesTher = Link.TherIsIn;
    
    %keyboard
end


return

