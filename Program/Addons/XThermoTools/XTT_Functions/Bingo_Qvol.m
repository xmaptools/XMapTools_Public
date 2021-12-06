function [Evaluation,Report] = Bingo_Qvol(WorkVariMod,WorkVariXMap,Link,Evaluation,Report,DoWePrint,handles)
% ComputeVolumeAssemblage is a function to estimate the Evaluation of the  
% model for the volume of stable phases.
%
% To be described later once the strategy is set.
% 
% Last change by Pierre Lanari (21.06.2019)
%

VolTherMatch = zeros(size(Link.PhasesNames));
VolXMapMatch = zeros(size(Link.PhasesNames));

VolTherMatch(find(Link.TherIsIn)) = WorkVariMod.VolFrac(Link.TherIndices(find(Link.TherIndices)));
VolXMapMatch(find(Link.XMapIsIn)) = WorkVariXMap.VolFrac(Link.XMapIndices(find(Link.XMapIndices)));


% -------------------------------------------------------------------------
% Qvol - Eqation (2) in Duesterhoeft & Lanari (in prep)

Evaluation.Volume = 100.*sqrt(sum((VolTherMatch+VolXMapMatch)/2.*(1-abs(VolTherMatch-VolXMapMatch)./(VolTherMatch+VolXMapMatch)).^2));

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
    
    disp(' ')
    fprintf('%s\n','##### Evaluation criterion (2) VOLUME FRACTIONS ##### ');
    fprintf(Code1,'Phases:',Link.PhasesNames{:})
    fprintf(Code2,'THER:',VolTherMatch);
    fprintf(Code2,'XMAP:',VolXMapMatch);
    fprintf(Code2,'abs(D):',abs(VolTherMatch-VolXMapMatch));
    fprintf('%s\n','  ');
    fprintf('%s\n','-------------');
    fprintf('%s\t%.2f\n','Qvol =',Evaluation.Volume);  
    fprintf('%s\n','-------------');
    
    
    % Replace the names
    TheNamesPlot = Link.PhasesNames;
    for i=1:length(TheNamesPlot)
        TempStr = char(TheNamesPlot{i});
        for j=1:length(TempStr)
            if isequal(TempStr(j),'_')
                TempStr(j) = ' ';
            end
        end
        if VolTherMatch(i) > 0
            TheNamesPlotTher{i} = TempStr;
        else
            TheNamesPlotTher{i} = ' ';
        end
        
        if VolXMapMatch(i) > 0
            TheNamesPlotMap{i} = TempStr;
        else
            TheNamesPlotMap{i} = ' ';
        end
    end
    
    Report.TheNamesPlotMap = TheNamesPlotMap;
    Report.TheNamesPlotTher = TheNamesPlotTher;
    
    Report.VolXMap = VolXMapMatch*100;
    Report.VolTher = VolTherMatch*100;
            
end

return

