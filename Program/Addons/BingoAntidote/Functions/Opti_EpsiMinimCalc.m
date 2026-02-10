function [Emin,Evaluation] = Opti_EpsiMinimCalc(WorkVariMod,WorkVariXMap,MinimOptions,DoWePrint,app)

Report = [];

% Step 0 – Exclude melt (added 11.09.2025)
if isequal(app.BinGfDef.Melt.Activate,1)
    if isequal(app.BinGfDef.Melt.Include,0)
        [IsMelt,WhereMelt] = find(ismember(WorkVariMod.Names,app.BinGfDef.Melt.DBName));
        if isequal(IsMelt,1)
            WorkVariMod.Names(WhereMelt) = [];
            WorkVariMod.Indice = [1:length(WorkVariMod.Names)];
            WorkVariMod.COMP(WhereMelt,:) = [];
            WorkVariMod.VolFrac(WhereMelt) = [];
            WorkVariMod.VolFrac = WorkVariMod.VolFrac ./ sum(WorkVariMod.VolFrac);
            WorkVariMod.Dens(WhereMelt) = [];
            WorkVariMod.NbPhases = length(WorkVariMod.Names);
        end
    end
end

[Evaluation.assemblage,Link] = Bingo_Qasm(WorkVariMod,WorkVariXMap,Report,DoWePrint,0,app);

[Evaluation] = Bingo_Qvol(WorkVariMod,WorkVariXMap,Link,Evaluation,Report,DoWePrint,0,app);

[Evaluation] = Bingo_Qcmp(WorkVariMod,WorkVariXMap,Link,Evaluation,Report,DoWePrint,0,app);


% Here we can propose both strategies of optimization
if MinimOptions.Weights.Use
    Wts = -MinimOptions.Weights.Values(MinimOptions.Weights.Selected,:);
    
    %[E_Minim2,Wts(3)];
    Emin = (Evaluation.assemblage*Wts(1)) + (Evaluation.Volume*Wts(2)) + (Evaluation.Compositions*Wts(3));
    
    % Normalization to fit with Tol(%) between the two equations
    %Emin = 100 * Emin;
    
else
    % Changed PL 09.03.19 to match BingoCall ** TESTED **
    Emin = -((Evaluation.assemblage+Evaluation.assemblage/100*Evaluation.Volume + Evaluation.assemblage/100*Evaluation.Compositions)/3); 
    
    % Old version:
    % (Evaluation.assemblage+Evaluation.assemblage/100*Evaluation.Volume + Evaluation.assemblage/100*Evaluation.Volume/100*Evaluation.Compositions)/3;
end

return
