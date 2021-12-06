function [Emin,Evaluation] = Core_EpsiMinimCalc(WorkVariMod,WorkVariXMap,MinimOptions,DoWePrint,handles)

Report = [];

[Evaluation.assemblage,Link] = Bingo_Qasm(WorkVariMod,WorkVariXMap,Report,DoWePrint,handles);

[Evaluation] = Bingo_Qvol(WorkVariMod,WorkVariXMap,Link,Evaluation,Report,DoWePrint,handles);

[Evaluation] = Bingo_Qcmp(WorkVariMod,WorkVariXMap,Link,Evaluation,Report,DoWePrint,handles);


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
