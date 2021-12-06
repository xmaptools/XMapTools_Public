function [Res,CompoMod] = OptiBingoPTMultiPhase(X,NORM,LIMS,BinSet,WorkVariXMap,BinPhaseDef)
%
Res = [];

T = X(1)*NORM(1);
P = X(2)*NORM(2);

if T  < LIMS(1) || T > LIMS(2) || P < LIMS(3) || P > LIMS(4)
    Res = 1e19;
    return
end

D_Temp = num2str(T);
D_Press = num2str(P);

[WorkVariMod] = TheriakCall(BinSet,D_Temp,D_Press);
%keyboard
[ESP,CompoMod] = Opti_EpsiMinimCalcMultiPhase(WorkVariMod,WorkVariXMap,BinPhaseDef);


%DoWePrint = 0;

%[Emin] = Opti_EpsiMinimCalc(WorkVariMod,WorkVariXMap,DoWePrint);

%[A1,V1,C1,E4] = BingoCall(WorkVariMod,WorkVariXMap,DoWePrint);
Res = ESP;
return