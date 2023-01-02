function [Res,CompoMod] = OptiBingoPTSinglePhase(X,NORM,LIMS,BinSet,WorkVariXMap,BinPhaseDef,app)
%
Res = [];

T = X(1)*NORM(1);
P = X(2)*NORM(2);

if T  < LIMS(1) || T > LIMS(2) || P < LIMS(3) || P > LIMS(4)
    Res = 1e19;
    return
end

D_Temp = num2str(T);
D_Press = num2str(P * 1e4);

[WorkVariMod] = TheriakCall(BinSet,D_Temp,D_Press);
%keyboard
[ESP,CompoMod] = Opti_EpsiMinimCalcSinglePhase(WorkVariMod,WorkVariXMap,BinPhaseDef,app);

Res = ESP;

return