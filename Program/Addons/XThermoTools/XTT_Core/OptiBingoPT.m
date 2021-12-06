function [Res,Evaluation,WorkVariMod] = Core_OptiBingoPT(X,NORM,LIMS,BinSet,WorkVariXMap,MinimOptions,handles)
%
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

DoWePrint = 0;

[Emin,Evaluation] = Opti_EpsiMinimCalc(WorkVariMod,WorkVariXMap,MinimOptions,DoWePrint,handles);


Res=Emin;

return