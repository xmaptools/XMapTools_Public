function [Res,Evaluation,WorkVariMod] = OptiBingoPT(X,NORM,LIMS,BinSet,WorkVariXMap,MinimOptions,app)
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
D_Press = num2str(P * 1e4);

%tic
[WorkVariMod] = TheriakCall(BinSet,D_Temp,D_Press);
%toc

DoWePrint = 0;

[Emin,Evaluation] = Opti_EpsiMinimCalc(WorkVariMod,WorkVariXMap,MinimOptions,DoWePrint,app);


if isequal(app.LiveDisplaySwitch.Value,'On')
    if app.LiveUpdate
        app.LIVE_Qtot_Gauge.Value = abs(Emin);
        app.EditField_BestQtot.Value = app.LIVE_Qtot_Gauge.Value;
        drawnow
    end
else
    app.LIVE_Qtot_Gauge.Value = 0;
    app.EditField_BestQtot.Value = 0;
end


Res=Emin;

return