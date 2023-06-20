function [Res,Evaluation,WorkVariMod] = OptiBingoPTX(X,NORM,LIMS,BinSet,WorkVariXMap,MinimOptions,TempBinBulk,ElementB,Bypass,IsHOptimized,app)
%

Res = [];

if length(Bypass)
    T = Bypass(1);
    P = Bypass(2);
else
    %T = X(1)*NORM(1);
    %P = X(2)*NORM(2);
    % I was rounding in SWOT 19 to help improving the convergence...
    % TESTED again in July 2019 and the rounding is critical for the
    % convergence of PTX problems. 
    T = round(X(1)*NORM(1));
    P = round(X(2)*NORM(2));
end

X_Vari = X(3:end).*NORM(3:end);

%disp(X_Vari);
%disp(LIMS);

VectTest = [T;P;X_Vari'];  

if sum(VectTest < LIMS(:,1)) || sum(VectTest > LIMS(:,2))
    Res = 1e19;
    return
end

% if T  < LIMS(1) || T > LIMS(2) || P < LIMS(3) || P > LIMS(4) || H < LIMS(5) || H > LIMS(6)
%     Res = 1e19;
%     return
% end

D_Temp = num2str(T);
D_Press = num2str(P);

% Here we need to update the bulk
[Bulk,TempBinBulk] = SuperFast_X_Update(TempBinBulk,ElementB,X_Vari);


Bulk(1) = '1';
BinSet.Bulk2Display = Bulk;

[WorkVariMod] = TheriakCall(BinSet,D_Temp,D_Press);

% New 10.03.2019
if IsHOptimized
    IsHSat2 = SaturationCheck4H(WorkVariMod); 
    if IsHSat2
        disp('H saturation - should not go there...')
        Res = 1e19;
        return
    end
end

DoWePrint = 0;

[Emin,Evaluation] = Opti_EpsiMinimCalc(WorkVariMod,WorkVariXMap,MinimOptions,DoWePrint,app);

% disp(' ')
% disp('-- OBJECTIVE FUNCTION --')
% disp(Bulk);
% fprintf('%s\t%.5f\n','T',T);
% fprintf('%s\t%.5f\n','P',P);
% fprintf('%s\t%.5f\n','X',X_Vari);
% disp(['Residual: ',num2str(Emin)])
% disp('------------------------')
% disp(' ')



% if length(WorkVariMod.FluidNames)
%     WorkVariMod.FluidNames
%     disp('Oups stauration...')
%     Res = 1e19;
%     return
% end

Res=Emin;
return

