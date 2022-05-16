function [OutputData,OutputVariables] = Biotite_T_H05(InputData,InputVariables,AddParameters,ElOxDataDef)
% -
% XMapTools External Function: biotite thermobarometry 
%  
% List of output variables:
%       - T_W20     Wu & Chen (2015)
%       - T_H05     Henry et al. (2005)
%
% 22 Oxygen-basis 
%
% P. Lanari - Last update 10.02.2022
% Find out more at https://xmaptools.ch

OutputVariables = {'T_H05','T_W15','Ti','XMg'};

OutputData = zeros(size(InputData,1),length(OutputVariables));

P_kbar = AddParameters.Values(1);
P_GPa = P_kbar/10;

% General structural formula function for 22 oxygen
WhereMin = find(sum(InputData,2) > 50);
[MatrixSF,ElementsList] = SF_OxNorm(InputData(WhereMin,:),InputVariables,22,ElOxDataDef);

Si = MatrixSF(:,1);
Ti = MatrixSF(:,2);
Al= MatrixSF(:,3);
Fe= MatrixSF(:,4)+MatrixSF(:,5);
Mn= MatrixSF(:,6);
Mg= MatrixSF(:,7);
Ca= MatrixSF(:,8);
Na= MatrixSF(:,9);
K= MatrixSF(:,10);

XMg = Mg./(Mg+Fe);

Alvi = Al-(6-Si);
X_Ti = Ti./(Fe+Mg+Alvi+Ti);
X_Fe = Fe./(Fe+Mg+Alvi+Ti);
X_Mg = Mg./(Fe+Mg+Alvi+Ti);

A = -2.3594;
B = 4.6482e-9;
C = -1.7283;

T_H05 = zeros(size(XMg));
for i = 1:length(T_H05)
    Temp = ((log(Ti(i))-A-C.*XMg(i).^3)/B).^0.333;
    if isreal(Temp)
        T_H05(i) = Temp;
    end
end

T_W15 = exp(6.313+0.224.*log(X_Ti)-0.288.*log(X_Fe)-0.449.*log(X_Mg)+0.15.*P_GPa);
Filter = find(T_W15 < 100 | T_W15 > 1000);
T_W15(Filter) = zeros(size(Filter));

OutputData(WhereMin,:) = [T_H05,T_W15,Ti,XMg];

end







