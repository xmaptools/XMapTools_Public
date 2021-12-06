function [OutputData,OutputVariables] = Biotite_T_H05(InputData,InputVariables,AddParameters,ElOxDataDef)
% -
% XMapTools External Function: biotite thermobarometry 
%  
% List of output variables:
%       - T_H05      Henry et al. (2005)
%
% 22 Oxygen-basis 
%
% P. Lanari - Last update 06.03.2021
% Find out more at https://xmaptools.ch

OutputVariables = {'T_H05','Ti','XMg'};

OutputData = zeros(size(InputData,1),length(OutputVariables));

% General structural formula function for 11 oxygen
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

OutputData(WhereMin,:) = [T_H05,Ti,XMg];

end







