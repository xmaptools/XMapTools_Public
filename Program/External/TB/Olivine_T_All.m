function [OutputData,OutputVariables] = Olivine_T_All(InputData,InputVariables,AddParameters,ElOxDataDef)
% -
% XMapTools External Function: olivine thermobarometry 
%  
% List of output variables:
%       - T_SC13    Shejwalkar and Coogan (2013)
%       - T_B17     Bussweiler et al (2017)
%
% 4 Oxygen-basis 
%
% R. Tamblyn & P. Lanari - Last update 30.10.2025
% Find out more at https://xmaptools.ch

OutputVariables = {'T_SC13','T_B17','Al_mg_g','XMg'};

OutputData = zeros(size(InputData,1),length(OutputVariables));

P_kbar = AddParameters.Values(1);

% General structural formula function for 4 oxygen
WhereMin = find(sum(InputData,2) > 70);
[MatrixSF,ElementsList] = SF_OxNorm(InputData(WhereMin,:),InputVariables,4,ElOxDataDef);

Si = MatrixSF(:,1);
Ti = MatrixSF(:,2);
Al= MatrixSF(:,3);
Fe= MatrixSF(:,4)+MatrixSF(:,5);
Mn= MatrixSF(:,6);
Mg= MatrixSF(:,7);
Ca= MatrixSF(:,8);
Na= MatrixSF(:,9);
K= MatrixSF(:,10);
Cr= MatrixSF(:,11);
Ni= MatrixSF(:,12);
XMg = Mg./(Mg+Fe);

mtc = Ca .* 2;
fo = (Mg - Ca) ./ 2;
fa = Fe ./ 2;
tep = Mn ./2;

Xmtc = mtc ./ (mtc+fo+fa+tep);
Xfo = fo ./ (mtc+fo+fa+tep);
Xfa = fa ./ (mtc+fo+fa+tep);
Xtep = tep ./ (mtc+fo+fa+tep);

Al2O3 = InputData(WhereMin,3); 
Al_mg_g = Al2O3 .* 0.529251 .* 1e4;

T_SC13 = zeros(size(XMg));
T_B17 = zeros(size(XMg));

for i = 1:length(T_SC13)
    Temp = (-12368 ./ (log(Xmtc(i)) - 6.395 + 3.234 .* Xfo(i))) - 273;
    if isreal(Temp) && Temp > 100
        T_SC13(i) = Temp;
    end
end
for i = 1:length(T_B17)
    Temp = (11245 + 48 * P_kbar) ./ (13.68 - log(Al_mg_g(i))) - 273;
    if isreal(Temp) && Temp > 100
        T_B17(i) = Temp;
    end
end

% T_SC13 = (-12368 ./ (log(Xmtc) - 6.395 + 3.234 .* Xfo)) - 273;
% T_B17 =  (11245 + 48 * P_kbar) ./ (13.68 - log(Al_mg_g)) - 273;

OutputData(WhereMin,:) = [T_SC13,T_B17,Al_mg_g,XMg];

end







