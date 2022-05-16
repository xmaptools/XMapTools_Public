function [OutputData,OutputVariables] = Phengite_P_All(InputData,InputVariables,AddParameters,ElOxDataDef)
% -
% XMapTools External Function: phengite barometry 
%  
% List of output variables:
%       - P_MS87      Massone & Schreyer, (1987)
%
% 11 Oxygen-basis 
%
% P. Lanari - Last update 07.01.2022
% Find out more at https://xmaptools.ch

OutputVariables = {'P_MS87','T','Si'};

OutputData = zeros(size(InputData,1),length(OutputVariables));

% General structural formula function for 11 oxygen
WhereMin = find(sum(InputData,2) > 50);
[MatrixSF,ElementsList] = SF_OxNorm(InputData(WhereMin,:),InputVariables,11,ElOxDataDef);

Si = MatrixSF(:,1);
Ti = MatrixSF(:,2);
Al= MatrixSF(:,3);
Fe= MatrixSF(:,4)+MatrixSF(:,5);
Mn= MatrixSF(:,6);
Mg= MatrixSF(:,7);
Ca= MatrixSF(:,8);
Na= MatrixSF(:,9);
K= MatrixSF(:,10);

TC = ones(size(Si))*AddParameters(1).Values;

P_MS87 = 0.0112*TC+(25*Si-80.36);    % This is a fit by Lanari made in 2011

Pbad = find(P_MS87 <= 0);
P_MS87(Pbad) = zeros(size(Pbad));

OutputData(WhereMin,:) = [P_MS87,TC,Si];

end







