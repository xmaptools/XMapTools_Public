function [OutputData,OutputVariables] = StructFctRutile(InputData,InputVariables,ElOxDataDef)
% -
% XMapTools External Function: structural formula of rutile 
%  
%  ++09.2021 Compatibility with XMapTools 4
%       - version without loop & Fe2O3 as possible input
%   
% 2 Oxygen-basis
%
% P. Lanari - Last update 08.09.2021
% Find out more at https://xmaptools.ch

OutputVariables = {'Ti','Zr','Mg','Fe','Mn','Si','SumCat'};

OutputData = zeros(size(InputData,1),length(OutputVariables));

% General structural formula function for 3 oxygen
WhereMin = find(sum(InputData,2) > 50);
[MatrixSF,ElementsList] = SF_OxNorm(InputData(WhereMin,:),InputVariables,2,ElOxDataDef);

Si = MatrixSF(:,1);
Ti = MatrixSF(:,2);
Al = MatrixSF(:,3);
Fe = MatrixSF(:,4)+MatrixSF(:,5);
Mn = MatrixSF(:,6);
Mg = MatrixSF(:,7);
Ca = MatrixSF(:,8);
Na = MatrixSF(:,9);
K = MatrixSF(:,10);
Zr = MatrixSF(:,11);

SumCat = Si+Ti+Al+Fe+Mn+Mg+Ca+Na+K+Zr;

OutputData(WhereMin,:) = [Ti,Zr,Mg,Fe,Mn,Si,SumCat];

end







