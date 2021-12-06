function [OutputData,OutputVariables] = StructFctAluminosilicate(InputData,InputVariables,ElOxDataDef)
% -
% XMapTools External Function: structural formula of cordierite 
%  
%  ++08.2021 Compatibility with XMapTools 4
%       - version without loop & all Fe as Fe2O3
%   
% 18 Oxygen-basis 
%
% P. Lanari - Last update 13.08.2021
% Find out more at https://xmaptools.ch

OutputVariables = {'Si','Al','Fe3','Mn','Mg','SumCat'};

OutputData = zeros(size(InputData,1),length(OutputVariables));

% General structural formula function for 5 oxygen
WhereMin = find(sum(InputData,2) > 50);

% We transform all the iron into Fe3+
InputDataCorr = InputData;
InputDataCorr(WhereMin,4) = InputData(WhereMin,4) .* 0;
InputDataCorr(WhereMin,5) = InputData(WhereMin,4) .* 1 .* (1/0.89992485);

[MatrixSF,ElementsList] = SF_OxNorm(InputDataCorr(WhereMin,:),InputVariables,5,ElOxDataDef);

Si = MatrixSF(:,1);
Ti = MatrixSF(:,2);
Al= MatrixSF(:,3);
Fe= MatrixSF(:,4)+MatrixSF(:,5);
Mn= MatrixSF(:,6);
Mg= MatrixSF(:,7);
Ca= MatrixSF(:,8);
Na= MatrixSF(:,9);
K= MatrixSF(:,10);


Fe3 = MatrixSF(:,5);
SumCat = Si+Ti+Al+Fe+Mn+Mg+Ca+Na+K;


OutputData(WhereMin,:) = [Si,Al,Fe3,Mn,Mg,SumCat];

end







