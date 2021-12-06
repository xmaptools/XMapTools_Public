function [OutputData,OutputVariables] = StructFctSerpentine(InputData,InputVariables,ElOxDataDef)
% -
% XMapTools External Function: structural formula of serpentine 
%  
%  ++09.2021 Compatibility with XMapTools 4
%       - version without loop & Fe2O3 as possible input
%
% 14.5 Oxygen-basis
%
% P. Lanari - Last update 16.09.2021
% Find out more at https://xmaptools.ch

OutputVariables = {'Si','Al','Fe','Mg','XFe','XMg','Mn','Cr','Ni','SumCat'};

OutputData = zeros(size(InputData,1),length(OutputVariables));

% General structural formula function for 14.5 oxygen
WhereMin = find(sum(InputData,2) > 70);
[MatrixSF,ElementsList] = SF_OxNorm(InputData(WhereMin,:),InputVariables,14.5,ElOxDataDef);

Si = MatrixSF(:,1);
Ti = MatrixSF(:,2);
Al= MatrixSF(:,3);
Fe= MatrixSF(:,4)+MatrixSF(:,5);
Mn= MatrixSF(:,6);
Mg= MatrixSF(:,7);
Ca= MatrixSF(:,8);
Na= MatrixSF(:,9);
K= MatrixSF(:,10);
Cr= MatrixSF(:,10);
Ni= MatrixSF(:,10);

SumCat = Si+Ti+Al+Fe+Mn+Mg+Ca+Na+K+Cr+Ni;

XMg = Mg./(Mg+Fe);
XFe = Fe./(Mg+Fe);

OutputData(WhereMin,:) = [Si,Al,Fe,Mg,XFe,XMg,Mn,Cr,Ni,SumCat];
 
end







