function [OutputData,OutputVariables] = StructFctSerpentine(InputData,InputVariables,ElOxDataDef)
% -
% XMapTools External Function: structural formula of serpentine 
%  
%  ++05.2022 Bug fix
%       - number of oxygen changed from 14.5 to 14
%       - add ratio_Si_SiFeMg
%  ++09.2021 Compatibility with XMapTools 4
%       - version without loop & Fe2O3 as possible input
% 
% 14 Oxygen-basis
%
% P. Lanari - Last update 09.05.2022
% Find out more at https://xmaptools.ch

OutputVariables = {'Si','Al','Fe','Mg','XFe','XMg','Mn','Cr','Ni','ratio_Si_SiFeMg','SumCat'};

OutputData = zeros(size(InputData,1),length(OutputVariables));

% General structural formula function for 14 oxygen
WhereMin = find(sum(InputData,2) > 70);
[MatrixSF,ElementsList] = SF_OxNorm(InputData(WhereMin,:),InputVariables,14,ElOxDataDef);

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

SumCat = Si+Ti+Al+Fe+Mn+Mg+Ca+Na+K+Cr+Ni;

XMg = Mg./(Mg+Fe);
XFe = Fe./(Mg+Fe);

ratio_Si_SiFeMg = Si./(Si+Fe+Mg);

OutputData(WhereMin,:) = [Si,Al,Fe,Mg,XFe,XMg,Mn,Cr,Ni,ratio_Si_SiFeMg,SumCat];
 
end







