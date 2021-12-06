function [OutputData,OutputVariables] = StructFctBrucite(InputData,InputVariables,ElOxDataDef)
% -
% XMapTools External Function: structural formula of brucite 
%  
%  ++09.2021 Compatibility with XMapTools 4
%       - version without loop & Fe2O3 as possible input
%       - As2O3 is used instead of As2O5 in XMapTools 3.4.2  
%   
% 1 Oxygen-basis 
%
% P. Lanari - Last update 09.09.2021
% Find out more at https://xmaptools.ch

OutputVariables = {'Si','Ti','Al','Fe','Mn','Mg','Ca','Na','K','As','Sb','Cs','XMg','SumCat'};

OutputData = zeros(size(InputData,1),length(OutputVariables));

% General structural formula function for 1 oxygen
WhereMin = find(sum(InputData,2) > 50);
[MatrixSF,ElementsList] = SF_OxNorm(InputData(WhereMin,:),InputVariables,1,ElOxDataDef);

Si = MatrixSF(:,1);
Ti = MatrixSF(:,2);
Al = MatrixSF(:,3);
Fe = MatrixSF(:,4) + MatrixSF(:,5);
Mn = MatrixSF(:,6);
Mg = MatrixSF(:,7);
Ca = MatrixSF(:,8);
Na = MatrixSF(:,9);
K  = MatrixSF(:,10);
As = MatrixSF(:,11);
Sb = MatrixSF(:,12);
Cs = MatrixSF(:,13);

XMg = Mg./(Mg+Fe);
SumCat = Si + Ti + Al + Fe + Mn + Mg + Ca + Na + K + As + Sb + Cs;

OutputData(WhereMin,:) = [Si,Ti,Al,Fe,Mn,Mg,Ca,Na,K,As,Sb,Cs,XMg,SumCat];

end







