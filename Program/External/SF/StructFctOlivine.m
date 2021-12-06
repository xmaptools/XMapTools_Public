function [OutputData,OutputVariables] = StructFctOlivine(InputData,InputVariables,ElOxDataDef)
% -
% XMapTools External Function: structural formula of olivine 
%  
%  ++09.2021 Compatibility with XMapTools 4
%       - version without loop & Fe2O3 as possible input
%   
%   ------------------------------------
%   End-member                   
%   ------------------------------------
%   Forsterite (Fo)   	Mg(2)Si(1)O(4)
%   Fayalite (Fa)       Fe(2)Si(1)O(4)
%   Tephroite (Tep)     Mn(2)Si(1)O(4)
%   ------------------------------------
%
% 4 Oxygen-basis
%
% P. Lanari - Last update 16.09.2021
% Find out more at https://xmaptools.ch

OutputVariables = {'Si','Fe','Mg','Mn','Cr','Ni','Ti','SumCat','Xfo','Xfa','Xtep'};

OutputData = zeros(size(InputData,1),length(OutputVariables));

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
Cr= MatrixSF(:,10);
Ni= MatrixSF(:,10);

SumCat = Si+Ti+Al+Fe+Mn+Mg+Ca+Na+K+Cr+Ni;

Xfo = Mg./(Mg+Fe+Mn);
Xfa = Fe./(Mg+Fe+Mn);
Xtep = Mn./(Mg+Fe+Mn);

OutputData(WhereMin,:) = [Si,Fe,Mg,Mn,Cr,Ni,Ti,SumCat,Xfo,Xfa,Xtep];
 
end







