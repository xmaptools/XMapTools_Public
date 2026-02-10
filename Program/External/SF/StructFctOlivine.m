function [OutputData,OutputVariables] = StructFctOlivine(InputData,InputVariables,ElOxDataDef)
% -
% XMapTools External Function: structural formula of olivine 
%  
%  ++10.2025 Add monticellite end-member (most common one according to Renée
%  Tamblyn)
%  ++09.2021 Compatibility with XMapTools 4
%       - version without loop & Fe2O3 as possible input
%   
%   ------------------------------------------
%   End-member                   
%   ------------------------------------------
%   Forsterite (Fo)   	       Mg(2)Si(1)O(4)
%   Fayalite (Fa)              Fe(2)Si(1)O(4)
%   Tephroite (Tep)            Mn(2)Si(1)O(4)
%   Monticellite (Mtc)    Ca(1)Mg(1)Si(1)O(4)
%   ------------------------------------------
%
% 4 Oxygen-basis
%
% P. Lanari - Last update 30.10.2025
% Find out more at https://xmaptools.ch

OutputVariables = {'Si','Fe','Mg','Mn','Cr','Ni','Ti','SumCat','Xfo','Xfa','Xtep','Xmtc'};

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
Cr= MatrixSF(:,11);
Ni= MatrixSF(:,12);

SumCat = Si+Ti+Al+Fe+Mn+Mg+Ca+Na+K+Cr+Ni;

mtc = Ca .* 2;
fo = (Mg - Ca) ./ 2;
fa = Fe ./ 2;
tep = Mn ./2;

Xmtc = mtc ./ (mtc+fo+fa+tep);
Xfo = fo ./ (mtc+fo+fa+tep);
Xfa = fa ./ (mtc+fo+fa+tep);
Xtep = tep ./ (mtc+fo+fa+tep);

%Xfo = Mg./(Mg+Fe+Mn);
%Xfa = Fe./(Mg+Fe+Mn);
%Xtep = Mn./(Mg+Fe+Mn);

OutputData(WhereMin,:) = [Si,Fe,Mg,Mn,Cr,Ni,Ti,SumCat,Xfo,Xfa,Xtep,Xmtc];
 
end







