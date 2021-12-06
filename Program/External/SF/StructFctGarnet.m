function [OutputData,OutputVariables] = StructFctGarnet(InputData,InputVariables,ElOxDataDef)
% -
% XMapTools External Function: structural formula of garnet 
%  
%  ++01.2021 Compatibility with XMapTools 4
%       - version without loop & Fe2O3 as possible input
%   
%   -----------------------------------------------
%   End-member          X(3)        Y(2)    O/T(3) 
%   -----------------------------------------------
%   Almandine (Alm)     Fe,Fe,Fe    Al,Al   (SiO4)3
%   Pyrope (Prp)        Mg,Mg,Mg    Al,Al   (SiO4)3 
%   Grossular (Grs)     Ca,Ca,Ca    Al,Al   (SiO4)3 
%   Spessartine (Sps)   Mn,Mn,Mn    Al,Al   (SiO4)3 
%   -----------------------------------------------  
%
% 12 Oxygen-basis 
%
% P. Lanari - Last update 14.01.2021
% Find out more at https://xmaptools.ch

OutputVariables = {'Si','Ti','Al','Mg','Fe','Mn','Ca','SumCat','Xalm','Xsps','Xprp','Xgrs','XMg','ratio_CaFe'};

OutputData = zeros(size(InputData,1),length(OutputVariables));

% General structural formula function for 12 oxygen
WhereMin = find(sum(InputData,2) > 70);
[MatrixSF,ElementsList] = SF_OxNorm(InputData(WhereMin,:),InputVariables,12,ElOxDataDef);

Si = MatrixSF(:,1);
Ti = MatrixSF(:,2);
Al= MatrixSF(:,3);
Fe= MatrixSF(:,4)+MatrixSF(:,5);
Mn= MatrixSF(:,6);
Mg= MatrixSF(:,7);
Ca= MatrixSF(:,8);
Na= MatrixSF(:,9);
K= MatrixSF(:,10);

SumCat = Si+Ti+Al+Fe+Mn+Mg+Ca+Na+K;

ratio_CaFe = Ca./Fe;

Xgrs = Ca./(Ca+Fe+Mg+Mn);
Xsps = Mn./(Ca+Fe+Mg+Mn);
Xprp = Mg./(Ca+Fe+Mg+Mn);
Xalm = Fe./(Ca+Fe+Mg+Mn);

XMg = Mg./(Mg+Fe);

OutputData(WhereMin,:) = [Si,Ti,Al,Mg,Fe,Mn,Ca,SumCat,Xalm,Xsps,Xprp,Xgrs,XMg,ratio_CaFe];

end







