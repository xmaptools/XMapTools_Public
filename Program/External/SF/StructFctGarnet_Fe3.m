function [OutputData,OutputVariables] = StructFctGarnet_Fe3(InputData,InputVariables,ElOxDataDef)
% -
% XMapTools External Function: structural formula of garnet 
%  
%  ++09.2021 Improvement in Fe3+ calculation
%       - Method of Droop (1987) implemented
%  ++01.2021 Compatibility with XMapTools 4
%       - Version without loop
%
%   -----------------------------------------------
%   End-member          X(3)        Y(2)    O/T(3) 
%   -----------------------------------------------
%   Almandine (Alm)     Fe,Fe,Fe    Al,Al   (SiO4)3
%   Pyrope (Prp)        Mg,Mg,Mg    Al,Al   (SiO4)3 
%   Grossular (Grs)     Ca,Ca,Ca    Al,Al   (SiO4)3 
%   Spessartine (Sps)   Mn,Mn,Mn    Al,Al   (SiO4)3
%   Andradite (and)     Ca,Ca,Ca    Fe,Fe   (SiO4)3
%   -----------------------------------------------  
%
% 12 Oxygen-basis 
%
% P. Lanari - Last update 14.01.2021
% Find out more at https://xmaptools.ch

OutputVariables = {'Si','Ti','Al','Mg','Fe2','Fe3','Mn','Ca','SumCat','Xalm','Xsps','Xprp','Xgrs','Xand','XMg','XFe3','ratio_CaFe'};

OutputData = zeros(size(InputData,1),length(OutputVariables));

% General structural formula function for 12 oxygen FeO as Fe_total
WhereMin = find(sum(InputData,2) > 70);
[MatrixSF,ElementsList] = SF_OxNorm(InputData(WhereMin,:),InputVariables,12,ElOxDataDef);


% Fe3+ approximation (Droop 1987)
Fe = MatrixSF(:,4)+MatrixSF(:,5);

Fe3_Droop = 2*12*(1-8./sum(MatrixSF,2));
ZeroFe3 = find(Fe3_Droop < 0);
Fe3_Droop(ZeroFe3) = zeros(size(ZeroFe3));
XFe3 = Fe3_Droop./Fe;
OneXFe3 = find(XFe3 > 1);
XFe3(OneXFe3) = ones(size(OneXFe3));

InputDataCorr = InputData; 

InputDataCorr(WhereMin,4) = InputData(WhereMin,4) .* (1-XFe3);
InputDataCorr(WhereMin,5) = InputData(WhereMin,4) .* XFe3 .* (1/0.89992485);


% General structural formula function for 12 oxygen including FeO and Fe2O3
[MatrixSF2,ElementsList] = SF_OxNorm(InputDataCorr(WhereMin,:),InputVariables,12,ElOxDataDef);

Si = MatrixSF2(:,1);
Ti = MatrixSF2(:,2);
Al = MatrixSF2(:,3);
Fe2 = MatrixSF2(:,4);
Fe3 = MatrixSF2(:,5);
Mn = MatrixSF2(:,6);
Mg = MatrixSF2(:,7);
Ca = MatrixSF2(:,8);
Na = MatrixSF2(:,9);
K = MatrixSF2(:,10);

SumCat = Si+Ti+Al+Fe2+Fe3+Mn+Mg+Ca+Na+K;

ratio_CaFe = Ca./Fe2;

Xalm = Fe2./(Fe2+Mg+Ca+Mn+Fe3);
Xsps = Mn./(Fe2+Mg+Ca+Mn+Fe3);
Xprp = Mg./(Fe2+Mg+Ca+Mn+Fe3);
Xgrs = Ca./(Fe2+Mg+Ca+Mn+Fe3);
Xand = Fe3./(Fe2+Mg+Ca+Mn+Fe3);

XMg = Mg./(Mg+Fe2);
XFe3 = Fe3./(Fe2+Fe3);

OutputData(WhereMin,:) = [Si,Ti,Al,Mg,Fe2,Fe3,Mn,Ca,SumCat,Xalm,Xsps,Xprp,Xgrs,Xand,XMg,XFe3,ratio_CaFe];

end





% % Fe3+ approximation (by PL; simple, wrong but efficient!)
% Si = MatrixSF(:,1);
% Ti = MatrixSF(:,2);
% Al = MatrixSF(:,3);
% Fe = MatrixSF(:,4);
% 
% Diff = 5 - (Si+Al+Ti);
% 
% Fe2 = Fe;
% Fe3 = zeros(size(Diff));
% XFe3 = zeros(size(Diff));
% 
% WherePos = find(Diff > 0);
% 
% Fe2(WherePos) = Fe(WherePos) - Diff(WherePos).*0.89992485;
% Fe3(WherePos) = Diff(WherePos).*0.89992485; 
% 
% XFe3 = Fe3./(Fe2+Fe3);




