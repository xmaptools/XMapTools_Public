function [OutputData,OutputVariables] = StructFctGarnet_Fe3(InputData,InputVariables,ElOxDataDef)
% -
% XMapTools External Function: structural formula of garnet with Fe3+
%  
%  ++09.2024 Fix an issue with end-member calculation for Fe3+ rich garnet
%       - Strategy changed
%       - Tested for And- and Alm-rich garnet compositions
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
% P. Lanari - Last update 26.09.2024
% Find out more at https://xmaptools.ch

OutputVariables = {'Si','Ti','Al','Mg','Fe2','Fe3','Mn','Ca','SumCat','Xalm','Xsps','Xprp','Xgrs','Xand','Xsum','XMg','XFe3','ratio_CaFe'};

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

% The end-member calculation function was changed on 26.09.24 to fix an
% issue with andradite garnet (reported by Rich Taylor). 

% Xand is now calculated first: 
Xand = Fe3./(Fe3+Al);
WhereZeros = find(Xand <= 0);
Xand(WhereZeros) = 0;

% Ca is corrected for the andradite components: 
Ca_corr = Ca - 3/2 .* Fe3;
WhereZeros = find(Ca_corr <= 0);
Ca_corr(WhereZeros) = 0;

% Other end-members are calculated on the remaining fraction from the
% proportions of Ca_corr, Fe, Mg and Mn:
Xalm = (1-Xand) .* (Fe2./(Fe2+Mg+Ca_corr+Mn));
Xsps = (1-Xand) .* (Mn./(Fe2+Mg+Ca_corr+Mn));
Xprp = (1-Xand) .* (Mg./(Fe2+Mg+Ca_corr+Mn));
Xgrs = (1-Xand) .* (Ca_corr./(Fe2+Mg+Ca_corr+Mn));

% The sum should always be one. 
Xsum = Xand + Xalm + Xsps + Xprp + Xgrs;

XMg = Mg./(Mg+Fe2);
XFe3 = Fe3./(Fe2+Fe3);

OutputData(WhereMin,:) = [Si,Ti,Al,Mg,Fe2,Fe3,Mn,Ca,SumCat,Xalm,Xsps,Xprp,Xgrs,Xand,Xsum,XMg,XFe3,ratio_CaFe];

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




