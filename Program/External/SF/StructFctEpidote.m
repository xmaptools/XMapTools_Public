function [OutputData,OutputVariables] = StructFctEpidote(InputData,InputVariables,ElOxDataDef)
% -
% XMapTools External Function: structural formula of epidote 
%  
%  ++01.2021 Compatibility with XMapTools 4
%       - version without loop & all iron as Fe3+
%   
%   -------------------------------------------------------------
%   End-member          T1(3)       M1(1)	M2(1)   M3(1)   A1(2) 
%   -------------------------------------------------------------
%   Epidote (Ep)        Si,Si,Si    Al      Al      Fe      Ca,Ca
%   Fe-epidote (fEp)    Si,Si,Si    Fe      Al      Fe      Ca,Ca
%   Zoisite (Zo)        Si,Si,Si    Al      Al      Al      Ca,Ca
%   Mn-epidote (mEp)    Si,Si,Si    Al      Al      Mn      Ca,Ca
%   -------------------------------------------------------------  
%
% 12.5 Oxygen-basis 
%
% P. Lanari - Last update 26.01.2021
% Find out more at https://xmaptools.ch

OutputVariables = {'Si','Ti','Al','Mg','Fe','Mn','Ca','SumCat','Al_M1','Al_M2','Fe_M1','Al_M3','Fe_M3','Mn_M3','Xep','Xfep','Xzo','Xmep'};

OutputData = zeros(size(InputData,1),length(OutputVariables));

% General structural formula function for 12.5 oxygen
WhereMin = find(sum(InputData,2) > 50);

% We transform all the iron into Fe3+
InputDataCorr = InputData;
InputDataCorr(WhereMin,4) = InputData(WhereMin,4) .* 0;
InputDataCorr(WhereMin,5) = InputData(WhereMin,4) .* 1 .* (1/0.89992485);

[MatrixSF,ElementsList] = SF_OxNorm(InputDataCorr(WhereMin,:),InputVariables,12.5,ElOxDataDef);

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

Al_M2 = ones(size(Fe));
AlM1M3 = Al-Al_M2;
WhereAlM1M3 = find(AlM1M3 < 0);
if ~isempty(WhereAlM1M3)
    Al_M2(WhereAlM1M3) = Al(WhereAlM1M3);
    AlM1M3(WhereAlM1M3) = zeros(size(WhereAlM1M3));
end

Mn_M3 = Mn;
Fe_M3 = Fe;
Al_M3 = zeros(size(Fe));
WhereFeMnSup1 = find(Fe+Mn >= 1);
WhereFeMnLow1 = find(Fe+Mn < 1);
if ~isempty(WhereFeMnSup1)
    Fe_M3(WhereFeMnSup1) = 1 - Mn_M3(WhereFeMnSup1);
end
if ~isempty(WhereFeMnLow1)
    Al_M3(WhereFeMnLow1) = 1 - Fe_M3(WhereFeMnLow1) - Mn_M3(WhereFeMnLow1);
end

Fe_M1 = Fe - Fe_M3;
Al_M1 = Al - Al_M2 - Al_M3;

Xmep = Mn_M3;
Xfep = Fe_M1;
Xep = Fe_M3-Xfep;
Xzo = 1-(Xep+Xfep-Xmep);

OutputData(WhereMin,:) = [Si,Ti,Al,Mg,Fe,Mn,Ca,SumCat,Al_M1,Al_M2,Fe_M1,Al_M3,Fe_M3,Mn_M3,Xep,Xfep,Xzo,Xmep];

end







