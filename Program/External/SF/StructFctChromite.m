function [OutputData,OutputVariables] = StructFctChromite(InputData,InputVariables,ElOxDataDef)
% -
% XMapTools External Function: structural formula of chromite 
%  
%
%  ++09.2021 Compatibility with XMapTools 4
%       - version without loop & Fe2O3 as possible input
%       - Fe3+ estimated using Droop (1987)
%   
%   ----------------------------------------------
%   End-member                  X(1) Y(2)
%   ----------------------------------------------
%
%   ----------------------------------------------
%
%
% 32 Oxygen-basis
%
% A. Hauteville & P. Lanari - Last update 28.10.2021  *** NOT TESTED IN THIS VERSION ***
% Find out more at https://xmaptools.ch

OutputVariables = {'Fe2','Mg','Cr','Al','Fe3','Ti','Ni','Zn','SumCat'};

OutputData = zeros(size(InputData,1),length(OutputVariables));

% General structural formula function for 32 oxygen
WhereMin = find(sum(InputData,2) > 50);
[MatrixSF,ElementsList] = SF_OxNorm(InputData(WhereMin,:),InputVariables,32,ElOxDataDef);

% Fe3+ approximation (Droop 1987)
Fe = MatrixSF(:,4)+MatrixSF(:,5);

Fe3_Droop = 2*32*(1-24./sum(MatrixSF,2));
ZeroFe3 = find(Fe3_Droop < 0);
Fe3_Droop(ZeroFe3) = zeros(size(ZeroFe3));
XFe3 = Fe3_Droop./Fe;
OneXFe3 = find(XFe3 > 1);
XFe3(OneXFe3) = ones(size(OneXFe3));

InputDataCorr = InputData; 

InputDataCorr(WhereMin,4) = InputData(WhereMin,4) .* (1-XFe3);
InputDataCorr(WhereMin,5) = InputData(WhereMin,4) .* XFe3 .* (1/0.89992485);


% General structural formula function for 12 oxygen including FeO and Fe2O3
[MatrixSF2,ElementsList] = SF_OxNorm(InputDataCorr(WhereMin,:),InputVariables,32,ElOxDataDef);

Cr = MatrixSF2(:,1);
Ti = MatrixSF2(:,2);
Al = MatrixSF2(:,3);
Fe2 = MatrixSF2(:,4);
Fe3 = MatrixSF2(:,5);
Mg = MatrixSF2(:,6);
Ni = MatrixSF2(:,7);
Zn = MatrixSF2(:,8);

SumCat = Cr+Ti+Al+Fe2+Fe3+Mg+Ni+Zn;

OutputData(WhereMin,:) = [Fe2,Mg,Cr,Al,Fe3,Ti,Ni,Zn,SumCat];

end







