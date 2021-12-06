function [OutputData,OutputVariables] = StructFctCpx_Fe3(InputData,InputVariables,ElOxDataDef)
% -
% XMapTools External Function: structural formula of clinopyroxene 
%  
%  ++01.2021 Compatibility with XMapTools 4
%       - version without loop & Fe2O3 as possible input
%   
%   ---------------------------------------------
%   End-member              T1(2)   M1(1)   M2(1) 
%   ---------------------------------------------
%   Jadeite (Jd)            Si,Si   Al      Na
%   Diopside (Di)           Si,Si   Mg      Ca
%   Hedenbergite (Hd)       Si,Si   Fe2     Ca
%   Ca-Tschermak (Cats)     Si,Al   Al      Ca
%   Acmite (Acm)            Si,Si   Fe3     Na 
%   ---------------------------------------------  
%
% 6 Oxygen-basis 
%
% P. Lanari - Last update 26.01.2021
% Find out more at https://xmaptools.ch

OutputVariables = {'Si','Ti','Al','Mg','Fe2','Fe3','Mn','Ca','Na','SumCat','XMg','XFe3','Al_T1','Al_M1','SumM1','SumM2','Xjd','Xdi','Xhd','Xcats','Xacm','Xsum'};

OutputData = zeros(size(InputData,1),length(OutputVariables));

% General structural formula function for 6 oxygen FeO as Fe_total
WhereMin = find(sum(InputData,2) > 70);
[MatrixSF,ElementsList] = SF_OxNorm(InputData(WhereMin,:),InputVariables,6,ElOxDataDef);

% Fe3+ approximation (by PL; simple, wrong but efficient!)
Si = MatrixSF(:,1);
Al = MatrixSF(:,3);
Fe = MatrixSF(:,4);
Na = MatrixSF(:,9);

Al_T1 = zeros(size(Si));
Si_T1Def = 2-Si;
WhereAl = find(Si_T1Def > 0);
if ~isempty(WhereAl)
    Al_T1(WhereAl) = 2-Si(WhereAl);
end
Xcats = Al_T1 / 2;

Al_M1 = Al - Al_T1;
WhereAlM1neg = find(Al_M1 < 0);
if ~isempty(WhereAlM1neg)
    Al_M1(WhereAlM1neg) = zeros(size(WhereAlM1neg));
end

Diff23 = Na-(Al_M1 - Xcats);

Diff23Pos = find(Diff23 > 0);
Diff23Neg = find(Diff23 <= 0);

Fe3 = zeros(size(Si));
Fe2 = zeros(size(Si));


Fe2(Diff23Neg) = Fe(Diff23Neg);

Fe3(Diff23Pos) = Diff23(Diff23Pos);
Fe2 = Fe - Fe3;

XFe3 = Fe3./(Fe2+Fe3);

InputDataCorr = InputData;

InputDataCorr(WhereMin,4) = InputData(WhereMin,4) .* (1-XFe3);
InputDataCorr(WhereMin,5) = InputData(WhereMin,4) .* XFe3 .* (1/0.89992485);


% General structural formula function for 6 oxygen including FeO and Fe2O3
[MatrixSF2,ElementsList] = SF_OxNorm(InputDataCorr(WhereMin,:),InputVariables,6,ElOxDataDef);

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

XMg = Mg./(Mg+Fe);
XFe3 = Fe3./(Fe2+Fe3);

Al_T1 = zeros(size(XMg));
Si_T1Def = 2-Si;
WhereAl = find(Si_T1Def > 0);
if ~isempty(WhereAl)
    Al_T1(WhereAl) = 2-Si(WhereAl);
end

Al_M1 = Al - Al_T1;
WhereAlM1neg = find(Al_M1 < 0);
if ~isempty(WhereAlM1neg)
    Al_M1(WhereAlM1neg) = zeros(size(WhereAlM1neg));
end

SumM1 = Fe2 + Fe3 + Mg + Al_M1;
SumM2 = Na + Ca;

Xdi = Mg;
Xhd = Fe2;
Xcats = Al_T1 / 2;
Xacm = Fe3;
Xjd = Na-Xacm;

Xsum = Xjd+Xdi+Xhd+Xcats+Xacm;

OutputData(WhereMin,:) = [Si,Ti,Al,Mg,Fe2,Fe3,Mn,Ca,Na,SumCat,XMg,XFe3,Al_T1,Al_M1,SumM1,SumM2,Xjd,Xdi,Xhd,Xcats,Xacm,Xsum];

end







