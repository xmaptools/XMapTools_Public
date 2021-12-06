function [OutputData,OutputVariables] = StructFctCpx(InputData,InputVariables,ElOxDataDef)
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
%   Hedenbergite (Hd)       Si,Si   Fe      Ca
%   Ca-Tschermak (Cats)     Si,Al   Al      Ca
%   ---------------------------------------------  
%
% 6 Oxygen-basis 
%
% P. Lanari - Last update 26.01.2021
% Find out more at https://xmaptools.ch

OutputVariables = {'Si','Ti','Al','Mg','Fe','Mn','Ca','Na','SumCat','XMg','Al_T1','Al_M1','SumM1','SumM2','Xjd','Xdi','Xhd','Xcats','Xsum'};

OutputData = zeros(size(InputData,1),length(OutputVariables));

% General structural formula function for 6 oxygen
WhereMin = find(sum(InputData,2) > 70);
[MatrixSF,ElementsList] = SF_OxNorm(InputData(WhereMin,:),InputVariables,6,ElOxDataDef);

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

XMg = Mg./(Mg+Fe);

Al_T1 = zeros(size(XMg));
Si_T1Def = 2-Si;
WhereAl = find(Si_T1Def > 0);
Al_T1(WhereAl) = 2-Si(WhereAl);

Al_M1 = Al - Al_T1;
WhereAlM1neg = find(Al_M1 < 0);
if ~isempty(WhereAlM1neg)
    Al_M1(WhereAlM1neg) = zeros(size(WhereAlM1neg));
end

SumM1 = Fe + Mg + Al_M1;
SumM2 = Na + Ca;

Xjd = Na;
Xdi = Mg;
Xhd = Fe;
Xcats = Al_T1 / 2;

Xsum = Xjd+Xdi+Xhd+Xcats;

OutputData(WhereMin,:) = [Si,Ti,Al,Mg,Fe,Mn,Ca,Na,SumCat,XMg,Al_T1,Al_M1,SumM1,SumM2,Xjd,Xdi,Xhd,Xcats,Xsum];

end







