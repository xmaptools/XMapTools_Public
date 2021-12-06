function [OutputData,OutputVariables] = StructFctOpx(InputData,InputVariables,ElOxDataDef)
% -
% XMapTools External Function: structural formula of clinopyroxene 
%  
%  ++08.2021 Compatibility with XMapTools 4
%       - version without loop & Fe2O3 as possible input
%   
%   ----------------------------------------
%   End-member              T1(1)   M1(1)   
%   ----------------------------------------
%   Enstatite (En)       	Si,Si   Mg,Mg
%   Ferrosilite (Fs)       	Si,Si   Fe,Fe
%   Tschermak-px (Mgts)    	Si,Al   Al,Mg
%   ----------------------------------------  
%
% 3 Oxygen-basis 
%
% P. Lanari - Last update 20.08.2021
% Find out more at https://xmaptools.ch

OutputVariables = {'Si','Ti','Al','Mg','Fe','Mn','Ca','Na','SumCat','XMg','Al_T1','Al_M1','SumT1','SumM1','Xen','Xfs','Xmgts','Xsum'};

OutputData = zeros(size(InputData,1),length(OutputVariables));

% General structural formula function for 3 oxygen
WhereMin = find(sum(InputData,2) > 70);
[MatrixSF,ElementsList] = SF_OxNorm(InputData(WhereMin,:),InputVariables,3,ElOxDataDef);

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

Al_T1 = 1-Si;
WhereAlneg = find(Al_T1 < 0);
if ~isempty(WhereAlneg)
    Al_T1(WhereAlneg) = zeros(size(WhereAlneg));
end

Al_M1 = Al - Al_T1;
WhereAlM1neg = find(Al_M1 < 0);
if ~isempty(WhereAlM1neg)
    Al_M1(WhereAlM1neg) = zeros(size(WhereAlM1neg));
end

SumT1 = Si + Al;
SumM1 = Fe + Mg + Al_M1;

Xen = Mg;
Xfs = Fe;
Xmgts = Al_M1;

Xsum = Xen+Xfs+Xmgts;

OutputData(WhereMin,:) = [Si,Ti,Al,Mg,Fe,Mn,Ca,Na,SumCat,XMg,Al_T1,Al_M1,SumT1,SumM1,Xen,Xfs,Xmgts,Xsum];

end







