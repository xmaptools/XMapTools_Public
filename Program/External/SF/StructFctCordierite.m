function [OutputData,OutputVariables] = StructFctCordierite(InputData,InputVariables,ElOxDataDef)
% -
% XMapTools External Function: structural formula of cordierite 
%  
%  ++08.2021 Compatibility with XMapTools 4
%       - version without loop & Fe2O3 as possible input
%   
%   -------------------------------------------------------------
%   End-member                   T1(2)         T2(3)      M1(2)
%   -------------------------------------------------------------
%   Mg-Cordierite (Crd)     Si,Si,Si,Si,Si	Al,Al,Al,Al   Mg,Mg
%   Fe-Cordierite (fCrd)    Si,Si,Si,Si,Si	Al,Al,Al,Al   Fe,Fe
%   Mn-Cordierite (mCrd)    Si,Si,Si,Si,Si	Al,Al,Al,Al   Mn,Mn
%   ------------------------------------------------------------- 
%
% 18 Oxygen-basis 
%
% P. Lanari - Last update 13.08.2021
% Find out more at https://xmaptools.ch

OutputVariables = {'Si','Al','Fe','Mn','Mg','Na','K','SumTet','SumOct','SumChannel','SumCat','XMg','Xcrd','Xfcrd','Xmcrd'};

OutputData = zeros(size(InputData,1),length(OutputVariables));

% General structural formula function for 18 oxygen
WhereMin = find(sum(InputData,2) > 50);
[MatrixSF,ElementsList] = SF_OxNorm(InputData(WhereMin,:),InputVariables,18,ElOxDataDef);

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
SumTet = Si+Ti+Al;
SumOct = Fe+Mn+Mg;
SumChannel = Na+K;

XMg = Mg./(Mg+Fe);

Xcrd = Mg./(Mg+Fe+Mn);
Xfcrd = Fe./(Mg+Fe+Mn);
Xmcrd = Mn./(Mg+Fe+Mn);

OutputData(WhereMin,:) = [Si,Al,Fe,Mn,Mg,Na,K,SumTet,SumOct,SumChannel,SumCat,XMg,Xcrd,Xfcrd,Xmcrd];

end







