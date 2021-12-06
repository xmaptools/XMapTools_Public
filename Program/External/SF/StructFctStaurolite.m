function [OutputData,OutputVariables] = StructFctStaurolite(InputData,InputVariables,ElOxDataDef)
% -
% XMapTools External Function: structural formula of staurolite 
%  
%  ++08.2021 Compatibility with XMapTools 4
%       - version without loop & Fe2O3 as possible input
%   
%   ------------------------------------------------------------------
%   End-member                  Z(11)        Y(2)       X(4)
%   ------------------------------------------------------------------
%   Staurolite (St)             Si(4)Al(7)   Al,Al      Fe,Fe,Fe,Fe
%   Mg-Staurolite (Mst)         Si(4)Al(7)   Al,Al      Mg,Mg,Mg,Mg          
%   Mn-Staurolite (MnSt)        Si(4)Al(7)   Al,Al      Mn,Mn,Mn,Mn
%   ------------------------------------------------------------------ 
%
% 24 Oxygen-basis 
%
% P. Lanari - Last update 28.10.2021
% Find out more at https://xmaptools.ch

OutputVariables = {'Si','Al','Fe','Mn','Mg','XMg','SumCat','Xst','Xmst','Xmnst'};

OutputData = zeros(size(InputData,1),length(OutputVariables));

% General structural formula function for 24 oxygen
WhereMin = find(sum(InputData,2) > 50);
[MatrixSF,ElementsList] = SF_OxNorm(InputData(WhereMin,:),InputVariables,24,ElOxDataDef);

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

Xmst = Mg./(Mg+Fe+Mn);
Xst = Fe./(Mg+Fe+Mn);
Xmnst = Mn./(Mg+Fe+Mn);

OutputData(WhereMin,:) = [Si,Al,Fe,Mn,Mg,XMg,SumCat,Xst,Xmst,Xmnst];

end







