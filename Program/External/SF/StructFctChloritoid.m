function [OutputData,OutputVariables] = StructFctChloritoid(InputData,InputVariables,ElOxDataDef)
% -
% XMapTools External Function: structural formula of chloritoid 
%  
%  ++01.2021 Compatibility with XMapTools 4
%       - version without loop & Fe2O3 as possible input
%   
%   -------------------------------------------------------------
%   End-member              T1(2)     T2(3)     M1(1)   M2(2)
%   -------------------------------------------------------------
%   Mg-chloritoid (Ctd)     Si,Si   Al,Al,Al	Al      Mg,Mg
%   Fe-chloritoid (fCtd)    Si,Si   Al,Al,Al	Al      Fe,Fe
%   Mn-chloritoid (mCtd)    Si,Si   Al,Al,Al	Al      Mn,Mn
%   ------------------------------------------------------------- 
%
% 12 Oxygen-basis 
%
% P. Lanari - Last update 20.01.2021
% Find out more at https://xmaptools.ch

OutputVariables = {'Si','Al','Fe','Mn','Mg','SumCat','XMg','Xctd','Xfctd','Xmctd'};

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

XMg = Mg./(Mg+Fe);

Xctd = Mg./(Mg+Fe+Mn);
Xfctd = Fe./(Mg+Fe+Mn);
Xmctd = Mn./(Mg+Fe+Mn);

OutputData(WhereMin,:) = [Si,Al,Fe,Mn,Mg,SumCat,XMg,Xctd,Xfctd,Xmctd];

end







