function [OutputData,OutputVariables] = StructFctFeldspar(InputData,InputVariables,ElOxDataDef)
% -
% XMapTools External Function: structural formula of feldspar 
%  
%  ++01.2021 Compatibility with XMapTools 4
%       - version without loop & Fe2O3 as possible input
%   
%   -----------------------------------------
%   End-member          T1(2)   T2(2)   M1(1) 
%   -----------------------------------------
%   Albite (Ab)         Si,Si   Si,Al	Na
%   Anorthite (An)      Si,Si   Al,Al	Ca
%   Sanidine (Sn)       Si,Si   Si,Al	K 
%   -----------------------------------------  
%
% 8 Oxygen-basis 
%
% P. Lanari - Last update 14.01.2021
% Find out more at https://xmaptools.ch

OutputVariables = {'Si','Al','Ca','Na','K','Sum_T','Sum_M','SumCat','Xab','Xan','Xsan'};

OutputData = zeros(size(InputData,1),length(OutputVariables));

% General structural formula function for 8 oxygen
WhereMin = find(sum(InputData,2) > 70);
[MatrixSF,ElementsList] = SF_OxNorm(InputData(WhereMin,:),InputVariables,8,ElOxDataDef);

Si = MatrixSF(:,1);
Ti = MatrixSF(:,2);
Al= MatrixSF(:,3);
Fe= MatrixSF(:,4)+MatrixSF(:,5);
Mn= MatrixSF(:,6);
Mg= MatrixSF(:,7);
Ca= MatrixSF(:,8);
Na= MatrixSF(:,9);
K= MatrixSF(:,10);

Sum_T = Si+Al;
Sum_M = Na+Ca+K;

SumCat = Si+Ti+Al+Fe+Mn+Mg+Ca+Na+K;

Xab = Na./(Na+Ca+K);
Xan = Ca./(Na+Ca+K);
Xsan = K./(Na+Ca+K);

OutputData(WhereMin,:) = [Si,Al,Ca,Na,K,Sum_T,Sum_M,SumCat,Xab,Xan,Xsan];

end







