function [OutputData,OutputVariables] = StructFctIlmenite(InputData,InputVariables,ElOxDataDef)
% -
% XMapTools External Function: structural formula of ilmenite 
%  
%  ++09.2022 
%       - Approximation of Fe3+ and Fe2+; works fine with Ca-Na-K ignored
%  ++08.2021 Compatibility with XMapTools 4
%       - version without loop & Fe2O3 as possible input
%   
%   ------------------------------------
%   End-member                   
%   ------------------------------------
%   Ilmenite (Ilm)       Fe(1)Ti(1)O(3)
%   Mn-Ilmenite (mIlm)   Mn(1)Ti(1)O(3)
%   Hematite (Hem)       Fe(2)O(3)
%   ------------------------------------
%
% 3 Oxygen-basis
%
% P. Lanari - Last update 02.09.2021
% Find out more at https://xmaptools.ch

OutputVariables = {'Ti','Fe2','Mn','Fe3','Sum_2','Sum_34','Sum_NaCaK','SumCat','Xilm','Xmilm','Xhem','Xsum'};

OutputData = zeros(size(InputData,1),length(OutputVariables));

% General structural formula function for 3 oxygen
WhereMin = find(sum(InputData,2) > 50);
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

% Fe3+ approximation (by PL; simple, wrong but efficient!)
ExcessFe = (Fe+Mn+Mg)-1;
WhereNeg = find(ExcessFe < 0);
ExcessFe(WhereNeg) = zeros(size(WhereNeg));

Fe3 = ExcessFe.*0.89992485;
Fe2 = Fe-Fe3;

XFe3 = Fe3./(Fe2+Fe3);

InputDataCorr = InputData;

InputDataCorr(WhereMin,4) = InputData(WhereMin,4) .* (1-XFe3);
InputDataCorr(WhereMin,5) = InputData(WhereMin,4) .* XFe3 .* (1/0.89992485);

% General structural formula function for 3 oxygen including FeO and Fe2O3
[MatrixSF2,ElementsList] = SF_OxNorm(InputDataCorr(WhereMin,:),InputVariables,3,ElOxDataDef);

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

Sum_2 = Fe2+Mg+Mn;
Sum_34 = Ti+Si+Fe3;
Sum_NaCaK = Ca+Na+K;

SumCat = Si+Ti+Al+Fe2+Fe3+Mn+Mg+Ca+Na+K;

Xilm_Temp = Ti;
Xhem = Fe3;
Xilm = Fe2./(Fe2+Mn) .* Xilm_Temp;
Xmilm = Mn./(Fe2+Mn) .* Xilm_Temp;

Xsum = Xhem+Xilm+Xmilm;

OutputData(WhereMin,:) = [Ti,Fe2,Mn,Fe3,Sum_2,Sum_34,Sum_NaCaK,SumCat,Xilm,Xmilm,Xhem,Xsum];

end







