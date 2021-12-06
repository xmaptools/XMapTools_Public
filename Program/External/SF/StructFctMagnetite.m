function [OutputData,OutputVariables] = StructFctMagnetite(InputData,InputVariables,ElOxDataDef)
% -
% XMapTools External Function: structural formula of magnetite 
%  
%  ++09.2021 Compatibility with XMapTools 4
%       - version without loop & Fe2O3 as possible input
%   
%
% Normalized to 3 cations (assuming 4 oxygen)
%
% P. Lanari & F. Piccoli - Last update 16.09.2021
% Find out more at https://xmaptools.ch

OutputVariables = {'Fe2','Fe3','Cr','Mg','Mn','Ni','Ti','Si','Al','Ca','Zn','O','XFe3','SumCat'};

OutputData = zeros(size(InputData,1),length(OutputVariables));

% General structural formula function for 3 cations
WhereMin = find(sum(InputData,2) > 70);
[NormCat,ElementsList] = SF_CatNorm(InputData(WhereMin,:),InputVariables,3,ElOxDataDef);

Charge = sum(NormCat.*[2,3,2,2,2,4,4,3,2,2,3,0],2);   % 0 is for Oxygen
Diff = 2*4-Charge;

Where = find(Diff > 0);

AtomicUnit = NormCat;

AtomicUnit(Where,end-1) =AtomicUnit(Where,end-1) + Diff(Where);     % Fe3
AtomicUnit(Where,end) =AtomicUnit(Where,end) + Diff(Where)/2;       % O
AtomicUnit(Where,1) =AtomicUnit(Where,1) - Diff(Where);           % Fe2

Fe2 = AtomicUnit(:,1);
Fe3 = AtomicUnit(:,end-1);
Cr = AtomicUnit(:,2);
Mg = AtomicUnit(:,3);
Mn = AtomicUnit(:,4);
Ni = AtomicUnit(:,5);
Ti = AtomicUnit(:,6);
Si = AtomicUnit(:,7);
Al = AtomicUnit(:,8);
Ca = AtomicUnit(:,9);
Zn =AtomicUnit(:,10);
O = AtomicUnit(:,end);
XFe3 = Fe3./(Fe2+Fe3);
SumCat = Fe2+Fe3+Cr+Mg+Mn+Ni+Ti+Si+Al+Ca+Zn+O;

OutputData(WhereMin,:) = [Fe2,Fe3,Cr,Mg,Mn,Ni,Ti,Si,Al,Ca,Zn,O,XFe3,SumCat];

end







