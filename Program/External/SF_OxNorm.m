function [MatrixSF,ElementsList] = SF_OxNorm(MatrixOxide,OxList,OxBasis,ElOxDataDef)
%
% XMapTools is a free software solution for the analysis of chemical maps
% Copyright © 2022-2023 University of Bern, Institute of Geological Sciences, Pierre Lanari
%
% XMapTools is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or any 
% later version.
%
% XMapTools is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with XMapTools. If not, see https://www.gnu.org/licenses.

ListOx = ElOxDataDef.OxList;
ListEl = ElOxDataDef.ElList(ElOxDataDef.OxElIdx);

NbEl = ElOxDataDef.OxNbCat;
NbOx = ElOxDataDef.OxNbOx;

MolarW = ElOxDataDef.OxMass;

if ~iscell(OxList)
    Ox = strread(OxList,'%s');
else
    Ox = OxList;
end

[lia,loc] = ismember(Ox,ListOx);

% added in 4.2 (12.05.2023) to filter non-oxides/elements
IdxOk = find(lia);
Ox = Ox(IdxOk); 
MatrixOxide = MatrixOxide(:,IdxOk);
loc = loc(IdxOk);
% --

NbCalc = size(MatrixOxide,1);
NbElem = size(MatrixOxide,2);

Num = repmat(NbEl(loc),NbCalc,1);
NumO = repmat(NbOx(loc),NbCalc,1);
Cst = repmat(MolarW(loc),NbCalc,1);

AtomicPer = MatrixOxide./Cst.*Num; 

TheSum = sum((AtomicPer .* NumO) ./ Num,2);
RefOx = repmat(TheSum/OxBasis,1,NbElem);

MatrixSF = AtomicPer ./ RefOx;

ElementsList = ListEl(loc);

end