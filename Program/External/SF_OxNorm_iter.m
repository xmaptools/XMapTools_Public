function [MatrixSF,ElementsList] = SF_OxNorm_iter(MatrixOxide,OxList,OxBasis,ElOxDataDef, corr_element_idx, corr_element_factor)
%
% XMapTools is a free software solution for the analysis of chemical maps
% Copyright © 2022-2026 University of Bern, Institute of Geological Sciences, Pierre Lanari
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

% Define initial guess for Ox_corr_guess
initial_Ox_corr_guess = 0.01;

% define solver options
options = optimset('Display', 'none');

Ox_corr = zeros(size(TheSum)); % Initialize vector for results
for i = 1:length(Ox_corr)
    % Solve for each element separately
    Ox_corr(i) = fsolve(@(Ox_corr_guess) ...
        Ox_guess_diff(AtomicPer(i,:), TheSum(i), OxBasis, Ox_corr_guess, NbElem, corr_element_idx, corr_element_factor), ...
        initial_Ox_corr_guess, options);
end

% RefOx = repmat(TheSum/(OxBasis+Ox_corr),1,NbElem);
RefOx = repmat(TheSum./(Ox_corr+OxBasis),1,NbElem);
MatrixSF = AtomicPer ./ RefOx;

ElementsList = ListEl(loc);

end


function [difference] = Ox_guess_diff(AtomicPer, TheSum, OxBasis, Ox_corr_guess, NbElem, corr_element_idx, corr_element_factor)

RefOx = repmat(TheSum./(Ox_corr_guess+OxBasis),1,NbElem);
VecSF = AtomicPer ./ RefOx;

% read element for correction and multiply with a correction factor
Vec_Ox_corr =  corr_element_factor .* VecSF(corr_element_idx);

difference = Vec_Ox_corr - Ox_corr_guess;

end
