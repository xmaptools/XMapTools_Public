function [IsHSat] = SaturationCheck4H(WorkVariMod)
%
% Updated 10.09.2019 (PL) - Loop added for cases with MELT + AQ_fluid
%


IsHSat = 0;

if WorkVariMod.FluidPhases
    for i = 1:WorkVariMod.FluidPhases
        WhereElem = find(WorkVariMod.FluidCOMP(i,:));

        FluidElems = cell2mat(WorkVariMod.Els(WhereElem));
        FluidComp = WorkVariMod.FluidCOMP(WhereElem);

        % Here we avoid the problems with H alone...
        if isequal(FluidElems,'OH') || isequal(FluidElems,'HO')
            IsHSat = 1;
        end
    end
end

return