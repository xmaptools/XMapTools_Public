function [OutputData,OutputVariables] = Core_Call_TB(Data,OxList,ExtFct,AddParameters,ElOxDataDef)
%
% XMapTools is a free software solution for the analysis of chemical maps
% Copyright © 2022-2025 University of Bern, Institute of Geological Sciences, Pierre Lanari
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

switch ExtFct
    
    case 'Amphibole_T_All'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = Amphibole_T_All(InputData,InputVariables,AddParameters,ElOxDataDef);
        
    case 'Biotite_T_All'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = Biotite_T_All(InputData,InputVariables,AddParameters,ElOxDataDef);
        
    case 'Chlorite_T_All'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = Chlorite_T_All(InputData,InputVariables,AddParameters,ElOxDataDef);
        
    case 'Quartz_T_All'
        InputVariables = {'TiO2'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = Quartz_T_All(InputData,InputVariables,AddParameters,ElOxDataDef);
        
    case 'Monazite_Age_M96'
        InputVariables = {'ThO2','UO2','PbO'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = Monazite_Age_M96(InputData,InputVariables,AddParameters,ElOxDataDef);
        
    case 'Phengite_P_All'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = Phengite_P_All(InputData,InputVariables,AddParameters,ElOxDataDef);
        
    case 'Rutile_T_All'
        InputVariables = {'ZrO2'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = Rutile_T_All(InputData,InputVariables,AddParameters,ElOxDataDef);
        
    case 'Titanite_T_All'
        InputVariables = {'ZrO2'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = Titanite_T_All(InputData,InputVariables,AddParameters,ElOxDataDef);
        
    case 'Zircon_T_All'
        InputVariables = {'TiO2'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = Zircon_T_All(InputData,InputVariables,AddParameters,ElOxDataDef);
        
end



end


function [InputData] = GenerateInputData(Data,OxList,InputVariables)
% Internal subroutine that re-organize the data to fit the Input format
% of the selected external function
%
% Pierre Lanari (last edit 14.01.21)

InputData = zeros(size(Data,1),length(InputVariables));

[IsEl,IndEl] = ismember(InputVariables,OxList);

for i = 1:length(IsEl)
    if IsEl(i)
        InputData(:,i) = Data(:,IndEl(i));
    end
end

end


