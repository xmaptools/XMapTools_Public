function [OutputData,OutputVariables] = Core_Call_ME(Data,OxList,ExtFct,AddParameters,ElOxDataDef)
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
%
% P. Lanari & J. Laughton - Last update 7.06.2021

switch ExtFct
    
    case 'GrtBt_T_All'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O'};
        InputData = GenerateInputDataME(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = GrtBt_T_All(InputData,InputVariables,AddParameters,ElOxDataDef);
        
    case 'GrtChl_T_All'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O'};
        InputData = GenerateInputDataME(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = GrtChl_T_All(InputData,InputVariables,AddParameters,ElOxDataDef);
        
    case 'GrtCpx_T_All'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O'};
        InputData = GenerateInputDataME(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = GrtCpx_T_All(InputData,InputVariables,AddParameters,ElOxDataDef);
        
    case 'GrtCrd_T_All'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O'};
        InputData = GenerateInputDataME(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = GrtCrd_T_All(InputData,InputVariables,AddParameters,ElOxDataDef);
        
    case 'GrtHbl_T_All'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O'};
        InputData = GenerateInputDataME(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = GrtHbl_T_All(InputData,InputVariables,ElOxDataDef);
        
    case 'GrtIlm_T_All'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O'};
        InputData = GenerateInputDataME(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = GrtIlm_T_All(InputData,InputVariables,ElOxDataDef);
        
    case 'GrtOpx_T_All'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O'};
        InputData = GenerateInputDataME(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = GrtOpx_T_All(InputData,InputVariables,AddParameters,ElOxDataDef);
        
    case 'GrtPh_T_All'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O'};
        InputData = GenerateInputDataME(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = GrtPh_T_All(InputData,InputVariables,AddParameters,ElOxDataDef);
        
end



end


function [InputData] = GenerateInputDataME(Data,OxList,InputVariables)
% Internal subroutine that re-organize the data to fit the Input format
% of the selected external function
%
% Pierre Lanari (last edit 16.04.21)

[IsEl,IndEl] = ismember(InputVariables,OxList);

Idx = find(IsEl);
IdxIdx = IndEl(Idx);

for i = 1:length(Data)
    InputData(i).Data = zeros(size(Data(i).Data,1),length(InputVariables));
    InputData(i).Mean = zeros(size(Data(i).Mean,1),length(InputVariables));
    InputData(i).Std = zeros(size(Data(i).Std,1),length(InputVariables));
    InputData(i).MC = zeros(size(Data(i).MC,1),length(InputVariables));
    
    InputData(i).Data(:,Idx) = Data(i).Data(:,IdxIdx);
    InputData(i).Mean(:,Idx) = Data(i).Mean(:,IdxIdx);
    InputData(i).Std(:,Idx) = Data(i).Std(:,IdxIdx);
    InputData(i).MC(:,Idx) = Data(i).MC(:,IdxIdx);
end

end


