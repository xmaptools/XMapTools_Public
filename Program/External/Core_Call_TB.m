function [OutputData,OutputVariables] = Core_Call_TB(Data,OxList,ExtFct,AddParameters,ElOxDataDef)
%

switch ExtFct
    
    case 'Amphibole_T_All'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = Amphibole_T_All(InputData,InputVariables,AddParameters,ElOxDataDef);
        
    case 'Biotite_T_H05'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = Biotite_T_H05(InputData,InputVariables,AddParameters,ElOxDataDef);
        
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


