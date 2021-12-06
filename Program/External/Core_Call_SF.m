function [OutputData,OutputVariables] = Core_Call_SF(Data,OxList,ExtFct,ElOxDataDef)
%

switch ExtFct
    
    case 'StructFctAluminosilicate'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = StructFctAluminosilicate(InputData,InputVariables,ElOxDataDef);
    
    case 'StructFctAmphiboleCa'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = StructFctAmphiboleCa(InputData,InputVariables,ElOxDataDef);
        
    case 'StructFctAmphiboleCa_Fe3'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = StructFctAmphiboleCa_Fe3(InputData,InputVariables,ElOxDataDef);
        
    case 'StructFctBiotite'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = StructFctBiotite(InputData,InputVariables,ElOxDataDef);
        
    case 'StructFctBrucite'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O','As2O3','Sb2O3','Cs2O'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = StructFctBrucite(InputData,InputVariables,ElOxDataDef);
    
    case 'StructFctChlorite'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = StructFctChlorite(InputData,InputVariables,ElOxDataDef);
        
    case 'StructFctChromite'
        InputVariables = {'Cr2O3','TiO2','Al2O3','FeO','Fe2O3','MgO','NiO','ZnO'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = StructFctChromite(InputData,InputVariables,ElOxDataDef);
        
    case 'StructFctChloritoid'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = StructFctChloritoid(InputData,InputVariables,ElOxDataDef);
    
    case 'StructFctCordierite'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = StructFctCordierite(InputData,InputVariables,ElOxDataDef);
        
    case 'StructFctCpx'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = StructFctCpx(InputData,InputVariables,ElOxDataDef);
     
    case 'StructFctCpx_Fe3'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = StructFctCpx_Fe3(InputData,InputVariables,ElOxDataDef);
    
    case 'StructFctEpidote'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = StructFctEpidote(InputData,InputVariables,ElOxDataDef);
            
    case 'StructFctFeldspar'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = StructFctFeldspar(InputData,InputVariables,ElOxDataDef);
        
    case 'StructFctMagnetite'
        InputVariables = {'FeO','Cr2O3','MgO','MnO','NiO','TiO2','SiO2','Al2O3','CaO','ZnO','Fe2O3'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = StructFctMagnetite(InputData,InputVariables,ElOxDataDef);
        
    case 'StructFctGarnet' 
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = StructFctGarnet(InputData,InputVariables,ElOxDataDef);
        
    case 'StructFctGarnet_Fe3' 
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = StructFctGarnet_Fe3(InputData,InputVariables,ElOxDataDef);
        
    case 'StructFctIlmenite' 
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = StructFctIlmenite(InputData,InputVariables,ElOxDataDef);    
    
    case 'StructFctOlivine'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O','Cr2O3','NiO'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = StructFctOlivine(InputData,InputVariables,ElOxDataDef);
        
    case 'StructFctOpx'   
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O'};   % add Cr and Ni???
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = StructFctOpx(InputData,InputVariables,ElOxDataDef);

    case 'StructFctRutile'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O','ZrO2'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = StructFctRutile(InputData,InputVariables,ElOxDataDef);
        
    case 'StructFctSerpentine'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O','Cr2O3','NiO'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = StructFctSerpentine(InputData,InputVariables,ElOxDataDef);
        
    case 'StructFctSpinel'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O','Cr2O3','NiO','ZnO'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = StructFctSpinel(InputData,InputVariables,ElOxDataDef);
        
    case 'StructFctStaurolite'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = StructFctStaurolite(InputData,InputVariables,ElOxDataDef);

    case 'StructFctWhiteMica'
        InputVariables = {'SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O'};
        InputData = GenerateInputData(Data,OxList,InputVariables);
        [OutputData,OutputVariables] = StructFctWhiteMica(InputData,InputVariables,ElOxDataDef);
        

        
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


