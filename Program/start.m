function [] = start(Input)
%
clc
close all


switch Input
    case 'XMapTools'
        open XMapTools
        
    case 'all'
        open XMapTools.mlapp
        open Tools_ImportMaps.mlapp
        open Tools_ConvertData.mlapp
        
    case 'laser'
        open XMapTools
        open Converter_LAICPMS
end

end