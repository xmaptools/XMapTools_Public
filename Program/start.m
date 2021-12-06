function [] = start(Input)
%
clc
close all


switch Input
    case 'XMapTools'
        open XMapTools.mlapp
        
    case 'all'
        open XMapTools.mlapp
        open /Modules/Tools_ImportMaps.mlapp
        open /Modules/Tools_ConvertData.mlapp
end

end