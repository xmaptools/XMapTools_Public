function [] = start(Input)
%
% XMapTools is a free software solution for the analysis of chemical maps
% Copyright © 2022-2026 University of Lausanne, Institute of Earth Sciences, Pierre Lanari
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