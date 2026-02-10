function [] = CleanRepository()
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


clear all, clc, close all

if exist('XMapTools.prj')
    delete('XMapTools.prj');
end
if exist(fullfile(cd,'XMapTools'),'dir')
    rmdir(fullfile(cd,'XMapTools'), 's');
end
if exist(fullfile(cd,'XMapTools_resources'),'dir')
    rmdir(fullfile(cd,'XMapTools_resources'), 's');
end

disp('Completed – you are ready to compile')