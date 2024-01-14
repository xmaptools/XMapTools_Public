% This script reset the configuration file config-xmaptools.mat
%
% XMapTools is a free software solution for the analysis of chemical maps
% Copyright © 2022-2024 University of Bern, Institute of Geological Sciences, Pierre Lanari
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

clear all
clc
close all

% XMapTools
config.xmaptools.setup_path = '';
config.xmaptools.last_path = '';

% config.xthermotools.theriak_path = '';

% Bingo-Antidote:
config.bingoantidote.theriak_path = '';
config.bingoantidote.setup_path = '';


save('config_xmaptools.mat','config');

clear all
disp('Completed – configuration has been reset')