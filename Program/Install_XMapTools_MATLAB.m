function [] = Install_XMapTools_MATLAB()
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

Where = which('XMapTools');
Where = Where(1:end-16);

addpath(Where);

addpath([Where,'/Core']);

addpath([Where,'/Dev']);
addpath([Where,'/Dev/Data_Std_LAICPMS']);
addpath([Where,'/Dev/help']);
addpath([Where,'/Dev/help/img']);
addpath([Where,'/Dev/icons']);
addpath([Where,'/Dev/logo']);
addpath([Where,'/Dev/splash']);

addpath([Where,'/External']);
addpath([Where,'/External/ME']);
addpath([Where,'/External/SF']);
addpath([Where,'/External/TB']);

addpath([Where,'/Modules']);

addpath([Where,'/Addons']);
% addpath([Where,'/Addons/XThermoTools']);
% addpath([Where,'/Addons/XThermoTools/XTT_Dev/logo']);
% addpath([Where,'/Addons/XThermoTools/XTT_Dev/img']);
% addpath([Where,'/Addons/XThermoTools/XTT_Core']);
% addpath([Where,'/Addons/XThermoTools/XTT_Databases']);
% addpath([Where,'/Addons/XThermoTools/XTT_Dev']);
% addpath([Where,'/Addons/XThermoTools/XTT_Functions']);
% addpath([Where,'/Addons/XThermoTools/XTT_Modules']);

% Bingo Antidote (2022.1)
addpath([Where,'/Addons/BingoAntidote']);
addpath([Where,'/Addons/BingoAntidote/Dev']);
addpath([Where,'/Addons/BingoAntidote/Dev/icons']);
addpath([Where,'/Addons/BingoAntidote/Dev/media']);
addpath([Where,'/Addons/BingoAntidote/Core']);
addpath([Where,'/Addons/BingoAntidote/Databases']);
addpath([Where,'/Addons/BingoAntidote/Functions']);


savepath

% Added on 21.02.2025 for MATLAB installation
RESET_CONFIG;

disp('Installation completed')

end