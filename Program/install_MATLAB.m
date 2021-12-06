function [] = install_MATLAB()
%

addpath([cd]);

addpath([cd,'/Core']);

addpath([cd,'/Dev']);
addpath([cd,'/Dev/help']);
addpath([cd,'/Dev/icons']);
addpath([cd,'/Dev/logo']);
addpath([cd,'/Dev/splash']);

addpath([cd,'/External']);
addpath([cd,'/External/SF']);
addpath([cd,'/External/TB']);
addpath([cd,'/External/ME']);

addpath([cd,'/Modules']);

addpath([cd,'/Addons']);
addpath([cd,'/Addons/XThermoTools']);
addpath([cd,'/Addons/XThermoTools/XTT_Dev/logo']);
addpath([cd,'/Addons/XThermoTools/XTT_Dev/img']);
addpath([cd,'/Addons/XThermoTools/XTT_Core']);
addpath([cd,'/Addons/XThermoTools/XTT_Databases']);
addpath([cd,'/Addons/XThermoTools/XTT_Dev']);
addpath([cd,'/Addons/XThermoTools/XTT_Functions']);
addpath([cd,'/Addons/XThermoTools/XTT_Modules']);

savepath

end