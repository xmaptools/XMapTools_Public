% This script reset the configuration file config-xmaptools.mat
clear all
clc
close all

config.xmaptools.setup_path = '';
config.xmaptools.last_path = '';
config.xthermotools.theriak_path = '';

save('config_xmaptools.mat','config');

clear all
disp('done')