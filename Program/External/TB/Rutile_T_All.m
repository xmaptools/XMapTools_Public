function [OutputData,OutputVariables] = Rutile_T_All(InputData,InputVariables,AddParameters,ElOxDataDef)
% -
% XMapTools External Function: rutile thermobarometry (all calibrations)
%
% List of input variables:
%       - ZrO2 (wt%)
%
% List of output variables:
%       - T_Z04     Zack et al. (2004)
%       - T_W06     Watson et al. (2006)
%       - T_FW07    Ferry & Watson (2007), CMP 154:429–437
%       - T_T07     Tomkins et al. (2007)
%  
% P. Lanari, J. Laughton & C. Martin - Last update 21.09.2021
% Find out more at https://xmaptools.ch

OutputVariables = {'T_Z04','T_W06','T_FW07','T_T07a','T_T07b','T_T07c','Zr_ppm'};
OutputData = zeros(size(InputData,1),length(OutputVariables));

%% Input
P_kbar = AddParameters.Values(1);
aSiO2 = AddParameters.Values(1);

Idx = find(InputData(:,1) > 0);
ZrO2 = InputData(Idx,1);
Zr_ppm = ZrO2./0.0001350000;    % Conversion from ZrO2 (wt%) to Zr (ug/g).

%% Zack et al. (2004)
T_Z04 = 127.8.*log(Zr_ppm)-10;

%% Watson et al. (2006)
T_W06 = 4470./(7.36-log10(Zr_ppm))-273.15;

%% Ferry & Watson (2007)
T_FW07 = -4530./(log10(Zr_ppm)-7.42-log10(aSiO2)) - 273.15;

%% Tomkins et al. (2007)
T_T07a = (83.9+0.410.*P_kbar)./(0.1428-0.0083144.*log(Zr_ppm))-273.15;
T_T07b = (85.7+0.473.*P_kbar)./(0.1453-0.0083144.*log(Zr_ppm))-273.15;
T_T07c = (88.1+0.206.*P_kbar)./(0.1412-0.0083144.*log(Zr_ppm))-273.15;

%% Output
OutputData(Idx,:) = [T_Z04,T_W06,T_FW07,T_T07a,T_T07b,T_T07c,Zr_ppm];
end