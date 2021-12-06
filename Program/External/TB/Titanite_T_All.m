function [OutputData,OutputVariables] = Titanite_T_All(InputData,InputVariables,AddParameters,ElOxDataDef)
% -
% XMapTools External Function: titanite thermometry (all calibrations) 
% 
% List of output variables:
%       - T_H08   Hayden et al. (2008)
%
% J. Laughton & P. Lanari - Last update 21.09.2021
% Find out more at https://xmaptools.ch

OutputVariables = {'T_H08','Zr_ppm'};
OutputData = zeros(size(InputData,1),length(OutputVariables));

%% Input
P_kbar = AddParameters.Values(1);
aTiO2 = AddParameters.Values(1);
aSiO2 = AddParameters.Values(1);

Idx = find(InputData(:,1) > 0);
ZrO2 = InputData(Idx,1);
Zr_ppm = ZrO2.*7403.09;

%% Hayden et al. (2008)
T_H08 = (7708+960.*P_kbar.*0.1)./(10.52-log10(aTiO2)-log10(aSiO2)-log10(Zr_ppm))-273.15;
                
%% Output
OutputData(Idx,:) = [T_H08,Zr_ppm];
end