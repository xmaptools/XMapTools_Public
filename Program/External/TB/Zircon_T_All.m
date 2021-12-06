function [OutputData,OutputVariables] = Zircon_T_All(InputData,InputVariables,AddParameters,ElOxDataDef)
% -
% XMapTools External Function: zircon thermobarometry (all calibrations)
%
% List of output variables:
%       - T_W06     Watson et al. (2006)
%       - T_FW07    Ferry & Watson (2007), CMP 154:429–437
%  
% J. Laughton & P. Lanari - Last update 21.09.2021
% Find out more at https://xmaptools.ch

OutputVariables = {'T_W06','T_FW07','Ti_ppm'};
OutputData = zeros(size(InputData,1),length(OutputVariables));

%% Input
aTiO2 = AddParameters.Values(1);
aSiO2 = AddParameters.Values(1);

Idx = find(InputData(:,1) > 0);
TiO2 = InputData(Idx,1);
Ti_ppm = TiO2.*5995.08;

%% Watson et al. (2006)
T_W06 = 5080./(6.01-log10(Ti_ppm))-273.15;

%% Ferry & Watson (2007)
T_FW07 = 4800./(5.711-log10(Ti_ppm)-log10(aSiO2)+log10(aTiO2))-273.15;

%% Output
OutputData(Idx,:) = [T_W06,T_FW07,Ti_ppm];
end