function [OutputData,OutputVariables] = Quartz_T_All(InputData,InputVariables,AddParameters,ElOxDataDef)
% -
% XMapTools External Function: quartz thermobarometry (all calibrations)
%
% List of output variables:
%       - T_WW06    Wark & Watson (2006)
%       - T_KO08    Kawasaki & Osanai (2008)
%       - T_T10     Thomas et al. (2010)
%       - T_HA12    Huang & Audetat (2012)
%  
% P. Lanari, J. Laughton & Regiane Andrade Fumes - Last update 21.09.2021
% Find out more at https://xmaptools.ch

OutputVariables = {'T_WW06','T_T10','T_HA12','Ti_ppm'};
OutputData = zeros(size(InputData,1),length(OutputVariables));

%% Input
P_kbar = AddParameters.Values(1);
aTiO2 = AddParameters.Values(1);

Idx = find(InputData(:,1) > 0);
TiO2 = InputData(Idx,1);
Ti_ppm = TiO2.*5995.08;
Ti_cpfu = (Ti_ppm./(10000.*0.599.*79.87))./((Ti_ppm./(10000.*0.599.*79.87))+...
                ((100-Ti_ppm./(10000.*0.599.*79.87)).*1./60.09));

%% Wark & Watson (2006)
T_WW06 = -3765./(log10(Ti_ppm)-5.69)-273.15;

%% Kawasaki & Osanai (2008)
% T_KO08 = -5895./(log(Ti_cpfu)+1.729)-273.15;  Needs to be corrected   

%% Thomas et al. (2010)
T_T10 = (60952+1741.*P_kbar)./(1.52-8.3145.*log(Ti_cpfu)+8.3145.*log(aTiO2))-273.15;

%% Huang & Audetat (2012)
T_HA12 = (2794.3+660.*P_kbar.^0.35)./(5.6459-log10(Ti_ppm))-273.15;

%% Output
OutputData(Idx,:) = [T_WW06,T_T10,T_HA12,Ti_ppm];
end