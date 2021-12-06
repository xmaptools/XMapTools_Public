function [OutputData,OutputVariables] = GrtIlm_T_All(InputData,InputVariables,ElOxDataDef)
% -
% XMapTools External Function: Garnet-Ilmenite thermometry (all calibrations) 
% 
% List of output variables:
%       - T_P87     Pownceby et al. (1987)                   
%       - T_P91     Pownceby et al. (1991)            
%       - T_M10     Martin et al. (2010)    
%
% 12 Oxygen-basis for Garnet
% 3 Oxygen-basis for Ilmenite
%
% J. Laughton - Last update 02.09.2021  *** NOT TESTED ***
% Find out more at https://xmaptools.ch

OutputVariables = {'T_P87','T_P91','T_M10'};

%% Garnet general structural formula (12 oxygen)
InputData_Mean_Grt = InputData(1).Mean;
InputData_MC_Grt = InputData(1).MC;
InputData_All_Grt = [InputData_Mean_Grt;InputData_MC_Grt];
[MatrixSF,ElementsList] = SF_OxNorm(InputData_All_Grt,InputVariables,12,ElOxDataDef);   

Si_Grt = MatrixSF(:,1);
Ti_Grt = MatrixSF(:,2);
Al_Grt = MatrixSF(:,3);
Fe_Grt = MatrixSF(:,4)+MatrixSF(:,5);
Mn_Grt = MatrixSF(:,6);
Mg_Grt = MatrixSF(:,7);
Ca_Grt = MatrixSF(:,8);
Na_Grt = MatrixSF(:,9);
K_Grt = MatrixSF(:,10);

%% Ilmenite general structural formula (3 oxygen)
InputData_Mean_Ilm = InputData(2).Mean;
InputData_MC_Ilm = InputData(2).MC;
InputData_All_Ilm = [InputData_Mean_Ilm;InputData_MC_Ilm];
[MatrixSF,ElementsList] = SF_OxNorm(InputData_All_Ilm,InputVariables,3,ElOxDataDef);

Si_Ilm = MatrixSF(:,1);
Ti_Ilm = MatrixSF(:,2);
Al_Ilm = MatrixSF(:,3);
Fe_Ilm = MatrixSF(:,4)+MatrixSF(:,5);
Mn_Ilm = MatrixSF(:,6);
Mg_Ilm = MatrixSF(:,7);
Ca_Ilm = MatrixSF(:,8);
Na_Ilm = MatrixSF(:,9);
K_Ilm = MatrixSF(:,10);

%% Variables
XFe_Grt = Fe_Grt./(Fe_Grt+Mg_Grt+Ca_Grt+Mn_Grt);
XMg_Grt = Mg_Grt./(Fe_Grt+Mg_Grt+Ca_Grt+Mn_Grt);
XCa_Grt = Ca_Grt./(Fe_Grt+Mg_Grt+Ca_Grt+Mn_Grt);
XMn_Grt = Mn_Grt./(Fe_Grt+Mg_Grt+Ca_Grt+Mn_Grt);

XFe_Ilm = Fe_Ilm./(Fe_Ilm+Mn_Ilm);
XMn_Ilm = Mn_Ilm./(Fe_Ilm+Mn_Ilm);

Ratio = (Fe_Grt./Mg_Grt)./(Fe_Ilm./Mg_Ilm);
WhereNeg = find(Ratio < 0);
Ratio(WhereNeg) = nan(size(WhereNeg));

lnKd = log(Ratio);

%% Pownceby et al. (1987)
T_P87 = ((-4089+420.*(2.*XMn_Ilm-1)-77.*(2.*XMn_Grt-1))./(-1.987*lnKd-1.44))-273.15; 

%% Pownceby et al. (1991)
T_P91 = ((14918-2200.*(2.*XMn_Ilm-1)+620.*(XMn_Grt-XFe_Grt)-972.*XCa_Grt)./(8.314.*lnKd+4.38))-273.15; 

%% Martin et al. (2010)
T_M10 = ((14642-2200.*(2.*XMn_Ilm-1)+539.*(XMn_Grt-XFe_Grt)+12083.*XMg_Grt)./(8.314.*lnKd+7.67.*XMg_Grt+4.203))-273.15;

%% Collecting all results
OutputData = [T_P87,T_P91,T_M10];

end