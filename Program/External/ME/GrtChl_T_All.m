function [OutputData,OutputVariables] = GrtChl_T_All(InputData,InputVariables,AddParameters,ElOxDataDef)
% -
% XMapTools External Function: Garnet-Chlorite thermometry (all calibrations) 
% 
% List of output variables:
%       - T_DH86    Dickenson & Hewitt (1986)      
%       - T_G90     Grambling (1990)
%       - T_P91     Perchuk (1991)
%
% 12 Oxygen-basis for Garnet
% 14 Oxygen-basis for Chlorite
%
% J. Laughton & P. Lanari - Last update 02.09.2021  
% Find out more at https://xmaptools.ch

OutputVariables = {'T_DH86','T_G90','T_P91'};

P_kbar = AddParameters.Values(1);

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

%% Chlorite general structural formula (14 oxygen)
InputData_Mean_Chl = InputData(2).Mean;
InputData_MC_Chl = InputData(2).MC;
InputData_All_Chl = [InputData_Mean_Chl;InputData_MC_Chl];
[MatrixSF,ElementsList] = SF_OxNorm(InputData_All_Chl,InputVariables,14,ElOxDataDef);

Si_Chl = MatrixSF(:,1);
Ti_Chl = MatrixSF(:,2);
Al_Chl = MatrixSF(:,3);
Fe_Chl = MatrixSF(:,4)+MatrixSF(:,5);
Mn_Chl = MatrixSF(:,6);
Mg_Chl = MatrixSF(:,7);
Ca_Chl = MatrixSF(:,8);
Na_Chl = MatrixSF(:,9);
K_Chl = MatrixSF(:,10);

%% Variables
lnKd = log((Fe_Grt./Mg_Grt)./(Fe_Chl./Mg_Chl));

%% Dickenson & Hewitt (1986)
T_DH86 = ((-51906-(0.438*P_kbar*1000))./(-7.541+(1.987*(15*-lnKd))))-273.15;

%% Grambling (1990)
T_G90 = ((-24156-(0.05*P_kbar*1000)-(4607*-lnKd))/-19.02)-273.15;

%% Perchuk (1991)
T_P91 = 3973./(lnKd+2.773)-273.15;
               
%% Collecting all results
OutputData = [T_DH86,T_G90,T_P91];

end