function [OutputData,OutputVariables] = GrtHbl_T_All(InputData,InputVariables,ElOxDataDef)
% -
% XMapTools External Function: Garnet-Hornblende thermometry (all calibrations) 
% 
% List of output variables:
%       - T_GP84    Graham & Powell (1984)               
%       - T_Per85   Perchuk et al. (1985)         
%       - T_Pow85   Powell (1985)    
%       - T_R00     Ravna (2000)   
%
% 12 Oxygen-basis for Garnet
% 23 Oxygen-basis for Hornblende
%
% J. Laughton & P. Lanari - Last update 02.09.2021  
% Find out more at https://xmaptools.ch

OutputVariables = {'T_GP84','T_Per85','T_Pow85','T_R00'};

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

%% Hornblende general structural formula (23 oxygen)
InputData_Mean_Hbl = InputData(2).Mean;
InputData_MC_Hbl = InputData(2).MC;
InputData_All_Hbl = [InputData_Mean_Hbl;InputData_MC_Hbl];
[MatrixSF,ElementsList] = SF_OxNorm(InputData_All_Hbl,InputVariables,23,ElOxDataDef);

Si_Hbl = MatrixSF(:,1);
Ti_Hbl = MatrixSF(:,2);
Al_Hbl = MatrixSF(:,3);
Fe_Hbl = MatrixSF(:,4)+MatrixSF(:,5);
Mn_Hbl = MatrixSF(:,6);
Mg_Hbl = MatrixSF(:,7);
Ca_Hbl = MatrixSF(:,8);
Na_Hbl = MatrixSF(:,9);
K_Hbl = MatrixSF(:,10);

%% Variables
XFe_Grt = Fe_Grt./(Fe_Grt+Mg_Grt+Ca_Grt+Mn_Grt);
XMg_Grt = Mg_Grt./(Fe_Grt+Mg_Grt+Ca_Grt+Mn_Grt);
XCa_Grt = Ca_Grt./(Fe_Grt+Mg_Grt+Ca_Grt+Mn_Grt);
XMn_Grt = Mn_Grt./(Fe_Grt+Mg_Grt+Ca_Grt+Mn_Grt);

lnKd = log((Fe_Grt./Mg_Grt)./(Fe_Hbl./Mg_Hbl));

%% Graham & Powell (1984)
T_GP84 = (2880+3280.*XCa_Grt)./(lnKd+2.426)-273.15;

%% Perchuk et al. (1985)
T_Per85 = 3330./(lnKd+2.333)-273.15;

%% Powell (1985)
T_Pow85 = (2580+3340.*XCa_Grt)./(lnKd+2.2)-273.15;

%% Ravna (2000)
T_R00 = (1504+1784.*(XCa_Grt+XMn_Grt))./(lnKd+0.72)-273.15;

%% Collecting all results
OutputData = [T_GP84,T_Per85,T_Pow85,T_R00];

end