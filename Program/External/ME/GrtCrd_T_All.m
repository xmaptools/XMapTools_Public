function [OutputData,OutputVariables] = GrtCrd_T_All(InputData,InputVariables,AddParameters,ElOxDataDef)
% -
% XMapTools External Function: Garnet-Cordierite thermometry (all calibrations) 
% 
% List of output variables:
%       - T_C71     Currie (1971)          
%       - T_T76     Thompson (1976)     
%       - T_HL77    Holdaway & Lee (1977
%       - T_P85     Perchuk et al. (1985)
%       - T_B88     Bhattacharya et al. (1988)
%       - T_B93     Bhattacharya (1993)
%
% 12 Oxygen-basis for Garnet
% 18 Oxygen-basis for Cordierite
%
% J. Laughton - Last update 02.09.2021  
% Find out more at https://xmaptools.ch

OutputVariables = {'T_C71','T_T76','T_HL77','T_P85','T_B88','T_B93'};

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

%% Cordierite general structural formula (18 oxygen)
InputData_Mean_Crd = InputData(2).Mean;
InputData_MC_Crd = InputData(2).MC;
InputData_All_Crd = [InputData_Mean_Crd;InputData_MC_Crd];
[MatrixSF,ElementsList] = SF_OxNorm(InputData_All_Crd,InputVariables,18,ElOxDataDef);

Si_Crd = MatrixSF(:,1);
Ti_Crd = MatrixSF(:,2);
Al_Crd = MatrixSF(:,3);
Fe_Crd = MatrixSF(:,4)+MatrixSF(:,5);
Mn_Crd = MatrixSF(:,6);
Mg_Crd = MatrixSF(:,7);
Ca_Crd = MatrixSF(:,8);
Na_Crd = MatrixSF(:,9);
K_Crd = MatrixSF(:,10);

%% Variables
XFe_Grt = Fe_Grt./(Fe_Grt+Mg_Grt+Ca_Grt+Mn_Grt);
XMg_Grt = Mg_Grt./(Fe_Grt+Mg_Grt+Ca_Grt+Mn_Grt);
XCa_Grt = Ca_Grt./(Fe_Grt+Mg_Grt+Ca_Grt+Mn_Grt);
XMn_Grt = Mn_Grt./(Fe_Grt+Mg_Grt+Ca_Grt+Mn_Grt);

XFe_Crd = Fe_Crd./(Fe_Crd+Mg_Crd);
XMg_Crd = Mg_Crd./(Fe_Crd+Mg_Crd);

lnKd = log((Fe_Grt./Mg_Grt)./(Fe_Crd./Mg_Crd));

%% Currie (1971)
T_C71 = 4515./(6.37-lnKd)-273.15;

%% Thompson (1976)
T_T76 = (2725+0.0155*P_kbar*1000)./(lnKd+0.896)-273.15;

%% Holdaway & Lee (1977)
T_HL77 = (3095+0.0153*P_kbar*1000)./(lnKd+1.354)-273.15;

%% Perchuk et al. (1985)
T_P85 = (3087+0.018*P_kbar*1000)./(lnKd+1.342)-273.15;

%% Bhattacharya et al. (1988)
T_B88 = (1814+0.0152*P_kbar*1000+1122.*(XMg_Crd-XFe_Crd)-1258.*...
    (XMg_Grt-XFe_Grt)+1510*(XCa_Grt+XMn_Grt))./(1.028+lnKd)-273.15;

%% Bhattacharya (1993)
T_B93 = (1928.61+0.0152*P_kbar*1000+1311.*(XFe_Grt-XMg_Grt)+1573.*...
    (XCa_Grt+XMn_Grt)+991.*(XMg_Crd-XFe_Crd))./(1+1.014*lnKd)-273.15;

%% Collecting all results
OutputData = [T_C71,T_T76,T_HL77,T_P85,T_B88,T_B93];

end