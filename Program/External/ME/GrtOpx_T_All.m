function [OutputData,OutputVariables] = GrtOpx_T_All(InputData,InputVariables,AddParameters,ElOxDataDef)
% -
% XMapTools External Function: Garnet-Orthopyroxene thermometry (all calibrations) 
% 
% List of output variables:
%       - T_MG78    Mori & Green (1978)          
%       - T_D80     Dahl (1980)     
%       - T_R83     Raith et al. (1983)
%       - T_H84     Harley (1984)
%       - T_LG84    Lee & Ganguly (1984)
%       - T_SB84    Sen & Bhattacharya (1984)
%       - T_P85     Perchuk et al. (1985)
%       - T_LG88    Lee & Ganguly (1988)
%       - T_AP89    Aranovich & Podlesskii (1989)
%       - T_PL90    Perchuk & Lavrente’va (1990)
%       - T_B91     Bhattacharya et al. (1991)
%
% 12 Oxygen-basis for Garnet
% 6 Oxygen-basis for Orthopyroxene
%
% J. Laughton - Last update 07.01.2022  
% Find out more at https://xmaptools.ch

OutputVariables = {'T_MG78','T_D80','T_R83','T_H84','T_LG84','T_SB84','T_P85','T_LG88','T_AP89','T_PL90','T_B91'};

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

%% Orthopyroxene general structural formula (6 oxygen)
InputData_Mean_Opx = InputData(2).Mean;
InputData_MC_Opx = InputData(2).MC;
InputData_All_Opx = [InputData_Mean_Opx;InputData_MC_Opx];
[MatrixSF,ElementsList] = SF_OxNorm(InputData_All_Opx,InputVariables,6,ElOxDataDef);

Si_Opx = MatrixSF(:,1);
Ti_Opx = MatrixSF(:,2);
Al_Opx = MatrixSF(:,3);
Fe_Opx = MatrixSF(:,4)+MatrixSF(:,5);
Mn_Opx = MatrixSF(:,6);
Mg_Opx = MatrixSF(:,7);
Ca_Opx = MatrixSF(:,8);
Na_Opx = MatrixSF(:,9);
K_Opx = MatrixSF(:,10);

%% Variables
XFe_Grt = Fe_Grt./(Fe_Grt+Mg_Grt+Ca_Grt+Mn_Grt);
XMg_Grt = Mg_Grt./(Fe_Grt+Mg_Grt+Ca_Grt+Mn_Grt);
XCa_Grt = Ca_Grt./(Fe_Grt+Mg_Grt+Ca_Grt+Mn_Grt);
XMn_Grt = Mn_Grt./(Fe_Grt+Mg_Grt+Ca_Grt+Mn_Grt);

XFe_Opx = Fe_Opx./(Fe_Opx+Mg_Opx+Al_Opx/2);
XMg_Opx = Mg_Opx./(Fe_Opx+Mg_Opx+Al_Opx/2);
XAl_Opx = (Al_Opx/2)./(Fe_Opx+Mg_Opx+Al_Opx/2);

lnKd = log((Fe_Grt./Mg_Grt)./(Fe_Opx./Mg_Opx));

%% Mori & Green (1978)
T_MG78 = 1300./(lnKd+0.12)-273.15;

%% Dahl (1980)
T_D80 = (1391+1509.*(XFe_Grt-XMg_Grt)+2810.*XCa_Grt+2855.*XMn_Grt)./(1.987.*lnKd)-273.15;

%% Raith et al. (1983)
T_R83 = 1684./(lnKd+0.334)-273.15;

%% Harley (1984)
T_H84 = (3740+1400.*XCa_Grt+22.86.*P_kbar)./(1.987.*lnKd+1.96)-273.15;

%% Lee & Ganguly (1984)
T_LG84 = (2187+1510.*(XCa_Grt-XMn_Grt)+8.6.*P_kbar)./(lnKd+1.071)-273.15;

%% Sen & Bhattacharya (1984)
T_SB84 = (2713+0.022.*P_kbar*1000+3300.*XCa_Grt+195.*(XFe_Grt-XMg_Grt))./...
    (-1.9872.*-lnKd+0.787+1.5.*XCa_Grt)-273.15;

%% Perchuk et al. (1985)
T_P85 = (4766+2533.*(XFe_Opx-XMg_Opx)-5214.*XAl_Opx+5704.*XCa_Grt+0.023.*P_kbar.*1000)./...
    (1.987.*lnKd+2.65+1.86.*(XFe_Opx-XMg_Opx)+1.242.*XCa_Grt)-273.15;

%% Lee & Ganguly (1988)
T_LG88 = (1981+1509.66.*(XCa_Grt-XMn_Grt)+11.91.*P_kbar)./(lnKd+0.97)-273.15;

%% Aranovich & Podlesskii (1989)
A_AP89 = -626.*XCa_Grt.^2-6642.*XFe_Grt.*XCa_Grt-8100.*XMg_Grt.*XCa_Grt+XCa_Grt.*(XMg_Grt-XFe_Grt).*1051.5;
B_AP89 = 1.266.*XCa_Grt.^2+2.836.*XFe_Grt.*XCa_Grt+3.*XMg_Grt.*XCa_Grt+XCa_Grt.*(XMg_Grt-XFe_Grt).*(-0.908);
T_AP89 = ((P_kbar.*1000-1).*0.02342+4766-A_AP89+(XFe_Opx-XMg_Opx).*2372-5204.*XAl_Opx)./...
    (1.987.*lnKd+2.654+B_AP89+1.69.*(XFe_Opx-XMg_Opx))-273.15;

%% Perchuk & Lavrente’va (1990)
T_PL90 = (4066-347.*(XMg_Opx-XFe_Opx)-17484.*XAl_Opx+5769.*XCa_Grt+23.42.*P_kbar)./...
    (1.987.*lnKd+2.143+0.0929.*(XMg_Opx-XFe_Opx)-12.8994.*XAl_Opx+3.846.*XCa_Grt)-273.15;

%% Bhattacharya et al. (1991)
A_B91 = -1220.*XFe_Grt.*XMg_Grt-441.*XCa_Grt.*(XMg_Grt-XFe_Grt)-136.*XMg_Grt.^2+746.*XFe_Grt.^2;
T_B91 = (1611+0.021.*P_kbar.*1000+906.*XCa_Grt+A_B91+477.*(2.*XMg_Opx-1))./(lnKd+0.796)-273.15;

%% Collecting all results
OutputData = [T_MG78,T_D80,T_R83,T_H84,T_LG84,T_SB84,T_P85,T_LG88,T_AP89,T_PL90,T_B91];

end