function [OutputData,OutputVariables] = GrtCpx_T_All(InputData,InputVariables,AddParameters,ElOxDataDef)
% -
% XMapTools External Function: Garnet-Clinopyroxene thermometry (all calibrations) 
% 
% List of output variables:
%       - T_MH72    Mysen & Heier (1972)      
%       - T_RG74    Raheim & Green (1974)
%       - T_MG78    Mori & Green (1978)
%       - T_EG79    Ellis & Green (1979)
%       - T_S79     Saxena (1979)
%       - T_D80     Dahl (1980)
%       - T_P85     Powell (1985)
%       - T_K88     Krogh (1988)
%       - T_A94     Ai (1994)
%       - T_R00     Ravna (2000)
%       - T_N09     Nakamura (2009)
%
% 12 Oxygen-basis for Garnet
% 6 Oxygen-basis for Clinopyroxene
%
% J. Laughton & P. Lanari - Last update 02.09.2021  
% Find out more at https://xmaptools.ch

OutputVariables = {'T_MH72','T_RG74','T_MG78','T_EG79','T_S79','T_D80','T_P85','T_K88','T_A94','T_R00','T_N09'};

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

%% Clinopyroxene general structural formula (6 oxygen)
InputData_Mean_Cpx = InputData(2).Mean;
InputData_MC_Cpx = InputData(2).MC;
InputData_All_Cpx = [InputData_Mean_Cpx;InputData_MC_Cpx];
[MatrixSF,ElementsList] = SF_OxNorm(InputData_All_Cpx,InputVariables,6,ElOxDataDef);

Si_Cpx = MatrixSF(:,1);
Ti_Cpx = MatrixSF(:,2);
Al_Cpx = MatrixSF(:,3);
Fe_Cpx = MatrixSF(:,4)+MatrixSF(:,5);
Mn_Cpx = MatrixSF(:,6);
Mg_Cpx = MatrixSF(:,7);
Ca_Cpx = MatrixSF(:,8);
Na_Cpx = MatrixSF(:,9);
K_Cpx = MatrixSF(:,10);

%% Variables
XFe_Grt = Fe_Grt./(Fe_Grt+Mg_Grt+Ca_Grt+Mn_Grt);
XMg_Grt = Mg_Grt./(Fe_Grt+Mg_Grt+Ca_Grt+Mn_Grt);
XCa_Grt = Ca_Grt./(Fe_Grt+Mg_Grt+Ca_Grt+Mn_Grt);
XMn_Grt = Mn_Grt./(Fe_Grt+Mg_Grt+Ca_Grt+Mn_Grt);
MgN_Grt = Mg_Grt./(Fe_Grt+Mg_Grt);

lnKd = log((Fe_Grt./Mg_Grt)./(Fe_Cpx./Mg_Cpx));

%% Mysen & Heier (1972)
T_MH72 = 2475./(lnKd+0.781)-273.15;
               
%% Raheim & Green (1974)
T_RG74 = (3686+28.35*P_kbar)./(lnKd+2.33)-273.15;

%% Mori & Green (1978)
T_MG78 = 2800./(lnKd+1.19)-273.15;

%% Ellis & Green (1979)
T_EG79 = (3104*XCa_Grt+3030+10.86*P_kbar)./(lnKd+1.9034)-273.15;

%% Saxena (1979)
XFe_S79 = Fe_Cpx./(Fe_Cpx+Mg_Cpx+Ca_Cpx+(Al_Cpx-Na_Cpx)./2);
XMg_S79 = Mg_Cpx./(Fe_Cpx+Mg_Cpx+Ca_Cpx+(Al_Cpx-Na_Cpx)./2);
XCa_S79 = Ca_Cpx./(Fe_Cpx+Mg_Cpx+Ca_Cpx+(Al_Cpx-Na_Cpx)./2);
XAl_S79 = 1-XFe_S79-XMg_S79-XCa_S79;                
Q1_S79 = 2710.*(XFe_Grt-XMg_Grt)+3150.*XCa_Grt+2600.*XMn_Grt;
Q2_S79 = -6594.*(XFe_S79.*(XFe_S79-2.*XMg_S79))...
    -12762.*(XFe_S79-XMg_S79.*(1-XFe_S79))...
    -11281.*(XCa_S79.*(1-XAl_S79)-2.*XMg_S79.*XCa_S79)...
    +6137.*(XCa_S79.*(2.*XMg_S79+XAl_S79))...
    +35791.*(XAl_S79.*(1-2.*XMg_S79))...
    +25409.*XCa_S79.^2-55137.*(XCa_S79.*(XMg_S79-XFe_S79))...
    -11338.*(XAl_S79.*(XFe_S79-XMg_S79));                
T_S79 = (8288+0.0276.*P_kbar.*1000+Q1_S79-Q2_S79)./(1.987.*lnKd+2.4083)-273.15;

%% Dahl (1980)
T_D80 = (2324+0.022.*P_kbar.*1000+1509.*XFe_Grt.*XMg_Grt+2810.*XCa_Grt+2855.*XMn_Grt)./(1.987.*lnKd)-273.15;

%% Powell (1985)
T_P85 = (2790+10.*P_kbar+3140.*XCa_Grt)./(lnKd+1.735)-273.15;

%% Krogh (1988)
T_K88 = (-6173.*XCa_Grt.^2+10.*P_kbar+6731.*XCa_Grt+1879)./(lnKd+1.393)-273.15;

%% Ai (1994)
T_A94 = (-1629.*XCa_Grt.^2+3648.55.*XCa_Grt-6.59.*MgN_Grt.*100+1987.98+17.66.*P_kbar)./(lnKd+1.076)-273.15;

%% Ravna (2000)
T_R00 = ((1939.9+3270.*XCa_Grt-1396.*XCa_Grt.^2+...
    3319.*XMn_Grt-3535.*XMn_Grt.^2+1105.*MgN_Grt-3561.*...
    MgN_Grt.^2+2324.*MgN_Grt.^3+169.4.*P_kbar./10)./(lnKd+1.223))-273.15;

%% Nakamura (2009)
A_N09 = 0.5*XCa_Grt.*(XMg_Grt-XFe_Grt-XMn_Grt);
B_N09 = 0.5*XCa_Grt.*(XMg_Grt-XFe_Grt+XMn_Grt);
C_N09 = 0.5*(XCa_Grt+XMn_Grt).*(XMg_Grt-XFe_Grt);               
XMg_N09 = Mg_Cpx./(Al_Cpx+Fe_Cpx+Mg_Cpx);
XFe_N09 = Fe_Cpx./(Al_Cpx+Fe_Cpx+Mg_Cpx);                
T_N09 = (2784+14.52.*P_kbar+(2601+1.44.*P_kbar).*(2.*XCa_Grt.*XMg_Grt-A_N09)+...
    (1183+6.98.*P_kbar).*(XCa_Grt.^2-A_N09)-105.*(2.*XCa_Grt.*XFe_Grt+B_N09)+...
    (814.6+3.61.*P_kbar).*(XCa_Grt.^2+B_N09)-(254.6+8.42.*P_kbar).*...
    (2.*XMg_Grt.*XFe_Grt-XFe_Grt.^2+C_N09)-83.6.*...
    (XMg_Grt.^2-2.*XMg_Grt.*XFe_Grt+C_N09)+1388.*XMn_Grt-462.*...
    (XMg_N09-XFe_N09))./(lnKd+1.431+0.695.*(2.*XCa_Grt.*XMg_Grt+XCa_Grt.^2-2.*A_N09)+...
    0.203.*(XCa_Grt.^2-2.*XCa_Grt.*XFe_Grt)+0.922.*XMn_Grt)-273.15;       

%% Collecting all results
OutputData = [T_MH72,T_RG74,T_MG78,T_EG79,T_S79,T_D80,T_P85,T_K88,T_A94,T_R00,T_N09];

end