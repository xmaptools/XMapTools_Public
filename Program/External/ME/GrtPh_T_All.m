function [OutputData,OutputVariables] = GrtPh_T_All(InputData,InputVariables,AddParameters,ElOxDataDef)
% -
% XMapTools External Function: Garnet-Phengite thermometry (all calibrations) 
% 
% List of output variables:
%       - T_KR88    Krogh & Raheim (1978)               
%       - T_GH82    Green & Hellman (1982)     
%       - T_HF88    Hynes & Forest (1988)
%       - T_W02     Wu et al. (2002)
%
% 12 Oxygen-basis for Garnet
% 11 Oxygen-basis for Phengite
%
% J. Laughton - Last update 07.01.2022  
% Find out more at https://xmaptools.ch

OutputVariables = {'T_KR78','T_GH82a','T_GH82b','T_GH82c','T_HF88','T_W02a','T_W02b'};

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

%% Phengite general structural formula (11 oxygen)
InputData_Mean_Ph = InputData(2).Mean;
InputData_MC_Ph = InputData(2).MC;
InputData_All_Ph = [InputData_Mean_Ph;InputData_MC_Ph];
[MatrixSF,ElementsList] = SF_OxNorm(InputData_All_Ph,InputVariables,11,ElOxDataDef);

Si_Ph = MatrixSF(:,1);
Ti_Ph = MatrixSF(:,2);
Al_Ph = MatrixSF(:,3);
Fe_Ph = MatrixSF(:,4)+MatrixSF(:,5);
Mn_Ph = MatrixSF(:,6);
Mg_Ph = MatrixSF(:,7);
Ca_Ph = MatrixSF(:,8);
Na_Ph = MatrixSF(:,9);
K_Ph = MatrixSF(:,10);

%% Variables
XFe_Grt = Fe_Grt./(Fe_Grt+Mg_Grt+Ca_Grt+Mn_Grt);
XMg_Grt = Mg_Grt./(Fe_Grt+Mg_Grt+Ca_Grt+Mn_Grt);
XCa_Grt = Ca_Grt./(Fe_Grt+Mg_Grt+Ca_Grt+Mn_Grt);
XMn_Grt = Mn_Grt./(Fe_Grt+Mg_Grt+Ca_Grt+Mn_Grt);

XAl_Ph = Al_Ph./(Al_Ph+Fe_Ph+Mg_Ph);
XFe_Ph = Fe_Ph./(Al_Ph+Fe_Ph+Mg_Ph);
XMg_Ph = Mg_Ph./(Al_Ph+Fe_Ph+Mg_Ph);

lnKd = log((Fe_Grt./Mg_Grt)./(Fe_Ph./Mg_Ph));

%% Krogh & Raheim (1978)
T_KR78 = ((3685+77.1.*P_kbar)./(lnKd+3.52))-273.15;

%% Green & Hellman (1982)
T_GH82a = ((5560+0.036.*P_kbar*1000)./(lnKd+4.65))-273.15;
T_GH82b = ((5680+0.036.*P_kbar*1000)./(lnKd+4.48))-273.15;
T_GH82c = ((5170+0.036.*P_kbar*1000)./(lnKd+4.17))-273.15;

%% Hynes & Forest (1988)
T_HF88 = ((4726+17.*P_kbar)./(lnKd+4.16))-273.15;

%% Wu et al. (2002)
Ga_W02 = 12.4.*XFe_Grt.^2+22.09.*XMg_Grt.^2-12.02.*XCa_Grt.^2+23.01.*...
    XMn_Grt.^2-68.98.*XFe_Grt.*XMg_Grt+37.33.*XFe_Grt.*...
    XCa_Grt+18.165.*XFe_Grt.*XMn_Grt+35.33.*XMg_Grt.*XCa_Grt+...
    27.855.*XMg_Grt.*XMn_Grt+35.165.*XCa_Grt.*XMn_Grt;
Gb_W02 = -0.05.*XFe_Grt.^2-0.034.*XMg_Grt.^2-0.005.*XCa_Grt.^2-0.014.*...
    XMn_Grt.^2+0.168.*XFe_Grt.*XMg_Grt+0.1565.*XFe_Grt.*...
    XCa_Grt-0.022.*XFe_Grt.*XMn_Grt-0.2125.*XMg_Grt.*XCa_Grt-...
    0.006.*XMg_Grt.*XMn_Grt-0.0305.*XCa_Grt.*XMn_Grt;
Gc_W02 = -22265.*XFe_Grt.^2-24166.*XMg_Grt.^2+3220.*XCa_Grt.^2-39632.*...
    XMn_Grt.^2+92862.*XFe_Grt.*XMg_Grt-67328.*XFe_Grt.*...
    XCa_Grt-38681.5.*XFe_Grt.*XMn_Grt-99262.*XMg_Grt.*XCa_Grt-...
    40582.5.*XMg_Grt.*XMn_Grt-79669.5.*XCa_Grt.*XMn_Grt;
T_W02a = ((969.9+P_kbar.*(1.3-9.1.*Gb_W02)-0.0091.*Gc_W02-4393.8.*...
    (XFe_Ph-XMg_Ph)+200.4.*XAl_Ph)./(1+0.0091.*(3.*8.3144.*lnKd+Ga_W02)))-273.15;
T_W02b = ((-1167.3-P_kbar.*(0.2+8.8.*Gb_W02)-0.0088.*Gc_W02-6878.1.*...
    (XFe_Ph-XMg_Ph)+2469.*XAl_Ph)./(1+0.0088.*(3.*8.3144.*lnKd+Ga_W02)))-273.15;

%% Collecting all results
OutputData = [T_KR78,T_GH82a,T_GH82b,T_GH82c,T_HF88,T_W02a,T_W02b];

end