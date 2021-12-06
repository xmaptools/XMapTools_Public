function [OutputData,OutputVariables] = GrtBt_T_All(InputData,InputVariables,AddParameters,ElOxDataDef)
% -
% XMapTools External Function: Garnet-Biotite thermometry (all calibrations) 
% 
% List of output variables:
%       - T_T76     Thompson (1976)         
%       - T_GA77    Goldman & Albee (1977)
%       - T_HL77    Holdaway & Lee (1977) 
%       - T_FS78    Ferry & Spear (1978)
%       - T_HS82    Hodges & Spear (1982)
%       - T_PL83    Perchuk & Lavrent'eva (1983) 
%       - T_GS84    Ganguly & Saxena (1984)
%       - T_IM85    Indares & Martignole (1985)
%       - T_P85     Perchuk et al. (1985)
%       - T_D91     Dasgupta et al. (1991)
%       - T_B92     Bhattacharya et al. (1992)
%
% 12 Oxygen-basis for Garnet
% 11 Oxygen-basis for Biotite
%
% J. Laughton & P. Lanari - Last update 02.09.2021 
% Find out more at https://xmaptools.ch

OutputVariables = {'T_HL77','T_GA77','T_T76','T_FS78','T_HS82','T_PL83a','T_PL83b',...
    'T_GS84','T_IM85a','T_IM85b','T_P85','T_D91','T_B92a','T_B92b'};

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

%% Biotite general structural formula (11 oxygen)
InputData_Mean_Bt = InputData(2).Mean;
InputData_MC_Bt = InputData(2).MC;
InputData_All_Bt = [InputData_Mean_Bt;InputData_MC_Bt];
[MatrixSF,ElementsList] = SF_OxNorm(InputData_All_Bt,InputVariables,11,ElOxDataDef);

Si_Bt = MatrixSF(:,1);
Ti_Bt = MatrixSF(:,2);
Al_Bt = MatrixSF(:,3);
Fe_Bt = MatrixSF(:,4)+MatrixSF(:,5);
Mn_Bt = MatrixSF(:,6);
Mg_Bt = MatrixSF(:,7);
Ca_Bt = MatrixSF(:,8);
Na_Bt = MatrixSF(:,9);
K_Bt = MatrixSF(:,10);

%% Variables
XFe_Grt = Fe_Grt./(Fe_Grt+Mg_Grt+Ca_Grt+Mn_Grt);
XMg_Grt = Mg_Grt./(Fe_Grt+Mg_Grt+Ca_Grt+Mn_Grt);
XCa_Grt = Ca_Grt./(Fe_Grt+Mg_Grt+Ca_Grt+Mn_Grt);
XMn_Grt = Mn_Grt./(Fe_Grt+Mg_Grt+Ca_Grt+Mn_Grt);

XTi_Bt = Ti_Bt./(Ti_Bt+Al_Bt+Fe_Bt+Mg_Bt+Mn_Bt);
XAl_Bt = Al_Bt./(Ti_Bt+Al_Bt+Fe_Bt+Mg_Bt+Mn_Bt);
XFe_Bt = Fe_Bt./(Ti_Bt+Al_Bt+Fe_Bt+Mg_Bt+Mn_Bt);
XMg_Bt = Mg_Bt./(Ti_Bt+Al_Bt+Fe_Bt+Mg_Bt+Mn_Bt);
XMn_Bt = Mn_Bt./(Ti_Bt+Al_Bt+Fe_Bt+Mg_Bt+Mn_Bt);

lnKd = log((Fe_Grt./Mg_Grt)./(Fe_Bt./Mg_Bt));

%% Thompson, A. B. (1976). Mineral reactions in pelitic rocks; I, Prediction of PTX (Fe-Mg) phase
%    relations. American Journal of Science, 276(4), 401-424.
T_T76 = (2739.3+0.0234.*P_kbar.*1000)./(lnKd+1.56)-273.15;

%% Goldman, D. S., & Albee, A. L. (1977). Correlation of Mg/Fe partitioning between garnet and
%    biotite with O18/O16 partitioning between quartz and magnetite. Am. J. Sci., 277(6).
T_GA77 = sqrt((-1.4315e6)./(-lnKd-0.522))-273.15;

%% Holdaway, M. J., & Lee, S. M. (1977). Fe-Mg cordierite stability in high-grade pelitic rocks
%    based on experimental, theoretical, and natural observations. Contributions to Mineralogy
%    and Petrology, 63(2), 175-198.
T_HL77 = (3095+0.0124.*P_kbar.*1000)./(lnKd+1.978)-273.15;

%% Ferry & Spear (1978)
T_FS78 = (-12454-0.057.*P_kbar.*1000)./((5.961.*-lnKd)-4.662)-273.15;

%% Hodges & Spear (1982)
T_HS82 = (12454+0.057.*P_kbar.*1000+9900.*(XCa_Grt.^2+XFe_Grt.*XCa_Grt+...
    XCa_Grt.*XMn_Grt+XMg_Grt.*XCa_Grt))./(4.5.*(XCa_Grt.^2+...
    XFe_Grt.*XCa_Grt+XCa_Grt.*XMn_Grt+XMg_Grt.*XCa_Grt)-...
    5.961.*(-lnKd)+4.662)-273.15;

%% Perchuk & Lavrent'eva (1983) 
T_PL83a = (7843.7-0.0577.*(P_kbar.*1000-6000))./(1.987.*lnKd+5.699)-273.15;
T_PL83b = (7843.7-0.0246.*(P_kbar.*1000-6000))./(1.987.*lnKd+5.699)-273.15;

%% Ganguly & Saxena (1984)
T_GS84 = (1175+P_kbar.*1000.*0.00945+(((XFe_Grt.*2500+XMg_Grt.*200)./...
    (XFe_Grt+XMg_Grt)).*(XFe_Grt-XMg_Grt)+3000.*XCa_Grt+...
    3000.*XMn_Grt)./1.987)./(0.782+lnKd)-273.15;                               

%% Indares & Martignole (1985)
T_IM85a = (12454+0.057.*P_kbar.*1000+3.*(-464.*XAl_Bt-6767.*XTi_Bt)+9900.*XCa_Grt)./...
    (4.662-5.9616.*-lnKd+4.5.*XCa_Grt)-273.15;
T_IM85b = (12454+0.057.*P_kbar.*1000+3.*(-1590.*XAl_Bt-7451.*XTi_Bt)-3.*(-3000.*(XCa_Grt+XMn_Grt)))./...
    (4.662-5.9616.*-lnKd)-273.15;

%% Perchuk et al. (1985)
T_P85 = (3720+2871.*XCa_Grt+0.038.*P_kbar.*1000)./(lnKd+0.625.*XCa_Grt+2.868)-273.15;

%% Dasgupta et al. (1991) 
T_D91 = (4301+3000.*XCa_Grt+1300.*XMn_Grt-495.*(XMg_Grt-XFe_Grt)-3595.*XAl_Bt-4423.*...
    XTi_Bt+1073.*(XMg_Bt-XFe_Bt)+0.0246.*P_kbar.*1000)./(1.85-1.987.*-lnKd)-273.15;

%% Bhattacharya et al. (1992)
T_B92a = (20286+0.0193.*P_kbar.*1000-(2080.*XMg_Grt.^2-6350.*XFe_Grt.^2-13807.*...
    XCa_Grt.*(1-XMn_Grt)+8540.*XFe_Grt.*XMg_Grt.*(1-XMn_Grt)+...
    4215.*XCa_Grt.*(XMg_Grt-XFe_Grt))+4441.*(2.*XMg_Bt-1))./(13.138+8.3143.*...
    lnKd+6.276.*XCa_Grt.*(1-XMn_Grt))-273.15;
T_B92b = (13538+0.0193.*P_kbar.*1000-(837.*XMg_Grt.^2-10460.*XFe_Grt.^2-13807.*...
    XCa_Grt.*(1-XMn_Grt)+19246.*XFe_Grt.*XMg_Grt.*(1-XMn_Grt)+...
    5649.*XCa_Grt.*(XMg_Grt-XFe_Grt))+7972.*(2.*XMg_Bt-1))./(6.778+8.3143.*...
    lnKd+6.276.*XCa_Grt.*(1-XMn_Grt))-273.15;

%% Collecting all results
OutputData = [T_HL77,T_GA77,T_T76,T_FS78,T_HS82,T_PL83a,T_PL83b,T_GS84,T_IM85a,T_IM85b,T_P85,T_D91,T_B92a,T_B92b];


end







