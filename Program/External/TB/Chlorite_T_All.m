function [OutputData,OutputVariables] = Chlorite_T_All(InputData,InputVariables,AddParameters,ElOxDataDef)
% -
% XMapTools External Function: chlorite thermometry (all calibrations) 
% 
% List of output variables:
%       - T_L14_2   Lanari et al. (2014) Contrib. Miner. Petrol. 167:167–968 (LWV2)
%       - T_B13     Bourdelle et al. (2013) Contrib. Miner. Petrol. 165:723–735
%       - T_ZF95    Zang & Fyfe (1995) Miner. Depos. 30:30–38
%       - T_HV91    Hillier & Velde, (1991) Clay Miner. 26:149–168
%       - T_J91     Jowett, (1991) Program Abstr. 16:A62
%       - T_C88     Cathelineau (1988) Clay Miner. 23:471-485
%       - T_KM87    Kranidiotis & MacLean, (1987) Econ. Geol. 82:1898–1911
%       - T_CN85    Cathelineau & Nieva (1985) Contrib. Miner. Petrol. 91:235–244
% 
%  ++01.2021 Compatibility with XMapTools 4
%  		- Fe2O3 added as possible input
%
% 14 Oxygen-basis 
%
% P. Lanari - Last update 15.04.2021
% Find out more at https://xmaptools.ch

OutputVariables = {'T_L14_2','T_B13','T_ZF95','T_HV91','T_J91','T_C88','T_KM87','T_CN85'};

OutputData = zeros(size(InputData,1),length(OutputVariables));

% Additional variables to be defined by the user
[P_kbar] = str2num(char(inputdlg({'Pressure (kbar)'},'Input',1,{'5'})));


% General structural formula function for 11 oxygen
WhereMin = find(sum(InputData,2) > 50);
[MatrixSF,ElementsList] = SF_OxNorm(InputData(WhereMin,:),InputVariables,14,ElOxDataDef);

Si = MatrixSF(:,1);
Ti = MatrixSF(:,2);
Al= MatrixSF(:,3);
Fe= MatrixSF(:,4)+MatrixSF(:,5);
Mn= MatrixSF(:,6);
Mg= MatrixSF(:,7);
Ca= MatrixSF(:,8);
Na= MatrixSF(:,9);
K= MatrixSF(:,10);

fe2 = Fe;
fe3 = zeros(size(Fe));

Aliv = 4 - Si;


%% Old calibrations

T_ZF95 = 106.2.*(Aliv*2-0.88.*(Fe./(Fe+Mg)-0.34))+17.5;     % Zang & Fyfe (1995); warning: 28 oxygen (Aliv*2)
T_HV91 = (Aliv*2 - 1.303546) ./ 0.004007;                   % Hillier & Velde, (1991); warning: 28 oxygen (Aliv*2)
T_J91 = (Aliv + 0.1.*(Fe./(Fe+Mg))) .* 319 - 69;            % Jowett (1991)
T_C88 = -61.9229 + 321.9772 * Aliv;                         % Cathelineau (1988)
T_KM87 = 106 .* (Aliv*2 + 0.7.*(Fe./(Fe+Mg)))+18;           % Kranidiotis & MacLean (1987); warning: 28 oxygen (Aliv*2)
T_CN85 = (Aliv + 8.26e-2) ./ 4.71e-3;                       % Cathelineau & Nieva (1985)


%% Bourdelle et al. (2013)
Alvi = Al-Aliv;
DT = (Alvi + fe3) - Aliv;
V_M1 = DT./2;

WhereId = find(V_M1 < 0);
V_M1(WhereId) = 1e-2*ones(size(WhereId));

XAl6 = Alvi./(Alvi+fe3);
Al_M23 = DT.*XAl6;

Al_M4 = Alvi-Al_M23;         % to avoid the loop (not tested)
WhereId = find(Al_M4 > 1);
Al_M4(WhereId) = ones(size(WhereId)) - fe3(WhereId);
WhereId = find(Al_M4 < 0);
Al_M4(WhereId) = zeros(size(WhereId));

Al_M1 = Alvi - Al_M4 - Al_M23;

XFe = fe2./(fe2+Mg);
XMg = 1-XFe;

Mg_M4 = (1-Al_M4-fe3).*XMg;
Fe_M4 = (1-Al_M4-fe3).*XFe;

FeMg_M1 = 1-Al_M1-V_M1;
FeMg_M23 = Mg + fe2 - FeMg_M1 - Mg_M4 - Fe_M4;

Mg_M1 = FeMg_M1.*XMg;
Mg_M23 = FeMg_M23.*XMg;

Mg_M14 = Mg_M1 + Mg_M4;
Al_M14 = Al_M1 + Al_M4;
V_M14 = V_M1;

XSi_T2 = (Si-2)/2;
XAl_T2 = Aliv/2;
XMg_M14 = Mg_M14/2;
XAl_M14 = Al_M14/2;
XV_M14 = V_M14/2;
XMg_M23 = Mg_M23/4;                         % corrected PL 01.11.16
XAl_M23 = Al_M23/4;                         % corrected PL 01.11.16

WhereOut = find(XSi_T2 < 0 | XSi_T2 > 1 |XAl_T2 < 0 | XAl_T2 > 1 |XMg_M14 < 0 | XMg_M14 > 1 |XAl_M14 < 0 | XAl_M14 > 1 |XV_M14 < 0 | XV_M14 > 1 |XMg_M23 < 0 | XMg_M23 > 1 |XAl_M23 < 0 | XAl_M23 > 1);

XSi_T2(WhereOut) = zeros(size(WhereOut));
XAl_T2(WhereOut) = zeros(size(WhereOut));
XMg_M14(WhereOut) = zeros(size(WhereOut));
XAl_M14(WhereOut) = zeros(size(WhereOut));
XV_M14(WhereOut) = zeros(size(WhereOut));
XMg_M23(WhereOut) = zeros(size(WhereOut)); 
XAl_M23(WhereOut) = zeros(size(WhereOut));

a_afchl = XSi_T2.*XSi_T2.*XMg_M14.*XMg_M14.*(XMg_M23.^4);
a_ames = XAl_T2.*XAl_T2.*XAl_M14.*XAl_M14.*XMg_M23.*XMg_M23.*XMg_M23.*XMg_M23;
a_sud = 256.*XSi_T2.*XAl_T2.*XV_M14.*XAl_M14.*XMg_M23.*XMg_M23.*XAl_M23.*XAl_M23;

Kratio = (a_ames.^3)./(a_afchl.*(a_sud.^3));

LnK = log10(Kratio);

T_B13 = (9304.2./(23.239-LnK))-273;

WhereNaN = find(isnan(T_B13));
T_B13(WhereNaN) = zeros(size(WhereNaN));


%% Lanari et al. (2014) LWV-2 (Fetot = Fe2+)
R = 8.314472;

R2 = fe2 + Mg + Mn + Ca; % With Ca (May 2010)
R1 = Na + K;

Si_T2 = Si - 2;
Al_T2 = 2 - Si_T2;
V_M1 = (Alvi - Aliv + fe3 - R1)./2; % OV (May 2010)

R = 8.314472;

x = Fe ./ (Fe+Mg);    % here it is iron total (!)
y = Al_M1;
z = V_M1;

% Site Fraction
XfeM23 = x.*(1 - 0.5.*z);
XmgM23 = (1 - x) .* (1 - 0.5.*z);
XalM23 = 0.5.*z;
XfeM1 = x.*(1 - y - z);
XmgM1 = (1 - x).*(1 - y - z);
XalM1 = y;
XvM1 = z;
XalT2 = 1-0.5.*(1-y);
XsiT2 = 0.5.*(1-y);


% (4) ideal activity coeficients:
ai_ames = (XmgM23.^4).*XalM1.*(XalT2.^2);
ai_clin = 4.*(XmgM23.^4).*XmgM1.*XalT2.*XsiT2;
ai_sud = 64.*(XalM23.^2).*(XmgM23.^2).*XvM1.*XalT2.*XsiT2;

lnK = zeros(size(ai_ames));
IdxVacOk = find(V_M1 > 0.03 & ai_ames > 0 & ai_clin > 0 & ai_sud > 0); % test on the activity added April 2021

lnK(IdxVacOk) = log((ai_ames(IdxVacOk).^4)./((ai_clin(IdxVacOk).^2).*(ai_sud(IdxVacOk).^3))); 

A = 203093.3315;
B = 4996.998585;   % as in Lanari et al. 2014 - original value 4596.998585
C = -445.782526;

T_L14_2 = zeros(size(lnK));
T_L14_2(IdxVacOk) = (A+B*P_kbar)./(-R.*lnK(IdxVacOk)-C) - 273.15;   % P_kbar is ok as in Lanari et al. (2014)


%% Collecting all results and filtering
OutputData(WhereMin,:) = [T_L14_2,T_B13,T_ZF95,T_HV91,T_J91,T_C88,T_KM87,T_CN85];

% Filter T < 10 °C and T > 600 °C (arbitrary)
OutIdx = find(OutputData < 10 | OutputData > 600);
OutputData(OutIdx) = zeros(size(OutIdx));

end







