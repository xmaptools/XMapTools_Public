function [OutputData,OutputVariables] = Amphibole_T_All(InputData,InputVariables,AddParameters,ElOxDataDef)
% -
% XMapTools External Function: amphibole thermobarometry
%  
% List of output variables:
%       - T_HB94       Holland & Blundy (1994) – no quartz
%       - T_HB94q      Holland & Blundy (1994) – with quartz
%
% 23 Oxgen-basis 
% 
% P. Lanari - Last update 28.10.2021
% Find out more at https://xmaptools.ch

OutputVariables = {'T_HB94','T_HB94q'};

OutputData = zeros(size(InputData,1),length(OutputVariables));

P = AddParameters.Values(1);
Xab = AddParameters.Values(2);
Xan = AddParameters.Values(3);

% General structural formula function for 23 oxY_calibAgen
WhereMin = find(sum(InputData,2) > 50);
[MatrixSF,ElementsList] = SF_OxNorm(InputData(WhereMin,:),InputVariables,23,ElOxDataDef);

Si = MatrixSF(:,1);
Ti = MatrixSF(:,2);
Al= MatrixSF(:,3);
Fe= MatrixSF(:,4)+MatrixSF(:,5);
Mn= MatrixSF(:,6);
Mg= MatrixSF(:,7);
Ca= MatrixSF(:,8);
Na= MatrixSF(:,9);
K= MatrixSF(:,10);


cm = Si+Al+Ti+Fe+Mg+Mn-13;

XSi_T1 = (Si - 4) / 4;
XAl_T1 = (8 - Si) / 4;
XAl_M2 = (Al + Si - 8) / 2;
XK_A = K; % not used
XV_A = 3 - Ca - Na - K - cm; % not used
XNa_A = Ca + Na + cm - 2;
XNa_M4 = (2 - Ca - cm) / 2;
XCa_M4 = Ca/2;

% Holland & BlundY_calibA (1994) equation A with quartz
R = 0.0083144;

Y_calibA = 0;
if Xab < 0.5
    Y_calibA = 12*(1 - Xab)^2 - 3;
end

Y_CalibB = 3;
if Xab < .5
    Y_CalibB = 12*(2*Xab - 1) + 3;
end

T_HB94 = zeros(size(Si));
T_HB94q = zeros(size(Si));

for i = 1:length(T_HB94)
    % Calib A (with Qz)
    leHaut = -76.95 + 0.79*P + Y_calibA + 39.4*XNa_A(i) + 22.4*XK_A(i) + (41.5 - 2.89*P)*XAl_M2(i);
    K = (27.*XV_A(i).*XSi_T1(i).*Xab)./(256.*XNa_A(i).*XAl_T1(i));

    Temp = leHaut./( - 0.0650 - R .* log(K)) - 273.15;
    
    if isreal(Temp) && Temp > 0 && Temp < 1200
        T_HB94q(i) = Temp;
    end 
    
    % Calib B (no Qz)
    leHaut = 78.44 + Y_CalibB - 33.6*(XNa_M4(i)) - (66.8 - 2.92*P)*(XAl_M2(i))+78.5*(XAl_T1(i)) + 9.4*XNa_A(i); 
    K = (27*XNa_M4(i)*XSi_T1(i)*Xan)/(64*XCa_M4(i)*XAl_T1(i)*Xab);
    
    Temp = leHaut/(0.0721 - R * log(K)) - 273.15;
    if isreal(Temp) && Temp > 0 && Temp < 1200
        T_HB94(i) = Temp;
    end 
end

OutputData(WhereMin,:) = [T_HB94,T_HB94q];

end







