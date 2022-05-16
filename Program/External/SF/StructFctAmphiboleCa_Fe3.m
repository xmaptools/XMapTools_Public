function [OutputData,OutputVariables] = StructFctAmphiboleCa_Fe3(InputData,InputVariables,ElOxDataDef)
% -
% XMapTools External Function: structural formula of Ca-amphibole 
%  
%  ++08.2021 Compatibility with XMapTools 4
%  		- Fe2O3 added as possible input
%  ++07.2019 
%       - Ti ordered onto M2 (Raase 1974)
%   
% 23 Oxygen-basis 
%
% P. Lanari - Last update 16.09.2021  *** NOT TESTED ***
% Find out more at https://xmaptools.ch

OutputVariables = {'Si','Al_iv','Al_vi', ...
                   'Al_T2','Al_M2','Ti_M2','Fe_M2','Mg_M2','Fe3_M2','Fe_M13','Mg_M13','Mn_M13', ...
                   'Na_M4','Ca_M4', ...
                   'V_A','Na_A', ...
                   'XMg','XFe3'};

OutputData = zeros(size(InputData,1),length(OutputVariables));

% General structural formula function for 23 oxygen
WhereMin = find(sum(InputData,2) > 50);
[MatrixSF,ElementsList] = SF_OxNorm(InputData(WhereMin,:),InputVariables,23,ElOxDataDef);

% Atom site repartition (loop)
for ii=1:length(WhereMin)
    
    i = WhereMin(ii);
    
    % Element composition expressed in apfu:
    Si = MatrixSF(ii,1);
    Ti = MatrixSF(ii,2);
    Al= MatrixSF(ii,3);
    Fe= MatrixSF(ii,4)+MatrixSF(ii,5);
    Mn= MatrixSF(ii,6);
    Mg= MatrixSF(ii,7);
    Ca= MatrixSF(ii,8);
    Na= MatrixSF(ii,9);
    K= MatrixSF(ii,10);
    
    WeExclude = 0;
    
    CatTotwithoutCaNaK = Si+Ti+Al+Fe+Mn+Mg;
    TS = 13/CatTotwithoutCaNaK;
    
    Si = Si*TS;
    Ti = Ti*TS;
    Al = Al*TS;
    Fe = Fe*TS;
    Mn = Mn*TS;
    Mg = Mg*TS;
    Ca = Ca*TS;
    Na = Na*TS;
    K = K*TS;
    
    Fe3 = 2*23*(1-TS);
    
    if Fe3>Fe
        Fe2=0;
    else
        Fe2=Fe-Fe3;
    end
    
    SumCat = Si+Ti+Al+Fe2+Fe3+Mn+Mg+Ca+Na+K;
    
    % Structural Formula (implementation P. Lanari, 2019)  *** NOT TESTED ***
    XMg = Mg/(Mg+Fe2);
    XFe = 1-XMg;
    
    Al_iv = 4 - (Si - 4);
    Al_vi = Al - Al_iv;
    Al_T2 = Al_iv;
    Al_M2 = Al_vi;
    
    Ti_M2 = Ti;
    
    if (Al_M2 + Fe3 + Ti_M2) < 2
        FeMg_M2 = 2 - (Al_M2 + Fe3 + Ti_M2);
    else
        FeMg_M2 = 0;
        WeExclude = 1;
    end
    
    Fe3_M2 = Fe3;
    FeMg_M13 = (Fe2+Mg) - FeMg_M2;
    
    Mg_M2 = FeMg_M2 * XMg;
    Fe_M2 = FeMg_M2 * XFe;
    
    Mg_M13 = FeMg_M13 * XMg;
    Fe_M13 = FeMg_M13 * XFe;
    Mn_M13 = Mn;
    
    Ca_M4 = Ca;
    Na_M4 = 2 - Ca_M4;
    Na_A = Na - Na_M4;
    V_A = 1-Na_A;
    
    XFe3 = Fe3/(Fe2+Fe3);
    
    if ~WeExclude || Al_iv > 0 || Al_vi > 0 || Al_T2 > 0 || Al_M2 > 0 || ...
            Mg_M2 > 0 || Fe_M2 > 0 || Mg_M13 > 0 || Fe_M13 > 0 || ...
            Ca_M4 > 0 || Na_M4 > 0 || Na_A > 0 || V_A > 0 || XFe3 > 0 || Fe3_M2 > 0
        
        % this condition is to reproduce the results of XMapTools 3 and
        % previous versions
        
        OutputData(i,:) = [Si,Al_iv,Al_vi, ...
            Al_T2,Al_M2,Ti_M2,Fe_M2,Mg_M2,Fe3_M2,Fe_M13,Mg_M13,Mn_M13, ...
            Na_M4,Ca_M4, ...
            V_A,Na_A, ...
            XMg,XFe3,];
    end
end

end







