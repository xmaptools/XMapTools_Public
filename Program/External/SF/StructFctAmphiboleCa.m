function [OutputData,OutputVariables] = StructFctCaAmphibole(InputData,InputVariables,ElOxDataDef)
% -
% XMapTools External Function: structural formula of biotite 
%  
%  ++08.2021 Compatibility with XMapTools 4
%  		- Fe2O3 added as possible input
%  ++07.2019 
%       - Ti ordered onto M2 (Raase 1974)
%   
%   ------------------------------------------------------------------------------
%   End-member             T1(4)         T2(2)      M2(2)    M13(2)    M4(2)  A(1)
%   ------------------------------------------------------------------------------
%   Glaucophane (Gln)   Si,Si,Si,Si   Si,Si,Si,Si   Al,Al   Mg,Mg,Mg   Na,Na   V 
%   Tremolite (Tr)      Si,Si,Si,Si   Si,Si,Si,Si   Mg,Mg   Mg,Mg,Mg   Ca,Ca   V 
%   F-tremolite (Ftr)   Si,Si,Si,Si   Si,Si,Si,Si   Fe,Fe   Fe,Fe,Fe   Ca,Ca   V 
%   Tschermakite (Ts)   Si,Si,Si,Si   Si,Si,Al,Al   Al,Al   Mg,Mg,Mg   Ca,Ca   V 
%   Pargasite (Prg)     Si,Si,Si,Si   Si,Si,Al,Al   Mg,Al   Mg,Mg,Mg   Ca,Ca   Na 
%   ------------------------------------------------------------------------------   
%   Other:                                          Ti      Mn
%   ------------------------------------------------------------------------------ 
%
% 23 Oxygen-basis 
%
% P. Lanari - Last update 20.08.2021
% Find out more at https://xmaptools.ch

OutputVariables = {'Si','Al_iv','Al_vi', ...
                   'Al_T2','Al_M2','Ti_M2','Fe_M2','Mg_M2','Fe_M13','Mg_M13','Mn_M13', ...
                   'Na_M4','Ca_M4', ...
                   'V_A','Na_A', ...
                   'XMg','XFe','SumCat', ...
                   'Xgln','Xtr','Xftr', ...
                   'Xts','Xprg','Xsum'};

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
    
    SumCat = Si+Ti+Al+Fe+Mn+Mg+Ca+Na+K;
    
    % Structural Formula (P. Lanari, 2019)
    XMg = Mg/(Mg+Fe);
    XFe = 1-XMg;
    
    Al_iv = 4 - (Si - 4);
    Al_vi = Al - Al_iv;
    Al_T2 = Al_iv;
    Al_M2 = Al_vi;
    
    Ti_M2 = Ti;
    
    if (Al_M2 + Ti_M2) < 2
        FeMg_M2 = 2 - Al_M2 - Ti_M2;
    else
        FeMg_M2 = 0;
    end
    
    FeMg_M13 = (Fe+Mg) - FeMg_M2;
    
    Mg_M2 = FeMg_M2 * XMg;
    Fe_M2 = FeMg_M2 * XFe;
    
    Mg_M13 = FeMg_M13 * XMg;
    Fe_M13 = FeMg_M13 * XFe;
    Mn_M13 = Mn;
    
    Ca_M4 = Ca;
    Na_M4 = 2 - Ca_M4;
    Na_A = Na - Na_M4;
    V_A = 1-Na_A;
    
    Xgln = Na_M4 / 2;
    Xprg = Na_A;
    
    TschParg = Al_T2 / 2;
    Xts = TschParg - Xprg;
    
    Reste = 1-Xgln-Xprg-Xts;
    Xtr = XMg * Reste;
    Xftr = XFe * Reste;
    
    Xsum = Xgln+Xprg+Xts+Xtr+Xftr;
    
    if Al_iv > 0 || Al_vi > 0 || Al_T2 > 0 || Al_M2 > 0 || ...
            Mg_M2 > 0 || Fe_M2 > 0 || Mg_M13 > 0 || Fe_M13 > 0 || ...
            Ca_M4 > 0 || Na_M4 > 0 || Na_A > 0 || V_A > 0
        
        % this condition is to reproduce the results of XMapTools 3 and
        % previous versions
        
        OutputData(i,:) = [Si,Al_iv,Al_vi, ...
            Al_T2,Al_M2,Ti_M2,Fe_M2,Mg_M2,Fe_M13,Mg_M13,Mn_M13, ...
            Na_M4,Ca_M4, ...
            V_A,Na_A, ...
            XMg,XFe,SumCat, ...
            Xgln,Xtr,Xftr, ...
            Xts,Xprg,Xsum];
    end
end

end







