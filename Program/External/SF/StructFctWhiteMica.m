function [OutputData,OutputVariables] = StructFctWhiteMica(InputData,InputVariables,ElOxDataDef)
% -
% XMapTools External Function: structural formula of White mica (generic) 
%  
%  ++01.2021 Compatibility with XMapTools 4
%       - version without loop & Fe2O3 as possible input
%   
%   -------------------------------------------------------------
%   End-member              T1(2)   T2(2)   M1(1)   M2(2)   A(1)
%   -------------------------------------------------------------
%   Celadonite-Mg (Cel)     Si,Si   Si,Si	V       Al,Mg   K
%   Celadonite-Fe (fCel)    Si,Si   Si,Si	V       Al,Fe   K
%   Muscovite (Ms)          Si,Si   Si,Al	V       Al,Al   K
%   Paragonite (Pg)         Si,Si   Si,Al	V       Al,Al   Na
%   Pyrophillite (Prl)      Si,Si   Si,Si	V       Al,Al   V
%   -------------------------------------------------------------    
%   Not considered:
%   Biotite                 Si,Si   Si,Al   MgFe    MgFeTi  K
%   ------------------------------------------------------------- 
%
% 11 Oxygen-basis 
%
% P. Lanari - Last update 20.01.2021
% Find out more at https://xmaptools.ch

OutputVariables = {'Si','Al','XMg','Al_T2','V_M1','Fe_M1','Mg_M1','Al_M2','Ti_M2','Fe_M2','Mg_M2','K_A','Na_A','V_A','SumCat','Xms','Xcel','Xfcel','Xpg','Xprl'};

OutputData = zeros(size(InputData,1),length(OutputVariables));

% General structural formula function for 11 oxygen
WhereMin = find(sum(InputData,2) > 50);
[MatrixSF,ElementsList] = SF_OxNorm(InputData(WhereMin,:),InputVariables,11,ElOxDataDef);

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
    
    % Structural Formula (version SWOT - March 2020)
    XMg = Mg/(Mg+Fe);
    XFe = 1-XMg;
    
    % T2:
    Al_T2 = 4-Si;
    
    % M2:
    Al_M2 = Al-Al_T2;
    Ti_M2 = Ti;
    
    if Al_M2+Ti_M2 < 2
        FeMg_M2 = 2-(Al_M2+Ti_M2);
    else
        FeMg_M2 = 0;
    end
    
    Mg_M2 = FeMg_M2*XMg;
    Fe_M2 = FeMg_M2*XFe;
    
    % M1:
    FeMg_M1 = (Fe+Mg) - FeMg_M2;
    if FeMg_M1 > 0
        Fe_M1 = XFe*FeMg_M1;
        Mg_M1 = XMg*FeMg_M1;
        V_M1 = 1-FeMg_M1;
    else
        Fe_M1 = 0;
        Mg_M1 = 0;
        V_M1 = 1;
    end
    
    % A:
    K_A = K;
    Na_A = Na;
    
    if K+Na < 1
        V_A = 1-(K+Na);
    else
        V_A = 0;
    end
    
    SumCat = Si+Ti+Al+Fe+Mn+Mg+Ca+Na+K;
    
    % End-member fractions (test - PL):
    Npar = Na_A;
    Nmu = Al_T2 - Na_A;
    Npyr = V_A;
    Ncel = FeMg_M2;
    
    Xpg = Npar/(Npar+Nmu+Npyr+Ncel);
    Xms = Nmu/(Npar+Nmu+Npyr+Ncel);
    Xprl = Npyr/(Npar+Nmu+Npyr+Ncel);
    Xcel = XMg*(Ncel/(Npar+Nmu+Npyr+Ncel));
    Xfcel = XFe*(Ncel/(Npar+Nmu+Npyr+Ncel));
    
    OutputData(i,:) = [Si,Al,XMg,Al_T2,V_M1,Fe_M1,Mg_M1,Al_M2,Ti_M2,Fe_M2,Mg_M2,K_A,Na_A,V_A,SumCat,Xms,Xcel,Xfcel,Xpg,Xprl];
    
end

end







