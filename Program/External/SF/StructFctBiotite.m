function [OutputData,OutputVariables] = StructFctBiotite(InputData,InputVariables,ElOxDataDef)
% -
% XMapTools External Function: structural formula of biotite 
%  
%  ++01.2021 Compatibility with XMapTools 4
%  		- Fe2O3 added as possible input
%  ++03.2020 New structural formula for biotite (including Ti and Mn EM)
%       - Ti assumed to be ordered onto M2
%       - Al assumed to be ordered onto M1
%       - Mn added (ordered onto M12)
%  ++11.2015 
%       - End-member proportions based on biotite Al-content
%   
%   ----------------------------------------------------
%   End-member      T1(2)   T2(2)   M1(1)   M2(2)   A(1)
%   ----------------------------------------------------
%   Phlogopite      Si,Si   Si,Al   Mg      Mg,Mg   K
%   Annite          Si,Si   Si,Al   Fe      Fe,Fe   K
%   Eastonite       Si,Si   Al,Al   Al      Mg,Mg   K
%   Siderophyllite  Si,Si   Al,Al   Al      Fe,Fe   K
%   Ti-biotite      Si,Si   Si,Al   Mg      Ti,Mg   K
%   Mn-biotite      Si,Si   Si,Al   Mn      Mn,Mn   K
%   ----------------------------------------------------   
%   Not considered:
%   Muscovite       Si,Si   Si,Al   V       Mg,Mg   K
%   ----------------------------------------------------
%
% 11 Oxygen-basis 
%
% P. Lanari - Last update 14.11.2022
% Find out more at https://xmaptools.ch


OutputVariables = {'Si','Al_T2', ...
                   'Al_M1','Fe_M1','Mg_M1', ...
                   'Mn_M1','V_M1','Ti_M2', ...
                   'Fe_M2','Mg_M2','Mn_M2', ...
                   'K_A','Ca_A','Na_A', ...
                   'V_A','XMg','XFe', ...
                   'XAnn','XPhl','XSid', ...
                   'XEas','XTibt','XMnbt', ...
                   'Xsum'};

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
    
    Al_T2_theo = 2-(Si-2);   % This is the amount free in T2 for Al
    
    if Al_T2_theo > Al
        Al_T2 = Al;         % we have not enough Al !!!
    else
        Al_T2 = Al_T2_theo;
    end
    
    Alvi = Al-Al_T2;
    
    % M2:
    Mn_M2 = 2*Mn/3;
    Ti_M2 = Ti;
    Mg_M2 = Ti;
    
    FeMg_M2 = 2-(Mn_M2+Ti_M2);
    FeMgEqui_M2 =  FeMg_M2-Mg_M2;
    
    Mg_M2 = Mg_M2 + FeMgEqui_M2*XMg;
    Fe_M2 = FeMgEqui_M2*XFe;
    
    %Sum_M2 = Mn_M2+Ti_M2+Mg_M2+Fe_M2;
    
    % M1:
    Mn_M1 = Mn/3;
    Mg_M1 = Ti;     % contribution from Tibt
    
    if (Alvi + Mn_M1 + Mg_M1) < 1
        Al_M1 = Alvi;
    else
        Al_M1 = 1 - (Mn_M1 + Mg_M1);
    end
    
    FeMg_Equi_M1 = (Fe+Mg) - (Fe_M2+Mg_M2) - Mg_M1;
    
    if FeMg_Equi_M1 + Mn_M1 + Mg_M1 + Al_M1 < 1
        V_M1 = 1 - (FeMg_Equi_M1 + Mn_M1 + Mg_M1 + Al_M1);
        Mg_M1 = Mg_M1 + XMg*FeMg_Equi_M1;
        Fe_M1 = XFe*FeMg_Equi_M1;
    else
        V_M1 = 0;
        AvailableFeMgM1 = 1 - (Mn_M1 + Mg_M1 + Al_M1);
        Mg_M1 = Mg_M1 + XMg*AvailableFeMgM1;
        Fe_M1 = XFe*AvailableFeMgM1;
    end
    
    % A
    K_A = K;
    Ca_A = Ca;
    Na_A = Na;
    
    if K+Ca+Na <= 1
        V_A = 1-(K+Ca+Na);
    else
        V_A = 0;            % to high interfoliar sum !!!
    end
    
    % EM proportions (version SWOT - March 2020)
    % I added filtering to avoid calculating EM fractions of mixing pixels
    XTibt = Ti_M2;    
    XMnbt = Mn_M1;
    
    if XTibt + XMnbt > 1 % Filter 1
        XTibt = Ti_M2/(Ti_M2 + Mn_M1);
        XMnbt = Mn_M1/(Ti_M2 + Mn_M1);
    end
    
    XOrd = 1 - (XTibt+XMnbt);
    XSidEast = Al_T2-1;
    XPhlAnn = XOrd - XSidEast;
    
    if (XTibt + XMnbt + abs(XSidEast) + abs(XPhlAnn)) <= 1 % Filter 2
        XAnn = XPhlAnn * XFe;
        XPhl = XPhlAnn * XMg;
        XSid = XSidEast * XFe;
        XEas = XSidEast * XMg;
    else
        XAnn = 0;
        XPhl = 0;
        XSid = 0;
        XEas = 0;
    end
    
    Xsum = XTibt+XMnbt+XAnn+XPhl+XSid+XEas;

    OutputData(i,:) = [Si,Al_T2, ...
                   Al_M1,Fe_M1,Mg_M1, ...
                   Mn_M1,V_M1,Ti_M2, ...
                   Fe_M2,Mg_M2,Mn_M2, ...
                   K_A,Ca_A,Na_A, ...
                   V_A,XMg,XFe, ...
                   XAnn,XPhl,XSid, ...
                   XEas,XTibt,XMnbt, ...
                   Xsum];
    
end

end







