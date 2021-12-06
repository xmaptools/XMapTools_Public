function [OutputData,OutputVariables] = StructFctChlorite(InputData,InputVariables,ElOxDataDef)
% -
% XMapTools External Function: structural formula of chlorite 
%  
%  ++01.2021 Compatibility with XMapTools 4
%       - Ferri-sudoite added; Trincal & Lanari (2016) Clay Min. 51,675-689
%  		- Fe2O3 added as possible input
%   
%   -----------------------------------------------------------------
%   End-member          T1(2)   T2(2)   M1(1)   M2-M3(4)        M4(1)
%   -----------------------------------------------------------------
%   Amesite (Ame)       Si,Si   Al,Al   Al      Mg,Mg,Mg,Mg     Al
%   Fe-Amesite (fAme)   Si,Si   Al,Al   Al      Fe,Fe,Fe,Fe     Al
%   Chlinochlore (Clc)  Si,Si   Si,Al   Mg      Mg,Mg,Mg,Mg     Al
%   Daphnite (Dph)      Si,Si   Si,Al   Fe      Fe,Fe,Fe,Fe     Al
%   Sudoite (Sud)       Si,Si   Si,Al   V       Mg,Mg,Al,Al     Al
%   FerriSudoite (fSud) Si,Si   Si,Al   V       Mg,Mg,Fe,Fe     Al
%   AlfreeChl (afchl)   Si,Si   Si,Si   Mg      Mg,Mg,Mg,Mg     Mg
%   -----------------------------------------------------------------
%
% 14 Oxygen-basis 
% 1  Input: XFe3 = Fe3/(Fe2+Fe3);
%
% P. Lanari - Last update 20.01.2021
% Find out more at https://xmaptools.ch

OutputVariables = {'Si','Al','Fe2','Fe3','XMg','XFe3','Al_T2','Al_M1','Fe_M1','Mg_M1','V_M1','Al_M2M3','Fe2_M2M3','Mg_M2M3','Fe3_M2M3','Al_M4','Mg_M4','Xame','Xfame','Xclc','Xdph','Xsud','Xfsud','Xafchl','Xsum'};

OutputData = zeros(size(InputData,1),length(OutputVariables));

WhereMin = find(sum(InputData,2) > 50);

% Apply XFe3+
XFe3_b = str2num(char(inputdlg('XFe3+ (0-0.50)','Input',1,{'0'})));

InputDataCorr = InputData;

InputDataCorr(WhereMin,4) = InputData(WhereMin,4) .* (1-XFe3_b);
InputDataCorr(WhereMin,5) = InputData(WhereMin,4) .* XFe3_b .* (1/0.89992485);

% General structural formula function for 14 oxygen, including Fe3+
[MatrixSF,ElementsList] = SF_OxNorm(InputDataCorr(WhereMin,:),InputVariables,14,ElOxDataDef);

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
    
    Fe2 = MatrixSF(ii,4);
    Fe3 = MatrixSF(ii,5);
    
    XMg = Mg/(Mg+Fe2);
    XFe = 1-XMg;
    XFe3 = Fe3/Fe;
    
    % Structural Formula:
    
    % T2:
    if Si <= 4
        Al_T2 = 4-Si;
    else
        Al_T2 = 0;
    end
    
    % M4:
    if Si <= 3
        Al_M4 = 1;
        Mg_M4 = 0;
    else
        if Si <= 4
            Al_M4 = 1-(Si-3);
            Mg_M4 = Si-3;
        else
            Al_M4 = 0;
            Mg_M4 = 1;
        end
    end
    
    % M1 & M2M3:
    if Al-Al_T2 > Al_T2
        V_M1 = ((Al-Al_T2)-Al_T2)/2;   % V = (Alvi-Aliv)/2
    else
        V_M1 = 0;
    end
    
    if (2*V_M1 - Fe3) > 0
        Al_M2M3 = 2*V_M1 - Fe3;
    else
        Al_M2M3 = 0;
    end
    
    if (Al-Al_T2) - Al_M4 - Al_M2M3 > 0
        Al_M1 = (Al-Al_T2) - Al_M4 - Al_M2M3;
    else
        Al_M1 = 0;
    end
    
    if Al_M1+V_M1 < 1
        Fe_M1 = XFe * (1-Al_M1-V_M1);
        Mg_M1 = XMg * (1-Al_M1-V_M1);
    else
        Fe_M1 = 0;
        Mg_M1 = 0;
    end
    
    % We fill last M2M3 for FeMg
    
    Fe3_M2M3 = Fe3;
    
    Fe2_M2M3 = Fe2 - Fe_M1;
    Mg_M2M3 = Mg - Mg_M1 - Mg_M4;
    
    Xame = XFe*Al_M1;
    Xfame = XMg*Al_M1;
    Xclc = Mg_M1;
    Xdph = Fe_M1;
    Xfsud = Fe3 / 2;
    Xsud = V_M1 - Xfsud;
    Xafchl = Mg_M4;
    
    Xsum = Xame+Xfame+Xclc+Xdph+Xfsud+Xsud+Xafchl;
    
    OutputData(i,:) = [Si,Al,Fe2,Fe3,XMg,XFe3,Al_T2,Al_M1,Fe_M1,Mg_M1,V_M1,Al_M2M3,Fe2_M2M3,Mg_M2M3,Fe3_M2M3,Al_M4,Mg_M4,Xame,Xfame,Xclc,Xdph,Xsud,Xfsud,Xafchl,Xsum];
    
end

end







