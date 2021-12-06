function [OutputData,OutputVariables] = StructFctSpinel(InputData,InputVariables,ElOxDataDef)
% -
% XMapTools External Function: structural formula of spinel 
%  
%
%  ++09.2021 Compatibility with XMapTools 4
%       - version without loop & Fe2O3 as possible input
%       - Fe3+ estimated using Droop (1987)
%   
%   ----------------------------------------------
%   End-member                  X(1) Y(2)
%   ----------------------------------------------
%           -- Al spinels (AlSpinel) --
%   Hercynite (Hc)              Fe(1)Al(2)O(4)  *
%   Spinel (Spl)                Mg(1)Al(2)O(4)  *
%   Galaxite (Glx)              Mn(1)Al(2)O(4)  *
%   Gahnite (Ghn)               Zn(1)Al(2)O(4)  *
%           -- Cr spinels (CrSpinel) --
%   Chromite (Chr)              Fe(1)Cr(2)O(4)  *  
%   Magnesiochromite (mChr)     Mg(1)Cr(2)O(4)  *  
%   Nichromite (Nic)            Ni(1)Cr(2)O(4) 
%           -- Fe spinels (FeSpinel) --
%   Magnesioferrite (Mfr)       Mg(1)Fe(2)O(4)  *  
%   Magnetite (Mag)             Fe(1)Fe(2)O(4)  *  
%   Ulvospinel (tSpl)           Ti(1)Fe(2)O(4)
%   Jacobsite (Jac)             Mn(1)Fe(2)O(4)  *
%   Franklinite (Frk)           Zn(1)Fe(2)O(4)  *
%   Trevorite (Tre)             Ni(1)Fe(2)O(4) 
%         -- Olivine spinels (OlSpinel) --
%   Ringwoodite (Rwd)           Si(1)Mg(2)O(4)
%   ----------------------------------------------
%   * Calculated assuming equipartition of Fe2+/Mg2+/Mn2+/Zn2+/Ni
%     see code
%
%   Not considered in the calculation:
%   ----------------------------------------------
%   Qandilite (Qnd)             Ti(1)Mg(2)O(4)  
%   ----------------------------------------------
%
%
% 4 Oxygen-basis
%
% P. Lanari - Last update 09.09.2021  *** NOT TESTED ***
% Find out more at https://xmaptools.ch

OutputVariables = {'Al','Fe3','Cr','Fe2','Mg','Zn','Mn','Zn','Si','SumCat_X','SumCat_Y','XAlSpinel','XCrSpinel','XFeSpinel','XOlSpinel','Xhc','Xspl','Xglx','Xghn','Xchr','Xmchr','Xnic','Xmfr','Xmag','Xtspl','Xjac','Xfrk','Xtre','Xrwd','Xsum'};

OutputData = zeros(size(InputData,1),length(OutputVariables));

% General structural formula function for 4 oxygen
WhereMin = find(sum(InputData,2) > 50);
[MatrixSF,ElementsList] = SF_OxNorm(InputData(WhereMin,:),InputVariables,4,ElOxDataDef);

% Fe3+ approximation (Droop 1987)
Fe = MatrixSF(:,4)+MatrixSF(:,5);

Fe3_Droop = 2*4*(1-3./sum(MatrixSF,2));
ZeroFe3 = find(Fe3_Droop < 0);
Fe3_Droop(ZeroFe3) = zeros(size(ZeroFe3));
XFe3 = Fe3_Droop./Fe;
OneXFe3 = find(XFe3 > 1);
XFe3(OneXFe3) = ones(size(OneXFe3));

InputDataCorr = InputData; 

InputDataCorr(WhereMin,4) = InputData(WhereMin,4) .* (1-XFe3);
InputDataCorr(WhereMin,5) = InputData(WhereMin,4) .* XFe3 .* (1/0.89992485);


% General structural formula function for 12 oxygen including FeO and Fe2O3
[MatrixSF2,ElementsList] = SF_OxNorm(InputDataCorr(WhereMin,:),InputVariables,4,ElOxDataDef);

Si = MatrixSF2(:,1);
Ti = MatrixSF2(:,2);
Al = MatrixSF2(:,3);
Fe2 = MatrixSF2(:,4);
Fe3 = MatrixSF2(:,5);
Mn = MatrixSF2(:,6);
Mg = MatrixSF2(:,7);
Ca = MatrixSF2(:,8);
Na = MatrixSF2(:,9);
K = MatrixSF2(:,10);
Cr = MatrixSF2(:,11);
Ni = MatrixSF2(:,12);
Zn = MatrixSF2(:,13);

Mg_Y = Si;
Mg_X = Mg-Mg_Y;

negMg_X = find(Mg_X < 0);
Mg_Y(negMg_X) = Si(negMg_X) + Mg_X(negMg_X); % correction
Mg_X(negMg_X) = zeros(size(negMg_X));

Xrwd = Mg_Y;

Sum_Y = Al+Cr+Fe3+Mg_Y;

SumCat_X = Fe2+Mg_X+Mn+Zn+Ti+Ni;
SumCat_Y = Sum_Y;

XAlSpinel = Al./Sum_Y;
XCrSpinel = Cr./Sum_Y; 
XFeSpinel = Fe3./Sum_Y; 

XOlSpinel = Xrwd;

%Al-spinel
XFe = Fe2./(Fe2+Mg_X+Mn+Zn);
XMg = Mg_X./(Fe2+Mg_X+Mn+Zn);
XMn = Mn./(Fe2+Mg_X+Mn+Zn);
XZn = Zn./(Fe2+Mg_X+Mn+Zn);

Xhc = XAlSpinel.*XFe;
Xspl = XAlSpinel.*XMg;
Xglx = XAlSpinel.*XMn;
Xghn = XAlSpinel.*XZn;

%Cr-spinel
XFe = Fe2./(Fe2+Mg_X+Ni);
XMg = Mg_X./(Fe2+Mg_X+Ni);
XNi = Ni./(Fe2+Mg_X+Ni);

Xchr = XCrSpinel.*XFe;
Xmchr = XCrSpinel.*XMg;
Xnic = XCrSpinel.*XNi;

%Fe-spinel
XFe = Fe2./(Fe2+Mg_X+Mn+Zn+Ni) .* (1-Ti);
XMg = Mg_X./(Fe2+Mg_X+Mn+Zn+Ni) .* (1-Ti);
XMn = Mn./(Fe2+Mg_X+Mn+Zn+Ni) .* (1-Ti);
XZn = Zn./(Fe2+Mg_X+Mn+Zn+Ni) .* (1-Ti);
XNi = Ni./(Fe2+Mg_X+Mn+Zn+Ni) .* (1-Ti);

Xmfr = XFeSpinel.*XMg;
Xmag = XFeSpinel.*XFe;
Xtspl = Ti;
Xjac = XFeSpinel.*XMn;
Xfrk = XFeSpinel.*XZn;
Xtre = XFeSpinel.*XNi;

Xsum = Xhc+Xspl+Xglx+Xghn+Xchr+Xmchr+Xnic+Xmfr+Xmag+Xtspl+Xjac+Xfrk+Xtre+Xrwd;

OutputData(WhereMin,:) = [Al,Fe3,Cr,Fe2,Mg,Zn,Mn,Zn,Si,SumCat_X,SumCat_Y,XAlSpinel,XCrSpinel,XFeSpinel,XOlSpinel,Xhc,Xspl,Xglx,Xghn,Xchr,Xmchr,Xnic,Xmfr,Xmag,Xtspl,Xjac,Xfrk,Xtre,Xrwd,Xsum];

end







