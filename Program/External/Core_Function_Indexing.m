function [Functions] = Core_Function_Indexing()
%
% This function contains all the definitions for the external functions
% provided with XMapTools 4 and more recent releases.
%
% Each function can be added in one or more mineral group as:
%
%     --------------------------------------------------------------------
%       Abbrev.     Type                    Apply to    Comment
%     --------------------------------------------------------------------
%       SF          Structural Formula      Map
%       TB          Thermobarometers        Map     
%       ME          Multi-equilbirium       Spots       NbMin, AddVar, Def
%     --------------------------------------------------------------------
%
%
% WARNING: All minerals must have at least 1 structural formula function
% because this is how the mineral names are indexed! 
%
%
% Last change PL 15.11.2021

Functions.Mineral = '';



%% ALUMINOSILICATE
Min = length(Functions.Mineral)+1;
Functions.Mineral{Min} = 'Aluminosilicate';

% -------------------------------------------------------------------------
% Structural Formulas
Functions.Min(Min).SF = [];

Fct = length(Functions.Min(Min).SF)+1;
Functions.Min(Min).SF.Name{Fct} = 'AlSil. (SF, 5-Ox. basis)';
Functions.Min(Min).SF.FileName{Fct} = 'StructFctAluminosilicate'; 

% -------------------------------------------------------------------------
% Thermobarometry
Functions.Min(Min).TB = [];

% -------------------------------------------------------------------------
% Multi-equilibrium thermobarometry
Functions.Min(Min).ME = []; 
Functions.Min(Min).ME.Names = {}; 




%% AMPHIBOLES
Min = length(Functions.Mineral)+1;
Functions.Mineral{Min} = 'Amphibole';

% -------------------------------------------------------------------------
% Structural Formulas
Functions.Min(Min).SF = [];

Fct = length(Functions.Min(Min).SF)+1;
Functions.Min(Min).SF.Name{Fct} = 'CaAmp (SF, 23-Ox. basis)';
Functions.Min(Min).SF.FileName{Fct} = 'StructFctAmphiboleCa'; 

Fct = length(Functions.Min(Min).SF)+1;
Functions.Min(Min).SF.Name{Fct} = 'CaAmp_Fe3+ (SF, 23-Ox. basis)';
Functions.Min(Min).SF.FileName{Fct} = 'StructFctAmphiboleCa_Fe3'; 

% -------------------------------------------------------------------------
% Thermobarometry
Functions.Min(Min).TB = [];

Fct = length(Functions.Min(Min).TB)+1;
Functions.Min(Min).TB.Name{Fct} = 'T.Amp (all calibrations)';
Functions.Min(Min).TB.FileName{Fct} = 'Amphibole_T_All';
Functions.Min(Min).TB.Details(Fct).AddVar = {'P_kbar','Xab','Xan'};
Functions.Min(Min).TB.Details(Fct).Def = [15,0.85,0.12];

% -------------------------------------------------------------------------
% Multi-equilibrium thermobarometry
Functions.Min(Min).ME = []; 
Functions.Min(Min).ME.Names = {}; 

Fct = length(Functions.Min(Min).ME.Names)+1;
Functions.Min(Min).ME.Names{Fct} = 'T.Grt-Hbl (all calibrations)';
Functions.Min(Min).ME.FileName{Fct} = 'GrtHbl_T_All';
Functions.Min(Min).ME.Details(Fct).Min = {'Grt','Hbl'};
Functions.Min(Min).ME.Details(Fct).AddVar = {};
Functions.Min(Min).ME.Details(Fct).Def = [];




%% BIOTITE 
Min = length(Functions.Mineral)+1;
Functions.Mineral{Min} = 'Biotite';

% -------------------------------------------------------------------------
% Structural Formulas
Functions.Min(Min).SF = [];

Fct = length(Functions.Min(Min).SF)+1;
Functions.Min(Min).SF.Name{Fct} = 'Bt (SF, 11-Ox. basis)';
Functions.Min(Min).SF.FileName{Fct} = 'StructFctBiotite'; 

% -------------------------------------------------------------------------
% Thermobarometry
Functions.Min(Min).TB = [];

Fct = length(Functions.Min(Min).TB)+1;
Functions.Min(Min).TB.Name{Fct} = 'T.Bt (Henry et al. 2005)';
Functions.Min(Min).TB.FileName{Fct} = 'Biotite_T_H05';
Functions.Min(Min).TB.Details(Fct).AddVar = {};
Functions.Min(Min).TB.Details(Fct).Def = [];

% -------------------------------------------------------------------------
% Multi-equilibrium thermobarometry
Functions.Min(Min).ME = []; 
Functions.Min(Min).ME.Names = {};

Fct = length(Functions.Min(Min).ME.Names)+1;
Functions.Min(Min).ME.Names{Fct} = 'T.Grt-Bt (all calibrations)';
Functions.Min(Min).ME.FileName{Fct} = 'GrtBt_T_All';
Functions.Min(Min).ME.Details(Fct).Min = {'Grt','Bt'};
Functions.Min(Min).ME.Details(Fct).AddVar = {'P_kbar'};
Functions.Min(Min).ME.Details(Fct).Def = [5];





%% BRUCITE
Min = length(Functions.Mineral)+1;
Functions.Mineral{Min} = 'Brucite';

% -------------------------------------------------------------------------
% Structural Formulas
Functions.Min(Min).SF = [];

Fct = length(Functions.Min(Min).SF)+1;
Functions.Min(Min).SF.Name{Fct} = 'Brc (SF, 1-Ox. basis)';
Functions.Min(Min).SF.FileName{Fct} = 'StructFctBrucite'; 

% -------------------------------------------------------------------------
% Thermobarometry
Functions.Min(Min).TB = [];

% -------------------------------------------------------------------------
% Multi-equilibrium thermobarometry
Functions.Min(Min).ME = []; 
Functions.Min(Min).ME.Names = {}; 




%% CHLORITE
Min = length(Functions.Mineral)+1;
Functions.Mineral{Min} = 'Chlorite';

% -------------------------------------------------------------------------
% Structural Formulas
Functions.Min(Min).SF = [];

Fct = length(Functions.Min(Min).SF)+1;
Functions.Min(Min).SF.Name{Fct} = 'Chl (SF, 14-Ox. basis)';
Functions.Min(Min).SF.FileName{Fct} = 'StructFctChlorite'; 

% -------------------------------------------------------------------------
% Thermobarometry
Functions.Min(Min).TB = [];

Fct = length(Functions.Min(Min).TB)+1;
Functions.Min(Min).TB.Name{Fct} = 'T.Chl (all calibrations)';
Functions.Min(Min).TB.FileName{Fct} = 'Chlorite_T_All'; 
Functions.Min(Min).TB.Details(Fct).AddVar = {};
Functions.Min(Min).TB.Details(Fct).Def = [];

% -------------------------------------------------------------------------
% Multi-equilibrium thermobarometry
Functions.Min(Min).ME = []; 
Functions.Min(Min).ME.Names = {}; 

Fct = length(Functions.Min(Min).ME.Names)+1;
Functions.Min(Min).ME.Names{Fct} = 'T.Grt-Chl (all calibrations)';
Functions.Min(Min).ME.FileName{Fct} = 'GrtChl_T_All';
Functions.Min(Min).ME.Details(Fct).Min = {'Grt','Chl'};
Functions.Min(Min).ME.Details(Fct).AddVar = {'P_kbar'};
Functions.Min(Min).ME.Details(Fct).Def = [5];





%% CHLORITOID
Min = length(Functions.Mineral)+1;
Functions.Mineral{Min} = 'Chloritoid';

% -------------------------------------------------------------------------
% Structural Formulas
Functions.Min(Min).SF = [];

Fct = length(Functions.Min(Min).SF)+1;
Functions.Min(Min).SF.Name{Fct} = 'Ctd (SF, 12-Ox. basis)';
Functions.Min(Min).SF.FileName{Fct} = 'StructFctChloritoid'; 

% -------------------------------------------------------------------------
% Thermobarometry
Functions.Min(Min).TB = [];

% -------------------------------------------------------------------------
% Multi-equilibrium thermobarometry
Functions.Min(Min).ME = []; 
Functions.Min(Min).ME.Names = {}; 





%% CHROMITE
Min = length(Functions.Mineral)+1;
Functions.Mineral{Min} = 'Chromite';

% -------------------------------------------------------------------------
% Structural Formulas
Functions.Min(Min).SF = [];

Fct = length(Functions.Min(Min).SF)+1;
Functions.Min(Min).SF.Name{Fct} = 'Chr (SF, 32-Ox. basis)';
Functions.Min(Min).SF.FileName{Fct} = 'StructFctChromite'; 

% -------------------------------------------------------------------------
% Thermobarometry
Functions.Min(Min).TB = [];

% -------------------------------------------------------------------------
% Multi-equilibrium thermobarometry
Functions.Min(Min).ME = []; 
Functions.Min(Min).ME.Names = {}; 



%% CLINOPYROXENE
Min = length(Functions.Mineral)+1;
Functions.Mineral{Min} = 'Clinopyroxene';

% -------------------------------------------------------------------------
% Structural Formulas
Functions.Min(Min).SF = [];

Fct = length(Functions.Min(Min).SF)+1;
Functions.Min(Min).SF.Name{Fct} = 'Cpx (SF, 6-Ox. basis)';
Functions.Min(Min).SF.FileName{Fct} = 'StructFctCpx'; 

Fct = length(Functions.Min(Min).SF)+1;
Functions.Min(Min).SF.Name{Fct} = 'Cpx_Fe3+ (SF, 6-Ox. basis)';
Functions.Min(Min).SF.FileName{Fct} = 'StructFctCpx_Fe3'; 

% -------------------------------------------------------------------------
% Thermobarometry
Functions.Min(Min).TB = [];

% -------------------------------------------------------------------------
% Multi-equilibrium thermobarometry
Functions.Min(Min).ME = []; 
Functions.Min(Min).ME.Names = {}; 

Fct = length(Functions.Min(Min).ME.Names)+1;
Functions.Min(Min).ME.Names{Fct} = 'T.Grt-Cpx (all calibrations)';
Functions.Min(Min).ME.FileName{Fct} = 'GrtCpx_T_All';
Functions.Min(Min).ME.Details(Fct).Min = {'Grt','Cpx'};
Functions.Min(Min).ME.Details(Fct).AddVar = {'P_kbar'};
Functions.Min(Min).ME.Details(Fct).Def = [20];





%% CORDIERITE
Min = length(Functions.Mineral)+1;
Functions.Mineral{Min} = 'Cordierite';

% -------------------------------------------------------------------------
% Structural Formulas
Functions.Min(Min).SF = [];

Fct = length(Functions.Min(Min).SF)+1;
Functions.Min(Min).SF.Name{Fct} = 'Crd (SF, 18-Ox. basis)';
Functions.Min(Min).SF.FileName{Fct} = 'StructFctCordierite'; 

% -------------------------------------------------------------------------
% Thermobarometry
Functions.Min(Min).TB = [];

% -------------------------------------------------------------------------
% Multi-equilibrium thermobarometry
Functions.Min(Min).ME = []; 
Functions.Min(Min).ME.Names = {}; 

Fct = length(Functions.Min(Min).ME.Names)+1;
Functions.Min(Min).ME.Names{Fct} = 'T.Grt-Crd (all calibrations)';
Functions.Min(Min).ME.FileName{Fct} = 'GrtCrd_T_All';
Functions.Min(Min).ME.Details(Fct).Min = {'Grt','Crd'};
Functions.Min(Min).ME.Details(Fct).AddVar = {'P_kbar'};
Functions.Min(Min).ME.Details(Fct).Def = [5];






%% EPIDOTE
Min = length(Functions.Mineral)+1;
Functions.Mineral{Min} = 'Epidote';

% -------------------------------------------------------------------------
% Structural Formulas
Functions.Min(Min).SF = [];

Fct = length(Functions.Min(Min).SF)+1;
Functions.Min(Min).SF.Name{Fct} = 'Ep (SF, 12.5-Ox. basis)';
Functions.Min(Min).SF.FileName{Fct} = 'StructFctEpidote'; 

% -------------------------------------------------------------------------
% Thermobarometry
Functions.Min(Min).TB = [];

% -------------------------------------------------------------------------
% Multi-equilibrium thermobarometry
Functions.Min(Min).ME = []; 
Functions.Min(Min).ME.Names = {}; 





%% FELDSPAR
Min = length(Functions.Mineral)+1;
Functions.Mineral{Min} = 'Feldspar';

% -------------------------------------------------------------------------
% Structural Formulas
Functions.Min(Min).SF = [];

Fct = length(Functions.Min(Min).SF)+1;
Functions.Min(Min).SF.Name{Fct} = 'Fsp (SF, 8-Ox. basis)';
Functions.Min(Min).SF.FileName{Fct} = 'StructFctFeldspar'; 

% -------------------------------------------------------------------------
% Thermobarometry
Functions.Min(Min).TB = [];

% -------------------------------------------------------------------------
% Multi-equilibrium thermobarometry
Functions.Min(Min).ME = []; 
Functions.Min(Min).ME.Names = {}; 




%% MAGNETITE
Min = length(Functions.Mineral)+1;
Functions.Mineral{Min} = 'Magnetite';

% -------------------------------------------------------------------------
% Structural Formulas
Functions.Min(Min).SF = [];

Fct = length(Functions.Min(Min).SF)+1;
Functions.Min(Min).SF.Name{Fct} = 'Mag (SF, 4-Ox./3-cat. basis)';
Functions.Min(Min).SF.FileName{Fct} = 'StructFctMagnetite'; 

% -------------------------------------------------------------------------
% Thermobarometry
Functions.Min(Min).TB = [];

% -------------------------------------------------------------------------
% Multi-equilibrium thermobarometry
Functions.Min(Min).ME = []; 
Functions.Min(Min).ME.Names = {}; 





%% GARNET
Min = length(Functions.Mineral)+1;
Functions.Mineral{Min} = 'Garnet';

% -------------------------------------------------------------------------
% Structural Formulas
Functions.Min(Min).SF = [];

Fct = length(Functions.Min(Min).SF)+1;
Functions.Min(Min).SF.Name{Fct} = 'Grt (SF, 12-Ox. basis)';
Functions.Min(Min).SF.FileName{Fct} = 'StructFctGarnet';

Fct = length(Functions.Min(Min).SF)+1;
Functions.Min(Min).SF.Name{Fct} = 'Grt_Fe3+ (SF, 12-Ox. basis)';
Functions.Min(Min).SF.FileName{Fct} = 'StructFctGarnet_Fe3';

% -------------------------------------------------------------------------
% Thermobarometry
Functions.Min(Min).TB = [];

% -------------------------------------------------------------------------
% Multi-equilibrium thermobarometry
Functions.Min(Min).ME = []; 
Functions.Min(Min).ME.Names = {}; 

Fct = length(Functions.Min(Min).ME.Names)+1;
Functions.Min(Min).ME.Names{Fct} = 'T.Grt-Bt (all calibrations)';
Functions.Min(Min).ME.FileName{Fct} = 'GrtBt_T_All'; 
Functions.Min(Min).ME.Details(Fct).Min = {'Grt','Bt'};
Functions.Min(Min).ME.Details(Fct).AddVar = {'P_kbar'};
Functions.Min(Min).ME.Details(Fct).Def = [5];

Fct = length(Functions.Min(Min).ME.Names)+1;
Functions.Min(Min).ME.Names{Fct} = 'T.Grt-Chl (all calibrations)';
Functions.Min(Min).ME.FileName{Fct} = 'GrtChl_T_All';
Functions.Min(Min).ME.Details(Fct).Min = {'Grt','Chl'};
Functions.Min(Min).ME.Details(Fct).AddVar = {'P_kbar'};
Functions.Min(Min).ME.Details(Fct).Def = [5];

Fct = length(Functions.Min(Min).ME.Names)+1;
Functions.Min(Min).ME.Names{Fct} = 'T.Grt-Cpx (all calibrations)';
Functions.Min(Min).ME.FileName{Fct} = 'GrtCpx_T_All';
Functions.Min(Min).ME.Details(Fct).Min = {'Grt','Cpx'};
Functions.Min(Min).ME.Details(Fct).AddVar = {'P_kbar'};
Functions.Min(Min).ME.Details(Fct).Def = [20];

Fct = length(Functions.Min(Min).ME.Names)+1;
Functions.Min(Min).ME.Names{Fct} = 'T.Grt-Crd (all calibrations)';
Functions.Min(Min).ME.FileName{Fct} = 'GrtCrd_T_All';
Functions.Min(Min).ME.Details(Fct).Min = {'Grt','Crd'};
Functions.Min(Min).ME.Details(Fct).AddVar = {'P_kbar'};
Functions.Min(Min).ME.Details(Fct).Def = [5];

Fct = length(Functions.Min(Min).ME.Names)+1;
Functions.Min(Min).ME.Names{Fct} = 'T.Grt-Hbl (all calibrations)';
Functions.Min(Min).ME.FileName{Fct} = 'GrtHbl_T_All';
Functions.Min(Min).ME.Details(Fct).Min = {'Grt','Hbl'};
Functions.Min(Min).ME.Details(Fct).AddVar = {};
Functions.Min(Min).ME.Details(Fct).Def = [];

Fct = length(Functions.Min(Min).ME.Names)+1;
Functions.Min(Min).ME.Names{Fct} = 'T.Grt-Ilm (all calibrations)';
Functions.Min(Min).ME.FileName{Fct} = 'GrtIlm_T_All';
Functions.Min(Min).ME.Details(Fct).Min = {'Grt','Ilm'};
Functions.Min(Min).ME.Details(Fct).AddVar = {};
Functions.Min(Min).ME.Details(Fct).Def = [];




%% ILMENITE
Min = length(Functions.Mineral)+1;
Functions.Mineral{Min} = 'Ilmenite';

% -------------------------------------------------------------------------
% Structural Formulas
Functions.Min(Min).SF = [];

Fct = length(Functions.Min(Min).SF)+1;
Functions.Min(Min).SF.Name{Fct} = 'Ilm (SF, 3-Ox. basis)';
Functions.Min(Min).SF.FileName{Fct} = 'StructFctIlmenite'; 

% -------------------------------------------------------------------------
% Thermobarometry
Functions.Min(Min).TB = [];

% -------------------------------------------------------------------------
% Multi-equilibrium thermobarometry
Functions.Min(Min).ME = []; 
Functions.Min(Min).ME.Names = {}; 

Fct = length(Functions.Min(Min).ME.Names)+1;
Functions.Min(Min).ME.Names{Fct} = 'T.Grt-Ilm (all calibrations)';
Functions.Min(Min).ME.FileName{Fct} = 'GrtIlm_T_All';
Functions.Min(Min).ME.Details(Fct).Min = {'Grt','Ilm'};
Functions.Min(Min).ME.Details(Fct).AddVar = {};
Functions.Min(Min).ME.Details(Fct).Def = [];





%% MONAZITE
Min = length(Functions.Mineral)+1;
Functions.Mineral{Min} = 'Monazite';

% -------------------------------------------------------------------------
% Structural Formulas
Functions.Min(Min).SF = [];

% -------------------------------------------------------------------------
% Thermobarometry
Functions.Min(Min).TB = [];

Fct = length(Functions.Min(Min).TB)+1;
Functions.Min(Min).TB.Name{Fct} = 'Age.Mz (Montel 1996)';
Functions.Min(Min).TB.FileName{Fct} = 'Monazite_Age_M96';
Functions.Min(Min).TB.Details(Fct).AddVar = {};
Functions.Min(Min).TB.Details(Fct).Def = [];

% -------------------------------------------------------------------------
% Multi-equilibrium thermobarometry
Functions.Min(Min).ME = []; 
Functions.Min(Min).ME.Names = {}; 





%% OLIVINE
Min = length(Functions.Mineral)+1;
Functions.Mineral{Min} = 'Olivine';

% -------------------------------------------------------------------------
% Structural Formulas
Functions.Min(Min).SF = [];

Fct = length(Functions.Min(Min).SF)+1;
Functions.Min(Min).SF.Name{Fct} = 'Ol (SF, 4-Ox. basis)';
Functions.Min(Min).SF.FileName{Fct} = 'StructFctOlivine'; 

% -------------------------------------------------------------------------
% Thermobarometry
Functions.Min(Min).TB = [];

% -------------------------------------------------------------------------
% Multi-equilibrium thermobarometry
Functions.Min(Min).ME = []; 
Functions.Min(Min).ME.Names = {}; 





%% ORTHOPYROXENE
Min = length(Functions.Mineral)+1;
Functions.Mineral{Min} = 'Orthopyroxene';

% -------------------------------------------------------------------------
% Structural Formulas
Functions.Min(Min).SF = [];

Fct = length(Functions.Min(Min).SF)+1;
Functions.Min(Min).SF.Name{Fct} = 'Opx (SF, 3-Ox. basis)';
Functions.Min(Min).SF.FileName{Fct} = 'StructFctOpx'; 

% -------------------------------------------------------------------------
% Thermobarometry
Functions.Min(Min).TB = [];

% -------------------------------------------------------------------------
% Multi-equilibrium thermobarometry
Functions.Min(Min).ME = []; 
Functions.Min(Min).ME.Names = {}; 




%% QUARTZ
Min = length(Functions.Mineral)+1;
Functions.Mineral{Min} = 'Quartz';

% -------------------------------------------------------------------------
% Structural Formulas
Functions.Min(Min).SF = [];

% -------------------------------------------------------------------------
% Thermobarometry
Functions.Min(Min).TB = [];

Fct = length(Functions.Min(Min).TB)+1;
Functions.Min(Min).TB.Name{Fct} = 'T.Qz (all calibrations)';
Functions.Min(Min).TB.FileName{Fct} = 'Quartz_T_All';
Functions.Min(Min).TB.Details(Fct).AddVar = {'P_kbar','aTiO2'};
Functions.Min(Min).TB.Details(Fct).Def = [5,1];

% -------------------------------------------------------------------------
% Multi-equilibrium thermobarometry
Functions.Min(Min).ME = []; 
Functions.Min(Min).ME.Names = {}; 





%% RUTILE
Min = length(Functions.Mineral)+1;
Functions.Mineral{Min} = 'Rutile';

% -------------------------------------------------------------------------
% Structural Formulas
Functions.Min(Min).SF = [];

Fct = length(Functions.Min(Min).SF)+1;
Functions.Min(Min).SF.Name{Fct} = 'Rt (SF, 2-Ox. basis)';
Functions.Min(Min).SF.FileName{Fct} = 'StructFctRutile'; 

% -------------------------------------------------------------------------
% Thermobarometry
Functions.Min(Min).TB = [];

Fct = length(Functions.Min(Min).TB)+1;
Functions.Min(Min).TB.Name{Fct} = 'T.Rt (all calibrations)';
Functions.Min(Min).TB.FileName{Fct} = 'Rutile_T_All';
Functions.Min(Min).TB.Details(Fct).AddVar = {'P_kbar','aSiO2'};
Functions.Min(Min).TB.Details(Fct).Def = [5,1];

% -------------------------------------------------------------------------
% Multi-equilibrium thermobarometry
Functions.Min(Min).ME = []; 
Functions.Min(Min).ME.Names = {}; 




%% SERPENTINE
Min = length(Functions.Mineral)+1;
Functions.Mineral{Min} = 'Serpentine';

% -------------------------------------------------------------------------
% Structural Formulas
Functions.Min(Min).SF = [];

Fct = length(Functions.Min(Min).SF)+1;
Functions.Min(Min).SF.Name{Fct} = 'Srp (SF, 14.5-Ox. basis)';
Functions.Min(Min).SF.FileName{Fct} = 'StructFctSerpentine'; 

% -------------------------------------------------------------------------
% Thermobarometry
Functions.Min(Min).TB = [];

% -------------------------------------------------------------------------
% Multi-equilibrium thermobarometry
Functions.Min(Min).ME = []; 
Functions.Min(Min).ME.Names = {}; 





%% SPINEL
Min = length(Functions.Mineral)+1;
Functions.Mineral{Min} = 'Spinel';

% -------------------------------------------------------------------------
% Structural Formulas
Functions.Min(Min).SF = [];

Fct = length(Functions.Min(Min).SF)+1;
Functions.Min(Min).SF.Name{Fct} = 'Spl (SF, 4-Ox. basis)';
Functions.Min(Min).SF.FileName{Fct} = 'StructFctSpinel'; 

% -------------------------------------------------------------------------
% Thermobarometry
Functions.Min(Min).TB = [];

% -------------------------------------------------------------------------
% Multi-equilibrium thermobarometry
Functions.Min(Min).ME = []; 
Functions.Min(Min).ME.Names = {}; 




%% STAUROLITE
Min = length(Functions.Mineral)+1;
Functions.Mineral{Min} = 'Staurolite';

% -------------------------------------------------------------------------
% Structural Formulas
Functions.Min(Min).SF = [];

Fct = length(Functions.Min(Min).SF)+1;
Functions.Min(Min).SF.Name{Fct} = 'St (SF, 22-Ox. basis)'; 
Functions.Min(Min).SF.FileName{Fct} = 'StructFctStaurolite'; 

% -------------------------------------------------------------------------
% Thermobarometry
Functions.Min(Min).TB = [];

% -------------------------------------------------------------------------
% Multi-equilibrium thermobarometry
Functions.Min(Min).ME = []; 
Functions.Min(Min).ME.Names = {}; 




%% TITANITE
Min = length(Functions.Mineral)+1;
Functions.Mineral{Min} = 'Titanite';

% -------------------------------------------------------------------------
% Structural Formulas
Functions.Min(Min).SF = [];

% -------------------------------------------------------------------------
% Thermobarometry
Functions.Min(Min).TB = [];

Fct = length(Functions.Min(Min).TB)+1;
Functions.Min(Min).TB.Name{Fct} = 'T.Ttn (all calibrations)';
Functions.Min(Min).TB.FileName{Fct} = 'Titanite_T_All';
Functions.Min(Min).TB.Details(Fct).AddVar = {'P_kbar','aTiO2','aSiO2'};
Functions.Min(Min).TB.Details(Fct).Def = [5,1,1];

% -------------------------------------------------------------------------
% Multi-equilibrium thermobarometry
Functions.Min(Min).ME = []; 
Functions.Min(Min).ME.Names = {}; 





%% WHITE MICA
Min = length(Functions.Mineral)+1;
Functions.Mineral{Min} = 'White mica';

% -------------------------------------------------------------------------
% Structural Formulas
Functions.Min(Min).SF = [];

Fct = length(Functions.Min(Min).SF)+1;
Functions.Min(Min).SF.Name{Fct} = 'Mica (SF, 11-Ox. basis)';
Functions.Min(Min).SF.FileName{Fct} = 'StructFctWhiteMica';

% -------------------------------------------------------------------------
% Thermobarometry
Functions.Min(Min).TB = [];


% -------------------------------------------------------------------------
% Multi-equilibrium thermobarometry
Functions.Min(Min).ME = []; 
Functions.Min(Min).ME.Names = {}; 





%% ZIRCON
Min = length(Functions.Mineral)+1;
Functions.Mineral{Min} = 'Zircon';

% -------------------------------------------------------------------------
% Structural Formulas
Functions.Min(Min).SF = [];

% -------------------------------------------------------------------------
% Thermobarometry
Functions.Min(Min).TB = [];

Fct = length(Functions.Min(Min).TB)+1;
Functions.Min(Min).TB.Name{Fct} = 'T.Zrn (all calibrations)';
Functions.Min(Min).TB.FileName{Fct} = 'Zircon_T_All';
Functions.Min(Min).TB.Details(Fct).AddVar = {'aTiO2','aSiO2'};
Functions.Min(Min).TB.Details(Fct).Def = [1,1];

% -------------------------------------------------------------------------
% Multi-equilibrium thermobarometry
Functions.Min(Min).ME = []; 
Functions.Min(Min).ME.Names = {}; 














%% Internal variable generation (DO NOT EDIT)

% Version PL 02.09.2021

% -------------------------------------------------------------------------
% Create mineral lists for XMapTools GUI
ComptSF = 0;
ComptTB = 0;
ComptME = 0;
Functions.ItemsSF = {};
Functions.ItemsTB = {};
Functions.ItemsME = {};
Functions.IdxSF = [];
Functions.IdxTB = [];
Functions.IdxME = [];

for i = 1:length(Functions.Mineral)
    if ~isempty(Functions.Min(i).SF)
        ComptSF = ComptSF+1;
        Functions.ItemsSF{ComptSF} = Functions.Mineral{i};
        Functions.IdxSF(ComptSF) = i;
    end
    if ~isempty(Functions.Min(i).TB)
        ComptTB = ComptTB+1;
        Functions.ItemsTB{ComptTB} = Functions.Mineral{i};
        Functions.IdxTB(ComptTB) = i;
    end
    if ~isempty(Functions.Min(i).ME.Names)
        ComptME = ComptME+1;
        Functions.ItemsME{ComptME} = Functions.Mineral{i};
        Functions.IdxME(ComptME) = i;
    end
end
    
   

end
