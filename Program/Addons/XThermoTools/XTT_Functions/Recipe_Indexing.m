function [RecipeIndex,RecipeFunctionDef] = Recipe_Indexing(input)
%
% Version PL (03.03.2020)


% -------------------------------------------------------------------------
% Below, define  at which row each recipe is located in the antidote menu
% (note that the menu is defined in the function Recipe_MenuDef)
% -------------------------------------------------------------------------

RecipeIndex(1) = 2;   % [Recipe 1]   Find optimal P-T(-X) 
RecipeIndex(2) = 3;   % [Recipe 2]   P-T map of Q factors
RecipeIndex(3) = 4;   % [Recipe 3]   P-T uncertrainty 
%
RecipeIndex(4) = 6;   % [Recipe 4]   Find optimal P-T (Single Phase)
RecipeIndex(5) = 7;   % [Recipe 5]   P-T map (Single Phase)
RecipeIndex(6) = 8;   % [Recipe 6]   P-T uncertrainty (Single Phase)
%
RecipeIndex(7) = 10;  % [Recipe 7]   Bulk sensitivity 
RecipeIndex(8) = 11;  % [Recipe 8]   P-T sensitivity 
RecipeIndex(9) = 12;  % [Recipe 9]   P-T-bulk sensitivity
%
RecipeIndex(10) = 14; % [Recipe 10] Floating window (fixed P-T, variable bulk)
RecipeIndex(11) = 15; % [Recipe 11] Scanning window (find optimal P-T, variable bulk)
RecipeIndex(12) = 16; % [Recipe 12] Growing window (find optimal P-T, variable bulk)
RecipeIndex(13) = 17; % [Recipe 13] Chemical potential mapping (fixed P-T)
%
RecipeIndex(14) = 19; % [Recipe 14] Scanning H (fixed P-T)
RecipeIndex(15) = 20; % [Recipe 15] Scanning C (fixed P-T)
RecipeIndex(16) = 21; % [Recipe 16] Scanning O (fixed P-T)
%
RecipeIndex(17) = 23; % [Recipe 17]   Advanced P-T map of Q factors
RecipeIndex(18) = 24; % [Recipe 18]   Advanced Find Optimal P-T (multi-phase)

% -------------------------------------------------------------------------
% Below, define the function name corresponding to each recipe
% -------------------------------------------------------------------------

RecipeFunctionDef{1} = 'Antidote_1_OptPTX';
RecipeFunctionDef{2} = 'Antidote_2_MapQ';
RecipeFunctionDef{3} = 'Antidote_3a6_PTunc';

RecipeFunctionDef{4} = 'Antidote_4_OptPTX';
RecipeFunctionDef{5} = 'Antidote_5_MapQ';
RecipeFunctionDef{6} = 'Antidote_3a6_PTunc';

RecipeFunctionDef{7} = 'Antidote_7a8a9_Unc';
RecipeFunctionDef{8} = 'Antidote_7a8a9_Unc';
RecipeFunctionDef{9} = 'Antidote_7a8a9_Unc';

RecipeFunctionDef{10} = 'Antidote_10_FloatW';
RecipeFunctionDef{11} = 'Antidote_11_ScanW';
RecipeFunctionDef{12} = 'Antidote_12_GrowW';
RecipeFunctionDef{13} = 'Antidote_13_ChemPotMap';

RecipeFunctionDef{14} = 'Antidote_14a15a16_ScanX';
RecipeFunctionDef{15} = 'Antidote_14a15a16_ScanX';
RecipeFunctionDef{16} = 'Antidote_14a15a16_ScanX';

RecipeFunctionDef{17} = 'Antidote_17_MapQ';
RecipeFunctionDef{18} = 'Antidote_18_AdvancedOptPTX';

return