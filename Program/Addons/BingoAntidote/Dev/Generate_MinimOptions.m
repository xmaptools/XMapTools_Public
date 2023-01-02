function [] = Generate_MinimOptions()
% Use this function to generate a default MinimOptions.mat file
% 
% Pierre Lanari (01.07.16)
%

disp(' ')
disp('  >> Generate the default file MinimOptions.mat')


WhereWeSave = which('Generate_MinimOptions.m');

WhereWeSave = [WhereWeSave(1:end-23),'MinimOptions.mat'];


% -------------------------------------------------------------------------
% Weighting: 
MinimOptions.Weights.Use = 0;
MinimOptions.Weights.Selected = 1;

MinimOptions.Weights.ListAutoWts{1} = 'Assemblage + Modes';
MinimOptions.Weights.Values(1,:) = [0.5,0.5,0];

MinimOptions.Weights.ListAutoWts{end+1} = 'Assemblage + Composition';
MinimOptions.Weights.Values(end+1,:) = [0.5,0.0,0.5];

MinimOptions.Weights.ListAutoWts{end+1} = 'Modes';
MinimOptions.Weights.Values(end+1,:) = [0,1,0];

MinimOptions.Weights.ListAutoWts{end+1} = 'Compositions';
MinimOptions.Weights.Values(end+1,:) = [0,0,1];

MinimOptions.Weights.ListAutoWts{end+1} = 'Mean of all';
MinimOptions.Weights.Values(end+1,:) = [0.33,0.33,0.33];

% -------------------------------------------------------------------------
% Search method: 
MinimOptions.Search.MethList{1} = 'Symplex Method (fminsearch)';
MinimOptions.Search.Symplex.FirstOpt = 1;
MinimOptions.Search.Symplex.NbSteps = '10';

% -------------------------------------------------------------------------
% P-T uncertainties:
MinimOptions.Uncertainty.NbPerm1 = 50;
MinimOptions.Uncertainty.NbPerm2 = 10;

% -------------------------------------------------------------------------
% TEST mode:
MinimOptions.TestMode = 0;


% SAVE: 

save(WhereWeSave,'MinimOptions'); 

disp('  >> Done (new file saved)')
disp(' ')
return