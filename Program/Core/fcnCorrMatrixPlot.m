
function [Rho P] = fcnCorrMatrixPlot(A, VarName, caption)
% This function produces a correlation matrix scatterplot with
% least-squared fitted lines.
% Input A is a mxn matrix (m: # of observations, n: # of variables).
% VarName is a cell array with variable names correspond to columns of A.
% Values displayed in @ subplot are r-value [p-value] N(sample size).
% Output: Correlation coefficients (Rho) and the associated p-values (P).
% Change values of gap, marg_x, & marg_y to adjust spacing of subplots.
% In the main program, use orient tall/landscape & saveas to save the plot.
% Example:
% figTitle = 'An example of correlation matrix scatterplots';
% Var ={'Age' 'Height' 'GaitSpd' 'StrLgn' 'Cadence'};
% data = [58 76 136 164 100; 24 63 129 126 123; 29 70 118 146 97;...
% 21 69 101 123 99; 49 69 135 153 106; 47 NaN 126 148 101;...
% 58 69 132 146 108; 31 72 125 146 103];
% fcnCorrMatrixPlot(data, Var, figTitle);
% FigFile = 'SamplePlots'; orient tall
% saveas(gcf, FigFile,'png')
% John W. Chow, jchow@mmrcrehab.org, September 16, 2015
% Methodist Rehabilitation Center, Jackson, Mississippi, USA
% Created using R2015a
% R1: (1) Ignore data pairs with NaNs. (2) Sample size added on plots.
%     (3) No statistics toolbox needed. (May 2020)
scrsz = get(0,'ScreenSize');
[m,n] = size(A);   % # of rows and columns of the input matrix
if n ~= length(VarName)
 disp('Warning: # of variables & # of columns not matched.'), pause
end
% Define a figure window position and size (full screen)
figure('Position',[.05*scrsz(3) .05*scrsz(4) .9*scrsz(3) .87*scrsz(4)]);
% Adjust marker and font sizes according to the # of columns
if n < 14
 gap_ht = .0195-.0015*n;          % Gap height between adjacent subplots
 gap_wd = .0195-.0015*n;          % Gap width between adjacent subplots
 symbolsize = 8.909-.4545*n;      % Marker size
 labelsize = 12.909-.5*n;         % Font size for text and ticklabel
else     % when # of variables >= 14
 gap_ht = 0; gap_wd = 0;
 symbolsize = 3;
 labelsize = 6;
end
%% Create subplot axes %%
% Modification of the function 'tight_subplot' by Pekka Kumpulainen
% www.mathworks.com/matlabcentral/fileexchange/27991-tight-subplot
gap = [gap_ht gap_wd];  % Gap height and width between adjacent subplots
marg_x = [.1 .05];      % Left and right margins of the figure
marg_y = [.1 .05];      % Bottom and top margins of the figure
% Determine the axis lengths of each subplot %
yAxLg = (1-sum(marg_y)-(n-2)*gap(1))/(n-1);
xAxLg = (1-sum(marg_x)-(n-2)*gap(2))/(n-1);
px = marg_x(1);    % x-origin of subplots in the 1st column
ha = zeros((n-2)*(n-2),1);   % Define an array of handles
counter = 0;
for i = 1:n-1      % # of columns of subplots = (n-1)
 
 py = 1-marg_y(2)-yAxLg;     % y-origin of the subplot in the 1st row
 
 if i > counter
  for j = 2:n      % # of rows of subplots
   if j > counter + 1
    pos = (i-1)+(j-2)*(n-1)+1;    % Subplot position (ID)
    ha(pos) = axes('Units','normalized','Position',[px py xAxLg yAxLg],...
     'XTickLabel','','YTickLabel','');
   end
   py = py-yAxLg-gap(1);     % y-origin of the next subplot in the same column
   if j == n
    counter = counter + 1;
    px = px+xAxLg+gap(2);    % x-origin of subplots in the next column
   end
  end    % j-loop for rows of subplot
  
 end     % if i > counter
 
end      % i-loop for columns of subplot
% End creating subplot axes
var = VarName;
%% Create scatterplots with fitted lines from top to bottom %%
counter = 0;
for i = 1:n-1      % Loop for rows
 if i > counter
  for j = 2:n      % Loop for columns
   if j > counter+1
    
    X = A(:,i); Y = A(:,j);       % Data to be plotted
    
    % Remove data pairs with missing data %
    for k = m:-1:1
     if isnan(X(k)) == 1 | isnan(Y(k)) == 1
      X(k) = [];  Y(k) = [];
     end
    end
    N = length(X);   % Number of data pairs to analyze
    
    [rho,p] = corrcoef(X,Y); % correlation coefficient & associated p-value
    Rho(i,j) = rho(2,1);     % Rho: accumulated r-values
    P(i,j) = p(2,1);         % P: accumulated p-values 
    
    loc = (i-1)+(j-2)*(n-1)+1;    % Subplot location
    axes(ha(loc));
    plot(X,Y,'o','MarkerSize',symbolsize);  % Marker shape: circle
    
    % Plot a least-squared fitted line %
    Po = polyfit(X,Y,1);     % 1st order polynomial (linear)
    Yfit = Po(1)*X+Po(2);    % y-values of the fitted line
    hold;                    % Allow superimposed plots
    plot(X,Yfit,'r-');       % Fitted line (color: red, type: solid)
    hold;
    
    % Define the ranges for the x- and y-axis %
    axis([min(X)-.1*(max(X)-min(X)) max(X)+.1*(max(X)-min(X))... 
     min(Y)-.1*(max(Y)-min(Y)) max(Y)+.25*(max(Y)-min(Y))]);
    xLimit = get(gca, 'xlim');
    xLoc = xLimit(1) + .05*(max(xLimit)-min(xLimit));   % x location for text
    yLimit = get(gca, 'ylim');
    yLoc = yLimit(1) + .925*(max(yLimit)-min(yLimit));   % y location for text
    
    % Add corr coef, p-value, and sample size to each plot %
    text(xLoc,yLoc,[num2str(Rho(i,j),'%.3f')...
     '[' num2str(P(i,j),'%.3f') ']' int2str(N)],'FontSize',labelsize);
    
    % Add ylabels to the subplots in the first column %
    if i == 1      
     ylabel(regexprep(var(j), '_', '-'),'FontSize',labelsize+2);
     ax = gca;
     ax.FontSize = labelsize;     % Ticklabel size
    else
     set(gca, 'YTickLabel','');
    end
    
    % Add xlabels to the subplots in the last row %
    if j == n      
     xlabel(regexprep(var(i), '_', '-'),'FontSize',labelsize+2);
     ax = gca;
     ax.FontSize = labelsize;
    else
     set(gca, 'XTickLabel','');
    end
    
    % Print caption (and other texts in the empty space if needed)
    if loc == 1
     text(xLoc+1.2*(max(X)-min(X)),yLoc,regexprep(caption, '_', '-'),...
      'FontSize',labelsize+2);
    end
    
    clear X Y* Po
    
   end   % if j > counter+1
   
   if j == n
    counter = counter+1;
   end   
   
  end    % j-loop for columns
 end     % if i > counter
end      % i-loop for rows
% Display accumulated r- and p-values
Rho
P
end
