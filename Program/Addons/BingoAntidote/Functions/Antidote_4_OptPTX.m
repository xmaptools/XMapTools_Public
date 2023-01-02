function [Output,Antidote_VARIABLES] = Antidote_4_OptPTX(WorkVariXMap,MinimOptions,app)
%
%
%

% Load MinimOptions to enable TEST mode
load('MinimOptions.mat');

BinPhaseDef = app.BinPhaseDef(app.SelectedPhaseOptiDropDown.Value);

if ~BinPhaseDef.SelForBA
    uialert(app.BingoAntidote_GUI,'This phase is not selected for BA and cannot be used for optimisation','Bingo-Antidote – Error');
    Output.WeCallBingo = 0;
    Antidote_VARIABLES = [];
    return
end

Tmin = app.TminEditField.Value;
Tmax = app.TmaxEditField.Value;
Pmin = app.PminEditField.Value;
Pmax = app.PmaxEditField.Value;
Res = app.AntidoteGridresolutionEditField.Value;

Ti = [Tmin:(Tmax-Tmin)/(Res-1):Tmax];
Pi = [Pmin:(Pmax-Pmin)/(Res-1):Pmax];

LIMS = [Ti(1),Ti(end),Pi(1),Pi(end)];

[BinSet] = SetBin(app);

app.Report_Antidote{end+1} = ['Antidote: Recipe [4] - Find Optimal P-T (single phase)'];
app.Report_Antidote{end+1} = ['Bulk: ',BinSet.Bulk2Display];
app.Report_Antidote{end+1} = ['Database: ',BinSet.Database];
app.Report_Antidote{end+1} = '';
app.Text_Report_Antidote.Value = app.Report_Antidote;


if isequal(MinimOptions.Search.Symplex.FirstOpt,1) % Preliminary P-T input
    app.Report_Antidote{end+1} = ['##### Exploratory P-T scanning (',num2str(Res),' x ',num2str(Res),') #####'];
    app.Report_Antidote{end+1} = '';
    app.Text_Report_Antidote.Value = app.Report_Antidote;
    
    Compt = 0;
    Compt2 = 0; LimDisp = 5;
    
    ESP = zeros(length(Pi),length(Ti));
    
    for iTC = 1:length(Ti)
        for iP = 1:length(Pi)
            [Res] = OptiBingoPTSinglePhase([1,1],[Ti(iTC),Pi(iP)],LIMS,BinSet,WorkVariXMap,BinPhaseDef,app);
            if Res < 150
                ESP(iP,iTC) = Res;
            else
                ESP(iP,iTC) = 0;
            end
        end
        imagesc(app.UIAxes_LiveAntidote1,Ti,Pi,ESP);
        app.UIAxes_LiveAntidote1.YDir = 'normal';
        colormap(app.UIAxes_LiveAntidote1,[RdYlBu(64);0,0,0]);
        colorbar(app.UIAxes_LiveAntidote1)
        xlabel(app.UIAxes_LiveAntidote1,'Temperature (°C)');
        ylabel(app.UIAxes_LiveAntidote1,'Pressure (GPa)');
        title(app.UIAxes_LiveAntidote1,'Objective function (-Q_{cmp})');
        hold(app.UIAxes_LiveAntidote1,'on')
        
        ShiftVal = 1;  % (in % of Qcmp)
        if max(ESP(:))+ShiftVal > 0
            MaxValuePlot = 0;
        else
            MaxValuePlot = max(ESP(:))+ShiftVal;
        end
        caxis(app.UIAxes_LiveAntidote1,[min(ESP(:))-ShiftVal MaxValuePlot])
        
        drawnow
    end
    ht1 = toc;
    
    ValueMin = min(ESP(:));
    [sP,sTC] = find(ESP==ValueMin);
    
    if length(sP) > 1 && length(sTC) >1 && ValueMin <= -100
        
        app.Report_Antidote{end+1} = [' ** WARNING **'];
        app.Report_Antidote{end+1} = [' The minimum is NON-UNIQUE as the bottom of the objective function is flat'];
        app.Report_Antidote{end+1} = [' with ',num2str(length(sP)),' pixels having a value of ',num2str(ValueMin)];
        app.Report_Antidote{end+1} = [' -> a random selection has been made and Antidote will not converge'];
        app.Report_Antidote{end+1} = [' in this flat region (non-unique minimum)'];
        app.Report_Antidote{end+1} = '';
        app.Text_Report_Antidote.Value = app.Report_Antidote;
        
        WeTake = floor(1+rand(1)*length(sTC));
        
        plot(app.UIAxes_LiveAntidote1,Ti(sTC),Pi(sP),'om')
        plot(app.UIAxes_LiveAntidote1,Ti(sTC(WeTake)),Pi(sP(WeTake)),'*m')
        drawnow
        
        X0 = [round(Ti(sTC(WeTake))),Pi(sP(WeTake))];
        
        
    elseif length(sP) > 1 && length(sTC)>1
        % Starting point from Bingo...
        
        app.Report_Antidote{end+1} = ['THE SELECTED PHASE IS NOT STABLE in the PT range -> Let''s try with P-T from Bingo '];
        app.Report_Antidote{end+1} = '';
        app.Text_Report_Antidote.Value = app.Report_Antidote;
        
        X0 = [app.BingoTemperatureEditField.Value,app.BingoPressureEditField.Value];
        
    else
        
        app.Report_Antidote{end+1} = ['RESULTS (Exploratory P-T scanning)'];
        app.Report_Antidote{end+1} = ['X0(1) = ',num2str(Pi(sP)),' (P,GPa)'];
        app.Report_Antidote{end+1} = ['X0(2) = ',num2str(Ti(sTC)),' (T,°C)'];
        app.Report_Antidote{end+1} = '';
        app.Text_Report_Antidote.Value = app.Report_Antidote;
       
        plot(app.UIAxes_LiveAntidote1,Ti(sTC),Pi(sP),'*m')
        drawnow
        
        X0 = [Ti(sTC),Pi(sP)];
    end
    
    app.Report_Antidote{end+1} = ['CPU time: ',num2str(ht1),' s'];
    app.Report_Antidote{end+1} = '';
    app.Text_Report_Antidote.Value = app.Report_Antidote;
    
else
    X0 = [app.BingoTemperatureEditField.Value,app.BingoPressureEditField.Value];
end

options = optimset('fminsearch'); options=optimset(options,'TolX',0.0001,'TolFun',0.0001,'display','iter','MaxFunEvals',300,'MaxIter',100);

NORM = X0;
X0 = X0./NORM;

app.Report_Antidote{end+1} = ['##### P-T Optimization (from: ',num2str(NORM(2)),' GPa; ',num2str(NORM(1)),' C) #####'];
app.Report_Antidote{end+1} = '';
app.Text_Report_Antidote.Value = app.Report_Antidote;

pause(0.1)
scroll(app.Text_Report_Antidote,'bottom');

tic
f = @OptiBingoPTSinglePhase;
[Result,Res] = fminsearch(f, X0,options,NORM,LIMS,BinSet,WorkVariXMap,BinPhaseDef,app);
ht2 = toc;

TCf = Result(1)*NORM(1);
Pf = Result(2)*NORM(2);

app.Report_Antidote{end+1} = ['P = ',num2str(Pf),' GPa'];
app.Report_Antidote{end+1} = ['T = ',num2str(TCf),' °C'];
app.Report_Antidote{end+1} = '';
app.Report_Antidote{end+1} = ['CPU time: ',num2str(ht2),' s'];
app.Report_Antidote{end+1} = '';
app.Text_Report_Antidote.Value = app.Report_Antidote;

ESP = Res;

app.BingoTemperatureEditField.Value = TCf;
app.BingoPressureEditField.Value = Pf;

Output.WeCallBingo = 1;
Output.WeSaveWorkspace = 1;
Output.Message = 'Success';

pause(0.1)
scroll(app.Text_Report_Antidote,'bottom');

w = whos;
for a = 1:length(w)
    if ~isequal(w(a).name,'eventdata') && ~isequal(w(a).name,'hObject') && ~isequal(w(a).name,'handles')
        Antidote_VARIABLES.(w(a).name) = eval(w(a).name); 
    end
end


return