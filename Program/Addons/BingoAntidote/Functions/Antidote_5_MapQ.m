function [Output,Antidote_VARIABLES] = Antidote_5_MapQ(WorkVariXMap,MinimOptions,app)
%
%

% Load MinimOptions to enable TEST mode
load('MinimOptions.mat');

Tmin = app.TminEditField.Value;
Tmax = app.TmaxEditField.Value;
Pmin = app.PminEditField.Value;
Pmax = app.PmaxEditField.Value;

Tstep = app.TstepEditField.Value;
Pstep = app.PstepEditField.Value;

Ti = [Tmin:Tstep:Tmax];
Pi = [Pmin:Pstep:Pmax];

LIMS = [Ti(1),Ti(end),Pi(1),Pi(end)];

BinPhaseDef = app.BinPhaseDef(app.SelectedPhaseOptiDropDown.Value);

[BinSet] = SetBin(app);

app.Report_Antidote{end+1} = ['Antidote: RECIPE 5 – P–T map of Q factors (single phase)'];
app.Report_Antidote{end+1} = ['Bulk: ',BinSet.Bulk2Display];
app.Report_Antidote{end+1} = ['Database: ',BinSet.Database];
app.Text_Report_Antidote.Value = app.Report_Antidote;

cla(app.UIAxes_LiveAntidote1,'reset')
xlabel(app.UIAxes_LiveAntidote1,'Temperature (°C)');
ylabel(app.UIAxes_LiveAntidote1,'Pressure (GPa)');
title(app.UIAxes_LiveAntidote1,'Residuals');
axis(app.UIAxes_LiveAntidote1,LIMS)
drawnow

ESP = 200.*ones(length(Pi),length(Ti));
COMP = zeros(length(Pi),length(Ti));
KeepIt = zeros(length(Pi),length(Ti));

ComptWait = 0;
ComptTEMP = 0;
LimitUpdate = 20;
NbMinimWait = length(Ti)*length(Pi);

tic
for iTC = 1:length(Ti)
    for iP = 1:length(Pi)
        
        [Res,CompoMod] = OptiBingoPTSinglePhase([1,1],[Ti(iTC),Pi(iP)],LIMS,BinSet,WorkVariXMap,BinPhaseDef,app);
        
        ESP(iP,iTC) = Res;
        COMP(iP,iTC) = CompoMod(1);
        
        if Res < 1e10
            KeepIt(iP,iTC) = 1;
        end

    end
    
    TimeSec = toc;
    ComptWait = ComptWait+1;
    
    TimePerAnal = TimeSec/ComptWait;
    TotalTime = TimePerAnal*NbMinimWait - TimeSec;
    
    if TotalTime  > 60
        if TotalTime >3600
            Totalh = floor(TotalTime/3600);
            Totalm = round((TotalTime/3600 - Totalh)*60);
            CodeDispTime = ['(~',num2str(Totalh),'h',num2str(Totalm),')'];
            
        else
            CodeDispTime = ['(~',num2str(round(TotalTime/60)),'m)'];
        end
    else
        CodeDispTime = ['(~',num2str(round(TotalTime)),'s)'];
    end
    
    
    % Update display
    imagesc(app.UIAxes_LiveAntidote1,Ti,Pi,ESP);
    app.UIAxes_LiveAntidote1.YDir = 'normal';
    xlabel(app.UIAxes_LiveAntidote1,'Temperature (°C)');
    ylabel(app.UIAxes_LiveAntidote1,'Pressure (GPa)');
    title(app.UIAxes_LiveAntidote1,'Residuals');
    axis(app.UIAxes_LiveAntidote1,LIMS)
    
    if length(find(KeepIt))
        ValueCMin = min(ESP(find(KeepIt)));
        ValueCMax = max(ESP(find(KeepIt)))+1e-10;
    else
        ValueCMin = -100;
        ValueCMax = 0;
    end
    
    caxis(app.UIAxes_LiveAntidote1,[ValueCMin ValueCMax])
    colormap(app.UIAxes_LiveAntidote1,[RdYlBu(64);0,0,0]);
    colorbar(app.UIAxes_LiveAntidote1)    
    drawnow
    
end
toc


% Update display
imagesc(app.UIAxes_LiveAntidote1,Ti,Pi,ESP)
colorbar(app.UIAxes_LiveAntidote1)
app.UIAxes_LiveAntidote1.YDir = 'normal';
ValueCMin = min(ESP(find(KeepIt)));
ValueCMax = max(ESP(find(KeepIt)));
caxis(app.UIAxes_LiveAntidote1,[ValueCMin ValueCMax])
colormap(app.UIAxes_LiveAntidote1,[RdYlBu(64);0,0,0]);
xlabel(app.UIAxes_LiveAntidote1,'Temperature (C)')
ylabel(app.UIAxes_LiveAntidote1,'Pressure (Kbar)')
title(app.UIAxes_LiveAntidote1,'Residuals')
drawnow


figure, 
imagesc(Ti,Pi,ESP), colorbar, set(gca,'ydir','norm')
ValueCMin = min(ESP(find(KeepIt)));
ValueCMax = max(ESP(find(KeepIt)));
caxis([ValueCMin ValueCMax]), colormap([RdYlBu(64);0,0,0]);
xlabel('Temperature (C)'), ylabel('Pressure (Kbar)')
title('Residuals')
drawnow


figure, hold on
ESP2 = ESP;
TooHigh = find(ESP > 200);
OK = find(ESP < 200);
ESP2(TooHigh) = (max(ESP(OK))+0.2*max(ESP2(OK)))*ones(size(TooHigh));
surf(Ti,Pi,ESP2,'EdgeColor','k'), colorbar, colormap([RdYlBu(64)]);
%colormap(jet)
title('ESP')
colorbar
grid on
drawnow

Output.WeCallBingo = 0;
Output.WeSaveWorkspace = 0;
Output.Message = 'Success';

Antidote_VARIABLES = [];

return
