function [Output,Antidote_VARIABLES,handles] = Antidote_5_MapQ(AntidoteRecipes,AntidoteMode,WorkVariXMap,MinimOptions,handles)
%
%

BinPhaseDef = handles.BinPhaseDef(get(handles.BinPopUpVarPhaseList,'Value'));

eval(['TCi = [',get(handles.BinTextBinTCminmax,'String'),'];']);
eval(['Pi = [',get(handles.BinTextBinPminmax,'String'),'];']);

LIMS = [TCi(1)-1,TCi(end)+1,Pi(1)-1,Pi(end)+1];

% Update the live display:
set(handles.axes_Live_PLOT5,'Visible','On','Box','On');
axes(handles.axes_Live_PLOT5), cla, colorbar,
xlabel('Temperature (C)'), ylabel('Pressure (Kbar)')
title('Residuals')
axis(LIMS)
drawnow

BinSet = SetBin(handles);

ESP = 200.*ones(length(Pi),length(TCi));
COMP = zeros(length(Pi),length(TCi));
KeepIt = zeros(length(Pi),length(TCi));

ComptWait = 0;
ComptTEMP = 0;
LimitUpdate = 20;
NbMinimWait = length(TCi)*length(Pi);

UpdateLIVEWaitBar(1,0,handles);

set(handles.Live_Disp1_Text1,'Visible','On','String','>>');
set(handles.Live_Disp1_Text2,'Visible','On','String','Residual P-T map');
set(handles.Live_Disp1_Text3,'Visible','On','String','Computing ...','ForegroundColor',[1,0,0]);

drawnow

%         sTC = 1;
%         sP = 1;

tic
%h = waitbar(0,'The antidote is working, please wait...');
for iTC = 1:length(TCi)
    for iP = 1:length(Pi)
        
        [Res,CompoMod] = OptiBingoPTSinglePhase([1,1],[TCi(iTC),Pi(iP)],LIMS,BinSet,WorkVariXMap,BinPhaseDef);
        
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
    axes(handles.axes_Live_PLOT5);
    imagesc(TCi,Pi,ESP), colorbar, set(gca,'ydir','norm')
    
    if length(find(KeepIt))
        ValueCMin = min(ESP(find(KeepIt)));
        ValueCMax = max(ESP(find(KeepIt)));
    else
        ValueCMin = -100;
        ValueCMax = 0;
    end
    
    caxis([ValueCMin ValueCMax]), colormap([RdYlBu(64);0,0,0]);
    %                     [sP(iTC),sTC(iTC)] = find(ESP==min(ESP(:)));
    %                     hold on
    %                     plot(TCi(sTC),Pi(sP),'-w'),
    %                     plot(TCi(sTC(end)),Pi(sP(end)),'.w');
    xlabel('Temperature (C)'), ylabel('Pressure (Kbar)')
    set(handles.axes_Live_PLOT5,'Box','On')
    title('Residuals')
    
    UpdateLIVEWaitBar(1,ComptWait/NbMinimWait,handles);
    set(handles.Live_Disp1_Text3,'Visible','On','String',['Computing ',CodeDispTime])
    
    drawnow
    
end
toc

UpdateLIVEWaitBar(1,1,handles);


set(handles.Live_Disp1_Text1,'Visible','On','String','>>');
set(handles.Live_Disp1_Text2,'Visible','On','String','Residual P-T map');
set(handles.Live_Disp1_Text3,'Visible','On','String','Done','ForegroundColor',[0,0,0]);

% Update display
axes(handles.axes_Live_PLOT5);
imagesc(TCi,Pi,ESP), colorbar, set(gca,'ydir','norm')
ValueCMin = min(ESP(find(KeepIt)));
ValueCMax = max(ESP(find(KeepIt)));
caxis([ValueCMin ValueCMax]), colormap([RdYlBu(64);0,0,0]);
xlabel('Temperature (C)'), ylabel('Pressure (Kbar)')
set(handles.axes_Live_PLOT5,'Visible','On','Box','On')
title('Residuals')

drawnow


figure, 
imagesc(TCi,Pi,ESP), colorbar, set(gca,'ydir','norm')
ValueCMin = min(ESP(find(KeepIt)));
ValueCMax = max(ESP(find(KeepIt)));
caxis([ValueCMin ValueCMax]), colormap([RdYlBu(64);0,0,0]);
xlabel('Temperature (C)'), ylabel('Pressure (Kbar)')
set(handles.axes_Live_PLOT5,'Visible','On','Box','On')
title('Residuals')

drawnow


figure, hold on
ESP2 = ESP;
TooHigh = find(ESP > 200);
OK = find(ESP < 200);
ESP2(TooHigh) = (max(ESP(OK))+0.2*max(ESP2(OK)))*ones(size(TooHigh));
surf(TCi,Pi,ESP2,'EdgeColor','k'), colorbar, colormap([RdYlBu(64)]);
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
