function [Output,Antidote_VARIABLES,handles] = Antidote_2_MapQ(AntidoteRecipes,AntidoteMode,WorkVariXMap,MinimOptions,handles)
%
%

eval(['TCi = [',get(handles.BinTextBinTCminmax,'String'),'];']);
eval(['Pi = [',get(handles.BinTextBinPminmax,'String'),'];']);

LIMS = [TCi(1)-1,TCi(end)+1,Pi(1)-1,Pi(end)+1];

% Update the live display:
set(handles.axes_Live_PLOT5,'Visible','On');
axes(handles.axes_Live_PLOT5), cla, colorbar,
xlabel('Temperature (C)'), ylabel('Pressure (Kbar)')
title('Objective function')
axis(LIMS)
drawnow

BinSet = SetBin(handles);

if MinimOptions.Weights.Use
    fprintf('%s\t%s\t%s\n','Equation:','Other',['[E4 = -(',num2str(MinimOptions.Weights.Values(MinimOptions.Weights.Selected,1)),'*E1 + ',num2str(MinimOptions.Weights.Values(MinimOptions.Weights.Selected,2)),'*E2 + ',num2str(MinimOptions.Weights.Values(MinimOptions.Weights.Selected,3)),'*E3)]']);
else
    fprintf('%s\t%s\t%s\n','Equation:','Classic','[E4 = -1/3*(E1 + (E1/100)*E2 + (E1/100)*(E2/100)*E3)]');
end
disp(' ')

%E4 = 200.*ones(length(Pi),length(TCi));
tic

E1 = nan(length(Pi),length(TCi));
E2 = nan(length(Pi),length(TCi));
E3 = nan(length(Pi),length(TCi));
E4 = nan(length(Pi),length(TCi));

ComptWait = 0;
NbMinimWait = length(Pi);

UpdateLIVEWaitBar(1,0,handles);

set(handles.Live_Disp1_Text1,'Visible','On','String','>>');
set(handles.Live_Disp1_Text2,'Visible','On','String','Objective function P-T map');
set(handles.Live_Disp1_Text3,'Visible','On','String','Computing ...','ForegroundColor',[1,0,0]);

drawnow

% New PL - (13.02.2017) ** SWOT **
% Create MapData to store all the information of mapping
MapData.QF.Q1 = zeros(length(Pi),length(TCi));
MapData.QF.Q2 = zeros(length(Pi),length(TCi));
MapData.QF.Q3 = zeros(length(Pi),length(TCi));
MapData.QF.Qt = zeros(length(Pi),length(TCi));

MapData.MinProp.ListMin = '';
MapData.MinProp.MapMatrix = zeros(length(Pi),length(TCi));

MapData.MinComp.ListMin = '';
MapData.MinComp.MinQ = zeros(length(Pi),length(TCi));
MapData.MinComp.MinQw = zeros(length(Pi),length(TCi));

MapData.ChemPot.ListChemPot = '';
MapData.ChemPot.ChemPotMatrix = zeros(length(Pi),length(TCi));

for iTC = 1:length(TCi)
    for iP = 1:length(Pi)
        
        [Res,Evaluation,WorkVariMod] = OptiBingoPT([1,1],[TCi(iTC),Pi(iP)],LIMS,BinSet,WorkVariXMap,MinimOptions,handles);
        
        E4(iP,iTC) = Res;
        
        E1(iP,iTC) = Evaluation.assemblage;
        E2(iP,iTC) = Evaluation.Volume;
        E3(iP,iTC) = Evaluation.Compositions;
        
        Evaluation.Quality = -Res;
        
        % New PL - (13.02.2017) ** SWOT **
        % Update the variable MapData
        MapData = UpdateMapData(MapData,iP,iTC,Evaluation,WorkVariMod);
        
    end
    
    ComptWait = ComptWait+1;
    TimeSec = toc;
    TimePerLoop = TimeSec/ComptWait;
    TotalTime = TimePerLoop*NbMinimWait - TimeSec;
    
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
    imagesc(TCi,Pi,E4), colorbar, set(gca,'ydir','norm'), axis(LIMS)
    caxis([min(E4(:))-5 max(E4(:))+5])
    xlabel('Temperature (C)'), ylabel('Pressure (Kbar)')
    set(handles.axes_Live_PLOT5,'Box','On')
    title('Objective function')
    
    UpdateLIVEWaitBar(1,ComptWait/NbMinimWait,handles);
    set(handles.Live_Disp1_Text3,'Visible','On','String',['Computing ',CodeDispTime])
    
    drawnow
    %waitbar(ComptWait/NbMinimWait,h,['The antidote is working, please wait... ',CodeDispTime]);
    
end
toc

set(handles.Live_Disp1_Text3,'Visible','On','String','Done','ForegroundColor',[0,0,0]);
drawnow

UpdateLIVEWaitBar(1,1,handles);

set(handles.Live_Disp2_Text1,'Visible','On','String','>>');
set(handles.Live_Disp2_Text2,'Visible','On','String','Saving results');
set(handles.Live_Disp2_Text3,'Visible','On','String','Please wait ...','ForegroundColor',[1,0,0]);
drawnow

% plot the results
FinalPlotMapData(MapData,TCi,Pi);

UpdateLIVEWaitBar(1,1,handles);

FunctionClearLivePanel(handles);


% Update display
axes(handles.axes_Live_PLOT1);
imagesc(TCi,Pi,E4), colorbar, set(gca,'ydir','norm'), axis(LIMS)
caxis([min(E4(:))-1 max(E4(:))+1])
xlabel('Temperature (C)'), ylabel('Pressure (Kbar)')
set(handles.axes_Live_PLOT1,'Visible','On','Box','On')
title('Objective function')

axes(handles.axes_Live_PLOT2), cla;
imagesc(TCi,Pi,E1), colorbar, set(gca,'ydir','norm'), axis(LIMS)
caxis([0 100])
xlabel('Temperature (C)'), ylabel('Pressure (Kbar)')
set(handles.axes_Live_PLOT2,'Visible','On','Box','On')
title('Qass')

axes(handles.axes_Live_PLOT3), cla;
imagesc(TCi,Pi,E2), colorbar, set(gca,'ydir','norm'), axis(LIMS)
caxis([min(E2(:))-1 max(E2(:))+1])
xlabel('Temperature (C)'), ylabel('Pressure (Kbar)')
set(handles.axes_Live_PLOT3,'Visible','On','Box','On')
title('Qvol')

axes(handles.axes_Live_PLOT4), cla;
imagesc(TCi,Pi,E3), colorbar, set(gca,'ydir','norm'), axis(LIMS)
caxis([min(E3(:))-2 max(E3(:))+2])
xlabel('Temperature (C)'), ylabel('Pressure (Kbar)')
set(handles.axes_Live_PLOT4,'Visible','On','Box','On')
title('Qcmp')

drawnow

set(handles.Live_Disp2_Text3,'Visible','On','String','Done','ForegroundColor',[0,0,0]);
drawnow



Output.WeCallBingo = 0;
Output.WeSaveWorkspace = 0;
Output.Message = 'Success';

Antidote_VARIABLES = [];

return



function [MapData] = UpdateMapData(MapData,iP,iTC,Evaluation,WorkVariMod);
%


MapData.QF.Q1(iP,iTC) = Evaluation.assemblage;
MapData.QF.Q2(iP,iTC) = Evaluation.Volume;
MapData.QF.Q3(iP,iTC) = Evaluation.Compositions;
MapData.QF.Qt(iP,iTC) = Evaluation.Quality;


MinStable = WorkVariMod.Names;
[Is,Ind] = ismember(MinStable,MapData.MinProp.ListMin);

for i = 1:length(Is)
    
    if Is(i)
       Where = Ind(i); 
    else
        % Create a new dimensions
        Where = length(MapData.MinProp.ListMin)+1;
        MapData.MinProp.ListMin{Where} = char(MinStable{i});
    end
    
    MapData.MinProp.MapMatrix(iP,iTC,Where) = WorkVariMod.VolFrac(i);
    % WorkVariMod.Dens(i);
    
end


for i = 1:length(Evaluation.MinIsCompared_TEMP)
    if Evaluation.MinIsCompared_TEMP(i)
        Ind2 = find(ismember(MapData.MinComp.ListMin,Evaluation.MinNames_TEMP{i}));
        
        if Ind2 
            Where2 = Ind2;
        else
            Where2 = length(MapData.MinComp.ListMin)+1;
            MapData.MinComp.ListMin{Where2} = char(Evaluation.MinNames_TEMP{i});
        end
        
        MapData.MinComp.MinQ(iP,iTC,Where2) = Evaluation.MinQ_TEMP(i);
        MapData.MinComp.MinQw(iP,iTC,Where2) = Evaluation.MinQw_TEMP(i);
        
    end
end

for i = 1:length(WorkVariMod.ChemComp)
    Ind3 = find(ismember(MapData.ChemPot.ListChemPot,WorkVariMod.ChemComp{i}));
    
    if Ind3
        Where3 = Ind3;
    else
        Where3 = length(MapData.ChemPot.ListChemPot)+1;
        MapData.ChemPot.ListChemPot{Where3} = char(WorkVariMod.ChemComp{i});
    end
    
    MapData.ChemPot.ChemPotMatrix(iP,iTC,Where3) = WorkVariMod.ChemPot(i);
    
end

% figure(666), 
% imagesc(MapData.ChemPot.ChemPotMatrix(:,:,1)), axis image, colorbar
% colormap([RdYlBu(64);0,0,0])
% drawnow

return


function FinalPlotMapData(MapData,TCi,Pi)
% This function generates and saves the figures for RECIPE 2 
%
% Slightly adjusted by PL 23.03.19 to save less figures
%

PlotChemPotMap = 0;

 
DateStr = char(datestr(now));
DateStr(find(DateStr == ' ')) = '_';
ProjectName = ['Antidote_Recipe2_',DateStr];

ProjectName = CleanProjectName(ProjectName);

[Success,Message,MessageID] = mkdir('Antidote');
cd Antidote
[Success,Message,MessageID] = mkdir(ProjectName);
cd ..

Directory = ['Antidote/',ProjectName,'/'];


% Maps of quality factors
h = figure;
imagesc(TCi,Pi,MapData.QF.Q1), colorbar
set(gca,'YDir','normal')
colormap([0,0,0;RdYlBu(64)])
xlabel('Temperature (C)')
ylabel('Pressure (kbar)')
title(['Evaluation ASSEMBLAGE (Q_a_s_m in %)'])

saveas(gcf,[Directory,'1_Qasm.pdf'],'pdf');
saveas(gcf,[Directory,'1_Qasm.fig'],'fig');

close(h);

%
h = figure;
imagesc(TCi,Pi,MapData.QF.Q2), colorbar
set(gca,'YDir','normal')
colormap([0,0,0;RdYlBu(64)])
xlabel('Temperature (C)')
ylabel('Pressure (kbar)')
title(['Evaluation MODES (Q_v_o_l in %)'])

saveas(gcf,[Directory,'2_Qvol.pdf'],'pdf');
saveas(gcf,[Directory,'2_Qvol.fig'],'fig');

close(h);

%
h = figure;
imagesc(TCi,Pi,MapData.QF.Q3), colorbar
set(gca,'YDir','normal')
colormap([0,0,0;RdYlBu(64)])
xlabel('Temperature (C)')
ylabel('Pressure (kbar)')
title(['Evaluation COMPOSITIONS (Q_c_m_p in %)'])

saveas(gcf,[Directory,'3_Qcmp.pdf'],'pdf');
saveas(gcf,[Directory,'3_Qcmp.fig'],'fig');

close(h)

%
h = figure;
imagesc(TCi,Pi,MapData.QF.Qt), colorbar
set(gca,'YDir','normal')
colormap([0,0,0;RdYlBu(64)])
xlabel('Temperature (C)')
ylabel('Pressure (kbar)')
title(['Evaluation QUALITY (Q_t_o_t in %)'])

saveas(gcf,[Directory,'4_Qtot.pdf'],'pdf');
saveas(gcf,[Directory,'4_Qtot.fig'],'fig');

close(h)

% MINERAL COMPOSITION
for i = 1:length(MapData.MinComp.ListMin)
    h = figure;
    imagesc(TCi,Pi,MapData.MinComp.MinQ(:,:,i)), colorbar
    set(gca,'YDir','normal')
    colormap([0,0,0;RdYlBu(64)])
    xlabel('Temperature (C)')
    ylabel('Pressure (kbar)')
    title(['Composition of ',char(MapData.MinComp.ListMin{i}),' (%)'])
    
    saveas(gcf,[Directory,'5_Qcmp_',char(MapData.MinComp.ListMin{i}),'.pdf'],'pdf');
    saveas(gcf,[Directory,'5_Qcmp_',char(MapData.MinComp.ListMin{i}),'.fig'],'fig');
    
    close(h)
    
    %     h = figure;
    %     imagesc(TCi,Pi,MapData.MinComp.MinQw(:,:,i)), colorbar
    %     set(gca,'YDir','normal')
    %     colormap([0,0,0;RdYlBu(64)])
    %     xlabel('Temperature (C)')
    %     ylabel('Pressure (kbar)')
    %     title(['Composition of ',char(MapData.MinComp.ListMin{i}),' (weigthed)'])
    %
    %     saveas(gcf,[Directory,'COMPw_',char(MapData.MinComp.ListMin{i}),'.pdf'],'pdf');
    %     saveas(gcf,[Directory,'COMPw_',char(MapData.MinComp.ListMin{i}),'.fig'],'fig');
    %
    %     close(h)
end

% MINERAL PROPORTIONS
for i = 1:length(MapData.MinProp.ListMin)
    h = figure;
    imagesc(TCi,Pi,MapData.MinProp.MapMatrix(:,:,i)), colorbar
    set(gca,'YDir','normal')
    colormap([0,0,0;RdYlBu(64)])
    xlabel('Temperature (C)')
    ylabel('Pressure (kbar)')
    title(['Mineral fraction of ',char(MapData.MinProp.ListMin{i}),' (vol%)'])
    
    saveas(gcf,[Directory,'6_VolFrac_',char(MapData.MinProp.ListMin{i}),'.pdf'],'pdf');
    saveas(gcf,[Directory,'6_VolFrac_',char(MapData.MinProp.ListMin{i}),'.fig'],'fig');
    
    close(h)
end

% We don't plot anymore this map automatically (PL  23.03.19)
if PlotChemPotMap
    for i = 1:length(MapData.ChemPot.ListChemPot)
        
        h = figure;
        imagesc(TCi,Pi,MapData.ChemPot.ChemPotMatrix(:,:,i)), colorbar
        set(gca,'YDir','normal')
        colormap([0,0,0;RdYlBu(64)])
        xlabel('Temperature (C)')
        ylabel('Pressure (kbar)')
        title(['Chemical Potential of ',char(MapData.ChemPot.ListChemPot{i}),' (J)'])
        
        saveas(gcf,[Directory,'MU',char(MapData.ChemPot.ListChemPot{i}),'.pdf'],'pdf');
        saveas(gcf,[Directory,'MU_',char(MapData.ChemPot.ListChemPot{i}),'.fig'],'fig');
        
        close(h)
    end
end

return
