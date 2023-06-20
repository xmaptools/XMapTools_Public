function [Output,Antidote_VARIABLES] = Antidote_2_MapQ(WorkVariXMap,MinimOptions,app)
%
%

Tmin = app.TminEditField.Value;
Tmax = app.TmaxEditField.Value;
Pmin = app.PminEditField.Value;
Pmax = app.PmaxEditField.Value;

Tstep = app.TstepEditField.Value;
Pstep = app.PstepEditField.Value;

Ti = [Tmin:Tstep:Tmax];
Pi = [Pmin:Pstep:Pmax];

LIMS = [Ti(1),Ti(end),Pi(1),Pi(end)];

dT4Plot = (Ti(2) - Ti(1))/2;
dP4Plot = (Pi(2) - Pi(1))/2;

Axis_Maps = [LIMS(1)-dT4Plot LIMS(2)+dT4Plot (LIMS(3)-dP4Plot) (LIMS(4)+dP4Plot)];

[BinSet] = SetBin(app);

app.Report_Antidote{end+1} = ['Antidote: Recipe [2] - P–T map of Q factors'];
app.Report_Antidote{end+1} = '';
app.Report_Antidote{end+1} = ['Bulk: ',BinSet.Bulk2Display];
app.Report_Antidote{end+1} = ['Database: ',BinSet.Database];
app.Report_Antidote{end+1} = '';
if MinimOptions.Weights.Use
    app.Report_Antidote{end+1} = ['Equation: Other',['[E4 = -(',num2str(MinimOptions.Weights.Values(MinimOptions.Weights.Selected,1)),'*E1 + ',num2str(MinimOptions.Weights.Values(MinimOptions.Weights.Selected,2)),'*E2 + ',num2str(MinimOptions.Weights.Values(MinimOptions.Weights.Selected,3)),'*E3)]']];
    app.Report_Antidote{end+1} = '';
else
    app.Report_Antidote{end+1} = ['Equation: Classic [E4 = -1/3*(E1 + (E1/100)*E2 + (E1/100)*(E2/100)*E3)]'];
    app.Report_Antidote{end+1} = '';
end
    
app.Text_Report_Antidote.Value = app.Report_Antidote;

tic

E1 = nan(length(Pi),length(Ti));
E2 = nan(length(Pi),length(Ti));
E3 = nan(length(Pi),length(Ti));
E4 = nan(length(Pi),length(Ti));

ComptWait = 0;
NbMinimWait = length(Pi);

% New PL - (13.02.2017) ** SWOT **
% Create MapData to store all the information of mapping
MapData.QF.Q1 = zeros(length(Pi),length(Ti));
MapData.QF.Q2 = zeros(length(Pi),length(Ti));
MapData.QF.Q3 = zeros(length(Pi),length(Ti));
MapData.QF.Qt = zeros(length(Pi),length(Ti));

MapData.MinProp.ListMin = '';
MapData.MinProp.MapMatrix = zeros(length(Pi),length(Ti));

MapData.MinComp.ListMin = '';
MapData.MinComp.MinQ = zeros(length(Pi),length(Ti));
MapData.MinComp.MinQw = zeros(length(Pi),length(Ti));

MapData.ChemPot.ListChemPot = '';
MapData.ChemPot.ChemPotMatrix = zeros(length(Pi),length(Ti));

for iTC = 1:length(Ti)
    for iP = 1:length(Pi)
        
        [Res,Evaluation,WorkVariMod] = OptiBingoPT([1,1],[Ti(iTC),Pi(iP)],LIMS,BinSet,WorkVariXMap,MinimOptions,app);
        
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
    
%     if TotalTime  > 60
%         if TotalTime >3600
%             Totalh = floor(TotalTime/3600);
%             Totalm = round((TotalTime/3600 - Totalh)*60);
%             CodeDispTime = ['(~',num2str(Totalh),'h',num2str(Totalm),')'];
%             
%         else
%             CodeDispTime = ['(~',num2str(round(TotalTime/60)),'m)'];
%         end
%     else
%         CodeDispTime = ['(~',num2str(round(TotalTime)),'s)'];
%     end
    
    % Update display
    
    imagesc(app.UIAxes_LiveAntidote1,Ti,Pi,abs(E4));
    app.UIAxes_LiveAntidote1.YDir = 'normal';
    axis(app.UIAxes_LiveAntidote1,Axis_Maps)
    colormap(app.UIAxes_LiveAntidote1,[0,0,0;RdYlBu(128)]);
    colorbar(app.UIAxes_LiveAntidote1)
    caxis(app.UIAxes_LiveAntidote1,[0 100]);
    xlabel(app.UIAxes_LiveAntidote1,'Temperature (°C)');
    ylabel(app.UIAxes_LiveAntidote1,'Pressure (GPa)');
    title(app.UIAxes_LiveAntidote1,'(Q_{tot})');
    
    imagesc(app.UIAxes_LiveAntidote2,Ti,Pi,abs(E1));
    app.UIAxes_LiveAntidote2.YDir = 'normal';
    axis(app.UIAxes_LiveAntidote2,Axis_Maps)
    colormap(app.UIAxes_LiveAntidote2,[0,0,0;RdYlBu(128)]);
    colorbar(app.UIAxes_LiveAntidote2)
    caxis(app.UIAxes_LiveAntidote2,[0 100]);
    xlabel(app.UIAxes_LiveAntidote2,'Temperature (°C)');
    ylabel(app.UIAxes_LiveAntidote2,'Pressure (GPa)');
    title(app.UIAxes_LiveAntidote2,'(Q_{asm})');
    
    imagesc(app.UIAxes_LiveAntidote3,Ti,Pi,abs(E2));
    app.UIAxes_LiveAntidote3.YDir = 'normal';
    axis(app.UIAxes_LiveAntidote3,Axis_Maps)
    colormap(app.UIAxes_LiveAntidote3,[0,0,0;RdYlBu(128)]);
    colorbar(app.UIAxes_LiveAntidote3)
    caxis(app.UIAxes_LiveAntidote3,[0 100]);
    xlabel(app.UIAxes_LiveAntidote3,'Temperature (°C)');
    ylabel(app.UIAxes_LiveAntidote3,'Pressure (GPa)');
    title(app.UIAxes_LiveAntidote3,'(Q_{vol})');
    
    imagesc(app.UIAxes_LiveAntidote4,Ti,Pi,abs(E3));
    app.UIAxes_LiveAntidote4.YDir = 'normal';
    axis(app.UIAxes_LiveAntidote4,Axis_Maps)
    colormap(app.UIAxes_LiveAntidote4,[0,0,0;RdYlBu(128)]);
    colorbar(app.UIAxes_LiveAntidote4)
    caxis(app.UIAxes_LiveAntidote4,[0 100]);
    xlabel(app.UIAxes_LiveAntidote4,'Temperature (°C)');
    ylabel(app.UIAxes_LiveAntidote4,'Pressure (GPa)');
    title(app.UIAxes_LiveAntidote4,'(Q_{cmp})');
    
    drawnow
    %waitbar(ComptWait/NbMinimWait,h,['The antidote is working, please wait... ',CodeDispTime]);
    
end
toc

% plot the results
FinalPlotMapData(MapData,Ti,Pi);


Output.WeCallBingo = 0;
Output.WeSaveWorkspace = 0;
Output.Message = 'Success';

Antidote_VARIABLES = [];

end



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

end

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
    title(['Composition of ',char(MapData.MinComp.ListMin{i}),' (%)'],'Interpreter','none')
    
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
    imagesc(TCi,Pi,MapData.MinProp.MapMatrix(:,:,i)*100), colorbar
    set(gca,'YDir','normal')
    colormap([0,0,0;RdYlBu(64)])
    xlabel('Temperature (C)')
    ylabel('Pressure (kbar)')
    title(['Mineral fraction of ',char(MapData.MinProp.ListMin{i}),' (vol%)'],'Interpreter','none')
    
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

end
