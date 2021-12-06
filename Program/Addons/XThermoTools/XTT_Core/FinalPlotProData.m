function FinalPlotProData(ProData,Nb)
% This function generates and saves the figures...

 
DateStr = char(datestr(now));
DateStr(find(DateStr == ' ')) = '_';
ProjectName = ['Antidote_Recipe10_',DateStr];

ProjectName = CleanProjectName(ProjectName);

[Success,Message,MessageID] = mkdir('Antidote');
cd Antidote
[Success,Message,MessageID] = mkdir(ProjectName);
cd ..

Directory = ['Antidote/',ProjectName,'/'];

% Q FACTORS
h = figure;
plot([ProData.Qfactors.Q1;ProData.Qfactors.Q2;ProData.Qfactors.Q3;abs(ProData.Qfactors.Qt)]','.-')
xlabel('Steps')
ylabel('Qfactors (%)')
legend({'Qass','Qvol','Qcmp','Qtotal'},'Location','Best')

saveas(h,[Directory,'QUALITY.pdf'],'pdf');
saveas(h,[Directory,'QUALITY.fig'],'fig');

close(h);


% MODELED MIN MODES
h = figure;
plot(100*ProData.MinProp.ProMatrix','.-')
xlabel('Steps')
ylabel('Modeled mineral modes (vol%)')
legend(ProData.MinProp.ListMin,'Location','Best')

saveas(h,[Directory,'VOL_MODEL.pdf'],'pdf');
saveas(h,[Directory,'VOL_MODEL.fig'],'fig');

close(h);


% MIN MODES FROM MAP
h = figure;
plot(100*ProData.MinPropXray.ProMatrix','.-')
xlabel('Steps')
ylabel('Observed mineral modes (vol%)')
legend(ProData.MinPropXray.ListMin,'Location','Best')

saveas(h,[Directory,'VOL_MAP.pdf'],'pdf');
saveas(h,[Directory,'VOL_MAP.fig'],'fig');

close(h);


% QUALITY OF MIN COMPOSITIONS (FIT)
h = figure;
plot(ProData.MinComp.MinQ','.-')
xlabel('Steps')
ylabel('Quality of predicted ineral compositions (%)')
legend(ProData.MinComp.ListMin,'Location','Best')

saveas(h,[Directory,'MIN_Q.pdf'],'pdf');
saveas(h,[Directory,'MIN_Q.fig'],'fig');

close(h);

if 0
    % CHEMICAL POTENTIALS
    for i = 1:length(ProData.ChemPot.ListChemPot)
        h = figure;
        plot(ProData.ChemPot.ChemPotMatrix(i,:),'.-')
        xlabel('Steps')
        ylabel('Chemical potential of components (J)')
        %legend(ProData.ChemPot.ListChemPot,'Location','Best')

        saveas(h,[Directory,'MU_',char(ProData.ChemPot.ListChemPot{i}),'.pdf'],'pdf');
        saveas(h,[Directory,'MU_',char(ProData.ChemPot.ListChemPot{i}),'.fig'],'fig');

        close(h);
    end
end

disp(' ')
disp(' ')
disp(' Exporting binary diagrams:')
disp(' ')

% DIAGRAMS FOR EQUILIBRIUM CHECK
for i = 1:length(ProData.MinProp.ListMin)
    h = figure;
    plot(100*ProData.MinProp.ProMatrix(i,:),-ProData.Qfactors.Qt,'o','MarkerFaceColor','w','MarkerEdgeColor','k')
    xlabel(['Modeled ',ProData.MinProp.ListMin{i},' (vol%)'])
    ylabel('Q_t_o_t_a_l')
    
    saveas(h,[Directory,'Qtot_BIN_',char(ProData.MinProp.ListMin{i}),'.pdf'],'pdf');
    saveas(h,[Directory,'Qtot_BIN_',char(ProData.MinProp.ListMin{i}),'.fig'],'fig');

    close(h); 
    
    Where = find(ismember(ProData.MinComp.ListMin,ProData.MinProp.ListMin{i}));
    
    if Where
        
        disp(['> ',ProData.MinProp.ListMin{i},' is ',ProData.MinComp.ListMin{i}])
        
        h = figure;
        plot(100*ProData.MinProp.ProMatrix(i,:),ProData.MinComp.MinQ(Where,:),'o','MarkerFaceColor','w','MarkerEdgeColor','k')
        xlabel(['Modeled ',ProData.MinProp.ListMin{i},' (vol%)'])
        ylabel(['Q_c_m_p of ',ProData.MinProp.ListMin{i}])
    
        saveas(h,[Directory,'Qcmp_BIN_',char(ProData.MinProp.ListMin{i}),'.pdf'],'pdf');
        saveas(h,[Directory,'Qcmp_BIN_',char(ProData.MinProp.ListMin{i}),'.fig'],'fig');

        close(h);
    else
        disp(['> ',ProData.MinProp.ListMin{i},' skipped!'])
    end
end

return





