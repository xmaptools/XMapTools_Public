function FinalPlotProData2(ProData2,Recipe,Element2Display,XVals,Name4SaveResults)
% This function generates and saves the figures...

ProData = ProData2;
 
DateStr = char(datestr(now));
DateStr(find(DateStr == ' ')) = '_';
ProjectName = ['Antidote_Recipe',num2str(Recipe),'_',DateStr];
ProjectName = CleanProjectName(ProjectName);

[Success,Message,MessageID] = mkdir('Antidote');
cd Antidote
[Success,Message,MessageID] = mkdir(ProjectName);
cd ..

Directory = ['Antidote/',ProjectName,'/'];

% Save File (new 11.03.2019)
fid = fopen([Directory,Name4SaveResults],'w');
fprintf(fid,'%s\n','Results Antidote (Recipe 13)');
fprintf(fid,'%s\n\n',datestr(now));

fprintf(fid,'%s\t%s\t%s\t%s\t%s\n','X   ','Qasm','Qvol','Qcmp','Qtot');
for i = 1:length(XVals)
    fprintf(fid,'%.4f\t%.2f\t%.3f\t%.3f\t%.3f\n',XVals(i),ProData.Qfactors.Q1(i),ProData.Qfactors.Q2(i),ProData.Qfactors.Q3(i),ProData.Qfactors.Qt(i));
end
fclose(fid);


% Q FACTORS
h = figure;
plot(repmat(XVals,4,1)',[ProData.Qfactors.Q1;ProData.Qfactors.Q2;ProData.Qfactors.Q3;abs(ProData.Qfactors.Qt)]','.-')
title(Name4SaveResults(1:end-4))
xlabel([Element2Display,' (mol)'])
ylabel('Qfactors (%)')
legend({'Qass','Qvol','Qcmp','Qtotal'},'Location','Best')

saveas(gcf,[Directory,'QUALITY.pdf'],'pdf');
saveas(gcf,[Directory,'QUALITY.fig'],'fig');

close(h);


% MODELED MIN MODES
h = figure;
plot(repmat(XVals,size(ProData.MinProp.ProMatrix,1),1)',100*ProData.MinProp.ProMatrix','.-')
title(Name4SaveResults(1:end-4))
xlabel([Element2Display,' (mol)'])
ylabel('Modeled mineral modes (vol%)')
legend(ProData.MinProp.ListMin,'Location','Best')

saveas(gcf,[Directory,'VOL_MODEL.pdf'],'pdf');
saveas(gcf,[Directory,'VOL_MODEL.fig'],'fig');

close(h);


% RELATIVE MIN MODES
ModMinProp = ProData.MinProp.ProMatrix;
ModListName = ProData.MinProp.ListMin;

XrayMinProp = ProData.MinPropXray.ProMatrix;
XrayListName = ProData.MinPropXray.ListMin;

XrayMinPropOK = zeros(size(ModMinProp));
[mYes,mWhere] = ismember(ModListName,XrayListName);

for i = 1:length(mYes)
    if mYes(i)
        XrayMinPropOK(i,:) = XrayMinProp(mWhere(i),:);
    end
end

SelPos = find(sum(XrayMinPropOK,2));

ModMinProp = ModMinProp(SelPos,:);
XrayMinPropOK = XrayMinPropOK(SelPos,:);
ModListName = ModListName(SelPos);

SelZero = find(~ModMinProp);
ModMinProp(SelZero) = NaN(size(SelZero));

h = figure;
plot(repmat(XVals,size(ModMinProp,1),1)',100*(XrayMinPropOK'-ModMinProp')./XrayMinPropOK','.-')
title(Name4SaveResults(1:end-4))
xlabel([Element2Display,' (mol)'])
ylabel('Relative difference to Map (%)')
legend(ModListName,'Location','Best')

saveas(gcf,[Directory,'VOL_REL2MAP.pdf'],'pdf');
saveas(gcf,[Directory,'VOL_REL2MAP.fig'],'fig');

close(h);


% QUALITY OF MIN COMPOSITIONS (FIT)
h = figure;
plot(repmat(XVals,size(ProData.MinComp.MinQ,1),1)',ProData.MinComp.MinQ','.-')
title(Name4SaveResults(1:end-4))
xlabel([Element2Display,' (mol)'])
ylabel('Quality of predicted ineral compositions (%)')
legend(ProData.MinComp.ListMin,'Location','Best')

saveas(gcf,[Directory,'MIN_Q.pdf'],'pdf');
saveas(gcf,[Directory,'MIN_Q.fig'],'fig');

close(h);


return
