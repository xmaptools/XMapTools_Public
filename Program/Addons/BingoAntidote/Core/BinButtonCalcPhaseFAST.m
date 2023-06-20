function [BinPhaseDef] = BinButtonCalcPhaseFAST(TempBinBulk,app)
% 


[LinS,ColS] = size(app.DensityMap);

h = TempBinBulk.DomainXrefYref;

MaskSel = poly2mask(h(:,1),h(:,2),LinS,ColS);
% MaskSel = ones(size(app.DensityMap));

% define the selected phases (checkbox =1)
BinPhaseDef = app.BinPhaseDef;

% disp('BinButtonCalcPhaseFAST > Before extracting new volumes:')
% extractfield(BinPhaseDef,'PhaseVolProp')

TheSelMaskOk = MaskSel .* app.MaskFile.MaskMap;

% figure, imagesc(TheSelMaskOk), axis image, colorbar

NbPixels = [];
Compt = 0;
for i=1:length(BinPhaseDef)
    if BinPhaseDef(i).SelForBA
        Compt = Compt+1;
        PhaseSelected(Compt) = i;
        NbPixels(Compt) = length(find(TheSelMaskOk(:) == i));
    end
end

Proportions = NbPixels./sum(NbPixels)*100;

% disp('New Proportions:')
% disp(Proportions)

Compt = 0;
for i=1:length(BinPhaseDef)
    if BinPhaseDef(i).SelForBA
        Compt = Compt+1;
        BinPhaseDef(i).PhaseVolProp = Proportions(Compt);
    end
end

% disp('BinButtonCalcPhaseFAST > After extracting new volumes:')
% extractfield(BinPhaseDef,'PhaseVolProp')

return