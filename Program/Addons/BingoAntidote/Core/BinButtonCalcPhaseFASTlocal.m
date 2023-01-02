function [BinPhaseDef] = BinButtonCalcPhaseFASTlocal(BinBulkTemp, app)
% P. Lanari for sliding window (14.02.2017)
% This one does not update handles.BinBulk (display)
%
        
%- MapWindow = handles.MapWindow;
BinBulk = BinBulkTemp;
%SelBinBulk = get(handles.BinPopUpBulkCompoList,'Value');

% (1) Get the shape of the selected pixels
h = BinBulk(1).DomainXrefYref;
[LinS,ColS] = size(app.MaskFile.MaskMap);
MaskSel = poly2mask(h(:,1),h(:,2),LinS,ColS);

% define the selected phases (checkbox =1)
BinPhaseDef = app.BinPhaseDef;

TheSelMaskOk = MaskSel .* app.MaskFile.MaskMap;
 
Compt = 0;
for i=1:length(BinPhaseDef)
    if BinPhaseDef(i).SelForBA  % Not 100 % sure this is ok (version 2022)
        Compt = Compt+1;
        PhaseSelected(Compt) = i;
        NbPixels(Compt) = length(find(TheSelMaskOk(:) == i));
    end
end

Proportions = NbPixels./sum(NbPixels)*100;

Compt = 0;
for i=1:length(BinPhaseDef)
    if BinPhaseDef(i).SelForBA
        Compt = Compt+1;
        BinPhaseDef(i).PhaseVolProp = Proportions(Compt);
    end
end

return


