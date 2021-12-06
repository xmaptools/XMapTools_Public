function [BinPhaseDef] = BinButtonCalcPhaseFAST(handles)
% eduester for sliding window
        
MapWindow = handles.MapWindow;
BinBulk = handles.BinBulk;
SelBinBulk = get(handles.BinPopUpBulkCompoList,'Value');

% (1) Get the shape of the selected pixels
h = BinBulk(SelBinBulk).DomainXrefYref;
[LinS,ColS] = size(MapWindow.Mask.Data);
MaskSel = Xpoly2maskX(h(:,1),h(:,2),LinS,ColS);

% define the selected phases (checkbox =1)
BinPhaseDef = handles.BinPhaseDef;

TheSelMaskOk = MaskSel .* MapWindow.Mask.Data;

%figure, imagesc(MapWindow.Mask.Data), axis image, colorbar
%keyboard
 
Compt = 0;
for i=1:length(BinPhaseDef)
    if BinPhaseDef(i).IsPhaseIn
        Compt = Compt+1;
        PhaseSelected(Compt) = i;
        NbPixels(Compt) = length(find(TheSelMaskOk(:) == i));
    end
end

Proportions = NbPixels./sum(NbPixels)*100;

Compt = 0;
for i=1:length(BinPhaseDef)
    if BinPhaseDef(i).IsPhaseIn
        Compt = Compt+1;
        BinPhaseDef(i).PhaseVolProp = Proportions(Compt);
    end
end

%handles.BinPhaseDef = BinPhaseDef;
%guidata(hObject, handles);

%BinPopUpPhaseList_Callback(hObject, eventdata, handles)

return