function [TempBinBulk] = BinUpdateBulkComposition(TempBinBulk,LBref,handles)
%

MapWindow = handles.MapWindow;
SF = handles.SF;
BinElData = handles.BinElData;

DensityMap = MapWindow.Mask.DensityMap;

% (1) Get the shape of the selected pixels
h = TempBinBulk.DomainXrefYref;
[LinS,ColS] = size(DensityMap);
MaskSel = Xpoly2maskX(h(:,1),h(:,2),LinS,ColS);

% (2) Remove the pixels without composition 
%     and remove the pixels of the unselected phases  (PL 14.06.17)
OxideSum=zeros(size(DensityMap));
PxSel4Bulk = zeros(size(DensityMap));
ListElem = MapWindow.Maps.ListMaps;
for i=1:length(ListElem)
    OxideSum = OxideSum + MapWindow.Maps.Data(i).ValuesOx;
end

for i = 1:length(handles.MapWindow.Mask.Selected4Bulk)
    if handles.MapWindow.Mask.Selected4Bulk(i)
        TheSpecMaskPixels = find(MapWindow.Mask.Data == i);
        PxSel4Bulk(TheSpecMaskPixels) = ones(size(TheSpecMaskPixels));
    end
end

MaskSelFinal = zeros(size(DensityMap));
PixelsSelected = find(OxideSum(:) > 0 & MaskSel(:) > 0 & PxSel4Bulk(:) > 0 & DensityMap(:)>0);
MaskSelFinal(PixelsSelected) = ones(size(PixelsSelected));

% Density correction for all maps
AverageDensity = mean(DensityMap(PixelsSelected));
%disp(num2str(AverageDensity));

for i=1:length(ListElem)
    TheMap = MapWindow.Maps.Data(i).ValuesOx;
    DCM(i).map = (TheMap.*DensityMap.*MaskSelFinal)./AverageDensity; 
end

% Atom weight for the selected elements
Concat = '0   ';
SelEl = find(BinElData.selected);
NbSelEl = length(SelEl);

FileName = 'OptionsXTT.txt';
[OptString,Valid] = ReadOptionXTT(FileName);

OptString1 = OptString{1};
OptString2 = OptString{2};

for i=1:NbSelEl
    ElCode = BinElData.listElem(SelEl(i));

    OxideValue = mean(DCM(SelEl(i)).map(PixelsSelected));
    AtomW = OxideValue/BinElData.MolarMass(SelEl(i))*BinElData.NbAtoms(SelEl(i));
    
    Concat = [Concat,char(ElCode),'(',num2str(AtomW),')'];
    
    Oxide2Print(i) = OxideValue;
    OxideName2Print{i} = char(BinElData.listOxides(SelEl(i)));
    
    %disp([char(ElCode),' ',char(num2str(OxideValue)),' ',char(num2str(AtomW))])
end 
Concat = [Concat,char(OptString1),'   ',char(OptString2),'LB_',num2str(LBref)];

TempBinBulk.CompositionOriginal = Concat;
TempBinBulk.CompositionModified = Concat;
TempBinBulk.CompositionIterative = Concat;

disp(' ')
disp(['----------------- NEW BULK COMPOSITION (',['LB_',num2str(LBref)],') -----------------'])

for i=1:length(Oxide2Print)
    fprintf('%s\t',char(OxideName2Print{i}));
end
fprintf('\n');
for i=1:length(Oxide2Print)
    fprintf('%.3f\t',Oxide2Print(i));
end
fprintf('\n\n');

return

