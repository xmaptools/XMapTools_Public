function [MatrixSF,ElementsList] = SF_CatNorm(MatrixOxide,OxList,CatBasis,ElOxDataDef)
%

ListOx = ElOxDataDef.OxList;
ListEl = ElOxDataDef.ElList(ElOxDataDef.OxElIdx);

NbEl = ElOxDataDef.OxNbCat;
NbOx = ElOxDataDef.OxNbOx;

MolarW = ElOxDataDef.OxMass;

if ~iscell(OxList)
    Ox = strread(OxList,'%s');
else
    Ox = OxList;
end

[lia,loc] = ismember(Ox,ListOx);

NbCalc = size(MatrixOxide,1);
NbElem = size(MatrixOxide,2);

Num = repmat(NbEl(loc),NbCalc,1);
NumO = repmat(NbOx(loc),NbCalc,1);
Cst = repmat(MolarW(loc),NbCalc,1);

AtomicPer = MatrixOxide./Cst.*Num; 
TheSum = sum(AtomicPer,2);

RefOx = repmat(TheSum/CatBasis,1,NbElem);

MatrixSF = AtomicPer ./ RefOx;
MatrixSF(:,end+1) = sum((MatrixSF.*NumO)./Num,2);

ElementsList = ListEl(loc);
ElementsList{end+1} = 'O';

end