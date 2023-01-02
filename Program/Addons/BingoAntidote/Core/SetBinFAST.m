function [BinSet] = SetBinFAST(ProgPath,DefMin,TheSelDatabase,Bulk,BinGfDef)
%

BinSet.Path = ProgPath;

for i=1:length(DefMin(:,1))
    BinSet.ListRefMiner{i} = DefMin{i,3};
end

if BinGfDef.Melt.Include
    i = i+1;
    BinSet.ListRefMiner{i} = BinGfDef.Melt.DBName;
    %keyboard
end

BinSet.Database = TheSelDatabase;
BinSet.Bulk2Display = Bulk;

BinSet.Bulk2Display(1) = '1'; % Very important

dlmwrite('XBIN',char(BinSet.Database,'no'),'delimiter','');

return
