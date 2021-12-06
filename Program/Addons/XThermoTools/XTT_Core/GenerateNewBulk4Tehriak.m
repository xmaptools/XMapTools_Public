function NewBulk = GenerateNewBulk4Tehriak(Elem4Bulk,Vals4Bulk,Extras4Bulks,Bulk)
%

NewBulk = [char(Extras4Bulks{1}),'   '];

Compt = 0;
AddOxy = 0;
for i = 1:length(Elem4Bulk)
    if isequal(Elem4Bulk{i},'O')
        AddOxy = 1;
        NewBulk = [NewBulk,'O(?)'];
    end
    NewBulk = [NewBulk,char(Elem4Bulk{i}),'(',num2str(Vals4Bulk(i)),')'];
end

if ~AddOxy
    NewBulk = [NewBulk,'O(?)'];
end
NewBulk = [NewBulk,'   *',Extras4Bulks{2}];

load('MinimOptions.mat');
if isequal(MinimOptions.TestMode,1)
    disp(' ')
    disp(['Previous bulk: ',Bulk])
    disp(['New bulk:      ',NewBulk])
end

return

