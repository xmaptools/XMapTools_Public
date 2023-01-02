function [Elem,Vals,Extras4Bulks] = ExtractElemValsFromBulk(Bulk)

WhereStar = find(ismember(Bulk,'*'));
BulkDelimS{1} = Bulk(1:WhereStar(1)-1);
BulkDelimS{2} = Bulk(WhereStar(1)+1:end);

%BulkDelimS = strread(Bulk,'%s','delimiter','*');
BulkRead = strread(BulkDelimS{1},'%s');

if isequal(length(BulkRead),2)
    Extras4Bulks{1} = BulkRead{1};
    Extras4Bulks{2} = BulkDelimS{2};
    
    BulkRead = char(BulkRead{2});
else
    disp('WARNING - there is a serious issue in reading the bulk [error E4623] ')
    disp('Please send this error message to pierre.lanari@geo.unibe.ch')
    disp(Bulk)
    disp(' ')
    keyboard 
end

for i = 1:length(BulkRead)
    if isequal(BulkRead(i),'(') || isequal(BulkRead(i),')')
        BulkRead(i) = '|';
    end
end

Elem = [];
Vals = [];
BulkReadStr = strread(BulkRead,'%s','delimiter','|');
compt = 0;
for i = 1:2:length(BulkReadStr)-1
    if  ~isequal(BulkReadStr{i},'O') && ~isequal(BulkReadStr{i},'H') && ~isequal(BulkReadStr{i},'C') % ~isequal(BulkReadStr{i+1},'?') ||we ignore this one...
        compt = compt+1;
        Elem{compt} = BulkReadStr{i};
        Vals(compt) = str2num(BulkReadStr{i+1});
    end
end

return
