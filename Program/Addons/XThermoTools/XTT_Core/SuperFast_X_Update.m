function [BulkFinal,BinBulk] = SuperFast_X_Update(BinBulk,ElList,XVals)
%



% Always use the iterative there...
SelectedCompo = 1;

BinBulk(SelectedCompo).CompoDisp = 3;  % We take the iterative bulk!
BulkOri = BinBulk(SelectedCompo).CompositionIterative;


WhereS = find(ismember(BulkOri,'*'));

Bulk = BulkOri(1:WhereS-1);
PartFinal = BulkOri(WhereS:end);


for i = 1:length(ElList)
    El = ElList{i};
    
    % Update the element:
    IdxS = find(ismember(Bulk,El));

    if length(IdxS) > 1 % This is O!
        IdxS = IdxS(end); 
        % we always update the last one... Valid for O and C
    end
    
    Part1 = Bulk(1:IdxS+1);
    while 1
        IdxS = IdxS + 1;
        if isequal(Bulk(IdxS),')')
            break
        end
    end
    Part2 = Bulk(IdxS:end);

    Bulk = [Part1,num2str(XVals(i)),Part2];
end

BulkFinal = [Bulk,PartFinal]; 
BinBulk(SelectedCompo).CompositionIterative = BulkFinal;

return


