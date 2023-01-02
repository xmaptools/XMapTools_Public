function [NewBulk,BinBulk] = SuperFastBulkUpdate(BinGfDef,BinBulk,SelectedCompo)

Keys = [];

Compt = 0;
%if BinGfDef.Fluids.Activate
for i = 1:length(BinGfDef.Fluids.Spec)
    if  BinGfDef.Fluids.Spec(i).IsActive
        Compt = Compt+1;
        Keys{Compt} = char(BinGfDef.Fluids.Spec(i).ListVariAntidote);
        
        if BinGfDef.Fluids.Spec(i).Optimize
            Vals(Compt) = BinGfDef.Fluids.Spec(i).Lower + 0.5*(BinGfDef.Fluids.Spec(i).Upper - BinGfDef.Fluids.Spec(i).Lower);
        else
            Vals(Compt) = BinGfDef.Fluids.Spec(i).Bulk;
        end
    end
end
%end

if BinGfDef.Oxygen.ActivateExtraO
    Compt = Compt+1;
    Keys{Compt} = 'O';
    if BinGfDef.Oxygen.OptimizeExtraO
        Vals(Compt) = BinGfDef.Oxygen.ExtraO.Lower + 0.5*(BinGfDef.Oxygen.ExtraO.Upper - BinGfDef.Oxygen.ExtraO.Lower);
    else
        Vals(Compt) = BinGfDef.Oxygen.ExtraO.Bulk;
    end
end

BulkOri = BinBulk(SelectedCompo).CompositionOriginal;

BinBulk(SelectedCompo).CompoDisp = 3;  % We take the iterative bulk!
Bulk = BinBulk(SelectedCompo).CompositionIterative;

% [S,IdxS] = ismember('*',BulkOri);
% while 1
%     IdxS = IdxS - 1;
%     if ~isequal(BulkOri(IdxS),' ')
%         break
%     end
% end
% 
% Bulk_Part2 = BulkOri(IdxS+1:end);
% BulkW = BulkOri(1:IdxS);

% -----------------------
% NEW by PL on 08.03.2019
% this only extracts the elements that are defined without O(?) and O(0) -
% to be added later on...
[Elem4Bulk,Vals4Bulk,Extras4Bulks] = ExtractElemValsFromBulk(Bulk);

if ~isempty(length(Keys))
    for i = 1:length(Keys)
        [Ok,WhereEl] = ismember(char(Keys{i}),Elem4Bulk);

        if Ok 
            % the element is already listed we just update
            Vals4Bulk(WhereEl) = Vals(i);
        else
            % we add the element
            Elem4Bulk(end+1) = Keys(i);
            Vals4Bulk(end+1) = Vals(i);
        end
    end
else
    Elem4Bulk = '';
    Vals4Bulk = [];
end

NewBulk = GenerateNewBulk4Theriak(Elem4Bulk,Vals4Bulk,Extras4Bulks,Bulk);


% for i = 1:length(Keys)
%     
%     [Ok,WhereEl] = ismember(char(Keys{i}),BulkW);
%      
%     % Problem of C and Ca
%     if isequal(char(Keys{i}),'C') && ~isequal(BulkW(WhereEl+1),')')
%         Ok = 0;
%     end
%     
%     if Ok
%         if length(Ok) > 1
%             % Oxygen probably here...
%             
%         else
%             T_Part1 = BulkW(1:WhereEl+1);
%             BulkShort = BulkW(WhereEl+2:end);
%             WhereP = find(ismember(BulkShort,')'));
%             T_Part2 = BulkW(WhereEl+1+WhereP(1):end);
%             
%             BulkW = [T_Part1,num2str(Vals(i)),T_Part2];
%         end
%     else
%         BulkW = [BulkW,char(Keys{i}),'(',num2str(Vals(i)),')'];  
%     end
%     
% end

%Bulk = [BulkW,Bulk_Part2];

return


