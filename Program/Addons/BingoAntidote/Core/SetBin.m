function [BinSet] = SetBin(app)
%

iDB = find(ismember(app.DatabaseListBox.Items,app.DatabaseListBox.Value));

BinSet.Path = app.BingoDefault.Theriak.Path;
DefMin = app.BingoDefault.Theriak.Database(iDB).DefMin;

for i=1:length(DefMin(:,1))
    BinSet.ListRefMiner{i} = DefMin{i,3};
end

% Melt Model to be added to the solids?
%if isfield(app,'BinGfDef')    % With that Bingo does not require BinGfDef to be defined
    if app.BinGfDef.Melt.Include
        i = i+1;
        BinSet.ListRefMiner{i} = app.BinGfDef.Melt.DBName;
        %keyboard
    end
%end

Databases = app.DatabaseListBox.Items;
BinSet.Database = Databases{iDB};
BinSet.Bulk2Display = app.LBCEditField.Value;

BinSet.Bulk2Display(1) = '1'; % Very important

dlmwrite('XBIN',char(BinSet.Database,'no'),'delimiter','');

%disp(['... XBIN has been updated by Bingo - database: ',char(BinSet.Database),' ...']);
%disp(' ')

return