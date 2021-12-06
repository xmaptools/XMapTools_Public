function [BinSet] = Bingo_SetBin(handles)
%


switch handles.BingoDefault.SelectedProgram
    case 1
       BinSet.Path = handles.BingoDefault.Theriak.Path; 
       DefMin = handles.BingoDefault.Theriak.Database(get(handles.BinPopUpDatabase,'Value')).DefMin;
    case 2
       BinSet.Path = handles.BingoDefault.PerpleX.Path;
       DefMin = handles.BingoDefault.PerpleX.Database(get(handles.BinPopUpDatabase,'Value')).DefMin;
end

for i=1:length(DefMin(:,1))
    BinSet.ListRefMiner{i} = DefMin{i,3};
end

% Melt Model to be added to the solids?
if isfield(handles,'BinGfDef')    % With that Bingo does not require BinGfDef to be defined
    if handles.BinGfDef.Melt.Include
        i = i+1;
        BinSet.ListRefMiner{i} = handles.BinGfDef.Melt.DBName;
        %keyboard
    end
end


Databases = get(handles.BinPopUpDatabase,'String');
BinSet.Database = Databases{get(handles.BinPopUpDatabase,'Value')};
BinSet.Bulk2Display = get(handles.BinTextBulkCompo,'String');

BinSet.Bulk2Display(1) = '1'; % Very important

dlmwrite('XBIN',char(BinSet.Database,'no'),'delimiter','');

%disp(['... XBIN has been updated by Bingo - database: ',char(BinSet.Database),' ...']);
%disp(' ')

return