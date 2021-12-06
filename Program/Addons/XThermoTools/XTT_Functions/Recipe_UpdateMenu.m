function [handles] = Recipe_UpdateMenu(hObject, eventdata, handles)
%
%

[RecipeIndex,RecipeFunctionDef] = Recipe_Indexing(1);

WhatRecipe = get(handles.BinPopUpListeAntidotes,'Value');

TheRecipeIndex = find(ismember(RecipeIndex,WhatRecipe));

BinPhaseDef = handles.BinPhaseDef;

set(handles.BinLabelAnt1,'String','Perm.');
set(handles.BinLabelAnt2,'String','dPx');
set(handles.BinTextAnt1,'String','100');
set(handles.BinTextAnt2,'String','40');

set(handles.push_loadSW,'Visible','off');
set(handles.check_textSW,'Visible','off');


if isempty(TheRecipeIndex)
    set(handles.BinTextAnt1,'Enable','off');
    set(handles.BinTextAnt2,'Enable','off');
    set(handles.BinTextAnt3,'Enable','off');
    return
end


if isequal(TheRecipeIndex,4) || isequal(TheRecipeIndex,5) || isequal(TheRecipeIndex,6)
    % Update list and activate
    
    Compt=0;
    for i=1:length(BinPhaseDef)
        if BinPhaseDef(i).IsPhaseIn
            Compt = Compt+1;
            
            StrP = '';
            for E1 = 1:length(BinPhaseDef(i).ListVariAntidote);
                StrP = [StrP,char(BinPhaseDef(i).ListVariAntidote{E1}),'-'];%,'(',num2str(BinPhaseDef(i).ListWeightAntidote(E1)),')'];
            end
            StrP = StrP(1:end-1);
            ListNames{i} = [BinPhaseDef(i).DBMinName,'_',StrP];
        else
            ListNames{i} = '... not available ...';
        end
        
    end

    set(handles.BinPopUpVarPhaseList,'String',ListNames,'Enable','on','Value',get(handles.BinPopUpPhaseList,'Value'));
    %
else
    set(handles.BinPopUpVarPhaseList,'String',{''},'Value',1,'Enable','off');
end

if isequal(WhatRecipe,4) || isequal(WhatRecipe,8)
    set(handles.BinTextAnt4,'Enable','on');
else
    set(handles.BinTextAnt4,'Enable','off');
end

if isequal(TheRecipeIndex,7) 
    set(handles.BinTextAnt1,'Enable','on');
    set(handles.BinTextAnt2,'Enable','on');
    set(handles.BinTextAnt3,'Enable','off');
    
elseif isequal(TheRecipeIndex,8)
    set(handles.BinTextAnt1,'Enable','on');
    set(handles.BinTextAnt2,'Enable','off');
    set(handles.BinTextAnt3,'Enable','on');
    
elseif isequal(TheRecipeIndex,9)
    set(handles.BinTextAnt1,'Enable','on');
    set(handles.BinTextAnt2,'Enable','on');
    set(handles.BinTextAnt3,'Enable','on');
    
elseif isequal(TheRecipeIndex,10)
    set(handles.BinTextAnt1,'Enable','off');
    set(handles.BinTextAnt2,'Enable','off');
    set(handles.BinTextAnt3,'Enable','off');
    
% elseif isequal(WhatRecipe,WhereRecipe11) %eduester19a floating window is now 14  
%     set(handles.BinTextAnt1,'Enable','on');
%     set(handles.BinTextAnt2,'Enable','on');
%     set(handles.BinTextAnt3,'Enable','off');
%     set(handles.BinLabelAnt1,'String','x_steps');
%     set(handles.BinLabelAnt2,'String','y_steps');
%     set(handles.BinTextAnt1,'String','3');
%     set(handles.BinTextAnt2,'String','3');
%     set(handles.check_textSW,'Visible','on');
%     set(handles.push_loadSW,'Visible','on');
       
 
elseif isequal(TheRecipeIndex,11) %eduester19a scanning window is now 15  
    set(handles.BinTextAnt1,'Enable','on');
    set(handles.BinTextAnt2,'Enable','on');
    set(handles.BinTextAnt3,'Enable','off');
    set(handles.BinLabelAnt1,'String','x_steps');
    set(handles.BinLabelAnt2,'String','y_steps');
    set(handles.BinTextAnt1,'String','3');
    set(handles.BinTextAnt2,'String','3');
    set(handles.check_textSW,'Visible','on');
    set(handles.push_loadSW,'Visible','on');
   
elseif isequal(TheRecipeIndex,12) %eduester19a growing window is now 16
    set(handles.BinTextAnt1,'Enable','on');
    set(handles.BinTextAnt2,'Enable','off');
    set(handles.BinTextAnt3,'Enable','off');
    set(handles.BinLabelAnt1,'String','steps');
    set(handles.BinLabelAnt2,'String','y_steps');
    set(handles.BinTextAnt1,'String','10');
    %set(handles.BinTextAnt2,'String','3');
    set(handles.check_textSW,'Visible','on');
    set(handles.push_loadSW,'Visible','on');
    
elseif isequal(TheRecipeIndex,13) 
    set(handles.BinLabelAnt2,'String','dPx');
    set(handles.BinTextAnt2,'String','75','Enable','on');
    
    
else
    set(handles.BinTextAnt1,'Enable','off');
    set(handles.BinTextAnt2,'Enable','off');
    set(handles.BinTextAnt3,'Enable','off');
end

return