function [x,y] = CoordinatesFromRef(xref,yref,handles)
% This function transform the true coordinates (Xref,Yref) get from 
% XginputX into map coordinates (x,y) for display.
%
% This function use the variable handles.rotateFig to get the orientation
% of the current figure (case 0, 90, 180, 270). 
%
% P. Lanari (25.04.13)


laSize = size(XimrotateX(handles.MapWindow.Maps.Data(1).ValuesOx,handles.rotateFig));

lesX = [0 laSize(2)]; %get(handles.axes1,'XLim');
lesY = [0 laSize(1)]; %get(handles.axes1,'YLim');


xLengthFig = lesX(2)-lesX(1);          % FOR Yfig and Yfig (not Xref and Yref)
yLengthFig = lesY(2)-lesY(1);


switch handles.rotateFig
    
    case 0
        
        x = xref;
        y = yref;
        
    case 90
        
        x = yref;
        y = yLengthFig - xref;
        
    case 180
        
        x = xLengthFig - xref;
        y = yLengthFig - yref;
        
    case 270
        
        x = xLengthFig - yref;
        y = xref;
                  
end



return


