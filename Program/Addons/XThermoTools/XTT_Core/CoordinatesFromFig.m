function [xref,yref] = CoordinatesFromFig(x,y,handles)
% This function transform the Fig coordinates (X,Y) get from 
% CoordinatesFromRef into Ref coordinates (Xref,YRef) for projectiob.
%
% This function use the variable handles.rotateFig to get the orientation
% of the current figure (case 0, 90, 180, 270). 
%
% P. Lanari (25.04.13)


laSize = size(XimrotateX(handles.MapWindow.Maps.Data(1).ValuesOx,handles.rotateFig));

lesX = [0 laSize(2)]; %get(handles.axes1,'XLim');
lesY = [0 laSize(1)]; %get(handles.axes1,'YLim');

xLengthFig = lesX(2)-lesX(1);
yLengthFig = lesY(2)-lesY(1);


switch handles.rotateFig
    
    case 0
        
        xref = x;
        yref = y;
        

    case 90
        
        xref = yLengthFig - y;
        yref = x;
        
        
    case 180
        
        xref = xLengthFig - x;
        yref = yLengthFig - y;
        
        
    case 270
        
        xref = y;
        yref = xLengthFig - x;
        
end
return
  