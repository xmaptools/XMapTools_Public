function InitPanelDisp(Value,handles)
% 

set(handles.Panel_MAP,'Visible','Off');
set(handles.Panel_RES,'Visible','Off');
set(handles.Panel_LIVE,'Visible','Off');

set(handles.PanButton_MAP,'Value',0);
set(handles.PanButton_RES,'Value',0);
set(handles.PanButton_LIVE,'Value',0);

set(handles.ResPopUpListReport,'Visible','Off');

switch Value
    case 1
        set(handles.Panel_MAP,'Visible','On');
        set(handles.PanButton_MAP,'Value',1);
        
        colormap([0,0,0;RdYlBu(64)])
        
    case 2
        set(handles.Panel_RES,'Visible','On');
        set(handles.PanButton_RES,'Value',1);
        set(handles.ResPopUpListReport,'Visible','On');
        
        
    case 3
        set(handles.Panel_LIVE,'Visible','On');
        set(handles.PanButton_LIVE,'Value',1);
        
        %colormap([RdYlBu(64)])
        
end

drawnow

return