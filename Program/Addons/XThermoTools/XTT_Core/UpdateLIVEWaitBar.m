function UpdateLIVEWaitBar(WhichBar,Value,handles)
%
%

switch WhichBar
    case 1
        
        if isequal(Value, 1) % Turn off display
            set(handles.Live_Disp1_Bar1,'Visible','Off');
            set(handles.Live_Disp1_Bar2,'Visible','Off');
            set(handles.Live_Disp1_Bar3,'Visible','Off');
        else
            set(handles.Live_Disp1_Bar1,'Visible','On');
            set(handles.Live_Disp1_Bar2,'Visible','On');
            set(handles.Live_Disp1_Bar3,'Visible','On');
            
            PositionBar = get(handles.Live_Disp1_Bar2,'Position');
            PositionBar(3) = Value*(PositionBar(3))+0.0000001;
            
            set(handles.Live_Disp1_Bar1,'Position',PositionBar);
        end
        
	case 2
        
        if isequal(Value, 1) % Turn off display
            set(handles.Live_Disp2_Bar1,'Visible','Off');
            set(handles.Live_Disp2_Bar2,'Visible','Off');
            set(handles.Live_Disp2_Bar3,'Visible','Off');
        else
            set(handles.Live_Disp2_Bar1,'Visible','On');
            set(handles.Live_Disp2_Bar2,'Visible','On');
            set(handles.Live_Disp2_Bar3,'Visible','On');
            
            PositionBar = get(handles.Live_Disp2_Bar2,'Position');
            PositionBar(3) = Value*(PositionBar(3))+0.0000001;
            
            set(handles.Live_Disp2_Bar1,'Position',PositionBar);
        end
        
        
	case 3
        
        if isequal(Value, 1) % Turn off display
            set(handles.Live_Disp3_Bar1,'Visible','Off');
            set(handles.Live_Disp3_Bar2,'Visible','Off');
            set(handles.Live_Disp3_Bar3,'Visible','Off');
        else
            set(handles.Live_Disp3_Bar1,'Visible','On');
            set(handles.Live_Disp3_Bar2,'Visible','On');
            set(handles.Live_Disp3_Bar3,'Visible','On');
            
            PositionBar = get(handles.Live_Disp3_Bar2,'Position');
            PositionBar(3) = Value*(PositionBar(3))+0.0000001;
            
            set(handles.Live_Disp3_Bar1,'Position',PositionBar);
        end
        
end

drawnow

return