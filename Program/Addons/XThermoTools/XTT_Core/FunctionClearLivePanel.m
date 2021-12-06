function FunctionClearLivePanel(handles)
%

axes(handles.axes_Live_PLOT1), cla, colorbar('delete'), legend OFF
set(handles.axes_Live_PLOT1,'Visible','Off');
axes(handles.axes_Live_PLOT2), cla, colorbar('delete'), legend OFF
set(handles.axes_Live_PLOT2,'Visible','Off');
axes(handles.axes_Live_PLOT3), cla, colorbar('delete'), legend OFF
set(handles.axes_Live_PLOT3,'Visible','Off');
axes(handles.axes_Live_PLOT4), cla, colorbar('delete'), legend OFF
set(handles.axes_Live_PLOT4,'Visible','Off');
axes(handles.axes_Live_PLOT5), cla, colorbar('delete'), legend OFF
set(handles.axes_Live_PLOT5,'Visible','Off');

set(handles.Live_Disp1_Text1,'Visible','Off');
set(handles.Live_Disp1_Text2,'Visible','Off');
set(handles.Live_Disp1_Text3,'Visible','Off');

set(handles.Live_Disp2_Text1,'Visible','Off');
set(handles.Live_Disp2_Text2,'Visible','Off');
set(handles.Live_Disp2_Text3,'Visible','Off');

set(handles.Live_Disp3_Text1,'Visible','Off');
set(handles.Live_Disp3_Text2,'Visible','Off');
set(handles.Live_Disp3_Text3,'Visible','Off');

UpdateLIVEWaitBar(1,1,handles);
UpdateLIVEWaitBar(2,1,handles);
UpdateLIVEWaitBar(3,1,handles);

set(handles.uitable_sliding,'Visible','Off');

drawnow

return
