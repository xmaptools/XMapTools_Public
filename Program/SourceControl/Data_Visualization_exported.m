classdef Data_Visualization_exported < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        VisualizationTool           matlab.ui.Figure
        GridLayout                  matlab.ui.container.GridLayout
        Image                       matlab.ui.control.Image
        TabGroup                    matlab.ui.container.TabGroup
        PlotTab                     matlab.ui.container.Tab
        GridLayout7_8               matlab.ui.container.GridLayout
        VariableDefinitionsLabel    matlab.ui.control.Label
        DropDown_X                  matlab.ui.control.DropDown
        CheckBox_X_Menu             matlab.ui.control.CheckBox
        CheckBox_X_Manual           matlab.ui.control.CheckBox
        EditField_X                 matlab.ui.control.EditField
        CheckBox_Y_Menu             matlab.ui.control.CheckBox
        DropDown_Y                  matlab.ui.control.DropDown
        CheckBox_Y_Manual           matlab.ui.control.CheckBox
        EditField_Y                 matlab.ui.control.EditField
        CheckBox_Z_Menu             matlab.ui.control.CheckBox
        DropDown_Z                  matlab.ui.control.DropDown
        CheckBox_Z_Manual           matlab.ui.control.CheckBox
        EditField_Z                 matlab.ui.control.EditField
        Label_X                     matlab.ui.control.Label
        Label_Y                     matlab.ui.control.Label
        Label_Z                     matlab.ui.control.Label
        Button_Histogram            matlab.ui.control.StateButton
        Button_Binary               matlab.ui.control.StateButton
        Button_RGB                  matlab.ui.control.StateButton
        Button_Ternary              matlab.ui.control.StateButton
        PlottingModeLabel           matlab.ui.control.Label
        CheckBox_Masks              matlab.ui.control.CheckBox
        Label_Maks                  matlab.ui.control.Label
        DropDown_Masks              matlab.ui.control.DropDown
        Label_Maks_2                matlab.ui.control.Label
        CheckBox_LowRessourcesMode  matlab.ui.control.CheckBox
        DropDown_Masks_2            matlab.ui.control.DropDown
        CheckBox_FilterZeros        matlab.ui.control.CheckBox
        Label_Maks_3                matlab.ui.control.Label
        DropDown_Masks_3            matlab.ui.control.DropDown
        CheckBox_BCR_Filter         matlab.ui.control.CheckBox
        Label_Maks_4                matlab.ui.control.Label
        ExploreClassifyTab          matlab.ui.container.Tab
        GridLayout7_5               matlab.ui.container.GridLayout
        IdentifyPixelsToolsLabel    matlab.ui.control.Label
        Explore_IdentifyPixels_PolygonButton  matlab.ui.control.Button
        MakeMaskFile_Button         matlab.ui.control.Button
        Explore_ResetROI_Button     matlab.ui.control.Button
        HoldonCheckBox              matlab.ui.control.CheckBox
        UITableRoiResults           matlab.ui.control.Table
        StatisticsOtherTab          matlab.ui.container.Tab
        GridLayout7_7               matlab.ui.container.GridLayout
        Statistics_TextArea         matlab.ui.control.TextArea
        Plotting_TextArea           matlab.ui.control.TextArea
        StatisticsPlottedDataLabel  matlab.ui.control.Label
        PlottingReportlastplotonlyLabel  matlab.ui.control.Label
        OptionsTab                  matlab.ui.container.Tab
        GridLayout5                 matlab.ui.container.GridLayout
        TabGroup2                   matlab.ui.container.TabGroup
        Tab_Options_Hist            matlab.ui.container.Tab
        GridLayout7_2               matlab.ui.container.GridLayout
        Tab_Options_Binary          matlab.ui.container.Tab
        GridLayout7                 matlab.ui.container.GridLayout
        PlothistogramsusinglogYscaleCheckBox  matlab.ui.control.CheckBox
        binary_densitymapinlowresourcemodeCheckBox  matlab.ui.control.CheckBox
        Tab_Options_RGB             matlab.ui.container.Tab
        GridLayout7_3               matlab.ui.container.GridLayout
        Tab_Options_Ternary         matlab.ui.container.Tab
        GridLayout7_4               matlab.ui.container.GridLayout
        DensityTernaryLRMCheckBox   matlab.ui.control.CheckBox
        OtherTab                    matlab.ui.container.Tab
        GridLayout7_9               matlab.ui.container.GridLayout
        DensityMapResolutionField   matlab.ui.control.NumericEditField
        Densitymapresolutiondefault100Label  matlab.ui.control.Label
        ThresholdLRMEditField       matlab.ui.control.NumericEditField
        ThresholdforlowresourcemodeinEditFieldLabel  matlab.ui.control.Label
        GridLayout2                 matlab.ui.container.GridLayout
        Button_help                 matlab.ui.control.Button
        GridLayout6                 matlab.ui.container.GridLayout
        AutoZoom                    matlab.ui.control.Button
        ResetZoom                   matlab.ui.control.Button
        YminLabel                   matlab.ui.control.Label
        ManualZoom                  matlab.ui.control.Button
        GridLayout8                 matlab.ui.container.GridLayout
        Field_Ymax                  matlab.ui.control.NumericEditField
        YmaxLabel                   matlab.ui.control.Label
        GridLayout8_2               matlab.ui.container.GridLayout
        Field_Ymin                  matlab.ui.control.NumericEditField
        log_X_CheckBox              matlab.ui.control.CheckBox
        log_Y_CheckBox              matlab.ui.control.CheckBox
        GridLayout9                 matlab.ui.container.GridLayout
        Field_Xmin                  matlab.ui.control.NumericEditField
        XminLabel                   matlab.ui.control.Label
        GridLayout9_2               matlab.ui.container.GridLayout
        Field_Xmax                  matlab.ui.control.NumericEditField
        XmaxLabel                   matlab.ui.control.Label
        HoldaxislimitsCheckBox      matlab.ui.control.CheckBox
        PlotBulkCheckBox            matlab.ui.control.CheckBox
        FigPlot4                    matlab.ui.control.UIAxes
        FigPlot5                    matlab.ui.control.UIAxes
        FigPlot6                    matlab.ui.control.UIAxes
        FigPlot1                    matlab.ui.control.UIAxes
        FigPlot3                    matlab.ui.control.UIAxes
        FigPlot2                    matlab.ui.control.UIAxes
    end

    
    properties (Access = private)
        XMapToolsApp % Description
        Names % Description
        Data
        MaskFile
        LastPlot % Description
        WaitBar % Description
        
        ROI_PI_PolygonSingle 
        ROI_PI_PolygonSingle_Listener
        
        ROI_PI_PolygonSingle_Dens 
        ROI_PI_PolygonSingle_Dens_Listener
        
        ROI_PI_PolygonSingleTernary 
        ROI_PI_PolygonSingleTernary_Listener
        
        ROI_PI_PolygonSingleTernary_Dens 
        ROI_PI_PolygonSingleTernary_Dens_Listener
        
        ROI_PI_Multi
        ROI_PI_Multi_Listener
        
        LastSelectedPx 
        
        FirstPlot
        AutoPlotLims 
        
        BRC
        
        MaskUsedInPlot 
    end
    
    methods (Access = private)
        
        function CheckState(app)
            
            app.CheckBox_Masks.Enable = 'off';
            app.Label_Maks.Enable = 'off';
            app.DropDown_Masks.Enable = 'off';
            
            % HISTOGRAM
            if isequal(app.Button_Histogram.Value,1)
                
                app.EditField_Y.Visible = 'off';
                app.CheckBox_Y_Manual.Visible = 'off';
                app.DropDown_Y.Visible = 'off';
                app.CheckBox_Y_Menu.Visible = 'off';
                app.Label_Y.Visible = 'off';
                
                app.EditField_Z.Visible = 'off';
                app.CheckBox_Z_Manual.Visible = 'off';
                app.DropDown_Z.Visible = 'off';
                app.CheckBox_Z_Menu.Visible = 'off';
                app.Label_Z.Visible = 'off';
                
                app.Explore_IdentifyPixels_PolygonButton.Enable = 'off';
                
                app.HoldaxislimitsCheckBox.Visible = 'off';
                
            end
            
            % BINARY
            if isequal(app.Button_Binary.Value,1)
                
                app.EditField_Y.Visible = 'on';
                app.CheckBox_Y_Manual.Visible = 'on';
                app.DropDown_Y.Visible = 'on';
                app.CheckBox_Y_Menu.Visible = 'on';
                app.Label_Y.Visible = 'on';
                
                app.EditField_Z.Visible = 'off';
                app.CheckBox_Z_Manual.Visible = 'off';
                app.DropDown_Z.Visible = 'off';
                app.CheckBox_Z_Menu.Visible = 'off';
                app.Label_Z.Visible = 'off';
                
                if ~isempty(app.MaskFile.Names)
                    app.CheckBox_Masks.Enable = 'on';
                    app.Label_Maks.Enable = 'on';
                    app.DropDown_Masks.Enable = 'on';
                end
                
                app.Explore_IdentifyPixels_PolygonButton.Enable = 'on';
                
                app.HoldaxislimitsCheckBox.Visible = 'on';
            end
            
            % RGB
            if isequal(app.Button_RGB.Value,1)
                
                app.EditField_Y.Visible = 'on';
                app.CheckBox_Y_Manual.Visible = 'on';
                app.DropDown_Y.Visible = 'on';
                app.CheckBox_Y_Menu.Visible = 'on';
                app.Label_Y.Visible = 'on';
                
                app.EditField_Z.Visible = 'on';
                app.CheckBox_Z_Manual.Visible = 'on';
                app.DropDown_Z.Visible = 'on';
                app.CheckBox_Z_Menu.Visible = 'on';
                app.Label_Z.Visible = 'on';
                
                app.Explore_IdentifyPixels_PolygonButton.Enable = 'off';
                
                app.HoldaxislimitsCheckBox.Visible = 'off';
            end
            
            % Ternary
            if isequal(app.Button_Ternary.Value,1)
                
                app.EditField_Y.Visible = 'on';
                app.CheckBox_Y_Manual.Visible = 'on';
                app.DropDown_Y.Visible = 'on';
                app.CheckBox_Y_Menu.Visible = 'on';
                app.Label_Y.Visible = 'on';
                
                app.EditField_Z.Visible = 'on';
                app.CheckBox_Z_Manual.Visible = 'on';
                app.DropDown_Z.Visible = 'on';
                app.CheckBox_Z_Menu.Visible = 'on';
                app.Label_Z.Visible = 'on';
                
                if ~isempty(app.MaskFile.Names)
                    app.CheckBox_Masks.Enable = 'on';
                    app.Label_Maks.Enable = 'on';
                    app.DropDown_Masks.Enable = 'on';
                end
                
                app.Explore_IdentifyPixels_PolygonButton.Enable = 'on';
                
                app.HoldaxislimitsCheckBox.Visible = 'off';
            end
            
            cla(app.FigPlot2,'reset')
            app.FigPlot2.Visible = 'off';
            
            PlotData(app);
        end
        
        
        function PlotData(app)
            
            app.WaitBar = uiprogressdlg(app.VisualizationTool,'Title','Display is Being Updated, Please Wait!','Indeterminate','on');
            
            % ROI_DeleteROI(app);
            Explore_ResetROI_ButtonPushed(app, 1);      % Change in 4.4
            
            app.WaitBar.Message = 'Extracting data';
            
            if isequal(app.CheckBox_X_Menu.Value,1)
                SelData_X = app.Data(app.DropDown_X.Value).Map(:);
                Labels_X = char(app.DropDown_X.Items(app.DropDown_X.Value));
            else
                SelData_X = ExtractDataFromCode(app,app.EditField_X.Value);
                SelData_X = SelData_X(:);
                if isequal(SelData_X,-1)
                    close(app.WaitBar)
                    return
                end
                Labels_X = app.EditField_X.Value;
            end
            
            if length(app.Data) > 1
                if isequal(app.CheckBox_Y_Menu.Value,1)
                    SelData_Y = app.Data(app.DropDown_Y.Value).Map(:);
                    Labels_Y = char(app.DropDown_Y.Items(app.DropDown_Y.Value));
                else
                    SelData_Y = ExtractDataFromCode(app,app.EditField_Y.Value);
                    SelData_Y = SelData_Y(:);
                    if isequal(SelData_Y,-1)
                        close(app.WaitBar)
                        return
                    end
                    Labels_Y = app.EditField_Y.Value;
                end
            else
                SelData_Y = [];
                Labels_Y = '';
            end
            
            if length(app.Data) > 2
                if isequal(app.CheckBox_Z_Menu.Value,1)
                    SelData_Z = app.Data(app.DropDown_Z.Value).Map(:);
                    Labels_Z = char(app.DropDown_Z.Items(app.DropDown_Z.Value));
                else
                    SelData_Z = ExtractDataFromCode(app,app.EditField_Z.Value);
                    SelData_Z = SelData_Z(:);
                    if isequal(SelData_Z,-1)
                        close(app.WaitBar)
                        return
                    end
                    Labels_Z = app.EditField_Z.Value;
                end
            else
                SelData_Z = [];
                Labels_Z = '';
            end
            
            % Potential Mask...
            if app.DropDown_Masks_2.Value > 0
                
                if app.DropDown_Masks_3.Value > 0 % submask
                    MaskPixels = find(app.MaskFile.Masks(app.DropDown_Masks.Value).SubMask(app.DropDown_Masks_2.Value+1).MaskSelMaskMap == app.DropDown_Masks_3.Value);
                else
                    MaskPixels = find(app.MaskFile.Masks(app.DropDown_Masks.Value).MaskMap == app.DropDown_Masks_2.Value);
                end
                
                Mask = zeros(size(SelData_X));
                Mask(MaskPixels) = 1;
                
                SelData_X = SelData_X.*Mask;
                if isequal(length(SelData_X),length(SelData_Y))
                    SelData_Y = SelData_Y.*Mask;
                end
                if isequal(length(SelData_X),length(SelData_Z))
                    SelData_Z = SelData_Z.*Mask;
                end
            end
            
            if isequal(app.CheckBox_BCR_Filter.Value,1)
                
                Mask = app.BRC(:);
                
                SelData_X = SelData_X.*Mask;
                if isequal(length(SelData_X),length(SelData_Y))
                    SelData_Y = SelData_Y.*Mask;
                end
                if isequal(length(SelData_X),length(SelData_Z))
                    SelData_Z = SelData_Z.*Mask;
                end
                
            end
            
            if app.CheckBox_FilterZeros.Value
                if isequal(app.Button_Histogram.Value,1)
                    Ind = find(SelData_X == 0);
                    SelData_X(Ind) = NaN;
                elseif isequal(app.Button_Binary.Value,1)
                    Ind = find(SelData_X == 0 & SelData_Y == 0);
                    SelData_X(Ind) = NaN;
                    SelData_Y(Ind) = NaN;
                else
                    Ind = find(SelData_X == 0 & SelData_Y == 0 & SelData_Z == 0);
                    SelData_X(Ind) = NaN;
                    SelData_Y(Ind) = NaN;
                    SelData_Z(Ind) = NaN;
                end
            end
            
            
            % HISTOGRAM
            if isequal(app.Button_Histogram.Value,1)
                app.WaitBar.Message = 'Plotting data';
                
                app.FigPlot1.Visible = 'on';
                
                %SelData_X = app.Data(app.DropDown_X.Value).Map(:);
                tic
                Text2Disp_Report = '';
                if app.CheckBox_LowRessourcesMode.Value
                    Text2Disp_Report = [Text2Disp_Report,'Plotting data in histogram [low ressource mode: ON]\n\n'];
                else
                    Text2Disp_Report = [Text2Disp_Report,'Plotting data in histogram \n\n'];
                end
                
                Text2Disp_Statistics = '';
                Text2Disp_Statistics = [Text2Disp_Statistics,'Histogram (',Labels_X,') \n\n'];
                
                if app.AutoPlotLims
                    Idx = find(SelData_X > 0);
                else
                    if app.Field_Xmin.Value >= app.Field_Xmax.Value
                        uialert(app.VisualizationTool,'Xmin must be lower than Xmax!','XMapTools - Error')
                        close(app.WaitBar)
                        return
                    end
                    Idx = find(SelData_X >= app.Field_Xmin.Value & SelData_X <= app.Field_Xmax.Value);
                end
                
                if isempty(Idx)
                    uialert(app.VisualizationTool,'There is no data to be plotted in the selected range!','XMapTools - Error')
                    close(app.WaitBar)
                    return
                end
                
                if app.CheckBox_LowRessourcesMode.Value
                    Val = rand(size(Idx),'single');
                    Where = find(Val>(1-app.ThresholdLRMEditField.Value/100));
                    Percentage = length(Where)/length(Val)*100;
                    Idx = Idx(Where);
                else
                    Percentage = 100;
                end
                
                Text2Disp_Report = [Text2Disp_Report,'Size of dataset: ',num2str(length(Idx)),' data points (',num2str(round(Percentage)),'%% plotted)\n'];
                
                histogram(app.FigPlot1,SelData_X(Idx),'BinCountsMode','auto');
                title(app.FigPlot1,['Histogram of ',Labels_X,' (',num2str(round(Percentage)),'% data plotted)'],'interpreter','none')
                
                
                if app.AutoPlotLims
                    axis(app.FigPlot1,'auto');
                    axL = axis(app.FigPlot1);
                    app.Field_Xmin.Value = axL(1);
                    app.Field_Xmax.Value = axL(2);
                else
                    app.FigPlot1.XLim = [app.Field_Xmin.Value,app.Field_Xmax.Value];
                end
                
                
                disableDefaultInteractivity(app.FigPlot1);
                tb = axtoolbar(app.FigPlot1,{'export'});
                
                app.LastPlot.X = SelData_X(Idx);
                app.LastPlot.SelData_X = SelData_X;
                
                Calc_Mean = mean(app.LastPlot.X);
                Calc_Median = median(app.LastPlot.X);
                Calc_Std = std(app.LastPlot.X);
                Calc_HWHM = Calc_Std*sqrt(2*log(2));
                
                Text2Disp_Statistics = [Text2Disp_Statistics,'> Assuming a Gaussian distribution:\n'];
                Text2Disp_Statistics = [Text2Disp_Statistics,'Mean\t\t',num2str(Calc_Mean),'\n'];
                Text2Disp_Statistics = [Text2Disp_Statistics,'Median\t\t',num2str(Calc_Median),'\n'];
                Text2Disp_Statistics = [Text2Disp_Statistics,'Stdev\t\t',num2str(Calc_Std),'\n'];
                Text2Disp_Statistics = [Text2Disp_Statistics,'HWHM\t\t',num2str(Calc_HWHM),'\n\n'];
                
                [h,p] = kstest(app.LastPlot.X);
                switch h
                    case 1
                        Text2Disp_Statistics = [Text2Disp_Statistics,'One-sample Kolmogorov-Smirnov test: failed (p-value: ',num2str(p),') \n'];
                    case 0
                        Text2Disp_Statistics = [Text2Disp_Statistics,'One-sample Kolmogorov-Smirnov test: passed (p-value: ',num2str(p),') \n'];
                end
                h = lillietest(app.LastPlot.X);
                switch h
                    case 1
                        Text2Disp_Statistics = [Text2Disp_Statistics,'Lilliefors test: failed \n'];
                    case 0
                        Text2Disp_Statistics = [Text2Disp_Statistics,'Lilliefors test: passed \n'];
                end
                
                if ~isempty(app.FigPlot3.Children) || isequal(app.FigPlot3.Visible,'on')
                    cla(app.FigPlot3,'reset')
                    app.FigPlot3.Visible = 'off';
                end
                if ~isempty(app.FigPlot4.Children) || isequal(app.FigPlot4.Visible,'on')
                    cla(app.FigPlot4,'reset')
                    app.FigPlot4.Visible = 'off';
                end
                if ~isempty(app.FigPlot5.Children) || isequal(app.FigPlot5.Visible,'on')
                    cla(app.FigPlot5,'reset')
                    app.FigPlot5.Visible = 'off';
                end
                if ~isempty(app.FigPlot6.Children) || isequal(app.FigPlot6.Visible,'on')
                    cla(app.FigPlot6,'reset')
                    app.FigPlot6.Visible = 'off';
                end
                
                cla(app.FigPlot2,'reset')
                app.FigPlot2.Visible = 'off';
                
                app.Field_Xmin.Visible = 'on';
                app.Field_Xmax.Visible = 'on';
                app.Field_Ymin.Visible = 'off';
                app.Field_Ymax.Visible = 'off';
                app.XminLabel.Visible = 'on';
                app.XmaxLabel.Visible = 'on';
                app.YminLabel.Visible = 'off';
                app.YmaxLabel.Visible = 'off';
                
                app.AutoZoom.Enable = 'off';
                app.ResetZoom.Enable = 'off';
                app.ManualZoom.Enable = 'off';
                
                ht = toc;
                Text2Disp_Report = [Text2Disp_Report,'Plotting histogram & adjusting plot options: ',num2str(ht), 's\n'];
                
                tic
                drawnow
                hf = toc;
                Text2Disp_Report = [Text2Disp_Report,'Drawnow : ',num2str(hf), 's\n'];
                
                app.Plotting_TextArea.Value = sprintf(Text2Disp_Report);
                app.Statistics_TextArea.Value = sprintf(Text2Disp_Statistics);
            end
            
            
            % Binary Diagram
            if isequal(app.Button_Binary.Value,1)
                
                %disp(' ')
                Text2Disp_Report = '';
                if app.CheckBox_LowRessourcesMode.Value
                    Text2Disp_Report = [Text2Disp_Report,'Plotting binary data [low ressource mode: ON]\n\n'];
                else
                    Text2Disp_Report = [Text2Disp_Report,'Plotting binary data \n\n'];
                end
                
                Text2Disp_Statistics = '';
                Text2Disp_Statistics = [Text2Disp_Statistics,'Binary (',Labels_X,' vs ',Labels_Y,') \n\n'];
                
                app.WaitBar.Message = 'Plotting data';
                
                app.FigPlot1.Visible = 'on';
                %app.FigPlot3.Visible = 'on';
                %app.FigPlot4.Visible = 'on';
                %app.FigPlot5.Visible = 'on';
                
                %SelData_X = app.Data(app.DropDown_X.Value).Map(:);
                %SelData_Y = app.Data(app.DropDown_Y.Value).Map(:);
                
                tic
                if app.CheckBox_Masks.Value
                    app.MaskUsedInPlot = 1;
                    
                    MaskId = app.DropDown_Masks.Value;
                    
                    MaskNames = app.MaskFile.Masks(MaskId).Names(2:end);
                    Colors2Plot = app.MaskFile.Masks(MaskId).Colors(2:end,:);    % InterpColor(app,length(MaskNames));
                    
                    Mask = app.MaskFile.Masks(MaskId).MaskMap;
                    
                    MaskNamesPlotted = {};
                    % we need to clean the axes and re-generate the plot
                    % (slow)
                    cla(app.FigPlot1,'reset')
                    hold(app.FigPlot1,'on')
                    for i = 1:length(MaskNames)
                        Idx = find(Mask(:) == i & SelData_X > 0 & SelData_Y > 0);
                        
                        if ~isempty(Idx)
                            if app.CheckBox_LowRessourcesMode.Value
                                Val = rand(size(Idx),'single');
                                Where = find(Val>(1-app.ThresholdLRMEditField.Value/100));
                                Percentage = length(Where)/length(Val)*100;
                                Idx = Idx(Where);
                                MaskNamesPlotted{end+1} = MaskNames{i};
                            else
                                Percentage = 100;
                            end
                            
                            plot(app.FigPlot1,-1,-1,'.k','Markersize',30,'Color',Colors2Plot(i,:))
                            plot(app.FigPlot1,SelData_X(Idx),SelData_Y(Idx),'.k','Markersize',1,'Color',Colors2Plot(i,:),'HandleVisibility','off')
                            
                        end
                    end
                    hold(app.FigPlot1,'off')
                    legend(app.FigPlot1,MaskNamesPlotted,'Location','best')
                    app.FigPlot1.Color = [0.1,0.1,0.1];
                    
                else
                    Idx = find(SelData_X > 0 & SelData_Y > 0);
                    
                    if app.CheckBox_LowRessourcesMode.Value
                        Val = rand(size(Idx),'single');
                        Where = find(Val>(1-app.ThresholdLRMEditField.Value/100));
                        Percentage = length(Where)/length(Val)*100;
                        Idx = Idx(Where);
                    else
                        Percentage = 100;
                    end
                    
                    if ~isequal(length(app.FigPlot1.Children),1) || app.FirstPlot
                        cla(app.FigPlot1,'reset')
                        plot(app.FigPlot1,SelData_X(Idx),SelData_Y(Idx),'.k','Markersize',1)
                        app.FirstPlot = 0;
                    elseif isequal(app.MaskUsedInPlot,1)
                        cla(app.FigPlot1,'reset')
                        plot(app.FigPlot1,SelData_X(Idx),SelData_Y(Idx),'.k','Markersize',1)
                        app.MaskUsedInPlot = 0;
                    else
                        app.FigPlot1.Children(1).XData = SelData_X(Idx);
                        app.FigPlot1.Children(1).YData = SelData_Y(Idx);
                    end
                    app.FigPlot1.Color = [1,1,1]; 
                    
                    if app.PlotBulkCheckBox.Value 
                        hold(app.FigPlot1,'on')
                        plot(app.FigPlot1,mean(SelData_X(Idx)),mean(SelData_Y(Idx)),'o','MarkerFaceColor','red',"MarkerEdgeColor",'red','markersize',10)
                        % plot(app.FigPlot1,median(SelData_X(Idx)),median(SelData_Y(Idx)),'o','MarkerFaceColor','blue',"MarkerEdgeColor",'blue','markersize',10)
                        hold(app.FigPlot1,'off')
                        
                        Text2Disp_Statistics = [Text2Disp_Statistics,'Bulk Value (average): ',Labels_X,'=',num2str(mean(SelData_X(Idx))),' ',Labels_Y,'=',num2str(mean(SelData_Y(Idx))),' \n'];
                        Text2Disp_Statistics = [Text2Disp_Statistics,'Bulk Value (std): ',Labels_X,'=',num2str(std(SelData_X(Idx))),' ',Labels_Y,'=',num2str(std(SelData_Y(Idx))),' \n\n'];
                        % Text2Disp_Statistics = [Text2Disp_Statistics,'Bulk Value (median): ',Labels_X,'=',num2str(median(SelData_X(Idx))),' ',Labels_Y,'=',num2str(median(SelData_Y(Idx))),' \n\n'];
                    end
                end
                
                if app.log_X_CheckBox.Value 
                    app.FigPlot1.XScale = 'log';
                else
                    app.FigPlot1.XScale = 'linear';
                end
                if app.log_Y_CheckBox.Value
                    app.FigPlot1.YScale = 'log';
                else
                    app.FigPlot1.YScale = 'linear';
                end
                
                Idx = find(SelData_X > 0 & SelData_Y > 0);
                                
                ht = toc;
                Text2Disp_Report = [Text2Disp_Report,'Size of dataset: ',num2str(length(Idx)),' data points (',num2str(round(Percentage)),'%% plotted)\n'];
                Text2Disp_Report = [Text2Disp_Report,'Extracting & Plotting data: ',num2str(ht),'s\n'];
                
                tic
                app.FigPlot1.XTickMode = 'Auto';
                app.FigPlot1.YTickMode = 'Auto';
                
                Xmin = min(SelData_X(Idx));
                Xmax = max(SelData_X(Idx));
                Ymin = min(SelData_Y(Idx));
                Ymax = max(SelData_Y(Idx));
                
                if isequal(app.HoldaxislimitsCheckBox.Value,0)
                    axis(app.FigPlot1,[Xmin,Xmax,Ymin,Ymax])
                    
                    app.Field_Xmin.Value = Xmin;
                    app.Field_Xmax.Value = Xmax;
                    app.Field_Ymin.Value = Ymin;
                    app.Field_Ymax.Value = Ymax;
                    
                    app.LastPlot.X = SelData_X(Idx);
                    app.LastPlot.Y = SelData_Y(Idx);
                    app.LastPlot.SelData_X = SelData_X;
                    app.LastPlot.SelData_Y = SelData_Y;
                end
                
                R = corrcoef(app.LastPlot.X,app.LastPlot.Y);
                Text2Disp_Statistics = [Text2Disp_Statistics,'Correlation coefficient\t',num2str(R(1,2)),'\n'];
                
                disableDefaultInteractivity(app.FigPlot1);
                tb = axtoolbar(app.FigPlot1,{'export'});
                
                title(app.FigPlot1,['Binary plot ',char(Labels_X),' vs ',char(Labels_Y),' (',num2str(round(Percentage)),'% data plotted)'],'interpreter','none')
                xlabel(app.FigPlot1,char(Labels_X),'interpreter','none')
                ylabel(app.FigPlot1,char(Labels_Y),'interpreter','none')
                
                ht = toc;
                Text2Disp_Report = [Text2Disp_Report,'Adjusting plot; force plotting: ',num2str(ht), 's\n'];
                
                if ~app.CheckBox_LowRessourcesMode.Value
                    tic
                    histogram(app.FigPlot3,app.LastPlot.X,'BinMethod','auto');
                    xlabel(app.FigPlot3,char(Labels_X),'interpreter','none')
                    ylabel(app.FigPlot3,'#')
                    title(app.FigPlot3,['Histogram for ',char(Labels_X)],'interpreter','none')
                    if isequal(app.PlothistogramsusinglogYscaleCheckBox.Value,1)
                        app.FigPlot3.YScale = 'log';
                    else
                        app.FigPlot3.YScale = 'linear';
                    end
                    
                    disableDefaultInteractivity(app.FigPlot3);
                    tb = axtoolbar(app.FigPlot3,{'export'});
                    
                    app.FigPlot3.FontSize = 9;
                    
                    ht = toc;
                    Text2Disp_Report = [Text2Disp_Report,'Plotting the first histogram : ',num2str(ht), 's\n'];
                    
                    tic
                    histogram(app.FigPlot4,app.LastPlot.Y,'BinMethod','auto');
                    xlabel(app.FigPlot4,char(Labels_Y),'interpreter','none')
                    title(app.FigPlot4,['Histogram for ',char(Labels_Y)],'interpreter','none')
                    ylabel(app.FigPlot4,'#')
                    if isequal(app.PlothistogramsusinglogYscaleCheckBox.Value,1)
                        app.FigPlot4.YScale = 'log';
                    else
                        app.FigPlot4.YScale = 'linear';
                    end
                    
                    disableDefaultInteractivity(app.FigPlot4);
                    tb = axtoolbar(app.FigPlot4,{'export'});
                    
                    app.FigPlot4.FontSize = 9;
                    
                    ht = toc;
                    Text2Disp_Report = [Text2Disp_Report,'Plotting the second histogram : ',num2str(ht), 's\n'];
                    
                    if app.log_X_CheckBox.Value
                        app.FigPlot3.XScale = 'log';
                        app.FigPlot4.XScale = 'log';
                    else
                        app.FigPlot3.XScale = 'linear';
                        app.FigPlot4.XScale = 'linear';
                    end
                    
                    app.FigPlot3.Visible = 'on';
                    app.FigPlot4.Visible = 'on';
                else
                    if ~isempty(app.FigPlot3.Children) || isequal(app.FigPlot3.Visible,'on')
                        cla(app.FigPlot3,'reset');
                        app.FigPlot3.Visible = 'off';
                        cla(app.FigPlot4,'reset');
                        app.FigPlot4.Visible = 'off';
                    end
                end
                
                app.WaitBar.Message = 'Generating density map';
                
                tic
                plotDensityBin(app)
                
                ht = toc;
                Text2Disp_Report = [Text2Disp_Report,'Calculating and plotting the density map : ',num2str(ht), 's\n'];
                
                app.WaitBar.Message = 'Finalizing plots';
                
                tic
                app.FigPlot6.Visible = 'off';
                app.FigPlot2.Visible = 'off';
                
                app.TabGroup2.SelectedTab = app.Tab_Options_Binary;
                
                app.Field_Xmin.Visible = 'on';
                app.Field_Xmax.Visible = 'on';
                app.Field_Ymin.Visible = 'on';
                app.Field_Ymax.Visible = 'on';
                app.XminLabel.Visible = 'on';
                app.XmaxLabel.Visible = 'on';
                app.YminLabel.Visible = 'on';
                app.YmaxLabel.Visible = 'on';
                
                cla(app.FigPlot6,'reset');
                app.FigPlot6.Visible = 'off';
                
                app.AutoZoom.Enable = 'on';
                app.ResetZoom.Enable = 'on';
                app.ManualZoom.Enable = 'on';
                
                ht = toc;
                Text2Disp_Report = [Text2Disp_Report,'Final update of plots & interface : ',num2str(ht), 's\n'];
                
                tic
                drawnow
                hf = toc;
                Text2Disp_Report = [Text2Disp_Report,'Drawnow : ',num2str(hf), 's\n'];
                
                app.Plotting_TextArea.Value = sprintf(Text2Disp_Report);
                app.Statistics_TextArea.Value = sprintf(Text2Disp_Statistics);
            end
            
            
            % Ternary diagram
            if isequal(app.Button_Ternary.Value,1)
                
                app.WaitBar.Message = 'Plotting data';
                
                Text2Disp_Report = '';
                if app.CheckBox_LowRessourcesMode.Value
                    Text2Disp_Report = [Text2Disp_Report,'Plotting ternary data [low ressource mode: ON]\n\n'];
                else
                    Text2Disp_Report = [Text2Disp_Report,'Plotting ternary data \n\n'];
                end
                
                Text2Disp_Statistics = '';
                Text2Disp_Statistics = [Text2Disp_Statistics,'Ternary Diagram (',Labels_X,' vs ',Labels_Y,' vs ',Labels_Z,') \n\n'];
                
                a = char(Labels_X);
                b = char(Labels_Y);
                c = char(Labels_Z);
                
                Xa = SelData_X./(SelData_X+SelData_Y+SelData_Z);
                Xb = SelData_Y./(SelData_X+SelData_Y+SelData_Z);
                Xc = SelData_Z./(SelData_X+SelData_Y+SelData_Z);
                
                tic
                cla(app.FigPlot1,'reset'); % this could be optimized as for binary
                
                if app.CheckBox_Masks.Value
                    
                    plot(app.FigPlot1,[0,0.5,1,0],[0,1,0,0],'-','color',[1,1,1],'LineWidth',2,'HandleVisibility','off')
                    hold(app.FigPlot1,'on')
                    
                    MaskId = app.DropDown_Masks.Value;
                    
                    MaskNames = app.MaskFile.Masks(MaskId).Names(2:end);
                    Colors2Plot = InterpColor(app,length(MaskNames));
                    
                    Mask = app.MaskFile.Masks(MaskId).MaskMap;
                    
                    hold(app.FigPlot1,'on')
                    for i = 1:length(MaskNames)
                        Idx = find(Mask(:) == i & Xa > 0 & Xb > 0 & Xc > 0);
                        
                        if app.CheckBox_LowRessourcesMode.Value
                            Val = rand(size(Idx),'single');
                            Where = find(Val>(1-app.ThresholdLRMEditField.Value/100));
                            Percentage = length(Where)/length(Val)*100;
                            Idx = Idx(Where);
                        else
                            Percentage = 100;
                        end
                        
                        if ~isempty(Idx)
                            plot(app.FigPlot1,-10,-10,'.','Markersize',30,'Color',Colors2Plot(i,:))
                            plot(app.FigPlot1,Xc(Idx)+(1-(Xc(Idx)+Xa(Idx)))./2,Xb(Idx),'.k','markersize',1,'Color',Colors2Plot(i,:),'HandleVisibility','off')
                            
                            %plot(app.FigPlot1,-1,-1,'.k','Markersize',30,'Color',Colors2Plot(i,:))
                            %plot(app.FigPlot1,SelData_X(Idx),SelData_Y(Idx),'.k','Markersize',1,'Color',Colors2Plot(i,:),'HandleVisibility','off')
                        end
                    end
                    hold(app.FigPlot1,'off')
                    legend(app.FigPlot1,MaskNames,'Location','best')
                    app.FigPlot1.Color = [0.1,0.1,0.1];
                    Idx = find(Xa > 0 & Xb > 0 & Xc > 0);
                else
                    
                    plot(app.FigPlot1,[0,0.5,1,0],[0,1,0,0],'-','color',[0,0,0],'LineWidth',1,'HandleVisibility','off')
                    hold(app.FigPlot1,'on')
                    
                    plot(app.FigPlot1,[0.4,0.8],[0.8,0],':','color',[0,0,0,0.5],'HandleVisibility','off')
                    plot(app.FigPlot1,[0.3,0.6],[0.6,0],':','color',[0,0,0,0.5],'HandleVisibility','off')
                    plot(app.FigPlot1,[0.2,0.4],[0.4,0],':','color',[0,0,0,0.5],'HandleVisibility','off')
                    plot(app.FigPlot1,[0.1,0.2],[0.2,0],':','color',[0,0,0,0.5],'HandleVisibility','off')
                    
                    plot(app.FigPlot1,[0.8,0.9],[0,0.2],':','color',[0,0,0,0.5],'HandleVisibility','off')
                    plot(app.FigPlot1,[0.6,0.8],[0,0.4],':','color',[0,0,0,0.5],'HandleVisibility','off')
                    plot(app.FigPlot1,[0.4,0.7],[0,0.6],':','color',[0,0,0,0.5],'HandleVisibility','off')
                    plot(app.FigPlot1,[0.2,0.6],[0,0.8],':','color',[0,0,0,0.5],'HandleVisibility','off')
                    
                    plot(app.FigPlot1,[0.4,0.6],[0.8,0.8],':','color',[0,0,0,0.5],'HandleVisibility','off')
                    plot(app.FigPlot1,[0.3,0.7],[0.6,0.6],':','color',[0,0,0,0.5],'HandleVisibility','off')
                    plot(app.FigPlot1,[0.2,0.8],[0.4,0.4],':','color',[0,0,0,0.5],'HandleVisibility','off')
                    plot(app.FigPlot1,[0.1,0.9],[0.2,0.2],':','color',[0,0,0,0.5],'HandleVisibility','off')
                    
                    Idx = find(Xa > 0 & Xb > 0 & Xc > 0);
                    
                    if app.CheckBox_LowRessourcesMode.Value
                        Val = rand(size(Idx),'single');
                        Where = find(Val>(1-app.ThresholdLRMEditField.Value/100));
                        Percentage = length(Where)/length(Val)*100;
                        Idx = Idx(Where);
                    else
                        Percentage = 100;
                    end
                    
                    plot(app.FigPlot1,Xc(Idx)+(1-(Xc(Idx)+Xa(Idx)))./2,Xb(Idx),'.k','markersize',1)
                    app.FigPlot1.Color = [1,1,1];
                    
                    if app.PlotBulkCheckBox.Value 
                        hold(app.FigPlot1,'on')
                        plot(app.FigPlot1,mean(Xc(Idx)+(1-(Xc(Idx)+Xa(Idx)))./2),mean(Xb(Idx)),'o','MarkerFaceColor','red',"MarkerEdgeColor",'red','markersize',10)
                        hold(app.FigPlot1,'off')
                        
                        Text2Disp_Statistics = [Text2Disp_Statistics,'Bulk Value (average): ',Labels_X,'=',num2str(mean(SelData_X(Idx))),' ',Labels_Y,'=',num2str(mean(SelData_Y(Idx))),' ',Labels_Z,'=',num2str(mean(SelData_Z(Idx))),' \n'];
                        Text2Disp_Statistics = [Text2Disp_Statistics,'Bulk Value (std): ',Labels_X,'=',num2str(std(SelData_X(Idx))),' ',Labels_Y,'=',num2str(std(SelData_Y(Idx))),' ',Labels_Z,'=',num2str(std(SelData_Z(Idx))),' \n\n'];
                        
                        Text2Disp_Statistics = [Text2Disp_Statistics,'Bulk Value (fraction): ',Labels_X,'=',num2str(mean(Xa(Idx))),' ',Labels_Y,'=',num2str(mean(Xb(Idx))),' ',Labels_Z,'=',num2str(mean(Xc(Idx))),' \n\n'];
                    end
                end
                ht = toc;
                Text2Disp_Report = [Text2Disp_Report,'Size of dataset: ',num2str(length(Idx)),' data points (',num2str(round(Percentage)),'%% plotted)\n'];
                Text2Disp_Report = [Text2Disp_Report,'Extracting & Plotting data: ',num2str(ht),'s\n'];
                
                tic
                axis(app.FigPlot1,[-0.2 1.2 -0.12 1.12])
                
                app.LastPlot.X = Xc(Idx)+(1-(Xc(Idx)+Xa(Idx)))./2;
                app.LastPlot.Y = Xb(Idx);
                app.LastPlot.SelData_X = Xc+(1-(Xc+Xa))./2;
                app.LastPlot.SelData_Y = Xb;
                
                if app.CheckBox_Masks.Value
                    ColorText = [1,1,1];
                else
                    ColorText = [0,0,0];
                end
                text(app.FigPlot1,0-(length(char(a))/2)*0.018,-0.05,char(a),'Color',ColorText,'interpreter','none')
                text(app.FigPlot1,0.5-(length(char(b))/2)*0.018,1.05,char(b),'Color',ColorText,'interpreter','none')
                text(app.FigPlot1,1-(length(char(c))/2)*0.018,-0.05,char(c),'Color',ColorText,'interpreter','none')
                
                hold(app.FigPlot1,'off')
                
                title(app.FigPlot1,['Ternary plot ',a,' - ',b,' - ',c,' (',num2str(round(Percentage)),'% data plotted)'],'interpreter','none')
                
                app.FigPlot1.XTickMode = 'Manual';
                app.FigPlot1.YTickMode = 'Manual';
                app.FigPlot1.XTick = [];
                app.FigPlot1.YTick = [];
                
                disableDefaultInteractivity(app.FigPlot1);
                tb = axtoolbar(app.FigPlot1,{'export'});
                
                ht = toc;
                Text2Disp_Report = [Text2Disp_Report,'Adjusting plot: ',num2str(ht), 's\n'];
                
                if ~app.CheckBox_LowRessourcesMode.Value
                    cla(app.FigPlot3,'reset');
                    histogram(app.FigPlot3,SelData_X(Idx),'BinMethod','auto');
                    xlabel(app.FigPlot3,a,'interpreter','none')
                    ylabel(app.FigPlot3,'#')
                    title(app.FigPlot3,['Histogram for ',a],'interpreter','none')
                    if isequal(app.PlothistogramsusinglogYscaleCheckBox.Value,1)
                        app.FigPlot3.YScale = 'log';
                    else
                        app.FigPlot3.YScale = 'linear';
                    end
                    disableDefaultInteractivity(app.FigPlot3);
                    tb = axtoolbar(app.FigPlot3,{'export'});
                    app.FigPlot3.FontSize = 9;
                    
                    cla(app.FigPlot4,'reset');
                    histogram(app.FigPlot4,SelData_Y(Idx),'BinMethod','auto');
                    xlabel(app.FigPlot4,b,'interpreter','none')
                    title(app.FigPlot4,['Histogram for ',b],'interpreter','none')
                    ylabel(app.FigPlot4,'#')
                    if isequal(app.PlothistogramsusinglogYscaleCheckBox.Value,1)
                        app.FigPlot4.YScale = 'log';
                    else
                        app.FigPlot4.YScale = 'linear';
                    end
                    disableDefaultInteractivity(app.FigPlot4);
                    tb = axtoolbar(app.FigPlot4,{'export'});
                    app.FigPlot4.FontSize = 9;
                    
                    cla(app.FigPlot6,'reset');
                    histogram(app.FigPlot6,SelData_Z(Idx),'BinMethod','auto');
                    xlabel(app.FigPlot6,c,'interpreter','none')
                    title(app.FigPlot6,['Histogram for ',c],'interpreter','none')
                    ylabel(app.FigPlot6,'#')
                    if isequal(app.PlothistogramsusinglogYscaleCheckBox.Value,1)
                        app.FigPlot6.YScale = 'log';
                    else
                        app.FigPlot6.YScale = 'linear';
                    end
                    disableDefaultInteractivity(app.FigPlot6);
                    tb = axtoolbar(app.FigPlot6,{'export'});
                    app.FigPlot6.FontSize = 9;
                else
                    if ~isempty(app.FigPlot3.Children) || isequal(app.FigPlot3.Visible,'on')
                        cla(app.FigPlot3,'reset');
                        app.FigPlot3.Visible = 'off';
                    end
                    if ~isempty(app.FigPlot4.Children) || isequal(app.FigPlot4.Visible,'on')
                        cla(app.FigPlot4,'reset');
                        app.FigPlot4.Visible = 'off';
                    end
                    if ~isempty(app.FigPlot6.Children) || isequal(app.FigPlot6.Visible,'on')
                        cla(app.FigPlot6,'reset');
                        app.FigPlot6.Visible = 'off';
                    end
                end
                
                app.Field_Xmin.Visible = 'off';
                app.Field_Xmax.Visible = 'off';
                app.Field_Ymin.Visible = 'off';
                app.Field_Ymax.Visible = 'off';
                app.XminLabel.Visible = 'off';
                app.XmaxLabel.Visible = 'off';
                app.YminLabel.Visible = 'off';
                app.YmaxLabel.Visible = 'off';
                
                if app.DensityTernaryLRMCheckBox.Value
                    tic
                    app.WaitBar.Message = 'Generating density map';
                    plotDensityTern(app)
                    ht = toc;
                    Text2Disp_Report = [Text2Disp_Report,'Calculating and plotting the density map: ',num2str(ht), 's\n'];
                else
                    if ~isempty(app.FigPlot5.Children) || isequal(app.FigPlot5.Visible,'on')
                        cla(app.FigPlot5,'reset');
                        app.FigPlot5.Visible = 'off';
                    end
                end
                
                app.WaitBar.Message = 'Finalizing plots';
                
                tic
                
                app.AutoZoom.Enable = 'off';
                app.ResetZoom.Enable = 'off';
                app.ManualZoom.Enable = 'off';
                
                drawnow
                ht = toc;
                Text2Disp_Report = [Text2Disp_Report,'Final update of plots & interface : ',num2str(ht), 's\n'];   
                
                app.Plotting_TextArea.Value = sprintf(Text2Disp_Report);
                app.Statistics_TextArea.Value = sprintf(Text2Disp_Statistics);
            end
            
            % RGB plot
            if isequal(app.Button_RGB.Value,1)
                
                app.WaitBar.Message = 'Plotting data';
                
                Text2Disp_Statistics = '';
                Text2Disp_Statistics = [Text2Disp_Statistics,'RGB Plot (',Labels_X,' vs ',Labels_Y,' vs ',Labels_Z,') \n\n'];
                
                Text2Disp_Report = '';
                Text2Disp_Report = [Text2Disp_Report,'no report available\n\n'];
                
                ReshapeSize = size(app.Data(1).Map);
                
                SelData_X = reshape(SelData_X,ReshapeSize);
                SelData_Y = reshape(SelData_Y,ReshapeSize);
                SelData_Z = reshape(SelData_Z,ReshapeSize);
                
                a = char(Labels_X);
                b = char(Labels_Y);
                c = char(Labels_Z);
                
                RedChannel = zeros(size(SelData_X));
                GreenChannel = zeros(size(SelData_X));
                BlueChannel = zeros(size(SelData_X));
                
                RedChannel = SelData_X./max(SelData_X(:));
                GreenChannel = SelData_Y./max(SelData_Y(:));
                BlueChannel = SelData_Z./max(SelData_Z(:)); 
                
                RgbImage = cat(3, RedChannel, GreenChannel, BlueChannel);
                
                cla(app.FigPlot1,'reset');
                image(app.FigPlot1,RgbImage)
                
                title(app.FigPlot1,['RGB image ',a,' - ',b,' - ',c],'interpreter','none')
                
                axis(app.FigPlot1,'image');
                
                disableDefaultInteractivity(app.FigPlot1);
                tb = axtoolbar(app.FigPlot1,{'export'});
                
                cla(app.FigPlot3,'reset');
                app.FigPlot3.Visible = 'off';
                cla(app.FigPlot4,'reset');
                app.FigPlot4.Visible = 'off';
                cla(app.FigPlot6,'reset');
                app.FigPlot6.Visible = 'off';
                
                close(app.WaitBar)
                
                plotTriangle4RGB(app,a,b,c)
                
                app.Field_Xmin.Visible = 'off';
                app.Field_Xmax.Visible = 'off';
                app.Field_Ymin.Visible = 'off';
                app.Field_Ymax.Visible = 'off';
                app.XminLabel.Visible = 'off';
                app.XmaxLabel.Visible = 'off';
                app.YminLabel.Visible = 'off';
                app.YmaxLabel.Visible = 'off';
                
                app.AutoZoom.Enable = 'off';
                app.ResetZoom.Enable = 'off';
                app.ManualZoom.Enable = 'off';
                
                app.Plotting_TextArea.Value = sprintf(Text2Disp_Report);
                app.Statistics_TextArea.Value = sprintf(Text2Disp_Statistics);
            end
            
            
            %keyboard
            
            close(app.WaitBar)
            
        end
        
        
        function plotDensityBin(app)
            
            if app.CheckBox_LowRessourcesMode.Value && isequal(app.binary_densitymapinlowresourcemodeCheckBox.Value,0)
                if ~isempty(app.FigPlot5.Children) || isequal(app.FigPlot5.Visible,'on')
                    cla(app.FigPlot5,'reset');
                    app.FigPlot5.Visible = 'off';
                end
                return
            end
            
            % Density map:
            Lims = axis(app.FigPlot1);
            E1 = app.LastPlot.X;
            E2 = app.LastPlot.Y;
            
            Xmin = Lims(1);
            Xmax = Lims(2);
            Ymin = Lims(3);
            Ymax = Lims(4);
            
            if app.log_X_CheckBox.Value
                E1 = log10(E1);
                Xmin = log10(Xmin);
                Xmax = log10(Xmax);
            end
            if app.log_Y_CheckBox.Value
                E2 = log10(E2);
                Ymin = log10(Ymin);
                Ymax = log10(Ymax);
            end
            
            points = [E1, E2];
            points  = points(find(points(:,1) >= Xmin & points(:,1) <= Xmax & points(:,2) >= Ymin & points(:,2) <= Ymax ),:);
            
            grid = app.DensityMapResolutionField.Value;   %refinement of map
            
            minvals = [Xmin Ymin];
            maxvals = [Xmax Ymax];
            
            rangevals = maxvals - minvals;
            
            xidx = 1 + round((points(:,1) - minvals(1)) ./ rangevals(1) * (grid-1));
            yidx = 1 + round((points(:,2) - minvals(2)) ./ rangevals(2) * (grid-1));
            
            density = accumarray([yidx, xidx], 1, [grid,grid]);  %note y is rows, x is cols
            
            cla(app.FigPlot5,'reset');
            imagesc(app.FigPlot5, log(density), 'xdata', [minvals(1), maxvals(1)], 'ydata', [minvals(2), maxvals(2)]);
            axis(app.FigPlot5,[Xmin Xmax Ymin Ymax]) 
            
            Cmin = min(log(density(find(density))))+1;
            Cmax = max(log(density(:)))+1;
            
            app.FigPlot5.YDir = 'normal';
            colormap(app.FigPlot5,Turbo(app,64))
            title(app.FigPlot5,['log(Density) map ',char(app.DropDown_X.Items(app.DropDown_X.Value)),' vs ',char(app.DropDown_Y.Items(app.DropDown_Y.Value))],'interpreter','none')
            xlabel(app.FigPlot5,char(app.DropDown_X.Items(app.DropDown_X.Value)),'interpreter','none')
            ylabel(app.FigPlot5,char(app.DropDown_Y.Items(app.DropDown_Y.Value)),'interpreter','none')
            
            app.FigPlot5.FontSize = 9;
            
            tb = axtoolbar(app.FigPlot5,{'export'});
            
            tk = logspace(log(Cmin),log(Cmax),5);
            
            hc = colorbar(app.FigPlot5,'vertical');
            colormap(app.FigPlot5,[Turbo(app,64)])
            
            Labels = get(hc,'YTickLabel');
            
            for i = 1:length(Labels)
                LabelsOk{i} = num2str(round(exp(str2num(char(Labels(i,:))))),'%.0f');
            end
            
            set(hc,'YTickLabel',LabelsOk);
            drawnow
            
            app.FigPlot5.Visible = 'on';
            
            
            
            return
            
            % previous version (slow) below
            
            if app.CheckBox_LowRessourcesMode.Value
                if ~isempty(app.FigPlot5.Children) || isequal(app.FigPlot5.Visible,'on')
                    cla(app.FigPlot5,'reset');
                    app.FigPlot5.Visible = 'off';
                end
                return
            end
            
            % Density map:
            
            MapSize = 100;
            %tic
            Lims = axis(app.FigPlot1);
            E1 = app.LastPlot.X;
            E2 = app.LastPlot.Y;
            
            Xmin = Lims(1);
            Xmax = Lims(2);
            Ymin = Lims(3);
            Ymax = Lims(4);
            
            % Update limits in the main plot
            app.Field_Xmin.Value = Xmin;
            app.Field_Xmax.Value = Xmax;
            app.Field_Ymin.Value = Ymin;
            app.Field_Ymax.Value = Ymax;
            
            Xstep = (Xmax-Xmin)/(MapSize-1);
            Ystep = (Ymax-Ymin)/(MapSize-1);
            
            Xi = Xmin:Xstep:Xmax;
            Yi = Ymin:Ystep:Ymax;
            
            X = E1(find(E1 > 0 & E2 > 0));
            Y = E2(find(E1 > 0 & E2 > 0));
            
            minx = Xmin;
            maxx = Xmax;
            miny = Ymin;
            maxy = Ymax;
            
            nbins = [min(numel(unique(X)),MapSize) ,min(numel(unique(Y)),MapSize) ];
            
            edges1 = linspace(minx, maxx, nbins(1)+1);
            ctrs1 = edges1(1:end-1) + .5*diff(edges1);
            edges1 = [-Inf edges1(2:end-1) Inf];
            edges2 = linspace(miny, maxy, nbins(2)+1);
            ctrs2 = edges2(1:end-1) + .5*diff(edges2);
            edges2 = [-Inf edges2(2:end-1) Inf];
            
            [n,p] = size(X);
            bin = zeros(n,2);
            % Reverse the columns to put the first column of X along the horizontal
            % axis, the second along the vertical.
            [dum,bin(:,2)] = histc(X,edges1);
            [dum,bin(:,1)] = histc(Y,edges2);
            
            ImageDistribution2D = accumarray(bin,1,nbins([2 1]));
            
            imagesc(app.FigPlot5,Xi,Yi,log(ImageDistribution2D))
            axis(app.FigPlot5,[Xmin Xmax Ymin Ymax]) 
            
            Cmin = min(log(ImageDistribution2D(find(ImageDistribution2D))))+1;
            Cmax = max(log(ImageDistribution2D(:)))+1;
            
            app.FigPlot5.YDir = 'normal';
            colormap(app.FigPlot5,Turbo(app,64))
            title(app.FigPlot5,['Density map ',char(app.DropDown_X.Items(app.DropDown_X.Value)),' vs ',char(app.DropDown_Y.Items(app.DropDown_Y.Value))],'interpreter','none')
            xlabel(app.FigPlot5,char(app.DropDown_X.Items(app.DropDown_X.Value)),'interpreter','none')
            ylabel(app.FigPlot5,char(app.DropDown_Y.Items(app.DropDown_Y.Value)),'interpreter','none')
            
            tb = axtoolbar(app.FigPlot5,{'export'});
            
            tk = logspace(log(Cmin),log(Cmax),5);
            
            %originalSize1 = get(gca, 'Position');
            hc = colorbar(app.FigPlot5,'vertical');
            colormap(app.FigPlot5,[Turbo(app,64)])
            %set(gca,'Position',originalSize1);
            
            Labels = get(hc,'YTickLabel');
            
            for i = 1:length(Labels)
                LabelsOk{i} = num2str(round(exp(str2num(char(Labels(i,:))))),'%.0f');
            end
            
            %LabelsOk;
            
            set(hc,'YTickLabel',LabelsOk);
            drawnow
            
            app.FigPlot5.Visible = 'on';
            %toc
            
        end
        
        
        function plotDensityTern(app)
            
            if app.CheckBox_LowRessourcesMode.Value && isequal(app.DensityTernaryLRMCheckBox.Value,0)
                if ~isempty(app.FigPlot5.Children) || isequal(app.FigPlot5.Visible,'on')
                    cla(app.FigPlot5,'reset');
                    app.FigPlot5.Visible = 'off';
                end
                return
            end
            
            % Density map:
            Lims = axis(app.FigPlot1);
            E1 = app.LastPlot.X;
            E2 = app.LastPlot.Y;
            
            Xmin = Lims(1);
            Xmax = Lims(2);
            Ymin = Lims(3);
            Ymax = Lims(4);
            
            points = [E1, E2];
            points  = points(find(points(:,1) >= Xmin & points(:,1) <= Xmax & points(:,2) >= Ymin & points(:,2) <= Ymax ),:);
            
            grid = app.DensityMapResolutionField.Value;   %refinement of map
            
            minvals = [Xmin Ymin];
            maxvals = [Xmax Ymax];
            
            rangevals = maxvals - minvals;
            
            xidx = 1 + round((points(:,1) - minvals(1)) ./ rangevals(1) * (grid-1));
            yidx = 1 + round((points(:,2) - minvals(2)) ./ rangevals(2) * (grid-1));
            
            density = accumarray([yidx, xidx], 1, [grid,grid]);  %note y is rows, x is cols
            
            cla(app.FigPlot5,'reset');
            imagesc(app.FigPlot5,log(density),'xdata', [minvals(1), maxvals(1)], 'ydata', [minvals(2), maxvals(2)])
            axis(app.FigPlot5,[Xmin Xmax Ymin Ymax]) 
            
            Cmin = min(log(density(find(density))))+1;
            Cmax = max(log(density(:)))+1;
            
            app.FigPlot5.YDir = 'normal';
            colormap(app.FigPlot5,Turbo(app,64))
            title(app.FigPlot5,['Density map ',char(app.DropDown_X.Items(app.DropDown_X.Value)),' vs ',char(app.DropDown_Y.Items(app.DropDown_Y.Value))],'interpreter','none')
            xlabel(app.FigPlot5,char(app.DropDown_X.Items(app.DropDown_X.Value)),'interpreter','none')
            ylabel(app.FigPlot5,char(app.DropDown_Y.Items(app.DropDown_Y.Value)),'interpreter','none')
            
            app.FigPlot5.FontSize = 9;
            
            tb = axtoolbar(app.FigPlot5,{'export'});
            
            tk = logspace(log(Cmin),log(Cmax),5);
            
            %originalSize1 = get(gca, 'Position');
            hc = colorbar(app.FigPlot5,'vertical');
            colormap(app.FigPlot5,[Turbo(app,64)])
            %set(gca,'Position',originalSize1);
            
            Labels = get(hc,'YTickLabel');
            
            for i = 1:length(Labels)
                LabelsOk{i} = num2str(round(exp(str2num(char(Labels(i,:))))),'%.0f');
            end
            
            %LabelsOk;
            
            set(hc,'YTickLabel',LabelsOk);
            
            hold(app.FigPlot5,'on')
            plot(app.FigPlot5,[0,0.5,1,0],[0,1,0,0],'-','color',[1,1,1],'linewidth',2)
            hold(app.FigPlot5,'off')
            
            app.FigPlot5.Title.String = 'log(Density) Map (Ternary Plot)';
            app.FigPlot5.XTickMode = 'Manual';
            app.FigPlot5.YTickMode = 'Manual';
            app.FigPlot5.XTick = [];
            app.FigPlot5.YTick = [];
            
            xlabel(app.FigPlot5,'');
            ylabel(app.FigPlot5,'');
            
            drawnow
            
            return
            
            MapSize = 120;
            %tic
            Lims = axis(app.FigPlot1);
            E1 = app.LastPlot.X;
            E2 = app.LastPlot.Y;
            
            Xmin = Lims(1);
            Xmax = Lims(2);
            Ymin = Lims(3);
            Ymax = Lims(4);
            
            Xstep = (Xmax-Xmin)/(MapSize-1);
            Ystep = (Ymax-Ymin)/(MapSize-1);
            
            Xi = Xmin:Xstep:Xmax;
            Yi = Ymin:Ystep:Ymax;
            
            X = E1(find(E1 > 0 & E2 > 0));
            Y = E2(find(E1 > 0 & E2 > 0));
            
            minx = Xmin;
            maxx = Xmax;
            miny = Ymin;
            maxy = Ymax;
            
            nbins = [min(numel(unique(X)),MapSize) ,min(numel(unique(Y)),MapSize) ];
            
            edges1 = linspace(minx, maxx, nbins(1)+1);
            ctrs1 = edges1(1:end-1) + .5*diff(edges1);
            edges1 = [-Inf edges1(2:end-1) Inf];
            edges2 = linspace(miny, maxy, nbins(2)+1);
            ctrs2 = edges2(1:end-1) + .5*diff(edges2);
            edges2 = [-Inf edges2(2:end-1) Inf];
            
            [n,p] = size(X);
            bin = zeros(n,2);
            % Reverse the columns to put the first column of X along the horizontal
            % axis, the second along the vertical.
            [dum,bin(:,2)] = histc(X,edges1);
            [dum,bin(:,1)] = histc(Y,edges2);
            
            ImageDistribution2D = accumarray(bin,1,nbins([2 1]));
            
            cla(app.FigPlot5,'reset');
            imagesc(app.FigPlot5,Xi,Yi,log(ImageDistribution2D))
            axis(app.FigPlot5,[Xmin Xmax Ymin Ymax]) 
            
            Cmin = min(log(ImageDistribution2D(find(ImageDistribution2D))))+1;
            Cmax = max(log(ImageDistribution2D(:)))+1;
            
            app.FigPlot5.YDir = 'normal';
            colormap(app.FigPlot5,Turbo(app,64))
            title(app.FigPlot5,['Density map ',char(app.DropDown_X.Items(app.DropDown_X.Value)),' vs ',char(app.DropDown_Y.Items(app.DropDown_Y.Value))],'interpreter','none')
            xlabel(app.FigPlot5,char(app.DropDown_X.Items(app.DropDown_X.Value)),'interpreter','none')
            ylabel(app.FigPlot5,char(app.DropDown_Y.Items(app.DropDown_Y.Value)),'interpreter','none')
            
            tb = axtoolbar(app.FigPlot5,{'export'});
            
            tk = logspace(log(Cmin),log(Cmax),5);
            
            %originalSize1 = get(gca, 'Position');
            hc = colorbar(app.FigPlot5,'vertical');
            colormap(app.FigPlot5,[Turbo(app,64)])
            %set(gca,'Position',originalSize1);
            
            Labels = get(hc,'YTickLabel');
            
            for i = 1:length(Labels)
                LabelsOk{i} = num2str(round(exp(str2num(char(Labels(i,:))))),'%.0f');
            end
            
            %LabelsOk;
            
            set(hc,'YTickLabel',LabelsOk);
            
            hold(app.FigPlot5,'on')
            plot(app.FigPlot5,[0,0.5,1,0],[0,1,0,0],'-','color',[1,1,1],'linewidth',2)
            hold(app.FigPlot5,'off')
            
            app.FigPlot5.Title.String = 'Density Map (Ternary Plot)';
            app.FigPlot5.XTickMode = 'Manual';
            app.FigPlot5.YTickMode = 'Manual';
            app.FigPlot5.XTick = [];
            app.FigPlot5.YTick = [];
            
            xlabel(app.FigPlot5,'');
            ylabel(app.FigPlot5,'');
            
            drawnow
            %toc
            
        end
        
        function plotTriangle4RGB(app,a,b,c)
            Nb=100;
            Step = (1-0)/(Nb);
            
            RiS = ones(Nb+1,Nb+1);
            BiS = ones(Nb+1,Nb+1);
            GiS = ones(Nb+1,Nb+1);
            
            for i = 1:Nb+1
                RiS(i,1:i) = (1-(i-1)*Step).*ones(size(RiS(i,1:i)));
                BiS(i:end,i) = (0+(i-1)*Step)*ones(size(BiS(i:end,i)));
                GiS(i:end,i) = [0:Step:(Nb+1-i)*Step]';
            end
            
            Fac = 2;
            
            Ri = ones(Fac*(Nb+1),Fac*(Nb+1));
            Bi = ones(Fac*(Nb+1),Fac*(Nb+1));
            Gi = ones(Fac*(Nb+1),Fac*(Nb+1));
            
            Where = round(Nb/Fac+1);
            
            Ri(Where:Where+Nb,Where:Where+Nb) = RiS;
            Bi(Where:Where+Nb,Where:Where+Nb) = BiS;
            Gi(Where:Where+Nb,Where:Where+Nb) = GiS;
            
            RgbImage = cat(3, Ri, Gi, Bi);
            
            cla(app.FigPlot5,'reset');
            image(app.FigPlot5,RgbImage); 
            hold(app.FigPlot5,'on')
            %imshow(RgbImage,'Border','loose'); hold on
            
            plot(app.FigPlot5,[(Nb+2)/Fac,(Nb+2)/Fac],[(Nb+2)/Fac,Nb+(Nb+2)/Fac],'k','LineWidth',1)
            plot(app.FigPlot5,[(Nb+2)/Fac,Nb+(Nb+2)/Fac],[(Nb+2)/Fac,Nb+(Nb+2)/Fac],'k','LineWidth',1)
            plot(app.FigPlot5,[(Nb+2)/Fac,Nb+(Nb+2)/Fac],[Nb+(Nb+2)/Fac,Nb+(Nb+2)/Fac],'k','LineWidth',1)
            
            plot(app.FigPlot5,(Nb+2)/Fac,(Nb+2)/Fac,'o','MarkerEdgeColor','k','MarkerFaceColor','r','MarkerSize',4);
            plot(app.FigPlot5,(Nb+2)/Fac,Nb+(Nb+2)/Fac,'o','MarkerEdgeColor','k','MarkerFaceColor','g','MarkerSize',4);
            plot(app.FigPlot5,Nb+(Nb+2)/Fac,Nb+(Nb+2)/Fac,'o','MarkerEdgeColor','k','MarkerFaceColor','b','MarkerSize',4);
            
            plot(app.FigPlot5,[1,1],[1,Fac*(Nb+1)],'k','LineWidth',1)
            plot(app.FigPlot5,[1,Fac*(Nb+1)],[1,1],'k','LineWidth',1)
            plot(app.FigPlot5,[Fac*(Nb+1),Fac*(Nb+1)],[0,Fac*(Nb+1)],'k','LineWidth',1)
            plot(app.FigPlot5,[1,Fac*(Nb+1)],[Fac*(Nb+1),Fac*(Nb+1)],'k','LineWidth',1)
            
            text(app.FigPlot5,Nb/Fac,Nb/Fac-0.15*Nb,a,'HorizontalAlignment','Center','interpreter','none');
            text(app.FigPlot5,Nb/Fac,Nb+Nb/Fac+0.15*Nb,b,'HorizontalAlignment','Center','interpreter','none');
            text(app.FigPlot5,Nb+Nb/Fac,Nb+Nb/Fac+0.15*Nb,c,'HorizontalAlignment','Center','interpreter','none');
            
            disableDefaultInteractivity(app.FigPlot5);
            tb = axtoolbar(app.FigPlot5,{'export'});
            
            app.FigPlot5.XTickMode = 'Manual';
            app.FigPlot5.YTickMode = 'Manual';
            app.FigPlot5.XTick = [];
            app.FigPlot5.YTick = [];
            
            app.FigPlot1.XTickMode = 'Manual';
            app.FigPlot1.YTickMode = 'Manual';
            app.FigPlot1.XTick = [];
            app.FigPlot1.YTick = [];

        end
        
        
        
        function PixelIndentifier(app,Mode) 
            
            switch Mode
                case 'Single'
                    
                    %x = app.Data(app.DropDown_X.Value).Map(:);
                    %y = app.Data(app.DropDown_Y.Value).Map(:);
                    
                    x = app.LastPlot.SelData_X;
                    y = app.LastPlot.SelData_Y;
                    
                    MapSize = size(app.Data(app.DropDown_X.Value).Map);
                    
                    P =  app.ROI_PI_PolygonSingle.Position;
                    
                    % Extract the pixels in the polygon
                    IN = inpolygon(x,y,P(:,1),P(:,2));
                    Indx = find(IN);
                    
                    % Check if all the pixels have been defined (or not)
                    NonNull = find(x>0 & y>0);
                    Null = find(x==0 & y==0);
                    
                    if isequal(length(NonNull),length(x))
                        OuiMask = 0;
                    else
                        OuiMask = 1;
                    end
                    
                    LeMask = zeros(MapSize);
                    LeMask(NonNull) = 1.5*ones(size(NonNull));
                    LeMask(Indx) = 2.5*ones(size(Indx));
                    
                    SelectedPx = zeros(MapSize);
                    SelectedPx(Indx) = 1*ones(size(Indx)); % for saving the maskfile
                    
                    app.LastSelectedPx = SelectedPx;
                    
                    imagesc(app.FigPlot2,LeMask)
                    axis(app.FigPlot2,'image')
                    
                    app.FigPlot2.XTick = [];
                    app.FigPlot2.YTick = [];
                    app.FigPlot2.XLabel.String = '';
                    app.FigPlot2.YLabel.String = '';
                    app.FigPlot2.Title.String = '';
                    app.FigPlot2.Visible = 'On';
                    
                    if isequal(OuiMask,1)
                        colormap(app.FigPlot2,[0,0,0;1,1,1;app.XMapToolsApp.GetROIColor])
                        
                        if size(LeMask,2) > size(LeMask,1)
                            colorbar(app.FigPlot2,'XTickLabel',{'Undefined','Unselected (>0)','Selected'},'Location','SouthOutside','XTickMode','manual','XTick',[0.5,1.5,2.5]); 
                        else
                            colorbar(app.FigPlot2,'XTickLabel',{'Undefined','Unselected (>0)','Selected'},'XTickMode','manual','XTick',[0.5,1.5,2.5])
 
                        end
                        
                        caxis(app.FigPlot2,[0 3]);
                        %handles.colorbar = hcb;
                        %set(hcb,'FontName','Times New Roman');
                    else
                        colormap(app.FigPlot2,[0,0,0;0.2,0.3,1;1,0,0])
                          
                        if size(LeMask,2) > size(LeMask,1)
                            colorbar(app.FigPlot2,'XTickLabel',{'Undefined','Unselected (>0)','Selected'},'Location','SouthOutside','XTickMode','manual','XTick',[1.5,2.5]);
                        else
                            colorbar(app.FigPlot2,'XTickLabel',{'Undefined','Unselected (>0)','Selected'},'XTickMode','manual','XTick',[1.5,2.5])
                        end
                        caxis(app.FigPlot2,[0 3]);
                        
                    end
                    
                    Table2Display = {};
                    Table2Display(:,1) = {'Undefined','Unselected','ROI-1'};
                    Prop = [length(find(LeMask == 0.5)), length(find(LeMask == 1.5)), length(find(LeMask == 2.5))];
                    PropPer = Prop./(sum(Prop)) *100;
                    
                    % sum(Prop)
                    
                    Table2Display(:,2) = {Prop(1), Prop(2), Prop(3)};
                    Table2Display(:,3) = {PropPer(1), PropPer(2), PropPer(3)};
                    
                    app.UITableRoiResults.Data = Table2Display;
                    app.UITableRoiResults.Visible = 'on';
                    
                case 'Multi'
                    
                    if app.ROI_PI_Multi.Nb > 1
                        ColorMap = app.XMapToolsApp.CalculateColorMap(2,app.ROI_PI_Multi.Nb);
                    else
                        ColorMap = [1,0,0];
                    end
                    
                    x = app.LastPlot.SelData_X;
                    y = app.LastPlot.SelData_Y;
                    
                    MapSize = size(app.Data(app.DropDown_X.Value).Map);
                    
                    % Check if all the pixels have been defined (or not)
                    NonNull = find(x>0 & y>0);
                    Null = find(x==0 & y==0);
                    
                    if isequal(length(NonNull),length(x))
                        OuiMask = 0;
                    else
                        OuiMask = 1;
                    end
                    
                    LeMask = zeros(MapSize);
                    LeMask(NonNull) = ones(size(NonNull));
                    
                    SelectedPx = zeros(MapSize);
                    
                    Table2Display = {};
                    Table2Display(:,1) = {'Undefined','Unselected'};
                    
                    Prop = [length(find(LeMask == 0)), length(find(LeMask == 1))];
                                
                    for i = 1:app.ROI_PI_Multi.Nb
                        
                        P =  app.ROI_PI_Multi.ROI(i).ROI.Position;
                    
                        % Extract the pixels in the polygon
                        IN = inpolygon(x,y,P(:,1),P(:,2));
                        Indx = find(IN);
                        
                        LeMask(Indx) = (i+1)*ones(size(Indx));
                        
                        SelectedPx(Indx) = (i+1)*ones(size(Indx)); % for saving the maskfile
                        
                        
                        app.ROI_PI_Multi.ROI(i).ROI.Color = ColorMap(i,:);
                        
                        Table2Display{i+2,1} = char(['ROI-',num2str(i)]);
                        Prop(i+2) = length(Indx);
                        Prop(2) = Prop(2) - Prop(i+2);
                    end
                    
                    % sum(Prop)
                    
                    PropPer = Prop./(sum(Prop)) *100;
                    
                    for j = 1:length(Prop)
                        Table2Display{j,2} = Prop(j);
                        Table2Display{j,3} = PropPer(j);
                    end
                    
                    app.LastSelectedPx = SelectedPx;
                    
                    imagesc(app.FigPlot2,LeMask)
                    axis(app.FigPlot2,'image')
                    
                    app.FigPlot2.XTick = [];
                    app.FigPlot2.YTick = [];
                    app.FigPlot2.XLabel.String = '';
                    app.FigPlot2.YLabel.String = '';
                    app.FigPlot2.Title.String = '';
                    app.FigPlot2.Visible = 'On';
                    
                    colormap(app.FigPlot2,[1,1,1;0,0,0;ColorMap])
                    colorbar(app.FigPlot2)
                    caxis(app.FigPlot2,[-0.5,i+1.5])
                    
                    app.UITableRoiResults.Data = Table2Display;
                    app.UITableRoiResults.Visible = 'on';
            end
            
            
        end
        
        function ROI_ShapeChangedMulti(app,~)
            
            PixelIndentifier(app,'Multi');
        end
        
        
        function ROI_ShapeChangedSingle(app,~)
            
            app.ROI_PI_PolygonSingle_Dens.Position = app.ROI_PI_PolygonSingle.Position;
            PixelIndentifier(app,'Single');
        end
        function ROI_ShapeChangedSingleDens(app,~)
            
            app.ROI_PI_PolygonSingle.Position = app.ROI_PI_PolygonSingle_Dens.Position;
            PixelIndentifier(app,'Single');
        end
        
        function ROI_ShapeChangedSingleTernary(app,~)
            
            app.ROI_PI_PolygonSingleTernary_Dens.Position = app.ROI_PI_PolygonSingleTernary.Position;
            PixelIndentifier(app,'Single-Ternary');
        end
        function ROI_ShapeChangedSingleTernaryDens(app,~)
            
            app.ROI_PI_PolygonSingleTernary.Position = app.ROI_PI_PolygonSingleTernary_Dens.Position;
            PixelIndentifier(app,'Single-Ternary');
        end 
        
        
        function ColorMap = InterpColor(app,ResColorMap)
            
            ColorData = [0.647	0.000	0.149; ...
                0.843	0.188	0.153; ...
                0.957	0.427	0.263; ...
                0.992	0.682	0.380; ...
                0.996	0.878	0.565; ...
                1.000	1.000	0.749; ...
                0.878	0.953	0.973; ...
                0.671	0.851	0.914; ...
                0.455	0.678	0.820; ...
                0.271	0.459	0.706; ...
                0.192	0.212	0.584];
            
            ColorData = flipud(ColorData);
            
            Xi = 1:ResColorMap;
            Step = (ResColorMap-1)/(size(ColorData,1)-1);
            X = 1:Step:ResColorMap;
            
            ColorMap = zeros(length(Xi),size(ColorData,2));
            for i = 1:size(ColorData,2)
                ColorMap(:,i) = interp1(X',ColorData(:,i),Xi);
                % = polyval(P,);
            end
            
            
        end
        
        
        
        function SelData = ExtractDataFromCode(app,Value)
            
            for i = 1:length(app.Names)
                N = matlab.lang.makeValidName(app.Names{i});
                eval([N,' = app.Data(i).Map;']);
            end
            
            try
                eval(['SelData = ',Value,';']);
            catch ME
                errordlg(ME.message,'Oups wrong code!')
                SelData = -1;
            end
            
        end
        
        function map = Turbo(app,Value)
            
            
            values = [
                0.18995 0.07176 0.23217
                0.19483 0.08339 0.26149
                0.19956 0.09498 0.29024
                0.20415 0.10652 0.31844
                0.20860 0.11802 0.34607
                0.21291 0.12947 0.37314
                0.21708 0.14087 0.39964
                0.22111 0.15223 0.42558
                0.22500 0.16354 0.45096
                0.22875 0.17481 0.47578
                0.23236 0.18603 0.50004
                0.23582 0.19720 0.52373
                0.23915 0.20833 0.54686
                0.24234 0.21941 0.56942
                0.24539 0.23044 0.59142
                0.24830 0.24143 0.61286
                0.25107 0.25237 0.63374
                0.25369 0.26327 0.65406
                0.25618 0.27412 0.67381
                0.25853 0.28492 0.69300
                0.26074 0.29568 0.71162
                0.26280 0.30639 0.72968
                0.26473 0.31706 0.74718
                0.26652 0.32768 0.76412
                0.26816 0.33825 0.78050
                0.26967 0.34878 0.79631
                0.27103 0.35926 0.81156
                0.27226 0.36970 0.82624
                0.27334 0.38008 0.84037
                0.27429 0.39043 0.85393
                0.27509 0.40072 0.86692
                0.27576 0.41097 0.87936
                0.27628 0.42118 0.89123
                0.27667 0.43134 0.90254
                0.27691 0.44145 0.91328
                0.27701 0.45152 0.92347
                0.27698 0.46153 0.93309
                0.27680 0.47151 0.94214
                0.27648 0.48144 0.95064
                0.27603 0.49132 0.95857
                0.27543 0.50115 0.96594
                0.27469 0.51094 0.97275
                0.27381 0.52069 0.97899
                0.27273 0.53040 0.98461
                0.27106 0.54015 0.98930
                0.26878 0.54995 0.99303
                0.26592 0.55979 0.99583
                0.26252 0.56967 0.99773
                0.25862 0.57958 0.99876
                0.25425 0.58950 0.99896
                0.24946 0.59943 0.99835
                0.24427 0.60937 0.99697
                0.23874 0.61931 0.99485
                0.23288 0.62923 0.99202
                0.22676 0.63913 0.98851
                0.22039 0.64901 0.98436
                0.21382 0.65886 0.97959
                0.20708 0.66866 0.97423
                0.20021 0.67842 0.96833
                0.19326 0.68812 0.96190
                0.18625 0.69775 0.95498
                0.17923 0.70732 0.94761
                0.17223 0.71680 0.93981
                0.16529 0.72620 0.93161
                0.15844 0.73551 0.92305
                0.15173 0.74472 0.91416
                0.14519 0.75381 0.90496
                0.13886 0.76279 0.89550
                0.13278 0.77165 0.88580
                0.12698 0.78037 0.87590
                0.12151 0.78896 0.86581
                0.11639 0.79740 0.85559
                0.11167 0.80569 0.84525
                0.10738 0.81381 0.83484
                0.10357 0.82177 0.82437
                0.10026 0.82955 0.81389
                0.09750 0.83714 0.80342
                0.09532 0.84455 0.79299
                0.09377 0.85175 0.78264
                0.09287 0.85875 0.77240
                0.09267 0.86554 0.76230
                0.09320 0.87211 0.75237
                0.09451 0.87844 0.74265
                0.09662 0.88454 0.73316
                0.09958 0.89040 0.72393
                0.10342 0.89600 0.71500
                0.10815 0.90142 0.70599
                0.11374 0.90673 0.69651
                0.12014 0.91193 0.68660
                0.12733 0.91701 0.67627
                0.13526 0.92197 0.66556
                0.14391 0.92680 0.65448
                0.15323 0.93151 0.64308
                0.16319 0.93609 0.63137
                0.17377 0.94053 0.61938
                0.18491 0.94484 0.60713
                0.19659 0.94901 0.59466
                0.20877 0.95304 0.58199
                0.22142 0.95692 0.56914
                0.23449 0.96065 0.55614
                0.24797 0.96423 0.54303
                0.26180 0.96765 0.52981
                0.27597 0.97092 0.51653
                0.29042 0.97403 0.50321
                0.30513 0.97697 0.48987
                0.32006 0.97974 0.47654
                0.33517 0.98234 0.46325
                0.35043 0.98477 0.45002
                0.36581 0.98702 0.43688
                0.38127 0.98909 0.42386
                0.39678 0.99098 0.41098
                0.41229 0.99268 0.39826
                0.42778 0.99419 0.38575
                0.44321 0.99551 0.37345
                0.45854 0.99663 0.36140
                0.47375 0.99755 0.34963
                0.48879 0.99828 0.33816
                0.50362 0.99879 0.32701
                0.51822 0.99910 0.31622
                0.53255 0.99919 0.30581
                0.54658 0.99907 0.29581
                0.56026 0.99873 0.28623
                0.57357 0.99817 0.27712
                0.58646 0.99739 0.26849
                0.59891 0.99638 0.26038
                0.61088 0.99514 0.25280
                0.62233 0.99366 0.24579
                0.63323 0.99195 0.23937
                0.64362 0.98999 0.23356
                0.65394 0.98775 0.22835
                0.66428 0.98524 0.22370
                0.67462 0.98246 0.21960
                0.68494 0.97941 0.21602
                0.69525 0.97610 0.21294
                0.70553 0.97255 0.21032
                0.71577 0.96875 0.20815
                0.72596 0.96470 0.20640
                0.73610 0.96043 0.20504
                0.74617 0.95593 0.20406
                0.75617 0.95121 0.20343
                0.76608 0.94627 0.20311
                0.77591 0.94113 0.20310
                0.78563 0.93579 0.20336
                0.79524 0.93025 0.20386
                0.80473 0.92452 0.20459
                0.81410 0.91861 0.20552
                0.82333 0.91253 0.20663
                0.83241 0.90627 0.20788
                0.84133 0.89986 0.20926
                0.85010 0.89328 0.21074
                0.85868 0.88655 0.21230
                0.86709 0.87968 0.21391
                0.87530 0.87267 0.21555
                0.88331 0.86553 0.21719
                0.89112 0.85826 0.21880
                0.89870 0.85087 0.22038
                0.90605 0.84337 0.22188
                0.91317 0.83576 0.22328
                0.92004 0.82806 0.22456
                0.92666 0.82025 0.22570
                0.93301 0.81236 0.22667
                0.93909 0.80439 0.22744
                0.94489 0.79634 0.22800
                0.95039 0.78823 0.22831
                0.95560 0.78005 0.22836
                0.96049 0.77181 0.22811
                0.96507 0.76352 0.22754
                0.96931 0.75519 0.22663
                0.97323 0.74682 0.22536
                0.97679 0.73842 0.22369
                0.98000 0.73000 0.22161
                0.98289 0.72140 0.21918
                0.98549 0.71250 0.21650
                0.98781 0.70330 0.21358
                0.98986 0.69382 0.21043
                0.99163 0.68408 0.20706
                0.99314 0.67408 0.20348
                0.99438 0.66386 0.19971
                0.99535 0.65341 0.19577
                0.99607 0.64277 0.19165
                0.99654 0.63193 0.18738
                0.99675 0.62093 0.18297
                0.99672 0.60977 0.17842
                0.99644 0.59846 0.17376
                0.99593 0.58703 0.16899
                0.99517 0.57549 0.16412
                0.99419 0.56386 0.15918
                0.99297 0.55214 0.15417
                0.99153 0.54036 0.14910
                0.98987 0.52854 0.14398
                0.98799 0.51667 0.13883
                0.98590 0.50479 0.13367
                0.98360 0.49291 0.12849
                0.98108 0.48104 0.12332
                0.97837 0.46920 0.11817
                0.97545 0.45740 0.11305
                0.97234 0.44565 0.10797
                0.96904 0.43399 0.10294
                0.96555 0.42241 0.09798
                0.96187 0.41093 0.09310
                0.95801 0.39958 0.08831
                0.95398 0.38836 0.08362
                0.94977 0.37729 0.07905
                0.94538 0.36638 0.07461
                0.94084 0.35566 0.07031
                0.93612 0.34513 0.06616
                0.93125 0.33482 0.06218
                0.92623 0.32473 0.05837
                0.92105 0.31489 0.05475
                0.91572 0.30530 0.05134
                0.91024 0.29599 0.04814
                0.90463 0.28696 0.04516
                0.89888 0.27824 0.04243
                0.89298 0.26981 0.03993
                0.88691 0.26152 0.03753
                0.88066 0.25334 0.03521
                0.87422 0.24526 0.03297
                0.86760 0.23730 0.03082
                0.86079 0.22945 0.02875
                0.85380 0.22170 0.02677
                0.84662 0.21407 0.02487
                0.83926 0.20654 0.02305
                0.83172 0.19912 0.02131
                0.82399 0.19182 0.01966
                0.81608 0.18462 0.01809
                0.80799 0.17753 0.01660
                0.79971 0.17055 0.01520
                0.79125 0.16368 0.01387
                0.78260 0.15693 0.01264
                0.77377 0.15028 0.01148
                0.76476 0.14374 0.01041
                0.75556 0.13731 0.00942
                0.74617 0.13098 0.00851
                0.73661 0.12477 0.00769
                0.72686 0.11867 0.00695
                0.71692 0.11268 0.00629
                0.70680 0.10680 0.00571
                0.69650 0.10102 0.00522
                0.68602 0.09536 0.00481
                0.67535 0.08980 0.00449
                0.66449 0.08436 0.00424
                0.65345 0.07902 0.00408
                0.64223 0.07380 0.00401
                0.63082 0.06868 0.00401
                0.61923 0.06367 0.00410
                0.60746 0.05878 0.00427
                0.59550 0.05399 0.00453
                0.58336 0.04931 0.00486
                0.57103 0.04474 0.00529
                0.55852 0.04028 0.00579
                0.54583 0.03593 0.00638
                0.53295 0.03169 0.00705
                0.51989 0.02756 0.00780
                0.50664 0.02354 0.00863
                0.49321 0.01963 0.00955
                0.47960 0.01583 0.01055
                ];
            
            P = size(values,1);
            map = interp1(1:size(values,1), values, linspace(1,P,Value), 'linear');            
        end
        
        
        function ROI_DeleteROI(app)
            % This function deletes all the ROI of the figures
            
            app.MakeMaskFile_Button.Visible = 'off';
            
            delete(findall(app.FigPlot1, 'Type',  'images.roi.Rectangle'));
            delete(findall(app.FigPlot1, 'Type',  'images.roi.Polygon'));
            delete(findall(app.FigPlot1, 'Type',  'images.roi.Ellipse'));
            delete(findall(app.FigPlot1, 'Type',  'images.roi.Circle'));
            delete(findall(app.FigPlot1, 'Type',  'images.roi.Point'));
            delete(findall(app.FigPlot1, 'Type',  'images.roi.Polyline'));
            
            delete(findall(app.FigPlot5, 'Type',  'images.roi.Rectangle'));
            delete(findall(app.FigPlot5, 'Type',  'images.roi.Polygon'));
            delete(findall(app.FigPlot5, 'Type',  'images.roi.Ellipse'));
            delete(findall(app.FigPlot5, 'Type',  'images.roi.Circle'));
            delete(findall(app.FigPlot5, 'Type',  'images.roi.Point'));
            delete(findall(app.FigPlot5, 'Type',  'images.roi.Polyline'));
        end
        
        
        function CalculateBRC(app)
            
            TheNbPx = 3;
            TheNbPxOnGarde = 80;
            
            MaskMap = app.MaskFile.Masks(app.DropDown_Masks.Value).MaskMap;
            MaskNames = app.MaskFile.Masks(app.DropDown_Masks.Value).Names;
            
            % Proceed to the correction
            TheLin = size(MaskMap,1);
            TheCol = size(MaskMap,2);
            %CoordMatrice = reshape([1:TheLin*TheCol],TheLin,TheCol);
            
            TheMaskFinal = zeros(size(MaskMap));
            
            Position = round(TheNbPx/2);
            TheNbPxInSel = TheNbPx^2;
            TheCriterion = TheNbPxInSel*TheNbPxOnGarde/100;
            
            for i=1:length(MaskNames) - 1                % for each phase
                
                TheMask = zeros(size(MaskMap));
                VectorOk = find(MaskMap == i);
                
                TheMask(VectorOk) = ones(size(VectorOk));
                
                TheWorkingMat = zeros(size(TheMask,1)*size(TheMask,2),TheNbPxInSel+1);
                
                VectMask = TheMask(:);
                TheWorkingMat(find(VectMask)) = 1000*ones(size(find(VectMask)));
                
                Compt = 1;
                for iLin = 1:TheNbPx
                    
                    
                    for iCol = 1:TheNbPx
                        
                        % SCAN
                        TheTempMat = zeros(size(TheMask));
                        TheTempMat(Position:end-(Position-1),Position:end-(Position-1)) = TheMask(iLin:end-(TheNbPx-iLin),iCol:end-(TheNbPx-iCol));
                        Compt = Compt+1;
                        TheWorkingMat(:,Compt) = TheTempMat(:);
                        
                    end
                end
                
                TheSum = sum(TheWorkingMat,2);
                OnVire1 = find(TheSum < 1000+TheCriterion & TheSum > 1000);
                TheMaskFinal(OnVire1) = ones(size(OnVire1));
                
            end
            
            app.BRC = ones(size(TheMaskFinal));
            app.BRC(find(TheMaskFinal(:) == 1)) = zeros(length(find(TheMaskFinal(:) == 1)),1);
            
        end
    end
    

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, XMapToolsApp, Names, Data, Selected, MaskFile)
            
            % XMapTools is a free software solution for the analysis of chemical maps
            % Copyright © 2022-2026 University of Lausanne, Institute of Earth Sciences, Pierre Lanari
            
            % XMapTools is free software: you can redistribute it and/or modify
            % it under the terms of the GNU General Public License as published by
            % the Free Software Foundation, either version 3 of the License, or any
            % later version.
            
            % XMapTools is distributed in the hope that it will be useful,
            % but WITHOUT ANY WARRANTY; without even the implied warranty of
            % MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
            % GNU General Public License for more details.
            
            % You should have received a copy of the GNU General Public License
            % along with XMapTools. If not, see https://www.gnu.org/licenses.
            
            app.VisualizationTool.Visible = 'off';
            
            diary('log_dvt.txt');
            
            if isequal(Selected,0)
                Selected = 1;
            end
            
            app.XMapToolsApp = XMapToolsApp;
            app.Names = Names;
            app.Data = Data;
            app.MaskFile = MaskFile;
            
            % Update GUI
            app.DropDown_X.Items = Names;
            app.DropDown_X.ItemsData = [1:length(Data)];
            app.CheckBox_X_Menu.Value = 1;
            app.CheckBox_X_Manual.Value = 0;
            
            app.DropDown_X.Value =  Selected;
            
            % Update the check boxes
            app.CheckBox_X_Menu.Enable = 'off';
            app.CheckBox_X_Menu.Value = 1;
            app.CheckBox_Y_Menu.Enable = 'off';
            app.CheckBox_Y_Menu.Value = 1;
            app.CheckBox_Z_Menu.Enable = 'off';
            app.CheckBox_Z_Menu.Value = 1;
            
            app.EditField_X.Enable = 'off';
            app.EditField_Y.Enable = 'off';
            app.EditField_Z.Enable = 'off';
            
            if length(Data) > 1
                app.DropDown_Y.Items = Names;
                app.DropDown_Y.ItemsData = [1:length(Data)];
                app.CheckBox_Y_Menu.Value = 1;
                app.CheckBox_Y_Manual.Value = 0;
                
                app.Button_Binary.Value = 1;
                
                if length(Data) >= Selected+1
                    app.DropDown_Y.Value =  Selected+1;
                else
                    app.DropDown_Y.Value = 2;
                end
            else
                app.EditField_Y.Visible = 'off';
                app.CheckBox_Y_Manual.Visible = 'off';
                app.DropDown_Y.Visible = 'off';
                app.CheckBox_Y_Menu.Visible = 'off';
                
                app.Button_Binary.Value = 1;
                
                % Need to start in Histogram Mode
                app.Button_Binary.Value = 0;
                app.Button_Histogram.Value = 1;
                
                app.Button_Binary.Visible = 'off';
                app.Button_Ternary.Visible = 'off';
                app.Button_RGB.Visible = 'off';
            end
            
            if isequal(length(Data),2)
                app.Button_Ternary.Visible = 'off';
                app.Button_RGB.Visible = 'off';
            end
            
            
            app.CheckBox_Masks.Value = 0;
            
            if ~isempty(app.MaskFile.Names)
                app.DropDown_Masks.Items = app.MaskFile.Names;
                app.DropDown_Masks.ItemsData = [1:length(app.MaskFile.Names)];
                app.DropDown_Masks.Value = 1;
                
                app.DropDown_Masks_2.Items = app.MaskFile.Masks(app.DropDown_Masks.Value).Names;
                app.DropDown_Masks_2.ItemsData = [0:length(app.MaskFile.Masks(app.DropDown_Masks.Value).Names)-1];
                app.DropDown_Masks_2.Value = 0;
                
                CalculateBRC(app);
                
            else
                app.DropDown_Masks.Visible = 'off';
                app.DropDown_Masks_2.Visible = 'off';
                app.DropDown_Masks_2.Items = {''};
                app.DropDown_Masks_2.ItemsData = 0;
                app.DropDown_Masks_2.Value = 0;
                
                app.CheckBox_BCR_Filter.Visible = 'off';
                app.Label_Maks_4.Visible = 'off';
            end
            
            app.DropDown_Masks_3.Visible = 'off';
            
            if length(Data) > 2
                app.DropDown_Z.Items = Names;
                app.DropDown_Z.ItemsData = [1:length(Data)];
                app.CheckBox_Z_Menu.Value = 1;
                app.CheckBox_Z_Manual.Value = 0;
                
                if length(Data) >= Selected+2
                    app.DropDown_Z.Value =  Selected+2;
                else
                    app.DropDown_Z.Value = 3;
                end
                
            else
                app.EditField_Z.Visible = 'off';
                app.CheckBox_Z_Manual.Visible = 'off';
                app.DropDown_Z.Visible = 'off';
                app.CheckBox_Z_Menu.Visible = 'off';
            end
            
            app.UITableRoiResults.Visible = 'off';
            
            app.ROI_PI_Multi.Nb = 0;
            app.ROI_PI_Multi.ROI = [];
            
            app.MakeMaskFile_Button.Visible = 'off';
            app.MaskUsedInPlot = 0;
            
            app.FirstPlot = 1;
            
            movegui(app.VisualizationTool,"center");
            
            app.VisualizationTool.Visible = 'on'; % Can be improved if waitbar deactivated while Visible is off
            
            CheckState(app);
        end

        % Value changed function: Button_Ternary
        function Button_TernaryValueChanged(app, event)
            % Check if the button was not selected before
            if isequal(app.Button_RGB.Value,0) && isequal(app.Button_Binary.Value,0) && isequal(app.Button_Histogram.Value,0)
                app.Button_Ternary.Value = 1;
                return
            else
                app.Button_RGB.Value = 0;
                app.Button_Binary.Value = 0;
                app.Button_Histogram.Value = 0;
            end
            
            CheckState(app);
        end

        % Value changed function: Button_RGB
        function Button_RGBValueChanged(app, event)
            % Check if the button was not selected before
            if isequal(app.Button_Ternary.Value,0) && isequal(app.Button_Binary.Value,0) && isequal(app.Button_Histogram.Value,0)
                app.Button_RGB.Value = 1;
                return
            else
                app.Button_Ternary.Value = 0;
                app.Button_Binary.Value = 0;
                app.Button_Histogram.Value = 0;
            end
            
            CheckState(app);
        end

        % Value changed function: Button_Binary
        function Button_BinaryValueChanged(app, event)
            % Check if the button was not selected before
            if isequal(app.Button_Ternary.Value,0) && isequal(app.Button_RGB.Value,0) && isequal(app.Button_Histogram.Value,0)
                app.Button_Binary.Value = 1;
                return
            else
                app.Button_Ternary.Value = 0;
                app.Button_RGB.Value = 0;
                app.Button_Histogram.Value = 0;
            end
            app.FirstPlot = 1;
            CheckState(app);
        end

        % Value changed function: Button_Histogram
        function Button_HistogramValueChanged(app, event)
            
            % Check if the button was not selected before
            if isequal(app.Button_Ternary.Value,0) && isequal(app.Button_RGB.Value,0) && isequal(app.Button_Binary.Value,0)
                app.Button_Histogram.Value = 1;
                return
            else
                app.Button_Ternary.Value = 0;
                app.Button_RGB.Value = 0;
                app.Button_Binary.Value = 0;
            end
            app.AutoPlotLims = 1;
            CheckState(app);
        end

        % Value changed function: DropDown_X
        function DropDown_XValueChanged(app, event)
            app.AutoPlotLims = 1;
            PlotData(app);
        end

        % Value changed function: DropDown_Y
        function DropDown_YValueChanged(app, event)
            app.AutoPlotLims = 1;
            PlotData(app);
        end

        % Value changed function: DropDown_Z
        function DropDown_ZValueChanged(app, event)
            app.AutoPlotLims = 1;
            PlotData(app);
        end

        % Button pushed function: AutoZoom
        function AutoZoomButtonPushed(app, event)
            
            if isequal(app.Button_Binary.Value,1)
                
                PreviousAxisValues = axis(app.FigPlot1);
                XminP = PreviousAxisValues(1);
                XmaxP = PreviousAxisValues(2);
                YminP = PreviousAxisValues(3);
                YmaxP = PreviousAxisValues(4);
                
                X = app.LastPlot.X;
                Y = app.LastPlot.Y;
                
                X = X(X >= XminP & X <= XmaxP);
                Y = Y(Y >= YminP & Y <= YmaxP);
                
                SortedDataX = sort(X(:));
                SortedDataY = sort(Y(:));
                SelCrit = round(numel(SortedDataX) * 0.0001);
                
                MinX1 = SortedDataX(SelCrit);
                MaxX1 = SortedDataX(end-SelCrit);
                
                MinY1 = SortedDataY(SelCrit);
                MaxY1 = SortedDataY(end-SelCrit);
                
                % Current values:
                axis(app.FigPlot1,[MinX1,MaxX1,MinY1,MaxY1])
                
                app.WaitBar = uiprogressdlg(app.VisualizationTool,'Title','Please Wait!','Indeterminate','on');
                app.WaitBar.Message = 'Generating density map';
                plotDensityBin(app)
                close(app.WaitBar)
            end
            
            
        end

        % Button pushed function: ResetZoom
        function ResetZoomButtonPushed(app, event)
            
            if isequal(app.Button_Binary.Value,1)
                X = app.LastPlot.X;
                Y = app.LastPlot.Y;
                
                axis(app.FigPlot1,[min(X(:)),max(X(:)),min(Y(:)),max(Y(:))]);
                
                app.WaitBar = uiprogressdlg(app.VisualizationTool,'Title','Please Wait!','Indeterminate','on');
                app.WaitBar.Message = 'Generating density map';
                plotDensityBin(app)
                close(app.WaitBar)
            end
            
        end

        % Selection change function: TabGroup
        function TabGroupSelectionChanged(app, event)
            selectedTab = app.TabGroup.SelectedTab;
            
            if isequal(selectedTab.Title,'Options')
                if isequal(app.Button_Binary.Value,1)
                    app.TabGroup2.SelectedTab = app.Tab_Options_Binary;
                    
                end
                if isequal(app.Button_Histogram.Value,1)
                    app.TabGroup2.SelectedTab = app.Tab_Options_Hist;
                end
                if isequal(app.Button_Ternary.Value,1)
                    app.TabGroup2.SelectedTab = app.Tab_Options_Ternary;
                end
            end
        end

        % Value changed function: 
        % PlothistogramsusinglogYscaleCheckBox
        function PlothistogramsusinglogYscaleCheckBoxValueChanged(app, event)
            PlotData(app);
        end

        % Value changed function: Field_Xmax, Field_Xmin, 
        % Field_Ymax, Field_Ymin
        function Field_XminXMaxYminYmaxValueChanged(app, event)
            
            if isequal(app.Button_Binary.Value,1)
                
                % Main figure
                axis(app.FigPlot1,[app.Field_Xmin.Value,app.Field_Xmax.Value,app.Field_Ymin.Value,app.Field_Ymax.Value]);
                
                % Histograms
                app.FigPlot3.XLim = [app.Field_Xmin.Value,app.Field_Xmax.Value];
                app.FigPlot4.XLim = [app.Field_Ymin.Value,app.Field_Ymax.Value];
                
                axis(app.FigPlot5,[app.Field_Xmin.Value,app.Field_Xmax.Value,app.Field_Ymin.Value,app.Field_Ymax.Value]);
                
                app.WaitBar = uiprogressdlg(app.VisualizationTool,'Title','Please Wait!','Indeterminate','on');
                app.WaitBar.Message = 'Generating density map';
                plotDensityBin(app)
                close(app.WaitBar)
                
            end
            
            if isequal(app.Button_Histogram.Value,1)
                app.AutoPlotLims = 0;
                PlotData(app);
            end
            
            
            %keyboard
        end

        % Button pushed function: 
        % Explore_IdentifyPixels_PolygonButton
        function Explore_IdentifyPixels_PolygonButtonPushed(app, event)
            
            if isequal(app.Button_Binary.Value,1) || isequal(app.Button_Ternary.Value,1)
                
                if isequal(app.HoldonCheckBox.Value,0)
                    Explore_ResetROI_ButtonPushed(app);
                    
                    app.ROI_PI_PolygonSingle = drawpolygon(app.FigPlot1,'Color',app.XMapToolsApp.GetROIColor, 'InteractionsAllowed','all');
                    app.ROI_PI_PolygonSingle_Listener = addlistener(app.ROI_PI_PolygonSingle, 'ROIMoved', @(varargin)ROI_ShapeChangedSingle(app, app.ROI_PI_PolygonSingle));
                    
                    PixelIndentifier(app,'Single');
                end
                
                
                
                if isequal(app.HoldonCheckBox.Value,1)
                    IdxROI = app.ROI_PI_Multi.Nb + 1;
                    
                    app.ROI_PI_Multi.ROI(IdxROI).ROI = drawpolygon(app.FigPlot1,'Color',app.XMapToolsApp.GetROIColor, 'InteractionsAllowed','all');
                    app.ROI_PI_Multi_Listener = addlistener(app.ROI_PI_Multi.ROI(IdxROI).ROI, 'ROIMoved', @(varargin)ROI_ShapeChangedMulti(app, app.ROI_PI_Multi));
                    
                    app.ROI_PI_Multi.Nb = IdxROI; 
                    
                    PixelIndentifier(app,'Multi');
                end
                
                % if isequal(app.CheckBox_LowRessourcesMode.Value,0)
                %    app.ROI_PI_PolygonSingle_Dens = drawpolygon(app.FigPlot5,'Position',app.ROI_PI_PolygonSingle.Position,'Color',[1,0,0]);
                %    app.ROI_PI_PolygonSingle_Dens_Listener = addlistener(app.ROI_PI_PolygonSingle_Dens, 'ROIMoved', @(varargin)ROI_ShapeChangedSingleDens(app, app.ROI_PI_PolygonSingle_Dens));
                % end
                
                
                
                app.MakeMaskFile_Button.Visible = 'on';
            end
           
        end

        % Value changed function: CheckBox_Masks
        function CheckBox_MasksValueChanged(app, event)
            PlotData(app);
        end

        % Value changed function: DropDown_Masks
        function DropDown_MasksValueChanged(app, event)
            app.DropDown_Masks_2.Items = app.MaskFile.Masks(app.DropDown_Masks.Value).Names;
            app.DropDown_Masks_2.ItemsData = [0:length(app.MaskFile.Masks(app.DropDown_Masks.Value).Names)-1];
            app.DropDown_Masks_2.Value = 0;
            
            app.WaitBar = uiprogressdlg(app.VisualizationTool,'Title','Please Wait!','Indeterminate','on');
            app.WaitBar.Message = 'Calculating BRC';
            CalculateBRC(app);
            close(app.WaitBar)
            
            PlotData(app);
        end

        % Button pushed function: ManualZoom
        function ManualZoomButtonPushed(app, event)
            
            Rect = drawrectangle(app.FigPlot1,'color',[0,0,0]);
            
            Position = Rect.Position;
            Rect.delete;
            
            axis(app.FigPlot1,[Position(1),Position(1)+Position(3),Position(2),Position(2)+Position(4)])
            
            app.WaitBar = uiprogressdlg(app.VisualizationTool,'Title','Please Wait!','Indeterminate','on');
            app.WaitBar.Message = 'Generating density map';
            plotDensityBin(app)
            close(app.WaitBar)
            
            
            
            
        end

        % Value changed function: CheckBox_X_Manual, 
        % CheckBox_Y_Manual, CheckBox_Z_Manual
        function ManualSelectionCheckBox_ValueChanged(app, event)
            Update = 1;
            if isequal(app.CheckBox_X_Manual.Value,1)
                app.CheckBox_X_Menu.Enable = 'on';
                app.CheckBox_X_Menu.Value = 0;
                app.CheckBox_X_Manual.Enable = 'off';
                app.EditField_X.Enable = 'on';
                app.DropDown_X.Enable = 'off';
                
                if isempty(app.EditField_X.Value)
                    app.EditField_X.Value = char(app.DropDown_X.Items(app.DropDown_X.Value));
                    Update = 0;
                end
                
            else
                app.CheckBox_X_Menu.Enable = 'off';
                app.CheckBox_X_Menu.Value = 1;
                app.CheckBox_X_Manual.Enable = 'on';
                app.EditField_X.Enable = 'off';
                app.DropDown_X.Enable = 'on';
            end
            
            if isequal(app.CheckBox_Y_Manual.Value,1)
                app.CheckBox_Y_Menu.Enable = 'on';
                app.CheckBox_Y_Menu.Value = 0;
                app.CheckBox_Y_Manual.Enable = 'off';
                app.EditField_Y.Enable = 'on';
                app.DropDown_Y.Enable = 'off';
                
                if isempty(app.EditField_Y.Value)
                    app.EditField_Y.Value = char(app.DropDown_Y.Items(app.DropDown_Y.Value));
                    Update = 0;
                end
                
            else
                app.CheckBox_Y_Menu.Enable = 'off';
                app.CheckBox_Y_Menu.Value = 1;
                app.CheckBox_Y_Manual.Enable = 'on';
                app.EditField_Y.Enable = 'off';
                app.DropDown_Y.Enable = 'on';
            end
            
            if isequal(app.CheckBox_Z_Manual.Value,1)
                app.CheckBox_Z_Menu.Enable = 'on';
                app.CheckBox_Z_Menu.Value = 0;
                app.CheckBox_Z_Manual.Enable = 'off';
                app.EditField_Z.Enable = 'on';
                app.DropDown_Z.Enable = 'off';
                
                if isempty(app.EditField_Z.Value)
                    app.EditField_Z.Value = char(app.DropDown_Z.Items(app.DropDown_Z.Value));
                    Update = 0;
                end
                
            else
                app.CheckBox_Z_Menu.Enable = 'off';
                app.CheckBox_Z_Menu.Value = 1;
                app.CheckBox_Z_Manual.Enable = 'on';
                app.EditField_Z.Enable = 'off';
                app.DropDown_Z.Enable = 'on';
            end
            
            if Update
                PlotData(app);
            end
            
        end

        % Value changed function: CheckBox_X_Menu, CheckBox_Y_Menu, 
        % CheckBox_Z_Menu
        function MenuSelectionCheckBox_ValueChanged(app, event)
            
            if isequal(app.CheckBox_X_Menu.Value,1)
                app.CheckBox_X_Manual.Enable = 'on';
                app.CheckBox_X_Manual.Value = 0;
                app.CheckBox_X_Menu.Enable = 'off';
                app.EditField_X.Enable = 'off';
                app.DropDown_X.Enable = 'on';
            else
                app.CheckBox_X_Manual.Enable = 'off';
                app.CheckBox_X_Manual.Value = 1;
                app.CheckBox_X_Menu.Enable = 'on';
                app.EditField_X.Enable = 'on';
                app.DropDown_X.Enable = 'off';
            end
            
            if isequal(app.CheckBox_Y_Menu.Value,1)
                app.CheckBox_Y_Manual.Enable = 'on';
                app.CheckBox_Y_Manual.Value = 0;
                app.CheckBox_Y_Menu.Enable = 'off';
                app.EditField_Y.Enable = 'off';
                app.DropDown_Y.Enable = 'on';
            else
                app.CheckBox_Y_Manual.Enable = 'off';
                app.CheckBox_Y_Manual.Value = 1;
                app.CheckBox_Y_Menu.Enable = 'on';
                app.EditField_Y.Enable = 'on';
                app.DropDown_Y.Enable = 'off';
            end
            
            if isequal(app.CheckBox_Z_Menu.Value,1)
                app.CheckBox_Z_Manual.Enable = 'on';
                app.CheckBox_Z_Manual.Value = 0;
                app.CheckBox_Z_Menu.Enable = 'off';
                app.EditField_Z.Enable = 'off';
                app.DropDown_Z.Enable = 'on';
            else
                app.CheckBox_Z_Manual.Enable = 'off';
                app.CheckBox_Z_Manual.Value = 1;
                app.CheckBox_Z_Menu.Enable = 'on';
                app.EditField_Y.Enable = 'on';
                app.DropDown_Z.Enable = 'off';
            end
            
            PlotData(app);
        end

        % Value changed function: EditField_X, EditField_Y, 
        % EditField_Z
        function EditField_XYZValueChanged(app, event)
            PlotData(app);
        end

        % Close request function: VisualizationTool
        function VisualizationToolCloseRequest(app, event)
            diary('off');
            delete(app)
        end

        % Value changed function: CheckBox_LowRessourcesMode
        function CheckBox_LowRessourcesModeValueChanged(app, event)
            PlotData(app);
        end

        % Button pushed function: Button_help
        function Button_helpPushed(app, event)
            if isempty(app.XMapToolsApp.Id_HelpTool)
                Help_Display(app.XMapToolsApp,'XMT_help_DataVizualizationTools.html');
            else
                app.XMapToolsApp.Id_HelpTool.UpdateTextHelp('XMT_help_DataVizualizationTools.html');
            end
        end

        % Value changed function: DropDown_Masks_2
        function DropDown_Masks_2ValueChanged(app, event)
            
            SelectedMask = app.DropDown_Masks.Value;
            SelectedMin = app.DropDown_Masks_2.Value;
            
            try
                if length(app.MaskFile.Masks(SelectedMask).SubMask(SelectedMin+1).Names) > 1
                    app.DropDown_Masks_3.Items = app.MaskFile.Masks(SelectedMask).SubMask(SelectedMin+1).Names;
                    app.DropDown_Masks_3.ItemsData = [0:length(app.MaskFile.Masks(SelectedMask).SubMask(SelectedMin+1).Names)-1];
                    app.DropDown_Masks_3.Value = 0;
                    
                    app.DropDown_Masks_3.Visible = 'on';
                else
                    app.DropDown_Masks_3.Visible = 'off';
                end
            catch ME
                app.DropDown_Masks_3.Visible = 'off';
            end
            
            PlotData(app);
        end

        % Value changed function: CheckBox_FilterZeros
        function CheckBox_FilterZerosValueChanged(app, event)
            PlotData(app);
        end

        % Value changed function: ThresholdLRMEditField
        function ThresholdLRMEditFieldValueChanged(app, event)
            PlotData(app);
        end

        % Value changed function: DensityTernaryLRMCheckBox
        function DensityTernaryLRMCheckBoxValueChanged(app, event)
            PlotData(app);
        end

        % Button pushed function: MakeMaskFile_Button
        function MakeMaskFile_ButtonPushed(app, event)
            
            % Check if we have a maskfile selected
            SubmaskExport = 0;
            if ~isempty(app.DropDown_Masks.Items) && ~isempty(app.DropDown_Masks_2.Items)
                if app.DropDown_Masks_2.Value > 0 % A mask is selected
                    SubmaskExport = 1;
                end
            end
            
            if isequal(SubmaskExport,1)
                
                SelectedMask = app.DropDown_Masks.Value;
                SelectedClass = app.DropDown_Masks_2.Value;
                
                MaskFile = app.XMapToolsApp.XMapToolsData.MapData.MaskFile;
                
                Mask = app.LastSelectedPx;
                NbMask = max(Mask(:)) - 1;
                
                TrainSetList = {};
                for i = 1:NbMask
                    TrainSetList{i} = [char(MaskFile.Masks(SelectedMask).Names{SelectedClass+1}),'_',num2str(i)];
                end
                
                Mask = Mask - 1;
                
                % Find unselected pixels
                MaskMap = zeros(size(MaskFile.Masks(SelectedMask).MaskMap));
                WherePxClass = find(MaskFile.Masks(SelectedMask).MaskMap == SelectedClass);
                MaskMap(WherePxClass) = 1;
                
                % figure, imagesc(MaskMap), axis image, colorbar
                
                UnselectedPx = find (MaskMap > 0 & Mask < 1);
                
                if ~isempty(UnselectedPx)
                    Mask(UnselectedPx) = NbMask + 1;
                    TrainSetList = {'None',TrainSetList{:},'UnselectedPx'};
                else
                    TrainSetList = {'None',TrainSetList{:}};
                end
                
                % figure, imagesc(Mask), axis image, colorbar
                
                SelectedClass = SelectedClass + 1;
                
                MaskFile.Masks(SelectedMask).SubMask(SelectedClass).Names = TrainSetList;
                MaskFile.Masks(SelectedMask).SubMask(SelectedClass).Densities = zeros(1,length(TrainSetList));
                MaskFile.Masks(SelectedMask).SubMask(SelectedClass).MaskSelMaskMap = Mask;
                MaskFile.Masks(SelectedMask).SubMask(SelectedClass).MaskProbability = 0;
                
                MaskFile.Masks(SelectedMask).SubMask(SelectedClass).Info.Algorithm = 'Manual classification using DVM';
                MaskFile.Masks(SelectedMask).SubMask(SelectedClass).Info.SelectedData = {};
                MaskFile.Masks(SelectedMask).SubMask(SelectedClass).Info.Scaling = '';
                MaskFile.Masks(SelectedMask).SubMask(SelectedClass).Info.Reproducibility = '';
                MaskFile.Masks(SelectedMask).SubMask(SelectedClass).Info.Classes = {};
                MaskFile.Masks(SelectedMask).SubMask(SelectedClass).Info.SelectedPx = [];
                MaskFile.Masks(SelectedMask).SubMask(SelectedClass).Info.TrainPx = [];
                MaskFile.Masks(SelectedMask).SubMask(SelectedClass).Info.TestPx = [];
                MaskFile.Masks(SelectedMask).SubMask(SelectedClass).Info.Accuracy = [];
                MaskFile.Masks(SelectedMask).SubMask(SelectedClass).Info.Precision = [];
                MaskFile.Masks(SelectedMask).SubMask(SelectedClass).Info.Recall = [];
                MaskFile.Masks(SelectedMask).SubMask(SelectedClass).Info.F1Score = [];
                MaskFile.Masks(SelectedMask).SubMask(SelectedClass).Info.Modes = [];
                
                app.XMapToolsApp.XMapToolsData.MapData.MaskFile = MaskFile;
                
                % Clear pre-existing tree
                for i = 1:length(app.XMapToolsApp.Node_Masks.Children(SelectedMask).Children(SelectedClass).Children)
                    app.XMapToolsApp.Node_Masks.Children(SelectedMask).Children(SelectedClass).Children(1).delete;
                end
                
                % Update the tree
                for i = 1:length(MaskFile.Masks(SelectedMask).SubMask(SelectedClass).Names)
                    p = uitreenode(app.XMapToolsApp.Node_Masks.Children(SelectedMask).Children(SelectedClass),'Text',char(MaskFile.Masks(SelectedMask).SubMask(SelectedClass).Names{i}),'NodeData',[11,SelectedMask,SelectedClass,i]);
                end
                
                app.XMapToolsApp.TreeData_Additional.SelectedNodes = app.XMapToolsApp.Node_Masks.Children(SelectedMask).Children(SelectedClass).Children(1);
                expand(app.XMapToolsApp.Node_Masks.Children(SelectedMask).Children(SelectedClass));
                
                uialert(app.VisualizationTool,'These submasks have been added to the selected maskfile in XMapTools. You can close the Data Visualization Module to display the new maskfile.','XMapTools','Icon','success');
                
            else
                % Send the maskfile to XMapTools
                
                Mask = app.LastSelectedPx;
                NbMask = max(Mask(:)) - 1;
                
                for i = 1:NbMask
                    TrainSetList{i} = ['Selection_',num2str(i)];
                end
                
                % Group the unclassified pixels in one group
                WhereZeros = find(Mask == 0);
                
                Mask = Mask - 1;
                
                if ~isempty(WhereZeros)
                    Mask(WhereZeros) = NbMask + 1;
                    TrainSetList{end+1} = 'Unselected';
                    NbMask = NbMask + 1;
                end
                
                MaskFile = app.XMapToolsApp.XMapToolsData.MapData.MaskFile;
                IdxMF = length(app.XMapToolsApp.XMapToolsData.MapData.MaskFile.Names)+1;
                
                NewMaskFileName = ['Maskfile_DVT_',num2str(IdxMF)];
                
                MaskFile.Names{IdxMF} = NewMaskFileName;
                
                MaskFile.Types(IdxMF) = 0;
                MaskFile.NbMasks(IdxMF) = NbMask+1;
                
                MaskFile.Masks(IdxMF).Names = ['None',TrainSetList];
                MaskFile.Masks(IdxMF).Densities = zeros(1,length(TrainSetList)+1);
                MaskFile.Masks(IdxMF).MaskMap = Mask;
                MaskFile.Masks(IdxMF).MaskProbability = 0;
                MaskFile.Masks(IdxMF).Signature = now;
                
                % Update 3.4.4
                if isequal(NbMask,1)
                    MaskFile.Masks(IdxMF).Colors = [0,0,0;0.9,0.1,0];
                else
                    MaskFile.Masks(IdxMF).Colors = [0,0,0;app.XMapToolsApp.CalculateColorMap(2,NbMask)];
                end
                MaskFile.Masks(IdxMF).Info.Algorithm = 'Manual classification using DVM';
                MaskFile.Masks(IdxMF).Info.SelectedData = {};
                MaskFile.Masks(IdxMF).Info.Scaling = '';
                MaskFile.Masks(IdxMF).Info.Reproducibility = '';
                MaskFile.Masks(IdxMF).Info.Classes = {};
                MaskFile.Masks(IdxMF).Info.SelectedPx = [];
                MaskFile.Masks(IdxMF).Info.TrainPx = [];
                MaskFile.Masks(IdxMF).Info.TestPx = [];
                MaskFile.Masks(IdxMF).Info.Accuracy = [];
                MaskFile.Masks(IdxMF).Info.Precision = [];
                MaskFile.Masks(IdxMF).Info.Recall = [];
                MaskFile.Masks(IdxMF).Info.F1Score = [];
                MaskFile.Masks(IdxMF).Info.Modes = [];
                %
                MaskFile.Masks(IdxMF).SubMask(1).Names = {};
                MaskFile.Masks(IdxMF).SubMask(1).Densities = [];
                MaskFile.Masks(IdxMF).SubMask(1).MaskMap = [];
                MaskFile.Masks(IdxMF).SubMask(1).MaskProbability = [];
                %
                MaskFile.Masks(IdxMF).SubMask(1).Info.Algorithm = '';
                MaskFile.Masks(IdxMF).SubMask(1).Info.SelectedData = {};
                MaskFile.Masks(IdxMF).SubMask(1).Info.Scaling = '';
                MaskFile.Masks(IdxMF).SubMask(1).Info.Reproducibility = '';
                MaskFile.Masks(IdxMF).SubMask(1).Info.Classes = {};
                MaskFile.Masks(IdxMF).SubMask(1).Info.SelectedPx = [];
                MaskFile.Masks(IdxMF).SubMask(1).Info.TrainPx = [];
                MaskFile.Masks(IdxMF).SubMask(1).Info.TestPx = [];
                MaskFile.Masks(IdxMF).SubMask(1).Info.Accuracy = [];
                MaskFile.Masks(IdxMF).SubMask(1).Info.Precision = [];
                MaskFile.Masks(IdxMF).SubMask(1).Info.Recall = [];
                MaskFile.Masks(IdxMF).SubMask(1).Info.F1Score = [];
                MaskFile.Masks(IdxMF).SubMask(1).Info.Modes = [];
                
                for i = 1:length(TrainSetList)+1
                    MaskFile.Masks(IdxMF).SubMask(i).Names = {};
                    MaskFile.Masks(IdxMF).SubMask(i).Densities = [];
                    MaskFile.Masks(IdxMF).SubMask(i).MaskMap = [];
                    MaskFile.Masks(IdxMF).SubMask(i).MaskProbability = [];
                end
                
                app.XMapToolsApp.XMapToolsData.MapData.MaskFile = MaskFile;
                
                p = uitreenode(app.XMapToolsApp.Node_Masks,'Text',char(app.XMapToolsApp.XMapToolsData.MapData.MaskFile.Names{IdxMF}),'NodeData',[11,IdxMF,0,0]);
                for i = 1:app.XMapToolsApp.XMapToolsData.MapData.MaskFile.NbMasks(IdxMF)
                    p1 = uitreenode(p,'Text',char(app.XMapToolsApp.XMapToolsData.MapData.MaskFile.Masks(IdxMF).Names{i}),'NodeData',[11,IdxMF,i,0]);
                end
                %expand(app.Node_Masks);
                app.XMapToolsApp.TreeData_Additional.SelectedNodes = p;
                expand(p);
                
                uialert(app.VisualizationTool,'This maskfile has been added to XMapTools. You can close the Data Visualization Module to display the new maskfile.','XMapTools','Icon','success');
                
            end
            
        end

        % Button pushed function: Explore_ResetROI_Button
        function Explore_ResetROI_ButtonPushed(app, event)
            ROI_DeleteROI(app);
            cla(app.FigPlot2,'reset')
            app.FigPlot2.Visible = 'off';
            app.ROI_PI_Multi.Nb = 0;
            app.ROI_PI_Multi.ROI = []; 
            app.UITableRoiResults.Visible = 'off';
        end

        % Value changed function: DropDown_Masks_3
        function DropDown_Masks_3ValueChanged(app, event)
            PlotData(app);
        end

        % Value changed function: DensityMapResolutionField
        function DensityMapResolutionFieldValueChanged(app, event)
            PlotData(app);
        end

        % Value changed function: 
        % binary_densitymapinlowresourcemodeCheckBox
        function binary_densitymapinlowresourcemodeCheckBoxValueChanged(app, event)
            PlotData(app);
        end

        % Value changed function: log_X_CheckBox
        function log_X_CheckBoxValueChanged(app, event)
            PlotData(app);
        end

        % Value changed function: log_Y_CheckBox
        function log_Y_CheckBoxValueChanged(app, event)
            PlotData(app);
        end

        % Value changed function: CheckBox_BCR_Filter
        function CheckBox_BCR_FilterValueChanged(app, event)
            PlotData(app);
        end

        % Value changed function: HoldonCheckBox
        function HoldonCheckBoxValueChanged(app, event)
            Explore_ResetROI_ButtonPushed(app);
        end

        % Value changed function: PlotBulkCheckBox
        function PlotBulkCheckBoxValueChanged(app, event)
            PlotData(app);
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create VisualizationTool and hide until all components are created
            app.VisualizationTool = uifigure('Visible', 'off');
            app.VisualizationTool.Position = [100 100 1262 793];
            app.VisualizationTool.Name = 'Data Visualization Module – XMapTools';
            app.VisualizationTool.CloseRequestFcn = createCallbackFcn(app, @VisualizationToolCloseRequest, true);

            % Create GridLayout
            app.GridLayout = uigridlayout(app.VisualizationTool);
            app.GridLayout.ColumnWidth = {'0.3x', '1x', '1x', '1x', '1x', '0.25x', '1x', '1x', '1x', '1x'};
            app.GridLayout.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.5x'};

            % Create Image
            app.Image = uiimage(app.GridLayout);
            app.Image.Layout.Row = 1;
            app.Image.Layout.Column = [1 3];
            app.Image.HorizontalAlignment = 'left';
            app.Image.ImageSource = 'logo_xmap_final.png';

            % Create TabGroup
            app.TabGroup = uitabgroup(app.GridLayout);
            app.TabGroup.SelectionChangedFcn = createCallbackFcn(app, @TabGroupSelectionChanged, true);
            app.TabGroup.Layout.Row = [2 4];
            app.TabGroup.Layout.Column = [1 6];

            % Create PlotTab
            app.PlotTab = uitab(app.TabGroup);
            app.PlotTab.Title = 'Plot';

            % Create GridLayout7_8
            app.GridLayout7_8 = uigridlayout(app.PlotTab);
            app.GridLayout7_8.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout7_8.RowHeight = {'0.8x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout7_8.ColumnSpacing = 4;
            app.GridLayout7_8.RowSpacing = 4;
            app.GridLayout7_8.Padding = [5 5 5 5];

            % Create VariableDefinitionsLabel
            app.VariableDefinitionsLabel = uilabel(app.GridLayout7_8);
            app.VariableDefinitionsLabel.BackgroundColor = [0.902 0.902 0.902];
            app.VariableDefinitionsLabel.HorizontalAlignment = 'center';
            app.VariableDefinitionsLabel.FontSize = 10;
            app.VariableDefinitionsLabel.FontAngle = 'italic';
            app.VariableDefinitionsLabel.FontColor = [0.149 0.149 0.149];
            app.VariableDefinitionsLabel.Layout.Row = 1;
            app.VariableDefinitionsLabel.Layout.Column = [1 17];
            app.VariableDefinitionsLabel.Text = 'Variable Definitions';

            % Create DropDown_X
            app.DropDown_X = uidropdown(app.GridLayout7_8);
            app.DropDown_X.Items = {'Option X'};
            app.DropDown_X.ValueChangedFcn = createCallbackFcn(app, @DropDown_XValueChanged, true);
            app.DropDown_X.Layout.Row = 2;
            app.DropDown_X.Layout.Column = [3 7];
            app.DropDown_X.Value = 'Option X';

            % Create CheckBox_X_Menu
            app.CheckBox_X_Menu = uicheckbox(app.GridLayout7_8);
            app.CheckBox_X_Menu.ValueChangedFcn = createCallbackFcn(app, @MenuSelectionCheckBox_ValueChanged, true);
            app.CheckBox_X_Menu.Text = '';
            app.CheckBox_X_Menu.Layout.Row = 2;
            app.CheckBox_X_Menu.Layout.Column = 1;
            app.CheckBox_X_Menu.Value = true;

            % Create CheckBox_X_Manual
            app.CheckBox_X_Manual = uicheckbox(app.GridLayout7_8);
            app.CheckBox_X_Manual.ValueChangedFcn = createCallbackFcn(app, @ManualSelectionCheckBox_ValueChanged, true);
            app.CheckBox_X_Manual.Text = '';
            app.CheckBox_X_Manual.Layout.Row = 2;
            app.CheckBox_X_Manual.Layout.Column = 8;
            app.CheckBox_X_Manual.Value = true;

            % Create EditField_X
            app.EditField_X = uieditfield(app.GridLayout7_8, 'text');
            app.EditField_X.ValueChangedFcn = createCallbackFcn(app, @EditField_XYZValueChanged, true);
            app.EditField_X.Layout.Row = 2;
            app.EditField_X.Layout.Column = [9 17];

            % Create CheckBox_Y_Menu
            app.CheckBox_Y_Menu = uicheckbox(app.GridLayout7_8);
            app.CheckBox_Y_Menu.ValueChangedFcn = createCallbackFcn(app, @MenuSelectionCheckBox_ValueChanged, true);
            app.CheckBox_Y_Menu.Text = '';
            app.CheckBox_Y_Menu.Layout.Row = 3;
            app.CheckBox_Y_Menu.Layout.Column = 1;
            app.CheckBox_Y_Menu.Value = true;

            % Create DropDown_Y
            app.DropDown_Y = uidropdown(app.GridLayout7_8);
            app.DropDown_Y.Items = {'Option Y'};
            app.DropDown_Y.ValueChangedFcn = createCallbackFcn(app, @DropDown_YValueChanged, true);
            app.DropDown_Y.Layout.Row = 3;
            app.DropDown_Y.Layout.Column = [3 7];
            app.DropDown_Y.Value = 'Option Y';

            % Create CheckBox_Y_Manual
            app.CheckBox_Y_Manual = uicheckbox(app.GridLayout7_8);
            app.CheckBox_Y_Manual.ValueChangedFcn = createCallbackFcn(app, @ManualSelectionCheckBox_ValueChanged, true);
            app.CheckBox_Y_Manual.Text = '';
            app.CheckBox_Y_Manual.Layout.Row = 3;
            app.CheckBox_Y_Manual.Layout.Column = 8;
            app.CheckBox_Y_Manual.Value = true;

            % Create EditField_Y
            app.EditField_Y = uieditfield(app.GridLayout7_8, 'text');
            app.EditField_Y.ValueChangedFcn = createCallbackFcn(app, @EditField_XYZValueChanged, true);
            app.EditField_Y.Layout.Row = 3;
            app.EditField_Y.Layout.Column = [9 17];

            % Create CheckBox_Z_Menu
            app.CheckBox_Z_Menu = uicheckbox(app.GridLayout7_8);
            app.CheckBox_Z_Menu.ValueChangedFcn = createCallbackFcn(app, @MenuSelectionCheckBox_ValueChanged, true);
            app.CheckBox_Z_Menu.Text = '';
            app.CheckBox_Z_Menu.Layout.Row = 4;
            app.CheckBox_Z_Menu.Layout.Column = 1;
            app.CheckBox_Z_Menu.Value = true;

            % Create DropDown_Z
            app.DropDown_Z = uidropdown(app.GridLayout7_8);
            app.DropDown_Z.Items = {'Option Z'};
            app.DropDown_Z.ValueChangedFcn = createCallbackFcn(app, @DropDown_ZValueChanged, true);
            app.DropDown_Z.Layout.Row = 4;
            app.DropDown_Z.Layout.Column = [3 7];
            app.DropDown_Z.Value = 'Option Z';

            % Create CheckBox_Z_Manual
            app.CheckBox_Z_Manual = uicheckbox(app.GridLayout7_8);
            app.CheckBox_Z_Manual.ValueChangedFcn = createCallbackFcn(app, @ManualSelectionCheckBox_ValueChanged, true);
            app.CheckBox_Z_Manual.Text = '';
            app.CheckBox_Z_Manual.Layout.Row = 4;
            app.CheckBox_Z_Manual.Layout.Column = 8;
            app.CheckBox_Z_Manual.Value = true;

            % Create EditField_Z
            app.EditField_Z = uieditfield(app.GridLayout7_8, 'text');
            app.EditField_Z.ValueChangedFcn = createCallbackFcn(app, @EditField_XYZValueChanged, true);
            app.EditField_Z.Layout.Row = 4;
            app.EditField_Z.Layout.Column = [9 17];

            % Create Label_X
            app.Label_X = uilabel(app.GridLayout7_8);
            app.Label_X.HorizontalAlignment = 'center';
            app.Label_X.FontWeight = 'bold';
            app.Label_X.Layout.Row = 2;
            app.Label_X.Layout.Column = 2;
            app.Label_X.Text = 'X';

            % Create Label_Y
            app.Label_Y = uilabel(app.GridLayout7_8);
            app.Label_Y.HorizontalAlignment = 'center';
            app.Label_Y.FontWeight = 'bold';
            app.Label_Y.Layout.Row = 3;
            app.Label_Y.Layout.Column = 2;
            app.Label_Y.Text = 'Y';

            % Create Label_Z
            app.Label_Z = uilabel(app.GridLayout7_8);
            app.Label_Z.HorizontalAlignment = 'center';
            app.Label_Z.FontWeight = 'bold';
            app.Label_Z.Layout.Row = 4;
            app.Label_Z.Layout.Column = 2;
            app.Label_Z.Text = 'Z';

            % Create Button_Histogram
            app.Button_Histogram = uibutton(app.GridLayout7_8, 'state');
            app.Button_Histogram.ValueChangedFcn = createCallbackFcn(app, @Button_HistogramValueChanged, true);
            app.Button_Histogram.Icon = 'XXX_stats.png';
            app.Button_Histogram.IconAlignment = 'top';
            app.Button_Histogram.Text = 'Histogram';
            app.Button_Histogram.FontSize = 10;
            app.Button_Histogram.Layout.Row = [2 3];
            app.Button_Histogram.Layout.Column = [19 20];

            % Create Button_Binary
            app.Button_Binary = uibutton(app.GridLayout7_8, 'state');
            app.Button_Binary.ValueChangedFcn = createCallbackFcn(app, @Button_BinaryValueChanged, true);
            app.Button_Binary.Icon = 'XXX_chess.png';
            app.Button_Binary.IconAlignment = 'top';
            app.Button_Binary.Text = 'Binary';
            app.Button_Binary.FontSize = 10;
            app.Button_Binary.Layout.Row = [2 3];
            app.Button_Binary.Layout.Column = [21 22];

            % Create Button_RGB
            app.Button_RGB = uibutton(app.GridLayout7_8, 'state');
            app.Button_RGB.ValueChangedFcn = createCallbackFcn(app, @Button_RGBValueChanged, true);
            app.Button_RGB.Icon = 'XXX_images.png';
            app.Button_RGB.IconAlignment = 'top';
            app.Button_RGB.Text = 'RGB';
            app.Button_RGB.FontSize = 10;
            app.Button_RGB.Layout.Row = [4 5];
            app.Button_RGB.Layout.Column = [19 20];

            % Create Button_Ternary
            app.Button_Ternary = uibutton(app.GridLayout7_8, 'state');
            app.Button_Ternary.ValueChangedFcn = createCallbackFcn(app, @Button_TernaryValueChanged, true);
            app.Button_Ternary.Icon = 'XXX_3d_modelling.png';
            app.Button_Ternary.IconAlignment = 'top';
            app.Button_Ternary.Text = 'Ternary';
            app.Button_Ternary.FontSize = 10;
            app.Button_Ternary.Layout.Row = [4 5];
            app.Button_Ternary.Layout.Column = [21 22];

            % Create PlottingModeLabel
            app.PlottingModeLabel = uilabel(app.GridLayout7_8);
            app.PlottingModeLabel.BackgroundColor = [0.902 0.902 0.902];
            app.PlottingModeLabel.HorizontalAlignment = 'center';
            app.PlottingModeLabel.FontSize = 10;
            app.PlottingModeLabel.FontAngle = 'italic';
            app.PlottingModeLabel.FontColor = [0.149 0.149 0.149];
            app.PlottingModeLabel.Layout.Row = 1;
            app.PlottingModeLabel.Layout.Column = [19 22];
            app.PlottingModeLabel.Text = 'Plotting Mode';

            % Create CheckBox_Masks
            app.CheckBox_Masks = uicheckbox(app.GridLayout7_8);
            app.CheckBox_Masks.ValueChangedFcn = createCallbackFcn(app, @CheckBox_MasksValueChanged, true);
            app.CheckBox_Masks.Text = '';
            app.CheckBox_Masks.Layout.Row = 6;
            app.CheckBox_Masks.Layout.Column = 1;
            app.CheckBox_Masks.Value = true;

            % Create Label_Maks
            app.Label_Maks = uilabel(app.GridLayout7_8);
            app.Label_Maks.HorizontalAlignment = 'center';
            app.Label_Maks.FontWeight = 'bold';
            app.Label_Maks.Layout.Row = 6;
            app.Label_Maks.Layout.Column = [2 3];
            app.Label_Maks.Text = 'Masks';

            % Create DropDown_Masks
            app.DropDown_Masks = uidropdown(app.GridLayout7_8);
            app.DropDown_Masks.Items = {'Option Masks'};
            app.DropDown_Masks.ValueChangedFcn = createCallbackFcn(app, @DropDown_MasksValueChanged, true);
            app.DropDown_Masks.Layout.Row = 6;
            app.DropDown_Masks.Layout.Column = [4 7];
            app.DropDown_Masks.Value = 'Option Masks';

            % Create Label_Maks_2
            app.Label_Maks_2 = uilabel(app.GridLayout7_8);
            app.Label_Maks_2.HorizontalAlignment = 'right';
            app.Label_Maks_2.FontWeight = 'bold';
            app.Label_Maks_2.Layout.Row = 6;
            app.Label_Maks_2.Layout.Column = [17 21];
            app.Label_Maks_2.Text = 'Low Resource Mode ';

            % Create CheckBox_LowRessourcesMode
            app.CheckBox_LowRessourcesMode = uicheckbox(app.GridLayout7_8);
            app.CheckBox_LowRessourcesMode.ValueChangedFcn = createCallbackFcn(app, @CheckBox_LowRessourcesModeValueChanged, true);
            app.CheckBox_LowRessourcesMode.Text = '';
            app.CheckBox_LowRessourcesMode.Layout.Row = 6;
            app.CheckBox_LowRessourcesMode.Layout.Column = 22;
            app.CheckBox_LowRessourcesMode.Value = true;

            % Create DropDown_Masks_2
            app.DropDown_Masks_2 = uidropdown(app.GridLayout7_8);
            app.DropDown_Masks_2.Items = {'Option Masks'};
            app.DropDown_Masks_2.ItemsData = 0;
            app.DropDown_Masks_2.ValueChangedFcn = createCallbackFcn(app, @DropDown_Masks_2ValueChanged, true);
            app.DropDown_Masks_2.Layout.Row = 6;
            app.DropDown_Masks_2.Layout.Column = [8 11];
            app.DropDown_Masks_2.Value = 0;

            % Create CheckBox_FilterZeros
            app.CheckBox_FilterZeros = uicheckbox(app.GridLayout7_8);
            app.CheckBox_FilterZeros.ValueChangedFcn = createCallbackFcn(app, @CheckBox_FilterZerosValueChanged, true);
            app.CheckBox_FilterZeros.Text = '';
            app.CheckBox_FilterZeros.Layout.Row = 5;
            app.CheckBox_FilterZeros.Layout.Column = 17;

            % Create Label_Maks_3
            app.Label_Maks_3 = uilabel(app.GridLayout7_8);
            app.Label_Maks_3.HorizontalAlignment = 'right';
            app.Label_Maks_3.Layout.Row = 5;
            app.Label_Maks_3.Layout.Column = [14 16];
            app.Label_Maks_3.Text = 'Filter zeros';

            % Create DropDown_Masks_3
            app.DropDown_Masks_3 = uidropdown(app.GridLayout7_8);
            app.DropDown_Masks_3.Items = {'Option Masks'};
            app.DropDown_Masks_3.ItemsData = 0;
            app.DropDown_Masks_3.ValueChangedFcn = createCallbackFcn(app, @DropDown_Masks_3ValueChanged, true);
            app.DropDown_Masks_3.Layout.Row = 6;
            app.DropDown_Masks_3.Layout.Column = [12 15];
            app.DropDown_Masks_3.Value = 0;

            % Create CheckBox_BCR_Filter
            app.CheckBox_BCR_Filter = uicheckbox(app.GridLayout7_8);
            app.CheckBox_BCR_Filter.ValueChangedFcn = createCallbackFcn(app, @CheckBox_BCR_FilterValueChanged, true);
            app.CheckBox_BCR_Filter.Text = '';
            app.CheckBox_BCR_Filter.Layout.Row = 5;
            app.CheckBox_BCR_Filter.Layout.Column = 13;

            % Create Label_Maks_4
            app.Label_Maks_4 = uilabel(app.GridLayout7_8);
            app.Label_Maks_4.HorizontalAlignment = 'right';
            app.Label_Maks_4.Layout.Row = 5;
            app.Label_Maks_4.Layout.Column = [10 12];
            app.Label_Maks_4.Text = 'BRC filter';

            % Create ExploreClassifyTab
            app.ExploreClassifyTab = uitab(app.TabGroup);
            app.ExploreClassifyTab.Title = 'Explore & Classify';

            % Create GridLayout7_5
            app.GridLayout7_5 = uigridlayout(app.ExploreClassifyTab);
            app.GridLayout7_5.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout7_5.RowHeight = {'0.8x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout7_5.ColumnSpacing = 4;
            app.GridLayout7_5.RowSpacing = 4;
            app.GridLayout7_5.Padding = [5 5 5 5];

            % Create IdentifyPixelsToolsLabel
            app.IdentifyPixelsToolsLabel = uilabel(app.GridLayout7_5);
            app.IdentifyPixelsToolsLabel.BackgroundColor = [0.902 0.902 0.902];
            app.IdentifyPixelsToolsLabel.HorizontalAlignment = 'center';
            app.IdentifyPixelsToolsLabel.FontSize = 10;
            app.IdentifyPixelsToolsLabel.FontAngle = 'italic';
            app.IdentifyPixelsToolsLabel.FontColor = [0.149 0.149 0.149];
            app.IdentifyPixelsToolsLabel.Layout.Row = 1;
            app.IdentifyPixelsToolsLabel.Layout.Column = [1 6];
            app.IdentifyPixelsToolsLabel.Text = 'Identify Pixels Tools';

            % Create Explore_IdentifyPixels_PolygonButton
            app.Explore_IdentifyPixels_PolygonButton = uibutton(app.GridLayout7_5, 'push');
            app.Explore_IdentifyPixels_PolygonButton.ButtonPushedFcn = createCallbackFcn(app, @Explore_IdentifyPixels_PolygonButtonPushed, true);
            app.Explore_IdentifyPixels_PolygonButton.Icon = 'XXX_pin_rounded_plus.png';
            app.Explore_IdentifyPixels_PolygonButton.IconAlignment = 'top';
            app.Explore_IdentifyPixels_PolygonButton.FontSize = 9;
            app.Explore_IdentifyPixels_PolygonButton.Layout.Row = [2 3];
            app.Explore_IdentifyPixels_PolygonButton.Layout.Column = [1 2];
            app.Explore_IdentifyPixels_PolygonButton.Text = 'Polygon';

            % Create MakeMaskFile_Button
            app.MakeMaskFile_Button = uibutton(app.GridLayout7_5, 'push');
            app.MakeMaskFile_Button.ButtonPushedFcn = createCallbackFcn(app, @MakeMaskFile_ButtonPushed, true);
            app.MakeMaskFile_Button.Icon = '165-layers.png';
            app.MakeMaskFile_Button.IconAlignment = 'top';
            app.MakeMaskFile_Button.FontSize = 9;
            app.MakeMaskFile_Button.Layout.Row = [2 3];
            app.MakeMaskFile_Button.Layout.Column = [3 4];
            app.MakeMaskFile_Button.Text = 'Make Mask';

            % Create Explore_ResetROI_Button
            app.Explore_ResetROI_Button = uibutton(app.GridLayout7_5, 'push');
            app.Explore_ResetROI_Button.ButtonPushedFcn = createCallbackFcn(app, @Explore_ResetROI_ButtonPushed, true);
            app.Explore_ResetROI_Button.Icon = '058-error.png';
            app.Explore_ResetROI_Button.IconAlignment = 'top';
            app.Explore_ResetROI_Button.FontSize = 9;
            app.Explore_ResetROI_Button.Layout.Row = [2 3];
            app.Explore_ResetROI_Button.Layout.Column = [5 6];
            app.Explore_ResetROI_Button.Text = 'Reset ROI';

            % Create HoldonCheckBox
            app.HoldonCheckBox = uicheckbox(app.GridLayout7_5);
            app.HoldonCheckBox.ValueChangedFcn = createCallbackFcn(app, @HoldonCheckBoxValueChanged, true);
            app.HoldonCheckBox.Text = 'Hold on';
            app.HoldonCheckBox.Layout.Row = 4;
            app.HoldonCheckBox.Layout.Column = [1 6];

            % Create UITableRoiResults
            app.UITableRoiResults = uitable(app.GridLayout7_5);
            app.UITableRoiResults.ColumnName = {'ROI'; 'Pixels'; 'Percentage'};
            app.UITableRoiResults.RowName = {};
            app.UITableRoiResults.Layout.Row = [1 6];
            app.UITableRoiResults.Layout.Column = [7 22];

            % Create StatisticsOtherTab
            app.StatisticsOtherTab = uitab(app.TabGroup);
            app.StatisticsOtherTab.Title = 'Statistics & Other';

            % Create GridLayout7_7
            app.GridLayout7_7 = uigridlayout(app.StatisticsOtherTab);
            app.GridLayout7_7.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout7_7.RowHeight = {'0.6x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout7_7.ColumnSpacing = 4;
            app.GridLayout7_7.RowSpacing = 4;
            app.GridLayout7_7.Padding = [5 5 5 5];

            % Create Statistics_TextArea
            app.Statistics_TextArea = uitextarea(app.GridLayout7_7);
            app.Statistics_TextArea.Editable = 'off';
            app.Statistics_TextArea.FontSize = 9.5;
            app.Statistics_TextArea.FontColor = [0.2784 0.2784 0.2784];
            app.Statistics_TextArea.BackgroundColor = [0.9412 0.9412 0.9412];
            app.Statistics_TextArea.Layout.Row = [2 6];
            app.Statistics_TextArea.Layout.Column = [1 10];

            % Create Plotting_TextArea
            app.Plotting_TextArea = uitextarea(app.GridLayout7_7);
            app.Plotting_TextArea.Editable = 'off';
            app.Plotting_TextArea.FontSize = 9.5;
            app.Plotting_TextArea.FontColor = [0.2784 0.2784 0.2784];
            app.Plotting_TextArea.BackgroundColor = [0.9412 0.9412 0.9412];
            app.Plotting_TextArea.Layout.Row = [2 6];
            app.Plotting_TextArea.Layout.Column = [11 21];

            % Create StatisticsPlottedDataLabel
            app.StatisticsPlottedDataLabel = uilabel(app.GridLayout7_7);
            app.StatisticsPlottedDataLabel.BackgroundColor = [0.902 0.902 0.902];
            app.StatisticsPlottedDataLabel.HorizontalAlignment = 'center';
            app.StatisticsPlottedDataLabel.FontSize = 10;
            app.StatisticsPlottedDataLabel.FontAngle = 'italic';
            app.StatisticsPlottedDataLabel.FontColor = [0.149 0.149 0.149];
            app.StatisticsPlottedDataLabel.Layout.Row = 1;
            app.StatisticsPlottedDataLabel.Layout.Column = [1 10];
            app.StatisticsPlottedDataLabel.Text = 'Statistics – Plotted Data';

            % Create PlottingReportlastplotonlyLabel
            app.PlottingReportlastplotonlyLabel = uilabel(app.GridLayout7_7);
            app.PlottingReportlastplotonlyLabel.BackgroundColor = [0.902 0.902 0.902];
            app.PlottingReportlastplotonlyLabel.HorizontalAlignment = 'center';
            app.PlottingReportlastplotonlyLabel.FontSize = 10;
            app.PlottingReportlastplotonlyLabel.FontAngle = 'italic';
            app.PlottingReportlastplotonlyLabel.FontColor = [0.149 0.149 0.149];
            app.PlottingReportlastplotonlyLabel.Layout.Row = 1;
            app.PlottingReportlastplotonlyLabel.Layout.Column = [11 21];
            app.PlottingReportlastplotonlyLabel.Text = 'Plotting Report (last plot only)';

            % Create OptionsTab
            app.OptionsTab = uitab(app.TabGroup);
            app.OptionsTab.Title = 'Options';

            % Create GridLayout5
            app.GridLayout5 = uigridlayout(app.OptionsTab);
            app.GridLayout5.ColumnWidth = {'0.05x', '1x'};
            app.GridLayout5.RowHeight = {'1x'};

            % Create TabGroup2
            app.TabGroup2 = uitabgroup(app.GridLayout5);
            app.TabGroup2.Layout.Row = 1;
            app.TabGroup2.Layout.Column = 2;

            % Create Tab_Options_Hist
            app.Tab_Options_Hist = uitab(app.TabGroup2);
            app.Tab_Options_Hist.Title = 'Hist';

            % Create GridLayout7_2
            app.GridLayout7_2 = uigridlayout(app.Tab_Options_Hist);
            app.GridLayout7_2.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout7_2.RowHeight = {'1x', '1x', '1x', '1x'};
            app.GridLayout7_2.ColumnSpacing = 4;
            app.GridLayout7_2.RowSpacing = 4;
            app.GridLayout7_2.Padding = [5 5 5 5];

            % Create Tab_Options_Binary
            app.Tab_Options_Binary = uitab(app.TabGroup2);
            app.Tab_Options_Binary.Title = 'Binary';

            % Create GridLayout7
            app.GridLayout7 = uigridlayout(app.Tab_Options_Binary);
            app.GridLayout7.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout7.RowHeight = {'1x', '1x', '1x', '1x'};
            app.GridLayout7.ColumnSpacing = 4;
            app.GridLayout7.RowSpacing = 4;
            app.GridLayout7.Padding = [5 5 5 5];

            % Create PlothistogramsusinglogYscaleCheckBox
            app.PlothistogramsusinglogYscaleCheckBox = uicheckbox(app.GridLayout7);
            app.PlothistogramsusinglogYscaleCheckBox.ValueChangedFcn = createCallbackFcn(app, @PlothistogramsusinglogYscaleCheckBoxValueChanged, true);
            app.PlothistogramsusinglogYscaleCheckBox.Text = 'Plot histograms using log Y-scale ';
            app.PlothistogramsusinglogYscaleCheckBox.Layout.Row = 1;
            app.PlothistogramsusinglogYscaleCheckBox.Layout.Column = [1 10];
            app.PlothistogramsusinglogYscaleCheckBox.Value = true;

            % Create binary_densitymapinlowresourcemodeCheckBox
            app.binary_densitymapinlowresourcemodeCheckBox = uicheckbox(app.GridLayout7);
            app.binary_densitymapinlowresourcemodeCheckBox.ValueChangedFcn = createCallbackFcn(app, @binary_densitymapinlowresourcemodeCheckBoxValueChanged, true);
            app.binary_densitymapinlowresourcemodeCheckBox.Text = 'Plot density map in low resource mode';
            app.binary_densitymapinlowresourcemodeCheckBox.Layout.Row = 2;
            app.binary_densitymapinlowresourcemodeCheckBox.Layout.Column = [1 10];
            app.binary_densitymapinlowresourcemodeCheckBox.Value = true;

            % Create Tab_Options_RGB
            app.Tab_Options_RGB = uitab(app.TabGroup2);
            app.Tab_Options_RGB.Title = 'RGB';

            % Create GridLayout7_3
            app.GridLayout7_3 = uigridlayout(app.Tab_Options_RGB);
            app.GridLayout7_3.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout7_3.RowHeight = {'1x', '1x', '1x', '1x'};
            app.GridLayout7_3.ColumnSpacing = 4;
            app.GridLayout7_3.RowSpacing = 4;
            app.GridLayout7_3.Padding = [5 5 5 5];

            % Create Tab_Options_Ternary
            app.Tab_Options_Ternary = uitab(app.TabGroup2);
            app.Tab_Options_Ternary.Title = 'Ternary';

            % Create GridLayout7_4
            app.GridLayout7_4 = uigridlayout(app.Tab_Options_Ternary);
            app.GridLayout7_4.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout7_4.RowHeight = {'1x', '1x', '1x', '1x'};
            app.GridLayout7_4.ColumnSpacing = 4;
            app.GridLayout7_4.RowSpacing = 4;
            app.GridLayout7_4.Padding = [5 5 5 5];

            % Create DensityTernaryLRMCheckBox
            app.DensityTernaryLRMCheckBox = uicheckbox(app.GridLayout7_4);
            app.DensityTernaryLRMCheckBox.ValueChangedFcn = createCallbackFcn(app, @DensityTernaryLRMCheckBoxValueChanged, true);
            app.DensityTernaryLRMCheckBox.Text = 'Plot density map in Low resource mode';
            app.DensityTernaryLRMCheckBox.Layout.Row = 1;
            app.DensityTernaryLRMCheckBox.Layout.Column = [1 10];
            app.DensityTernaryLRMCheckBox.Value = true;

            % Create OtherTab
            app.OtherTab = uitab(app.TabGroup2);
            app.OtherTab.Title = 'Other';

            % Create GridLayout7_9
            app.GridLayout7_9 = uigridlayout(app.OtherTab);
            app.GridLayout7_9.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout7_9.RowHeight = {'1x', '1x', '1x', '1x'};
            app.GridLayout7_9.ColumnSpacing = 4;
            app.GridLayout7_9.RowSpacing = 4;
            app.GridLayout7_9.Padding = [5 5 5 5];

            % Create DensityMapResolutionField
            app.DensityMapResolutionField = uieditfield(app.GridLayout7_9, 'numeric');
            app.DensityMapResolutionField.Limits = [10 1000];
            app.DensityMapResolutionField.ValueChangedFcn = createCallbackFcn(app, @DensityMapResolutionFieldValueChanged, true);
            app.DensityMapResolutionField.FontSize = 11;
            app.DensityMapResolutionField.Layout.Row = 2;
            app.DensityMapResolutionField.Layout.Column = [9 10];
            app.DensityMapResolutionField.Value = 100;

            % Create Densitymapresolutiondefault100Label
            app.Densitymapresolutiondefault100Label = uilabel(app.GridLayout7_9);
            app.Densitymapresolutiondefault100Label.HorizontalAlignment = 'right';
            app.Densitymapresolutiondefault100Label.FontSize = 11;
            app.Densitymapresolutiondefault100Label.Layout.Row = 2;
            app.Densitymapresolutiondefault100Label.Layout.Column = [1 8];
            app.Densitymapresolutiondefault100Label.Text = 'Density map resolution (default 100)';

            % Create ThresholdLRMEditField
            app.ThresholdLRMEditField = uieditfield(app.GridLayout7_9, 'numeric');
            app.ThresholdLRMEditField.Limits = [1 100];
            app.ThresholdLRMEditField.ValueChangedFcn = createCallbackFcn(app, @ThresholdLRMEditFieldValueChanged, true);
            app.ThresholdLRMEditField.FontSize = 11;
            app.ThresholdLRMEditField.Layout.Row = 1;
            app.ThresholdLRMEditField.Layout.Column = [9 10];
            app.ThresholdLRMEditField.Value = 20;

            % Create ThresholdforlowresourcemodeinEditFieldLabel
            app.ThresholdforlowresourcemodeinEditFieldLabel = uilabel(app.GridLayout7_9);
            app.ThresholdforlowresourcemodeinEditFieldLabel.HorizontalAlignment = 'right';
            app.ThresholdforlowresourcemodeinEditFieldLabel.FontSize = 11;
            app.ThresholdforlowresourcemodeinEditFieldLabel.Layout.Row = 1;
            app.ThresholdforlowresourcemodeinEditFieldLabel.Layout.Column = [1 8];
            app.ThresholdforlowresourcemodeinEditFieldLabel.Text = 'Threshold for low resource mode (in %)';

            % Create GridLayout2
            app.GridLayout2 = uigridlayout(app.GridLayout);
            app.GridLayout2.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout2.RowHeight = {'1x'};
            app.GridLayout2.Padding = [10 15 10 15];
            app.GridLayout2.Layout.Row = 1;
            app.GridLayout2.Layout.Column = [4 6];

            % Create Button_help
            app.Button_help = uibutton(app.GridLayout2, 'push');
            app.Button_help.ButtonPushedFcn = createCallbackFcn(app, @Button_helpPushed, true);
            app.Button_help.Icon = '061-info.png';
            app.Button_help.Layout.Row = 1;
            app.Button_help.Layout.Column = 4;
            app.Button_help.Text = '';

            % Create GridLayout6
            app.GridLayout6 = uigridlayout(app.GridLayout);
            app.GridLayout6.ColumnWidth = {'1x'};
            app.GridLayout6.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout6.Padding = [6 2 6 2];
            app.GridLayout6.Layout.Row = [6 10];
            app.GridLayout6.Layout.Column = 1;

            % Create AutoZoom
            app.AutoZoom = uibutton(app.GridLayout6, 'push');
            app.AutoZoom.ButtonPushedFcn = createCallbackFcn(app, @AutoZoomButtonPushed, true);
            app.AutoZoom.Icon = 'XXX_magic-wand.png';
            app.AutoZoom.Layout.Row = 2;
            app.AutoZoom.Layout.Column = 1;
            app.AutoZoom.Text = '';

            % Create ResetZoom
            app.ResetZoom = uibutton(app.GridLayout6, 'push');
            app.ResetZoom.ButtonPushedFcn = createCallbackFcn(app, @ResetZoomButtonPushed, true);
            app.ResetZoom.Icon = 'XXX_home.png';
            app.ResetZoom.Layout.Row = 4;
            app.ResetZoom.Layout.Column = 1;
            app.ResetZoom.Text = '';

            % Create YminLabel
            app.YminLabel = uilabel(app.GridLayout6);
            app.YminLabel.HorizontalAlignment = 'center';
            app.YminLabel.VerticalAlignment = 'bottom';
            app.YminLabel.FontSize = 9;
            app.YminLabel.Layout.Row = 9;
            app.YminLabel.Layout.Column = 1;
            app.YminLabel.Text = 'Ymin';

            % Create ManualZoom
            app.ManualZoom = uibutton(app.GridLayout6, 'push');
            app.ManualZoom.ButtonPushedFcn = createCallbackFcn(app, @ManualZoomButtonPushed, true);
            app.ManualZoom.Icon = 'XXX_zoom_in.png';
            app.ManualZoom.Layout.Row = 3;
            app.ManualZoom.Layout.Column = 1;
            app.ManualZoom.Text = '';

            % Create GridLayout8
            app.GridLayout8 = uigridlayout(app.GridLayout);
            app.GridLayout8.ColumnWidth = {'1x'};
            app.GridLayout8.RowHeight = {'1x', '0.8x'};
            app.GridLayout8.ColumnSpacing = 2;
            app.GridLayout8.RowSpacing = 2;
            app.GridLayout8.Padding = [2 2 2 6];
            app.GridLayout8.Layout.Row = 5;
            app.GridLayout8.Layout.Column = 1;

            % Create Field_Ymax
            app.Field_Ymax = uieditfield(app.GridLayout8, 'numeric');
            app.Field_Ymax.ValueChangedFcn = createCallbackFcn(app, @Field_XminXMaxYminYmaxValueChanged, true);
            app.Field_Ymax.HorizontalAlignment = 'center';
            app.Field_Ymax.FontSize = 9;
            app.Field_Ymax.Layout.Row = 1;
            app.Field_Ymax.Layout.Column = 1;

            % Create YmaxLabel
            app.YmaxLabel = uilabel(app.GridLayout8);
            app.YmaxLabel.HorizontalAlignment = 'center';
            app.YmaxLabel.VerticalAlignment = 'top';
            app.YmaxLabel.FontSize = 9;
            app.YmaxLabel.Layout.Row = 2;
            app.YmaxLabel.Layout.Column = 1;
            app.YmaxLabel.Text = 'Ymax';

            % Create GridLayout8_2
            app.GridLayout8_2 = uigridlayout(app.GridLayout);
            app.GridLayout8_2.ColumnWidth = {'1x'};
            app.GridLayout8_2.RowHeight = {'1x', '1x', '1x'};
            app.GridLayout8_2.ColumnSpacing = 2;
            app.GridLayout8_2.RowSpacing = 4;
            app.GridLayout8_2.Padding = [2 2 2 4];
            app.GridLayout8_2.Layout.Row = [11 12];
            app.GridLayout8_2.Layout.Column = 1;

            % Create Field_Ymin
            app.Field_Ymin = uieditfield(app.GridLayout8_2, 'numeric');
            app.Field_Ymin.ValueChangedFcn = createCallbackFcn(app, @Field_XminXMaxYminYmaxValueChanged, true);
            app.Field_Ymin.HorizontalAlignment = 'center';
            app.Field_Ymin.FontSize = 9;
            app.Field_Ymin.Layout.Row = 1;
            app.Field_Ymin.Layout.Column = 1;

            % Create log_X_CheckBox
            app.log_X_CheckBox = uicheckbox(app.GridLayout8_2);
            app.log_X_CheckBox.ValueChangedFcn = createCallbackFcn(app, @log_X_CheckBoxValueChanged, true);
            app.log_X_CheckBox.Text = 'log';
            app.log_X_CheckBox.FontSize = 9;
            app.log_X_CheckBox.Layout.Row = 3;
            app.log_X_CheckBox.Layout.Column = 1;

            % Create log_Y_CheckBox
            app.log_Y_CheckBox = uicheckbox(app.GridLayout8_2);
            app.log_Y_CheckBox.ValueChangedFcn = createCallbackFcn(app, @log_Y_CheckBoxValueChanged, true);
            app.log_Y_CheckBox.Text = 'log';
            app.log_Y_CheckBox.FontSize = 9;
            app.log_Y_CheckBox.Layout.Row = 2;
            app.log_Y_CheckBox.Layout.Column = 1;

            % Create GridLayout9
            app.GridLayout9 = uigridlayout(app.GridLayout);
            app.GridLayout9.RowHeight = {'1x'};
            app.GridLayout9.ColumnSpacing = 8;
            app.GridLayout9.RowSpacing = 2;
            app.GridLayout9.Padding = [25 2 30 2];
            app.GridLayout9.Layout.Row = 12;
            app.GridLayout9.Layout.Column = 2;

            % Create Field_Xmin
            app.Field_Xmin = uieditfield(app.GridLayout9, 'numeric');
            app.Field_Xmin.ValueChangedFcn = createCallbackFcn(app, @Field_XminXMaxYminYmaxValueChanged, true);
            app.Field_Xmin.HorizontalAlignment = 'center';
            app.Field_Xmin.FontSize = 9;
            app.Field_Xmin.Layout.Row = 1;
            app.Field_Xmin.Layout.Column = 1;

            % Create XminLabel
            app.XminLabel = uilabel(app.GridLayout9);
            app.XminLabel.FontSize = 9;
            app.XminLabel.Layout.Row = 1;
            app.XminLabel.Layout.Column = 2;
            app.XminLabel.Text = 'Xmin';

            % Create GridLayout9_2
            app.GridLayout9_2 = uigridlayout(app.GridLayout);
            app.GridLayout9_2.RowHeight = {'1x'};
            app.GridLayout9_2.ColumnSpacing = 8;
            app.GridLayout9_2.RowSpacing = 2;
            app.GridLayout9_2.Padding = [60 2 35 2];
            app.GridLayout9_2.Layout.Row = 12;
            app.GridLayout9_2.Layout.Column = [5 6];

            % Create Field_Xmax
            app.Field_Xmax = uieditfield(app.GridLayout9_2, 'numeric');
            app.Field_Xmax.ValueChangedFcn = createCallbackFcn(app, @Field_XminXMaxYminYmaxValueChanged, true);
            app.Field_Xmax.HorizontalAlignment = 'center';
            app.Field_Xmax.FontSize = 9;
            app.Field_Xmax.Layout.Row = 1;
            app.Field_Xmax.Layout.Column = 2;

            % Create XmaxLabel
            app.XmaxLabel = uilabel(app.GridLayout9_2);
            app.XmaxLabel.HorizontalAlignment = 'right';
            app.XmaxLabel.FontSize = 9;
            app.XmaxLabel.Layout.Row = 1;
            app.XmaxLabel.Layout.Column = 1;
            app.XmaxLabel.Text = 'Xmax';

            % Create HoldaxislimitsCheckBox
            app.HoldaxislimitsCheckBox = uicheckbox(app.GridLayout);
            app.HoldaxislimitsCheckBox.Text = 'Hold axis limits';
            app.HoldaxislimitsCheckBox.Layout.Row = 12;
            app.HoldaxislimitsCheckBox.Layout.Column = 7;

            % Create PlotBulkCheckBox
            app.PlotBulkCheckBox = uicheckbox(app.GridLayout);
            app.PlotBulkCheckBox.ValueChangedFcn = createCallbackFcn(app, @PlotBulkCheckBoxValueChanged, true);
            app.PlotBulkCheckBox.Text = 'Plot "Bulk Value" (Average)';
            app.PlotBulkCheckBox.Layout.Row = 12;
            app.PlotBulkCheckBox.Layout.Column = [3 4];

            % Create FigPlot4
            app.FigPlot4 = uiaxes(app.GridLayout);
            title(app.FigPlot4, 'Title')
            xlabel(app.FigPlot4, 'X')
            ylabel(app.FigPlot4, 'Y')
            zlabel(app.FigPlot4, 'Z')
            app.FigPlot4.PlotBoxAspectRatio = [1.67883211678832 1 1];
            app.FigPlot4.FontSize = 9;
            app.FigPlot4.Layout.Row = [1 3];
            app.FigPlot4.Layout.Column = [9 10];

            % Create FigPlot5
            app.FigPlot5 = uiaxes(app.GridLayout);
            title(app.FigPlot5, 'Title')
            xlabel(app.FigPlot5, 'X')
            ylabel(app.FigPlot5, 'Y')
            zlabel(app.FigPlot5, 'Z')
            app.FigPlot5.PlotBoxAspectRatio = [1.67883211678832 1 1];
            app.FigPlot5.FontSize = 9;
            app.FigPlot5.Layout.Row = [4 6];
            app.FigPlot5.Layout.Column = [7 8];

            % Create FigPlot6
            app.FigPlot6 = uiaxes(app.GridLayout);
            title(app.FigPlot6, 'Title')
            xlabel(app.FigPlot6, 'X')
            ylabel(app.FigPlot6, 'Y')
            zlabel(app.FigPlot6, 'Z')
            app.FigPlot6.PlotBoxAspectRatio = [1.67883211678832 1 1];
            app.FigPlot6.FontSize = 9;
            app.FigPlot6.Layout.Row = [4 6];
            app.FigPlot6.Layout.Column = [9 10];

            % Create FigPlot1
            app.FigPlot1 = uiaxes(app.GridLayout);
            title(app.FigPlot1, 'Title')
            xlabel(app.FigPlot1, 'X')
            ylabel(app.FigPlot1, 'Y')
            zlabel(app.FigPlot1, 'Z')
            app.FigPlot1.PlotBoxAspectRatio = [1.28186274509804 1 1];
            app.FigPlot1.Layout.Row = [5 11];
            app.FigPlot1.Layout.Column = [2 5];

            % Create FigPlot3
            app.FigPlot3 = uiaxes(app.GridLayout);
            title(app.FigPlot3, 'Title')
            xlabel(app.FigPlot3, 'X')
            ylabel(app.FigPlot3, 'Y')
            zlabel(app.FigPlot3, 'Z')
            app.FigPlot3.PlotBoxAspectRatio = [1.67883211678832 1 1];
            app.FigPlot3.FontSize = 9;
            app.FigPlot3.Layout.Row = [1 3];
            app.FigPlot3.Layout.Column = [7 8];

            % Create FigPlot2
            app.FigPlot2 = uiaxes(app.GridLayout);
            title(app.FigPlot2, 'Title')
            xlabel(app.FigPlot2, 'X')
            ylabel(app.FigPlot2, 'Y')
            zlabel(app.FigPlot2, 'Z')
            app.FigPlot2.PlotBoxAspectRatio = [1.91176470588235 1 1];
            app.FigPlot2.Layout.Row = [7 11];
            app.FigPlot2.Layout.Column = [7 10];

            % Show the figure after all components are created
            app.VisualizationTool.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = Data_Visualization_exported(varargin)

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.VisualizationTool)

            % Execute the startup function
            runStartupFcn(app, @(app)startupFcn(app, varargin{:}))

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.VisualizationTool)
        end
    end
end