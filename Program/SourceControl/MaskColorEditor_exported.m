classdef MaskColorEditor_exported < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        MaskColorEditorGUI        matlab.ui.Figure
        GridLayout                matlab.ui.container.GridLayout
        UITable                   matlab.ui.control.Table
        Image                     matlab.ui.control.Image
        GridLayout2               matlab.ui.container.GridLayout
        AppyColor                 matlab.ui.control.Button
        XMapToolsColorPaletteforMineralsPanel  matlab.ui.container.Panel
        GridLayout3               matlab.ui.container.GridLayout
        ResetButton               matlab.ui.control.Button
        ApplydefaultcolorsLabel   matlab.ui.control.Label
        OtherColorPalettePanel    matlab.ui.container.Panel
        GridLayout3_2             matlab.ui.container.GridLayout
        ColorPalettesApplyButton  matlab.ui.control.Button
        ColorPalettesDropDown     matlab.ui.control.DropDown
        SelectacolorpaletteandapplycolorsautomaticallyLabel  matlab.ui.control.Label
        UIAxes                    matlab.ui.control.UIAxes
    end

    
    properties (Access = private)
        XMapToolsApp 
        MaskId
        
        Mask
        
        MineralColorData
        SelectedMineralColorData
        
        ActiveColorbar
        WaitBar
        
        StartModule % Description
    end
    
    methods (Access = private)
        
        function GenerateTable(app)
            
            if ~app.StartModule
                app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
                app.WaitBar.Message = 'Updating colors';
            end
            
            app.UITable.ColumnFormat = {'',app.MineralColorData,''};
            
            Cell4Table = {};
            for i = 1:length(app.Mask.Names)
                Cell4Table{i,1} = app.Mask.Names{i};
                Cell4Table{i,2} = app.SelectedMineralColorData{i};
                Cell4Table{i,3} = '';
            end
            
            app.UITable.Data = Cell4Table;
            
            for i = 1:length(app.Mask.Names)
                s = uistyle('BackgroundColor',app.Mask.Colors(i,:));
                addStyle(app.UITable,s,'cell',[i,3]);
                
                s1 = uistyle('FontWeight','bold');
                addStyle(app.UITable,s1,'cell',[i,1]);
            end
            
            % close(app.WaitBar);
            
        end
        
        
        function PlotMaskMap(app)
            
            if ~app.StartModule
                app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
                app.WaitBar.Message = 'Plotting the mask image';
            end
            
            imagesc(app.UIAxes,app.Mask.MaskMap);
            axis(app.UIAxes,'image');
            app.ActiveColorbar = colorbar(app.UIAxes,'Location','EastOutside');
            
            colormap(app.UIAxes,app.Mask.Colors);
            
            NbMasks = length(app.Mask.Names) - 1;
            
            caxis(app.UIAxes,[0 NbMasks+1]);
            
            app.ActiveColorbar.TickLabelsMode = 'manual';
            app.ActiveColorbar.Ticks = [0.5:1:NbMasks+2];
            app.ActiveColorbar.TickLabels = app.Mask.Names(1:end);
            
            drawnow
            
            if ~app.StartModule
                close(app.WaitBar);
            end
        end
    end
    

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, XMapToolsApp, MaskId)
            
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
            
            app.StartModule = 1;
            
            app.MaskColorEditorGUI.Visible = 'off';
            
            movegui(app.MaskColorEditorGUI,'center');
            
            app.XMapToolsApp = XMapToolsApp;
            app.MaskId = MaskId;
            
            app.Mask = app.XMapToolsApp.XMapToolsData.MapData.MaskFile.Masks(MaskId);
            
            app.MineralColorData = app.XMapToolsApp.MineralColorData.Names;
            app.MineralColorData{end+1} = 'Unavailable';
            app.MineralColorData{end+1} = 'Edited';
            
            % Initiate Selected Mineral Color Data
            app.SelectedMineralColorData = {};
            
            for i = 1:length(app.Mask.Names)
                WhereMin = find(ismember(app.MineralColorData,app.Mask.Names{i}));
                if ~isempty(WhereMin)
                   if isequal(app.Mask.Colors(i,:),app.XMapToolsApp.MineralColorData.RGB(WhereMin,:))
                        app.SelectedMineralColorData{i} = app.MineralColorData{WhereMin};
                   else
                       app.SelectedMineralColorData{i} = app.MineralColorData{end};  % edited
                   end
                else
                    app.SelectedMineralColorData{i} = app.MineralColorData{end-1};   % unavailable
                end
            end
            
            GenerateTable(app);
            PlotMaskMap(app);
            
            app.ColorPalettesDropDown.Items = app.XMapToolsApp.Options_ColormapDropDown.Items;
            
            app.MaskColorEditorGUI.Visible = 'on';
            
            app.StartModule = 0;
            
        end

        % Cell selection callback: UITable
        function UITableCellSelection(app, event)
            indices = event.Indices;
            if isequal(indices(2),3) && indices(1) > 1
                c = uisetcolor( app.Mask.Colors(indices(1),:),'Select a color');
                app.Mask.Colors(indices(1),:) = c;
                app.SelectedMineralColorData{indices(1)} = app.MineralColorData{end};
                GenerateTable(app);
                PlotMaskMap(app);
                figure(app.MaskColorEditorGUI)
            end
        end

        % Cell edit callback: UITable
        function UITableCellEdit(app, event)
            indices = event.Indices;
            newData = event.NewData;
            
            Where = find(ismember(app.MineralColorData,newData));
            if ~isempty(Where) && Where <= length(app.XMapToolsApp.MineralColorData.RGB) && indices(1) > 1
                app.SelectedMineralColorData{indices(1)} = app.MineralColorData{Where};
                app.Mask.Colors(indices(1),:) = app.XMapToolsApp.MineralColorData.RGB(Where,:);
                GenerateTable(app);
                PlotMaskMap(app);
                figure(app.MaskColorEditorGUI)
            end
            
        end

        % Button pushed function: AppyColor
        function AppyColorButtonPushed(app, event)
            app.XMapToolsApp.XMapToolsData.MapData.MaskFile.Masks(app.MaskId).Colors = app.Mask.Colors;
            
            MaskColorEditorGUICloseRequest(app);
        end

        % Close request function: MaskColorEditorGUI
        function MaskColorEditorGUICloseRequest(app, event)
            delete(app);
        end

        % Button pushed function: ColorPalettesApplyButton
        function ColorPalettesApplyButtonPushed(app, event)
            
            ColorMapSelectedMenu = app.ColorPalettesDropDown.Value;
            
            SelColorMap = find(ismember(app.ColorPalettesDropDown.Items,ColorMapSelectedMenu));
            ColorData = app.XMapToolsApp.ColorMaps(SelColorMap).Code;
            
            Resolution = length(app.Mask.Names)-1;
            
            Xi = 1:Resolution;
            Step = (Resolution-1)/(size(ColorData,1)-1);
            X = 1:Step:Resolution;
            
            ColorMap = zeros(length(Xi),size(ColorData,2));
            for i = 1:size(ColorData,2)
                ColorMap(:,i) = interp1(X',ColorData(:,i),Xi);
            end
            
            ColorMap = [0,0,0;ColorMap];
            
            app.Mask.Colors = ColorMap;
            GenerateTable(app);
            PlotMaskMap(app);
            
        end

        % Button pushed function: ResetButton
        function ResetButtonPushed(app, event)
            
            ColorMap = [];
            
            for i = 1:length(app.Mask.Names)
                WhereMin = find(ismember(app.XMapToolsApp.MineralColorData.Names,app.Mask.Names{i}));
                if ~isempty(WhereMin)
                    ColorMap(i,:) = app.XMapToolsApp.MineralColorData.RGB(WhereMin,:);
                else
                    ColorMap(i,:) = [0,0,0];
                end
            end
            
            app.Mask.Colors = ColorMap;
            GenerateTable(app);
            PlotMaskMap(app);
            
            
            
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create MaskColorEditorGUI and hide until all components are created
            app.MaskColorEditorGUI = uifigure('Visible', 'off');
            app.MaskColorEditorGUI.Position = [100 100 948 746];
            app.MaskColorEditorGUI.Name = 'Mask Color Editor – XMapTools';
            app.MaskColorEditorGUI.CloseRequestFcn = createCallbackFcn(app, @MaskColorEditorGUICloseRequest, true);

            % Create GridLayout
            app.GridLayout = uigridlayout(app.MaskColorEditorGUI);
            app.GridLayout.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout.ColumnSpacing = 5;
            app.GridLayout.RowSpacing = 5;

            % Create UITable
            app.UITable = uitable(app.GridLayout);
            app.UITable.ColumnName = {'Mask'; 'Database'; 'Color'};
            app.UITable.RowName = {};
            app.UITable.ColumnEditable = [false true false];
            app.UITable.CellEditCallback = createCallbackFcn(app, @UITableCellEdit, true);
            app.UITable.CellSelectionCallback = createCallbackFcn(app, @UITableCellSelection, true);
            app.UITable.Layout.Row = [6 19];
            app.UITable.Layout.Column = [1 8];

            % Create Image
            app.Image = uiimage(app.GridLayout);
            app.Image.Layout.Row = [1 2];
            app.Image.Layout.Column = [1 8];
            app.Image.ImageSource = 'logo_xmap_final.png';

            % Create GridLayout2
            app.GridLayout2 = uigridlayout(app.GridLayout);
            app.GridLayout2.ColumnWidth = {'0.5x', '1x', '1x', '1x', '0.5x'};
            app.GridLayout2.RowHeight = {'1x'};
            app.GridLayout2.Padding = [10 15 10 15];
            app.GridLayout2.Layout.Row = [1 2];
            app.GridLayout2.Layout.Column = [17 24];

            % Create AppyColor
            app.AppyColor = uibutton(app.GridLayout2, 'push');
            app.AppyColor.ButtonPushedFcn = createCallbackFcn(app, @AppyColorButtonPushed, true);
            app.AppyColor.Icon = '044-repeat.png';
            app.AppyColor.FontSize = 14;
            app.AppyColor.FontWeight = 'bold';
            app.AppyColor.Layout.Row = 1;
            app.AppyColor.Layout.Column = [2 4];
            app.AppyColor.Text = 'Apply Changes';

            % Create XMapToolsColorPaletteforMineralsPanel
            app.XMapToolsColorPaletteforMineralsPanel = uipanel(app.GridLayout);
            app.XMapToolsColorPaletteforMineralsPanel.TitlePosition = 'centertop';
            app.XMapToolsColorPaletteforMineralsPanel.Title = 'XMapTools Color Palette for Minerals ';
            app.XMapToolsColorPaletteforMineralsPanel.Layout.Row = [3 5];
            app.XMapToolsColorPaletteforMineralsPanel.Layout.Column = [1 12];
            app.XMapToolsColorPaletteforMineralsPanel.FontWeight = 'bold';
            app.XMapToolsColorPaletteforMineralsPanel.FontSize = 14;

            % Create GridLayout3
            app.GridLayout3 = uigridlayout(app.XMapToolsColorPaletteforMineralsPanel);
            app.GridLayout3.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout3.RowHeight = {'0.5x', '1x'};
            app.GridLayout3.ColumnSpacing = 5;
            app.GridLayout3.RowSpacing = 5;

            % Create ResetButton
            app.ResetButton = uibutton(app.GridLayout3, 'push');
            app.ResetButton.ButtonPushedFcn = createCallbackFcn(app, @ResetButtonPushed, true);
            app.ResetButton.FontWeight = 'bold';
            app.ResetButton.Layout.Row = 2;
            app.ResetButton.Layout.Column = [1 3];
            app.ResetButton.Text = 'Reset';

            % Create ApplydefaultcolorsLabel
            app.ApplydefaultcolorsLabel = uilabel(app.GridLayout3);
            app.ApplydefaultcolorsLabel.HorizontalAlignment = 'center';
            app.ApplydefaultcolorsLabel.VerticalAlignment = 'bottom';
            app.ApplydefaultcolorsLabel.FontSize = 11;
            app.ApplydefaultcolorsLabel.FontAngle = 'italic';
            app.ApplydefaultcolorsLabel.Layout.Row = 1;
            app.ApplydefaultcolorsLabel.Layout.Column = [1 3];
            app.ApplydefaultcolorsLabel.Text = 'Apply default colors';

            % Create OtherColorPalettePanel
            app.OtherColorPalettePanel = uipanel(app.GridLayout);
            app.OtherColorPalettePanel.TitlePosition = 'centertop';
            app.OtherColorPalettePanel.Title = 'Other Color Palette';
            app.OtherColorPalettePanel.Layout.Row = [3 5];
            app.OtherColorPalettePanel.Layout.Column = [13 24];
            app.OtherColorPalettePanel.FontWeight = 'bold';
            app.OtherColorPalettePanel.FontSize = 14;

            % Create GridLayout3_2
            app.GridLayout3_2 = uigridlayout(app.OtherColorPalettePanel);
            app.GridLayout3_2.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout3_2.RowHeight = {'0.5x', '1x'};
            app.GridLayout3_2.ColumnSpacing = 5;
            app.GridLayout3_2.RowSpacing = 5;

            % Create ColorPalettesApplyButton
            app.ColorPalettesApplyButton = uibutton(app.GridLayout3_2, 'push');
            app.ColorPalettesApplyButton.ButtonPushedFcn = createCallbackFcn(app, @ColorPalettesApplyButtonPushed, true);
            app.ColorPalettesApplyButton.FontWeight = 'bold';
            app.ColorPalettesApplyButton.Layout.Row = 2;
            app.ColorPalettesApplyButton.Layout.Column = [8 9];
            app.ColorPalettesApplyButton.Text = 'Apply';

            % Create ColorPalettesDropDown
            app.ColorPalettesDropDown = uidropdown(app.GridLayout3_2);
            app.ColorPalettesDropDown.Layout.Row = 2;
            app.ColorPalettesDropDown.Layout.Column = [1 7];

            % Create SelectacolorpaletteandapplycolorsautomaticallyLabel
            app.SelectacolorpaletteandapplycolorsautomaticallyLabel = uilabel(app.GridLayout3_2);
            app.SelectacolorpaletteandapplycolorsautomaticallyLabel.HorizontalAlignment = 'center';
            app.SelectacolorpaletteandapplycolorsautomaticallyLabel.VerticalAlignment = 'bottom';
            app.SelectacolorpaletteandapplycolorsautomaticallyLabel.FontSize = 11;
            app.SelectacolorpaletteandapplycolorsautomaticallyLabel.FontAngle = 'italic';
            app.SelectacolorpaletteandapplycolorsautomaticallyLabel.Layout.Row = 1;
            app.SelectacolorpaletteandapplycolorsautomaticallyLabel.Layout.Column = [1 7];
            app.SelectacolorpaletteandapplycolorsautomaticallyLabel.Text = 'Select a color palette and apply colors automatically';

            % Create UIAxes
            app.UIAxes = uiaxes(app.GridLayout);
            app.UIAxes.XTick = [];
            app.UIAxes.YTick = [];
            app.UIAxes.Box = 'on';
            app.UIAxes.Layout.Row = [6 19];
            app.UIAxes.Layout.Column = [10 23];

            % Show the figure after all components are created
            app.MaskColorEditorGUI.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = MaskColorEditor_exported(varargin)

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.MaskColorEditorGUI)

            % Execute the startup function
            runStartupFcn(app, @(app)startupFcn(app, varargin{:}))

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.MaskColorEditorGUI)
        end
    end
end