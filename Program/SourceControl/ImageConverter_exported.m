classdef ImageConverter_exported < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        ImageConverterGUI       matlab.ui.Figure
        GridLayout              matlab.ui.container.GridLayout
        Image                   matlab.ui.control.Image
        Image2                  matlab.ui.control.Image
        Image2_2                matlab.ui.control.Image
        Image2_3                matlab.ui.control.Image
        Panel                   matlab.ui.container.Panel
        GridLayout2             matlab.ui.container.GridLayout
        Label                   matlab.ui.control.Label
        LoadImageButton         matlab.ui.control.Button
        PanelApply              matlab.ui.container.Panel
        GridLayout3             matlab.ui.container.GridLayout
        ApplyButton             matlab.ui.control.Button
        Image2_4                matlab.ui.control.Image
        MaxvalueEditField       matlab.ui.control.NumericEditField
        MaxvalueEditFieldLabel  matlab.ui.control.Label
        MinvalueEditField       matlab.ui.control.NumericEditField
        MinvalueEditFieldLabel  matlab.ui.control.Label
        SaveMapButton           matlab.ui.control.Button
        CropPanel               matlab.ui.container.Panel
        GridLayout4             matlab.ui.container.GridLayout
        XminEditField           matlab.ui.control.NumericEditField
        XminEditFieldLabel      matlab.ui.control.Label
        XmaxEditField           matlab.ui.control.NumericEditField
        XmaxEditFieldLabel      matlab.ui.control.Label
        YminEditField           matlab.ui.control.NumericEditField
        YminEditFieldLabel      matlab.ui.control.Label
        YmaxEditField           matlab.ui.control.NumericEditField
        YmaxEditFieldLabel      matlab.ui.control.Label
        CropImageButton         matlab.ui.control.Button
        UIAxes_2                matlab.ui.control.UIAxes
        UIAxes                  matlab.ui.control.UIAxes
    end

    
    properties (Access = private)
        XMapToolsApp
        WaitBar
        
        ImageRawData
        ImageConvertedData
        
        FileName
        
        Crop_ROI
        ROI_Listener
        
    end
    
    methods (Access = private)
        
        function updateplot(app)
            
            if ~isempty(app.ImageRawData)
                image(app.UIAxes,app.ImageRawData)
                axis(app.UIAxes,'image');
                
                app.UIAxes.Visible = "on";
                
                disableDefaultInteractivity(app.UIAxes);
                
                app.PanelApply.Visible = 'on';
                
                ImagePosition = [1,1,size(app.ImageRawData,2)-1,size(app.ImageRawData,1)-1];
                
                app.Crop_ROI = drawrectangle(app.UIAxes,'Position',ImagePosition,'Color',app.XMapToolsApp.GetROIColor,'InteractionsAllowed','all');
                
                UpdateCropLimits(app);
                
                app.ROI_Listener = addlistener(app.Crop_ROI, 'MovingROI', @(varargin)ROI_Position_Changed(app, app.Crop_ROI));
                
            end
            
            if ~isempty(app.ImageConvertedData)
                imagesc(app.UIAxes_2,app.ImageConvertedData)
                axis(app.UIAxes_2,'image');
                if size(app.ImageConvertedData,2) > 1.2*size(app.ImageConvertedData,1)
                    colorbar(app.UIAxes_2,'horizontal');
                else
                    colorbar(app.UIAxes_2,'vertical');
                end
                colormap(app.UIAxes_2,app.XMapToolsApp.ColorMapValues_noMask);
                
                app.UIAxes_2.Visible = "on";
                
                disableDefaultInteractivity(app.UIAxes_2);
                
                app.SaveMapButton.Visible = 'on';
                
            end
            
        end
        
        function ApplyConversion(app)
            
            B = im2double(app.ImageRawData);
            
            if size(B,3) > 1
                B = rgb2gray(B);
            end
            
            Min = app.MinvalueEditField.Value;
            Max = app.MaxvalueEditField.Value;
            
            app.ImageConvertedData = Min + B.*(Max-Min);
            
            updateplot(app)
        end
        
        function UpdateCropLimits(app)
            
            app.XminEditField.Value = round(app.Crop_ROI.Position(1));
            app.XmaxEditField.Value = round(app.Crop_ROI.Position(3));
            
            app.YminEditField.Value = round(app.Crop_ROI.Position(2));
            app.YmaxEditField.Value = round(app.Crop_ROI.Position(4));
            
        end
        
        function ROI_Position_Changed(app, ~)
            UpdateCropLimits(app);
        end
        
        function AdjustROI(app)
            
            app.Crop_ROI.Position(1) = app.XminEditField.Value;
            app.Crop_ROI.Position(3) = app.XmaxEditField.Value;
            
            app.Crop_ROI.Position(2) = app.YminEditField.Value;
            app.Crop_ROI.Position(4) = app.YmaxEditField.Value;
            
        end
    end
    

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, XMapToolsApp)
            
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
            
            
            app.ImageConverterGUI.Visible = 'off';
            
            movegui(app.ImageConverterGUI,'center');
            
            app.XMapToolsApp = XMapToolsApp;
            
            app.UIAxes.Visible = 'off';
            app.UIAxes_2.Visible = 'off';
            app.PanelApply.Visible = 'off';
            app.SaveMapButton.Visible = 'off';
            app.CropPanel.Visible = 'off';
            
            app.ImageRawData = [];
            app.ImageConvertedData = [];
            
            app.ImageConverterGUI.Visible = 'on';
        end

        % Button pushed function: LoadImageButton
        function LoadImageButtonPushed(app, event)
            
            app.WaitBar = uiprogressdlg(app.ImageConverterGUI,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Pick an image file';
            
            f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
            [FileName,PathName] = uigetfile({'*.bmp;*.tif;*.tiff;*.png;*.jpg;*.jpeg','Image (*.bmp, *.tif, *.tiff, *.png, *.jpg, *,jpeg)';'*.*',  'All Files (*.*)'},'Pick a file');
            close(f);
            figure(app.ImageConverterGUI)
            if isempty(FileName)
                close(app.WaitBar);
                return
            end
            
            app.WaitBar.Message = 'Loading the image';
            
            app.ImageRawData = imread([PathName,FileName]);
            
            app.FileName = FileName;
            
            updateplot(app)
            
            app.CropPanel.Visible = 'on';
            
            close(app.WaitBar)
            
        end

        % Button pushed function: ApplyButton
        function ApplyButtonPushed(app, event)
            ApplyConversion(app);
            
        end

        % Value changed function: MinvalueEditField
        function MinvalueEditFieldValueChanged(app, event)
            if app.MinvalueEditField.Value >= app.MaxvalueEditField.Value
                app.MinvalueEditField.Value = app.MaxvalueEditField.Value - 1;
            end
            ApplyConversion(app);
        end

        % Value changed function: MaxvalueEditField
        function MaxvalueEditFieldValueChanged(app, event)
            if app.MinvalueEditField.Value >= app.MaxvalueEditField.Value
                app.MinvalueEditField.Value = app.MaxvalueEditField.Value - 1;
            end
            ApplyConversion(app);
        end

        % Button pushed function: SaveMapButton
        function SaveMapButtonPushed(app, event)
            
            app.WaitBar = uiprogressdlg(app.ImageConverterGUI,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Save the file as...';
            
            NameStr = strread(app.FileName,'%s','delimiter','.');
            NameFileOri = char(NameStr{1});
            
            f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
            [FileName,PathName] = uiputfile([NameFileOri,'.txt'], 'Save the map as...');
            close(f);
            figure(app.ImageConverterGUI)
            
            if isempty(FileName) || isequal(FileName,0)
                close(app.WaitBar);
                return
            end
            
            app.WaitBar.Message = 'Saving the file';
            
            B = app.ImageConvertedData;
            
            save([PathName,FileName],'B','-ASCII');
            
            close(app.WaitBar)
            
        end

        % Button pushed function: CropImageButton
        function CropImageButtonPushed(app, event)
            
            app.ImageRawData = app.ImageRawData(app.YminEditField.Value:app.YmaxEditField.Value,app.XminEditField.Value:app.XmaxEditField.Value,:);
            
            updateplot(app);
            
        end

        % Value changed function: XminEditField
        function XminEditFieldValueChanged(app, event)
            AdjustROI(app);
        end

        % Value changed function: XmaxEditField
        function XmaxEditFieldValueChanged(app, event)
            AdjustROI(app);
        end

        % Value changed function: YminEditField
        function YminEditFieldValueChanged(app, event)
            AdjustROI(app);
        end

        % Value changed function: YmaxEditField
        function YmaxEditFieldValueChanged(app, event)
            AdjustROI(app);
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create ImageConverterGUI and hide until all components are created
            app.ImageConverterGUI = uifigure('Visible', 'off');
            app.ImageConverterGUI.Position = [100 100 967 561];
            app.ImageConverterGUI.Name = 'Image Converter – XMapTools';

            % Create GridLayout
            app.GridLayout = uigridlayout(app.ImageConverterGUI);
            app.GridLayout.ColumnWidth = {'0.5x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.5x'};
            app.GridLayout.RowHeight = {'0.1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.2x'};

            % Create Image
            app.Image = uiimage(app.GridLayout);
            app.Image.Layout.Row = [2 3];
            app.Image.Layout.Column = [2 9];
            app.Image.ImageSource = 'logo_xmap_final.png';

            % Create Image2
            app.Image2 = uiimage(app.GridLayout);
            app.Image2.Layout.Row = [2 3];
            app.Image2.Layout.Column = [17 18];
            app.Image2.ImageSource = 'IMGConv_image.png';

            % Create Image2_2
            app.Image2_2 = uiimage(app.GridLayout);
            app.Image2_2.Layout.Row = [2 3];
            app.Image2_2.Layout.Column = 19;
            app.Image2_2.ImageSource = 'IMGConv_arrow.png';

            % Create Image2_3
            app.Image2_3 = uiimage(app.GridLayout);
            app.Image2_3.Layout.Row = [2 3];
            app.Image2_3.Layout.Column = [20 21];
            app.Image2_3.ImageSource = 'IMGConv_text.png';

            % Create Panel
            app.Panel = uipanel(app.GridLayout);
            app.Panel.Layout.Row = [2 3];
            app.Panel.Layout.Column = [11 15];

            % Create GridLayout2
            app.GridLayout2 = uigridlayout(app.Panel);
            app.GridLayout2.ColumnWidth = {'1x'};
            app.GridLayout2.RowHeight = {'1x'};
            app.GridLayout2.Padding = [5 5 5 5];

            % Create Label
            app.Label = uilabel(app.GridLayout2);
            app.Label.WordWrap = 'on';
            app.Label.FontSize = 9;
            app.Label.Layout.Row = 1;
            app.Label.Layout.Column = 1;
            app.Label.Text = 'ImageConverter is a module that converts images into numerical values. Load an image file (.bmp, .tif, .tiff, .png, .jpg, .jpeg), check the conversion and save your file to a text file.';

            % Create LoadImageButton
            app.LoadImageButton = uibutton(app.GridLayout, 'push');
            app.LoadImageButton.ButtonPushedFcn = createCallbackFcn(app, @LoadImageButtonPushed, true);
            app.LoadImageButton.Icon = '056-plus.png';
            app.LoadImageButton.Layout.Row = 5;
            app.LoadImageButton.Layout.Column = [3 5];
            app.LoadImageButton.Text = 'Load Image';

            % Create PanelApply
            app.PanelApply = uipanel(app.GridLayout);
            app.PanelApply.BorderType = 'none';
            app.PanelApply.Layout.Row = [7 12];
            app.PanelApply.Layout.Column = [10 13];

            % Create GridLayout3
            app.GridLayout3 = uigridlayout(app.PanelApply);
            app.GridLayout3.ColumnWidth = {'1x', '1x', '1x', '1x'};
            app.GridLayout3.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout3.Padding = [0 0 0 0];

            % Create ApplyButton
            app.ApplyButton = uibutton(app.GridLayout3, 'push');
            app.ApplyButton.ButtonPushedFcn = createCallbackFcn(app, @ApplyButtonPushed, true);
            app.ApplyButton.Icon = '044-repeat.png';
            app.ApplyButton.Layout.Row = 1;
            app.ApplyButton.Layout.Column = [2 3];
            app.ApplyButton.Text = 'Apply';

            % Create Image2_4
            app.Image2_4 = uiimage(app.GridLayout3);
            app.Image2_4.Layout.Row = [4 5];
            app.Image2_4.Layout.Column = [2 3];
            app.Image2_4.ImageSource = 'IMGConv_arrow.png';

            % Create MaxvalueEditField
            app.MaxvalueEditField = uieditfield(app.GridLayout3, 'numeric');
            app.MaxvalueEditField.ValueChangedFcn = createCallbackFcn(app, @MaxvalueEditFieldValueChanged, true);
            app.MaxvalueEditField.FontSize = 10;
            app.MaxvalueEditField.Layout.Row = 3;
            app.MaxvalueEditField.Layout.Column = 3;
            app.MaxvalueEditField.Value = 1000;

            % Create MaxvalueEditFieldLabel
            app.MaxvalueEditFieldLabel = uilabel(app.GridLayout3);
            app.MaxvalueEditFieldLabel.HorizontalAlignment = 'right';
            app.MaxvalueEditFieldLabel.FontSize = 11;
            app.MaxvalueEditFieldLabel.Layout.Row = 3;
            app.MaxvalueEditFieldLabel.Layout.Column = [1 2];
            app.MaxvalueEditFieldLabel.Text = 'Max value';

            % Create MinvalueEditField
            app.MinvalueEditField = uieditfield(app.GridLayout3, 'numeric');
            app.MinvalueEditField.ValueChangedFcn = createCallbackFcn(app, @MinvalueEditFieldValueChanged, true);
            app.MinvalueEditField.FontSize = 10;
            app.MinvalueEditField.Layout.Row = 2;
            app.MinvalueEditField.Layout.Column = 3;

            % Create MinvalueEditFieldLabel
            app.MinvalueEditFieldLabel = uilabel(app.GridLayout3);
            app.MinvalueEditFieldLabel.HorizontalAlignment = 'right';
            app.MinvalueEditFieldLabel.FontSize = 11;
            app.MinvalueEditFieldLabel.Layout.Row = 2;
            app.MinvalueEditFieldLabel.Layout.Column = [1 2];
            app.MinvalueEditFieldLabel.Text = 'Min value';

            % Create SaveMapButton
            app.SaveMapButton = uibutton(app.GridLayout, 'push');
            app.SaveMapButton.ButtonPushedFcn = createCallbackFcn(app, @SaveMapButtonPushed, true);
            app.SaveMapButton.Icon = '096-upload.png';
            app.SaveMapButton.Layout.Row = 5;
            app.SaveMapButton.Layout.Column = [18 20];
            app.SaveMapButton.Text = 'Save Map';

            % Create CropPanel
            app.CropPanel = uipanel(app.GridLayout);
            app.CropPanel.TitlePosition = 'centertop';
            app.CropPanel.Title = 'Crop';
            app.CropPanel.Layout.Row = [4 5];
            app.CropPanel.Layout.Column = [9 15];
            app.CropPanel.FontSize = 10;

            % Create GridLayout4
            app.GridLayout4 = uigridlayout(app.CropPanel);
            app.GridLayout4.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout4.Padding = [5 5 5 5];

            % Create XminEditField
            app.XminEditField = uieditfield(app.GridLayout4, 'numeric');
            app.XminEditField.ValueChangedFcn = createCallbackFcn(app, @XminEditFieldValueChanged, true);
            app.XminEditField.FontSize = 10;
            app.XminEditField.Layout.Row = 1;
            app.XminEditField.Layout.Column = 2;

            % Create XminEditFieldLabel
            app.XminEditFieldLabel = uilabel(app.GridLayout4);
            app.XminEditFieldLabel.HorizontalAlignment = 'right';
            app.XminEditFieldLabel.FontSize = 11;
            app.XminEditFieldLabel.Layout.Row = 1;
            app.XminEditFieldLabel.Layout.Column = 1;
            app.XminEditFieldLabel.Text = 'Xmin';

            % Create XmaxEditField
            app.XmaxEditField = uieditfield(app.GridLayout4, 'numeric');
            app.XmaxEditField.ValueChangedFcn = createCallbackFcn(app, @XmaxEditFieldValueChanged, true);
            app.XmaxEditField.FontSize = 10;
            app.XmaxEditField.Layout.Row = 2;
            app.XmaxEditField.Layout.Column = 2;

            % Create XmaxEditFieldLabel
            app.XmaxEditFieldLabel = uilabel(app.GridLayout4);
            app.XmaxEditFieldLabel.HorizontalAlignment = 'right';
            app.XmaxEditFieldLabel.FontSize = 11;
            app.XmaxEditFieldLabel.Layout.Row = 2;
            app.XmaxEditFieldLabel.Layout.Column = 1;
            app.XmaxEditFieldLabel.Text = 'Xmax';

            % Create YminEditField
            app.YminEditField = uieditfield(app.GridLayout4, 'numeric');
            app.YminEditField.ValueChangedFcn = createCallbackFcn(app, @YminEditFieldValueChanged, true);
            app.YminEditField.FontSize = 10;
            app.YminEditField.Layout.Row = 1;
            app.YminEditField.Layout.Column = 4;

            % Create YminEditFieldLabel
            app.YminEditFieldLabel = uilabel(app.GridLayout4);
            app.YminEditFieldLabel.HorizontalAlignment = 'right';
            app.YminEditFieldLabel.FontSize = 11;
            app.YminEditFieldLabel.Layout.Row = 1;
            app.YminEditFieldLabel.Layout.Column = 3;
            app.YminEditFieldLabel.Text = 'Ymin';

            % Create YmaxEditField
            app.YmaxEditField = uieditfield(app.GridLayout4, 'numeric');
            app.YmaxEditField.ValueChangedFcn = createCallbackFcn(app, @YmaxEditFieldValueChanged, true);
            app.YmaxEditField.FontSize = 10;
            app.YmaxEditField.Layout.Row = 2;
            app.YmaxEditField.Layout.Column = 4;

            % Create YmaxEditFieldLabel
            app.YmaxEditFieldLabel = uilabel(app.GridLayout4);
            app.YmaxEditFieldLabel.HorizontalAlignment = 'right';
            app.YmaxEditFieldLabel.FontSize = 11;
            app.YmaxEditFieldLabel.Layout.Row = 2;
            app.YmaxEditFieldLabel.Layout.Column = 3;
            app.YmaxEditFieldLabel.Text = 'Ymax';

            % Create CropImageButton
            app.CropImageButton = uibutton(app.GridLayout4, 'push');
            app.CropImageButton.ButtonPushedFcn = createCallbackFcn(app, @CropImageButtonPushed, true);
            app.CropImageButton.Icon = 'XXX_cut_round.png';
            app.CropImageButton.IconAlignment = 'top';
            app.CropImageButton.Layout.Row = [1 2];
            app.CropImageButton.Layout.Column = [5 6];
            app.CropImageButton.Text = 'Crop Image';

            % Create UIAxes_2
            app.UIAxes_2 = uiaxes(app.GridLayout);
            app.UIAxes_2.XTick = [];
            app.UIAxes_2.YTick = [];
            app.UIAxes_2.Layout.Row = [6 13];
            app.UIAxes_2.Layout.Column = [14 21];

            % Create UIAxes
            app.UIAxes = uiaxes(app.GridLayout);
            app.UIAxes.XTick = [];
            app.UIAxes.YTick = [];
            app.UIAxes.Layout.Row = [6 13];
            app.UIAxes.Layout.Column = [2 9];

            % Show the figure after all components are created
            app.ImageConverterGUI.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = ImageConverter_exported(varargin)

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.ImageConverterGUI)

            % Execute the startup function
            runStartupFcn(app, @(app)startupFcn(app, varargin{:}))

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.ImageConverterGUI)
        end
    end
end