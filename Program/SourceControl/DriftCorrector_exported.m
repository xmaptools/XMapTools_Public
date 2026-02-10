classdef DriftCorrector_exported < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        DriftCorrectorGUI              matlab.ui.Figure
        GridLayout                     matlab.ui.container.GridLayout
        Image                          matlab.ui.control.Image
        GridLayout2                    matlab.ui.container.GridLayout
        AppyCorrection                 matlab.ui.control.Button
        MaskSelectionOptionsPanel      matlab.ui.container.Panel
        GridLayout3                    matlab.ui.container.GridLayout
        DropDownMask                   matlab.ui.control.DropDown
        MaskLabel                      matlab.ui.control.Label
        FilterCheckBox                 matlab.ui.control.CheckBox
        DropDownFilter                 matlab.ui.control.DropDown
        CorrectionschemePanel          matlab.ui.container.Panel
        GridLayout3_2                  matlab.ui.container.GridLayout
        CorrectionLabel                matlab.ui.control.Label
        DropDownCorrection             matlab.ui.control.DropDown
        AddROIButton                   matlab.ui.control.Button
        ApplyAutoButton                matlab.ui.control.StateButton
        ResolutionValuePxInput         matlab.ui.control.NumericEditField
        RespixelsLabel                 matlab.ui.control.Label
        DisplaytheCorrectionMapButton  matlab.ui.control.Button
        Image2                         matlab.ui.control.Image
        AutoContrast_Button            matlab.ui.control.Button
        MaxEditField                   matlab.ui.control.NumericEditField
        MinEditField                   matlab.ui.control.NumericEditField
        MinLabel                       matlab.ui.control.Label
        MaxLabel                       matlab.ui.control.Label
        UIAxes                         matlab.ui.control.UIAxes
        UIAxes2                        matlab.ui.control.UIAxes
        UIAxes3                        matlab.ui.control.UIAxes
    end

    
    properties (Access = private)
        XMapToolsApp
        MaskId
        MapID
        Mask
        Map
        
        
        Data4Correction 
        DataCorrected 
        BRC 
        
        CorrectionMapVq
    end
    
    methods (Access = private)
        
        function PlotFunction(app)
            
            app.DisplaytheCorrectionMapButton.Visible = 'off';
            
            Data2Plot = app.Map;
            
            ValueMask = app.DropDownMask.Value;
            
            if ValueMask
                UnselectedPx = find(app.Mask.MaskMap ~= ValueMask);
                Data2Plot(UnselectedPx) = 0;
            end
            
            if app.FilterCheckBox.Value
                Data2Plot = Data2Plot .* app.BRC;
            end
            
            imagesc(app.UIAxes,Data2Plot)
            axis(app.UIAxes,'image')
            colorbar(app.UIAxes)
            colormap(app.UIAxes,app.XMapToolsApp.ColorMapValues)
            
            % keyboard
            
            if app.DropDownCorrection.Value && isequal(app.ApplyAutoButton.Value,1)
                % We calculate a correction
                app.Data4Correction = Data2Plot;
                
                ApplySelectedDataCorrection(app)
                
            end
            
            if app.DropDownCorrection.Value
                Corrected = app.DataCorrected;

                if ValueMask
                    UnselectedPx = find(app.Mask.MaskMap ~= ValueMask);
                    Corrected(UnselectedPx) = 0;
                end
                
                if app.FilterCheckBox.Value
                    Corrected = Corrected .* app.BRC;
                end
                
                imagesc(app.UIAxes2,Corrected)
                axis(app.UIAxes2,'image')
                colorbar(app.UIAxes2)
                colormap(app.UIAxes2,app.XMapToolsApp.ColorMapValues)
                
                app.DisplaytheCorrectionMapButton.Visible = 'on';
                app.AppyCorrection.Enable = 'on';
            else
                app.DisplaytheCorrectionMapButton.Visible = 'off';
                app.AppyCorrection.Enable = 'off';
            end
            
            DataPlot = ExtractAllDataValuesFromPlot(app);
            
            app.MinEditField.Value = min(DataPlot);
            app.MaxEditField.Value = max(DataPlot);
            
            caxis(app.UIAxes,[app.MinEditField.Value,app.MaxEditField.Value]);
            
            if app.DropDownCorrection.Value
                caxis(app.UIAxes2,[app.MinEditField.Value,app.MaxEditField.Value]);
            end
            
        end
        
        function AdjustMinMax(app,Mode)
            
            ImageData = ExtractAllDataValuesFromPlot(app);          % extract ImageData from the figure
            ImageData = ImageData(find(ImageData));                 % exclude zeros (new 4.0.0)
            IdxNotNan = find(~isnan(ImageData));
            if length(IdxNotNan) > 0
                ImageData = ImageData(IdxNotNan);
            end
            SortedData = sort(ImageData(:));
            SelCrit = round(numel(SortedData) * 0.065);
            
            Min1 = SortedData(SelCrit);
            Max1 = SortedData(end-SelCrit);
            
            SortedData2 = SortedData(SelCrit:end-SelCrit);
            SelCrit2 = round(numel(SortedData2) * 0.065);
            
            Min2 = SortedData2(SelCrit2);
            Max2 = SortedData2(end-SelCrit2);
            
            %             Q = quantile(SortedData,12);
            %             Min1 = Q(1);
            %             Max1 = Q(10);
            %             Min2 = Q(2);
            %             Max2 = Q(9);
            
            % Current values:
            [Min,Max] = caxis(app.UIAxes);
            
            % Check values:
            if isequal(Min,Min1) && isequal(Max,Max1)
                Min = Min2;
                Max = Max2;
            elseif isequal(Min,Min2) && isequal(Max,Max2)
                Min = min(ImageData(:));
                Max = max(ImageData(:));
            else
                Min = Min1;
                Max = Max1;
            end
            
            app.MinEditField.Value = Min;
            app.MaxEditField.Value = Max;
            
            caxis(app.UIAxes,[app.MinEditField.Value,app.MaxEditField.Value]);
            if app.DropDownCorrection.Value
                caxis(app.UIAxes2,[app.MinEditField.Value,app.MaxEditField.Value]);
            end
        end
        
        function DataPlot = ExtractAllDataValuesFromPlot(app)
            
            h1 = findall(app.UIAxes,'Type','Image');
            if ~isempty(h1)
                DataPlot = h1.CData(find(h1.CData));
            end
            
            h2 = findall(app.UIAxes2,'Type','Image');
            if ~isempty(h2)
                DataPlot = [DataPlot;h2.CData(find(h2.CData))];
            end
            
        end
        
        function ApplySelectedDataCorrection(app)
            
            Xray = app.Data4Correction;
            
            if app.FilterCheckBox.Value
                Xray = Xray .* app.BRC;
            end
            
            switch app.DropDownCorrection.Value
                
                case 1    % 2D
                    dX = 50;
                    
                    GridX = [1+dX/2:dX:size(Xray,2)-dX/2];
                    GridY = [1+dX/2:dX:size(Xray,1)-dX/2];
                    
                    Grid = nan(numel(GridY),numel(GridX));
                    
                    for i = 1:numel(GridX)
                        for j = 1:numel(GridY)
                            PxSel1 = Xray(GridY(j)-dX/2:GridY(j)+dX/2,GridX(i)-dX/2:GridX(i)+dX/2);
                            PxSel = find(PxSel1 > 0);
                            if length(PxSel) > 0.15*dX^2   % at least 5 % of the pixels
                                Grid(j,i) = median(PxSel1(PxSel));
                            end
                        end
                    end
                    
                    Xq = [1:size(Xray,2)];
                    Yq = [1:size(Xray,1)];
                    
                    [X,Y] = meshgrid(GridX,GridY);
                    
                    Where = find(Grid > 1e-4);
                    F = scatteredInterpolant(X(Where), Y(Where), Grid(Where));
                    
                    [xq,yq] = meshgrid(Xq,Yq);
                    Vq = F(xq,yq);
                    
                    app.CorrectionMapVq = Vq;
                    
                    app.DataCorrected = ApplyCorrection2Map(app,Vq,app.Map);
                    
            end
            
        end
        
        function CalculateBRC(app)
            
            TheNbPx = 3;
            TheNbPxOnGarde = 80;
            
            
            % Proceed to the correction
            TheLin = size(app.Mask.MaskMap,1);
            TheCol = size(app.Mask.MaskMap,2);
            %CoordMatrice = reshape([1:TheLin*TheCol],TheLin,TheCol);
            
            TheMaskFinal = zeros(size(app.Mask.MaskMap));
            
            Position = round(TheNbPx/2);
            TheNbPxInSel = TheNbPx^2;
            TheCriterion = TheNbPxInSel*TheNbPxOnGarde/100;
            
            for i=1:length(app.Mask.Names) - 1                % for each phase
                
                TheMask = zeros(size(app.Mask.MaskMap));
                VectorOk = find(app.Mask.MaskMap == i);
                
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
        
        function DataCorrected = ApplyCorrection2Map(app,Vq,Map)
            
            [Value,IndinInd] =  max(Vq(:));
            
            CorrectionMatrix = Value-Vq;
            CorrectionMatrixPer = CorrectionMatrix./Vq;
            
            DataCorrected = Map + CorrectionMatrix;
            
            % Apply a new correction to keep the same average
            MeanInit = mean(Map(:));
            newMean = mean(DataCorrected(:));
            
            Delta = newMean-MeanInit;
            
            DataCorrected = DataCorrected - Delta;
            
        end
    end
    

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, XMapToolsApp, MapID, MaskId)
            
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
            
            app.DriftCorrectorGUI.Visible = 'off';
            
            movegui(app.DriftCorrectorGUI,'center');
            
            app.XMapToolsApp = XMapToolsApp;
            app.MaskId = MaskId;
            app.MapID = MapID;
            
            app.Mask = app.XMapToolsApp.XMapToolsData.MapData.MaskFile.Masks(MaskId);
            
            app.Map = app.XMapToolsApp.XMapToolsData.MapData.It.Data(MapID).Map;
            
            app.AppyCorrection.Enable = 'off';
            
            app.DropDownMask.Items = app.Mask.Names;
            app.DropDownMask.ItemsData = [0:length(app.Mask.Names)-1];
            
            CalculateBRC(app);
            
            PlotFunction(app);
            
            app.DriftCorrectorGUI.Visible = 'on';
            
            DropDownCorrectionValueChanged(app);
            %keyboard
            
        end

        % Value changed function: DropDownMask
        function DropDownMaskValueChanged(app, event)
            PlotFunction(app);
            
        end

        % Value changed function: ApplyAutoButton
        function ApplyAutoButtonValueChanged(app, event)
            PlotFunction(app);
            
        end

        % Button pushed function: AddROIButton
        function AddROIButtonPushed(app, event)
            
        end

        % Value changed function: DropDownCorrection
        function DropDownCorrectionValueChanged(app, event)
            
            app.AddROIButton.Visible = 'off';
            app.ResolutionValuePxInput.Visible = 'off';
            app.RespixelsLabel.Visible = 'off';
            
            switch app.DropDownCorrection.Value
                case 1
                    app.ResolutionValuePxInput.Visible = 'on';
                    app.RespixelsLabel.Visible = 'on';
                    
                case 2
                    app.AddROIButton.Visible = 'on';
                    
                case 3
                    app.AddROIButton.Visible = 'on';
            end
            
            
            PlotFunction(app);
            
        end

        % Value changed function: MinEditField
        function MinEditFieldValueChanged(app, event)
            AdjustMinMax(app,'auto');
        end

        % Value changed function: MaxEditField
        function MaxEditFieldValueChanged(app, event)
            AdjustMinMax(app,'auto');
        end

        % Button pushed function: AutoContrast_Button
        function AutoContrast_ButtonPushed(app, event)
            AdjustMinMax(app,'magic');
        end

        % Value changed function: FilterCheckBox
        function FilterCheckBoxValueChanged(app, event)
            PlotFunction(app);
        end

        % Value changed function: DropDownFilter
        function DropDownFilterValueChanged(app, event)
            PlotFunction(app);
        end

        % Button pushed function: DisplaytheCorrectionMapButton
        function DisplaytheCorrectionMapButtonPushed(app, event)
            
            
            [Value,IndinInd] =  max(app.CorrectionMapVq(:));
            
            CorrectionMatrix = Value-app.CorrectionMapVq;
            CorrectionMatrixPer = CorrectionMatrix./app.CorrectionMapVq;
            
            figure, 
            tiledlayout('flow')
            
            nexttile
            imagesc(CorrectionMatrix), axis image, title('Correction matrix')
            colormap(app.XMapToolsApp.ColorMapValues)
            
            nexttile
            imagesc(CorrectionMatrix), axis image, title('Correction matrix (in %)')
            colormap(app.XMapToolsApp.ColorMapValues)
            
            
        end

        % Value changed function: ResolutionValuePxInput
        function ResolutionValuePxInputValueChanged(app, event)
            PlotFunction(app);
        end

        % Button pushed function: AppyCorrection
        function AppyCorrectionButtonPushed(app, event)
            
            % Apply the correction to the entire map:
            
            CorrectedMap = ApplyCorrection2Map(app, app.CorrectionMapVq, app.Map);
            
            app.XMapToolsApp.XMapToolsData.MapData.It.Data(app.MapID).Map = CorrectedMap;
            
            close(app.DriftCorrectorGUI)
            
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create DriftCorrectorGUI and hide until all components are created
            app.DriftCorrectorGUI = uifigure('Visible', 'off');
            app.DriftCorrectorGUI.Position = [100 100 1152 796];
            app.DriftCorrectorGUI.Name = 'Drift Correction Module  – XMapTools';

            % Create GridLayout
            app.GridLayout = uigridlayout(app.DriftCorrectorGUI);
            app.GridLayout.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout.ColumnSpacing = 5;
            app.GridLayout.RowSpacing = 5;

            % Create Image
            app.Image = uiimage(app.GridLayout);
            app.Image.Layout.Row = [1 3];
            app.Image.Layout.Column = [1 11];
            app.Image.ImageSource = 'logo_xmap_final.png';

            % Create GridLayout2
            app.GridLayout2 = uigridlayout(app.GridLayout);
            app.GridLayout2.ColumnWidth = {'0.5x', '1x', '1x', '1x', '0.5x'};
            app.GridLayout2.RowHeight = {'1x'};
            app.GridLayout2.Padding = [10 25 10 25];
            app.GridLayout2.Layout.Row = [1 3];
            app.GridLayout2.Layout.Column = [26 35];

            % Create AppyCorrection
            app.AppyCorrection = uibutton(app.GridLayout2, 'push');
            app.AppyCorrection.ButtonPushedFcn = createCallbackFcn(app, @AppyCorrectionButtonPushed, true);
            app.AppyCorrection.Icon = '044-repeat.png';
            app.AppyCorrection.FontSize = 14;
            app.AppyCorrection.FontWeight = 'bold';
            app.AppyCorrection.Layout.Row = 1;
            app.AppyCorrection.Layout.Column = [2 4];
            app.AppyCorrection.Text = 'Apply Changes';

            % Create MaskSelectionOptionsPanel
            app.MaskSelectionOptionsPanel = uipanel(app.GridLayout);
            app.MaskSelectionOptionsPanel.TitlePosition = 'centertop';
            app.MaskSelectionOptionsPanel.Title = 'Mask Selection & Options';
            app.MaskSelectionOptionsPanel.Layout.Row = [4 6];
            app.MaskSelectionOptionsPanel.Layout.Column = [1 12];
            app.MaskSelectionOptionsPanel.FontWeight = 'bold';
            app.MaskSelectionOptionsPanel.FontSize = 14;

            % Create GridLayout3
            app.GridLayout3 = uigridlayout(app.MaskSelectionOptionsPanel);
            app.GridLayout3.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout3.RowHeight = {'0.5x', '1x'};
            app.GridLayout3.ColumnSpacing = 5;
            app.GridLayout3.RowSpacing = 5;

            % Create DropDownMask
            app.DropDownMask = uidropdown(app.GridLayout3);
            app.DropDownMask.ValueChangedFcn = createCallbackFcn(app, @DropDownMaskValueChanged, true);
            app.DropDownMask.Layout.Row = 2;
            app.DropDownMask.Layout.Column = [1 4];

            % Create MaskLabel
            app.MaskLabel = uilabel(app.GridLayout3);
            app.MaskLabel.HorizontalAlignment = 'center';
            app.MaskLabel.VerticalAlignment = 'bottom';
            app.MaskLabel.FontSize = 11;
            app.MaskLabel.FontAngle = 'italic';
            app.MaskLabel.Layout.Row = 1;
            app.MaskLabel.Layout.Column = [1 4];
            app.MaskLabel.Text = 'Mask';

            % Create FilterCheckBox
            app.FilterCheckBox = uicheckbox(app.GridLayout3);
            app.FilterCheckBox.ValueChangedFcn = createCallbackFcn(app, @FilterCheckBoxValueChanged, true);
            app.FilterCheckBox.Text = 'Filter';
            app.FilterCheckBox.Layout.Row = 2;
            app.FilterCheckBox.Layout.Column = [5 6];

            % Create DropDownFilter
            app.DropDownFilter = uidropdown(app.GridLayout3);
            app.DropDownFilter.Items = {'BRC (3,80)'};
            app.DropDownFilter.ItemsData = 1;
            app.DropDownFilter.ValueChangedFcn = createCallbackFcn(app, @DropDownFilterValueChanged, true);
            app.DropDownFilter.Layout.Row = 2;
            app.DropDownFilter.Layout.Column = [7 9];
            app.DropDownFilter.Value = 1;

            % Create CorrectionschemePanel
            app.CorrectionschemePanel = uipanel(app.GridLayout);
            app.CorrectionschemePanel.TitlePosition = 'centertop';
            app.CorrectionschemePanel.Title = 'Correction scheme';
            app.CorrectionschemePanel.Layout.Row = [4 6];
            app.CorrectionschemePanel.Layout.Column = [13 35];
            app.CorrectionschemePanel.FontWeight = 'bold';
            app.CorrectionschemePanel.FontSize = 14;

            % Create GridLayout3_2
            app.GridLayout3_2 = uigridlayout(app.CorrectionschemePanel);
            app.GridLayout3_2.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout3_2.RowHeight = {'0.5x', '1x'};
            app.GridLayout3_2.ColumnSpacing = 5;
            app.GridLayout3_2.RowSpacing = 5;

            % Create CorrectionLabel
            app.CorrectionLabel = uilabel(app.GridLayout3_2);
            app.CorrectionLabel.HorizontalAlignment = 'center';
            app.CorrectionLabel.VerticalAlignment = 'bottom';
            app.CorrectionLabel.FontSize = 11;
            app.CorrectionLabel.FontAngle = 'italic';
            app.CorrectionLabel.Layout.Row = 1;
            app.CorrectionLabel.Layout.Column = [14 16];
            app.CorrectionLabel.Text = 'Correction';

            % Create DropDownCorrection
            app.DropDownCorrection = uidropdown(app.GridLayout3_2);
            app.DropDownCorrection.Items = {'--- Select a correction method', 'Auto 2D', 'Auto 1D (horizontal)', 'Auto 1D (vertical)'};
            app.DropDownCorrection.ItemsData = [0 1 2 3];
            app.DropDownCorrection.ValueChangedFcn = createCallbackFcn(app, @DropDownCorrectionValueChanged, true);
            app.DropDownCorrection.Layout.Row = 2;
            app.DropDownCorrection.Layout.Column = [1 7];
            app.DropDownCorrection.Value = 0;

            % Create AddROIButton
            app.AddROIButton = uibutton(app.GridLayout3_2, 'push');
            app.AddROIButton.ButtonPushedFcn = createCallbackFcn(app, @AddROIButtonPushed, true);
            app.AddROIButton.FontWeight = 'bold';
            app.AddROIButton.Layout.Row = 2;
            app.AddROIButton.Layout.Column = [8 9];
            app.AddROIButton.Text = 'Add ROI';

            % Create ApplyAutoButton
            app.ApplyAutoButton = uibutton(app.GridLayout3_2, 'state');
            app.ApplyAutoButton.ValueChangedFcn = createCallbackFcn(app, @ApplyAutoButtonValueChanged, true);
            app.ApplyAutoButton.Text = 'Apply Auto';
            app.ApplyAutoButton.FontWeight = 'bold';
            app.ApplyAutoButton.Layout.Row = 2;
            app.ApplyAutoButton.Layout.Column = [14 16];
            app.ApplyAutoButton.Value = true;

            % Create ResolutionValuePxInput
            app.ResolutionValuePxInput = uieditfield(app.GridLayout3_2, 'numeric');
            app.ResolutionValuePxInput.Limits = [2 1000];
            app.ResolutionValuePxInput.RoundFractionalValues = 'on';
            app.ResolutionValuePxInput.ValueDisplayFormat = '%.0f';
            app.ResolutionValuePxInput.ValueChangedFcn = createCallbackFcn(app, @ResolutionValuePxInputValueChanged, true);
            app.ResolutionValuePxInput.HorizontalAlignment = 'center';
            app.ResolutionValuePxInput.Layout.Row = 2;
            app.ResolutionValuePxInput.Layout.Column = [10 11];
            app.ResolutionValuePxInput.Value = 50;

            % Create RespixelsLabel
            app.RespixelsLabel = uilabel(app.GridLayout3_2);
            app.RespixelsLabel.HorizontalAlignment = 'center';
            app.RespixelsLabel.VerticalAlignment = 'bottom';
            app.RespixelsLabel.FontSize = 11;
            app.RespixelsLabel.FontAngle = 'italic';
            app.RespixelsLabel.Layout.Row = 1;
            app.RespixelsLabel.Layout.Column = [10 11];
            app.RespixelsLabel.Text = 'Res (pixels)';

            % Create DisplaytheCorrectionMapButton
            app.DisplaytheCorrectionMapButton = uibutton(app.GridLayout3_2, 'push');
            app.DisplaytheCorrectionMapButton.ButtonPushedFcn = createCallbackFcn(app, @DisplaytheCorrectionMapButtonPushed, true);
            app.DisplaytheCorrectionMapButton.FontSize = 9;
            app.DisplaytheCorrectionMapButton.FontWeight = 'bold';
            app.DisplaytheCorrectionMapButton.Layout.Row = 2;
            app.DisplaytheCorrectionMapButton.Layout.Column = [12 13];
            app.DisplaytheCorrectionMapButton.Text = {'Display the'; 'Correction Map'};

            % Create Image2
            app.Image2 = uiimage(app.GridLayout);
            app.Image2.Layout.Row = [19 21];
            app.Image2.Layout.Column = [17 19];
            app.Image2.ImageSource = 'Arrow.png';

            % Create AutoContrast_Button
            app.AutoContrast_Button = uibutton(app.GridLayout, 'push');
            app.AutoContrast_Button.ButtonPushedFcn = createCallbackFcn(app, @AutoContrast_ButtonPushed, true);
            app.AutoContrast_Button.Icon = 'XXX_magic-wand.png';
            app.AutoContrast_Button.Layout.Row = 18;
            app.AutoContrast_Button.Layout.Column = 18;
            app.AutoContrast_Button.Text = '';

            % Create MaxEditField
            app.MaxEditField = uieditfield(app.GridLayout, 'numeric');
            app.MaxEditField.ValueChangedFcn = createCallbackFcn(app, @MaxEditFieldValueChanged, true);
            app.MaxEditField.HorizontalAlignment = 'center';
            app.MaxEditField.Layout.Row = 23;
            app.MaxEditField.Layout.Column = [18 19];

            % Create MinEditField
            app.MinEditField = uieditfield(app.GridLayout, 'numeric');
            app.MinEditField.ValueChangedFcn = createCallbackFcn(app, @MinEditFieldValueChanged, true);
            app.MinEditField.HorizontalAlignment = 'center';
            app.MinEditField.Layout.Row = 22;
            app.MinEditField.Layout.Column = [17 18];

            % Create MinLabel
            app.MinLabel = uilabel(app.GridLayout);
            app.MinLabel.FontSize = 11;
            app.MinLabel.FontAngle = 'italic';
            app.MinLabel.Layout.Row = 22;
            app.MinLabel.Layout.Column = [19 20];
            app.MinLabel.Text = 'Min';

            % Create MaxLabel
            app.MaxLabel = uilabel(app.GridLayout);
            app.MaxLabel.HorizontalAlignment = 'right';
            app.MaxLabel.FontSize = 11;
            app.MaxLabel.FontAngle = 'italic';
            app.MaxLabel.Layout.Row = 23;
            app.MaxLabel.Layout.Column = [16 17];
            app.MaxLabel.Text = 'Max';

            % Create UIAxes
            app.UIAxes = uiaxes(app.GridLayout);
            app.UIAxes.XTick = [];
            app.UIAxes.YTick = [];
            app.UIAxes.FontSize = 9;
            app.UIAxes.Box = 'on';
            app.UIAxes.Layout.Row = [16 24];
            app.UIAxes.Layout.Column = [1 15];

            % Create UIAxes2
            app.UIAxes2 = uiaxes(app.GridLayout);
            app.UIAxes2.XTick = [];
            app.UIAxes2.YTick = [];
            app.UIAxes2.FontSize = 9;
            app.UIAxes2.Box = 'on';
            app.UIAxes2.Layout.Row = [16 24];
            app.UIAxes2.Layout.Column = [21 35];

            % Create UIAxes3
            app.UIAxes3 = uiaxes(app.GridLayout);
            app.UIAxes3.FontSize = 9;
            app.UIAxes3.Layout.Row = [7 15];
            app.UIAxes3.Layout.Column = [1 35];

            % Show the figure after all components are created
            app.DriftCorrectorGUI.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = DriftCorrector_exported(varargin)

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.DriftCorrectorGUI)

            % Execute the startup function
            runStartupFcn(app, @(app)startupFcn(app, varargin{:}))

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.DriftCorrectorGUI)
        end
    end
end