classdef Spider_exported < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        Spider_GUI                 matlab.ui.Figure
        GridLayout                 matlab.ui.container.GridLayout
        Image                      matlab.ui.control.Image
        GridLayout2                matlab.ui.container.GridLayout
        Button_help                matlab.ui.control.Button
        UITable                    matlab.ui.control.Table
        GridLayout3                matlab.ui.container.GridLayout
        AutoContrast               matlab.ui.control.Button
        Element_Menu               matlab.ui.control.DropDown
        UncertaintyDropDown        matlab.ui.control.DropDown
        UncertaintyDropDownLabel   matlab.ui.control.Label
        DisplayToolsLabel          matlab.ui.control.Label
        GridLayout4                matlab.ui.container.GridLayout
        NormalizationSchemeLabel   matlab.ui.control.Label
        Normalization_Menu         matlab.ui.control.DropDown
        GridLayout5                matlab.ui.container.GridLayout
        Circle_Button              matlab.ui.control.Button
        Path_Button                matlab.ui.control.Button
        ColorPaletteDropDownLabel  matlab.ui.control.Label
        ColorPaletteDropDown       matlab.ui.control.DropDown
        HoldonCheckBox             matlab.ui.control.CheckBox
        ROI_Button                 matlab.ui.control.Button
        SamplingToolsLabel         matlab.ui.control.Label
        GridLayout6                matlab.ui.container.GridLayout
        UIAxes_Plot                matlab.ui.control.UIAxes
        UIAxes_Map                 matlab.ui.control.UIAxes
    end

    
    properties (Access = private)
        XMapToolsApp
        SpiderData 
        MapData 
        
        ROI_sampling
        ROI_sampling_Listener
        ROI_sampling_Listener2
        
        ROI_samplingPolyline
        ROI_samplingPolyline_Listener
    end
    
    methods (Access = private)
        
        function ReadSpiderData(app)
            
            fid = fopen('XMap_SpiderNormalizations.txt','r');
            Compt = 0;
            
            app.SpiderData.Names = {};
            app.SpiderData.Data(1).El = 'None';
            app.SpiderData.Data(1).Conc = 0;
            
            ErrorLoad = 0;
            while 1
                tline = fgetl(fid);
                
                if isequal(tline,-1)
                    break
                end
                
                if length(tline) >= 1
                    if isequal(tline(1),'>')
                        Str1 = tline(3:end);
                        Str2 = strread(fgetl(fid),'%s');
                        Str3 = strread(fgetl(fid),'%s');
                        
                        if isequal(length(Str2),length(Str3))
                            Compt = Compt+1;
                            app.SpiderData.Names{Compt} = Str1;
                            app.SpiderData.Data(Compt).El = Str2;
                            Int3 = [];
                            for i=1:length(Str3)
                                Int3(i) = str2num(Str3{i});
                            end
                            app.SpiderData.Data(Compt).Conc = Int3;
                            
                        else
                            ErrorLoad = 1;
                            break, break
                        end
                    end
                end
    
            end
            fclose(fid);
            
        end
        
        function PlotSelectedMap(app)
            
            SelMap = app.Element_Menu.Value;
            
            Map = app.MapData.CData(SelMap).Map;
            
            if isempty(app.UIAxes_Map.Children)
                imagesc(app.UIAxes_Map,Map)
                axis(app.UIAxes_Map,'image');
            else
                app.UIAxes_Map.Children(end).CData = Map;
            end
            colorbar(app.UIAxes_Map)
            
            colormap(app.UIAxes_Map,app.XMapToolsApp.ColorMapValues_noMask)
            
            tb = axtoolbar(app.UIAxes_Map,{'pan','zoomin','zoomout','restoreview'});
            disableDefaultInteractivity(app.UIAxes_Map);
            
        end
        
        function ChekMethodChanged(app)
            
            SelMeth = app.Normalization_Menu.Value;
            
            % Check if maps are available
            [Ok,Idx] = ismember(app.SpiderData.Data(SelMeth).El,app.MapData.ElNames);
            
            TableDisp = cell(length(app.SpiderData.Data(SelMeth).El),5);
            for i = 1:length(app.SpiderData.Data(SelMeth).El)
                TableDisp{i,1} = app.SpiderData.Data(SelMeth).El{i};
                TableDisp{i,2} = app.SpiderData.Data(SelMeth).Conc(i);
                TableDisp{i,3} = Ok(i);
                TableDisp{i,4} = Idx(i);
                if Ok(i)
                    TableDisp{i,5} = app.MapData.ElNames{Idx(i)};
                else
                    TableDisp{i,5} = 'not available';
                end
            end
           
            app.UITable.Data = TableDisp;
            
        end
        
        function ROI_Value_Extractor(app, ~)
            
            DeactivatePlotZoomPanOptions(app);
            
            SelMeth = app.Normalization_Menu.Value;
            
            if app.ROI_sampling(end).Nb > 1
                ColorMap = app.XMapToolsApp.CalculateColorMap(app.ColorPaletteDropDown.Value,app.ROI_sampling(end).Nb);
            else
                ColorMap = app.XMapToolsApp.GetROIColor;
            end
            
            for i = 1:app.ROI_sampling(end).Nb
                Mask = createMask(app.ROI_sampling(i).ROI);
                Ind = find(Mask);
                
                MapIdx = cell2mat(app.UITable.Data(:,4));
                
                ConcData = nan(size(MapIdx));
                ConcDataStdev = nan(size(MapIdx));
                for j = 1 :length(MapIdx)
                    if MapIdx(j) > 0
                        ConcData(j) = mean(app.MapData.CData(MapIdx(j)).Map(Ind));
                        ConcDataStdev(j) = std(app.MapData.CData(MapIdx(j)).Map(Ind));
                    end
                    
                end
                
                ConcDataStErr = ConcDataStdev./sqrt(length(Ind));
                
                Data4Plot = ConcData./app.SpiderData.Data(SelMeth).Conc';
                
                plot(app.UIAxes_Plot,Data4Plot,'-k','linewidth',2,'Color',ColorMap(i,:));
                
                app.ROI_sampling(i).ROI.Color = ColorMap(i,:);
                
                hold(app.UIAxes_Plot,'on')
                switch app.UncertaintyDropDown.Value
                    case '1 se'
                        plot(app.UIAxes_Plot,(ConcData+ConcDataStErr)./app.SpiderData.Data(SelMeth).Conc',':k','linewidth',1,'Color',ColorMap(i,:));
                        plot(app.UIAxes_Plot,(ConcData-ConcDataStErr)./app.SpiderData.Data(SelMeth).Conc',':k','linewidth',1,'Color',ColorMap(i,:));
                    case '1 sd'
                        plot(app.UIAxes_Plot,(ConcData+ConcDataStdev)./app.SpiderData.Data(SelMeth).Conc',':k','linewidth',1,'Color',ColorMap(i,:));
                        plot(app.UIAxes_Plot,(ConcData-ConcDataStdev)./app.SpiderData.Data(SelMeth).Conc',':k','linewidth',1,'Color',ColorMap(i,:));
                end
                app.UIAxes_Plot.YScale = 'log';
                app.UIAxes_Plot.XTick = [1:length(app.SpiderData.Data(SelMeth).El)]; 
                app.UIAxes_Plot.XTickLabel = app.SpiderData.Data(SelMeth).El;
            end
            hold(app.UIAxes_Plot,'off')
        end
        
        
        function ROI_Line_Value_Extractor(app, ~)
            
            DeactivatePlotZoomPanOptions(app);
            
            SelMeth = app.Normalization_Menu.Value;
            
            Positions = app.ROI_samplingPolyline.Position;
            
            X = Positions(:,1);
            Y = Positions(:,2);
            
            LesX = [];
            LesY = [];
            for i = 2:length(X)
                Xs = [X(i-1),X(i)];
                Ys = [Y(i-1),Y(i)];
                
                LX = max(Xs) - min(Xs);
                LY = max(Ys) - min(Ys);
                
                LZ = round(sqrt(LX^2 + LY^2));
                
                if Xs(2) > Xs(1)
                    LesXs = Xs(1):(Xs(2)-Xs(1))/(LZ-1):Xs(2);
                elseif Xs(2) < Xs(1)
                    LesXs = Xs(1):-(Xs(1)-Xs(2))/(LZ-1):Xs(2);
                else
                    LesXs = ones(LZ,1) * Xs(1);
                end
                
                if Ys(2) > Ys(1)
                    LesYs = Ys(1):(Ys(2)-Ys(1))/(LZ-1):Ys(2);
                elseif Ys(2) < Ys(1)
                    LesYs = Ys(1):-(Ys(1)-Ys(2))/(LZ-1):Ys(2);
                else
                    LesYs = ones(LZ,1) * Ys(1);
                end
                
                LesX(end+1:end+length(LesXs)) = LesXs;
                LesY(end+1:end+length(LesYs)) = LesYs;
            end
            
            % Indexation
            XCoo = 1:1:length(app.MapData.CData(1).Map(1,:));
            YCoo = 1:1:length(app.MapData.CData(1).Map(:,1));
            
            for i = 1 : length(LesX)
                [V(i,1), IdxAll(i,1)] = min(abs(XCoo-LesX(i))); % Index X
                [V(i,2), IdxAll(i,2)] = min(abs(YCoo-LesY(i))); % Index Y
            end
            
            MapIdx = cell2mat(app.UITable.Data(:,4));
            
            ConcData = nan(size(MapIdx,1),size(IdxAll,1));
            for i = 1 :length(MapIdx)
                if MapIdx(i) > 0
                    for j = 1:size(IdxAll,1)
                        ConcData(i,j) = app.MapData.CData(MapIdx(i)).Map(IdxAll(j,2),IdxAll(j,1));
                    end
                end
            end
            
            Data4Plot = ConcData./repmat(app.SpiderData.Data(SelMeth).Conc',1,size(ConcData,2));
            
            ColorMap = app.XMapToolsApp.CalculateColorMap(app.ColorPaletteDropDown.Value,size(Data4Plot,2));
            
            for i = 1:size(Data4Plot,2)
                plot(app.UIAxes_Plot,Data4Plot(:,i),'-k','linewidth',1,'Color',ColorMap(i,:));
                hold(app.UIAxes_Plot,'on')
            end
            hold(app.UIAxes_Plot,'off')
            
            app.UIAxes_Plot.YScale = 'log';
            app.UIAxes_Plot.XTick = [1:length(app.SpiderData.Data(SelMeth).El)];
            app.UIAxes_Plot.XTickLabel = app.SpiderData.Data(SelMeth).El;
            colorbar(app.UIAxes_Plot)
            colormap(app.UIAxes_Plot,ColorMap)
            
        end
        
        function DeactivatePlotZoomPanOptions(app)
            
            if ~isempty(app.UIAxes_Map)
                
                if ~isempty(app.UIAxes_Map.Toolbar.Children)
                    app.UIAxes_Map.Toolbar.Children(1).Value = 'off';
                    matlab.graphics.interaction.webmodes.toggleMode(app.UIAxes_Map,'pan',app.UIAxes_Map.Toolbar.Children(1).Value)
                    
                    app.UIAxes_Map.Toolbar.Children(2).Value = 'off';
                    matlab.graphics.interaction.webmodes.toggleMode(app.UIAxes_Map,'zoom',app.UIAxes_Map.Toolbar.Children(2).Value)
                    
                    app.UIAxes_Map.Toolbar.Children(3).Value = 'off';
                    matlab.graphics.interaction.webmodes.toggleMode(app.UIAxes_Map,'zoomout',app.UIAxes_Map.Toolbar.Children(3).Value)
                end
            end
        end
        
        function ImageData = PlotMap_ExtractPlottedImage(app)
            lesInd = app.UIAxes_Map.Children;
            for i=1:numel(lesInd)
                if isequal(lesInd(i).Type,'image')
                    ImageData = lesInd(i).CData;
                    break
                else
                    ImageData = [];
                end
            end
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
            
            app.Spider_GUI.Visible = 'off';
            
            app.XMapToolsApp = XMapToolsApp;
            
            ReadSpiderData(app);
            
            app.Normalization_Menu.Items = app.SpiderData.Names;
            app.Normalization_Menu.ItemsData = [1:length(app.Normalization_Menu.Items)];
            
            NodeData = app.XMapToolsApp.TreeData_Main.SelectedNodes.NodeData;
            
            app.MapData = app.XMapToolsApp.XMapToolsData.MapData.Qt.Data(NodeData(2));
            
            app.Element_Menu.Items = app.MapData.ElNames;
            app.Element_Menu.ItemsData = [1:length(app.Element_Menu.Items)];
            
            PlotSelectedMap(app);
            
            ChekMethodChanged(app);
            
            app.ColorPaletteDropDown.Items = extractfield(app.XMapToolsApp.ColorMaps,'Name');
            app.ColorPaletteDropDown.ItemsData = [1:length(app.ColorPaletteDropDown.Items)];
            
            app.ROI_sampling.Nb = 0;
            
            movegui(app.Spider_GUI,'center');
            
            app.Spider_GUI.Visible = 'on';  
        end

        % Value changed function: Element_Menu
        function Element_MenuValueChanged(app, event)
            PlotSelectedMap(app);
        end

        % Value changed function: Normalization_Menu
        function Normalization_MenuValueChanged(app, event)
            ChekMethodChanged(app)
            
            if length(findall(app.UIAxes_Map, 'Type',  'images.roi.Circle')) > 0
                ROI_Value_Extractor(app, 0);
            end
            if length(findall(app.UIAxes_Map, 'Type',  'images.roi.Polyline')) > 0
                ROI_Line_Value_Extractor(app, 0)
            end
            
        end

        % Button pushed function: Circle_Button
        function Circle_ButtonPushed(app, event)
            
            delete(findall(app.UIAxes_Map, 'Type',  'images.roi.Polyline'));
            
            DeactivatePlotZoomPanOptions(app);
            
            if ~app.HoldonCheckBox.Value
                delete(findall(app.UIAxes_Map, 'Type',  'images.roi.Circle'));
                delete(findall(app.UIAxes_Map, 'Type',  'images.roi.Polygon'));
                app.ROI_sampling = [];
                app.ROI_sampling.Nb = 0;
            end
            
            PosROI = app.ROI_sampling(end).Nb+1;
            
            app.ROI_sampling(PosROI).ROI = drawcircle(app.UIAxes_Map,'Color',app.XMapToolsApp.GetROIColor,'InteractionsAllowed','all');
            
            app.ROI_sampling_Listener = addlistener(app.ROI_sampling(PosROI).ROI, 'MovingROI', @(varargin)ROI_Value_Extractor(app, app.ROI_sampling));
            
            app.ROI_sampling(PosROI).Nb = PosROI;
            
            ROI_Value_Extractor(app, app.ROI_sampling);
            
        end

        % Value changed function: ColorPaletteDropDown
        function ColorPaletteDropDownValueChanged(app, event)
            if length(findall(app.UIAxes_Map, 'Type',  'images.roi.Circle')) > 0
                ROI_Value_Extractor(app, 0);
            end
            if length(findall(app.UIAxes_Map, 'Type',  'images.roi.Polyline')) > 0
                ROI_Line_Value_Extractor(app, 0)
            end
            
        end

        % Button pushed function: ROI_Button
        function ROI_ButtonPushed(app, event)
            
            delete(findall(app.UIAxes_Map, 'Type',  'images.roi.Polyline'));
            
            DeactivatePlotZoomPanOptions(app);
            
            if ~app.HoldonCheckBox.Value
                delete(findall(app.UIAxes_Map, 'Type',  'images.roi.Circle'));
                delete(findall(app.UIAxes_Map, 'Type',  'images.roi.Polygon'));
                app.ROI_sampling = [];
                app.ROI_sampling.Nb = 0;
            end
            
            PosROI = app.ROI_sampling(end).Nb+1;
            
            app.ROI_sampling(PosROI).ROI = drawpolygon(app.UIAxes_Map,'Color',app.XMapToolsApp.GetROIColor,'InteractionsAllowed','all');
            
            app.ROI_sampling_Listener = addlistener(app.ROI_sampling(PosROI).ROI, 'MovingROI', @(varargin)ROI_Value_Extractor(app, app.ROI_sampling));
            
            app.ROI_sampling(PosROI).Nb = PosROI;
            
            ROI_Value_Extractor(app, app.ROI_sampling);
            
            
            
        end

        % Button pushed function: Path_Button
        function Path_ButtonPushed(app, event)
            
            delete(findall(app.UIAxes_Map, 'Type',  'images.roi.Circle'));
            delete(findall(app.UIAxes_Map, 'Type',  'images.roi.Polygon'));
            delete(findall(app.UIAxes_Map, 'Type',  'images.roi.Polyline'));
            
            DeactivatePlotZoomPanOptions(app);
            
            app.ROI_samplingPolyline = drawpolyline(app.UIAxes_Map,'Color',app.XMapToolsApp.GetROIColor,'InteractionsAllowed','all');
            
            app.ROI_samplingPolyline_Listener = addlistener(app.ROI_samplingPolyline, 'ROIMoved', @(varargin)ROI_Line_Value_Extractor(app, app.ROI_samplingPolyline));
           
            ROI_Line_Value_Extractor(app, app.ROI_samplingPolyline);
            
        end

        % Button pushed function: Button_help
        function Button_helpPushed(app, event)
            
            
            if isempty(app.XMapToolsApp.Id_HelpTool)
                Help_Display(app.XMapToolsApp,'XMT_help_Spider.html');
            else
                app.XMapToolsApp.Id_HelpTool.UpdateTextHelp('XMT_help_Spider.html');
            end
            
            
            
        end

        % Button pushed function: AutoContrast
        function AutoContrastButtonPushed(app, event)
            
            ImageData = PlotMap_ExtractPlottedImage(app);       % extract ImageData from the figure
            ImageData = ImageData(find(ImageData));             % exclude zeros (new 4.0.0)
            IdxNotNan = find(~isnan(ImageData));
            if length(IdxNotNan) > 0
                ImageData = ImageData(IdxNotNan);
            end
            SortedData = sort(ImageData(:));
            SelCrit = round(numel(SortedData) * 0.065);
            
            Min1 = SortedData(SelCrit);
            Max1 = SortedData(end-SelCrit);
            
            [Min,Max] = caxis(app.UIAxes_Map);
            
            if isequal(Min,Min1) && isequal(Max,Max1)
                caxis(app.UIAxes_Map,[min(ImageData(:)),max(ImageData(:))])
            else
                caxis(app.UIAxes_Map,[Min1,Max1])
            end
            
        end

        % Value changed function: UncertaintyDropDown
        function UncertaintyDropDownValueChanged(app, event)
            
            if length(findall(app.UIAxes_Map, 'Type',  'images.roi.Circle')) > 0
                ROI_Value_Extractor(app, 0);
            end
            if length(findall(app.UIAxes_Map, 'Type',  'images.roi.Polyline')) > 0
                ROI_Line_Value_Extractor(app, 0)
            end
            
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create Spider_GUI and hide until all components are created
            app.Spider_GUI = uifigure('Visible', 'off');
            app.Spider_GUI.Position = [100 100 1262 793];
            app.Spider_GUI.Name = 'Spider Plot – XMapTools';

            % Create GridLayout
            app.GridLayout = uigridlayout(app.Spider_GUI);
            app.GridLayout.ColumnWidth = {'0.15x', '1x', '1x', '1x', '1x', '0.3x', '1x', '1x', '1x', '1x', '0.15x'};
            app.GridLayout.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.5x'};

            % Create Image
            app.Image = uiimage(app.GridLayout);
            app.Image.Layout.Row = 1;
            app.Image.Layout.Column = [1 3];
            app.Image.HorizontalAlignment = 'left';
            app.Image.ImageSource = 'logo_xmap_final.png';

            % Create GridLayout2
            app.GridLayout2 = uigridlayout(app.GridLayout);
            app.GridLayout2.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout2.RowHeight = {'1x'};
            app.GridLayout2.Padding = [10 13 10 13];
            app.GridLayout2.Layout.Row = 1;
            app.GridLayout2.Layout.Column = [4 6];

            % Create Button_help
            app.Button_help = uibutton(app.GridLayout2, 'push');
            app.Button_help.ButtonPushedFcn = createCallbackFcn(app, @Button_helpPushed, true);
            app.Button_help.Icon = '061-info.png';
            app.Button_help.Layout.Row = 1;
            app.Button_help.Layout.Column = 4;
            app.Button_help.Text = '';

            % Create UITable
            app.UITable = uitable(app.GridLayout);
            app.UITable.ColumnName = {'Element'; 'Reference'; 'Map Available?'; 'Map Index'; 'Map Name'};
            app.UITable.RowName = {};
            app.UITable.Layout.Row = [1 5];
            app.UITable.Layout.Column = [7 10];
            app.UITable.FontSize = 11;

            % Create GridLayout3
            app.GridLayout3 = uigridlayout(app.GridLayout);
            app.GridLayout3.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout3.RowHeight = {'0.5x', '1x'};
            app.GridLayout3.RowSpacing = 5;
            app.GridLayout3.Padding = [10 3 10 3];
            app.GridLayout3.Layout.Row = 5;
            app.GridLayout3.Layout.Column = [2 5];

            % Create AutoContrast
            app.AutoContrast = uibutton(app.GridLayout3, 'push');
            app.AutoContrast.ButtonPushedFcn = createCallbackFcn(app, @AutoContrastButtonPushed, true);
            app.AutoContrast.Icon = 'XXX_magic-wand.png';
            app.AutoContrast.Layout.Row = 2;
            app.AutoContrast.Layout.Column = 4;
            app.AutoContrast.Text = '';

            % Create Element_Menu
            app.Element_Menu = uidropdown(app.GridLayout3);
            app.Element_Menu.ValueChangedFcn = createCallbackFcn(app, @Element_MenuValueChanged, true);
            app.Element_Menu.Layout.Row = 2;
            app.Element_Menu.Layout.Column = [1 3];

            % Create UncertaintyDropDown
            app.UncertaintyDropDown = uidropdown(app.GridLayout3);
            app.UncertaintyDropDown.Items = {'1 se', '1 sd'};
            app.UncertaintyDropDown.ValueChangedFcn = createCallbackFcn(app, @UncertaintyDropDownValueChanged, true);
            app.UncertaintyDropDown.Layout.Row = 2;
            app.UncertaintyDropDown.Layout.Column = [11 12];
            app.UncertaintyDropDown.Value = '1 se';

            % Create UncertaintyDropDownLabel
            app.UncertaintyDropDownLabel = uilabel(app.GridLayout3);
            app.UncertaintyDropDownLabel.HorizontalAlignment = 'right';
            app.UncertaintyDropDownLabel.Layout.Row = 2;
            app.UncertaintyDropDownLabel.Layout.Column = [9 10];
            app.UncertaintyDropDownLabel.Text = 'Uncertainty';

            % Create DisplayToolsLabel
            app.DisplayToolsLabel = uilabel(app.GridLayout3);
            app.DisplayToolsLabel.VerticalAlignment = 'bottom';
            app.DisplayToolsLabel.FontSize = 11;
            app.DisplayToolsLabel.FontAngle = 'italic';
            app.DisplayToolsLabel.Layout.Row = 1;
            app.DisplayToolsLabel.Layout.Column = [1 7];
            app.DisplayToolsLabel.Text = 'Display Tools';

            % Create GridLayout4
            app.GridLayout4 = uigridlayout(app.GridLayout);
            app.GridLayout4.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout4.RowHeight = {'1x'};
            app.GridLayout4.Padding = [10 13 10 13];
            app.GridLayout4.Layout.Row = 2;
            app.GridLayout4.Layout.Column = [2 5];

            % Create NormalizationSchemeLabel
            app.NormalizationSchemeLabel = uilabel(app.GridLayout4);
            app.NormalizationSchemeLabel.HorizontalAlignment = 'right';
            app.NormalizationSchemeLabel.Layout.Row = 1;
            app.NormalizationSchemeLabel.Layout.Column = [1 4];
            app.NormalizationSchemeLabel.Text = 'Normalization Scheme';

            % Create Normalization_Menu
            app.Normalization_Menu = uidropdown(app.GridLayout4);
            app.Normalization_Menu.ValueChangedFcn = createCallbackFcn(app, @Normalization_MenuValueChanged, true);
            app.Normalization_Menu.Layout.Row = 1;
            app.Normalization_Menu.Layout.Column = [5 12];

            % Create GridLayout5
            app.GridLayout5 = uigridlayout(app.GridLayout);
            app.GridLayout5.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout5.RowHeight = {'0.5x', '1x'};
            app.GridLayout5.RowSpacing = 5;
            app.GridLayout5.Padding = [10 3 10 3];
            app.GridLayout5.Layout.Row = 3;
            app.GridLayout5.Layout.Column = [2 5];

            % Create Circle_Button
            app.Circle_Button = uibutton(app.GridLayout5, 'push');
            app.Circle_Button.ButtonPushedFcn = createCallbackFcn(app, @Circle_ButtonPushed, true);
            app.Circle_Button.Icon = '142-target.png';
            app.Circle_Button.HorizontalAlignment = 'left';
            app.Circle_Button.Layout.Row = 2;
            app.Circle_Button.Layout.Column = [1 2];
            app.Circle_Button.Text = 'Circle';

            % Create Path_Button
            app.Path_Button = uibutton(app.GridLayout5, 'push');
            app.Path_Button.ButtonPushedFcn = createCallbackFcn(app, @Path_ButtonPushed, true);
            app.Path_Button.Icon = '135-map.png';
            app.Path_Button.HorizontalAlignment = 'left';
            app.Path_Button.Layout.Row = 2;
            app.Path_Button.Layout.Column = [5 6];
            app.Path_Button.Text = 'Path';

            % Create ColorPaletteDropDownLabel
            app.ColorPaletteDropDownLabel = uilabel(app.GridLayout5);
            app.ColorPaletteDropDownLabel.HorizontalAlignment = 'right';
            app.ColorPaletteDropDownLabel.Layout.Row = 2;
            app.ColorPaletteDropDownLabel.Layout.Column = [9 10];
            app.ColorPaletteDropDownLabel.Text = 'Color Palette';

            % Create ColorPaletteDropDown
            app.ColorPaletteDropDown = uidropdown(app.GridLayout5);
            app.ColorPaletteDropDown.ValueChangedFcn = createCallbackFcn(app, @ColorPaletteDropDownValueChanged, true);
            app.ColorPaletteDropDown.Layout.Row = 2;
            app.ColorPaletteDropDown.Layout.Column = [11 12];

            % Create HoldonCheckBox
            app.HoldonCheckBox = uicheckbox(app.GridLayout5);
            app.HoldonCheckBox.Text = 'Hold on';
            app.HoldonCheckBox.Layout.Row = 2;
            app.HoldonCheckBox.Layout.Column = [7 8];

            % Create ROI_Button
            app.ROI_Button = uibutton(app.GridLayout5, 'push');
            app.ROI_Button.ButtonPushedFcn = createCallbackFcn(app, @ROI_ButtonPushed, true);
            app.ROI_Button.Icon = '139-placeholder.png';
            app.ROI_Button.HorizontalAlignment = 'left';
            app.ROI_Button.Layout.Row = 2;
            app.ROI_Button.Layout.Column = [3 4];
            app.ROI_Button.Text = 'ROI';

            % Create SamplingToolsLabel
            app.SamplingToolsLabel = uilabel(app.GridLayout5);
            app.SamplingToolsLabel.VerticalAlignment = 'bottom';
            app.SamplingToolsLabel.FontSize = 11;
            app.SamplingToolsLabel.FontAngle = 'italic';
            app.SamplingToolsLabel.Layout.Row = 1;
            app.SamplingToolsLabel.Layout.Column = [1 7];
            app.SamplingToolsLabel.Text = 'Sampling Tools';

            % Create GridLayout6
            app.GridLayout6 = uigridlayout(app.GridLayout);
            app.GridLayout6.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout6.RowHeight = {'1x'};
            app.GridLayout6.Padding = [10 13 10 13];
            app.GridLayout6.Layout.Row = 4;
            app.GridLayout6.Layout.Column = [2 5];

            % Create UIAxes_Plot
            app.UIAxes_Plot = uiaxes(app.GridLayout);
            ylabel(app.UIAxes_Plot, 'Normalized Abundance')
            app.UIAxes_Plot.PlotBoxAspectRatio = [1.50588235294118 1 1];
            app.UIAxes_Plot.Layout.Row = [6 11];
            app.UIAxes_Plot.Layout.Column = [7 10];

            % Create UIAxes_Map
            app.UIAxes_Map = uiaxes(app.GridLayout);
            app.UIAxes_Map.XTick = [];
            app.UIAxes_Map.YTick = [];
            app.UIAxes_Map.Layout.Row = [6 11];
            app.UIAxes_Map.Layout.Column = [2 5];

            % Show the figure after all components are created
            app.Spider_GUI.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = Spider_exported(varargin)

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.Spider_GUI)

            % Execute the startup function
            runStartupFcn(app, @(app)startupFcn(app, varargin{:}))

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.Spider_GUI)
        end
    end
end