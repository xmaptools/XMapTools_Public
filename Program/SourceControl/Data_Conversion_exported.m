classdef Data_Conversion_exported < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        DataConversionModule            matlab.ui.Figure
        GridLayout                      matlab.ui.container.GridLayout
        Image                           matlab.ui.control.Image
        GridLayout2                     matlab.ui.container.GridLayout
        HelpButton                      matlab.ui.control.Button
        GridLayout4                     matlab.ui.container.GridLayout
        Appy                            matlab.ui.control.Button
        SelecttheconversionmethodLabel  matlab.ui.control.Label
        GridLayout5                     matlab.ui.container.GridLayout
        Method                          matlab.ui.control.DropDown
        MethodDropDownLabel             matlab.ui.control.Label
    end

    
    properties (Access = private)
        XMapToolsApp % Description
        Mode
    end
    

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, XMapToolsApp, Mode)
            
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
            
            app.DataConversionModule.Visible = 'off';
            
            app.XMapToolsApp = XMapToolsApp;
            app.Mode = Mode;
            
            MethodList = {'Element (ug/g) -> Oxide (wt%)', ...        % 1
                'Element (ug/g) -> Oxide (ug/g)', ...                 % 2
                'Element (wt%) -> Oxide (wt%)', ...                   % 3
                'Element (wt%) -> Oxide (ug/g)', ...                  % 4
                'Oxide (wt%) -> Element (ug/g)', ...                  % 5
                'Oxide (wt%) -> Element (wt%)', ...                   % 6
                'Oxide (ug/g) -> Element (ug/g)', ...                 % 7
                'Oxide (ug/g) -> Element (wt%)', ...                  % 8
                'Oxide (wt%) -> Oxide (mol)', ...                     % 9
                };                   
            
            
            
            app.Method.Items = MethodList;
            app.Method.ItemsData = 1:length(app.Method.Items);
            
            movegui(app.DataConversionModule,'center');
            
            app.DataConversionModule.Visible = 'on';
        end

        % Button pushed function: Appy
        function AppyButtonPushed(app, event)
            
            NodeData = app.XMapToolsApp.TreeData_Main.SelectedNodes.NodeData;
            
            Method_Idx = app.Method.Value;
            
            switch app.Mode
                case 'Quanti'
                    Type = 'Qt';
                case 'Merged'
                    Type = 'Me';
            end
            % Only in Qt
            Idx = NodeData(2);
            Pos = length(app.XMapToolsApp.XMapToolsData.MapData.(Type).Names)+1;
            
            % Duplicate
            app.XMapToolsApp.XMapToolsData.MapData.(Type).Names{Pos} = [char(app.XMapToolsApp.XMapToolsData.MapData.(Type).Names{Idx}),'_conv'];
            if isequal(Method_Idx,1) || isequal(Method_Idx,2) || isequal(Method_Idx,3) || isequal(Method_Idx,4)
                app.XMapToolsApp.XMapToolsData.MapData.(Type).IsOxide(Pos) = 1;
            else
                app.XMapToolsApp.XMapToolsData.MapData.(Type).IsOxide(Pos) = 0;
            end
            app.XMapToolsApp.XMapToolsData.MapData.(Type).MaskFile{Pos} = app.XMapToolsApp.XMapToolsData.MapData.(Type).MaskFile{Idx};
            app.XMapToolsApp.XMapToolsData.MapData.(Type).NbCalibPoints(Pos) = app.XMapToolsApp.XMapToolsData.MapData.(Type).NbCalibPoints(Idx);
            app.XMapToolsApp.XMapToolsData.MapData.(Type).Data(Pos) = app.XMapToolsApp.XMapToolsData.MapData.(Type).Data(Idx);
            
            % CONVERT DATA 
            
            % (Block 1) Element to Oxide
            if isequal(Method_Idx,1) || isequal(Method_Idx,2) || isequal(Method_Idx,3) || isequal(Method_Idx,4)
                
                ElList = app.XMapToolsApp.XMapToolsData.MapData.(Type).Data(Pos).ElNames;
                ElList_DB = app.XMapToolsApp.ElOxDataDef.ElList;
                [Ok,ElIdx] = ismember(ElList,ElList_DB);
                
                OxIdConv = zeros(size(ElIdx));
                OxNames = cell(size(ElIdx));
                for i = 1:length(ElIdx)
                    El = ElIdx(i);
                    OxId = find(app.XMapToolsApp.ElOxDataDef.OxElIdx == El);
                    
                    if ~isempty(OxId)
                        OxIdConv(i) = OxId(1);
                        OxNames{i} = app.XMapToolsApp.ElOxDataDef.OxList{OxId(1)};
                    else
                        OxNames{i} = '';
                    end
                end
                
                for i = 1:length(OxIdConv)
                    
                    Map = app.XMapToolsApp.XMapToolsData.MapData.(Type).Data(Pos).CData(i).Map;
                    
                    if ~isequal(OxIdConv(i),0)
                        
                        app.XMapToolsApp.XMapToolsData.MapData.(Type).Data(Pos).ElNames{i} = OxNames{i};
                        app.XMapToolsApp.XMapToolsData.MapData.(Type).Data(Pos).ElInd(i) = OxIdConv(i);
                        
                        f = app.XMapToolsApp.ElOxDataDef.OxFact(OxIdConv(i));
                        
                        switch Method_Idx
                            
                            case 1 % Element (ug/g) -> Oxide (wt%)
                                app.XMapToolsApp.XMapToolsData.MapData.(Type).Data(Pos).CData(i).Map = Map ./ 1e4 .* 1./f ;
                                
                            case 2 % Element (ug/g) -> Oxide (ug/g)
                                app.XMapToolsApp.XMapToolsData.MapData.(Type).Data(Pos).CData(i).Map = Map .* 1./f;
                                
                            case 3 % Element (wt%) -> Oxide (wt%)
                                app.XMapToolsApp.XMapToolsData.MapData.(Type).Data(Pos).CData(i).Map = Map .* 1./f;
                                
                            case 4 % Element (wt%) -> Oxide (ug/g)
                                app.XMapToolsApp.XMapToolsData.MapData.(Type).Data(Pos).CData(i).Map = Map .* 1e4 .* 1./f;
                        end
                        
                        
                    else
                        switch Method_Idx
                            
                            case 1 % Element (ug/g) -> Oxide (wt%)
                                app.XMapToolsApp.XMapToolsData.MapData.(Type).Data(Pos).CData(i).Map = Map ./ 1e4;
                            
                            case 4 % Element (wt%) -> Oxide (ug/g)
                                app.XMapToolsApp.XMapToolsData.MapData.(Type).Data(Pos).CData(i).Map = Map .* 1e4;
                        end
                        
                    end
                end
                close(app.DataConversionModule);
            end
            
            
            % (Block 2) Oxide to Element
            if isequal(Method_Idx,5) || isequal(Method_Idx,6) || isequal(Method_Idx,7) || isequal(Method_Idx,8)
                
                OxList = app.XMapToolsApp.XMapToolsData.MapData.(Type).Data(Pos).ElNames;
                OxList_DB = app.XMapToolsApp.ElOxDataDef.OxList;
                
                [Ok,OxIdx] = ismember(OxList,OxList_DB);
                
                for i = 1:length(OxIdx)
                    if isequal(Ok(i),1)
                        app.XMapToolsApp.XMapToolsData.MapData.(Type).Data(Pos).ElNames{i} = app.XMapToolsApp.ElOxDataDef.ElList{app.XMapToolsApp.ElOxDataDef.OxElIdx(OxIdx(i))};
                        app.XMapToolsApp.XMapToolsData.MapData.(Type).Data(Pos).ElInd(i) = find(ismember(app.XMapToolsApp.ElOxDataDef.ElList,app.XMapToolsApp.XMapToolsData.MapData.(Type).Data(Pos).ElNames{i}));
                        
                        Map = app.XMapToolsApp.XMapToolsData.MapData.(Type).Data(Pos).CData(i).Map;
                        f = app.XMapToolsApp.ElOxDataDef.OxFact(OxIdx(i));
                        
                        switch Method_Idx
                            
                            case 5 % Oxide (wt%) -> Element (ug/g)
                                app.XMapToolsApp.XMapToolsData.MapData.(Type).Data(Pos).CData(i).Map = Map .* 1e4 .* f ;
                                
                            case 6 % Oxide (wt%) -> Element (wt%)
                                app.XMapToolsApp.XMapToolsData.MapData.(Type).Data(Pos).CData(i).Map = Map .* f;
                                
                            case 7 % Oxide (ug/g) -> Element (ug/g)
                                app.XMapToolsApp.XMapToolsData.MapData.(Type).Data(Pos).CData(i).Map = Map .* f;
                                
                            case 8 % Oxide (ug/g) -> Element (wt%)
                                app.XMapToolsApp.XMapToolsData.MapData.(Type).Data(Pos).CData(i).Map = Map ./ 1e4 .* f;
                        end
                        
                    else
                        
                        switch Method_Idx
                            
                            case 5 % Oxide (wt%) -> Element (ug/g)
                                app.XMapToolsApp.XMapToolsData.MapData.(Type).Data(Pos).CData(i).Map = Map .* 1e4;
                                
                            case 8 % Oxide (ug/g) -> Element (wt%)
                                app.XMapToolsApp.XMapToolsData.MapData.(Type).Data(Pos).CData(i).Map = Map ./ 1e4;
                        end
                        
                    end
                end
                close(app.DataConversionModule);
            end
            
            % (Block 3) Oxide wt to mol
            if isequal(Method_Idx,9)
                
                OxList = app.XMapToolsApp.XMapToolsData.MapData.(Type).Data(Pos).ElNames;
                OxList_DB = app.XMapToolsApp.ElOxDataDef.OxList;
                
                [Ok,OxIdx] = ismember(OxList,OxList_DB);
                
                for i = 1:length(OxIdx)
                    
                    OxideMass = app.XMapToolsApp.ElOxDataDef.OxMass(OxIdx(i));
                    % OxNbCat = app.XMapToolsApp.ElOxDataDef.OxNbCat(OxIdx(i)); 
                    
                    Map = app.XMapToolsApp.XMapToolsData.MapData.(Type).Data(Pos).CData(i).Map;
                    
                    app.XMapToolsApp.XMapToolsData.MapData.(Type).Data(Pos).CData(i).Map = Map ./ OxideMass;
                end
                close(app.DataConversionModule);
            end
            
            
            
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create DataConversionModule and hide until all components are created
            app.DataConversionModule = uifigure('Visible', 'off');
            app.DataConversionModule.Position = [100 100 693 379];
            app.DataConversionModule.Name = 'Data Conversion – XMapTools';

            % Create GridLayout
            app.GridLayout = uigridlayout(app.DataConversionModule);
            app.GridLayout.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout.RowHeight = {'1x', '1x', '0.6x', '1x', '1x', '1x', '0.2x'};

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
            app.GridLayout2.Padding = [10 10 8 8];
            app.GridLayout2.Layout.Row = 1;
            app.GridLayout2.Layout.Column = [4 7];

            % Create HelpButton
            app.HelpButton = uibutton(app.GridLayout2, 'push');
            app.HelpButton.Icon = '061-info.png';
            app.HelpButton.Layout.Row = 1;
            app.HelpButton.Layout.Column = 4;
            app.HelpButton.Text = '';

            % Create GridLayout4
            app.GridLayout4 = uigridlayout(app.GridLayout);
            app.GridLayout4.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout4.RowHeight = {'1x'};
            app.GridLayout4.Layout.Row = 4;
            app.GridLayout4.Layout.Column = [1 7];

            % Create Appy
            app.Appy = uibutton(app.GridLayout4, 'push');
            app.Appy.ButtonPushedFcn = createCallbackFcn(app, @AppyButtonPushed, true);
            app.Appy.Icon = '044-repeat.png';
            app.Appy.Layout.Row = 1;
            app.Appy.Layout.Column = [4 5];
            app.Appy.Text = 'Apply';

            % Create SelecttheconversionmethodLabel
            app.SelecttheconversionmethodLabel = uilabel(app.GridLayout);
            app.SelecttheconversionmethodLabel.HorizontalAlignment = 'center';
            app.SelecttheconversionmethodLabel.VerticalAlignment = 'bottom';
            app.SelecttheconversionmethodLabel.Layout.Row = 2;
            app.SelecttheconversionmethodLabel.Layout.Column = [1 7];
            app.SelecttheconversionmethodLabel.Text = 'Select the conversion method';

            % Create GridLayout5
            app.GridLayout5 = uigridlayout(app.GridLayout);
            app.GridLayout5.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout5.RowHeight = {'1x'};
            app.GridLayout5.Layout.Row = 5;
            app.GridLayout5.Layout.Column = [1 7];

            % Create Method
            app.Method = uidropdown(app.GridLayout);
            app.Method.Layout.Row = 3;
            app.Method.Layout.Column = [2 7];

            % Create MethodDropDownLabel
            app.MethodDropDownLabel = uilabel(app.GridLayout);
            app.MethodDropDownLabel.HorizontalAlignment = 'right';
            app.MethodDropDownLabel.Layout.Row = 3;
            app.MethodDropDownLabel.Layout.Column = 1;
            app.MethodDropDownLabel.Text = 'Method';

            % Show the figure after all components are created
            app.DataConversionModule.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = Data_Conversion_exported(varargin)

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.DataConversionModule)

            % Execute the startup function
            runStartupFcn(app, @(app)startupFcn(app, varargin{:}))

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.DataConversionModule)
        end
    end
end