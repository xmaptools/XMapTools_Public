classdef Rearranger_exported < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        RearrangerGUI          matlab.ui.Figure
        GridLayout             matlab.ui.container.GridLayout
        Image                  matlab.ui.control.Image
        Tree_1                 matlab.ui.container.Tree
        Tree_2                 matlab.ui.container.Tree
        AddButton              matlab.ui.control.Button
        RemoveButton           matlab.ui.control.Button
        ClearAllButton         matlab.ui.control.Button
        GridLayout3            matlab.ui.container.GridLayout
        VALIDATECHANGESButton  matlab.ui.control.Button
    end

    
    properties (Access = private)
        XMapToolsApp
        Mode
        Selected
        
        Mask
        
        
    end
    
    methods (Access = private)
        
        function [NodeData_1,NodeData_2] = ExtractNodeData(app)
            NodeData_1 = [];
            NodeData_2 = [];
            
            for i = 1:length(app.Tree_1.Children)
                NodeData_1(i) = app.Tree_1.Children(i).NodeData;
            end
            
            for i = 1:length(app.Tree_2.Children)
                NodeData_2(i) = app.Tree_2.Children(i).NodeData;
            end
        end
        
        function CheckValidate(app)
            [NodeData_1,NodeData_2] = ExtractNodeData(app);
            if isequal(length(NodeData_1),length(NodeData_2))
                app.VALIDATECHANGESButton.Enable = 'on';
            else
                app.VALIDATECHANGESButton.Enable = 'off';
            end
        end
    end
    

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, XMapToolsApp, Mode, Selected)
            
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
            
            
            app.RearrangerGUI.Visible = 'off';
            
            movegui(app.RearrangerGUI,'center');
            
            app.XMapToolsApp = XMapToolsApp;
            app.Mode = Mode;
            app.Selected = Selected;
            
            switch Mode
                case 'MaskFile'
                    app.Mask = app.XMapToolsApp.XMapToolsData.MapData.MaskFile.Masks(app.Selected);
                    
                    for i = 2:length(app.Mask.Names)
                        p = uitreenode(app.Tree_1,'Text',char(app.Mask.Names{i}),'NodeData',[i]);
                    end
            end
            
            app.RearrangerGUI.Visible = 'on';
            
        end

        % Close request function: RearrangerGUI
        function RearrangerGUICloseRequest(app, event)
            delete(app);
        end

        % Selection changed function: Tree_1
        function Tree_1SelectionChanged(app, event)
            SelectedNode = app.Tree_1.SelectedNodes.NodeData;
            
            [NodeData_1,NodeData_2] = ExtractNodeData(app);
            
            if find(ismember(NodeData_2,NodeData_1(SelectedNode-1)))
                app.AddButton.Enable = 'off';
            else
                app.AddButton.Enable = 'on';
            end
            
            if length(NodeData_2) >= 1
                app.ClearAllButton.Enable = 'on';
            else
                app.ClearAllButton.Enable = 'off';
            end
            
            app.RemoveButton.Enable = 'off';
            CheckValidate(app);
        end

        % Button pushed function: AddButton
        function AddButtonPushed(app, event)
            SelectedNode = app.Tree_1.SelectedNodes.NodeData;
            p = uitreenode(app.Tree_2,'Text',char(app.Mask.Names{SelectedNode}),'NodeData',[SelectedNode]);
            CheckValidate(app);
        end

        % Button pushed function: RemoveButton
        function RemoveButtonPushed(app, event)
            SelectedNode = app.Tree_2.SelectedNodes.NodeData;
            app.Tree_2.SelectedNodes.delete;
            CheckValidate(app);
        end

        % Button pushed function: ClearAllButton
        function ClearAllButtonPushed(app, event)
            for i = 1:length(app.Tree_2.Children)
                app.Tree_2.Children(i).delete;
            end
            CheckValidate(app);
        end

        % Selection changed function: Tree_2
        function Tree_2SelectionChanged(app, event)
            SelectedNode = app.Tree_2.SelectedNodes.NodeData;
            
            app.RemoveButton.Enable = 'on';
            app.AddButton.Enable = 'off';
            app.ClearAllButton.Enable = 'on';
            
            CheckValidate(app);
        end

        % Button pushed function: VALIDATECHANGESButton
        function VALIDATECHANGESButtonPushed(app, event)
            
            [NodeData_1,NodeData_2] = ExtractNodeData(app);

            switch app.Mode
                case 'MaskFile'
                    
                    NewMask = app.Mask;
                    
                    NewMask.MaskMap = zeros(size(NewMask.MaskMap));
                    NewMask.MaskProbability = zeros(size(NewMask.MaskProbability));
                    NewMask.Colors = zeros(size(NewMask.Colors));
                    
                    for i = 1:length(NodeData_2)
                        Idx = NodeData_2(i);
                        
                        NewMask.Names{i+1} = app.Mask.Names{Idx};
                        NewMask.Densities(i) = app.Mask.Densities(Idx-1);
                        
                        SelPx = find(app.Mask.MaskMap == Idx-1);
                        NewMask.MaskMap(SelPx) = i;
                        
                        NewMask.MaskProbability(:,:,i) = app.Mask.MaskProbability(:,:,Idx-1);
                        NewMask.Colors(i+1,:) = app.Mask.Colors(Idx,:);
                        
                        NewMask.SubMask(i) = app.Mask.SubMask(Idx-1);
                    end

                    app.XMapToolsApp.XMapToolsData.MapData.MaskFile.Masks(app.Selected) = NewMask;
                    
                    for i = 1:length(app.XMapToolsApp.TreeData_Additional.Children(1).Children(app.Selected).Children)
                        app.XMapToolsApp.TreeData_Additional.Children(1).Children(app.Selected).Children(i).Text = NewMask.Names{i};
                    end
            end
            
            RearrangerGUICloseRequest(app);

        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create RearrangerGUI and hide until all components are created
            app.RearrangerGUI = uifigure('Visible', 'off');
            app.RearrangerGUI.Position = [100 100 967 561];
            app.RearrangerGUI.Name = 'Rearranger – XMapTools';
            app.RearrangerGUI.CloseRequestFcn = createCallbackFcn(app, @RearrangerGUICloseRequest, true);

            % Create GridLayout
            app.GridLayout = uigridlayout(app.RearrangerGUI);
            app.GridLayout.ColumnWidth = {'0.5x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.5x'};
            app.GridLayout.RowHeight = {'0.1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.2x'};

            % Create Image
            app.Image = uiimage(app.GridLayout);
            app.Image.Layout.Row = [2 3];
            app.Image.Layout.Column = [2 9];
            app.Image.ImageSource = 'logo_xmap_final.png';

            % Create Tree_1
            app.Tree_1 = uitree(app.GridLayout);
            app.Tree_1.SelectionChangedFcn = createCallbackFcn(app, @Tree_1SelectionChanged, true);
            app.Tree_1.Layout.Row = [4 13];
            app.Tree_1.Layout.Column = [3 9];

            % Create Tree_2
            app.Tree_2 = uitree(app.GridLayout);
            app.Tree_2.SelectionChangedFcn = createCallbackFcn(app, @Tree_2SelectionChanged, true);
            app.Tree_2.Layout.Row = [4 13];
            app.Tree_2.Layout.Column = [14 20];

            % Create AddButton
            app.AddButton = uibutton(app.GridLayout, 'push');
            app.AddButton.ButtonPushedFcn = createCallbackFcn(app, @AddButtonPushed, true);
            app.AddButton.Icon = '056-plus.png';
            app.AddButton.IconAlignment = 'top';
            app.AddButton.FontWeight = 'bold';
            app.AddButton.Layout.Row = [6 7];
            app.AddButton.Layout.Column = [11 12];
            app.AddButton.Text = 'Add';

            % Create RemoveButton
            app.RemoveButton = uibutton(app.GridLayout, 'push');
            app.RemoveButton.ButtonPushedFcn = createCallbackFcn(app, @RemoveButtonPushed, true);
            app.RemoveButton.Icon = '057-minus.png';
            app.RemoveButton.IconAlignment = 'top';
            app.RemoveButton.FontWeight = 'bold';
            app.RemoveButton.Layout.Row = [8 9];
            app.RemoveButton.Layout.Column = [11 12];
            app.RemoveButton.Text = 'Remove';

            % Create ClearAllButton
            app.ClearAllButton = uibutton(app.GridLayout, 'push');
            app.ClearAllButton.ButtonPushedFcn = createCallbackFcn(app, @ClearAllButtonPushed, true);
            app.ClearAllButton.Icon = '058-error.png';
            app.ClearAllButton.IconAlignment = 'top';
            app.ClearAllButton.FontWeight = 'bold';
            app.ClearAllButton.Layout.Row = [10 11];
            app.ClearAllButton.Layout.Column = [11 12];
            app.ClearAllButton.Text = 'Clear All';

            % Create GridLayout3
            app.GridLayout3 = uigridlayout(app.GridLayout);
            app.GridLayout3.ColumnWidth = {'1x'};
            app.GridLayout3.RowHeight = {'1x'};
            app.GridLayout3.ColumnSpacing = 5;
            app.GridLayout3.RowSpacing = 5;
            app.GridLayout3.Padding = [70 20 70 20];
            app.GridLayout3.Layout.Row = [2 3];
            app.GridLayout3.Layout.Column = [13 21];

            % Create VALIDATECHANGESButton
            app.VALIDATECHANGESButton = uibutton(app.GridLayout3, 'push');
            app.VALIDATECHANGESButton.ButtonPushedFcn = createCallbackFcn(app, @VALIDATECHANGESButtonPushed, true);
            app.VALIDATECHANGESButton.Icon = '042-shuffle.png';
            app.VALIDATECHANGESButton.FontSize = 14;
            app.VALIDATECHANGESButton.FontWeight = 'bold';
            app.VALIDATECHANGESButton.Enable = 'off';
            app.VALIDATECHANGESButton.Layout.Row = 1;
            app.VALIDATECHANGESButton.Layout.Column = 1;
            app.VALIDATECHANGESButton.Text = 'VALIDATE CHANGES';

            % Show the figure after all components are created
            app.RearrangerGUI.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = Rearranger_exported(varargin)

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.RearrangerGUI)

            % Execute the startup function
            runStartupFcn(app, @(app)startupFcn(app, varargin{:}))

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.RearrangerGUI)
        end
    end
end