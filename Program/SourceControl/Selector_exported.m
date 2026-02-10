classdef Selector_exported < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        Select        matlab.ui.Figure
        GridLayout    matlab.ui.container.GridLayout
        Image         matlab.ui.control.Image
        Label         matlab.ui.control.Label
        Tree          matlab.ui.container.Tree
        CancelButton  matlab.ui.control.Button
        ApplyButton   matlab.ui.control.Button
    end

    
    properties (Access = private)
        XMapToolsApp
    end
    

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, XMapToolsApp, Names, Label, Mode)
            
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
            
            app.Select.Visible = 'off';
            
            for i = 1:length(Names)
                p = uitreenode(app.Tree,'Text',char(Names{i}),'UserData',i);
            end
            
            app.Label.Text = Label;
            app.XMapToolsApp = XMapToolsApp;
            
            if isequal(Mode,'Single')
                app.Tree.Multiselect = 'off';
            end
            
            movegui(app.Select,"center");
            app.Select.Visible = 'on';
            
        end

        % Close request function: Select
        function SelectCloseRequest(app, event)
            
            delete(app)
            
        end

        % Button pushed function: CancelButton
        function CancelButtonPushed(app, event)
            app.XMapToolsApp.ExchangeSelector = [];
            SelectCloseRequest(app);
        end

        % Button pushed function: ApplyButton
        function ApplyButtonPushed(app, event)
            for i = 1:length(app.Tree.SelectedNodes)
                Names{i} = app.Tree.SelectedNodes(i).Text;
                SelectedID(i) = app.Tree.SelectedNodes(i).UserData;
            end
            app.XMapToolsApp.ExchangeSelector = Names;
            app.XMapToolsApp.ExchangeSelectorId = SelectedID;
            
            SelectCloseRequest(app);
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create Select and hide until all components are created
            app.Select = uifigure('Visible', 'off');
            app.Select.Position = [100 100 308 727];
            app.Select.Name = 'Select';
            app.Select.CloseRequestFcn = createCallbackFcn(app, @SelectCloseRequest, true);

            % Create GridLayout
            app.GridLayout = uigridlayout(app.Select);
            app.GridLayout.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};

            % Create Image
            app.Image = uiimage(app.GridLayout);
            app.Image.Layout.Row = [1 2];
            app.Image.Layout.Column = [1 2];
            app.Image.ImageSource = 'logo_xmap_final.png';

            % Create Label
            app.Label = uilabel(app.GridLayout);
            app.Label.HorizontalAlignment = 'center';
            app.Label.Layout.Row = 3;
            app.Label.Layout.Column = [1 2];

            % Create Tree
            app.Tree = uitree(app.GridLayout);
            app.Tree.Multiselect = 'on';
            app.Tree.Layout.Row = [4 19];
            app.Tree.Layout.Column = [1 2];

            % Create CancelButton
            app.CancelButton = uibutton(app.GridLayout, 'push');
            app.CancelButton.ButtonPushedFcn = createCallbackFcn(app, @CancelButtonPushed, true);
            app.CancelButton.Layout.Row = 20;
            app.CancelButton.Layout.Column = 1;
            app.CancelButton.Text = 'Cancel';

            % Create ApplyButton
            app.ApplyButton = uibutton(app.GridLayout, 'push');
            app.ApplyButton.ButtonPushedFcn = createCallbackFcn(app, @ApplyButtonPushed, true);
            app.ApplyButton.Layout.Row = 20;
            app.ApplyButton.Layout.Column = 2;
            app.ApplyButton.Text = 'Apply';

            % Show the figure after all components are created
            app.Select.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = Selector_exported(varargin)

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.Select)

            % Execute the startup function
            runStartupFcn(app, @(app)startupFcn(app, varargin{:}))

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.Select)
        end
    end
end