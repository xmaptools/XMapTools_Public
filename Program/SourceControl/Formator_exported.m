classdef Formator_exported < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        Select             matlab.ui.Figure
        GridLayout         matlab.ui.container.GridLayout
        Image              matlab.ui.control.Image
        Label              matlab.ui.control.Label
        Tree               matlab.ui.container.Tree
        CancelButton       matlab.ui.control.Button
        ApplyButton        matlab.ui.control.Button
        EntryT             matlab.ui.control.Label
        ManualFormatCB     matlab.ui.control.CheckBox
        ManualName         matlab.ui.control.EditField
        GridLayout2        matlab.ui.container.GridLayout
        AutomatedFormatCB  matlab.ui.control.CheckBox
        Image_ok           matlab.ui.control.Image
    end

    
    properties (Access = private)
        CallingApp
        Entry
        Labels
    end
    

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, CallingApp, Entry, Labels, Selected)
            
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
            
            app.CallingApp = CallingApp;
            
            for i = 1:length(Labels)
                p = uitreenode(app.Tree,'Text',char(Labels{i}));
            end
            
            IdxSel = find(ismember(Labels,Selected));
            if ~isempty(IdxSel)
                app.Tree.SelectedNodes = app.Tree.Children(IdxSel);
                app.Image_ok.ImageSource = 'Valid.png';
                
                
                if isprop(app.CallingApp,'SkipDateTimeformatconfirmationMenu')
                    if isequal(app.CallingApp.SkipDateTimeformatconfirmationMenu.Checked,1)
                                                
                        app.CallingApp.ExchangeFormator = app.Tree.SelectedNodes.Text;
            
                        SelectCloseRequest(app);
                        return
                    end
                end
            else
                app.Tree.SelectedNodes = app.Tree.Children(1);
                app.Image_ok.ImageSource = 'NotValid.png';
                app.AutomatedFormatCB.Text = 'Use format from list';
            end
            TreeSelectionChanged(app);
            
            app.Tree.Multiselect = 'off';
            
            app.EntryT.Text = char(Entry);
            
            app.Entry = Entry;
            app.Labels = Labels;
            
            movegui(app.Select,"center");
            app.Select.Visible = 'on';
            
        end

        % Close request function: Select
        function SelectCloseRequest(app, event)
            
            delete(app)
            
        end

        % Button pushed function: CancelButton
        function CancelButtonPushed(app, event)
            app.CallingApp.ExchangeFormator = [];
            SelectCloseRequest(app);
        end

        % Button pushed function: ApplyButton
        function ApplyButtonPushed(app, event)
            
            % Test
            
            if isequal(app.ManualFormatCB.Value,1)
                Format = app.ManualName.Value;
            else
                Format = char(app.Tree.SelectedNodes.Text);
            end
            
            try
                Test = datetime(app.Entry,'InputFormat',Format);
            catch ME
                uialert(app.Select,'Error, the format does not work','XMapTools')
                return
            end
            
            app.CallingApp.ExchangeFormator = Format;
            
            SelectCloseRequest(app);
        end

        % Selection changed function: Tree
        function TreeSelectionChanged(app, event)
            app.ManualName.Value = app.Tree.SelectedNodes.Text;
            
        end

        % Value changed function: AutomatedFormatCB
        function AutomatedFormatCBValueChanged(app, event)
            value = app.AutomatedFormatCB.Value;
            
            if value
                app.ManualFormatCB.Value = 0;
            else
                app.ManualFormatCB.Value = 1;
            end
        end

        % Value changed function: ManualFormatCB
        function ManualFormatCBValueChanged(app, event)
            value = app.ManualFormatCB.Value;
            
            if value
                app.AutomatedFormatCB.Value = 0;
            else
                app.AutomatedFormatCB.Value = 1;
            end
            
        end

        % Value changed function: ManualName
        function ManualNameValueChanged(app, event)
            
            app.AutomatedFormatCB.Value = 0;
            app.ManualFormatCB.Value = 1;
            
            app.Tree.SelectedNodes = [];
            
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
            app.Label.FontSize = 14;
            app.Label.FontWeight = 'bold';
            app.Label.Layout.Row = 3;
            app.Label.Layout.Column = [1 2];

            % Create Tree
            app.Tree = uitree(app.GridLayout);
            app.Tree.Multiselect = 'on';
            app.Tree.SelectionChangedFcn = createCallbackFcn(app, @TreeSelectionChanged, true);
            app.Tree.Layout.Row = [7 17];
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

            % Create EntryT
            app.EntryT = uilabel(app.GridLayout);
            app.EntryT.HorizontalAlignment = 'center';
            app.EntryT.Layout.Row = [4 5];
            app.EntryT.Layout.Column = [1 2];
            app.EntryT.Text = 'Entry';

            % Create ManualFormatCB
            app.ManualFormatCB = uicheckbox(app.GridLayout);
            app.ManualFormatCB.ValueChangedFcn = createCallbackFcn(app, @ManualFormatCBValueChanged, true);
            app.ManualFormatCB.Text = 'Manual Format Definition';
            app.ManualFormatCB.Layout.Row = 18;
            app.ManualFormatCB.Layout.Column = [1 2];

            % Create ManualName
            app.ManualName = uieditfield(app.GridLayout, 'text');
            app.ManualName.ValueChangedFcn = createCallbackFcn(app, @ManualNameValueChanged, true);
            app.ManualName.Layout.Row = 19;
            app.ManualName.Layout.Column = [1 2];

            % Create GridLayout2
            app.GridLayout2 = uigridlayout(app.GridLayout);
            app.GridLayout2.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout2.RowHeight = {'1x'};
            app.GridLayout2.ColumnSpacing = 5;
            app.GridLayout2.Padding = [0 0 0 0];
            app.GridLayout2.Layout.Row = 6;
            app.GridLayout2.Layout.Column = [1 2];

            % Create AutomatedFormatCB
            app.AutomatedFormatCB = uicheckbox(app.GridLayout2);
            app.AutomatedFormatCB.ValueChangedFcn = createCallbackFcn(app, @AutomatedFormatCBValueChanged, true);
            app.AutomatedFormatCB.Text = 'Automated Format Detection';
            app.AutomatedFormatCB.Layout.Row = 1;
            app.AutomatedFormatCB.Layout.Column = [1 7];
            app.AutomatedFormatCB.Value = true;

            % Create Image_ok
            app.Image_ok = uiimage(app.GridLayout2);
            app.Image_ok.Layout.Row = 1;
            app.Image_ok.Layout.Column = 9;
            app.Image_ok.ImageSource = 'NotValid.png';

            % Show the figure after all components are created
            app.Select.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = Formator_exported(varargin)

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