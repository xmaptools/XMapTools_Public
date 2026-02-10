classdef Update_XMapTools_exported < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        Update_XMapTools_GUI   matlab.ui.Figure
        GridLayout             matlab.ui.container.GridLayout
        Image                  matlab.ui.control.Image
        AnewreleaseofXMapToolsisavailableLabel  matlab.ui.control.Label
        Copyright              matlab.ui.control.Label
        CodeToCopyEditField    matlab.ui.control.EditField
        CopythecodebelowLabel  matlab.ui.control.Label
        CopyCodeButton         matlab.ui.control.Button
        InstructionsLabel      matlab.ui.control.Label
        AbortandresumeusingXMapToolnotrecommendedButton  matlab.ui.control.Button
        CloseXMapToolsButton   matlab.ui.control.Button
        UpgradenowtothelatestversionLabel  matlab.ui.control.Label
        OpenhelpwebCheckBox    matlab.ui.control.CheckBox
    end

    
    properties (Access = private)
        XMapToolsApp 
        SkipOption
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
            
            app.Update_XMapTools_GUI.Visible = 'off';
            app.XMapToolsApp = XMapToolsApp;
            
            movegui(app.Update_XMapTools_GUI,'center');
            
            if ispc
                app.CodeToCopyEditField.Value = 'iex "& { $(irm https://xmaptools.ch/install.ps1) } --update"';
                app.CodeToCopyEditField.FontSize = 12;
                app.InstructionsLabel.FontSize = 14;
                app.InstructionsLabel.Text = {'Instructions:','- Press the button Close XMapTools -->','- Open a PowerShell as an administrator (right-click on the application)','- Paste the code, and press Enter to update.'};
            else
                app.CodeToCopyEditField.Value = 'curl -fsSL https://xmaptools.ch/install.sh | bash -s -- --update';
                app.InstructionsLabel.Text = {'Instructions:','- Press the button Close XMapTools -->','- Open a terminal window, paste the code, and press Enter to update.'};
            end
            
            app.Update_XMapTools_GUI.Visible = 'on';
        end

        % Button pushed function: CopyCodeButton
        function CopyCodeButtonPushed(app, event)
            
            clipboard('copy',app.CodeToCopyEditField.Value);
            
        end

        % Button pushed function: CloseXMapToolsButton
        function CloseXMapToolsButtonPushed(app, event)
            if isequal(app.OpenhelpwebCheckBox.Value,1)
                if ispc
                    web('https://xmaptools.ch/install-update/#update-windows')
                else
                    web('https://xmaptools.ch/install-update/#update-macos')
                end
            end
            delete(app);
        end

        % Button pushed function: 
        % AbortandresumeusingXMapToolnotrecommendedButton
        function AbortandresumeusingXMapToolnotrecommendedButtonPushed(app, event)
            app.XMapToolsApp.XMapTools_SkipUpdate = 1;
            delete(app);
        end

        % Close request function: Update_XMapTools_GUI
        function Update_XMapTools_GUICloseRequest(app, event)
            delete(app);
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create Update_XMapTools_GUI and hide until all components are created
            app.Update_XMapTools_GUI = uifigure('Visible', 'off');
            app.Update_XMapTools_GUI.Position = [100 100 778 549];
            app.Update_XMapTools_GUI.Name = 'Update – XMapTools';
            app.Update_XMapTools_GUI.Icon = 'xmaptools_ios_icon_HR.png';
            app.Update_XMapTools_GUI.CloseRequestFcn = createCallbackFcn(app, @Update_XMapTools_GUICloseRequest, true);

            % Create GridLayout
            app.GridLayout = uigridlayout(app.Update_XMapTools_GUI);
            app.GridLayout.ColumnWidth = {'0.1x', '0.5x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.5x', '0.1x'};
            app.GridLayout.RowHeight = {'0.1x', '1x', '1x', '0.5x', '1x', '1x', '0.5x', '1x', '1x', '0.7x', '0.5x', '1x', '0.5x', '1x', '0.1x'};

            % Create Image
            app.Image = uiimage(app.GridLayout);
            app.Image.Layout.Row = [2 3];
            app.Image.Layout.Column = [2 7];
            app.Image.ImageSource = 'logo_xmap_final.png';

            % Create AnewreleaseofXMapToolsisavailableLabel
            app.AnewreleaseofXMapToolsisavailableLabel = uilabel(app.GridLayout);
            app.AnewreleaseofXMapToolsisavailableLabel.HorizontalAlignment = 'center';
            app.AnewreleaseofXMapToolsisavailableLabel.FontSize = 18;
            app.AnewreleaseofXMapToolsisavailableLabel.Layout.Row = 2;
            app.AnewreleaseofXMapToolsisavailableLabel.Layout.Column = [8 14];
            app.AnewreleaseofXMapToolsisavailableLabel.Text = 'A new release of XMapTools is available.';

            % Create Copyright
            app.Copyright = uilabel(app.GridLayout);
            app.Copyright.HorizontalAlignment = 'center';
            app.Copyright.VerticalAlignment = 'bottom';
            app.Copyright.FontAngle = 'italic';
            app.Copyright.Layout.Row = 14;
            app.Copyright.Layout.Column = [2 14];
            app.Copyright.Text = '© 2021-2025, University of Lausanne, Institute of Earth Sciences, Pierre Lanari';

            % Create CodeToCopyEditField
            app.CodeToCopyEditField = uieditfield(app.GridLayout, 'text');
            app.CodeToCopyEditField.Editable = 'off';
            app.CodeToCopyEditField.HorizontalAlignment = 'center';
            app.CodeToCopyEditField.FontSize = 14;
            app.CodeToCopyEditField.Layout.Row = 6;
            app.CodeToCopyEditField.Layout.Column = [3 11];
            app.CodeToCopyEditField.Value = 'curl -fsSL https://xmaptools.ch/install.sh | bash -s -- --update';

            % Create CopythecodebelowLabel
            app.CopythecodebelowLabel = uilabel(app.GridLayout);
            app.CopythecodebelowLabel.FontSize = 16;
            app.CopythecodebelowLabel.FontWeight = 'bold';
            app.CopythecodebelowLabel.Layout.Row = 5;
            app.CopythecodebelowLabel.Layout.Column = [3 11];
            app.CopythecodebelowLabel.Text = 'Copy the code below';

            % Create CopyCodeButton
            app.CopyCodeButton = uibutton(app.GridLayout, 'push');
            app.CopyCodeButton.ButtonPushedFcn = createCallbackFcn(app, @CopyCodeButtonPushed, true);
            app.CopyCodeButton.FontSize = 15;
            app.CopyCodeButton.FontWeight = 'bold';
            app.CopyCodeButton.Layout.Row = 6;
            app.CopyCodeButton.Layout.Column = [12 13];
            app.CopyCodeButton.Text = 'Copy Code';

            % Create InstructionsLabel
            app.InstructionsLabel = uilabel(app.GridLayout);
            app.InstructionsLabel.FontSize = 16;
            app.InstructionsLabel.FontWeight = 'bold';
            app.InstructionsLabel.Layout.Row = [8 9];
            app.InstructionsLabel.Layout.Column = [3 11];
            app.InstructionsLabel.Text = {'Instructions:'; '- Press the button Close XMapTools'; '- open a terminal, and paste the code to update'};

            % Create AbortandresumeusingXMapToolnotrecommendedButton
            app.AbortandresumeusingXMapToolnotrecommendedButton = uibutton(app.GridLayout, 'push');
            app.AbortandresumeusingXMapToolnotrecommendedButton.ButtonPushedFcn = createCallbackFcn(app, @AbortandresumeusingXMapToolnotrecommendedButtonPushed, true);
            app.AbortandresumeusingXMapToolnotrecommendedButton.FontSize = 16;
            app.AbortandresumeusingXMapToolnotrecommendedButton.Layout.Row = 12;
            app.AbortandresumeusingXMapToolnotrecommendedButton.Layout.Column = [3 13];
            app.AbortandresumeusingXMapToolnotrecommendedButton.Text = 'Abort and resume using XMapTool (not recommended)';

            % Create CloseXMapToolsButton
            app.CloseXMapToolsButton = uibutton(app.GridLayout, 'push');
            app.CloseXMapToolsButton.ButtonPushedFcn = createCallbackFcn(app, @CloseXMapToolsButtonPushed, true);
            app.CloseXMapToolsButton.FontSize = 15;
            app.CloseXMapToolsButton.FontWeight = 'bold';
            app.CloseXMapToolsButton.Layout.Row = [8 9];
            app.CloseXMapToolsButton.Layout.Column = [12 13];
            app.CloseXMapToolsButton.Text = {'Close'; 'XMapTools'};

            % Create UpgradenowtothelatestversionLabel
            app.UpgradenowtothelatestversionLabel = uilabel(app.GridLayout);
            app.UpgradenowtothelatestversionLabel.HorizontalAlignment = 'center';
            app.UpgradenowtothelatestversionLabel.FontSize = 14;
            app.UpgradenowtothelatestversionLabel.FontAngle = 'italic';
            app.UpgradenowtothelatestversionLabel.Layout.Row = 3;
            app.UpgradenowtothelatestversionLabel.Layout.Column = [8 14];
            app.UpgradenowtothelatestversionLabel.Text = 'Upgrade now to the latest version!';

            % Create OpenhelpwebCheckBox
            app.OpenhelpwebCheckBox = uicheckbox(app.GridLayout);
            app.OpenhelpwebCheckBox.Text = 'Open help (web)';
            app.OpenhelpwebCheckBox.Layout.Row = 10;
            app.OpenhelpwebCheckBox.Layout.Column = [12 14];
            app.OpenhelpwebCheckBox.Value = true;

            % Show the figure after all components are created
            app.Update_XMapTools_GUI.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = Update_XMapTools_exported(varargin)

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.Update_XMapTools_GUI)

            % Execute the startup function
            runStartupFcn(app, @(app)startupFcn(app, varargin{:}))

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.Update_XMapTools_GUI)
        end
    end
end