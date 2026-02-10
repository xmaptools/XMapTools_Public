classdef About_XMapTools_exported < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        XMapTools_Info  matlab.ui.Figure
        GridLayout      matlab.ui.container.GridLayout
        Image           matlab.ui.control.Image
        VersionLabel    matlab.ui.control.Label
        Copyright       matlab.ui.control.Label
        Information     matlab.ui.control.Label
    end

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, XMapToolsapp)
            
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
            
            
            app.XMapTools_Info.Visible = 'off';
            
            app.VersionLabel.Text = XMapToolsapp.XMapTools_version.Text;
            
            movegui(app.XMapTools_Info,'center');
            
            app.XMapTools_Info.Visible = 'on';
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create XMapTools_Info and hide until all components are created
            app.XMapTools_Info = uifigure('Visible', 'off');
            app.XMapTools_Info.Position = [100 100 595 439];
            app.XMapTools_Info.Name = 'About – XMapTools';
            app.XMapTools_Info.Icon = 'xmaptools_ios_icon_HR.png';

            % Create GridLayout
            app.GridLayout = uigridlayout(app.XMapTools_Info);
            app.GridLayout.ColumnWidth = {'0.3x', '1x', '1x', '1x', '1x', '1x', '1x', '0.3x'};
            app.GridLayout.RowHeight = {'0.5x', '1x', '1x', '0.6x', '1x', '0.5x', '1.5x', '1x', '1x', '1x', '1x'};

            % Create Image
            app.Image = uiimage(app.GridLayout);
            app.Image.Layout.Row = [2 3];
            app.Image.Layout.Column = [2 7];
            app.Image.ImageSource = 'logo_xmap_final.png';

            % Create VersionLabel
            app.VersionLabel = uilabel(app.GridLayout);
            app.VersionLabel.HorizontalAlignment = 'center';
            app.VersionLabel.FontSize = 13;
            app.VersionLabel.FontWeight = 'bold';
            app.VersionLabel.FontAngle = 'italic';
            app.VersionLabel.Layout.Row = 5;
            app.VersionLabel.Layout.Column = [2 7];
            app.VersionLabel.Text = 'Version';

            % Create Copyright
            app.Copyright = uilabel(app.GridLayout);
            app.Copyright.HorizontalAlignment = 'center';
            app.Copyright.VerticalAlignment = 'bottom';
            app.Copyright.FontSize = 10;
            app.Copyright.FontWeight = 'bold';
            app.Copyright.Layout.Row = 11;
            app.Copyright.Layout.Column = [2 7];
            app.Copyright.Text = '© 2021-2026, University of Lausanne, Institute of Earth Sciences, Pierre Lanari';

            % Create Information
            app.Information = uilabel(app.GridLayout);
            app.Information.VerticalAlignment = 'top';
            app.Information.WordWrap = 'on';
            app.Information.FontSize = 11;
            app.Information.Layout.Row = [7 10];
            app.Information.Layout.Column = [2 7];
            app.Information.Text = {'XMapTools is a free software solution for the analysis of chemical maps currently developed by Prof. Pierre Lanari at the University of Lausanne, Switzerland.  and previously at the University of Bern.'; ''; 'Main contributors: Pierre Lanari, Thorsten Markmann, Renée Tamblyn, Joshua Laughton, Philip Hartmeir, and Mahyra Tedeschi  '; ''; 'License: GNU General Public License v3.0'; ''; '  Website: https://xmaptools.ch '; 'Resources: https://resources.xmaptools.ch'; ' Github: https://github.com/xmaptools '; 'PTtoolbox (by J. Laughton): https://github.com/JoshuaLaughton/PTtoolbox '};

            % Show the figure after all components are created
            app.XMapTools_Info.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = About_XMapTools_exported(varargin)

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.XMapTools_Info)

            % Execute the startup function
            runStartupFcn(app, @(app)startupFcn(app, varargin{:}))

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.XMapTools_Info)
        end
    end
end