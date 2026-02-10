classdef Help_Display_exported < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        XMapToolsDocumentationHelpUIFigure  matlab.ui.Figure
        GridLayout   matlab.ui.container.GridLayout
        Image        matlab.ui.control.Image
        GridLayout3  matlab.ui.container.GridLayout
        GridLayout2  matlab.ui.container.GridLayout
        HTML         matlab.ui.control.HTML
    end

    
    properties (Access = private)
        text % Description
        XMapToolsApp
        
        TestMode
    end
    
    properties (Access = public)
    end
    
    methods (Access = private)
        
        
        
        function FormatText2HTML(app)
            
            app.text = regexprep(app.text, '\n', '<br>'); 
            app.text = regexprep(app.text, ' ', '&nbsp;');
            
            Style1 = '<p style="font-family:monospace">';

            app.text = [Style1,app.text,'</p>'];
            
        end
        
        function text = ReadMFile(app,CheckFile)
            
            Temp = fileread(CheckFile);
            Test = strread(Temp,'%s','delimiter','\n');
            
            ReadingHeader = 1;
            
            Txt_Function = '';
            Txt_Header = '';
            
            for i = 2:size(Test,1)
                if isequal(ReadingHeader,0)
                    Txt_Function = [Txt_Function,char(Test{i}),newline];
                end
                if length(Test{i}) > 1
                    if isequal(Test{i}(1),'%') && isequal(ReadingHeader,1)
                        Txt_Header = [Txt_Header,char(Test{i}(2:end)),newline];
                    else
                        ReadingHeader = 0;
                    end
                end
            end
            
            text = [Txt_Header,newline,newline,'---------------------------------------',newline,'The source code is shown below',newline,'---------------------------------------',newline,Txt_Function];

        end
    end
    
    methods (Access = public)
        
        function UpdateTextHelp(app,HelpPage)

            if isequal(HelpPage(end-3:end),'html')
                app.HTML.HTMLSource = fullfile(app.XMapToolsApp.config.xmaptools.setup_path,'Dev','help',HelpPage);
                
                if app.TestMode 
                    disp(['app.HTML.HTMLSource = ',app.HTML.HTMLSource])
                end
            else
                try 
                    HelpPage = [HelpPage,'.m'];
                    
                    CheckFile = fullfile(app.XMapToolsApp.config.xmaptools.setup_path,'External','SF',HelpPage);
                    if exist(CheckFile)
                        app.text = ReadMFile(app,CheckFile);   %app.text = help(CheckFile);
                    end
                    
                    CheckFile = fullfile(app.XMapToolsApp.config.xmaptools.setup_path,'External','ME',HelpPage);
                    if exist(CheckFile)
                        app.text = ReadMFile(app,CheckFile);   %app.text = help(CheckFile);
                    end
                    
                    CheckFile = fullfile(app.XMapToolsApp.config.xmaptools.setup_path,'External','TB',HelpPage);
                    if exist(CheckFile)
                        app.text = ReadMFile(app,CheckFile);   %app.text = help(CheckFile);
                    end
                    
                    FormatText2HTML(app);
                    app.HTML.HTMLSource = app.text;
                    
                catch ME
                    disp(['File ',fullfile(app.XMapToolsApp.config.xmaptools.setup_path,'External','SF',HelpPage),' not found'])
                    delete(app)
                    return
                end
            end
            
            if app.TestMode
                disp('** Function UpdateTextHelp called from XMapTools')
                %disp(['app.HTML.HTMLSource = ',app.HTML.HTMLSource])
            end
            
            figure(app.XMapToolsDocumentationHelpUIFigure);
            
        end
    end
        

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, XMapToolsApp, HelpPage)
            
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
            
            
            app.XMapToolsDocumentationHelpUIFigure.Visible = 'off';
            
            app.TestMode = 0;
            
            movegui(app.XMapToolsDocumentationHelpUIFigure,'center'); 
            
            app.XMapToolsApp = XMapToolsApp;
            app.XMapToolsApp.Id_HelpTool = app;
            
            if app.TestMode 
                disp(' ')
                disp(' ')
                disp('Help module activated by user')
                disp(['File: ',HelpPage])
            end
            
            
            if isequal(HelpPage(end-3:end),'html')  
                app.HTML.HTMLSource = '';
                app.HTML.HTMLSource = fullfile(app.XMapToolsApp.config.xmaptools.setup_path,'Dev','help',HelpPage);
                
                if app.TestMode 
                    disp(['app.HTML.HTMLSource = ',app.HTML.HTMLSource])
                end
            else
                HelpPage = [HelpPage,'.m'];
                if app.TestMode
                    disp(['File changed to: ',HelpPage])
                end
                try 
                    CheckFile = fullfile(app.XMapToolsApp.config.xmaptools.setup_path,'External','SF',HelpPage);
                    if exist(CheckFile)
                        app.text = ReadMFile(app,CheckFile);   %app.text = help(CheckFile);
                    end
                    
                    CheckFile = fullfile(app.XMapToolsApp.config.xmaptools.setup_path,'External','ME',HelpPage);
                    if exist(CheckFile)
                        app.text = ReadMFile(app,CheckFile);   %app.text = help(CheckFile);
                    end
                    
                    CheckFile = fullfile(app.XMapToolsApp.config.xmaptools.setup_path,'External','TB',HelpPage);
                    if exist(CheckFile)
                        app.text = ReadMFile(app,CheckFile);   %app.text = help(CheckFile);
                    end
                    
                    FormatText2HTML(app);
                    app.HTML.HTMLSource = app.text;
                    
                catch 
                    disp(['File ',fullfile(app.XMapToolsApp.config.xmaptools.setup_path,'External','SF',HelpPage),' not found'])
                    delete(app)
                    return
                end
            end
            
            %app.HTML.HTMLSource = app.text;
            
            figure(app.XMapToolsDocumentationHelpUIFigure);
            
            app.XMapToolsDocumentationHelpUIFigure.Visible = 'on';
        end

        % Callback function
        function CloseButtonPushed(app, event)
            
            XMapToolsDocumentationHelpUIFigureCloseRequest(app, event)
            
        end

        % Close request function: XMapToolsDocumentationHelpUIFigure
        function XMapToolsDocumentationHelpUIFigureCloseRequest(app, event)
            
            app.XMapToolsApp.Id_HelpTool = [];
            delete(app)
            
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create XMapToolsDocumentationHelpUIFigure and hide until all components are created
            app.XMapToolsDocumentationHelpUIFigure = uifigure('Visible', 'off');
            app.XMapToolsDocumentationHelpUIFigure.Position = [100 100 757 544];
            app.XMapToolsDocumentationHelpUIFigure.Name = 'XMapTools Documentation & Help';
            app.XMapToolsDocumentationHelpUIFigure.CloseRequestFcn = createCallbackFcn(app, @XMapToolsDocumentationHelpUIFigureCloseRequest, true);

            % Create GridLayout
            app.GridLayout = uigridlayout(app.XMapToolsDocumentationHelpUIFigure);
            app.GridLayout.ColumnWidth = {'1x', '1x', '1x'};
            app.GridLayout.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};

            % Create Image
            app.Image = uiimage(app.GridLayout);
            app.Image.Layout.Row = 1;
            app.Image.Layout.Column = 1;
            app.Image.ImageSource = 'logo_xmap_final.png';

            % Create GridLayout3
            app.GridLayout3 = uigridlayout(app.GridLayout);
            app.GridLayout3.ColumnWidth = {'1x'};
            app.GridLayout3.RowHeight = {'1x'};
            app.GridLayout3.Padding = [70 12 70 12];
            app.GridLayout3.Layout.Row = 1;
            app.GridLayout3.Layout.Column = 3;

            % Create GridLayout2
            app.GridLayout2 = uigridlayout(app.GridLayout);
            app.GridLayout2.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout2.RowHeight = {'1x'};
            app.GridLayout2.Layout.Row = 1;
            app.GridLayout2.Layout.Column = 2;

            % Create HTML
            app.HTML = uihtml(app.GridLayout);
            app.HTML.Layout.Row = [2 9];
            app.HTML.Layout.Column = [1 3];

            % Show the figure after all components are created
            app.XMapToolsDocumentationHelpUIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = Help_Display_exported(varargin)

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.XMapToolsDocumentationHelpUIFigure)

            % Execute the startup function
            runStartupFcn(app, @(app)startupFcn(app, varargin{:}))

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.XMapToolsDocumentationHelpUIFigure)
        end
    end
end