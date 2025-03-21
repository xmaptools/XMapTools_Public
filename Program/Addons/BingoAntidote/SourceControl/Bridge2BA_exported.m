classdef Bridge2BA_exported < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        Launcher                 matlab.ui.Figure
        GridLayout               matlab.ui.container.GridLayout
        Image                    matlab.ui.control.Image
        Text_Settings            matlab.ui.control.Label
        Text_Options             matlab.ui.control.Label
        TextArea                 matlab.ui.control.TextArea
        MergedmapDropDownLabel   matlab.ui.control.Label
        IsOk_Theriak             matlab.ui.control.Image
        Menu_Option1             matlab.ui.control.DropDown
        MaskfileDropDownLabel    matlab.ui.control.Label
        Menu_Option2             matlab.ui.control.DropDown
        DensitymapLabel          matlab.ui.control.Label
        Menu_Option3             matlab.ui.control.DropDown
        IsOk_Option1             matlab.ui.control.Image
        IsOk_Option2             matlab.ui.control.Image
        IsOk_Option3             matlab.ui.control.Image
        BingoAntidote_Button     matlab.ui.control.Button
        UITable                  matlab.ui.control.Table
        SetPathToTheriak_Button  matlab.ui.control.Button
    end

    
    properties (Access = private)
        XMapToolsApp
        
        Test_TD
        Test_1
        Test_2
        Test_3
        Version 
        WaitBar
    end
    
    methods (Access = private)
        
        function CheckTheriak(app)
            if ismac
                if isequal(exist([app.XMapToolsApp.config.bingoantidote.theriak_path,'/theriak']),2)
                    app.IsOk_Theriak.ImageSource = 'Valid.png';
                    app.Test_TD = 1;
                else
                    app.IsOk_Theriak.ImageSource = 'NotValid.png';
                    app.Test_TD = 0;
                end
            else
                if isequal(exist([app.XMapToolsApp.config.bingoantidote.theriak_path,'\theriak.exe']),2)
                    app.IsOk_Theriak.ImageSource = 'Valid.png';
                    app.Test_TD = 1;
                else
                    app.IsOk_Theriak.ImageSource = 'NotValid.png';
                    app.Test_TD = 0;
                end
            end
            
        end
        
        
        
        
        function CheckFinal(app)
            
            if app.Test_TD && app.Test_1 && app.Test_2 && app.Test_3
                app.BingoAntidote_Button.Enable = 'on';
            else
                app.BingoAntidote_Button.Enable = 'off';
            end
             
        end
    end
    

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, XMapToolsApp)
            app.Launcher.Visible = 'off';
            movegui(app.Launcher,'center');
            
            app.XMapToolsApp = XMapToolsApp;
            
            app.Version = 'Bingo-Antidote 2.1';
            
            % Update GUI
            %app.XMapToolsApp.config.bingoantidote.theriak_path; 
            if isequal(app.XMapToolsApp.config.bingoantidote.theriak_path,0)
                app.XMapToolsApp.config.bingoantidote.theriak_path = '';
            end
            TextValue = {app.Version,' ',app.XMapToolsApp.config.bingoantidote.theriak_path};
            app.TextArea.Value = TextValue;
            
            CheckTheriak(app);
            
            if ~isempty(app.XMapToolsApp.XMapToolsData.MapData.Me.Names)
                app.Menu_Option1.Items = app.XMapToolsApp.XMapToolsData.MapData.Me.Names;
                app.Menu_Option1.ItemsData = [1:length(app.XMapToolsApp.XMapToolsData.MapData.Me.Names)]; 
                app.IsOk_Option1.ImageSource = 'Valid.png';
                app.Test_1 = 1;
                Menu_Option1ValueChanged(app);
            else
                app.IsOk_Option1.ImageSource = 'NotValid.png';
                app.Menu_Option1.Items = {'not available'};
                app.Menu_Option1.Enable = 'off';
                app.Test_1 = 0;
            end
            
            if ~isempty(app.XMapToolsApp.XMapToolsData.MapData.MaskFile.Names)
                app.Menu_Option2.Items = app.XMapToolsApp.XMapToolsData.MapData.MaskFile.Names;
                app.Menu_Option2.ItemsData = [1:length(app.XMapToolsApp.XMapToolsData.MapData.MaskFile.Names)]; 
                app.IsOk_Option2.ImageSource = 'Valid.png';
                app.Test_2 = 1;
                % WE DO NOT CHECK COMPATIBILITY with app.XMapToolsApp.XMapToolsData.MapData.Me(1).MaskFile
            else
                app.IsOk_Option2.ImageSource = 'NotValid.png';
                app.Menu_Option2.Items = {'not available'};
                app.Menu_Option2.Enable = 'off';
                app.Test_2 = 0;
            end
            
            OtherNames = app.XMapToolsApp.XMapToolsData.MapData.Ot.Names;
            OtherTypes = app.XMapToolsApp.XMapToolsData.MapData.Ot.Types;
            
            IsDen = find(OtherTypes == 10);
            if ~isempty(IsDen)
                app.Menu_Option3.Items = app.XMapToolsApp.XMapToolsData.MapData.Ot.Names(IsDen);
                app.Menu_Option3.ItemsData = IsDen; 
                app.IsOk_Option3.ImageSource = 'Valid.png';
                app.Test_3 = 1;
            else
                app.IsOk_Option3.ImageSource = 'NotValid.png';
                app.Menu_Option3.Items = {'not available'};
                app.Menu_Option3.Enable = 'off';
                app.Test_3 = 0;
            end
            
            % app.XMapToolsApp.XMapToolsData.MapData.Me(1).MaskFile
            
            CheckFinal(app);
            app.Launcher.Visible = 'on';
        end

        % Button pushed function: BingoAntidote_Button
        function BingoAntidote_ButtonPressed(app, event)
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','Bingo-Antidote','Indeterminate','on');
            app.WaitBar.Message = 'Bingo-Antidote is starting, please wait...';
            
            % Preparing Data for Bingo-Antidote:
            MaskFile = app.XMapToolsApp.XMapToolsData.MapData.MaskFile.Masks(app.Menu_Option2.Value);
            
            MapData = app.XMapToolsApp.XMapToolsData.MapData.Me.Data(app.Menu_Option1.Value);

            UnSelected = find(~cell2mat(app.UITable.Data(:,2)));
            if ~isempty(UnSelected)
                MapData.ElNames(UnSelected) = [];
                MapData.ElInd(UnSelected) = [];
                MapData.CData(UnSelected) = [];
            end
            
            % DensityMap
            DensityMap = app.XMapToolsApp.XMapToolsData.MapData.Ot.Data(app.Menu_Option3.Value).Map;
            
            BingoAntidote(app.XMapToolsApp,MaskFile,MapData,DensityMap);
            
            pause(0.1)
            LauncherCloseRequest(app);
            
            
%             
%             
%             
%             SelMap = app.Menu_Option1.Value;
%             SelMask = app.Menu_Option2.Value;
%             SelDens = app.Menu_Option3.Value;
%             
%             ElNames = app.XMapToolsApp.XMapToolsData.MapData.Me.Data(SelMap).ElNames;
%             Selected = cell2mat(app.UITable.Data(:,2));
%             
%             TheQuanti.listname = upper(ElNames(Selected));
%             
%             Compt = 0;
%             for i = 1:length(ElNames)
%                 if Selected(i)
%                     Compt = Compt+1;
%                     TheQuanti.elem(Compt).quanti = app.XMapToolsApp.XMapToolsData.MapData.Me.Data(SelMap).CData(i).Map;
%                 end
%             end
%             
%             
%             
%             
%             
% 
%             XThermoTools_Path = [app.XMapToolsApp.config.xmaptools.setup_path,'/Addons/XThermoTools'];
%             
%             
%             XThermoTools(TheQuanti,TheMaskFile,XThermoTools_Path,app.XMapToolsApp.config.bingoantidote.theriak_path);
%             
%             
%             pause(0.1)
% 
%             LauncherCloseRequest(app)
            
            
            
            
        end

        % Value changed function: Menu_Option1
        function Menu_Option1ValueChanged(app, event)
            
            SelMap = app.Menu_Option1.Value;
            
            ElNames = app.XMapToolsApp.XMapToolsData.MapData.Me.Data(SelMap).ElNames;
            
            TableData = {};
            for i = 1:length(ElNames)
                TableData{i,1} = char(ElNames{i});
                if ~isequal(char(ElNames{i}),'Total(wt%)')
                    TableData{i,2} = true;
                else
                    TableData{i,2} = false;
                end
            end 
            
            app.UITable.Data = TableData;
            
        end

        % Close request function: Launcher
        function LauncherCloseRequest(app, event)
            delete(app)
            
        end

        % Button pushed function: SetPathToTheriak_Button
        function SetPathToTheriak_ButtonValueChanged(app, event)
            
            Path2Config = which('config_xmaptools.mat');
            load('config_xmaptools.mat');
            
            PathTher = app.XMapToolsApp.config.bingoantidote.theriak_path;
            
            f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
            %MosaicDirectory = uigetdir([config.xmaptools.setup_path,'Addons/XThermoTools/XTT_Programs/'], 'Theriak Directory');
            MosaicDirectory = uigetdir([cd], 'Theriak Directory');
            close(f);
            figure(app.Launcher);
            
            if ~isempty(MosaicDirectory)
                config.bingoantidote.theriak_path = MosaicDirectory; 
            end
            
            app.XMapToolsApp.config = config;
            save(Path2Config,'config');
            
            app.XMapToolsApp.config = config;
            
            % Update
            TextValue = {app.Version,' ',app.XMapToolsApp.config.bingoantidote.theriak_path};
            app.TextArea.Value = TextValue;
            
            % Check
            CheckTheriak(app);
            CheckFinal(app);
            
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create Launcher and hide until all components are created
            app.Launcher = uifigure('Visible', 'off');
            app.Launcher.Position = [100 100 745 565];
            app.Launcher.Name = 'Bingo-Antidote';
            app.Launcher.CloseRequestFcn = createCallbackFcn(app, @LauncherCloseRequest, true);

            % Create GridLayout
            app.GridLayout = uigridlayout(app.Launcher);
            app.GridLayout.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout.RowHeight = {'0.4x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.5x'};

            % Create Image
            app.Image = uiimage(app.GridLayout);
            app.Image.Layout.Row = [1 6];
            app.Image.Layout.Column = [2 5];
            app.Image.ImageSource = 'logo_transparent.png';

            % Create Text_Settings
            app.Text_Settings = uilabel(app.GridLayout);
            app.Text_Settings.FontSize = 16;
            app.Text_Settings.FontWeight = 'bold';
            app.Text_Settings.Layout.Row = 2;
            app.Text_Settings.Layout.Column = [7 16];
            app.Text_Settings.Text = 'Settings of Bingo-Antidote Add-on';

            % Create Text_Options
            app.Text_Options = uilabel(app.GridLayout);
            app.Text_Options.FontSize = 16;
            app.Text_Options.FontWeight = 'bold';
            app.Text_Options.Layout.Row = 8;
            app.Text_Options.Layout.Column = [3 17];
            app.Text_Options.Text = 'Importing Data from XMapTools to Bingo-Antidote';

            % Create TextArea
            app.TextArea = uitextarea(app.GridLayout);
            app.TextArea.Editable = 'off';
            app.TextArea.Layout.Row = [3 5];
            app.TextArea.Layout.Column = [7 18];
            app.TextArea.Value = {'XThermoTools version XXX'; ''; ''; 'TheriakDomino: Path...'};

            % Create MergedmapDropDownLabel
            app.MergedmapDropDownLabel = uilabel(app.GridLayout);
            app.MergedmapDropDownLabel.HorizontalAlignment = 'right';
            app.MergedmapDropDownLabel.Layout.Row = 9;
            app.MergedmapDropDownLabel.Layout.Column = [4 6];
            app.MergedmapDropDownLabel.Text = 'Merged map';

            % Create IsOk_Theriak
            app.IsOk_Theriak = uiimage(app.GridLayout);
            app.IsOk_Theriak.Layout.Row = 2;
            app.IsOk_Theriak.Layout.Column = 18;
            app.IsOk_Theriak.ImageSource = 'Valid.png';

            % Create Menu_Option1
            app.Menu_Option1 = uidropdown(app.GridLayout);
            app.Menu_Option1.ValueChangedFcn = createCallbackFcn(app, @Menu_Option1ValueChanged, true);
            app.Menu_Option1.Layout.Row = 9;
            app.Menu_Option1.Layout.Column = [7 14];

            % Create MaskfileDropDownLabel
            app.MaskfileDropDownLabel = uilabel(app.GridLayout);
            app.MaskfileDropDownLabel.HorizontalAlignment = 'right';
            app.MaskfileDropDownLabel.Layout.Row = 10;
            app.MaskfileDropDownLabel.Layout.Column = [4 6];
            app.MaskfileDropDownLabel.Text = 'Maskfile';

            % Create Menu_Option2
            app.Menu_Option2 = uidropdown(app.GridLayout);
            app.Menu_Option2.Layout.Row = 10;
            app.Menu_Option2.Layout.Column = [7 14];

            % Create DensitymapLabel
            app.DensitymapLabel = uilabel(app.GridLayout);
            app.DensitymapLabel.HorizontalAlignment = 'right';
            app.DensitymapLabel.Layout.Row = 11;
            app.DensitymapLabel.Layout.Column = [4 6];
            app.DensitymapLabel.Text = 'Density map';

            % Create Menu_Option3
            app.Menu_Option3 = uidropdown(app.GridLayout);
            app.Menu_Option3.Layout.Row = 11;
            app.Menu_Option3.Layout.Column = [7 14];

            % Create IsOk_Option1
            app.IsOk_Option1 = uiimage(app.GridLayout);
            app.IsOk_Option1.Layout.Row = 9;
            app.IsOk_Option1.Layout.Column = 16;
            app.IsOk_Option1.ImageSource = 'Valid.png';

            % Create IsOk_Option2
            app.IsOk_Option2 = uiimage(app.GridLayout);
            app.IsOk_Option2.Layout.Row = 10;
            app.IsOk_Option2.Layout.Column = 16;
            app.IsOk_Option2.ImageSource = 'Valid.png';

            % Create IsOk_Option3
            app.IsOk_Option3 = uiimage(app.GridLayout);
            app.IsOk_Option3.Layout.Row = 11;
            app.IsOk_Option3.Layout.Column = 16;
            app.IsOk_Option3.ImageSource = 'Valid.png';

            % Create BingoAntidote_Button
            app.BingoAntidote_Button = uibutton(app.GridLayout, 'push');
            app.BingoAntidote_Button.ButtonPushedFcn = createCallbackFcn(app, @BingoAntidote_ButtonPressed, true);
            app.BingoAntidote_Button.FontSize = 14;
            app.BingoAntidote_Button.FontWeight = 'bold';
            app.BingoAntidote_Button.Layout.Row = [14 15];
            app.BingoAntidote_Button.Layout.Column = [12 18];
            app.BingoAntidote_Button.Text = 'Open Bingo-Antidote';

            % Create UITable
            app.UITable = uitable(app.GridLayout);
            app.UITable.ColumnName = {'Map'; 'Selected'};
            app.UITable.RowName = {};
            app.UITable.ColumnEditable = [false true];
            app.UITable.Layout.Row = [12 15];
            app.UITable.Layout.Column = [2 6];

            % Create SetPathToTheriak_Button
            app.SetPathToTheriak_Button = uibutton(app.GridLayout, 'push');
            app.SetPathToTheriak_Button.ButtonPushedFcn = createCallbackFcn(app, @SetPathToTheriak_ButtonValueChanged, true);
            app.SetPathToTheriak_Button.Layout.Row = 6;
            app.SetPathToTheriak_Button.Layout.Column = [15 18];
            app.SetPathToTheriak_Button.Text = 'Set Path to Theriak';

            % Show the figure after all components are created
            app.Launcher.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = Bridge2BA_exported(varargin)

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.Launcher)

            % Execute the startup function
            runStartupFcn(app, @(app)startupFcn(app, varargin{:}))

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.Launcher)
        end
    end
end