classdef Generator_exported < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        GeneratorGUI            matlab.ui.Figure
        GridLayout              matlab.ui.container.GridLayout
        Image                   matlab.ui.control.Image
        TypeyourequationbelowEditFieldLabel  matlab.ui.control.Label
        Equation                matlab.ui.control.EditField
        UITable                 matlab.ui.control.Table
        MapsVariableNamesLabel  matlab.ui.control.Label
        SelectalistofoperationsinthelistbelowDropDownLabel  matlab.ui.control.Label
        RecipeMenu              matlab.ui.control.DropDown
        CalculateField          matlab.ui.control.Button
        CalculateList           matlab.ui.control.Button
        TextArea                matlab.ui.control.TextArea
        ListVariables_Field     matlab.ui.control.TextArea
        Eliminate_Button        matlab.ui.control.Button
    end

    
    properties (Access = private)
        GeneratorVariables
        XMapToolsApp 
        MapNames
        VariableNames
        Data
        ExcludeUnderscore
    end
    
    methods (Access = private)
        
        
        
        function [Success,Error,DataOut,NameOut] = CheckAndRunCode(app,Mode,TheCode)
            
            DataOut = [];
            NameOut = [];
            
            Error = '';
            Success = 0;
            
            Data = app.Data;
            VariableNames = app.VariableNames;
            MapNames = app.MapNames;
            
            % Find null pixels (2.6.1)
            ValSum = zeros(size(Data(1).Map));
            for i = 1:length(Data)
                ValSum = ValSum + Data(i).Map;
            end
            UndefinedPx = find(ValSum == 0);
            
            % Clean and remove the spaces
            TheCodeNoSpace = '';
            Compt = 0;
            for i=1:length(TheCode)
                if ~isequal(TheCode(i),' ')
                    Compt = Compt+1;
                    TheCodeNoSpace = [TheCodeNoSpace,TheCode(i)];
                end
            end
            
            % add the ";"
            if ~isequal(TheCodeNoSpace(end),';')
                TheCodeNoSpace(1,end+1) = ';';
            end
            
            % Check the output variable name:
            Str2 = strread(TheCodeNoSpace,'%s','delimiter','=');
            switch length(Str2)
                case 0
                    Error = 'ERROR: This is not a valid MATLAB code.';
                    %set(handles.DispError,'Visible','on','String',Error);
                    return
                case 1
                    Error = 'ERROR: ''='' is missing.';
                    %set(handles.DispError,'Visible','on','String',Error);
                    return
                case 2
                    
                otherwise
                    Error = 'ERROR';
                    %set(handles.DispError,'Visible','on','String',Error)
                    %keyboard
                    return
                    
            end
            
            OutputVariable = Str2{1};
            OutputName = OutputVariable;
            RawCode = Str2{2};
            
            if ~isvarname(OutputVariable)
                OutputVariable = genvarname(OutputVariable);
            end
            
            
            % Check that this map does not exist ...
            TestMapNames = find(ismember(MapNames,OutputName));
            TestVariableNames = find(ismember(VariableNames,OutputVariable));
            
            if TestMapNames
                
                Error = 'A map with this name already exist and cannot be replaced';
                return
         
            else
                Where = length(Data)+1;
                ReplaceMap = 0;
            end
            
            
            % Check RawCode and add spaces
            StrCode = RawCode;
            for i = 1:length(RawCode)
                
                % check for + and - operators or parentheses
                if isequal(RawCode(i),'+') || isequal(RawCode(i),'-') || isequal(RawCode(i),'(') || isequal(RawCode(i),')') || isequal(RawCode(i),';')
                    StrCode(i) = ' ';
                end
                
                if isequal(RawCode(i),'.')
                    if i+1 <= length(RawCode)
                        % Check for operators at i+1
                        if isequal(RawCode(i+1),'*') || isequal(RawCode(i+1),'/') || isequal(RawCode(i+1),'^')
                            StrCode(i) = ' ';
                            StrCode(i+1) = ' ';
                            Skip = 1;
                        end
                    else
                        Skip = 0;
                    end
                else
                    Skip = 0;
                end
                
                if ~Skip
                    % check for normal *, / and ^ operators
                    if isequal(RawCode(i),'*') || isequal(RawCode(i),'/') || isequal(RawCode(i),'^')
                        StrCode(i) = ' ';
                    end
                end
            end
            
            
            VarNum = strread(StrCode,'%s');
            ComptVar = 0;
            
            for i = 1:length(VarNum)
                Test = isstrprop(VarNum{i},'digit');
                Test2 = isstrprop(VarNum{i},'punct');
                
                % Check for digits with '.':
                WhereDot = find(Test2);
                if isequal(length(WhereDot),1)
                    Test = Test+Test2;
                end
                
                if ~isequal(VarNum{i},'log') && ~isequal(VarNum{i},'log10') && ~isequal(VarNum{i},'exp')
                    if ~isequal(sum(Test),length(Test))
                        % we have a variable, let's check:
                        if ~isvarname(VarNum{i})
                                Error = ['ERROR: "',VarNum{i},'" is not a valid variable name.'];
                                %set(handles.DispError,'Visible','on','String',Error);
                                return
                        end
                        
                        if ~ismember(VarNum{i},VariableNames)
                            Error = ['ERROR: "',VarNum{i},'" is not available.'];
                            %set(handles.DispError,'Visible','on','String',Error);
                            return
                        else
                            ComptVar = ComptVar+1;
                            VarList{ComptVar} = VarNum{i};
                        end
                    end
                end
            end
            
            
            % If we are here we can continue and generate the variables...
            for i = 1:length(Data);
                eval([char(VariableNames{i}),' = Data(i).Map;']);
            end
            
            try
                %disp(['NewVariable = ',RawCode])
                eval(['NewVariable = ',RawCode])
                
            catch ME
                %keyboard
                Error = ME.message
                %set(handles.DispError,'Visible','on','String',Error);
                return
            end
            
            switch Mode
                
                case 'Single'
                    
                    eval(['NewVariable = ',RawCode])
                    
                    disp(['  # New variable: ',char(OutputVariable), ' (map: ',char(OutputName),')']);
                    
                    WhereNaN = find(isnan(NewVariable));
                    WhereInf = find(isinf(NewVariable));
                    
                    if length(WhereNaN)
                        NewVariable(WhereNaN) = zeros(size(WhereNaN));
                        disp(['  ',num2str(length(WhereNaN)),' NaNs have been replaced by Zeros']);
                    end
                    
                    if length(WhereInf)
                        NewVariable(WhereInf) = zeros(size(WhereInf));
                        disp(['  ',num2str(length(WhereInf)),' Inf have been replaced by Zeros']);
                    end
                    
                    % new 1.6.2
                    if length(UndefinedPx)
                        NewVariable(UndefinedPx) = zeros(size(UndefinedPx));
                        disp(['  ',num2str(length(UndefinedPx)),' undefined values have been replaced by Zeros']);
                    end
                    
                    disp(' ')
                    
                    
                    DataOut = NewVariable;
                    NameOut = OutputName;
            end
            
            Success = 1;
        end
        
        
        
        function GenerateVariableNames(app)
            app.VariableNames = cell(size(app.MapNames));
            VarTable = cell(length(app.MapNames),2);
            
            for i=1:length(app.MapNames)
                Name = char(app.MapNames{i});
                if isequal(app.ExcludeUnderscore,1)
                    
                    Und = find(ismember(Name,'_'));
                    if ~isempty(Und)
                        Name = Name(1:Und(1)-1);
                    end
                    
                end
                VarTable{i,1} = Name;
                VarTable{i,2} = genvarname(char(Name));
                
                app.MapNames{i} = VarTable{i,1};
                app.VariableNames{i} = VarTable{i,2};
            end
            
            app.UITable.Data = VarTable;
        end
        
        function WriteInXMapTools(app,DataOut,NameOut,SelectResultInXMapTools)
                
                NodeData = app.XMapToolsApp.TreeData_Main.SelectedNodes.NodeData;
                
                switch NodeData(1)
                    case {1,2,3} 
                        
                        % Write in Other (in the future request?)
                        
                        Idx = length(app.XMapToolsApp.XMapToolsData.MapData.Ot.Names) + 1;
                        app.XMapToolsApp.XMapToolsData.MapData.Ot.Names{Idx} = NameOut;
                        app.XMapToolsApp.XMapToolsData.MapData.Ot.Types(Idx) = 1;
                        app.XMapToolsApp.XMapToolsData.MapData.Ot.Data(Idx).Map = DataOut;
                        
                        p = uitreenode(app.XMapToolsApp.Node_Ot,'Text',char(app.XMapToolsApp.XMapToolsData.MapData.Ot.Names{Idx}),'NodeData',[5,Idx]);
                
                        if SelectResultInXMapTools
                            app.XMapToolsApp.TreeData_Main.SelectedNodes =  app.XMapToolsApp.Node_Ot.Children(Idx);
                        end
                        
                    case 4
                        Pos = NodeData(2);
                        Idx = length(app.XMapToolsApp.XMapToolsData.MapData.Re.Data(Pos).Labels)+1;
                        
                        app.XMapToolsApp.XMapToolsData.MapData.Re.Data(Pos).Labels{Idx} = NameOut;
                        app.XMapToolsApp.XMapToolsData.MapData.Re.Data(Pos).CData(Idx).Map = DataOut;
                        
                        p = uitreenode(app.XMapToolsApp.Node_Re.Children(NodeData(2)),'Text',char(app.XMapToolsApp.XMapToolsData.MapData.Re.Data(Pos).Labels{Idx}),'NodeData',[4,NodeData(2),Idx]);
                
                        if SelectResultInXMapTools
                            app.XMapToolsApp.TreeData_Main.SelectedNodes =  app.XMapToolsApp.Node_Re.Children(NodeData(2)).Children(Idx);
                        end
                        
                       
                        
                end
            
            
            
        end
    end
    

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, XMapToolsApp, Names, Data)
            %
            % XMapTools is a free software solution for the analysis of chemical maps
            % Copyright © 2022-2025 University of Bern, Institute of Geological Sciences, Pierre Lanari
            %
            % XMapTools is free software: you can redistribute it and/or modify
            % it under the terms of the GNU General Public License as published by
            % the Free Software Foundation, either version 3 of the License, or any
            % later version.
            %
            % XMapTools is distributed in the hope that it will be useful,
            % but WITHOUT ANY WARRANTY; without even the implied warranty of
            % MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
            % GNU General Public License for more details.
            %
            % You should have received a copy of the GNU General Public License
            % along with XMapTools. If not, see https://www.gnu.org/licenses.
            
            
            app.GeneratorGUI.Visible = 'off';
            
            movegui(app.GeneratorGUI,'center');
            
            app.XMapToolsApp = XMapToolsApp;
            app.MapNames = Names;
            app.Data = Data;
            
            GenerateVariableNames(app);
            
            % Variables definition for GENERATOR:
            fid = fopen('Xmap_VarDefinition.txt','r');
            Compt = 0;
            
            while 1
                tline = fgets(fid);
                if isequal(tline,-1)
                    break
                end
                
                if length(tline) > 1
                    if isequal(tline(1),'>')
                        Compt = Compt+1;
                        app.GeneratorVariables(Compt).Title = tline(3:end-1);
                        
                        ComptVar = 0;
                        while 1
                            tline = fgets(fid);
                            if length(tline) > 3
                                ComptVar = ComptVar+1;
                                Code = strread(tline,'%s','delimiter','');
                                %                     while isequal(Code(end),sprintf('\n'))
                                %                         disp('Format issue with the file Xmap_VarDefinition.txt')
                                %                         Code = Code(1:end-1);
                                %                         keyboard
                                %                     end
                                app.GeneratorVariables(Compt).Code{ComptVar} = char(Code);
                            else
                                break
                            end
                        end
                    end
                end
            end
            fclose(fid);
            
            % Update DefaultProperties
            app.RecipeMenu.Items = extractfield(app.GeneratorVariables,'Title');
            app.RecipeMenu.ItemsData = [1:length(app.GeneratorVariables)];
            
            RecipeMenuValueChanged(app);
            
            app.ExcludeUnderscore = 0;
            
            app.GeneratorGUI.Visible = 'on';
            
        end

        % Value changed function: RecipeMenu
        function RecipeMenuValueChanged(app, event)
            
            RecipeValue = app.RecipeMenu.Value;
            
            
            
            %keyboard
            
            Compt = 0;
            CodeFinal = '';
            for i = 1:length(app.GeneratorVariables(RecipeValue).Code)
                [Success,Error] = CheckAndRunCode(app,'Check',app.GeneratorVariables(RecipeValue).Code{i});
                
                if isequal(Success,1) && isempty(Error)
                    Compt = Compt+1;
                    CodeFinal{Compt} = app.GeneratorVariables(RecipeValue).Code{i};
                end
            end
            
            app.ListVariables_Field.Value = CodeFinal;
            
            
            
            
            
            
        end

        % Button pushed function: Eliminate_Button
        function Eliminate_ButtonPushed(app, event)
            app.ExcludeUnderscore = 1;
            GenerateVariableNames(app);
            RecipeMenuValueChanged(app);
        end

        % Button pushed function: CalculateField
        function CalculateFieldButtonPushed(app, event)
            
            [Success,Error,DataOut,NameOut] = CheckAndRunCode(app, 'Single', app.Equation.Value);
            
            if isequal(Success,1)
                % To be saved before to close 
                % Here we are...
                
                WriteInXMapTools(app,DataOut,NameOut,1);
                
                GeneratorGUICloseRequest(app);
                %keyboard
            else
                errordlg(Error);
            end
            
        end

        % Button pushed function: CalculateList
        function CalculateListButtonPushed(app, event)
            
            ListCode = app.ListVariables_Field.Value;
            
            Compt = 0;
            for i = 1:length(ListCode)
                
                [Success,Error,DataOut,NameOut] = CheckAndRunCode(app, 'Single', char(ListCode{i}));
                
                if isequal(Success,1)
                    Compt = Compt+1;
                    Results(Compt).DataOut = DataOut;
                    Results(Compt).NameOut = NameOut;
                else
                    errordlg(Error);
                end
                
            end
            
            if Compt > 0
                for i = 1:length(Results)
                    
                    if i < length(Results)
                        WriteInXMapTools(app,Results(i).DataOut,Results(i).NameOut,0);
                    else
                        WriteInXMapTools(app,Results(i).DataOut,Results(i).NameOut,1);
                    end
                    
                    
                end
                GeneratorGUICloseRequest(app);
            end
            
            
            
        end

        % Close request function: GeneratorGUI
        function GeneratorGUICloseRequest(app, event)
            delete(app)
            
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create GeneratorGUI and hide until all components are created
            app.GeneratorGUI = uifigure('Visible', 'off');
            app.GeneratorGUI.Position = [100 100 967 561];
            app.GeneratorGUI.Name = 'Generator – XMapTools';
            app.GeneratorGUI.CloseRequestFcn = createCallbackFcn(app, @GeneratorGUICloseRequest, true);

            % Create GridLayout
            app.GridLayout = uigridlayout(app.GeneratorGUI);
            app.GridLayout.ColumnWidth = {'0.5x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.5x'};
            app.GridLayout.RowHeight = {'0.1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.2x'};

            % Create Image
            app.Image = uiimage(app.GridLayout);
            app.Image.Layout.Row = [2 3];
            app.Image.Layout.Column = [2 9];
            app.Image.ImageSource = 'logo_xmap_final.png';

            % Create TypeyourequationbelowEditFieldLabel
            app.TypeyourequationbelowEditFieldLabel = uilabel(app.GridLayout);
            app.TypeyourequationbelowEditFieldLabel.HorizontalAlignment = 'center';
            app.TypeyourequationbelowEditFieldLabel.VerticalAlignment = 'bottom';
            app.TypeyourequationbelowEditFieldLabel.FontWeight = 'bold';
            app.TypeyourequationbelowEditFieldLabel.Layout.Row = 4;
            app.TypeyourequationbelowEditFieldLabel.Layout.Column = [2 15];
            app.TypeyourequationbelowEditFieldLabel.Text = 'Type your equation below';

            % Create Equation
            app.Equation = uieditfield(app.GridLayout, 'text');
            app.Equation.HorizontalAlignment = 'center';
            app.Equation.FontWeight = 'bold';
            app.Equation.Layout.Row = 5;
            app.Equation.Layout.Column = [2 15];
            app.Equation.Value = 'XMg = Mg./(Mg+Fe);';

            % Create UITable
            app.UITable = uitable(app.GridLayout);
            app.UITable.ColumnName = {'Map'; 'Variable name'};
            app.UITable.RowName = {};
            app.UITable.Layout.Row = [3 11];
            app.UITable.Layout.Column = [17 21];

            % Create MapsVariableNamesLabel
            app.MapsVariableNamesLabel = uilabel(app.GridLayout);
            app.MapsVariableNamesLabel.HorizontalAlignment = 'center';
            app.MapsVariableNamesLabel.VerticalAlignment = 'bottom';
            app.MapsVariableNamesLabel.FontWeight = 'bold';
            app.MapsVariableNamesLabel.Layout.Row = 2;
            app.MapsVariableNamesLabel.Layout.Column = [17 21];
            app.MapsVariableNamesLabel.Text = 'Maps & Variable Names';

            % Create SelectalistofoperationsinthelistbelowDropDownLabel
            app.SelectalistofoperationsinthelistbelowDropDownLabel = uilabel(app.GridLayout);
            app.SelectalistofoperationsinthelistbelowDropDownLabel.HorizontalAlignment = 'center';
            app.SelectalistofoperationsinthelistbelowDropDownLabel.VerticalAlignment = 'bottom';
            app.SelectalistofoperationsinthelistbelowDropDownLabel.FontWeight = 'bold';
            app.SelectalistofoperationsinthelistbelowDropDownLabel.Layout.Row = 9;
            app.SelectalistofoperationsinthelistbelowDropDownLabel.Layout.Column = [2 15];
            app.SelectalistofoperationsinthelistbelowDropDownLabel.Text = 'Select a list of operations in the list below';

            % Create RecipeMenu
            app.RecipeMenu = uidropdown(app.GridLayout);
            app.RecipeMenu.ValueChangedFcn = createCallbackFcn(app, @RecipeMenuValueChanged, true);
            app.RecipeMenu.Layout.Row = 10;
            app.RecipeMenu.Layout.Column = [2 15];

            % Create CalculateField
            app.CalculateField = uibutton(app.GridLayout, 'push');
            app.CalculateField.ButtonPushedFcn = createCallbackFcn(app, @CalculateFieldButtonPushed, true);
            app.CalculateField.Icon = '042-shuffle.png';
            app.CalculateField.IconAlignment = 'top';
            app.CalculateField.FontWeight = 'bold';
            app.CalculateField.Layout.Row = [6 7];
            app.CalculateField.Layout.Column = [14 15];
            app.CalculateField.Text = 'Generate';

            % Create CalculateList
            app.CalculateList = uibutton(app.GridLayout, 'push');
            app.CalculateList.ButtonPushedFcn = createCallbackFcn(app, @CalculateListButtonPushed, true);
            app.CalculateList.Icon = '042-shuffle.png';
            app.CalculateList.IconAlignment = 'top';
            app.CalculateList.FontWeight = 'bold';
            app.CalculateList.Layout.Row = [11 12];
            app.CalculateList.Layout.Column = [14 15];
            app.CalculateList.Text = 'Generate';

            % Create TextArea
            app.TextArea = uitextarea(app.GridLayout);
            app.TextArea.Editable = 'off';
            app.TextArea.FontSize = 10;
            app.TextArea.Layout.Row = [6 7];
            app.TextArea.Layout.Column = [2 9];
            app.TextArea.Value = {'Use array operations:'; ' - division: ./'; ' - multiplication: .*'; ' - power: .^'; ''; 'The dot notation is used for element-wise operations on arrays'};

            % Create ListVariables_Field
            app.ListVariables_Field = uitextarea(app.GridLayout);
            app.ListVariables_Field.FontSize = 10;
            app.ListVariables_Field.Layout.Row = [11 13];
            app.ListVariables_Field.Layout.Column = [2 10];
            app.ListVariables_Field.Value = {'Calculations go here'};

            % Create Eliminate_Button
            app.Eliminate_Button = uibutton(app.GridLayout, 'push');
            app.Eliminate_Button.ButtonPushedFcn = createCallbackFcn(app, @Eliminate_ButtonPushed, true);
            app.Eliminate_Button.FontSize = 9;
            app.Eliminate_Button.Layout.Row = 12;
            app.Eliminate_Button.Layout.Column = [20 21];
            app.Eliminate_Button.Text = 'Eliminate "_"';

            % Show the figure after all components are created
            app.GeneratorGUI.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = Generator_exported(varargin)

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.GeneratorGUI)

            % Execute the startup function
            runStartupFcn(app, @(app)startupFcn(app, varargin{:}))

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.GeneratorGUI)
        end
    end
end