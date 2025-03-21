classdef LogGenerator_exported < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        LogGeneratorGUI                 matlab.ui.Figure
        GridLayout                      matlab.ui.container.GridLayout
        Image                           matlab.ui.control.Image
        TabGroup                        matlab.ui.container.TabGroup
        MapSettingsTab                  matlab.ui.container.Tab
        GridLayout2                     matlab.ui.container.GridLayout
        XstartingpositionLabel          matlab.ui.control.Label
        X_startEditField                matlab.ui.control.NumericEditField
        YstartingpositionLabel          matlab.ui.control.Label
        Y_startEditField                matlab.ui.control.NumericEditField
        LaserdiametermEditFieldLabel_2  matlab.ui.control.Label
        LaserDiameterEditField          matlab.ui.control.NumericEditField
        ScanspeedmsLabel                matlab.ui.control.Label
        LaserDiameterEditField_2        matlab.ui.control.NumericEditField
        FileNamesTab                    matlab.ui.container.Tab
        GridLayout6                     matlab.ui.container.GridLayout
        NamepositionEditFieldLabel      matlab.ui.control.Label
        FileNames_NamepositionEditField  matlab.ui.control.NumericEditField
        DelimiterEditFieldLabel         matlab.ui.control.Label
        FileNames_DelimiterEditField    matlab.ui.control.EditField
        FileNames_Label                 matlab.ui.control.Label
        FileNames_ApplytoallButton      matlab.ui.control.Button
        FileNames_SplitatthelastCheckBox  matlab.ui.control.CheckBox
        orLabel                         matlab.ui.control.Label
        FileNames_KeepOnlyLetters       matlab.ui.control.Button
        UITable                         matlab.ui.control.Table
        UITableSignalSelection          matlab.ui.control.Table
        Panel                           matlab.ui.container.Panel
        TabGroup2                       matlab.ui.container.TabGroup
        SignalDefinitionTab             matlab.ui.container.Tab
        GridLayout3                     matlab.ui.container.GridLayout
        SelectedFileName                matlab.ui.control.Label
        InfoSweep                       matlab.ui.control.Label
        Sweep_StartLabel                matlab.ui.control.Label
        Sweep1Field                     matlab.ui.control.NumericEditField
        Sweep_EndLabel                  matlab.ui.control.Label
        Sweep2Field                     matlab.ui.control.NumericEditField
        TypeDropDownLabel               matlab.ui.control.Label
        TypeDropDown                    matlab.ui.control.DropDown
        ResetTableButton                matlab.ui.control.Button
        AddROIButton                    matlab.ui.control.Button
        AddtotableButton                matlab.ui.control.Button
        ResetROIButton                  matlab.ui.control.Button
        APPLYTab                        matlab.ui.container.Tab
        GridLayout4                     matlab.ui.container.GridLayout
        ApplytoSelectedButton           matlab.ui.control.Button
        ApplytoSameNameButton           matlab.ui.control.Button
        ApplytoAllButton                matlab.ui.control.Button
        GridLayout5                     matlab.ui.container.GridLayout
        GENERATELOGButton               matlab.ui.control.Button
        HelpButton                      matlab.ui.control.Button
        Plot                            matlab.ui.control.UIAxes
        ContextMenu                     matlab.ui.container.ContextMenu
        RenameMenu                      matlab.ui.container.Menu
        ContextMenu2                    matlab.ui.container.ContextMenu
        EliminateRowMenu                matlab.ui.container.Menu
    end

    
    properties (Access = private)
        
        XMapToolsApp 
        AppConverter
        
        DataFiles 
        SelectedCells 
        SelectedCells_Table2
        
        ROI_Object
        ROI_Listener
        
        Log 
        WaitBar
        
    end
    
    methods (Access = private)
        
        
        
        function ROI_changed(app,~)
            
            if app.ROI_Object.Position(1) < 1
                app.Sweep1Field.Value = 1;
            else
                app.Sweep1Field.Value = round(app.ROI_Object.Position(1));
            end
            
            MaxLines = size(app.DataFiles(app.SelectedCells(1)).DataCps,1);
            
            if round(app.ROI_Object.Position(1)+app.ROI_Object.Position(3)) > MaxLines
                app.Sweep2Field.Value = MaxLines;
            else
                app.Sweep2Field.Value = round(app.ROI_Object.Position(1)+app.ROI_Object.Position(3));
            end
            
            
        end
        
        function Str = ExtractCodeFromTable(app)
            
            TableData = app.UITableSignalSelection.Data;
            
            Str = '';
            for i = 1:size(TableData,1)
                Str = [Str,TableData{i,4},':',num2str(TableData{i,2}),',',num2str(TableData{i,3}),';'];
            end
            Str = Str(1:end-1);
            
        end
        
        function CheckTableState(app)
            
            Count = 0;
            for i = 1:size(app.UITable.Data,1)
                if ~isempty(app.UITable.Data{i,3})
                    Count = Count+1;
                end
            end
            
            if isequal(Count,size(app.UITable.Data,1))
                app.GENERATELOGButton.Enable = 'on';
            else
                app.GENERATELOGButton.Enable = 'off';
            end
            
        end
        
        function Sequence = ReadSequence(app,Name,Mode,Str,ScanPosition)
            
            Str2 = strread(Str,'%s','delimiter',';');
            NbInt = length(Str2);
            Count = 0;
            
            for it = 1:NbInt
                Str3 = strread(Str2{it},'%s','delimiter',':');
                
                Type = Str3{1};
                Lims = strread(Str3{2},'%f','delimiter',',');
                
                % First Position
                Count = Count + 1;
                if isequal(Lims(1),1)
                    Sequence(Count).TimePosition = Lims(1);
                else
                    Sequence(Count).TimePosition = Lims(1)-1;
                end
                switch Type
                    case 'Background'
                        Sequence(Count).LaserState = 'Off';
                    case 'Measurement'
                        Sequence(Count).LaserState = 'Off'; % Changed (first of the duplicate) !!!!
                end
                
                X1 = app.X_startEditField.Value;
                Y1 = app.Y_startEditField.Value+(app.LaserDiameterEditField.Value*(ScanPosition-1));
                
                if isequal(Mode,'Scan')
                    Sequence(Count).XY = [X1,Y1];
                    Sequence(Count).SpotSize = app.LaserDiameterEditField.Value;
                    Sequence(Count).ScanSpeed = app.LaserDiameterEditField_2.Value;
                else
                    Sequence(Count).XY = [0,0];
                    Sequence(Count).SpotSize = app.LaserDiameterEditField.Value;
                    Sequence(Count).ScanSpeed = app.LaserDiameterEditField_2.Value;
                end
                
                % First Position
                Count = Count + 1;
                if isequal(Lims(1),1)
                    Sequence(Count).TimePosition = Lims(1)+1;
                else
                    Sequence(Count).TimePosition = Lims(1);
                end
                switch Type
                    case 'Background'
                        Sequence(Count).LaserState = 'Off';
                    case 'Measurement'
                        Sequence(Count).LaserState = 'On';
                end
                
                if isequal(Mode,'Scan')
                    Sequence(Count).XY = [X1,Y1];
                    Sequence(Count).SpotSize = app.LaserDiameterEditField.Value;
                    Sequence(Count).ScanSpeed = app.LaserDiameterEditField_2.Value;
                else
                    Sequence(Count).XY = [0,0];
                    Sequence(Count).SpotSize = app.LaserDiameterEditField.Value;
                    Sequence(Count).ScanSpeed = app.LaserDiameterEditField_2.Value;
                end
                
                % Last Position - 1
                Count = Count + 1;
                Sequence(Count).TimePosition = Lims(2)-1;
                switch Type
                    case 'Background'
                        Sequence(Count).LaserState = 'Off';
                    case 'Measurement'
                        Sequence(Count).LaserState = 'On';
                end
                
                if isequal(Mode,'Scan')
                    dt = app.DataFiles(1).dt*(Lims(2)-Lims(1));     % in seconds
                    
                    X2 = X1+Sequence(Count-1).ScanSpeed*dt;
                    Y2 = Y1;
                    
                    Sequence(Count).XY = [X2,Y2];
                    Sequence(Count).SpotSize = app.LaserDiameterEditField.Value;
                    Sequence(Count).ScanSpeed = app.LaserDiameterEditField_2.Value;
                else
                    Sequence(Count).XY = [0,0];
                    Sequence(Count).SpotSize = app.LaserDiameterEditField.Value;
                    Sequence(Count).ScanSpeed = app.LaserDiameterEditField_2.Value;
                end
                
                
                % Last Position (laser off)
                Count = Count + 1;
                Sequence(Count).TimePosition = Lims(2);
                Sequence(Count).LaserState = 'Off';
                
                if isequal(Mode,'Scan')
                    Sequence(Count).XY = [X2,Y2];
                    Sequence(Count).SpotSize = app.LaserDiameterEditField.Value;
                    Sequence(Count).ScanSpeed = app.LaserDiameterEditField_2.Value;
                else
                    Sequence(Count).XY = [0,0];
                    Sequence(Count).SpotSize = app.LaserDiameterEditField.Value;
                    Sequence(Count).ScanSpeed = app.LaserDiameterEditField_2.Value;
                end
            end
            
        end
        
        function CheckButtonState(app)
            
            if size(app.UITableSignalSelection.Data,1) >= 2
                app.ApplytoAllButton.Enable  = 'on';
                app.ApplytoSameNameButton.Enable  = 'on';
                app.ApplytoSelectedButton.Enable  = 'on';
            else
                app.ApplytoAllButton.Enable  = 'off';
                app.ApplytoSameNameButton.Enable  = 'off';
                app.ApplytoSelectedButton.Enable  = 'off';
            end
            
        end
        
        function NewFileName = EditFileNameApply(app,FileName)
            TheStr = strread(FileName,'%s','delimiter',app.FileNames_DelimiterEditField.Value);
            if length(TheStr) > 1 && isequal(app.FileNames_SplitatthelastCheckBox.Value,1)
                Name = '';
                for i = 1:length(TheStr)-1
                    Name = [Name,TheStr{i},app.FileNames_DelimiterEditField.Value];
                end
                NewFileName = Name(1:end-1);
            else
                if length(TheStr) >= app.FileNames_NamepositionEditField.Value
                    NewFileName = TheStr{app.FileNames_NamepositionEditField.Value};
                else
                    NewFileName = 'ERROR';
                end
            end
        end
        
        function DeactivatePlotZoomPanOptions(app)
            
            matlab.graphics.interaction.webmodes.toggleMode(app.Plot,'pan','off')
            matlab.graphics.interaction.webmodes.toggleMode(app.Plot,'zoom','off')
            matlab.graphics.interaction.webmodes.toggleMode(app.Plot,'zoomout','off')
            
        end
    end
    

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, AppConverter, DataFiles, XMapToolsApp)
            
            % XMapTools is a free software solution for the analysis of chemical maps
            % Copyright © 2022-2025 University of Lausanne, Institute of Earth Sciences, Pierre Lanari
            
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
            
            app.LogGeneratorGUI.Visible = 'off';
            
            movegui(app.LogGeneratorGUI,"center");
            
            app.LogGeneratorGUI.Visible = 'on';
            
            app.Plot.Visible = 'off';
            app.ResetROIButton.Enable  = 'off';
            app.AddROIButton.Enable  = 'off';
            app.ResetTableButton.Enable  = 'off';
            app.AddtotableButton.Enable  = 'off';
            
            app.ApplytoAllButton.Enable  = 'off';
            app.ApplytoSameNameButton.Enable  = 'off';
            app.ApplytoSelectedButton.Enable  = 'off';
            
            app.XMapToolsApp = XMapToolsApp;
            app.AppConverter = AppConverter;
            app.DataFiles = DataFiles;
            
            app.SelectedCells = [];
            app.SelectedCells_Table2 = [];
            
            app.UITable.ColumnFormat = {'char',{'Scan','Spot'},'char'};
            
            Data4Table = cell(length(app.DataFiles),3);
            
            for i = 1:length(app.DataFiles)
                Data4Table{i,1} = app.DataFiles(i).FileName(1:end-4);
                Data4Table{i,2} = 'Scan';
            end
            
            app.UITable.Data = Data4Table;
            
            NewFileName = EditFileNameApply(app,app.UITable.Data{1,1});
            app.FileNames_Label.Text = ['e.g. ',app.UITable.Data{1,1},' -> ',NewFileName];
            
            CheckTableState(app)
            CheckButtonState(app)
            
            % keyboard
        end

        % Menu selected function: RenameMenu
        function RenameMenuSelected(app, event)
            
            DefName = char(app.UITable.Data(app.SelectedCells(1,1),1));
            Str = strread(DefName,'%s','delimiter','-');
            if iscell(Str)
                DefName = Str{end};
            end
            
            answer = inputdlg('Enter new value','XMapTools',1,{DefName});
            
            for i = 1:size(app.SelectedCells,1)
                app.UITable.Data(app.SelectedCells(i,1),app.SelectedCells(i,2)) = answer;
            end
            
        end

        % Cell selection callback: UITable
        function UITableCellSelection(app, event)
            app.SelectedCells = event.Indices;
            
            if isequal(size(app.SelectedCells,1),1)
                if isequal(app.SelectedCells(2),1)
                    Idx = app.SelectedCells(1);
                    
                    SignalSum = sum(app.DataFiles(Idx).DataCps,2);
                    
                    plot(app.Plot,SignalSum,'-')
                    app.Plot.YScale = 'log';
                    
                    app.SelectedFileName.Text = app.DataFiles(Idx).FileName;
                    app.InfoSweep.Text = ['with ',num2str(length(SignalSum)),' sweeps'];
                    
                    app.Plot.Visible = 'on';
                    app.ResetROIButton.Enable  = 'on';
                    app.AddROIButton.Enable  = 'on';
                    
                    app.AddtotableButton.Enable  = 'on';
                    app.ResetTableButton.Enable  = 'on';
                    
                    tb = axtoolbar(app.Plot,{'pan','zoomin','zoomout','restoreview'});
                    disableDefaultInteractivity(app.Plot);
                    
                end
            end
            
        end

        % Cell edit callback: UITable
        function UITableCellEdit(app, event)
            indices = event.Indices;
            newData = event.NewData;
            
            if isequal(size(indices,1),1)
                if isequal(indices(2),2)
                    Name = app.UITable.Data(indices(1),1);
                    ListNames = app.UITable.Data(:,1);
                    
                    Idx = find(ismember(ListNames,Name));
                    
                    if length(Idx) > 1
                        Answer = uiconfirm(app.LogGeneratorGUI,'Do you want to apply this change to all measurements with identical names?','XMapTools','Options',{'Yes','No'},'DefaultOption','Yes');
                        for i = 1:length(Idx)
                            app.UITable.Data(Idx(i),2) = {newData};
                        end
                    else
                        for i = 1:length(ListNames)
                            TempName = char(ListNames(i));
                            Split = find(ismember(TempName,'-'));
                            if ~isempty(Split)
                                ListNames{i} = TempName(1:Split(end)-1);
                            end
                        end
                        
                        % ListNames
                        
                        Idx = find(ismember(ListNames,Name));
                        
                        if length(Idx) > 1
                            Answer = uiconfirm(app.LogGeneratorGUI,'Do you want to apply this change to all measurements with identical names?','XMapTools','Options',{'Yes','No'},'DefaultOption','Yes');
                            for i = 1:length(Idx)
                                app.UITable.Data(Idx(i),2) = {newData};
                            end
                        end
                    end
                end
            end
        end

        % Menu selected function: EliminateRowMenu
        function EliminateRowMenuSelected(app, event)
            
            if ~isempty(app.SelectedCells_Table2)
                
                Data4Table = app.UITableSignalSelection.Data;
                
                Data4Table(app.SelectedCells_Table2(1,1),:) = [];
                
                app.UITableSignalSelection.Data = Data4Table;
            end
            
            CheckButtonState(app);
            
        end

        % Button pushed function: ApplytoAllButton
        function ApplytoAllButtonPushed(app, event)
            app.WaitBar = uiprogressdlg(app.LogGeneratorGUI,'Title','XMapTools');
            app.WaitBar.Message = 'Applying settings to all, please wait';
            for i = 1:size(app.UITable.Data(Selected,1))
                app.UITable.Data(Selected(i,1),3) = {Str};
                app.WaitBar.Value = i/size(app.UITable.Data(Selected,1));
            end
            close(app.WaitBar);
        end

        % Button pushed function: ApplytoSelectedButton
        function ApplytoSelectedButtonPushed(app, event)
            
            Str = ExtractCodeFromTable(app);
            
            if ~isempty(app.SelectedCells)
                for i = 1:size(app.SelectedCells,1)
                    app.UITable.Data(app.SelectedCells(i,1),3) = {Str};
                end
            end
            
            CheckTableState(app);
            
        end

        % Button pushed function: ApplytoSameNameButton
        function ApplytoSameNameButtonPushed(app, event)
            
            Str = ExtractCodeFromTable(app);
            
            SelName = char(app.UITable.Data(app.SelectedCells(1,1),1));
            Selected = find(ismember(app.UITable.Data(:,1),SelName));
            
            if ~isempty(Selected)
                app.WaitBar = uiprogressdlg(app.LogGeneratorGUI,'Title','XMapTools');
                app.WaitBar.Message = 'Applying settings to identical file names, please wait';
                for i = 1:size(Selected,1)
                    app.UITable.Data(Selected(i,1),3) = {Str};
                    app.WaitBar.Value = i/size(Selected,1);
                end
                close(app.WaitBar);
            else
                ListNames = app.UITable.Data(:,1);
                
                for i = 1:length(ListNames)
                    TempName = char(ListNames(i));
                    Split = find(ismember(TempName,'-'));
                    if ~isempty(Split)
                        ListNames{i} = TempName(1:Split(end)-1);
                    end
                end
                
                ListNames;
                
                Selected = find(ismember(ListNames,SelName));
                
                if ~isempty(Selected)
                    for i = 1:size(Selected,1)
                        app.UITable.Data(Selected(i,1),3) = {Str};
                    end
                end
            end
            
            CheckTableState(app);
            
        end

        % Button pushed function: ResetTableButton
        function ResetTableButtonPushed(app, event)
            
            app.UITableSignalSelection.Data = {};
            delete(findall(app.Plot, 'Type',  'images.roi.Rectangle'));
            
            CheckButtonState(app)
            
        end

        % Button pushed function: AddROIButton
        function AddROIButtonPushed(app, event)
            delete(findall(app.Plot, 'Type',  'images.roi.Rectangle'));
            
            DeactivatePlotZoomPanOptions(app);
            
            app.ROI_Object = drawrectangle(app.Plot,'Color',[0.57,0.00,0.69]);
            
            app.ROI_Listener = addlistener(app.ROI_Object, 'ROIMoved', @(varargin)ROI_changed(app, app.ROI_Object));
            
            % Extract data
            ROI_changed(app,app.ROI_Object);
            
        end

        % Button pushed function: AddtotableButton
        function AddtotableButtonPushed(app, event)
            
            Idx = size(app.UITableSignalSelection.Data,1)+1;
            
            app.UITableSignalSelection.Data{Idx,1} = Idx;
            app.UITableSignalSelection.Data{Idx,2}  = app.Sweep1Field.Value;
            app.UITableSignalSelection.Data{Idx,3}  = app.Sweep2Field.Value;
            app.UITableSignalSelection.Data(Idx,4)  = app.TypeDropDown.Items(app.TypeDropDown.Value);
            
            CheckButtonState(app)
            
        end

        % Button pushed function: GENERATELOGButton
        function GENERATELOGButtonPushed(app, event)
            
            TableData = app.UITable.Data;
            
            app.Log.Table.SequenceNumber = [];
            app.Log.Table.Comment = {};
            app.Log.Table.Timestamp = datetime;
            %app.Log.Table.DT_Log = datetime;
            %app.Log.Table.DT_Log_Corr = datetime;
            app.Log.Table.LaserState = {};
            app.Log.Table.X_um_ = [];
            app.Log.Table.Y_um_ = [];
            app.Log.Table.SpotSize_um_ = [];
            app.Log.Table.ScanVelocity_um_s_ = [];
            
            ScanPosition = 0;
            
            for iFile = 1:size(TableData,1)
                
                if isequal(TableData{iFile,2},'Scan')
                    ScanPosition = ScanPosition + 1;
                end
                
                Sequence = ReadSequence(app,TableData{iFile,1},TableData{iFile,2},TableData{iFile,3},ScanPosition);
                
                for i = 1:length(Sequence)
                    
                    Idx = length(app.Log.Table.SequenceNumber)+1;
                    
                    if isequal(i,1)
                        app.Log.Table.SequenceNumber(Idx,1) = i;
                        app.Log.Table.Comment{Idx,1} = [TableData{iFile,1},' - ',num2str(iFile)];
                    else
                        app.Log.Table.SequenceNumber(Idx,1) = nan(1);
                        app.Log.Table.Comment{Idx,1} = '';
                    end
                    app.Log.Table.Timestamp(Idx,1) = app.DataFiles(iFile).DT_Map(Sequence(i).TimePosition);
                    %app.Log.Table.DT_Log(Idx) = app.DataFiles(iFile).DT_Map(Sequence(i).TimePosition);
                    %app.Log.Table.DT_Log_Corr(Idx) = app.DataFiles(iFile).DT_Map(Sequence(i).TimePosition);
                    
                    app.Log.Table.LaserState{Idx,1} = Sequence(i).LaserState;
                    
                    app.Log.Table.X_um_(Idx,1) = Sequence(i).XY(1);
                    app.Log.Table.Y_um_(Idx,1) = Sequence(i).XY(2);
                    
                    app.Log.Table.SpotSize_um_(Idx,1) = Sequence(i).SpotSize;
                    app.Log.Table.ScanVelocity_um_s_(Idx,1) = Sequence(i).ScanSpeed;
                end
                
            end
            
            LogGeneratorGUICloseRequest(app, 1)
            
        end

        % Close request function: LogGeneratorGUI
        function LogGeneratorGUICloseRequest(app, event)
            
            
            app.AppConverter.Log = app.Log;
            
            %keyboard
            
            delete(app)
            
            
        end

        % Button pushed function: ResetROIButton
        function ResetROIButtonPushed(app, event)
            delete(findall(app.Plot, 'Type',  'images.roi.Rectangle'));
        end

        % Cell selection callback: UITableSignalSelection
        function UITableSignalSelectionCellSelection(app, event)
            app.SelectedCells_Table2 = event.Indices;
        end

        % Button pushed function: FileNames_ApplytoallButton
        function FileNames_ApplytoallButtonPushed(app, event)
            app.WaitBar = uiprogressdlg(app.LogGeneratorGUI,'Title','XMapTools');
            app.WaitBar.Message = 'Editing the names, please wait';
            for i = 1:size(app.UITable.Data,1)
                NewFileName = EditFileNameApply(app,app.UITable.Data{i,1});
                if ~isequal(NewFileName,'ERROR')
                    app.UITable.Data{i,1} = NewFileName;
                end
                app.WaitBar.Value = i/size(app.UITable.Data,1);
            end
            close(app.WaitBar);
        end

        % Value changed function: FileNames_NamepositionEditField
        function FileNames_NamepositionEditFieldValueChanged(app, event)
            NewFileName = EditFileNameApply(app,app.UITable.Data{1,1});
            app.FileNames_Label.Text = ['e.g. ',app.UITable.Data{1,1},' -> ',NewFileName];
        end

        % Value changed function: FileNames_DelimiterEditField
        function FileNames_DelimiterEditFieldValueChanged(app, event)
            NewFileName = EditFileNameApply(app,app.UITable.Data{1,1});
            app.FileNames_Label.Text = ['e.g. ',app.UITable.Data{1,1},' -> ',NewFileName];
        end

        % Value changed function: FileNames_SplitatthelastCheckBox
        function FileNames_SplitatthelastCheckBoxValueChanged(app, event)
            NewFileName = EditFileNameApply(app,app.UITable.Data{1,1});
            app.FileNames_Label.Text = ['e.g. ',app.UITable.Data{1,1},' -> ',NewFileName];
        end

        % Button pushed function: FileNames_KeepOnlyLetters
        function FileNames_KeepOnlyLettersPushed(app, event)
            app.WaitBar = uiprogressdlg(app.LogGeneratorGUI,'Title','XMapTools');
            app.WaitBar.Message = 'Editing the names, please wait';
            for i = 1:size(app.UITable.Data,1)
                OldFileName = app.UITable.Data{i,1};
                WhereLetter = find(isletter(OldFileName));
                app.UITable.Data{i,1} = OldFileName(WhereLetter);
                app.WaitBar.Value = i/size(app.UITable.Data,1);
            end
            close(app.WaitBar);
        end

        % Button pushed function: HelpButton
        function HelpButtonPushed(app, event)
            
            if isempty(app.XMapToolsApp.Id_HelpTool)
                Help_Display(app.XMapToolsApp,'Module_LogGenerator.html');
            else
                app.XMapToolsApp.Id_HelpTool.UpdateTextHelp('Module_LogGenerator.html');
            end
            
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create LogGeneratorGUI and hide until all components are created
            app.LogGeneratorGUI = uifigure('Visible', 'off');
            app.LogGeneratorGUI.Position = [100 100 1100 720];
            app.LogGeneratorGUI.Name = 'Log Generator – XMapTools';
            app.LogGeneratorGUI.CloseRequestFcn = createCallbackFcn(app, @LogGeneratorGUICloseRequest, true);

            % Create GridLayout
            app.GridLayout = uigridlayout(app.LogGeneratorGUI);
            app.GridLayout.ColumnWidth = {'1.5x', '0.03x', '1x', '0.03x', '1x', '0.03x', '1x'};
            app.GridLayout.RowHeight = {'0.5x', '0.1x', '0.2x', '1x', '0.1x', '1x', '0.1x', '1x', '0.1x', '1x'};

            % Create Image
            app.Image = uiimage(app.GridLayout);
            app.Image.Layout.Row = 1;
            app.Image.Layout.Column = 1;
            app.Image.ImageSource = 'logo_xmap_final.png';

            % Create TabGroup
            app.TabGroup = uitabgroup(app.GridLayout);
            app.TabGroup.Layout.Row = [2 4];
            app.TabGroup.Layout.Column = 1;

            % Create MapSettingsTab
            app.MapSettingsTab = uitab(app.TabGroup);
            app.MapSettingsTab.Title = 'Map Settings';

            % Create GridLayout2
            app.GridLayout2 = uigridlayout(app.MapSettingsTab);
            app.GridLayout2.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout2.RowHeight = {'1x', '1x', '1x', '1x'};
            app.GridLayout2.ColumnSpacing = 5;
            app.GridLayout2.RowSpacing = 5;

            % Create XstartingpositionLabel
            app.XstartingpositionLabel = uilabel(app.GridLayout2);
            app.XstartingpositionLabel.HorizontalAlignment = 'right';
            app.XstartingpositionLabel.Layout.Row = 1;
            app.XstartingpositionLabel.Layout.Column = [1 5];
            app.XstartingpositionLabel.Text = 'X (starting position)';

            % Create X_startEditField
            app.X_startEditField = uieditfield(app.GridLayout2, 'numeric');
            app.X_startEditField.Layout.Row = 1;
            app.X_startEditField.Layout.Column = [6 7];
            app.X_startEditField.Value = 1;

            % Create YstartingpositionLabel
            app.YstartingpositionLabel = uilabel(app.GridLayout2);
            app.YstartingpositionLabel.HorizontalAlignment = 'right';
            app.YstartingpositionLabel.Layout.Row = 2;
            app.YstartingpositionLabel.Layout.Column = [1 5];
            app.YstartingpositionLabel.Text = 'Y (starting position)';

            % Create Y_startEditField
            app.Y_startEditField = uieditfield(app.GridLayout2, 'numeric');
            app.Y_startEditField.Layout.Row = 2;
            app.Y_startEditField.Layout.Column = [6 7];
            app.Y_startEditField.Value = 1;

            % Create LaserdiametermEditFieldLabel_2
            app.LaserdiametermEditFieldLabel_2 = uilabel(app.GridLayout2);
            app.LaserdiametermEditFieldLabel_2.HorizontalAlignment = 'right';
            app.LaserdiametermEditFieldLabel_2.Layout.Row = 3;
            app.LaserdiametermEditFieldLabel_2.Layout.Column = [1 5];
            app.LaserdiametermEditFieldLabel_2.Text = 'Laser diameter (µm)';

            % Create LaserDiameterEditField
            app.LaserDiameterEditField = uieditfield(app.GridLayout2, 'numeric');
            app.LaserDiameterEditField.Layout.Row = 3;
            app.LaserDiameterEditField.Layout.Column = [6 7];
            app.LaserDiameterEditField.Value = 15;

            % Create ScanspeedmsLabel
            app.ScanspeedmsLabel = uilabel(app.GridLayout2);
            app.ScanspeedmsLabel.HorizontalAlignment = 'right';
            app.ScanspeedmsLabel.Layout.Row = 4;
            app.ScanspeedmsLabel.Layout.Column = [1 5];
            app.ScanspeedmsLabel.Text = 'Scan speed (µm/s)';

            % Create LaserDiameterEditField_2
            app.LaserDiameterEditField_2 = uieditfield(app.GridLayout2, 'numeric');
            app.LaserDiameterEditField_2.Layout.Row = 4;
            app.LaserDiameterEditField_2.Layout.Column = [6 7];
            app.LaserDiameterEditField_2.Value = 15;

            % Create FileNamesTab
            app.FileNamesTab = uitab(app.TabGroup);
            app.FileNamesTab.Title = 'File Names';

            % Create GridLayout6
            app.GridLayout6 = uigridlayout(app.FileNamesTab);
            app.GridLayout6.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout6.RowHeight = {'1x', '1x', '1x', '1x'};
            app.GridLayout6.ColumnSpacing = 5;
            app.GridLayout6.RowSpacing = 5;

            % Create NamepositionEditFieldLabel
            app.NamepositionEditFieldLabel = uilabel(app.GridLayout6);
            app.NamepositionEditFieldLabel.HorizontalAlignment = 'right';
            app.NamepositionEditFieldLabel.Layout.Row = 1;
            app.NamepositionEditFieldLabel.Layout.Column = [1 4];
            app.NamepositionEditFieldLabel.Text = 'Name position';

            % Create FileNames_NamepositionEditField
            app.FileNames_NamepositionEditField = uieditfield(app.GridLayout6, 'numeric');
            app.FileNames_NamepositionEditField.Limits = [1 Inf];
            app.FileNames_NamepositionEditField.ValueChangedFcn = createCallbackFcn(app, @FileNames_NamepositionEditFieldValueChanged, true);
            app.FileNames_NamepositionEditField.HorizontalAlignment = 'center';
            app.FileNames_NamepositionEditField.Layout.Row = 1;
            app.FileNames_NamepositionEditField.Layout.Column = 5;
            app.FileNames_NamepositionEditField.Value = 1;

            % Create DelimiterEditFieldLabel
            app.DelimiterEditFieldLabel = uilabel(app.GridLayout6);
            app.DelimiterEditFieldLabel.HorizontalAlignment = 'right';
            app.DelimiterEditFieldLabel.Layout.Row = 2;
            app.DelimiterEditFieldLabel.Layout.Column = [1 4];
            app.DelimiterEditFieldLabel.Text = 'Delimiter';

            % Create FileNames_DelimiterEditField
            app.FileNames_DelimiterEditField = uieditfield(app.GridLayout6, 'text');
            app.FileNames_DelimiterEditField.ValueChangedFcn = createCallbackFcn(app, @FileNames_DelimiterEditFieldValueChanged, true);
            app.FileNames_DelimiterEditField.HorizontalAlignment = 'center';
            app.FileNames_DelimiterEditField.Layout.Row = 2;
            app.FileNames_DelimiterEditField.Layout.Column = 5;
            app.FileNames_DelimiterEditField.Value = '-';

            % Create FileNames_Label
            app.FileNames_Label = uilabel(app.GridLayout6);
            app.FileNames_Label.HorizontalAlignment = 'center';
            app.FileNames_Label.FontSize = 11;
            app.FileNames_Label.Layout.Row = 3;
            app.FileNames_Label.Layout.Column = [1 10];
            app.FileNames_Label.Text = 'Label2';

            % Create FileNames_ApplytoallButton
            app.FileNames_ApplytoallButton = uibutton(app.GridLayout6, 'push');
            app.FileNames_ApplytoallButton.ButtonPushedFcn = createCallbackFcn(app, @FileNames_ApplytoallButtonPushed, true);
            app.FileNames_ApplytoallButton.Layout.Row = 2;
            app.FileNames_ApplytoallButton.Layout.Column = [6 10];
            app.FileNames_ApplytoallButton.Text = 'Apply to all';

            % Create FileNames_SplitatthelastCheckBox
            app.FileNames_SplitatthelastCheckBox = uicheckbox(app.GridLayout6);
            app.FileNames_SplitatthelastCheckBox.ValueChangedFcn = createCallbackFcn(app, @FileNames_SplitatthelastCheckBoxValueChanged, true);
            app.FileNames_SplitatthelastCheckBox.Text = 'Split at the last';
            app.FileNames_SplitatthelastCheckBox.Layout.Row = 1;
            app.FileNames_SplitatthelastCheckBox.Layout.Column = [7 10];

            % Create orLabel
            app.orLabel = uilabel(app.GridLayout6);
            app.orLabel.HorizontalAlignment = 'center';
            app.orLabel.Layout.Row = 1;
            app.orLabel.Layout.Column = 6;
            app.orLabel.Text = 'or';

            % Create FileNames_KeepOnlyLetters
            app.FileNames_KeepOnlyLetters = uibutton(app.GridLayout6, 'push');
            app.FileNames_KeepOnlyLetters.ButtonPushedFcn = createCallbackFcn(app, @FileNames_KeepOnlyLettersPushed, true);
            app.FileNames_KeepOnlyLetters.Layout.Row = 4;
            app.FileNames_KeepOnlyLetters.Layout.Column = [1 4];
            app.FileNames_KeepOnlyLetters.Text = 'Keep only letters';

            % Create UITable
            app.UITable = uitable(app.GridLayout);
            app.UITable.ColumnName = {'File Name'; 'Type'; 'Signal Definition'};
            app.UITable.RowName = {};
            app.UITable.ColumnEditable = [true true false];
            app.UITable.CellEditCallback = createCallbackFcn(app, @UITableCellEdit, true);
            app.UITable.CellSelectionCallback = createCallbackFcn(app, @UITableCellSelection, true);
            app.UITable.Layout.Row = [5 10];
            app.UITable.Layout.Column = 1;

            % Create UITableSignalSelection
            app.UITableSignalSelection = uitable(app.GridLayout);
            app.UITableSignalSelection.ColumnName = {'#'; 'Sweep_Start'; 'Sweep_End'; 'Type'};
            app.UITableSignalSelection.RowName = {};
            app.UITableSignalSelection.ColumnEditable = [false true true true];
            app.UITableSignalSelection.CellSelectionCallback = createCallbackFcn(app, @UITableSignalSelectionCellSelection, true);
            app.UITableSignalSelection.Layout.Row = [7 10];
            app.UITableSignalSelection.Layout.Column = [5 7];

            % Create Panel
            app.Panel = uipanel(app.GridLayout);
            app.Panel.Layout.Row = [7 10];
            app.Panel.Layout.Column = 3;

            % Create TabGroup2
            app.TabGroup2 = uitabgroup(app.Panel);
            app.TabGroup2.Position = [0 -1 221 293];

            % Create SignalDefinitionTab
            app.SignalDefinitionTab = uitab(app.TabGroup2);
            app.SignalDefinitionTab.Title = 'Signal Definition';

            % Create GridLayout3
            app.GridLayout3 = uigridlayout(app.SignalDefinitionTab);
            app.GridLayout3.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout3.RowHeight = {'1x', '0.7x', '0.2x', '1x', '0.2x', '1x', '1x', '1x', '0.2x', '1x'};
            app.GridLayout3.ColumnSpacing = 5;
            app.GridLayout3.RowSpacing = 5;

            % Create SelectedFileName
            app.SelectedFileName = uilabel(app.GridLayout3);
            app.SelectedFileName.HorizontalAlignment = 'center';
            app.SelectedFileName.FontWeight = 'bold';
            app.SelectedFileName.Layout.Row = 1;
            app.SelectedFileName.Layout.Column = [1 6];

            % Create InfoSweep
            app.InfoSweep = uilabel(app.GridLayout3);
            app.InfoSweep.HorizontalAlignment = 'center';
            app.InfoSweep.VerticalAlignment = 'top';
            app.InfoSweep.FontSize = 11;
            app.InfoSweep.Layout.Row = 2;
            app.InfoSweep.Layout.Column = [1 6];

            % Create Sweep_StartLabel
            app.Sweep_StartLabel = uilabel(app.GridLayout3);
            app.Sweep_StartLabel.HorizontalAlignment = 'right';
            app.Sweep_StartLabel.Layout.Row = 6;
            app.Sweep_StartLabel.Layout.Column = [1 3];
            app.Sweep_StartLabel.Text = 'Sweep_Start';

            % Create Sweep1Field
            app.Sweep1Field = uieditfield(app.GridLayout3, 'numeric');
            app.Sweep1Field.Layout.Row = 6;
            app.Sweep1Field.Layout.Column = [4 6];
            app.Sweep1Field.Value = 1;

            % Create Sweep_EndLabel
            app.Sweep_EndLabel = uilabel(app.GridLayout3);
            app.Sweep_EndLabel.HorizontalAlignment = 'right';
            app.Sweep_EndLabel.Layout.Row = 7;
            app.Sweep_EndLabel.Layout.Column = [1 3];
            app.Sweep_EndLabel.Text = 'Sweep_End';

            % Create Sweep2Field
            app.Sweep2Field = uieditfield(app.GridLayout3, 'numeric');
            app.Sweep2Field.Layout.Row = 7;
            app.Sweep2Field.Layout.Column = [4 6];
            app.Sweep2Field.Value = 2;

            % Create TypeDropDownLabel
            app.TypeDropDownLabel = uilabel(app.GridLayout3);
            app.TypeDropDownLabel.HorizontalAlignment = 'right';
            app.TypeDropDownLabel.Layout.Row = 8;
            app.TypeDropDownLabel.Layout.Column = [1 3];
            app.TypeDropDownLabel.Text = 'Type';

            % Create TypeDropDown
            app.TypeDropDown = uidropdown(app.GridLayout3);
            app.TypeDropDown.Items = {'Background', 'Measurement', 'Other laser on', 'Other laser off'};
            app.TypeDropDown.ItemsData = [1 2 3 4];
            app.TypeDropDown.Layout.Row = 8;
            app.TypeDropDown.Layout.Column = [4 6];
            app.TypeDropDown.Value = 1;

            % Create ResetTableButton
            app.ResetTableButton = uibutton(app.GridLayout3, 'push');
            app.ResetTableButton.ButtonPushedFcn = createCallbackFcn(app, @ResetTableButtonPushed, true);
            app.ResetTableButton.Layout.Row = 10;
            app.ResetTableButton.Layout.Column = [1 3];
            app.ResetTableButton.Text = 'Reset Table';

            % Create AddROIButton
            app.AddROIButton = uibutton(app.GridLayout3, 'push');
            app.AddROIButton.ButtonPushedFcn = createCallbackFcn(app, @AddROIButtonPushed, true);
            app.AddROIButton.Layout.Row = 4;
            app.AddROIButton.Layout.Column = [4 6];
            app.AddROIButton.Text = 'Add ROI';

            % Create AddtotableButton
            app.AddtotableButton = uibutton(app.GridLayout3, 'push');
            app.AddtotableButton.ButtonPushedFcn = createCallbackFcn(app, @AddtotableButtonPushed, true);
            app.AddtotableButton.Layout.Row = 10;
            app.AddtotableButton.Layout.Column = [4 6];
            app.AddtotableButton.Text = 'Add to table';

            % Create ResetROIButton
            app.ResetROIButton = uibutton(app.GridLayout3, 'push');
            app.ResetROIButton.ButtonPushedFcn = createCallbackFcn(app, @ResetROIButtonPushed, true);
            app.ResetROIButton.Layout.Row = 4;
            app.ResetROIButton.Layout.Column = [1 3];
            app.ResetROIButton.Text = 'Reset ROI';

            % Create APPLYTab
            app.APPLYTab = uitab(app.TabGroup2);
            app.APPLYTab.Title = 'APPLY';

            % Create GridLayout4
            app.GridLayout4 = uigridlayout(app.APPLYTab);
            app.GridLayout4.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout4.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout4.ColumnSpacing = 5;
            app.GridLayout4.RowSpacing = 5;

            % Create ApplytoSelectedButton
            app.ApplytoSelectedButton = uibutton(app.GridLayout4, 'push');
            app.ApplytoSelectedButton.ButtonPushedFcn = createCallbackFcn(app, @ApplytoSelectedButtonPushed, true);
            app.ApplytoSelectedButton.Layout.Row = 4;
            app.ApplytoSelectedButton.Layout.Column = [2 5];
            app.ApplytoSelectedButton.Text = 'Apply to Selected';

            % Create ApplytoSameNameButton
            app.ApplytoSameNameButton = uibutton(app.GridLayout4, 'push');
            app.ApplytoSameNameButton.ButtonPushedFcn = createCallbackFcn(app, @ApplytoSameNameButtonPushed, true);
            app.ApplytoSameNameButton.Layout.Row = 5;
            app.ApplytoSameNameButton.Layout.Column = [2 5];
            app.ApplytoSameNameButton.Text = 'Apply to Same Name';

            % Create ApplytoAllButton
            app.ApplytoAllButton = uibutton(app.GridLayout4, 'push');
            app.ApplytoAllButton.ButtonPushedFcn = createCallbackFcn(app, @ApplytoAllButtonPushed, true);
            app.ApplytoAllButton.Layout.Row = 3;
            app.ApplytoAllButton.Layout.Column = [2 5];
            app.ApplytoAllButton.Text = 'Apply to All';

            % Create GridLayout5
            app.GridLayout5 = uigridlayout(app.GridLayout);
            app.GridLayout5.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout5.RowHeight = {'1x'};
            app.GridLayout5.Layout.Row = 1;
            app.GridLayout5.Layout.Column = [3 7];

            % Create GENERATELOGButton
            app.GENERATELOGButton = uibutton(app.GridLayout5, 'push');
            app.GENERATELOGButton.ButtonPushedFcn = createCallbackFcn(app, @GENERATELOGButtonPushed, true);
            app.GENERATELOGButton.Icon = '042-shuffle.png';
            app.GENERATELOGButton.FontSize = 14;
            app.GENERATELOGButton.FontWeight = 'bold';
            app.GENERATELOGButton.Enable = 'off';
            app.GENERATELOGButton.Layout.Row = 1;
            app.GENERATELOGButton.Layout.Column = [6 10];
            app.GENERATELOGButton.Text = 'GENERATE LOG';

            % Create HelpButton
            app.HelpButton = uibutton(app.GridLayout5, 'push');
            app.HelpButton.ButtonPushedFcn = createCallbackFcn(app, @HelpButtonPushed, true);
            app.HelpButton.Icon = '061-info.png';
            app.HelpButton.Layout.Row = 1;
            app.HelpButton.Layout.Column = 15;
            app.HelpButton.Text = '';

            % Create Plot
            app.Plot = uiaxes(app.GridLayout);
            xlabel(app.Plot, 'Time')
            ylabel(app.Plot, 'Intensity')
            app.Plot.PlotBoxAspectRatio = [2.47156726768377 1 1];
            app.Plot.FontSize = 10;
            app.Plot.Layout.Row = [2 6];
            app.Plot.Layout.Column = [3 7];

            % Create ContextMenu
            app.ContextMenu = uicontextmenu(app.LogGeneratorGUI);
            
            % Assign app.ContextMenu
            app.UITable.ContextMenu = app.ContextMenu;

            % Create RenameMenu
            app.RenameMenu = uimenu(app.ContextMenu);
            app.RenameMenu.MenuSelectedFcn = createCallbackFcn(app, @RenameMenuSelected, true);
            app.RenameMenu.Text = 'Rename';

            % Create ContextMenu2
            app.ContextMenu2 = uicontextmenu(app.LogGeneratorGUI);
            
            % Assign app.ContextMenu2
            app.UITableSignalSelection.ContextMenu = app.ContextMenu2;

            % Create EliminateRowMenu
            app.EliminateRowMenu = uimenu(app.ContextMenu2);
            app.EliminateRowMenu.MenuSelectedFcn = createCallbackFcn(app, @EliminateRowMenuSelected, true);
            app.EliminateRowMenu.Text = 'Eliminate Row';

            % Show the figure after all components are created
            app.LogGeneratorGUI.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = LogGenerator_exported(varargin)

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.LogGeneratorGUI)

            % Execute the startup function
            runStartupFcn(app, @(app)startupFcn(app, varargin{:}))

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.LogGeneratorGUI)
        end
    end
end