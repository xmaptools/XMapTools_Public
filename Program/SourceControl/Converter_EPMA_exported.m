classdef Converter_EPMA_exported < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        ConverterTool      matlab.ui.Figure
        GridLayout         matlab.ui.container.GridLayout
        GenStandards       matlab.ui.control.Button
        GenClassification  matlab.ui.control.Button
        Table_Std          matlab.ui.control.Table
        Table_Maps         matlab.ui.control.Table
        Select_Std         matlab.ui.control.Button
        Select_Map         matlab.ui.control.Button
        Image              matlab.ui.control.Image
        GridLayout2        matlab.ui.container.GridLayout
        FormatMenu         matlab.ui.control.DropDown
        Button_SetDest     matlab.ui.control.Button
        Button_Help        matlab.ui.control.Button
        Comment_Maps       matlab.ui.control.Label
        Comment_Spots      matlab.ui.control.Label
        Xmin               matlab.ui.control.NumericEditField
        Xmax               matlab.ui.control.NumericEditField
        Ymax               matlab.ui.control.NumericEditField
        Ymin               matlab.ui.control.NumericEditField
    end

    
    properties (Access = private)
        XMapToolsApp                % Description
        handles                     % Description
        WhereState
        WhereStateDestination
        WhereStateSource
        Recipe                      % Description
        Data4Conv                   % Description
        MapCoord                    % Description
        
        WaitBar                     % Description
        DwellTime                   % Description
    end
    
    methods (Access = private)
        function OnEstOu(app)
            %
            
            % Methods:  (3) EPMA - JEOL SUN
            %           (4) EPMA - JEOL WINDOWS
            %           (6) LA-ICP-MS
            
            
            ValidSelections = [3,4,6];     % UPDATE HERE!
            IsSpotRequired = [1,1,1];
            
            app.Xmin.Value = app.MapCoord(1);
            app.Xmax.Value = app.MapCoord(2);
            app.Ymax.Value = app.MapCoord(3);
            app.Ymin.Value = app.MapCoord(4);
            
            app.Button_SetDest.Visible = 'Off';
            app.FormatMenu.Enable = 'Off';
            app.Select_Map.Visible = 'Off';
            app.Select_Std.Visible = 'Off';
            app.GenClassification.Visible = 'Off';
            app.GenStandards.Visible = 'Off';
            
            if isequal(app.WhereState,0)
                app.FormatMenu.Enable = 'On';
                
            elseif isequal(app.WhereState,1)            % Menu selected
                app.FormatMenu.Enable = 'On';
                app.Button_SetDest.Visible = 'On';
                
            elseif isequal(app.WhereState,2)            % Destination folder selected
                app.Select_Map.Visible = 'On';
                
            elseif isequal(app.WhereState,3)            % maps have been copied
                app.GenClassification.Visible = 'On';
                
            elseif isequal(app.WhereState,4)            % Classification.txt has been generated
                app.Select_Std.Visible = 'On';
                
            elseif isequal(app.WhereState,5)            % Standard data read
                app.Select_Std.Visible = 'On';
                app.GenStandards.Visible = 'On';
                
            end
            
            ValueMenu = str2num(app.FormatMenu.Value);
            
            if find(ismember(ValidSelections,ValueMenu))
                
                if length(app.WhereStateDestination) > 3
                    
                    Where = find(ismember(ValidSelections,ValueMenu));
                    
                    % Maps
                    
                    if app.Data4Conv.Map.NbMaps > 0
                        
                        Data2TableMap = cell(app.Data4Conv.Map.NbMaps,3);
                        
                        for i = 1:app.Data4Conv.Map.NbMaps
                            
                            Data2TableMap(i,1) = app.Data4Conv.Map.ListMaps(i);
                            Data2TableMap(i,2) = {'Copied to -->'};
                            Data2TableMap(i,3) = app.Data4Conv.Map.ListMapCopied(i);
                        end
                        
                        set(app.Table_Maps,'Units','pixels');
                        PositionTable = get(app.Table_Maps,'Position');
                        set(app.Table_Maps,'Units','normalized');
                        set(app.Table_Maps,'Data',Data2TableMap,'ColumnWidth',{0.30*PositionTable(3),0.399*PositionTable(3),0.30*PositionTable(3)});
                        
                        set(app.Table_Maps,'Visible','On');
                        set(app.Select_Map,'Visible','Off');
                        
                        set(app.Xmin,'Visible','On')
                        set(app.Xmax,'Visible','On')
                        set(app.Ymax,'Visible','On')
                        set(app.Ymin,'Visible','On')
                        
                    else
                        set(app.Table_Maps,'Visible','Off');
                    end
                    
                    % Std
                    if IsSpotRequired(Where) > 0
                        
                        if app.Data4Conv.Std.NbDataset >= 1
                            
                            Data2TableStd = cell(1000,1+2+length(app.Data4Conv.Std.ListElem)+2);
                            Compt = 0;
                            for i = 1:app.Data4Conv.Std.NbDataset
                                for j = 1:length(app.Data4Conv.Std.Dataset(i).Com1)
                                    Compt = Compt+1;
                                    
                                    if app.Data4Conv.Std.Dataset(i).IsInMap(j) > 0
                                        Data2TableStd(Compt,1) = {'Yes'};
                                    else
                                        Data2TableStd(Compt,1) = {'No'};
                                    end
                                    
                                    Data2TableStd(Compt,2) = app.Data4Conv.Std.Dataset(i).Com1(j);
                                    Data2TableStd(Compt,3) = app.Data4Conv.Std.Dataset(i).Com2(j);
                                    
                                    Data2TableStd(Compt,4:end-2) = num2cell(app.Data4Conv.Std.Dataset(i).Data(j,:));
                                    
                                    Data2TableStd(Compt,end-1:end) = num2cell(app.Data4Conv.Std.Dataset(i).DataCoor(j,:));
                                end
                            end
                            
                            Data2TableStd = Data2TableStd(1:Compt,:);
                            set(app.Table_Std,'Data',Data2TableStd);
                            
                            set(app.Table_Std,'ColumnName',['In Map','Comment (summary)','Comment (stage)',app.Data4Conv.Std.ListElem,'X','Y']);
                            set(app.Table_Std,'Visible','On');
                            
                            %set(handles.GenStandards,'Visible','On');
                        else
                            
                            
                            %keyboard
                            set(app.Table_Std,'Visible','Off');
                        end
                    else
                        %set(handles.Select_Std,'Visible','Off');
                        set(app.Table_Std,'Visible','Off');
                    end
                    
                else
                    app.Table_Maps.Visible = 'Off';
                    app.Table_Std.Visible = 'Off';
                end
                
                
                
            else
                
                app.Table_Maps.Visible = 'Off';
                app.Table_Std.Visible = 'Off';
                
                app.Xmin.Visible = 'Off';
                app.Xmax.Visible = 'Off';
                app.Ymax.Visible = 'Off';
                app.Ymin.Visible = 'Off';
            end
            
            
            
            
            return
        end
        
        function Name = ReadCndFileMapSUN(app, FileName)
            % Subroutine to read a cnd map file.
            %
            
            fid = fopen(FileName);
            
            while 1
                TheLine = fgetl(fid);
                
                if isequal(TheLine,-1)
                    fclose(fid);
                    break
                end
                
                if length(TheLine)
                    TheStr = strread(TheLine,'%s');
                    
                    if iscell(TheStr) && length(TheStr) > 1
                        
                        if isequal(TheStr{1},'$XM_ELEMENT')
                            Name = TheStr{2};
                        end
                        
                        if isequal(TheStr{1},'$XM_CRYSTAL')
                            Cryst = TheStr{2};
                            
                            switch Cryst(1:3)
                                case 'ROI'
                                    Name = ['_',Name];
                                case 'TOP'
                                    Name = 'TOPO';
                                case 'SEI'
                                    Name = 'SEI';
                            end
                            
                        end
                        
                        
                    end
                end
            end
            
            return
        end
        
        function [Name, MapCoord, DwellTime] = ReadCndFileMapWIN(app, FileName)
            % Subroutine to read a cnd map file.
            %
            
            fid = fopen(FileName);
            
            X = zeros(4,1);
            Y = zeros(4,1);
            
            while 1
                TheLine = fgetl(fid);
                
                if isequal(TheLine,-1)
                    fclose(fid);
                    break
                end
                
                if length(TheLine)
                    TheStr = strread(TheLine,'%s');
                    
                    if iscell(TheStr) && length(TheStr) > 1
                        
                        if isequal(TheStr{1},'$XM_ELEM_NAME%0')
                            Name = TheStr{2};
                        end
                        
                        if isequal(TheStr{1},'$XM_ELEM_EDS_ROI_NUMBER_OF_ROI%0')
                            Name = ['_',Name];
                        end
                        
                        if isequal(TheStr{1},'$XM_ELEM_IMS_SIGNAL_TYPE%0')
                            Type = TheStr{2}
                            
                            switch Type(1:3)
                                case 'TOP'
                                    Name = 'TOPO';
                                case 'SEI'
                                    Name = 'SEI';
                                case 'COM'
                                    Name = 'BSE';
                                case 'CLI'
                                    Name = 'CL';
                            end
                        end
                        
                        if isequal(TheStr{1},'$XM_AP_SA_DWELL_TIME%0')
                            DwellTime = str2num(TheStr{2});
                        end
                        
                        if isequal(TheStr{1},'$XM_AP_SA_STAGE_POS%0_1')
                            X(1) = str2num(TheStr{2});
                            Y(1) = str2num(TheStr{3});
                        end
                        
                        if isequal(TheStr{1},'$XM_AP_SA_STAGE_POS%0_2')
                            X(2) = str2num(TheStr{2});
                            Y(2) = str2num(TheStr{3});
                        end
                        
                        if isequal(TheStr{1},'$XM_AP_SA_STAGE_POS%0_3')
                            X(3) = str2num(TheStr{2});
                            Y(3) = str2num(TheStr{3});
                        end
                        
                        if isequal(TheStr{1},'$XM_AP_SA_STAGE_POS%0_4')
                            X(4) = str2num(TheStr{2});
                            Y(4) = str2num(TheStr{3});
                        end
                        
                    end
                end
            end
            
            MapCoord = [max(X),min(X),max(Y),min(Y)];
            
            return
        end
        
        function [Coord, Comment] = ReadStageFile(app, FileName)
            % Subroutine to read stage file.
            %
            
            fid = fopen(FileName);
            
            while 1
                TheLine = fgetl(fid);
                
                if isequal(TheLine,-1)
                    fclose(fid);
                    break
                end
                
                if length(TheLine)
                    TheStr = strread(TheLine,'%s');
                    
                    if iscell(TheStr) && length(TheStr)
                        
                        if isequal(TheStr{1},'No.')
                            Labels = TheStr;
                            WhereX = find(ismember(Labels,'X'));
                            WhereY = find(ismember(Labels,'Y'));
                            
                            WhereComment = find(ismember(Labels,'Comment'));
                            
                            Compt = 0;
                            while 1
                                TheLine = fgetl(fid);
                                if length(TheLine) > 10
                                    Compt = Compt+1;
                                    TheStr = strread(TheLine,'%s','delimiter','\t');
                                    Coord(Compt,2) = str2double(TheStr(WhereX));    % INVERTED (SUN)
                                    Coord(Compt,1) = str2double(TheStr(WhereY));    % INVERTED (SUN)
                                    Comment{Compt} = TheStr{WhereComment};
                                else
                                    break
                                end
                                
                                
                            end
                        end
                    end
                end
            end
            
        end
        
        function Data = ReadSummaryFileSUN(app, FileName)
            % Subroutine to reand Summary file.
            %
            
            fid = fopen(FileName);
            
            while 1
                TheLine = fgetl(fid);
                
                if isequal(TheLine,-1)
                    fclose(fid);
                    break
                end
                
                if length(TheLine)
                    TheStr = strread(TheLine,'%s');
                    
                    if iscell(TheStr) && length(TheStr)
                        
                        if isequal(TheStr{1},'No.')
                            Data.Labels = TheStr(2:end-2);
                            
                            Compt = 0;
                            while 1
                                TheLine = fgetl(fid);
                                if length(TheLine) > 10
                                    Compt = Compt+1;
                                    TheStr = strread(TheLine,'%s','delimiter','\t');
                                    Data.Data(Compt,:) = str2double(TheStr(2:end-2));
                                    Data.Comments{Compt} = TheStr{end};
                                else
                                    break
                                end
                                
                                
                            end
                        end
                    end
                end
            end
            
            %Data.Labels{end+1} = 'X';
            %Data.Labels{end+1} = 'Y';
            
            return
        end
        
        function DataOk = ReadSummaryFileWIN(app, FileName)
            % Subroutine to reand Summary file.
            %
            
            fid = fopen(FileName);
            
            Data.Data = [];
            Data.Comments = {};
            Count = 0;
            
            while 1
                TheLine = fgetl(fid);
                Count = Count+1;
                if isequal(TheLine,-1)
                    fclose(fid);
                    break
                end
                
                
                if length(TheLine)
                    TheStr = textscan(TheLine,'%s','delimiter',',');
                    TheStr = TheStr{1};
                    
                    if iscell(TheStr) && length(TheStr)
                        
                        if isequal(length(TheStr),1)
                            TheStr = textscan(TheLine,'%s');
                            TheStr = TheStr{1};
                        end
                        
                        if isequal(TheStr{1},'Point') 
                            Data.Labels = TheStr(2:end);   % we skip the raw number
                            
                            NbLabels = length(Data.Labels);
                            Compt = 0;
                            
                            while 1
                                TheLine = fgetl(fid);
                                if isequal(TheLine,-1)
                                    break
                                end
                                TheStr = textscan(TheLine,'%s','delimiter',',');
                                TheStr = TheStr{1};
                                
                                if isequal(length(TheStr),1)
                                    TheStr = textscan(TheLine,'%s');
                                    TheStr = TheStr{1};
                                end
                                
                                Empty = zeros(length(TheStr),1);
                                for i = 1:length(TheStr) 
                                    if isempty(TheStr{i})
                                        Empty(i) = 1;
                                    end
                                end
                                if isequal(sum(Empty),length(Empty))
                                    break
                                end
    
%                                 if length(TheStr) > NbLabels + 5 % , delimiter
%                                     TheStr = strread(TheLine,'%s','delimiter',',');
%                                 end
                                
                                if length(TheStr) > 10 
                                    Compt = Compt+1;
                                    
                                    if length(TheStr)-NbLabels >= 2
                                        Data.Comments{Compt} = strcat(TheStr{2:end-NbLabels});
                                        Data.Data(Compt,:) = str2double(TheStr(end-(NbLabels-1):end-1));
                                    else
                                        Data.Comments{Compt} = strcat(TheStr{2});
                                        Data.Data(Compt,:) = str2double(TheStr(3:end));
                                    end
                                else
                                    break
                                end
                                
                                
                            end
                        end
                    end
                end
            end
            
            Labels = Data.Labels;
            Type = zeros(size(Labels));
            Compt = 0;
            
            for i = 1:length(Labels)
                if isequal(Labels{i},'Comment')
                    Type(i) = 1;
                elseif isequal(Labels{i},'X')
                    Type(i) = 2;
                elseif isequal(Labels{i},'Y')
                    Type(i) = 3;
                elseif length(Labels{i}) > 6
                    if isequal(Labels{i}(end-6:end),'(Mass%)')
                        Compt = Compt+1;
                        Elem{Compt} = Labels{i}(1:end-7);
                        Type(i) = 4;
                    end
                end
            end
            
            % We exclude the total:
            
            WhereOk = find(Type == 4)-1;    %
            WhereOk = WhereOk(1:end-1);     % Exclude the total
            
            DataOk.Labels = Elem(1:end-1);
            DataOk.Data = Data.Data(:,WhereOk);
            
            DataOk.Comments = Data.Comments;
            
            WhereX = find(Type == 2)-1;
            WhereY = find(Type == 3)-1;
            
            DataOk.Coord = [Data.Data(:,WhereX),Data.Data(:,WhereY)];
            
        end
        
        function DataOk = ReadSummaryFileCAMECA(app, FileName)
            % Subroutine to reand Summary file.
            %
            
            fid = fopen(FileName);
            
            Data.Data = [];
            Data.Comments = {};
            Count = 0;
            
            while 1
                TheLine = fgetl(fid);
                Count = Count+1;
                if isequal(TheLine,-1)
                    fclose(fid);
                    break
                end
                
                
                if length(TheLine)
                    TheStr = strread(TheLine,'%s','delimiter','\t,');
                    
                    if iscell(TheStr) && length(TheStr)
                        
                        if contains(TheStr{1},'DataSet/Point')  % isequal(char(TheStr{1}),'DataSet/Point')
                            
                            Data.Labels = TheStr(1:end);   % we skip the raw number
                            
                            NbLabels = length(Data.Labels);
                            Compt = 0;
                            
                            while 1
                                TheLine = fgetl(fid);
                                if isequal(TheLine,-1)
                                    break
                                end
                                TheStr = strread(TheLine,'%s','delimiter','\t,');
                                
                                if length(TheStr) < 10
                                    break
                                end
                                
                                Compt = Compt+1;
                                try
                                    Data.Data(Compt,:) = str2double(TheStr(:));     % we keep the entire line and the skip option is implemented below!
                                catch ME
                                    keyboard 
                                end
                                Data.Comments{Compt} = TheStr{1};
                            end
                        end
                    end
                end
            end
            
            Labels = Data.Labels;
            Type = zeros(size(Labels));
            Compt = 0;
            
            WhereTotal = find(ismember(Labels,'Total'));
            WhereX = find(ismember(Labels,'X '));
            if isempty(WhereX)
                WhereX = find(ismember(Labels,'X'));
            end
            WhereY = find(ismember(Labels,'Y '));
            if isempty(WhereY)
                WhereY = find(ismember(Labels,'Y'));
            end
            
            WhereDataset = find(ismember(Labels,'DataSet/Point'));
            
            ShiftFirstBlock = 0;    % because above: TheStr = strread(TheLine,'%s','delimiter','\t,');
                                    % Otherwise it is 3
            
            if length(WhereTotal) > 1
                
                % DataSet/Point	Na	Mg	Al	Si	K 	Ca	Ti	Cr	Mn	Fe	O 	Total	Na	Mg	Al	Si	K 	Ca	Ti	Cr	Mn	Fe	O 	Total	Na2O	MgO	Al2O3	SiO2	K2O	CaO	TiO2	Cr2O3	MnO	FeO	Total	 X 	 Y 	 Z 	 Beam X 	 Beam Y 	Comment	Distance (?)	Mean Z	Point#	Date
                % 1 / 1 . 	0.743369	7.000587	0.992360	23.675010	0.000010	15.496050	0.028793	0.050500	0.047470	7.429382	41.095420	96.558950	0.753605	6.712945	0.857190	19.646370	0.000006	9.010886	0.014010	0.022636	0.020138	3.100467	59.861750	100.000000	1.002046	11.609060	1.875058	50.649700	0.000012	21.682070	0.048028	0.073809	0.061296	9.557870	96.558950	-10413.0	32015.0	60.0	 	 	BOO17-3S_map2_cpx1	0.00	12.714160	1	Monday, September 13, 2021 4:45:45 PM	

                Elem = Labels(WhereTotal(2)+1:WhereTotal(3)-1);
                WhereOk = WhereTotal(2)+ShiftFirstBlock+1:WhereTotal(3)+ShiftFirstBlock-1;
                WhereX = WhereX+ShiftFirstBlock;
                WhereY = WhereY+ShiftFirstBlock;
            else
                 
                % DataSet/Point	Na2O	MgO	SiO2	Al2O3	K2O	FeO	MnO	BaO	Cr2O3	Cl	CaO	TiO2	Total	X	Y	Z	Comment	Mean Z	Date
                % 1 / 1 .	0.000	0.047	0.000	0.000	0.012	0.049	0.102	0.000	0.000	0.019	54.334	0.000	54.562	7160	-17670	49	 	9.049	21.01.13 12:56
                % 1 / 2 .   0.043	0.020	29.964	1.474	0.019	0.716	0.118	0.435	0.014	0.013	27.921	37.349	98.086	6456	-17357	50	 	14.564	21.01.13 12:59
                
                % or 
                
                % DataSet/Point	Na2O	MgO	SiO2	Al2O3	K2O	FeO	MnO	BaO	Cr2O3	Cl	CaO	TiO2	Total	X	Y	Z	Comment	Mean Z	Date
                % 1 0.000	0.047	0.000	0.000	0.012	0.049	0.102	0.000	0.000	0.019	54.334	0.000	54.562	7160	-17670	49	 	9.049	21.01.13 12:56
                % 2 0.043	0.020	29.964	1.474	0.019	0.716	0.118	0.435	0.014	0.013	27.921	37.349	98.086	6456	-17357	50	 	14.564	21.01.13 12:59
                
                Elem = Labels(2:WhereTotal-1);
                WhereOk = ShiftFirstBlock+1+1:WhereTotal+ShiftFirstBlock-1;
                
                WhereX = WhereX+ShiftFirstBlock;
                WhereY = WhereY+ShiftFirstBlock;
            end 
                
            %Elem{end+1} = 'X';
            %Elem{end+1} = 'Y';
            
            %WhereOk(end+1) = WhereX;
            %WhereOk(end+1) = WhereY;
            
            DataOk.Labels = Elem;
            DataOk.Data = Data.Data(:,WhereOk);
            
            DataOk.Comments = Data.Comments;
            
            DataOk.Coord = [Data.Data(:,WhereX(1)),Data.Data(:,WhereY(1))];  % (1) because it can appear multiple times (yes!)
            
        end
        
        
        function ReadZeroCndFile(app, FileName)
            % Subroutine to read 0.cnd file.
            %
            
            fid = fopen(FileName);
            
            while 1
                TheLine = fgetl(fid);
                
                if isequal(TheLine,-1)
                    fclose(fid);
                    break
                end
                
                if ~isempty(TheLine)
                    TheStr = strread(TheLine,'%s','delimiter','\t');
                    
                    switch char(TheStr{end})
                        case 'Measurement Center Position X [mm]'
                            CenterPosition(1) = str2num(TheStr{1});
                            
                        case 'Measurement Center Position Y [mm]'
                            CenterPosition(2) = str2num(TheStr{1});
                            
                        case 'X-axis Step Number [1~1024]'
                            MapSize(1) = str2num(TheStr{1});
                            
                        case 'Y-axis Step Number [1~1024]'
                            MapSize(2) = str2num(TheStr{1});
                            
                        case 'X Step Size [um], or Beam Dots Width'
                            StepSize(1) = str2num(TheStr{1})/1000;
                            
                        case 'Y Step Size [um], or Beam Dots Width'
                            StepSize(2) = str2num(TheStr{1})/1000;
                        case 'Dwell Time [msec]'
                            app.DwellTime = str2num(TheStr{1});
                    end
                end
            end
            
            app.MapCoord(1) = CenterPosition(2) - MapSize(2)*StepSize(2)/2;
            app.MapCoord(2) = CenterPosition(2) + MapSize(2)*StepSize(2)/2;
            
            app.MapCoord(3) = CenterPosition(1) + MapSize(1)*StepSize(1)/2;
            app.MapCoord(4) = CenterPosition(1) - MapSize(1)*StepSize(1)/2;
            
        end
        
        function RunOnEstOu_Callback(app, hObject, eventdata, handles)
            %
            OnEstOu(app, handles);
            return
        end
        
        % Update components that require runtime configuration
        function addRuntimeConfigurations(app)
            
            % Set component properties that require runtime configuration
            app.Table_Maps.ColumnFormat = {[] [] []};
        end
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
            
            app.XMapToolsApp = XMapToolsApp;
            
            app.ConverterTool.Visible = 'off';
            
            movegui(app.ConverterTool,"center");
            
            app.Recipe = 0;
            
            app.WhereState = 0;
            
            app.WhereStateDestination = '';
            app.WhereStateSource = '';
            
            app.Data4Conv.Map.NbMaps = 0;
            app.Data4Conv.Map.ListMaps = '';
            app.Data4Conv.Map.ListMapCopied = [];
            
            app.MapCoord = [0,0,0,0];
            
            app.Data4Conv.Std.NbElem = 0;
            app.Data4Conv.Std.ListElem = '';
            app.Data4Conv.Std.ListCoor = {'X','Y'};
            app.Data4Conv.Std.NbDataset = 0;
            app.Data4Conv.Std.Dataset(1).Data = [];
            app.Data4Conv.Std.Dataset(1).Com1 = '';
            app.Data4Conv.Std.Dataset(1).DataCoor = [];
            app.Data4Conv.Std.Dataset(1).Com2 = '';
            app.Data4Conv.Std.Dataset(1).IsInMap = [];
            
            OnEstOu(app);
            
            app.ConverterTool.Visible = 'on';
            
        end

        % Button pushed function: Button_SetDest
        function Button_SetDest_Callback(app, event)
            
            %
            f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
            WhereDestinationInput = uigetdir(cd, ['Create the Map Directory']);
            delete(f)
            figure(app.ConverterTool);
            
            % Check if the destination folder is empty (should be empty)
            Files = dir(WhereDestinationInput);
            
            for i = 1:length(Files)
                if Files(i).bytes > 0
                    
%                     f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
%                     Answer = questdlg(char('Directory is not empty and files will be deleted. Continue?'),'Warning','Yes','No','Yes');
%                     delete(f)
%                     figure(app.ConverterTool);
%                     
                    Answer = uiconfirm(app.ConverterTool, 'Directory is not empty and existing files will be deleted. Would you like to continue?', 'Warning', 'Options', {'Yes','No'});
                    
                    if ~isequal(Answer,'Yes')
                        return
                    end
                    
                    [SUCCESS,MESSAGE,MESSAGEID] = rmdir(WhereDestinationInput,'s');
                    
                    break
                    
                end
            end
            
            [SUCCESS,MESSAGE,MESSAGEID] = mkdir(WhereDestinationInput);
            
            cd(WhereDestinationInput);
            cd ..
            app.XMapToolsApp.XMapTools_LastDir = cd;
            
            app.WhereState = 2;
            
            app.WhereStateDestination = WhereDestinationInput;
            
            OnEstOu(app);
        end

        % Value changed function: FormatMenu
        function FormatMenu_Callback(app, event)
            
            %
            ValidSelections = [3,4,6];     % UPDATE HERE!
            
            if find(ismember(ValidSelections,str2num(app.FormatMenu.Value)))
                app.WhereState = 1;
                app.Recipe = find(ismember(ValidSelections,str2num(app.FormatMenu.Value)));
                
                switch app.Recipe
                    
                    % Indices changed in XMapTools 4.4: 
                    
                    case 1      % JEOL WINDOWS
                        app.Comment_Maps.Text = {'> Required JEOL files (MAPS):','   2 files/map: data00X.cnd & data00X.csv'};
                        app.Comment_Spots.Text = {'> Required JEOL files (STD):','   1 file/dataset: summary.csv'};
                        app.Comment_Maps.Visible = 'on';
                        app.Comment_Spots.Visible = 'on';
                        
                        app.Button_SetDest.Visible = 'on';
                    
                    case 2      % JEOL SUN   
                        app.Comment_Maps.Text = {'> Required JEOL files (MAPS):','   2 files/map: XX.cnd & XX_map.txt'};
                        app.Comment_Spots.Text = {'> Required JEOL files (STD):','   2 files/dataset: summary.txt & stage.txt'};
                        app.Comment_Maps.Visible = 'on';
                        app.Comment_Spots.Visible = 'on';
                        
                        app.Button_SetDest.Visible = 'on';

                    case 3      % CAMECA
                        app.Comment_Maps.Text =  {'> Required CAMECA files (MAPS):','   1 file/map: XXX.txt'};
                        app.Comment_Spots.Text = {'> Required CAMECA files (STD):','   1 file/dataset: YYY.csv'};
                        app.Comment_Maps.Visible = 'on';
                        app.Comment_Spots.Visible = 'on';
                        
                        app.Button_SetDest.Visible = 'on';
                end
                
                
            else
                app.Button_SetDest.Visible = 'off';
                
                app.Comment_Maps.Visible = 'off';
                app.Comment_Spots.Visible = 'off';
                
                app.WhereState = 0;
                app.Recipe = 0;
            end
            
        end

        % Button pushed function: GenClassification
        function GenClassification_Callback(app, event)
             
%             WhereDestination = app.WhereStateDestination;
%             
%             disp('  '); disp('  ');
%             
%             fid = fopen([WhereDestination,'/Classification.txt'],'w');
%             fprintf(fid,'\n\n%s\n','! Below define the input pixels for the classification function');
%             fprintf(fid,'%s\n','! Format: MINERAL_NAME_(no blank!)   X   Y');
%             fprintf(fid,'%s\n','>1');
%             fprintf(fid,'\n\n\n%s\n','! Below define the density of mineral phases (same order as >1)');
%             fprintf(fid,'%s\n','! Format: DENSITY');
%             fprintf(fid,'%s\n','>2');
%             fprintf(fid,'\n\n\n');
%             fclose(fid);
%             
%             disp('>> Classification.txt has been created ');
            
            app.WhereState = 4;
            OnEstOu(app);
            
        end

        % Button pushed function: GenStandards
        function GenStandards_Callback(app, event)
            
            WhereDestination = app.WhereStateDestination;
            
            ElList = app.Data4Conv.Std.ListElem;
            
            
            fid = fopen([WhereDestination,'/Standards.txt'],'w');
            fprintf(fid,'\n\n%s\n','>1');
            fprintf(fid,'%f\t%f\t%f\t%f\n',app.MapCoord);
            fprintf(fid,'\n\n%s\n','>2');
            for i=1:length(ElList)
                fprintf(fid,'%s\t',ElList{i});
            end
            fprintf(fid,'%s\t%s','X','Y');
            fprintf(fid,'\n\n\n%s\n','>3');
            
            
            for i = 1:app.Data4Conv.Std.NbDataset
                for j = 1:length(app.Data4Conv.Std.Dataset(i).Com1)
                    
                    if app.Data4Conv.Std.Dataset(i).IsInMap(j)
                        ConvInd = app.Data4Conv.Std.Dataset(i).Data(j,:);
                        
                        for iC = 1:numel(ConvInd)
                            if ConvInd(iC) > 0
                                fprintf(fid,'%f\t',ConvInd(iC));
                            else
                                fprintf(fid,'%f\t',0);
                            end
                        end
                        fprintf(fid,'%f\t',app.Data4Conv.Std.Dataset(i).DataCoor(j,1));
                        fprintf(fid,'%f\n',app.Data4Conv.Std.Dataset(i).DataCoor(j,2));
                    end
                end
            end
            
            fclose(fid);
            
            %keyboard
            
            cd(WhereDestination);    % Implemented in XMapTools
            
            close(app.ConverterTool);
            
            
        end

        % Button pushed function: Select_Map
        function Select_Map_Callback(app, event)
            
            f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
            WhereSource = uigetdir(cd, ['Pick a Directory']);
            
            if isequal(WhereSource,0) 
                return
            end
            
            delete(f)
            figure(app.ConverterTool);
            
            if isequal(WhereSource,app.WhereStateDestination)
                errordlg('The source and destination directories must be different!','Error')
                return
            end
            
            d = uiprogressdlg(app.ConverterTool,'Message','Copying the maps','Title','Please Wait!','Indeterminate','on');
            
            Compt = 0;
            for i = 1:1000
                FileName = num2str(i);
                
                switch app.Recipe
                    
                    % Indices changed in XMapTools 4.4:
                    
                    case 2          % JEOL SUN
                        
                        if exist([WhereSource,'/',FileName,'.cnd']) && exist([WhereSource,'/',FileName,'_map.txt'])
                            [Name] = ReadCndFileMapSUN(app, [WhereSource,'/',FileName,'.cnd']);
                            
                            [Success,Message,MessageID] = copyfile([WhereSource,'/',FileName,'_map.txt'],[app.WhereStateDestination,'/',Name,'.txt']);
                            
                            if Success
                                disp(['>> Copying ',FileName,'_map.txt  ->  ',Name,'.txt   -  Done']);
                                
                                Compt = Compt+1;
                                
                                app.Data4Conv.Map.NbMaps = Compt;
                                app.Data4Conv.Map.ListMaps{Compt} = [FileName,'_map.txt'];
                                app.Data4Conv.Map.ListMapCopied{Compt} = [Name,'.txt'];
                                
                            else
                                disp('  ')
                                disp(['** WARNING ** ',FileName,'_map.txt  not found']);
                                disp('  ')
                            end
                            
                        else
                            break
                        end
                        
                        
                    case 1      % JEOL WIN
                        
                        D = dir([WhereSource,'/*.csv']);
                        
                        for i = 1:length(D)
                            
                            NameOk = D(i).name;
                            WhereS = find(ismember(NameOk,'_'));
                            if ~isempty(WhereS)
                                NameOk = NameOk(1:WhereS(1)-1);
                            end
                            WhereS = find(ismember(NameOk,'.'));
                            if ~isempty(WhereS)
                                NameOk = NameOk(1:WhereS(1)-1);
                            end
                            
                            CheckCND = exist([WhereSource,'/',NameOk,'.cnd']);
                            
                            if isequal(CheckCND,2)
                                [Name,MapCoord,DwellTime] = ReadCndFileMapWIN(app, [WhereSource,'/',NameOk,'.cnd']);
                                [Success,Message,MessageID] = copyfile([WhereSource,'/',D(i).name],[app.WhereStateDestination,'/',Name,'.csv']);
                                if Success
                                    disp(['>> Copying ',D(i).name,'  ->  ',Name,'.csv   -  Done']);
                                    Compt = Compt+1;
                                    
                                    app.Data4Conv.Map.NbMaps = Compt;
                                    app.Data4Conv.Map.ListMaps{Compt} = [D(i).name];
                                    app.Data4Conv.Map.ListMapCopied{Compt} = [Name,'.csv'];
                                    
                                    
                                    if isequal(sum(app.MapCoord),0)
                                        app.MapCoord = MapCoord;
                                        disp('Map coordinates read')
                                    end
                                    if isempty(app.DwellTime)
                                        if ~isempty(DwellTime)
                                            if DwellTime > 0
                                                app.DwellTime = DwellTime;
                                                disp('Dwelltime value read')
                                            end
                                        end
                                    end
                                    
                                end
                            end
                        end
                        
                        break

%                         if i >= 10
%                             Frag = 'Data0';
%                         else
%                             Frag = 'Data00';
%                         end
%                         
%                         
%                         if exist([WhereSource,'/',Frag,FileName,'.cnd']) && exist([WhereSource,'/',Frag,FileName,'.csv'])
%                             
%                             [Name,MapCoord] = ReadCndFileMapWIN(app, [WhereSource,'/',Frag,FileName,'.cnd']);
%                             
%                             [Success,Message,MessageID] = copyfile([WhereSource,'/',Frag,FileName,'.csv'],[app.WhereStateDestination,'/',Name,'.csv']);
%                             
%                             if Success
%                                 disp(['>> Copying ',FileName,'_map.txt  ->  ',Name,'.txt   -  Done']);
%                                 
%                                 Compt = Compt+1;
%                                 
%                                 app.Data4Conv.Map.NbMaps = Compt;
%                                 app.Data4Conv.Map.ListMaps{Compt} = [Frag,FileName,'.csv'];
%                                 app.Data4Conv.Map.ListMapCopied{Compt} = [Name,'.csv'];
%                                 
%                                 
%                                 if isequal(sum(app.MapCoord),0)
%                                     app.MapCoord = MapCoord;
%                                     disp('Map coordinates read')
%                                 end
%                                 
%                             else
%                                 disp('  ')
%                                 disp(['** WARNING ** ',FileName,'_map.txt  not found']);
%                                 disp('  ')
%                             end
%                             
%                         else
%                             break
%                         end
                        
                        
                    case 3      % CAMECA
                        
                        Files = dir(WhereSource);
                        
                        ReadCoor = 0;
                        ReadStep = 0;
                        ReadStepSize = 0;
                        RedLine = 0;
                        CalcCoord = 0;
                        
                        Step = 100000;
                        Line = 100000;
                        
                        Compt = 0;
                        
                        for ii = 1:length(Files)
                            if ~Files(ii).isdir && ~isequal(Files(ii).name,'.DS_Store')
                                
                                fid = fopen(char([Files(ii).folder,'/',Files(ii).name]),'r');
                                
                                Str1 = textscan(Files(ii).name,'%s','delimiter','_');
                                Str1 = Str1{1};
                                Str2 = textscan(Str1{end},'%s','delimiter',' ');
                                Str2 = Str2{1};
                                
                                Name = char(Str2{1});
                                
                                TheMap = [];
                                
                                while 1
                                    TheLin = fgetl(fid); 
                                    
                                    if isequal(TheLin,-1)
                                        break
                                    end
                                    
                                    if length(TheLin) > 1
                                        TheStr = textscan(TheLin,'%s');
                                        TheStr = TheStr{1};
                                        
                                        if iscell(TheStr) && length(TheStr) > 3
                                            if isequal(TheStr{1},'Start') && ~ReadCoor
                                                StartPoint = [str2num(TheStr{5}),str2num(TheStr{8})];
                                                ReadCoor = 1;
                                            end
                                            if isequal(TheStr{1},'Step') && isequal(TheStr{2},'Number') && ~ReadStep
                                                Step = str2num(TheStr{4});
                                                ReadStep = 1;
                                            end
                                            if isequal(TheStr{1},'Step') && isequal(TheStr{2},'Size') && ~ReadStepSize
                                                StepSize = str2num(TheStr{4});
                                                ReadStepSize = 1;
                                            end
                                            if isequal(TheStr{1},'Line') && isequal(TheStr{2},'Number') && ~RedLine
                                                Line = str2num(TheStr{4});
                                                RedLine = 1;
                                            end
                                            
                                            if isequal(ReadCoor,1) && isequal(ReadStep,1) && isequal(ReadStepSize,1) && isequal(RedLine,1) && ~CalcCoord
                                                Xmin = StartPoint(1) - 0.5*Step*StepSize;
                                                Xmax = StartPoint(1) + 0.5*Step*StepSize;
                                                Ymin = StartPoint(2) - 0.5*Line*StepSize;
                                                Ymax = StartPoint(2) + 0.5*Line*StepSize;
                                                app.MapCoord = [Xmin,Xmax,Ymin,Ymax];
                                                CalcCoord = 1;
                                            end
                                            
                                            if isequal(TheStr{1},'Image')
                                                Name = char(TheStr{3});
                                            end
                                            
                                            if isequal(length(TheStr),Step) || isequal(length(TheStr),Step+1)
                                                TheMap = zeros(Line,Step);
                                                
                                                %TheLin = fgetl(fid)
                                                TheStr = textscan(TheLin,'%f');
                                                TheStr = TheStr{1};
                                                
                                                if isequal(TheStr(1),1) && isequal(TheStr(2),2)  % Celine's format we skip
                                                    for i = 1:Line
                                                        TheLin = fgetl(fid);
                                                        TheStr = textscan(TheLin,'%f');
                                                        TheStr = TheStr{1};
                                                        TheMap(i,:) = TheStr(2:end)';
                                                    end
                                                else
                                                    TheMap(1,:) = TheStr';
                                                    for i = 2:Line
                                                        TheLin = fgetl(fid);
                                                        TheStr = textscan(TheLin,'%f');
                                                        TheStr = TheStr{1};
                                                        TheMap(i,:) = TheStr';
                                                    end
                                                end
                                            end
                                           
                                        end
                                    end
                                end
                                
                                fclose(fid);
                                
                                save([app.WhereStateDestination,'/',Name,'.txt'],'TheMap','-ASCII');
                                
                                Compt = Compt+1;
                                
                                app.Data4Conv.Map.NbMaps = Compt;
                                app.Data4Conv.Map.ListMaps{Compt} = Files(ii).name;
                                app.Data4Conv.Map.ListMapCopied{Compt} = [Name,'.txt'];
                                
                                
                            end
                        end
                        
                        break
                end
            end
            
            app.WhereStateSource = WhereSource;
            
            if isequal(app.Recipe,2)
                % Extract the map coordinates
                ReadZeroCndFile(app, [WhereSource,'/0.cnd']);
                
                disp('  '); disp('  ');
                disp('>> Coordinates extracted from 0.cnd ');
                
            end
            
            if ~isempty(app.DwellTime)
                
                fid = fopen(fullfile(app.WhereStateDestination,'Import.txt'),'w');
                fprintf(fid,'%s\n','#######################################################');
                fprintf(fid,'%s\n','#     XMapTools Last Import Settings (DO NOT EDIT)    #');
                fprintf(fid,'%s\n','#######################################################');
                fprintf(fid,'%s\n','...');
                fprintf(fid,'%s\n','DT_correction:      1');
                fprintf(fid,'%s\t\t%.3f\t%.3f\n','DT_param(DwellTime,DeadTime):',app.DwellTime,300);
                fprintf(fid,'%s\n','Fact(Col,Lin):						1			1');
                fprintf(fid,'%s\n','Rotation:							0');
                fprintf(fid,'%s\n','Replace_Negative_Values:			1');
                fprintf(fid,'%s\n','Replace_NaN_Values:					1');
                fprintf(fid,'\n');
                fclose(fid);
                
                disp('  '); disp('  ');
                disp('>> Dwell time read and file Import.txt created');
            end
            
            close(d);
            
            app.WhereState = 4;     % changed to 4 to skip next step in 4.2 (13.05.2023)
            
            OnEstOu(app);
            
        end

        % Button pushed function: Select_Std
        function Select_Std_Callback(app, event)
            
            WhereSource = app.WhereStateSource;
            
            NbDataset = app.Data4Conv.Std.NbDataset;
            
            WhereDS = NbDataset+1;
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Select file(s)';
             
            f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
            [filename, pathname, filterindex] = uigetfile('*','Select SUMMARY file...');   % {'*.txt','TEXT-file (*.txt)';'*.csv','CSV-file (*.csv)';'*.*',  'All Files (*.*)'}
            delete(f)
            figure(app.ConverterTool);
            
            if isequal(filename,0)
                close(app.WaitBar);
                return
            end
            
            try
                switch app.Recipe
                    % Indices changed in XMapTools 4.4:
                    case 1          % JEOL WINDOWS
                        DataTEMP = ReadSummaryFileWIN(app, [pathname,filename]);
                    case 2          % JEOL SUN
                        DataTEMP = ReadSummaryFileSUN(app, [pathname,filename]);
                    case 3
                        DataTEMP = ReadSummaryFileCAMECA(app, [pathname,filename]);
                end
            catch 
                close(app.WaitBar);
                uialert(gcbf,'Not a valid file','Error')
                return
            end
            
            app.WaitBar.Message = 'Importing spot analyses';
            
            app.Data4Conv.Std.NbDataset = WhereDS;
            
            for i = 1:length(DataTEMP.Labels)
                
                El = DataTEMP.Labels{i};
                Where = find(ismember(app.Data4Conv.Std.ListElem,El));
                
                if isempty(Where)
                    Idx = length(app.Data4Conv.Std.ListElem)+1;
                    app.Data4Conv.Std.ListElem{Idx} = El;
                else
                    Idx = Where;
                end
                
                app.Data4Conv.Std.Dataset(WhereDS).Data(:,Idx) = DataTEMP.Data(:,i);
            end
            
            app.Data4Conv.Std.Dataset(WhereDS).Com1 = DataTEMP.Comments';
            
            switch app.Recipe
                % Indices changed in XMapTools 4.4:
                case 2          % JEOL SUN
                    f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
                    [filename, pathname, filterindex] = uigetfile({'*.txt','TEXT-file (*.txt)';'*.csv','CSV-file (*.csv)'; ...
                        '*.*',  'All Files (*.*)'},'Select STAGE file...');
                    delete(f)
                    figure(app.ConverterTool);
                    
                    [Coord,Comment] = ReadStageFile(app, [pathname,filename]);
                    
                    
                case {1,3}
                    Coord = DataTEMP.Coord;
                    Comment = DataTEMP.Comments;
                    
                    
            end
            
            app.Data4Conv.Std.Dataset(WhereDS).DataCoor = Coord;
            app.Data4Conv.Std.Dataset(WhereDS).Com2 = Comment';
            
            IsInMap = zeros(size(Coord,1),1);
            
            for i = 1:length(IsInMap)
                switch app.Recipe
                    case 2          % JEOL SUN
                        if Coord(i,1) > app.MapCoord(1) &&  Coord(i,1) < app.MapCoord(2) && Coord(i,2) > app.MapCoord(4) &&  Coord(i,2) < app.MapCoord(3)
                            IsInMap(i) =1;
                        end
                    case 1          % JEOL WIN
                        if Coord(i,1) < app.MapCoord(1) &&  Coord(i,1) > app.MapCoord(2) && Coord(i,2) < app.MapCoord(3) &&  Coord(i,2) > app.MapCoord(4)
                            IsInMap(i) =1;
                        end
                    case 3          % CAMECA
                        if Coord(i,1) > app.MapCoord(1) &&  Coord(i,1) < app.MapCoord(2) && Coord(i,2) > app.MapCoord(3) &&  Coord(i,2) < app.MapCoord(4)
                            IsInMap(i) =1;
                        end
                        
                end
            end
            
            app.Data4Conv.Std.Dataset(WhereDS).IsInMap = IsInMap;
            
            
            % Check the size before to continue (adding more datasets might
            % have added new columns for new elements)
            if WhereDS > 1
                
                NbElTotal = length(app.Data4Conv.Std.ListElem);
                
                for i = 1:WhereDS
                    [NbAn_i,NbEl_i] = size(app.Data4Conv.Std.Dataset(i).Data);
                    if NbEl_i < NbElTotal
                        TempData = app.Data4Conv.Std.Dataset(i).Data;
                        app.Data4Conv.Std.Dataset(i).Data = zeros(NbAn_i,NbElTotal);
                        app.Data4Conv.Std.Dataset(i).Data(:,1:NbEl_i) = TempData;
                    end
                end
            end
            
            app.WhereState = 5;
            
            OnEstOu(app);
            close(app.WaitBar);
        end

        % Button pushed function: Button_Help
        function Button_HelpPushed(app, event)
            
            if isempty(app.XMapToolsApp.Id_HelpTool)
                Help_Display(app.XMapToolsApp,'XMT_help_Converter_EPMA.html');
            else
                app.XMapToolsApp.Id_HelpTool.UpdateTextHelp('XMT_help_Converter_EPMA.html');
            end
            
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create ConverterTool and hide until all components are created
            app.ConverterTool = uifigure('Visible', 'off');
            app.ConverterTool.Position = [100 200 1021 610];
            app.ConverterTool.Name = 'Converter For EPMA Data – XMapTools';
            app.ConverterTool.HandleVisibility = 'callback';
            app.ConverterTool.Tag = 'figure1';

            % Create GridLayout
            app.GridLayout = uigridlayout(app.ConverterTool);
            app.GridLayout.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};

            % Create GenStandards
            app.GenStandards = uibutton(app.GridLayout, 'push');
            app.GenStandards.ButtonPushedFcn = createCallbackFcn(app, @GenStandards_Callback, true);
            app.GenStandards.Tag = 'GenStandards';
            app.GenStandards.Icon = '023-next.png';
            app.GenStandards.Layout.Row = 13;
            app.GenStandards.Layout.Column = [12 15];
            app.GenStandards.Text = 'Generate Standards.txt';

            % Create GenClassification
            app.GenClassification = uibutton(app.GridLayout, 'push');
            app.GenClassification.ButtonPushedFcn = createCallbackFcn(app, @GenClassification_Callback, true);
            app.GenClassification.Tag = 'GenClassification';
            app.GenClassification.Icon = '023-next.png';
            app.GenClassification.Layout.Row = 13;
            app.GenClassification.Layout.Column = [6 9];
            app.GenClassification.Text = 'Validate Map Selection';

            % Create Table_Std
            app.Table_Std = uitable(app.GridLayout);
            app.Table_Std.Tag = 'Table_Std';
            app.Table_Std.Layout.Row = [4 12];
            app.Table_Std.Layout.Column = [6 15];
            app.Table_Std.FontSize = 10;

            % Create Table_Maps
            app.Table_Maps = uitable(app.GridLayout);
            app.Table_Maps.ColumnName = {'Original file'; 'Action'; 'Destination file'};
            app.Table_Maps.RowName = {};
            app.Table_Maps.ColumnEditable = [false false false];
            app.Table_Maps.Tag = 'Table_Maps';
            app.Table_Maps.Layout.Row = [4 14];
            app.Table_Maps.Layout.Column = [1 5];
            app.Table_Maps.FontSize = 10;

            % Create Select_Std
            app.Select_Std = uibutton(app.GridLayout, 'push');
            app.Select_Std.ButtonPushedFcn = createCallbackFcn(app, @Select_Std_Callback, true);
            app.Select_Std.Tag = 'Select_Std';
            app.Select_Std.Icon = '323-add.png';
            app.Select_Std.Layout.Row = 3;
            app.Select_Std.Layout.Column = [6 9];
            app.Select_Std.Text = 'Add Standard Data';

            % Create Select_Map
            app.Select_Map = uibutton(app.GridLayout, 'push');
            app.Select_Map.ButtonPushedFcn = createCallbackFcn(app, @Select_Map_Callback, true);
            app.Select_Map.Tag = 'Select_Map';
            app.Select_Map.Icon = '323-add.png';
            app.Select_Map.Layout.Row = 3;
            app.Select_Map.Layout.Column = [1 2];
            app.Select_Map.Text = 'Add Map Folder';

            % Create Image
            app.Image = uiimage(app.GridLayout);
            app.Image.Layout.Row = [1 2];
            app.Image.Layout.Column = [1 5];
            app.Image.ImageSource = 'logo_xmap_final.png';

            % Create GridLayout2
            app.GridLayout2 = uigridlayout(app.GridLayout);
            app.GridLayout2.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout2.RowHeight = {'1x'};
            app.GridLayout2.Padding = [10 22 10 22];
            app.GridLayout2.Layout.Row = [1 2];
            app.GridLayout2.Layout.Column = [6 15];

            % Create FormatMenu
            app.FormatMenu = uidropdown(app.GridLayout2);
            app.FormatMenu.Items = {'Select Data Format', '----------------------', 'EPMA - JEOL (WINDOWS)', 'EPMA - JEOL (SUN)', '----------------------', 'EPMA - CAMECA', '----------------------'};
            app.FormatMenu.ItemsData = {'1', '2', '3', '4', '5', '6', '7'};
            app.FormatMenu.ValueChangedFcn = createCallbackFcn(app, @FormatMenu_Callback, true);
            app.FormatMenu.Tag = 'FormatMenu';
            app.FormatMenu.Layout.Row = 1;
            app.FormatMenu.Layout.Column = [4 8];
            app.FormatMenu.Value = '1';

            % Create Button_SetDest
            app.Button_SetDest = uibutton(app.GridLayout2, 'push');
            app.Button_SetDest.ButtonPushedFcn = createCallbackFcn(app, @Button_SetDest_Callback, true);
            app.Button_SetDest.Tag = 'Button_SetDest';
            app.Button_SetDest.Icon = '321-exit.png';
            app.Button_SetDest.Layout.Row = 1;
            app.Button_SetDest.Layout.Column = [10 14];
            app.Button_SetDest.Text = 'Set destination folder';

            % Create Button_Help
            app.Button_Help = uibutton(app.GridLayout2, 'push');
            app.Button_Help.ButtonPushedFcn = createCallbackFcn(app, @Button_HelpPushed, true);
            app.Button_Help.Icon = '061-info.png';
            app.Button_Help.Layout.Row = 1;
            app.Button_Help.Layout.Column = 2;
            app.Button_Help.Text = '';

            % Create Comment_Maps
            app.Comment_Maps = uilabel(app.GridLayout);
            app.Comment_Maps.HorizontalAlignment = 'center';
            app.Comment_Maps.FontSize = 10;
            app.Comment_Maps.Visible = 'off';
            app.Comment_Maps.Layout.Row = 3;
            app.Comment_Maps.Layout.Column = [3 5];
            app.Comment_Maps.Text = '';

            % Create Comment_Spots
            app.Comment_Spots = uilabel(app.GridLayout);
            app.Comment_Spots.HorizontalAlignment = 'center';
            app.Comment_Spots.FontSize = 10;
            app.Comment_Spots.Visible = 'off';
            app.Comment_Spots.Layout.Row = 3;
            app.Comment_Spots.Layout.Column = [10 15];
            app.Comment_Spots.Text = '';

            % Create Xmin
            app.Xmin = uieditfield(app.GridLayout, 'numeric');
            app.Xmin.Editable = 'off';
            app.Xmin.HorizontalAlignment = 'center';
            app.Xmin.FontSize = 10;
            app.Xmin.Layout.Row = 14;
            app.Xmin.Layout.Column = 6;

            % Create Xmax
            app.Xmax = uieditfield(app.GridLayout, 'numeric');
            app.Xmax.Editable = 'off';
            app.Xmax.HorizontalAlignment = 'center';
            app.Xmax.FontSize = 10;
            app.Xmax.Layout.Row = 14;
            app.Xmax.Layout.Column = 7;

            % Create Ymax
            app.Ymax = uieditfield(app.GridLayout, 'numeric');
            app.Ymax.Editable = 'off';
            app.Ymax.HorizontalAlignment = 'center';
            app.Ymax.FontSize = 10;
            app.Ymax.Layout.Row = 14;
            app.Ymax.Layout.Column = 8;

            % Create Ymin
            app.Ymin = uieditfield(app.GridLayout, 'numeric');
            app.Ymin.Editable = 'off';
            app.Ymin.HorizontalAlignment = 'center';
            app.Ymin.FontSize = 10;
            app.Ymin.Layout.Row = 14;
            app.Ymin.Layout.Column = 9;

            % Show the figure after all components are created
            app.ConverterTool.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = Converter_EPMA_exported(varargin)

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.ConverterTool)

            % Execute the startup function
            runStartupFcn(app, @(app)startupFcn(app, varargin{:}))

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.ConverterTool)
        end
    end
end