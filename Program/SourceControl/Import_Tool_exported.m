classdef Import_Tool_exported < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        ImportToolXMapToolsUIFigure     matlab.ui.Figure
        GridLayout                      matlab.ui.container.GridLayout
        Image                           matlab.ui.control.Image
        GridLayout3                     matlab.ui.container.GridLayout
        ImportdataButton                matlab.ui.control.Button
        GridLayout2                     matlab.ui.container.GridLayout
        Button_add                      matlab.ui.control.Button
        Button_help                     matlab.ui.control.Button
        TabGroup                        matlab.ui.container.TabGroup
        ListofmapsTab                   matlab.ui.container.Tab
        GridLayout4                     matlab.ui.container.GridLayout
        UITable                         matlab.ui.control.Table
        CorrectionsTab                  matlab.ui.container.Tab
        GridLayout5                     matlab.ui.container.GridLayout
        DeadtimecorrectionforWDSmapsEPMALabel  matlab.ui.control.Label
        ApplyDTcorr                     matlab.ui.control.CheckBox
        DwelltimemsEditFieldLabel       matlab.ui.control.Label
        DwellTimeValue                  matlab.ui.control.NumericEditField
        DetectordeadtimensEditFieldLabel  matlab.ui.control.Label
        DeadTimeValue                   matlab.ui.control.NumericEditField
        GeneralcorrectionsallmapsLabel  matlab.ui.control.Label
        ReplaceNeG                      matlab.ui.control.CheckBox
        ReplaceNaN                      matlab.ui.control.CheckBox
        ResamplingOrientationLAICPMSdatafromIoliteonlyLabel  matlab.ui.control.Label
        XcolumnsEditFieldLabel          matlab.ui.control.Label
        NbColOri                        matlab.ui.control.NumericEditField
        YrowsEditFieldLabel             matlab.ui.control.Label
        NbLinOri                        matlab.ui.control.NumericEditField
        NbColMod                        matlab.ui.control.NumericEditField
        NbLinMod                        matlab.ui.control.NumericEditField
        FactCol                         matlab.ui.control.NumericEditField
        FactLin                         matlab.ui.control.NumericEditField
        RotationcounterclockwiseEditFieldLabel  matlab.ui.control.Label
        RotAngle                        matlab.ui.control.NumericEditField
        OriginalLabel                   matlab.ui.control.Label
        ModifiedLabel                   matlab.ui.control.Label
        ScalingfactorLabel              matlab.ui.control.Label
        Label                           matlab.ui.control.Label
        ReplaceINF                      matlab.ui.control.CheckBox
        VisualizationTab                matlab.ui.container.Tab
        GridLayout6                     matlab.ui.container.GridLayout
        ListMaps                        matlab.ui.control.DropDown
        UncorrecteddataLabel            matlab.ui.control.Label
        CorrecteddataLabel              matlab.ui.control.Label
        UIAxes_Cor                      matlab.ui.control.UIAxes
        UIAxes_Ori                      matlab.ui.control.UIAxes
    end

    
    properties (Access = private)
        XMapToolsApp            % Access to XMapToolsApp
        ImportData              % Contain the maps and some information
        ApplyAll_Type           % Is 1 and 0 after the type has been changed once
        ApplyAll_Destination    % Is 1 and 0 after the destination has been changed once
    end
    
    properties (Access = public)
    end
    
    methods (Access = private)
        
        function GenerateTable(app)
            app.UITable.Data = [];   % Reset
            
            Types={'Element','Oxide','Isotope','Other'};
            Data = {'n.a.','Intensity','wt/wt'};
            Special = {'n.a.','WDS(?)','EDS','Background'};
            Destination = {'Intensity','Quanti','Merged','Result','Other'};
            
            app.UITable.ColumnFormat = {'char',Types,'char',Data,Special,'logical','logical',Destination,{'Keep','Eliminate'}};
            app.UITable.ColumnEditable = [false,true,true,true,false,false,false,true,true];
            Table = cell(length(app.ImportData),9);
            
            GlobalCheck = 1;
            
            for i = 1:length(app.ImportData)
                Table{i,1} = app.ImportData(i).FileName;
                
                Type1 = app.ImportData(i).Type(1);
                Type1_base = Type1;
                if Type1 > 9
                    Type1_base = floor(Type1/10);
                end
                
                Table{i,2} = Types{Type1_base};
                
                Table{i,3} = app.ImportData(i).Name;
                
                Table{i,4} = Data{app.ImportData(i).Type(2)+1};  % Here
                Table{i,5} = Special{app.ImportData(i).Type(3)+1};
                if isequal(app.ImportData(i).Type(1),1) && isequal(app.ImportData(i).Type(3),1)
                    Table{i,6} = 1;
                else
                    Table{i,6} = 0;
                end
                if isequal(app.ImportData(i).Type(1),3)
                    Table{i,7} = 1;
                else
                    Table{i,7} = 0;
                end
                switch app.ImportData(i).Type(1)
                    case 1
                        Table{i,8} = Destination{1};
                    case 2
                        Table{i,8} = Destination{3};
                    case 3
                        Table{i,8} = Destination{2};
                    case 4
                        Table{i,8} = Destination{5};
                    case 11
                        Table{i,8} = Destination{2};
                    case 12
                        Table{i,8} = Destination{3};
                end
                Table{i,9} = 'Keep';
                
                if isequal(app.ImportData(i).Checked,1)
                    s1 = uistyle('BackgroundColor',[0.3010 0.7450 0.9330]);
                    addStyle(app.UITable,s1,'cell',[i*ones(9,1),[1:9]']);
                else
                    s1 = uistyle('BackgroundColor',[255/255 94/255 94/255]);
                    addStyle(app.UITable,s1,'cell',[i*ones(9,1),[1:9]']);
                    GlobalCheck = 0;
                end
                
            end
            app.UITable.Data = Table;
            
            
            
            
            if isequal(GlobalCheck,1)
                app.ImportdataButton.Enable = 'on';
            else
                app.ImportdataButton.Enable = 'off';
            end
            
            % Apply corrections and update fields
            CalcCorrection(app);
            
            % Update menu
            if length(app.ImportData) > 1
                app.ListMaps.Items = extractfield(app.ImportData,'NameDisp');
                app.ListMaps.ItemsData = [1:length(app.ListMaps.Items)];
            else
                app.ListMaps.Items{1} = app.ImportData.NameDisp;
                app.ListMaps.ItemsData(1) = 1;
            end
            
            GeneratePlot(app);
            
        end
        
        function GeneratePlot(app)
            
            if ~isequal(app.ListMaps.Value,'No map available')
                
                imagesc(app.UIAxes_Ori,app.ImportData(app.ListMaps.Value).Data)
                axis(app.UIAxes_Ori,'image');
                colorbar(app.UIAxes_Ori);
                
                imagesc(app.UIAxes_Cor,app.ImportData(app.ListMaps.Value).DataCorr)
                axis(app.UIAxes_Cor,'image');
                colorbar(app.UIAxes_Cor);
                
                app.NbColMod.Value = size(app.ImportData(app.ListMaps.Value).DataCorr,2);
                app.NbLinMod.Value = size(app.ImportData(app.ListMaps.Value).DataCorr,1);
                
                % app.XMapToolsApp.ActiveColorbar
                
            end
        end
        
        function CalcCorrection(app)
            
            FactColVal = app.FactCol.Value;
            FactLinVal = app.FactLin.Value;
            
            for i = 1:length(app.ImportData)
                
                Data = app.ImportData(i).Data;
                
                % display to all data
                app.NbColOri.Value = size(Data,2);
                app.NbLinOri.Value = size(Data,1);
                
                % if isotopes, apply correction else display the same
                % if isequal(app.ImportData(i).Type(1),3) && ~isequal(FactColVal,0) && ~isequal(FactLinVal,0)
                if ~isequal(FactColVal,0) || ~isequal(FactLinVal,0)
                    integerTest1 =~ mod(FactColVal,1);
                    integerTest2 =~ mod(FactLinVal,1);
                    
                    if integerTest1 && FactColVal < 0
                        app.NbColMod.Value = app.NbColOri.Value*abs(FactColVal);
                    else
                        app.NbColMod.Value = app.NbColOri.Value/FactColVal;
                    end
                    
                    if integerTest2 && FactLinVal < 0
                        app.NbLinMod.Value = app.NbLinOri.Value*abs(FactLinVal);
                    else
                        app.NbLinMod.Value = app.NbLinOri.Value/FactLinVal;
                    end
                    
                    DataCorr = CorrectMapShape(app,Data);
                    
                    if isequal(app.RotAngle.Value,90) || isequal(app.RotAngle.Value,180) || isequal(app.RotAngle.Value,270)
                        DataCorr = imrotate(DataCorr,app.RotAngle.Value);
                    end
                    
                else
                    %app.NbColMod.Value = app.NbColOri.Value;
                    %app.NbLinMod.Value = app.NbLinOri.Value;
                    DataCorr = Data;
                end
                
                % If X-ray apply correction
                if app.ApplyDTcorr.Value && isequal(app.ImportData(i).Type(3),1)
                    MapCps = DataCorr/(app.DwellTimeValue.Value/1000);
                    MapCpsCorr = MapCps./(1 - (app.DeadTimeValue.Value*10^-9) .* MapCps);
                    
                    DTcorrection1 = MapCpsCorr-MapCps;
                    %imagesc(DTcorrection1), axis image; colorbar;
                    
                    DataCorr = DataCorr + DTcorrection1*(app.DwellTimeValue.Value/1000);
                end
                
                if app.ReplaceNeG.Value
                    WhereNeG = find(DataCorr(:) < 0);
                    DataCorr(WhereNeG) = zeros(size(WhereNeG));
                    if length(WhereNeG) > 1
                        disp(['***  ',num2str(length(WhereNeG)),' negative pixel values replaced'])
                    end
                end
                
                if app.ReplaceNaN.Value
                    WhereNaN = find(isnan(DataCorr(:)));
                    DataCorr(WhereNaN) = zeros(size(WhereNaN));
                    if length(WhereNaN) > 1
                        disp(['***  ',num2str(length(WhereNaN)),' NaN pixel values replaced'])
                    end
                end
                if app.ReplaceINF.Value
                    WhereINF = find(isinf(DataCorr(:)));
                    DataCorr(WhereINF) = zeros(size(WhereINF));
                    if length(WhereINF) > 1
                        disp(['***  ',num2str(length(WhereINF)),' Inf pixel values replaced'])
                    end
                end
                
                
                app.ImportData(i).DataCorr = DataCorr;
            end
            
            GeneratePlot(app);
        end
        
        
        function Data2Plot_CORR = CorrectMapShape(app,Data2Plot_UNCOR)
            % Function taken from XMapTools 3 and variable names adjusted
            % this code was tested on 10.05.16 by PL
            
            SizeOri = size(Data2Plot_UNCOR);
            Data2Plot_CORR = Data2Plot_UNCOR;
            
            if app.FactCol.Value >0 && app.FactLin.Value > 0
                Data2Plot_CORR = Data2Plot_UNCOR(1:app.FactLin.Value:end,1:app.FactCol.Value:end);
            end
            
            if app.FactCol.Value >0 && app.FactLin.Value < 0
                
                SizeModi = SizeOri;
                SizeModi(1) = SizeOri(1)*abs(app.FactLin.Value);
                
                Data2Plot_CORR = zeros(SizeModi);
                
                for i = 1:abs(app.FactLin.Value)
                    Data2Plot_CORR(i:abs(app.FactLin.Value):end-(abs(app.FactLin.Value)-i),:) = Data2Plot_UNCOR;
                end
                
            end
            
            if app.FactCol.Value <0 && app.FactLin.Value > 0
                
                SizeModi = SizeOri;
                SizeModi(2) = SizeOri(2)*abs(app.FactCol.Value);
                
                Data2Plot_CORR = zeros(SizeModi);
                
                for i = 1:abs(app.FactCol.Value)
                    Data2Plot_CORR(:,i:abs(app.FactCol.Value):end-(abs(app.FactCol.Value)-i)) = Data2Plot_UNCOR;
                end
                
            end
            
            if app.FactCol.Value <0 && app.FactLin.Value < 0
                
                SizeModi = SizeOri;
                SizeModi(1) = SizeOri(1)*abs(app.FactLin.Value);
                SizeModi(2) = SizeOri(2)*abs(app.FactCol.Value);
                
                Data2Plot_CORR = zeros(SizeModi);
                
                for i = 1:abs(app.FactCol.Value)
                    Data2Plot_CORR(1:abs(app.FactLin.Value):end-1,i:abs(app.FactCol.Value):end-(abs(app.FactCol.Value)-i)) = Data2Plot_UNCOR;
                end
                
                % Then duplicate the rows
                for i = 1:abs(app.FactLin.Value)
                    Data2Plot_CORR(i:abs(app.FactLin.Value):end-(abs(app.FactLin.Value)-i),:) = Data2Plot_CORR(1:abs(app.FactLin.Value):end-(abs(app.FactLin.Value)-1),:);
                end
            end
            
            
            
            
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
            
            app.ImportToolXMapToolsUIFigure.Visible = 'off';
            
            movegui(app.ImportToolXMapToolsUIFigure,'center');
            
            app.XMapToolsApp = XMapToolsApp;
            
            app.ImportData(1).FileName = '';
            app.ImportData(1).Name = '';
            app.ImportData(1).NameDisp = '';
            app.ImportData(1).Type = [];
            app.ImportData(1).ElInd = [];
            app.ImportData(1).OxInd = [];
            app.ImportData(1).Data = [];
            app.ImportData(1).DataCorr = [];
            
            % Read the last settings (if available)
            if isequal(exist('Import.txt'),2)
                fid =fopen('Import.txt','r');
                
                while 1
                    TheLin = fgetl(fid);
                    
                    if isequal(TheLin,-1) || isequal(TheLin,'')
                        break
                    end
                    
                    TheStr = textscan(TheLin,'%s');
                    TheStr = TheStr{1};
                    
                    if iscell(TheStr)
                        switch TheStr{1}
                            case 'DT_correction:'
                                app.ApplyDTcorr.Value = str2double(char(TheStr{2}));
                                
                            case 'DT_param(DwellTime,DeadTime):'
                                app.DwellTimeValue.Value = str2double(TheStr{2});
                                app.DeadTimeValue.Value = str2double(TheStr{3});
                                
                            case 'Fact(Col,Lin):'
                                if ~isequal(str2double(TheStr{2}),0)
                                    app.FactCol.Value = str2double(TheStr{2});
                                else
                                    app.FactCol.Value = 1;
                                end
                                if ~isequal(str2double(TheStr{3}),0)
                                    app.FactLin.Value = str2double(TheStr{3});
                                else
                                    app.FactLin.Value = 1;
                                end
                                
                            case 'Rotation:'
                                app.RotAngle.Value = str2double(TheStr{2});
                                
                            case 'Replace_Negative_Values:'
                                app.ReplaceNeG.Value = str2double(char(TheStr{2}));
                                
                            case 'Replace_NaN_Values:'
                                app.ReplaceNaN.Value = str2double(char(TheStr{2}));
                                
                        end
                    end
                end
            else
                app.FactCol.Value = 1;
                app.FactLin.Value = 1;
            end
            app.ImportToolXMapToolsUIFigure.Visible = 'on';
            
            app.ApplyAll_Type = 1;
            app.ApplyAll_Destination = 1;
            
            Button_addPushed(app, 0);
        end

        % Button pushed function: Button_add
        function Button_addPushed(app, event)
            
            if length(app.ImportData) > 1
                PrevPos = length(app.ImportData);
            elseif isempty(app.ImportData)
                PrevPos = 0;
            elseif length(app.ImportData(1).FileName) > 1
                PrevPos = 1;
            else
                PrevPos = 0;
            end
            
            d = uiprogressdlg(app.ImportToolXMapToolsUIFigure,'Message','Select map files','Title','XMapTools');
            
            ElOxDataDef = app.XMapToolsApp.ElOxDataDef;
            
            f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
            cd(app.XMapToolsApp.XMapTools_LastDir);
            [FileName,PathName,FilterIndex] = uigetfile({'*.txt;*.asc;*.dat;*.csv;','Map Files (*.txt, *.asc, *.dat, *.csv)';'*.*',  'All Files (*.*)'},'Pick map file(s)','MultiSelect', 'on');
            delete(f)
            figure(app.ImportToolXMapToolsUIFigure);
            
            % Update working directory
            %cd(PathName);
            %app.XMapToolsApp.XMapTools_LastDir = cd;
            
            
            if ~FilterIndex
                return
            end
            
            if iscell(FileName)
                NbMaps = length(FileName);
            else
                NbMaps = 1;
                FileName = {FileName};
            end
            
            WarningMessage = {};
            Skip = 0;
            
            
            d = uiprogressdlg(app.ImportToolXMapToolsUIFigure,'Message','Loading selected files','Title','XMapTools');
            d.Value = 0;
            
            for i = 1:NbMaps
                
                d.Value = i/NbMaps;
                
                NameCell = textscan(FileName{i},'%s','Delimiter','.');
                Name = NameCell{1}{1};
                
                %   -----------------------------------------------
                %    FileName    Description             Variable
                %   -----------------------------------------------
                %    Si.txt      WDS map         ->      Si
                %    _Si.txt     EDS map         ->      Si_EDS
                %    *Si.txt     Quanti map      ->      Si
                
                %    #-Si.txt    Bkg(-) map      ->      Si_back-
                %    #Si.txt     Bkg(+) map      ->      Si_back+
                %   -----------------------------------------------
                
                TheCode = Name(1);
                Type = [0,0,0];
                ElInd = 0;
                OxInd = 0;
                
                %   -----------------------------------------------
                %                       Type (1)
                %   -----------------------------------------------
                %   ->  1       Element
                %   ->  2       Oxide
                %   ->  3       Isotope
                %   ->  4       Other Type (cannot be calibrated)
                %   -----------------------------------------------
                %                       Type (2)
                %   -----------------------------------------------
                %   ->  0       Other (not applicable)
                %   ->  1       Intensity
                %   ->  2       wt/wt
                %   -----------------------------------------------
                %                       Type (3)
                %   -----------------------------------------------
                %   ->  0       Other (not applicable)
                %   ->  1       Potentially WDS
                %   ->  2       EDS
                %   ->  3       Background
                
                AddName = '';
                
                switch TheCode    % Check for EDS and Quanti
                    case '_'   % EDS
                        Type(2) = 1;    % We assume it is intensity
                        Type(3) = 2;
                        Name2 = Name(2:end);
                        AddName = '_EDS';
                    case '*'   % Quanti
                        Type(2) = 2;
                        Name2 = Name(2:end);
                    otherwise
                        Type(3) = 1;  % To be checked with the name
                        Name2 = Name;
                        NameForIndex = Name2;
                end
                
                % Filter for comments in name after _ or - (for Rich Taylor / Zeiss)
                Check4Separator = find(ismember(Name2,'_'));
                if isempty(Check4Separator)
                    Check4Separator = find(ismember(Name2,'-'));
                end
                if isempty(Check4Separator)
                    Name3 = Name2;
                else
                    Name3 = Name2(1:Check4Separator(1)-1);
                end
                
                % Map Type 1:
                if ~isempty(str2num(Name3(1)))
                    % Name starts with a digit...
                    CharIdx = regexp(NameForIndex,['\D']);
                    if length(CharIdx) >= 1
                        NameForEl = Name2(CharIdx);
                        Idx = find(ismember(ElOxDataDef.ElList,NameForEl));
                        if ~isempty(Idx)
                            Type(1) = 3;
                            Type(3) = 0;
                            ElInd = Idx;
                        else
                            Type(1) = 4;
                        end
                    else
                        Type(1) = 4;
                    end
                else
                    % Name starts with a letter...
                    NameForIndex = Name3;
                    Idx = find(ismember(ElOxDataDef.ElList,NameForIndex));
                    if ~isempty(Idx)
                        Type(1) = 1;
                        Type(2) = 1;    %PL 23.11.20
                        ElInd = Idx;
                    else
                        Idx = find(ismember(ElOxDataDef.OxList,NameForIndex));
                        if ~isempty(Idx)
                            Type(1) = 2;
                            Type(2) = 2;
                            Type(3) = 0;
                            OxInd = Idx;
                            ElInd = ElOxDataDef.OxElIdx(Idx);   % 13.04.23 when importing oxides, the corresponding elements must be indexed
                        else
                            Type(1) = 4;
                            Type(3) = 0;
                        end
                    end
                end
                
                % Try first to load to avoid writting in Import Data
                WriteMap = 1;
                try
                    if ~isequal(char(FileName{i}),'Import.txt') && ~isequal(char(FileName{i}),'last_calibration.txt') && ~isequal(char(FileName{i}),'last_k_data.txt') && ~isequal(char(FileName{i}),'log_xmaptools.txt') && ~isequal(char(FileName{i}),'log_dvt.txt')
                        
                        Extension  = FileName{i}(end-2:end);
                        
                        if isequal(Extension,'csv')
                            NewMap = readtable([PathName,char(FileName{i})]);
                            NewMap = table2array(NewMap);
                        else
                            NewMap = load([PathName,char(FileName{i})]);
                        end
                        
                    else
                        error(['*** Error: ',char(FileName{i}),' is a log file or an internal file']);
                    end
                catch ME
                    WriteMap = 0;
                    WarningMessage{end+1} = ['File ',char(FileName{i}),' cannot be imported (wrong format) skipped'];
                    Skip = Skip + 1;
                end
                
                if WriteMap
                    app.ImportData(PrevPos+i-Skip).FileName = char(FileName{i});
                    app.ImportData(PrevPos+i-Skip).Name = char(NameForIndex);
                    app.ImportData(PrevPos+i-Skip).NameDisp = [char(NameForIndex),AddName];
                    app.ImportData(PrevPos+i-Skip).Type = Type;
                    app.ImportData(PrevPos+i-Skip).ElInd = ElInd;
                    app.ImportData(PrevPos+i-Skip).OxInd = OxInd;
                    app.ImportData(PrevPos+i-Skip).Checked = 1;
                    
                    app.ImportData(PrevPos+i-Skip).Data = NewMap;
                    app.ImportData(PrevPos+i-Skip).DataCorr = NewMap;
                end
            end
            
            % APPLY CORRECTIONS
            
            GenerateTable(app); % which also plots
            
            close(d)
            
        end

        % Cell edit callback: UITable
        function UITableCellEdit(app, event)
            indices = event.Indices;
            newData = event.NewData;
            
            Table = app.UITable.Data;
            if isequal(indices(2),size(Table,2)) && isequal(char(Table(indices(1),indices(2))),'Eliminate')
                % Delete
                app.ImportData(indices(1)) = [];
                if isempty(app.ImportData)
                    app.UITable.Data = {};
                    app.ImportdataButton.Enable = 'off';
                    return
                else
                    GenerateTable(app);
                end
            end
            
            if isequal(indices(2),8)
                % we change the destination
                PreviousType = app.ImportData(indices(1)).Type(1);
                if PreviousType > 9
                    OriginalType = floor(PreviousType/10);
                else
                    OriginalType = PreviousType;
                end
                
                switch newData
                    case 'Intensity'
                        
                    case 'Quanti'
                        app.ImportData(indices(1)).Type(1) = 11;
                        app.ImportData(indices(1)).Type(2) = 2;     % wt-%
                        app.ImportData(indices(1)).Type(3) = 0;     % other
                        
                    case 'Merged'
                        app.ImportData(indices(1)).Type(1) = 12;
                        app.ImportData(indices(1)).Type(2) = 2;     % wt-%
                        app.ImportData(indices(1)).Type(3) = 0;     % other
                        
                        if isequal(app.ApplyAll_Destination,1)
                            app.ApplyAll_Destination = 0;
                            Answer = uiconfirm(app.ImportToolXMapToolsUIFigure, 'Apply this change to all maps?', 'XMapTools', 'Options', {'Yes','No'},'DefaultOption', 'No');
                            %Answer = questdlg('Apply this change to all maps?','XMapTools','Yes','No','No');
                            
                            if isequal(Answer,'Yes')
                                for i = 1:length(app.ImportData)
                                    app.ImportData(i).Type(1) = 12;
                                    app.ImportData(i).Type(2) = 2;     % wt-%
                                    app.ImportData(i).Type(3) = 0;     % other
                                end
                            end
                        end
                        
                end
                
                
                % Type(2) = 2;      % wt-%
                % Type(3) = 0;      % other
                GenerateTable(app);
            end
            
            if isequal(indices(2),2)
                switch newData
                    case 'Element'
                        app.ImportData(indices(1)).Checked = 0;
                        app.ImportData(indices(1)).Type(1) = 1;
                        %app.ImportData(indices(1)).Name = '';
                        app.ImportData(indices(1)).ElInd = 0;
                        app.ImportData(indices(1)).OxInd = 0;
                    case 'Oxide'
                        app.ImportData(indices(1)).Checked = 0;
                        app.ImportData(indices(1)).Type(1) = 2;
                        %app.ImportData(indices(1)).Name = '';
                        app.ImportData(indices(1)).ElInd = 0;
                        app.ImportData(indices(1)).OxInd = 0;
                    case 'Isotope'
                        app.ImportData(indices(1)).Checked = 0;
                        app.ImportData(indices(1)).Type(1) = 3;
                        %app.ImportData(indices(1)).Name = '';
                        app.ImportData(indices(1)).ElInd = 0;
                        app.ImportData(indices(1)).OxInd = 0;
                        
                        if isequal(app.ApplyAll_Type,1)
                            app.ApplyAll_Type = 0;
                            %Answer = questdlg('Apply this change to all maps?','XMapTools','Yes','No','No');
                            Answer = uiconfirm(app.ImportToolXMapToolsUIFigure, 'Apply this change to all maps?', 'XMapTools', 'Options', {'Yes','No'},'DefaultOption', 'No');
                            
                            
                            if isequal(Answer,'Yes')
                                for i = 1:length(app.ImportData)
                                    app.ImportData(i).Checked = 0;
                                    app.ImportData(i).Type(1) = 3;
                                    app.ImportData(i).ElInd = 0;
                                    app.ImportData(i).OxInd = 0;
                                    
                                    Idx = find(ismember(app.XMapToolsApp.ElOxDataDef.ElList,app.ImportData(i).Name));
                                    if Idx
                                        app.ImportData(i).Checked = 1;
                                        app.ImportData(indices(1)).Type(1) = 3;
                                        app.ImportData(indices(1)).Type(2) = 1;     % Intensity
                                        app.ImportData(indices(1)).Type(3) = 0;     % other
                                        app.ImportData(indices(1)).ElInd = Idx;
                                        app.ImportData(indices(1)).OxInd = 0;
                                    end
                                    
                                end
                            end
                        end
                        
                    case 'Other'
                        app.ImportData(indices(1)).Checked = 1;
                end
                
                newData = app.ImportData(indices(1)).Name;
                
                GenerateTable(app);
            end
            
            if isequal(indices(2),3) || isequal(indices(2),2)
                % simply check the element/oxide name
                switch app.ImportData(indices(1)).Type(1)
                    case {1,11,12}
                        Idx = find(ismember(app.XMapToolsApp.ElOxDataDef.ElList,newData));
                        
                        if Idx
                            app.ImportData(indices(1)).Checked = 1;
                            app.ImportData(indices(1)).Type(1) = 1;
                            app.ImportData(indices(1)).Type(2) = 1;     % Intensity
                            app.ImportData(indices(1)).Type(3) = 1;     % wds?
                            app.ImportData(indices(1)).Name = newData;
                            app.ImportData(indices(1)).ElInd = Idx;
                            app.ImportData(indices(1)).OxInd = 0;
                        end
                    case 2
                        Idx = find(ismember(app.XMapToolsApp.ElOxDataDef.OxList,newData));
                        
                        if Idx
                            app.ImportData(indices(1)).Checked = 1;
                            app.ImportData(indices(1)).Type(1) = 2;
                            app.ImportData(indices(1)).Type(2) = 2;     % wt-%
                            app.ImportData(indices(1)).Type(3) = 0;     % other
                            app.ImportData(indices(1)).Name = newData;
                            app.ImportData(indices(1)).ElInd = 0;
                            app.ImportData(indices(1)).OxInd = Idx;
                            
                        end
                    case 3
                        Idx = find(ismember(app.XMapToolsApp.ElOxDataDef.ElList,newData));
                        
                        if Idx
                            app.ImportData(indices(1)).Checked = 1;
                            app.ImportData(indices(1)).Type(1) = 3;
                            app.ImportData(indices(1)).Type(2) = 1;     % Intensity
                            app.ImportData(indices(1)).Type(3) = 0;     % other
                            app.ImportData(indices(1)).Name = newData;
                            app.ImportData(indices(1)).ElInd = Idx;
                            app.ImportData(indices(1)).OxInd = 0;
                        end
                end
                
                GenerateTable(app);
            end
            
            
        end

        % Value changed function: ListMaps
        function ListMapsValueChanged(app, event)
            GeneratePlot(app);
        end

        % Value changed function: ApplyDTcorr, DeadTimeValue, 
        % DwellTimeValue, FactCol, FactLin, ReplaceNaN, ReplaceNeG, 
        % RotAngle
        function ValueChangedCorrection(app, event)
            if isequal(app.FactCol.Value,0)
                app.FactCol.Value = 1;
            end
            if isequal(app.FactLin.Value,0)
                app.FactLin.Value = 1;
            end
            
            CalcCorrection(app);
            
        end

        % Button pushed function: ImportdataButton
        function ImportdataButtonPushed(app, event)
            
            % we add the data to the XMapTools' data and close the window
            
            IdxMe = 0;
            IdxMeEl = 0;
            IdxQt = 0;
            IdxQtEl = 0;
            
            for i = 1:length(app.ImportData)
                
                %   Type(1)     Type            Destination
                % ------------------------------------------------
                %   ->  1       Element     ->  Intensity
                %   ->  2       Oxide       ->  Merged
                %   ->  3       Isotope     ->  Quanti
                %   ->  4       Other       ->  Other
                
                %   ->  11      Element     ->  Quanti
                %   ->  12      Element     ->  Merged
                
                
                switch app.ImportData(i).Type(1)
                    case 1                                  % It
                        
                        Idx = length(app.XMapToolsApp.XMapToolsData.MapData.It.Names)+1;
                        
                        app.XMapToolsApp.XMapToolsData.MapData.It.Names{Idx} = app.ImportData(i).NameDisp;
                        
                        % Types is for now Type (3) check compatibility
                        % with pre-XMapTools 4 projects
                        app.XMapToolsApp.XMapToolsData.MapData.It.Types(Idx) = app.ImportData(i).Type(3);
                        app.XMapToolsApp.XMapToolsData.MapData.It.ElInd(Idx) = app.ImportData(i).ElInd;
                        app.XMapToolsApp.XMapToolsData.MapData.It.Data(Idx).Map = app.ImportData(i).DataCorr;
                        
                        
                    case {2,12}                                  % Me
                        
                        if isequal(IdxMe,0)
                            
                            IdxMe = length(app.XMapToolsApp.XMapToolsData.MapData.Me.Names)+1;
                            
                            app.XMapToolsApp.XMapToolsData.MapData.Me.Names{IdxMe} = 'Imported_Maps';
                            app.XMapToolsApp.XMapToolsData.MapData.Me.IsOxide(IdxMe) = 1;
                            app.XMapToolsApp.XMapToolsData.MapData.Me.MaskFile{IdxMe} = 'none';
                            app.XMapToolsApp.XMapToolsData.MapData.Me.NbCalibPoints(IdxMe) = 0;
                        end
                        
                        IdxMeEl = IdxMeEl+1;
                        
                        app.XMapToolsApp.XMapToolsData.MapData.Me.Data(IdxMe).ElNames{IdxMeEl} = app.ImportData(i).NameDisp;
                        app.XMapToolsApp.XMapToolsData.MapData.Me.Data(IdxMe).ElInd(IdxMeEl) = app.ImportData(i).ElInd;
                        app.XMapToolsApp.XMapToolsData.MapData.Me.Data(IdxMe).OxInd(IdxMeEl) = app.ImportData(i).OxInd;
                        app.XMapToolsApp.XMapToolsData.MapData.Me.Data(IdxMe).CData(IdxMeEl).Map = app.ImportData(i).DataCorr;
                        
                        
                    case {3,11}                                  % Qt
                        
                        if isequal(IdxQt,0)
                            
                            IdxQt = length(app.XMapToolsApp.XMapToolsData.MapData.Qt.Names)+1;
                            
                            app.XMapToolsApp.XMapToolsData.MapData.Qt.Names{IdxQt} = 'Imported_Maps';
                            app.XMapToolsApp.XMapToolsData.MapData.Qt.IsOxide(IdxQt) = 0;
                            app.XMapToolsApp.XMapToolsData.MapData.Qt.MaskFile{IdxQt} = 'none';
                            app.XMapToolsApp.XMapToolsData.MapData.Qt.NbCalibPoints(IdxQt) = 0;
                        end
                        
                        IdxQtEl = IdxQtEl+1;
                        
                        app.XMapToolsApp.XMapToolsData.MapData.Qt.Data(IdxQt).ElNames{IdxQtEl} = app.ImportData(i).Name;
                        app.XMapToolsApp.XMapToolsData.MapData.Qt.Data(IdxQt).ElInd(IdxQtEl) = app.ImportData(i).ElInd;
                        app.XMapToolsApp.XMapToolsData.MapData.Qt.Data(IdxQt).OxInd(IdxQtEl) = app.ImportData(i).OxInd;
                        app.XMapToolsApp.XMapToolsData.MapData.Qt.Data(IdxQt).CData(IdxQtEl).Map = app.ImportData(i).DataCorr;
                        
                        
                        
                    case 4                                  % Ot
                        
                        Idx = length(app.XMapToolsApp.XMapToolsData.MapData.Ot.Names)+1;
                        
                        app.XMapToolsApp.XMapToolsData.MapData.Ot.Names{Idx} = app.ImportData(i).NameDisp;
                        app.XMapToolsApp.XMapToolsData.MapData.Ot.Types(Idx) = 0; % feature not used yet!
                        app.XMapToolsApp.XMapToolsData.MapData.Ot.Data(Idx).Map = app.ImportData(i).DataCorr;
                        
                end
                
            end
            
            
            % Save settings
            
            fid = fopen('Import.txt','w');
            fprintf(fid,'%s\n','#######################################################');
            fprintf(fid,'%s\n','#     XMapTools Last Import Settings (DO NOT EDIT)    #');
            fprintf(fid,'%s\n','#######################################################');
            fprintf(fid,'%s\n','...');
            fprintf(fid,'%s\t\t\t\t\t\t%.0f\n','DT_correction:',app.ApplyDTcorr.Value);
            fprintf(fid,'%s\t\t%.3f\t\t%.3f\n','DT_param(DwellTime,DeadTime):',app.DwellTimeValue.Value,app.DeadTimeValue.Value);
            fprintf(fid,'%s\t\t\t\t\t\t%.0f\t\t\t%.0f\n','Fact(Col,Lin):',app.FactCol.Value,app.FactLin.Value);
            fprintf(fid,'%s\t\t\t\t\t\t\t%.0f\n','Rotation:',app.RotAngle.Value);
            fprintf(fid,'%s\t\t\t%.0f\n','Replace_Negative_Values:',app.ReplaceNeG.Value);
            fprintf(fid,'%s\t\t\t\t\t%.0f\n','Replace_NaN_Values:',app.ReplaceNaN.Value);
            fclose(fid);
            
            
            
            
            %keyboard
            
            %app.XMapToolsApp.XMapToolsData.MapData
            delete(app);
            
        end

        % Button pushed function: Button_help
        function Button_helpPushed(app, event)
            
            if isempty(app.XMapToolsApp.Id_HelpTool)
                Help_Display(app.XMapToolsApp,'XMT_help_ImportMaps.html');
            else
                app.XMapToolsApp.Id_HelpTool.UpdateTextHelp('XMT_help_ImportMaps.html');
            end
            
        end

        % Close request function: ImportToolXMapToolsUIFigure
        function ImportToolXMapToolsUIFigureCloseRequest(app, event)
            a = 1;
            delete(app)
            pause(0.1)
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create ImportToolXMapToolsUIFigure and hide until all components are created
            app.ImportToolXMapToolsUIFigure = uifigure('Visible', 'off');
            app.ImportToolXMapToolsUIFigure.Position = [100 100 888 640];
            app.ImportToolXMapToolsUIFigure.Name = 'Import Tool – XMapTools ';
            app.ImportToolXMapToolsUIFigure.CloseRequestFcn = createCallbackFcn(app, @ImportToolXMapToolsUIFigureCloseRequest, true);

            % Create GridLayout
            app.GridLayout = uigridlayout(app.ImportToolXMapToolsUIFigure);
            app.GridLayout.ColumnWidth = {'1x', '1x', '1x'};
            app.GridLayout.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};

            % Create Image
            app.Image = uiimage(app.GridLayout);
            app.Image.Layout.Row = 1;
            app.Image.Layout.Column = 1;
            app.Image.ImageSource = 'logo_xmap_final.png';

            % Create GridLayout3
            app.GridLayout3 = uigridlayout(app.GridLayout);
            app.GridLayout3.ColumnWidth = {'1x'};
            app.GridLayout3.RowHeight = {'1x'};
            app.GridLayout3.Padding = [50 10 50 10];
            app.GridLayout3.Layout.Row = 1;
            app.GridLayout3.Layout.Column = 3;

            % Create ImportdataButton
            app.ImportdataButton = uibutton(app.GridLayout3, 'push');
            app.ImportdataButton.ButtonPushedFcn = createCallbackFcn(app, @ImportdataButtonPushed, true);
            app.ImportdataButton.Icon = '323-add.png';
            app.ImportdataButton.Enable = 'off';
            app.ImportdataButton.Layout.Row = 1;
            app.ImportdataButton.Layout.Column = 1;
            app.ImportdataButton.Text = 'Import data';

            % Create GridLayout2
            app.GridLayout2 = uigridlayout(app.GridLayout);
            app.GridLayout2.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout2.RowHeight = {'1x'};
            app.GridLayout2.Layout.Row = 1;
            app.GridLayout2.Layout.Column = 2;

            % Create Button_add
            app.Button_add = uibutton(app.GridLayout2, 'push');
            app.Button_add.ButtonPushedFcn = createCallbackFcn(app, @Button_addPushed, true);
            app.Button_add.Icon = '056-plus.png';
            app.Button_add.IconAlignment = 'center';
            app.Button_add.Layout.Row = 1;
            app.Button_add.Layout.Column = 4;
            app.Button_add.Text = '';

            % Create Button_help
            app.Button_help = uibutton(app.GridLayout2, 'push');
            app.Button_help.ButtonPushedFcn = createCallbackFcn(app, @Button_helpPushed, true);
            app.Button_help.Icon = '061-info.png';
            app.Button_help.Layout.Row = 1;
            app.Button_help.Layout.Column = 1;
            app.Button_help.Text = '';

            % Create TabGroup
            app.TabGroup = uitabgroup(app.GridLayout);
            app.TabGroup.Layout.Row = [2 11];
            app.TabGroup.Layout.Column = [1 3];

            % Create ListofmapsTab
            app.ListofmapsTab = uitab(app.TabGroup);
            app.ListofmapsTab.Title = 'List of maps';

            % Create GridLayout4
            app.GridLayout4 = uigridlayout(app.ListofmapsTab);
            app.GridLayout4.ColumnWidth = {'1x', '1x', '1x'};
            app.GridLayout4.RowHeight = {'1x', '1x', '1x'};
            app.GridLayout4.Padding = [30 30 30 30];

            % Create UITable
            app.UITable = uitable(app.GridLayout4);
            app.UITable.ColumnName = {'File'; 'Type'; 'Map'; 'Data'; 'Special'; 'DTC'; 'OC'; 'Destination*'; 'Action*'};
            app.UITable.RowName = {};
            app.UITable.CellEditCallback = createCallbackFcn(app, @UITableCellEdit, true);
            app.UITable.Layout.Row = [1 3];
            app.UITable.Layout.Column = [1 3];
            app.UITable.FontSize = 9;

            % Create CorrectionsTab
            app.CorrectionsTab = uitab(app.TabGroup);
            app.CorrectionsTab.Title = 'Corrections';

            % Create GridLayout5
            app.GridLayout5 = uigridlayout(app.CorrectionsTab);
            app.GridLayout5.ColumnWidth = {'0.3x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.3x'};
            app.GridLayout5.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout5.ColumnSpacing = 8;
            app.GridLayout5.RowSpacing = 8;
            app.GridLayout5.Padding = [20 20 20 20];

            % Create DeadtimecorrectionforWDSmapsEPMALabel
            app.DeadtimecorrectionforWDSmapsEPMALabel = uilabel(app.GridLayout5);
            app.DeadtimecorrectionforWDSmapsEPMALabel.FontWeight = 'bold';
            app.DeadtimecorrectionforWDSmapsEPMALabel.Layout.Row = 4;
            app.DeadtimecorrectionforWDSmapsEPMALabel.Layout.Column = [1 5];
            app.DeadtimecorrectionforWDSmapsEPMALabel.Text = 'Dead time correction for WDS maps (EPMA)';

            % Create ApplyDTcorr
            app.ApplyDTcorr = uicheckbox(app.GridLayout5);
            app.ApplyDTcorr.ValueChangedFcn = createCallbackFcn(app, @ValueChangedCorrection, true);
            app.ApplyDTcorr.Text = 'Apply correction';
            app.ApplyDTcorr.Layout.Row = 5;
            app.ApplyDTcorr.Layout.Column = [2 3];
            app.ApplyDTcorr.Value = true;

            % Create DwelltimemsEditFieldLabel
            app.DwelltimemsEditFieldLabel = uilabel(app.GridLayout5);
            app.DwelltimemsEditFieldLabel.HorizontalAlignment = 'right';
            app.DwelltimemsEditFieldLabel.Layout.Row = 5;
            app.DwelltimemsEditFieldLabel.Layout.Column = [4 5];
            app.DwelltimemsEditFieldLabel.Text = 'Dwell time (ms)';

            % Create DwellTimeValue
            app.DwellTimeValue = uieditfield(app.GridLayout5, 'numeric');
            app.DwellTimeValue.Limits = [0 Inf];
            app.DwellTimeValue.ValueChangedFcn = createCallbackFcn(app, @ValueChangedCorrection, true);
            app.DwellTimeValue.Layout.Row = 5;
            app.DwellTimeValue.Layout.Column = 6;
            app.DwellTimeValue.Value = 150;

            % Create DetectordeadtimensEditFieldLabel
            app.DetectordeadtimensEditFieldLabel = uilabel(app.GridLayout5);
            app.DetectordeadtimensEditFieldLabel.HorizontalAlignment = 'right';
            app.DetectordeadtimensEditFieldLabel.Layout.Row = 5;
            app.DetectordeadtimensEditFieldLabel.Layout.Column = [7 9];
            app.DetectordeadtimensEditFieldLabel.Text = 'Detector dead time (ns)';

            % Create DeadTimeValue
            app.DeadTimeValue = uieditfield(app.GridLayout5, 'numeric');
            app.DeadTimeValue.Limits = [0 Inf];
            app.DeadTimeValue.ValueChangedFcn = createCallbackFcn(app, @ValueChangedCorrection, true);
            app.DeadTimeValue.Layout.Row = 5;
            app.DeadTimeValue.Layout.Column = 10;
            app.DeadTimeValue.Value = 300;

            % Create GeneralcorrectionsallmapsLabel
            app.GeneralcorrectionsallmapsLabel = uilabel(app.GridLayout5);
            app.GeneralcorrectionsallmapsLabel.FontWeight = 'bold';
            app.GeneralcorrectionsallmapsLabel.Layout.Row = 1;
            app.GeneralcorrectionsallmapsLabel.Layout.Column = [1 5];
            app.GeneralcorrectionsallmapsLabel.Text = 'General corrections (all maps)';

            % Create ReplaceNeG
            app.ReplaceNeG = uicheckbox(app.GridLayout5);
            app.ReplaceNeG.ValueChangedFcn = createCallbackFcn(app, @ValueChangedCorrection, true);
            app.ReplaceNeG.Text = 'Replace negative values (by zeros)';
            app.ReplaceNeG.Layout.Row = 2;
            app.ReplaceNeG.Layout.Column = [2 4];
            app.ReplaceNeG.Value = true;

            % Create ReplaceNaN
            app.ReplaceNaN = uicheckbox(app.GridLayout5);
            app.ReplaceNaN.ValueChangedFcn = createCallbackFcn(app, @ValueChangedCorrection, true);
            app.ReplaceNaN.Text = 'Replace NaN (by zeros)';
            app.ReplaceNaN.Layout.Row = 2;
            app.ReplaceNaN.Layout.Column = [5 7];
            app.ReplaceNaN.Value = true;

            % Create ResamplingOrientationLAICPMSdatafromIoliteonlyLabel
            app.ResamplingOrientationLAICPMSdatafromIoliteonlyLabel = uilabel(app.GridLayout5);
            app.ResamplingOrientationLAICPMSdatafromIoliteonlyLabel.FontWeight = 'bold';
            app.ResamplingOrientationLAICPMSdatafromIoliteonlyLabel.Layout.Row = 7;
            app.ResamplingOrientationLAICPMSdatafromIoliteonlyLabel.Layout.Column = [1 8];
            app.ResamplingOrientationLAICPMSdatafromIoliteonlyLabel.Text = 'Re-sampling & Orientation (LA-ICP-MS data from Iolite only!)';

            % Create XcolumnsEditFieldLabel
            app.XcolumnsEditFieldLabel = uilabel(app.GridLayout5);
            app.XcolumnsEditFieldLabel.HorizontalAlignment = 'right';
            app.XcolumnsEditFieldLabel.Layout.Row = 9;
            app.XcolumnsEditFieldLabel.Layout.Column = 2;
            app.XcolumnsEditFieldLabel.Text = 'X (columns)';

            % Create NbColOri
            app.NbColOri = uieditfield(app.GridLayout5, 'numeric');
            app.NbColOri.Layout.Row = 9;
            app.NbColOri.Layout.Column = 3;

            % Create YrowsEditFieldLabel
            app.YrowsEditFieldLabel = uilabel(app.GridLayout5);
            app.YrowsEditFieldLabel.HorizontalAlignment = 'right';
            app.YrowsEditFieldLabel.Layout.Row = 10;
            app.YrowsEditFieldLabel.Layout.Column = 2;
            app.YrowsEditFieldLabel.Text = 'Y (rows)';

            % Create NbLinOri
            app.NbLinOri = uieditfield(app.GridLayout5, 'numeric');
            app.NbLinOri.Layout.Row = 10;
            app.NbLinOri.Layout.Column = 3;

            % Create NbColMod
            app.NbColMod = uieditfield(app.GridLayout5, 'numeric');
            app.NbColMod.Layout.Row = 9;
            app.NbColMod.Layout.Column = 4;

            % Create NbLinMod
            app.NbLinMod = uieditfield(app.GridLayout5, 'numeric');
            app.NbLinMod.Layout.Row = 10;
            app.NbLinMod.Layout.Column = 4;

            % Create FactCol
            app.FactCol = uieditfield(app.GridLayout5, 'numeric');
            app.FactCol.ValueChangedFcn = createCallbackFcn(app, @ValueChangedCorrection, true);
            app.FactCol.Layout.Row = 9;
            app.FactCol.Layout.Column = 6;

            % Create FactLin
            app.FactLin = uieditfield(app.GridLayout5, 'numeric');
            app.FactLin.ValueChangedFcn = createCallbackFcn(app, @ValueChangedCorrection, true);
            app.FactLin.Layout.Row = 10;
            app.FactLin.Layout.Column = 6;

            % Create RotationcounterclockwiseEditFieldLabel
            app.RotationcounterclockwiseEditFieldLabel = uilabel(app.GridLayout5);
            app.RotationcounterclockwiseEditFieldLabel.HorizontalAlignment = 'right';
            app.RotationcounterclockwiseEditFieldLabel.Layout.Row = 9;
            app.RotationcounterclockwiseEditFieldLabel.Layout.Column = [7 9];
            app.RotationcounterclockwiseEditFieldLabel.Text = 'Rotation (counterclockwise)';

            % Create RotAngle
            app.RotAngle = uieditfield(app.GridLayout5, 'numeric');
            app.RotAngle.ValueChangedFcn = createCallbackFcn(app, @ValueChangedCorrection, true);
            app.RotAngle.Layout.Row = 9;
            app.RotAngle.Layout.Column = 10;

            % Create OriginalLabel
            app.OriginalLabel = uilabel(app.GridLayout5);
            app.OriginalLabel.HorizontalAlignment = 'center';
            app.OriginalLabel.VerticalAlignment = 'bottom';
            app.OriginalLabel.Layout.Row = 8;
            app.OriginalLabel.Layout.Column = 3;
            app.OriginalLabel.Text = 'Original';

            % Create ModifiedLabel
            app.ModifiedLabel = uilabel(app.GridLayout5);
            app.ModifiedLabel.HorizontalAlignment = 'center';
            app.ModifiedLabel.VerticalAlignment = 'bottom';
            app.ModifiedLabel.Layout.Row = 8;
            app.ModifiedLabel.Layout.Column = 4;
            app.ModifiedLabel.Text = 'Modified';

            % Create ScalingfactorLabel
            app.ScalingfactorLabel = uilabel(app.GridLayout5);
            app.ScalingfactorLabel.HorizontalAlignment = 'center';
            app.ScalingfactorLabel.VerticalAlignment = 'bottom';
            app.ScalingfactorLabel.Layout.Row = 8;
            app.ScalingfactorLabel.Layout.Column = [5 7];
            app.ScalingfactorLabel.Text = 'Scaling factor';

            % Create Label
            app.Label = uilabel(app.GridLayout5);
            app.Label.HorizontalAlignment = 'right';
            app.Label.FontAngle = 'italic';
            app.Label.Layout.Row = 10;
            app.Label.Layout.Column = [7 9];
            app.Label.Text = '0 | 90 | 180 | 270';

            % Create ReplaceINF
            app.ReplaceINF = uicheckbox(app.GridLayout5);
            app.ReplaceINF.Text = 'Replace Inf & -Inf (by zeros)';
            app.ReplaceINF.Layout.Row = 2;
            app.ReplaceINF.Layout.Column = [8 10];
            app.ReplaceINF.Value = true;

            % Create VisualizationTab
            app.VisualizationTab = uitab(app.TabGroup);
            app.VisualizationTab.Title = 'Visualization';

            % Create GridLayout6
            app.GridLayout6 = uigridlayout(app.VisualizationTab);
            app.GridLayout6.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout6.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout6.Padding = [20 20 20 20];

            % Create ListMaps
            app.ListMaps = uidropdown(app.GridLayout6);
            app.ListMaps.Items = {'No map available', ''};
            app.ListMaps.ValueChangedFcn = createCallbackFcn(app, @ListMapsValueChanged, true);
            app.ListMaps.Layout.Row = 1;
            app.ListMaps.Layout.Column = [1 2];
            app.ListMaps.Value = 'No map available';

            % Create UncorrecteddataLabel
            app.UncorrecteddataLabel = uilabel(app.GridLayout6);
            app.UncorrecteddataLabel.HorizontalAlignment = 'center';
            app.UncorrecteddataLabel.VerticalAlignment = 'bottom';
            app.UncorrecteddataLabel.FontWeight = 'bold';
            app.UncorrecteddataLabel.Layout.Row = 2;
            app.UncorrecteddataLabel.Layout.Column = [1 4];
            app.UncorrecteddataLabel.Text = 'Uncorrected data';

            % Create CorrecteddataLabel
            app.CorrecteddataLabel = uilabel(app.GridLayout6);
            app.CorrecteddataLabel.HorizontalAlignment = 'center';
            app.CorrecteddataLabel.VerticalAlignment = 'bottom';
            app.CorrecteddataLabel.FontWeight = 'bold';
            app.CorrecteddataLabel.Layout.Row = 2;
            app.CorrecteddataLabel.Layout.Column = [5 8];
            app.CorrecteddataLabel.Text = 'Corrected data';

            % Create UIAxes_Cor
            app.UIAxes_Cor = uiaxes(app.GridLayout6);
            xlabel(app.UIAxes_Cor, 'X (columns)')
            ylabel(app.UIAxes_Cor, 'Y (rows)')
            app.UIAxes_Cor.PlotBoxAspectRatio = [1.11174242424242 1 1];
            app.UIAxes_Cor.YTick = [0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1];
            app.UIAxes_Cor.Layout.Row = [3 9];
            app.UIAxes_Cor.Layout.Column = [5 8];

            % Create UIAxes_Ori
            app.UIAxes_Ori = uiaxes(app.GridLayout6);
            xlabel(app.UIAxes_Ori, 'X (columns)')
            ylabel(app.UIAxes_Ori, 'Y (rows)')
            app.UIAxes_Ori.PlotBoxAspectRatio = [1.11174242424242 1 1];
            app.UIAxes_Ori.YTick = [0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1];
            app.UIAxes_Ori.Layout.Row = [3 9];
            app.UIAxes_Ori.Layout.Column = [1 4];

            % Show the figure after all components are created
            app.ImportToolXMapToolsUIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = Import_Tool_exported(varargin)

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.ImportToolXMapToolsUIFigure)

            % Execute the startup function
            runStartupFcn(app, @(app)startupFcn(app, varargin{:}))

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.ImportToolXMapToolsUIFigure)
        end
    end
end