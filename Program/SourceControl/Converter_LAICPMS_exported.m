classdef Converter_LAICPMS_exported < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        ConverterLAICPMS                matlab.ui.Figure
        GridLayout                      matlab.ui.container.GridLayout
        Image                           matlab.ui.control.Image
        Tree                            matlab.ui.container.Tree
        Node_Background                 matlab.ui.container.TreeNode
        Node_PrimaryStandard            matlab.ui.container.TreeNode
        Node_SecondaryStandard          matlab.ui.container.TreeNode
        Node_Scans                      matlab.ui.container.TreeNode
        GridLayout2                     matlab.ui.container.GridLayout
        LoadDatafilesButton             matlab.ui.control.Button
        TypeOfInstrumentDropDown        matlab.ui.control.DropDown
        LogfileCheckBox                 matlab.ui.control.CheckBox
        TypeOfFileDropDown              matlab.ui.control.DropDown
        HelpButton                      matlab.ui.control.Button
        MassSpectrometerLabel           matlab.ui.control.Label
        LaserLabel                      matlab.ui.control.Label
        NameFormatDropDown              matlab.ui.control.DropDown
        BACKGROUNDPanel                 matlab.ui.container.Panel
        GridLayout5                     matlab.ui.container.GridLayout
        ExtractIntegrationsBackgroundButton  matlab.ui.control.Button
        BackgroundFilterPercentSpinnerLabel  matlab.ui.control.Label
        BackgroundFilterPercentSpinner  matlab.ui.control.Spinner
        Background_CorrectionList       matlab.ui.control.DropDown
        BackgroundApplyButton           matlab.ui.control.Button
        PRIMARYSTANDARDPanel            matlab.ui.container.Panel
        GridLayout6                     matlab.ui.container.GridLayout
        PrimaryStd_FilterPercentSpinnerLabel  matlab.ui.control.Label
        PrimaryStd_FilterPercentSpinner  matlab.ui.control.Spinner
        PrimaryStd_List                 matlab.ui.control.DropDown
        PrimaryStd_ApplyButton          matlab.ui.control.Button
        PrimaryStd_CorrectionList       matlab.ui.control.DropDown
        SECONDARYSTANDARDPanel          matlab.ui.container.Panel
        GridLayout7                     matlab.ui.container.GridLayout
        SecondaryStd_List               matlab.ui.control.DropDown
        SecondaryStd_FilterPercentSpinnerLabel  matlab.ui.control.Label
        SecondaryStd_FilterPercentSpinner  matlab.ui.control.Spinner
        SecondaryStd_UITable            matlab.ui.control.Table
        SecondaryStd_IntStdLabel        matlab.ui.control.Label
        SecondaryStd_IntStd             matlab.ui.control.DropDown
        SecondaryStd_ApplyButton        matlab.ui.control.Button
        SecondaryStd_CalibrationMtx     matlab.ui.control.Button
        SecondaryStd_PlotSecStd         matlab.ui.control.Button
        SecondaryStd_Cancel             matlab.ui.control.Button
        SecondaryStd_ListPS             matlab.ui.control.DropDown
        MAPSPanel                       matlab.ui.container.Panel
        GridLayout8                     matlab.ui.container.GridLayout
        Maps_ApplyButton                matlab.ui.control.Button
        Maps_TimeshiftSpinnerLabel      matlab.ui.control.Label
        Maps_TimeshiftSpinner           matlab.ui.control.Spinner
        Maps_List                       matlab.ui.control.DropDown
        CheckButton                     matlab.ui.control.Button
        ShowButton                      matlab.ui.control.Button
        ExportButton                    matlab.ui.control.Button
        Maps_SelElement                 matlab.ui.control.DropDown
        GridLayout9                     matlab.ui.container.GridLayout
        SweepLabel                      matlab.ui.control.Label
        Position_Min                    matlab.ui.control.Spinner
        Position_Max                    matlab.ui.control.Spinner
        TimeshiftSpinner                matlab.ui.control.Spinner
        TimeshiftLabel                  matlab.ui.control.Label
        IntegrationMenuLabel            matlab.ui.control.Label
        GridLayout10                    matlab.ui.container.GridLayout
        ZoomIn                          matlab.ui.control.Button
        ZoomOut                         matlab.ui.control.Button
        ZoomReset                       matlab.ui.control.Button
        PlotMenuDropDown                matlab.ui.control.DropDown
        PlotMenuDropDownLabel           matlab.ui.control.Label
        GridLayout11                    matlab.ui.container.GridLayout
        StdfilesLabel                   matlab.ui.control.Label
        Options_AddStdButton            matlab.ui.control.Button
        Options_StandarddataDropDown    matlab.ui.control.DropDown
        OptionsLabel                    matlab.ui.control.Label
        DebugMode                       matlab.ui.control.CheckBox
        TestMode                        matlab.ui.control.CheckBox
        Plot                            matlab.ui.control.UIAxes
        ContextMenu                     matlab.ui.container.ContextMenu
        CopyMenu                        matlab.ui.container.Menu
    end

    
    properties (Access = private)
        XMapToolsApp
        LastDir         % from XMapTools (startup)
        DataFiles       % Data not organised from different files
        Data            % Data clean
        
        TimeShiftCorrData   % Data from the automated time shift correction
        Integrations
        Data4Plot       % Contain all data to be plotted in PlotDropDown
        Start_Idx_Data4Plot_BackgroundCorrected % Description
        RefData_LAICPMS % Description
        PathStdFolder   % Description
        WaitBar         % Handle of the waitbar
        ContextMenu_Tree_Background
        ContextMenu_Tree_Standard
        
        iPrStd          % Index of the primary standard data to be saved
        
        DataFromLogGenerator
        
        ErrorTracker
        
        PxDataRaw
        
        CheckDateTimeFormat
        DateTimeFormat
        
    end
    
    properties (Access = public)
        Log             % Data from the log file
        ExchangeFormator
        
    end
    
    methods (Access = private)
        
        function ExtractTimeIntegration(app)
            
            t = app.Data.tsec;
            
            %List_Type = '';
            Type = zeros(size(t));
            SeqN = zeros(size(t));
            SeqListName = {};
            StartSeq = zeros(size(t));
            BackMeas = zeros(size(t));
            BackMeasID = zeros(size(t));
            SignalMeas = zeros(size(t));
            SignalMeasTotal = zeros(size(t));
            SignalMeasID = zeros(size(t));
            
            ComptScan = 0;
            
            app.Integrations.SeqListName = '';
            
            app.Integrations.Background.Names = {};
            app.Integrations.Background.PositionsOri = [];
            app.Integrations.Background.Positions = [];
            app.Integrations.Background.Times = NaT(1);
            %app.Integrations.Background.ROI(1).ROI = [];
            
            app.Integrations.TypeNames = '';
            app.Integrations.Measurements(1).Names = {};
            app.Integrations.Measurements(1).PositionsOri = [];
            app.Integrations.Measurements(1).Positions = [];
            app.Integrations.Measurements(1).Times = NaT(1);
            
            app.Integrations.Measurements(1).X1 = [];
            app.Integrations.Measurements(1).Y1 = [];
            app.Integrations.Measurements(1).X2 = [];
            app.Integrations.Measurements(1).Y2 = [];
            app.Integrations.Measurements(1).SpotSize = [];
            app.Integrations.Measurements(1).ScanVel = [];
            app.Integrations.Measurements(1).Distance = [];
            
            app.Integrations.Measurements(1).Slope = [];
            app.Integrations.Measurements(1).Intercept = [];
            
            
            %figure, hold on
            
            CountMeasurements = zeros(10);
            
            % Sequence
            SqIndex = find(~isnan(app.Log.Table.SequenceNumber));
            for i = 1:length(SqIndex)
                
                Id1 = SqIndex(i);
                t1 = app.Log.DT_Log_Corr(Id1);
                if i < length(SqIndex)
                    Id2 = SqIndex(i+1);
                    t2 = app.Log.DT_Log_Corr(Id2);
                else
                    Id2 = length(app.Log.Table.SequenceNumber);
                    t2 = app.Log.DT_Log_Corr(Id2);
                end
                
                tf = find(isbetween(app.Data.time_DT,t1,t2));
                SeqN(tf) = i*ones(size(tf));
                
                SeqLaserState = app.Log.Table.LaserState(Id1:Id2);
                IsLaserOn = find(ismember(SeqLaserState,'On'));
                
                StartSeq(tf(1)) = 1;
                for j = Id1:Id2
                    if isequal(app.Log.Table.LaserState{j},'On')
                        tback = find(isbetween(app.Data.time_DT,app.Log.DT_Log_Corr(SqIndex(i)),app.Log.DT_Log_Corr(j)));
                        % we filter 10%
                        %Filter = round(length(tback)*0.05);
                        %BackMeas(tback(Filter:end-Filter)) = ones(size(tback(Filter:end-Filter)));
                        %BackMeasID(tback(Filter:end-Filter)) = i*ones(size(tback(Filter:end-Filter)));
                        break
                    end
                end
                
                SeqName = app.Log.Table.Comment{SqIndex(i)};  
                %if isequal(length(SeqName{1}),3)         
                %    AnaID = str2num(SeqName{1}{3}); 
                %else 
                %    AnaID = 0;
                %end
                
                if isequal(app.NameFormatDropDown.Value,1)
                    SeqName = textscan(SeqName,'%s'); 
                    SeqName = SeqName{1}{1};
                end
                
                if isequal(app.NameFormatDropDown.Value,2)
                    SeqName = textscan(SeqName,'%s','delimiter','_'); 
                    SeqName = SeqName{1}{1};
                end
                
                IsSeq = find(ismember(app.Integrations.TypeNames,SeqName));
                if ~isempty(IsSeq)
                    Type(tf) = IsSeq*ones(size(tf));
                else
                    app.Integrations.TypeNames{end+1} = SeqName;
                    IsSeq = length(app.Integrations.TypeNames);
                    Type(tf) = IsSeq*ones(size(tf));
                end
                
                SeqListName{i} = SeqName;
                
                app.Integrations.Background.Names{i} = SeqName;
                app.Integrations.Background.PositionsOri(i,1:2) = [tback(1),tback(end)];
                app.Integrations.Background.Positions(i,1:2) = [tback(1),tback(end)];
                app.Integrations.Background.Times(i,1:2) = [app.Data.time_DT(tback(1)),app.Data.time_DT(tback(end))];
                
                % SIGNAL
                CountMeasurements(IsSeq) =  CountMeasurements(IsSeq) + 1;
                IdxCount = CountMeasurements(IsSeq);
                
                app.Integrations.Measurements(IsSeq).Names{IdxCount} = SeqName;
                
                tAblation = find(isbetween(app.Data.time_DT,app.Log.DT_Log_Corr(SqIndex(i)+IsLaserOn(end-1)-1),app.Log.DT_Log_Corr(SqIndex(i)+IsLaserOn(end-1))));
                
                app.Integrations.Measurements(IsSeq).PositionsOri(IdxCount,1:2) = [tAblation(1),tAblation(end)];
                app.Integrations.Measurements(IsSeq).Positions(IdxCount,1:2) = [tAblation(1),tAblation(end)];
                app.Integrations.Measurements(IsSeq).Times(IdxCount,1:2) = [app.Data.time_DT(tAblation(1)),app.Data.time_DT(tAblation(end))];
                
                % add later the coordinates and scan speed, etc...
                app.Integrations.Measurements(IsSeq).X1(CountMeasurements(IsSeq)) = app.Log.Table.X_um_(SqIndex(i)+IsLaserOn(end-1)-1);
                app.Integrations.Measurements(IsSeq).Y1(CountMeasurements(IsSeq)) = app.Log.Table.Y_um_(SqIndex(i)+IsLaserOn(end-1)-1);
                app.Integrations.Measurements(IsSeq).X2(CountMeasurements(IsSeq)) = app.Log.Table.X_um_(SqIndex(i)+IsLaserOn(end-1));
                app.Integrations.Measurements(IsSeq).Y2(CountMeasurements(IsSeq)) = app.Log.Table.Y_um_(SqIndex(i)+IsLaserOn(end-1));
                app.Integrations.Measurements(IsSeq).SpotSize(CountMeasurements(IsSeq)) = app.Log.Table.SpotSize_um_(SqIndex(i)+IsLaserOn(end-1)-1);
                app.Integrations.Measurements(IsSeq).ScanVel(CountMeasurements(IsSeq)) = app.Log.Table.ScanVelocity_um_s_(SqIndex(i)+IsLaserOn(end-1));
            end
            
            app.Integrations.SeqListName = SeqListName;
            
            app.Data.BackgroundCorrection = zeros(size(t));
            
            UpdateTimeIntegration_Background(app);
            
            % UPDATE TREE
            CleanTree(app,'Background');
            for i = 1:length(SeqListName)
                p = uitreenode(app.Node_Background,'Text',char(SeqListName{i}),'NodeData',[1,i]);
            end
            expand(app.Node_Background);
        end
        
        function Plot2CheckShift(app)
            
            hold(app.Plot,'off')
            plot(app.Plot,app.Data.time_DT,app.Data.SumData,'k');
            hold(app.Plot,'on')
            
            app.Plot.YScale = 'log';
            
            rp = rulerPanInteraction('Dimensions','x');
            app.Plot.Interactions = [rp];
            app.Plot.Visible = 'on';
            
            LaserOn = find(ismember(app.Log.Table.LaserState,'On'));
            TimeLaserOn = app.Log.DT_Log_Corr(LaserOn);
            plot(app.Plot,[TimeLaserOn,TimeLaserOn]',repmat([app.Plot.YLim(1),app.Plot.YLim(2)],length(TimeLaserOn),1)','r-');
            
            tb = axtoolbar(app.Plot,{'export','pan','zoomin','zoomout','restoreview'});
        end
        
        function SearchShiftAuto(app)
            
            % Detection of the first peak in the derivative
            DiffSumData = diff(app.Data.SumData);
            Mean10 = mean(DiffSumData(1:15));
            Std10 = std(DiffSumData(1:15));
            
            for iPos = 2:length(DiffSumData)
                if DiffSumData(iPos)-DiffSumData(iPos-1) > Mean10 + 20*Std10
                    break
                end
            end
            AblationStartSignal = app.Data.time_DT(iPos+1);
            
            % Laser log
            LaserOn = find(ismember(app.Log.Table.LaserState,'On'));
            AblationStartLog = app.Log.Table.Timestamp(LaserOn(1));
            
            % Shift & Correction
            TimeShift = duration(AblationStartSignal-AblationStartLog);
            
            app.Log.DT_Log_Corr = app.Log.DT_Log + TimeShift;
            
            app.TimeshiftSpinner.Value = seconds(TimeShift);
            app.TimeshiftSpinner.Visible = 'on';
            app.TimeshiftLabel.Visible = 'on';
            
        end
        
        function CleanTree(app,Tree)
            
            switch Tree
                case 'Background'
                    for i = 1:length(app.Node_Background.Children)
                        app.Node_Background.Children(1).delete;
                    end
                case 'Primary'
                    for i = 1:length(app.Node_PrimaryStandard.Children)
                        app.Node_PrimaryStandard.Children(1).delete;
                    end
                case 'Secondary'
                    for i = 1:length(app.Node_SecondaryStandard.Children)
                        app.Node_SecondaryStandard.Children(1).delete;
                    end
                case 'Scans'
                    for i = 1:length(app.Node_Scans.Children)
                        app.Node_Scans.Children(1).delete;
                    end
            end
            
        end
        
        function UpdateTimeIntegration_Background(app)
            
            for i = 1:size(app.Integrations.Background.PositionsOri,1)
                
                Interval = [app.Integrations.Background.PositionsOri(i,1):1:app.Integrations.Background.PositionsOri(i,2)];
                Filter = round(length(Interval)*(app.BackgroundFilterPercentSpinner.Value/100));
                
                app.Integrations.Background.Positions(i,1) = app.Integrations.Background.PositionsOri(i,1)+Filter;
                app.Integrations.Background.Positions(i,2) = app.Integrations.Background.PositionsOri(i,2)-Filter;
                
                app.Integrations.Background.Times(i,1:2) = [app.Data.time_DT(app.Integrations.Background.Positions(i,1)),app.Data.time_DT(app.Integrations.Background.Positions(i,2))];
                
            end
            
            % Update number of analytes for backgroud
            app.Data.BackgroundNbIntegration = zeros(size(app.Data.BackgroundCorrection(:,1)));
            for i = 1:size(app.Integrations.Background.Positions,1)
                NbIntegration = app.Integrations.Background.Positions(i,2)-app.Integrations.Background.Positions(i,1)+1;
                IntStart = app.Integrations.Background.Positions(i,2) + 1;
                if i < size(app.Integrations.Background.Positions,1)
                    IntEnd = app.Integrations.Background.Positions(i+1,1) - 1;
                    app.Data.BackgroundNbIntegration(IntStart:IntEnd) = NbIntegration*ones(size(app.Data.BackgroundNbIntegration(IntStart:IntEnd)));
                else
                    app.Data.BackgroundNbIntegration(IntStart:end) = NbIntegration*ones(size(app.Data.BackgroundNbIntegration(IntStart:end)));
                end
            end
            
        end
        
        function UpdateTimeIntegration_PrimaryStd(app)
            
            Value = app.PrimaryStd_List.Value;
            
            for i = 1:length(app.Integrations.Measurements(Value).Names)
                
                Interval = [app.Integrations.Measurements(Value).PositionsOri(i,1):1:app.Integrations.Measurements(Value).PositionsOri(i,2)];
                Filter = round(length(Interval)*(app.PrimaryStd_FilterPercentSpinner.Value/100));
                
                app.Integrations.Measurements(Value).Positions(i,1) = app.Integrations.Measurements(Value).PositionsOri(i,1)+Filter;
                app.Integrations.Measurements(Value).Positions(i,2) = app.Integrations.Measurements(Value).PositionsOri(i,2)-Filter;
                
                app.Integrations.Measurements(Value).Times(i,1:2) = [app.Data.time_DT(app.Integrations.Measurements(Value).Positions(i,1)),app.Data.time_DT(app.Integrations.Measurements(Value).Positions(i,2))];
            end
            
        end
        
        
        function ReadStandardData(app)
            
            app.RefData_LAICPMS.FileNames = '';
            app.RefData_LAICPMS.StdNames = '';
            app.RefData_LAICPMS.Data(1).Elem = '';
            app.RefData_LAICPMS.Data(1).Unit = '';
            app.RefData_LAICPMS.Data(1).Value = [];
            app.RefData_LAICPMS.Data(1).Unc = [];
            
            DIR = dir([app.PathStdFolder]);
            
            ComptFile = 0;
            for i = 1:length(DIR)
                
                if ~isequal(DIR(i).name,'.') && ~isequal(DIR(i).name,'..') && ~isequal(DIR(i).name,'.DS_Store')
                    ComptFile = ComptFile+1;
                    
                    FileDir = [app.PathStdFolder,'/',DIR(i).name];
                    FileName = DIR(i).name;
                    
                    AddSingleStandardFile(app,FileDir,FileName);
                    
                end
            end
            
            app.Options_StandarddataDropDown.Items = app.RefData_LAICPMS.FileNames;
            
        end
        
        function AddSingleStandardFile(app,FileDir,FileName)
            try
                Position = length(app.RefData_LAICPMS.FileNames) + 1;
                
                app.RefData_LAICPMS.FileNames{Position} = char(FileName);
                app.RefData_LAICPMS.StdNames{Position} = upper(char(FileName(1:end-4)));
                
                ComptStd = 0;
                
                fid = fopen(FileDir,'r');
                while 1
                    TheL = fgetl(fid);
                    
                    if isequal(TheL,-1)
                        break
                    end
                    
                    if length(TheL) > 1
                        if ~isequal(TheL(1),'!')
                            ComptStd = ComptStd+1;
                            
                            Out = textscan(TheL,'%s%s%f%s');
                            app.RefData_LAICPMS.Data(Position).Elem{ComptStd} = char(Out{1});
                            app.RefData_LAICPMS.Data(Position).Unit{ComptStd} = char(Out{2});
                            app.RefData_LAICPMS.Data(Position).Value(ComptStd) = Out{3};
                            if isequal(char(Out{4}),'-')
                                app.RefData_LAICPMS.Data(Position).Unc(ComptStd) = 0;
                            else
                                app.RefData_LAICPMS.Data(Position).Unc(ComptStd) = str2num(char(Out{4}));
                            end
                            
                        end
                    end
                end
                fclose(fid);
            catch ME
                disp(['Standard file ERROR: ',FileName])
            end
        end
        
        
        function PrepareSecondaryStandardData(app)
            
            SecStd = app.SecondaryStd_List.Items{app.SecondaryStd_List.Value};
            
            [IsStd] = ismember(app.RefData_LAICPMS.StdNames,upper(SecStd));
            
            if ~isequal(sum(IsStd),1)
                ListNames = app.RefData_LAICPMS.StdNames;
                ListNames{end+1} = 'Other...';
                
                Answer = listdlg('ListString',ListNames,'SelectionMode','single','Name','XMapTools','PromptString','Select your secondary standard in the list below, or other to select a file manually');
                
                if isempty(Answer)
                    return
                end
                
                if Answer < length(ListNames)
                    WhereStd = Answer;
                else
                    errordlg('You need to add your standard first in the option section!','XMapTools');
                    return
                end
            else
                WhereStd = find(IsStd);
            end
            
            ElListStd = app.RefData_LAICPMS.Data(WhereStd).Elem;
            ElAnalysis = app.Data.ElName;
            
            [IsElem,ElemIdx] = ismember(ElAnalysis,ElListStd);
            
            if ~isequal(sum(IsElem),length(IsElem))
                
                for i =1:length(IsElem)
                    if IsElem(i)
                        app.Data.SecondaryStandard_ElemConc(i) = app.RefData_LAICPMS.Data(WhereStd).Value(ElemIdx(i));
                    else
                        app.Data.SecondaryStandard_ElemConc(i) = 0;
                    end
                end
                
                %MissingEl = ElAnalysis(find(IsElem == 0));
                %uialert(app.ConverterLAICPMS,['Warning: reference data is not defined in the standard file for the following elements:',MissingEl,{''},'A composition of zero is assumed for the reference material in this case'],'XMapTools','CloseFcn','uiresume(gcbf)');
                %uiwait(gcbf)
                
            else
                % All elements were found in the standard file:
                app.Data.SecondaryStandard_ElemConc = app.RefData_LAICPMS.Data(WhereStd).Value(ElemIdx);
            end
            
            ElNames4Menu = '';
            for i = 1:length(app.Data.SecondaryStandard_ElemConc)
                ElNames4Menu{i} = [char(ElAnalysis{i}),' (',num2str(app.Data.SecondaryStandard_ElemConc(i)),' µg/g)'];
            end
            app.SecondaryStd_IntStd.Items = ElNames4Menu;
            app.SecondaryStd_IntStd.ItemsData = [1:length(ElNames4Menu)];
            
            Value = app.SecondaryStd_List.Value;
            
            % UPDATE TREE
            CleanTree(app,'Secondary');
            for i = 1:length(app.Integrations.Measurements(Value).Names)
                p = uitreenode(app.Node_SecondaryStandard,'Text',char(app.Integrations.Measurements(Value).Names{i}),'NodeData',[3,i]);
            end
            collapse(app.Node_PrimaryStandard);
            expand(app.Node_SecondaryStandard);
            
            
            UpdateTimeIntegration_SecondaryStd(app);
            
        end
        
        
        function UpdateTimeIntegration_SecondaryStd(app)
            
            Value = app.SecondaryStd_List.Value;
            
            for i = 1:length(app.Integrations.Measurements(Value).Names)
                
                Interval = [app.Integrations.Measurements(Value).PositionsOri(i,1):1:app.Integrations.Measurements(Value).PositionsOri(i,2)];
                Filter = round(length(Interval)*(app.SecondaryStd_FilterPercentSpinner.Value/100));
                
                app.Integrations.Measurements(Value).Positions(i,1) = app.Integrations.Measurements(Value).PositionsOri(i,1)+Filter;
                app.Integrations.Measurements(Value).Positions(i,2) = app.Integrations.Measurements(Value).PositionsOri(i,2)-Filter;
                
                app.Integrations.Measurements(Value).Times(i,1:2) = [app.Data.time_DT(app.Integrations.Measurements(Value).Positions(i,1)),app.Data.time_DT(app.Integrations.Measurements(Value).Positions(i,2))];
            end
            
        end
        
        
        
        function Con_Unk_Median = CalibrateSecondaryStandardFromIt(app,IntStdIdx)
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'XMapTools is running numbers';
            
            Con_Std = app.Data.PS(app.SecondaryStd_ListPS.Value).PrimaryStandard_ElemConc(IntStdIdx);
            Con_Unk = app.Data.SecondaryStandard_ElemConc(IntStdIdx);
            
            % Extract Itensity
            ValuePs = app.SecondaryStd_List.Value;
            
            SignalMeas = zeros(size(app.Data.time_DT));
            for i = 1:size(app.Integrations.Measurements(ValuePs).Positions,1)
                SignalMeas(app.Integrations.Measurements(ValuePs).Positions(i,1):app.Integrations.Measurements(ValuePs).Positions(i,2)) = ones(length([app.Integrations.Measurements(ValuePs).Positions(i,1):app.Integrations.Measurements(ValuePs).Positions(i,2)]),1);
            end
            IndSignal = find(SignalMeas);
            
            if app.DebugMode.Value
                app.WaitBar.Message = 'XMapTools is saving LAICPMS_SecondaryStd_BCInt.txt (debug mode on) 1/2';
                fid = fopen(fullfile(app.XMapToolsApp.XMapTools_LastDir,'LAICPMS_SecondaryStd_BCInt.txt'),'w');
                
                CodeS = '%s';
                CodeF = '%s';
                for i = 1:length(app.Data.ElName)
                    CodeS = [CodeS,'\t%s'];
                    CodeF = [CodeF,'\t%f'];
                end
                CodeS = [CodeS,'\n'];
                CodeF = [CodeF,'\n'];
                
                fprintf(fid,CodeS,'TimeStamp',app.Data.ElName{:});
                
                for i = 1:length(IndSignal)
                    fprintf(fid,CodeF,app.Data.time_DT(IndSignal(i)),app.Data.Cps_BackCorr(IndSignal(i),:));
                end
                fclose(fid);
                app.WaitBar.Message = 'XMapTools is running numbers';
            end
            
            % Outlier rejection
            Yi = app.Data.Cps_BackCorr(IndSignal,IntStdIdx);
            TF = isoutlier(Yi);
            IndSignal = IndSignal(find(TF == 0));
            
            It_Unk = app.Data.Cps_BackCorr(IndSignal,IntStdIdx);
            It_Std = app.Data.PS(app.SecondaryStd_ListPS.Value).Cps_PrimaryStandard(IndSignal,IntStdIdx);
            
            % Calculate k for the selected internal standard
            k = (It_Unk.*Con_Std)./(Con_Unk.*It_Std);
            k_mtx = repmat(k,1,size(app.Data.Cps_BackCorr,2));
            
            It_Unk_mtx = app.Data.Cps_BackCorr(IndSignal,:);
            It_Std_mtx = app.Data.PS(app.SecondaryStd_ListPS.Value).Cps_PrimaryStandard(IndSignal,:);
            Con_Std_mxt = repmat(app.Data.PS(app.SecondaryStd_ListPS.Value).PrimaryStandard_ElemConc,size(It_Unk_mtx,1),1);
            
            Con_Unk_mtx = (It_Unk_mtx.*Con_Std_mxt)./(k_mtx.*It_Std_mtx);
            
            % Filter NaN for low concentration element or high background
            % (PL 21.12.2022)
            Con_Unk_Median = zeros(size(app.Data.ElName));
            for i = 1:length(app.Data.ElName)
                WhereOk = find(Con_Unk_mtx(:,i) > 0);
                if ~isempty(WhereOk)
                    Con_Unk_Median(i) = median(Con_Unk_mtx(WhereOk,i));
                else
                    Con_Unk_Median(i) = NaN;
                end
            end
            % Con_Unk_Median = median(Con_Unk_mtx,1);
            
            if app.DebugMode.Value
                app.WaitBar.Message = 'XMapTools is saving LAICPMS_SecondaryStd_Comp.txt (debug mode on) 2/2';
                fid = fopen(fullfile(app.XMapToolsApp.XMapTools_LastDir,'LAICPMS_SecondaryStd_Comp.txt'),'w');
                
                fprintf(fid,CodeS,'TimeStamp',app.Data.ElName{:});
                
                for i = 1:length(IndSignal)
                    fprintf(fid,CodeF,app.Data.time_DT(IndSignal(i)),Con_Unk_mtx(i,:));
                end
                fclose(fid);
                app.WaitBar.Message = 'XMapTools is running numbers';
            end
            
            app.Data.SecondaryStandard_ElemConcMatrix = Con_Unk_mtx;
            
            close(app.WaitBar);
        end
        
        function PrepareScanData(app)
            
            Value = app.Maps_List.Value;
            
            % UPDATE TREE
            CleanTree(app,'Scans');
            for i = 1:length(app.Integrations.Measurements(Value).Names)
                p = uitreenode(app.Node_Scans,'Text',char(app.Integrations.Measurements(Value).Names{i}),'NodeData',[4,i]);
            end
            collapse(app.Node_SecondaryStandard);
            expand(app.Node_Scans);
            
            UpdateTimeIntegration_Scans(app);
            
        end
        
        function UpdateTimeIntegration_Scans(app)
            
            Value = app.Maps_List.Value;
            
            for i = 1:length(app.Integrations.Measurements(Value).Names)
                
                dt = app.Maps_TimeshiftSpinner.Value;
                
                app.Integrations.Measurements(Value).Positions(i,1) = app.Integrations.Measurements(Value).PositionsOri(i,1)+dt;
                app.Integrations.Measurements(Value).Positions(i,2) = app.Integrations.Measurements(Value).PositionsOri(i,2)+dt;
                
                app.Integrations.Measurements(Value).Times(i,1:2) = [app.Data.time_DT(app.Integrations.Measurements(Value).Positions(i,1)),app.Data.time_DT(app.Integrations.Measurements(Value).Positions(i,2))];
            end
            
        end
        
        function CalculateMapsCassis(app)
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'XMapTools is running numbers';
            
            Value = app.Maps_List.Value;
            
            X1 = app.Integrations.Measurements(Value).X1;
            Y1 = app.Integrations.Measurements(Value).Y1;
            
            X2 = app.Integrations.Measurements(Value).X2;
            Y2 = app.Integrations.Measurements(Value).Y2;
            
            Slope = (Y2-Y1)./(X2-X1);
            Int = Y1-Slope.*X1;
            Distance = sqrt((X2-X1).^2+(Y2-Y1).^2);
            
            SpotSize = app.Integrations.Measurements(Value).SpotSize;
            ScanVel = app.Integrations.Measurements(Value).ScanVel;
            
            DurationReal = Distance./ScanVel; % in seconds
            NbPixels = floor((Distance-SpotSize)./SpotSize); % -SpotSize added because 1/2 spot size is ignored at the begining and at the end
            
            DtPixel = seconds(DurationReal./(Distance./SpotSize));  % this is correct to get a constant DtPixel.
            % DtPixel = seconds(DurationReal./NbPixels);
            
            % I don't understand this line below (12.04.22)
            %NbSwipePerPixel = floor(DurationReal./NbPixels);
            
            % To avoid having zeros!
            %WhereZeros = find(NbSwipePerPixel == 0);
            %NbSwipePerPixel(WhereZeros) = ones(size(WhereZeros));
            
            NbSwipePerPixel = zeros(size(DurationReal));
            
            % -------------------------------------------------------------
            % Map reconstruction (second version Theoule-sur-Mer)
            Xi_all = [];
            Yi_all = [];
            Ti_all = [];
            
            %Xi = [];
            %Yi = [];
            %TableXY(1).Xi = [];
            %TableXY(1).Yi = [];
            %DistanceCheck = zeros(size(Distance));
            
            %CountPx = 0;
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools');
            app.WaitBar.Message = 'Building raw intensity datasets, please wait';
            
            for i = 1:length(X1)
                
                % Method: Theoule
                ti = find(isbetween(app.Data.time_DT,app.Integrations.Measurements(Value).Times(i,1),app.Integrations.Measurements(Value).Times(i,2)))';
                
                if isequal(X1(i),X2(i))
                    xi = X1(i).*ones(size(ti));
                else
                    xi = X1(i):(X2(i)-X1(i))/(length(ti)-1):X2(i);
                end
                if isequal(Y1(i),Y2(i))
                    yi = Y1(i).*ones(size(ti));
                else
                    yi = Y1(i):(Y2(i)-Y1(i))/(length(ti)-1):Y2(i);
                end
                
                Xi_all(end+1:end+length(xi)) = xi;
                Yi_all(end+1:end+length(yi)) = yi;
                Ti_all(end+1:end+length(ti)) = ti;
                
                if floor(length(xi)/NbPixels(i)) > 0
                    NbSwipePerPixel(i) = floor(length(xi)/NbPixels(i));
                end
                
                % The old Wengen method has been deleted in this function (4.4)
            end
            
            [X_grid,Y_grid] = meshgrid([min(Xi_all):SpotSize(1):max(Xi_all)],[min(Yi_all):SpotSize(1):max(Yi_all)]);
            
            EdgesX = min(X_grid(:))-0.5*SpotSize(1):SpotSize(1):max(X_grid(:))+0.5*SpotSize(1);
            EdgesY = min(Y_grid(:))-0.5*SpotSize(1):SpotSize(1):max(Y_grid(:))+0.5*SpotSize(1);
            
            xbin = discretize(X_grid(1,:), EdgesX);
            ybin = discretize(Y_grid(:,1)', EdgesY);
            
            
            % ----------------------------------------------------------------------------------------------------------------
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools');
            app.WaitBar.Message = 'Preparing the extraction of sweep data';
            
            % Search for number of sweeps within each pixel (Cassis):
            PixelIndices = 1:1:prod(size(X_grid));
            % PixelIndices = reshape(PixelIndices, size(X_grid));
            
            app.PxDataRaw.ElNames = app.Data.ElName;
            app.PxDataRaw.PixelIndices = PixelIndices;
            app.PxDataRaw.PixelCoordXY = zeros(length(PixelIndices),2);
            
            for iEl = 1:size(app.Data.Cps_BackCorr,2)
                for i = 1:length(PixelIndices)
                    app.PxDataRaw.ElData(iEl).PxData(i).NbSweep = [];
                    app.PxDataRaw.ElData(iEl).PxData(i).SweepIndices = [];
                    app.PxDataRaw.ElData(iEl).PxData(i).Intensity = [];
                end
            end
            
            MatrixNbSweepPerPixel = zeros(size(X_grid));
            MatrixX = zeros(size(X_grid));
            MatrixY = zeros(size(X_grid));
            
            count = 0;
            for i = 1:length(EdgesY)-1
                app.WaitBar.Value = i/(length(EdgesY)-1);
                
                for j = 1:length(EdgesX)-1
                    SweepIndices = find(Yi_all >= EdgesY(i) & Yi_all < EdgesY(i+1) & Xi_all >= EdgesX(j) & Xi_all < EdgesX(j+1));
                    MatrixNbSweepPerPixel(i,j) = length(SweepIndices);
                    
                    count = count + 1;
                    app.PxDataRaw.PixelCoordXY(count,1) = j; 
                    app.PxDataRaw.PixelCoordXY(count,2) = i; 
                    
                    MatrixX(i,j) = j;
                    MatrixY(i,j) = i;
                    
                    for iEl = 1:size(app.Data.Cps_BackCorr,2)
                        app.PxDataRaw.ElData(iEl).PxData(PixelIndices(count)).NbSweep = MatrixNbSweepPerPixel(i,j);
                        app.PxDataRaw.ElData(iEl).PxData(PixelIndices(count)).SweepIndices = SweepIndices;
                        app.PxDataRaw.ElData(iEl).PxData(PixelIndices(count)).Intensity = zeros(size(SweepIndices));
                    end
                end
            end
            % ----------------------------------------------------------------------------------------------------------------
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools');
            app.WaitBar.Message = 'Interpolating data and generating intensity maps';
            
            for i = 1:size(app.Data.Cps_BackCorr,2)
                
                app.WaitBar.Value = i/size(app.Data.Cps_BackCorr,2);
                
                % Method: Theoule
                app.Data.Cps_Maps(i).Map = zeros(size(X_grid));
                app.Data.Cps_Maps(i).Std_Map = zeros(size(X_grid));
                
                PxCompt = app.Data.Cps_BackCorr(Ti_all,i);
                IdxOk = find(isnan(PxCompt) == 0); % filter NaN out otherwise interpolation does not work fine
                Vq = griddata(Xi_all(IdxOk),Yi_all(IdxOk),PxCompt(IdxOk),X_grid,Y_grid,'cubic');
                
                app.Data.Cps_Maps(i).Map = Vq;
                
                for j = 1:length(app.Data.PS)
                    PxCompStd = app.Data.PS(j).Cps_PrimaryStandard(Ti_all,i);
                    IdxOk_Std = find(isnan(PxCompStd) == 0);
                    Vq_Std = griddata(Xi_all(IdxOk_Std),Yi_all(IdxOk_Std),PxCompStd(IdxOk_Std),X_grid,Y_grid);
                    
                    app.Data.StdMaps(i).Std_Map(j).Cps = Vq_Std;
                    app.Data.StdMaps(i).Std_Map(j).Conc = app.Data.PS(j).PrimaryStandard_ElemConc(i).*ones(size(Vq));
                    app.Data.StdMaps(i).Std_Map(j).StdName = app.SecondaryStd_ListPS.Items{j};
                end
                
                Vq_BackNbIntegration = griddata(Xi_all,Yi_all,app.Data.BackgroundNbIntegration(Ti_all),X_grid,Y_grid,'nearest');
                Vq_BackgroundCorrection = griddata(Xi_all,Yi_all,app.Data.BackgroundCorrection(Ti_all,i),X_grid,Y_grid,'nearest');
                Vq_PixelNbIntegration = NbSwipePerPixel(1).*ones(size(Vq_BackNbIntegration));
                
                app.Data.StdMaps(i).Int_Back = Vq_BackgroundCorrection;
                app.Data.StdMaps(i).Sweeps_Back = Vq_BackNbIntegration;
                app.Data.StdMaps(i).Sweeps_Pixel = Vq_PixelNbIntegration;
                
                % ----------------------------------------------------------------------------------------------------------------
                % Extract the sweep data                                     New 4.4
                for IdxPx = 1:length(app.PxDataRaw.PixelIndices)
                    app.PxDataRaw.ElData(i).PxData(IdxPx).Intensity = PxCompt(app.PxDataRaw.ElData(i).PxData(IdxPx).SweepIndices);
                end
                % ----------------------------------------------------------------------------------------------------------------
                
                if 0 && isequal(i,36)
                    figure, imagesc(X_grid(1,:),Y_grid(:,1)',Vq), hold on, scatter(Xi_all,Yi_all,20*ones(size(Xi_all)),PxCompt,'filled'), colorbar
                    axis([1.2385e+05    1.2643e+05    0.5492e+05    0.5508e+05])
                    
                    caxis([0 1000])
                    colormap([0,0,0;parula(64)]);
                    
                    %figure, imagesc(X_grid(1,:),Y_grid(:,1)',Vq_Std), hold on, scatter(Xi_all,Yi_all,20*ones(size(Xi_all)),PxCompStd,'filled'), colorbar
                    
                    IdxOk = find(isnan(PxCompt) == 0);
                    
                    Vq_nearest = griddata(Xi_all(IdxOk),Yi_all(IdxOk),PxCompt(IdxOk),X_grid,Y_grid,'nearest');
                    Vq_linear = griddata(Xi_all(IdxOk),Yi_all(IdxOk),PxCompt(IdxOk),X_grid,Y_grid,'linear');
                    Vq_natural = griddata(Xi_all(IdxOk),Yi_all(IdxOk),PxCompt(IdxOk),X_grid,Y_grid,'natural');
                    Vq_cubic = griddata(Xi_all(IdxOk),Yi_all(IdxOk),PxCompt(IdxOk),X_grid,Y_grid,'cubic');
                    
                    Sel = find(Yi_all > 5.492e4 & Yi_all < 5.493e4);
                    figure, hold on, plot(Xi_all(Sel),PxCompt(Sel),'.-k'),
                    Sel = find(Y_grid(:) > 5.492e4 & Y_grid(:) < 5.493e4);
                    plot(X_grid(Sel),Vq_nearest(Sel),'o-r')
                    plot(X_grid(Sel),Vq_linear(Sel),'o-b')
                    %plot(X_grid(Sel),Vq_natural(Sel),'o-g')
                    plot(X_grid(Sel),Vq_cubic(Sel),'o-m')
                    plot(X_grid(Sel),Vq_old(Sel),'o-g')
                    
                    legend('scan','nearest','linear','cubic','old')
                    %legend('scan','nearest','linear','natural','cubic','old')
                    
                    keyboard
                end
                
            end
            
            % ----------------------------------------------------------------------------------------------------------------
            if 0
                % benchmark test for sweep data extraction
                ElIdx = 1;   % Si for Jiahui's test dataset
                
                TheMapTheoule = app.Data.Cps_Maps(ElIdx).Map;
                
                TheMapCassis = zeros(size(TheMapTheoule));
                
                for i = 1:length(app.PxDataRaw.PixelCoordXY)
                    if ~isempty(app.PxDataRaw.ElData(ElIdx).PxData(i).Intensity)
                        TheMapCassis(app.PxDataRaw.PixelCoordXY(i,2),app.PxDataRaw.PixelCoordXY(i,1)) = mean(app.PxDataRaw.ElData(ElIdx).PxData(i).Intensity);
                    end
                end
                
                figure,
                tiledlayout('flow')
                nexttile
                imagesc(TheMapTheoule), axis image, colorbar
                %caxis([3e6,1e7])
                nexttile
                imagesc(TheMapCassis), axis image, colorbar
                %caxis([3e6,1e7])
            end
            
            app.Data.MapGeometryCheck.XY_Spots = [Xi_all;Yi_all]';
            app.Data.MapGeometryCheck.Radius_Spots = repmat(SpotSize(1)/2,length(Xi_all),1);
            app.Data.MapGeometryCheck.X_grid = X_grid;
            app.Data.MapGeometryCheck.Y_grid = Y_grid;
            app.Data.MapGeometryCheck.xbin = [];            % not used in this version
            app.Data.MapGeometryCheck.ybin = [];            % not used in this version
            
            close(app.WaitBar)
            
            return
            
        end
        
        
        
        function CalculateMapsTheoule(app)
            
            CalcWengenOld = 0;
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'XMapTools is running numbers';
            
            Value = app.Maps_List.Value;
            
            X1 = app.Integrations.Measurements(Value).X1;
            Y1 = app.Integrations.Measurements(Value).Y1;
            
            X2 = app.Integrations.Measurements(Value).X2;
            Y2 = app.Integrations.Measurements(Value).Y2;
            
            Slope = (Y2-Y1)./(X2-X1);
            Int = Y1-Slope.*X1;
            Distance = sqrt((X2-X1).^2+(Y2-Y1).^2);
            
            SpotSize = app.Integrations.Measurements(Value).SpotSize;
            ScanVel = app.Integrations.Measurements(Value).ScanVel;
            
            DurationReal = Distance./ScanVel; % in seconds
            NbPixels = floor((Distance-SpotSize)./SpotSize); % -SpotSize added because 1/2 spot size is ignored at the begining and at the end
            
            DtPixel = seconds(DurationReal./(Distance./SpotSize));  % this is correct to get a constant DtPixel.
            % DtPixel = seconds(DurationReal./NbPixels);
            
            % I don't understand this line below (12.04.22)
            %NbSwipePerPixel = floor(DurationReal./NbPixels);
            
            % To avoid having zeros!
            %WhereZeros = find(NbSwipePerPixel == 0);
            %NbSwipePerPixel(WhereZeros) = ones(size(WhereZeros));
            
            NbSwipePerPixel = zeros(size(DurationReal));
            
            % -------------------------------------------------------------
            % Map reconstruction (second version Theoule-sur-Mer)
            Xi_all = [];
            Yi_all = [];
            Ti_all = [];
            
            Xi = [];
            Yi = [];
            %TableXY(1).Xi = [];
            %TableXY(1).Yi = [];
            %DistanceCheck = zeros(size(Distance));
            
            CountPx = 0;
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools');
            app.WaitBar.Message = 'Building raw intensity datasets, please wait';
            
            for i = 1:length(X1)
                
                % Method: Theoule
                ti = find(isbetween(app.Data.time_DT,app.Integrations.Measurements(Value).Times(i,1),app.Integrations.Measurements(Value).Times(i,2)))';
                
                if isequal(X1(i),X2(i))
                    xi = X1(i).*ones(size(ti));
                else
                    xi = X1(i):(X2(i)-X1(i))/(length(ti)-1):X2(i);
                end
                if isequal(Y1(i),Y2(i))
                    yi = Y1(i).*ones(size(ti));
                else
                    yi = Y1(i):(Y2(i)-Y1(i))/(length(ti)-1):Y2(i);
                end
                
                Xi_all(end+1:end+length(xi)) = xi;
                Yi_all(end+1:end+length(yi)) = yi;
                Ti_all(end+1:end+length(ti)) = ti;
                
                if floor(length(xi)/NbPixels(i)) > 0
                    NbSwipePerPixel(i) = floor(length(xi)/NbPixels(i));
                end
                
                % Method: Wengen
                if CalcWengenOld
                    app.WaitBar.Value = i/length(X1);
                    
                    x = [X1(i) X2(i)];
                    y = [Y1(i) Y2(i)];
                    
                    marker_dist = SpotSize(i);
                    dist_from_start = [0, sqrt((x(2:end)-x(1:end-1)).^2 + (y(2:end)-y(1:end-1)).^2)];
                    marker_locs = dist_from_start(1)+marker_dist/2 : marker_dist : dist_from_start(end) - marker_dist/2;
                    marker_indices = interp1( dist_from_start, 1 : length(dist_from_start), marker_locs);
                    marker_base_pos = floor(marker_indices);
                    
                    marker_base_pos = marker_base_pos(find(marker_base_pos == 1));
                    marker_indices = marker_indices(find(marker_base_pos == 1));
                    
                    weight_second = marker_indices - marker_base_pos;
                    
                    Xi_s = x(marker_base_pos) .* (1-weight_second) + x(marker_base_pos+1) .* weight_second;   % position of each marker in xy
                    Yi_s = y(marker_base_pos) .* (1-weight_second) + y(marker_base_pos+1) .* weight_second;   % position of each marker in xy
                    
                    Xi(end+1:end+length(Xi_s)) = Xi_s;
                    Yi(end+1:end+length(Xi_s)) = Yi_s;
                    
                    dti = DtPixel(i);
                    
                    Ti_j = app.Integrations.Measurements(Value).Times(i,1);
                    for j = 1:length(Xi_s)
                        Ti_all_old = find(isbetween(app.Data.time_DT,Ti_j,Ti_j+dti));
                        Ti_j = Ti_j+dti;
                        
                        CountPx = CountPx +1;
                        Vect_Start_t(CountPx) = Ti_all_old(1);
                        Vect_End_t(CountPx) = Ti_all_old(end);
                    end
                end
            end
            
            [X_grid,Y_grid] = meshgrid([min(Xi_all):SpotSize(1):max(Xi_all)],[min(Yi_all):SpotSize(1):max(Yi_all)]);
            
            EdgesX = min(X_grid(:))-0.5*SpotSize(1):SpotSize(1):max(X_grid(:))+0.5*SpotSize(1);
            EdgesY = min(Y_grid(:))-0.5*SpotSize(1):SpotSize(1):max(Y_grid(:))+0.5*SpotSize(1);
            
            xbin = discretize(X_grid(1,:), EdgesX);
            ybin = discretize(Y_grid(:,1)', EdgesY);
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools');
            app.WaitBar.Message = 'Interpolating data and generating intensity maps';
            
            for i = 1:size(app.Data.Cps_BackCorr,2)
                
                app.WaitBar.Value = i/size(app.Data.Cps_BackCorr,2);
                
                % Method: Theoule
                app.Data.Cps_Maps(i).Map = zeros(size(X_grid));
                app.Data.Cps_Maps(i).Std_Map = zeros(size(X_grid));
                
                PxCompt = app.Data.Cps_BackCorr(Ti_all,i);
                IdxOk = find(isnan(PxCompt) == 0); % filter NaN out otherwise interpolation does not work fine
                Vq = griddata(Xi_all(IdxOk),Yi_all(IdxOk),PxCompt(IdxOk),X_grid,Y_grid,'cubic');
                
                app.Data.Cps_Maps(i).Map = Vq;
                
                for j = 1:length(app.Data.PS)
                    PxCompStd = app.Data.PS(j).Cps_PrimaryStandard(Ti_all,i);
                    IdxOk_Std = find(isnan(PxCompStd) == 0);
                    Vq_Std = griddata(Xi_all(IdxOk_Std),Yi_all(IdxOk_Std),PxCompStd(IdxOk_Std),X_grid,Y_grid);
                    
                    app.Data.StdMaps(i).Std_Map(j).Cps = Vq_Std;
                    app.Data.StdMaps(i).Std_Map(j).Conc = app.Data.PS(j).PrimaryStandard_ElemConc(i).*ones(size(Vq));
                    app.Data.StdMaps(i).Std_Map(j).StdName = app.SecondaryStd_ListPS.Items{j};
                end
                
                Vq_BackNbIntegration = griddata(Xi_all,Yi_all,app.Data.BackgroundNbIntegration(Ti_all),X_grid,Y_grid,'nearest');
                Vq_BackgroundCorrection = griddata(Xi_all,Yi_all,app.Data.BackgroundCorrection(Ti_all,i),X_grid,Y_grid,'nearest');
                Vq_PixelNbIntegration = NbSwipePerPixel(1).*ones(size(Vq_BackNbIntegration));
                
                app.Data.StdMaps(i).Int_Back = Vq_BackgroundCorrection;
                app.Data.StdMaps(i).Sweeps_Back = Vq_BackNbIntegration;
                app.Data.StdMaps(i).Sweeps_Pixel = Vq_PixelNbIntegration;
                
                % Method Wengen:
                if CalcWengenOld
                    PxCompMap = zeros(size(Vect_Start_t));
                    PxCompStd = zeros(size(Vect_Start_t));
                    
                    for j = 1:length(Vect_Start_t)
                        
                        PxCompt_old = app.Data.Cps_BackCorr(Vect_Start_t(j):Vect_End_t(j),i);
                        PxComptNotNaN = PxCompt_old(~isnan(PxCompt_old));
                        
                        if isempty(PxComptNotNaN)
                            PxCompMap(j) = NaN;
                        else
                            PxCompMap(j) = mean(PxComptNotNaN);
                        end
                        
                        PxCompt_old = app.Data.PS(app.iPrStd).Cps_PrimaryStandard(Vect_Start_t(j):Vect_End_t(j),i);
                        PxCompStd(j) = mean(PxCompt_old);
                        
                        %PxCompMap(j,1:length(app.Data.Cps_BackCorr(Vect_Start_t(j):Vect_End_t(j),i))) = app.Data.Cps_BackCorr(Vect_Start_t(j):Vect_End_t(j),i);
                        %PxCompStd(j,1:length(app.Data.PS(app.iPrStd).Cps_PrimaryStandard(Vect_Start_t(j):Vect_End_t(j),i))) = app.Data.PS(app.iPrStd).Cps_PrimaryStandard(Vect_Start_t(j):Vect_End_t(j),i);
                    end
                    
                    Vq_old = griddata(Xi,Yi,PxCompMap,X_grid,Y_grid);
                    Vq_Std_old = griddata(Xi,Yi,PxCompStd,X_grid,Y_grid);
                end
                
                if 0 && isequal(i,36)
                    
                    
                    figure, imagesc(X_grid(1,:),Y_grid(:,1)',Vq), hold on, scatter(Xi_all,Yi_all,20*ones(size(Xi_all)),PxCompt,'filled'), colorbar
                    axis([1.2385e+05    1.2643e+05    0.5492e+05    0.5508e+05])
                    
                    caxis([0 1000])
                    colormap([0,0,0;parula(64)]);
                    
                    %figure, imagesc(X_grid(1,:),Y_grid(:,1)',Vq_Std), hold on, scatter(Xi_all,Yi_all,20*ones(size(Xi_all)),PxCompStd,'filled'), colorbar
                    
                    IdxOk = find(isnan(PxCompt) == 0);
                    
                    Vq_nearest = griddata(Xi_all(IdxOk),Yi_all(IdxOk),PxCompt(IdxOk),X_grid,Y_grid,'nearest');
                    Vq_linear = griddata(Xi_all(IdxOk),Yi_all(IdxOk),PxCompt(IdxOk),X_grid,Y_grid,'linear');
                    Vq_natural = griddata(Xi_all(IdxOk),Yi_all(IdxOk),PxCompt(IdxOk),X_grid,Y_grid,'natural');
                    Vq_cubic = griddata(Xi_all(IdxOk),Yi_all(IdxOk),PxCompt(IdxOk),X_grid,Y_grid,'cubic');
                    
                    Sel = find(Yi_all > 5.492e4 & Yi_all < 5.493e4);
                    figure, hold on, plot(Xi_all(Sel),PxCompt(Sel),'.-k'),
                    Sel = find(Y_grid(:) > 5.492e4 & Y_grid(:) < 5.493e4);
                    plot(X_grid(Sel),Vq_nearest(Sel),'o-r')
                    plot(X_grid(Sel),Vq_linear(Sel),'o-b')
                    %plot(X_grid(Sel),Vq_natural(Sel),'o-g')
                    plot(X_grid(Sel),Vq_cubic(Sel),'o-m')
                    plot(X_grid(Sel),Vq_old(Sel),'o-g')
                    
                    legend('scan','nearest','linear','cubic','old')
                    %legend('scan','nearest','linear','natural','cubic','old')
                    
                    keyboard
                end
                
            end
            
            app.Data.MapGeometryCheck.XY_Spots = [Xi_all;Yi_all]';
            app.Data.MapGeometryCheck.Radius_Spots = repmat(SpotSize(1)/2,length(Xi_all),1);
            app.Data.MapGeometryCheck.X_grid = X_grid;
            app.Data.MapGeometryCheck.Y_grid = Y_grid;
            app.Data.MapGeometryCheck.xbin = [];            % not used in this version
            app.Data.MapGeometryCheck.ybin = [];            % not used in this version
            
            close(app.WaitBar)
            
            return
            
        end
        
        
        function CalculateMaps(app)
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'XMapTools is running numbers';
            
            Value = app.Maps_List.Value;
            
            X1 = app.Integrations.Measurements(Value).X1;
            Y1 = app.Integrations.Measurements(Value).Y1;
            
            X2 = app.Integrations.Measurements(Value).X2;
            Y2 = app.Integrations.Measurements(Value).Y2;
            
            Slope = (Y2-Y1)./(X2-X1);
            Int = Y1-Slope.*X1;
            Distance = sqrt((X2-X1).^2+(Y2-Y1).^2);
            
            SpotSize = app.Integrations.Measurements(Value).SpotSize;
            ScanVel = app.Integrations.Measurements(Value).ScanVel;
            
            DurationReal = Distance./ScanVel; % in seconds
            NbPixels = floor((Distance-SpotSize)./SpotSize); % -SpotSize added because 1/2 spot size is ignored at the begining and at the end
            
            DtPixel = seconds(DurationReal./(Distance./SpotSize));  % this is correct to get a constant DtPixel.
            % DtPixel = seconds(DurationReal./NbPixels);
            
            % -------------------------------------------------------------
            % Map reconstruction (first version Theoule-sur-Mer)
            Xi = [];
            Yi = [];
            TableXY(1).Xi = [];
            TableXY(1).Yi = [];
            DistanceCheck = zeros(size(Distance));
            
            CountPx = 0;
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools');
            app.WaitBar.Message = 'Building raw intensity datasets, please wait';
            
            for i = 1:length(X1)
                
                app.WaitBar.Value = i/length(X1);
                
                marker_dist = SpotSize(i);
                x = [X1(i) X2(i)];
                y = [Y1(i) Y2(i)];
                dist_from_start = [0, sqrt((x(2:end)-x(1:end-1)).^2 + (y(2:end)-y(1:end-1)).^2)];
                marker_locs = dist_from_start(1)+marker_dist/2 : marker_dist : dist_from_start(end) - marker_dist/2;
                marker_indices = interp1( dist_from_start, 1 : length(dist_from_start), marker_locs);
                marker_base_pos = floor(marker_indices);
                
                marker_base_pos = marker_base_pos(find(marker_base_pos == 1));
                marker_indices = marker_indices(find(marker_base_pos == 1));
                
                weight_second = marker_indices - marker_base_pos;
                
                Xi_s = x(marker_base_pos) .* (1-weight_second) + x(marker_base_pos+1) .* weight_second;   % position of each marker in xy
                Yi_s = y(marker_base_pos) .* (1-weight_second) + y(marker_base_pos+1) .* weight_second;   % position of each marker in xy
                
                %Xi_s = [X1(i),Xi_s];    % Add the first position!
                %Yi_s = [Y1(i),Yi_s];    % Add the first position!
                
                DistanceCheck(i) = sqrt((Xi_s(end)-Xi_s(1)).^2+(Yi_s(end)-Yi_s(1)).^2);
                
                %                 if Distance(i)-DistanceCheck(i) >= SpotSize(i)
                %                     % the last point was not catched and we add it manually
                %                     Xi_s = [Xi_s,X2(i)];
                %                     Yi_s = [Yi_s,Y2(i)];
                %                 end
                
                TableXY(i).Xi = Xi_s;   % This is the pixel position!
                TableXY(i).Yi = Yi_s;
                
                Xi(end+1:end+length(Xi_s)) = Xi_s;
                Yi(end+1:end+length(Xi_s)) = Yi_s;
                
                % Times
                dti = DtPixel(i);
                
                Ti_j = app.Integrations.Measurements(Value).Times(i,1);
                for j = 1:length(Xi_s)
                    Ti_all = find(isbetween(app.Data.time_DT,Ti_j,Ti_j+dti));
                    Ti_j = Ti_j+dti;
                    %Table_Start_t(i,j) = Ti_all(1);
                    %Table_End_t(i,j) = Ti_all(end);
                    
                    CountPx = CountPx +1;
                    Vect_Start_t(CountPx) = Ti_all(1);
                    Vect_End_t(CountPx) = Ti_all(end);
                end
                
                
            end
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Correcting for time shift';
            
            % Correct for time shift:
            TimeShift = app.Maps_TimeshiftSpinner.Value;
            %Table_Start_t = Table_Start_t+TimeShift;
            %Table_End_t = Table_End_t+TimeShift;
            
            Vect_Start_t = Vect_Start_t + TimeShift;
            Vect_End_t = Vect_End_t + TimeShift;
            
            % Interpolate with gridata
            EdgesX = min(Xi)-0.5*SpotSize(1):SpotSize(1):max(Xi)+0.5*SpotSize(1);
            EdgesY = min(Yi)-0.5*SpotSize(1):SpotSize(1):max(Yi)+0.5*SpotSize(1);
            
            xbin = discretize(Xi, EdgesX);
            ybin = discretize(Yi, EdgesY);
            
            [X_grid,Y_grid] = meshgrid([min(Xi):SpotSize(1):max(Xi)],[min(Yi):SpotSize(1):max(Yi)]);
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools');
            app.WaitBar.Message = 'Interpolating data and generating intensity maps';
            
            for i = 1:size(app.Data.Cps_BackCorr,2)
                
                app.WaitBar.Value = i/size(app.Data.Cps_BackCorr,2);
                
                app.Data.Cps_Maps(i).Map = zeros(size(X_grid));
                app.Data.Cps_Maps(i).Std_Map = zeros(size(X_grid));
                
                PxCompMap = zeros(size(Vect_Start_t));
                PxCompStd = zeros(size(Vect_Start_t));
                
                for j = 1:length(Vect_Start_t)
                    
                    PxCompt = app.Data.Cps_BackCorr(Vect_Start_t(j):Vect_End_t(j),i);
                    PxComptNotNaN = PxCompt(~isnan(PxCompt));
                    
                    if isempty(PxComptNotNaN)
                        PxCompMap(j) = NaN;
                    else
                        PxCompMap(j) = mean(PxComptNotNaN);
                    end
                    
                    PxCompt = app.Data.PS(app.iPrStd).Cps_PrimaryStandard(Vect_Start_t(j):Vect_End_t(j),i);
                    PxCompStd(j) = mean(PxCompt);
                    
                    1
                    %PxCompMap(j,1:length(app.Data.Cps_BackCorr(Vect_Start_t(j):Vect_End_t(j),i))) = app.Data.Cps_BackCorr(Vect_Start_t(j):Vect_End_t(j),i);
                    %PxCompStd(j,1:length(app.Data.PS(app.iPrStd).Cps_PrimaryStandard(Vect_Start_t(j):Vect_End_t(j),i))) = app.Data.PS(app.iPrStd).Cps_PrimaryStandard(Vect_Start_t(j):Vect_End_t(j),i);
                end
                
                Vq = griddata(Xi,Yi,PxCompMap,X_grid,Y_grid);
                Vq_Std = griddata(Xi,Yi,PxCompStd,X_grid,Y_grid);
                
                app.Data.Cps_Maps(i).Map = Vq;
                app.Data.StdMaps(i).Std_Map_Cps = Vq_Std;
                app.Data.StdMaps(i).Std_Map_Conc = app.Data.PS(app.iPrStd).PrimaryStandard_ElemConc(i).*ones(size(Vq));
                
            end
            
            app.Data.MapGeometryCheck.XY_Spots = [Xi;Yi]';
            app.Data.MapGeometryCheck.Radius_Spots = repmat(SpotSize(1)/2,length(Xi),1);
            app.Data.MapGeometryCheck.X_grid = X_grid;
            app.Data.MapGeometryCheck.Y_grid = Y_grid;
            app.Data.MapGeometryCheck.xbin = xbin;
            app.Data.MapGeometryCheck.ybin = ybin;
            
            close(app.WaitBar)
            
        end
        
        function CalculateMaps_Wengen(app)
            
            % This is a backup of the function before the coding camp in
            % Theoule-sur-Mer.
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'XMapTools is running numbers';
            
            Value = app.Maps_List.Value;
            
            X1 = app.Integrations.Measurements(Value).X1;
            Y1 = app.Integrations.Measurements(Value).Y1;
            
            X2 = app.Integrations.Measurements(Value).X2;
            Y2 = app.Integrations.Measurements(Value).Y2;
            
            Slope = (Y2-Y1)./(X2-X1);
            Int = Y1-Slope.*X1;
            Distance = sqrt((X2-X1).^2+(Y2-Y1).^2);
            
            SpotSize = app.Integrations.Measurements(Value).SpotSize;
            ScanVel = app.Integrations.Measurements(Value).ScanVel;
            
            DurationReal = Distance./ScanVel; % in seconds
            NbPixels = floor(Distance./SpotSize);
            
            DtPixel = seconds(DurationReal./(Distance./SpotSize));  % this is correct to get a constant DtPixel.
            % DtPixel = seconds(DurationReal./NbPixels);
            
            % -------------------------------------------------------------
            % Map reconstruction
            Xi = [];
            Yi = [];
            TableXY(1).Xi = [];
            TableXY(1).Yi = [];
            DistanceCheck = zeros(size(Distance));
            
            CountPx = 0;
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools');
            app.WaitBar.Message = 'Building raw intensity datasets, please wait';
            
            for i = 1:length(X1)
                
                app.WaitBar.Value = i/length(X1);
                
                marker_dist = SpotSize(i);
                x = [X1(i) X2(i)];
                y = [Y1(i) Y2(i)];
                dist_from_start = [0, sqrt((x(2:end)-x(1:end-1)).^2 + (y(2:end)-y(1:end-1)).^2)];
                marker_locs = marker_dist : marker_dist : dist_from_start(end);                 % ignore first position (for now), added below
                marker_indices = interp1( dist_from_start, 1 : length(dist_from_start), marker_locs);
                %marker_indices = marker_indices(1:length()-1);
                marker_base_pos = floor(marker_indices);
                
                marker_base_pos = marker_base_pos(find(marker_base_pos == 1));
                marker_indices = marker_indices(find(marker_base_pos == 1));
                
                weight_second = marker_indices - marker_base_pos;
                
                Xi_s = x(marker_base_pos) .* (1-weight_second) + x(marker_base_pos+1) .* weight_second;   % position of each marker in xy
                Yi_s = y(marker_base_pos) .* (1-weight_second) + y(marker_base_pos+1) .* weight_second;   % position of each marker in xy
                
                Xi_s = [X1(i),Xi_s];    % Add the first position!
                Yi_s = [Y1(i),Yi_s];    % Add the first position!
                
                DistanceCheck(i) = sqrt((Xi_s(end)-Xi_s(1)).^2+(Yi_s(end)-Yi_s(1)).^2);
                
                if Distance(i)-DistanceCheck(i) >= SpotSize(i)
                    % one point was not catched and we add it manually
                    % (should rarely happen)
                    Xi_s = [Xi_s,X2(i)];
                    Yi_s = [Yi_s,Y2(i)];
                    %keyboard
                end
                
                TableXY(i).Xi = Xi_s;   % This is the pixel position!
                TableXY(i).Yi = Yi_s;
                
                Xi(end+1:end+length(Xi_s)) = Xi_s;
                Yi(end+1:end+length(Xi_s)) = Yi_s;
                
                % Times
                dti = DtPixel(i);
                
                Ti_j = app.Integrations.Measurements(Value).Times(i,1);
                for j = 1:length(Xi_s)
                    Ti_all = find(isbetween(app.Data.time_DT,Ti_j,Ti_j+dti));
                    Ti_j = Ti_j+dti;
                    Table_Start_t(i,j) = Ti_all(1);
                    Table_End_t(i,j) = Ti_all(end);
                    
                    CountPx = CountPx +1;
                    Vect_Start_t(CountPx) = Ti_all(1);
                    Vect_End_t(CountPx) = Ti_all(end);
                end
            end
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Correcting for time shift';
            
            % Correct for time shift:
            TimeShift = app.Maps_TimeshiftSpinner.Value;
            Table_Start_t = Table_Start_t+TimeShift;
            Table_End_t = Table_End_t+TimeShift;
            
            Vect_Start_t = Vect_Start_t + TimeShift;
            Vect_End_t = Vect_End_t + TimeShift;
            
            EdgesX = min(Xi)-0.5*SpotSize(1):SpotSize(1):max(Xi)+0.5*SpotSize(1);
            EdgesY = min(Yi)-0.5*SpotSize(1):SpotSize(1):max(Yi)+0.5*SpotSize(1);
            
            xbin = discretize(Xi, EdgesX);
            ybin = discretize(Yi, EdgesY);
            
            [X_grid,Y_grid] = meshgrid([min(Xi):SpotSize(1):max(Xi)],[min(Yi):SpotSize(1):max(Yi)]);
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools');
            app.WaitBar.Message = 'Interpolating data and generating intensity maps';
            
            % Interpolate with gridata
            for i = 1:size(app.Data.Cps_BackCorr,2)
                
                app.WaitBar.Value = i/size(app.Data.Cps_BackCorr,2);
                
                app.Data.Cps_Maps(i).Map = zeros(size(X_grid));
                app.Data.Cps_Maps(i).Std_Map = zeros(size(X_grid));
                
                PxCompMap = zeros(size(Vect_Start_t));
                PxCompStd = zeros(size(Vect_Start_t));
                
                for j = 1:length(Vect_Start_t)
                    
                    PxCompt = app.Data.Cps_BackCorr(Vect_Start_t(j):Vect_End_t(j),i);
                    PxComptNotNaN = PxCompt(~isnan(PxCompt));
                    
                    if isempty(PxComptNotNaN)
                        PxCompMap(j) = NaN;
                    else
                        PxCompMap(j) = mean(PxComptNotNaN);
                    end
                    
                    PxCompt = app.Data.PS(app.iPrStd).Cps_PrimaryStandard(Vect_Start_t(j):Vect_End_t(j),i);
                    PxCompStd(j) = mean(PxCompt);
                    
                    
                    %PxCompMap(j,1:length(app.Data.Cps_BackCorr(Vect_Start_t(j):Vect_End_t(j),i))) = app.Data.Cps_BackCorr(Vect_Start_t(j):Vect_End_t(j),i);
                    %PxCompStd(j,1:length(app.Data.PS(app.iPrStd).Cps_PrimaryStandard(Vect_Start_t(j):Vect_End_t(j),i))) = app.Data.PS(app.iPrStd).Cps_PrimaryStandard(Vect_Start_t(j):Vect_End_t(j),i);
                end
                
                Vq = griddata(Xi,Yi,PxCompMap,X_grid,Y_grid);
                Vq_Std = griddata(Xi,Yi,PxCompStd,X_grid,Y_grid);
                
                app.Data.Cps_Maps(i).Map = Vq;
                app.Data.StdMaps(i).Std_Map_Cps = Vq_Std;
                app.Data.StdMaps(i).Std_Map_Conc = app.Data.PS(app.iPrStd).PrimaryStandard_ElemConc(i).*ones(size(Vq));
                
            end
            
            app.Data.MapGeometryCheck.XY_Spots = [Xi;Yi]';
            app.Data.MapGeometryCheck.Radius_Spots = repmat(SpotSize(1)/2,length(Xi),1);
            app.Data.MapGeometryCheck.X_grid = X_grid;
            app.Data.MapGeometryCheck.Y_grid = Y_grid;
            app.Data.MapGeometryCheck.xbin = xbin;
            app.Data.MapGeometryCheck.ybin = ybin;
            
            close(app.WaitBar)
            
        end
        
        
        
        function ContextMenu_Tree_EliminateBackground(app)
            
            % Background
            Sel = app.Tree.SelectedNodes.NodeData(2);
            
            app.Integrations.Background.Names(Sel) = [];
            app.Integrations.Background.PositionsOri(Sel,:) = [];
            app.Integrations.Background.Positions(Sel,:) = [];
            app.Integrations.Background.Times(Sel,:) = [];
            
            app.Tree.SelectedNodes.delete;
            
            RebuildNodeData(app);
            
            if app.Background_CorrectionList.Value > 1
                Background_CorrectionListValueChanged(app,0);
            end
            
        end
        
        function ContextMenu_Tree_EliminateStandard(app)
            
            Sel = app.Tree.SelectedNodes.NodeData(2);
            
            ValuePs = app.PrimaryStd_List.Value;
            
            app.Integrations.Measurements(ValuePs).Names(Sel) = [];
            app.Integrations.Measurements(ValuePs).PositionsOri(Sel,:) = [];
            app.Integrations.Measurements(ValuePs).Positions(Sel,:) = [];
            app.Integrations.Measurements(ValuePs).Times(Sel,:) = [];
            app.Integrations.Measurements(ValuePs).X1(Sel) = [];
            app.Integrations.Measurements(ValuePs).Y1(Sel) = [];
            app.Integrations.Measurements(ValuePs).X2(Sel) = [];
            app.Integrations.Measurements(ValuePs).Y2(Sel) = [];
            app.Integrations.Measurements(ValuePs).SpotSize(Sel) = [];
            app.Integrations.Measurements(ValuePs).ScanVel(Sel) = [];
            
            app.Tree.SelectedNodes.delete;
            
            RebuildNodeData(app);
            
            if app.PrimaryStd_CorrectionList.Value > 1
                PrimaryStd_CorrectionListValueChanged(app,0);
            end
            
        end
        
        function RebuildNodeData(app)
            
            for i = 1:length(app.Tree.Children)
                for j = 1:length(app.Tree.Children(i).Children)
                    app.Tree.Children(i).Children(j).NodeData = [i,j];
                end
            end
            
        end
        
        function ReadDataFile_Agilent_Legacy(app,PathName,FileName,Idx)
            %
            try
                TableData = readtable([PathName,FileName],'NumHeaderLines',3);
                DataCps = table2array(TableData);
                DataCps = DataCps(1:end-1,2:end); % exclude time
                
                fid = fopen([PathName,FileName]);
                TheL = fgetl(fid); TheL = fgetl(fid); TheL = fgetl(fid);
                TheInput = fgetl(fid);
                fclose(fid);
                
                Str = textscan(TheInput,'%s','Delimiter',',');
                ElList = Str{1}(2:end);
                for i = 1:length(ElList)
                    CString = char(ElList{i});
                    TF = isletter(CString);
                    ElListClean{i} = CString(find(TF));
                    ElListFormated{i} = [CString(find(TF)),'_',CString(find(TF==0))];
                end
                
                Str = textscan(TheL,'%s');
                Str = Str{1};
                
                % Check for AM or PM in the date
                PositionAMPM = [];
                CheckPM = find(ismember(Str,'PM'));
                if ~isempty(CheckPM)
                    PositionAMPM = CheckPM;
                end
                CheckAM = find(ismember(Str,'AM'));
                if ~isempty(CheckAM)
                    PositionAMPM = CheckAM;
                end
                
                if isempty(PositionAMPM)
                    StartingDate = [Str{3},' ',Str{4},'.000'];
                    
                    try
                        DT_MapStart = datetime(StartingDate,'InputFormat','yyyy-MM-dd HH:mm:ss.SSS');   % DT is for "DateTime"
                    catch ME
                        try
                            DT_MapStart = datetime(StartingDate,'InputFormat','dd/MM/yyyy HH:mm:ss.SSS');   % DT is for "DateTime"
                        catch ME
                            DT_MapStart = datetime(StartingDate,'InputFormat','MM/dd/yyyy HH:mm:ss.SSS');   % DT is for "DateTime"
                        end
                    end
                    
                else
                    StartingDate = [Str{3},' ',Str{4},'.000',' ',Str{PositionAMPM}];
                    
                    try
                        DT_MapStart = datetime(StartingDate,'InputFormat','yyyy-MM-dd hh:mm:ss.SSS a');   % DT is for "DateTime"
                    catch ME
                        try
                            DT_MapStart = datetime(StartingDate,'InputFormat','dd/MM/yyyy hh:mm:ss.SSS a');   % DT is for "DateTime"
                        catch ME
                            DT_MapStart = datetime(StartingDate,'InputFormat','MM/dd/yyyy hh:mm:ss.SSS a');   % DT is for "DateTime"
                        end
                    end
                end
                
                % disp([datestr(StartingDate),' -> ',datestr(DT_MapStart)])
                
                t =  TableData.Time_Sec_(1:end-1);
                DT_Map = seconds(t)+DT_MapStart;
                
                DT_DN = datenum(DT_Map);
                
                % Adjust FileName for multi-phase
                FileName = AdjustFileName(app,FileName);
            catch ME
                uialert(app.ConverterLAICPMS,['Error while reading file: ',FileName],'Error – XMapTools');
                close(app.WaitBar)
                app.ErrorTracker = 1;
            end
            
            % Update the variable DataFiles
            app.DataFiles(Idx).FileName = FileName;
            app.DataFiles(Idx).DataCps = DataCps;
            app.DataFiles(Idx).ElListClean = ElListClean;
            app.DataFiles(Idx).ElList = ElList;
            app.DataFiles(Idx).ElListFormated = ElListFormated;
            app.DataFiles(Idx).t  = t;
            app.DataFiles(Idx).DT_Map = DT_Map;
            app.DataFiles(Idx).DT_DN = DT_DN;
            app.DataFiles(Idx).dt = t(2)-t(1);
            
        end
        
        function ReadDataFile_Agilent(app,PathName,FileName,Idx)
            %
            try
                TableData = readtable([PathName,FileName],'NumHeaderLines',3);
                DataCps = table2array(TableData);
                DataCps = DataCps(1:end-1,2:end); % exclude time
                
                fid = fopen([PathName,FileName]);
                TheL = fgetl(fid); TheL = fgetl(fid); TheL = fgetl(fid);
                TheInput = fgetl(fid);
                fclose(fid);
                
                Str = textscan(TheInput,'%s','Delimiter',',');
                ElList = Str{1}(2:end);
                for i = 1:length(ElList)
                    CString = char(ElList{i});
                    TF = isletter(CString);
                    ElListClean{i} = CString(find(TF));
                    ElListFormated{i} = [CString(find(TF)),'_',CString(find(TF==0))];
                end
                
                Str = textscan(TheL,'%s');
                Str = Str{1};
                
                % Check for AM or PM in the date
                PositionAMPM = [];
                CheckPM = find(ismember(Str,'PM'));
                if ~isempty(CheckPM)
                    PositionAMPM = CheckPM;
                end
                CheckAM = find(ismember(Str,'AM'));
                if ~isempty(CheckAM)
                    PositionAMPM = CheckAM;
                end
                
                if isempty(PositionAMPM)
                    StartingDate = [Str{3},' ',Str{4},'.000'];
                else
                    StartingDate = [Str{3},' ',Str{4},'.000',' ',Str{PositionAMPM}];
                end
                
                DT_MapStart = ReadDateTime(app,StartingDate);
                
                % disp([datestr(StartingDate),' -> ',datestr(DT_MapStart)])
                
                t =  TableData.Time_Sec_(1:end-1);
                DT_Map = seconds(t)+DT_MapStart;
                
                DT_DN = datenum(DT_Map);
                
                % Adjust FileName for multi-phase
                FileName = AdjustFileName(app,FileName);
                
            catch ME
                uialert(app.ConverterLAICPMS,['Error while reading file: ',FileName],'Error – XMapTools');
                close(app.WaitBar)
                app.ErrorTracker = 1;
            end
            
            % Update the variable DataFiles
            app.DataFiles(Idx).FileName = FileName;
            app.DataFiles(Idx).DataCps = DataCps;
            app.DataFiles(Idx).ElListClean = ElListClean;
            app.DataFiles(Idx).ElList = ElList;
            app.DataFiles(Idx).ElListFormated = ElListFormated;
            app.DataFiles(Idx).t  = t;
            app.DataFiles(Idx).DT_Map = DT_Map;
            app.DataFiles(Idx).DT_DN = DT_DN;
            app.DataFiles(Idx).dt = t(2)-t(1);
            
            app.CheckDateTimeFormat = 0;
        end
        
        
        function ReadDataFile_Thermo(app,PathName,FileName,Idx)
            
            try
                NumHeaderLines = 13;
                
                TableData = readtable([PathName,FileName],'NumHeaderLines',NumHeaderLines);
                DataCpsRaw = table2array(TableData);
                DataCps = DataCpsRaw(3:end-1,2:end); % exclude time & first two rows (shit)
                
                fid = fopen([PathName,FileName]);
                for i = 1:NumHeaderLines
                    if isequal(i,1)
                        TheL1 = fgetl(fid);
                    else
                        TheL = fgetl(fid);
                    end
                end
                TheInput = fgetl(fid);
                fclose(fid);
                
                Str = textscan(TheInput,'%s','Delimiter',',');
                ElList = Str{1}(2:end);
                for i = 1:length(ElList)
                    CString = char(ElList{i});
                    TF = isletter(CString);
                    ElListClean{i} = CString(find(TF));
                    ElListFormated{i} = [CString(find(TF)),'_',CString(find(TF==0))];
                end
                
                WhereSep = find(ismember(TheL1,':'));
                TheLOk = TheL1(WhereSep(1)+1:end-1); % to avoid the stupid ";"
                
                Str = textscan(TheLOk,'%s');  % to avoid the stupid ";"
                Str = Str{1};
                
                % Check for AM or PM in the date
                PositionAMPM = [];
                CheckPM = find(ismember(Str,'PM'));
                if ~isempty(CheckPM)
                    PositionAMPM = CheckPM;
                end
                CheckAM = find(ismember(Str,'AM'));
                if ~isempty(CheckAM)
                    PositionAMPM = CheckAM;
                end
                
                if isempty(PositionAMPM)
                    StartingDate = [Str{1},' ',Str{2},'.000'];
                else
                    StartingDate = [Str{1},' ',Str{2},'.000',' ',Str{PositionAMPM}];
                end
                
                DT_MapStart = ReadDateTime(app,StartingDate);
                
                % disp([datestr(StartingDate),' -> ',datestr(DT_MapStart)])
                
                t =  DataCpsRaw(3:end-1,1);
                
                DT_Map = seconds(t)+DT_MapStart;
                
                DT_DN = datenum(DT_Map);
                
                % Adjust FileName for multi-phase
                FileName = AdjustFileName(app,FileName);
                
            catch ME
                uialert(app.ConverterLAICPMS,['Error while reading file: ',FileName],'Error – XMapTools');
                close(app.WaitBar)
                app.ErrorTracker = 1;
            end
            
            % Update the variable DataFiles
            app.DataFiles(Idx).FileName = FileName;
            app.DataFiles(Idx).DataCps = DataCps;
            app.DataFiles(Idx).ElListClean = ElListClean;
            app.DataFiles(Idx).ElList = ElList;
            app.DataFiles(Idx).ElListFormated = ElListFormated;
            app.DataFiles(Idx).t  = t;
            app.DataFiles(Idx).DT_Map = DT_Map;
            app.DataFiles(Idx).DT_DN = DT_DN;
            app.DataFiles(Idx).dt = t(2)-t(1);
            
            app.CheckDateTimeFormat = 0;
            
            
        end
        
        function NewFileName = AdjustFileName(app,FileName)
            
            NewFileName = '';
            for i =1:length(FileName)
                if ~isequal(FileName(i),' ')
                    NewFileName(end+1) = FileName(i);
                end
            end
            
        end
        
        function DT_Formated = ReadDateTime(app,TimeStr)
            
            if isequal(app.CheckDateTimeFormat,1)
                SelFormat = CheckDateTimeFormatFct(app,TimeStr);
                app.DateTimeFormat = SelFormat;
            else
                SelFormat = app.DateTimeFormat;
            end
            
            DT_Formated = datetime(TimeStr,'InputFormat',SelFormat);
            
        end
        
        function SelFormat = CheckDateTimeFormatFct(app,TimeStr)
            Formats = { 'yyyy-MM-dd HH:mm:ss.SSS', ...
                'dd-MM-yyyy HH:mm:ss.SSS', ...
                'MM-dd-yyyy HH:mm:ss.SSS', ...
                'dd/MM/yyyy HH:mm:ss.SSS', ...
                'MM/dd/yyyy HH:mm:ss.SSS', ...
                'yyyy-MM-dd hh:mm:ss.SSS a', ...
                'dd-MM-yyyy hh:mm:ss.SSS a', ...
                'MM-dd-yyyy hh:mm:ss.SSS a', ...
                'dd/MM/yyyy hh:mm:ss.SSS a', ...
                'MM/dd/yyyy hh:mm:ss.SSS a'};
            
            % - a	    Day period (AM or PM)
            
            % taken from https://www.mathworks.com/help/matlab/ref/datetime.html#buhzxmk-1-Format
            
            % Auto Date Detection:
            TestF = zeros(size(Formats));
            for i = 1:length(Formats)
                try
                    Test = datetime(TimeStr,'InputFormat',Formats{i});
                    TestF(i) = 1;
                end
            end
            AutoFormatPos = find(TestF);
            if isempty(AutoFormatPos)
                AutoDetectFormat = '';
            else
                AutoDetectFormat = char(Formats(AutoFormatPos(1)));
            end
            
            app.ExchangeFormator = '';
            
            waitfor(Formator(app, TimeStr, Formats, AutoDetectFormat))
            
            SelFormat = app.ExchangeFormator;
            
        end
    end
    

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, XMapToolsApp)
            
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
            
            app.ConverterLAICPMS.Visible = 'off';
            
            app.XMapToolsApp = XMapToolsApp;
            app.LastDir = cd; %app.XMapToolsApp.XMapTools_LastDir;
            
            movegui(app.ConverterLAICPMS,"center");
            
            app.PlotMenuDropDown.Visible = 'off';
            app.PlotMenuDropDownLabel.Visible = 'off';
            app.TimeshiftSpinner.Visible = 'off';
            app.TimeshiftLabel.Visible = 'off';
            app.Plot.Visible = 'off';
            
            app.BackgroundFilterPercentSpinner.Visible = 'off';
            app.BackgroundFilterPercentSpinnerLabel.Visible = 'off';
            
            app.BACKGROUNDPanel.Visible = 'off';
            app.PRIMARYSTANDARDPanel.Visible = 'off';
            app.SECONDARYSTANDARDPanel.Visible = 'off';
            app.MAPSPanel.Visible = 'off';
            
            app.SweepLabel.Visible = 'off';
            app.Position_Min.Visible = 'off';
            app.Position_Max.Visible = 'off';
            
            app.BackgroundApplyButton.Visible = 'off';
            app.Background_CorrectionList.Visible = 'off';
            
            app.CheckButton.Visible = 'off';
            app.ShowButton.Visible = 'off';
            app.ExportButton.Visible = 'off';
            app.Maps_SelElement.Visible = 'off';
            
            app.ZoomIn.Visible = 'off';
            app.ZoomOut.Visible = 'off';
            app.ZoomReset.Visible = 'off';
            
            app.Data4Plot.List = {};
            app.Data4Plot.Data(1).Type = '';
            app.Data4Plot.Data(1).Labels = '';
            app.Data4Plot.Data(1).X = NaT(1);
            app.Data4Plot.Data(1).Y = [];
            
            app.CheckDateTimeFormat = 1;
            
            % Create the context menus:
            app.ContextMenu_Tree_Background = uicontextmenu(app.ConverterLAICPMS);
            m1 = uimenu(app.ContextMenu_Tree_Background,'Text','Eliminate Background Selection','MenuSelectedFcn',@(varargin)ContextMenu_Tree_EliminateBackground(app));
            
            app.ContextMenu_Tree_Standard = uicontextmenu(app.ConverterLAICPMS);
            m1 = uimenu(app.ContextMenu_Tree_Standard,'Text','Eliminate Standard Selection','MenuSelectedFcn',@(varargin)ContextMenu_Tree_EliminateStandard(app));
            
            app.iPrStd = 1;  % we write the first primar standard first...
            
            app.ErrorTracker = 0;
            
            % Load Standard data
            app.PathStdFolder = [app.XMapToolsApp.config.xmaptools.setup_path,'/Dev/Data_Std_LAICPMS'];
            
            ReadStandardData(app);
            
            app.ConverterLAICPMS.Visible = 'on';
            
        end

        % Button pushed function: LoadDatafilesButton
        function LoadDatafilesButtonPushed(app, event)
            
            %app.TypeOfInstrumentDropDown
            %app.LogfileCheckBox
            %app.TypeOfFileDropDown
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            
            app.DataFiles(1).FileName = '';
            app.DataFiles(1).DataCps = [];
            app.DataFiles(1).ElListClean = {};
            app.DataFiles(1).ElList = {};
            app.DataFiles(1).ElListFormated = {};
            app.DataFiles(1).t  = [];
            app.DataFiles(1).DT_Map = [];
            app.DataFiles(1).DT_DN = [];
            app.DataFiles(1).dt = [];
            
            if isequal(app.TypeOfFileDropDown.Value,'Multiple-file')
                
                app.WaitBar.Message = 'Select compatible data files';
                
                f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
                cd(app.LastDir);
                [FileName,PathName,FilterIndex] = uigetfile({'*.csv','CSV-file (*.csv)'; '*.txt','TEXT-file (*.txt)'; ...
                    '*.*',  'All Files (*.*)'},'Select DATA files...','MultiSelect', 'on');
                delete(f)
                figure(app.ConverterLAICPMS);
                
                app.WaitBar.Message = 'Reading data files, please wait';
                app.WaitBar.Indeterminate = 'off';
                warning('off','all')
                for i = 1:length(FileName)
                    
                    switch app.TypeOfInstrumentDropDown.Value
                        case 'Agilent'
                            ReadDataFile_Agilent(app,PathName,FileName{i},i);
                        case 'Thermo'
                            ReadDataFile_Thermo(app,PathName,FileName{i},i);
                    end
                    
                    % ReadDataFile_Agilent(app,PathName,FileName{i},i);
                    if isequal(app.ErrorTracker,1)
                        app.ErrorTracker = 0;
                        return
                    end
                    app.WaitBar.Value = i/length(FileName);
                end
                warning('on','all')
                app.WaitBar.Indeterminate = 'on';
                
                % sort by time
                StartingTimes = zeros(length(length(app.DataFiles)),1);
                for i = 1:length(app.DataFiles)
                    StartingTimes(i) = app.DataFiles(i).DT_DN(1);
                end
                [vals, idx] = sort(StartingTimes,'ascend');
                
                DataFilesUnsorted = app.DataFiles;
                
                app.DataFiles = app.DataFiles(idx);
                
            end
            
            if isequal(app.TypeOfFileDropDown.Value,'Single-file')
                
                app.WaitBar.Message = 'Select a compatible data file';
                
                f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
                cd(app.LastDir);
                [FileName,PathName,FilterIndex] = uigetfile({'*.csv','CSV-file (*.csv)'; '*.txt','TEXT-file (*.txt)'; ...
                    '*.*',  'All Files (*.*)'},'Select DATA file...');
                delete(f)
                figure(app.ConverterLAICPMS);
                
                app.WaitBar.Message = 'Importing the data file, please wait...';
                
                warning('off','all')
                try
                    switch app.TypeOfInstrumentDropDown.Value
                        case 'Agilent'
                            ReadDataFile_Agilent(app,PathName,FileName,1);
                        case 'Thermo'
                            ReadDataFile_Thermo(app,PathName,FileName,1);
                    end
                    
                catch ME
                   errordlg('This file is not yet a valid file for XMapTools. ','XMapTools')
                   close(app.WaitBar)
                end
                warning('on','all')
                
            end
            
            if isequal(app.LogfileCheckBox.Value,1)
                app.WaitBar.Message = 'Select a compatible log file';
                
                % ------------------------------
                % CHEMICAL DATA: Log File
                f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
                cd(app.LastDir);
                [FileName_Log,PathName_Log,FilterIndex] = uigetfile({'*.csv','CSV-file (*.csv)'; '*.txt','TEXT-file (*.txt)'; ...
                    '*.*',  'All Files (*.*)'},'Select LOG file...');
                delete(f)
                figure(app.ConverterLAICPMS);
                
                app.WaitBar.Message = 'Importing the data file, please wait...';
                
                try
                    warning('off','all')
                    
                    opts = detectImportOptions([PathName_Log,FileName_Log],'NumHeaderLines',0);
                    TableLOG = readtable([PathName_Log,FileName_Log], opts); % 'DatetimeType','text'
                    warning('on','all')
                    
                    % DtLOG = ReadDateTime(app,TableLOG.Timestamp);
                    
                    % DtLOG = datetime(TableLOG.Timestamp,'InputFormat','yyyy-dd-MM HH:mm:ss.SSS');
                    
                    
                    app.CheckDateTimeFormat = 1;    % we always check for the format of the log file (4.4)
                    SelFormat = CheckDateTimeFormatFct(app,TableLOG.Timestamp(1));
                    
                    DtLOG = datetime(TableLOG.Timestamp,'InputFormat',SelFormat);
                    
                catch ME
                    errordlg('This log file is not a valid file','XMapTools')
                    close(app.WaitBar)
                    return
                end
                
                app.Log.Table = TableLOG;
                app.Log.DT_Log = DtLOG;
                
            end
            
            if isequal(app.LogfileCheckBox.Value,0)
                app.WaitBar.Message = 'Waiting for file definitions from the LOG Generator';
                
                waitfor(LogGenerator(app,app.DataFiles, app.XMapToolsApp));
                try
                    app.Log.DT_Log = app.Log.Table.Timestamp;
                catch ME
                    close(app.WaitBar)
                    return
                end
                %keyboard
                
            end
            
            app.WaitBar.Message = 'Saving data and preparing the interface';
            
            % ------------------------------
            % SAVING DATA
            if length(app.DataFiles) > 1
                
                app.Data.Cps = [];
                app.Data.ElName = '';
                app.Data.ElNameOri = '';
                app.Data.ElNameFormated = '';
                app.Data.tsec = [];
                app.Data.time_DT = NaT(0);      % DT for date/time
                app.Data.time_DN = [];          % DN for date number
                
                for i = 1:length(app.DataFiles)
                    app.Data.Cps(end+1:end+size(app.DataFiles(i).DataCps,1),:) = app.DataFiles(i).DataCps;
                    if isempty(app.Data.ElName)
                        app.Data.ElName = app.DataFiles(i).ElListClean;
                    end
                    if isempty(app.Data.ElNameOri)
                        app.Data.ElNameOri = app.DataFiles(i).ElList;
                    end
                    if isempty(app.Data.ElNameFormated)
                        app.Data.ElNameFormated = app.DataFiles(i).ElListFormated;
                    end
                    
                    app.Data.tsec(end+1:end+length(app.DataFiles(i).t),1) = app.DataFiles(i).t;
                    app.Data.time_DT(end+1:end+length(app.DataFiles(i).DT_Map),1) = app.DataFiles(i).DT_Map;
                    app.Data.time_DN(end+1:end+length(app.DataFiles(i).DT_DN),1) = app.DataFiles(i).DT_DN;
                end
                app.Data.dt = app.Data.tsec(2)-app.Data.tsec(1);
                
            else
                
                app.Data.Cps = app.DataFiles(1).DataCps;
                app.Data.ElName = app.DataFiles(1).ElListClean;
                app.Data.ElNameOri = app.DataFiles(1).ElList;
                app.Data.ElNameFormated = app.DataFiles(1).ElListFormated;
                app.Data.tsec = app.DataFiles(1).t;
                app.Data.time_DT = app.DataFiles(1).DT_Map;     % DT for date/time
                app.Data.time_DN = app.DataFiles(1).DT_DN;      % DN for date number
                app.Data.dt = app.DataFiles(1).dt;
            end
            
            %             app.Data.Cps = DataCps;
            %             app.Data.ElName = ElListClean;
            %             app.Data.ElNameOri = ElList;
            %             app.Data.tsec = t;
            %             app.Data.time_DT = DT_Map;   % DT for date/time
            %             app.Data.time_DN = DT_DN;    % DN for date number
            %             app.Data.dt = t(2)-t(1);
            
            SumData = sum(app.Data.Cps,2);
            app.Data.SumData = SumData;
            
            % ------------------------------
            % MATCHING
            SearchShiftAuto(app);
            
            % ------------------------------
            % PLOT CORRECTION
            Plot2CheckShift(app);
            
            app.BACKGROUNDPanel.Visible = 'on';
            
            app.ZoomIn.Visible = 'on';
            app.ZoomOut.Visible = 'on';
            app.ZoomReset.Visible = 'on';
            
            close(app.WaitBar)
            %ExtractTimeIntegration(app);
            
        end

        % Close request function: ConverterLAICPMS
        function ConverterLAICPMSCloseRequest(app, event)
            
            figure(app.XMapToolsApp.XMapTools_GUI);
            
            delete(app)
            
        end

        % Value changed function: TimeshiftSpinner
        function TimeshiftSpinnerValueChanged(app, event)
            value = app.TimeshiftSpinner.Value;
            
            app.Log.DT_Log_Corr = app.Log.DT_Log + seconds(value);
            
            Plot2CheckShift(app);
        end

        % Button pushed function: 
        % ExtractIntegrationsBackgroundButton
        function ExtractIntegrationsBackgroundButtonPushed(app, event)
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'XMapTools is running numbers';
            
            app.TimeshiftSpinner.Enable = 'off';   % We can no longer change the signal
            
            app.Data4Plot.List = {'Raw_Sum'};
            app.Data4Plot.Data(1).Type = 'Raw';
            app.Data4Plot.Data(1).X = app.Data.time_DT;
            app.Data4Plot.Data(1).Y = app.Data.SumData;
            app.Data4Plot.Data(1).Background = [];
            
            app.Data4Plot.List{2} = 'Raw_All';
            app.Data4Plot.Data(2).Type = 'Raw_All';
            app.Data4Plot.Data(2).Labels = app.Data.ElName;
            app.Data4Plot.Data(2).X = app.Data.time_DT;
            app.Data4Plot.Data(2).Y = app.Data.Cps;
            app.Data4Plot.Data(2).Background = [];
            
            app.Data4Plot.List{3} = '---';
            app.Data4Plot.Data(3).Type = 'Empty';
            app.Data4Plot.Data(3).X = 0;
            app.Data4Plot.Data(3).Y = 0;
            app.Data4Plot.Data(3).Background = [];
            
            NbOri = length(app.Data4Plot.List);
            for i = 1:length(app.Data.ElName)
                app.Data4Plot.List{NbOri+i} = ['Raw_',char(app.Data.ElName{i})];
                app.Data4Plot.Data(NbOri+i).Type = 'Raw';
                app.Data4Plot.Data(NbOri+i).X = app.Data.time_DT;
                app.Data4Plot.Data(NbOri+i).Y = app.Data.Cps(:,i);
                app.Data4Plot.Data(NbOri+i).Background = [];
            end
            
            app.PlotMenuDropDown.Items = app.Data4Plot.List;
            app.PlotMenuDropDown.ItemsData = [1:length(app.Data4Plot.List)];
            app.PlotMenuDropDown.Value = 1;
            app.PlotMenuDropDown.Visible = 'On';
            app.PlotMenuDropDownLabel.Visible = 'On';
            
            PlotMenuDropDownValueChanged(app,0);
            
            ExtractTimeIntegration(app);
            
            app.WaitBar.Message = 'Saving data and preparing display';
            
            app.Background_CorrectionList.Visible = 'On';
            app.BackgroundApplyButton.Visible = 'On';
            app.BackgroundApplyButton.Enable = 'Off';
            
            app.LoadDatafilesButton.Enable = 'Off';
            
            app.BackgroundFilterPercentSpinner.Visible = 'on';
            app.BackgroundFilterPercentSpinnerLabel.Visible = 'on';
            
            app.Tree.SelectedNodes = app.Node_Background;   % 4.4
            
            ZoomResetButtonPushed(app);
            
            close(app.WaitBar)
            
        end

        % Value changed function: PlotMenuDropDown
        function PlotMenuDropDownValueChanged(app, event)
            value = app.PlotMenuDropDown.Value;
            
            cla(app.Plot)
            
            switch app.Data4Plot.Data(value).Type
                
                case 'Empty'
                    return
                    
                    
                case 'Raw'
                    X = app.Data4Plot.Data(value).X;
                    Y = app.Data4Plot.Data(value).Y;
                    plot(app.Plot,app.Data4Plot.Data(value).X,app.Data4Plot.Data(value).Y,'.-','MarkerSize',5);
                    legend(app.Plot,'hide');
                    
                    if app.Background_CorrectionList.Value > 1 && isequal(app.Background_CorrectionList.Enable,'on')
                        plot(app.Plot,app.Data.time_DT,app.Data4Plot.Data(value).Background,'-r','LineWidth',2);
                    end
                    
                    
                case 'Raw_All'
                    X = app.Data4Plot.Data(value).X;
                    plot(app.Plot,app.Data4Plot.Data(value).X,app.Data4Plot.Data(value).Y,'.-','MarkerSize',5);
                    legend(app.Plot,app.Data4Plot.Data(value).Labels,'Location','best');
                    
                    app.Plot.YScale = 'log';
                    
                    rp = rulerPanInteraction('Dimensions','xy');
                    app.Plot.Interactions = [rp];
                    app.Plot.Visible = 'on';
                    
                    tb = axtoolbar(app.Plot,{'export','pan','zoomin','zoomout','restoreview'});
                    
                    return
                    
                case 'BC'
                    X = app.Data4Plot.Data(value).X;
                    Y = app.Data4Plot.Data(value).Y;
                    plot(app.Plot,app.Data4Plot.Data(value).X,app.Data4Plot.Data(value).Y,'.-','MarkerSize',5);
                    legend(app.Plot,'hide');
                    
                    if app.PrimaryStd_CorrectionList.Value > 1 && isequal(app.PrimaryStd_CorrectionList.Enable,'on')
                        plot(app.Plot,app.Data.time_DT,app.Data4Plot.Data(value).Cps_PrimaryStandard,'-k','LineWidth',2);
                    end
                    
                case 'BC_All'
                    X = app.Data4Plot.Data(value).X;
                    plot(app.Plot,app.Data4Plot.Data(value).X,app.Data4Plot.Data(value).Y,'.-','MarkerSize',5);
                    legend(app.Plot,app.Data4Plot.Data(value).Labels,'Location','best');
                    
            end
            
            hold(app.Plot,'on')
            app.Plot.YScale = 'log';
            
            rp = rulerPanInteraction('Dimensions','xy');
            app.Plot.Interactions = [rp];
            app.Plot.Visible = 'on';
            
            tb = axtoolbar(app.Plot,{'export','pan','zoomin','zoomout','restoreview'});
            
            % Should we plot something  on top?
            if ~isempty(app.Tree.SelectedNodes)
                NodeData = app.Tree.SelectedNodes.NodeData;
                
                % General code for all cases (4.4)
                if NodeData(2) > 0  % && isequal(NodeData(1),1)  
                    
                    if isequal(NodeData(1),1)
                        Xinterval = app.Integrations.Background.Times(NodeData(2),:);
                        Yi = Y(app.Integrations.Background.Positions(NodeData(2),1):app.Integrations.Background.Positions(NodeData(2),2));
                    else
                        if isequal(NodeData(1),2)
                            ValuePs = app.PrimaryStd_List.Value;
                        elseif isequal(NodeData(1),3)
                            ValuePs = app.SecondaryStd_List.Value;
                        else
                            ValuePs = app.Maps_List.Value;
                        end

                        Xinterval = app.Integrations.Measurements(ValuePs).Times(NodeData(2),:);
                        Yi = Y(app.Integrations.Measurements(ValuePs).Positions(NodeData(2),1):app.Integrations.Measurements(ValuePs).Positions(NodeData(2),2));
                    
                    end
                    
                    % new version (4.4) with no outlier rejection
                    SelectedBackgroundPlot = find(Yi > 0);
                    
                    if ~isempty(SelectedBackgroundPlot)
                        
                        YValue = mean(Yi(SelectedBackgroundPlot));
                        YStd = std(Yi(SelectedBackgroundPlot));
                        
                        if YStd < 0.1
                            YStd = 0.1 * YValue;
                        end
                        
                        ModeLarge = 0;
                        if YStd > 0.5*YValue % the red box will be outside
                            ModeLarge = 1;
                        end
                        
                        % TF = isoutlier(Yi);   % deactivated in 4.4
                        % YValue = mean(Yi(find(TF == 0)));
                        % YStd = std(Yi(find(TF == 0)));
                        
                        p=fill(app.Plot,[Xinterval(1) Xinterval(2) Xinterval(2) Xinterval(1)],[YValue+0.5*YStd YValue+0.5*YStd YValue-0.5*YStd YValue-0.5*YStd],'r');
                        p.FaceAlpha = 0.5;
                        
                        app.Position_Min.Value = app.Integrations.Background.Positions(NodeData(2),1);
                        app.Position_Max.Value = app.Integrations.Background.Positions(NodeData(2),2);
                        
                        app.SweepLabel.Visible = 'on';
                        app.Position_Min.Visible = 'on';
                        app.Position_Max.Visible = 'on';
                        
                        % Update Plot
                        DurationView = duration(app.Plot.XLim(2)-app.Plot.XLim(1));
                        DurationInter = Xinterval(2)-Xinterval(1);
                        PositionInter = Xinterval(1)+DurationInter/2;
                        
                        app.Plot.XLim = [PositionInter-3*DurationInter PositionInter+3*DurationInter];
                        
                        if ModeLarge
                            Min = min(Yi(SelectedBackgroundPlot)) - 0.1 * min(Yi(SelectedBackgroundPlot));
                            Max = max(Yi(SelectedBackgroundPlot)) + 0.1 * max(Yi(SelectedBackgroundPlot));
                        else
                            Min = YValue-0.5*YValue;
                            if Min <= 0
                                Min = 1; % counts/s
                            end
                            Max = YValue+0.5*YValue;
                            if Max <= 0
                                Max = 2; % counts/s
                            end
                        end
                        app.Plot.YLim = [Min Max];
                        
                    else
                        
                        YValue = min(Y(find(Y > 0)));
                        YStd = 0.1 * YValue;
                        
                        p=fill(app.Plot,[Xinterval(1) Xinterval(2) Xinterval(2) Xinterval(1)],[YValue+0.5*YStd YValue+0.5*YStd YValue-0.5*YStd YValue-0.5*YStd],'k');
                        p.FaceAlpha = 0.5;
                        
                        %
                        DurationView = duration(app.Plot.XLim(2)-app.Plot.XLim(1));
                        DurationInter = Xinterval(2)-Xinterval(1);
                        PositionInter = Xinterval(1)+DurationInter/2;
                        
                        app.Plot.XLim = [PositionInter-3*DurationInter PositionInter+3*DurationInter];
                        
                        XLimTemp = app.Plot.XLim;
                        SelYDataIdx = find(X > XLimTemp(1) & X < XLimTemp(2));
                        Ysel =  Y(SelYDataIdx);
                        YselPos = find(Ysel > 0);
                        if ~isempty(YselPos)
                            app.Plot.YLim = [min(Ysel(YselPos))-0.5*min(Ysel(YselPos)), max(Ysel(YselPos))+0.2*max(Ysel(YselPos))];
                        end
                        
                    end
                    
                    %app.Plot.XLim = [PositionInter-DurationView/2 PositionInter+DurationView/2];
                    %DeltaY = app.Plot.YLim(2)-app.Plot.YLim(1);
                    %app.Plot.YLim = [YValue-(DeltaY/2) YValue+(DeltaY/2)];
                    
                    return % to keep the display
                else
                    if value > 1
                        % 4.4
                        XLimTemp = app.Plot.XLim;
                        SelYDataIdx = find(X > XLimTemp(1) & X < XLimTemp(2));
                        Ysel =  Y(SelYDataIdx);
                        YselPos = find(Ysel > 0);
                        if ~isempty(YselPos)
                            app.Plot.YLim = [min(Ysel(YselPos))-0.5*min(Ysel(YselPos)), max(Ysel(YselPos))+0.2*max(Ysel(YselPos))];
                        end
                        
                        % figure, plot(Ysel,'.-')
                        return
                    else
                        ZoomResetButtonPushed(app);
                        app.SweepLabel.Visible = 'off';
                        app.Position_Min.Visible = 'off';
                        app.Position_Max.Visible = 'off';
                    end
                end
                
                
%                 if isequal(NodeData(1),2) && NodeData(2) > 0
%                     
%                     ValuePs = app.PrimaryStd_List.Value;
%                     
%                     Xinterval = app.Integrations.Measurements(ValuePs).Times(NodeData(2),:);
%                     Yi = Y(app.Integrations.Measurements(ValuePs).Positions(NodeData(2),1):app.Integrations.Measurements(ValuePs).Positions(NodeData(2),2));
%                     Yi = Yi(find(~isnan(Yi)));
%                     TF = isoutlier(Yi);
%                     YValue = mean(Yi(find(TF == 0)));
%                     YStd = std(Yi(find(TF == 0)));
%                     
%                     p=fill(app.Plot,[Xinterval(1) Xinterval(2) Xinterval(2) Xinterval(1)],[YValue+0.5*YStd YValue+0.5*YStd YValue-0.5*YStd YValue-0.5*YStd],'r');
%                     p.FaceAlpha = 0.5;
%                     
%                     app.Position_Min.Value = app.Integrations.Measurements(ValuePs).Positions(NodeData(2),1);
%                     app.Position_Max.Value = app.Integrations.Measurements(ValuePs).Positions(NodeData(2),2);
%                     
%                     app.SweepLabel.Visible = 'on';
%                     app.Position_Min.Visible = 'on';
%                     app.Position_Max.Visible = 'on';
%                     
%                     % Update Plot
%                     DurationView = duration(app.Plot.XLim(2)-app.Plot.XLim(1));
%                     DurationInter = Xinterval(2)-Xinterval(1);
%                     PositionInter = Xinterval(1)+DurationInter/2;
%                     
%                     app.Plot.XLim = [PositionInter-3*DurationInter PositionInter+3*DurationInter];
%                     Min = YValue-0.4*YValue;
%                     if Min <= 0
%                         Min = 1; % counts/s
%                     end
%                     Max = YValue+0.4*YValue;
%                     if Max <= 0
%                         Max = 2; % counts/s
%                     end
%                     app.Plot.YLim = [Min Max];
%                     
%                     %app.Plot.XLim = [PositionInter-DurationView/2 PositionInter+DurationView/2];
%                     
%                     %DeltaY = app.Plot.YLim(2)-app.Plot.YLim(1);
%                     %app.Plot.YLim = [YValue-(DeltaY/2) YValue+(DeltaY/2)];
%                     
%                     return % to keep the display
%                 else
%                     ZoomResetButtonPushed(app);
%                     
%                     app.SweepLabel.Visible = 'off';
%                     app.Position_Min.Visible = 'off';
%                     app.Position_Max.Visible = 'off';
%                 end
%                 
%                 if isequal(NodeData(1),3) && NodeData(2) > 0
%                     
%                     ValuePs = app.SecondaryStd_List.Value;
%                     
%                     Xinterval = app.Integrations.Measurements(ValuePs).Times(NodeData(2),:);
%                     Yi = Y(app.Integrations.Measurements(ValuePs).Positions(NodeData(2),1):app.Integrations.Measurements(ValuePs).Positions(NodeData(2),2));
%                     Yi = Yi(find(~isnan(Yi)));
%                     TF = isoutlier(Yi);
%                     YValue = mean(Yi(find(TF == 0)));
%                     YStd = std(Yi(find(TF == 0)));
%                     
%                     p=fill(app.Plot,[Xinterval(1) Xinterval(2) Xinterval(2) Xinterval(1)],[YValue+0.5*YStd YValue+0.5*YStd YValue-0.5*YStd YValue-0.5*YStd],'r');
%                     p.FaceAlpha = 0.5;
%                     
%                     app.Position_Min.Value = app.Integrations.Measurements(ValuePs).Positions(NodeData(2),1);
%                     app.Position_Max.Value = app.Integrations.Measurements(ValuePs).Positions(NodeData(2),2);
%                     
%                     app.SweepLabel.Visible = 'on';
%                     app.Position_Min.Visible = 'on';
%                     app.Position_Max.Visible = 'on';
%                     
%                     % Update Plot
%                     DurationView = duration(app.Plot.XLim(2)-app.Plot.XLim(1));
%                     DurationInter = Xinterval(2)-Xinterval(1);
%                     PositionInter = Xinterval(1)+DurationInter/2;
%                     
%                     app.Plot.XLim = [PositionInter-3*DurationInter PositionInter+3*DurationInter];
%                     Min = YValue-0.4*YValue;
%                     if Min <= 0
%                         Min = 1; % counts/s
%                     end
%                     Max = YValue+0.4*YValue;
%                     if Max <= 0
%                         Max = 2; % counts/s
%                     end
%                     app.Plot.YLim = [Min Max];
%                     
%                     %                     app.Plot.XLim = [PositionInter-DurationView/2 PositionInter+DurationView/2];
%                     %                     DeltaY = app.Plot.YLim(2)-app.Plot.YLim(1);
%                     %                     app.Plot.YLim = [YValue-(DeltaY/2) YValue+(DeltaY/2)];
%                     
%                     return % to keep the display
%                 else
%                     ZoomResetButtonPushed(app);
%                     app.SweepLabel.Visible = 'off';
%                     app.Position_Min.Visible = 'off';
%                     app.Position_Max.Visible = 'off';
%                 end
%                 
%                 
%                 if isequal(NodeData(1),4) && NodeData(2) > 0
%                     
%                     ValuePs = app.Maps_List.Value;
%                     
%                     Xinterval = app.Integrations.Measurements(ValuePs).Times(NodeData(2),:);
%                     Yi = Y(app.Integrations.Measurements(ValuePs).Positions(NodeData(2),1):app.Integrations.Measurements(ValuePs).Positions(NodeData(2),2));
%                     Yi = Yi(find(~isnan(Yi)));
%                     TF = isoutlier(Yi);
%                     YValue = mean(Yi(find(TF == 0)));
%                     YStd = std(Yi(find(TF == 0)));
%                     
%                     p=fill(app.Plot,[Xinterval(1) Xinterval(2) Xinterval(2) Xinterval(1)],[YValue+0.5*YStd YValue+0.5*YStd YValue-0.5*YStd YValue-0.5*YStd],'r');
%                     p.FaceAlpha = 0.5;
%                     
%                     % Update Plot
%                     DurationView = duration(app.Plot.XLim(2)-app.Plot.XLim(1));
%                     DurationInter = Xinterval(2)-Xinterval(1);
%                     PositionInter = Xinterval(1)+DurationInter/2;
%                     
%                     app.Plot.XLim = [PositionInter-3*DurationInter PositionInter+3*DurationInter];
%                     Min = YValue-0.4*YValue;
%                     if Min <= 0
%                         Min = 1; % counts/s
%                     end
%                     Max = YValue+0.4*YValue;
%                     if Max <= 0
%                         Max = 2; % counts/s
%                     end
%                     app.Plot.YLim = sort([Min Max]);
%                     
%                     %app.Plot.XLim = [PositionInter-DurationView/2 PositionInter+DurationView/2];
%                     %DeltaY = app.Plot.YLim(2)-app.Plot.YLim(1);
%                     %app.Plot.YLim = [YValue-(DeltaY/2) YValue+(DeltaY/2)];
%                     
%                     return % to keep the display
%                 end
                
            else
                
                % update 4.4
                %
                %                 XLimTemp = app.Plot.XLim;
                %                 YLimTemp = app.Plot.YLim;
                %
                %
                %                 keyboard
                
                if app.Background_CorrectionList.Value > 1 && isequal(app.Background_CorrectionList.Enable,'on')
                    Min = min(app.Data4Plot.Data(value).Background)-0.25*min(app.Data4Plot.Data(value).Background);
                    Max = max(app.Data4Plot.Data(value).Background)+0.25*max(app.Data4Plot.Data(value).Background);
                    if Min > 0 && Max > 0 && Max > Min
                        app.Plot.YLim = [Min Max];
                    end
                end
                
                if app.PrimaryStd_CorrectionList.Value > 1 && isequal(app.PrimaryStd_CorrectionList.Enable,'on')
                    Min = min(app.Data4Plot.Data(value).Cps_PrimaryStandard)-0.25*min(app.Data4Plot.Data(value).Cps_PrimaryStandard);
                    Max = max(app.Data4Plot.Data(value).Cps_PrimaryStandard)+0.25*max(app.Data4Plot.Data(value).Cps_PrimaryStandard);
                    if ~isempty(Min) && ~isempty(Max)   % Can be empty if no data (bad selection)
                        if Min > 0 && Max > 0 && Max > Min
                            app.Plot.YLim = [Min Max];
                        end
                    end
                end
                
            end
            
            
        end

        % Selection changed function: Tree
        function TreeSelectionChanged(app, event)
            %selectedNodes = app.Tree.SelectedNodes;
            
            if isempty(app.Node_Background.Children)
                return
            end
            
            if isequal(app.Tree.SelectedNodes.NodeData(1),1) && app.Tree.SelectedNodes.NodeData(2) > 0
                app.Tree.SelectedNodes.ContextMenu = app.ContextMenu_Tree_Background;
            end
            if isequal(app.Tree.SelectedNodes.NodeData(1),2) && app.Tree.SelectedNodes.NodeData(2) > 0
                app.Tree.SelectedNodes.ContextMenu = app.ContextMenu_Tree_Standard;
            end
            
            PlotMenuDropDownValueChanged(app,0);
            
            
        end

        % Value changed function: BackgroundFilterPercentSpinner
        function BackgroundFilterPercentSpinnerValueChanged(app, event)
            %value = app.BackgroundFilterPercentSpinner.Value;
            
            UpdateTimeIntegration_Background(app);
            PlotMenuDropDownValueChanged(app,0);
            
        end

        % Value changed function: Position_Max, Position_Min
        function Background_MinValueChanged(app, event)
            
            NodeData = app.Tree.SelectedNodes.NodeData;
            
            switch NodeData(1)
                case 1
                    Idx = NodeData(2);
                    
                    if Idx
                        app.Integrations.Background.Positions(Idx,1) = app.Position_Min.Value;
                        app.Integrations.Background.Positions(Idx,2) = app.Position_Max.Value;
                        
                        app.Integrations.Background.Times(Idx,1:2) = [app.Data.time_DT(app.Integrations.Background.Positions(Idx,1)),app.Data.time_DT(app.Integrations.Background.Positions(Idx,2))];
                        
                    end
                    PlotMenuDropDownValueChanged(app, 0);
                    
                case 2
                    Idx = NodeData(2);
                    ValuePs = app.PrimaryStd_List.Value;
                    
                    if Idx
                        app.Integrations.Measurements(ValuePs).Positions(Idx,1) = app.Position_Min.Value;
                        app.Integrations.Measurements(ValuePs).Positions(Idx,2) = app.Position_Max.Value;
                        
                        app.Integrations.Measurements(ValuePs).Times(Idx,1:2) = [app.Data.time_DT(app.Integrations.Measurements(ValuePs).Positions(Idx,1)),app.Data.time_DT(app.Integrations.Measurements(ValuePs).Positions(Idx,2))];
                        
                    end
                    PlotMenuDropDownValueChanged(app, 0);
                    
                    
            end
            
            
        end

        % Value changed function: Background_CorrectionList
        function Background_CorrectionListValueChanged(app, event)
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'XMapTools is running numbers';
            
            % clear Tree selection
            app.Tree.SelectedNodes = [];
            ZoomResetButtonPushed(app);
            
            Method = app.Background_CorrectionList.Value;
            
            BackMeas = zeros(size(app.Data.time_DT));
            for i = 1:size(app.Integrations.Background.Positions,1)
                BackMeas(app.Integrations.Background.Positions(i,1):app.Integrations.Background.Positions(i,2)) = ones(length([app.Integrations.Background.Positions(i,1):app.Integrations.Background.Positions(i,2)]),1);
            end
            IndBack = find(BackMeas);
            
            app.Data.BackgroundCorrection = zeros(size(app.Data.Cps));
            
            % Export data
            if app.DebugMode.Value
                app.WaitBar.Message = 'XMapTools is saving LAICPMS_RawBackground.txt (debug mode on)';
                fid = fopen(fullfile(app.XMapToolsApp.XMapTools_LastDir,'LAICPMS_RawBackground.txt'),'w');
                
                CodeS = '%s';
                CodeF = '%s';
                for i = 1:length(app.Data.ElName)
                    CodeS = [CodeS,'\t%s'];
                    CodeF = [CodeF,'\t%f'];
                end
                CodeS = [CodeS,'\n'];
                CodeF = [CodeF,'\n'];
                
                fprintf(fid,CodeS,'TimeStamp',app.Data.ElName{:});
                
                for i = 1:length(IndBack)
                    fprintf(fid,CodeF,app.Data.time_DT(IndBack(i)),app.Data.Cps(IndBack(i),:));
                end
                fclose(fid);
                app.WaitBar.Message = 'XMapTools is running numbers';
            end
            
            switch Method
                case 1 % None
                    PlotMenuDropDownValueChanged(app, 0);
                    app.BackgroundApplyButton.Enable = 'off';
                    ZoomResetButtonPushed(app);
                    
                    close(app.WaitBar)
                    return
                    
                    
                case 2 % Linear
                    for i = 1:size(app.Data.Cps,2)
                        x = app.Data.time_DN(IndBack);
                        y = app.Data.Cps(IndBack,i);
                        
                        TF = isoutlier(y);
                        y = y(find(TF == 0));
                        x = x(find(TF == 0));
                        
                        warning('off','all')
                        p = polyfit(x,y,1);
                        warning('on','all')
                        Yi = polyval(p,app.Data.time_DN);
                        
                        app.Data.BackgroundCorrection(:,i) = Yi;
                        app.Data4Plot.Data(3+i).Background = Yi;
                    end
                    
                case 3 % Polynomial
                    for i = 1:size(app.Data.Cps,2)
                        x = app.Data.time_DN(IndBack);
                        y = app.Data.Cps(IndBack,i);
                        
                        %TF = isoutlier(y);
                        %y = y(find(TF == 0));
                        %x = x(find(TF == 0));
                        
                        warning('off','all')
                        p = polyfit(x,y,2);
                        warning('on','all')
                        Yi = polyval(p,app.Data.time_DN);
                        
                        app.Data.BackgroundCorrection(:,i) = Yi;
                        app.Data4Plot.Data(3+i).Background = Yi;
                    end
                    
                case 4 % Step function
                    for i = 1:size(app.Data.Cps,2)
                        Yi = zeros(size(app.Data.time_DN));
                        for j = 1:size(app.Integrations.Background.Positions,1)
                            ValY = app.Data.Cps(app.Integrations.Background.Positions(j,1):app.Integrations.Background.Positions(j,2),i);
                            
                            %TF = isoutlier(ValY);
                            %ValY = ValY(find(TF == 0));
                            
                            MeanValue = mean(ValY);
                            if isequal(j,1)
                                Yi(1:app.Integrations.Background.Positions(j+1,1)-1) = MeanValue.*ones(size(Yi(1:app.Integrations.Background.Positions(j+1,1)-1)));
                            elseif j<size(app.Integrations.Background.Positions,2)
                                Yi(app.Integrations.Background.Positions(j,1):app.Integrations.Background.Positions(j+1,1)-1) = MeanValue.*ones(size(Yi(PreviousPos+1:app.Integrations.Background.Positions(j+1,1)-1)));
                            else
                                Yi(app.Integrations.Background.Positions(j,1):end) = MeanValue.*ones(size(Yi(app.Integrations.Background.Positions(j,1):end)));
                            end
                            
                        end
                        
                        app.Data.BackgroundCorrection(:,i) = Yi;
                        app.Data4Plot.Data(3+i).Background = Yi;
                    end
                    
                case 5 % Spline
                    for i = 1:size(app.Data.Cps,2)
                        %                         x = app.Data.time_DN(IndBack);
                        %                         y = app.Data.Cps(IndBack,i);
                        %
                        %                         TF = isoutlier(y);
                        %                         y = y(find(TF == 0));
                        %                         x = x(find(TF == 0));
                        
                        Yi_t = zeros(size(app.Data.time_DN));
                        for j = 1:size(app.Integrations.Background.Positions,1)
                            ValY = app.Data.Cps(app.Integrations.Background.Positions(j,1):app.Integrations.Background.Positions(j,2),i);
                            TF = isoutlier(ValY);
                            ValY = ValY(find(TF == 0));
                            MeanValue = mean(ValY);
                            if isequal(MeanValue,0)
                                MeanValue = 1;
                            end
                            Yi_t(app.Integrations.Background.Positions(j,1)) = MeanValue;
                            Yi_t(app.Integrations.Background.Positions(j,2)) = MeanValue;
                        end
                        
                        IndBackSpec = find(Yi_t > 0);
                        
                        x = app.Data.time_DN(IndBackSpec);
                        y = Yi_t(IndBackSpec);
                        
                        warning('off','all')
                        Yi = spline(x,y,app.Data.time_DN);
                        warning('on','all')
                        
                        app.Data.BackgroundCorrection(:,i) = Yi;
                        app.Data4Plot.Data(3+i).Background = Yi;
                    end
            end
            
            UpdateTimeIntegration_Background(app);
            
            app.Tree.SelectedNodes = [];
            
            app.Data4Plot.Data(1).Background = sum(app.Data.BackgroundCorrection,2);
            
            % Plot the correction
            PlotMenuDropDownValueChanged(app, 0);
            
            app.BackgroundApplyButton.Enable = 'on';
            
            close(app.WaitBar)
            
        end

        % Button pushed function: BackgroundApplyButton
        function BackgroundApplyButtonPushed(app, event)
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Applying background correction';
            
            app.Tree.SelectedNodes = [];
            
            app.ExtractIntegrationsBackgroundButton.Enable = 'off';
            app.BackgroundFilterPercentSpinnerLabel.Enable = 'off';
            app.BackgroundFilterPercentSpinner.Enable = 'off';
            app.Background_CorrectionList.Enable = 'off';
            app.BackgroundApplyButton.Enable = 'off';
            
            % Generate corrected signal
            app.Data.Cps_BackCorr = app.Data.Cps - app.Data.BackgroundCorrection;
            
            WereNeg = find(app.Data.Cps_BackCorr <= 0);
            app.Data.Cps_BackCorr(WereNeg) = nan(size(WereNeg));
            
            app.Data.SumData_BackCorr = sum(app.Data.Cps_BackCorr,2);
            
            Pos = length(app.Data4Plot.List)+1;
            PosNew = Pos;
            
            app.Data4Plot.List{Pos} = '---';
            app.Data4Plot.Data(Pos).Type = 'Empty';
            app.Data4Plot.Data(Pos).X = 0;
            app.Data4Plot.Data(Pos).Y = 0;
            app.Data4Plot.Data(Pos).Background = [];
            
            Pos = Pos+1;
            app.Data4Plot.List{Pos} = 'BackCorr_Sum';
            app.Data4Plot.Data(Pos).Type = 'BC';
            app.Data4Plot.Data(Pos).X = app.Data.time_DT;
            app.Data4Plot.Data(Pos).Y = app.Data.SumData_BackCorr;
            app.Data4Plot.Data(Pos).Background = [];
            
            Pos = Pos+1;
            app.Data4Plot.List{Pos} = 'BackCorr_All';
            app.Data4Plot.Data(Pos).Type = 'BC_All';
            app.Data4Plot.Data(Pos).Labels = app.Data.ElName;
            app.Data4Plot.Data(Pos).X = app.Data.time_DT;
            app.Data4Plot.Data(Pos).Y = app.Data.Cps_BackCorr;
            app.Data4Plot.Data(Pos).Background = [];
            
            Pos = Pos+1;
            app.Data4Plot.List{Pos} = '---';
            app.Data4Plot.Data(Pos).Type = 'Empty';
            app.Data4Plot.Data(Pos).X = 0;
            app.Data4Plot.Data(Pos).Y = 0;
            app.Data4Plot.Data(Pos).Background = [];
            
            app.Start_Idx_Data4Plot_BackgroundCorrected = Pos;   % work for +i
            
            for i = 1:length(app.Data.ElName)
                app.Data4Plot.List{Pos+i} = ['BackCorr_',char(app.Data.ElName{i})];
                app.Data4Plot.Data(Pos+i).Type = 'BC';
                app.Data4Plot.Data(Pos+i).X = app.Data.time_DT;
                app.Data4Plot.Data(Pos+i).Y = app.Data.Cps_BackCorr(:,i);
                app.Data4Plot.Data(Pos+i).Background = [];
            end
            
            app.PlotMenuDropDown.Items = app.Data4Plot.List;
            app.PlotMenuDropDown.ItemsData = [1:length(app.Data4Plot.List)];
            app.PlotMenuDropDown.Value = PosNew+1;
            
            ZoomResetButtonPushed(app);
            
            PlotMenuDropDownValueChanged(app,0);
            
            app.PRIMARYSTANDARDPanel.Visible = 'on';
            
            app.PrimaryStd_List.Items = app.Integrations.TypeNames;
            app.PrimaryStd_List.ItemsData = [1:length(app.Integrations.TypeNames)];
            app.PrimaryStd_List.Value = 1;
            
            app.SecondaryStd_List.Items = app.Integrations.TypeNames;
            app.SecondaryStd_List.ItemsData = [1:length(app.Integrations.TypeNames)];
            app.SecondaryStd_List.Value = 2;
            
            app.Maps_List.Items = app.Integrations.TypeNames;
            app.Maps_List.ItemsData = [1:length(app.Integrations.TypeNames)];
            app.Maps_List.Value = length(app.Maps_List.Items);                  % was 3; solve a problem when only 2 types of data are available (only 1 standard)
            
            app.PrimaryStd_ApplyButton.Enable = 'off';
            
            collapse(app.Node_Background);
            
            PrimaryStd_ListValueChanged(app,0);
            
            close(app.WaitBar)
            
        end

        % Value changed function: PrimaryStd_CorrectionList
        function PrimaryStd_CorrectionListValueChanged(app, event)
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'XMapTools is running numbers';
            
            app.Tree.SelectedNodes = [];
            ZoomResetButtonPushed(app);
            
            Method = app.PrimaryStd_CorrectionList.Value;
            
            ValuePs = app.PrimaryStd_List.Value;
            
            SignalMeas = zeros(size(app.Data.time_DT));
            for i = 1:size(app.Integrations.Measurements(ValuePs).Positions,1)
                SignalMeas(app.Integrations.Measurements(ValuePs).Positions(i,1):app.Integrations.Measurements(ValuePs).Positions(i,2)) = ones(length([app.Integrations.Measurements(ValuePs).Positions(i,1):app.Integrations.Measurements(ValuePs).Positions(i,2)]),1);
            end
            IndSignal = find(SignalMeas);
            
            app.Data.PS(app.iPrStd).PrimaryStandard = zeros(size(app.Data.Cps));
            
            % Export data
            if app.DebugMode.Value
                app.WaitBar.Message = 'XMapTools is saving LAICPMS_PrimaryStd_BCInt.txt (debug mode on)';
                fid = fopen(fullfile(app.XMapToolsApp.XMapTools_LastDir,'LAICPMS_PrimaryStd_BCInt.txt'),'w');
                
                CodeS = '%s';
                CodeF = '%s';
                for i = 1:length(app.Data.ElName)
                    CodeS = [CodeS,'\t%s'];
                    CodeF = [CodeF,'\t%f'];
                end
                CodeS = [CodeS,'\n'];
                CodeF = [CodeF,'\n'];
                
                fprintf(fid,CodeS,'TimeStamp',app.Data.ElName{:});
                
                for i = 1:length(IndSignal)
                    fprintf(fid,CodeF,app.Data.time_DT(IndSignal(i)),app.Data.Cps_BackCorr(IndSignal(i),:));
                end
                fclose(fid);
            end
            
            app.WaitBar.Message = 'Interpolating signal for primary standard';
            switch Method
                case 1 % None
                    PlotMenuDropDownValueChanged(app, 0);
                    app.PrimaryStd_ApplyButton.Enable = 'off';
                    ZoomResetButtonPushed(app);
                    close(app.WaitBar)
                    return
                    
                    
                case 2 % Linear
                    for i = 1:size(app.Data.Cps_BackCorr,2)
                        x = app.Data.time_DN(IndSignal);
                        y = app.Data.Cps_BackCorr(IndSignal,i);
                        
                        IndNotNan = find(~isnan(y));
                        x = x(IndNotNan);
                        y = y(IndNotNan);
                        
                        TF = isoutlier(y);
                        y = y(find(TF == 0));
                        x = x(find(TF == 0));
                        
                        warning('off','all')
                        p = polyfit(x,y,1);
                        warning('on','all')
                        Yi = polyval(p,app.Data.time_DN);
                        
                        app.Data.PS(app.iPrStd).Cps_PrimaryStandard(:,i) = Yi;
                        app.Data4Plot.Data(app.Start_Idx_Data4Plot_BackgroundCorrected+i).Cps_PrimaryStandard = Yi;
                        
                    end
                    
                case 3 % Polynomial
                    for i = 1:size(app.Data.Cps_BackCorr,2)
                        x = app.Data.time_DN(IndSignal);
                        y = app.Data.Cps_BackCorr(IndSignal,i);
                        
                        IndNotNan = find(~isnan(y));
                        x = x(IndNotNan);
                        y = y(IndNotNan);
                        
                        TF = isoutlier(y);
                        y = y(find(TF == 0));
                        x = x(find(TF == 0));
                        
                        warning('off','all')
                        p = polyfit(x,y,2);
                        warning('on','all')
                        Yi = polyval(p,app.Data.time_DN);
                        
                        app.Data.PS(app.iPrStd).Cps_PrimaryStandard(:,i) = Yi;
                        app.Data4Plot.Data(app.Start_Idx_Data4Plot_BackgroundCorrected+i).Cps_PrimaryStandard = Yi;
                    end
                    
                    
                case 4 % Step function
                    for i = 1:size(app.Data.Cps_BackCorr,2)
                        Yi = zeros(size(app.Data.time_DN));
                        for j = 1:size(app.Integrations.Measurements(ValuePs).Positions,1)
                            ValY = app.Data.Cps_BackCorr(app.Integrations.Measurements(ValuePs).Positions(j,1):app.Integrations.Measurements(ValuePs).Positions(j,2),i);
                            IndNotNan = find(~isnan(ValY));
                            ValY = ValY(IndNotNan);
                            TF = isoutlier(ValY);
                            ValY = ValY(find(TF == 0));
                            MeanValue = mean(ValY);
                            if isequal(j,1)
                                Yi(1:app.Integrations.Measurements(ValuePs).Positions(j+1,1)-1) = MeanValue.*ones(size(Yi(1:app.Integrations.Measurements(ValuePs).Positions(j+1,1)-1)));
                            elseif j<size(app.Integrations.Measurements(ValuePs).Positions,2)
                                Yi(app.Integrations.Measurements(ValuePs).Positions(j,1):app.Integrations.Measurements(ValuePs).Positions(j+1,1)-1) = MeanValue.*ones(size(Yi(PreviousPos+1:app.Integrations.Measurements(ValuePs).Positions(j+1,1)-1)));
                            else
                                Yi(app.Integrations.Measurements(ValuePs).Positions(j,1):end) = MeanValue.*ones(size(Yi(app.Integrations.Measurements(ValuePs).Positions(j,1):end)));
                            end
                        end
                        
                        app.Data.PS(app.iPrStd).Cps_PrimaryStandard(:,i) = Yi;
                        app.Data4Plot.Data(app.Start_Idx_Data4Plot_BackgroundCorrected+i).Cps_PrimaryStandard = Yi;
                    end
                    
                case 5 % Spline
                    for i = 1:size(app.Data.Cps,2)
                        Yi_t = zeros(size(app.Data.time_DN));
                        for j = 1:size(app.Integrations.Measurements(ValuePs).Positions,1)
                            ValY = app.Data.Cps_BackCorr(app.Integrations.Measurements(ValuePs).Positions(j,1):app.Integrations.Measurements(ValuePs).Positions(j,2),i);
                            IndNotNan = find(~isnan(ValY));
                            ValY = ValY(IndNotNan);
                            TF = isoutlier(ValY);
                            ValY = ValY(find(TF == 0));
                            MeanValue = mean(ValY);
                            if isequal(MeanValue,0)
                                MeanValue = 1;    % not sure if this judicious here
                            end
                            Yi_t(app.Integrations.Measurements(ValuePs).Positions(j,1)) = MeanValue;
                            Yi_t(app.Integrations.Measurements(ValuePs).Positions(j,2)) = MeanValue;
                        end
                        
                        IndMeasurementSpec = find(Yi_t > 0);
                        
                        x = app.Data.time_DN(IndMeasurementSpec);
                        y = Yi_t(IndMeasurementSpec);
                        
                        warning('off','all')
                        Yi = spline(x,y,app.Data.time_DN);
                        warning('on','all')
                        
                        app.Data.PS(app.iPrStd).Cps_PrimaryStandard(:,i) = Yi;
                        app.Data4Plot.Data(app.Start_Idx_Data4Plot_BackgroundCorrected+i).Cps_PrimaryStandard = Yi;
                    end
                    
                    
                    
                    
            end
            
            app.Data4Plot.Data(app.Start_Idx_Data4Plot_BackgroundCorrected-2).Cps_PrimaryStandard = sum(app.Data.PS(app.iPrStd).Cps_PrimaryStandard,2);
            
            ZoomResetButtonPushed(app);
            app.Tree.SelectedNodes = [];
            
            % Plot the correction
            PlotMenuDropDownValueChanged(app, 0);
            
            app.PrimaryStd_ApplyButton.Enable = 'on';
            
            close(app.WaitBar)
            
        end

        % Value changed function: PrimaryStd_List
        function PrimaryStd_ListValueChanged(app, event)
            Value = app.PrimaryStd_List.Value;
            
            % UPDATE TREE
            CleanTree(app,'Primary');
            for i = 1:length(app.Integrations.Measurements(Value).Names)
                p = uitreenode(app.Node_PrimaryStandard,'Text',char(app.Integrations.Measurements(Value).Names{i}),'NodeData',[2,i]);
            end
            expand(app.Node_PrimaryStandard);
            
            UpdateTimeIntegration_PrimaryStd(app);
            
            if app.PrimaryStd_CorrectionList.Value > 1
                PrimaryStd_CorrectionListValueChanged(app);
            end
            
            
        end

        % Value changed function: PrimaryStd_FilterPercentSpinner
        function PrimaryStd_FilterPercentSpinnerValueChanged(app, event)
            UpdateTimeIntegration_PrimaryStd(app);
            
            if app.PrimaryStd_CorrectionList.Value > 1
                PrimaryStd_CorrectionListValueChanged(app, event);
            end
            
            PlotMenuDropDownValueChanged(app,0);
        end

        % Button pushed function: PrimaryStd_ApplyButton
        function PrimaryStd_ApplyButtonPushed(app, event)
            
            Std = app.PrimaryStd_List.Items{app.PrimaryStd_List.Value};
            
            [IsStd] = ismember(app.RefData_LAICPMS.StdNames,upper(Std));
            
            if ~isequal(sum(IsStd),1)
                
                ListNames = app.RefData_LAICPMS.StdNames;
                ListNames{end+1} = 'Other...';
                
                Answer = listdlg('ListString',ListNames,'SelectionMode','single','Name','XMapTools','PromptString','Select a primary standard in the list below, or other to select a file manually');
                
                if isempty(Answer)
                    return
                end
                
                if Answer < length(ListNames)
                    WhereStd = Answer;
                else
                    uialert(app.ConverterLAICPMS,'You need to add your standard first using the tool available in the option section!','XMapTools');
                    return
                end
                
            else
                WhereStd = find(IsStd);
            end
            
            ElListStd = app.RefData_LAICPMS.Data(WhereStd).Elem;
            ElAnalysis = app.Data.ElName;
            
            [IsElem,ElemIdx] = ismember(ElAnalysis,ElListStd);
            
            if ~isequal(sum(IsElem),length(IsElem))
                
                for i =1:length(IsElem)
                    if IsElem(i)
                        app.Data.PS(app.iPrStd).PrimaryStandard_ElemConc(i) = app.RefData_LAICPMS.Data(WhereStd).Value(ElemIdx(i));
                    else
                        app.Data.PS(app.iPrStd).PrimaryStandard_ElemConc(i) = 0;
                    end
                end
                
                MissingEl = ElAnalysis(find(IsElem == 0));
                
                uialert(app.ConverterLAICPMS,['Warning: reference data is not defined in the standard file for the following elements:',MissingEl,{''},'A composition of zero is assumed for the reference material in this case'],'XMapTools','CloseFcn','uiresume(gcbf)');
                uiwait(gcbf)
                
            else
                % All elements were found in the standard file:
                app.Data.PS(app.iPrStd).PrimaryStandard_ElemConc = app.RefData_LAICPMS.Data(WhereStd).Value(ElemIdx);
            end
            
            app.Data.PS(app.iPrStd).Name = [app.PrimaryStd_List.Items{app.PrimaryStd_List.Value},'_',app.PrimaryStd_CorrectionList.Items{app.PrimaryStd_CorrectionList.Value}];
            
            Selection = uiconfirm(app.ConverterLAICPMS,'Would you like to select an additional primary standard?','XMapTools','Options',{'Yes','No (Continue)'},'DefaultOption','No (Continue)');
            
            switch Selection
                case 'Yes'
                    app.PrimaryStd_CorrectionList.Value = 1;
                    PrimaryStd_CorrectionListValueChanged(app,0);
                    
                    app.iPrStd = app.iPrStd + 1;
                    
                case 'No (Continue)'
                    app.PrimaryStd_CorrectionList.Enable = 'off';
                    app.PrimaryStd_List.Enable = 'off';
                    app.PrimaryStd_FilterPercentSpinner.Enable = 'off';
                    app.PrimaryStd_FilterPercentSpinnerLabel.Enable = 'off';
                    app.PrimaryStd_ApplyButton.Enable = 'off';
                    
                    app.SecondaryStd_ListPS.Items = extractfield(app.Data.PS,'Name');
                    app.SecondaryStd_ListPS.ItemsData = [1:length(app.SecondaryStd_ListPS.Items)];
                    app.SecondaryStd_ListPS.Value = 1;
                    
                    app.SECONDARYSTANDARDPanel.Visible = 'on';
                    app.SecondaryStd_UITable.Visible = 'off';
                    
                    SecondaryStd_ListValueChanged(app,0);
            end
            
        end

        % Value changed function: SecondaryStd_IntStd
        function SecondaryStd_IntStdValueChanged(app, event)
            
            IntStdIdx = app.SecondaryStd_IntStd.Value;
            
            Con_Unk_Median = CalibrateSecondaryStandardFromIt(app,IntStdIdx);
            
            TableSecStd = {};
            for i = 1:length(Con_Unk_Median)
                TableSecStd{i,1} = app.Data.ElName{i};
                TableSecStd{i,2} = app.Data.SecondaryStandard_ElemConc(i);
                TableSecStd{i,3} = Con_Unk_Median(i);
                TableSecStd{i,4} = (app.Data.SecondaryStandard_ElemConc(i)-Con_Unk_Median(i))/app.Data.SecondaryStandard_ElemConc(i)*100;
            end
            
            app.SecondaryStd_UITable.Data = TableSecStd;
            app.SecondaryStd_UITable.Visible = 'On';
            
            app.SecondaryStd_ApplyButton.Enable = 'on';
            
            %keyboard
        end

        % Button pushed function: SecondaryStd_CalibrationMtx
        function SecondaryStd_CalibrationMtxButtonPushed(app, event)
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Applying calibration and generating the figure';
            
            NbEl = length(app.SecondaryStd_IntStd.Items);
            
            CalMtx = zeros(NbEl);
            
            for i = 1:NbEl
                Con_Unk_Median = CalibrateSecondaryStandardFromIt(app,i);
                CalMtx(i,:) = (app.Data.SecondaryStandard_ElemConc-Con_Unk_Median)./app.Data.SecondaryStandard_ElemConc*100;
            end
            
            ColorPalette = [ ...
                1	0	0; ...
                1	0.0417	0; ...
                1	0.0833	0; ...
                1	0.125	0; ...
                1	0.1667	0; ...
                1	0.2083	0; ...
                1	0.25	0; ...
                1	0.2917	0; ...
                1	0.3333	0; ...
                1	0.375	0; ...
                1	0.4167	0; ...
                1	0.4583	0; ...
                1	0.5	0; ...
                1	0.5417	0; ...
                1	0.5833	0; ...
                1	0.625	0; ...
                1	0.6667	0; ...
                1	0.7083	0; ...
                1	0.75	0; ...
                1	0.7917	0; ...
                1	0.8333	0; ...
                1	0.875	0; ...
                1	0.9167	0; ...
                1	0.9583	0; ...
                1	1	0; ...
                1	1	0.0625; ...
                1	1	0.125; ...
                1	1	0.1875; ...
                1	1	0.25; ...
                1	1	0.3125; ...
                1	1	0.375; ...
                1	1	0.4375; ...
                1	1	0.5; ...
                1	1	0.5625; ...
                1	1	0.625; ...
                1	1	0.6875; ...
                1	1	0.75; ...
                1	1	0.8125; ...
                1	1	0.875; ...
                1	1	0.9375; ...
                1	1	1; ...
                1	1	1; ...
                1	1	0.9375; ...
                1	1	0.875; ...
                1	1	0.8125; ...
                1	1	0.75; ...
                1	1	0.6875; ...
                1	1	0.625; ...
                1	1	0.5625; ...
                1	1	0.5; ...
                1	1	0.4375; ...
                1	1	0.375; ...
                1	1	0.3125; ...
                1	1	0.25; ...
                1	1	0.1875; ...
                1	1	0.125; ...
                1	1	0.0625; ...
                1	1	0; ...
                1	0.9583	0; ...
                1	0.9167	0; ...
                1	0.875	0; ...
                1	0.8333	0; ...
                1	0.7917	0; ...
                1	0.75	0; ...
                1	0.7083	0; ...
                1	0.6667	0; ...
                1	0.625	0; ...
                1	0.5833	0; ...
                1	0.5417	0; ...
                1	0.5	0; ...
                1	0.4583	0; ...
                1	0.4167	0; ...
                1	0.375	0; ...
                1	0.3333	0; ...
                1	0.2917	0; ...
                1	0.25	0; ...
                1	0.2083	0; ...
                1	0.1667	0; ...
                1	0.125	0; ...
                1	0.0833	0; ...
                1	0.0417	0; ...
                1	0	0];
            
            figure
            imagesc(CalMtx); axis image
            ylabel('Internal Standard')
            hcb = colorbar;
            ylabel(hcb,'Variation compared to reference composition (in %)')
            MaxMax = max([abs(min(CalMtx(:))),max(CalMtx(:))]);
            caxis([-MaxMax MaxMax])
            colormap(ColorPalette);
            ax = gca;
            ax.XTick = [1:1:NbEl];
            ax.XTickLabel = app.Data.ElName;
            ax.XAxisLocation = 'top';
            ax.YTick = [1:1:NbEl];
            ax.YTickLabel = app.Data.ElName;
            ax.YTickLabelRotation = 90;
            for i = 1:NbEl
                for j = 1:NbEl
                    if CalMtx(i,j) > 10 || CalMtx(i,j) < -10
                        t = text(i,j,num2str(round(CalMtx(i,j),1)));
                    else
                        t = text(i,j,num2str(round(CalMtx(i,j),2)));
                    end
                    t.HorizontalAlignment = 'center';
                end
            end
            
            close(app.WaitBar)
            
        end

        % Value changed function: SecondaryStd_FilterPercentSpinner
        function SecondaryStd_FilterPercentSpinnerValueChanged(app, event)
            UpdateTimeIntegration_SecondaryStd(app);
            PlotMenuDropDownValueChanged(app,0);
        end

        % Value changed function: SecondaryStd_List
        function SecondaryStd_ListValueChanged(app, event)
            
            PrepareSecondaryStandardData(app);
            
            app.SecondaryStd_ApplyButton.Enable = 'off';
            
            SecondaryStd_IntStdValueChanged(app);
            
        end

        % Button pushed function: SecondaryStd_ApplyButton
        function SecondaryStd_ApplyButtonPushed(app, event)
            
            % save the table
            
            fid = fopen('last_std_calibration_test.txt','w');
            
            fprintf(fid,'%s\n%s\n','--','Last standard calibration test exported from the XMapTools converter module for LA-ICPMS data');
            fprintf(fid,'%s\n\n',datestr(now));
            
            fprintf(fid,'%s\t%s\n','Primary standard:',app.SecondaryStd_ListPS.Items{app.SecondaryStd_ListPS.Value});
            fprintf(fid,'%s\t%s\n','Secontary standard:',app.SecondaryStd_List.Items{app.SecondaryStd_List.Value});
            fprintf(fid,'%s\t%s\n','Internal standard:',app.SecondaryStd_IntStd.Items{app.SecondaryStd_IntStd.Value});
            
            fprintf(fid,'\n%s\t%s\t%s\t%s\t\n','El','Ref µg/g','Calc µg/g','Delta %');
            
            for i = 1:size(app.SecondaryStd_UITable.Data,1)
                fprintf(fid,'%s\t%.2f\t%.2f\t%.2f\t\n',app.SecondaryStd_UITable.Data{i,1},app.SecondaryStd_UITable.Data{i,2},app.SecondaryStd_UITable.Data{i,3},app.SecondaryStd_UITable.Data{i,4});
            end
            
            fclose(fid);
            
            app.SecondaryStd_ApplyButton.Enable = 'off';
            app.SecondaryStd_IntStd.Enable = 'off';
            app.SecondaryStd_IntStdLabel.Enable = 'off';
            
            app.SecondaryStd_CalibrationMtx.Enable = 'off';
            app.SecondaryStd_PlotSecStd.Enable = 'off';
            
            app.SecondaryStd_ListPS.Enable = 'off';
            app.SecondaryStd_Cancel.Enable = 'off';
            
            app.SecondaryStd_UITable.Enable = 'off';
            app.SecondaryStd_FilterPercentSpinner.Enable = 'off';
            app.SecondaryStd_FilterPercentSpinnerLabel.Enable = 'off';
            app.SecondaryStd_List.Enable = 'off';
            
            collapse(app.Node_SecondaryStandard);
            
            app.MAPSPanel.Visible = 'on';
            
            PrepareScanData(app);
            
        end

        % Menu selected function: CopyMenu
        function CopyMenuSelected(app, event)
            
            
            d = [app.SecondaryStd_UITable.Data];
            
            str = '';
            for i = 1:size(d,1)
                str = sprintf('%s\n%s\t%f\t%f\t%f',str,char(d{i,1}),d{i,2},d{i,3},d{i,4});
            end
            
            clipboard ('copy',str);
            
        end

        % Button pushed function: Maps_ApplyButton
        function Maps_ApplyButtonPushed(app, event)
            
            CalculateMapsCassis(app);            % XMapTools 4.4
            %CalculateMapsTheoule(app);          % XMapTools 4.3
            %CalculateMaps(app);
            %CalculateMaps_Wengen(app);
            
            app.CheckButton.Visible = 'On';
            app.ShowButton.Visible = 'On';
            app.ExportButton.Visible = 'On';
            app.Maps_SelElement.Visible = 'On';
            
            app.Maps_SelElement.Items = app.Data.ElName;
            app.Maps_SelElement.ItemsData = [1:length(app.Data.ElName)];
            
            app.Maps_ApplyButton.Enable = 'Off';
            app.Maps_TimeshiftSpinner.Enable = 'Off';
            app.Maps_List.Enable = 'Off';
            
            
            
        end

        % Value changed function: Maps_List
        function Maps_ListValueChanged(app, event)
            
            PrepareScanData(app);
            
        end

        % Value changed function: Maps_TimeshiftSpinner
        function Maps_TimeshiftSpinnerValueChanged(app, event)
            UpdateTimeIntegration_Scans(app);
            PlotMenuDropDownValueChanged(app,0);
        end

        % Button pushed function: CheckButton
        function CheckButtonPushed(app, event)
            figure, viscircles(app.Data.MapGeometryCheck.XY_Spots,app.Data.MapGeometryCheck.Radius_Spots)
            hold on
            plot(app.Data.MapGeometryCheck.X_grid,app.Data.MapGeometryCheck.Y_grid,'sk','MarkerFaceColor','k')
            axis image
            ax = gca;
            ax.YDir = 'reverse';
            %figure, plot(app.Data.MapGeometryCheck.xbin,app.Data.MapGeometryCheck.ybin,'+')
            %axis image
        end

        % Button pushed function: ShowButton
        function ShowButtonPushed(app, event)
            
            Value = app.Maps_SelElement.Value;
            
            TheMapTheoule = app.Data.Cps_Maps(Value).Map;
            TheMapCassis = zeros(size(TheMapTheoule));
            
            for i = 1:length(app.PxDataRaw.PixelCoordXY)
                if ~isempty(app.PxDataRaw.ElData(Value).PxData(i).Intensity)
                    Sel = find(app.PxDataRaw.ElData(Value).PxData(i).Intensity > 0);
                    TheMapCassis(app.PxDataRaw.PixelCoordXY(i,2),app.PxDataRaw.PixelCoordXY(i,1)) = mean(app.PxDataRaw.ElData(Value).PxData(i).Intensity(Sel));
                end
            end
            
            figure,
            
            tiledlayout('flow','TileSpacing','Compact')
            
            nexttile
            imagesc(app.Data.Cps_Maps(Value).Map), axis image, set(gca,'ColorScale','log'), colorbar, colormap([0,0,0;parula(128)])
            climit = caxis(gca);
            title('Intensity map for unknowns (interp. + back. corrected)')
            
            nexttile
            imagesc(TheMapCassis), axis image, set(gca,'ColorScale','log'), colorbar
            caxis(climit)
            title('Intensity map mean(sweeps) (back. corrected)')
            
            nexttile
            imagesc(app.Data.StdMaps(Value).Std_Map(app.SecondaryStd_ListPS.Value).Cps), axis image, colorbar
            title(['Intensity map for standard ',app.Data.StdMaps(Value).Std_Map(app.SecondaryStd_ListPS.Value).StdName],'Interpreter','none')
            
            nexttile
            imagesc(app.Data.StdMaps(Value).Int_Back), axis image, colorbar
            title('Background intensity map')
            
            nexttile
            imagesc(app.Data.StdMaps(Value).Std_Map(app.SecondaryStd_ListPS.Value).Conc), axis image, colorbar
            title(['Composition map for standard ',app.Data.StdMaps(Value).Std_Map(app.SecondaryStd_ListPS.Value).StdName],'Interpreter','none')
            
            nexttile
            imagesc(app.Data.StdMaps(Value).Sweeps_Back), axis image, colorbar
            title('Number of sweeps for background')
            
            nexttile
            imagesc(app.Data.StdMaps(Value).Sweeps_Pixel), axis image, colorbar
            title('Number of sweeps for unknowns')
            
        end

        % Button pushed function: ExportButton
        function ExportButtonPushed(app, event)
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Exporting maps';
            
            if isdir(fullfile(cd,'Maps_cps'))
                
                Answer = uiconfirm(app.ConverterLAICPMS, 'Directory is not empty and existing files will be deleted. Would you like to continue?', 'Warning', 'Options', {'Yes','No'});
                
                if isequal(Answer,'Yes')
                    [SUCCESS,MESSAGE,MESSAGEID] = rmdir(fullfile(cd,'Maps_cps'),'s');
                    [Success,Message,MessageID] = mkdir(fullfile(cd,'Maps_cps'));
                    Directory = fullfile(cd,'Maps_cps');
                else
                    app.WaitBar.Message = 'Maps_cps already exist, select an empty directory for saving data';
                    
                    f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
                    [Directory] = uigetdir('Pick a directory for saving the maps');
                    delete(f);
                    figure(app.ConverterLAICPMS)
                    
                    if isequal(Directory,0) || isequal(Directory,fullfile(cd,'Maps_cps'))
                        close(app.WaitBar)
                        return
                    end
                end
            else
                [Success,Message,MessageID] = mkdir(fullfile(cd,'Maps_cps'));
                Directory = fullfile(cd,'Maps_cps');
            end
            
            if isequal(app.TestMode.Value,1)
                Directory_AVG = [Directory,'_AVG'];
                [Success,Message,MessageID] = mkdir(Directory_AVG);
            end
            
            if exist('AcqMethod.xml')
                [EN,DT,MZ] = ReadXML_Method_LAICPMS('AcqMethod.xml');
                for i = 1:length(EN)
                    EN_Ori{i} = [EN{i},num2str(MZ(i))];
                end
            else
                EN = {};
                EN_Ori = {};
                DT = [];
                MZ = [];
            end
            
            EN_Ok = {};
            DT_Ok = 10*ones(size(app.Data.ElName));
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools');
            app.WaitBar.Message = 'Saving map and standard files';
            for i = 1:length(app.Data.Cps_Maps)
                app.WaitBar.Value = i/length(app.Data.Cps_Maps);
                TheMap = app.Data.Cps_Maps(i).Map;
                save(fullfile(Directory,[char(app.Data.ElNameFormated{i}),'.txt']),'TheMap','-ascii');
                
                if isequal(app.TestMode.Value,1)
                    TheMapCassis = zeros(size(TheMap));
              
                    for ii = 1:length(app.PxDataRaw.PixelCoordXY)
                        if ~isempty(app.PxDataRaw.ElData(i).PxData(ii).Intensity)
                            Sel = find(app.PxDataRaw.ElData(i).PxData(ii).Intensity > 0);
                            TheMapCassis(app.PxDataRaw.PixelCoordXY(ii,2),app.PxDataRaw.PixelCoordXY(ii,1)) = mean(app.PxDataRaw.ElData(i).PxData(ii).Intensity(Sel));
                        end
                    end
                    
                    save(fullfile(Directory_AVG,[char(app.Data.ElNameFormated{i}),'.txt']),'TheMapCassis','-ascii');
                end
                
                Id = find(ismember(EN_Ori,app.Data.ElNameOri{i}));
                if length(Id) > 1
                    Id = Id(1);      % we always take the first one
                end
                if ~isempty(Id)
                    EN_Ok{end+1} = app.Data.ElName{i};
                    DT_Ok(i) = DT(Id);
                end
            end
            
            % ------------------------------
            % MapStandards_Import: 
            
            MapStandards_Import.StdMaps = app.Data.StdMaps;
            MapStandards_Import.ElName = app.Data.ElName;
            
            save(fullfile(Directory,'MapStandards_Import.mat'),'MapStandards_Import');
            
            % ------------------------------
            % SweepData_Import (4.4): 
            
            SweepData_Import.PxDataRaw = app.PxDataRaw;
            save(fullfile(Directory,'SweepData_Import.mat'),'SweepData_Import');
            
            
            fid = fopen(fullfile(Directory,'Import.txt'),'w');
            fprintf(fid,'%s\n','#######################################################');
            fprintf(fid,'%s\n','#     XMapTools Last Import Settings (DO NOT EDIT)    #');
            fprintf(fid,'%s\n','#######################################################');
            fprintf(fid,'%s\n','...');
            fprintf(fid,'%s\n','DT_correction:						0');
            fprintf(fid,'%s\n','DT_param(DwellTime,DeadTime):		150.000		300.000');
            fprintf(fid,'%s\n','Fact(Col,Lin):						1			1');
            fprintf(fid,'%s\n','Rotation:							0');
            fprintf(fid,'%s\n','Replace_Negative_Values:			1');
            fprintf(fid,'%s\n','Replace_NaN_Values:					1');
            fclose(fid);
            
            fid = fopen(fullfile(cd,'Import.txt'),'w');
            fprintf(fid,'%s\n','#######################################################');
            fprintf(fid,'%s\n','#     XMapTools Last Import Settings (DO NOT EDIT)    #');
            fprintf(fid,'%s\n','#######################################################');
            fprintf(fid,'%s\n','...');
            fprintf(fid,'%s\n','DT_correction:						0');
            fprintf(fid,'%s\n','DT_param(DwellTime,DeadTime):		150.000		300.000');
            fprintf(fid,'%s\n','Fact(Col,Lin):						1			1');
            fprintf(fid,'%s\n','Rotation:							0');
            fprintf(fid,'%s\n','Replace_Negative_Values:			1');
            fprintf(fid,'%s\n','Replace_NaN_Values:					1');
            fclose(fid);
            
            if isequal(length(EN_Ok),length(DT_Ok))
                fid = fopen(fullfile(cd,'Import_DT.txt'),'w');
                fprintf(fid,'%s\n','##########################################################################');
                fprintf(fid,'%s\n','#     XMapTools Last Import Settings for DT & STANDARDS (DO NOT EDIT)    #');
                fprintf(fid,'%s\n','##########################################################################');
                fprintf(fid,'%s\n','...');
                for i = 1:length(EN_Ok)
                    fprintf(fid,'%s\t%s\t%f\n',EN_Ok{i},MapStandards_Import.StdMaps(1).Std_Map(1).StdName,DT_Ok(i));
                end
                fclose(fid);
            end
            
            % ------------------------------
            % Copy last standard calibration test (4.4): 
            try 
                copyfile(fullfile(cd,'last_std_calibration_test.txt'),fullfile(Directory,'last_std_calibration_test.txt'));    
            catch ME
                disp('Error copying the last_std_calibration_test.txt file (export continued...)')
            end
            
            app.WaitBar.Message = 'Maps have been successfully exported';
            
            if length(app.Maps_List.Items) > 3
                Answer = uiconfirm(app.ConverterLAICPMS,'Would you like to process other maps from the same dataset?','XMapTools','Options',{'Yes','No'},'DefaultOption','No');
                %Answer = questdlg('Would you like to process other maps from the same dataset?','XMapTools','Yes','No','No');
                
                if isequal(Answer,'Yes')
                    
                    app.Maps_ApplyButton.Enable = 'On';
                    app.Maps_TimeshiftSpinner.Enable = 'On';
                    app.Maps_List.Enable = 'On';
                    
                    app.CheckButton.Visible = 'off';
                    app.ShowButton.Visible = 'off';
                    app.ExportButton.Visible = 'off';
                    app.Maps_SelElement.Visible = 'off';
                    
                    close(app.WaitBar)
                    return
                end
            end
            
            close(app.WaitBar)
            close(app.ConverterLAICPMS);
            
        end

        % Button pushed function: HelpButton
        function HelpButtonPushed(app, event)
            
            if isempty(app.XMapToolsApp.Id_HelpTool)
                Help_Display(app.XMapToolsApp,'Converter_LAICPMS.html');
            else
                app.XMapToolsApp.Id_HelpTool.UpdateTextHelp('Converter_LAICPMS.html');
            end
            
        end

        % Button pushed function: ZoomIn
        function ZoomInButtonPushed(app, event)
            
            switch app.Plot.Toolbar.Children(3).Value
                case 'on'
                    app.Plot.Toolbar.Children(3).Value = 'off';
                    matlab.graphics.interaction.webmodes.toggleMode(app.Plot,'zoom',app.Plot.Toolbar.Children(3).Value)
                case 'off'
                    app.Plot.Toolbar.Children(3).Value = 'on';
                    matlab.graphics.interaction.webmodes.toggleMode(app.Plot,'zoom',app.Plot.Toolbar.Children(3).Value)
            end
            
            
        end

        % Button pushed function: ZoomOut
        function ZoomOutButtonPushed(app, event)
            
            switch app.Plot.Toolbar.Children(4).Value
                case 'on'
                    app.Plot.Toolbar.Children(4).Value = 'off';
                    matlab.graphics.interaction.webmodes.toggleMode(app.Plot,'zoomout',app.Plot.Toolbar.Children(4).Value)
                case 'off'
                    app.Plot.Toolbar.Children(4).Value = 'on';
                    matlab.graphics.interaction.webmodes.toggleMode(app.Plot,'zoomout',app.Plot.Toolbar.Children(4).Value)
            end
            
        end

        % Button pushed function: ZoomReset
        function ZoomResetButtonPushed(app, event)
            matlab.graphics.controls.internal.resetHelper(app.Plot,true);
            matlab.graphics.interaction.webmodes.toggleMode(app.Plot,'zoom','off')
        end

        % Button pushed function: Options_AddStdButton
        function Options_AddStdButtonPushed(app, event)
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Pick a standard file';
            
            f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
            [FileName, pathname] = uigetfile('*.txt', 'Pick a standard file');
            delete(f);
            figure(app.ConverterLAICPMS)
            if isequal(FileName,0)
                close(app.WaitBar)
                return
            end
            
            close(app.WaitBar)
            
            FileDir = [pathname,FileName];
            
            AddSingleStandardFile(app,FileDir,FileName)
            
            app.Options_StandarddataDropDown.Items = app.RefData_LAICPMS.FileNames;
        end

        % Callback function
        function ImportButtonPushed(app, event)
            
            
            
            
            
            keyboard
            
        end

        % Value changed function: DebugMode
        function DebugModeValueChanged(app, event)
            value = app.DebugMode.Value;
            
            if isequal(value,1)
                Answer = questdlg('The module becomes very slow when the debug mode is turned on and several files are automatically generated. These files are undocumented. Are you sure you want to continue?','XMapTools','No');
                if ~isequal(Answer,'Yes')
                    app.DebugMode.Value = 0;
                end
            end
            
            
        end

        % Button pushed function: SecondaryStd_PlotSecStd
        function SecondaryStd_PlotSecStdButtonPushed(app, event)
            
            SelectedEl = listdlg('ListString',app.Data.ElName);
            Data = app.Data.SecondaryStandard_ElemConcMatrix(:,SelectedEl);
            
            figure;
            tiledlayout('flow');
            nexttile
            plot(Data,'.','MarkerSize',5)
            
            hold on
            
            MC = mean(Data);
            STD = std(Data);
            
            plot([0 length(Data)],[MC,MC],'-r','LineWidth',2)
            plot([0 length(Data)],[MC+STD,MC+STD],'-k','LineWidth',2)
            plot([0 length(Data)],[MC-STD,MC-STD],'-k','LineWidth',2)
            
            for i = 1:length(Data)
                STEi(i) = std(Data(1:i))/sqrt(i);
            end
            STEi(1) = NaN;
            
            plot(MC+STEi,':r','LineWidth',2)
            plot(MC-STEi,':r','LineWidth',2)
            
            axis([0 length(Data) MC-4*STD,MC+4*STD])
            
            title(['Secondary Standard Composition (',app.Data.ElName{SelectedEl},')'])
            xlabel('Number of readings')
            ylabel('Composition in µg/g [range ± 4std only!]')
            
            nexttile
            yy = smooth(Data,'loess');
            yy2 = smooth(yy);
            yy3 = smooth(yy2);
            yy4 = smooth(yy3);
            
            plot(yy,'b'), hold on
            plot(yy4,'r','linewidth',2)
            plot([0 length(Data)],[MC,MC],'-k','LineWidth',3)
            
            axis([0 length(Data) MC-4*STD,MC+4*STD])
            
            title(['Secondary Standard Composition (',app.Data.ElName{SelectedEl},')'])
            xlabel('Number of readings')
            ylabel('Composition in µg/g [range ± 4std only!]')
            
            %keyboard
            
        end

        % Button pushed function: SecondaryStd_Cancel
        function SecondaryStd_CancelButtonPushed(app, event)
            app.PrimaryStd_CorrectionList.Enable = 'on';
            app.PrimaryStd_List.Enable = 'on';
            app.PrimaryStd_FilterPercentSpinner.Enable = 'on';
            app.PrimaryStd_FilterPercentSpinnerLabel.Enable = 'on';
            app.PrimaryStd_ApplyButton.Enable = 'on';
            
            app.SECONDARYSTANDARDPanel.Visible = 'off';
            
        end

        % Callback function
        function PrimaryStd_CancelButtonPushed(app, event)
            
            app.Tree.SelectedNodes = [];
            
            app.ExtractIntegrationsBackgroundButton.Enable = 'on';
            app.BackgroundFilterPercentSpinnerLabel.Enable = 'on';
            app.BackgroundFilterPercentSpinner.Enable = 'on';
            app.Background_CorrectionList.Enable = 'on';
            app.BackgroundApplyButton.Enable = 'on';
            
            app.PRIMARYSTANDARDPanel.Visible = 'off';
        end

        % Value changed function: SecondaryStd_ListPS
        function SecondaryStd_ListPSValueChanged(app, event)
            if isequal(app.MAPSPanel.Visible,'off')
                SecondaryStd_IntStdValueChanged(app);
            end
        end

        % Value changed function: TypeOfFileDropDown
        function TypeOfFileDropDownValueChanged(app, event)
            value = app.TypeOfFileDropDown.Value;
            if isequal(value,'Multiple-file')
                app.LogfileCheckBox.Value = 0;
            end
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create ConverterLAICPMS and hide until all components are created
            app.ConverterLAICPMS = uifigure('Visible', 'off');
            app.ConverterLAICPMS.Position = [100 100 1258 760];
            app.ConverterLAICPMS.Name = 'Converter For LA-ICPMS Data – XMapTools';
            app.ConverterLAICPMS.CloseRequestFcn = createCallbackFcn(app, @ConverterLAICPMSCloseRequest, true);

            % Create GridLayout
            app.GridLayout = uigridlayout(app.ConverterLAICPMS);
            app.GridLayout.ColumnWidth = {'0.8x', '0.03x', '0.8x', '0.03x', '0.8x', '0.03x', '1.2x'};
            app.GridLayout.RowHeight = {'0.5x', '0.1x', '0.2x', '4x', '0.1x', '1x', '0.1x', '1x'};
            app.GridLayout.ColumnSpacing = 5;
            app.GridLayout.RowSpacing = 5;

            % Create Image
            app.Image = uiimage(app.GridLayout);
            app.Image.Layout.Row = 1;
            app.Image.Layout.Column = 1;
            app.Image.ImageSource = 'logo_xmap_final.png';

            % Create Tree
            app.Tree = uitree(app.GridLayout);
            app.Tree.SelectionChangedFcn = createCallbackFcn(app, @TreeSelectionChanged, true);
            app.Tree.Layout.Row = 4;
            app.Tree.Layout.Column = 1;

            % Create Node_Background
            app.Node_Background = uitreenode(app.Tree);
            app.Node_Background.NodeData = [1 0];
            app.Node_Background.Icon = 'XXX_Background.png';
            app.Node_Background.Text = 'Background';

            % Create Node_PrimaryStandard
            app.Node_PrimaryStandard = uitreenode(app.Tree);
            app.Node_PrimaryStandard.NodeData = [2 0];
            app.Node_PrimaryStandard.Icon = 'XXX_PrimaryStd.png';
            app.Node_PrimaryStandard.Text = 'Primary Standard';

            % Create Node_SecondaryStandard
            app.Node_SecondaryStandard = uitreenode(app.Tree);
            app.Node_SecondaryStandard.NodeData = [3 0];
            app.Node_SecondaryStandard.Icon = 'XXX_SecondaryStd.png';
            app.Node_SecondaryStandard.Text = 'Secondary Standard';

            % Create Node_Scans
            app.Node_Scans = uitreenode(app.Tree);
            app.Node_Scans.NodeData = [4 0];
            app.Node_Scans.Icon = 'XXX_Scans.png';
            app.Node_Scans.Text = 'Scans';

            % Create GridLayout2
            app.GridLayout2 = uigridlayout(app.GridLayout);
            app.GridLayout2.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout2.RowHeight = {'1x'};
            app.GridLayout2.ColumnSpacing = 5;
            app.GridLayout2.RowSpacing = 5;
            app.GridLayout2.Layout.Row = 1;
            app.GridLayout2.Layout.Column = [3 7];

            % Create LoadDatafilesButton
            app.LoadDatafilesButton = uibutton(app.GridLayout2, 'push');
            app.LoadDatafilesButton.ButtonPushedFcn = createCallbackFcn(app, @LoadDatafilesButtonPushed, true);
            app.LoadDatafilesButton.Icon = '323-add.png';
            app.LoadDatafilesButton.FontSize = 13;
            app.LoadDatafilesButton.Layout.Row = 1;
            app.LoadDatafilesButton.Layout.Column = [21 24];
            app.LoadDatafilesButton.Text = ' Load Datafiles';

            % Create TypeOfInstrumentDropDown
            app.TypeOfInstrumentDropDown = uidropdown(app.GridLayout2);
            app.TypeOfInstrumentDropDown.Items = {'Agilent', 'Thermo'};
            app.TypeOfInstrumentDropDown.Layout.Row = 1;
            app.TypeOfInstrumentDropDown.Layout.Column = [6 8];
            app.TypeOfInstrumentDropDown.Value = 'Agilent';

            % Create LogfileCheckBox
            app.LogfileCheckBox = uicheckbox(app.GridLayout2);
            app.LogfileCheckBox.Text = 'Log-file';
            app.LogfileCheckBox.Layout.Row = 1;
            app.LogfileCheckBox.Layout.Column = [14 15];
            app.LogfileCheckBox.Value = true;

            % Create TypeOfFileDropDown
            app.TypeOfFileDropDown = uidropdown(app.GridLayout2);
            app.TypeOfFileDropDown.Items = {'Single-file', 'Multiple-file'};
            app.TypeOfFileDropDown.ValueChangedFcn = createCallbackFcn(app, @TypeOfFileDropDownValueChanged, true);
            app.TypeOfFileDropDown.Layout.Row = 1;
            app.TypeOfFileDropDown.Layout.Column = [9 11];
            app.TypeOfFileDropDown.Value = 'Single-file';

            % Create HelpButton
            app.HelpButton = uibutton(app.GridLayout2, 'push');
            app.HelpButton.ButtonPushedFcn = createCallbackFcn(app, @HelpButtonPushed, true);
            app.HelpButton.Icon = '061-info.png';
            app.HelpButton.Layout.Row = 1;
            app.HelpButton.Layout.Column = 26;
            app.HelpButton.Text = '';

            % Create MassSpectrometerLabel
            app.MassSpectrometerLabel = uilabel(app.GridLayout2);
            app.MassSpectrometerLabel.HorizontalAlignment = 'right';
            app.MassSpectrometerLabel.FontSize = 14;
            app.MassSpectrometerLabel.FontWeight = 'bold';
            app.MassSpectrometerLabel.Layout.Row = 1;
            app.MassSpectrometerLabel.Layout.Column = [1 5];
            app.MassSpectrometerLabel.Text = 'Mass Spectrometer:  ';

            % Create LaserLabel
            app.LaserLabel = uilabel(app.GridLayout2);
            app.LaserLabel.HorizontalAlignment = 'right';
            app.LaserLabel.FontSize = 14;
            app.LaserLabel.FontWeight = 'bold';
            app.LaserLabel.Layout.Row = 1;
            app.LaserLabel.Layout.Column = [12 13];
            app.LaserLabel.Text = 'Laser: ';

            % Create NameFormatDropDown
            app.NameFormatDropDown = uidropdown(app.GridLayout2);
            app.NameFormatDropDown.Items = {'Format 1 (Name - ID)', 'Format 2 (Name_ID)'};
            app.NameFormatDropDown.ItemsData = [1 2];
            app.NameFormatDropDown.FontSize = 10;
            app.NameFormatDropDown.Layout.Row = 1;
            app.NameFormatDropDown.Layout.Column = [16 19];
            app.NameFormatDropDown.Value = 1;

            % Create BACKGROUNDPanel
            app.BACKGROUNDPanel = uipanel(app.GridLayout);
            app.BACKGROUNDPanel.Title = 'BACKGROUND';
            app.BACKGROUNDPanel.Layout.Row = 6;
            app.BACKGROUNDPanel.Layout.Column = 3;

            % Create GridLayout5
            app.GridLayout5 = uigridlayout(app.BACKGROUNDPanel);
            app.GridLayout5.ColumnWidth = {'1x', '1x', '1x', '1x'};

            % Create ExtractIntegrationsBackgroundButton
            app.ExtractIntegrationsBackgroundButton = uibutton(app.GridLayout5, 'push');
            app.ExtractIntegrationsBackgroundButton.ButtonPushedFcn = createCallbackFcn(app, @ExtractIntegrationsBackgroundButtonPushed, true);
            app.ExtractIntegrationsBackgroundButton.Layout.Row = 1;
            app.ExtractIntegrationsBackgroundButton.Layout.Column = [1 2];
            app.ExtractIntegrationsBackgroundButton.Text = 'Extract Integrations';

            % Create BackgroundFilterPercentSpinnerLabel
            app.BackgroundFilterPercentSpinnerLabel = uilabel(app.GridLayout5);
            app.BackgroundFilterPercentSpinnerLabel.HorizontalAlignment = 'right';
            app.BackgroundFilterPercentSpinnerLabel.FontSize = 10;
            app.BackgroundFilterPercentSpinnerLabel.Layout.Row = 1;
            app.BackgroundFilterPercentSpinnerLabel.Layout.Column = 3;
            app.BackgroundFilterPercentSpinnerLabel.Text = 'Filter (%)';

            % Create BackgroundFilterPercentSpinner
            app.BackgroundFilterPercentSpinner = uispinner(app.GridLayout5);
            app.BackgroundFilterPercentSpinner.Limits = [0 49];
            app.BackgroundFilterPercentSpinner.ValueChangedFcn = createCallbackFcn(app, @BackgroundFilterPercentSpinnerValueChanged, true);
            app.BackgroundFilterPercentSpinner.FontSize = 10;
            app.BackgroundFilterPercentSpinner.Layout.Row = 1;
            app.BackgroundFilterPercentSpinner.Layout.Column = 4;
            app.BackgroundFilterPercentSpinner.Value = 10;

            % Create Background_CorrectionList
            app.Background_CorrectionList = uidropdown(app.GridLayout5);
            app.Background_CorrectionList.Items = {'None', 'Linear', 'Polynomial', 'Step function', 'Spline'};
            app.Background_CorrectionList.ItemsData = [1 2 3 4 5];
            app.Background_CorrectionList.ValueChangedFcn = createCallbackFcn(app, @Background_CorrectionListValueChanged, true);
            app.Background_CorrectionList.Layout.Row = 2;
            app.Background_CorrectionList.Layout.Column = [1 3];
            app.Background_CorrectionList.Value = 1;

            % Create BackgroundApplyButton
            app.BackgroundApplyButton = uibutton(app.GridLayout5, 'push');
            app.BackgroundApplyButton.ButtonPushedFcn = createCallbackFcn(app, @BackgroundApplyButtonPushed, true);
            app.BackgroundApplyButton.Layout.Row = 2;
            app.BackgroundApplyButton.Layout.Column = 4;
            app.BackgroundApplyButton.Text = 'Apply';

            % Create PRIMARYSTANDARDPanel
            app.PRIMARYSTANDARDPanel = uipanel(app.GridLayout);
            app.PRIMARYSTANDARDPanel.Title = 'PRIMARY STANDARD';
            app.PRIMARYSTANDARDPanel.Layout.Row = 6;
            app.PRIMARYSTANDARDPanel.Layout.Column = 5;

            % Create GridLayout6
            app.GridLayout6 = uigridlayout(app.PRIMARYSTANDARDPanel);
            app.GridLayout6.ColumnWidth = {'0.6x', '1x', '0.4x', '0.4x', '0.4x', '0.4x'};

            % Create PrimaryStd_FilterPercentSpinnerLabel
            app.PrimaryStd_FilterPercentSpinnerLabel = uilabel(app.GridLayout6);
            app.PrimaryStd_FilterPercentSpinnerLabel.HorizontalAlignment = 'right';
            app.PrimaryStd_FilterPercentSpinnerLabel.FontSize = 10;
            app.PrimaryStd_FilterPercentSpinnerLabel.Layout.Row = 1;
            app.PrimaryStd_FilterPercentSpinnerLabel.Layout.Column = [3 4];
            app.PrimaryStd_FilterPercentSpinnerLabel.Text = 'Filter (%)';

            % Create PrimaryStd_FilterPercentSpinner
            app.PrimaryStd_FilterPercentSpinner = uispinner(app.GridLayout6);
            app.PrimaryStd_FilterPercentSpinner.Limits = [0 49];
            app.PrimaryStd_FilterPercentSpinner.ValueChangedFcn = createCallbackFcn(app, @PrimaryStd_FilterPercentSpinnerValueChanged, true);
            app.PrimaryStd_FilterPercentSpinner.FontSize = 10;
            app.PrimaryStd_FilterPercentSpinner.Layout.Row = 1;
            app.PrimaryStd_FilterPercentSpinner.Layout.Column = [5 6];
            app.PrimaryStd_FilterPercentSpinner.Value = 10;

            % Create PrimaryStd_List
            app.PrimaryStd_List = uidropdown(app.GridLayout6);
            app.PrimaryStd_List.Items = {'None', ''};
            app.PrimaryStd_List.ItemsData = 1;
            app.PrimaryStd_List.ValueChangedFcn = createCallbackFcn(app, @PrimaryStd_ListValueChanged, true);
            app.PrimaryStd_List.Layout.Row = 1;
            app.PrimaryStd_List.Layout.Column = [1 2];
            app.PrimaryStd_List.Value = 1;

            % Create PrimaryStd_ApplyButton
            app.PrimaryStd_ApplyButton = uibutton(app.GridLayout6, 'push');
            app.PrimaryStd_ApplyButton.ButtonPushedFcn = createCallbackFcn(app, @PrimaryStd_ApplyButtonPushed, true);
            app.PrimaryStd_ApplyButton.Layout.Row = 2;
            app.PrimaryStd_ApplyButton.Layout.Column = [5 6];
            app.PrimaryStd_ApplyButton.Text = 'Apply';

            % Create PrimaryStd_CorrectionList
            app.PrimaryStd_CorrectionList = uidropdown(app.GridLayout6);
            app.PrimaryStd_CorrectionList.Items = {'None', 'Linear', 'Polynomial', 'Step function', 'Spline'};
            app.PrimaryStd_CorrectionList.ItemsData = [1 2 3 4 5];
            app.PrimaryStd_CorrectionList.ValueChangedFcn = createCallbackFcn(app, @PrimaryStd_CorrectionListValueChanged, true);
            app.PrimaryStd_CorrectionList.Layout.Row = 2;
            app.PrimaryStd_CorrectionList.Layout.Column = [1 4];
            app.PrimaryStd_CorrectionList.Value = 1;

            % Create SECONDARYSTANDARDPanel
            app.SECONDARYSTANDARDPanel = uipanel(app.GridLayout);
            app.SECONDARYSTANDARDPanel.Title = 'SECONDARY STANDARD';
            app.SECONDARYSTANDARDPanel.Layout.Row = [6 8];
            app.SECONDARYSTANDARDPanel.Layout.Column = 7;

            % Create GridLayout7
            app.GridLayout7 = uigridlayout(app.SECONDARYSTANDARDPanel);
            app.GridLayout7.ColumnWidth = {'1x', '0.4x', '0.4x', '0.4x', '0.4x', '0.4x', '0.4x', '0.4x', '0.4x', '0.4x'};
            app.GridLayout7.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout7.ColumnSpacing = 5;
            app.GridLayout7.RowSpacing = 7;

            % Create SecondaryStd_List
            app.SecondaryStd_List = uidropdown(app.GridLayout7);
            app.SecondaryStd_List.Items = {'None'};
            app.SecondaryStd_List.ItemsData = 1;
            app.SecondaryStd_List.ValueChangedFcn = createCallbackFcn(app, @SecondaryStd_ListValueChanged, true);
            app.SecondaryStd_List.Layout.Row = 1;
            app.SecondaryStd_List.Layout.Column = [3 5];
            app.SecondaryStd_List.Value = 1;

            % Create SecondaryStd_FilterPercentSpinnerLabel
            app.SecondaryStd_FilterPercentSpinnerLabel = uilabel(app.GridLayout7);
            app.SecondaryStd_FilterPercentSpinnerLabel.HorizontalAlignment = 'right';
            app.SecondaryStd_FilterPercentSpinnerLabel.FontSize = 10;
            app.SecondaryStd_FilterPercentSpinnerLabel.Layout.Row = 1;
            app.SecondaryStd_FilterPercentSpinnerLabel.Layout.Column = [6 7];
            app.SecondaryStd_FilterPercentSpinnerLabel.Text = 'Filter (%)';

            % Create SecondaryStd_FilterPercentSpinner
            app.SecondaryStd_FilterPercentSpinner = uispinner(app.GridLayout7);
            app.SecondaryStd_FilterPercentSpinner.Limits = [0 49];
            app.SecondaryStd_FilterPercentSpinner.ValueChangedFcn = createCallbackFcn(app, @SecondaryStd_FilterPercentSpinnerValueChanged, true);
            app.SecondaryStd_FilterPercentSpinner.FontSize = 10;
            app.SecondaryStd_FilterPercentSpinner.Layout.Row = 1;
            app.SecondaryStd_FilterPercentSpinner.Layout.Column = [8 9];
            app.SecondaryStd_FilterPercentSpinner.Value = 10;

            % Create SecondaryStd_UITable
            app.SecondaryStd_UITable = uitable(app.GridLayout7);
            app.SecondaryStd_UITable.ColumnName = {'El'; 'Ref (ppm)'; 'Calc (ppm)'; 'Delta (%)'};
            app.SecondaryStd_UITable.RowName = {};
            app.SecondaryStd_UITable.Layout.Row = [2 6];
            app.SecondaryStd_UITable.Layout.Column = [2 10];
            app.SecondaryStd_UITable.FontSize = 9;

            % Create SecondaryStd_IntStdLabel
            app.SecondaryStd_IntStdLabel = uilabel(app.GridLayout7);
            app.SecondaryStd_IntStdLabel.HorizontalAlignment = 'center';
            app.SecondaryStd_IntStdLabel.VerticalAlignment = 'bottom';
            app.SecondaryStd_IntStdLabel.Layout.Row = 4;
            app.SecondaryStd_IntStdLabel.Layout.Column = 1;
            app.SecondaryStd_IntStdLabel.Text = 'Int. Std:';

            % Create SecondaryStd_IntStd
            app.SecondaryStd_IntStd = uidropdown(app.GridLayout7);
            app.SecondaryStd_IntStd.ValueChangedFcn = createCallbackFcn(app, @SecondaryStd_IntStdValueChanged, true);
            app.SecondaryStd_IntStd.Layout.Row = 5;
            app.SecondaryStd_IntStd.Layout.Column = 1;

            % Create SecondaryStd_ApplyButton
            app.SecondaryStd_ApplyButton = uibutton(app.GridLayout7, 'push');
            app.SecondaryStd_ApplyButton.ButtonPushedFcn = createCallbackFcn(app, @SecondaryStd_ApplyButtonPushed, true);
            app.SecondaryStd_ApplyButton.Layout.Row = 6;
            app.SecondaryStd_ApplyButton.Layout.Column = 1;
            app.SecondaryStd_ApplyButton.Text = 'Apply';

            % Create SecondaryStd_CalibrationMtx
            app.SecondaryStd_CalibrationMtx = uibutton(app.GridLayout7, 'push');
            app.SecondaryStd_CalibrationMtx.ButtonPushedFcn = createCallbackFcn(app, @SecondaryStd_CalibrationMtxButtonPushed, true);
            app.SecondaryStd_CalibrationMtx.FontSize = 11;
            app.SecondaryStd_CalibrationMtx.Layout.Row = 2;
            app.SecondaryStd_CalibrationMtx.Layout.Column = 1;
            app.SecondaryStd_CalibrationMtx.Text = 'Cal. Matrix';

            % Create SecondaryStd_PlotSecStd
            app.SecondaryStd_PlotSecStd = uibutton(app.GridLayout7, 'push');
            app.SecondaryStd_PlotSecStd.ButtonPushedFcn = createCallbackFcn(app, @SecondaryStd_PlotSecStdButtonPushed, true);
            app.SecondaryStd_PlotSecStd.FontSize = 11;
            app.SecondaryStd_PlotSecStd.Layout.Row = 3;
            app.SecondaryStd_PlotSecStd.Layout.Column = 1;
            app.SecondaryStd_PlotSecStd.Text = 'Plot Std';

            % Create SecondaryStd_Cancel
            app.SecondaryStd_Cancel = uibutton(app.GridLayout7, 'push');
            app.SecondaryStd_Cancel.ButtonPushedFcn = createCallbackFcn(app, @SecondaryStd_CancelButtonPushed, true);
            app.SecondaryStd_Cancel.Icon = 'XXX_closeMini.png';
            app.SecondaryStd_Cancel.IconAlignment = 'center';
            app.SecondaryStd_Cancel.FontSize = 11;
            app.SecondaryStd_Cancel.Tooltip = {'Cancel'};
            app.SecondaryStd_Cancel.Layout.Row = 1;
            app.SecondaryStd_Cancel.Layout.Column = 10;
            app.SecondaryStd_Cancel.Text = '';

            % Create SecondaryStd_ListPS
            app.SecondaryStd_ListPS = uidropdown(app.GridLayout7);
            app.SecondaryStd_ListPS.Items = {'None'};
            app.SecondaryStd_ListPS.ItemsData = 1;
            app.SecondaryStd_ListPS.ValueChangedFcn = createCallbackFcn(app, @SecondaryStd_ListPSValueChanged, true);
            app.SecondaryStd_ListPS.Layout.Row = 1;
            app.SecondaryStd_ListPS.Layout.Column = [1 2];
            app.SecondaryStd_ListPS.Value = 1;

            % Create MAPSPanel
            app.MAPSPanel = uipanel(app.GridLayout);
            app.MAPSPanel.Title = 'MAPS (SCANS)';
            app.MAPSPanel.Layout.Row = 8;
            app.MAPSPanel.Layout.Column = [3 5];

            % Create GridLayout8
            app.GridLayout8 = uigridlayout(app.MAPSPanel);
            app.GridLayout8.ColumnWidth = {'1x', '1x', '1x', '0.1x', '1x', '1x', '1x', '1x'};

            % Create Maps_ApplyButton
            app.Maps_ApplyButton = uibutton(app.GridLayout8, 'push');
            app.Maps_ApplyButton.ButtonPushedFcn = createCallbackFcn(app, @Maps_ApplyButtonPushed, true);
            app.Maps_ApplyButton.Layout.Row = 2;
            app.Maps_ApplyButton.Layout.Column = 3;
            app.Maps_ApplyButton.Text = 'Apply';

            % Create Maps_TimeshiftSpinnerLabel
            app.Maps_TimeshiftSpinnerLabel = uilabel(app.GridLayout8);
            app.Maps_TimeshiftSpinnerLabel.HorizontalAlignment = 'right';
            app.Maps_TimeshiftSpinnerLabel.FontSize = 9;
            app.Maps_TimeshiftSpinnerLabel.Layout.Row = 2;
            app.Maps_TimeshiftSpinnerLabel.Layout.Column = 1;
            app.Maps_TimeshiftSpinnerLabel.Text = 'Cycle shift';

            % Create Maps_TimeshiftSpinner
            app.Maps_TimeshiftSpinner = uispinner(app.GridLayout8);
            app.Maps_TimeshiftSpinner.Limits = [0 Inf];
            app.Maps_TimeshiftSpinner.RoundFractionalValues = 'on';
            app.Maps_TimeshiftSpinner.ValueDisplayFormat = '%.0f';
            app.Maps_TimeshiftSpinner.ValueChangedFcn = createCallbackFcn(app, @Maps_TimeshiftSpinnerValueChanged, true);
            app.Maps_TimeshiftSpinner.FontSize = 9;
            app.Maps_TimeshiftSpinner.Layout.Row = 2;
            app.Maps_TimeshiftSpinner.Layout.Column = 2;

            % Create Maps_List
            app.Maps_List = uidropdown(app.GridLayout8);
            app.Maps_List.Items = {'None'};
            app.Maps_List.ItemsData = 1;
            app.Maps_List.ValueChangedFcn = createCallbackFcn(app, @Maps_ListValueChanged, true);
            app.Maps_List.Layout.Row = 1;
            app.Maps_List.Layout.Column = [1 2];
            app.Maps_List.Value = 1;

            % Create CheckButton
            app.CheckButton = uibutton(app.GridLayout8, 'push');
            app.CheckButton.ButtonPushedFcn = createCallbackFcn(app, @CheckButtonPushed, true);
            app.CheckButton.Icon = '091-share.png';
            app.CheckButton.IconAlignment = 'top';
            app.CheckButton.Layout.Row = [1 2];
            app.CheckButton.Layout.Column = 5;
            app.CheckButton.Text = 'Check';

            % Create ShowButton
            app.ShowButton = uibutton(app.GridLayout8, 'push');
            app.ShowButton.ButtonPushedFcn = createCallbackFcn(app, @ShowButtonPushed, true);
            app.ShowButton.Icon = '004-picture.png';
            app.ShowButton.IconAlignment = 'top';
            app.ShowButton.Layout.Row = [1 2];
            app.ShowButton.Layout.Column = 7;
            app.ShowButton.Text = 'Show';

            % Create ExportButton
            app.ExportButton = uibutton(app.GridLayout8, 'push');
            app.ExportButton.ButtonPushedFcn = createCallbackFcn(app, @ExportButtonPushed, true);
            app.ExportButton.Icon = '311-app.png';
            app.ExportButton.IconAlignment = 'top';
            app.ExportButton.Layout.Row = [1 2];
            app.ExportButton.Layout.Column = 8;
            app.ExportButton.Text = 'Export';

            % Create Maps_SelElement
            app.Maps_SelElement = uidropdown(app.GridLayout8);
            app.Maps_SelElement.FontSize = 10;
            app.Maps_SelElement.Layout.Row = 1;
            app.Maps_SelElement.Layout.Column = 6;

            % Create GridLayout9
            app.GridLayout9 = uigridlayout(app.GridLayout);
            app.GridLayout9.ColumnWidth = {'1x', '1x', '1x'};
            app.GridLayout9.RowHeight = {'1x', '1x', '1x'};
            app.GridLayout9.ColumnSpacing = 8;
            app.GridLayout9.Padding = [10 18 10 1];
            app.GridLayout9.Layout.Row = 6;
            app.GridLayout9.Layout.Column = 1;

            % Create SweepLabel
            app.SweepLabel = uilabel(app.GridLayout9);
            app.SweepLabel.HorizontalAlignment = 'right';
            app.SweepLabel.Layout.Row = 1;
            app.SweepLabel.Layout.Column = 1;
            app.SweepLabel.Text = 'Sweep';

            % Create Position_Min
            app.Position_Min = uispinner(app.GridLayout9);
            app.Position_Min.ValueChangedFcn = createCallbackFcn(app, @Background_MinValueChanged, true);
            app.Position_Min.Layout.Row = 1;
            app.Position_Min.Layout.Column = 2;

            % Create Position_Max
            app.Position_Max = uispinner(app.GridLayout9);
            app.Position_Max.ValueChangedFcn = createCallbackFcn(app, @Background_MinValueChanged, true);
            app.Position_Max.Layout.Row = 1;
            app.Position_Max.Layout.Column = 3;

            % Create TimeshiftSpinner
            app.TimeshiftSpinner = uispinner(app.GridLayout9);
            app.TimeshiftSpinner.Step = 0.1;
            app.TimeshiftSpinner.ValueDisplayFormat = '%.1f';
            app.TimeshiftSpinner.ValueChangedFcn = createCallbackFcn(app, @TimeshiftSpinnerValueChanged, true);
            app.TimeshiftSpinner.Layout.Row = 2;
            app.TimeshiftSpinner.Layout.Column = 3;

            % Create TimeshiftLabel
            app.TimeshiftLabel = uilabel(app.GridLayout9);
            app.TimeshiftLabel.HorizontalAlignment = 'right';
            app.TimeshiftLabel.Layout.Row = 2;
            app.TimeshiftLabel.Layout.Column = 2;
            app.TimeshiftLabel.Text = 'Time shift';

            % Create IntegrationMenuLabel
            app.IntegrationMenuLabel = uilabel(app.GridLayout);
            app.IntegrationMenuLabel.VerticalAlignment = 'bottom';
            app.IntegrationMenuLabel.FontSize = 11;
            app.IntegrationMenuLabel.FontAngle = 'italic';
            app.IntegrationMenuLabel.Layout.Row = 3;
            app.IntegrationMenuLabel.Layout.Column = 1;
            app.IntegrationMenuLabel.Text = 'Integration Menu';

            % Create GridLayout10
            app.GridLayout10 = uigridlayout(app.GridLayout);
            app.GridLayout10.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout10.RowHeight = {'1x'};
            app.GridLayout10.ColumnSpacing = 5;
            app.GridLayout10.RowSpacing = 5;
            app.GridLayout10.Padding = [2 4 2 6];
            app.GridLayout10.Layout.Row = [2 3];
            app.GridLayout10.Layout.Column = [3 7];

            % Create ZoomIn
            app.ZoomIn = uibutton(app.GridLayout10, 'push');
            app.ZoomIn.ButtonPushedFcn = createCallbackFcn(app, @ZoomInButtonPushed, true);
            app.ZoomIn.Icon = 'XXX_zoom_in.png';
            app.ZoomIn.Layout.Row = 1;
            app.ZoomIn.Layout.Column = 15;
            app.ZoomIn.Text = '';

            % Create ZoomOut
            app.ZoomOut = uibutton(app.GridLayout10, 'push');
            app.ZoomOut.ButtonPushedFcn = createCallbackFcn(app, @ZoomOutButtonPushed, true);
            app.ZoomOut.Icon = 'XXX_zoom_out.png';
            app.ZoomOut.Layout.Row = 1;
            app.ZoomOut.Layout.Column = 16;
            app.ZoomOut.Text = '';

            % Create ZoomReset
            app.ZoomReset = uibutton(app.GridLayout10, 'push');
            app.ZoomReset.ButtonPushedFcn = createCallbackFcn(app, @ZoomResetButtonPushed, true);
            app.ZoomReset.Icon = 'XXX_home.png';
            app.ZoomReset.Layout.Row = 1;
            app.ZoomReset.Layout.Column = 17;
            app.ZoomReset.Text = '';

            % Create PlotMenuDropDown
            app.PlotMenuDropDown = uidropdown(app.GridLayout10);
            app.PlotMenuDropDown.ValueChangedFcn = createCallbackFcn(app, @PlotMenuDropDownValueChanged, true);
            app.PlotMenuDropDown.Layout.Row = 1;
            app.PlotMenuDropDown.Layout.Column = [27 31];

            % Create PlotMenuDropDownLabel
            app.PlotMenuDropDownLabel = uilabel(app.GridLayout10);
            app.PlotMenuDropDownLabel.HorizontalAlignment = 'right';
            app.PlotMenuDropDownLabel.Layout.Row = 1;
            app.PlotMenuDropDownLabel.Layout.Column = [24 26];
            app.PlotMenuDropDownLabel.Text = 'Plot Menu';

            % Create GridLayout11
            app.GridLayout11 = uigridlayout(app.GridLayout);
            app.GridLayout11.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout11.RowHeight = {'1x', '1x', '1x'};
            app.GridLayout11.Layout.Row = 8;
            app.GridLayout11.Layout.Column = 1;

            % Create StdfilesLabel
            app.StdfilesLabel = uilabel(app.GridLayout11);
            app.StdfilesLabel.HorizontalAlignment = 'right';
            app.StdfilesLabel.FontSize = 10;
            app.StdfilesLabel.Layout.Row = 2;
            app.StdfilesLabel.Layout.Column = [1 2];
            app.StdfilesLabel.Text = 'Std files';

            % Create Options_AddStdButton
            app.Options_AddStdButton = uibutton(app.GridLayout11, 'push');
            app.Options_AddStdButton.ButtonPushedFcn = createCallbackFcn(app, @Options_AddStdButtonPushed, true);
            app.Options_AddStdButton.Layout.Row = 2;
            app.Options_AddStdButton.Layout.Column = [7 8];
            app.Options_AddStdButton.Text = 'Add';

            % Create Options_StandarddataDropDown
            app.Options_StandarddataDropDown = uidropdown(app.GridLayout11);
            app.Options_StandarddataDropDown.FontSize = 10;
            app.Options_StandarddataDropDown.Layout.Row = 2;
            app.Options_StandarddataDropDown.Layout.Column = [3 6];

            % Create OptionsLabel
            app.OptionsLabel = uilabel(app.GridLayout11);
            app.OptionsLabel.HorizontalAlignment = 'center';
            app.OptionsLabel.FontWeight = 'bold';
            app.OptionsLabel.Layout.Row = 1;
            app.OptionsLabel.Layout.Column = [1 8];
            app.OptionsLabel.Text = 'Options';

            % Create DebugMode
            app.DebugMode = uicheckbox(app.GridLayout11);
            app.DebugMode.ValueChangedFcn = createCallbackFcn(app, @DebugModeValueChanged, true);
            app.DebugMode.Text = 'Debug mode';
            app.DebugMode.FontSize = 10;
            app.DebugMode.Layout.Row = 3;
            app.DebugMode.Layout.Column = [1 4];

            % Create TestMode
            app.TestMode = uicheckbox(app.GridLayout11);
            app.TestMode.Text = 'Test mode';
            app.TestMode.FontSize = 10;
            app.TestMode.Layout.Row = 3;
            app.TestMode.Layout.Column = [5 8];

            % Create Plot
            app.Plot = uiaxes(app.GridLayout);
            xlabel(app.Plot, 'Time')
            ylabel(app.Plot, 'Intensity')
            app.Plot.PlotBoxAspectRatio = [2.47156726768377 1 1];
            app.Plot.FontSize = 10;
            app.Plot.Layout.Row = 4;
            app.Plot.Layout.Column = [3 7];

            % Create ContextMenu
            app.ContextMenu = uicontextmenu(app.ConverterLAICPMS);
            
            % Assign app.ContextMenu
            app.SecondaryStd_UITable.ContextMenu = app.ContextMenu;

            % Create CopyMenu
            app.CopyMenu = uimenu(app.ContextMenu);
            app.CopyMenu.MenuSelectedFcn = createCallbackFcn(app, @CopyMenuSelected, true);
            app.CopyMenu.Text = 'Copy';

            % Show the figure after all components are created
            app.ConverterLAICPMS.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = Converter_LAICPMS_exported(varargin)

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.ConverterLAICPMS)

            % Execute the startup function
            runStartupFcn(app, @(app)startupFcn(app, varargin{:}))

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.ConverterLAICPMS)
        end
    end
end