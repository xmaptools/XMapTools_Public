classdef BingoAntidote_exported < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        BingoAntidote_GUI               matlab.ui.Figure
        Toolbar                         matlab.ui.container.Toolbar
        PushOpen                        matlab.ui.container.toolbar.PushTool
        PushSave                        matlab.ui.container.toolbar.PushTool
        PushDeleteSystemFiles           matlab.ui.container.toolbar.PushTool
        GridLayout                      matlab.ui.container.GridLayout
        TabGroup                        matlab.ui.container.TabGroup
        SettingsTab                     matlab.ui.container.Tab
        GridLayout2                     matlab.ui.container.GridLayout
        ChemicalSystemPanel             matlab.ui.container.Panel
        GridLayout4                     matlab.ui.container.GridLayout
        El14Button                      matlab.ui.control.StateButton
        El13Button                      matlab.ui.control.StateButton
        El12Button                      matlab.ui.control.StateButton
        El11Button                      matlab.ui.control.StateButton
        El10Button                      matlab.ui.control.StateButton
        El9Button                       matlab.ui.control.StateButton
        El8Button                       matlab.ui.control.StateButton
        El7Button                       matlab.ui.control.StateButton
        El6Button                       matlab.ui.control.StateButton
        El5Button                       matlab.ui.control.StateButton
        El4Button                       matlab.ui.control.StateButton
        El3Button                       matlab.ui.control.StateButton
        El2Button                       matlab.ui.control.StateButton
        El1Button                       matlab.ui.control.StateButton
        NbSystComponentsLabel           matlab.ui.control.Label
        ApplyStep1Button                matlab.ui.control.Button
        PressureTemperatureRangePanel   matlab.ui.container.Panel
        GridLayout5                     matlab.ui.container.GridLayout
        TminEditField                   matlab.ui.control.NumericEditField
        TminEditFieldLabel              matlab.ui.control.Label
        TmaxEditField                   matlab.ui.control.NumericEditField
        TmaxEditFieldLabel              matlab.ui.control.Label
        TstepEditField                  matlab.ui.control.NumericEditField
        TstepEditFieldLabel             matlab.ui.control.Label
        PminEditField                   matlab.ui.control.NumericEditField
        PminEditFieldLabel              matlab.ui.control.Label
        PmaxEditField                   matlab.ui.control.NumericEditField
        PmaxEditFieldLabel              matlab.ui.control.Label
        PstepEditField                  matlab.ui.control.NumericEditField
        PstepEditFieldLabel             matlab.ui.control.Label
        TNbSteps                        matlab.ui.control.NumericEditField
        PNbSteps                        matlab.ui.control.NumericEditField
        ApplyStep3Button                matlab.ui.control.Button
        DefaultoptionsDropDownLabel     matlab.ui.control.Label
        DefaultoptionsDropDown          matlab.ui.control.DropDown
        ThermodynamicDatabasePanel      matlab.ui.container.Panel
        GridLayout5_2                   matlab.ui.container.GridLayout
        DatabaseListBoxLabel            matlab.ui.control.Label
        DatabaseListBox                 matlab.ui.control.ListBox
        ApplyStep2Button                matlab.ui.control.Button
        ForceDBDownloadButton           matlab.ui.control.Button
        InfoDBButton                    matlab.ui.control.Button
        Settings_NextButton             matlab.ui.control.Button
        InitialPressureTemperatureGuestimatePanel  matlab.ui.container.Panel
        GridLayout5_4                   matlab.ui.container.GridLayout
        TemperatureEditFieldLabel       matlab.ui.control.Label
        TemperatureEditField            matlab.ui.control.NumericEditField
        PressureEditFieldLabel          matlab.ui.control.Label
        PressureEditField               matlab.ui.control.NumericEditField
        LBCTab                          matlab.ui.container.Tab
        GridLayout6                     matlab.ui.container.GridLayout
        AddManageROIPanel               matlab.ui.container.Panel
        GridLayout5_3                   matlab.ui.container.GridLayout
        ROITree                         matlab.ui.container.Tree
        UITable                         matlab.ui.control.Table
        ButtonAddROI                    matlab.ui.control.Button
        ButtonEliminateROI              matlab.ui.control.Button
        MethodROIDropDown               matlab.ui.control.DropDown
        LBCOptionsFluidMeltandOxygenFugacityPanel  matlab.ui.container.Panel
        GridLayout5_9                   matlab.ui.container.GridLayout
        Tree_FluidModels                matlab.ui.container.Tree
        ActivateFluidSpecie_CheckBox    matlab.ui.control.CheckBox
        OptimizeNFluid_CheckBox         matlab.ui.control.CheckBox
        ActivateMeltModel_CheckBox      matlab.ui.control.CheckBox
        MeltModelPhase_DropDown         matlab.ui.control.DropDown
        IncudeMeltEvaluation_CheckBox   matlab.ui.control.CheckBox
        MinNfluid_EditField             matlab.ui.control.NumericEditField
        MinEditFieldLabel               matlab.ui.control.Label
        MaxNfluid_EditField             matlab.ui.control.NumericEditField
        MaxEditFieldLabel               matlab.ui.control.Label
        BulkNfluid_EditField            matlab.ui.control.NumericEditField
        BulkNEditFieldLabel             matlab.ui.control.Label
        ActivateExtraOxygen_CheckBox    matlab.ui.control.CheckBox
        BulkO_EditField                 matlab.ui.control.NumericEditField
        BulkOEditFieldLabel             matlab.ui.control.Label
        OptimizeO_CheckBox              matlab.ui.control.CheckBox
        OxMin_EditField                 matlab.ui.control.NumericEditField
        MinEditField_2Label             matlab.ui.control.Label
        OxMax_EditField                 matlab.ui.control.NumericEditField
        MaxEditField_2Label             matlab.ui.control.Label
        ActivateOxygenBuffer_CheckBox   matlab.ui.control.CheckBox
        Buffer_DropDown                 matlab.ui.control.DropDown
        LBC_NextButton                  matlab.ui.control.Button
        LBCEditField                    matlab.ui.control.EditField
        LBCEditFieldLabel               matlab.ui.control.Label
        PhasesTab                       matlab.ui.container.Tab
        GridLayout7                     matlab.ui.container.GridLayout
        AddManagePhasesSolutionsPanel   matlab.ui.container.Panel
        GridLayout5_10                  matlab.ui.container.GridLayout
        Tree_PhasesModel                matlab.ui.container.Tree
        SelectforLBCCheckBox            matlab.ui.control.CheckBox
        SelectforBACheckBox             matlab.ui.control.CheckBox
        UITable_PhasesModel             matlab.ui.control.Table
        ModelDropDownLabel              matlab.ui.control.Label
        ModelPhaseDropDown              matlab.ui.control.DropDown
        ButtonAddROI_Phase              matlab.ui.control.Button
        ButtonEliminateROI_Phase        matlab.ui.control.Button
        MethodROIDropDown_Phase         matlab.ui.control.DropDown
        TreeMinGroups                   matlab.ui.container.Tree
        Phase_NextButton                matlab.ui.control.Button
        BingoTab                        matlab.ui.container.Tab
        GridLayout8                     matlab.ui.container.GridLayout
        SinglePTXminimizationBingoPanel  matlab.ui.container.Panel
        GridLayout5_6                   matlab.ui.container.GridLayout
        TemperatureCLabel               matlab.ui.control.Label
        BingoTemperatureEditField       matlab.ui.control.NumericEditField
        PressureGPaLabel                matlab.ui.control.Label
        BingoPressureEditField          matlab.ui.control.NumericEditField
        BingoButton                     matlab.ui.control.Button
        ButtonSaveResultBingo           matlab.ui.control.Button
        Text_Report                     matlab.ui.control.TextArea
        AntidoteTab                     matlab.ui.container.Tab
        GridLayout9                     matlab.ui.container.GridLayout
        InversionoptimizationAntidotePanel  matlab.ui.container.Panel
        GridLayout5_7                   matlab.ui.container.GridLayout
        AntidoteTree                    matlab.ui.container.Tree
        GlobalinversionNode             matlab.ui.container.TreeNode
        Recipe1_Node                    matlab.ui.container.TreeNode
        Recipe2_Node                    matlab.ui.container.TreeNode
        Recipe3_Node                    matlab.ui.container.TreeNode
        SinglephasethermobarometryNode  matlab.ui.container.TreeNode
        Recipe4_Node                    matlab.ui.container.TreeNode
        Recipe5_Node                    matlab.ui.container.TreeNode
        Recipe6_Node                    matlab.ui.container.TreeNode
        SensitivitytestsNode            matlab.ui.container.TreeNode
        Recipe7_Node                    matlab.ui.container.TreeNode
        Recipe8_Node                    matlab.ui.container.TreeNode
        Recipe9_Node                    matlab.ui.container.TreeNode
        TexturalinvestigationNode       matlab.ui.container.TreeNode
        Recipe10_Node                   matlab.ui.container.TreeNode
        Recipe11_Node                   matlab.ui.container.TreeNode
        Recipe12_Node                   matlab.ui.container.TreeNode
        Recipe13_Node                   matlab.ui.container.TreeNode
        OptimizationofcompositionalandactivityvariablesNode  matlab.ui.container.TreeNode
        Recipe14_Node                   matlab.ui.container.TreeNode
        Recipe15_Node                   matlab.ui.container.TreeNode
        Recipe16_Node                   matlab.ui.container.TreeNode
        AntidoteButton                  matlab.ui.control.Button
        TabAntidote                     matlab.ui.container.TabGroup
        InversionTab                    matlab.ui.container.Tab
        GridLayout12                    matlab.ui.container.GridLayout
        AntidoteExploratoryscanningCheckBox  matlab.ui.control.CheckBox
        GridsizepxEditFieldLabel        matlab.ui.control.Label
        AntidoteGridresolutionEditField  matlab.ui.control.NumericEditField
        SelectedPhaseOptiDropDown       matlab.ui.control.DropDown
        SelectedPhaseLabel              matlab.ui.control.Label
        MCTab                           matlab.ui.container.Tab
        GridLayout14                    matlab.ui.container.GridLayout
        ToleranceLabel                  matlab.ui.control.Label
        AntidoteToleranceEditField      matlab.ui.control.NumericEditField
        Stage1Label                     matlab.ui.control.Label
        AntidoteNbPerm1EditField        matlab.ui.control.NumericEditField
        Stage2Label                     matlab.ui.control.Label
        AntidoteNbPerm2EditField        matlab.ui.control.NumericEditField
        Positionpx1sLabel               matlab.ui.control.Label
        AntidoteNbPerm1EditField_2      matlab.ui.control.NumericEditField
        TempC1sLabel                    matlab.ui.control.Label
        AntidoteNbPerm1EditField_3      matlab.ui.control.NumericEditField
        NumberofsimulationsLabel        matlab.ui.control.Label
        UncertaintiesLabel              matlab.ui.control.Label
        PressGPa1sLabel                 matlab.ui.control.Label
        AntidoteNbPerm1EditField_4      matlab.ui.control.NumericEditField
        ROITab                          matlab.ui.container.Tab
        GridLayout15                    matlab.ui.container.GridLayout
        TextureROITree                  matlab.ui.container.Tree
        ButtonROI_Add                   matlab.ui.control.Button
        ButtonROI_Eliminate             matlab.ui.control.Button
        ButtonROI_Play                  matlab.ui.control.Button
        ParametersLabel                 matlab.ui.control.Label
        AntidoteROIparamNbStepsEditField  matlab.ui.control.NumericEditField
        NbstepspathLabel                matlab.ui.control.Label
        ButtonROI_Duplicate             matlab.ui.control.Button
        ROItexturalinvestigationsLabel  matlab.ui.control.Label
        HTML_AntidoteReport             matlab.ui.control.HTML
        Text_Report_Antidote            matlab.ui.control.TextArea
        Image                           matlab.ui.control.Image
        TabGroup2                       matlab.ui.container.TabGroup
        MapTab                          matlab.ui.container.Tab
        GridLayout3                     matlab.ui.container.GridLayout
        UIAxes                          matlab.ui.control.UIAxes
        LiveTab                         matlab.ui.container.Tab
        GridLayout13                    matlab.ui.container.GridLayout
        EditField_BestQtot              matlab.ui.control.NumericEditField
        LIVEQtotalvalueLabel            matlab.ui.control.Label
        LIVE_Qtot_Gauge                 matlab.ui.control.Gauge
        TotalLabel_2                    matlab.ui.control.Label
        LiveDisplaySwitchLabel          matlab.ui.control.Label
        LiveDisplaySwitch               matlab.ui.control.Switch
        UIAxes_LiveAntidote1            matlab.ui.control.UIAxes
        UIAxes_LiveAntidote2            matlab.ui.control.UIAxes
        UIAxes_LiveAntidote3            matlab.ui.control.UIAxes
        UIAxes_LiveAntidote4            matlab.ui.control.UIAxes
        ResultsTab                      matlab.ui.container.Tab
        GridLayout11                    matlab.ui.container.GridLayout
        Gaude_Qasm                      matlab.ui.control.SemicircularGauge
        Gauge_Qcmp                      matlab.ui.control.SemicircularGauge
        Gauge_Qvol                      matlab.ui.control.SemicircularGauge
        AssemblageLabel                 matlab.ui.control.Label
        ModesLabel                      matlab.ui.control.Label
        CompositionsLabel               matlab.ui.control.Label
        EditField_Qasm                  matlab.ui.control.NumericEditField
        EditField_Qvol                  matlab.ui.control.NumericEditField
        EditField_Qcmp                  matlab.ui.control.NumericEditField
        Gauge_Qtotal                    matlab.ui.control.Gauge
        TotalLabel                      matlab.ui.control.Label
        EditField_Qtotal                matlab.ui.control.NumericEditField
        UIAxes2                         matlab.ui.control.UIAxes
        Result_Plot1                    matlab.ui.control.UIAxes
        Result_Plot2                    matlab.ui.control.UIAxes
        Tree_Phases                     matlab.ui.container.Tree
        Tree_Elem                       matlab.ui.container.Tree
    end

    
    properties (Access = public)
        XMapToolsApp
        
        MaskFile
        MapData
        DensityMap
        
        ProgramState
        
        BinBulk
        BinGfDef
        SF
        BinElData
        BinPhaseDef
        AssemblageCode
        OptionsXTT
        
        % The following properties are not saved --------------------------
        LocBase
        TheriakBase
        DefOxideName
        BingoDefault
        
        FirstPlot
        Data2Plot
        SelectedROI
        SelectedMaps
        ROI_Bulk_Listener
        ROI_Texture_Listener
        
        LiveUpdate      % for displaying results (live)
        WaitBar
        
        Report_Bingo
        Report_Antidote
        
        ProjectPath
        
        TextureROI
        
        % The following properties are not used (empty) -------------------
        Compositions
        
    end
    
    properties (Access = private)
         % Description
    end
    
    
    methods (Access = public)
        
        function InitializeApp(app)
            
            % Tree menus for plotting
            for i = 1:length(app.MapData.ElNames)
                p = uitreenode(app.Tree_Elem,'Text',char(app.MapData.ElNames{i}),'NodeData',i);
            end
            app.Tree_Elem.SelectedNodes = app.Tree_Elem.Children(1);
            
            for i = 1:length(app.MaskFile.Names)
                p = uitreenode(app.Tree_Phases,'Text',char(app.MaskFile.Names{i}),'NodeData',i);
                if i > 1
                    p2 = uitreenode(app.Tree_PhasesModel,'Text',char(app.MaskFile.Names{i}),'NodeData',i-1);
                end
            end
            app.Tree_Phases.SelectedNodes = app.Tree_Phases.Children(1);
            app.Tree_PhasesModel.SelectedNodes = app.Tree_PhasesModel.Children(1);
            
            %
            for i =1:14
                if i > length(app.MapData.ElNames)
                    eval(['app.El',num2str(i),'Button.Visible = ''off'';'])
                else
                    eval(['app.El',num2str(i),'Button.Text = app.MapData.ElNames{i};'])
                    eval(['app.El',num2str(i),'Button.Visible = ''on'';'])
                    eval(['app.El',num2str(i),'Button.Value = 1;'])
                end
            end
            CheckElButtonSelection(app);
            
            app.TextureROI.ROIdata = [];
            
            app.ThermodynamicDatabasePanel.Visible = 'off';
            app.PressureTemperatureRangePanel.Visible = 'off';
            app.InitialPressureTemperatureGuestimatePanel.Visible = 'off';
            app.InitialPressureTemperatureGuestimatePanel.Visible = 'off';
            app.Settings_NextButton.Visible = 'off';
            
            app.BingoButton.Enable = 'off';
            app.AntidoteButton.Enable = 'off'; 
            
            app.ButtonEliminateROI.Enable = 'off';
            app.LBC_NextButton.Visible = 'off';
            
            app.SelectedPhaseLabel.Visible = 'off';
            app.SelectedPhaseOptiDropDown.Visible = 'off';
            
        end
        
        function Results = CheckFiles4Theriak(app)
            Results = [0,0]; % DB and Theriak.ini
            
            % 
            Check = exist(fullfile(cd,'theriak.ini'));
            if ~Check
                [SUCCESS,MESSAGE,MESSAGEID] = copyfile(fullfile(app.LocBase,'Databases','theriak.ini'),fullfile(cd,'theriak.ini'),'f');
                if SUCCESS
                    Results(2) = 1;
                end
            else
                Results(2) = 1;
            end
            
            CheckDB = exist(fullfile(cd,app.DatabaseListBox.Value));
            if ~CheckDB
                [SUCCESS,MESSAGE,MESSAGEID] = copyfile(fullfile(app.LocBase,'Databases',app.DatabaseListBox.Value),fullfile(cd,app.DatabaseListBox.Value),'f');
                if SUCCESS
                    Results(1) = 1;
                end
            else
                Results(1) = 1;
            end
            
        end
        
        function CheckElButtonSelection(app)
            NbEl = 0;
            ElSel = zeros(14,1);
            for i =1:14
                if isequal(eval(['app.El',num2str(i),'Button.Value']),1)
                    eval(['app.El',num2str(i),'Button.BackgroundColor = [0.6941,0.8510,0.9843];']);
                    NbEl = NbEl + 1;
                    ElSel(i) = 1;
                else
                    eval(['app.El',num2str(i),'Button.BackgroundColor = [0.96,0.96,0.96];']);
                end
            end
            app.NbSystComponentsLabel.Text = [num2str(NbEl),' system components are selected'];
            
            app.SelectedMaps = find(ElSel);
            
            app.BinElData.selected = zeros(size(app.BinElData.selected));
            app.BinElData.selected(app.SelectedMaps) = 1;
            
        end
        
        function PlotSelectedData(app)
            
            app.TabGroup2.SelectedTab = app.MapTab;
            
            SelectedEl = app.Tree_Elem.SelectedNodes.NodeData;
            SelectedPhase = app.Tree_Phases.SelectedNodes.NodeData;
            
            app.Data2Plot = app.MapData.CData(SelectedEl).Map;
            
            if isequal(SelectedPhase,1)
                MaskMap = ones(size(app.MaskFile.MaskMap));
            else
                MaskMap = zeros(size(app.MaskFile.MaskMap));
                Idx = find(app.MaskFile.MaskMap == SelectedPhase-1);
                MaskMap(Idx) = 1;
            end
            
            app.Data2Plot = app.Data2Plot.*MaskMap;
            
            if app.FirstPlot
                imagesc(app.UIAxes,app.Data2Plot);
                axis(app.UIAxes,'image');
                colorbar(app.UIAxes);
                colormap(app.UIAxes,[0,0,0;RdYlBu(128)]);
                
                tb = axtoolbar(app.UIAxes,'default');
                
                btn = axtoolbarbtn(tb,'push');
                btn.Icon = '078-magic wand.png';
                btn.Tooltip = 'Auto contrast';
                btn.ButtonPushedFcn = @(varargin)Button_UIAxes_AutoContrastPushed(app);
                
                
                app.FirstPlot = 0;
            else
                app.UIAxes.Children(end).CData = app.Data2Plot;
                caxis(app.UIAxes,'auto');
                
                disableDefaultInteractivity(app.UIAxes);
            end
            
            %keyboard
        end
        
        function Button_UIAxes_AutoContrastPushed(app)
            
            ImageData = PlotMap_ExtractPlottedImage(app);       % extract ImageData from the figure
            ImageData = ImageData(find(ImageData));             % exclude zeros (new 4.0.0)
            IdxNotNan = find(~isnan(ImageData));
            if length(IdxNotNan) > 0
                ImageData = ImageData(IdxNotNan);
            end
            SortedData = sort(ImageData(:));
            SelCrit = round(numel(SortedData) * 0.065);
            
            Min1 = SortedData(SelCrit);
            Max1 = SortedData(end-SelCrit);
            
            SortedData2 = SortedData(SelCrit:end-SelCrit);
            SelCrit2 = round(numel(SortedData2) * 0.065);
            
            Min2 = SortedData2(SelCrit2);
            Max2 = SortedData2(end-SelCrit2);
            
            % Current values:
            [Min,Max] = caxis(app.UIAxes);
            
            % Check values:
            if isequal(Min,Min1) && isequal(Max,Max1)
                Min = Min2;
                Max = Max2;
            elseif isequal(Min,Min2) && isequal(Max,Max2)
                Min = min(ImageData(:));
                Max = max(ImageData(:));
            else
                Min = Min1;
                Max = Max1;
            end
            
            % Update plot
            caxis(app.UIAxes,[Min,Max]);
        end
        
        function ImageData = PlotMap_ExtractPlottedImage(app)
            lesInd = app.UIAxes.Children;
            for i=1:numel(lesInd)
                if isequal(lesInd(i).Type,'image')
                    ImageData = lesInd(i).CData;
                    break
                else
                    ImageData = [];
                end
            end
        end
        
        function CheckProgramState(app)
            
            if app.ProgramState > 1.1
                app.ThermodynamicDatabasePanel.Visible = 'on';
                
                % Temporary (beta)
                app.ApplyStep1Button.Enable = 'off';
                app.El1Button.Enable = 'off';
                app.El2Button.Enable = 'off';
                app.El3Button.Enable = 'off';
                app.El4Button.Enable = 'off';
                app.El5Button.Enable = 'off';
                app.El6Button.Enable = 'off';
                app.El7Button.Enable = 'off';
                app.El8Button.Enable = 'off';
                app.El9Button.Enable = 'off';
                app.El10Button.Enable = 'off';
                app.El11Button.Enable = 'off';
                app.El12Button.Enable = 'off';
                app.El13Button.Enable = 'off';
                app.El14Button.Enable = 'off';
                
                
            end
            if app.ProgramState > 1.2
                app.PressureTemperatureRangePanel.Visible = 'on';
                
                % Temporary (beta)
                app.ApplyStep2Button.Enable = 'off';
                % app.DatabaseListBox.Enable = 'off';
                
            end
            if app.ProgramState > 1.3
                app.InitialPressureTemperatureGuestimatePanel.Visible = 'on';
                app.Settings_NextButton.Visible = 'on';
                
                % Temporary (beta)
                app.ApplyStep3Button.Enable = 'off';
            end
            
            % LBC
            if app.ProgramState >= 2
                app.Settings_NextButton.Enable = 'off';
            end
            if isequal(app.BinBulk(1).Type,1)
                app.LBC_NextButton.Visible = 'on';
            else
                app.LBC_NextButton.Visible = 'off';
            end
            
            if app.ProgramState >= 4.0
                app.BingoButton.Enable = 'on';
                app.AntidoteButton.Enable = 'on';
            end 
            
        end
        
        function BingoInitialization(app)
            
            app.ProjectPath = '';
            
            app.BingoDefault.SelectedProgram = 0;
            app.BingoDefault.SelectedDatabase = 0;
            
            app.BingoDefault.Theriak.Path = '';                          % Not defined here
            app.BingoDefault.Theriak.Database(1).Label = '';
            app.BingoDefault.Theriak.Database(1).File1 = '';
            app.BingoDefault.Theriak.Database(1).Version = '';
            app.BingoDefault.Theriak.Database(1).Comment = '';
            app.BingoDefault.Theriak.Database(1).MinTrans = '';
            app.BingoDefault.Theriak.Database(1).PathForDB = '';
            app.BingoDefault.Theriak.InputBulk = [];
            
            ThePathForDB = [app.LocBase,'/Databases/'];
            fid = fopen([ThePathForDB,'Database_Definitions.txt']);
            
            Compt = 0;
            while 1
                TheLine = fgetl(fid);
                
                if isequal(TheLine,-1)
                    break
                end
                
                if isequal(TheLine,'###')
                    
                    Compt = Compt+1;
                    
                    % FileName:
                    TheStr = strread(fgetl(fid),'%s');
                    app.BingoDefault.Theriak.Database(Compt).Label = TheStr{2};
                    app.BingoDefault.Theriak.Database(Compt).File1 = TheStr{2};
                    
                    % DefinitionFile:
                    TheStr = strread(fgetl(fid),'%s');
                    app.BingoDefault.Theriak.Database(Compt).MinTrans = TheStr{2};
                    
                    % Version:
                    TheStr = strread(fgetl(fid),'%s');
                    app.BingoDefault.Theriak.Database(Compt).Version = TheStr{2};
                    
                    % Comment:
                    TheLine = fgetl(fid);
                    app.BingoDefault.Theriak.Database(Compt).Comment = TheLine(11:end);
                    
                    % Path for DB
                    app.BingoDefault.Theriak.Database(Compt).PathForDB = ThePathForDB;
                end
            end
            
            fclose(fid);
            
            % READ XTT_Min_ files
            for i=1:length(app.BingoDefault.Theriak.Database)
                
                [DefMin,DefGf] = ReadMinTrans(app,app.BingoDefault.Theriak.Database(i).MinTrans,app.BingoDefault.Theriak.Database(i).PathForDB);
                
                app.BingoDefault.Theriak.Database(i).DefMin = DefMin;
                app.BingoDefault.Theriak.Database(i).DefGf = DefGf;
            end
            
            % Update interface
            for i = 1:length(app.BingoDefault.Theriak.Database)
                Items{i} = app.BingoDefault.Theriak.Database(i).Label;
            end
            app.DatabaseListBox.Items = Items;
            
            % Read AddPhaseDefinitions.txt (4.1 – 14.05.23)
            if exist(fullfile(cd,'AddPhaseDefinitions.txt'))
                fid = fopen(fullfile(cd,'AddPhaseDefinitions.txt'));                
                while 1
                    TheL = fgetl(fid);
                    
                    if isequal(TheL,-1)
                        break
                    end
                    
                    if length(TheL) > 1
                        if ~isequal(TheL(1),'!')
                            TheStr = strread(TheL,'%s');
                            if isequal(length(TheStr),6)
                                IdxDB = find(ismember(Items,TheStr{1}));
                                if ~isempty(IdxDB)
                                    switch TheStr{2}
                                        case '1'
                                            Where = size(app.BingoDefault.Theriak.Database(IdxDB).DefMin,1)+1;
                                            app.BingoDefault.Theriak.Database(IdxDB).DefMin{Where,1} = TheStr{3};
                                            app.BingoDefault.Theriak.Database(IdxDB).DefMin{Where,2} = str2num(TheStr{4});
                                            app.BingoDefault.Theriak.Database(IdxDB).DefMin{Where,3} = TheStr{5};
                                            app.BingoDefault.Theriak.Database(IdxDB).DefMin{Where,4} = strread(TheStr{6}(2:end-1),'%s','delimiter',',')';
                                        case '2'
                                            Where = size(app.BingoDefault.Theriak.Database(IdxDB).DefGf,1)+1;
                                            app.BingoDefault.Theriak.Database(IdxDB).DefGf{Where,1} = TheStr{3};
                                            app.BingoDefault.Theriak.Database(IdxDB).DefGf{Where,2} = str2num(TheStr{4});
                                            app.BingoDefault.Theriak.Database(IdxDB).DefGf{Where,3} = TheStr{5};
                                            app.BingoDefault.Theriak.Database(IdxDB).DefGf{Where,4} = strread(TheStr{6}(2:end-1),'%s','delimiter',',')';
                                    end                                    
                                end
                            end
                        end
                    end
                end
            end
            
        end
        
        function [DefMin,DefGf] = ReadMinTrans(app,MinTransFile,Path4DB)
            %
            DefMin = [];
            DefGf = [];
            
            fid = fopen([Path4DB,MinTransFile]); Compt = 0;
            while 1
                lalign = fgetl(fid);
                if isequal(lalign,-1)
                    break
                end
                
                if length(lalign) > 1
                    if isequal(lalign(1:2),'>1')
                        Compt = 0;
                        while 1
                            lalign = fgetl(fid);
                            if isequal(lalign,'')
                                break
                            end
                            Compt = Compt+1;
                            Temp = strread(lalign,'%s');
                            DefMin{Compt,1} = Temp{1};
                            DefMin{Compt,2} = str2num(Temp{2});
                            DefMin{Compt,3} = Temp{3};
                            DefMin{Compt,4} = strread(Temp{4}(2:end-1),'%s','delimiter',',')';
                            %DefMin{Compt,5} = str2double(strread(Temp{5}(2:end-1),'%s','delimiter',','))';
                        end
                    end
                    
                end
                
                if length(lalign) > 1
                    if isequal(lalign(1:2),'>2')
                        Compt = 0;
                        while 1
                            lalign = fgetl(fid);
                            if isequal(lalign,'')
                                break
                            end
                            Compt = Compt+1;
                            Temp = strread(lalign,'%s');
                            DefGf{Compt,1} = Temp{1};
                            DefGf{Compt,2} = str2num(Temp{2});
                            DefGf{Compt,3} = Temp{3};
                            DefGf{Compt,4} = strread(Temp{4}(2:end-1),'%s','delimiter',',')';
                            %DefGf{Compt,5} = str2double(strread(Temp{5}(2:end-1),'%s','delimiter',','))';
                        end
                    end
                end
            end
            
            
        end
        
        function UpdatePTsteps(app)
            
            app.OptionsXTT.Tmin = app.TminEditField.Value;
            app.OptionsXTT.Tinc = app.TstepEditField.Value;
            app.OptionsXTT.Tmax = app.TmaxEditField.Value;
            
            app.OptionsXTT.Pmin = app.PminEditField.Value;
            app.OptionsXTT.Pinc = app.PstepEditField.Value;
            app.OptionsXTT.Pmax = app.PmaxEditField.Value;
            
            app.TNbSteps.Value = length([app.OptionsXTT.Tmin:app.OptionsXTT.Tinc:app.OptionsXTT.Tmax]);
            app.PNbSteps.Value = length([app.OptionsXTT.Pmin:app.OptionsXTT.Pinc:app.OptionsXTT.Pmax]);
        end
        
        function ROI_DeleteROI(app)
            % This function deletes all the ROI of the main figure
            
            % List to be updated later on
            delete(findall(app.UIAxes, 'Type',  'images.roi.Rectangle'));
            delete(findall(app.UIAxes, 'Type',  'images.roi.Polygon'));
            delete(findall(app.UIAxes, 'Type',  'images.roi.Ellipse'));
            delete(findall(app.UIAxes, 'Type',  'images.roi.Circle'));
            delete(findall(app.UIAxes, 'Type',  'images.roi.Point'));
            
            delete(findall(app.UIAxes, 'Type',  'images.roi.Polyline'));
            
            delete(findall(app.UIAxes, 'Type',  'line'));
        end
        
        function DeactivatePlotZoomPanOptions(app)
            
            if ~isempty(app.UIAxes)
                if ~isempty(app.UIAxes.Toolbar.Children)
                    app.UIAxes.Toolbar.Children(5).Value = 'off';
                    matlab.graphics.interaction.webmodes.toggleMode(app.UIAxes,'pan',app.UIAxes.Toolbar.Children(5).Value)
                    
                    app.UIAxes.Toolbar.Children(6).Value = 'off';
                    matlab.graphics.interaction.webmodes.toggleMode(app.UIAxes,'zoom',app.UIAxes.Toolbar.Children(6).Value)
                    
                    app.UIAxes.Toolbar.Children(7).Value = 'off';
                    matlab.graphics.interaction.webmodes.toggleMode(app.UIAxes,'zoomout',app.UIAxes.Toolbar.Children(7).Value)
                end
            end
        end
        
        function [TempBinBulk] = BinUpdateBulkComposition(app,TempBinBulk,LBref)
            %
            ListElem = app.MapData.ElNames;
            
            % (1) Get the shape of the selected pixels
            h = TempBinBulk.DomainXrefYref;
            [LinS,ColS] = size(app.DensityMap);
            MaskSel = poly2mask(h(:,1),h(:,2),LinS,ColS);
            
            % (2) Remove the pixels without composition
            %     and remove the pixels of the unselected phases  (PL 14.06.17)
            OxideSum=zeros(size(app.DensityMap));
            PxSel4Bulk = zeros(size(app.DensityMap));
            for i=1:length(ListElem)
                OxideSum = OxideSum + app.MapData.CData(i).Map;
            end
            
            % (3) Phase rejection:
            PxSel4Bulk = zeros(size(app.DensityMap));
            for i = 1:length(app.BinPhaseDef)
                if app.BinPhaseDef(i).SelForBulk
                    TheSpecMaskPixels = find(app.MaskFile.MaskMap == i);
                    PxSel4Bulk(TheSpecMaskPixels) = ones(size(TheSpecMaskPixels));
                end
            end
            
            MaskSelFinal = zeros(size(app.DensityMap));
            PixelsSelected = find(OxideSum(:) > 0 & MaskSel(:) > 0 & PxSel4Bulk(:) > 0 & app.DensityMap(:)>0);
            MaskSelFinal(PixelsSelected) = ones(size(PixelsSelected));
            
            % Density correction for all maps
            AverageDensity = mean(app.DensityMap(PixelsSelected));
            %disp(num2str(AverageDensity));
            
            for i=1:length(ListElem)
                TheMap = app.MapData.CData(i).Map;
                DCM(i).map = (TheMap.*app.DensityMap.*MaskSelFinal)./AverageDensity;
            end
            
            % Atom weight for the selected elements
            Concat = '0   ';
            SelEl = app.SelectedMaps; 
            NbSelEl = length(SelEl);
            
            %FileName = 'OptionsXTT.txt';
            %[OptString,Valid] = ReadOptionXTT(FileName);
            
            %OptString1 = OptString{1};
            %OptString2 = OptString{2};
            
            for i=1:NbSelEl
                ElCode = app.BinElData.listElem(SelEl(i));
                
                OxideValue = mean(DCM(SelEl(i)).map(PixelsSelected));
                AtomW = OxideValue/app.BinElData.MolarMass(SelEl(i))*app.BinElData.NbAtoms(SelEl(i));
                
                Concat = [Concat,char(ElCode),'(',num2str(AtomW),')'];
                
                Oxide2Print(i) = OxideValue;
                OxideName2Print{i} = char(app.BinElData.listOxides(SelEl(i)));
                
                %disp([char(ElCode),' ',char(num2str(OxideValue)),' ',char(num2str(AtomW))])
            end
            %Concat = [Concat,char(OptString1),'   ',char(OptString2),'LB_',num2str(LBref)];
            Concat = [Concat,'H(1)O(?)O(0)C(0)    * LB_',num2str(LBref)];
            
            TempBinBulk.CompositionOriginal = Concat;
            TempBinBulk.CompositionModified = Concat;
            TempBinBulk.CompositionIterative = Concat;
            
            TempBinBulk.Oxides = OxideName2Print;
            TempBinBulk.OxideCompositions = Oxide2Print;
            
            disp(' ')
            disp(['----------------- NEW BULK COMPOSITION (',['LB_',num2str(LBref)],') -----------------'])
            
            for i=1:length(Oxide2Print)
                fprintf('%s\t',char(OxideName2Print{i}));
            end
            fprintf('\n');
            for i=1:length(Oxide2Print)
                fprintf('%.3f\t',Oxide2Print(i));
            end
            fprintf('\n\n');
            
        end
        
        function CheckPhaseSolutionList(app)
            ROI_DeleteROI(app);     % first we eliminate all ROI
            
            Data4Table = cell(length(app.BinPhaseDef),5);
            
            iDB = find(ismember(app.DatabaseListBox.Items,app.DatabaseListBox.Value));
            DefMin = app.BingoDefault.Theriak.Database(iDB).DefMin;
            
            for i=1:length(DefMin(:,1))
                ListRefMiner{i,1} = DefMin{i,1};
                ListDBMiner{i,1} = DefMin{i,3};
                OxygenRefMiner(i,1) = DefMin{i,2};
            end
            
            for i = 1:length(app.BinPhaseDef)
                
                Data4Table{i,1} = app.BinPhaseDef(i).name;
                
                if ~isequal(app.BinPhaseDef(i).DBName,app.DatabaseListBox.Value) || isempty(app.BinPhaseDef(i).DBMinName)
                    % We update database and models
                    app.BinPhaseDef(i).DBName = app.DatabaseListBox.Value;
                    
                    NamePhaseForSearch = upper(app.BinPhaseDef(i).name);
                    [YesIs,WereIsPhase] = ismember(NamePhaseForSearch,ListRefMiner);
                    
                    if YesIs
                        app.BinPhaseDef(i).DBMinName = ListDBMiner{WereIsPhase};
                        app.BinPhaseDef(i).OxBasis = OxygenRefMiner(WereIsPhase);
                        app.BinPhaseDef(i).ListVariAntidote = DefMin{WereIsPhase,4};
                    end
                end
                Data4Table{i,2} = app.BinPhaseDef(i).DBMinName;
                
                
                % group
                if app.BinPhaseDef(i).NbGrps > 0
                    Data4Table{i,3} = app.BinPhaseDef(i).Grps(app.BinPhaseDef(i).SelGrps).Name;
                else
                    Data4Table{i,3} = 'undefined';
                end
                
                % Selections
                if app.BinPhaseDef(i).SelForBA
                    Data4Table{i,4} = 'Yes';
                else
                    Data4Table{i,4} = 'No';
                end
                if app.BinPhaseDef(i).SelForBulk
                    Data4Table{i,5} = 'Yes';
                else
                    Data4Table{i,5} = 'No';
                end
            end
            
            app.UITable_PhasesModel.Data = Data4Table;
            
            % selected...
            if ~isempty(app.Tree_PhasesModel.SelectedNodes)
                
                SelNodeData = app.Tree_PhasesModel.SelectedNodes(1).NodeData;
                
                app.SelectforBACheckBox.Value = app.BinPhaseDef(SelNodeData).SelForBA;
                app.SelectforLBCCheckBox.Value = app.BinPhaseDef(SelNodeData).SelForBulk;
                
                ListDBMinerDisp = ['None';ListDBMiner];
                Idx = find(ismember(ListDBMinerDisp,app.BinPhaseDef(SelNodeData).DBMinName));
                if length(Idx) > 1
                    Idx = Idx(1);
                end
                
                app.ModelPhaseDropDown.Items = ListDBMinerDisp;
                app.ModelPhaseDropDown.ItemsData = [1:length(ListDBMinerDisp)];
                if ~isempty(Idx)
                    app.ModelPhaseDropDown.Value = Idx;
                else
                    app.ModelPhaseDropDown.Value = 1;
                end
                
                % Update Groups
                SelMin = app.Tree_PhasesModel.SelectedNodes.NodeData;
                
                for i = 1:length(app.TreeMinGroups.Children)
                    app.TreeMinGroups.Children(i).delete;
                end
                
                if app.BinPhaseDef(SelMin).NbGrps > 0
                    for i = 1:length(app.BinPhaseDef(SelMin).NbGrps)
                        p = uitreenode(app.TreeMinGroups,'Text',char(app.BinPhaseDef(SelMin).Grps(i).Name),'NodeData',i);
                    end
                    app.TreeMinGroups.SelectedNodes = app.TreeMinGroups.Children(1);
                    TreeMinGroupsSelectionChanged(app,0);
                end
                
            end
            
            if isequal(app.ModelPhaseDropDown.Value,1)
                app.MethodROIDropDown_Phase.Enable = 'off';
                app.ButtonAddROI_Phase.Enable = 'off';
                app.ButtonEliminateROI_Phase.Enable = 'off';
                app.TreeMinGroups.Enable = 'off';
            else
                app.MethodROIDropDown_Phase.Enable = 'on';
                app.ButtonAddROI_Phase.Enable = 'on';
                % app.ButtonEliminateROI_Phase.Enable = 'on';
                app.TreeMinGroups.Enable = 'on';
                
            end
            
            
            % Check if the button Next can be displayed.
            Valid = zeros(length(app.BinPhaseDef),1);
            for i = 1:length(app.BinPhaseDef)
                if app.BinPhaseDef(i).SelForBA
                    if app.BinPhaseDef(i).NbGrps > 0
                        if sum(app.BinPhaseDef(i).Grps(app.BinPhaseDef(i).SelGrps).OxideCompositions) > 0
                            Valid(i) = 1;
                        end
                    end
                    
                else
                    Valid(i) = 1;
                end
            end
            
            if isequal(sum(Valid),length(Valid))
                app.Phase_NextButton.Enable = 'on';
                app.ProgramState = 3.1;
                
                ListNames4Menu = {};
                % Update Antidote Menu
                for i = 1:length(app.BinPhaseDef)
                    if app.BinPhaseDef(i).SelForBA
                        ListNames4Menu{i} = app.BinPhaseDef(i).name;
                    else
                        ListNames4Menu{i} = '*** not selected';
                    end
                end
                app.SelectedPhaseOptiDropDown.Items = ListNames4Menu;
                app.SelectedPhaseOptiDropDown.ItemsData = 1:length(ListNames4Menu);
                
                % This is recalculated everytime something might have
                % changed:
                CalculatePhaseProportions(app);
                
            else
                app.Phase_NextButton.Enable = 'off';
            end
            
        end
        
        function Bulk_ROI_changed_shape(app,~)
            selectedNodes = app.ROITree.SelectedNodes.NodeData;
            
            TempBinBulk  = app.BinBulk(selectedNodes);
            
            % We update the positio
            Method = TempBinBulk.TypeROI;
            switch Method
                case 'Rectangle ROI'
                    Coordinates = app.SelectedROI.Vertices;
                case 'Polygon ROI'
                    Coordinates = app.SelectedROI.Position;
            end
            TempBinBulk.PositionROI = app.SelectedROI.Position;
            TempBinBulk.DomainXrefYref = Coordinates;
            
            [TempBinBulk] = BinUpdateBulkComposition(app,TempBinBulk,selectedNodes);
            
            Cell2Disp = cell(length(TempBinBulk.Oxides),2);
            for i = 1:length(TempBinBulk.Oxides)
                Cell2Disp{i,1} = TempBinBulk.Oxides{i};
                Cell2Disp{i,2} = TempBinBulk.OxideCompositions(i);
            end
            app.UITable.Data = Cell2Disp;
            app.LBCEditField.Value = TempBinBulk.CompositionIterative;
            
            app.BinBulk(selectedNodes) = TempBinBulk;
            
        end
        
        function SelPixels = BinUpdatePhaseDef(app,SelMin,GrpId)
            
            % --------------------------------------------------------
            % Import required variables
            MaskMap = app.MaskFile.MaskMap;
            
            MaskLayer = zeros(size(MaskMap));
            WhichOnesPx = find(MaskMap(:) == SelMin);
            MaskLayer(WhichOnesPx) = ones(size(WhichOnesPx));
            
            OxideSum=zeros(size(MaskMap));
            ListElem = app.MapData.ElNames;
            
            [nPixV, nPixH] = size(app.MapData.CData(1).Map);
            Chemic = zeros(nPixV*nPixH,length(ListElem));
            
            for i=1:length(ListElem)
                OxideSum = OxideSum + app.MapData.CData(i).Map;
                Chemic(:,i) = app.MapData.CData(i).Map(:).*MaskLayer(:);
            end
            
            % --------------------------------------------------------
            % Compositions (at the right format)
            [LinS,ColS] = size(app.DensityMap);
            TheMapCoord = reshape([1:LinS*ColS],LinS,ColS);
            
            h = app.BinPhaseDef(SelMin).Grps(GrpId).XrefYref;
            
            MaskSel = poly2mask(h(:,1),h(:,2),LinS,ColS);
            
            SelPixels = find(MaskMap(:) == SelMin & MaskSel(:) == 1);
            
            if isempty(SelPixels)
                uialert(app.BingoAntidote_GUI,['The selection does not belong to ',char(app.MaskFile.Names{SelMin+1})],'XMapTools');
                return
            end
            
            disp(' ')
            disp(['Mineral: ',app.BinPhaseDef(SelMin).name, ' || Grp: ',num2str(GrpId)])
            disp(' ')
            
            for i=1:length(app.SelectedMaps)
                
                % With outlier rejection                                (PL - 18.05.18)
                SelData = app.MapData.CData(app.SelectedMaps(i)).Map(SelPixels);
                
                NbPixels = length(SelData);
                Average = mean(SelData);
                Sigma6 = 6*std(SelData);
                Outliers = find(SelData > Average+Sigma6 | SelData < Average-Sigma6);
                NotOutliers = find(SelData < Average+Sigma6 | SelData > Average-Sigma6);
                
                NbOutliers = length(Outliers);
                
                if NbOutliers/NbPixels < 0.05 && NbOutliers/NbPixels  > 0
                    disp(sprintf('\t%s\t%s',char(app.BinElData.listElem{app.SelectedMaps(i)}),[num2str(NbOutliers),'/',num2str(NbPixels),' (',num2str(NbOutliers/NbPixels*100),' %) of the pixel(s) rejected (6 sigma test, 5% cut-off threshold)']));
                    SelData = SelData(NotOutliers);
                else
                    disp(sprintf('\t%s\t%s',[char(app.BinElData.listElem{app.SelectedMaps(i)}),'*'],[num2str(NbOutliers),'/',num2str(NbPixels),' (',num2str(NbOutliers/NbPixels*100),' %) of the pixels are apparently outliers (6 sigma test, 5% cut-off threshold) - ALL PIXELS SELECTED']));
                end
                
                CompositionOxide(i) = mean(SelData);
                SigmaOxide(i) = std(SelData);
                OxideLabels{i} = app.BinElData.listOxides{app.SelectedMaps(i)};
                SFLabels{i} = app.BinElData.listElem{app.SelectedMaps(i)};
                MolarMass(i) = app.BinElData.MolarMass(app.SelectedMaps(i));
                NbAtoms(i) = app.BinElData.NbAtoms(app.SelectedMaps(i));
                NbOxygen(i) = app.BinElData.NbOxygen(app.SelectedMaps(i));
                
                if isnan(SigmaOxide(i))
                    disp('This shouldn''t have happened, please contact pierre.lanari@geo.unibe.ch (ERR1628)')
                    disp('type ''return'' to continue')
                    keyboard
                end
            end
            
            [SFCompositions,SigmaSF] = BinStructForm(app,CompositionOxide,SigmaOxide,MolarMass,NbAtoms,NbOxygen,app.BinPhaseDef(SelMin).OxBasis);
            
            % Check for NaN (zero concentrations in oxide)
            WhereNaN = find(isnan(SigmaSF));
            if length(WhereNaN)
                SigmaSF(WhereNaN) = zeros(size(WhereNaN));
            end
            
            disp(' ')
            
            h = sprintf('\t');
            for n1=1:length(OxideLabels)
                h = [h,sprintf('%s\t',char(OxideLabels{n1}))];
            end
            disp(h)
            h = sprintf('\t');
            for n1=1:length(CompositionOxide)
                h = [h,sprintf('%.2f\t',CompositionOxide(n1))];
            end
            disp(h)
            h = sprintf('\t');
            for n1=1:length(SigmaOxide)
                h = [h,sprintf('%.2f\t',SigmaOxide(n1))];
            end
            disp(h)
            disp(' ')
            h = sprintf('\t');
            for n1=1:length(SFLabels)
                h = [h,sprintf('%s\t',char(SFLabels{n1}))];
            end
            disp(h)
            h = sprintf('\t');
            for n1=1:length(SFCompositions)
                h = [h,sprintf('%.2f\t',SFCompositions(n1))];
            end
            disp(h)
            h = sprintf('\t');
            for n1=1:length(SigmaSF)
                h = [h,sprintf('%.2f\t',SigmaSF(n1))];
            end
            disp(h)
            
            app.BinPhaseDef(SelMin).Grps(GrpId).OxideCompositions = CompositionOxide;
            app.BinPhaseDef(SelMin).Grps(GrpId).SigmaOxide = SigmaOxide;
            app.BinPhaseDef(SelMin).Grps(GrpId).OxideLabels = OxideLabels;
            app.BinPhaseDef(SelMin).Grps(GrpId).SFCompositions = SFCompositions;
            app.BinPhaseDef(SelMin).Grps(GrpId).SigmaSF = SigmaSF;
            app.BinPhaseDef(SelMin).Grps(GrpId).SFLabels = SFLabels;
            
        end
        
        function [SFCompositions,SigmaSF] = BinStructForm(app,CompositionOxide,SigmaOxide,MolarMass,NbAtoms,NbOxygen,OxBasis)
            %
            AtomicPer = CompositionOxide./MolarMass.*NbAtoms;
            TheSum = sum((AtomicPer .* NbOxygen) ./ NbAtoms);
            RefOx = TheSum/OxBasis;
            
            SFCompositions = AtomicPer / RefOx;
            
            
            % Uncertainty propagation  - - - - - - - - - - - - - - - -  (PL - 28.05.18)
            % Tested using a MC test (see below)
            
            SigmaAtomicPer = AtomicPer.*(SigmaOxide./CompositionOxide);     % Tested OK
            
            A = (AtomicPer .* NbOxygen) ./ NbAtoms;
            SigmaA = A.*SigmaAtomicPer./AtomicPer;                          % Tested OK
            
            % Check for NaN...
            WhereNaN = find(isnan(SigmaA));
            if length(WhereNaN)
                SigmaA(WhereNaN) = zeros(size(WhereNaN));
            end
            
            SigmaTheSum = sqrt(sum(SigmaA.^2));
            
            SigmaRefOx = RefOx*(SigmaTheSum/TheSum);
            
            SigmaSF = SFCompositions.*(sqrt((SigmaAtomicPer./AtomicPer).^2+(SigmaRefOx./RefOx)^2));
            
            
            % Check for NaN (zero concentrations in oxide)
            WhereNaN = find(isnan(SigmaSF));
            if length(WhereNaN)
                SigmaSF(WhereNaN) = zeros(size(WhereNaN));
            end
            
            % Monte Carlo TEST
            MCtest = 0;
            if isequal(MCtest,1)
                
                NbPerm = 10000;
                
                CompositionOxideMC = repmat(CompositionOxide,NbPerm,1) + randn(NbPerm,size(CompositionOxide,2)).*repmat(SigmaOxide,NbPerm,1);
                
                disp(num2str(CompositionOxide))
                disp(num2str(mean(CompositionOxideMC)))
                disp(' ')
                disp(num2str(SigmaOxide))
                disp(num2str(std(CompositionOxideMC)))
                disp(' - - - - - - ')
                
                AtomicPerMC = zeros(NbPerm,size(AtomicPer,2));
                TheSumMC = zeros(NbPerm,size(TheSum,2));
                RefOxMC = zeros(NbPerm,size(RefOx,2));
                SFCompositionsMC = zeros(NbPerm,size(SFCompositions,2));
                
                for i = 1:NbPerm
                    
                    AtomicPerMC(i,:) = CompositionOxideMC(i,:)./MolarMass.*NbAtoms;
                    TheSumMC(i) = sum((AtomicPerMC(i,:) .* NbOxygen) ./ NbAtoms);
                    RefOxMC(i) = TheSumMC(i)/OxBasis;
                    
                    SFCompositionsMC(i,:) = AtomicPerMC(i,:) / RefOxMC(i);
                    
                    %SFCompositionsMC(i,:) = BinStructForm(CompositionOxideMC(i,:),SigmaOxide,MolarMass,NbAtoms,NbOxygen,BinPhaseDef(SelectedPhase).OxBasis);
                end
                
                disp(' ')
                disp(num2str(SFCompositions))
                disp(num2str(mean(SFCompositionsMC)))
                disp(' ')
                disp(num2str(SigmaSF))
                disp(num2str(std(SFCompositionsMC)))
            end
            
        end
        
        function CalculatePhaseProportions(app)
            
            selectedNodes = app.ROITree.SelectedNodes.NodeData;
            TempBinBulk  = app.BinBulk(selectedNodes);
            
            % (1) Get the shape of the selected pixels
            h = TempBinBulk.DomainXrefYref;
            [LinS,ColS] = size(app.DensityMap);
            MaskSel = poly2mask(h(:,1),h(:,2),LinS,ColS);
            
            TheSelMaskOk = MaskSel .* app.MaskFile.MaskMap;
            
            Compt = 0;
            for i=1:length(app.BinPhaseDef)
                if app.BinPhaseDef(i).SelForBulk
                    Compt = Compt+1;
                    PhaseSelected(Compt) = i;
                    NbPixels(Compt) = length(find(TheSelMaskOk(:) == i));
                end
            end
            
            Proportions = NbPixels./sum(NbPixels)*100;
            
            Compt = 0;
            for i=1:length(app.BinPhaseDef)
                if app.BinPhaseDef(i).SelForBulk
                    Compt = Compt+1;
                    app.BinPhaseDef(i).PhaseVolProp = Proportions(Compt);
                end
            end
            
        end
        
        function UpdateFluidMeltBufferPanel(app)
            
            Species = extractfield(app.BinGfDef.Fluids.Spec,'Name');
            
            % Delete nodes
            for i = 1:length(app.Tree_FluidModels.Children)
                app.Tree_FluidModels.Children(1).delete;
            end
            
            for i = 1:length(Species)
                p = uitreenode(app.Tree_FluidModels,'Text',char(Species{i}),'NodeData',i);
            end
            
            if ~isempty(length(app.Tree_FluidModels.Children))
                app.Tree_FluidModels.SelectedNodes = app.Tree_FluidModels.Children(1);
                UpdateDisplayedFluid(app);
            end
            
            if ~isempty(app.BinGfDef.Melt.Name)
                app.ActivateMeltModel_CheckBox.Value = app.BinGfDef.Melt.Activate;
                app.MeltModelPhase_DropDown.Items = {app.BinGfDef.Melt.Name};
                app.MeltModelPhase_DropDown.ItemsData = 1;
                app.MeltModelPhase_DropDown.Value = 1;
                app.IncudeMeltEvaluation_CheckBox.Value = app.BinGfDef.Melt.Include;
                
                app.ActivateMeltModel_CheckBox.Enable = 'on';
                app.MeltModelPhase_DropDown.Enable = 'on';
                app.IncudeMeltEvaluation_CheckBox.Enable = 'on';
            else
                app.ActivateMeltModel_CheckBox.Value = 0;
                app.MeltModelPhase_DropDown.Items = {'No melt model available in DB'};
                app.MeltModelPhase_DropDown.ItemsData = {'No melt model available in DB'};
                app.MeltModelPhase_DropDown.Value = 'No melt model available in DB';
                app.IncudeMeltEvaluation_CheckBox.Value = 0;
                
                app.ActivateMeltModel_CheckBox.Enable = 'off';
                app.MeltModelPhase_DropDown.Enable = 'off';
                app.IncudeMeltEvaluation_CheckBox.Enable = 'off';
            end
            
            app.ActivateExtraOxygen_CheckBox.Value = app.BinGfDef.Oxygen.ActivateExtraO;
            app.OptimizeO_CheckBox.Value = app.BinGfDef.Oxygen.OptimizeExtraO;
            app.BulkO_EditField.Value = app.BinGfDef.Oxygen.ExtraO.Bulk;
            app.OxMin_EditField.Value = app.BinGfDef.Oxygen.ExtraO.Lower;
            app.OxMax_EditField.Value = app.BinGfDef.Oxygen.ExtraO.Upper;
            
            app.ActivateOxygenBuffer_CheckBox.Enable = 'off';
            app.Buffer_DropDown.Items = {'Not available'};
            app.Buffer_DropDown.Enable = 'off';
            
        end
        
        function UpdateDisplayedFluid(app)
            
            FluidID = app.Tree_FluidModels.SelectedNodes.NodeData;
            
            app.ActivateFluidSpecie_CheckBox.Value = app.BinGfDef.Fluids.Spec(FluidID).IsActive;
            app.OptimizeNFluid_CheckBox.Value = app.BinGfDef.Fluids.Spec(FluidID).Optimize;
            
            app.BulkNfluid_EditField.Value = app.BinGfDef.Fluids.Spec(FluidID).Bulk;
            app.MinNfluid_EditField.Value = app.BinGfDef.Fluids.Spec(FluidID).Lower;
            app.MaxNfluid_EditField.Value = app.BinGfDef.Fluids.Spec(FluidID).Upper;
            
        end
        
        function FreezeBingoAntidoteInterface(app,State)
            
            switch State
                case {'on','On'}
                    app.AntidoteButton.Enable = 'off';
                    app.BingoButton.Enable = 'off';
                    app.Tree_Elem.Enable = 'off';
                    app.Tree_Phases.Enable = 'off';
                    
                case {'off','Off'}
                    app.AntidoteButton.Enable = 'on';
                    app.BingoButton.Enable = 'on';
                    app.Tree_Elem.Enable = 'on';
                    app.Tree_Phases.Enable = 'on';
                    
            end
            
            drawnow
            
        end
        
        function CleanInterface(app)
            
            if ~isempty(app.ROITree.Children)
                for i = 1:length(app.ROITree.Children)
                    app.ROITree.Children(i).delete;
                end
            end
            
        end
        
        
        function Texture_ROI_changed_shape(app,~)
            
            WhereItGoes = app.TextureROITree.SelectedNodes.NodeData;
            app.TextureROI.ROIdata(WhereItGoes).Vertices = app.SelectedROI.Vertices;
            app.TextureROI.ROIdata(WhereItGoes).Position = app.SelectedROI.Position;
        end
        
        function GenerateROIPath(app,Mode)
            
            NbSteps = app.AntidoteROIparamNbStepsEditField.Value;
            
            InterpPositions = [];
            
            for iSeg = 1:length(app.TextureROI.ROIdata)-1
                
                StartPos = app.TextureROI.ROIdata(iSeg).Position;
                EndPos = app.TextureROI.ROIdata(iSeg+1).Position;
                
                InterpPos = zeros(NbSteps,length(StartPos));
                
                for i = 1:length(StartPos)
                    InterpPos(:,i) = interp1([1,NbSteps],[StartPos(i),EndPos(i)],1:NbSteps)';
                end
                InterpPositions = [InterpPositions;InterpPos];
            end
            
            app.TextureROI.InterpPositions = InterpPositions;
            
            switch Mode
                case 'on'
                    h = figure;
                    ax2 = gca;
                    axis tight manual % this ensures that getframe() returns a consistent size
                    filename = 'PathAnimated.gif';
                    
                    ROI_DeleteROI(app);
                    
                    copyobj(app.UIAxes.Children,ax2);
                    axis(ax2,'image');
                    colormap(ax2,app.UIAxes.Colormap)
                    colorbar('vertical');
                    ax2.XTick = app.UIAxes.XTick;
                    ax2.YTick = app.UIAxes.YTick;
                    ax2.YDir = app.UIAxes.YDir;
                    ax2.CLim = app.UIAxes.CLim;
                    
                    ROI = drawrectangle(ax2,'Position',app.TextureROI.InterpPositions(1,:),'Color',[1,0,0.7137],'InteractionsAllowed','none');
                    title(ax2,['Step ',num2str(1),' / ',num2str(size(app.TextureROI.InterpPositions,1))])
                    
                    frame = getframe(h);
                    im = frame2im(frame);
                    [imind,cm] = rgb2ind(im,256);
                    imwrite(imind,cm,filename,'gif', 'Loopcount',inf);
                    
                    for i = 2:size(app.TextureROI.InterpPositions,1)
                        drawnow
                        pause(0.01)
                        ROI.Position = app.TextureROI.InterpPositions(i,:);
                        title(ax2,['Step ',num2str(i),' / ',num2str(size(app.TextureROI.InterpPositions,1))])
                        
                        frame = getframe(h);
                        im = frame2im(frame);
                        [imind,cm] = rgb2ind(im,256);
                        imwrite(imind,cm,filename,'gif','WriteMode','append');
                    end
            end
            
            
            
        end
        
        function [iX,iY] = CalcIndices(app,iLoop,NbXi,NbYi)
            
            if iLoop <= NbYi
                iY = iLoop;
                iX = 1;
            elseif ~isequal(iLoop/NbYi,NbXi)
                iX = ceil(iLoop/NbYi);
                iY = iLoop - (iX-1)*NbYi;
            else
                iX = NbXi;
                iY = NbYi;
            end
            
        end
        
    end
    
    methods (Access = private)
        
        
        
        
        
        
        
        
    end
    

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, XMapToolsApp, MaskFile, MapData, DensityMap)
            app.BingoAntidote_GUI.Visible = 'off';
            
            app.XMapToolsApp = XMapToolsApp;
            app.MaskFile = MaskFile;
            app.MapData = MapData;
            app.DensityMap = DensityMap;
            
            app.ProgramState = 1;
            
            
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            % Fortran libraries for Theriak
            setenv('DYLD_LIBRARY_PATH', '/usr/local/bin')
            setenv('GFORTRAN_STDIN_UNIT', '5')
            setenv('GFORTRAN_STDOUT_UNIT', '6')
            setenv('GFORTRAN_STDERR_UNIT', '0')
            
            app.LocBase = app.XMapToolsApp.config.bingoantidote.setup_path;
            app.TheriakBase = app.XMapToolsApp.config.bingoantidote.theriak_path;
            
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            % Oxyde names (to be updated later?)
            fid = fopen([app.LocBase,'/Dev/XTT_Def_OxideName.txt']); Compt = 0;
            while 1
                lalign = fgetl(fid);
                if isequal(lalign,'') || isequal(lalign,-1)
                    break
                end
                if ~isequal(lalign(1),'!')
                    Compt = Compt+1;
                    Temp = strread(lalign,'%s');
                    app.DefOxideName{Compt,1} = Temp{1};
                    app.DefOxideName{Compt,2} = Temp{2};
                    app.DefOxideName{Compt,3} = str2num(Temp{3});
                    app.DefOxideName{Compt,4} = str2num(Temp{4});
                    app.DefOxideName{Compt,5} = str2num(Temp{5});
                end
            end
            
            BingoInitialization(app);
            
            app.BingoDefault.Theriak.Path = fullfile(app.TheriakBase,'theriak');
            
            % if ispc
            %     app.BingoDefault.Theriak.Path = [app.TheriakBase,'\theriak'];
            % else
            %     app.BingoDefault.Theriak.Path = [app.TheriakBase,'/theriak'];
            % end
            
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            % Create the variable BinBulk(i) .Type
            %
            app.BinBulk(1).Type = 0;   % Bulk Composition
            app.BinBulk(1).Name = ''; % 'Bulk rock composition';
            app.BinBulk(1).CompositionOriginal = ''; % app.BingoDefault.Theriak.InputBulk;
            app.BinBulk(1).CompositionModified = ''; % app.BingoDefault.Theriak.InputBulk;
            app.BinBulk(1).CompositionIterative = ''; % app.BingoDefault.Theriak.InputBulk;
            app.BinBulk(1).Oxides = {};
            app.BinBulk(1).OxideCompositions = {};
            app.BinBulk(1).CompoDisp = 0; % 1;
            app.BinBulk(1).DomainXrefYref = [0 0];
            app.BinBulk(1).TypeROI = '';
            app.BinBulk(1).PositionROI = [];
            %app.BinBulk(1).SelectedPhases = 0;
            
            for i=1:length(app.BinBulk)
                ListNamesBulk{i} = app.BinBulk(i).Name;
            end
            %set(app.BinPopUpBulkCompoList,'String',ListNamesBulk);
            
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            % Create the variable SF (Structural formula) .DefOxide
            %                                             .DefElem
            %                                             .ListOxide
            %                                             .ListElem
            Compt = 0;
            PreviousElem = '';
            
            for i=1:size(app.DefOxideName,1)
                TheElem = app.DefOxideName{i,1};
                if ~isequal(TheElem,PreviousElem)
                    Compt = Compt+1;
                    PreviousElem = TheElem;
                    
                    app.SF.ListElem{Compt} = TheElem;
                    app.SF.DefElem(Compt).Name = TheElem;
                    
                end
                app.SF.DefOxide(Compt).NameOxi = app.DefOxideName{i,2};
                app.SF.DefOxide(Compt).IndiceOxi = i;
                app.SF.DefOxide(Compt).Elem = TheElem;
                app.SF.DefOxide(Compt).IndiceElem = Compt;
                app.SF.DefOxide(Compt).MolarMass = app.DefOxideName{i,3};
                app.SF.DefOxide(Compt).NbAtoms = app.DefOxideName{i,4};
                app.SF.DefOxide(Compt).NbOxygen = app.DefOxideName{i,5};
                
                app.SF.ListOxide{i} = app.DefOxideName{i,2};
                
                ListElemRepet{i} = TheElem;
                
            end
            
            for i=1:length(app.SF.DefElem)
                TheSum = find(ismember(ListElemRepet,app.SF.DefElem(i).Name));
                app.SF.DefElem(i).Sum = TheSum;
            end
            
            app.Compositions = [];
            
            % BinElData:
            ListElem = app.MapData.ElNames;
            
            for i=1:length(ListElem)
                
                [IsElemInSF,WhereOxideInSF] = ismember(upper(ListElem{i}),app.SF.ListOxide);
                
                if ~IsElemInSF
                    warndlg(['The Element ',char(ListElem{i}),' is not available in XThermoTools ...'],'Warning');
                    return
                end
                
                app.BinElData.listElem{i} = app.SF.DefOxide(WhereOxideInSF).Elem;
                app.BinElData.listOxides{i} = ListElem{i};
                app.BinElData.selected(i) = 1;
                app.BinElData.MolarMass(i) = app.SF.DefOxide(WhereOxideInSF).MolarMass;
                app.BinElData.NbAtoms(i) = app.SF.DefOxide(WhereOxideInSF).NbAtoms;
                app.BinElData.NbOxygen(i) = app.SF.DefOxide(WhereOxideInSF).NbOxygen;
            end
            
            
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            % Create the variable BinPhaseDef
            %
            %
            for i=2:length(app.MaskFile.Names)                              % we exluce "none" (new 2.0)
                app.BinPhaseDef(i-1).name = app.MaskFile.Names{i};
                app.BinPhaseDef(i-1).SelForBulk = 1;                        % default (new 2.0)
                app.BinPhaseDef(i-1).SelForBA = 1;                          % default (new 2.0)
                app.BinPhaseDef(i-1).IsPhaseIn = 0;
                app.BinPhaseDef(i-1).OxBasis = [];
                app.BinPhaseDef(i-1).ListVariAntidote = [];
                app.BinPhaseDef(i-1).ListWeightAntidote = [];
                app.BinPhaseDef(i-1).DBName = '';
                app.BinPhaseDef(i-1).DBMinName = '';
                app.BinPhaseDef(i-1).PhaseVolProp = [];
                app.BinPhaseDef(i-1).NbGrps = 0;
                app.BinPhaseDef(i-1).SelGrps = 0;
                app.BinPhaseDef(i-1).VolGrps = [];
                app.BinPhaseDef(i-1).Grps(1).IsGrp = 0;
                app.BinPhaseDef(i-1).Grps(1).Name = '';
                app.BinPhaseDef(i-1).Grps(1).XrefYref = [];
                app.BinPhaseDef(i-1).Grps(1).OxideCompositions = [];
                app.BinPhaseDef(i-1).Grps(1).SigmaOxide = [];
                app.BinPhaseDef(i-1).Grps(1).OxideLabels = {};
                app.BinPhaseDef(i-1).Grps(1).SFCompositions = [];
                app.BinPhaseDef(i-1).Grps(1).SigmaSF = [];
                app.BinPhaseDef(i-1).Grps(1).SFLabels = {};
                app.BinPhaseDef(i-1).Grps(1).TypeROI = '';
                app.BinPhaseDef(i-1).Grps(1).PositionROI = [];
            end
            
            CheckPhaseSolutionList(app);
            
            app.AssemblageCode = ones(1,6);
            
            
            % -----------------
            InitializeApp(app);
            
            app.FirstPlot = 1;
            
            PlotSelectedData(app);
            
            movegui(app.BingoAntidote_GUI,"center");
            app.BingoAntidote_GUI.Visible = 'on';
            
        end

        % Selection changed function: Tree_Elem
        function Tree_ElemSelectionChanged(app, event)
            PlotSelectedData(app);
        end

        % Selection changed function: Tree_Phases
        function Tree_PhasesSelectionChanged(app, event)
            PlotSelectedData(app);
        end

        % Value changed function: El10Button, El11Button, 
        % El12Button, El13Button, El14Button, El1Button, El2Button, 
        % El3Button, El4Button, El5Button, El6Button, El7Button, 
        % El8Button, El9Button
        function El1ButtonValueChanged(app, event)
            CheckElButtonSelection(app);
        end

        % Button pushed function: ApplyStep1Button
        function ApplyStep1ButtonPushed(app, event)
            app.ProgramState = 1.2;
            DatabaseListBoxValueChanged(app,0);
            CheckProgramState(app);
        end

        % Button pushed function: ApplyStep2Button
        function ApplyStep2ButtonPushed(app, event)
            app.ProgramState = 1.3;
            CheckProgramState(app);
            DefaultoptionsDropDownValueChanged(app);
        end

        % Value changed function: TminEditField
        function TminEditFieldValueChanged(app, event)
            UpdatePTsteps(app);
        end

        % Value changed function: TmaxEditField
        function TmaxEditFieldValueChanged(app, event)
            UpdatePTsteps(app);
        end

        % Value changed function: TstepEditField
        function TstepEditFieldValueChanged(app, event)
            UpdatePTsteps(app);
        end

        % Value changed function: PminEditField
        function PminEditFieldValueChanged(app, event)
            UpdatePTsteps(app);
        end

        % Value changed function: PmaxEditField
        function PmaxEditFieldValueChanged(app, event)
            UpdatePTsteps(app);
        end

        % Value changed function: PstepEditField
        function PstepEditFieldValueChanged(app, event)
            UpdatePTsteps(app);
        end

        % Button pushed function: ApplyStep3Button
        function ApplyStep3ButtonPushed(app, event)
            app.ProgramState = 1.4;
            CheckProgramState(app);
        end

        % Button pushed function: Settings_NextButton
        function Settings_NextButtonPushed(app, event)
            app.ProgramState = 2.0;
            app.TabGroup.SelectedTab = app.LBCTab;
            CheckProgramState(app);
            TabGroupSelectionChanged(app, 0);
        end

        % Button pushed function: ButtonAddROI
        function ButtonAddROIPushed(app, event)
            ROI_DeleteROI(app);
            
            DeactivatePlotZoomPanOptions(app);
            
            Method = app.MethodROIDropDown.Value;
            switch Method
                case 'Rectangle ROI'
                    app.SelectedROI = drawrectangle(app.UIAxes,'Color',[0.47,0.67,0.19],'InteractionsAllowed','all');
                    Coordinates = app.SelectedROI.Vertices;
                case 'Polygon ROI'
                    app.SelectedROI = drawpolygon(app.UIAxes,'Color',[0.47,0.67,0.19],'InteractionsAllowed','all');
                    Coordinates = app.SelectedROI.Position;
            end
            
            app.ROI_Bulk_Listener = addlistener(app.SelectedROI, 'ROIMoved', @(varargin)Bulk_ROI_changed_shape(app, app.SelectedROI));
            
            WhereItGoes = length(app.ROITree.Children) + 1;
            
            TempBinBulk.Type = 1;
            TempBinBulk.Name = ['LB_',num2str(WhereItGoes)];
            TempBinBulk.CompositionOriginal = 'Not yet available';
            TempBinBulk.CompositionModified = 'Not yet available';
            TempBinBulk.CompositionIterative = 'Not yet available';
            TempBinBulk.Oxides = {};
            TempBinBulk.OxideCompositions = {};
            TempBinBulk.CompoDisp = 1;
            TempBinBulk.DomainXrefYref = Coordinates;
            TempBinBulk.TypeROI = Method;
            TempBinBulk.PositionROI = app.SelectedROI.Position;
            
            app.WaitBar = uiprogressdlg(app.BingoAntidote_GUI,'Title','Bingo-Antidote','Indeterminate','on');
            app.WaitBar.Message = 'Generating a LBC, please wait...';
            
            [TempBinBulk] = BinUpdateBulkComposition(app,TempBinBulk,WhereItGoes);
            
            % Add Tree node:
            p = uitreenode(app.ROITree,'Text',char(TempBinBulk.Name),'NodeData',WhereItGoes);
            app.ROITree.SelectedNodes = app.ROITree.Children(WhereItGoes);
            
            app.BinBulk(WhereItGoes) = TempBinBulk;
            
            % Apply FMB settings: 
            [Bulk,app.BinBulk] = SuperFastBulkUpdate(app.BinGfDef,app.BinBulk,WhereItGoes);
            
            app.BinBulk(WhereItGoes).CompositionIterative = Bulk;
            
            close(app.WaitBar);
            
            ROITreeSelectionChanged(app);
            
            app.ProgramState = 2.1;
            CheckProgramState(app);
        end

        % Button pushed function: ButtonEliminateROI
        function ButtonEliminateROIPushed(app, event)
            % ...
            
            
            CheckProgramState(app);
        end

        % Selection changed function: ROITree
        function ROITreeSelectionChanged(app, event)
            selectedNodes = app.ROITree.SelectedNodes.NodeData;
            
            ROI_DeleteROI(app);
            
            TempBinBulk  = app.BinBulk(selectedNodes);
            
            Cell2Disp = cell(length(TempBinBulk.Oxides),2);
            for i = 1:length(TempBinBulk.Oxides)
                Cell2Disp{i,1} = TempBinBulk.Oxides{i};
                Cell2Disp{i,2} = TempBinBulk.OxideCompositions(i);
            end
            app.UITable.Data = Cell2Disp;
            app.LBCEditField.Value = TempBinBulk.CompositionIterative;
            
            switch app.BinBulk(selectedNodes).TypeROI
                case 'Rectangle ROI'
                    app.SelectedROI = drawrectangle(app.UIAxes,'Position',app.BinBulk(selectedNodes).PositionROI,'Color',[0.47,0.67,0.19],'InteractionsAllowed','all');
                case 'Polygon ROI'
                    app.SelectedROI = drawpolygon(app.UIAxes,'Position',app.BinBulk(selectedNodes).PositionROI,'Color',[0.47,0.67,0.19],'InteractionsAllowed','all');
            end
            
            app.ROI_Bulk_Listener = addlistener(app.SelectedROI, 'ROIMoved', @(varargin)Bulk_ROI_changed_shape(app, app.SelectedROI));
            
            % Check if fluids are optimized or not
            app.BinGfDef.Fluids.Optimize = 0;
            for i = 1:length(app.BinGfDef.Fluids.Spec)
                if app.BinGfDef.Fluids.Spec(i).Optimize
                    app.BinGfDef.Fluids.Optimize = 1;
                end
            end
            
        end

        % Button pushed function: LBC_NextButton
        function LBC_NextButtonPushed(app, event)
            app.ProgramState = 3.0;
            app.TabGroup.SelectedTab = app.PhasesTab;
            CheckProgramState(app);
            ROI_DeleteROI(app);
            % select first
            app.Tree_PhasesModel.SelectedNodes = app.Tree_PhasesModel.Children(1);
            Tree_PhasesModelSelectionChanged(app, 0);
        end

        % Selection changed function: Tree_PhasesModel
        function Tree_PhasesModelSelectionChanged(app, event)
            CheckPhaseSolutionList(app);
            
            %app.Tree_PhasesModel.SelectedNodes.NodeData;
            app.Tree_Phases.SelectedNodes = app.Tree_Phases.Children(app.Tree_PhasesModel.SelectedNodes.NodeData+1);
            PlotSelectedData(app);
            %keyboard
        end

        % Value changed function: ModelPhaseDropDown
        function ModelPhaseDropDownValueChanged(app, event)
            value = app.ModelPhaseDropDown.Value;
            
            SelMin = app.Tree_PhasesModel.SelectedNodes.NodeData;
            
            iDB = find(ismember(app.DatabaseListBox.Items,app.DatabaseListBox.Value));
            DefMin = app.BingoDefault.Theriak.Database(iDB).DefMin;
            
            for i=1:length(DefMin(:,1))
                OxygenRefMiner(i,1) = DefMin{i,2};
            end
            
            app.BinPhaseDef(SelMin).DBMinName = char(app.ModelPhaseDropDown.Items(value));
            app.BinPhaseDef(SelMin).OxBasis = OxygenRefMiner(value-1);
            app.BinPhaseDef(SelMin).ListVariAntidote = DefMin{value-1,4};
            
            CheckPhaseSolutionList(app);
        end

        % Button pushed function: ButtonAddROI_Phase
        function ButtonAddROI_PhasePushed(app, event)
            ROI_DeleteROI(app);
            
            DeactivatePlotZoomPanOptions(app);
            
            Method = app.MethodROIDropDown_Phase.Value;
            switch Method
                case 'Rectangle ROI'
                    app.SelectedROI = drawrectangle(app.UIAxes);
                    Coordinates = app.SelectedROI.Vertices;
                case 'Polygon ROI'
                    app.SelectedROI = drawpolygon(app.UIAxes);
                    Coordinates = app.SelectedROI.Position;
            end
            
            SelMin = app.Tree_PhasesModel.SelectedNodes.NodeData;
            GrpId = 1; % TEMPORARY (DEMO) % app.BinPhaseDef(SelMin).NbGrps + 1;
            
            % TEMPORARY (DEMO)
            for i = 1:length(app.TreeMinGroups.Children)
                app.TreeMinGroups.Children(i).delete;
            end
            
            app.BinPhaseDef(SelMin).NbGrps = GrpId;
            app.BinPhaseDef(SelMin).SelGrps = GrpId;
            app.BinPhaseDef(SelMin).VolGrps(GrpId) = 0;
            
            app.BinPhaseDef(SelMin).Grps(GrpId).IsGrp = 1;
            app.BinPhaseDef(SelMin).Grps(GrpId).Name = ['Grp_',num2str(GrpId)];
            
            app.BinPhaseDef(SelMin).Grps(GrpId).XrefYref = Coordinates;
            app.BinPhaseDef(SelMin).Grps(GrpId).OxideCompositions = [];
            app.BinPhaseDef(SelMin).Grps(GrpId).SigmaOxide = [];
            app.BinPhaseDef(SelMin).Grps(GrpId).OxideLabels = {};
            app.BinPhaseDef(SelMin).Grps(GrpId).SFCompositions = [];
            app.BinPhaseDef(SelMin).Grps(GrpId).SigmaSF = [];
            app.BinPhaseDef(SelMin).Grps(GrpId).SFLabels = {};
            app.BinPhaseDef(SelMin).Grps(GrpId).TypeROI = Method;
            app.BinPhaseDef(SelMin).Grps(GrpId).PositionROI = app.SelectedROI.Position;
            
            
            SelPixels = BinUpdatePhaseDef(app,SelMin,GrpId);
            
            if isempty(SelPixels)
                ROI_DeleteROI(app);
                return
            end
            
            p = uitreenode(app.TreeMinGroups,'Text',char(app.BinPhaseDef(SelMin).Grps(GrpId).Name),'NodeData',GrpId);
            app.TreeMinGroups.SelectedNodes = app.TreeMinGroups.Children(GrpId);
            
            CheckPhaseSolutionList(app);
            
        end

        % Button pushed function: ButtonEliminateROI_Phase
        function ButtonEliminateROI_PhasePushed(app, event)
            
        end

        % Selection changed function: TreeMinGroups
        function TreeMinGroupsSelectionChanged(app, event)
            selectedNodes = app.TreeMinGroups.SelectedNodes;
            
            ROI_DeleteROI(app);
            
            SelMin = app.Tree_PhasesModel.SelectedNodes.NodeData;
            GrpId = app.TreeMinGroups.SelectedNodes.NodeData;
            
            Method = app.BinPhaseDef(SelMin).Grps(GrpId).TypeROI;
            switch Method
                case 'Rectangle ROI'
                    app.SelectedROI = drawrectangle(app.UIAxes,'Position',app.BinPhaseDef(SelMin).Grps(GrpId).PositionROI);
                case 'Polygon ROI'
                    app.SelectedROI = drawpolygon(app.UIAxes,'Position',app.BinPhaseDef(SelMin).Grps(GrpId).PositionROI);
            end
            
            
        end

        % Button pushed function: Phase_NextButton
        function Phase_NextButtonPushed(app, event)
            app.ProgramState = 4.0;
            app.TabGroup.SelectedTab = app.BingoTab;
            CheckProgramState(app);
            ROI_DeleteROI(app);
            
            expand(app.GlobalinversionNode);
            app.AntidoteTree.SelectedNodes = app.Recipe1_Node;
            TabGroupSelectionChanged(app, 0);
        end

        % Value changed function: SelectforBACheckBox
        function SelectforBACheckBoxValueChanged(app, event)
            SelMin = app.Tree_PhasesModel.SelectedNodes.NodeData;
            app.BinPhaseDef(SelMin).SelForBA = app.SelectforBACheckBox.Value;
            
            CheckPhaseSolutionList(app);
        end

        % Value changed function: SelectforLBCCheckBox
        function SelectforLBCCheckBoxValueChanged(app, event)
            SelMin = app.Tree_PhasesModel.SelectedNodes.NodeData;
            app.BinPhaseDef(SelMin).SelForBulk = app.SelectforLBCCheckBox.Value;
            
            CheckPhaseSolutionList(app);
            
            % Here we need to re-calculate the local bulk composition
            for i = 1:length(app.BinBulk)
                TempBinBulk  = app.BinBulk(i);
                [TempBinBulk] = BinUpdateBulkComposition(app,TempBinBulk,i);
                app.BinBulk(i) = TempBinBulk;
            end
            
        end

        % Value changed function: TemperatureEditField
        function TemperatureEditFieldValueChanged(app, event)
            app.BingoTemperatureEditField.Value = app.TemperatureEditField.Value;
        end

        % Value changed function: PressureEditField
        function PressureEditFieldValueChanged(app, event)
            app.BingoPressureEditField.Value = app.PressureEditField.Value;
        end

        % Button pushed function: BingoButton
        function BingoButtonPushed(app, event)
            CheckFiles4Theriak(app);
            
            app.WaitBar = uiprogressdlg(app.BingoAntidote_GUI,'Title','Bingo-Antidote','Indeterminate','on');
            app.WaitBar.Message = 'Theriak and Bingo are running numbers';
            
            app.Report_Bingo = {};
            app.Report_Bingo{end+1} = ['-----------------------------------------------------'];
            app.Report_Bingo{end+1} = ['>>> New BINGO Run: ',datestr(now),'  <<<'];
            app.Report_Bingo{end+1} = [' '];
            app.Text_Report.Value = app.Report_Bingo;
            
            load('MinimOptions.mat');
            
            [WorkVariXMap] = GenerateResXMap(app.BinPhaseDef);
            
            [BinSet] = SetBin(app);
            
            app.Report_Bingo{end+1} = ['... XBIN has been updated by Bingo - database: ',char(BinSet.Database),' ...'];
            app.Report_Bingo{end+1} = [' '];
            app.Text_Report.Value = app.Report_Bingo;
            
            D_Temp = num2str(app.BingoTemperatureEditField.Value);
            D_Press = num2str(app.BingoPressureEditField.Value * 1e4);
            
            plot(app.UIAxes2,app.BingoTemperatureEditField.Value,app.BingoPressureEditField.Value,'ok')
            axis(app.UIAxes2,[app.TminEditField.Value,app.TmaxEditField.Value,app.PminEditField.Value,app.PmaxEditField.Value])
            
            
            app.Report_Bingo{end+1} = ['Bulk: ',BinSet.Bulk2Display];
            app.Report_Bingo{end+1} = ['Database: ',BinSet.Database];
            app.Report_Bingo{end+1} = ['P(GPa): ',num2str(app.BingoPressureEditField.Value)];
            app.Report_Bingo{end+1} = ['P(bar): ',D_Press];
            app.Report_Bingo{end+1} = ['T(C): ',num2str(app.BingoTemperatureEditField.Value)];
            app.Report_Bingo{end+1} = [' '];
            app.Text_Report.Value = app.Report_Bingo;
            
            % -------------------------------------------------------------------------
            % (-) Call Theriak
            WorkVariMod = TheriakCall(BinSet,D_Temp,D_Press);
            
            % -------------------------------------------------------------------------
            % (-) Call Bingo
            DoWePrint = 1;
            
            Report.Mode = 'BINGO';
            Report.Tdeg = D_Temp;
            Report.Pbar = D_Press;
            
            cla(app.UIAxes2,'reset');
            plot(app.UIAxes2,app.BingoTemperatureEditField.Value,app.BingoPressureEditField.Value,'ok')
            axis(app.UIAxes2,[app.TminEditField.Value,app.TmaxEditField.Value,app.PminEditField.Value,app.PmaxEditField.Value]);
            hold(app.UIAxes2,'on')
            xlabel(app.UIAxes2,'Temperature (°C)'), ylabel(app.UIAxes2,'Pressure (GPa)')
            drawnow

            UpdateText2Disp = 1;
            
            [A1,V1,C1,TOTAL2,Report] = BingoCall(WorkVariMod,WorkVariXMap,Report,DoWePrint,UpdateText2Disp,app);
            
            app.Gaude_Qasm.Value = A1;
            app.Gauge_Qvol.Value = V1;
            app.Gauge_Qcmp.Value = C1;
            
            app.EditField_Qasm.Value = A1;
            app.EditField_Qvol.Value = V1;
            app.EditField_Qcmp.Value = C1;
            
            app.Gauge_Qtotal.Value = TOTAL2;
            app.EditField_Qtotal.Value = TOTAL2;
            
            TheDate = datestr(now);
            
            app.TabGroup2.SelectedTab = app.ResultsTab;
            
            app.Report_Bingo{end+1} = ['>>> End BINGO Run: ',TheDate,'  <<<'];
            app.Report_Bingo{end+1} = ['-----------------------------------------------------'];
            app.Text_Report.Value = app.Report_Bingo;
            
            hp1 = pie(app.Result_Plot1,Report.VolXMap+0.000000000001,Report.TheNamesPlotMap);
            set(hp1(2:2:end),'FontSize',9);
            colormap(app.Result_Plot1,RdYlBu(length(Report.VolXMap)))
            title(app.Result_Plot1,'Observations (map)','FontSize',13);
            
            hp2 = pie(app.Result_Plot2,Report.VolTher+0.000000000001,Report.TheNamesPlotTher);
            set(hp2(2:2:end),'FontSize',9);
            colormap(app.Result_Plot2,RdYlBu(length(Report.VolXMap)))
            title(app.Result_Plot2,'Model (Theriak)','FontSize',13);
            
            close(app.WaitBar);

            pause(0.1)
            scroll(app.Text_Report,'bottom');
            
            
        end

        % Selection changed function: AntidoteTree
        function AntidoteTreeSelectionChanged(app, event)
            NodeData = app.AntidoteTree.SelectedNodes.NodeData;
            
            app.AntidoteButton.Enable = 'on';
            
            app.SelectedPhaseLabel.Visible = 'off';
            app.SelectedPhaseOptiDropDown.Visible = 'off';
            
            if isequal(NodeData,[1,1])
                % RECIPE 1 – Find optimal P–T(–X) 
                app.TabAntidote.SelectedTab = app.InversionTab;
                app.LiveDisplaySwitch.Value = 'On';
            end
            if isequal(NodeData,[1,2])
                % RECIPE 2 – P–T map of Q factors
                app.TabAntidote.SelectedTab = app.InversionTab;
            end
            if isequal(NodeData,[1,3])
                % RECIPE 3 – P–T uncertainty
                app.TabAntidote.SelectedTab = app.MCTab;
            end
            if isequal(NodeData,[2,1]) || isequal(NodeData,[2,2]) || isequal(NodeData,[2,3])
                % RECIPE 1 – Find optimal P–T(–X) 
                app.TabAntidote.SelectedTab = app.InversionTab;
                app.SelectedPhaseLabel.Visible = 'on';
                app.SelectedPhaseOptiDropDown.Visible = 'on';
            end
            if isequal(NodeData,[3,1]) || isequal(NodeData,[3,2]) || isequal(NodeData,[3,3])
                % RECIPE 7-8-9 – Uncertainty
                app.TabAntidote.SelectedTab = app.MCTab;
            end
            if isequal(NodeData,[4,1]) || isequal(NodeData,[4,2]) || isequal(NodeData,[4,3]) || isequal(NodeData,[4,4])
                % RECIPE 10-11-12-13 – Textural investigation
                app.TabAntidote.SelectedTab = app.ROITab;
                ROI_DeleteROI(app);
                if isequal(NodeData,[4,1])
                    app.TextureROITree.Enable = 'on';
                    app.ButtonROI_Add.Enable = 'on';
                    
                    %
                    app.NbstepspathLabel.Text = 'Nb steps / path';
                    app.AntidoteROIparamNbStepsEditField.Value = 20;
                    
                    if length(app.TextureROITree.Children) >= 2
                        app.ButtonROI_Play.Enable = 'on';
                    elseif length(app.TextureROITree.Children) >= 1
                        app.ButtonROI_Eliminate.Enable = 'on';
                        app.ButtonROI_Duplicate.Enable = 'on';
                        app.ButtonROI_Play.Enable = 'off';
                    else
                        app.ButtonROI_Play.Enable = 'off';
                        app.ButtonROI_Eliminate.Enable = 'off';
                        app.ButtonROI_Duplicate.Enable = 'off';
                    end
                    
                else
                    app.TextureROITree.Enable = 'off';
                    app.ButtonROI_Add.Enable = 'off';
                    app.ButtonROI_Eliminate.Enable = 'off';
                    
                    app.ButtonROI_Duplicate.Enable = 'off';
                    app.ButtonROI_Play.Enable = 'off';
                    
                    if isequal(NodeData,[4,2]) || isequal(NodeData,[4,3]) || isequal(NodeData,[4,4])
                        app.ButtonROI_Play.Enable = 'on';
                    else
                        app.ButtonROI_Play.Enable = 'off';
                    end
                    
                    if length(app.TextureROI.ROIdata) > 1 && isequal(app.AntidoteTree.SelectedNodes.NodeData,[4,1])
                        app.AntidoteButton.Enable = 'on';
                    else
                        app.AntidoteButton.Enable = 'off';
                    end
                    if isequal(NodeData,[4,2])
                        %
                        app.NbstepspathLabel.Text = 'Grid res. (pixels)';
                        app.AntidoteROIparamNbStepsEditField.Value = 300;
                        
                        app.AntidoteButton.Enable = 'on';
                    end
                    if isequal(NodeData,[4,3])
                        %
                        app.NbstepspathLabel.Text = 'Nb steps';
                        app.AntidoteROIparamNbStepsEditField.Value = 5;
                        
                        app.AntidoteButton.Enable = 'on';
                    end
                    if isequal(NodeData,[4,4])
                        %
                        app.NbstepspathLabel.Text = 'Grid res. (pixels)';
                        app.AntidoteROIparamNbStepsEditField.Value = 200;
                        
                        app.AntidoteButton.Enable = 'on';
                    end
                    
                end
                
                
                
            else
                app.TextureROITree.Enable = 'off';
                app.ButtonROI_Add.Enable = 'off';
                app.ButtonROI_Eliminate.Enable = 'off';
                app.ButtonROI_Play.Enable = 'off';
                app.ButtonROI_Duplicate.Enable = 'off';
            end
            if isequal(NodeData,[5,1]) || isequal(NodeData,[5,3])
                % RECIPE 14 – Scanning H (fixed P–T)
                app.TabAntidote.SelectedTab = app.InversionTab;
            end
            
            
            % TEMPORARY (demo)
            if isequal(NodeData,[5,2])
                app.AntidoteButton.Enable = 'off';
            end
            
            
            
        end

        % Button pushed function: AntidoteButton
        function AntidoteButtonPushed(app, event)
            CheckFiles4Theriak(app);
            
            app.WaitBar = uiprogressdlg(app.BingoAntidote_GUI,'Title','Bingo-Antidote','Indeterminate','on');
            app.WaitBar.Message = 'Theriak and Antidote are running numbers';
            
            delete(findall(app.UIAxes, 'Type',  'line'));
            
            cla(app.UIAxes_LiveAntidote1,'reset');
            cla(app.UIAxes_LiveAntidote2,'reset');
            cla(app.UIAxes_LiveAntidote3,'reset');
            cla(app.UIAxes_LiveAntidote4,'reset');
            
            app.TabGroup2.SelectedTab = app.LiveTab;
            
            %HTML_1 = '<!DOCTYPE html> <html> <head> <style> body { background-color: white; font-size: 0.7em; white-space: nowrap; font-family: Courier New;} </style> </head> <body>';
            %HTML_2 = '</body> </html>';
            
            app.Report_Antidote = {};
            app.Report_Antidote{end+1} = '-----------------------------------------------------';
            app.Report_Antidote{end+1} = ['>>> New ANTIDOTE job: ',datestr(now),'  <<<'];
            app.Report_Antidote{end+1} = '';
            app.Text_Report_Antidote.Value = app.Report_Antidote;
            
            load('MinimOptions.mat');
            
            [WorkVariXMap] = GenerateResXMap(app.BinPhaseDef);
            
            NodeData = app.AntidoteTree.SelectedNodes.NodeData;
            
            if isequal(NodeData,[1,1])
                % RECIPE 1 – Find optimal P–T(–X) 
                
                app.UIAxes_LiveAntidote2.Visible = 'off';
                app.UIAxes_LiveAntidote3.Visible = 'off';
                app.UIAxes_LiveAntidote4.Visible = 'off';
                drawnow
                
                [Output] = Antidote_1_OptPTX(WorkVariXMap,MinimOptions,app); 
                
                if Output.WeCallBingo
                    BingoButtonPushed(app,0);
                    app.TabGroup.SelectedTab = app.BingoTab;
                end
            end
            if isequal(NodeData,[1,2])
                % RECIPE 2 – P–T map of Q factors
                
                app.LiveDisplaySwitch.Value = 'Off';
                
                app.UIAxes_LiveAntidote2.Visible = 'on';
                app.UIAxes_LiveAntidote3.Visible = 'on';
                app.UIAxes_LiveAntidote4.Visible = 'on';
                drawnow
                
                [Output] = Antidote_2_MapQ(WorkVariXMap,MinimOptions,app);
                
            end
            
            if isequal(NodeData,[1,3])
                % RECIPE 3 – P–T uncertainty
                
                app.LiveDisplaySwitch.Value = 'Off';
                
                app.UIAxes_LiveAntidote2.Visible = 'on';
                app.UIAxes_LiveAntidote3.Visible = 'on';
                app.UIAxes_LiveAntidote4.Visible = 'on';
                drawnow
                
                [Output] = Antidote_3a6_PTunc('All',WorkVariXMap,MinimOptions,app);
                
            end
            
            if isequal(NodeData,[2,1])
                % RECIPE 4 – Find Optimal P-T (single phase)
                
                app.UIAxes_LiveAntidote2.Visible = 'off';
                app.UIAxes_LiveAntidote3.Visible = 'off';
                app.UIAxes_LiveAntidote4.Visible = 'off';
                drawnow
                
                [Output] = Antidote_4_OptPTX(WorkVariXMap,MinimOptions,app);
                
                if Output.WeCallBingo
                    BingoButtonPushed(app,0);
                    app.TabGroup.SelectedTab = app.BingoTab;
                end
            end
            
            if isequal(NodeData,[2,2])
                % RECIPE 5 – P–T map of Q factors (single phase)
                
                app.LiveDisplaySwitch.Value = 'Off';
                
                app.UIAxes_LiveAntidote2.Visible = 'off';
                app.UIAxes_LiveAntidote3.Visible = 'off';
                app.UIAxes_LiveAntidote4.Visible = 'off';
                drawnow
                
                [Output] = Antidote_5_MapQ(WorkVariXMap,MinimOptions,app);
                
            end
            
            if isequal(NodeData,[2,3])
                % RECIPE 6 – P–T uncertainty
                
                app.LiveDisplaySwitch.Value = 'Off';
                
                app.UIAxes_LiveAntidote2.Visible = 'on';
                app.UIAxes_LiveAntidote3.Visible = 'on';
                app.UIAxes_LiveAntidote4.Visible = 'on';
                drawnow
                
                [Output] = Antidote_3a6_PTunc('Cmp',WorkVariXMap,MinimOptions,app);
                
            end
            
            if isequal(NodeData,[3,1])
                % RECIPE 7 – Bulk sensitivity
                
                app.LiveDisplaySwitch.Value = 'Off';
                
                app.UIAxes_LiveAntidote2.Visible = 'off';
                app.UIAxes_LiveAntidote3.Visible = 'off';
                app.UIAxes_LiveAntidote4.Visible = 'off';
                drawnow
                
                [Output] = Antidote_7a8a9_Unc('Bulk',WorkVariXMap,MinimOptions,app);
            end
            
            if isequal(NodeData,[3,2])
                % RECIPE 8 – P–T sensitivity
                
                app.LiveDisplaySwitch.Value = 'Off';
                
                app.UIAxes_LiveAntidote2.Visible = 'off';
                app.UIAxes_LiveAntidote3.Visible = 'off';
                app.UIAxes_LiveAntidote4.Visible = 'off';
                drawnow
                
                [Output] = Antidote_7a8a9_Unc('PT',WorkVariXMap,MinimOptions,app);
            end
            
            if isequal(NodeData,[3,3])
                % RECIPE 9 – P–T-bulk sensitivity
                
                app.LiveDisplaySwitch.Value = 'Off';
                
                app.UIAxes_LiveAntidote2.Visible = 'off';
                app.UIAxes_LiveAntidote3.Visible = 'off';
                app.UIAxes_LiveAntidote4.Visible = 'off';
                drawnow
                
                [Output] = Antidote_7a8a9_Unc('Bulk+PT',WorkVariXMap,MinimOptions,app);
            end
            
            if isequal(NodeData,[4,1])
                % RECIPE 10 – Floating window (fixed P–T, variable bulk)
                
                app.TabGroup2.SelectedTab = app.MapTab;
                
                [Output] = Antidote_10_Float(WorkVariXMap,MinimOptions,app);

            end
            
            if isequal(NodeData,[4,2])
                % RECIPE 11 – Scanning window (find optimal P–T, variable bulk)
                
                app.UIAxes_LiveAntidote2.Visible = 'off';
                app.UIAxes_LiveAntidote3.Visible = 'off';
                app.UIAxes_LiveAntidote4.Visible = 'off';
                
                app.TabGroup2.SelectedTab = app.MapTab;
                
                [Output] = Antidote_11a12_Wind('Scan',WorkVariXMap,MinimOptions,app);
                % [Output] = Antidote_11_ScanWin(WorkVariXMap,MinimOptions,app);

            end
            
            if isequal(NodeData,[4,3])
                % RECIPE 11 – Scanning window (find optimal P–T, variable bulk)
                
                app.UIAxes_LiveAntidote2.Visible = 'off';
                app.UIAxes_LiveAntidote3.Visible = 'off';
                app.UIAxes_LiveAntidote4.Visible = 'off';
                
                app.TabGroup2.SelectedTab = app.MapTab;
                
                [Output] = Antidote_11a12_Wind('Growth',WorkVariXMap,MinimOptions,app);

            end
            
            if isequal(NodeData,[4,4])
                % RECIPE 13 – Chemical potential mapping (fixed P–T)
                
                app.TabGroup2.SelectedTab = app.MapTab;
                
                [Output] = Antidote_13_ChemPot(WorkVariXMap,MinimOptions,app);

            end
            
            
            if isequal(NodeData,[5,1])
                % RECIPE 14 – Scanning H (fixed P–T)
                
                app.LiveDisplaySwitch.Value = 'Off';
                
                app.UIAxes_LiveAntidote2.Visible = 'on';
                app.UIAxes_LiveAntidote3.Visible = 'on';
                app.UIAxes_LiveAntidote4.Visible = 'on';
                drawnow
                
                [Output] = Antidote_14a15a16_ScanX('H',WorkVariXMap,MinimOptions,app);
                
            end
            
            if isequal(NodeData,[5,3])
                % RECIPE 14 – Scanning O (fixed P–T)
                
                app.LiveDisplaySwitch.Value = 'Off';
                
                app.UIAxes_LiveAntidote2.Visible = 'on';
                app.UIAxes_LiveAntidote3.Visible = 'on';
                app.UIAxes_LiveAntidote4.Visible = 'on';
                drawnow
                
                [Output] = Antidote_14a15a16_ScanX('O',WorkVariXMap,MinimOptions,app);
                
            end
            
            %keyboard
            
            %app.AntidoteTree.SelectedNodes.NodeData
            
            
            pause(0.1)
            scroll(app.Text_Report_Antidote,'bottom');
            
            
            close(app.WaitBar);
            
        end

        % Value changed function: DatabaseListBox
        function DatabaseListBoxValueChanged(app, event)
            
            iDB = find(ismember(app.DatabaseListBox.Items,app.DatabaseListBox.Value));
            
            DefGf = app.BingoDefault.Theriak.Database(iDB).DefGf;
            
            % reset BinGfDef
            app.BinGfDef.Fluids.Activate = 0;
            app.BinGfDef.Fluids.Optimize = 0;
            app.BinGfDef.Fluids.Spec(1).IsActive = [];
            app.BinGfDef.Fluids.Spec(1).Optimize = [];
            app.BinGfDef.Fluids.Spec(1).Name = '';
            app.BinGfDef.Fluids.Spec(1).DBMinName = '';
            app.BinGfDef.Fluids.Spec(1).DBName = '';
            app.BinGfDef.Fluids.Spec(1).ListVariAntidote = [];
            app.BinGfDef.Fluids.Spec(1).ListWeightAntidote = 0;
            app.BinGfDef.Fluids.Spec(1).Bulk = 0.5;
            app.BinGfDef.Fluids.Spec(1).Lower = 0.0001;
            app.BinGfDef.Fluids.Spec(1).Upper = 1;
            
            app.BinGfDef.Melt.Activate = 0;
            app.BinGfDef.Melt.Include = 0;
            app.BinGfDef.Melt.Name = '';                  % we can have only one melt model
            app.BinGfDef.Melt.DBName = '';
            
            app.BinGfDef.Oxygen.ActivateExtraO = 0;
            app.BinGfDef.Oxygen.OptimizeExtraO = 0;
            app.BinGfDef.Oxygen.ExtraO.Bulk = 0;
            app.BinGfDef.Oxygen.ExtraO.Lower = 0.0001;
            app.BinGfDef.Oxygen.ExtraO.Upper = 1;
            
            app.BinGfDef.Oxygen.ActivateBuffer = 0;
            app.BinGfDef.Oxygen.Buffer.Name = '';
            app.BinGfDef.Oxygen.Buffer.DBName = '';
            
            ComptFSpec = 0;
            ComptBuffer = 0;
            
            for i = 1:size(DefGf,1)
                
                Name = DefGf{i,1};
                NameS = strread(Name,'%s','delimiter','_');
                
                if iscell(NameS)
                    if isequal(NameS{1},'Fluid')
                        ComptFSpec = ComptFSpec +1;
                        
                        app.BinGfDef.Fluids.Spec(ComptFSpec).IsActive = 0;
                        app.BinGfDef.Fluids.Spec(ComptFSpec).Optimize = 0;
                        app.BinGfDef.Fluids.Spec(ComptFSpec).Name = DefGf{i,1};
                        app.BinGfDef.Fluids.Spec(ComptFSpec).DBMinName = DefGf{i,3};
                        app.BinGfDef.Fluids.Spec(ComptFSpec).DBName = app.DatabaseListBox.Value;
                        app.BinGfDef.Fluids.Spec(ComptFSpec).ListVariAntidote = DefGf{i,4};
                        app.BinGfDef.Fluids.Spec(ComptFSpec).ListWeightAntidote = ''; %DefGf{i,5};
                        app.BinGfDef.Fluids.Spec(ComptFSpec).Bulk = 0;
                        app.BinGfDef.Fluids.Spec(ComptFSpec).Lower = 0.0001;
                        app.BinGfDef.Fluids.Spec(ComptFSpec).Upper = 1;
                        
                        
                    elseif isequal(NameS{1},'Buffer')
                        
                        ComptBuffer = ComptBuffer+1;
                        
                        app.BinGfDef.Oxygen.ActivateBuffer = 0;
                        app.BinGfDef.Oxygen.Buffer.Name = DefGf{i,1};
                        app.BinGfDef.Oxygen.Buffer.DBName = DefGf{i,3};
                        
                    end
                    
                end
                
                if isequal(Name,'Melt')
                    
                    app.BinGfDef.Melt.Activate = 0;
                    app.BinGfDef.Melt.Include = 0;
                    app.BinGfDef.Melt.Name = DefGf{i,1};
                    app.BinGfDef.Melt.DBName = DefGf{i,3};
                    
                    % Option to compare the composition to be added later on!
                end
                
                % New PL october 2022:
                app.BinGfDef.Fluids.Optimize = zeros(1,ComptFSpec);
                
                % New PL july 2019 to start with H:
                app.BinGfDef.Fluids.Activate = 1;
                app.BinGfDef.Fluids.Spec(1).IsActive = 1;
                app.BinGfDef.Fluids.Spec(1).Bulk = 1;
            end
            
            UpdateFluidMeltBufferPanel(app);
            
            Results = CheckFiles4Theriak(app);
            
            if app.ProgramState > 2.0 && app.ProgramState <= 3.0
                uialert(app.BingoAntidote_GUI,{'A new database is selected:','- Check the fluid/melt options in the workspace LCB'},'Warning');
            elseif app.ProgramState > 3.0
                uialert(app.BingoAntidote_GUI,{'A new database is selected:','- Check the fluid/melt options in the workspace LCB','- Check the mineral definitions in the workspace Phases'},'Warning');
                CheckPhaseSolutionList(app);
            end
            
            
        end

        % Selection changed function: Tree_FluidModels
        function Tree_FluidModelsSelectionChanged(app, event)
            UpdateDisplayedFluid(app);
        end

        % Value changed function: ActivateFluidSpecie_CheckBox
        function ActivateFluidSpecie_CheckBoxValueChanged(app, event)
            FluidID = app.Tree_FluidModels.SelectedNodes.NodeData;
            app.BinGfDef.Fluids.Spec(FluidID).IsActive = app.ActivateFluidSpecie_CheckBox.Value;
            
            if ~isempty(app.ROITree.SelectedNodes)
                WhereItGoes = app.ROITree.SelectedNodes.NodeData;
                [Bulk,app.BinBulk] = SuperFastBulkUpdate(app.BinGfDef,app.BinBulk,WhereItGoes);
                app.BinBulk(WhereItGoes).CompositionIterative = Bulk;
                ROITreeSelectionChanged(app);
            end
        end

        % Value changed function: OptimizeNFluid_CheckBox
        function OptimizeNFluid_CheckBoxValueChanged(app, event)
            FluidID = app.Tree_FluidModels.SelectedNodes.NodeData;
            app.BinGfDef.Fluids.Spec(FluidID).Optimize = app.OptimizeNFluid_CheckBox.Value;
            
            if ~isempty(app.ROITree.SelectedNodes)
                WhereItGoes = app.ROITree.SelectedNodes.NodeData;
                [Bulk,app.BinBulk] = SuperFastBulkUpdate(app.BinGfDef,app.BinBulk,WhereItGoes);
                app.BinBulk(WhereItGoes).CompositionIterative = Bulk;
                ROITreeSelectionChanged(app);
            end
        end

        % Value changed function: ActivateMeltModel_CheckBox
        function ActivateMeltModel_CheckBoxValueChanged(app, event)
            app.BinGfDef.Melt.Activate = app.ActivateMeltModel_CheckBox.Value;
        end

        % Value changed function: MeltModelPhase_DropDown
        function MeltModelPhase_DropDownValueChanged(app, event)
            value = app.MeltModelPhase_DropDown.Value;
        end

        % Value changed function: IncudeMeltEvaluation_CheckBox
        function IncudeMeltEvaluation_CheckBoxValueChanged(app, event)
            app.BinGfDef.Melt.Include = app.IncudeMeltEvaluation_CheckBox.Value;
        end

        % Value changed function: MinNfluid_EditField
        function MinNfluid_EditFieldValueChanged(app, event)
            FluidID = app.Tree_FluidModels.SelectedNodes.NodeData;
            app.BinGfDef.Fluids.Spec(FluidID).Lower = app.MinNfluid_EditField.Value;
            
            if ~isempty(app.ROITree.SelectedNodes)
                WhereItGoes = app.ROITree.SelectedNodes.NodeData;
                [Bulk,app.BinBulk] = SuperFastBulkUpdate(app.BinGfDef,app.BinBulk,WhereItGoes);
                app.BinBulk(WhereItGoes).CompositionIterative = Bulk;
                ROITreeSelectionChanged(app);
            end
        end

        % Value changed function: MaxNfluid_EditField
        function MaxNfluid_EditFieldValueChanged(app, event)
            FluidID = app.Tree_FluidModels.SelectedNodes.NodeData;
            app.BinGfDef.Fluids.Spec(FluidID).Upper = app.MaxNfluid_EditField.Value;
            
            if ~isempty(app.ROITree.SelectedNodes)
                WhereItGoes = app.ROITree.SelectedNodes.NodeData;
                [Bulk,app.BinBulk] = SuperFastBulkUpdate(app.BinGfDef,app.BinBulk,WhereItGoes);
                app.BinBulk(WhereItGoes).CompositionIterative = Bulk;
                ROITreeSelectionChanged(app);
            end
        end

        % Value changed function: BulkNfluid_EditField
        function BulkNfluid_EditFieldValueChanged(app, event)
            FluidID = app.Tree_FluidModels.SelectedNodes.NodeData;
            app.BinGfDef.Fluids.Spec(FluidID).Bulk = app.BulkNfluid_EditField.Value;
            
            if ~isempty(app.ROITree.SelectedNodes)
                WhereItGoes = app.ROITree.SelectedNodes.NodeData;
                [Bulk,app.BinBulk] = SuperFastBulkUpdate(app.BinGfDef,app.BinBulk,WhereItGoes);
                app.BinBulk(WhereItGoes).CompositionIterative = Bulk;
                ROITreeSelectionChanged(app);
            end
        end

        % Value changed function: ActivateExtraOxygen_CheckBox
        function ActivateExtraOxygen_CheckBoxValueChanged(app, event)
            app.BinGfDef.Oxygen.ActivateExtraO = app.ActivateExtraOxygen_CheckBox.Value;
            
            if ~isempty(app.ROITree.SelectedNodes)
                WhereItGoes = app.ROITree.SelectedNodes.NodeData;
                [Bulk,app.BinBulk] = SuperFastBulkUpdate(app.BinGfDef,app.BinBulk,WhereItGoes);
                app.BinBulk(WhereItGoes).CompositionIterative = Bulk;
                ROITreeSelectionChanged(app);
            end
        end

        % Value changed function: BulkO_EditField
        function BulkO_EditFieldValueChanged(app, event)
            app.BinGfDef.Oxygen.ExtraO.Bulk = app.BulkO_EditField.Value;
            
            if ~isempty(app.ROITree.SelectedNodes)
                WhereItGoes = app.ROITree.SelectedNodes.NodeData;
                [Bulk,app.BinBulk] = SuperFastBulkUpdate(app.BinGfDef,app.BinBulk,WhereItGoes);
                app.BinBulk(WhereItGoes).CompositionIterative = Bulk;
                ROITreeSelectionChanged(app);
            end
        end

        % Value changed function: OptimizeO_CheckBox
        function OptimizeO_CheckBoxValueChanged(app, event)
            app.BinGfDef.Oxygen.OptimizeExtraO = app.OptimizeO_CheckBox.Value;
            
            if ~isempty(app.ROITree.SelectedNodes)
                WhereItGoes = app.ROITree.SelectedNodes.NodeData;
                [Bulk,app.BinBulk] = SuperFastBulkUpdate(app.BinGfDef,app.BinBulk,WhereItGoes);
                app.BinBulk(WhereItGoes).CompositionIterative = Bulk;
                ROITreeSelectionChanged(app);
            end
        end

        % Value changed function: OxMin_EditField
        function OxMin_EditFieldValueChanged(app, event)
            app.BinGfDef.Oxygen.ExtraO.Lower = app.OxMin_EditField.Value;
            
            if ~isempty(app.ROITree.SelectedNodes)
                WhereItGoes = app.ROITree.SelectedNodes.NodeData;
                [Bulk,app.BinBulk] = SuperFastBulkUpdate(app.BinGfDef,app.BinBulk,WhereItGoes);
                app.BinBulk(WhereItGoes).CompositionIterative = Bulk;
                ROITreeSelectionChanged(app);
            end
        end

        % Value changed function: OxMax_EditField
        function OxMax_EditFieldValueChanged(app, event)
            app.BinGfDef.Oxygen.ExtraO.Upper = app.OxMax_EditField.Value;
            
            if ~isempty(app.ROITree.SelectedNodes)
                WhereItGoes = app.ROITree.SelectedNodes.NodeData;
                [Bulk,app.BinBulk] = SuperFastBulkUpdate(app.BinGfDef,app.BinBulk,WhereItGoes);
                app.BinBulk(WhereItGoes).CompositionIterative = Bulk;
                ROITreeSelectionChanged(app);
            end
        end

        % Close request function: BingoAntidote_GUI
        function BingoAntidote_GUICloseRequest(app, event)
            Answer = uiconfirm(gcbf,'Do you want to close Bingo-Antidote?','Confirm','Options',{'Yes','Cancel (unfreeze)'},'DefaultOption',1,'CancelOption',2,'Icon','question');
            if isequal(Answer,'Cancel (unfreeze)')
                close(app.WaitBar)
                return
            end
            
            delete(app)
        end

        % Selection change function: TabGroup
        function TabGroupSelectionChanged(app, event)
            selectedTab = app.TabGroup.SelectedTab;
            
            ROI_DeleteROI(app);
            
            
            switch selectedTab.Title
                case {'LBC','Bingo','Antidote'}
                    app.Tree_Phases.SelectedNodes = app.Tree_Phases.Children(1);
                    PlotSelectedData(app);
                    if app.ProgramState >= 2.1
                        ROITreeSelectionChanged(app, 0);
                    end
                    matlab.graphics.controls.internal.resetHelper(app.UIAxes,true);
                    
                case {'Settings'}
                    app.Tree_Phases.SelectedNodes = app.Tree_Phases.Children(1);
                    PlotSelectedData(app);
                    
                case {'Phases'}
                    Tree_PhasesModelSelectionChanged(app, 0);  
            end
            
            
            
            %keyboard
            
        end

        % Button pushed function: ButtonSaveResultBingo
        function ButtonSaveResultBingoPushed(app, event)
            
            [Success,Message,MessageID] = mkdir('Bingo');
            
            [filename, pathname] = uiputfile('Bingo/LastBingo.txt', 'Save as');
            if isequal(filename,0) || isequal(pathname,0)
                return
            end
            
            Name4OtherFiles = filename(1:end-4);
            
            fid = fopen(fullfile(pathname,filename),'w');
            for i = 1:length(app.Text_Report.Value)
                fprintf(fid,'%s\n',char(app.Text_Report.Value{i}));
            end
            fclose(fid);
            
            
            f2 = figure;
            tiledlayout('flow');
            
            nexttile
            ax2 = gca;
            copyobj(app.Result_Plot1.Children,ax2);
            axis(ax2,axis(app.Result_Plot1));
            colormap(ax2,app.Result_Plot1.Colormap)
            pbaspect(ax2,[1 1 1])
            ax2.Color = 'none';
            ax2.XTick = [];
            ax2.XColor = 'none';
            ax2.YTick = [];
            ax2.YColor = 'none';
            ax2.Title.String = app.Result_Plot1.Title.String;
            
            nexttile
            ax3 = gca;
            copyobj(app.Result_Plot2.Children,ax3);
            axis(ax3,axis(app.Result_Plot2));
            colormap(ax3,app.Result_Plot2.Colormap)
            pbaspect(ax3,[1 1 1])
            ax3.Color = 'none';
            ax3.Color = 'none';
            ax3.XTick = [];
            ax3.XColor = 'none';
            ax3.YTick = [];
            ax3.YColor = 'none';
            ax3.Title.String = app.Result_Plot2.Title.String;
            
            saveas(f2,fullfile(pathname,[Name4OtherFiles,'.pdf']))
            
            close(f2);
            figure(app.BingoAntidote_GUI);
            
        end

        % Selection change function: TabGroup2
        function TabGroup2SelectionChanged(app, event)
            selectedTab = app.TabGroup2.SelectedTab;
        end

        % Button pushed function: ForceDBDownloadButton
        function ForceDBDownloadButtonPushed(app, event)
            [SUCCESS,MESSAGE,MESSAGEID] = copyfile(fullfile(app.LocBase,'Databases',app.DatabaseListBox.Value),fullfile(cd,app.DatabaseListBox.Value),'f');
            if SUCCESS
                uialert(app.BingoAntidote_GUI,'Database file successfully copied','Bingo-Antidote');
            end
        end

        % Value changed function: DefaultoptionsDropDown
        function DefaultoptionsDropDownValueChanged(app, event)
            switch app.DefaultoptionsDropDown.Value
                case 'Amphibolite facies'
                    app.TminEditField.Value = 400;
                    app.TmaxEditField.Value = 800;
                    app.TstepEditField.Value = 10;
                    app.PminEditField.Value = 0.1;
                    app.PmaxEditField.Value = 1.2;
                    app.PstepEditField.Value = 0.03;
                case 'Eclogite facies'
                    app.TminEditField.Value = 400;
                    app.TmaxEditField.Value = 800;
                    app.TstepEditField.Value = 10;
                    app.PminEditField.Value = 1.3;
                    app.PmaxEditField.Value = 3.0;
                    app.PstepEditField.Value = 0.04;
                case 'Granulite facies'
                    app.TminEditField.Value = 600;
                    app.TmaxEditField.Value = 1000;
                    app.TstepEditField.Value = 10;
                    app.PminEditField.Value = 0.3;
                    app.PmaxEditField.Value = 1.4;
                    app.PstepEditField.Value = 0.03;
                case 'Greenschist facies'
                    app.TminEditField.Value = 200;
                    app.TmaxEditField.Value = 450;
                    app.TstepEditField.Value = 6;
                    app.PminEditField.Value = 0.1;
                    app.PmaxEditField.Value = 0.8;
                    app.PstepEditField.Value = 0.02;
            end
            UpdatePTsteps(app);
        end

        % Button pushed function: InfoDBButton
        function InfoDBButtonPushed(app, event)
            iDB = find(ismember(app.DatabaseListBox.Items,app.DatabaseListBox.Value));
            uialert(app.BingoAntidote_GUI,{['Name: ',app.BingoDefault.Theriak.Database(iDB).Label],['Version: ',app.BingoDefault.Theriak.Database(iDB).Version],['Description: ',app.BingoDefault.Theriak.Database(iDB).Comment]},'Selected database','Icon','info');
        end

        % Clicked callback: PushSave
        function PushSaveClicked(app, event)
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','Bingo-Antidote','Indeterminate','on');
            
            %if isempty(app.ProjectPath)
            app.WaitBar.Message = 'Save the current project as...';
            
            f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
            [filename, pathname] = uiputfile('BA_project.mat', 'Save Project as');
            delete(f);
            figure(app.BingoAntidote_GUI)
            if isequal(filename,0)
                return
            end
            
            app.ProjectPath = [pathname,filename];
            %end
            
            app.WaitBar.Message = 'Saving Bingo-Antidote project';
            
            % Extracting variables:
            MaskFile = app.MaskFile;
            MapData = app.MapData;
            DensityMap = app.DensityMap;
            
            ProgramState = app.ProgramState;
            
            BinBulk = app.BinBulk;
            BinGfDef = app.BinGfDef;
            SF = app.SF;
            BinElData = app.BinElData;
            BinPhaseDef = app.BinPhaseDef;
            AssemblageCode = app.AssemblageCode;
            OptionsXTT = app.OptionsXTT;
            
            Other.BingoTemperature = app.BingoTemperatureEditField.Value;
            Other.BingoPressure = app.BingoPressureEditField.Value;
            Other.SelectedDatabase = app.DatabaseListBox.Value;
            
            save(app.ProjectPath,'MaskFile','MapData','DensityMap','ProgramState','BinBulk','BinGfDef','SF','BinElData','BinPhaseDef','AssemblageCode','OptionsXTT','Other','-v7.3');
            
            close(app.WaitBar);
        end

        % Clicked callback: PushOpen
        function PushOpenClicked(app, event)
            
            f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
            [filename, pathname] = uigetfile(fullfile(cd,'BA_project.mat'), 'Open project');
            delete(f);
            figure(app.BingoAntidote_GUI)
            if isequal(filename,0)
                return
            end
            
            CleanInterface(app);
            
            load(fullfile(pathname,filename));
            
            app.MaskFile = MaskFile;
            app.MapData = MapData;
            app.DensityMap = DensityMap;
            
            app.ProgramState = ProgramState;
            
            app.BinBulk = BinBulk;
            app.BinGfDef = BinGfDef;
            app.SF = SF;
            app.BinElData = BinElData;
            app.BinPhaseDef = BinPhaseDef;
            app.AssemblageCode = AssemblageCode;
            app.OptionsXTT = OptionsXTT;
            
            % Update the interface
            app.BingoTemperatureEditField.Value = Other.BingoTemperature;
            app.BingoPressureEditField.Value = Other.BingoPressure;
            
            % Elements
            for i =1:14
                if i <= length(app.BinElData.listElem)
                    eval(['app.El',num2str(i),'Button.Text = app.MapData.ElNames{i};'])
                    eval(['app.El',num2str(i),'Button.Visible = ''on'';'])
                    eval(['app.El',num2str(i),'Button.Value = 1;'])
                    
                    if app.BinElData.selected(i)
                        eval(['app.El',num2str(i),'Button.BackgroundColor = [0.6941,0.8510,0.9843];']);
                        eval(['app.El',num2str(i),'Button.Value = 1;']);
                    else
                        eval(['app.El',num2str(i),'Button.BackgroundColor = [0.96,0.96,0.96];']);
                        eval(['app.El',num2str(i),'Button.Value = 0;']);
                    end
                    
                else
                    eval(['app.El',num2str(i),'Button.Visible = ''off'';'])
                end
            end
            
            % Check and update the element list and chemical system
            CheckElButtonSelection(app);
            
            % Check program state first
            CheckProgramState(app);
            
            % Database
            if app.ProgramState > 1.1
                try app.DatabaseListBox.Value = Other.SelectedDatabase;
                    
                catch ME
                    % Database is no longer available?
                    app.DatabaseListBox.Value = app.DatabaseListBox.Items{1};
                    disp(' Database is no longer available? ')
                end
                UpdateFluidMeltBufferPanel(app);
            end
            
            % PT range
            if app.ProgramState > 1.2
                app.TminEditField.Value = app.OptionsXTT.Tmin;
                app.TmaxEditField.Value = app.OptionsXTT.Tmax;
                app.TstepEditField.Value = app.OptionsXTT.Tinc;
                app.PminEditField.Value = app.OptionsXTT.Pmin;
                app.PmaxEditField.Value = app.OptionsXTT.Pmax;
                app.PstepEditField.Value = app.OptionsXTT.Pinc;
            end
            
            if app.ProgramState > 2
                
                app.TabGroup.SelectedTab = app.LBCTab;
                
                for i = 1:length(app.BinBulk)
                    % Add Tree node:
                    p = uitreenode(app.ROITree,'Text',char(app.BinBulk(i).Name),'NodeData',i);
                end
                app.ROITree.SelectedNodes = app.ROITree.Children(1);
                
                ROITreeSelectionChanged(app);
            end
            
            if app.ProgramState > 3
                
                app.TabGroup.SelectedTab = app.PhasesTab;
                
                CheckPhaseSolutionList(app);
                
                app.ProgramState = ProgramState;
            end
            
            if app.ProgramState >= 4
                
                app.TabGroup.SelectedTab = app.BingoTab;
            end
            
        end

        % Button pushed function: ButtonROI_Add
        function ButtonROI_AddPushed(app, event)
            ROI_DeleteROI(app);
            
            app.TabGroup2.SelectedTab = app.MapTab;
            
            DeactivatePlotZoomPanOptions(app);

            app.SelectedROI = drawrectangle(app.UIAxes,'Color',[1,0,0.7137],'InteractionsAllowed','all');
            app.ROI_Texture_Listener = addlistener(app.SelectedROI, 'ROIMoved', @(varargin)Texture_ROI_changed_shape(app, app.SelectedROI));
            
            WhereItGoes = length(app.TextureROITree.Children) + 1;
            
            p = uitreenode(app.TextureROITree,'Text',['ROI_',num2str(WhereItGoes)],'NodeData',WhereItGoes);
            app.TextureROITree.SelectedNodes = app.TextureROITree.Children(WhereItGoes);
            
            app.TextureROI.ROIdata(WhereItGoes).Vertices = app.SelectedROI.Vertices;
            app.TextureROI.ROIdata(WhereItGoes).Position = app.SelectedROI.Position;
            
            if length(app.TextureROI.ROIdata) > 1 && isequal(app.AntidoteTree.SelectedNodes.NodeData,[4,1])
                app.AntidoteButton.Enable = 'on';
                app.ButtonROI_Play.Enable = 'on';
            else
                app.AntidoteButton.Enable = 'off';
                app.ButtonROI_Play.Enable = 'off';
            end
            
            app.ButtonROI_Eliminate.Enable = 'on';
            app.ButtonROI_Duplicate.Enable = 'on';
            
        end

        % Button pushed function: ButtonROI_Play
        function ButtonROI_PlayPushed(app, event)
            NodeData = app.AntidoteTree.SelectedNodes.NodeData;
            app.TabGroup2.SelectedTab = app.MapTab;
            
            if isequal(NodeData,[4,1])
                GenerateROIPath(app,'on');
            end
            
            MapSize = size(app.MaskFile.MaskMap);
            
            if isequal(NodeData,[4,2]) || isequal(NodeData,[4,4])
                
                dX = app.AntidoteROIparamNbStepsEditField.Value;
                dY = dX;
                
                Step = dX;
                
                Xi = [floor((dX-1)/2):Step:MapSize(2) - floor((dX-1)/2)];
                Yi = [floor((dY-1)/2):Step:MapSize(1) - floor((dY-1)/2)];
                
                NbLoops = length(Xi)*length(Yi);
                
                h = figure;
                ax2 = gca;
                axis tight manual % this ensures that getframe() returns a consistent size
                if isequal(NodeData,[4,3])
                    filename = 'ScanningWindowAnimated.gif';
                else
                    filename = 'ChemPotGridAnimated.gif';
                end
                
                ROI_DeleteROI(app);
                
                copyobj(app.UIAxes.Children,ax2);
                axis(ax2,'image');
                colormap(ax2,app.UIAxes.Colormap)
                colorbar('vertical');
                ax2.XTick = app.UIAxes.XTick;
                ax2.YTick = app.UIAxes.YTick;
                ax2.YDir = app.UIAxes.YDir;
                ax2.CLim = app.UIAxes.CLim;
                
                [iX,iY] = CalcIndices(app,1,length(Xi),length(Yi));
                
                XVals = [Xi(iX)-floor((dX-1)/2),Xi(iX)+floor((dX-1)/2),Xi(iX)+floor((dX-1)/2),Xi(iX)-floor((dX-1)/2)];
                YVals = [Yi(iY)+floor((dY-1)/2),Yi(iY)+floor((dY-1)/2),Yi(iY)-floor((dY-1)/2),Yi(iY)-floor((dY-1)/2)];
                
                Position = [XVals(end),YVals(end),dX,dX];
                
                ROI = drawrectangle(ax2,'Position',Position,'Color',[1,0,0.7137],'InteractionsAllowed','none');
                title(ax2,['Step ',num2str(1),' / ',num2str(NbLoops)])
                
                frame = getframe(h);
                im = frame2im(frame);
                [imind,cm] = rgb2ind(im,256);
                imwrite(imind,cm,filename,'gif', 'Loopcount',inf);
                
                for iLoop = 2:NbLoops
                    drawnow
                    pause(0.01)
                    
                    [iX,iY] = CalcIndices(app,iLoop,length(Xi),length(Yi));
                    
                    XVals = [Xi(iX)-floor((dX-1)/2),Xi(iX)+floor((dX-1)/2),Xi(iX)+floor((dX-1)/2),Xi(iX)-floor((dX-1)/2)];
                    YVals = [Yi(iY)+floor((dY-1)/2),Yi(iY)+floor((dY-1)/2),Yi(iY)-floor((dY-1)/2),Yi(iY)-floor((dY-1)/2)];
                    
                    ROI.Position = [XVals(end),YVals(end),dX,dX];
                    title(ax2,['Step ',num2str(iLoop),' / ',num2str(NbLoops)])
                    
                    frame = getframe(h);
                    im = frame2im(frame);
                    [imind,cm] = rgb2ind(im,256);
                    imwrite(imind,cm,filename,'gif','WriteMode','append');
                end
            end
            
            if isequal(NodeData,[4,3]) 
                NbSteps = app.AntidoteROIparamNbStepsEditField.Value;
                
                StartPos = [round(0.5*MapSize(2) - 0.5*0.1*MapSize(2)),round(0.5*MapSize(1) - 0.5*0.1*MapSize(1)),round(0.1*MapSize(2)),round(0.1*MapSize(1))];
                EndPos = [1,1,MapSize(2)-1,MapSize(1)-1];
                
                InterpPositions = zeros(NbSteps,length(StartPos));
                
                for i = 1:length(StartPos)
                    InterpPositions(:,i) = interp1([1,NbSteps],[StartPos(i),EndPos(i)],1:NbSteps)';
                end
                
                NbLoops = NbSteps; 
                
                h = figure;
                ax2 = gca;
                axis tight manual % this ensures that getframe() returns a consistent size
                filename = 'GrowingWindowAnimated.gif';
                
                ROI_DeleteROI(app);
                
                copyobj(app.UIAxes.Children,ax2);
                axis(ax2,'image');
                colormap(ax2,app.UIAxes.Colormap)
                colorbar('vertical');
                ax2.XTick = app.UIAxes.XTick;
                ax2.YTick = app.UIAxes.YTick;
                ax2.YDir = app.UIAxes.YDir;
                ax2.CLim = app.UIAxes.CLim;
                
                ROI = drawrectangle(ax2,'Position',InterpPositions(1,:),'Color',[1,0,0.7137],'InteractionsAllowed','none');
                title(ax2,['Step ',num2str(1),' / ',num2str(size(InterpPositions,1))])
                
                frame = getframe(h);
                im = frame2im(frame);
                [imind,cm] = rgb2ind(im,256);
                imwrite(imind,cm,filename,'gif', 'Loopcount',inf);
                
                for iLoop = 2:NbLoops
                    drawnow
                    pause(0.01)
                    ROI.Position = InterpPositions(iLoop,:);
                    title(ax2,['Step ',num2str(iLoop),' / ',num2str(size(InterpPositions,1))])
                    
                    frame = getframe(h);
                    im = frame2im(frame);
                    [imind,cm] = rgb2ind(im,256);
                    imwrite(imind,cm,filename,'gif','WriteMode','append');
                end
            end
            
            
            %keyboard
            
            
        end

        % Selection changed function: TextureROITree
        function TextureROITreeSelectionChanged(app, event)
            SelectedNodes = app.TextureROITree.SelectedNodes;
            
            app.TabGroup2.SelectedTab = app.MapTab;
            
            ROI_DeleteROI(app);
            
            app.SelectedROI = drawrectangle(app.UIAxes,'Position',app.TextureROI.ROIdata(SelectedNodes.NodeData).Position,'Color',[1,0,0.7137],'InteractionsAllowed','all');
            app.ROI_Texture_Listener = addlistener(app.SelectedROI, 'ROIMoved', @(varargin)Texture_ROI_changed_shape(app, app.SelectedROI));
            
        end

        % Button pushed function: ButtonROI_Duplicate
        function ButtonROI_DuplicatePushed(app, event)
            SelectedNodes = app.TextureROITree.SelectedNodes;
            
            app.TabGroup2.SelectedTab = app.MapTab;
            
            ROI_DeleteROI(app);
            
            app.SelectedROI = drawrectangle(app.UIAxes,'Position',app.TextureROI.ROIdata(SelectedNodes.NodeData).Position,'Color',[1,0,0.7137],'InteractionsAllowed','all');
            app.ROI_Texture_Listener = addlistener(app.SelectedROI, 'ROIMoved', @(varargin)Texture_ROI_changed_shape(app, app.SelectedROI));
            
            WhereItGoes = length(app.TextureROITree.Children) + 1;
            
            app.TextureROI.ROIdata(WhereItGoes).Vertices = app.TextureROI.ROIdata(SelectedNodes.NodeData).Vertices;
            app.TextureROI.ROIdata(WhereItGoes).Position = app.TextureROI.ROIdata(SelectedNodes.NodeData).Position;
            
            p = uitreenode(app.TextureROITree,'Text',['ROI_',num2str(WhereItGoes)],'NodeData',WhereItGoes);
            app.TextureROITree.SelectedNodes = app.TextureROITree.Children(WhereItGoes);
            
            if length(app.TextureROI.ROIdata) > 1 && isequal(app.AntidoteTree.SelectedNodes.NodeData,[4,1])
                app.AntidoteButton.Enable = 'on';
            else
                app.AntidoteButton.Enable = 'off';
            end
            
        end

        % Button pushed function: ButtonROI_Eliminate
        function ButtonROI_EliminatePushed(app, event)
            SelectedNodes = app.TextureROITree.SelectedNodes.NodeData;
            
            app.TabGroup2.SelectedTab = app.MapTab;
            
            app.TextureROI.ROIdata(SelectedNodes) = [];
            app.TextureROITree.Children(SelectedNodes).delete;
            
            for i = 1:length(app.TextureROITree.Children)
                app.TextureROITree.Children(i).Text = ['ROI_',num2str(i)];
                app.TextureROITree.Children(i).NodeData = i;
            end
            
            if length(app.TextureROITree.Children) >= 2
                app.ButtonROI_Play.Enable = 'on';
            elseif length(app.TextureROITree.Children) >= 1
                app.ButtonROI_Eliminate.Enable = 'on';
                app.ButtonROI_Duplicate.Enable = 'on';
                app.ButtonROI_Play.Enable = 'off';
            else
                app.ButtonROI_Play.Enable = 'off';
                app.ButtonROI_Eliminate.Enable = 'off';
                app.ButtonROI_Duplicate.Enable = 'off';
            end
            
            
            ROI_DeleteROI(app);
            
        end

        % Clicked callback: PushDeleteSystemFiles
        function PushDeleteSystemFilesClicked(app, event)
            
            Files2Delete = {'theriak.ini','theriak.last','THERIN','XBIN'};
            
            for i = 1:length(Files2Delete)
                if exist(fullfile(cd,Files2Delete{i}))
                    delete(fullfile(cd,Files2Delete{i}));
                end
            end
            
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create BingoAntidote_GUI and hide until all components are created
            app.BingoAntidote_GUI = uifigure('Visible', 'off');
            app.BingoAntidote_GUI.Position = [100 100 1465 836];
            app.BingoAntidote_GUI.Name = 'Bingo-Antidote 2.1';
            app.BingoAntidote_GUI.Icon = 'logo_transparent.png';
            app.BingoAntidote_GUI.CloseRequestFcn = createCallbackFcn(app, @BingoAntidote_GUICloseRequest, true);

            % Create Toolbar
            app.Toolbar = uitoolbar(app.BingoAntidote_GUI);

            % Create PushOpen
            app.PushOpen = uipushtool(app.Toolbar);
            app.PushOpen.Tooltip = {'Open a Bingo-Antidote project'};
            app.PushOpen.ClickedCallback = createCallbackFcn(app, @PushOpenClicked, true);
            app.PushOpen.Icon = '241-folder.png';

            % Create PushSave
            app.PushSave = uipushtool(app.Toolbar);
            app.PushSave.Tooltip = {'Save the current Bingo-Antidote project as...'};
            app.PushSave.ClickedCallback = createCallbackFcn(app, @PushSaveClicked, true);
            app.PushSave.Icon = '022-save.png';

            % Create PushDeleteSystemFiles
            app.PushDeleteSystemFiles = uipushtool(app.Toolbar);
            app.PushDeleteSystemFiles.Tooltip = {'Trash system files (TD)'};
            app.PushDeleteSystemFiles.ClickedCallback = createCallbackFcn(app, @PushDeleteSystemFilesClicked, true);
            app.PushDeleteSystemFiles.Icon = '276-trash.png';
            app.PushDeleteSystemFiles.Separator = 'on';

            % Create GridLayout
            app.GridLayout = uigridlayout(app.BingoAntidote_GUI);
            app.GridLayout.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout.Padding = [20 20 20 20];

            % Create TabGroup
            app.TabGroup = uitabgroup(app.GridLayout);
            app.TabGroup.TabLocation = 'left';
            app.TabGroup.SelectionChangedFcn = createCallbackFcn(app, @TabGroupSelectionChanged, true);
            app.TabGroup.Layout.Row = [1 12];
            app.TabGroup.Layout.Column = [1 5];

            % Create SettingsTab
            app.SettingsTab = uitab(app.TabGroup);
            app.SettingsTab.Title = 'Settings';

            % Create GridLayout2
            app.GridLayout2 = uigridlayout(app.SettingsTab);
            app.GridLayout2.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout2.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout2.ColumnSpacing = 5;
            app.GridLayout2.RowSpacing = 5;

            % Create ChemicalSystemPanel
            app.ChemicalSystemPanel = uipanel(app.GridLayout2);
            app.ChemicalSystemPanel.BorderType = 'none';
            app.ChemicalSystemPanel.TitlePosition = 'centertop';
            app.ChemicalSystemPanel.Title = 'Chemical System';
            app.ChemicalSystemPanel.Layout.Row = [1 4];
            app.ChemicalSystemPanel.Layout.Column = [1 13];
            app.ChemicalSystemPanel.FontAngle = 'italic';

            % Create GridLayout4
            app.GridLayout4 = uigridlayout(app.ChemicalSystemPanel);
            app.GridLayout4.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout4.RowHeight = {'1x', '1x', '1x'};
            app.GridLayout4.ColumnSpacing = 5;
            app.GridLayout4.RowSpacing = 5;
            app.GridLayout4.Padding = [10 12 10 12];

            % Create El14Button
            app.El14Button = uibutton(app.GridLayout4, 'state');
            app.El14Button.ValueChangedFcn = createCallbackFcn(app, @El1ButtonValueChanged, true);
            app.El14Button.Text = 'El14';
            app.El14Button.Layout.Row = 2;
            app.El14Button.Layout.Column = [13 14];

            % Create El13Button
            app.El13Button = uibutton(app.GridLayout4, 'state');
            app.El13Button.ValueChangedFcn = createCallbackFcn(app, @El1ButtonValueChanged, true);
            app.El13Button.Text = 'El13';
            app.El13Button.Layout.Row = 2;
            app.El13Button.Layout.Column = [11 12];

            % Create El12Button
            app.El12Button = uibutton(app.GridLayout4, 'state');
            app.El12Button.ValueChangedFcn = createCallbackFcn(app, @El1ButtonValueChanged, true);
            app.El12Button.Text = 'El12';
            app.El12Button.Layout.Row = 2;
            app.El12Button.Layout.Column = [9 10];

            % Create El11Button
            app.El11Button = uibutton(app.GridLayout4, 'state');
            app.El11Button.ValueChangedFcn = createCallbackFcn(app, @El1ButtonValueChanged, true);
            app.El11Button.Text = 'El11';
            app.El11Button.Layout.Row = 2;
            app.El11Button.Layout.Column = [7 8];

            % Create El10Button
            app.El10Button = uibutton(app.GridLayout4, 'state');
            app.El10Button.ValueChangedFcn = createCallbackFcn(app, @El1ButtonValueChanged, true);
            app.El10Button.Text = 'El10';
            app.El10Button.Layout.Row = 2;
            app.El10Button.Layout.Column = [5 6];

            % Create El9Button
            app.El9Button = uibutton(app.GridLayout4, 'state');
            app.El9Button.ValueChangedFcn = createCallbackFcn(app, @El1ButtonValueChanged, true);
            app.El9Button.Text = 'El9';
            app.El9Button.Layout.Row = 2;
            app.El9Button.Layout.Column = [3 4];

            % Create El8Button
            app.El8Button = uibutton(app.GridLayout4, 'state');
            app.El8Button.ValueChangedFcn = createCallbackFcn(app, @El1ButtonValueChanged, true);
            app.El8Button.Text = 'El8';
            app.El8Button.Layout.Row = 2;
            app.El8Button.Layout.Column = [1 2];

            % Create El7Button
            app.El7Button = uibutton(app.GridLayout4, 'state');
            app.El7Button.ValueChangedFcn = createCallbackFcn(app, @El1ButtonValueChanged, true);
            app.El7Button.Text = 'El7';
            app.El7Button.Layout.Row = 1;
            app.El7Button.Layout.Column = [13 14];

            % Create El6Button
            app.El6Button = uibutton(app.GridLayout4, 'state');
            app.El6Button.ValueChangedFcn = createCallbackFcn(app, @El1ButtonValueChanged, true);
            app.El6Button.Text = 'El6';
            app.El6Button.Layout.Row = 1;
            app.El6Button.Layout.Column = [11 12];

            % Create El5Button
            app.El5Button = uibutton(app.GridLayout4, 'state');
            app.El5Button.ValueChangedFcn = createCallbackFcn(app, @El1ButtonValueChanged, true);
            app.El5Button.Text = 'El5';
            app.El5Button.Layout.Row = 1;
            app.El5Button.Layout.Column = [9 10];

            % Create El4Button
            app.El4Button = uibutton(app.GridLayout4, 'state');
            app.El4Button.ValueChangedFcn = createCallbackFcn(app, @El1ButtonValueChanged, true);
            app.El4Button.Text = 'El4';
            app.El4Button.Layout.Row = 1;
            app.El4Button.Layout.Column = [7 8];

            % Create El3Button
            app.El3Button = uibutton(app.GridLayout4, 'state');
            app.El3Button.ValueChangedFcn = createCallbackFcn(app, @El1ButtonValueChanged, true);
            app.El3Button.Text = 'El3';
            app.El3Button.Layout.Row = 1;
            app.El3Button.Layout.Column = [5 6];

            % Create El2Button
            app.El2Button = uibutton(app.GridLayout4, 'state');
            app.El2Button.ValueChangedFcn = createCallbackFcn(app, @El1ButtonValueChanged, true);
            app.El2Button.Text = 'El2';
            app.El2Button.Layout.Row = 1;
            app.El2Button.Layout.Column = [3 4];

            % Create El1Button
            app.El1Button = uibutton(app.GridLayout4, 'state');
            app.El1Button.ValueChangedFcn = createCallbackFcn(app, @El1ButtonValueChanged, true);
            app.El1Button.Text = 'El1';
            app.El1Button.Layout.Row = 1;
            app.El1Button.Layout.Column = [1 2];

            % Create NbSystComponentsLabel
            app.NbSystComponentsLabel = uilabel(app.GridLayout4);
            app.NbSystComponentsLabel.FontSize = 11;
            app.NbSystComponentsLabel.FontAngle = 'italic';
            app.NbSystComponentsLabel.Layout.Row = 3;
            app.NbSystComponentsLabel.Layout.Column = [1 11];

            % Create ApplyStep1Button
            app.ApplyStep1Button = uibutton(app.GridLayout4, 'push');
            app.ApplyStep1Button.ButtonPushedFcn = createCallbackFcn(app, @ApplyStep1ButtonPushed, true);
            app.ApplyStep1Button.FontWeight = 'bold';
            app.ApplyStep1Button.Layout.Row = 3;
            app.ApplyStep1Button.Layout.Column = [12 14];
            app.ApplyStep1Button.Text = 'Apply';

            % Create PressureTemperatureRangePanel
            app.PressureTemperatureRangePanel = uipanel(app.GridLayout2);
            app.PressureTemperatureRangePanel.BorderType = 'none';
            app.PressureTemperatureRangePanel.TitlePosition = 'centertop';
            app.PressureTemperatureRangePanel.Title = 'Pressure–Temperature Range';
            app.PressureTemperatureRangePanel.Layout.Row = [11 14];
            app.PressureTemperatureRangePanel.Layout.Column = [1 13];
            app.PressureTemperatureRangePanel.FontAngle = 'italic';

            % Create GridLayout5
            app.GridLayout5 = uigridlayout(app.PressureTemperatureRangePanel);
            app.GridLayout5.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout5.RowHeight = {'1x', '1x', '1x'};
            app.GridLayout5.ColumnSpacing = 5;
            app.GridLayout5.RowSpacing = 5;
            app.GridLayout5.Padding = [10 12 10 12];

            % Create TminEditField
            app.TminEditField = uieditfield(app.GridLayout5, 'numeric');
            app.TminEditField.Limits = [0 2000];
            app.TminEditField.ValueChangedFcn = createCallbackFcn(app, @TminEditFieldValueChanged, true);
            app.TminEditField.Layout.Row = 1;
            app.TminEditField.Layout.Column = [3 4];
            app.TminEditField.Value = 400;

            % Create TminEditFieldLabel
            app.TminEditFieldLabel = uilabel(app.GridLayout5);
            app.TminEditFieldLabel.HorizontalAlignment = 'right';
            app.TminEditFieldLabel.Layout.Row = 1;
            app.TminEditFieldLabel.Layout.Column = [1 2];
            app.TminEditFieldLabel.Text = 'Tmin';

            % Create TmaxEditField
            app.TmaxEditField = uieditfield(app.GridLayout5, 'numeric');
            app.TmaxEditField.Limits = [100 3000];
            app.TmaxEditField.ValueChangedFcn = createCallbackFcn(app, @TmaxEditFieldValueChanged, true);
            app.TmaxEditField.Layout.Row = 1;
            app.TmaxEditField.Layout.Column = [7 8];
            app.TmaxEditField.Value = 800;

            % Create TmaxEditFieldLabel
            app.TmaxEditFieldLabel = uilabel(app.GridLayout5);
            app.TmaxEditFieldLabel.HorizontalAlignment = 'right';
            app.TmaxEditFieldLabel.Layout.Row = 1;
            app.TmaxEditFieldLabel.Layout.Column = [5 6];
            app.TmaxEditFieldLabel.Text = 'Tmax';

            % Create TstepEditField
            app.TstepEditField = uieditfield(app.GridLayout5, 'numeric');
            app.TstepEditField.Limits = [0.01 500];
            app.TstepEditField.ValueChangedFcn = createCallbackFcn(app, @TstepEditFieldValueChanged, true);
            app.TstepEditField.Layout.Row = 1;
            app.TstepEditField.Layout.Column = [11 12];
            app.TstepEditField.Value = 10;

            % Create TstepEditFieldLabel
            app.TstepEditFieldLabel = uilabel(app.GridLayout5);
            app.TstepEditFieldLabel.HorizontalAlignment = 'right';
            app.TstepEditFieldLabel.Layout.Row = 1;
            app.TstepEditFieldLabel.Layout.Column = [9 10];
            app.TstepEditFieldLabel.Text = 'Tstep';

            % Create PminEditField
            app.PminEditField = uieditfield(app.GridLayout5, 'numeric');
            app.PminEditField.Limits = [0.001 10];
            app.PminEditField.ValueChangedFcn = createCallbackFcn(app, @PminEditFieldValueChanged, true);
            app.PminEditField.Layout.Row = 2;
            app.PminEditField.Layout.Column = [3 4];
            app.PminEditField.Value = 0.1;

            % Create PminEditFieldLabel
            app.PminEditFieldLabel = uilabel(app.GridLayout5);
            app.PminEditFieldLabel.HorizontalAlignment = 'right';
            app.PminEditFieldLabel.Layout.Row = 2;
            app.PminEditFieldLabel.Layout.Column = [1 2];
            app.PminEditFieldLabel.Text = 'Pmin';

            % Create PmaxEditField
            app.PmaxEditField = uieditfield(app.GridLayout5, 'numeric');
            app.PmaxEditField.Limits = [0.002 10];
            app.PmaxEditField.ValueChangedFcn = createCallbackFcn(app, @PmaxEditFieldValueChanged, true);
            app.PmaxEditField.Layout.Row = 2;
            app.PmaxEditField.Layout.Column = [7 8];
            app.PmaxEditField.Value = 1.2;

            % Create PmaxEditFieldLabel
            app.PmaxEditFieldLabel = uilabel(app.GridLayout5);
            app.PmaxEditFieldLabel.HorizontalAlignment = 'right';
            app.PmaxEditFieldLabel.Layout.Row = 2;
            app.PmaxEditFieldLabel.Layout.Column = [5 6];
            app.PmaxEditFieldLabel.Text = 'Pmax';

            % Create PstepEditField
            app.PstepEditField = uieditfield(app.GridLayout5, 'numeric');
            app.PstepEditField.Limits = [0.001 1];
            app.PstepEditField.ValueChangedFcn = createCallbackFcn(app, @PstepEditFieldValueChanged, true);
            app.PstepEditField.Layout.Row = 2;
            app.PstepEditField.Layout.Column = [11 12];
            app.PstepEditField.Value = 0.03;

            % Create PstepEditFieldLabel
            app.PstepEditFieldLabel = uilabel(app.GridLayout5);
            app.PstepEditFieldLabel.HorizontalAlignment = 'right';
            app.PstepEditFieldLabel.Layout.Row = 2;
            app.PstepEditFieldLabel.Layout.Column = [9 10];
            app.PstepEditFieldLabel.Text = 'Pstep';

            % Create TNbSteps
            app.TNbSteps = uieditfield(app.GridLayout5, 'numeric');
            app.TNbSteps.Editable = 'off';
            app.TNbSteps.FontSize = 10;
            app.TNbSteps.Enable = 'off';
            app.TNbSteps.Layout.Row = 1;
            app.TNbSteps.Layout.Column = [13 14];

            % Create PNbSteps
            app.PNbSteps = uieditfield(app.GridLayout5, 'numeric');
            app.PNbSteps.Editable = 'off';
            app.PNbSteps.FontSize = 10;
            app.PNbSteps.Enable = 'off';
            app.PNbSteps.Layout.Row = 2;
            app.PNbSteps.Layout.Column = [13 14];

            % Create ApplyStep3Button
            app.ApplyStep3Button = uibutton(app.GridLayout5, 'push');
            app.ApplyStep3Button.ButtonPushedFcn = createCallbackFcn(app, @ApplyStep3ButtonPushed, true);
            app.ApplyStep3Button.FontWeight = 'bold';
            app.ApplyStep3Button.Layout.Row = 3;
            app.ApplyStep3Button.Layout.Column = [12 14];
            app.ApplyStep3Button.Text = 'Apply';

            % Create DefaultoptionsDropDownLabel
            app.DefaultoptionsDropDownLabel = uilabel(app.GridLayout5);
            app.DefaultoptionsDropDownLabel.HorizontalAlignment = 'right';
            app.DefaultoptionsDropDownLabel.Layout.Row = 3;
            app.DefaultoptionsDropDownLabel.Layout.Column = [1 3];
            app.DefaultoptionsDropDownLabel.Text = 'Default options';

            % Create DefaultoptionsDropDown
            app.DefaultoptionsDropDown = uidropdown(app.GridLayout5);
            app.DefaultoptionsDropDown.Items = {'Amphibolite facies', 'Eclogite facies', 'Granulite facies', 'Greenschist facies'};
            app.DefaultoptionsDropDown.ValueChangedFcn = createCallbackFcn(app, @DefaultoptionsDropDownValueChanged, true);
            app.DefaultoptionsDropDown.Layout.Row = 3;
            app.DefaultoptionsDropDown.Layout.Column = [4 9];
            app.DefaultoptionsDropDown.Value = 'Amphibolite facies';

            % Create ThermodynamicDatabasePanel
            app.ThermodynamicDatabasePanel = uipanel(app.GridLayout2);
            app.ThermodynamicDatabasePanel.BorderType = 'none';
            app.ThermodynamicDatabasePanel.TitlePosition = 'centertop';
            app.ThermodynamicDatabasePanel.Title = 'Thermodynamic Database';
            app.ThermodynamicDatabasePanel.Layout.Row = [6 9];
            app.ThermodynamicDatabasePanel.Layout.Column = [1 13];
            app.ThermodynamicDatabasePanel.FontAngle = 'italic';

            % Create GridLayout5_2
            app.GridLayout5_2 = uigridlayout(app.ThermodynamicDatabasePanel);
            app.GridLayout5_2.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout5_2.RowHeight = {'1x', '1x', '1x'};
            app.GridLayout5_2.ColumnSpacing = 5;
            app.GridLayout5_2.RowSpacing = 5;
            app.GridLayout5_2.Padding = [10 12 10 12];

            % Create DatabaseListBoxLabel
            app.DatabaseListBoxLabel = uilabel(app.GridLayout5_2);
            app.DatabaseListBoxLabel.HorizontalAlignment = 'right';
            app.DatabaseListBoxLabel.Layout.Row = 1;
            app.DatabaseListBoxLabel.Layout.Column = [1 3];
            app.DatabaseListBoxLabel.Text = 'Database';

            % Create DatabaseListBox
            app.DatabaseListBox = uilistbox(app.GridLayout5_2);
            app.DatabaseListBox.ValueChangedFcn = createCallbackFcn(app, @DatabaseListBoxValueChanged, true);
            app.DatabaseListBox.Layout.Row = [1 3];
            app.DatabaseListBox.Layout.Column = [4 11];

            % Create ApplyStep2Button
            app.ApplyStep2Button = uibutton(app.GridLayout5_2, 'push');
            app.ApplyStep2Button.ButtonPushedFcn = createCallbackFcn(app, @ApplyStep2ButtonPushed, true);
            app.ApplyStep2Button.FontWeight = 'bold';
            app.ApplyStep2Button.Layout.Row = 3;
            app.ApplyStep2Button.Layout.Column = [12 14];
            app.ApplyStep2Button.Text = 'Apply';

            % Create ForceDBDownloadButton
            app.ForceDBDownloadButton = uibutton(app.GridLayout5_2, 'push');
            app.ForceDBDownloadButton.ButtonPushedFcn = createCallbackFcn(app, @ForceDBDownloadButtonPushed, true);
            app.ForceDBDownloadButton.Icon = 'XXX_images.png';
            app.ForceDBDownloadButton.Tooltip = {'Copy database from source and replace the existing file'};
            app.ForceDBDownloadButton.Layout.Row = 1;
            app.ForceDBDownloadButton.Layout.Column = 13;
            app.ForceDBDownloadButton.Text = '';

            % Create InfoDBButton
            app.InfoDBButton = uibutton(app.GridLayout5_2, 'push');
            app.InfoDBButton.ButtonPushedFcn = createCallbackFcn(app, @InfoDBButtonPushed, true);
            app.InfoDBButton.Icon = 'XXX_question.png';
            app.InfoDBButton.Tooltip = {'Database information'};
            app.InfoDBButton.Layout.Row = 1;
            app.InfoDBButton.Layout.Column = 12;
            app.InfoDBButton.Text = '';

            % Create Settings_NextButton
            app.Settings_NextButton = uibutton(app.GridLayout2, 'push');
            app.Settings_NextButton.ButtonPushedFcn = createCallbackFcn(app, @Settings_NextButtonPushed, true);
            app.Settings_NextButton.Icon = '023-next.png';
            app.Settings_NextButton.IconAlignment = 'top';
            app.Settings_NextButton.Layout.Row = [20 21];
            app.Settings_NextButton.Layout.Column = [12 13];
            app.Settings_NextButton.Text = 'Next';

            % Create InitialPressureTemperatureGuestimatePanel
            app.InitialPressureTemperatureGuestimatePanel = uipanel(app.GridLayout2);
            app.InitialPressureTemperatureGuestimatePanel.BorderType = 'none';
            app.InitialPressureTemperatureGuestimatePanel.TitlePosition = 'centertop';
            app.InitialPressureTemperatureGuestimatePanel.Title = 'Initial Pressure–Temperature Guestimate';
            app.InitialPressureTemperatureGuestimatePanel.Layout.Row = [16 19];
            app.InitialPressureTemperatureGuestimatePanel.Layout.Column = [1 13];
            app.InitialPressureTemperatureGuestimatePanel.FontAngle = 'italic';

            % Create GridLayout5_4
            app.GridLayout5_4 = uigridlayout(app.InitialPressureTemperatureGuestimatePanel);
            app.GridLayout5_4.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout5_4.RowHeight = {'1x', '1x', '1x'};
            app.GridLayout5_4.ColumnSpacing = 5;
            app.GridLayout5_4.RowSpacing = 5;
            app.GridLayout5_4.Padding = [10 12 10 12];

            % Create TemperatureEditFieldLabel
            app.TemperatureEditFieldLabel = uilabel(app.GridLayout5_4);
            app.TemperatureEditFieldLabel.HorizontalAlignment = 'right';
            app.TemperatureEditFieldLabel.Layout.Row = 2;
            app.TemperatureEditFieldLabel.Layout.Column = [1 4];
            app.TemperatureEditFieldLabel.Text = 'Temperature';

            % Create TemperatureEditField
            app.TemperatureEditField = uieditfield(app.GridLayout5_4, 'numeric');
            app.TemperatureEditField.Limits = [50 1500];
            app.TemperatureEditField.ValueChangedFcn = createCallbackFcn(app, @TemperatureEditFieldValueChanged, true);
            app.TemperatureEditField.Layout.Row = 2;
            app.TemperatureEditField.Layout.Column = [5 6];
            app.TemperatureEditField.Value = 680;

            % Create PressureEditFieldLabel
            app.PressureEditFieldLabel = uilabel(app.GridLayout5_4);
            app.PressureEditFieldLabel.HorizontalAlignment = 'right';
            app.PressureEditFieldLabel.Layout.Row = 2;
            app.PressureEditFieldLabel.Layout.Column = [7 10];
            app.PressureEditFieldLabel.Text = 'Pressure';

            % Create PressureEditField
            app.PressureEditField = uieditfield(app.GridLayout5_4, 'numeric');
            app.PressureEditField.Limits = [0.01 10];
            app.PressureEditField.ValueChangedFcn = createCallbackFcn(app, @PressureEditFieldValueChanged, true);
            app.PressureEditField.Layout.Row = 2;
            app.PressureEditField.Layout.Column = [11 12];
            app.PressureEditField.Value = 0.7;

            % Create LBCTab
            app.LBCTab = uitab(app.TabGroup);
            app.LBCTab.Title = 'LBC';

            % Create GridLayout6
            app.GridLayout6 = uigridlayout(app.LBCTab);
            app.GridLayout6.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout6.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout6.ColumnSpacing = 5;
            app.GridLayout6.RowSpacing = 5;

            % Create AddManageROIPanel
            app.AddManageROIPanel = uipanel(app.GridLayout6);
            app.AddManageROIPanel.BorderType = 'none';
            app.AddManageROIPanel.TitlePosition = 'centertop';
            app.AddManageROIPanel.Title = 'Add / Manage ROI ';
            app.AddManageROIPanel.Layout.Row = [1 10];
            app.AddManageROIPanel.Layout.Column = [1 13];
            app.AddManageROIPanel.FontAngle = 'italic';

            % Create GridLayout5_3
            app.GridLayout5_3 = uigridlayout(app.AddManageROIPanel);
            app.GridLayout5_3.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout5_3.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout5_3.ColumnSpacing = 5;
            app.GridLayout5_3.RowSpacing = 5;

            % Create ROITree
            app.ROITree = uitree(app.GridLayout5_3);
            app.ROITree.SelectionChangedFcn = createCallbackFcn(app, @ROITreeSelectionChanged, true);
            app.ROITree.Layout.Row = [2 10];
            app.ROITree.Layout.Column = [1 8];

            % Create UITable
            app.UITable = uitable(app.GridLayout5_3);
            app.UITable.ColumnName = {'Oxide'; 'wt%'};
            app.UITable.RowName = {};
            app.UITable.Layout.Row = [2 10];
            app.UITable.Layout.Column = [9 14];

            % Create ButtonAddROI
            app.ButtonAddROI = uibutton(app.GridLayout5_3, 'push');
            app.ButtonAddROI.ButtonPushedFcn = createCallbackFcn(app, @ButtonAddROIPushed, true);
            app.ButtonAddROI.Icon = 'plus_circle.png';
            app.ButtonAddROI.Layout.Row = 1;
            app.ButtonAddROI.Layout.Column = 5;
            app.ButtonAddROI.Text = '';

            % Create ButtonEliminateROI
            app.ButtonEliminateROI = uibutton(app.GridLayout5_3, 'push');
            app.ButtonEliminateROI.ButtonPushedFcn = createCallbackFcn(app, @ButtonEliminateROIPushed, true);
            app.ButtonEliminateROI.Icon = 'minus_circle.png';
            app.ButtonEliminateROI.Layout.Row = 1;
            app.ButtonEliminateROI.Layout.Column = 6;
            app.ButtonEliminateROI.Text = '';

            % Create MethodROIDropDown
            app.MethodROIDropDown = uidropdown(app.GridLayout5_3);
            app.MethodROIDropDown.Items = {'Rectangle ROI', 'Polygon ROI'};
            app.MethodROIDropDown.FontSize = 10;
            app.MethodROIDropDown.Layout.Row = 1;
            app.MethodROIDropDown.Layout.Column = [1 4];
            app.MethodROIDropDown.Value = 'Rectangle ROI';

            % Create LBCOptionsFluidMeltandOxygenFugacityPanel
            app.LBCOptionsFluidMeltandOxygenFugacityPanel = uipanel(app.GridLayout6);
            app.LBCOptionsFluidMeltandOxygenFugacityPanel.BorderType = 'none';
            app.LBCOptionsFluidMeltandOxygenFugacityPanel.TitlePosition = 'centertop';
            app.LBCOptionsFluidMeltandOxygenFugacityPanel.Title = 'LBC Options – Fluid, Melt and Oxygen Fugacity';
            app.LBCOptionsFluidMeltandOxygenFugacityPanel.Layout.Row = [11 19];
            app.LBCOptionsFluidMeltandOxygenFugacityPanel.Layout.Column = [1 13];
            app.LBCOptionsFluidMeltandOxygenFugacityPanel.FontAngle = 'italic';

            % Create GridLayout5_9
            app.GridLayout5_9 = uigridlayout(app.LBCOptionsFluidMeltandOxygenFugacityPanel);
            app.GridLayout5_9.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout5_9.RowHeight = {'1x', '1x', '1x', '0.2x', '1x', '0.2x', '1x', '1x', '0.2x', '1x'};
            app.GridLayout5_9.ColumnSpacing = 5;
            app.GridLayout5_9.RowSpacing = 5;

            % Create Tree_FluidModels
            app.Tree_FluidModels = uitree(app.GridLayout5_9);
            app.Tree_FluidModels.SelectionChangedFcn = createCallbackFcn(app, @Tree_FluidModelsSelectionChanged, true);
            app.Tree_FluidModels.Layout.Row = [1 3];
            app.Tree_FluidModels.Layout.Column = [1 6];

            % Create ActivateFluidSpecie_CheckBox
            app.ActivateFluidSpecie_CheckBox = uicheckbox(app.GridLayout5_9);
            app.ActivateFluidSpecie_CheckBox.ValueChangedFcn = createCallbackFcn(app, @ActivateFluidSpecie_CheckBoxValueChanged, true);
            app.ActivateFluidSpecie_CheckBox.Text = 'Activate Fluid Specie';
            app.ActivateFluidSpecie_CheckBox.Layout.Row = 1;
            app.ActivateFluidSpecie_CheckBox.Layout.Column = [7 11];

            % Create OptimizeNFluid_CheckBox
            app.OptimizeNFluid_CheckBox = uicheckbox(app.GridLayout5_9);
            app.OptimizeNFluid_CheckBox.ValueChangedFcn = createCallbackFcn(app, @OptimizeNFluid_CheckBoxValueChanged, true);
            app.OptimizeNFluid_CheckBox.Text = 'Optimize N';
            app.OptimizeNFluid_CheckBox.Layout.Row = 1;
            app.OptimizeNFluid_CheckBox.Layout.Column = [12 14];

            % Create ActivateMeltModel_CheckBox
            app.ActivateMeltModel_CheckBox = uicheckbox(app.GridLayout5_9);
            app.ActivateMeltModel_CheckBox.ValueChangedFcn = createCallbackFcn(app, @ActivateMeltModel_CheckBoxValueChanged, true);
            app.ActivateMeltModel_CheckBox.Text = 'Activate Melt Model';
            app.ActivateMeltModel_CheckBox.Layout.Row = 5;
            app.ActivateMeltModel_CheckBox.Layout.Column = [1 5];

            % Create MeltModelPhase_DropDown
            app.MeltModelPhase_DropDown = uidropdown(app.GridLayout5_9);
            app.MeltModelPhase_DropDown.ValueChangedFcn = createCallbackFcn(app, @MeltModelPhase_DropDownValueChanged, true);
            app.MeltModelPhase_DropDown.Layout.Row = 5;
            app.MeltModelPhase_DropDown.Layout.Column = [6 9];

            % Create IncudeMeltEvaluation_CheckBox
            app.IncudeMeltEvaluation_CheckBox = uicheckbox(app.GridLayout5_9);
            app.IncudeMeltEvaluation_CheckBox.ValueChangedFcn = createCallbackFcn(app, @IncudeMeltEvaluation_CheckBoxValueChanged, true);
            app.IncudeMeltEvaluation_CheckBox.Text = 'Incude in the evaluation';
            app.IncudeMeltEvaluation_CheckBox.Layout.Row = 5;
            app.IncudeMeltEvaluation_CheckBox.Layout.Column = [10 14];

            % Create MinNfluid_EditField
            app.MinNfluid_EditField = uieditfield(app.GridLayout5_9, 'numeric');
            app.MinNfluid_EditField.Limits = [0 2000];
            app.MinNfluid_EditField.ValueChangedFcn = createCallbackFcn(app, @MinNfluid_EditFieldValueChanged, true);
            app.MinNfluid_EditField.Layout.Row = 3;
            app.MinNfluid_EditField.Layout.Column = [9 10];
            app.MinNfluid_EditField.Value = 0.0001;

            % Create MinEditFieldLabel
            app.MinEditFieldLabel = uilabel(app.GridLayout5_9);
            app.MinEditFieldLabel.HorizontalAlignment = 'right';
            app.MinEditFieldLabel.Layout.Row = 3;
            app.MinEditFieldLabel.Layout.Column = [7 8];
            app.MinEditFieldLabel.Text = 'Min';

            % Create MaxNfluid_EditField
            app.MaxNfluid_EditField = uieditfield(app.GridLayout5_9, 'numeric');
            app.MaxNfluid_EditField.Limits = [0 2000];
            app.MaxNfluid_EditField.ValueChangedFcn = createCallbackFcn(app, @MaxNfluid_EditFieldValueChanged, true);
            app.MaxNfluid_EditField.Layout.Row = 3;
            app.MaxNfluid_EditField.Layout.Column = [13 14];
            app.MaxNfluid_EditField.Value = 1;

            % Create MaxEditFieldLabel
            app.MaxEditFieldLabel = uilabel(app.GridLayout5_9);
            app.MaxEditFieldLabel.HorizontalAlignment = 'right';
            app.MaxEditFieldLabel.Layout.Row = 3;
            app.MaxEditFieldLabel.Layout.Column = [11 12];
            app.MaxEditFieldLabel.Text = 'Max';

            % Create BulkNfluid_EditField
            app.BulkNfluid_EditField = uieditfield(app.GridLayout5_9, 'numeric');
            app.BulkNfluid_EditField.Limits = [0 2000];
            app.BulkNfluid_EditField.ValueChangedFcn = createCallbackFcn(app, @BulkNfluid_EditFieldValueChanged, true);
            app.BulkNfluid_EditField.Layout.Row = 2;
            app.BulkNfluid_EditField.Layout.Column = [9 10];

            % Create BulkNEditFieldLabel
            app.BulkNEditFieldLabel = uilabel(app.GridLayout5_9);
            app.BulkNEditFieldLabel.HorizontalAlignment = 'right';
            app.BulkNEditFieldLabel.Layout.Row = 2;
            app.BulkNEditFieldLabel.Layout.Column = [7 8];
            app.BulkNEditFieldLabel.Text = 'Bulk (N)';

            % Create ActivateExtraOxygen_CheckBox
            app.ActivateExtraOxygen_CheckBox = uicheckbox(app.GridLayout5_9);
            app.ActivateExtraOxygen_CheckBox.ValueChangedFcn = createCallbackFcn(app, @ActivateExtraOxygen_CheckBoxValueChanged, true);
            app.ActivateExtraOxygen_CheckBox.Text = 'Activate Extra Oxygen';
            app.ActivateExtraOxygen_CheckBox.Layout.Row = 7;
            app.ActivateExtraOxygen_CheckBox.Layout.Column = [1 5];

            % Create BulkO_EditField
            app.BulkO_EditField = uieditfield(app.GridLayout5_9, 'numeric');
            app.BulkO_EditField.Limits = [0 2000];
            app.BulkO_EditField.ValueChangedFcn = createCallbackFcn(app, @BulkO_EditFieldValueChanged, true);
            app.BulkO_EditField.Layout.Row = 7;
            app.BulkO_EditField.Layout.Column = [9 10];

            % Create BulkOEditFieldLabel
            app.BulkOEditFieldLabel = uilabel(app.GridLayout5_9);
            app.BulkOEditFieldLabel.HorizontalAlignment = 'right';
            app.BulkOEditFieldLabel.Layout.Row = 7;
            app.BulkOEditFieldLabel.Layout.Column = [7 8];
            app.BulkOEditFieldLabel.Text = 'Bulk (O)';

            % Create OptimizeO_CheckBox
            app.OptimizeO_CheckBox = uicheckbox(app.GridLayout5_9);
            app.OptimizeO_CheckBox.ValueChangedFcn = createCallbackFcn(app, @OptimizeO_CheckBoxValueChanged, true);
            app.OptimizeO_CheckBox.Text = 'Optimize O';
            app.OptimizeO_CheckBox.Layout.Row = 7;
            app.OptimizeO_CheckBox.Layout.Column = [12 14];

            % Create OxMin_EditField
            app.OxMin_EditField = uieditfield(app.GridLayout5_9, 'numeric');
            app.OxMin_EditField.Limits = [0 2000];
            app.OxMin_EditField.ValueChangedFcn = createCallbackFcn(app, @OxMin_EditFieldValueChanged, true);
            app.OxMin_EditField.Layout.Row = 8;
            app.OxMin_EditField.Layout.Column = [9 10];
            app.OxMin_EditField.Value = 0.0001;

            % Create MinEditField_2Label
            app.MinEditField_2Label = uilabel(app.GridLayout5_9);
            app.MinEditField_2Label.HorizontalAlignment = 'right';
            app.MinEditField_2Label.Layout.Row = 8;
            app.MinEditField_2Label.Layout.Column = [7 8];
            app.MinEditField_2Label.Text = 'Min';

            % Create OxMax_EditField
            app.OxMax_EditField = uieditfield(app.GridLayout5_9, 'numeric');
            app.OxMax_EditField.Limits = [0 2000];
            app.OxMax_EditField.ValueChangedFcn = createCallbackFcn(app, @OxMax_EditFieldValueChanged, true);
            app.OxMax_EditField.Layout.Row = 8;
            app.OxMax_EditField.Layout.Column = [13 14];
            app.OxMax_EditField.Value = 1;

            % Create MaxEditField_2Label
            app.MaxEditField_2Label = uilabel(app.GridLayout5_9);
            app.MaxEditField_2Label.HorizontalAlignment = 'right';
            app.MaxEditField_2Label.Layout.Row = 8;
            app.MaxEditField_2Label.Layout.Column = [11 12];
            app.MaxEditField_2Label.Text = 'Max';

            % Create ActivateOxygenBuffer_CheckBox
            app.ActivateOxygenBuffer_CheckBox = uicheckbox(app.GridLayout5_9);
            app.ActivateOxygenBuffer_CheckBox.Text = 'Activate Oxygen Buffer';
            app.ActivateOxygenBuffer_CheckBox.Layout.Row = 10;
            app.ActivateOxygenBuffer_CheckBox.Layout.Column = [1 5];

            % Create Buffer_DropDown
            app.Buffer_DropDown = uidropdown(app.GridLayout5_9);
            app.Buffer_DropDown.Layout.Row = 10;
            app.Buffer_DropDown.Layout.Column = [6 9];

            % Create LBC_NextButton
            app.LBC_NextButton = uibutton(app.GridLayout6, 'push');
            app.LBC_NextButton.ButtonPushedFcn = createCallbackFcn(app, @LBC_NextButtonPushed, true);
            app.LBC_NextButton.Icon = '023-next.png';
            app.LBC_NextButton.IconAlignment = 'top';
            app.LBC_NextButton.Layout.Row = [20 21];
            app.LBC_NextButton.Layout.Column = [12 13];
            app.LBC_NextButton.Text = 'Next';

            % Create LBCEditField
            app.LBCEditField = uieditfield(app.GridLayout6, 'text');
            app.LBCEditField.HorizontalAlignment = 'center';
            app.LBCEditField.FontSize = 10;
            app.LBCEditField.Layout.Row = 21;
            app.LBCEditField.Layout.Column = [2 11];
            app.LBCEditField.Value = 'Click + above and select a ROI';

            % Create LBCEditFieldLabel
            app.LBCEditFieldLabel = uilabel(app.GridLayout6);
            app.LBCEditFieldLabel.HorizontalAlignment = 'right';
            app.LBCEditFieldLabel.Layout.Row = 21;
            app.LBCEditFieldLabel.Layout.Column = 1;
            app.LBCEditFieldLabel.Text = 'LBC';

            % Create PhasesTab
            app.PhasesTab = uitab(app.TabGroup);
            app.PhasesTab.Title = 'Phases';

            % Create GridLayout7
            app.GridLayout7 = uigridlayout(app.PhasesTab);
            app.GridLayout7.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout7.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout7.ColumnSpacing = 5;
            app.GridLayout7.RowSpacing = 5;

            % Create AddManagePhasesSolutionsPanel
            app.AddManagePhasesSolutionsPanel = uipanel(app.GridLayout7);
            app.AddManagePhasesSolutionsPanel.BorderType = 'none';
            app.AddManagePhasesSolutionsPanel.TitlePosition = 'centertop';
            app.AddManagePhasesSolutionsPanel.Title = 'Add / Manage Phases & Solutions';
            app.AddManagePhasesSolutionsPanel.Layout.Row = [1 17];
            app.AddManagePhasesSolutionsPanel.Layout.Column = [1 13];
            app.AddManagePhasesSolutionsPanel.FontAngle = 'italic';

            % Create GridLayout5_10
            app.GridLayout5_10 = uigridlayout(app.AddManagePhasesSolutionsPanel);
            app.GridLayout5_10.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout5_10.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout5_10.ColumnSpacing = 5;
            app.GridLayout5_10.RowSpacing = 5;

            % Create Tree_PhasesModel
            app.Tree_PhasesModel = uitree(app.GridLayout5_10);
            app.Tree_PhasesModel.SelectionChangedFcn = createCallbackFcn(app, @Tree_PhasesModelSelectionChanged, true);
            app.Tree_PhasesModel.Layout.Row = [1 9];
            app.Tree_PhasesModel.Layout.Column = [1 6];

            % Create SelectforLBCCheckBox
            app.SelectforLBCCheckBox = uicheckbox(app.GridLayout5_10);
            app.SelectforLBCCheckBox.ValueChangedFcn = createCallbackFcn(app, @SelectforLBCCheckBoxValueChanged, true);
            app.SelectforLBCCheckBox.Text = 'Select for LBC';
            app.SelectforLBCCheckBox.Layout.Row = 1;
            app.SelectforLBCCheckBox.Layout.Column = [11 14];

            % Create SelectforBACheckBox
            app.SelectforBACheckBox = uicheckbox(app.GridLayout5_10);
            app.SelectforBACheckBox.ValueChangedFcn = createCallbackFcn(app, @SelectforBACheckBoxValueChanged, true);
            app.SelectforBACheckBox.Text = 'Select for BA';
            app.SelectforBACheckBox.Layout.Row = 1;
            app.SelectforBACheckBox.Layout.Column = [7 10];

            % Create UITable_PhasesModel
            app.UITable_PhasesModel = uitable(app.GridLayout5_10);
            app.UITable_PhasesModel.ColumnName = {'Phase'; 'Model'; 'Group'; 'Bingo'; 'LBC'};
            app.UITable_PhasesModel.RowName = {};
            app.UITable_PhasesModel.Layout.Row = [11 17];
            app.UITable_PhasesModel.Layout.Column = [1 14];

            % Create ModelDropDownLabel
            app.ModelDropDownLabel = uilabel(app.GridLayout5_10);
            app.ModelDropDownLabel.HorizontalAlignment = 'right';
            app.ModelDropDownLabel.Layout.Row = 3;
            app.ModelDropDownLabel.Layout.Column = [7 8];
            app.ModelDropDownLabel.Text = 'Model';

            % Create ModelPhaseDropDown
            app.ModelPhaseDropDown = uidropdown(app.GridLayout5_10);
            app.ModelPhaseDropDown.ValueChangedFcn = createCallbackFcn(app, @ModelPhaseDropDownValueChanged, true);
            app.ModelPhaseDropDown.Layout.Row = 3;
            app.ModelPhaseDropDown.Layout.Column = [9 14];

            % Create ButtonAddROI_Phase
            app.ButtonAddROI_Phase = uibutton(app.GridLayout5_10, 'push');
            app.ButtonAddROI_Phase.ButtonPushedFcn = createCallbackFcn(app, @ButtonAddROI_PhasePushed, true);
            app.ButtonAddROI_Phase.Icon = 'plus_circle.png';
            app.ButtonAddROI_Phase.Layout.Row = 5;
            app.ButtonAddROI_Phase.Layout.Column = 12;
            app.ButtonAddROI_Phase.Text = '';

            % Create ButtonEliminateROI_Phase
            app.ButtonEliminateROI_Phase = uibutton(app.GridLayout5_10, 'push');
            app.ButtonEliminateROI_Phase.ButtonPushedFcn = createCallbackFcn(app, @ButtonEliminateROI_PhasePushed, true);
            app.ButtonEliminateROI_Phase.Icon = 'minus_circle.png';
            app.ButtonEliminateROI_Phase.Enable = 'off';
            app.ButtonEliminateROI_Phase.Layout.Row = 5;
            app.ButtonEliminateROI_Phase.Layout.Column = 13;
            app.ButtonEliminateROI_Phase.Text = '';

            % Create MethodROIDropDown_Phase
            app.MethodROIDropDown_Phase = uidropdown(app.GridLayout5_10);
            app.MethodROIDropDown_Phase.Items = {'Rectangle ROI', 'Polygon ROI'};
            app.MethodROIDropDown_Phase.FontSize = 10;
            app.MethodROIDropDown_Phase.Layout.Row = 5;
            app.MethodROIDropDown_Phase.Layout.Column = [8 11];
            app.MethodROIDropDown_Phase.Value = 'Rectangle ROI';

            % Create TreeMinGroups
            app.TreeMinGroups = uitree(app.GridLayout5_10);
            app.TreeMinGroups.SelectionChangedFcn = createCallbackFcn(app, @TreeMinGroupsSelectionChanged, true);
            app.TreeMinGroups.Layout.Row = [6 9];
            app.TreeMinGroups.Layout.Column = [8 13];

            % Create Phase_NextButton
            app.Phase_NextButton = uibutton(app.GridLayout7, 'push');
            app.Phase_NextButton.ButtonPushedFcn = createCallbackFcn(app, @Phase_NextButtonPushed, true);
            app.Phase_NextButton.Icon = '023-next.png';
            app.Phase_NextButton.IconAlignment = 'top';
            app.Phase_NextButton.Layout.Row = [20 21];
            app.Phase_NextButton.Layout.Column = [12 13];
            app.Phase_NextButton.Text = 'Next';

            % Create BingoTab
            app.BingoTab = uitab(app.TabGroup);
            app.BingoTab.Title = 'Bingo';

            % Create GridLayout8
            app.GridLayout8 = uigridlayout(app.BingoTab);
            app.GridLayout8.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout8.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout8.ColumnSpacing = 5;
            app.GridLayout8.RowSpacing = 5;

            % Create SinglePTXminimizationBingoPanel
            app.SinglePTXminimizationBingoPanel = uipanel(app.GridLayout8);
            app.SinglePTXminimizationBingoPanel.BorderType = 'none';
            app.SinglePTXminimizationBingoPanel.TitlePosition = 'centertop';
            app.SinglePTXminimizationBingoPanel.Title = 'Single P–T–X minimization (Bingo)';
            app.SinglePTXminimizationBingoPanel.Layout.Row = [1 4];
            app.SinglePTXminimizationBingoPanel.Layout.Column = [1 13];
            app.SinglePTXminimizationBingoPanel.FontAngle = 'italic';

            % Create GridLayout5_6
            app.GridLayout5_6 = uigridlayout(app.SinglePTXminimizationBingoPanel);
            app.GridLayout5_6.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.3x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout5_6.RowHeight = {'0.6x', '1x', '1x', '0.2x'};
            app.GridLayout5_6.ColumnSpacing = 5;
            app.GridLayout5_6.RowSpacing = 5;

            % Create TemperatureCLabel
            app.TemperatureCLabel = uilabel(app.GridLayout5_6);
            app.TemperatureCLabel.HorizontalAlignment = 'right';
            app.TemperatureCLabel.Layout.Row = 2;
            app.TemperatureCLabel.Layout.Column = [3 5];
            app.TemperatureCLabel.Text = 'Temperature (°C)';

            % Create BingoTemperatureEditField
            app.BingoTemperatureEditField = uieditfield(app.GridLayout5_6, 'numeric');
            app.BingoTemperatureEditField.Limits = [50 1500];
            app.BingoTemperatureEditField.Layout.Row = 2;
            app.BingoTemperatureEditField.Layout.Column = [6 7];
            app.BingoTemperatureEditField.Value = 680;

            % Create PressureGPaLabel
            app.PressureGPaLabel = uilabel(app.GridLayout5_6);
            app.PressureGPaLabel.HorizontalAlignment = 'right';
            app.PressureGPaLabel.Layout.Row = 3;
            app.PressureGPaLabel.Layout.Column = [3 5];
            app.PressureGPaLabel.Text = 'Pressure (GPa)';

            % Create BingoPressureEditField
            app.BingoPressureEditField = uieditfield(app.GridLayout5_6, 'numeric');
            app.BingoPressureEditField.Limits = [0.01 10];
            app.BingoPressureEditField.Layout.Row = 3;
            app.BingoPressureEditField.Layout.Column = [6 7];
            app.BingoPressureEditField.Value = 0.7;

            % Create BingoButton
            app.BingoButton = uibutton(app.GridLayout5_6, 'push');
            app.BingoButton.ButtonPushedFcn = createCallbackFcn(app, @BingoButtonPushed, true);
            app.BingoButton.Icon = 'logo_transparent.png';
            app.BingoButton.IconAlignment = 'top';
            app.BingoButton.FontSize = 9;
            app.BingoButton.FontWeight = 'bold';
            app.BingoButton.Layout.Row = [2 3];
            app.BingoButton.Layout.Column = [9 10];
            app.BingoButton.Text = 'Bingo';

            % Create ButtonSaveResultBingo
            app.ButtonSaveResultBingo = uibutton(app.GridLayout8, 'push');
            app.ButtonSaveResultBingo.ButtonPushedFcn = createCallbackFcn(app, @ButtonSaveResultBingoPushed, true);
            app.ButtonSaveResultBingo.Icon = '022-save-as.png';
            app.ButtonSaveResultBingo.Layout.Row = 5;
            app.ButtonSaveResultBingo.Layout.Column = 13;
            app.ButtonSaveResultBingo.Text = '';

            % Create Text_Report
            app.Text_Report = uitextarea(app.GridLayout8);
            app.Text_Report.WordWrap = 'off';
            app.Text_Report.FontName = 'Courier New';
            app.Text_Report.FontSize = 10;
            app.Text_Report.Layout.Row = [6 21];
            app.Text_Report.Layout.Column = [1 13];

            % Create AntidoteTab
            app.AntidoteTab = uitab(app.TabGroup);
            app.AntidoteTab.Title = 'Antidote';

            % Create GridLayout9
            app.GridLayout9 = uigridlayout(app.AntidoteTab);
            app.GridLayout9.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout9.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout9.ColumnSpacing = 5;
            app.GridLayout9.RowSpacing = 5;

            % Create InversionoptimizationAntidotePanel
            app.InversionoptimizationAntidotePanel = uipanel(app.GridLayout9);
            app.InversionoptimizationAntidotePanel.BorderType = 'none';
            app.InversionoptimizationAntidotePanel.TitlePosition = 'centertop';
            app.InversionoptimizationAntidotePanel.Title = 'Inversion & optimization (Antidote)';
            app.InversionoptimizationAntidotePanel.Layout.Row = [1 12];
            app.InversionoptimizationAntidotePanel.Layout.Column = [1 13];
            app.InversionoptimizationAntidotePanel.FontAngle = 'italic';

            % Create GridLayout5_7
            app.GridLayout5_7 = uigridlayout(app.InversionoptimizationAntidotePanel);
            app.GridLayout5_7.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout5_7.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout5_7.ColumnSpacing = 5;
            app.GridLayout5_7.RowSpacing = 5;

            % Create AntidoteTree
            app.AntidoteTree = uitree(app.GridLayout5_7);
            app.AntidoteTree.SelectionChangedFcn = createCallbackFcn(app, @AntidoteTreeSelectionChanged, true);
            app.AntidoteTree.FontSize = 11;
            app.AntidoteTree.Layout.Row = [1 12];
            app.AntidoteTree.Layout.Column = [1 9];

            % Create GlobalinversionNode
            app.GlobalinversionNode = uitreenode(app.AntidoteTree);
            app.GlobalinversionNode.NodeData = [1 0];
            app.GlobalinversionNode.Text = 'Global inversion';

            % Create Recipe1_Node
            app.Recipe1_Node = uitreenode(app.GlobalinversionNode);
            app.Recipe1_Node.NodeData = [1 1];
            app.Recipe1_Node.Icon = 'download.png';
            app.Recipe1_Node.Text = 'Find optimal P–T(–X)';

            % Create Recipe2_Node
            app.Recipe2_Node = uitreenode(app.GlobalinversionNode);
            app.Recipe2_Node.NodeData = [1 2];
            app.Recipe2_Node.Icon = 'connectivity.png';
            app.Recipe2_Node.Text = 'P–T map of Q factors';

            % Create Recipe3_Node
            app.Recipe3_Node = uitreenode(app.GlobalinversionNode);
            app.Recipe3_Node.NodeData = [1 3];
            app.Recipe3_Node.Icon = 'connection_pattern.png';
            app.Recipe3_Node.Text = 'P–T uncertainty';

            % Create SinglephasethermobarometryNode
            app.SinglephasethermobarometryNode = uitreenode(app.AntidoteTree);
            app.SinglephasethermobarometryNode.NodeData = [2 0];
            app.SinglephasethermobarometryNode.Text = 'Single-phase thermobarometry';

            % Create Recipe4_Node
            app.Recipe4_Node = uitreenode(app.SinglephasethermobarometryNode);
            app.Recipe4_Node.NodeData = [2 1];
            app.Recipe4_Node.Icon = 'download_2.png';
            app.Recipe4_Node.Text = 'Find optimal P–T';

            % Create Recipe5_Node
            app.Recipe5_Node = uitreenode(app.SinglephasethermobarometryNode);
            app.Recipe5_Node.NodeData = [2 2];
            app.Recipe5_Node.Icon = 'exit_focus_round.png';
            app.Recipe5_Node.Text = 'P–T map';

            % Create Recipe6_Node
            app.Recipe6_Node = uitreenode(app.SinglephasethermobarometryNode);
            app.Recipe6_Node.NodeData = [2 3];
            app.Recipe6_Node.Icon = 'arrow_all_2.png';
            app.Recipe6_Node.Text = 'P–T uncertainty';

            % Create SensitivitytestsNode
            app.SensitivitytestsNode = uitreenode(app.AntidoteTree);
            app.SensitivitytestsNode.NodeData = [3 0];
            app.SensitivitytestsNode.Text = 'Sensitivity tests';

            % Create Recipe7_Node
            app.Recipe7_Node = uitreenode(app.SensitivitytestsNode);
            app.Recipe7_Node.NodeData = [3 1];
            app.Recipe7_Node.Icon = 'dice_3.png';
            app.Recipe7_Node.Text = 'Bulk sensitivity';

            % Create Recipe8_Node
            app.Recipe8_Node = uitreenode(app.SensitivitytestsNode);
            app.Recipe8_Node.NodeData = [3 2];
            app.Recipe8_Node.Icon = 'dice_2.png';
            app.Recipe8_Node.Text = 'P–T sensitivity';

            % Create Recipe9_Node
            app.Recipe9_Node = uitreenode(app.SensitivitytestsNode);
            app.Recipe9_Node.NodeData = [3 3];
            app.Recipe9_Node.Icon = 'dice_5.png';
            app.Recipe9_Node.Text = 'P–T-bulk sensitivity';

            % Create TexturalinvestigationNode
            app.TexturalinvestigationNode = uitreenode(app.AntidoteTree);
            app.TexturalinvestigationNode.NodeData = [4 0];
            app.TexturalinvestigationNode.Text = 'Textural investigation';

            % Create Recipe10_Node
            app.Recipe10_Node = uitreenode(app.TexturalinvestigationNode);
            app.Recipe10_Node.NodeData = [4 1];
            app.Recipe10_Node.Icon = 'grid_system.png';
            app.Recipe10_Node.Text = 'Floating window (fixed P–T, variable bulk)';

            % Create Recipe11_Node
            app.Recipe11_Node = uitreenode(app.TexturalinvestigationNode);
            app.Recipe11_Node.NodeData = [4 2];
            app.Recipe11_Node.Icon = 'grid.png';
            app.Recipe11_Node.Text = 'Scanning window (find optimal P–T, variable bulk)';

            % Create Recipe12_Node
            app.Recipe12_Node = uitreenode(app.TexturalinvestigationNode);
            app.Recipe12_Node.NodeData = [4 3];
            app.Recipe12_Node.Icon = 'images_round.png';
            app.Recipe12_Node.Text = 'Growing window (find optimal P–T, variable bulk)';

            % Create Recipe13_Node
            app.Recipe13_Node = uitreenode(app.TexturalinvestigationNode);
            app.Recipe13_Node.NodeData = [4 4];
            app.Recipe13_Node.Icon = 'weighted_storage_cube.png';
            app.Recipe13_Node.Text = 'Chemical potential mapping (fixed P–T)';

            % Create OptimizationofcompositionalandactivityvariablesNode
            app.OptimizationofcompositionalandactivityvariablesNode = uitreenode(app.AntidoteTree);
            app.OptimizationofcompositionalandactivityvariablesNode.NodeData = [5 0];
            app.OptimizationofcompositionalandactivityvariablesNode.Text = 'Optimization of compositional and activity variables';

            % Create Recipe14_Node
            app.Recipe14_Node = uitreenode(app.OptimizationofcompositionalandactivityvariablesNode);
            app.Recipe14_Node.NodeData = [5 1];
            app.Recipe14_Node.Icon = 'drop_3.png';
            app.Recipe14_Node.Text = 'Scanning H (fixed P–T)';

            % Create Recipe15_Node
            app.Recipe15_Node = uitreenode(app.OptimizationofcompositionalandactivityvariablesNode);
            app.Recipe15_Node.NodeData = [5 2];
            app.Recipe15_Node.Icon = 'drop_2.png';
            app.Recipe15_Node.Text = 'Scanning C (fixed P–T)';

            % Create Recipe16_Node
            app.Recipe16_Node = uitreenode(app.OptimizationofcompositionalandactivityvariablesNode);
            app.Recipe16_Node.NodeData = [5 3];
            app.Recipe16_Node.Icon = 'drop.png';
            app.Recipe16_Node.Text = 'Scanning O (fixed P–T)';

            % Create AntidoteButton
            app.AntidoteButton = uibutton(app.GridLayout5_7, 'push');
            app.AntidoteButton.ButtonPushedFcn = createCallbackFcn(app, @AntidoteButtonPushed, true);
            app.AntidoteButton.Icon = 'logo_transparent.png';
            app.AntidoteButton.IconAlignment = 'top';
            app.AntidoteButton.FontSize = 9;
            app.AntidoteButton.FontWeight = 'bold';
            app.AntidoteButton.Layout.Row = [11 12];
            app.AntidoteButton.Layout.Column = [13 14];
            app.AntidoteButton.Text = 'Antidote';

            % Create TabAntidote
            app.TabAntidote = uitabgroup(app.GridLayout5_7);
            app.TabAntidote.Layout.Row = [1 10];
            app.TabAntidote.Layout.Column = [10 14];

            % Create InversionTab
            app.InversionTab = uitab(app.TabAntidote);
            app.InversionTab.Title = 'Inversion';

            % Create GridLayout12
            app.GridLayout12 = uigridlayout(app.InversionTab);
            app.GridLayout12.ColumnWidth = {'1x', '1x', '1x', '1x', '1x'};
            app.GridLayout12.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout12.ColumnSpacing = 5;
            app.GridLayout12.RowSpacing = 5;

            % Create AntidoteExploratoryscanningCheckBox
            app.AntidoteExploratoryscanningCheckBox = uicheckbox(app.GridLayout12);
            app.AntidoteExploratoryscanningCheckBox.Text = 'Exploratory scanning';
            app.AntidoteExploratoryscanningCheckBox.FontSize = 10;
            app.AntidoteExploratoryscanningCheckBox.Layout.Row = 1;
            app.AntidoteExploratoryscanningCheckBox.Layout.Column = [1 5];
            app.AntidoteExploratoryscanningCheckBox.Value = true;

            % Create GridsizepxEditFieldLabel
            app.GridsizepxEditFieldLabel = uilabel(app.GridLayout12);
            app.GridsizepxEditFieldLabel.FontSize = 10;
            app.GridsizepxEditFieldLabel.Layout.Row = 2;
            app.GridsizepxEditFieldLabel.Layout.Column = [1 3];
            app.GridsizepxEditFieldLabel.Text = 'Grid size (px)';

            % Create AntidoteGridresolutionEditField
            app.AntidoteGridresolutionEditField = uieditfield(app.GridLayout12, 'numeric');
            app.AntidoteGridresolutionEditField.HorizontalAlignment = 'center';
            app.AntidoteGridresolutionEditField.Layout.Row = 2;
            app.AntidoteGridresolutionEditField.Layout.Column = [4 5];
            app.AntidoteGridresolutionEditField.Value = 10;

            % Create SelectedPhaseOptiDropDown
            app.SelectedPhaseOptiDropDown = uidropdown(app.GridLayout12);
            app.SelectedPhaseOptiDropDown.FontSize = 11;
            app.SelectedPhaseOptiDropDown.Layout.Row = 5;
            app.SelectedPhaseOptiDropDown.Layout.Column = [1 5];

            % Create SelectedPhaseLabel
            app.SelectedPhaseLabel = uilabel(app.GridLayout12);
            app.SelectedPhaseLabel.VerticalAlignment = 'bottom';
            app.SelectedPhaseLabel.FontSize = 10;
            app.SelectedPhaseLabel.FontWeight = 'bold';
            app.SelectedPhaseLabel.Layout.Row = 4;
            app.SelectedPhaseLabel.Layout.Column = [1 5];
            app.SelectedPhaseLabel.Text = 'Selected Phase:';

            % Create MCTab
            app.MCTab = uitab(app.TabAntidote);
            app.MCTab.Title = 'MC';

            % Create GridLayout14
            app.GridLayout14 = uigridlayout(app.MCTab);
            app.GridLayout14.ColumnWidth = {'1x', '1x', '1x', '1x', '1x'};
            app.GridLayout14.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout14.ColumnSpacing = 5;
            app.GridLayout14.RowSpacing = 5;

            % Create ToleranceLabel
            app.ToleranceLabel = uilabel(app.GridLayout14);
            app.ToleranceLabel.FontSize = 11;
            app.ToleranceLabel.Layout.Row = 1;
            app.ToleranceLabel.Layout.Column = [1 3];
            app.ToleranceLabel.Text = 'Tolerance (%)';

            % Create AntidoteToleranceEditField
            app.AntidoteToleranceEditField = uieditfield(app.GridLayout14, 'numeric');
            app.AntidoteToleranceEditField.Limits = [1e-05 99.99999];
            app.AntidoteToleranceEditField.HorizontalAlignment = 'center';
            app.AntidoteToleranceEditField.FontSize = 11;
            app.AntidoteToleranceEditField.Layout.Row = 1;
            app.AntidoteToleranceEditField.Layout.Column = [4 5];
            app.AntidoteToleranceEditField.Value = 2;

            % Create Stage1Label
            app.Stage1Label = uilabel(app.GridLayout14);
            app.Stage1Label.FontSize = 11;
            app.Stage1Label.Layout.Row = 3;
            app.Stage1Label.Layout.Column = [1 3];
            app.Stage1Label.Text = 'Stage [1]';

            % Create AntidoteNbPerm1EditField
            app.AntidoteNbPerm1EditField = uieditfield(app.GridLayout14, 'numeric');
            app.AntidoteNbPerm1EditField.Limits = [10 1000];
            app.AntidoteNbPerm1EditField.HorizontalAlignment = 'center';
            app.AntidoteNbPerm1EditField.FontSize = 11;
            app.AntidoteNbPerm1EditField.Layout.Row = 3;
            app.AntidoteNbPerm1EditField.Layout.Column = [4 5];
            app.AntidoteNbPerm1EditField.Value = 50;

            % Create Stage2Label
            app.Stage2Label = uilabel(app.GridLayout14);
            app.Stage2Label.FontSize = 11;
            app.Stage2Label.Layout.Row = 4;
            app.Stage2Label.Layout.Column = [1 3];
            app.Stage2Label.Text = 'Stage [2]';

            % Create AntidoteNbPerm2EditField
            app.AntidoteNbPerm2EditField = uieditfield(app.GridLayout14, 'numeric');
            app.AntidoteNbPerm2EditField.Limits = [10 1000];
            app.AntidoteNbPerm2EditField.HorizontalAlignment = 'center';
            app.AntidoteNbPerm2EditField.FontSize = 11;
            app.AntidoteNbPerm2EditField.Layout.Row = 4;
            app.AntidoteNbPerm2EditField.Layout.Column = [4 5];
            app.AntidoteNbPerm2EditField.Value = 10;

            % Create Positionpx1sLabel
            app.Positionpx1sLabel = uilabel(app.GridLayout14);
            app.Positionpx1sLabel.FontSize = 11;
            app.Positionpx1sLabel.Layout.Row = 6;
            app.Positionpx1sLabel.Layout.Column = [1 3];
            app.Positionpx1sLabel.Text = 'Position (px, 1s)';

            % Create AntidoteNbPerm1EditField_2
            app.AntidoteNbPerm1EditField_2 = uieditfield(app.GridLayout14, 'numeric');
            app.AntidoteNbPerm1EditField_2.Limits = [10 1000];
            app.AntidoteNbPerm1EditField_2.HorizontalAlignment = 'center';
            app.AntidoteNbPerm1EditField_2.FontSize = 11;
            app.AntidoteNbPerm1EditField_2.Layout.Row = 6;
            app.AntidoteNbPerm1EditField_2.Layout.Column = [4 5];
            app.AntidoteNbPerm1EditField_2.Value = 15;

            % Create TempC1sLabel
            app.TempC1sLabel = uilabel(app.GridLayout14);
            app.TempC1sLabel.FontSize = 11;
            app.TempC1sLabel.Layout.Row = 7;
            app.TempC1sLabel.Layout.Column = [1 3];
            app.TempC1sLabel.Text = 'Temp (°C, 1s)';

            % Create AntidoteNbPerm1EditField_3
            app.AntidoteNbPerm1EditField_3 = uieditfield(app.GridLayout14, 'numeric');
            app.AntidoteNbPerm1EditField_3.Limits = [10 1000];
            app.AntidoteNbPerm1EditField_3.HorizontalAlignment = 'center';
            app.AntidoteNbPerm1EditField_3.FontSize = 11;
            app.AntidoteNbPerm1EditField_3.Layout.Row = 7;
            app.AntidoteNbPerm1EditField_3.Layout.Column = [4 5];
            app.AntidoteNbPerm1EditField_3.Value = 15;

            % Create NumberofsimulationsLabel
            app.NumberofsimulationsLabel = uilabel(app.GridLayout14);
            app.NumberofsimulationsLabel.HorizontalAlignment = 'center';
            app.NumberofsimulationsLabel.VerticalAlignment = 'bottom';
            app.NumberofsimulationsLabel.FontSize = 10;
            app.NumberofsimulationsLabel.FontWeight = 'bold';
            app.NumberofsimulationsLabel.Layout.Row = 2;
            app.NumberofsimulationsLabel.Layout.Column = [1 5];
            app.NumberofsimulationsLabel.Text = 'Number of simulations';

            % Create UncertaintiesLabel
            app.UncertaintiesLabel = uilabel(app.GridLayout14);
            app.UncertaintiesLabel.HorizontalAlignment = 'center';
            app.UncertaintiesLabel.VerticalAlignment = 'bottom';
            app.UncertaintiesLabel.FontSize = 10;
            app.UncertaintiesLabel.FontWeight = 'bold';
            app.UncertaintiesLabel.Layout.Row = 5;
            app.UncertaintiesLabel.Layout.Column = [1 5];
            app.UncertaintiesLabel.Text = 'Uncertainties';

            % Create PressGPa1sLabel
            app.PressGPa1sLabel = uilabel(app.GridLayout14);
            app.PressGPa1sLabel.FontSize = 11;
            app.PressGPa1sLabel.Layout.Row = 8;
            app.PressGPa1sLabel.Layout.Column = [1 3];
            app.PressGPa1sLabel.Text = 'Press (GPa, 1s)';

            % Create AntidoteNbPerm1EditField_4
            app.AntidoteNbPerm1EditField_4 = uieditfield(app.GridLayout14, 'numeric');
            app.AntidoteNbPerm1EditField_4.Limits = [0.001 3];
            app.AntidoteNbPerm1EditField_4.HorizontalAlignment = 'center';
            app.AntidoteNbPerm1EditField_4.FontSize = 11;
            app.AntidoteNbPerm1EditField_4.Layout.Row = 8;
            app.AntidoteNbPerm1EditField_4.Layout.Column = [4 5];
            app.AntidoteNbPerm1EditField_4.Value = 0.1;

            % Create ROITab
            app.ROITab = uitab(app.TabAntidote);
            app.ROITab.Title = 'ROI';

            % Create GridLayout15
            app.GridLayout15 = uigridlayout(app.ROITab);
            app.GridLayout15.ColumnWidth = {'1x', '1x', '1x', '1x', '1x'};
            app.GridLayout15.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout15.ColumnSpacing = 5;
            app.GridLayout15.RowSpacing = 5;

            % Create TextureROITree
            app.TextureROITree = uitree(app.GridLayout15);
            app.TextureROITree.SelectionChangedFcn = createCallbackFcn(app, @TextureROITreeSelectionChanged, true);
            app.TextureROITree.Layout.Row = [2 5];
            app.TextureROITree.Layout.Column = [1 4];

            % Create ButtonROI_Add
            app.ButtonROI_Add = uibutton(app.GridLayout15, 'push');
            app.ButtonROI_Add.ButtonPushedFcn = createCallbackFcn(app, @ButtonROI_AddPushed, true);
            app.ButtonROI_Add.Icon = 'plus_circle.png';
            app.ButtonROI_Add.Tooltip = {'Add new ROI'};
            app.ButtonROI_Add.Layout.Row = 2;
            app.ButtonROI_Add.Layout.Column = 5;
            app.ButtonROI_Add.Text = '';

            % Create ButtonROI_Eliminate
            app.ButtonROI_Eliminate = uibutton(app.GridLayout15, 'push');
            app.ButtonROI_Eliminate.ButtonPushedFcn = createCallbackFcn(app, @ButtonROI_EliminatePushed, true);
            app.ButtonROI_Eliminate.Icon = 'minus_circle.png';
            app.ButtonROI_Eliminate.Tooltip = {'Eliminate selected ROI'};
            app.ButtonROI_Eliminate.Layout.Row = 3;
            app.ButtonROI_Eliminate.Layout.Column = 5;
            app.ButtonROI_Eliminate.Text = '';

            % Create ButtonROI_Play
            app.ButtonROI_Play = uibutton(app.GridLayout15, 'push');
            app.ButtonROI_Play.ButtonPushedFcn = createCallbackFcn(app, @ButtonROI_PlayPushed, true);
            app.ButtonROI_Play.Icon = 'play.png';
            app.ButtonROI_Play.Tooltip = {'Play path'};
            app.ButtonROI_Play.Layout.Row = 5;
            app.ButtonROI_Play.Layout.Column = 5;
            app.ButtonROI_Play.Text = '';

            % Create ParametersLabel
            app.ParametersLabel = uilabel(app.GridLayout15);
            app.ParametersLabel.HorizontalAlignment = 'center';
            app.ParametersLabel.VerticalAlignment = 'bottom';
            app.ParametersLabel.FontSize = 10;
            app.ParametersLabel.FontWeight = 'bold';
            app.ParametersLabel.Layout.Row = 6;
            app.ParametersLabel.Layout.Column = [1 5];
            app.ParametersLabel.Text = 'Parameters';

            % Create AntidoteROIparamNbStepsEditField
            app.AntidoteROIparamNbStepsEditField = uieditfield(app.GridLayout15, 'numeric');
            app.AntidoteROIparamNbStepsEditField.Limits = [1 1000];
            app.AntidoteROIparamNbStepsEditField.HorizontalAlignment = 'center';
            app.AntidoteROIparamNbStepsEditField.FontSize = 11;
            app.AntidoteROIparamNbStepsEditField.Layout.Row = 7;
            app.AntidoteROIparamNbStepsEditField.Layout.Column = [4 5];
            app.AntidoteROIparamNbStepsEditField.Value = 20;

            % Create NbstepspathLabel
            app.NbstepspathLabel = uilabel(app.GridLayout15);
            app.NbstepspathLabel.FontSize = 11;
            app.NbstepspathLabel.Layout.Row = 7;
            app.NbstepspathLabel.Layout.Column = [1 3];
            app.NbstepspathLabel.Text = 'Nb steps / path';

            % Create ButtonROI_Duplicate
            app.ButtonROI_Duplicate = uibutton(app.GridLayout15, 'push');
            app.ButtonROI_Duplicate.ButtonPushedFcn = createCallbackFcn(app, @ButtonROI_DuplicatePushed, true);
            app.ButtonROI_Duplicate.Icon = 'duplicate.png';
            app.ButtonROI_Duplicate.Tooltip = {'Duplicate selected ROI'};
            app.ButtonROI_Duplicate.Layout.Row = 4;
            app.ButtonROI_Duplicate.Layout.Column = 5;
            app.ButtonROI_Duplicate.Text = '';

            % Create ROItexturalinvestigationsLabel
            app.ROItexturalinvestigationsLabel = uilabel(app.GridLayout15);
            app.ROItexturalinvestigationsLabel.HorizontalAlignment = 'center';
            app.ROItexturalinvestigationsLabel.VerticalAlignment = 'bottom';
            app.ROItexturalinvestigationsLabel.FontSize = 10;
            app.ROItexturalinvestigationsLabel.FontWeight = 'bold';
            app.ROItexturalinvestigationsLabel.Layout.Row = 1;
            app.ROItexturalinvestigationsLabel.Layout.Column = [1 5];
            app.ROItexturalinvestigationsLabel.Text = 'ROI (textural investigations)';

            % Create HTML_AntidoteReport
            app.HTML_AntidoteReport = uihtml(app.GridLayout9);
            app.HTML_AntidoteReport.Layout.Row = 21;
            app.HTML_AntidoteReport.Layout.Column = [1 13];

            % Create Text_Report_Antidote
            app.Text_Report_Antidote = uitextarea(app.GridLayout9);
            app.Text_Report_Antidote.WordWrap = 'off';
            app.Text_Report_Antidote.FontName = 'Courier New';
            app.Text_Report_Antidote.FontSize = 10;
            app.Text_Report_Antidote.Layout.Row = [13 21];
            app.Text_Report_Antidote.Layout.Column = [1 13];

            % Create Image
            app.Image = uiimage(app.GridLayout);
            app.Image.Layout.Row = [9 12];
            app.Image.Layout.Column = [11 12];
            app.Image.ImageSource = 'logo_transparent.png';

            % Create TabGroup2
            app.TabGroup2 = uitabgroup(app.GridLayout);
            app.TabGroup2.TabLocation = 'right';
            app.TabGroup2.SelectionChangedFcn = createCallbackFcn(app, @TabGroup2SelectionChanged, true);
            app.TabGroup2.Layout.Row = [1 8];
            app.TabGroup2.Layout.Column = [6 12];

            % Create MapTab
            app.MapTab = uitab(app.TabGroup2);
            app.MapTab.Title = 'Map';

            % Create GridLayout3
            app.GridLayout3 = uigridlayout(app.MapTab);
            app.GridLayout3.ColumnWidth = {'1x'};
            app.GridLayout3.RowHeight = {'1x'};
            app.GridLayout3.ColumnSpacing = 1;
            app.GridLayout3.RowSpacing = 1;
            app.GridLayout3.Padding = [20 20 20 20];

            % Create UIAxes
            app.UIAxes = uiaxes(app.GridLayout3);
            app.UIAxes.XTick = [];
            app.UIAxes.YTick = [];
            app.UIAxes.Box = 'on';
            app.UIAxes.Layout.Row = 1;
            app.UIAxes.Layout.Column = 1;

            % Create LiveTab
            app.LiveTab = uitab(app.TabGroup2);
            app.LiveTab.Title = 'Live';

            % Create GridLayout13
            app.GridLayout13 = uigridlayout(app.LiveTab);
            app.GridLayout13.ColumnWidth = {'0.5x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.5x'};
            app.GridLayout13.RowHeight = {'0.7x', '1x', '1x', '1x', '1x', '1x', '1x', '0.5x', '1x', '1x', '1x', '1x', '1x', '1x', '0.5x'};
            app.GridLayout13.ColumnSpacing = 5;
            app.GridLayout13.RowSpacing = 5;

            % Create EditField_BestQtot
            app.EditField_BestQtot = uieditfield(app.GridLayout13, 'numeric');
            app.EditField_BestQtot.HorizontalAlignment = 'center';
            app.EditField_BestQtot.FontSize = 11;
            app.EditField_BestQtot.FontWeight = 'bold';
            app.EditField_BestQtot.Layout.Row = 5;
            app.EditField_BestQtot.Layout.Column = [11 13];

            % Create LIVEQtotalvalueLabel
            app.LIVEQtotalvalueLabel = uilabel(app.GridLayout13);
            app.LIVEQtotalvalueLabel.HorizontalAlignment = 'center';
            app.LIVEQtotalvalueLabel.FontWeight = 'bold';
            app.LIVEQtotalvalueLabel.Layout.Row = 6;
            app.LIVEQtotalvalueLabel.Layout.Column = [14 19];
            app.LIVEQtotalvalueLabel.Text = 'LIVE – Qtotal value';

            % Create LIVE_Qtot_Gauge
            app.LIVE_Qtot_Gauge = uigauge(app.GridLayout13, 'circular');
            app.LIVE_Qtot_Gauge.Layout.Row = [1 5];
            app.LIVE_Qtot_Gauge.Layout.Column = [14 19];

            % Create TotalLabel_2
            app.TotalLabel_2 = uilabel(app.GridLayout13);
            app.TotalLabel_2.HorizontalAlignment = 'center';
            app.TotalLabel_2.VerticalAlignment = 'bottom';
            app.TotalLabel_2.FontSize = 11;
            app.TotalLabel_2.FontWeight = 'bold';
            app.TotalLabel_2.FontAngle = 'italic';
            app.TotalLabel_2.Layout.Row = 4;
            app.TotalLabel_2.Layout.Column = [11 13];
            app.TotalLabel_2.Text = 'Total';

            % Create LiveDisplaySwitchLabel
            app.LiveDisplaySwitchLabel = uilabel(app.GridLayout13);
            app.LiveDisplaySwitchLabel.HorizontalAlignment = 'center';
            app.LiveDisplaySwitchLabel.VerticalAlignment = 'bottom';
            app.LiveDisplaySwitchLabel.FontSize = 11;
            app.LiveDisplaySwitchLabel.FontWeight = 'bold';
            app.LiveDisplaySwitchLabel.Layout.Row = 1;
            app.LiveDisplaySwitchLabel.Layout.Column = [11 13];
            app.LiveDisplaySwitchLabel.Text = 'Live Display';

            % Create LiveDisplaySwitch
            app.LiveDisplaySwitch = uiswitch(app.GridLayout13, 'slider');
            app.LiveDisplaySwitch.Layout.Row = 2;
            app.LiveDisplaySwitch.Layout.Column = [11 13];
            app.LiveDisplaySwitch.Value = 'On';

            % Create UIAxes_LiveAntidote1
            app.UIAxes_LiveAntidote1 = uiaxes(app.GridLayout13);
            title(app.UIAxes_LiveAntidote1, 'Title')
            xlabel(app.UIAxes_LiveAntidote1, 'X')
            ylabel(app.UIAxes_LiveAntidote1, 'Y')
            app.UIAxes_LiveAntidote1.PlotBoxAspectRatio = [1.60102301790281 1 1];
            app.UIAxes_LiveAntidote1.FontSize = 10;
            app.UIAxes_LiveAntidote1.Layout.Row = [1 7];
            app.UIAxes_LiveAntidote1.Layout.Column = [1 10];

            % Create UIAxes_LiveAntidote2
            app.UIAxes_LiveAntidote2 = uiaxes(app.GridLayout13);
            title(app.UIAxes_LiveAntidote2, 'Title')
            xlabel(app.UIAxes_LiveAntidote2, 'X')
            ylabel(app.UIAxes_LiveAntidote2, 'Y')
            zlabel(app.UIAxes_LiveAntidote2, 'Z')
            app.UIAxes_LiveAntidote2.PlotBoxAspectRatio = [1.3804347826087 1 1];
            app.UIAxes_LiveAntidote2.FontSize = 9;
            app.UIAxes_LiveAntidote2.Layout.Row = [9 13];
            app.UIAxes_LiveAntidote2.Layout.Column = [2 7];

            % Create UIAxes_LiveAntidote3
            app.UIAxes_LiveAntidote3 = uiaxes(app.GridLayout13);
            title(app.UIAxes_LiveAntidote3, 'Title')
            xlabel(app.UIAxes_LiveAntidote3, 'X')
            ylabel(app.UIAxes_LiveAntidote3, 'Y')
            zlabel(app.UIAxes_LiveAntidote3, 'Z')
            app.UIAxes_LiveAntidote3.FontSize = 9;
            app.UIAxes_LiveAntidote3.Layout.Row = [9 13];
            app.UIAxes_LiveAntidote3.Layout.Column = [8 13];

            % Create UIAxes_LiveAntidote4
            app.UIAxes_LiveAntidote4 = uiaxes(app.GridLayout13);
            title(app.UIAxes_LiveAntidote4, 'Title')
            xlabel(app.UIAxes_LiveAntidote4, 'X')
            ylabel(app.UIAxes_LiveAntidote4, 'Y')
            zlabel(app.UIAxes_LiveAntidote4, 'Z')
            app.UIAxes_LiveAntidote4.PlotBoxAspectRatio = [1.3804347826087 1 1];
            app.UIAxes_LiveAntidote4.FontSize = 9;
            app.UIAxes_LiveAntidote4.Layout.Row = [9 13];
            app.UIAxes_LiveAntidote4.Layout.Column = [14 19];

            % Create ResultsTab
            app.ResultsTab = uitab(app.TabGroup2);
            app.ResultsTab.Title = 'Results';

            % Create GridLayout11
            app.GridLayout11 = uigridlayout(app.ResultsTab);
            app.GridLayout11.ColumnWidth = {'0.5x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.5x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.5x'};
            app.GridLayout11.RowHeight = {'0.7x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.5x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout11.ColumnSpacing = 5;
            app.GridLayout11.RowSpacing = 5;

            % Create Gaude_Qasm
            app.Gaude_Qasm = uigauge(app.GridLayout11, 'semicircular');
            app.Gaude_Qasm.Layout.Row = [2 3];
            app.Gaude_Qasm.Layout.Column = [12 14];

            % Create Gauge_Qcmp
            app.Gauge_Qcmp = uigauge(app.GridLayout11, 'semicircular');
            app.Gauge_Qcmp.Layout.Row = [2 3];
            app.Gauge_Qcmp.Layout.Column = [18 20];

            % Create Gauge_Qvol
            app.Gauge_Qvol = uigauge(app.GridLayout11, 'semicircular');
            app.Gauge_Qvol.Layout.Row = [2 3];
            app.Gauge_Qvol.Layout.Column = [15 17];

            % Create AssemblageLabel
            app.AssemblageLabel = uilabel(app.GridLayout11);
            app.AssemblageLabel.HorizontalAlignment = 'center';
            app.AssemblageLabel.VerticalAlignment = 'bottom';
            app.AssemblageLabel.FontSize = 11;
            app.AssemblageLabel.FontWeight = 'bold';
            app.AssemblageLabel.FontAngle = 'italic';
            app.AssemblageLabel.Layout.Row = 1;
            app.AssemblageLabel.Layout.Column = [12 14];
            app.AssemblageLabel.Text = 'Assemblage';

            % Create ModesLabel
            app.ModesLabel = uilabel(app.GridLayout11);
            app.ModesLabel.HorizontalAlignment = 'center';
            app.ModesLabel.VerticalAlignment = 'bottom';
            app.ModesLabel.FontSize = 11;
            app.ModesLabel.FontWeight = 'bold';
            app.ModesLabel.FontAngle = 'italic';
            app.ModesLabel.Layout.Row = 1;
            app.ModesLabel.Layout.Column = [15 17];
            app.ModesLabel.Text = 'Modes';

            % Create CompositionsLabel
            app.CompositionsLabel = uilabel(app.GridLayout11);
            app.CompositionsLabel.HorizontalAlignment = 'center';
            app.CompositionsLabel.VerticalAlignment = 'bottom';
            app.CompositionsLabel.FontSize = 11;
            app.CompositionsLabel.FontWeight = 'bold';
            app.CompositionsLabel.FontAngle = 'italic';
            app.CompositionsLabel.Layout.Row = 1;
            app.CompositionsLabel.Layout.Column = [18 20];
            app.CompositionsLabel.Text = 'Compositions';

            % Create EditField_Qasm
            app.EditField_Qasm = uieditfield(app.GridLayout11, 'numeric');
            app.EditField_Qasm.HorizontalAlignment = 'center';
            app.EditField_Qasm.FontSize = 11;
            app.EditField_Qasm.FontWeight = 'bold';
            app.EditField_Qasm.Layout.Row = 4;
            app.EditField_Qasm.Layout.Column = [12 14];

            % Create EditField_Qvol
            app.EditField_Qvol = uieditfield(app.GridLayout11, 'numeric');
            app.EditField_Qvol.HorizontalAlignment = 'center';
            app.EditField_Qvol.FontSize = 11;
            app.EditField_Qvol.FontWeight = 'bold';
            app.EditField_Qvol.Layout.Row = 4;
            app.EditField_Qvol.Layout.Column = [15 17];

            % Create EditField_Qcmp
            app.EditField_Qcmp = uieditfield(app.GridLayout11, 'numeric');
            app.EditField_Qcmp.HorizontalAlignment = 'center';
            app.EditField_Qcmp.FontSize = 11;
            app.EditField_Qcmp.FontWeight = 'bold';
            app.EditField_Qcmp.Layout.Row = 4;
            app.EditField_Qcmp.Layout.Column = [18 20];

            % Create Gauge_Qtotal
            app.Gauge_Qtotal = uigauge(app.GridLayout11, 'circular');
            app.Gauge_Qtotal.Layout.Row = [5 7];
            app.Gauge_Qtotal.Layout.Column = [12 14];

            % Create TotalLabel
            app.TotalLabel = uilabel(app.GridLayout11);
            app.TotalLabel.HorizontalAlignment = 'center';
            app.TotalLabel.VerticalAlignment = 'bottom';
            app.TotalLabel.FontSize = 11;
            app.TotalLabel.FontWeight = 'bold';
            app.TotalLabel.FontAngle = 'italic';
            app.TotalLabel.Layout.Row = 6;
            app.TotalLabel.Layout.Column = [15 17];
            app.TotalLabel.Text = 'Total';

            % Create EditField_Qtotal
            app.EditField_Qtotal = uieditfield(app.GridLayout11, 'numeric');
            app.EditField_Qtotal.HorizontalAlignment = 'center';
            app.EditField_Qtotal.FontSize = 11;
            app.EditField_Qtotal.FontWeight = 'bold';
            app.EditField_Qtotal.Layout.Row = 7;
            app.EditField_Qtotal.Layout.Column = [15 17];

            % Create UIAxes2
            app.UIAxes2 = uiaxes(app.GridLayout11);
            xlabel(app.UIAxes2, 'Temperature (°C)')
            ylabel(app.UIAxes2, 'Pressure (GPa)')
            zlabel(app.UIAxes2, 'Z')
            app.UIAxes2.PlotBoxAspectRatio = [1.38478260869565 1 1];
            app.UIAxes2.XTick = [0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1];
            app.UIAxes2.FontSize = 9;
            app.UIAxes2.Layout.Row = [1 8];
            app.UIAxes2.Layout.Column = [1 10];

            % Create Result_Plot1
            app.Result_Plot1 = uiaxes(app.GridLayout11);
            app.Result_Plot1.XTick = [];
            app.Result_Plot1.YTick = [];
            app.Result_Plot1.FontSize = 9;
            app.Result_Plot1.Layout.Row = [10 15];
            app.Result_Plot1.Layout.Column = [4 9];

            % Create Result_Plot2
            app.Result_Plot2 = uiaxes(app.GridLayout11);
            app.Result_Plot2.PlotBoxAspectRatio = [1.11977715877437 1 1];
            app.Result_Plot2.XTick = [];
            app.Result_Plot2.YTick = [];
            app.Result_Plot2.FontSize = 9;
            app.Result_Plot2.Layout.Row = [10 15];
            app.Result_Plot2.Layout.Column = [13 18];

            % Create Tree_Phases
            app.Tree_Phases = uitree(app.GridLayout);
            app.Tree_Phases.SelectionChangedFcn = createCallbackFcn(app, @Tree_PhasesSelectionChanged, true);
            app.Tree_Phases.Layout.Row = [9 12];
            app.Tree_Phases.Layout.Column = [8 9];

            % Create Tree_Elem
            app.Tree_Elem = uitree(app.GridLayout);
            app.Tree_Elem.SelectionChangedFcn = createCallbackFcn(app, @Tree_ElemSelectionChanged, true);
            app.Tree_Elem.Layout.Row = [9 12];
            app.Tree_Elem.Layout.Column = [6 7];

            % Show the figure after all components are created
            app.BingoAntidote_GUI.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = BingoAntidote_exported(varargin)

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.BingoAntidote_GUI)

            % Execute the startup function
            runStartupFcn(app, @(app)startupFcn(app, varargin{:}))

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.BingoAntidote_GUI)
        end
    end
end