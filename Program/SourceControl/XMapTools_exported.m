classdef XMapTools_exported < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        XMapTools_GUI                   matlab.ui.Figure
        GridLayout                      matlab.ui.container.GridLayout
        LeftPanel                       matlab.ui.container.Panel
        GridLayout7                     matlab.ui.container.GridLayout
        TreeData_Main                   matlab.ui.container.Tree
        Node_It                         matlab.ui.container.TreeNode
        Node_Qt                         matlab.ui.container.TreeNode
        Node_Me                         matlab.ui.container.TreeNode
        Node_Re                         matlab.ui.container.TreeNode
        Node_Ot                         matlab.ui.container.TreeNode
        Node_Ct                         matlab.ui.container.TreeNode
        Node_ROI                        matlab.ui.container.TreeNode
        Node_Im                         matlab.ui.container.TreeNode
        TreeData_Additional             matlab.ui.container.Tree
        Node_Masks                      matlab.ui.container.TreeNode
        Node_TrainingSet                matlab.ui.container.TreeNode
        Node_Corrections                matlab.ui.container.TreeNode
        Node_SegScheme                  matlab.ui.container.TreeNode
        Node_Filters                    matlab.ui.container.TreeNode
        Node_Standards                  matlab.ui.container.TreeNode
        Node_MapStandards               matlab.ui.container.TreeNode
        Node_LOD                        matlab.ui.container.TreeNode
        PrimaryTreeMenuLabel            matlab.ui.control.Label
        SecondaryTreeMenuLabel          matlab.ui.control.Label
        CenterPanel                     matlab.ui.container.Panel
        GridLayout2                     matlab.ui.container.GridLayout
        TabButtonGroup                  matlab.ui.container.TabGroup
        PROJECTIMPORTTab                matlab.ui.container.Tab
        GridLayout_ImportTab            matlab.ui.container.GridLayout
        ProjectOpen                     matlab.ui.control.Button
        ProjectSave                     matlab.ui.control.Button
        ProjectSaveAs                   matlab.ui.control.Button
        PROJECTLabel                    matlab.ui.control.Label
        Label                           matlab.ui.control.Label
        ProjectName_DisplayField        matlab.ui.control.EditField
        DATACONVERSIONLabel             matlab.ui.control.Label
        ProjectName_Options             matlab.ui.control.DropDown
        ButtonImportMaps                matlab.ui.control.Button
        OPTIONSPROJECTLabel             matlab.ui.control.Label
        ResolutionmEditFieldLabel       matlab.ui.control.Label
        ResolutionField                 matlab.ui.control.NumericEditField
        DisplayScaleBar                 matlab.ui.control.CheckBox
        ColorScaleBar                   matlab.ui.control.DropDown
        ButtonRotateView                matlab.ui.control.Button
        FieldAngleView                  matlab.ui.control.NumericEditField
        ImportTab_help                  matlab.ui.control.Button
        Import_CTdata                   matlab.ui.control.Button
        MakeMosaicStd                   matlab.ui.control.Button
        MakeMosaic                      matlab.ui.control.Button
        ButtonConvertProject            matlab.ui.control.Button
        Image                           matlab.ui.control.Image
        Image_7                         matlab.ui.control.Image
        Image_8                         matlab.ui.control.Image
        Image_9                         matlab.ui.control.Image
        Image_29                        matlab.ui.control.Image
        IMPORTMAPSIMAGESLabel           matlab.ui.control.Label
        ButtonConvertLaserData          matlab.ui.control.Button
        mapsizeLabel                    matlab.ui.control.Label
        CLASSIFYTab                     matlab.ui.container.Tab
        GridLayout4                     matlab.ui.container.GridLayout
        Classify_AddTrainingSet         matlab.ui.control.Button
        EditFieldLabel                  matlab.ui.control.Label
        Classify_NameField              matlab.ui.control.EditField
        Classification_Menu             matlab.ui.control.DropDown
        Classify_Button                 matlab.ui.control.Button
        Classify_ElemList               matlab.ui.control.EditField
        Classify_Manage_Elem            matlab.ui.control.Button
        Classify_AddAllElem             matlab.ui.control.Button
        Classify_ROI_Menu               matlab.ui.control.DropDown
        TRAININGSETLabel                matlab.ui.control.Label
        CLASSIFICATIONPARAMETERSLabel   matlab.ui.control.Label
        APPLYLabel_2                    matlab.ui.control.Label
        Masks_ButtonPlotPie             matlab.ui.control.Button
        MASKANALYSISVISUALIZATIONLabel  matlab.ui.control.Label
        ClassifyTab_help                matlab.ui.control.Button
        FILTERINGOPTIONSLabel           matlab.ui.control.Label
        Classify_FilterLowProbPixels    matlab.ui.control.CheckBox
        ProbabilityLabel                matlab.ui.control.Label
        Classify_FilterProbValue        matlab.ui.control.NumericEditField
        Classify_PCA1CheckBox           matlab.ui.control.CheckBox
        Classify_RunPCAButton           matlab.ui.control.Button
        Classify_FilterMaskFile         matlab.ui.control.Button
        Classify_Modes_AddROI           matlab.ui.control.Button
        Classify_Modes_ROI_menu         matlab.ui.control.DropDown
        Image_30                        matlab.ui.control.Image
        Image_31                        matlab.ui.control.Image
        Image_4                         matlab.ui.control.Image
        Image_5                         matlab.ui.control.Image
        Image_6                         matlab.ui.control.Image
        ExtractmodesLabel               matlab.ui.control.Label
        Classify_PCA2CheckBox           matlab.ui.control.CheckBox
        Classify_TEXFCheckBox           matlab.ui.control.CheckBox
        Classify_MapsCheckBox           matlab.ui.control.CheckBox
        Classify_MixingPixelsPer        matlab.ui.control.Label
        ScalingLabel                    matlab.ui.control.Label
        Classify_ScalingMode            matlab.ui.control.DropDown
        Classify_Reproducibility        matlab.ui.control.CheckBox
        Classify_ReproducibilityValue   matlab.ui.control.Spinner
        Classify_OptionLabel            matlab.ui.control.Label
        Classify_OptionValue            matlab.ui.control.Spinner
        Classify_ActivateAssistant      matlab.ui.control.CheckBox
        Classify_PointCountingModes     matlab.ui.control.Button
        PointcountingLabel              matlab.ui.control.Label
        Classify_NbPointCounting        matlab.ui.control.NumericEditField
        Classify_MC_PointCountingCheckBox  matlab.ui.control.CheckBox
        Classify_Manage_Elem_DeleteAll  matlab.ui.control.Button
        CALIBRATETab                    matlab.ui.container.Tab
        CalibrateGridLayout             matlab.ui.container.GridLayout
        EPMASTANDARDDATALabel           matlab.ui.control.Label
        ImportStandards                 matlab.ui.control.Button
        AddStandardsManual              matlab.ui.control.Button
        ImportAutoMode                  matlab.ui.control.CheckBox
        STANDARDIZATIONLabel            matlab.ui.control.Label
        Label_2                         matlab.ui.control.Label
        ButtonCalibrate                 matlab.ui.control.Button
        LOCALBULKCOMPOSITIONLabel       matlab.ui.control.Label
        Calibrate_GenerateDensity       matlab.ui.control.Button
        Calibrate_Merge                 matlab.ui.control.Button
        Calibrate_AddROIforLBC          matlab.ui.control.Button
        Calibrate_ROI_menu              matlab.ui.control.DropDown
        Calibrate_MultiROICheckBox      matlab.ui.control.CheckBox
        CalibratetTab_help              matlab.ui.control.Button
        Image_15                        matlab.ui.control.Image
        Image_16                        matlab.ui.control.Image
        Image_17                        matlab.ui.control.Image
        Image_18                        matlab.ui.control.Image
        Image_19                        matlab.ui.control.Image
        STANDARDIZATIONLAICPMSLabel     matlab.ui.control.Label
        ButtonCalibrate_LAICPMS         matlab.ui.control.Button
        ImportStandards_LAICPMS         matlab.ui.control.Button
        Calibrate_Spider_Button         matlab.ui.control.Button
        LBC_UncCalc                     matlab.ui.control.Button
        PxLabel                         matlab.ui.control.Label
        LBC_ValueMC                     matlab.ui.control.NumericEditField
        LBC_NbSimMC                     matlab.ui.control.NumericEditField
        SimLabel                        matlab.ui.control.Label
        Calibrate_LOD_CalcButton        matlab.ui.control.Button
        Calibrate_LOD_menu              matlab.ui.control.DropDown
        Calibrate_ApplyLODfilter        matlab.ui.control.Button
        POINTCOUNTINGLabel              matlab.ui.control.Label
        FUNCTIONSTab                    matlab.ui.container.Tab
        GridLayout_ExternalFctTab       matlab.ui.container.GridLayout
        NORMALIZATIONSTRUCTURALFORMULALabel  matlab.ui.control.Label
        SF_OxygenNorm_CheckBox          matlab.ui.control.CheckBox
        SF_CationNorm_CheckBox          matlab.ui.control.CheckBox
        SF_NbOx                         matlab.ui.control.NumericEditField
        SF_NbCat                        matlab.ui.control.NumericEditField
        SF_Function_CheckBox            matlab.ui.control.CheckBox
        SF_MineralList                  matlab.ui.control.DropDown
        SF_FunctionList                 matlab.ui.control.DropDown
        SF_ApplyButton                  matlab.ui.control.Button
        SF_HelpFile                     matlab.ui.control.Button
        THERMOBAROMETRYOTHERMETHODSLabel  matlab.ui.control.Label
        Other_MethodList                matlab.ui.control.DropDown
        Other_FunctionList              matlab.ui.control.DropDown
        Other_MultiEquilibriumButton    matlab.ui.control.Button
        Other_ApplyButton               matlab.ui.control.Button
        Other_MineralList               matlab.ui.control.DropDown
        Other_HelpFile                  matlab.ui.control.Button
        Other_ROI_menu                  matlab.ui.control.DropDown
        SF_ExportMinCompositions        matlab.ui.control.Button
        SF_ROI_menu                     matlab.ui.control.DropDown
        FunctionTab_help                matlab.ui.control.Button
        Image_20                        matlab.ui.control.Image
        Image_21                        matlab.ui.control.Image
        Image_22                        matlab.ui.control.Image
        SEGMENTCTTab                    matlab.ui.container.Tab
        GridLayout_SegmentTab           matlab.ui.container.GridLayout
        SEGMENTATIONSCHEMELabel         matlab.ui.control.Label
        SEGMENTATIONPARAMETERSTESTLabel  matlab.ui.control.Label
        APPLYLabel                      matlab.ui.control.Label
        Segment_AddScheme               matlab.ui.control.Button
        Segment_NameField               matlab.ui.control.EditField
        Segment_SegmentButton           matlab.ui.control.Button
        Segment_ErosionFilterCheckBox   matlab.ui.control.CheckBox
        Segment_ErosionFilterSpinner    matlab.ui.control.Spinner
        Segment_TrySegmentationButton   matlab.ui.control.Button
        Segment_ButtonPlot              matlab.ui.control.Button
        DATAVISUALIZATIONLabel          matlab.ui.control.Label
        SegmentTab_help                 matlab.ui.control.Button
        Segment_SavePhaseProp           matlab.ui.control.Button
        Image_10                        matlab.ui.control.Image
        Image_11                        matlab.ui.control.Image
        Image_12                        matlab.ui.control.Image
        Image_13                        matlab.ui.control.Image
        Image_14                        matlab.ui.control.Image
        Segment_InterpFilterCheckBox    matlab.ui.control.CheckBox
        Segment_InterpOrderSpinner      matlab.ui.control.Spinner
        OrderLabel                      matlab.ui.control.Label
        Segment_GradientMapButton       matlab.ui.control.Button
        Segment_SmoothingFact           matlab.ui.control.Spinner
        SmoothingFactLabel              matlab.ui.control.Label
        SchemeDropDown                  matlab.ui.control.DropDown
        ThresholdLabel                  matlab.ui.control.Label
        OrderLabel_2                    matlab.ui.control.Label
        Segment_InterpOrderFilterGBSpinner  matlab.ui.control.Spinner
        Segment_ExportROI_TXT           matlab.ui.control.Button
        ADDONSTab                       matlab.ui.container.Tab
        GridLayout_AddonsTab            matlab.ui.container.GridLayout
        AddonsTab_help                  matlab.ui.control.Button
        Addons_BingoAntidote_2          matlab.ui.control.Button
        THERMODYNAMICMODELINGLabel      matlab.ui.control.Label
        Image_32                        matlab.ui.control.Image
        Image_33                        matlab.ui.control.Image
        OTHERTOOLSLabel                 matlab.ui.control.Label
        Tool_ExportCompositions         matlab.ui.control.Button
        OPTIONSTab                      matlab.ui.container.Tab
        OptionsGridLayout               matlab.ui.container.GridLayout
        ColormapDropDownLabel           matlab.ui.control.Label
        Options_ColormapDropDown        matlab.ui.control.DropDown
        Options_LowerCheckBox           matlab.ui.control.CheckBox
        Options_UpperCheckBox           matlab.ui.control.CheckBox
        Options_LowerColor              matlab.ui.control.DropDown
        Options_UpperColor              matlab.ui.control.DropDown
        Options_LogColormap             matlab.ui.control.CheckBox
        Options_Colorbar_Inverse        matlab.ui.control.CheckBox
        Options_ColormapResEditField    matlab.ui.control.NumericEditField
        COLORBARCOLORPALETTELabel       matlab.ui.control.Label
        Options_DispNegativeValues      matlab.ui.control.CheckBox
        Options_resolutionLabel         matlab.ui.control.Label
        OTHEROPTIONSLabel               matlab.ui.control.Label
        OptionsTab_help                 matlab.ui.control.Button
        Image_24                        matlab.ui.control.Image
        Image_26                        matlab.ui.control.Image
        CLASSIFICATIONLabel             matlab.ui.control.Label
        Options_Medfilter3DsurfaceSpinnerLabel  matlab.ui.control.Label
        Options_Medfilter3DsurfaceSpinner  matlab.ui.control.Spinner
        Options_ApplyAutoContrast       matlab.ui.control.CheckBox
        Options_ColormapResEditField_2  matlab.ui.control.NumericEditField
        Options_LogColormap_2           matlab.ui.control.CheckBox
        Options_MaskSelectionForMerged  matlab.ui.control.CheckBox
        Image_34                        matlab.ui.control.Image
        UnskmeanLabel                   matlab.ui.control.Label
        Options_KmeansAlgorithm         matlab.ui.control.DropDown
        ColorMapPreview                 matlab.ui.control.UIAxes
        DEVELOPERTab                    matlab.ui.container.Tab
        GridLayout5                     matlab.ui.container.GridLayout
        Command_Keyboard                matlab.ui.control.Button
        RebuildButton                   matlab.ui.control.Button
        DEVELOPPERTOOLSLabel            matlab.ui.control.Label
        DEVELOPPERMODELabel             matlab.ui.control.Label
        AdminAccess                     matlab.ui.control.EditField
        UnlockButton                    matlab.ui.control.Button
        Image_27                        matlab.ui.control.Image
        Image_28                        matlab.ui.control.Image
        PlotEngineDropDownLabel         matlab.ui.control.Label
        PlotEngineDropDown              matlab.ui.control.DropDown
        GridLayout_Bottom               matlab.ui.container.GridLayout
        MapSlider                       matlab.ui.control.Slider
        Value_MapSlider                 matlab.ui.control.NumericEditField
        ROI_DispVertical                matlab.ui.control.CheckBox
        ROI_VerticalSlider              matlab.ui.control.Slider
        ROI_AngleOrientation            matlab.ui.control.Spinner
        GridLayout_Credits              matlab.ui.container.GridLayout
        GridLayout_MainFigure           matlab.ui.container.GridLayout
        PanelMulti                      matlab.ui.container.Panel
        PanelSingle                     matlab.ui.container.Panel
        GridLayout14                    matlab.ui.container.GridLayout
        PanelFigMain                    matlab.ui.container.Panel
        GridLayout6                     matlab.ui.container.GridLayout
        Image2_3                        matlab.ui.control.Image
        Image2_2                        matlab.ui.control.Image
        Image2                          matlab.ui.control.Image
        Button_FigMain_AutoContrast     matlab.ui.control.Button
        Button_FigMain_MedianFilter     matlab.ui.control.Button
        Button_FigMain_ResetDisplay     matlab.ui.control.Button
        Button_FigMain_ZoomIn           matlab.ui.control.Button
        Button_FigMain_ZoomOut          matlab.ui.control.Button
        Button_FigMain_Pan              matlab.ui.control.Button
        Button_FigMain_ResetZoomPan     matlab.ui.control.Button
        Button_FigMain_OpenNewWindow    matlab.ui.control.Button
        Button_FigMain_CopyImage        matlab.ui.control.Button
        Button_FigMain_PlotSurface      matlab.ui.control.Button
        Button_FigMain_CursorInfo       matlab.ui.control.Button
        RightPanel                      matlab.ui.container.Panel
        GridLayout8                     matlab.ui.container.GridLayout
        EditField_LivePeak              matlab.ui.control.EditField
        EditField_LivePosition          matlab.ui.control.NumericEditField
        EditField_LiveMin               matlab.ui.control.NumericEditField
        EditField_LiveMax               matlab.ui.control.NumericEditField
        TabGroup                        matlab.ui.container.TabGroup
        InformationTab                  matlab.ui.container.Tab
        GridLayout9                     matlab.ui.container.GridLayout
        XMapTools_logo                  matlab.ui.control.Image
        XMapTools_version               matlab.ui.control.Label
        MapInfo_TextArea                matlab.ui.control.TextArea
        SamplingTab                     matlab.ui.container.Tab
        GridLayout9_2                   matlab.ui.container.GridLayout
        GridLayout13                    matlab.ui.container.GridLayout
        Sampling_SelectCircleButton     matlab.ui.control.Button
        Sampling_SelectAreaButton       matlab.ui.control.Button
        Sampling_SelectTransectButton   matlab.ui.control.Button
        Sampling_SelectStripeButton     matlab.ui.control.Button
        Sampling_ExportButton           matlab.ui.control.Button
        Sampling_ResetButton            matlab.ui.control.Button
        Sampling_Plot1                  matlab.ui.control.UIAxes
        Sampling_Plot2                  matlab.ui.control.UIAxes
        StandardsTab                    matlab.ui.container.Tab
        GridLayout9_3                   matlab.ui.container.GridLayout
        SubTabStandard                  matlab.ui.container.TabGroup
        StdDataTab                      matlab.ui.container.Tab
        GridLayout10                    matlab.ui.container.GridLayout
        StandardLabel                   matlab.ui.control.Label
        CoordinatesSpotLabel            matlab.ui.control.Label
        X_OriginalLabel                 matlab.ui.control.Label
        Y_OriginalLabel                 matlab.ui.control.Label
        X_MapLabel                      matlab.ui.control.Label
        Y_MapLabel                      matlab.ui.control.Label
        Xori                            matlab.ui.control.NumericEditField
        Yori                            matlab.ui.control.NumericEditField
        Xm                              matlab.ui.control.NumericEditField
        Ym                              matlab.ui.control.NumericEditField
        Standard_UITable                matlab.ui.control.Table
        StdAllPlotTab                   matlab.ui.container.Tab
        GridLayout11                    matlab.ui.container.GridLayout
        GridLayout12                    matlab.ui.container.GridLayout
        Std_Shift_X                     matlab.ui.control.NumericEditField
        Std_Shift_Y                     matlab.ui.control.NumericEditField
        StdAll_Synchronize              matlab.ui.control.Button
        StdAll_profil                   matlab.ui.control.UIAxes
        StdAll_map2                     matlab.ui.control.UIAxes
        StdAll_map1                     matlab.ui.control.UIAxes
        CompositionTab                  matlab.ui.container.Tab
        GridLayout9_4                   matlab.ui.container.GridLayout
        CompViewer_DensityMenu          matlab.ui.control.DropDown
        CompViewer_Button_Copy          matlab.ui.control.Button
        CompViewer_Button_Save          matlab.ui.control.Button
        CompViewer_Label                matlab.ui.control.Label
        CompViewer_UITable              matlab.ui.control.Table
        CompViewer_Button_DeleteROI     matlab.ui.control.Button
        CompViewer_PlotTable            matlab.ui.control.UIAxes
        XMapToolsAssistantTab           matlab.ui.container.Tab
        GridLayout15                    matlab.ui.container.GridLayout
        Assistant_TextArea              matlab.ui.control.TextArea
        MessagesLabel                   matlab.ui.control.Label
        FigHistLive                     matlab.ui.control.UIAxes
        XMapToolsMenu                   matlab.ui.container.Menu
        AboutMenu                       matlab.ui.container.Menu
        LicenseMenu                     matlab.ui.container.Menu
        SetWorkingDirectoryMenu         matlab.ui.container.Menu
        StartNewSessionMenu             matlab.ui.container.Menu
        QuitMenu                        matlab.ui.container.Menu
        FileMenu                        matlab.ui.container.Menu
        InfosMenu                       matlab.ui.container.Menu
        SaveImageMenu                   matlab.ui.container.Menu
        OpenProjectMenu                 matlab.ui.container.Menu
        SaveProjectMenu                 matlab.ui.container.Menu
        SaveProjectAsMenu               matlab.ui.container.Menu
        EditMenu                        matlab.ui.container.Menu
        MapMenu                         matlab.ui.container.Menu
        DuplicateMenu                   matlab.ui.container.Menu
        DuplicateandAdjustMergedMenu    matlab.ui.container.Menu
        DeleteMenu                      matlab.ui.container.Menu
        EliminatePixelsMenu             matlab.ui.container.Menu
        SelectROIMenu                   matlab.ui.container.Menu
        RectangleMenu                   matlab.ui.container.Menu
        PolygonMenu                     matlab.ui.container.Menu
        EliminateinsideMenu             matlab.ui.container.Menu
        EliminateoutsideMenu            matlab.ui.container.Menu
        ExportMapDataMenu               matlab.ui.container.Menu
        CopyMapDataMenu                 matlab.ui.container.Menu
        SelectAreaMenu                  matlab.ui.container.Menu
        CropMenu                        matlab.ui.container.Menu
        DatasetMenu                     matlab.ui.container.Menu
        ExportMergedMenu                matlab.ui.container.Menu
        Exportashdf5ResultsMenu         matlab.ui.container.Menu
        UpdateElementOxideIndexationMenu  matlab.ui.container.Menu
        MaskMenu                        matlab.ui.container.Menu
        ExportMaskFileMenu              matlab.ui.container.Menu
        ReorderMaskMenu                 matlab.ui.container.Menu
        ApplyColorsAutoMenu             matlab.ui.container.Menu
        EditColorsManualMenu            matlab.ui.container.Menu
        DriftCorrectionMenu             matlab.ui.container.Menu
        ReorderMenu                     matlab.ui.container.Menu
        CopyImageMenu                   matlab.ui.container.Menu
        OpenImageNewWindowMenu_2        matlab.ui.container.Menu
        Plot3DsurfaceMenu               matlab.ui.container.Menu
        ResetROIMenu_2                  matlab.ui.container.Menu
        PlotMenu                        matlab.ui.container.Menu
        CorrelationMenu                 matlab.ui.container.Menu
        PlotMatrixMenu                  matlab.ui.container.Menu
        PlotMatrixValuesMenu            matlab.ui.container.Menu
        PlotScatterplotMatrixMenu       matlab.ui.container.Menu
        SaveDataMenu                    matlab.ui.container.Menu
        AutoContrastMenu                matlab.ui.container.Menu
        MedianFilterMenu                matlab.ui.container.Menu
        ResetDisplayMenu                matlab.ui.container.Menu
        ImageMenu                       matlab.ui.container.Menu
        Image_MultiSelectionModeMenu    matlab.ui.container.Menu
        Image_AddCurrentImageMenu       matlab.ui.container.Menu
        Image_AddMultiPlotImageMenu     matlab.ui.container.Menu
        Image_AddMultiLayerImagesharedscaleMenu  matlab.ui.container.Menu
        Image_AddMultiLayerImageMenu    matlab.ui.container.Menu
        SamplingMenu                    matlab.ui.container.Menu
        CircleMenu                      matlab.ui.container.Menu
        AreaPolygonMenu                 matlab.ui.container.Menu
        TransectMenu                    matlab.ui.container.Menu
        StripMenu                       matlab.ui.container.Menu
        SaveResultsMenu                 matlab.ui.container.Menu
        SingleMapMenu                   matlab.ui.container.Menu
        MultipleMapsMenu                matlab.ui.container.Menu
        ResetROIMenu                    matlab.ui.container.Menu
        HoldROIonMenu                   matlab.ui.container.Menu
        WorkspacesMenu                  matlab.ui.container.Menu
        Workspace_ProjectImportMenu     matlab.ui.container.Menu
        Workspace_ClassifyMenu          matlab.ui.container.Menu
        Workspace_CalibrateMenu         matlab.ui.container.Menu
        Workspace_FunctionsMenu         matlab.ui.container.Menu
        Workspace_SegmentCTMenu         matlab.ui.container.Menu
        Workspace_AddonsMenu            matlab.ui.container.Menu
        Workspace_OptionsMenu           matlab.ui.container.Menu
        Workspace_DeveloperMenu         matlab.ui.container.Menu
        ModulesMenu                     matlab.ui.container.Menu
        DataVisualizationMenu           matlab.ui.container.Menu
        SpiderPlotMenu                  matlab.ui.container.Menu
        GeneratorMenu                   matlab.ui.container.Menu
        HelpMenu                        matlab.ui.container.Menu
        Help_TabsMenu                   matlab.ui.container.Menu
        Help_ProjectImportMenu          matlab.ui.container.Menu
        Help_ClassifyMenu               matlab.ui.container.Menu
        Help_CalibrateMenu              matlab.ui.container.Menu
        Help_FunctionsMenu              matlab.ui.container.Menu
        Help_SegmentMenu                matlab.ui.container.Menu
        Help_AddonsMenu                 matlab.ui.container.Menu
        Help_OptionsMenu                matlab.ui.container.Menu
        Help_SamplingToolsMenu          matlab.ui.container.Menu
        Help_DataVizualisationToolMenu  matlab.ui.container.Menu
        ImagesMenu                      matlab.ui.container.Menu
    end

    % Properties that correspond to apps with auto-reflow
    properties (Access = private)
        onePanelWidth = 576;
        twoPanelWidth = 768;
    end

    properties (Access = public)
        XMapToolsData                       % All XMapTools Data
        XMapTools_VER                       % Version Updated in the startup function
        XMapTools_LastDir                   % Last directory
        XMapTools_Position
        
        ColorMaps                           % Variable containing all colormaps
        ColorMapValues
        ColorMapValues_noMask
        ActiveColorbar                      % Variable to know the active colorbar
        
        ElOxDataDef                         % Variable containing all element and oxide definitions
        DensityData                         % Variable containing the density data
        MineralColorData                    % Variable containing the mineral color data (02.2023)
        
        Id_StandardViewer                   % ID of the Standard Viewer
        ROI_std                             % This property stores the listeners or all ROI displayed
        
        FigMain                             % handle of the main plot
        
        Id_CompositionViewer
        ROI_Modes
        ROI_Modes_Listener
        ROI_LBC
        ROI_LBC_Listener
        ROI_COMP
        ROI_COMP_Listener
        ROI_EXTFCT
        ROI_EXTFCT_Listener
        ROI_LOD
        ROI_LOD_Listener
        
        ROI_SelectionTool
        
        Id_HelpTool                         % ID of the help tool
        
        config                              % For add-ons
        
        ExchangeSelector                    % To communicate with Selector
        ExchangeSelectorId                  % added 4.4
        
        ExchangeClassification              % To communicate with the classification module (01.2024 – 4.4)
        
    end
    
    properties (Access = private)
        %XMapToolsData                             % Description
        CurrentProject                      % Contain the path of the active project (if it exist)
        
        LastData2Plot                       % Contains the data of the last plot
        LastMedianFilter                    % Contains the value of the last median filter
        SliderMinHandle                     % Description
        SliderMaxHandle                     % Description
        SliderPeakHandle                    % Description
        Hist_dragging                       % Description
        Hist_dragging_limits                % Description
        Hist_dragging_which                 % Description
        
        hVerticalLines                      % new slider from Nils
        hLineToDrag
        
        WaitBar                             % This is the handle of the waitbar
        Jiahui                              % This is a special property of XMapTools
        
        ExternalFunctions                   % Data of the external function for display in the menu
        ExternalFunctions_AddParameters     % Description
        
        SaveRequired                        % If 1 the program will ask you to save the project
        
        SelectedROI                         % ROI for training sets
        TempROI                             % ROI for eliminating pixels from maps
        
        ROIobjectListener                   % This property stores a single listener for a ROI displayed
        ROIobjectListener_Delete            % Thus property contains the listener for the delete function
        SelectedROI_Idx                     % This property contain the access to the Position in TrainingSet
        
        ROIobjectListener_StdSpots          % Description
        
        ROI_sampling                        % This propery stores the listener of the sampling tools
        ROI_sampling_Listener
        
        Handle_ExtFigure
        GCA_ExtFigure
        
        % Context Menus
        ContextMenu_MainTree_C              % Menu: Clear All
        ContextMenu_MainTree_ID             % Menu: Info/Delete
        ContextMenu_MainTree_IDE            % Menu: Info/Delete/Export
        ContextMenu_MainTree_IDD            % Menu: Info/Duplicate/Delete
        ContextMenu_MainTree_IDCD           % Menu: Info/Duplicate/Convert/Delete
        ContextMenu_MainTree_IDCED          % Menu: Info/Duplicate/Convert/Export All/Delete
        ContextMenu_MainTree_IDCSD          % Menu: Info/Duplicate/Convert/Split/Delete
        ContextMenu_MainTree_I              % Menu: Info
        ContextMenu_MainTree_D              % Menu: Delete
        ContextMenu_MainTree_DG             % Menu: Delete/SaveAsGIF
        ContextMenu_AdditionalTree_1_D      % Menu: Delete              (Maskfiles)
        ContextMenu_AdditionalTree_7_DD     % Menu: Duplicate/Delete    (TrainingSets)
        ContextMenu_AdditionalTree_2_ID     % Menu: Info/Delete         (PhaseDef)
        ContextMenu_AdditionalTree_3_I      % Menu: Info
        ContextMenu_AdditionalTree_4_IDE    % Menu: Info/Delete/Export/
        ContextMenu_AdditionalTree_5_DE     % Menu: Delete/Edit         (Standards)
        ContextMenu_AdditionalTree_6_IC     % Menu: Info/Add Training Set
        
        SelectedNodes_MainTree              % For reselecting after cleaning
        
        LiveSliderValueForROI               % Description
        
        Info_MapType
        Info_Phase
        Info_MapName
        Data2Plot
        
        ROI_Position_Map1
        ROI_listener_PositionMap1
        ROI_Position_Map2
        ROI_listener_PositionMap2
        
        Password                            % Access to the developer mode
        
        Classify_ElemList_ElList
        Classify_ElemList_ElSource
        Classify_ElemList_CodePos
        Classify_ElemList_CodePosMap
        
        DataCursorMode % Description
        AxMultiplePlots % Description
        WindowsButtonDownValue % Description
        
        CovarMatrix
        
        DrawingModeLastTab
    end
    
    
    
    methods (Access = private)
        
        function SetInterfaceAvailability(app,Mode)
            
            app.mapsizeLabel.Text = '';
            
            % To be implemented in the App designer
            
            % Classify Tab
            app.Classify_AddTrainingSet.Enable = 'off';
            app.Classify_Button.Enable = 'off';
            app.Classify_Button.Tooltip = 'Add maps and select a training set to activate the classify tool';
            app.Classify_Manage_Elem.Enable = 'off';
            app.Classify_Manage_Elem_DeleteAll.Enable = 'off';
            app.Classify_AddAllElem.Enable = 'off';
            app.Classify_RunPCAButton.Enable = 'off';
            app.Masks_ButtonPlotPie.Enable = 'off';
            
            app.Calibrate_GenerateDensity.Enable = 'off';
            
            app.Classify_FilterLowProbPixels.Enable = 'off';
            app.Classify_FilterProbValue.Enable = 'off';
            app.ProbabilityLabel.Enable = 'off';
            app.Classify_FilterMaskFile.Enable = 'off';
            
            app.Classify_Modes_AddROI.Enable = 'off';
            app.Classify_PointCountingModes.Enable = 'off';
            
            % Calibrate
            app.AddStandardsManual.Enable = 'off';
            app.ButtonCalibrate.Enable = 'off';
            app.ButtonCalibrate.Tooltip = 'Import standards and select a maskfile to activate the calibration tools';
            
            app.ButtonCalibrate_LAICPMS.Enable = 'off';
            app.Calibrate_Spider_Button.Enable = 'off';
            app.SpiderPlotMenu.Enable = 'off';
            app.LBC_UncCalc.Enable = 'off';
            app.LBC_ValueMC.Enable = 'off';
            app.LBC_NbSimMC.Enable = 'off';
            app.PxLabel.Enable = 'off';
            app.SimLabel.Enable = 'off';
            
            app.Calibrate_Merge.Enable = 'off';
            app.Calibrate_AddROIforLBC.Enable = 'off';
            
            
            % Functions
            app.SF_ApplyButton.Enable = 'off';
            app.Other_ApplyButton.Enable = 'off';
            app.Other_MultiEquilibriumButton.Enable = 'off';
            app.Other_ROI_menu.Enable = 'off';
            
            app.SF_ExportMinCompositions.Enable = 'off';
            
            % Menu
            app.EliminateinsideMenu.Enable = 'off';
            app.EliminateoutsideMenu.Enable = 'off';
            app.SaveResultsMenu.Enable = 'off';
            app.Exportashdf5ResultsMenu.Enable = 'off';
            app.MaskMenu.Enable = 'off';
            
            app.DriftCorrectionMenu.Enable = 'off';
            
            % Other
            app.MapSlider.Visible = 'off';
            app.Value_MapSlider.Visible = 'off';
            
            app.ROI_DispVertical.Visible = 'off';
            app.ROI_VerticalSlider.Visible = 'off';
            app.ROI_AngleOrientation.Visible = 'off';
            
            app.ButtonRotateView.Enable = 'off';
            
            app.Button_FigMain_AutoContrast.Enable = 'off';
            app.Button_FigMain_MedianFilter.Enable = 'off';
            app.Button_FigMain_ResetDisplay.Enable = 'off';
            
            app.Button_FigMain_CursorInfo.Enable = 'off';
            
            app.Button_FigMain_ZoomIn.Enable = 'off';
            app.Button_FigMain_ZoomOut.Enable = 'off';
            app.Button_FigMain_Pan.Enable = 'off';
            app.Button_FigMain_ResetZoomPan.Enable = 'off';
            
            app.Button_FigMain_OpenNewWindow.Enable = 'off';
            app.Button_FigMain_CopyImage.Enable = 'off';
            app.Button_FigMain_PlotSurface.Enable = 'off';
            
            app.Sampling_Plot1.Visible = 'off';
            app.Sampling_Plot2.Visible = 'off';
            
            app.Sampling_ExportButton.Enable = 'off';
            app.Sampling_ResetButton.Enable = 'off';
            
            app.EditField_LivePosition.Visible = 'on';
            app.EditField_LivePeak.Visible = 'off';
            
            
            
        end
        
        function InitializeXMapToolsData(app)
            
            % -------------------------------------------------------------
            % (1) Initialize MapData
            % -------------------------------------------------------------
            
            MapData.It.Names = {};
            MapData.It.Types = [];
            MapData.It.ElInd = [];
            MapData.It.Data(1).Map = [];
            %MapData.It.Data(1).SpectraSettings = [];
            
            MapData.Qt.Names = {};
            MapData.Qt.IsOxide = [];
            MapData.Qt.MaskFile = {};
            MapData.Qt.NbCalibPoints = [];
            
            MapData.Qt.Data(1).ElNames = {};
            MapData.Qt.Data(1).ElInd = [];
            MapData.Qt.Data(1).StdData = [];     % simply reset as variables can be variable in there
            MapData.Qt.Data(1).CData(1).Map = [];
            MapData.Qt.Data(1).OxInd = [];
            
            MapData.Me.Names = {};
            MapData.Me.IsOxide = [];
            MapData.Me.MaskFile = {};
            MapData.Me.NbCalibPoints = [];
            
            MapData.Me.Data(1).ElNames = {};
            MapData.Me.Data(1).ElInd = [];
            MapData.Me.Data(1).CData(1).Map = [];
            MapData.Me.Data(1).OxInd = [];
            
            MapData.Re.Names = {};
            MapData.Re.Types = [];
            MapData.Re.Coord = [];
            MapData.Re.Data(1).Labels =  {};
            MapData.Re.Data(1).CData(1).Map =  [];
            
            MapData.Ot.Names = {};
            MapData.Ot.Types = [];
            MapData.Ot.Data(1).Map = [];
            
            MapData.Ct.Names = {};
            MapData.Ct.Types = [];
            MapData.Ct.Data(1).Map = [];
            
            MapData.ROI.Names = {};
            MapData.ROI.Types = [];
            MapData.ROI.Data(1).Names = {};
            MapData.ROI.Data(1).ROI = [];
            
            MapData.Im.Names = {};
            MapData.Im.Types = [];
            MapData.Im.Data(1).Labels = {};
            MapData.Im.Data(1).CData(1).Map = [];
            MapData.Im.Data(1).CData(1).Lim = [];
            MapData.Im.Data(1).CData(1).ColorMap = [];
            
            
            % -------------------------------------------------------------
            
            MapData.MaskFile.Names = {};
            MapData.MaskFile.Types = [];
            MapData.MaskFile.NbMasks = [];
            MapData.MaskFile.Masks(1).Names = {};
            MapData.MaskFile.Masks(1).Densities = [];
            MapData.MaskFile.Masks(1).MaskMap = [];
            MapData.MaskFile.Masks(1).MaskProbability = [];
            MapData.MaskFile.Masks(1).Signature = [];
            MapData.MaskFile.Masks(1).Colors = [];
            %
            MapData.MaskFile.Masks(1).Info.Algorithm = '';
            MapData.MaskFile.Masks(1).Info.SelectedData = {};
            MapData.MaskFile.Masks(1).Info.Scaling = '';
            MapData.MaskFile.Masks(1).Info.Reproducibility = '';
            MapData.MaskFile.Masks(1).Info.Classes = {};
            MapData.MaskFile.Masks(1).Info.SelectedPx = [];
            MapData.MaskFile.Masks(1).Info.TrainPx = [];
            MapData.MaskFile.Masks(1).Info.TestPx = [];
            MapData.MaskFile.Masks(1).Info.Accuracy = [];
            MapData.MaskFile.Masks(1).Info.Precision = [];
            MapData.MaskFile.Masks(1).Info.Recall = [];
            MapData.MaskFile.Masks(1).Info.F1Score = [];
            MapData.MaskFile.Masks(1).Info.Modes = [];
            %
            MapData.MaskFile.Masks(1).SubMask(1).Names = {};
            MapData.MaskFile.Masks(1).SubMask(1).Densities = [];
            MapData.MaskFile.Masks(1).SubMask(1).MaskSelMaskMap = [];
            MapData.MaskFile.Masks(1).SubMask(1).MaskProbability = [];
            %
            MapData.MaskFile.Masks(1).SubMask(1).Info.Algorithm = '';
            MapData.MaskFile.Masks(1).SubMask(1).Info.SelectedData = {};
            MapData.MaskFile.Masks(1).SubMask(1).Info.Scaling = '';
            MapData.MaskFile.Masks(1).SubMask(1).Info.Reproducibility = '';
            MapData.MaskFile.Masks(1).SubMask(1).Info.Classes = {};
            MapData.MaskFile.Masks(1).SubMask(1).Info.SelectedPx = [];
            MapData.MaskFile.Masks(1).SubMask(1).Info.TrainPx = [];
            MapData.MaskFile.Masks(1).SubMask(1).Info.TestPx = [];
            MapData.MaskFile.Masks(1).SubMask(1).Info.Accuracy = [];
            MapData.MaskFile.Masks(1).SubMask(1).Info.Precision = [];
            MapData.MaskFile.Masks(1).SubMask(1).Info.Recall = [];
            MapData.MaskFile.Masks(1).SubMask(1).Info.F1Score = [];
            MapData.MaskFile.Masks(1).SubMask(1).Info.Modes = [];
            
            % -------------------------------------------------------------
            % Initialize TrainingSet
            % -------------------------------------------------------------
            
            TrainingSet.Names = {};
            TrainingSet.Types = [];
            TrainingSet.MaskSignature = [];
            TrainingSet.MaskNode = [];
            TrainingSet.Nb = [];
            TrainingSet.Data(1).Names = {};
            %TrainingSet.Data(1).Types = {};        % changed March 2021
            TrainingSet.Data(1).ROI(1).Types = {};
            TrainingSet.Data(1).ROI(1).Data(1).Coordinates = [];
            
            % -------------------------------------------------------------
            % Initialize SegScheme
            % -------------------------------------------------------------
            
            SegScheme.Names = {};
            SegScheme.Types = [];
            SegScheme.Nb = [];
            SegScheme.Data(1).Names = {};
            SegScheme.Data(1).Range = [];
            
            % -------------------------------------------------------------
            % Initialize MapSizeCheck
            % -------------------------------------------------------------
            
            MapSizeCheck.OriginalSize = [];
            MapSizeCheck.ActualSize = [];
            MapSizeCheck.SizeChanged = 0;
            
            % -------------------------------------------------------------
            % Initialize Standards, LOI and PxDataRaw (LA-ICPMS)
            % -------------------------------------------------------------
            
            [Standards] = InitializeXMapToolsData_Standards(app);
            [MapStandards] = InitializeXMapToolsData_MapStandards(app);
            [MapLOD] = InitializeXMapToolsData_MapLOD(app);
            
            [PxDataRaw] = InitializeXMapToolsData_PxDataRaw(app);
            
            % -------------------------------------------------------------
            % Create app.XMapToolsData
            % -------------------------------------------------------------
            
            app.XMapToolsData.TrainingSet = TrainingSet;
            app.XMapToolsData.MapData = MapData;
            app.XMapToolsData.MapSizeCheck = MapSizeCheck;
            app.XMapToolsData.Standards = Standards;
            app.XMapToolsData.MapStandards = MapStandards;
            app.XMapToolsData.MapLOD = MapLOD;
            app.XMapToolsData.PxDataRaw = PxDataRaw;
            app.XMapToolsData.SegScheme = SegScheme;
            
            app.SaveRequired = 0;
            
        end
        
        function [PxDataRaw] = InitializeXMapToolsData_PxDataRaw(app)
            PxDataRaw.ElNames = {};
            PxDataRaw.PixelIndices = [];
            PxDataRaw.ElData(1).PxData(1).NbSweep = [];
            PxDataRaw.ElData(1).PxData(1).SweepIndices = [];
            PxDataRaw.ElData(1).PxData(1).Intensity = [];
        end
        
        function [MapStandards] = InitializeXMapToolsData_MapStandards(app)
            MapStandards(1).StandardName = {};
            MapStandards(1).Names = {};
            MapStandards(1).Types = [];
            MapStandards(1).Data(1).Map = [];
            MapStandards(1).Data(1).Map_Int_Back = [];
            MapStandards(1).Data(1).Map_Sweeps_Back = [];
            MapStandards(1).Data(1).Map_Sweeps_Pixel = [];
        end
        
        function [MapLOD] = InitializeXMapToolsData_MapLOD(app)
            % new 4.2
            MapLOD(1).Names = {};
            MapLOD(1).Types = [];
            MapLOD(1).Data(1).ElNames = [];
            MapLOD(1).Data(1).ElData(1).Label_LOD = '';
            MapLOD(1).Data(1).ElData(1).Map_LOD = [];
            MapLOD(1).Data(1).ElData(1).Label_Si_mtx = '';
            MapLOD(1).Data(1).ElData(1).Map_Si_mtx = [];
            MapLOD(1).Data(1).ElData(1).Label_Back_mtx = '';
            MapLOD(1).Data(1).ElData(1).Map_Back_mtx = [];
            
            MapLOD(1).Data(1).ElData(1).DTi = [];
            MapLOD(1).Data(1).ElData(1).NbAnal = [];
            MapLOD(1).Data(1).ElData(1).NbBack = [];
        end
        
        function [Standards] = InitializeXMapToolsData_Standards(app)
            Standards.Coord = [];
            Standards.Types = [];
            Standards.Labels = {};
            Standards.Selected = [];
            Standards.XCoo = [];
            Standards.YCoo = [];
            Standards.XY = [];
            Standards.ElemImport = {};
            Standards.RefEl = [];
            Standards.RefOx = [];
            Standards.DataPro = [];
            Standards.ElMap = [];
            Standards.DataIt = [];
            Standards.DataItAv = [];
        end
        
        function ReadDefFiles(app)
            
            fid = fopen('XMap_Def_Element.txt','r');
            Compt = 0;
            
            app.ElOxDataDef.ElList = [];
            app.ElOxDataDef.ElIdx = [];
            app.ElOxDataDef.ElMass = [];
            
            while 1
                tline = fgetl(fid);
                
                if isequal(tline,-1)
                    break
                end
                
                if length(tline) >= 1
                    if isequal(tline(1),'>')
                        
                        while 1
                            tline = fgetl(fid);
                            
                            if isequal(tline,-1) || isequal(tline,'')
                                break
                            end
                            
                            Compt = Compt + 1;
                            
                            TheStr = strread(tline,'%s');
                            
                            app.ElOxDataDef.ElList{Compt} = TheStr{1};
                            app.ElOxDataDef.ElIdx(Compt) = Compt;
                            app.ElOxDataDef.ElMass(Compt) = str2num(TheStr{2});
                        end
                    end
                end
            end
            
            fclose(fid);
            
            fid = fopen('XMap_Def_Oxide.txt','r');
            Compt = 0;
            
            app.ElOxDataDef.OxList = [];
            app.ElOxDataDef.OxElIdx = [];
            app.ElOxDataDef.OxNbCat = [];
            app.ElOxDataDef.OxNbOx = [];
            app.ElOxDataDef.OxMass = [];
            app.ElOxDataDef.OxFact = [];
            
            while 1
                tline = fgetl(fid);
                
                if isequal(tline,-1)
                    break
                end
                
                if length(tline) >= 1
                    if isequal(tline(1),'>')
                        
                        while 1
                            tline = fgetl(fid);
                            
                            if isequal(tline,-1) || isequal(tline,'')
                                break
                            end
                            
                            Compt = Compt + 1;
                            
                            TheStr = strread(tline,'%s');
                            
                            app.ElOxDataDef.OxList{Compt} = TheStr{1};
                            
                            Element = TheStr{2};
                            Idx = find(ismember(app.ElOxDataDef.ElList,Element));
                            
                            app.ElOxDataDef.OxElIdx(Compt) = Idx;
                            app.ElOxDataDef.OxNbCat(Compt) = str2num(TheStr{3});
                            app.ElOxDataDef.OxNbOx(Compt) = str2num(TheStr{4});
                            app.ElOxDataDef.OxMass(Compt) = str2num(TheStr{5});
                            app.ElOxDataDef.OxFact(Compt) = str2num(TheStr{6});
                            
                        end
                    end
                end
            end
            fclose(fid);
        end
        
        
        function ReadColorMaps(app)
            fid = fopen('XMap_ColorMaps.txt','r');
            Compt = 0;
            
            app.ColorMaps(1).Name = 'None';
            app.ColorMaps(1).Code = 0;
            
            Compt = 0;
            ErrorLoad = 0;
            while 1
                tline = fgetl(fid);
                
                if isequal(tline,-1)
                    break
                end
                
                if length(tline) >= 1
                    if isequal(tline(1),'>')
                        Compt = Compt+1;
                        app.ColorMaps(Compt).Name = tline(3:end);
                        Row = 0;
                        while 1
                            tline = fgetl(fid);
                            if isequal(tline,-1) || isequal(tline,'')
                                break
                            end
                            NUM = strread(tline,'%f');
                            Row = Row+1;
                            app.ColorMaps(Compt).Code(Row,1:3) = NUM(1:3);
                        end
                        
                    end
                end
            end
            fclose(fid);
            
            % Update the menu
            for i = 1:numel(app.ColorMaps)
                Items{i} = app.ColorMaps(i).Name;
            end
            app.Options_ColormapDropDown.Items =  Items; %extractfield(ColorMaps,'Name');
            
        end
        
        
        function ReadDensityDataFile(app)
            fid = fopen('XMap_MinDensity.txt','r');
            
            ComptSS = 0;
            ComptOP = 0;
            ComptMA = 0;
            
            tline = fgetl(fid);
            while 1
                
                if length(tline > 1)
                    if isequal(tline(1),'#')
                        switch tline(2)
                            case '1' % Solid solutions
                                
                                while 1
                                    tline = fgetl(fid);
                                    
                                    if length(tline > 1)
                                        
                                        if isequal(tline(1),'<')
                                            break
                                        end
                                        
                                        if isequal(tline(1),'>')
                                            TheStr = textscan(tline,'%s');
                                            TheStr = TheStr{1};
                                            
                                            ComptSS = ComptSS+1;
                                            SS(ComptSS).name = TheStr{2};
                                            SS(ComptSS).NbEM = str2num(TheStr{3});
                                            SS(ComptSS).Density = 0;
                                            
                                            for i = 1:SS(ComptSS).NbEM
                                                tline = fgetl(fid);
                                                TheStr = textscan(tline,'%s');
                                                TheStr = TheStr{1};
                                                SS(ComptSS).EMnames{i} = TheStr{1};
                                                SS(ComptSS).EMdensity(i) = str2num(TheStr{2});
                                                SS(ComptSS).EMfraction(i) = str2num(TheStr{3});
                                            end
                                            
                                            SS(ComptSS).Density = sum(SS(ComptSS).EMdensity.*SS(ComptSS).EMfraction);
                                            
                                        end
                                        
                                    end
                                    
                                    
                                end
                                
                                
                                
                            case '2' % Other phases
                                while 1
                                    tline = fgetl(fid);
                                    
                                    if length(tline) > 1
                                        TheStr = textscan(tline,'%s');
                                        TheStr = TheStr{1};
                                        ComptOP = ComptOP+1;
                                        OPnames{ComptOP} = TheStr{1};
                                        OPdensity(ComptOP) = str2num(TheStr{2});
                                        
                                    else
                                        break
                                    end
                                    
                                    
                                end
                                
                            case '3' % Metals & Alloys
                                while 1
                                    tline = fgetl(fid);
                                    
                                    if length(tline) > 1
                                        TheStr = textscan(tline,'%s');
                                        TheStr = TheStr{1};
                                        ComptMA = ComptMA+1;
                                        MAnames{ComptMA} = TheStr{1};
                                        MAdensity(ComptMA) = str2num(TheStr{2});
                                        
                                    else
                                        break
                                    end
                                    
                                    
                                end
                                
                        end
                        
                    end
                end
                
                tline = fgetl(fid);
                
                if isequal(tline,-1)
                    break
                end
                
                
            end
            fclose(fid);
            
            
            app.DensityData.Names = {};
            app.DensityData.Density = [];
            app.DensityData.Type = {};
            
            Compt = 0;
            for i = 1:length(SS)
                Compt = Compt+1;
                app.DensityData.Names{Compt} = SS(i).name;
                app.DensityData.Density(Compt) = SS(i).Density;
                app.DensityData.Type{Compt} = 'Solid solution';
            end
            for i = 1:length(SS)
                for j = 1:length(SS(i).EMnames)
                    Compt = Compt+1;
                    app.DensityData.Names{Compt} = SS(i).EMnames{j};
                    app.DensityData.Density(Compt) = SS(i).EMdensity(j);
                    app.DensityData.Type{Compt} = 'End member';
                end
            end
            for i = 1:length(OPnames)
                Compt = Compt+1;
                app.DensityData.Names{Compt} = OPnames{i};
                app.DensityData.Density(Compt) = OPdensity(i);
                app.DensityData.Type{Compt} = 'Pure phases';
            end
            for i = 1:length(MAnames)
                Compt = Compt+1;
                app.DensityData.Names{Compt} = MAnames{i};
                app.DensityData.Density(Compt) = MAdensity(i);
                app.DensityData.Type{Compt} = 'Metals & Alloys';
            end
            
        end
        
        function ReadColorDataFile(app)
            fid = fopen('XMap_MinColors.txt','r');
            
            Compt = 0;
            
            tline = fgetl(fid);
            
            MineralNames = {};
            ColorData = [];
            
            while 1
                if length(tline > 1)
                    if isequal(tline(1),'>')
                        while 1
                            tline = fgetl(fid);
                            
                            if isequal(tline,-1)
                                break
                            end
                            
                            if length(tline > 5)
                                
                                TheStr = textscan(tline,'%s');
                                TheStr = TheStr{1};
                                
                                Compt = Compt+1;
                                
                                MineralNames{Compt} = TheStr{1};
                                ColorData(Compt,:) = [str2num(TheStr{2}),str2num(TheStr{3}),str2num(TheStr{4})];
                                
                            end
                        end
                    end
                end
                
                tline = fgetl(fid);
                
                if isequal(tline,-1)
                    break
                end
                
                
            end
            fclose(fid);
            
            ColorDataNORM = ColorData./(255.*ones(size(ColorData)));
            
            app.MineralColorData.Names = MineralNames;
            app.MineralColorData.RGB = ColorDataNORM;
        end
        
        function UpdateGUI_Function_Other(app)
            
            switch app.Other_MethodList.Value
                
                case 'Map-mode'
                    app.Other_MineralList.Items = app.ExternalFunctions.ItemsTB;
                    app.Other_FunctionList.Items = app.ExternalFunctions.Min(app.ExternalFunctions.IdxTB(1)).TB.Name;
                    
                case 'Multi-equilibrium'
                    app.Other_MineralList.Items = app.ExternalFunctions.ItemsME;
                    app.Other_FunctionList.Items = app.ExternalFunctions.Min(app.ExternalFunctions.IdxME(1)).ME.Names;
            end
            
            
            
            
        end
        
        
        function LoadProjectFile(app,ProjectPath,ProjectName)
            
            %app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar = uiprogressdlg(app.XMapTools_GUI,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Preparing the interface';
            
            InitializeXMapToolsData(app);
            
            %             MapStandards = InitializeXMapToolsData_MapStandards(app);
            %             Standards = InitializeXMapToolsData_Standards(app);
            %             MapLOD = InitializeXMapToolsData_MapLOD(app);
            
            ResetGUI(app);
            
            app.WaitBar.Message = ['Loading project ',ProjectName];
            
            %load([ProjectPath,'/',ProjectName]);
            load(fullfile(ProjectPath,ProjectName));
            
            %
            if exist('Data','var')
                % Project version 3 to be converted
                app.WaitBar.Message = 'Adjusting the data format';
                
                
                for i = 1:length(Data.map)
                    app.XMapToolsData.MapData.It.Names{i} = char(Data.map(i).name);
                    app.XMapToolsData.MapData.It.Types(i) = Data.map(i).type;
                    app.XMapToolsData.MapData.It.ElInd(i) = Data.map(i).ref;
                    app.XMapToolsData.MapData.It.Data(i).Map = Data.map(i).values;
                end
                
                
                if length(Quanti) > 1    % first is none
                    IsMergedMap = zeros(length(Quanti),1);
                    i = 0;
                    for ii = 1:length(Quanti)-1
                        
                        if isequal(char(Quanti(ii+1).maskfile),'none BULK-Quanti')
                            IsMergedMap(ii) = 1;
                        elseif isequal(char(Quanti(ii+1).maskfile),'none BULK-Quanti (DC)')
                            IsMergedMap(ii) = 2;
                        else
                            
                            i = i+1;
                            
                            app.XMapToolsData.MapData.Qt.Names{i} = char(Quanti(i+1).mineral);
                            if isfield(Quanti,'isoxide')
                                app.XMapToolsData.MapData.Qt.IsOxide(i) = Quanti(i+1).isoxide;
                            else
                                app.XMapToolsData.MapData.Qt.IsOxide(i) = 1;
                            end
                            app.XMapToolsData.MapData.Qt.MaskFile{i} = char(Quanti(i+1).maskfile);
                            app.XMapToolsData.MapData.Qt.NbCalibPoints(i) = Quanti(i+1).nbpoints;
                            
                            for j = 1:length(Quanti(i+1).elem)
                                app.XMapToolsData.MapData.Qt.Data(i).ElNames{j} = char(Quanti(i+1).elem(j).name);
                                app.XMapToolsData.MapData.Qt.Data(i).ElInd(j) = Quanti(i+1).elem(j).ref;
                                
                                if isfield(Quanti(i+1).elem(j),'Ra')
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.It = Quanti(i+1).elem(j).Ra';
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.It_unc = Quanti(i+1).elem(j).RaUnc';
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.Qt = Quanti(i+1).elem(j).Ox';
                                    [Is,Index] = ismember(Quanti(i+1).elem(j).Ox,Quanti(i+1).elem(j).values);
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.XY = Quanti(i+1).elem(j).coor(Index(find(Index)),:);
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.Selected =  Quanti(i+1).elem(j).Selected;
                                    
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.PlotX =  Quanti(i+1).elem(j).plotX;
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.PlotY =  Quanti(i+1).elem(j).plotY;
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.PlotXi =  Quanti(i+1).elem(j).plotXi;
                                else
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.It = [];
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.It_unc = [];
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.Qt = [];
                                    
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.XY = [];
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.Selected =  [];
                                    
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.PlotX =  [];
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.PlotY =  [];
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.PlotXi =  [];
                                end
                                
                                app.XMapToolsData.MapData.Qt.Data(i).StdData.Calibration =  Quanti(i+1).elem(j).param;
                                
                                if isfield(Quanti(i+1).elem(j),'paramType')
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.Type =  Quanti(i+1).elem(j).paramType;
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.R2 =  Quanti(i+1).elem(j).R2;
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.Polyfit =  Quanti(i+1).elem(j).polyfit;
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.Warning =  Quanti(i+1).elem(j).warning;
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.UsePolyfit =  Quanti(i+1).elem(j).usepolyfit;
                                else
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.Type =  [];
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.R2 =  [];
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.Polyfit =  [];
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.Warning =  [];
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.UsePolyfit =  [];
                                end
                                if isfield(Quanti(i+1).elem(j),'BackgroundResiduals')
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.BackgroundResiduals =  Quanti(i+1).elem(j).BackgroundResiduals;
                                else
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.BackgroundResiduals =  [];
                                end
                                if isfield(Quanti(i+1).elem(j),'BackgroundResidualsFirstSol')
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.BackgroundResidualsFirstSol =  Quanti(i+1).elem(j).BackgroundResidualsFirstSol;
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.BackgroundResidualsBestSol =  Quanti(i+1).elem(j).BackgroundResidualsBestSol;
                                else
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.BackgroundResidualsFirstSol =  [];
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.BackgroundResidualsBestSol =  [];
                                end
                                if isfield(Quanti(i+1).elem(j),'BackgroundValue')
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.BackgroundValue =  Quanti(i+1).elem(j).BackgroundValue;
                                else
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.BackgroundValue =  0;
                                end
                                if isfield(Quanti(i+1).elem(j),'BackgroundMinResidualValue')
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.BackgroundMinResidualValue =  Quanti(i+1).elem(j).BackgroundMinResidualValue;
                                else
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.BackgroundMinResidualValue =  [];
                                end
                                if isfield(Quanti(i+1).elem(j),'Standardization')
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.Standardization =  Quanti(i+1).elem(j).Standardization;
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.StandardizationType =  Quanti(i+1).elem(j).StandardizationType;
                                else
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.Standardization =  [];
                                    app.XMapToolsData.MapData.Qt.Data(i).StdData.StandardizationType =  [];
                                end
                                
                                app.XMapToolsData.MapData.Qt.Data(i).CData(j).Map = Quanti(i+1).elem(j).quanti;
                            end
                        end
                    end
                    
                    i = 0;
                    for ii = 1:length(IsMergedMap)-1
                        if IsMergedMap(ii) > 0
                            i = i+1;
                            app.XMapToolsData.MapData.Me.Names{i} = char(Quanti(ii+1).mineral);
                            if isfield(Quanti,'isoxide')
                                app.XMapToolsData.MapData.Me.IsOxide(i) = Quanti(ii+1).isoxide;
                            else
                                app.XMapToolsData.MapData.Me.IsOxide(i) = 1;
                            end
                            app.XMapToolsData.MapData.Me.MaskFile{i} = char(Quanti(ii+1).maskfile);
                            app.XMapToolsData.MapData.Me.NbCalibPoints(i) = Quanti(ii+1).nbpoints;
                            
                            for j = 1:length(Quanti(ii+1).elem)
                                app.XMapToolsData.MapData.Me.Data(i).ElNames{j} = char(Quanti(ii+1).elem(j).name);
                                app.XMapToolsData.MapData.Me.Data(i).ElInd(j) = Quanti(ii+1).elem(j).ref;
                                app.XMapToolsData.MapData.Me.Data(i).CData(j).Map = Quanti(ii+1).elem(j).quanti;
                            end
                        end
                    end
                end
                
                if length(Results) > 1
                    for i = 1:length(Results)
                        app.XMapToolsData.MapData.Re.Names{i} = [char(Results(i).method),'_',char(Results(i).mineral)];
                        app.XMapToolsData.MapData.Re.Types(i) = Results(i).type;
                        app.XMapToolsData.MapData.Re.Coord(i) = Results(i).coord;
                        app.XMapToolsData.MapData.Re.Data(i).Labels = Results(i).labels;
                        for j = 1:length(Results(i).labels)
                            app.XMapToolsData.MapData.Re.Data(i).CData(j).Map = reshape(Results(i).values(:,j),Results(i).reshape);
                        end
                    end
                end
                
                if isfield(MaskFile,'Name')
                    for i = 1:length(MaskFile)
                        app.XMapToolsData.MapData.MaskFile.Names{i} =  char(MaskFile(i).Name);
                        app.XMapToolsData.MapData.MaskFile.Types(i) =  MaskFile(i).type;
                        app.XMapToolsData.MapData.MaskFile.NbMasks(i) =  MaskFile(i).Nb;
                        for j = 1:app.XMapToolsData.MapData.MaskFile.NbMasks(i)
                            app.XMapToolsData.MapData.MaskFile.Masks(i).Names{j} = char(MaskFile(i).NameMinerals{j});
                            PxInd = find(MaskFile(i).Mask == j);
                            if numel(PxInd)
                                if isfield('MaskFile','DensityMap')
                                    Density = MaskFile(i).DensityMap(PxInd(1));
                                else
                                    Density = 0;
                                end
                            else
                                Density = 0;
                            end
                            app.XMapToolsData.MapData.MaskFile.Masks(i).Density(j) = Density;
                            app.XMapToolsData.MapData.MaskFile.Masks(i).MaskProbability = [];
                            
                            app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Names = {};
                            app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Densities = [];
                            app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).MaskMap = [];
                            app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).MaskProbability = [];
                            app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Info.Algorithm = '';
                            app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Info.SelectedData = {};
                            app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Info.Scaling = '';
                            app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Info.Reproducibility = '';
                            app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Info.Classes = {};
                            app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Info.SelectedPx = [];
                            app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Info.TrainPx = [];
                            app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Info.TestPx = [];
                            app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Info.Accuracy = [];
                            app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Info.Precision = [];
                            app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Info.Recall = [];
                            app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Info.F1Score = [];
                            app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Info.Modes = [];
                            
                        end
                        app.XMapToolsData.MapData.MaskFile.Masks(i).MaskMap = MaskFile(i).Mask;
                        
                        Colors4Mask = UpdateColorsMasks(app,app.XMapToolsData.MapData.MaskFile.Masks(i).Names);
                        app.XMapToolsData.MapData.MaskFile.Masks(i).Colors = Colors4Mask;
                        
                        app.XMapToolsData.MapData.MaskFile.Masks(i).Info.Algorithm = '';
                        app.XMapToolsData.MapData.MaskFile.Masks(i).Info.SelectedData = {};
                        app.XMapToolsData.MapData.MaskFile.Masks(i).Info.Scaling = '';
                        app.XMapToolsData.MapData.MaskFile.Masks(i).Info.Reproducibility = '';
                        app.XMapToolsData.MapData.MaskFile.Masks(i).Info.Classes = {};
                        app.XMapToolsData.MapData.MaskFile.Masks(i).Info.SelectedPx = [];
                        app.XMapToolsData.MapData.MaskFile.Masks(i).Info.TrainPx = [];
                        app.XMapToolsData.MapData.MaskFile.Masks(i).Info.TestPx = [];
                        app.XMapToolsData.MapData.MaskFile.Masks(i).Info.Accuracy = [];
                        app.XMapToolsData.MapData.MaskFile.Masks(i).Info.Precision = [];
                        app.XMapToolsData.MapData.MaskFile.Masks(i).Info.Recall = [];
                        app.XMapToolsData.MapData.MaskFile.Masks(i).Info.F1Score = [];
                        app.XMapToolsData.MapData.MaskFile.Masks(i).Info.Modes = [];
                    end
                end
                
                % Check the indexes and update the values using the new
                % database
                for i = 1:length(app.XMapToolsData.MapData.It.Names)
                    where = find(ismember(app.ElOxDataDef.ElList,app.XMapToolsData.MapData.It.Names{i}));
                    if ~isempty(where)
                        app.XMapToolsData.MapData.It.ElInd(i) = where;
                    else
                        app.XMapToolsData.MapData.It.ElInd(i) = 0;
                    end
                end
                
                app.CurrentProject = [ProjectPath,'/',ProjectName(1:end-4),'_converted.mat'];
                ProjectName_OptionsValueChanged(app);
                
                app.WaitBar.Message = 'Finishing';
                
                ResetGUI(app);
                UpdateGUI(app);
                
                % Display the first map if available (in the future we could
                % restore the previous state)
                if length(app.XMapToolsData.MapData.It.Names) > 1
                    app.Node_It.expand();
                    app.TreeData_Main.SelectedNodes = app.Node_It.Children(1);
                    TreeData_MainSelectionChanged(app);
                end
                
                close(app.WaitBar);
                return
                
            end
            
            app.WaitBar.Message = 'Checking data compatibility';
            
            app.XMapToolsData.MapData = MapData;
            
            % -------------------------------------------------------------
            % COMPATIBILITY CHECKS
            % -------------------------------------------------------------
            
            % Check (BETA) for compatibility
            if ~isfield(MapData,'Ot')
                app.XMapToolsData.MapData.Ot.Names = {};
                app.XMapToolsData.MapData.Ot.Types = [];
                app.XMapToolsData.MapData.Ot.Data(1).Map = [];
            end
            
            % Check (BETA) for compatibility
            if ~isfield(MapData,'Ct')
                app.XMapToolsData.MapData.Ct.Names = {};
                app.XMapToolsData.MapData.Ct.Types = [];
                app.XMapToolsData.MapData.Ct.Data(1).Map = [];
            end
            
            % Check (BETA) for compatibility
            if ~isfield(MapData,'Im')
                app.XMapToolsData.MapData.Im.Names = {};
                app.XMapToolsData.MapData.Im.Types = [];
                app.XMapToolsData.MapData.Im.Data(1).Labels = {};
                app.XMapToolsData.MapData.Im.Data(1).CData(1).Map = [];
                app.XMapToolsData.MapData.Im.Data(1).CData(1).Lim = [];
                app.XMapToolsData.MapData.Im.Data(1).CData(1).ColorMap = [];
            end
            
            % Update Merged & Quanti for OxInd  % added 4.4 (can create problem?)
            if ~isempty(app.XMapToolsData.MapData.Me.Names)
                if ~isfield(app.XMapToolsData.MapData.Me.Data,'OxInd')
                    for i = 1:length(app.XMapToolsData.MapData.Me.Data)
                        app.XMapToolsData.MapData.Me.Data(i).OxInd = zeros(size(app.XMapToolsData.MapData.Me.Data(i).ElInd));
                    end
                end
            end
            if ~isempty(app.XMapToolsData.MapData.Qt.Names)
                if ~isfield(app.XMapToolsData.MapData.Qt.Data,'OxInd')
                    for i = 1:length(app.XMapToolsData.MapData.Qt.Data)
                        app.XMapToolsData.MapData.MQte.Data(i).OxInd = zeros(size(app.XMapToolsData.MapData.Qt.Data(i).ElInd));
                    end
                end
            else
                if ~isfield(app.XMapToolsData.MapData.Qt.Data,'OxInd')
                    app.XMapToolsData.MapData.Qt.Data.OxInd = [];
                end
            end
            
            %
            if ~isfield(app.XMapToolsData.MapData.MaskFile.Masks,'MaskProbability')
                for i = 1:length(app.XMapToolsData.MapData.MaskFile.Masks)
                    app.XMapToolsData.MapData.MaskFile.Masks(i).MaskProbability = 0;
                end
            end
            
            if ~isfield(app.XMapToolsData.MapData.MaskFile.Masks,'Signature')
                for i = 1:length(app.XMapToolsData.MapData.MaskFile.Masks)
                    app.XMapToolsData.MapData.MaskFile.Masks(i).Signature = now;
                end
            end
            
            if ~isfield(TrainingSet,'MaskSignature')
                TrainingSet.MaskSignature = zeros(size(TrainingSet.Names));
                TrainingSet.MaskNode = zeros(size(TrainingSet.Names));
            end
            
            app.XMapToolsData.TrainingSet = TrainingSet;
            
            if isfield(app.XMapToolsData.TrainingSet.Data,'Types')
                % Adjust format (multiple-ROI)
                OldTrainingSet = app.XMapToolsData.TrainingSet;
                
                app.XMapToolsData.TrainingSet = [];
                for i = 1:length(OldTrainingSet.Names)
                    app.XMapToolsData.TrainingSet.Names{i} = OldTrainingSet.Names{i};
                    app.XMapToolsData.TrainingSet.Types(i) = OldTrainingSet.Types(i);
                    app.XMapToolsData.TrainingSet.Nb(i) = OldTrainingSet.Nb(i);
                    
                    for j = 1:length(OldTrainingSet.Data(i).Names)
                        app.XMapToolsData.TrainingSet.Data(i).Names{j} = OldTrainingSet.Data(i).Names{j};
                        
                        app.XMapToolsData.TrainingSet.Data(i).ROI(j).Types{1} = OldTrainingSet.Data(i).Types{j};
                        app.XMapToolsData.TrainingSet.Data(i).ROI(j).Data(1).Coordinates = OldTrainingSet.Data(i).ROI(j).Coordinates;
                        
                    end
                end
            end
            
            % Cleaning and updating maskfile
            if isfield(app.XMapToolsData.MapData.MaskFile.Masks,'PhaseNames')
                app.XMapToolsData.MapData.MaskFile.Masks = rmfield(app.XMapToolsData.MapData.MaskFile.Masks,'PhaseNames');
            end
            if isfield(MapData.MaskFile.Masks,'Density')
                app.XMapToolsData.MapData.MaskFile.Masks = rmfield(app.XMapToolsData.MapData.MaskFile.Masks,'Density');
            end
            
            % Update colors (if needed)
            if ~isempty(app.XMapToolsData.MapData.MaskFile.Names)
                if ~isfield(app.XMapToolsData.MapData.MaskFile.Masks(1),'Colors')
                    for i = 1:length(app.XMapToolsData.MapData.MaskFile.Masks)
                        % Mask colors (added on 24.02.2023)
                        Colors4Mask = UpdateColorsMasks(app,app.XMapToolsData.MapData.MaskFile.Masks(i).Names);
                        app.XMapToolsData.MapData.MaskFile.Masks(i).Colors = Colors4Mask;
                    end
                end
            end
            
            % Create SubMasks
            if ~isfield(app.XMapToolsData.MapData.MaskFile.Masks,'SubMask')
                if ~isempty(app.XMapToolsData.MapData.MaskFile.Names)
                    for i = 1:length(app.XMapToolsData.MapData.MaskFile.Masks)
                        for j = 1:length(app.XMapToolsData.MapData.MaskFile.Masks(i).Names)
                            app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Names = {};
                            app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Densities = [];
                            app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).MaskMap = [];
                            app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).MaskProbability = [];
                        end
                    end
                end
            else
                % Check for size error (submasks created before 4.3)
                if ~isempty(app.XMapToolsData.MapData.MaskFile.Names)
                    for i = 1:length(app.XMapToolsData.MapData.MaskFile.Masks)
                        if ~isequal(length(app.XMapToolsData.MapData.MaskFile.Masks(i).Names),length(app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask))
                            % We reset the submasks in thise case
                            for j = 1:length(app.XMapToolsData.MapData.MaskFile.Masks(i).Names)
                                app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Names = {};
                                app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Densities = [];
                                app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).MaskMap = [];
                                app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).MaskProbability = [];
                            end
                        end
                    end
                end
            end
            
            % Check SubMask data structure and create an empty structure
            if ~isempty(app.XMapToolsData.MapData.MaskFile.Masks)                      %  solve an error with XMapTools 4.1
                if ~isempty(app.XMapToolsData.MapData.MaskFile.Names)
                    if ~isempty(app.XMapToolsData.MapData.MaskFile.Masks(1).Names)
                        for i = 1:length(app.XMapToolsData.MapData.MaskFile.Masks)
                            % Update 4.3 (now same size, see above)
                            for j = 1:length(app.XMapToolsData.MapData.MaskFile.Masks(i).Names)
                                if length(app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask) < j
                                    app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Names = {};
                                    app.XMapToolsData.MapData.XMapToolsData.MapDataata.MaskFile.Masks(i).SubMask(j).Densities = [];
                                    app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).MaskMap = [];
                                    app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).MaskProbability = [];
                                end
                            end
                            %                             if length(app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask) > length(app.XMapToolsData.MapData.MaskFile.Masks(i).Names)-1
                            %                                 for j = length(app.XMapToolsData.MapData.MaskFile.Masks(i).Names):length(app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask)
                            %                                     app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j) = [];
                            %                                 end
                            %                             end
                            %
                            %                             if ~isequal(length(app.XMapToolsData.MapData.MaskFile.Masks(i).Names)-1,length(app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask))
                            %                                 for j = 1:length(app.XMapToolsData.MapData.MaskFile.Masks(i).Names)-1
                            %                                     if length(app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask) < j
                            %                                         app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Names = {};
                            %                                         app.XMapToolsData.MapData.XMapToolsData.MapDataata.MaskFile.Masks(i).SubMask(j).Densities = [];
                            %                                         app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).MaskMap = [];
                            %                                         app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).MaskProbability = [];
                            %                                     end
                            %                                 end
                            %                             end
                        end
                    end
                end
            end
            
            % Check for Mask info (22.11.2023)
            %
            if ~isfield(app.XMapToolsData.MapData.MaskFile.Masks,'Info')
                
                for i = 1:length(app.XMapToolsData.MapData.MaskFile.Masks)
                    app.XMapToolsData.MapData.MaskFile.Masks(i).Info.Algorithm = '';
                    app.XMapToolsData.MapData.MaskFile.Masks(i).Info.SelectedData = {};
                    app.XMapToolsData.MapData.MaskFile.Masks(i).Info.Scaling = '';
                    app.XMapToolsData.MapData.MaskFile.Masks(i).Info.Reproducibility = '';
                    app.XMapToolsData.MapData.MaskFile.Masks(i).Info.Classes = {};
                    app.XMapToolsData.MapData.MaskFile.Masks(i).Info.SelectedPx = [];
                    app.XMapToolsData.MapData.MaskFile.Masks(i).Info.TrainPx = [];
                    app.XMapToolsData.MapData.MaskFile.Masks(i).Info.TestPx = [];
                    app.XMapToolsData.MapData.MaskFile.Masks(i).Info.Accuracy = [];
                    app.XMapToolsData.MapData.MaskFile.Masks(i).Info.Precision = [];
                    app.XMapToolsData.MapData.MaskFile.Masks(i).Info.Recall = [];
                    app.XMapToolsData.MapData.MaskFile.Masks(i).Info.F1Score = [];
                    app.XMapToolsData.MapData.MaskFile.Masks(i).Info.Modes = [];
                    
                    for j = 1:length(app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask)
                        app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Info.Algorithm = '';
                        app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Info.SelectedData = {};
                        app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Info.Scaling = '';
                        app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Info.Reproducibility = '';
                        app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Info.Classes = {};
                        app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Info.SelectedPx = [];
                        app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Info.TrainPx = [];
                        app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Info.TestPx = [];
                        app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Info.Accuracy = [];
                        app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Info.Precision = [];
                        app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Info.Recall = [];
                        app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Info.F1Score = [];
                        app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Info.Modes = [];
                    end
                end
            end
            
            % Check for PxDataRaw (4.4)
            if ~exist('PxDataRaw','var')
                [PxDataRaw] = InitializeXMapToolsData_PxDataRaw(app);
            end
            
            app.XMapToolsData.PxDataRaw = PxDataRaw;
            
            % Other initializations:
            if ~exist('Standards','var')
                [Standards] = InitializeXMapToolsData_Standards(app);
            end
            
            if isempty(Standards)
                [Standards] = InitializeXMapToolsData_Standards(app);
            end
            
            app.XMapToolsData.Standards = Standards;
            
            if ~exist('MapStandards','var')
                [MapStandards] = InitializeXMapToolsData_MapStandards(app);
            end
            
            app.XMapToolsData.MapStandards = MapStandards;
            
            if ~exist('MapLOD','var')
                [MapLOD] = InitializeXMapToolsData_MapLOD(app);
            end
            
            app.XMapToolsData.MapLOD = MapLOD;
            
            if ~exist('MapSizeCheck','var')
                MapSizeCheck.OriginalSize = [];
                MapSizeCheck.ActualSize = [];
                MapSizeCheck.SizeChanged = 0;
            end
            
            if isempty(MapSizeCheck.OriginalSize)
                if length(app.XMapToolsData.MapData.It.Names)>0
                    MapSizeCheck.OriginalSize = size(app.XMapToolsData.MapData.It.Data(1).Map);
                else
                    if length(app.XMapToolsData.MapData.Ct.Names)>0
                        MapSizeCheck.OriginalSize = size(app.XMapToolsData.MapData.Ct.Data(1).Map);
                    else
                        MapSizeCheck.OriginalSize = [0,0];
                    end
                end
            end
            if isempty(MapSizeCheck.ActualSize)
                MapSizeCheck.ActualSize = MapSizeCheck.OriginalSize ;
            end
            
            app.XMapToolsData.MapSizeCheck = MapSizeCheck;
            app.mapsizeLabel.Text = ['Map size: ',num2str(app.XMapToolsData.MapSizeCheck.ActualSize(2)),' x ',num2str(app.XMapToolsData.MapSizeCheck.ActualSize(1)), ' (X,Y)'];
            
            
            if ~exist('SegScheme','var')
                SegScheme.Names = {};
                SegScheme.Types = [];
                SegScheme.Nb = [];
                SegScheme.Data(1).Names = {};
                SegScheme.Data(1).Range = [];
            end
            
            app.XMapToolsData.SegScheme = SegScheme;
            
            if exist('Options','var')
                
                if isfield(Options,'MapResolution') % version 09.11.2020
                    app.ResolutionField.Value = Options.MapResolution;
                    app.DisplayScaleBar.Value = Options.DisplayScaleBar;
                    app.ColorScaleBar.Value = Options.ScaleBarColor;
                    app.FieldAngleView.Value = Options.RotationAngle;
                    
                    try
                        app.Options_ColormapDropDown.Value = Options.Colormap;
                    catch ME
                        % This is for the case the colormap name has
                        % changed
                    end
                    app.Options_ColormapResEditField.Value = Options.ColormapRes;
                    app.Options_LogColormap.Value = Options.ColormapLog;
                    app.Options_LowerCheckBox.Value = Options.Colormap_LowerLayer;
                    app.Options_LowerColor.Value = Options.Colormap_LowerLayerColor;
                    app.Options_UpperCheckBox.Value = Options.Colormap_UpperLayer;
                    app.Options_UpperColor.Value = Options.Colormap_UpperLayerColor;
                end
                
                % Following implementations with individual check for
                % compatibility:
                
                if isfield(Options,'ColormapInverse')
                    app.Options_Colorbar_Inverse.Value = Options.ColormapInverse;
                end
                if isfield(Options,'Reproducibility')
                    app.Classify_Reproducibility.Value = Options.Reproducibility;
                end
                if isfield(Options,'ReproducibilityValue')
                    app.Classify_ReproducibilityValue.Value = Options.ReproducibilityValue;
                end
                if isfield(Options,'NumberOfTrees')
                    app.Classify_OptionValue.Value = Options.NumberOfTrees;
                end
                if isfield(Options,'Medfilter3DsurfaceValue')
                    app.Options_Medfilter3DsurfaceSpinner.Value = Options.Medfilter3DsurfaceValue;
                end
                if isfield(Options,'ApplyAutoContrast')
                    app.Options_ApplyAutoContrast.Value = Options.ApplyAutoContrast;
                end
                if isfield(Options,'MaskSelectionForMerged')
                    app.Options_MaskSelectionForMerged.Value = Options.MaskSelectionForMerged;
                end
            end
            
            app.CurrentProject = [ProjectPath,'/',ProjectName];
            ProjectName_OptionsValueChanged(app);
            
            % Check the indexes and update the values using the new
            % database
            for i = 1:length(app.XMapToolsData.MapData.It.Names)
                ElName = app.XMapToolsData.MapData.It.Names{i};
                
                UnderScore = find(ismember(ElName,'_'));
                if ~isempty(UnderScore)
                    ElName = ElName(1:UnderScore-1);
                end
                
                where = find(ismember(app.ElOxDataDef.ElList,ElName));
                if ~isempty(where)
                    app.XMapToolsData.MapData.It.ElInd(i) = where;
                else
                    app.XMapToolsData.MapData.It.ElInd(i) = 0;
                end
            end
            
            app.WaitBar.Message = 'Finishing';
            
            ResetGUI(app);
            UpdateGUI(app);
            
            % Display the first map if available (in the future we could
            % restore the previous state)
            if length(app.XMapToolsData.MapData.It.Names) > 1
                app.Node_It.expand();
                app.TreeData_Main.SelectedNodes = app.Node_It.Children(1);
                TreeData_MainSelectionChanged(app);
                Button_FigMain_ResetZoomPanPushed(app, 0);
            end
            
            close(app.WaitBar);
        end
        
        
        function SaveProject(app,ProjectName)
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Saving project';
            
            MapData = app.XMapToolsData.MapData;
            TrainingSet = app.XMapToolsData.TrainingSet;
            Standards = app.XMapToolsData.Standards;
            MapStandards = app.XMapToolsData.MapStandards;
            SegScheme = app.XMapToolsData.SegScheme;
            MapSizeCheck = app.XMapToolsData.MapSizeCheck;
            MapLOD = app.XMapToolsData.MapLOD;
            PxDataRaw = app.XMapToolsData.PxDataRaw;
            Version = app.XMapTools_VER;
            
            % Options
            Options.MapResolution = app.ResolutionField.Value;
            Options.DisplayScaleBar = app.DisplayScaleBar.Value;
            Options.ScaleBarColor = app.ColorScaleBar.Value;
            Options.RotationAngle = app.FieldAngleView.Value;
            
            Options.Colormap = app.Options_ColormapDropDown.Value;
            Options.ColormapRes = app.Options_ColormapResEditField.Value;
            Options.ColormapLog = app.Options_LogColormap.Value;
            Options.ColormapInverse = app.Options_Colorbar_Inverse.Value;
            Options.Colormap_LowerLayer = app.Options_LowerCheckBox.Value;
            Options.Colormap_LowerLayerColor = app.Options_LowerColor.Value;
            Options.Colormap_UpperLayer = app.Options_UpperCheckBox.Value;
            Options.Colormap_UpperLayerColor = app.Options_UpperColor.Value;
            
            Options.DisplayNegativeValues = app.Options_DispNegativeValues.Value;
            Options.ApplyAutoContrast = app.Options_ApplyAutoContrast.Value;
            
            Options.Medfilter3DsurfaceValue = app.Options_Medfilter3DsurfaceSpinner.Value;
            
            Options.Reproducibility = app.Classify_Reproducibility.Value;
            
            Options.ReproducibilityValue = app.Classify_ReproducibilityValue.Value;
            Options.NumberOfTrees = app.Classify_OptionValue.Value;
            
            Options.MaskSelectionForMerged = app.Options_MaskSelectionForMerged.Value;
            
            % SAVE file
            save([ProjectName],'MapData','TrainingSet','Standards','MapStandards','MapSizeCheck','SegScheme','MapLOD','Options','Version','-v7.3');
            
            % 'PxDataRaw' not saved yet because of size problem
            
            app.CurrentProject = ProjectName;
            
            [filepath,name,ext] = fileparts(app.CurrentProject);
            switch app.ProjectName_Options.Value
                case 'File'
                    app.ProjectName_DisplayField.Value = [name,ext];
                    app.ProjectName_DisplayField.HorizontalAlignment = 'Center';
                    app.ProjectName_DisplayField.FontSize = 11;
                case 'Path'
                    app.ProjectName_DisplayField.Value = [filepath];
                    app.ProjectName_DisplayField.HorizontalAlignment = 'Left';
                    app.ProjectName_DisplayField.FontSize = 10;
            end
            
            app.SaveRequired = 0;
            
            close(app.WaitBar);
            %uiwait(msgbox('The project was saved','XMapTools','modal'));
            
        end
        
        function SaveProjectAs(app)
            f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
            [filename, pathname] = uiputfile('aaa4.mat', 'Save Project as');
            delete(f);
            figure(app.XMapTools_GUI)
            if isequal(filename,0)
                return
            end
            
            SaveProject(app,[pathname,filename]);
        end
        
        
        
        
        
        
        function RebuildGUI(app,Mode)
            % this function rebuilds the node data of a given Tree node
            switch Mode
                case 'It'
                    for i = 1:length(app.Node_It.Children)
                        app.Node_It.Children(i).NodeData = [1,i];
                    end
                case 'Qt'
                    for i = 1:length(app.Node_Qt.Children)
                        app.Node_Qt.Children(i).NodeData = [2,i,0];
                        for j = 1:length(app.Node_Qt.Children(i).Children)
                            app.Node_Qt.Children(i).Children(j).NodeData = [2,i,j];
                        end
                    end
                case 'Me'
                    for i = 1:length(app.Node_Me.Children)
                        app.Node_Me.Children(i).NodeData = [3,i,0];
                        for j = 1:length(app.Node_Me.Children(i).Children)
                            app.Node_Me.Children(i).Children(j).NodeData = [3,i,j];
                        end
                    end
                case 'Re'
                    for i = 1:length(app.Node_Re.Children)
                        app.Node_Re.Children(i).NodeData = [4,i,0];
                        for j = 1:length(app.Node_Re.Children(i).Children)
                            app.Node_Re.Children(i).Children(j).NodeData = [4,i,j];
                        end
                    end
                case 'Ot'
                    for i = 1:length(app.Node_Ot.Children)
                        app.Node_Ot.Children(i).NodeData = [5,i];
                    end
                case 'ROI'
                    for i = 1:length(app.Node_ROI.Children)
                        app.Node_ROI.Children(i).NodeData = [7,i];
                    end
                    
                case 'Im'
                    for i = 1:length(app.Node_Im.Children)
                        app.Node_Im.Children(i).NodeData = [8,i,0];
                        for j = 1:length(app.Node_Im.Children(i).Children)
                            app.Node_Im.Children(i).Children(j).NodeData = [8,i,j];
                        end
                    end
                    
                case 'MaskFiles'
                    for i = 1:length(app.Node_Masks.Children)
                        app.Node_Masks.Children(i).NodeData = [11,i,0,0];
                        for j = 1:length(app.Node_Masks.Children(i).Children)
                            app.Node_Masks.Children(i).Children(j).NodeData = [11,i,j,0];
                            for k = 1:length(app.Node_Masks.Children(i).Children(j).Children)
                                app.Node_Masks.Children(i).Children(j).Children(k).NodeData = [11,i,j,k];
                            end
                        end
                    end
                case 'TrainingSet'
                    for i = 1:length(app.Node_TrainingSet.Children)
                        app.Node_TrainingSet.Children(i).NodeData = [12,i,0];
                        for j = 1:length(app.Node_TrainingSet.Children(i).Children)
                            app.Node_TrainingSet.Children(i).Children(j).NodeData = [12,i,j];
                            for k = 1:length(app.Node_TrainingSet.Children(i).Children(j).Children)
                                app.Node_TrainingSet.Children(i).Children(j).Children(k).NodeData = [12,i,j,k];
                            end
                        end
                    end
                case 'SegSchemes'
                    for i = 1:length(app.Node_SegScheme.Children)
                        app.Node_SegScheme.Children(i).NodeData = [13,1,i,0];
                        for j = 1:length(app.Node_SegScheme.Children(i).Children)
                            app.Node_SegScheme.Children(i).Children(j).NodeData = [13,1,i,j];
                            for k = 1:length(app.Node_SegScheme.Children(i).Children(j).Children)
                                app.Node_SegScheme.Children(i).Children(j).Children(k).NodeData = [13,1,i,j,k];
                            end
                        end
                    end
                    
                case 'Standards'
                    for i = 1:length(app.Node_Standards.Children)
                        app.Node_Standards.Children(i).NodeData = [14,i];
                    end
                    
                case 'LOD'
                    for i = 1:length(app.Node_LOD.Children)
                        app.Node_LOD.Children(i).NodeData = [16,i,0,0];
                        for j = 1:length(app.Node_LOD.Children(i).Children)
                            app.Node_LOD.Children(i).Children(j).NodeData = [16,i,j,0];
                            for k = 1:length(app.Node_LOD.Children(i).Children(j).Children)
                                app.Node_LOD.Children(i).Children(j).Children(k).NodeData = [16,i,j,k];
                            end
                        end
                    end
            end
            
        end
        
        function SelectNodeFromNodeData(app,NodeData)
            
            switch NodeData(1)
                case 1
                    if NodeData(2) > 0
                        app.TreeData_Main.SelectedNodes = app.Node_It.Children(NodeData(2));
                    end
            end
            
            
        end
        
        function PlotMap_CreateDisplayMultiMap(app,SelectedNodes,SelectedAdditional)
            
            Idx = SelectedNodes.NodeData(2);
            
            Data = app.XMapToolsData.MapData.Im.Data(Idx);
            
            switch app.XMapToolsData.MapData.Im.Types(Idx)
                case 2
                    
                    app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
                    app.WaitBar.Message = 'Plotting data';
                    
                    NbMaps = length(Data.Labels);
                    
                    % Set the grid
                    Lims   = [3,8,18,32,50,60,80,100];
                    idx = find(Lims>=NbMaps);
                    
                    Lin = idx(1);
                    Col = ceil(NbMaps/Lin);
                    
                    TileContainer = tiledlayout(app.PanelMulti,Lin,Col);
                    app.AxMultiplePlots = [];
                    
                    TileAx = [];
                    
                    for i = 1:NbMaps
                        
                        app.AxMultiplePlots(i).ax = nexttile(TileContainer);
                        
                        imagesc(app.AxMultiplePlots(i).ax,Data.CData(i).Map);
                        app.AxMultiplePlots(i).ax.XTick = [];
                        app.AxMultiplePlots(i).ax.YTick = [];
                        app.AxMultiplePlots(i).ax.Title.String = Data.Labels{i};
                        axis(app.AxMultiplePlots(i).ax,'image');
                        caxis(app.AxMultiplePlots(i).ax,[Data.CData(i).Lim(1),Data.CData(i).Lim(2)])
                        colormap(app.AxMultiplePlots(i).ax,Data.CData(i).ColorMap);
                        colorbar(app.AxMultiplePlots(i).ax,'SouthOutside');
                        
                        if app.Options_LogColormap.Value
                            app.AxMultiplePlots(i).ax.ColorScale = 'log';
                        else
                            app.AxMultiplePlots(i).ax.ColorScale = 'linear';
                        end
                        
                        TileAx = [TileAx,app.AxMultiplePlots(i).ax];
                    end
                    
                    linkaxes(TileAx,'xy');
                    
                    app.PanelSingle.Visible = 'off';
                    app.PanelMulti.Visible = 'on';
                    
                    close(app.WaitBar);
                    
                    %cla(app.FigHistLive,'reset');
                    app.FigHistLive.Visible = 'off';
                    
                    
                case 3
                    
                    % Update interface
                    app.PanelSingle.Visible = 'on';
                    app.PanelMulti.Visible = 'off';
                    
                    CompositeMap = zeros(size(Data.CData(1).Map));
                    
                    for i = 1:length(Data.CData)
                        
                        Map = Data.CData(i).Map;
                        Min = Data.CData(i).Lim(1);
                        if isequal(Min,0)
                            Min = 1e-15;
                        end
                        Max = Data.CData(i).Lim(2);
                        
                        SelPx = find(Map >= Min & Map <= Max);
                        
                        PxData = Map(SelPx);
                        
                        CompositeMap(SelPx) = PxData;
                    end
                    
                    cla(app.FigMain,'reset')
                    
                    imagesc(app.FigMain,CompositeMap);
                    caxis(app.FigMain,'auto')
                    app.FigMain.XTick = [];
                    app.FigMain.YTick = [];
                    axis(app.FigMain,'image');
                    
                    c = colorbar(app.FigMain);
                    colormap(app.FigMain,app.ColorMapValues);
                    
                    caxis(app.FigMain,[Data.CData(1).Lim(1),Data.CData(1).Lim(2)])
                    
                    Data2PlotNonZero = CompositeMap(find(CompositeMap > 0 & CompositeMap < inf));
                    DataMin = Data.CData(1).Lim(1);
                    DataMax = Data.CData(1).Lim(2);
                    
                    DataMinHist = double(min(Data2PlotNonZero));
                    DataMaxHist = double(max(Data2PlotNonZero));
                    
                    UpdateLiveHistogram(app,Data2PlotNonZero,DataMinHist,DataMaxHist,DataMin,DataMax)
                    
                    % Add colormap here... We could use the first one
                    
                    %c = colorbar(app.FigMain);
                    %colormap(app.FigMain,CompositeColorMap);
                    
                    
                    
                case 4
                    
                    % Update interface
                    app.PanelSingle.Visible = 'on';
                    app.PanelMulti.Visible = 'off';
                    
                    CompositeMap = zeros(size(Data.CData(1).Map));
                    Ticks = [];
                    Labels = {};
                    
                    for i = 1:length(Data.CData)
                        
                        Map = Data.CData(i).Map;
                        Min = Data.CData(i).Lim(1);
                        if isequal(Min,0)
                            Min = 1e-15;
                        end
                        Max = Data.CData(i).Lim(2);
                        
                        SelPx = find(Map >= Min & Map <= Max);
                        
                        PxData = Map(SelPx);
                        PxDataStd = (i-1) + (PxData - Min)./(Max-Min);
                        
                        CompositeMap(SelPx) = PxDataStd;
                        
                        if isequal(i,1)
                            CompositeColorMap = Data.CData(i).ColorMap;
                        else
                            CompositeColorMap = [CompositeColorMap;Data.CData(i).ColorMap];
                        end
                        
                        Ticks_Temp = [i-1+0.2:0.2:i-0.2];
                        Values = (Ticks_Temp - i + 1).*(Max-Min) + Min;
                        Labels_Temp = {};
                        for i =1:length(Values)
                            Labels_Temp{i} = num2str(Values(i));
                        end
                        
                        Ticks = [Ticks,Ticks_Temp];
                        Labels = [Labels,Labels_Temp];
                        
                    end
                    
                    cla(app.FigMain,'reset')
                    
                    imagesc(app.FigMain,CompositeMap);
                    caxis(app.FigMain,'auto')
                    app.FigMain.XTick = [];
                    app.FigMain.YTick = [];
                    axis(app.FigMain,'image');
                    c = colorbar(app.FigMain);
                    colormap(app.FigMain,CompositeColorMap);
                    
                    c.Ticks = Ticks;
                    c.TickLabels = Labels';
                    
                    %cla(app.FigHistLive,'reset');
                    app.FigHistLive.Visible = 'off';
            end
            
            if isequal(app.XMapToolsData.MapData.Im.Types(Idx),2) || isequal(app.XMapToolsData.MapData.Im.Types(Idx),3) || isequal(app.XMapToolsData.MapData.Im.Types(Idx),4)
                app.Button_FigMain_AutoContrast.Enable = 'off';
                app.Button_FigMain_MedianFilter.Enable = 'off';
                app.Button_FigMain_ResetDisplay.Enable = 'off';
                app.Button_FigMain_Pan.Enable = 'off';
                app.Button_FigMain_ZoomOut.Enable = 'off';
                app.Button_FigMain_ZoomIn.Enable = 'off';
                app.Button_FigMain_CursorInfo.Enable = 'off';
                app.Button_FigMain_PlotSurface.Enable = 'off';
                app.Button_FigMain_CopyImage.Enable = 'off';
                app.Button_FigMain_OpenNewWindow.Enable = 'off';
                app.Button_FigMain_ResetZoomPan.Enable = 'off';
                
                app.Image_AddMultiPlotImageMenu.Enable = 'on';
                app.Image_AddMultiLayerImagesharedscaleMenu.Enable = 'on';
                app.Image_AddMultiLayerImageMenu.Enable = 'on';
                
                app.Image_AddCurrentImageMenu.Enable = 'off';
                
                app.MapInfo_TextArea.Value = sprintf('');
            end
            
        end
        
        function PlotMap_DisplayMultipleMaps(app,SelectedNodes,SelectedAdditional)
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Plotting data';
            
            try
                % Update interface
                app.PanelSingle.Visible = 'off';
                app.PanelMulti.Visible = 'on';
                
                NbMaps = length(SelectedNodes);
                
                % Set the grid
                %                 Lims   = [3,8,18,32,50,60,80,100];
                %                 idx = find(Lims>=NbMaps);
                %
                %                 Lin = idx(1);
                %                 Col = ceil(NbMaps/Lin);
                
                TileContainer = tiledlayout(app.PanelMulti,'flow');
                app.AxMultiplePlots = [];
                
                TileAx = [];
                for i = 1:NbMaps
                    
                    MapCode = SelectedNodes(i).NodeData;
                    
                    if isequal(MapCode(1),1) % It
                        if MapCode(2) > 0
                            Data2Plot_Temp = app.XMapToolsData.MapData.It.Data(MapCode(2)).Map;
                            Info_MapName_Temp = app.XMapToolsData.MapData.It.Names{MapCode(2)};
                        else
                            close(app.WaitBar);
                            return
                        end
                    elseif isequal(MapCode(1),2) % Qt
                        if MapCode(2) > 0 && MapCode(3) > 0
                            Data2Plot_Temp = app.XMapToolsData.MapData.Qt.Data(MapCode(2)).CData(MapCode(3)).Map;
                            Info_MapName_Temp = char(app.XMapToolsData.MapData.Qt.Data(MapCode(2)).ElNames(MapCode(3)));
                        else
                            close(app.WaitBar);
                            return
                        end
                    elseif isequal(MapCode(1),3) % Me
                        if MapCode(2) > 0 && MapCode(3) > 0
                            Data2Plot_Temp = app.XMapToolsData.MapData.Me.Data(MapCode(2)).CData(MapCode(3)).Map;
                            Info_MapName_Temp = char(app.XMapToolsData.MapData.Me.Data(MapCode(2)).ElNames(MapCode(3)));
                        else
                            close(app.WaitBar);
                            return
                        end
                    elseif isequal(MapCode(1),4) % Re
                        if MapCode(2) > 0 && MapCode(3) > 0
                            Data2Plot_Temp = app.XMapToolsData.MapData.Re.Data(MapCode(2)).CData(MapCode(3)).Map;
                            Info_MapName_Temp = char(app.XMapToolsData.MapData.Re.Data(MapCode(2)).Labels(MapCode(3)));
                        else
                            close(app.WaitBar);
                            return
                        end
                    elseif isequal(MapCode(1),5) % Ot
                        if MapCode(2) > 0
                            Data2Plot_Temp = app.XMapToolsData.MapData.Ot.Data(MapCode(2)).Map;
                            Info_MapName_Temp = char(app.XMapToolsData.MapData.Ot.Names{MapCode(2)});
                        else
                            close(app.WaitBar);
                            return
                        end
                    elseif isequal(MapCode(1),6) % Ct
                        if MapCode(2) > 0
                            Data2Plot_Temp = app.XMapToolsData.MapData.Ct.Data(MapCode(2)).Map;
                            Info_MapName_Temp = char(app.XMapToolsData.MapData.Ct.Names{MapCode(2)});
                        else
                            close(app.WaitBar);
                            return
                        end
                    end
                    
                    app.AxMultiplePlots(i).ax = nexttile(TileContainer);
                    
                    imagesc(app.AxMultiplePlots(i).ax,Data2Plot_Temp);
                    app.AxMultiplePlots(i).ax.XTick = [];
                    app.AxMultiplePlots(i).ax.YTick = [];
                    app.AxMultiplePlots(i).ax.Title.String = Info_MapName_Temp;
                    axis(app.AxMultiplePlots(i).ax,'image');
                    colormap(app.AxMultiplePlots(i).ax,app.ColorMapValues);
                    colorbar(app.AxMultiplePlots(i).ax,'SouthOutside');
                    
                    TileAx = [TileAx,app.AxMultiplePlots(i).ax];
                    
                end
                
                linkaxes(TileAx,'xy');
                
            catch ME
                close(app.WaitBar);
            end
            close(app.WaitBar);
            
        end
        
        
        
        function PlotMap_DisplaySelectedMap(app,MapCode,AddCode)
            %profile on
            
            % Update interface
            app.PanelSingle.Visible = 'on';
            app.PanelMulti.Visible = 'off';
            
            app.ButtonRotateView.Enable = 'on';
            app.Button_FigMain_AutoContrast.Enable = 'on';
            app.Button_FigMain_MedianFilter.Enable = 'on';
            app.Button_FigMain_ResetDisplay.Enable = 'on';
            
            app.Button_FigMain_CursorInfo.Enable = 'on';
            
            app.Button_FigMain_ZoomIn.Enable = 'on';
            app.Button_FigMain_ZoomOut.Enable = 'on';
            app.Button_FigMain_Pan.Enable = 'on';
            app.Button_FigMain_ResetZoomPan.Enable = 'on';
            
            app.Button_FigMain_OpenNewWindow.Enable = 'on';
            app.Button_FigMain_CopyImage.Enable = 'on';
            app.Button_FigMain_PlotSurface.Enable = 'on';
            
            app.DataCursorMode = 0;
            ApplyCursorMode(app);
            
            ROImode = 0;
            SingleLayerMode = 0;
            app.Jiahui = 0;
            app.CovarMatrix = [];
            
            % Extract the data (TO BE CHANGED?)
            if isequal(MapCode(1),1) % It
                if MapCode(2) > 0
                    app.Data2Plot = app.XMapToolsData.MapData.It.Data(MapCode(2)).Map;
                    app.Info_MapType = 'Intensity';
                    app.Info_Phase = 'none';
                    app.Info_MapName = app.XMapToolsData.MapData.It.Names{MapCode(2)};
                else
                    if length(app.XMapToolsData.MapData.It.Names) >= 1
                        app.Data2Plot = app.XMapToolsData.MapData.It.Data(1).Map;
                        app.Info_MapType = 'Intensity';
                        app.Info_Phase = 'none';
                        app.Info_MapName = app.XMapToolsData.MapData.It.Names{1};
                    else
                        return
                    end
                end
            elseif isequal(MapCode(1),2) % Qt
                if MapCode(2) > 0 && MapCode(3) > 0
                    app.Data2Plot = app.XMapToolsData.MapData.Qt.Data(MapCode(2)).CData(MapCode(3)).Map;
                    app.Info_MapType = 'Quanti';
                    app.Info_Phase = char(app.XMapToolsData.MapData.Qt.Names(MapCode(2)));
                    app.Info_MapName = char(app.XMapToolsData.MapData.Qt.Data(MapCode(2)).ElNames(MapCode(3)));
                elseif MapCode(2) > 0 && isequal(MapCode(3),0)
                    app.Data2Plot = app.XMapToolsData.MapData.Qt.Data(MapCode(2)).CData(1).Map;
                    app.Info_MapType = 'Quanti';
                    app.Info_Phase = char(app.XMapToolsData.MapData.Qt.Names(MapCode(2)));
                    app.Info_MapName = char(app.XMapToolsData.MapData.Qt.Data(MapCode(2)).ElNames(1));
                else
                    if length(app.XMapToolsData.MapData.Qt.Names) >= 1
                        app.Data2Plot = app.XMapToolsData.MapData.Qt.Data(1).CData(1).Map;
                        app.Info_MapType = 'Quanti';
                        app.Info_Phase = char(app.XMapToolsData.MapData.Qt.Names(1));
                        app.Info_MapName = char(app.XMapToolsData.MapData.Qt.Data(1).ElNames(1));
                    else
                        return
                    end
                end
            elseif isequal(MapCode(1),3) % Me
                if MapCode(2) > 0 && MapCode(3) > 0
                    app.Data2Plot = app.XMapToolsData.MapData.Me.Data(MapCode(2)).CData(MapCode(3)).Map;
                    app.Info_MapType = 'Merged';
                    app.Info_Phase = char(app.XMapToolsData.MapData.Me.Names(MapCode(2)));
                    app.Info_MapName = char(app.XMapToolsData.MapData.Me.Data(MapCode(2)).ElNames(MapCode(3)));
                elseif MapCode(2) > 0 && isequal(MapCode(3),0)
                    app.Data2Plot = app.XMapToolsData.MapData.Me.Data(MapCode(2)).CData(1).Map;
                    app.Info_MapType = 'Merged';
                    app.Info_Phase = char(app.XMapToolsData.MapData.Me.Names(MapCode(2)));
                    app.Info_MapName = char(app.XMapToolsData.MapData.Me.Data(MapCode(2)).ElNames(1));
                else
                    if length(app.XMapToolsData.MapData.Me.Names) >= 1
                        app.Data2Plot = app.XMapToolsData.MapData.Me.Data(1).CData(1).Map;
                        app.Info_MapType = 'Merged';
                        app.Info_Phase = char(app.XMapToolsData.MapData.Me.Names(1));
                        app.Info_MapName = char(app.XMapToolsData.MapData.Me.Data(1).ElNames(1));
                    else
                        return
                    end
                end
            elseif isequal(MapCode(1),4) % Re
                if MapCode(2) > 0 && MapCode(3) > 0
                    app.Data2Plot = app.XMapToolsData.MapData.Re.Data(MapCode(2)).CData(MapCode(3)).Map;
                    app.Info_MapType = 'Results';
                    app.Info_Phase = char(app.XMapToolsData.MapData.Re.Names(MapCode(2)));
                    app.Info_MapName = char(app.XMapToolsData.MapData.Re.Data(MapCode(2)).Labels(MapCode(3)));
                elseif MapCode(2) > 0 && isequal(MapCode(3),0)
                    app.Data2Plot = app.XMapToolsData.MapData.Re.Data(MapCode(2)).CData(1).Map;
                    app.Info_MapType = 'Results';
                    app.Info_Phase = char(app.XMapToolsData.MapData.Re.Names(MapCode(2)));
                    app.Info_MapName = char(app.XMapToolsData.MapData.Re.Data(MapCode(2)).Labels(1));
                else
                    if length(app.XMapToolsData.MapData.Re.Names) >= 1
                        app.Data2Plot = app.XMapToolsData.MapData.Re.Data(1).CData(1).Map;
                        app.Info_MapType = 'Results';
                        app.Info_Phase = char(app.XMapToolsData.MapData.Re.Names(1));
                        app.Info_MapName = char(app.XMapToolsData.MapData.Re.Data(1).Labels(1));
                    else
                        return
                    end
                end
            elseif isequal(MapCode(1),5) % Ot
                if MapCode(2) > 0
                    app.Data2Plot = app.XMapToolsData.MapData.Ot.Data(MapCode(2)).Map;
                    app.Info_MapType = 'Other';
                    app.Info_Phase = 'none';
                else
                    if length(app.XMapToolsData.MapData.Ot.Names) >= 1
                        app.Data2Plot = app.XMapToolsData.MapData.Ot.Data(1).Map;
                        app.Info_MapType = 'Other';
                        app.Info_Phase = 'none';
                    else
                        return
                    end
                end
            elseif isequal(MapCode(1),6) % Ct
                if MapCode(2) > 0
                    app.Data2Plot = app.XMapToolsData.MapData.Ct.Data(MapCode(2)).Map;
                    app.Info_MapType = 'CT';
                    app.Info_Phase = 'none';
                else
                    return
                end
            elseif isequal(MapCode(1),7) % ROI
                if MapCode(2) > 0
                    if isempty(app.LiveSliderValueForROI)
                        app.LiveSliderValueForROI = 1;
                    end
                    SizeROI = size(app.XMapToolsData.MapData.ROI.Data(MapCode(2)).ROI);
                    if isequal(length(SizeROI),2) % single ROI
                        app.LiveSliderValueForROI = 1;
                    end
                    if isequal(app.ROI_DispVertical.Value,1) && ~isequal(length(SizeROI),2)
                        %app.Data2Plot = squeeze(app.XMapToolsData.MapData.ROI.Data(MapCode(2)).ROI(app.LiveSliderValueForROI,:,:));
                        
                        SizeROI = size(app.XMapToolsData.MapData.ROI.Data(MapCode(2)).ROI);
                        
                        Angle = app.ROI_AngleOrientation.Value; %app.ROI_VerticalSlider.Value;
                        
                        [B,x,y,z] = obliqueslice(app.XMapToolsData.MapData.ROI.Data(MapCode(2)).ROI,[round(SizeROI(1)/2),round(SizeROI(2)/2),round(SizeROI(3)/2)],[cosd(Angle),sind(Angle),0],'Method','nearest');
                        %[B,x,y,z] = obliqueslice(app.XMapToolsData.MapData.ROI.Data(MapCode(2)).ROI,[400,400,round(SizeROI(3)/2)],[cosd(Angle),sind(Angle),0],'Method','nearest');
                        app.Data2Plot = imrotate(B,-90+Angle,'bilinear','crop');
                        
                        MaxH = SizeROI(3);
                        MaxW = max(SizeROI(1:2));
                        
                        DiffH = size(app.Data2Plot,1)-MaxH;
                        if DiffH > 0
                            app.Data2Plot = app.Data2Plot(floor(DiffH/2):end-ceil(DiffH/2),:);
                        end
                        DiffW = size(app.Data2Plot,2)-MaxW;
                        if DiffW > 0
                            app.Data2Plot = app.Data2Plot(:,floor(DiffW/2):end-ceil(DiffW/2));
                        end
                        
                        size(app.Data2Plot);
                        %keyboard
                        
                        %Angle = 135;
                        %[B,x,y,z] = obliqueslice(app.XMapToolsData.MapData.ROI.Data(MapCode(2)).ROI,[468,468,410],[cosd(Angle),sind(Angle),0]);
                        %K = imrotate(B,-90+Angle,'bilinear','crop');
                        %figure, imagesc(K), axis image, colorbar
                        %figure, plot3(x,y,z), grid on
                        %xlabel('x'), ylabel('y'), zlabel('z')
                        %axis([0 1000 0 1000 0 1000])
                    else
                        app.Data2Plot = app.XMapToolsData.MapData.ROI.Data(MapCode(2)).ROI(:,:,app.LiveSliderValueForROI);
                    end
                    
                    Labels4Colorbar = app.XMapToolsData.MapData.ROI.Data(MapCode(2)).Names;
                    ROImode = 1;
                    
                else
                    return
                end
                
            elseif isequal(MapCode(1),8) % Multi
                if isequal(MapCode(3),0)
                    MapCode(3) = 1;
                end
                if MapCode(2) > 0
                    SingleLayerMode = 1;
                    app.Data2Plot = app.XMapToolsData.MapData.Im.Data(MapCode(2)).CData(MapCode(3)).Map;
                    SingleLayerMin = app.XMapToolsData.MapData.Im.Data(MapCode(2)).CData(MapCode(3)).Lim(1);
                    SingleLayerMax = app.XMapToolsData.MapData.Im.Data(MapCode(2)).CData(MapCode(3)).Lim(2);
                    app.Info_MapType = 'Other image';
                    app.Info_Phase = 'none';
                end
                
            elseif isequal(MapCode(1),-1) % Plot comes from the Additiional Menu
                if isequal(AddCode(3),0)
                    return
                end
                app.Data2Plot = app.XMapToolsData.MapStandards(AddCode(2)).Data(AddCode(3)).Map;
                
            elseif isequal(MapCode(1),-2)
                if AddCode(4) < 2
                    app.Data2Plot = app.XMapToolsData.MapLOD.Data(AddCode(2)).ElData(AddCode(3)).Map_LOD;
                elseif isequal(AddCode(4),2)
                    app.Data2Plot = app.XMapToolsData.MapLOD.Data(AddCode(2)).ElData(AddCode(3)).Map_Si_mtx;
                else
                    app.Data2Plot = app.XMapToolsData.MapLOD.Data(AddCode(2)).ElData(AddCode(3)).Map_Back_mtx;
                end
            else
                
                return
            end
            
            % Special ROI mode
            if ROImode
                
                if isempty(app.FigMain)
                    disp('Container for plot created');
                    TileContainer = tiledlayout(app.PanelFigMain,1,1,'Padding','compact');
                    app.FigMain = nexttile(TileContainer);
                    
                    imagesc(app.FigMain,app.Data2Plot+1);
                    
                    ApplySettingsPlotEngine(app);
                    
                    
                else
                    % Update only the data to improve display performances
                    app.FigMain.Children(end).CData = app.Data2Plot+1;
                    
                    DeactivatePlotZoomPanOptions(app);
                end
                
                NbROI = length(Labels4Colorbar);
                
                caxis(app.FigMain,[1 NbROI+2])
                app.ActiveColorbar = colorbar(app.FigMain);
                PlotMap_UpdateColormap(app,'Mask',NbROI+1);  % Update colormap (TEMP)
                
                app.ActiveColorbar.TickLabelsMode = 'manual';
                app.ActiveColorbar.Ticks = [1.5:1:NbROI+2];
                app.ActiveColorbar.TickLabels = {'unselected',Labels4Colorbar{:}};
                
                ColorMap = colormap(app.FigMain);
                ColorMap(1,:) = [0,0,0];
                colormap(app.FigMain,ColorMap);
                
                if length(app.FigMain.XTick) > 1
                    %axis(app.FigMain,'image');
                    app.FigMain.XTick = [];
                    app.FigMain.YTick = [];
                end
                
                %drawnow
                return
                
            else
                
                app.FigHistLive.Visible = 'on';
                
                % MASK AND SUBMASK SELECTION
                if ~isempty(AddCode)
                    if isequal(MapCode(1),1) || isequal(MapCode(1),4) || isequal(MapCode(1),3)
                        
                        % Check if a mask is selected
                        if isequal(AddCode(1,1),11) && AddCode(1,3) > 1 && isequal(AddCode(1,4),0)
                            SelectedMaskMap = app.XMapToolsData.MapData.MaskFile.Masks(AddCode(1,2)).MaskMap;
                            MaskMap = zeros(size(SelectedMaskMap));
                            for i = 1:size(AddCode,1)
                                MaskInd = find(SelectedMaskMap == AddCode(i,3)-1);
                                MaskMap(MaskInd) = ones(size(MaskInd));
                            end
                            
                            if isequal(MapCode(1),3) && isequal(app.Options_MaskSelectionForMerged.Value,0)
                                MaskMap = ones(size(SelectedMaskMap));
                            end
                            
                            app.Data2Plot = app.Data2Plot.*MaskMap;
                            
                            if size(AddCode,1) > 1
                                app.Info_Phase = 'Multiple selection';
                            else
                                app.Info_Phase = char(app.XMapToolsData.MapData.MaskFile.Masks(AddCode(2)).Names(AddCode(3)));
                            end
                        end
                        
                        % Check if a sub training set is selected (added 10.07.22)
                        if isequal(AddCode(1,1),12) && AddCode(1,2) > 0 && isequal(AddCode(1,3),0)
                            if isequal(app.XMapToolsData.TrainingSet.Types(AddCode(2)),2)
                                % Check maskfile signature
                                Signatures = extractfield(app.XMapToolsData.MapData.MaskFile.Masks,'Signature');
                                MaskInd = find(ismember(Signatures,app.XMapToolsData.TrainingSet.MaskSignature(AddCode(2))));
                                if ~isempty(MaskInd)
                                    SelectedMaskMap = app.XMapToolsData.MapData.MaskFile.Masks(MaskInd).MaskMap;
                                    MaskMap = zeros(size(SelectedMaskMap));
                                    PxSelected = find(SelectedMaskMap == app.XMapToolsData.TrainingSet.MaskNode(AddCode(2))-1);
                                    MaskMap(PxSelected) = ones(size(PxSelected));
                                    
                                    app.Data2Plot = app.Data2Plot.*MaskMap;
                                    
                                    app.Info_Phase = char(app.XMapToolsData.MapData.MaskFile.Masks(MaskInd).Names(app.XMapToolsData.TrainingSet.MaskNode(AddCode(2))));
                                end
                            end
                        end
                        
                        % Check if a submask is selected
                        if isequal(AddCode(1,1),11) && AddCode(1,3) > 1 && AddCode(1,4) > 0
                            if isequal(AddCode(4),1)
                                % Plot mask image
                            else
                                % Filter group
                                SelectedMaskMap = app.XMapToolsData.MapData.MaskFile.Masks(AddCode(1,2)).SubMask(AddCode(1,3)).MaskSelMaskMap;
                                MaskMap = zeros(size(SelectedMaskMap));
                                MaskInd = find(SelectedMaskMap == AddCode(1,4)-1);
                                MaskMap(MaskInd) = ones(size(MaskInd));
                                
                                app.Data2Plot = app.Data2Plot.*MaskMap;
                                if size(AddCode,1) > 1
                                    app.Info_Phase = 'Multiple selection';
                                else
                                    app.Info_Phase = char(app.XMapToolsData.MapData.MaskFile.Masks(AddCode(2)).SubMask(AddCode(3)).Names{AddCode(4)});
                                end
                            end
                        end
                        
                    end
                end
                
                
                %                 if isequal(size(AddCode,2),3) && isequal(MapCode(1),1) || isequal(size(AddCode,2),3) && isequal(MapCode(1),4)
                %                     % check if a mask is selected
                %                     if isequal(AddCode(1,1),11) && AddCode(1,3) > 1
                %
                %                     end
                %
                %                 end
                %
                
                if isequal(app.Options_DispNegativeValues.Value,0)
                    NegValues = find(app.Data2Plot < 0);
                    if length(NegValues)
                        app.Data2Plot(NegValues) = zeros(size(NegValues));
                    end
                end
                
                Data2PlotNonZero = app.Data2Plot(app.Data2Plot(:) ~= 0 & app.Data2Plot(:) < inf );
                
                if SingleLayerMode
                    DataMin = SingleLayerMin;
                    DataMax = SingleLayerMax;
                    
                    DataMinHist =  double(min(Data2PlotNonZero(:)));
                    DataMaxHist = double(max(Data2PlotNonZero(:)));
                else
                    DataMin = min(Data2PlotNonZero(:));
                    DataMax = max(Data2PlotNonZero(:));
                    if isempty(DataMin)
                        DataMin = 0;
                    end
                    if isempty(DataMax)
                        DataMax = 0;
                    end
                    DataMinHist =  double(DataMin);
                    DataMaxHist = double(DataMax);
                end
                
                % Check for Segment
                if isequal(size(AddCode,1),1) && isequal(AddCode(1,1:2),[13,1])
                    if length(AddCode) >= 4
                        if AddCode(4) > 0
                            if ~isempty(app.XMapToolsData.SegScheme.Data(AddCode(3)).ROI(AddCode(4)).Coordinates)
                                DataMin = app.XMapToolsData.SegScheme.Data(AddCode(3)).ROI(AddCode(4)).Coordinates(1);
                                DataMax = app.XMapToolsData.SegScheme.Data(AddCode(3)).ROI(AddCode(4)).Coordinates(2);
                            end
                            
                        end
                    end
                end
                
                % NOT NEEDED AFTER UPDATING PLOT FUNCTION
                
                IsROI = 0;
                if ~isempty(app.ROI_sampling)
                    if isvalid(app.ROI_sampling)
                        IsROI = 1;
                        ROI_Position = app.ROI_sampling.Position;
                        
                        app.SaveResultsMenu.Enable = 'on';
                        
                        switch app.ROI_sampling.Type
                            case 'images.roi.polyline'
                                ROI_Type = 'polyline';
                            case 'images.roi.circle'
                                ROI_Type = 'circle';
                                ROI_Center = app.ROI_sampling.Center;
                                ROI_Radius = app.ROI_sampling.Radius;
                            case 'images.roi.polygon'
                                ROI_Type = 'polygon';
                            case 'images.roi.rectangle'
                                ROI_Type = 'rectangle';
                                ROI_Angle = app.ROI_sampling.RotationAngle;
                                
                        end
                        
                    else
                        app.ROI_sampling = [];
                    end
                end
                
                if isempty(app.FigMain)
                    disp('Container for plot created');
                    TileContainer = tiledlayout(app.PanelFigMain,1,1,'Padding','compact');
                    app.FigMain = nexttile(TileContainer);
                    
                    imagesc(app.FigMain,app.Data2Plot);
                    
                    ApplySettingsPlotEngine(app);
                    
                    %                     tb = axtoolbar(app.FigMain,{'pan','zoomin','zoomout','restoreview'});
                    %
                    %                     btn = axtoolbarbtn(tb,'push');
                    %                     btn.Icon = '078-magic wand.png';
                    %                     btn.Tooltip = 'Auto contrast';
                    %                     btn.ButtonPushedFcn = @(varargin)Button_FigMain_AutoContrastPushed(app);
                    %
                    %                     app.FigMain.XTick = [];
                    %                     app.FigMain.YTick = [];
                    %
                else
                    % Update only the data to improve display performances
                    app.FigMain.Children(end).CData = app.Data2Plot;
                    
                    disableDefaultInteractivity(app.FigMain);
                    DeactivatePlotZoomPanOptions(app);
                end
                
                % NOT NEEDED AFTER UPDATING PLOT FUNCTION
                
                %                 if IsROI
                %                     switch ROI_Type
                %                         case 'polyline'
                %                             app.ROI_sampling = drawpolyline(app.FigMain,'Color',[0.57,0.00,0.69],'InteractionsAllowed','all','Position',ROI_Position);
                %                             app.ROI_sampling_Listener = addlistener(app.ROI_sampling, 'ROIMoved', @(varargin)Sampling_ROI_changed_line(app, app.ROI_sampling));
                %                             %Sampling_ROI_changed_line(app,app.ROI_sampling);
                %
                %                             app.SaveResultsMenu.Enable = 'on';
                %                         case 'circle'
                %                             app.ROI_sampling = drawcircle(app.FigMain,'Color',[0.57,0.00,0.69],'InteractionsAllowed','all','Center',ROI_Center,'Radius',ROI_Radius);
                %                             app.ROI_sampling_Listener = addlistener(app.ROI_sampling, 'ROIMoved', @(varargin)Sampling_ROI_changed_shape(app, app.ROI_sampling));
                %                             %Sampling_ROI_changed_shape(app,app.ROI_sampling);
                %
                %                             app.SaveResultsMenu.Enable = 'on';
                %
                %                         case 'polygon'
                %                             app.ROI_sampling = drawpolygon(app.FigMain,'Color',[0.57,0.00,0.69],'InteractionsAllowed','all','Position',ROI_Position);
                %                             app.ROI_sampling_Listener = addlistener(app.ROI_sampling, 'ROIMoved', @(varargin)Sampling_ROI_changed_shape(app, app.ROI_sampling));
                %                             %Sampling_ROI_changed_line(app,app.ROI_sampling);
                %
                %                             app.SaveResultsMenu.Enable = 'on';
                %
                %                         case 'rectangle'
                %                             app.ROI_sampling = drawrectangle(app.FigMain,'Color',[0.57,0.00,0.69],'Rotatable',1,'InteractionsAllowed','all','Position',ROI_Position,'RotationAngle',ROI_Angle);
                %                             app.ROI_sampling_Listener = addlistener(app.ROI_sampling, 'ROIMoved', @(varargin)Sampling_ROI_changed_strip(app, app.ROI_sampling));
                %                             %Sampling_ROI_changed_line(app,app.ROI_sampling);
                %
                %                             app.SaveResultsMenu.Enable = 'on';
                %                     end
                %                end
                
                %                 if ~ResetAxis
                %                     axis(app.FigMain,AxisTEMP);
                %                 end
                
                % Check for existing colormap (for images)
                ExistColorMap = 0;
                if isequal(MapCode(1),8)
                    if MapCode(3) > 0
                        ExistColorMap = 1;
                    end
                end
                if ~ExistColorMap
                    app.ActiveColorbar = colorbar(app.FigMain);
                    PlotMap_UpdateColormap(app,'Map');  % Update colormap (TEMP)
                else
                    c = colorbar(app.FigMain);
                    if length(MapCode) > 2
                        colormap(app.FigMain,app.XMapToolsData.MapData.Im.Data(MapCode(2)).CData(MapCode(3)).ColorMap);
                    end
                    app.ActiveColorbar = c;
                end
                app.EditField_LiveMin.Value = double(DataMin);
                if isequal(DataMax,255)
                    app.EditField_LiveMax.Value = 256;
                else
                    app.EditField_LiveMax.Value = double(DataMax);
                end
                
                % order changed (now caxis comes last)
                if DataMax > DataMin
                    caxis(app.FigMain,[DataMin,DataMax]);
                    drawnow
                else
                    caxis(app.FigMain,[DataMin-0.2*DataMin,DataMax+0.2*DataMax]); % +/- 20%
                end
                
                PlotMap_AddScaleBar(app); % Add a scalebar if the option is selected
                
                app.FigMain.View = [app.FieldAngleView.Value,90];
                
                UpdateLiveHistogram(app,Data2PlotNonZero,DataMinHist,DataMaxHist,DataMin,DataMax);
                
                app.LastData2Plot = app.Data2Plot;
                app.LastMedianFilter = 0;
                
                if ~isempty(app.ROI_sampling)
                    if isvalid(app.ROI_sampling)
                        switch app.ROI_sampling.Type
                            case 'images.roi.polyline'
                                Sampling_ROI_changed_line(app,app.ROI_sampling);
                            case {'images.roi.circle','images.roi.polygon'}
                                Sampling_ROI_changed_shape(app,app.ROI_sampling);
                            case 'images.roi.rectangle'
                                Sampling_ROI_changed_strip(app,app.ROI_sampling);
                        end
                        
                    end
                end
                
                % (1) Check if standards are selected
                if ~isempty(AddCode)
                    if isequal(AddCode(1),14)
                        Standards_PlotStdROI(app, AddCode(2));
                        return
                    end
                    
                    if isequal(AddCode(1),15)
                        return
                    end
                end
                
                UpdateInfoWindow(app);
                
                % try app.FigMain.Children(end).CData(1,1)=app.FigMain.Children(end).CData(1,1); catch ME, disp('Error code WhiteScreen-01'); end
                
                if app.Options_ApplyAutoContrast.Value
                    Button_FigMain_AutoContrastPushed(app);
                end
                
                %profile viewer
            end
            
        end
        
        function UpdateLiveHistogram(app,Data2PlotNonZero,DataMinHist,DataMaxHist,DataMin,DataMax)
            % DataMinHist and DataMaxHist must be double!
            
            [N,EDGES] = histcounts(Data2PlotNonZero);
            [Val,PosMax] = max(N);
            dPos = (EDGES(2)-EDGES(1)); %/2;
            PosMainPeak = EDGES(PosMax)+dPos;
            
            area(app.FigHistLive,EDGES(1:end-1)+(EDGES(2)-EDGES(1)),N,'FaceColor',[0.8 0.8 0.8],'LineWidth',1)
            dX = (DataMaxHist-DataMinHist)/50;
            if isequal(dX,0)
                dX = 1;
            end
            if DataMin > 0
                app.FigHistLive.XLim = [0-dX DataMaxHist+dX];
            else
                app.FigHistLive.XLim = [DataMinHist-dX DataMaxHist+dX];
            end
            toolbar = axtoolbar(app.FigHistLive);
            toolbar.Visible = 'off';
            app.FigHistLive.XTickMode = 'auto';
            app.FigHistLive.XTick = [];
            app.FigHistLive.YTickMode = 'auto';
            app.FigHistLive.YTick = [];
            app.FigHistLive.YScale = 'log';
            
            if PosMainPeak > DataMin + 0.03*(DataMax-DataMin)
                %app.EditField_LivePeak.Visible = 'on';
                %app.EditField_LivePeak.Value = num2str(PosMainPeak,'%3.4g');
                %app.SliderPeakHandle = xline(app.FigHistLive, double(PosMainPeak),'-','LineWidth',3,'Color',[0.57,0.00,0.69],'hittest','off');
                
                app.EditField_LivePosition.Value = PosMainPeak;
            else
                %app.SliderPeakHandle = [];
                %app.EditField_LivePeak.Visible = 'off';
                
                app.EditField_LivePosition.Value = mean([DataMinHist,DataMaxHist]);
            end
            
            %app.SliderMinHandle = xline(app.FigHistLive, double(DataMin),'r-','LineWidth',3);
            %app.SliderMaxHandle = xline(app.FigHistLive, double(DataMax),'r-','LineWidth',3);
            
            %app.SliderMinHandle.ButtonDownFcn = @app.dragObject;
            %app.SliderMaxHandle.ButtonDownFcn = @app.dragObject;
            
            app.hVerticalLines = [xline(app.FigHistLive, double(app.EditField_LivePosition.Value),'-','LineWidth',3,'Color',[0.57,0.00,0.69]),xline(app.FigHistLive, double(DataMin),'r-','LineWidth',3),xline(app.FigHistLive, double(DataMax),'r-','LineWidth',3)];
            set(app.hVerticalLines, 'hittest', 'off'); % Nils: it took me a while to figure this one out they need to be untouchable otherwise we get no values from the button down function
            app.hLineToDrag = [];
            
            removeToolbarExplorationButtons(app.FigHistLive);
            
        end
        
        function UpdateLimitsForMultiPlot(app)
            % Multiplot
            MultiMode = 0;
            if isequal(length(app.TreeData_Main.SelectedNodes),1)
                if isequal(app.TreeData_Main.SelectedNodes.NodeData(1),8)
                    MapCode = app.TreeData_Main.SelectedNodes.NodeData;
                    MultiMode = 1;
                    MultiType = app.XMapToolsData.MapData.Im.Types(MapCode(2));
                end
            end
            if MultiMode
                if isequal(MultiType,3)
                    for i = 1:length(app.XMapToolsData.MapData.Im.Data(MapCode(2)).CData)
                        app.XMapToolsData.MapData.Im.Data(MapCode(2)).CData(i).Lim(1) = app.hVerticalLines(2).Value;
                        app.XMapToolsData.MapData.Im.Data(MapCode(2)).CData(i).Lim(2) = app.hVerticalLines(3).Value;
                    end
                else
                    if isequal(MapCode(3),0)
                        PosMap = 1; % single map
                    else
                        PosMap = MapCode(3);
                    end
                    app.XMapToolsData.MapData.Im.Data(MapCode(2)).CData(PosMap).Lim(1) = app.hVerticalLines(2).Value;
                    app.XMapToolsData.MapData.Im.Data(MapCode(2)).CData(PosMap).Lim(2) = app.hVerticalLines(3).Value;
                end
            end
        end
        
        function UpdateInfoWindow(app)
            
            SelectedNodes = app.TreeData_Main.SelectedNodes;
            if isempty(SelectedNodes)
                app.MapInfo_TextArea.Value = sprintf('');
                return
            end
            if isequal(SelectedNodes.NodeData(1),6)
                app.MapInfo_TextArea.Value = sprintf('');
                return
            end
            
            % Update info window
            if isempty(app.ROI_sampling)
                app.TabGroup.SelectedTab  = app.InformationTab;
                cla(app.Sampling_Plot1,'reset')
                cla(app.Sampling_Plot2,'reset')
                app.Sampling_Plot1.Visible = 'off';
                app.Sampling_Plot2.Visible = 'off';
                app.Sampling_ExportButton.Enable = 'off';
            else
                if ~isvalid(app.ROI_sampling)
                    app.TabGroup.SelectedTab  = app.InformationTab;
                    cla(app.Sampling_Plot1,'reset')
                    cla(app.Sampling_Plot2,'reset')
                    app.Sampling_Plot1.Visible = 'off';
                    app.Sampling_Plot2.Visible = 'off';
                    app.Sampling_ExportButton.Enable = 'off';
                else
                    app.TabGroup.SelectedTab  = app.SamplingTab;
                end
            end
            
            Text2Disp = '';
            
            Text2Disp = [Text2Disp,'-----------------------------------------------\n'];
            Text2Disp = [Text2Disp,'Map type\t\t\t',app.Info_MapType,'\n'];
            Text2Disp = [Text2Disp,'Dataset\t\t\t',app.Info_Phase,'\n'];
            Text2Disp = [Text2Disp,'Quantity\t\t\t',app.Info_MapName,'\n'];
            Text2Disp = [Text2Disp,'-----------------------------------------------\n'];
            Text2Disp = [Text2Disp,'Min\t\t\t\t',num2str(min(app.Data2Plot(:))),'\n'];
            Text2Disp = [Text2Disp,'Max\t\t\t\t',num2str(max(app.Data2Plot(:))),'\n'];
            IdxPos = find(app.Data2Plot(:) > 0 & app.Data2Plot(:) ~= nan);
            Text2Disp = [Text2Disp,'-----------------------------------------------\n'];
            Text2Disp = [Text2Disp,'>> Considering data > 0 \n'];
            Text2Disp = [Text2Disp,'Nb\t\t\t\t',num2str(length(IdxPos)),'/',num2str(numel(app.Data2Plot(:))),' (',num2str(round(numel(IdxPos)/length(app.Data2Plot(:))*100*100)/100),'%%)\n'];
            Text2Disp = [Text2Disp,'Min\t\t\t\t',num2str(min(app.Data2Plot(IdxPos))),'\n'];
            Text2Disp = [Text2Disp,'Mean\t\t\t',num2str(mean(app.Data2Plot(IdxPos))),'\n'];
            Text2Disp = [Text2Disp,'Median\t\t\t',num2str(median(app.Data2Plot(IdxPos))),'\n'];
            Text2Disp = [Text2Disp,'Stdev\t\t\t',num2str(std(app.Data2Plot(IdxPos))),'\n'];
            %             if isequal(app.EditField_LivePositionVisible,'on')
            %                 Text2Disp = [Text2Disp,'Peak\t\t\t',num2str(app.EditField_LivePeak.Value),'\n'];
            %             else
            %                 Text2Disp = [Text2Disp,'Peak\t\t\t','not available','\n'];
            %             end
            Text2Disp = [Text2Disp,'-----------------------------------------------\n'];
            Text2Disp = [Text2Disp,'>> Color bar options \n'];
            Text2Disp = [Text2Disp,'Colormap\t\t',char(app.Options_ColormapDropDown.Value),'\n'];
            Text2Disp = [Text2Disp,'Resolution\t\t',num2str(app.Options_ColormapResEditField.Value),'\n'];
            if isequal(app.Options_LogColormap.Value,1)
                Text2Disp = [Text2Disp,'Scale\t\t\t','log','\n'];
            else
                Text2Disp = [Text2Disp,'Scale\t\t\t','linear','\n'];
            end
            WarningLower = 0;
            if app.Options_LowerCheckBox.Value
                Text2Disp = [Text2Disp,'Lower-filter\t\t',char(app.Options_LowerColor.Value),'\n'];
            else
                Text2Disp = [Text2Disp,'Lower-filter\t\t','none','\n'];
                WarningLower = 1;
            end
            WarningUpper = 0;
            if app.Options_UpperCheckBox.Value
                Text2Disp = [Text2Disp,'Upper-filter\t\t',char(app.Options_UpperColor.Value),'\n'];
            else
                Text2Disp = [Text2Disp,'Upper-filter\t\t','none','\n'];
                WarningUpper = 1;
            end
            Text2Disp = [Text2Disp,'-----------------------------------------------\n'];
            Text2Disp = [Text2Disp,'>> Considering plotted data only \n'];
            MinV = app.EditField_LiveMin.Value;
            MaxV = app.EditField_LiveMax.Value;
            DataSel = app.Data2Plot(find(app.Data2Plot(:) >= MinV & app.Data2Plot(:) <= MaxV));
            
            Text2Disp = [Text2Disp,'Nb\t\t\t\t',num2str(length(DataSel)),'/',num2str(numel(app.Data2Plot(:))),' (',num2str(round(numel(DataSel)/length(app.Data2Plot(:))*100*100)/100),'%%)\n'];
            Text2Disp = [Text2Disp,'Min\t\t\t\t',num2str(min(DataSel(:))),'\n'];
            Text2Disp = [Text2Disp,'Max\t\t\t\t',num2str(max(DataSel(:))),'\n'];
            Text2Disp = [Text2Disp,'Mean\t\t\t',num2str(mean(DataSel)),'\n'];
            Text2Disp = [Text2Disp,'Median\t\t\t',num2str(median(DataSel)),'\n'];
            Text2Disp = [Text2Disp,'Stdev\t\t\t',num2str(std(DataSel)),'\n'];
            Text2Disp = [Text2Disp,'-----------------------------------------------\n'];
            if WarningLower || WarningUpper
                Text2Disp = [Text2Disp,'>> Warning: pixel saturation (nb. outside) \n'];
                if WarningLower
                    LowPx = find(app.Data2Plot(:) < MinV);
                    Text2Disp = [Text2Disp,'Lower-limit\t\t',num2str(numel(LowPx)),' (',num2str(round(numel(LowPx)/length(DataSel)*100*100)/100),'%%)\n'];
                end
                if WarningUpper
                    HighPx = find(app.Data2Plot(:) > MaxV);
                    Text2Disp = [Text2Disp,'Upper-limit\t\t',num2str(numel(HighPx)),' (',num2str(round(numel(HighPx)/length(DataSel)*100*100)/100),'%%)\n'];
                end
                Text2Disp = [Text2Disp,'-----------------------------------------------\n'];
            end
            
            
            app.MapInfo_TextArea.Value = sprintf(Text2Disp);
            
        end
        
        function UpdateInfoWindowForClassification(app)
            
            NodeData = app.TreeData_Additional.SelectedNodes.NodeData;
            
            if isequal(NodeData(3),0)
                Info = app.XMapToolsData.MapData.MaskFile.Masks(NodeData(2)).Info;
            else
                Info = app.XMapToolsData.MapData.MaskFile.Masks(NodeData(2)).SubMask(NodeData(3)).Info;
            end
            
            if ~isempty(Info.Algorithm)
                Text2Disp = '';
                
                Text2Disp = [Text2Disp,'-----------------------------------------------\n'];
                Text2Disp = [Text2Disp,'XMapTools classification\n'];
                Text2Disp = [Text2Disp,'-----------------------------------------------\n'];
                Text2Disp = [Text2Disp,'Algorithm: ',Info.Algorithm,'\n'];
                Text2Disp = [Text2Disp,'Data Scaling: ',Info.Scaling,'\n'];
                Text2Disp = [Text2Disp,'Reproducibility: ',Info.Reproducibility,'\n'];
                Text2Disp = [Text2Disp,'-----------------------------------------------\n'];
                
                Text2Disp = [Text2Disp,'Results: \tMode \tName \n'];
                for i = 1:length(Info.Classes)
                    Text2Disp = [Text2Disp,'  -  (',num2str(i),')\t',num2str(round(Info.Modes(i)*100,2)),'%%\t',char(Info.Classes{i}),'\n'];
                end
                
                Text2Disp = [Text2Disp,'-----------------------------------------------\n'];
                Text2Disp = [Text2Disp,'Classification accuracy: ',num2str(Info.Accuracy*100),'%%\n'];
                
                Text2Disp = [Text2Disp,'Classes: \tPrecision\tRecall \tF1score \tName \n'];
                for i = 1:length(Info.Classes)
                    Text2Disp = [Text2Disp,'  -  (',num2str(i),')\t'];
                    if Info.Precision(i)*100 < 10
                        Text2Disp = [Text2Disp,sprintf('%.3f',Info.Precision(i)*100),'\t'];
                    elseif Info.Precision(i)*100 < 100
                        Text2Disp = [Text2Disp,sprintf('%.2f',Info.Precision(i)*100),'\t'];
                    else
                        Text2Disp = [Text2Disp,sprintf('%.1f',Info.Precision(i)*100),'\t'];
                    end
                    if Info.Recall(i)*100 < 10
                        Text2Disp = [Text2Disp,sprintf('%.3f',Info.Recall(i)*100),'\t'];
                    elseif Info.Recall(i)*100 < 100
                        Text2Disp = [Text2Disp,sprintf('%.2f',Info.Recall(i)*100),'\t'];
                    else
                        Text2Disp = [Text2Disp,sprintf('%.1f',Info.Recall(i)*100),'\t'];
                    end
                    if Info.F1Score(i)*100 < 10
                        Text2Disp = [Text2Disp,sprintf('%.3f',Info.F1Score(i)*100),'\t'];
                    elseif Info.F1Score(i)*100 < 100
                        Text2Disp = [Text2Disp,sprintf('%.2f',Info.F1Score(i)*100),'\t'];
                    else
                        Text2Disp = [Text2Disp,sprintf('%.1f',Info.F1Score(i)*100),'\t'];
                    end
                    Text2Disp = [Text2Disp,char(Info.Classes{i}),'\n'];
                end
                Text2Disp = [Text2Disp,'-----------------------------------------------\n'];
                Text2Disp = [Text2Disp,'Selected Data for classification: \n'];
                for i = 1:length(Info.SelectedData)
                    Text2Disp = [Text2Disp,'  -  x',num2str(i),'\t',char(Info.SelectedData{i}),'\n'];
                end
                
                app.MapInfo_TextArea.Value = sprintf(Text2Disp);
                
                
            else
                Text2Disp = '';
                Text2Disp = [Text2Disp,'-----------------------------------------------\n'];
                Text2Disp = [Text2Disp,'No information available for this classification\n'];
                Text2Disp = [Text2Disp,'-----------------------------------------------\n'];
                Text2Disp = [Text2Disp,'This classification was done with a previous version of XMapTools (pre 4.3). You can perform a new classification with the same training set to get more detailed information about the classification.\n'];
                app.MapInfo_TextArea.Value = sprintf(Text2Disp);
            end
            
        end
        
        function [Colors4Mask] = UpdateColorsMasks(app,Names)
            for i = 1:length(Names)
                WhereMin = find(ismember(app.MineralColorData.Names,Names{i}));
                if ~isempty(WhereMin)
                    Colors4Mask(i,:) = app.MineralColorData.RGB(WhereMin,:);
                else
                    Colors4Mask(i,:) = [0,0,0];
                end
            end
            
            if isequal(sum(Colors4Mask(:)),0)
                
                ColorMapSelectedMenu = app.Options_ColormapDropDown.Value;
                
                SelColorMap = find(ismember(app.Options_ColormapDropDown.Items,ColorMapSelectedMenu));
                ColorData = app.ColorMaps(SelColorMap).Code;
                
                Resolution = length(Names)-1;
                
                Xi = 1:Resolution;
                Step = (Resolution-1)/(size(ColorData,1)-1);
                X = 1:Step:Resolution;
                
                Colors4Mask = zeros(length(Xi),size(ColorData,2));
                for i = 1:size(ColorData,2)
                    Colors4Mask(:,i) = interp1(X',ColorData(:,i),Xi);
                end
                
                Colors4Mask = [0,0,0;Colors4Mask];
                
            end
            
        end
        
        function PlotMap_AdjustMinMax(app)
            
            %Min = app.SliderMinHandle.Value;
            %Max = app.SliderMaxHandle.Value;
            
            GreenBar = app.hVerticalLines(1).Value;
            
            Min = app.hVerticalLines(2).Value;
            Max = app.hVerticalLines(3).Value;
            
            if Min >= Max
                return
            end
            
            caxis(app.FigMain,[Min,Max])
            
            app.EditField_LiveMin.Value = Min;
            app.EditField_LiveMax.Value = Max;
            app.EditField_LivePosition.Value = GreenBar;
            
            UpdateLimitsForMultiPlot(app);
            
            % Sampling option
            if ~isempty(app.ROI_sampling)
                if isvalid(app.ROI_sampling)
                    switch app.ROI_sampling.Type
                        case 'images.roi.polyline'
                            Sampling_ROI_changed_line(app,app.ROI_sampling);
                        case {'images.roi.circle','images.roi.polygon'}
                            Sampling_ROI_changed_shape(app,app.ROI_sampling);
                        case 'images.roi.rectangle'
                            Sampling_ROI_changed_strip(app,app.ROI_sampling);
                    end
                end
            end
            UpdateInfoWindow(app)
            
            % try app.FigMain.Children(end).CData(1,1)=app.FigMain.Children(end).CData(1,1); catch ME, disp('Error code WhiteScreen-01'); end
        end
        
        function ImageData = PlotMap_ExtractPlottedImage(app)
            lesInd = app.FigMain.Children;
            for i=1:numel(lesInd)
                if isequal(lesInd(i).Type,'image')
                    ImageData = lesInd(i).CData;
                    break
                else
                    ImageData = [];
                end
            end
        end
        
        
        
        function PlotMap_UpdateColormap(app,mode,res)
            % This function probably needs to be re-organized properly
            
            if isequal(mode,'Map')
                Resolution = app.Options_ColormapResEditField.Value;
                MapOptions = 1;
            elseif isequal(mode,'Mask')
                Resolution = res;
                MapOptions = 0;
            end
            
            ColorMapSelectedMenu = app.Options_ColormapDropDown.Value;
            
            %             % Special colormap mode
            %             SelectedNodes = app.TreeData_Main.SelectedNodes;
            %             if ~isempty(SelectedNodes)
            %                 NodeData = app.TreeData_Main.SelectedNodes.NodeData;
            %                 if isequal(NodeData(1),8)
            %                     if isequal(NodeData(1),8) && NodeData(3) > 0
            %                         ColorMapSelectedMenu = app.XMapToolsData.MapData.Im.Data(NodeData(2)).CData(NodeData(3)).ColorMap;
            %                     end
            %                 end
            %             end
            
            
            SelColorMap = find(ismember(app.Options_ColormapDropDown.Items,ColorMapSelectedMenu));
            ColorData = app.ColorMaps(SelColorMap).Code;
            
            if isequal(app.Options_Colorbar_Inverse.Value,0)
                ColorData = flip(ColorData);
            end
            
            if Resolution > 1
                Xi = 1:Resolution;
                Step = (Resolution-1)/(size(ColorData,1)-1);
                X = 1:Step:Resolution;
                
                ColorMap = zeros(length(Xi),size(ColorData,2));
                for i = 1:size(ColorData,2)
                    ColorMap(:,i) = interp1(X',ColorData(:,i),Xi);
                end
            else
                ColorMap(1,:) = ColorData(1,:);
            end
            
            
            if MapOptions
                % upper & lower
                if app.Options_LowerCheckBox.Value
                    if isequal(app.Options_LowerColor.Value,'Black')
                        ColorMap(1,:) = [0,0,0];
                    else
                        ColorMap(1,:) = [1,1,1];
                    end
                end
                if app.Options_UpperCheckBox.Value
                    if isequal(app.Options_UpperColor.Value,'Black')
                        ColorMap(end,:) = [0,0,0];
                    else
                        ColorMap(end,:) = [1,1,1];
                    end
                end
                
                %log
                if app.Options_LogColormap.Value
                    app.FigMain.ColorScale = 'log';
                else
                    app.FigMain.ColorScale = 'linear';
                end
            end
            
            % update
            colormap(app.FigMain,ColorMap);
            
            app.ColorMapValues = ColorMap;
            
            if isequal(mode,'Map')
                app.ColorMapValues_noMask = ColorMap;
            end
            
            % Special colormap mode
            SelectedNodes = app.TreeData_Main.SelectedNodes;
            if ~isempty(SelectedNodes)
                NodeData = app.TreeData_Main.SelectedNodes.NodeData;
                if isequal(NodeData(1),8)
                    if isequal(NodeData(1),8) && NodeData(3) > 0
                        app.XMapToolsData.MapData.Im.Data(NodeData(2)).CData(NodeData(3)).ColorMap = ColorMap;
                    end
                end
            end
            
            % display the colormap (added by Nils Gies)
            imagesc(app.ColorMapPreview,[(1:app.Options_ColormapResEditField.Value);(1:app.Options_ColormapResEditField.Value)])
            colormap(app.ColorMapPreview,app.ColorMapValues)
            app.ColorMapPreview.XLim=[0 app.Options_ColormapResEditField.Value];
            app.ColorMapPreview.YLim=[0.5 2.5];
            
            
        end
        
        function PlotMap_AddScaleBar(app)
            
            ChildrenText = findobj(app.FigMain.Children,'Type','text');
            ChildrenLine = findobj(app.FigMain.Children,'Type','line');
            
            if isequal(app.DisplayScaleBar.Value,1)
                
                Ax = axis(app.FigMain);
                Xpx = Ax(2)-Ax(1);
                Ypx = Ax(4)-Ax(3);
                
                if Xpx > 600 && Ypx > 600
                    Dist = round(max(Xpx,Ypx)/6,-2);
                    dT = 15;
                elseif Xpx > 400 && Ypx > 400
                    Dist = round(max(Xpx,Ypx)/7,-2);
                    dT = 12;
                else
                    Dist = round(Xpx/7,-1);
                    dT = 10;
                end
                PosY = Ax(end)-0.02*Ax(end);
                dX = 10;
                
                switch app.ColorScaleBar.Value
                    case 'White'
                        ColorCode = '-w';
                        ColorValues = [1,1,1];
                    case 'Black'
                        ColorCode = 'k';
                        ColorValues = [0,0,0];
                end
                
                
                hold(app.FigMain, 'on')
                switch app.FieldAngleView.Value
                    
                    case 0
                        if ~isempty(ChildrenLine)
                            ChildrenLine.XData = [Ax(1)+dX Ax(1)+Dist+dX];
                            ChildrenLine.YData = [PosY PosY];
                            ChildrenLine.Color = ColorValues;
                        else
                            plot(app.FigMain,[Ax(1)+dX Ax(1)+Dist+dX],[PosY PosY],ColorCode, 'LineWidth', 2)
                        end
                        
                        if ~isempty(ChildrenText)
                            ChildrenText.Position = [Ax(1)+dX+Dist/2, PosY-dT, 0];
                            ChildrenText.String = [num2str(round(Dist*app.ResolutionField.Value)),' µm'];
                            ChildrenText.Color = ColorValues;
                        else
                            text(app.FigMain,Ax(1)+dX+Dist/2,PosY-dT, [num2str(round(Dist*app.ResolutionField.Value)),' µm'], 'HorizontalAlignment','center','Color',app.ColorScaleBar.Value,'FontSize',12);
                        end
                        
                    case 90
                        if ~isempty(ChildrenLine)
                            ChildrenLine.XData = [Ax(2)-dX Ax(2)-dX];
                            ChildrenLine.YData = [Ax(4)-dX Ax(4)-dX-Dist];
                            ChildrenLine.Color = ColorValues;
                        else
                            plot(app.FigMain,[Ax(2)-dX Ax(2)-dX],[Ax(4)-dX Ax(4)-dX-Dist],ColorCode, 'LineWidth', 2)
                        end
                        
                        if ~isempty(ChildrenText)
                            ChildrenText.Position = [Ax(2)-dX-dT, Ax(4)-dX-Dist/2, 0];
                            ChildrenText.String = [num2str(round(Dist*app.ResolutionField.Value)),' µm'];
                            ChildrenText.Color = ColorValues;
                        else
                            text(app.FigMain,Ax(2)-dX-dT,Ax(4)-dX-Dist/2, [num2str(round(Dist*app.ResolutionField.Value)),' µm'], 'HorizontalAlignment','center','Color',app.ColorScaleBar.Value,'FontSize',12);
                        end
                        
                    case 180
                        if ~isempty(ChildrenLine)
                            ChildrenLine.XData = [Ax(2)-dX Ax(2)-Dist-dX];
                            ChildrenLine.YData = [Ax(3)+dX Ax(3)+dX];
                            ChildrenLine.Color = ColorValues;
                        else
                            plot(app.FigMain,[Ax(2)-dX Ax(2)-Dist-dX],[Ax(3)+dX Ax(3)+dX],ColorCode, 'LineWidth', 2)
                        end
                        
                        if ~isempty(ChildrenText)
                            ChildrenText.Position = [Ax(2)-Dist/2-dX, Ax(3)+dX+dT, 0];
                            ChildrenText.String = [num2str(round(Dist*app.ResolutionField.Value)),' µm'];
                            ChildrenText.Color = ColorValues;
                        else
                            text(app.FigMain,Ax(2)-Dist/2-dX,Ax(3)+dX+dT, [num2str(round(Dist*app.ResolutionField.Value)),' µm'], 'HorizontalAlignment','center','Color',app.ColorScaleBar.Value,'FontSize',12);
                        end
                        
                    case 270
                        if ~isempty(ChildrenLine)
                            ChildrenLine.XData = [Ax(1)+dX Ax(1)+dX];
                            ChildrenLine.YData = [Ax(3)+dX Ax(3)+Dist+dX];
                            ChildrenLine.Color = ColorValues;
                        else
                            plot(app.FigMain,[Ax(1)+dX Ax(1)+dX],[Ax(3)+dX Ax(3)+Dist+dX],ColorCode, 'LineWidth', 2)
                        end
                        
                        if ~isempty(ChildrenText)
                            ChildrenText.Position = [Ax(1)+dX+dT, Ax(3)+dX+Dist/2, 0];
                            ChildrenText.String = [num2str(round(Dist*app.ResolutionField.Value)),' µm'];
                            ChildrenText.Color = ColorValues;
                        else
                            text(app.FigMain,Ax(1)+dX+dT,Ax(3)+dX+Dist/2, [num2str(round(Dist*app.ResolutionField.Value)),' µm'], 'HorizontalAlignment','center','Color',app.ColorScaleBar.Value,'FontSize',12);
                        end
                end
                hold(app.FigMain, 'off');
            else
                
                % Eliminate label
                if ~isempty(ChildrenLine)
                    delete(ChildrenLine);
                end
                if ~isempty(ChildrenText)
                    delete(ChildrenText);
                end
                
            end
        end
        
        function dragObject(app,hObject,~)
            
            % this function is depreciated (see new implementation)
            
            Min = app.SliderMinHandle.Value;
            Max = app.SliderMaxHandle.Value;
            
            dMin = abs(hObject.Value-Min);
            dMax = abs(hObject.Value-Max);
            
            Xlim = app.FigHistLive.XLim;
            
            if dMin > dMax % we move dMax
                Limit = [Min Xlim(2)];
                WhichOne = 2;  % dMax
            else
                Limit = [Xlim(1) Max];
                WhichOne = 1;  % dMin
            end
            
            %             if dMin < Xlim(1)
            %                 Limit = [Xlim(1) Max];
            %             end
            %
            %             if dMax > Xlim(2)
            %                 Limit = [Min Xlim(2)];
            %             end
            
            
            app.Hist_dragging = hObject;
            app.Hist_dragging_limits = Limit;
            app.Hist_dragging_which = WhichOne;
            
            % Value applied in: XMapTools_GUIWindowButtonUp(app, event)
            
        end
        
        
        
        
        function ROI_MovingShape(app,~)
            % update the position of the selected ROI
            NodeData = app.TreeData_Additional.SelectedNodes.NodeData;
            Method = char(app.XMapToolsData.TrainingSet.Data(NodeData(2)).ROI(NodeData(3)).Types(NodeData(4)));
            
            if isempty(Method)
                return
            end
            
            switch Method
                case 'Rectangle ROI'
                    app.XMapToolsData.TrainingSet.Data(NodeData(2)).ROI(NodeData(3)).Data(NodeData(4)).Coordinates = app.SelectedROI.Position;
                case 'Polygon ROI'
                    app.XMapToolsData.TrainingSet.Data(NodeData(2)).ROI(NodeData(3)).Data(NodeData(4)).Coordinates = app.SelectedROI.Position;
                case 'Ellipse ROI'
                    app.XMapToolsData.TrainingSet.Data(NodeData(2)).ROI(NodeData(3)).Data(NodeData(4)).Coordinates = [app.SelectedROI.Center,app.SelectedROI.SemiAxes,app.SelectedROI.RotationAngle];
                case 'Circle ROI'
                    app.XMapToolsData.TrainingSet.Data(NodeData(2)).ROI(NodeData(3)).Data(NodeData(4)).Coordinates = [app.SelectedROI.Center,app.SelectedROI.Radius];
            end
        end
        
        function ROI_DeletingShape(app,~)
            
            NodeData = app.TreeData_Additional.SelectedNodes.NodeData;
            
            app.XMapToolsData.TrainingSet.Data(NodeData(2)).ROI(NodeData(3)).Types(NodeData(4)) = [];
            app.XMapToolsData.TrainingSet.Data(NodeData(2)).ROI(NodeData(3)).Data(NodeData(4)) = [];
            
            app.TreeData_Additional.SelectedNodes.delete;
            app.TreeData_Additional.SelectedNodes = [];
            RebuildGUI(app,'TrainingSet'); % update indexes
            
            ROI_DeleteROI(app);
        end
        
        
        function app = PlotMaskFile(app,MaskFileIdx)
            MaskMap = app.XMapToolsData.MapData.MaskFile.Masks(MaskFileIdx).MaskMap;
            MaskProbability = app.XMapToolsData.MapData.MaskFile.Masks(MaskFileIdx).MaskProbability;
            PhaseNames = app.XMapToolsData.MapData.MaskFile.Masks(MaskFileIdx).Names;
            
            NbMask = numel(PhaseNames)-1;
            
            if isequal(app.Classify_FilterLowProbPixels.Value,1)
                if ~isempty(MaskProbability)
                    for i = 1:size(MaskProbability,3)
                        %keyboard
                        WhereNotOne = find(MaskProbability(:,:,i) > 0 & MaskProbability(:,:,i) < app.Classify_FilterProbValue.Value & MaskMap == i);
                        if ~isempty(WhereNotOne)
                            MaskMap(WhereNotOne) = zeros(size(WhereNotOne));
                        end
                    end
                    NbZeros = find(MaskMap == 0);
                    app.Classify_MixingPixelsPer.Text = ['Mix. pixels: ',num2str(length(NbZeros)/numel(MaskMap)*100),' %'];
                else
                    app.Classify_MixingPixelsPer.Text = 'No probability maps';
                end
            else
                app.Classify_MixingPixelsPer.Text = '';
            end
            
            app.FigMain.Children(end).CData = MaskMap;
            
            colormap(app.FigMain,app.XMapToolsData.MapData.MaskFile.Masks(MaskFileIdx).Colors);
            
            if 0
                PlotMap_UpdateColormap(app,'Mask',NbMask);  % Update colormap (TEMP)
                
                ColormapTemp = colormap(app.FigMain);
                ColormapTemp= [0,0,0;ColormapTemp];
                colormap(app.FigMain,ColormapTemp);
            end
            
            caxis(app.FigMain,[0 NbMask+1])
            
            app.ActiveColorbar.TickLabelsMode = 'manual';
            app.ActiveColorbar.Ticks = [0.5:1:NbMask+2];
            app.ActiveColorbar.TickLabels = PhaseNames(1:end);
                       
            app.FigMain.ColorScale = 'linear';
            
            UpdateInfoWindowForClassification(app)
            
            % try app.FigMain.Children(end).CData(1,1)=app.FigMain.Children(end).CData(1,1); catch ME, disp('Error code WhiteScreen-01'); end
        end
        
        function app = PlotSubMaskFile(app,SelectedAdditional)
            
            MaskMap = app.XMapToolsData.MapData.MaskFile.Masks(SelectedAdditional(2)).SubMask(SelectedAdditional(3)).MaskSelMaskMap;
            MaskProbability = app.XMapToolsData.MapData.MaskFile.Masks(SelectedAdditional(2)).SubMask(SelectedAdditional(3)).MaskProbability;
            PhaseNames = app.XMapToolsData.MapData.MaskFile.Masks(SelectedAdditional(2)).SubMask(SelectedAdditional(3)).Names;
            
            NbMask = numel(PhaseNames)-1;
            
            if isequal(app.Classify_FilterLowProbPixels.Value,1)
                if ~isempty(MaskProbability)
                    for i = 1:size(MaskProbability,3)
                        %keyboard
                        WhereNotOne = find(MaskProbability(:,:,i) > 0 & MaskProbability(:,:,i) < app.Classify_FilterProbValue.Value & MaskMap == i);
                        if ~isempty(WhereNotOne)
                            MaskMap(WhereNotOne) = zeros(size(WhereNotOne));
                        end
                    end
                    NbZeros = find(MaskMap == 0);
                    app.Classify_MixingPixelsPer.Text = ['Mix. pixels: ',num2str(length(NbZeros)/numel(MaskMap)*100),' %'];
                else
                    app.Classify_MixingPixelsPer.Text = 'No probability maps';
                end
            else
                app.Classify_MixingPixelsPer.Text = '';
            end
            
            %imagesc(app.FigMain,MaskMap)
            
            %app.FigMain.XTick = [];
            %app.FigMain.YTick = [];
            %axis(app.FigMain,'image');
            
            app.FigMain.Children(end).CData = MaskMap;
            
            %app.ActiveColorbar = colorbar(app.FigMain);
            PlotMap_UpdateColormap(app,'Mask',NbMask);  % Update colormap (TEMP)
            
            ColormapTemp = colormap(app.FigMain);
            ColormapTemp= [0,0,0;ColormapTemp];
            colormap(app.FigMain,ColormapTemp);
            
            caxis(app.FigMain,[0 NbMask+1])
            
            app.ActiveColorbar.TickLabelsMode = 'manual';
            app.ActiveColorbar.Ticks = [0.5:1:NbMask+2];
            app.ActiveColorbar.TickLabels = PhaseNames(1:end);
            
            app.FigMain.ColorScale = 'linear';
            
            UpdateInfoWindowForClassification(app)
            
            % try app.FigMain.Children(end).CData(1,1)=app.FigMain.Children(end).CData(1,1); catch ME, disp('Error code WhiteScreen-01'); end
        end
        
        function DispCode = Classify_GenerateDispCode(app,ElNames)
            DispCode = '';
            for i = 1:numel(ElNames)
                DispCode = [DispCode,char(ElNames{i}),','];
            end
            DispCode = DispCode(1:end-1);
        end
        
        
        function Modes_ROI_changed_shape(app, ~)
            
            NodeData = app.TreeData_Additional.SelectedNodes.NodeData;
            
            MaskId = NodeData(2);
            
            MaskMap = app.XMapToolsData.MapData.MaskFile.Masks(MaskId).MaskMap;
            Names = app.XMapToolsData.MapData.MaskFile.Masks(MaskId).Names(2:end);
            Colors = app.XMapToolsData.MapData.MaskFile.Masks(MaskId).Colors;
            
            % Extract data
            mask = createMask(app.ROI_Modes);
            
            Ind = find(mask);
            MaskSel = MaskMap(Ind);
            
            Counts = zeros(size(Names));
            for i = 1:length(Counts)
                Counts(i) = length(find(MaskSel == i));
            end
            
            ProPer = Counts./sum(Counts)*100;
            
            DataTable = {};
            for i = 1:length(Counts)
                DataTable(i,:) = {Names{i},ProPer(i),Counts(i)};
            end
            
            DataTable(end+1,:) = {' ',' ',' '};
            % Add submasks
            for i = 1:length(app.XMapToolsData.MapData.MaskFile.Masks(MaskId).SubMask)
                if ~isempty(app.XMapToolsData.MapData.MaskFile.Masks(MaskId).SubMask(i).Names)
                    CountsSM = zeros(1,length(app.XMapToolsData.MapData.MaskFile.Masks(MaskId).SubMask(i).Names)-1);
                    for j = 1:length(CountsSM)
                        CountsSM(j) = length(find(app.XMapToolsData.MapData.MaskFile.Masks(MaskId).SubMask(i).MaskSelMaskMap(Ind) == j));
                    end
                    ProPerSM = CountsSM./sum(CountsSM)*100;
                    for j = 1:length(CountsSM)
                        DataTable(end+1,:) = {app.XMapToolsData.MapData.MaskFile.Masks(MaskId).SubMask(i).Names{j+1},ProPerSM(j),CountsSM(j)};
                    end
                    DataTable(end+1,:) = {' ',' ',' '};
                end
            end
            
            app.CompViewer_Label.Text = 'Modes';
            
            app.CompViewer_UITable.Data = DataTable;
            app.CompViewer_UITable.ColumnName = {'Mask','Mode (surf%)','Nb pixels'};
            app.CompViewer_DensityMenu.Visible = 'off';
            
            [Vals,Indx] = sort(ProPer);
            pie(app.CompViewer_PlotTable,Vals,Names(Indx));
            
            %pie(app.CompViewer_PlotTable,ProPer,Names);
            
            app.CompViewer_PlotTable.Colormap = Colors(Indx+1,:);
            
            
            app.CompViewer_Button_Copy.Enable = 'on';
            app.CompViewer_Button_Save.Enable = 'on';
            app.CompViewer_Button_DeleteROI.Enable = 'on';
            
            app.TabGroup.SelectedTab = app.CompositionTab;
            
        end
        
        
        
        
        function ContextMenu_MainTree_ClearAllPushed(app, ~)
            
            NodeData = app.TreeData_Main.SelectedNodes.NodeData;
            
            switch NodeData(1)
                case 1
                    
                    app.XMapToolsData.MapData.It.Names = {};
                    app.XMapToolsData.MapData.It.Types = [];
                    app.XMapToolsData.MapData.It.ElInd = [];
                    
                    app.XMapToolsData.MapData.It.Data = [];
                    app.XMapToolsData.MapData.It.Data(1).Maps = [];
                    
                    app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
                    app.WaitBar.Message = 'Updating data';
                    while ~isempty(app.Node_It.Children)
                        app.Node_It.Children(1).delete;
                    end
                    close(app.WaitBar);
                    
                case 2
                    app.XMapToolsData.MapData.Qt.Names = {};
                    app.XMapToolsData.MapData.Qt.IsOxide = [];
                    app.XMapToolsData.MapData.Qt.MaskFile = {};
                    app.XMapToolsData.MapData.Qt.NbCalibPoints = [];
                    
                    app.XMapToolsData.MapData.Qt.Data = [];
                    app.XMapToolsData.MapData.Qt.Data(1).ElNames = {};
                    app.XMapToolsData.MapData.Qt.Data(1).ElInd = [];
                    app.XMapToolsData.MapData.Qt.Data(1).StdData = [];
                    app.XMapToolsData.MapData.Qt.Data(1).CData(1).Map = [];
                    
                    app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
                    app.WaitBar.Message = 'Updating data';
                    while ~isempty(app.Node_Qt.Children)
                        app.Node_Qt.Children(1).delete;
                    end
                    close(app.WaitBar);
                    
                case 3
                    app.XMapToolsData.MapData.Me.Names = {};
                    app.XMapToolsData.MapData.Me.IsOxide = [];
                    app.XMapToolsData.MapData.Me.MaskFile = {};
                    app.XMapToolsData.MapData.Me.NbCalibPoints = [];
                    
                    app.XMapToolsData.MapData.Me.Data = [];
                    app.XMapToolsData.MapData.Me.Data(1).ElNames = {};
                    app.XMapToolsData.MapData.Me.Data(1).ElInd = [];
                    app.XMapToolsData.MapData.Me.Data(1).CData(1).Map = [];
                    
                    app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
                    app.WaitBar.Message = 'Updating data';
                    while ~isempty(app.Node_Me.Children)
                        app.Node_Me.Children(1).delete;
                    end
                    close(app.WaitBar);
                    
                case 4
                    app.XMapToolsData.MapData.Re.Names = {};
                    app.XMapToolsData.MapData.Re.Types = [];
                    app.XMapToolsData.MapData.Re.Coord = [];
                    
                    app.XMapToolsData.MapData.Re.Data = [];
                    app.XMapToolsData.MapData.Re.Data(1).Labels = {};
                    app.XMapToolsData.MapData.Re.Data(1).CData(1).Map = [];
                    
                    app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
                    app.WaitBar.Message = 'Updating data';
                    while ~isempty(app.Node_Re.Children)
                        app.Node_Re.Children(1).delete;
                    end
                    close(app.WaitBar);
                    
                case 5
                    app.XMapToolsData.MapData.Ot.Names = {};
                    app.XMapToolsData.MapData.Ot.Types = [];
                    
                    app.XMapToolsData.MapData.Ot.Data = [];
                    app.XMapToolsData.MapData.Ot.Data(1).Map = [];
                    
                    app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
                    app.WaitBar.Message = 'Updating data';
                    while ~isempty(app.Node_Ot.Children)
                        app.Node_Ot.Children(1).delete;
                    end
                    close(app.WaitBar);
                    
                case 6
                    app.XMapToolsData.MapData.Ct.Names = {};
                    app.XMapToolsData.MapData.Ct.Types = [];
                    
                    app.XMapToolsData.MapData.Ct.Data = [];
                    app.XMapToolsData.MapData.Ct.Data(1).Map = [];
                    
                    app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
                    app.WaitBar.Message = 'Eliminating data (this action cake take up to several minutes for large CT dasets)';
                    while ~isempty(app.Node_Ct.Children)
                        app.Node_Ct.Children(1).delete;
                    end
                    close(app.WaitBar);
                    
                    
                    %                     app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
                    %                     app.WaitBar.Message = 'Eliminating tree data (this action cake take up to several minutes for CT data)';
                    %
                    %                     a = app.Node_Ct.Children;  % CT-data
                    %                     delete(a)
                    %
                    %                     %a.delete;
                    %
                    %                     % Add MapData Ct
                    %                     if numel(app.XMapToolsData.MapData.Ct.Names) > 1
                    %                         app.Node_Ct.ContextMenu = app.ContextMenu_MainTree_C;
                    %                         for i = 1:numel(app.XMapToolsData.MapData.Ct.Names)
                    %                             p = uitreenode(app.Node_Ct,'Text',char(app.XMapToolsData.MapData.Ct.Names{i}),'NodeData',[6,i]);
                    %                         end
                    %                     end
                    %
                    %                     close(app.WaitBar);
                    %                     return
                    
                case 7
                    app.XMapToolsData.MapData.ROI.Names = {};
                    app.XMapToolsData.MapData.ROI.Types = [];
                    
                    app.XMapToolsData.MapData.ROI.Data = [];
                    app.XMapToolsData.MapData.ROI.Data(1).Names = {};
                    app.XMapToolsData.MapData.ROI.Data(1).ROI = [];
                    
                    app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
                    app.WaitBar.Message = 'Updating data';
                    while ~isempty(app.Node_ROI.Children)
                        app.Node_ROI.Children(1).delete;
                    end
                    close(app.WaitBar);
                    
                    %                     app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
                    %                     app.WaitBar.Message = 'Updating data';
                    %
                    %                     a = app.Node_ROI.Children;  % ROI
                    %                     a.delete;
                    %
                    %                     % Add ROI
                    %                     if numel(app.XMapToolsData.MapData.ROI.Names) > 0
                    %                         app.Node_ROI.ContextMenu = app.ContextMenu_MainTree_C;
                    %                         for i = 1:numel(app.XMapToolsData.MapData.ROI.Names)
                    %                             p = uitreenode(app.Node_ROI,'Text',char(app.XMapToolsData.MapData.ROI.Names{i}),'NodeData',[7,i]);
                    %                         end
                    %                     end
                    %
                    %
                    %                     close(app.WaitBar);
                    %                     return
            end
            
            %app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            %app.WaitBar.Message = 'Updating data';
            
            %ResetGUI(app);
            %UpdateGUI(app);
            
            %close(app.WaitBar);
            
        end
        
        
        function ContextMenu_MainTree_DeletePushed(app, ~)
            % Here we are sure that the node is properly selected
            % Do not call this function directly from the menu!
            
            NodeData = app.TreeData_Main.SelectedNodes.NodeData;
            
            switch NodeData(1)
                case 1   % It data
                    app.XMapToolsData.MapData.It.Names(NodeData(2)) = [];
                    app.XMapToolsData.MapData.It.Types(NodeData(2)) = [];
                    app.XMapToolsData.MapData.It.ElInd(NodeData(2)) = [];
                    app.XMapToolsData.MapData.It.Data(NodeData(2)) = [];
                    
                    app.TreeData_Main.SelectedNodes.delete;
                    app.TreeData_Main.SelectedNodes = [];
                    RebuildGUI(app,'It');
                    
                case 2 % Qt data
                    % The button "Delete" is only available for Qt data
                    % (not for individual elements/oxides)
                    app.XMapToolsData.MapData.Qt.Names(NodeData(2)) = [];
                    app.XMapToolsData.MapData.Qt.IsOxide(NodeData(2)) = [];
                    app.XMapToolsData.MapData.Qt.MaskFile(NodeData(2)) = [];
                    app.XMapToolsData.MapData.Qt.NbCalibPoints(NodeData(2)) = [];
                    app.XMapToolsData.MapData.Qt.Data(NodeData(2)) = [];
                    
                    app.TreeData_Main.SelectedNodes.delete;
                    app.TreeData_Main.SelectedNodes = [];
                    RebuildGUI(app,'Qt');
                    
                case 3 % Me data
                    app.XMapToolsData.MapData.Me.Names(NodeData(2)) = [];
                    app.XMapToolsData.MapData.Me.IsOxide(NodeData(2)) = [];
                    app.XMapToolsData.MapData.Me.MaskFile(NodeData(2)) = [];
                    app.XMapToolsData.MapData.Me.NbCalibPoints(NodeData(2)) = [];
                    app.XMapToolsData.MapData.Me.Data(NodeData(2)) = [];
                    
                    app.TreeData_Main.SelectedNodes.delete;
                    app.TreeData_Main.SelectedNodes = [];
                    RebuildGUI(app,'Me');
                    
                case 4 % Re data
                    app.XMapToolsData.MapData.Re.Names(NodeData(2)) = [];
                    app.XMapToolsData.MapData.Re.Types(NodeData(2)) = [];
                    app.XMapToolsData.MapData.Re.Coord(NodeData(2)) = [];
                    app.XMapToolsData.MapData.Re.Data(NodeData(2)) = [];
                    
                    app.TreeData_Main.SelectedNodes.delete;
                    app.TreeData_Main.SelectedNodes = [];
                    RebuildGUI(app,'Re');
                    
                case 5 % Ot data
                    app.XMapToolsData.MapData.Ot.Names(NodeData(2)) = [];
                    app.XMapToolsData.MapData.Ot.Types(NodeData(2)) = [];
                    app.XMapToolsData.MapData.Ot.Data(NodeData(2)) = [];
                    
                    app.TreeData_Main.SelectedNodes.delete;
                    app.TreeData_Main.SelectedNodes = [];
                    RebuildGUI(app,'Ot');
                    
                case 7
                    
                    app.XMapToolsData.MapData.ROI.Names(NodeData(2)) = [];
                    app.XMapToolsData.MapData.ROI.Types(NodeData(2)) = [];
                    app.XMapToolsData.MapData.ROI.Data(NodeData(2)) = [];
                    
                    app.TreeData_Main.SelectedNodes.delete;
                    app.TreeData_Main.SelectedNodes = [];
                    RebuildGUI(app,'ROI');
                    
                case 8
                    if isequal(NodeData(3),0)
                        app.XMapToolsData.MapData.Im.Names(NodeData(2)) = [];
                        app.XMapToolsData.MapData.Im.Types(NodeData(2)) = [];
                        app.XMapToolsData.MapData.Im.Data(NodeData(2)) = [];
                        
                        app.TreeData_Main.SelectedNodes.delete;
                        app.TreeData_Main.SelectedNodes = [];
                        RebuildGUI(app,'Im');
                    else
                        app.XMapToolsData.MapData.Im.Data(NodeData(2)).Labels(NodeData(3)) = [];
                        app.XMapToolsData.MapData.Im.Data(NodeData(2)).CData(NodeData(3)) = [];
                        
                        app.TreeData_Main.SelectedNodes.delete;
                        app.TreeData_Main.SelectedNodes = [];
                        
                        if isempty(app.XMapToolsData.MapData.Im.Data(NodeData(2)).Labels)
                            app.XMapToolsData.MapData.Im.Names(NodeData(2)) = [];
                            app.XMapToolsData.MapData.Im.Types(NodeData(2)) = [];
                            app.XMapToolsData.MapData.Im.Data(NodeData(2)) = [];
                            
                            app.TreeData_Main.Children(NodeData(1)).Children(NodeData(2)).delete;
                        end
                        
                        RebuildGUI(app,'Im');
                    end
            end
            
            % Eliminate classification data
            app.Classify_ElemList_ElList = {};
            app.Classify_ElemList_ElSource = [];
            app.Classify_ElemList_CodePos = [];
            app.Classify_ElemList_CodePosMap = [];
            
            app.Classify_ElemList.Value = '';
            
            app.SaveRequired = 1;
            
        end
        
        
        function ContextMenu_MainTree_ExportPushed(app, ~)
            
            NodeData = app.TreeData_Main.SelectedNodes.NodeData;
            
            
            switch NodeData(1)
                
                case 7 % ROI
                    
                    ROI = app.XMapToolsData.MapData.ROI.Data(NodeData(2)).ROI;
                    
                    if max(ROI(:)) < 256
                        ROI = uint8(ROI);
                    end
                    
                    f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
                    [filename, pathname] = uiputfile('ROI.mat', 'Save ROI as');
                    delete(f);
                    
                    if ~isequal(filename,0)
                        app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
                        app.WaitBar.Message = 'XMapTools is exporting data';
                        
                        save([pathname,filename],'ROI');
                        
                        close(app.WaitBar);
                    end
                    
            end
            
            
            
        end
        
        function ContextMenu_MainTree_DuplicatePushed(app, ~)
            % Here we are sure that the node is properly selected
            % Do not call this function directly from the menu!
            NodeData = app.TreeData_Main.SelectedNodes.NodeData;
            
            switch NodeData(1)
                case 2
                    app.XMapToolsData.MapData.Qt.Names{end+1} = [char(app.XMapToolsData.MapData.Qt.Names(NodeData(2))),'_copy'];
                    app.XMapToolsData.MapData.Qt.IsOxide(end+1) = app.XMapToolsData.MapData.Qt.IsOxide(NodeData(2));
                    app.XMapToolsData.MapData.Qt.MaskFile(end+1) = app.XMapToolsData.MapData.Qt.MaskFile(NodeData(2));
                    app.XMapToolsData.MapData.Qt.NbCalibPoints(end+1) = app.XMapToolsData.MapData.Qt.NbCalibPoints(NodeData(2));
                    app.XMapToolsData.MapData.Qt.Data(end+1) = app.XMapToolsData.MapData.Qt.Data(NodeData(2));
                    
                    i = length(app.XMapToolsData.MapData.Qt.Names);
                    p = uitreenode(app.Node_Qt,'Text',char(app.XMapToolsData.MapData.Qt.Names{i}),'NodeData',[2,i,0]);
                    
                    for j = 1:length(app.XMapToolsData.MapData.Qt.Data(i).ElNames)
                        p1 = uitreenode(p,'Text',char(app.XMapToolsData.MapData.Qt.Data(i).ElNames{j}),'NodeData',[2,i,j]);
                    end
                    
                    
                case 3
                    app.XMapToolsData.MapData.Me.Names{end+1} = [char(app.XMapToolsData.MapData.Me.Names(NodeData(2))),'_copy'];
                    app.XMapToolsData.MapData.Me.IsOxide(end+1) = app.XMapToolsData.MapData.Me.IsOxide(NodeData(2));
                    app.XMapToolsData.MapData.Me.MaskFile(end+1) = app.XMapToolsData.MapData.Me.MaskFile(NodeData(2));
                    app.XMapToolsData.MapData.Me.NbCalibPoints(end+1) = app.XMapToolsData.MapData.Me.NbCalibPoints(NodeData(2));
                    app.XMapToolsData.MapData.Me.Data(end+1) = app.XMapToolsData.MapData.Me.Data(NodeData(2));
                    
                    i = length(app.XMapToolsData.MapData.Me.Names);
                    p = uitreenode(app.Node_Me,'Text',char(app.XMapToolsData.MapData.Me.Names{i}),'NodeData',[3,i,0]);
                    
                    for j = 1:length(app.XMapToolsData.MapData.Me.Data(i).ElNames)
                        p1 = uitreenode(p,'Text',char(app.XMapToolsData.MapData.Me.Data(i).ElNames{j}),'NodeData',[3,i,j]);
                    end
                    
            end
            
            app.SaveRequired = 1;
            
        end
        
        function ContextMenu_MainTree_ConvertPushed(app, ~)
            % Do not call this function directly from the menu!
            
            NodeData = app.TreeData_Main.SelectedNodes.NodeData;
            
            switch NodeData(1)
                case 2
                    
                    app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
                    app.WaitBar.Message = 'Select the conversion format';
                    
                    NbQuantiBefore = length(app.XMapToolsData.MapData.Qt.Names);
                    waitfor(Data_Conversion(app,'Quanti'));
                    NbQuantiAfter = length(app.XMapToolsData.MapData.Qt.Names);
                    
                    app.WaitBar.Message = 'XMapTools is running numbers';
                    
                    if NbQuantiAfter > NbQuantiBefore
                        %ResetGUI(app);
                        %UpdateGUI(app);
                        
                        p = uitreenode(app.Node_Qt,'Text',char(app.XMapToolsData.MapData.Qt.Names{NbQuantiAfter}),'NodeData',[2,NbQuantiAfter,0]);
                        
                        for j = 1:length(app.XMapToolsData.MapData.Qt.Data(NbQuantiAfter).ElNames)
                            p1 = uitreenode(p,'Text',char(app.XMapToolsData.MapData.Qt.Data(NbQuantiAfter).ElNames{j}),'NodeData',[2,NbQuantiAfter,j]);
                        end
                        
                        collapse(app.TreeData_Main)
                        expand(app.Node_Qt.Children(end))
                        app.TreeData_Main.SelectedNodes = app.Node_Qt.Children(end).Children(1);
                        TreeData_MainSelectionChanged(app);
                    end
                    close(app.WaitBar)
                    
                case 3
                    
                    app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
                    app.WaitBar.Message = 'Select the conversion format';
                    
                    NbQuantiBefore = length(app.XMapToolsData.MapData.Me.Names);
                    waitfor(Data_Conversion(app,'Merged'));
                    NbQuantiAfter = length(app.XMapToolsData.MapData.Me.Names);
                    
                    app.WaitBar.Message = 'XMapTools is running numbers';
                    
                    if NbQuantiAfter > NbQuantiBefore
                        ResetGUI(app);
                        UpdateGUI(app);
                        
                        collapse(app.TreeData_Main)
                        expand(app.Node_Me.Children(end))
                        app.TreeData_Main.SelectedNodes = app.Node_Me.Children(end).Children(1);
                        TreeData_MainSelectionChanged(app);
                    end
                    close(app.WaitBar)
            end
            
            app.SaveRequired = 1;
            
        end
        
        function ContextMenu_MainTree_SplitPushed(app, ~)
            % Only accessible if a maskfile is selected
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'XMapTools is spliting data';
            
            NbQuantiBefore = length(app.XMapToolsData.MapData.Qt.Names);
            
            NodeData = app.TreeData_Main.SelectedNodes.NodeData;
            SelMaskFile = app.TreeData_Additional.SelectedNodes.NodeData(2);
            MaskFile = app.XMapToolsData.MapData.MaskFile.Names(SelMaskFile);
            
            MaskNames = app.XMapToolsData.MapData.MaskFile.Masks(SelMaskFile).Names(2:end);
            Mask = app.XMapToolsData.MapData.MaskFile.Masks(SelMaskFile).MaskMap;
            
            IsOxide = app.XMapToolsData.MapData.Me.IsOxide(NodeData(2));
            
            Pos = length(app.XMapToolsData.MapData.Qt.Names);
            
            for i = 1:length(MaskNames)
                app.XMapToolsData.MapData.Qt.Names{Pos+i} = MaskNames{i};
                app.XMapToolsData.MapData.Qt.IsOxide(Pos+i) = IsOxide;
                app.XMapToolsData.MapData.Qt.MaskFile{Pos+i} = MaskFile;
                app.XMapToolsData.MapData.Qt.NbCalibPoints(Pos+i) = 0;
                
                MaskTemp = zeros(size(Mask));
                Idx = find(Mask == i);
                MaskTemp(Idx) = ones(size(Idx));
                
                for j = 1:length(app.XMapToolsData.MapData.Me.Data(NodeData(2)).ElNames)
                    
                    app.XMapToolsData.MapData.Qt.Data(i).ElNames{j} = app.XMapToolsData.MapData.Me.Data(NodeData(2)).ElNames{j};
                    app.XMapToolsData.MapData.Qt.Data(i).ElInd(j) = app.XMapToolsData.MapData.Me.Data(NodeData(2)).ElInd(j);
                    app.XMapToolsData.MapData.Qt.Data(i).CData(j).Map = app.XMapToolsData.MapData.Me.Data(NodeData(2)).CData(j).Map .* MaskTemp;
                    app.XMapToolsData.MapData.Qt.Data(i).OxInd(j) = app.XMapToolsData.MapData.Me.Data(NodeData(2)).OxInd(j);
                    
                end
                
            end
            
            NbQuantiAfter = length(app.XMapToolsData.MapData.Qt.Names);
            
            if NbQuantiAfter > NbQuantiBefore
                ResetGUI(app);
                UpdateGUI(app);
                
                collapse(app.TreeData_Main)
                expand(app.Node_Qt.Children(end))
                app.TreeData_Main.SelectedNodes = app.Node_Qt.Children(end).Children(1);
                TreeData_MainSelectionChanged(app);
            end
            
            app.SaveRequired = 1;
            
            close(app.WaitBar);
            
        end
        
        function ContextMenu_MainTree_InfoPushed(app, ~)
            % Here we are sure that the node is properly selected
            % Do not call this function directly from the menu!
            
            if isempty(app.TreeData_Main.SelectedNodes)
                return
            end
            NodeData = app.TreeData_Main.SelectedNodes.NodeData;
            
            % This export the data of the displayed map (if selected)
            if NodeData(2) > 0
                if isequal(NodeData(1),1)
                    Text = 'Itensity data';
                    Map = app.TreeData_Main.SelectedNodes.Text;
                else  % was elseif NodeData(3) > 0
                    switch NodeData(1)
                        case 2
                            Text = ['Quanti / ',app.Node_Qt.Children(NodeData(2)).Text];
                        case 3
                            Text = ['Merged / ',app.Node_Me.Children(NodeData(2)).Text];
                        case 4
                            Text = ['Results / ',app.Node_Re.Children(NodeData(2)).Text];
                            
                    end
                    Map = app.TreeData_Main.SelectedNodes.Text;
                end
                
                % Export image data
                ImageData = PlotMap_ExtractPlottedImage(app);
                
                Min = app.hVerticalLines(2).Value;
                Max = app.hVerticalLines(3).Value;
                
                switch app.Options_LogColormap.Value
                    case 1
                        Scale = 'log';
                    otherwise
                        Scale = 'linear';
                end
                
                ColorPal = app.Options_ColormapDropDown.Value;
                
                NbPxZero = numel(find(ImageData == 0));
                NbPxDisp = length(find(ImageData(:) >= Min & ImageData(:) <= Max));
                NbPxUpper = length(find(ImageData(:) > Max));
                NbPxLower = length(find(ImageData(:) < Min));
                
                Res = prod(size(ImageData))/1e6;
                if Res >= 1
                    ResStr = sprintf('%.2f',Res);
                else
                    ResStr = sprintf('%.3f',Res);
                end
                
                uiwait(msgbox({...
                    ['Type: ',Text], ...
                    ['Map: ',Map], ...
                    '- - - - - - - - - - - - - - - - - - - -', ...
                    ['Width: ',num2str(size(ImageData,2)),' px'], ...
                    ['Height: ',num2str(size(ImageData,1)),' px'], ...
                    ['Resolution: ',ResStr,' MP'], ...
                    '- - - - - - - - - - - - - - - - - - - -',...
                    ['Image Min: ',num2str(Min)], ...
                    ['Image Max: ',num2str(Max)], ...
                    ['Data Min: ',num2str(min(ImageData(find(ImageData))))], ...
                    ['Data Max: ',num2str(max(ImageData(:)))], ...
                    '- - - - - - - - - - - - - - - - - - - -',...
                    ['Nb Px disp: ',num2str(NbPxDisp),' (',num2str(round(NbPxDisp/(NbPxDisp+NbPxZero)*100)),'%)'], ...
                    ['Nb Px zero: ',num2str(NbPxZero),' (',num2str(round(NbPxZero/(NbPxDisp+NbPxZero)*100)),'%)'], ...
                    ['Nb Px >max: ',num2str(NbPxUpper)], ...
                    ['Nb Px <min: ',num2str(NbPxLower-NbPxZero)], ...
                    '- - - - - - - - - - - - - - - - - - - -',...
                    ['Color palette: ',char(ColorPal)], ...
                    ['Scale: ',Scale], ...
                    ['Resolution: ',num2str(app.Options_ColormapResEditField.Value)], ...
                    '- - - - - - - - - - - - - - - - - - - -'...
                    },'Image Info','help'));
                
                figure(app.XMapTools_GUI);
            end
            
            
            
        end
        
        function ContextMenu_MainTree_SaveAsGifPushed(app, ~)
            %
            NodeData = app.TreeData_Main.SelectedNodes.NodeData;
            
            h = figure;
            axis tight manual % this ensures that getframe() returns a consistent size
            filename = 'testAnimated.gif';
            
            ax = gca;
            ax.Interactions = [];
            
            h.Color = 'w';
            
            for i = 1:length(app.XMapToolsData.MapData.Im.Data(NodeData(2)).Labels)
                TheMap = app.XMapToolsData.MapData.Im.Data(NodeData(2)).CData(i).Map;
                a1 = imagesc(size(TheMap,2),size(TheMap,1),TheMap);
                axis image
                caxis(app.XMapToolsData.MapData.Im.Data(NodeData(2)).CData(i).Lim)
                colormap(app.XMapToolsData.MapData.Im.Data(NodeData(2)).CData(i).ColorMap)
                a1.Parent.XTick = [];
                a1.Parent.YTick = [];
                
                if app.Options_LogColormap.Value
                    a1.Parent.ColorScale = 'log';
                else
                    a1.Parent.ColorScale = 'linear';
                end
                
                % title(app.XMapToolsData.MapData.Im.Data(NodeData(2)).Labels{i});
                cb = colorbar('horizontal');
                cb.Label.String = app.XMapToolsData.MapData.Im.Data(NodeData(2)).Labels{i};
                
                drawnow
                
                frame = getframe(h);
                im = frame2im(frame);
                [imind,cm] = rgb2ind(im,256);
                
                if i == 1
                    imwrite(imind,cm,filename,'gif', 'Loopcount',inf);
                else
                    imwrite(imind,cm,filename,'gif','WriteMode','append');
                end
                
            end
            
        end
        
        function ContextMenu_MainTree_ExportAllPushed(app, ~)
            
            NodeData = app.TreeData_Main.SelectedNodes.NodeData;
            
            app.XMapToolsData.MapData.Qt.Data(NodeData(2))
            
            
            DefOrder = {'Ref','SiO2','TiO2','Al2O3','FeO','Fe2O3','MnO','MgO','CaO','Na2O','K2O','Fe3'};
            
            Text='';
            for i =1:length(DefOrder)
                Text = [Text,char(DefOrder{i}),' '];
            end
            options.Resize='on';
            Text = inputdlg('Order to export','Input',8,{Text},options);
            
            Order = strread(char(Text),'%s','delimiter',' ');
            
            [Is,Idx] = ismember(Order,app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElNames);
            
            SumQuanti = zeros(size(app.XMapToolsData.MapData.Qt.Data(NodeData(2)).CData(1).Map));
            AllData = zeros(size(app.XMapToolsData.MapData.Qt.Data(NodeData(2)).CData(1).Map,1),size(app.XMapToolsData.MapData.Qt.Data(NodeData(2)).CData(1).Map,2),length(app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElNames));
            for i = 1:length(app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElNames)
                if ~isequal(app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElNames{i},'Total(wt%)')
                    SumQuanti = SumQuanti + app.XMapToolsData.MapData.Qt.Data(NodeData(2)).CData(i).Map;
                end
                AllData(:,:,i) = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).CData(i).Map;
            end
            
            IdxPixels = find(SumQuanti > 0);
            
            [Success,Message,MessageID] = mkdir('Exported-Oxides');
            
            cd Exported-Oxides
            f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
            [Directory, pathname] = uiputfile({'*.txt', 'TXT Files (*.txt)'}, 'Export compositions as');
            close(f)
            cd ..
            
            figure(app.XMapTools_GUI)
            
            if ~Directory
                return
            end
            
            fid = fopen([pathname,Directory],'w');
            fprintf(fid,'%s\n%s\n','--','Mineral compositions (wt%) exported from XMapTools');
            fprintf(fid,'%s\n\n',datestr(now));
            
            Format='';
            for i = 1:length(Order)
                fprintf(fid,'%s\t',Order{i});
                if isequal(Order{i},'Ref')
                    Format = [Format,'%.0f\t'];
                else
                    Format = [Format,'%.3f\t'];
                end
            end
            Format(end) = 'n';
            
            fprintf(fid,'\n');
            
            Mtx = zeros(numel(IdxPixels),numel(Order));
            for i = 1:length(Order)
                if isequal(Order{i},'Ref')
                    Mtx(:,i) = IdxPixels;
                elseif Is(i)
                    Map = AllData(:,:,Idx(i));
                    Mtx(:,i) = Map(IdxPixels);
                end
            end
            
            for i = 1:(size(Mtx,1))
                fprintf(fid,Format,Mtx(i,:));
            end
            
            
            
        end
        
        function ContextMenu_AdditionalTree_DuplicatePushed(app, ~)
            % Here we are sure that the node is properly selected
            % Do not call this function directly from the menu!
            
            NodeData = app.TreeData_Additional.SelectedNodes.NodeData;
            
            SelTrainingSet = NodeData(2);
            PosDuplicate = length(app.XMapToolsData.TrainingSet.Names)+1;
            
            switch NodeData(1)
                case 12
                    if NodeData(3) > 0
                        % a phase of the maskfile is selected (cannot be deleted)
                        return
                    end
                    
                    app.XMapToolsData.TrainingSet.Names{PosDuplicate} = [char(app.XMapToolsData.TrainingSet.Names{SelTrainingSet}),'_copy'];
                    app.XMapToolsData.TrainingSet.Types(PosDuplicate) = app.XMapToolsData.TrainingSet.Types(SelTrainingSet);
                    app.XMapToolsData.TrainingSet.Nb(PosDuplicate) = app.XMapToolsData.TrainingSet.Nb(SelTrainingSet);
                    app.XMapToolsData.TrainingSet.Data(PosDuplicate) = app.XMapToolsData.TrainingSet.Data(SelTrainingSet);
                    app.XMapToolsData.TrainingSet.MaskSignature(PosDuplicate) = app.XMapToolsData.TrainingSet.MaskSignature(SelTrainingSet);
                    app.XMapToolsData.TrainingSet.MaskNode(PosDuplicate) = app.XMapToolsData.TrainingSet.MaskNode(SelTrainingSet);
                    
                    p = uitreenode(app.Node_TrainingSet,'Text',char(app.XMapToolsData.TrainingSet.Names{PosDuplicate}),'NodeData',[12,PosDuplicate,0]);
                    for i = 1:length(app.XMapToolsData.TrainingSet.Data(PosDuplicate).Names)
                        p1 = uitreenode(p,'Text',char(app.XMapToolsData.TrainingSet.Data(PosDuplicate).Names{i}),'NodeData',[2,PosDuplicate,i]);
                        for j = 1:length(app.XMapToolsData.TrainingSet.Data(PosDuplicate).ROI(i).Types)
                            p2 = uitreenode(p1,'Text',char(app.XMapToolsData.TrainingSet.Data(PosDuplicate).ROI(i).Types{j}),'NodeData',[2,PosDuplicate,i,j]);
                        end
                    end
                    RebuildGUI(app,'TrainingSet'); % update indexes
                    
            end
            
        end
        
        function ContextMenu_AdditionalTree_DeletePushed(app, ~)
            % Here we are sure the TreeData_Main is selected (Delete)
            NodeData = app.TreeData_Additional.SelectedNodes.NodeData;
            
            switch NodeData(1)
                case 11   % MaskFile data
                    if NodeData(3) > 0
                        % a phase of the maskfile is selected (cannot be deleted)
                        return
                    end
                    app.XMapToolsData.MapData.MaskFile.Names(NodeData(2)) = [];
                    app.XMapToolsData.MapData.MaskFile.Types(NodeData(2)) = [];
                    app.XMapToolsData.MapData.MaskFile.NbMasks(NodeData(2)) = [];
                    app.XMapToolsData.MapData.MaskFile.Masks(NodeData(2)) = [];
                    
                    if isempty(app.XMapToolsData.MapData.MaskFile.Names)
                        % Reset the mask file variable to resolve an issue
                        % with XMapTools 4.1
                        app.XMapToolsData.MapData.MaskFile.Names = {};
                        app.XMapToolsData.MapData.MaskFile.Types = [];
                        app.XMapToolsData.MapData.MaskFile.NbMasks = [];
                        app.XMapToolsData.MapData.MaskFile.Masks(1).Names = {};
                        app.XMapToolsData.MapData.MaskFile.Masks(1).Densities = [];
                        app.XMapToolsData.MapData.MaskFile.Masks(1).MaskMap = [];
                        app.XMapToolsData.MapData.MaskFile.Masks(1).MaskProbability = [];
                        app.XMapToolsData.MapData.MaskFile.Masks(1).Signature = [];
                        app.XMapToolsData.MapData.MaskFile.Masks(1).SubMask(1).Names = {};
                        app.XMapToolsData.MapData.MaskFile.Masks(1).SubMask(1).Densities = [];
                        app.XMapToolsData.MapData.MaskFile.Masks(1).SubMask(1).MaskSelMaskMap = [];
                        app.XMapToolsData.MapData.MaskFile.Masks(1).SubMask(1).MaskProbability = [];
                    end
                    
                    app.TreeData_Additional.SelectedNodes.delete;
                    app.TreeData_Additional.SelectedNodes = [];
                    RebuildGUI(app,'MaskFiles'); % update indexes
                    
                case 12
                    if isequal(NodeData(2),0)
                        return
                    end
                    DeleteROI = 0;
                    DeletePhase = 0;
                    DeleteTrainingSet = 0;
                    
                    if length(NodeData) > 3
                        if NodeData(4) > 0
                            DeleteROI = 1;
                        else
                            DeletePhase = 1;
                        end
                    else
                        if NodeData(3) > 0
                            DeletePhase = 1;
                        else
                            DeleteTrainingSet = 1;
                        end
                    end
                    
                    if DeleteROI
                        ROI_DeletingShape(app);
                        app.SaveRequired = 1;
                        return
                        %app.XMapToolsData.TrainingSet.Data(NodeData(2)).ROI(NodeData(3)).Types(NodeData(4)) = [];
                        %app.XMapToolsData.TrainingSet.Data(NodeData(2)).ROI(NodeData(3)).Data(NodeData(4)) = [];
                    end
                    
                    if DeletePhase
                        app.XMapToolsData.TrainingSet.Data(NodeData(2)).Names(NodeData(3)) = [];
                        %app.XMapToolsData.TrainingSet.Data(NodeData(2)).Types(NodeData(3)) = [];
                        app.XMapToolsData.TrainingSet.Data(NodeData(2)).ROI(NodeData(3)) = [];
                        %app.XMapToolsData.TrainingSet.Data(NodeData(2)).Density(NodeData(3)) = [];
                        if ~isempty(app.XMapToolsData.TrainingSet.Data(NodeData(2)).Names)
                            app.XMapToolsData.TrainingSet.Nb(NodeData(2)) = length(app.XMapToolsData.TrainingSet.Data(NodeData(2)).Names);
                        else
                            app.XMapToolsData.TrainingSet.Nb(NodeData(2)) = 0;
                        end
                    end
                    if DeleteTrainingSet
                        % Delete the training set
                        app.XMapToolsData.TrainingSet.Names(NodeData(2)) = [];
                        app.XMapToolsData.TrainingSet.Types(NodeData(2)) = [];
                        app.XMapToolsData.TrainingSet.Nb(NodeData(2)) = [];
                        app.XMapToolsData.TrainingSet.Data(NodeData(2)) = [];
                        
                    end
                    app.TreeData_Additional.SelectedNodes.delete;
                    app.TreeData_Additional.SelectedNodes = [];
                    RebuildGUI(app,'TrainingSet'); % update indexes
                    
                case 13
                    % We are editing a segmentation scheme
                    
                    DeleteSEGM = 0;
                    DeletePhase = 0;
                    
                    if length(NodeData) > 3
                        if NodeData(4) > 0
                            DeletePhase = 1;
                        else
                            DeleteSEGM = 1;
                        end
                    end
                    
                    if DeletePhase
                        app.XMapToolsData.SegScheme.Data(NodeData(3)).Names(NodeData(4)) = [];
                        app.XMapToolsData.SegScheme.Data(NodeData(3)).ROI(NodeData(4)) = [];
                        app.XMapToolsData.SegScheme.Nb(NodeData(3)) = length(app.XMapToolsData.SegScheme.Data(NodeData(3)).Names);
                    end
                    
                    if DeleteSEGM
                        app.XMapToolsData.SegScheme.Names(NodeData(3)) = [];
                        app.XMapToolsData.SegScheme.Types(NodeData(3)) = [];
                        app.XMapToolsData.SegScheme.Nb(NodeData(3)) = [];
                        app.XMapToolsData.SegScheme.Data(NodeData(3)) = [];
                    end
                    
                    app.TreeData_Additional.SelectedNodes.delete;
                    app.TreeData_Additional.SelectedNodes = [];
                    RebuildGUI(app,'SegSchemes'); % update indexes
                    
                    
                case 14
                    % we delete the std spot
                    
                    app.XMapToolsData.Standards.Coord(NodeData(2),:) = [];
                    app.XMapToolsData.Standards.Types(NodeData(2)) = [];
                    app.XMapToolsData.Standards.Labels(NodeData(2)) = [];
                    app.XMapToolsData.Standards.Selected(NodeData(2)) = [];
                    app.XMapToolsData.Standards.XCoo(NodeData(2)) = [];
                    app.XMapToolsData.Standards.YCoo(NodeData(2)) = [];
                    app.XMapToolsData.Standards.XY(NodeData(2),:) = [];
                    app.XMapToolsData.Standards.DataPro(NodeData(2),:) = [];
                    app.XMapToolsData.Standards.DataIt(NodeData(2),:) = [];
                    app.XMapToolsData.Standards.DataItAv(NodeData(2),:) = [];
                    
                    app.TreeData_Additional.SelectedNodes.delete;
                    app.TreeData_Additional.SelectedNodes = [];
                    RebuildGUI(app,'Standards'); % update indexes
                    
                case 16
                    
                    app.XMapToolsData.MapLOD.Names(NodeData(2)) = [];
                    app.XMapToolsData.MapLOD.Types(NodeData(2)) = [];
                    app.XMapToolsData.MapLOD.Data(NodeData(2)) = [];
                    
                    app.TreeData_Additional.SelectedNodes.delete;
                    app.TreeData_Additional.SelectedNodes = [];
                    RebuildGUI(app,'LOD'); % update indexes
                    
            end
            
            app.SaveRequired = 1;
            
        end
        
        function ContextMenu_AdditionalTree_InfoPushed(app, ~)
            
            %keyboard
        end
        
        function ContextMenu_AdditionalTree_CreateTrainingSetPushed(app, ~)
            
            SelectedNodes = app.TreeData_Additional.SelectedNodes;
            NodeData = SelectedNodes.NodeData;
            
            TrainingSet = app.XMapToolsData.TrainingSet;
            Idx = numel(TrainingSet.Types);
            
            Idx = Idx+1;
            Name = ['SubPhaseDef_',char(SelectedNodes.Text)];
            
            TrainingSet.Names{Idx} = Name;
            TrainingSet.Types(Idx) = 2;                 % This type is for submasks
            TrainingSet.MaskSignature(Idx) = app.XMapToolsData.MapData.MaskFile.Masks(NodeData(2)).Signature;
            TrainingSet.MaskNode(Idx) = NodeData(3);
            TrainingSet.Nb(Idx) = 0;
            TrainingSet.Data(Idx).Names = {};
            TrainingSet.Data(Idx).ROI(1).Types = {};
            TrainingSet.Data(Idx).ROI(1).Data(1).Coordinates = [];
            
            p = uitreenode(app.Node_TrainingSet,'Text',char(TrainingSet.Names(Idx)),'NodeData',[12,Idx,0]);
            
            expand(app.Node_TrainingSet);
            app.TreeData_Additional.SelectedNodes = p;
            
            NamesDB = app.DensityData.Names;
            [uniqueA] = unique(NamesDB,'first');
            for i = 1:20
                AddNames{i} = [char(SelectedNodes.Text),'_',num2str(i)];
            end
            AddNames2 = {'--- GENERIC PHASE NAMES WITH INCICES ---','Phase_1','Phase_2','Phase_3','Phase_4','Phase_5','Phase_6','Phase_7','Phase_8','Phase_9','Phase_10','Phase_11','Phase_12','Phase_13','Phase_14','Phase_15', ...
                '--- FULL DATABASE IS SHOWN BELLOW ---'};
            
            PhaseList = [AddNames,AddNames2,uniqueA];
            
            if isequal(app.Classify_ActivateAssistant.Value,1)
                app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
                app.WaitBar.Message = 'Select phase(s) and press OK to continue...';
                waitfor(Selector(app,PhaseList,'Select phases in the list below','Multiple'));
                close(app.WaitBar)
                
                for i = 1:length(app.ExchangeSelector)
                    Name = char(app.ExchangeSelector{i});
                    IdxMask = TrainingSet.Nb(Idx)+1;        % we add one
                    TrainingSet.Data(Idx).Names{IdxMask} = Name;
                    TrainingSet.Data(Idx).ROI(IdxMask).Types{1} = '';
                    TrainingSet.Data(Idx).ROI(IdxMask).Data(1).Coordinates = [];
                    
                    p = uitreenode(app.Node_TrainingSet.Children(Idx),'Text',char(Name),'NodeData',[12,Idx,IdxMask]);
                    
                    TrainingSet.Nb(Idx) = IdxMask;
                    
                    expand(app.Node_TrainingSet.Children(Idx));
                end
            end
            
            app.XMapToolsData.TrainingSet = TrainingSet;
        end
        
        function ContextMenu_AdditionalTree_ExportPushed(app, ~)
            % We are sure the additional tree is selected
            NodeData = app.TreeData_Additional.SelectedNodes.NodeData;
            
            switch NodeData(1)
                case 11  % Export MaskFile
                    
                    MaskId = NodeData(2);
                    
                    MaskFileName = app.XMapToolsData.MapData.MaskFile.Names{MaskId};
                    FinalMaskMap = app.XMapToolsData.MapData.MaskFile.Masks(MaskId).MaskMap;
                    
                    NbPixels = zeros(app.XMapToolsData.MapData.MaskFile.NbMasks(MaskId)-1,1); % exclude 'none'
                    for i = 1:length(NbPixels)
                        NbPixels(i) = length(find(FinalMaskMap == i));
                    end
                    
                    MaskNames = app.XMapToolsData.MapData.MaskFile.Masks(MaskId).Names(2:end);
                    Selected = ones(size(MaskNames));
                    UnselectedPx = sum(NbPixels)-numel(FinalMaskMap);
                    MaskProp = NbPixels./repmat(sum(NbPixels),size(NbPixels,1),1)*100;
                    
                    [Success,Message,MessageID] = mkdir('Maskfiles');
                    
                    cd Maskfiles
                    f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
                    [FinalMaskFileName, Directory] = uiputfile({'*.txt', 'TXT Files (*.txt)'},'Export mask file as',[char(MaskFileName),'.txt']);
                    delete(f);
                    figure(app.XMapTools_GUI)
                    cd ..
                    
                    save([Directory,FinalMaskFileName],'FinalMaskMap', '-ASCII');
                    
                    fid = fopen([char(Directory),'info_',char(FinalMaskFileName)],'w');
                    
                    fprintf(fid,'%s\n',['This file was generated by ',app.XMapTools_VER]);
                    fprintf(fid,'%s\t%s\n','Mask file:',MaskFileName);
                    fprintf(fid,'%s\t%s\n','File name:',FinalMaskFileName);
                    fprintf(fid,'%s\t%s\n\n\n','Date/time:',datetime('now'));
                    
                    fprintf(fid,'%s\n','---------------------------------------------------');
                    fprintf(fid,'%s\n','Calculated mask proportions (copy/paste to Excel)');
                    fprintf(fid,'%s\n','---------------------------------------------------');
                    fprintf(fid,'%s\t%s\t%s\n','Mask-Name','Prop(%)','Nb pixels');
                    for i=1:length(MaskNames)
                        fprintf(fid,'%s\t%.4f\t%.0f\n',MaskNames{i},MaskProp(i),NbPixels(i));
                    end
                    fprintf(fid,'%s\t%.4f\t%.0f%s%.0f\n','Total',sum(MaskProp),sum(NbPixels),'/',numel(FinalMaskMap));
                    fprintf(fid,'%s\n\n\n\n','---------------------------------------------------');
                    
                    fprintf(fid,'%s\n','############ WARNING: DO NOT EDIT BELOW ###########');
                    fprintf(fid,'\n%s\n','>1 Type');
                    fprintf(fid,'%s\n\n','1        | Maskfile exported from XMapTools');
                    
                    fprintf(fid,'\n%s\n','>2 Selected masks for import in XMapTools');
                    for i = 1:length(Selected)
                        fprintf(fid,'%.0f\n',Selected(i));
                    end
                    
                    fprintf(fid,'\n\n%s\n','>3 Mask names');
                    for i = 1:length(Selected)
                        fprintf(fid,'%s\n',char(MaskNames{i}));
                    end
                    
                    fprintf(fid,'\n\n');
                    fprintf(fid,'%s\n','###################################################');
                    fprintf(fid,'\n\n');
                    
                    fclose(fid);
                    
            end
            
            
            
            
            %keyboard
        end
        
        function ContextMenu_AdditionalTree_Cleaning(app)
            % MaskFiles:
            for i = 1:length(app.Node_Masks.Children)
                app.Node_Masks.Children(i).ContextMenu = [];
                for j = 1:length(app.Node_Masks.Children(i).Children)
                    app.Node_Masks.Children(i).Children(j).ContextMenu = [];
                end
            end
            % Training Sets:
            for i = 1:length(app.Node_TrainingSet.Children)
                app.Node_TrainingSet.Children(i).ContextMenu = [];
                for j = 1:length(app.Node_TrainingSet.Children(i).Children)
                    app.Node_TrainingSet.Children(i).Children(j).ContextMenu = [];
                end
            end
            
        end
        
        function ContextMenu_AdditionalTree_EditROIPoint(app)
            
            
            
            keyboard
        end
        
        
        
        function EliminatePixelsFromROI(app,mode)
            
            bw = createMask(app.TempROI);
            
            switch mode
                case 'in'
                    IdxPx = find(bw);
                case 'out'
                    IdxPx = find(bw == 0);
            end
            
            if numel(IdxPx) > 0
                NodeData = app.TreeData_Main.SelectedNodes.NodeData;
                
                if ~isequal(NodeData(1),3) % Only for Me
                    return
                end
                
                for i = 1:length(app.XMapToolsData.MapData.Me.Data(NodeData(2)).ElNames)
                    app.XMapToolsData.MapData.Me.Data(NodeData(2)).CData(i).Map(IdxPx) = zeros(size(IdxPx));
                end
                
                TreeData_MainSelectionChanged(app);
            end
        end
        
        
        function [Valid,ErrorMessage] = CheckMapSizeConsistencyIntensity(app)
            % This function check the consistency of map size in Intensity
            % data
            
            NbMaps = length(app.XMapToolsData.MapData.It.Names);
            
            if isequal(NbMaps,0)
                Valid = 0;
                ErrorMessage = 'No map in Intensity – Map size cannot be determined';
                return
            end
            
            SizeMat = zeros(NbMaps,2);
            
            for i = 1:NbMaps
                SizeMat(i,:) = size(app.XMapToolsData.MapData.It.Data(i).Map);
            end
            
            CheckRows = isequal(mean(SizeMat(:,1)),SizeMat(1,1));
            CheckCol = isequal(mean(SizeMat(:,2)),SizeMat(1,2));
            
            if ~CheckRows || ~CheckCol
                
                ErrorMessage = 'Maps have different sizes:';
                for i = 1:NbMaps
                    ErrorMessage = [ErrorMessage,' ',char(app.XMapToolsData.MapData.It.Names{i}),' (',num2str(SizeMat(i,1)),'/',num2str(SizeMat(i,2)),');'];
                end
                Valid = 0;
            else
                Valid = 1;
                ErrorMessage = '';
            end
        end
        
        
        function Standards_PlotStdROI(app,Idx)
            
            app.WaitBar = uiprogressdlg(app.XMapTools_GUI,'Title','XMapTools');
            app.WaitBar.Message = 'Updating plots...';
            
            
            app.TabGroup.SelectedTab = app.StandardsTab;
            
            if size(app.XMapToolsData.Standards.Coord,1) > 0
                
                if isequal(Idx,0)
                    
                    % We recalculate the maps (for debuging)
                    % Standards_CalculateMapPosition(app);
                    
                    XY = app.XMapToolsData.Standards.XY;
                    
                    try NodeData = app.TreeData_Main.SelectedNodes.NodeData;
                    catch ME
                        close(app.WaitBar)
                        return
                        
                    end
                    
                    if isequal(NodeData(1),1) && NodeData(2) > 0
                        SelectedMap = NodeData(2);
                    else
                        close(app.WaitBar)
                        return
                    end
                    
                    app.SubTabStandard.SelectedTab = app.StdAllPlotTab;
                    
                    SelElMap = app.XMapToolsData.MapData.It.Names{SelectedMap};
                    ElMap = app.XMapToolsData.Standards.ElMap;
                    
                    IdxElPro = find(ismember(ElMap,SelElMap));
                    
                    if isempty(IdxElPro)   % HERE ERROR!
                        close(app.WaitBar)
                        return
                    end
                    
                    DataPro = app.XMapToolsData.Standards.DataPro(:,IdxElPro);
                    DataIt = app.XMapToolsData.Standards.DataIt(:,IdxElPro);
                    
                    yyaxis(app.StdAll_profil,'right');
                    plot(app.StdAll_profil,DataPro,'o-');
                    ylabel(app.StdAll_profil,'wt%')
                    yyaxis(app.StdAll_profil,'left');
                    plot(app.StdAll_profil,DataIt,'o-');
                    
                    MapCorr_Xi = app.XMapToolsData.Standards.MapCorr_Xi;
                    MapCorr_Yi = app.XMapToolsData.Standards.MapCorr_Yi;
                    MapCorr_Data = app.XMapToolsData.Standards.MapCorr_Data;
                    MapCorr_LSm = app.XMapToolsData.Standards.MapCorr_LSm;
                    
                    imagesc(app.StdAll_map1,MapCorr_Xi,MapCorr_Yi,MapCorr_Data(:,:,IdxElPro));
                    colorbar(app.StdAll_map1)
                    axis(app.StdAll_map1,'image');
                    app.StdAll_map1.Title.String = SelElMap;
                    app.ROI_Position_Map1 = images.roi.Point(app.StdAll_map1,'Position',[0,0]);
                    app.ROI_listener_PositionMap1 = addlistener(app.ROI_Position_Map1, 'ROIMoved', @(varargin)MovingROI_PositionMap1(app, app.ROI_Position_Map1));
                    
                    disableDefaultInteractivity(app.StdAll_map1);
                    toolbar = axtoolbar(app.StdAll_map1);
                    toolbar.Visible = 'off';
                    
                    imagesc(app.StdAll_map2,MapCorr_Xi,MapCorr_Yi,MapCorr_LSm(:,:));
                    colorbar(app.StdAll_map2)
                    axis(app.StdAll_map2,'image');
                    app.ROI_Position_Map2 = images.roi.Point(app.StdAll_map2,'Position',[0,0]);
                    app.ROI_listener_PositionMap2 = addlistener(app.ROI_Position_Map2, 'ROIMoved', @(varargin)MovingROI_PositionMap2(app, app.ROI_Position_Map2));
                    
                    toolbar = axtoolbar(app.StdAll_map2);
                    toolbar.Visible = 'off';
                    
                    disableDefaultInteractivity(app.StdAll_map2);
                    
                    
                else
                    XY = app.XMapToolsData.Standards.XY(Idx,:);
                    
                    app.SubTabStandard.SelectedTab = app.StdDataTab;
                    
                    % We add the data to the table
                    ElName = app.XMapToolsData.Standards.Labels{Idx};
                    ElMap = app.XMapToolsData.Standards.ElMap';
                    ElemImport = app.XMapToolsData.Standards.ElemImport;
                    DataPro = app.XMapToolsData.Standards.DataPro(Idx,:);
                    DataIt = app.XMapToolsData.Standards.DataIt(Idx,:);
                    DataItAv = app.XMapToolsData.Standards.DataItAv(Idx,:);
                    
                    app.StandardLabel.Text = ElName;
                    
                    app.Standard_UITable.Data = [ElMap,num2cell(DataIt'),ElemImport,num2cell(DataPro')];
                    
                    app.Standard_UITable.Data(end+1,1) = {''};
                    app.Standard_UITable.Data(end,3) = {'Total:'};
                    app.Standard_UITable.Data(end,4) = {sum(cell2mat(app.Standard_UITable.Data(:,end)))};
                    
                    ColWidth = app.Standard_UITable.Position(3)/4;
                    dCW = ColWidth/4;
                    app.Standard_UITable.ColumnWidth = {ColWidth-dCW,ColWidth+dCW,ColWidth-dCW,ColWidth+dCW};
                    
                    if isequal(app.XMapToolsData.Standards.Types(Idx),2)
                        Seq = false(1,4);
                        Seq(4) = true(1);
                        app.Standard_UITable.ColumnEditable = Seq;
                        
                        s1 = uistyle('BackgroundColor',[0.3010 0.7450 0.9330]);
                        addStyle(app.Standard_UITable,s1,'cell',[[1:length(ElemImport)]',4*ones(size(ElemImport))]);
                        
                        WherePos = find(DataPro);
                        if ~isempty(WherePos)
                            s2 = uistyle('BackgroundColor',[0.3010 0.9330 0.7450]);
                            addStyle(app.Standard_UITable,s2,'cell',[WherePos',4*ones(length(WherePos),1)]);
                        end
                    else
                        Seq = false(1,4);
                        AppId.UITable.ColumnEditable = Seq;
                        
                        s1 = uistyle('BackgroundColor',[1 1 1]);
                        addStyle(app.Standard_UITable,s1,'cell',[[1:length(ElemImport)]',4*ones(size(ElemImport))]);
                    end
                    
                    app.Xori.Value = app.XMapToolsData.Standards.Coord(Idx,1);
                    app.Yori.Value = app.XMapToolsData.Standards.Coord(Idx,2);
                    
                    app.Xm.Value = app.XMapToolsData.Standards.XY(Idx,1);
                    app.Ym.Value = app.XMapToolsData.Standards.XY(Idx,2);
                    
                    
                end
                
                switch app.ColorScaleBar.Value
                    case 'White'
                        ColorCode = 'w';
                    case 'Black'
                        ColorCode = 'k';
                end
                
                for i = 1:size(XY,1)
                    if isequal(Idx,0)
                        if isequal(app.XMapToolsData.Standards.Types(i),1)
                            app.ROI_std(i).ROI = images.roi.Point(app.FigMain,'Position',XY(i,:),'InteractionsAllowed','none','Color','m','Label',app.XMapToolsData.Standards.Labels{i},'LabelAlpha',0,'LabelTextColor',ColorCode);
                            %app.ROIobjectListener_StdSpots = addlistener(app.ROI_std(i).ROI, 'ROIMoved', @(varargin)Standards_MovingStdROI_Main(app, app.ROI_std(i).ROI));
                        else
                            app.ROI_std(i).ROI = images.roi.Point(app.FigMain,'Position',XY(i,:),'InteractionsAllowed','none','Color','b','Label',app.XMapToolsData.Standards.Labels{i},'LabelAlpha',0,'LabelTextColor',ColorCode);
                            %app.ROIobjectListener_StdSpots = addlistener(app.ROI_std(i).ROI, 'ROIMoved', @(varargin)Standards_MovingStdROI_Main(app, app.ROI_std(i).ROI));
                        end
                    else
                        if isequal(app.XMapToolsData.Standards.Types(Idx),1)
                            app.ROI_std(i).ROI = images.roi.Point(app.FigMain,'Position',XY(i,:),'InteractionsAllowed','all','Color','m','Label',app.XMapToolsData.Standards.Labels{Idx},'LabelAlpha',0,'LabelTextColor',ColorCode);
                            app.ROIobjectListener_StdSpots = addlistener(app.ROI_std(i).ROI, 'ROIMoved', @(varargin)Standards_MovingStdROI_Main(app, app.ROI_std(i).ROI));
                        else
                            app.ROI_std(i).ROI = images.roi.Point(app.FigMain,'Position',XY(i,:),'InteractionsAllowed','all','Color','b','Label',app.XMapToolsData.Standards.Labels{Idx},'LabelAlpha',0,'LabelTextColor',ColorCode);
                            app.ROIobjectListener_StdSpots = addlistener(app.ROI_std(i).ROI, 'ROIMoved', @(varargin)Standards_MovingStdROI_Main(app, app.ROI_std(i).ROI));
                        end
                    end
                end
            end
            close(app.WaitBar)
            
        end
        
        
        
        function Standards_UpdateIntensities(app)
            
            app.WaitBar = uiprogressdlg(app.XMapTools_GUI,'Title','XMapTools');
            app.WaitBar.Message = 'Extracting intensity data from positions';
            
            DataIt = zeros(size(app.XMapToolsData.Standards.DataPro));
            DataItAv = zeros(size(app.XMapToolsData.Standards.DataPro));
            
            for i = 1:length(app.XMapToolsData.Standards.ElemImport)
                
                app.WaitBar.Value = 1/length(app.XMapToolsData.Standards.ElemImport);
                
                StdEl = app.XMapToolsData.Standards.RefEl(i);
                StdOx = app.XMapToolsData.Standards.RefOx(i);
                
                if StdEl > 0    % not tested!
                    WhereMap = find(ismember(app.XMapToolsData.MapData.It.ElInd,StdEl));
                else
                    IdxElement = app.ElOxDataDef.OxElIdx(StdOx);
                    WhereMap = find(ismember(app.XMapToolsData.MapData.It.ElInd,IdxElement));
                end
                
                if length(WhereMap) > 1             % We have several maps...
                    ElementSel = app.ElOxDataDef.ElList(app.XMapToolsData.MapData.It.ElInd(WhereMap(1)));
                    warndlg({['Several maps found for ',char(ElementSel)],app.XMapToolsData.MapData.It.Names{WhereMap},'The last map was arbitrarly selected!'},'Warning')
                    WhereMap = WhereMap(end);
                end
                
                if ~isempty(WhereMap)
                    ElMap{i} = app.XMapToolsData.MapData.It.Names{WhereMap};
                    % We need to get the intensity values
                    for j = 1:size(app.XMapToolsData.Standards.XY,1)
                        MapSize = size(app.XMapToolsData.MapData.It.Data(WhereMap).Map);
                        if app.XMapToolsData.Standards.XY(j,1) > 0 && app.XMapToolsData.Standards.XY(j,2) > 0 && app.XMapToolsData.Standards.XY(j,2) < MapSize(1) && app.XMapToolsData.Standards.XY(j,1) < MapSize(2)    % not tested
                            DataIt(j,i) = app.XMapToolsData.MapData.It.Data(WhereMap).Map(app.XMapToolsData.Standards.XY(j,2),app.XMapToolsData.Standards.XY(j,1));
                        end
                    end
                else
                    ElMap{i} = 'none';
                end
                
            end
            
            app.XMapToolsData.Standards.ElMap = ElMap;
            app.XMapToolsData.Standards.DataIt = DataIt;
            app.XMapToolsData.Standards.DataItAv = DataItAv;
            
            Standards_CalculateMapPosition(app);
            
            close(app.WaitBar);
            
        end
        
        function Standards_CalculateMapPosition(app)
            
            app.WaitBar = uiprogressdlg(app.XMapTools_GUI,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'XMapTools is running numbers';
            
            Standards = app.XMapToolsData.Standards;
            
            % Map size consistency was already checked!
            
            MapSize = size(app.XMapToolsData.MapData.It.Data(1).Map);
            Xmin = 1;
            Xmax = MapSize(2);
            Ymin = 1;
            Ymax = MapSize(1);
            
            dX = 2;
            Xlim = 30;
            Ylim = 30;
            Xi = [-Xlim:dX:Xlim];
            Yi = [-Ylim:dX:Ylim];
            
            X = Standards.XY(:,1);
            Y = Standards.XY(:,2);
            
            [Xshift,Yshift] = meshgrid(Xi,Yi);
            
            NbC = numel(Xshift);
            NbR = length(X);
            
            Xseries = repmat(X,1,NbC)+repmat(Xshift(:)',NbR,1);
            Yseries = repmat(Y,1,NbC)+repmat(Yshift(:)',NbR,1);
            
            IdxInMap = find(Xseries(:) >= Xmin & Xseries(:) <= Xmax & Yseries(:) >= Ymin & Yseries(:) <= Ymax);
            
            ItSeries = zeros(NbR*NbC,length(Standards.ElemImport));
            
            Yind = Yseries(IdxInMap);
            Xind = Xseries(IdxInMap);
            
            Positions = (Xind-1)*Ymax + Yind;
            
            for i = 1:length(Standards.ElemImport)
                
                ElMap = Standards.ElMap{i};
                ListMap = app.XMapToolsData.MapData.It.Names;
                
                Idx = find(ismember(ListMap,ElMap));
                
                if ~isempty(Idx)
                    Map = app.XMapToolsData.MapData.It.Data(Idx).Map;
                    ItSeries(IdxInMap,i) = Map(Positions);
                end
            end
            
            ItSeriesMtx = zeros(NbR,NbC,length(Standards.ElemImport));
            OxSeriesMtx = zeros(NbR,NbC,length(Standards.ElemImport));
            
            CorrMap = zeros(length(Yi),length(Xi),length(Standards.ElemImport));
            
            for i = 1:length(Standards.ElemImport)
                ItSeriesMtx(:,:,i) = reshape(ItSeries(:,i),NbR,NbC);
                
                OxSeries = repmat(Standards.DataPro(:,i),1,NbC);
                %OxSeriesNaN = zeros(NbR,NbC);
                %OxSeriesNaN(IdxInMap) = OxSeries(IdxInMap);
                
                OxSeriesMtx(:,:,i) = OxSeries; %OxSeriesNaN;
                
                % Calculate the correlation coefficients
                
                CorrCoefs = NaN(NbC,1);
                for j = 1:NbC
                    CorrCoefs(j) = corr(OxSeriesMtx(:,j,i),ItSeriesMtx(:,j,i));
                end
                CorrMap(:,:,i) = reshape(CorrCoefs,length(Yi),length(Xi));
            end
            
            WhereNan = find(isnan(CorrMap));
            CorrMap(WhereNan) = zeros(size(WhereNan));
            
            app.XMapToolsData.Standards.MapCorr_Xi = Xi;
            app.XMapToolsData.Standards.MapCorr_Yi = Yi;
            app.XMapToolsData.Standards.MapCorr_Data = CorrMap;
            
            app.XMapToolsData.Standards.MapCorr_LSm = sqrt(sum((CorrMap+1).^2,3));
            
            close(app.WaitBar)
        end
        
        function Standards_MovingStdROI_Main(app,ROI)
            
            NodeData = app.TreeData_Additional.SelectedNodes.NodeData;
            
            if isequal(NodeData(2),0)
                uialert(gcbf,'Select a point in the secondary menu to edit his position!','XMapTools – Error');
                %errordlg('Select a point in the secondary menu to edit his position!')
                return
            end
            
            app.XMapToolsData.Standards.XY(NodeData(2),:) = round(ROI.Position);
            
            %keyboard
            
            Standards_UpdateIntensities(app);
            TreeData_AdditionalSelectionChanged(app,0);
            
            
            % We should recalculate the intensities
            % We should change the X-Y coordinates in the display panel
            
            
            
            %             AppId = app.Id_StandardViewer;
            %             if ~isempty(AppId)
            %                 AppId.Xm.Value = round(ROI.Position(1));
            %                 AppId.Ym.Value = round(ROI.Position(2));
            %             end
            
        end
        
        function [SegNames,Ranges] = Segmentation_ExtractDetails(app)
            
            NodeData = app.TreeData_Additional.SelectedNodes.NodeData;
            
            NbMax = length(app.XMapToolsData.SegScheme.Data(NodeData(3)).Names);
            
            count = 0;
            for i = 1:NbMax
                RangeI = app.XMapToolsData.SegScheme.Data(NodeData(3)).ROI(i).Coordinates;
                if ~isempty(RangeI)
                    count = count+1;
                    SegNames{count} = app.XMapToolsData.SegScheme.Data(NodeData(3)).Names{i};
                    Ranges(count,:) = RangeI;
                end
            end
        end
        
        function Segmentation_PerformSegmentation(app,SelectedImages,SegNames,Ranges)
            
            NbImages = length(SelectedImages);
            
            ImageData = app.XMapToolsData.MapData.Ct.Data(SelectedImages(1)).Map;
            SizeMap = size(ImageData);
            
            ROI = zeros([SizeMap,NbImages],'int8');
            ROIfilter = zeros([SizeMap,NbImages],'int8');
            
            NbSeg = length(Ranges);
            
            %ROI = zeros([SizeMap,NbImages]);
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools');
            app.WaitBar.Message = 'XMapTools is running numbers (Segmentation), please wait ...';
            
            GradientThreshold = app.Segment_ErosionFilterSpinner.Value;
            FilterOrder = app.Segment_InterpOrderSpinner.Value;
            
            CountWB = 0;
            for i = 1:NbImages
                ImageTEMP = app.XMapToolsData.MapData.Ct.Data(SelectedImages(i)).Map;
                SegTEMP = zeros(SizeMap);
                for j = 1:NbSeg
                    Idx = find(ImageTEMP > Ranges(j,1) & ImageTEMP < Ranges(j,2));
                    if ~isempty(Idx)
                        SegTEMP(Idx) = j*ones(size(Idx));
                    end
                end
                %figure, imagesc(SegTEMP), axis image, colorbar, colormap([0,0,0;parula(68)])
                
                % Apply filter
                if app.Segment_ErosionFilterCheckBox.Value
                    Threshold = app.Segment_ErosionFilterSpinner.Value;
                    Textured = CalculateTextureFilter(app,ImageTEMP);
                    
                    %figure, imagesc(Textured), axis image, colorbar
                    
                    FilterIdx = find(Textured(:)>Threshold);
                    
                    LowerLim = -0.05;
                    
                    SegTEMP(FilterIdx) = LowerLim*ones(size(FilterIdx));
                    
                    
                    %figure, imagesc(Gmag), axis image, colorbar, colormap([0,0,0;parula(68)])
                end
                
                if app.Segment_InterpFilterCheckBox.Value
                    K = ordfilt2(SegTEMP,FilterOrder^2,true(FilterOrder));
                    IdxOk = find(K(FilterIdx) > 0);
                    Val = K(FilterIdx(IdxOk));
                    Val = round(Val);
                    SegTEMP(FilterIdx(IdxOk)) = Val;
                end
                
                %keyboard
                
                %figure, imagesc(Ko), axis image, colorbar, colormap([0,0,0;parula(68)])
                
                %keyboard
                
                ROI(:,:,i) = SegTEMP;
                
                CountWB = CountWB+1;
                if CountWB > 5
                    app.WaitBar.Value = i/NbImages;
                    CountWB = 0;
                end
                
            end
            app.WaitBar.Value = 1;
            close(app.WaitBar);
            
            IdX = length(app.XMapToolsData.MapData.ROI.Names)+1;
            
            if isequal(NbImages,1)
                Name = ['ROI_#',num2str(SelectedImages)];
            else
                Name = 'ROI';
            end
            
            app.XMapToolsData.MapData.ROI.Names{IdX} = Name;
            app.XMapToolsData.MapData.ROI.Types(IdX) = 1;
            app.XMapToolsData.MapData.ROI.Data(IdX).Names = SegNames;
            app.XMapToolsData.MapData.ROI.Data(IdX).ROI = ROI;
            
            p = uitreenode(app.Node_ROI,'Text',char(Name),'NodeData',[7,IdX]);
            app.Node_It.ContextMenu = app.ContextMenu_MainTree_C;
            
            app.SaveRequired = 1;
        end
        
        
        
        
        function Sampling_ROI_changed_shape(app,~)
            
            % Extract data
            ImageData = PlotMap_ExtractPlottedImage(app);
            mask = createMask(app.ROI_sampling);
            
            Ind = find(mask & ImageData > 0);
            
            app.EditField_LivePosition.Visible = 'off';
            app.EditField_LivePeak.Value = [num2str(mean(double(ImageData(Ind))),'%3.4g'),' ± ',num2str(std(double(ImageData(Ind))),'%3.4g')];
            app.EditField_LivePeak.Visible = 'on';
            
            DataSelected = ImageData(Ind);
            histogram(app.Sampling_Plot1,DataSelected(:))
            xlabel(app.Sampling_Plot1,'Value')
            ylabel(app.Sampling_Plot1,'#')
            title(app.Sampling_Plot1,['Px selected = ',num2str(numel(Ind))])
            app.Sampling_Plot1.Visible = 'on';
            
            app.hVerticalLines(1).Value = double(mean(ImageData(Ind)));
            
            %             if ~isempty(app.SliderPeakHandle)
            %                 app.SliderPeakHandle.Value = mean(ImageData(Ind));
            %             else
            %                 app.SliderPeakHandle = xline(app.FigHistLive, double(mean(ImageData(Ind))),'-','LineWidth',3,'Color',[0.57,0.00,0.69]);
            %             end
            
        end
        
        function LesData = Sampling_CalculateLineXY(app,X,Y,ImageData)
            
            LesX = [];
            LesY = [];
            for i = 2:length(X)
                Xs = [X(i-1),X(i)];
                Ys = [Y(i-1),Y(i)];
                
                LX = max(Xs) - min(Xs);
                LY = max(Ys) - min(Ys);
                
                LZ = round(sqrt(LX^2 + LY^2));
                
                if Xs(2) > Xs(1)
                    LesXs = Xs(1):(Xs(2)-Xs(1))/(LZ-1):Xs(2);
                elseif Xs(2) < Xs(1)
                    LesXs = Xs(1):-(Xs(1)-Xs(2))/(LZ-1):Xs(2);
                else
                    LesXs = ones(LZ,1) * Xs(1);
                end
                
                if Ys(2) > Ys(1)
                    LesYs = Ys(1):(Ys(2)-Ys(1))/(LZ-1):Ys(2);
                elseif Ys(2) < Ys(1)
                    LesYs = Ys(1):-(Ys(1)-Ys(2))/(LZ-1):Ys(2);
                else
                    LesYs = ones(LZ,1) * Ys(1);
                end
                
                LesX(end+1:end+length(LesXs)) = LesXs;
                LesY(end+1:end+length(LesYs)) = LesYs;
            end
            
            % Indexation
            XCoo = 1:1:length(ImageData(1,:));
            YCoo = 1:1:length(ImageData(:,1));
            
            for i = 1 : length(LesX)
                [V(i,1), IdxAll(i,1)] = min(abs(XCoo-LesX(i))); % Index X
                [V(i,2), IdxAll(i,2)] = min(abs(YCoo-LesY(i))); % Index Y
            end
            
            LesData = zeros(size(IdxAll,1),3);
            for i=1:size(IdxAll,1) % Quanti
                LesData(i,1) = i;
                if i>1
                    LesData(i,2) = LesData(i-1,2)+sqrt((LesX(i)-LesX(i-1))^2+(LesY(i)-LesY(i-1))^2);
                end
                LesData(i,3) = ImageData(IdxAll(i,2),IdxAll(i,1));
                if LesData(i,3) == 0
                    LesData(i,3) = NaN;
                end
            end
        end
        
        function Sampling_ROI_changed_line(app,~)
            
            ImageData = PlotMap_ExtractPlottedImage(app);
            Positions = app.ROI_sampling.Position;
            
            X = Positions(:,1);
            Y = Positions(:,2);
            
            [LesData] = Sampling_CalculateLineXY(app,X,Y,ImageData);
            
            app.TabGroup.SelectedTab = app.SamplingTab;
            
            Res = app.ResolutionField.Value;
            
            plot(app.Sampling_Plot1,LesData(:,2)*Res,LesData(:,3),'-k')
            hold(app.Sampling_Plot1,'on');
            scatter(app.Sampling_Plot1,LesData(:,2)*Res,LesData(:,3),[],LesData(:,3),'filled','MarkerEdgeColor',[0.5 .5 .5])
            
            xlabel(app.Sampling_Plot1,'distance µm')
            
            colorbar(app.Sampling_Plot1,'horizontal');
            colormap(app.Sampling_Plot1,app.ColorMapValues);
            CLim = caxis(app.FigMain);
            caxis(app.Sampling_Plot1,CLim);
            hold(app.Sampling_Plot1,'off');
            
            app.Sampling_Plot1.Visible = 'on';
            app.Sampling_Plot2.Visible = 'off';
            
            app.Sampling_ExportButton.Enable = 'on';
            app.Sampling_ResetButton.Enable = 'on';
            
        end
        
        function Sampling_ROI_changing_strip(app,~)
            % Orientation indication using label (4.3):
            RotationAngle = app.ROI_sampling.RotationAngle;
            if RotationAngle >= 337.5 || RotationAngle < 22.5
                app.ROI_sampling.Label = '>>';
            elseif RotationAngle >= 22.5 && RotationAngle < 67.5
                app.ROI_sampling.Label = '/^';
            elseif RotationAngle >= 67.5 && RotationAngle < 112.5
                app.ROI_sampling.Label = '^^';
            elseif RotationAngle >= 112.5 && RotationAngle < 157.5
                app.ROI_sampling.Label = '^\';
            elseif RotationAngle >= 157.5 && RotationAngle < 202.5
                app.ROI_sampling.Label = '<<';
            elseif RotationAngle >= 202.5 && RotationAngle < 247.5
                app.ROI_sampling.Label = 'v/';
            elseif RotationAngle >= 247.5 && RotationAngle < 292.5
                app.ROI_sampling.Label = 'vv';
            elseif RotationAngle >= 292.5 && RotationAngle < 337.5
                app.ROI_sampling.Label = '\v';
            end
            
        end
        
        function Sampling_ROI_changed_strip(app,~)
            
            ImageData = PlotMap_ExtractPlottedImage(app);
            Verticles = app.ROI_sampling.Vertices;
            
            % Verticle 1 --> Point 4 (in XMapTools 3)
            % Verticle 2 --> Point 6
            % Verticle 3 --> Point 7
            % Verticle 4 --> Point 5
            try
                [MatrixProfils,Distances] = ExtractMatrixProfils(app,ImageData,Verticles(1,1),Verticles(1,2),Verticles(2,1),Verticles(2,2),Verticles(3,1),Verticles(3,2));   % previously X4,Y4,X6,Y6,X7,Y7
            catch
                return
            end
            
            TheMean = zeros(1,size(MatrixProfils,1));
            TheMedian = zeros(1,size(MatrixProfils,1));
            TheStd = zeros(1,size(MatrixProfils,1));
            for i=1:size(MatrixProfils,1)
                WhereValues = find(MatrixProfils(i,:) > 1e-19);
                TheMean(i) = mean(MatrixProfils(i,WhereValues));
                TheMedian(i) = median(MatrixProfils(i,WhereValues));
                TheStd(i) = std(MatrixProfils(i,WhereValues));
            end
            
            Nb = zeros(1,size(MatrixProfils,1));
            for i=1:size(MatrixProfils,1)
                Nb(i) = length(find(MatrixProfils(i,:)>1e-19));
            end
            FractPer = Nb/size(MatrixProfils,2)*100;
            
            % This has to be done later tp avoir NaN in the computation of the mean
            WhereZero = find(MatrixProfils(:) < 1e-19);
            MatrixProfils(WhereZero) = nan(size(WhereZero));
            
            app.TabGroup.SelectedTab = app.SamplingTab;
            
            Res = app.ResolutionField.Value;
            
            plot(app.Sampling_Plot1,Res*Distances,TheMedian,'-b','linewidth',2);
            hold(app.Sampling_Plot1,'on')
            plot(app.Sampling_Plot1,Res*Distances,TheMean,'-r','linewidth',2);
            app.Sampling_Plot1.Box = 'on';
            hold(app.Sampling_Plot1,'off')
            
            xlabel(app.Sampling_Plot1,'Distance µm')
            
            app.Sampling_Plot1.Title.String = 'median (blue) & mean (red)';
            
            if size(MatrixProfils,2) > 1000
                plot(app.Sampling_Plot2,repmat(Res*Distances',1,size(MatrixProfils,2)),MatrixProfils,'-','markersize',1,'color', [0.4 0.4 0.4 0.05]);
            elseif size(MatrixProfils,2) > 20
                plot(app.Sampling_Plot2,repmat(Res*Distances',1,size(MatrixProfils,2)),MatrixProfils,'-','markersize',1,'color', [0.4 0.4 0.4 0.2]);
            else
                plot(app.Sampling_Plot2,repmat(Res*Distances',1,size(MatrixProfils,2)),MatrixProfils,'-','markersize',1,'color', [0.4 0.4 0.4 0.5]);
            end
            app.Sampling_Plot2.Title.String = [num2str(size(MatrixProfils,2)),' transects (strip)'];
            
            xlabel(app.Sampling_Plot2,'Distance µm')
            
            app.Sampling_Plot1.Visible = 'on';
            app.Sampling_Plot2.Visible = 'on';
            
            app.Sampling_ExportButton.Enable = 'on';
            app.Sampling_ResetButton.Enable = 'on';
            
            
            %             plot(app.GCA_ExtFigure,repmat(Distances',1,size(MatrixProfils,2)),MatrixProfils,'color', [0.5 0.5 0.5]);
            %             hold(app.GCA_ExtFigure,'on')
            %             plot(app.GCA_ExtFigure,Distances,TheMedian,'-b','linewidth',2);
            %             plot(app.GCA_ExtFigure,Distances,TheMean,'-r','linewidth',2);
            %             app.GCA_ExtFigure.Box = 'on';
            %             hold(app.GCA_ExtFigure,'off')
        end
        
        function [MatrixProfils,Distances] = ExtractMatrixProfils(app,Data,X4,Y4,X6,Y6,X7,Y7)
            %
            
            [lesXOri,lesYOri] = LineCoordinate(app,round([X6,Y6]),round([X4,Y4]),size(Data));
            [lesXref,lesYref] = LineCoordinate(app,round([X6,Y6]),round([X7,Y7]),size(Data));
            
            MatrixProfils = zeros(length(lesXref),length(lesXOri));  % lines = profils % col = Exp
            
            for i=1:length(lesXOri)
                
                DeltaX = lesXOri(1)-lesXOri(i);
                DeltaY = lesYOri(1)-lesYOri(i);
                
                lesX = lesXref - DeltaX;
                lesY = lesYref - DeltaY;
                
                lesX = round(lesX);
                lesY = round(lesY);
                
                for j=1:length(lesY)
                    
                    if lesX(j) >= 1 && lesY(j) >= 1 && lesY(j) <= size(Data,1) && lesX(j) <= size(Data,2)
                        MatrixProfils(j,i) = Data(lesY(j),lesX(j));
                    else
                        MatrixProfils(j,i) = NaN;
                    end
                end
                
                %NbOK = length(find(lesX>0 & lesY>0));
            end
            
            
            % Calculate the distances
            dX = lesX(2) - lesX(1);
            dY = lesY(2) - lesY(1);
            
            Dist2Steps = sqrt(dX^2+dY^2);
            Distances = Dist2Steps*([0:1:length(lesX)-1]);
            
            
        end
        
        function [lesX,lesY] = LineCoordinate(app,A,B,MapSize)
            %
            
            X = [A(1),B(1)];
            Y = [A(2),B(2)];
            
            LX = max(X) - min(X);
            LY = max(Y) - min(Y);
            
            LZ = round(sqrt(LX^2 + LY^2));
            
            if X(2) > X(1)
                LesX = X(1):(X(2)-X(1))/(LZ-1):X(2);
            elseif X(2) < X(1)
                LesX = X(1):-(X(1)-X(2))/(LZ-1):X(2);
            else
                LesX = ones(LZ,1) * X(1);
            end
            
            if Y(2) > Y(1)
                LesY = Y(1):(Y(2)-Y(1))/(LZ-1):Y(2);
            elseif Y(2) < Y(1)
                LesY = Y(1):-(Y(1)-Y(2))/(LZ-1):Y(2);
            else
                LesY = ones(LZ,1) * Y(1);
            end
            
            
            XCoo = 1:1:MapSize(2);
            YCoo = 1:1:MapSize(1);
            
            for i = 1 : length(LesX)
                [V(i,1), IdxAll(i,1)] = min(abs(XCoo-LesX(i))); % Index X
                [V(i,2), IdxAll(i,2)] = min(abs(YCoo-LesY(i))); % Index Y
                
                
                if V(i,1) > 1 && IdxAll(i,1) < 2
                    IdxAll(i,1) = 1-abs(V(i,1));
                end
                
                if V(i,2) > 1 && IdxAll(i,2) < 2
                    IdxAll(i,2) = 1-abs(V(i,2));
                end
                
                
                if V(i,2) > 1 && IdxAll(i,2) > 2      % Y
                    IdxAll(i,2) = MapSize(1)+abs(V(i,2));
                end
                
                if V(i,1) > 1 && IdxAll(i,1) > 2
                    IdxAll(i,1) = MapSize(2)+abs(V(i,1));
                end
                
            end
            
            lesX = IdxAll(:,1);
            lesY = IdxAll(:,2);
        end
        
        
        
        function [ResampledMap] = ResampleMapFct(app, Matrix, SizePixelIni, SizePixelFinal )
            %[ ResampledMap ] = ResampleMap(app, Matrix, SizePixelIni, SizePixelFinal )
            %   Resample a Map of pixel size SizePixelIni in order to get a map with
            %   SizePixelFinal
            
            F = griddedInterpolant(double(Matrix));
            Ratio=SizePixelIni/SizePixelFinal; %nb pixels equivalent to 1 initial pixel
            
            [sx,sy]=size(Matrix);
            xq=(0:1/Ratio:sx)';
            yq=(0:1/Ratio:sy)';
            vq=(F({xq,yq}));
            ResampledMap=vq;
            
        end
        
        
        function [MatrixSF,ElementsList,NameResult] = Call_SF_External(app,QtElNames,QtData,QtName)
            
            if isequal(app.SF_Function_CheckBox.Value,1)
                Mode = 1;
            elseif isequal(app.SF_OxygenNorm_CheckBox.Value,1)
                Mode = 2;
            else
                Mode = 3;
            end
            
            %app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            %app.WaitBar.Message = 'XMapTools is running numbers (structural formula calculation)';
            
            try
                switch Mode
                    
                    case 1
                        MinList = app.SF_MineralList.Items;
                        FctList = app.SF_FunctionList.Items;
                        
                        WhereMin = find(ismember(MinList,app.SF_MineralList.Value));
                        WhereMin = app.ExternalFunctions.IdxSF(WhereMin);
                        WhereFct = find(ismember(FctList,app.SF_FunctionList.Value));
                        
                        ExtFct = char(app.ExternalFunctions.Min(WhereMin).SF.FileName{WhereFct});
                        ExtFctLabel = char(app.ExternalFunctions.Min(WhereMin).SF.Name{WhereFct});
                        
                        [MatrixSF,ElementsList] = Core_Call_SF(QtData,QtElNames,ExtFct,app.ElOxDataDef);
                        
                        
                        NameResult = [QtName,' ',ExtFctLabel];
                        
                        
                        
                    case 2
                        
                        [MatrixSF,ElementsList] = SF_OxNorm(QtData,QtElNames,app.SF_NbOx.Value,app.ElOxDataDef);
                        
                        MatrixSF(:,end+1) = sum(MatrixSF,2);
                        ElementsList{end+1} = 'Sum';
                        
                        WhereMg = find(ismember(ElementsList,'Mg'));
                        WhereFe = find(ismember(ElementsList,'Fe'));
                        
                        if ~isempty(WhereMg) && ~isempty(WhereFe)
                            FeMg = MatrixSF(:,WhereMg) + MatrixSF(:,WhereFe);
                            MatrixSF(:,end+1) = MatrixSF(:,WhereMg)./FeMg;
                            ElementsList{end+1} = 'XMg';
                        end
                        
                        WhereCa = find(ismember(ElementsList,'Ca'));
                        WhereNa = find(ismember(ElementsList,'Na'));
                        WhereK = find(ismember(ElementsList,'K'));
                        
                        if ~isempty(WhereCa) && ~isempty(WhereNa) && ~isempty(WhereK)
                            CaNaK = MatrixSF(:,WhereCa) + MatrixSF(:,WhereNa) + MatrixSF(:,WhereK);
                            
                            MatrixSF(:,end+1) = MatrixSF(:,WhereCa)./CaNaK;
                            ElementsList{end+1} = 'XCa';
                            MatrixSF(:,end+1) = MatrixSF(:,WhereNa)./CaNaK;
                            ElementsList{end+1} = 'XNa';
                            MatrixSF(:,end+1) = MatrixSF(:,WhereK)./CaNaK;
                            ElementsList{end+1} = 'XK';
                        end
                        
                        NameResult = [QtName,' (auto-SF, ',num2str(app.SF_NbOx.Value),'-oxygen basis)'];
                        
                        
                    case 3
                        [MatrixSF,ElementsList] = SF_CatNorm(QtData,QtElNames,app.SF_NbCat.Value,app.ElOxDataDef);
                        
                        WhereMg = find(ismember(ElementsList,'Mg'));
                        WhereFe = find(ismember(ElementsList,'Fe'));
                        
                        if ~isempty(WhereMg) && ~isempty(WhereFe)
                            FeMg = MatrixSF(:,WhereMg) + MatrixSF(:,WhereFe);
                            MatrixSF(:,end+1) = MatrixSF(:,WhereMg)./FeMg;
                            ElementsList{end+1} = 'XMg';
                        end
                        
                        WhereCa = find(ismember(ElementsList,'Ca'));
                        WhereNa = find(ismember(ElementsList,'Na'));
                        WhereK = find(ismember(ElementsList,'K'));
                        
                        if ~isempty(WhereCa) && ~isempty(WhereNa) && ~isempty(WhereK)
                            CaNaK = MatrixSF(:,WhereCa) + MatrixSF(:,WhereNa) + MatrixSF(:,WhereK);
                            
                            MatrixSF(:,end+1) = MatrixSF(:,WhereCa)./CaNaK;
                            ElementsList{end+1} = 'XCa';
                            MatrixSF(:,end+1) = MatrixSF(:,WhereNa)./CaNaK;
                            ElementsList{end+1} = 'XNa';
                            MatrixSF(:,end+1) = MatrixSF(:,WhereK)./CaNaK;
                            ElementsList{end+1} = 'XK';
                        end
                        
                        NameResult = [QtName,' (auto-SF, ',num2str(app.SF_NbCat.Value),'-cation basis)']
                end
            catch ME
                uialert(app.XMapTools_GUI,{'Error in the computation of the structural formula:',ME.message},'XMapTools – Error');
                close(app.WaitBar);
                return
            end
            
            
        end
        
        function Check4Config(app)
            
            Path2Config = which('config_xmaptools.mat');
            
            load('config_xmaptools.mat'); % for compilation
            
            config = CheckConfigHealth(app,config);
            app.config = config;
            
            if isempty(app.config.xmaptools.last_path) || isempty(app.config.xmaptools.setup_path)
                waitfor(warndlg({['You are now ready to use ',char(app.XMapTools_VER)], ' ','After pressing "OK", a Finder window will appear and you will be asked to select a working directory (i.e. where your map files are located).',' '}, 'XMapTools setup complete', 'modal'));
                
                app.config.xmaptools.setup_path = Path2Config(1:end-20);
                app.config.xmaptools.last_path = cd;
                
                UpdateConfig(app);
                
                Path2Config;
                cd;
            end
            
            if isempty(app.config.bingoantidote.setup_path)
                Path2ConfigBA = which('BingoAntidote.mlapp');
                app.config.bingoantidote.setup_path = Path2ConfigBA(1:end-19);
                
                UpdateConfig(app);
            end
            
            %             if isempty(app.config.xthermotools.theriak_path)
            %                 if ismac
            %                     app.config.xthermotools.theriak_path = [app.config.xmaptools.setup_path,'Addons/XThermoTools/XTT_Programs/TheriakDominoMAC/'];
            %                 end
            %             end
            
            config = app.config;
            % config = CheckConfigHealth(app,config);
            
            if isdeployed
                try
                    save(Path2Config,'config');
                catch ME
                    waitfor(errordlg({'XMapTools cannot save the configuration and therefore cannot work on this computer',['File: ',Path2Config],'Check permissions and authorize writting to solve this problem'}));
                    close(app.WaitBar);
                    diary('off');
                    delete(app.XMapTools_GUI);
                end
            else
                save(Path2Config,'config');
            end
        end
        
        function UpdateConfig(app)
            app.config.xmaptools.last_path = cd;
            Path2Config = which('config_xmaptools.mat');
            
            config = app.config;
            config = CheckConfigHealth(app,config);
            
            save(Path2Config,'config');
            disp('config file saved!')
        end
        
        function config = CheckConfigHealth(app,config)
            % Check that config is not altered
            if ~isfield(config,'xmaptools')
                config.xmaptools.setup_path = '';
                config.xmaptools.last_path = '';
                disp('Something went wrong with the config file (field: xmaptools)!');
            end
            if ~isfield(config.xmaptools,'setup_path')
                config.xmaptools.setup_path = '';
                disp('Something went wrong with the config file (field: setup_path)!')
            end
            if ~isfield(config.xmaptools,'last_path')
                config.xmaptools.last_path = '';
                disp('Something went wrong with the config file (field: last_path)!')
            end
            %             if ~isfield(config,'xthermotools')
            %                 config.xthermotools.theriak_path = '';
            %                 disp('Something went wrong with the config file (field: xthermotools)!')
            %             end
            %             if ~isfield(config.xthermotools,'theriak_path')
            %                 config.xmaptools.theriak_path = '';
            %                 disp('Something went wrong with the config file (field: theriak_path)!')
            %             end
            % Bingo Antidote (2022)
            if ~isfield(config,'bingoantidote')
                config.bingoantidote.setup_path = '';
                config.bingoantidote.theriak_path = '';
                disp('Something went wrong with the config file (Bingo-Antidote field: xthermotools)!')
            end
            if ~isfield(config.bingoantidote,'setup_path')
                config.bingoantidote.setup_path = '';
                disp('Something went wrong with the config file (Bingo-Antidote field: setup_path)!')
            end
            if ~isfield(config.bingoantidote,'theriak_path')
                config.bingoantidote.theriak_path = '';
                disp('Something went wrong with the config file (Bingo-Antidote field: theriak_path)!')
            end
        end
        
        function DeactivatePlotZoomPanOptions(app)
            
            if ~isempty(app.FigMain)
                if ~isempty(app.FigMain.Toolbar.Children)
                    app.FigMain.Toolbar.Children(2).Value = 'off';
                    matlab.graphics.interaction.webmodes.toggleMode(app.FigMain,'pan',app.FigMain.Toolbar.Children(2).Value)
                    
                    app.FigMain.Toolbar.Children(3).Value = 'off';
                    matlab.graphics.interaction.webmodes.toggleMode(app.FigMain,'zoom',app.FigMain.Toolbar.Children(3).Value)
                    
                    app.FigMain.Toolbar.Children(4).Value = 'off';
                    matlab.graphics.interaction.webmodes.toggleMode(app.FigMain,'zoomout',app.FigMain.Toolbar.Children(4).Value)
                end
            end
            
        end
        
        
        
        function MovingROI_PositionMap1(app,ROI)
            
            app.ROI_Position_Map2.Position = app.ROI_Position_Map1.Position;
            app.Std_Shift_X.Value = round(app.ROI_Position_Map1.Position(1));
            app.Std_Shift_Y.Value = round(app.ROI_Position_Map1.Position(2));
            
            %
            if ~isempty(app.ROI_std)
                % ROI are displayed in XMapTools
                XY = app.XMapToolsData.Standards.XY;
                
                if isequal(size(XY,1),length(app.ROI_std))
                    for i = 1:length(app.ROI_std)
                        app.ROI_std(i).ROI.Position = [XY(i,1)+app.Std_Shift_X.Value,XY(i,2)+app.Std_Shift_Y.Value];
                    end
                end
            end
            
            if isequal(app.Std_Shift_X.Value,0) && isequal(app.Std_Shift_Y.Value,0)
                app.StdAll_Synchronize.Enable = 'off';
            else
                app.StdAll_Synchronize.Enable = 'on';
            end
        end
        
        function MovingROI_PositionMap2(app,ROI)
            
            app.ROI_Position_Map1.Position = app.ROI_Position_Map2.Position;
            app.Std_Shift_X.Value = round(app.ROI_Position_Map2.Position(1));
            app.Std_Shift_Y.Value = round(app.ROI_Position_Map2.Position(2));
            
            %
            if ~isempty(app.ROI_std)
                % ROI are displayed in XMapTools
                XY = app.XMapToolsData.Standards.XY;
                
                if isequal(size(XY,1),length(app.ROI_std))
                    for i = 1:length(app.ROI_std)
                        app.ROI_std(i).ROI.Position = [XY(i,1)+app.Std_Shift_X.Value,XY(i,2)+app.Std_Shift_Y.Value];
                    end
                end
            end
            
            if isequal(app.Std_Shift_X.Value,0) && isequal(app.Std_Shift_Y.Value,0)
                app.StdAll_Synchronize.Enable = 'off';
            else
                app.StdAll_Synchronize.Enable = 'on';
            end
        end
        
        function str = Table2Str_LBC(app,d)
            
            %str = '';
            str = sprintf('%s\t%s',char(d{1,1}),d{1,2});
            for i = 2:size(d,1)
                str = sprintf('%s\n%s\t%f',str,char(d{i,1}),d{i,2});
            end
            
        end
        
        function str = Table2Str_LBC_UNC(app,d)
            
            %str = '';
            str = sprintf('%s\t%s\t%s\t%s',char(d{1,1}),d{1,2},d{1,3},d{1,4});
            for i = 2:size(d,1)
                str = sprintf('%s\n%s\t%f\t%f\t%f',str,char(d{i,1}),d{i,2},d{i,3},d{i,4});
            end
            
        end
        
        function str = Table2Str_LOD(app,d)
            %str = '';
            str = sprintf('%s\t%s\t%s',char(d{1,1}),d{1,2},d{1,3});
            for i = 2:size(d,1)
                str = sprintf('%s\n%s\t%f\t%s',str,char(d{i,1}),d{i,2},d{i,3});
            end
        end
        
        function str = Table2Str_Modes(app,d)
            %str = '';
            str = sprintf('%s\t%s\t%s',char(d{1,1}),d{1,2},d{1,3});
            for i = 2:size(d,1)
                str = sprintf('%s\n%s\t%f\t%f',str,char(d{i,1}),d{i,2},d{i,3});
            end
        end
        
        function str = Table2Str_ME(app,d)
            %str = '';
            str = sprintf('%s\t%s\t%s\t%s',char(d{1,1}),d{1,2},d{1,3},d{1,4});
            for i = 2:size(d,1)
                str = sprintf('%s\n%s\t%f\t%f\t%f',str,char(d{i,1}),d{i,2},d{i,3},d{i,4});
            end
        end
        
        
        
        function ApplyCursorMode(app)
            
            switch app.DataCursorMode
                case 1
                    datacursormode(app.XMapTools_GUI, 'on');
                    app.Button_FigMain_CursorInfo.Icon = 'XXX_CursorMode_Deactivate.png';
                    app.Button_FigMain_CursorInfo.Tooltip = 'Disable data tips';
                    
                case 0
                    datacursormode(app.XMapTools_GUI, 'off');
                    app.Button_FigMain_CursorInfo.Icon = 'XXX_CursorMode_Activate.png';
                    app.Button_FigMain_CursorInfo.Tooltip = 'Enable data tips';
                    
                    delete(findall(app.FigMain,'Type','hggroup'));
            end
            
        end
        
        
        function Textured = CalculateTextureFilter(app,ImageTEMP)
            
            Order = app.Segment_InterpOrderFilterGBSpinner.Value;
            
            switch app.SchemeDropDown.Value
                case 'Local Gradient'
                    [Gmag,Gdir] = imgradient(ImageTEMP);
                    Textured = rescale(Gmag);
                    
                case 'Local Standard Deviation'
                    S = stdfilt(ImageTEMP,ones(Order));
                    Textured = rescale(S);
                    
                case 'Local Entropy'
                    E = entropyfilt(ImageTEMP);
                    Textured = rescale(E);
                    
                case 'Local Range'
                    R = rangefilt(ImageTEMP,ones(Order));
                    Textured = rescale(R);
                    
            end
            
            
            
        end
        
        
        function ExtractData4MultiplePlot(app)
            
            Idx = length(app.XMapToolsData.MapData.Im.Names)+1;
            
            app.XMapToolsData.MapData.Im.Names{Idx} = 'temp';
            app.XMapToolsData.MapData.Im.Types(Idx) = 0;
            
            Compt = 0;
            for i = 1:length(app.TreeData_Main.SelectedNodes)
                
                MapCode = app.TreeData_Main.SelectedNodes(i).NodeData;
                
                Data2Plot_Temp = [];
                Info_MapName_Temp = [];
                
                if isequal(MapCode(1),1) % It
                    if MapCode(2) > 0
                        
                        Data2Plot_Temp = app.XMapToolsData.MapData.It.Data(MapCode(2)).Map;
                        Info_MapName_Temp = app.XMapToolsData.MapData.It.Names{MapCode(2)};
                        
                        if ~isempty(app.TreeData_Additional.SelectedNodes)
                            for j=1:numel(app.TreeData_Additional.SelectedNodes)
                                AddCode(j,:) = app.TreeData_Additional.SelectedNodes(j).NodeData;
                            end
                        else
                            AddCode = [];
                        end
                        
                        % MaskFile selected?
                        if isequal(size(AddCode,2),3) && isequal(MapCode(1),1) % Only for X-ray right now...
                            % check if a mask is selected
                            if isequal(AddCode(1,1),11) && AddCode(1,3) > 1
                                MaskMap = zeros(size(Data2Plot_Temp));
                                SelectedMaskMap = app.XMapToolsData.MapData.MaskFile.Masks(AddCode(1,2)).MaskMap;
                                for j = 1:size(AddCode,1)
                                    MaskInd = find(SelectedMaskMap == AddCode(j,3)-1);
                                    MaskMap(MaskInd) = ones(size(MaskInd));
                                end
                                
                                Data2Plot_Temp = Data2Plot_Temp.*MaskMap;
                            end
                        end
                    end
                    
                elseif isequal(MapCode(1),2) % Qt
                    if MapCode(2) > 0 && MapCode(3) > 0
                        Data2Plot_Temp = app.XMapToolsData.MapData.Qt.Data(MapCode(2)).CData(MapCode(3)).Map;
                        Info_MapName_Temp = [char(app.XMapToolsData.MapData.Qt.Data(MapCode(2)).ElNames(MapCode(3))),' [',char(app.XMapToolsData.MapData.Qt.Names{MapCode(2)}),']'];
                    end
                elseif isequal(MapCode(1),3) % Me
                    if MapCode(2) > 0 && MapCode(3) > 0
                        Data2Plot_Temp = app.XMapToolsData.MapData.Me.Data(MapCode(2)).CData(MapCode(3)).Map;
                        Info_MapName_Temp = char(app.XMapToolsData.MapData.Me.Data(MapCode(2)).ElNames(MapCode(3)));
                    end
                elseif isequal(MapCode(1),4) % Re
                    if MapCode(2) > 0 && MapCode(3) > 0
                        Data2Plot_Temp = app.XMapToolsData.MapData.Re.Data(MapCode(2)).CData(MapCode(3)).Map;
                        Info_MapName_Temp = [char(app.XMapToolsData.MapData.Re.Data(MapCode(2)).Labels(MapCode(3))),' [',char(app.XMapToolsData.MapData.Re.Names{MapCode(2)}),']'];
                    end
                elseif isequal(MapCode(1),5) % Ot
                    if MapCode(2) > 0
                        Data2Plot_Temp = app.XMapToolsData.MapData.Ot.Data(MapCode(2)).Map;
                        Info_MapName_Temp = char(app.XMapToolsData.MapData.Ot.Names{MapCode(2)});
                    end
                elseif isequal(MapCode(1),6) % Ct
                    if MapCode(2) > 0
                        Data2Plot_Temp = app.XMapToolsData.MapData.Ct.Data(MapCode(2)).Map;
                        Info_MapName_Temp = char(app.XMapToolsData.MapData.Ct.Names{MapCode(2)});
                    end
                end
                
                if ~isempty(Data2Plot_Temp)
                    Compt = Compt +1;
                    
                    IdXTemp = find(Data2Plot_Temp ~= inf & Data2Plot_Temp ~= NaN);
                    
                    app.XMapToolsData.MapData.Im.Data(Idx).Labels{Compt} = Info_MapName_Temp;
                    app.XMapToolsData.MapData.Im.Data(Idx).CData(Compt).Map = Data2Plot_Temp;
                    app.XMapToolsData.MapData.Im.Data(Idx).CData(Compt).Lim = [min(Data2Plot_Temp(IdXTemp)),max(Data2Plot_Temp(IdXTemp))];
                    app.XMapToolsData.MapData.Im.Data(Idx).CData(Compt).ColorMap = app.ColorMapValues;
                end
                
            end
            
            if isequal(length(app.TreeData_Main.SelectedNodes),1)
                % single mode
                app.XMapToolsData.MapData.Im.Data(Idx).CData(Compt).Lim = [app.EditField_LiveMin.Value,app.EditField_LiveMax.Value];
                
            end
            
        end
        
        function CorrelationPlot(app,type)
            
            NodeData = app.TreeData_Main.SelectedNodes.NodeData;
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Generating the plot ...';
            
            
            Names = [];
            Data = [];
            
            switch NodeData(1)
                case 1
                    Selected = NodeData(2);
                    Names = app.XMapToolsData.MapData.It.Names;
                    Data = app.XMapToolsData.MapData.It.Data;
                    
                case 2
                    if NodeData(2) > 0
                        Selected = NodeData(3);
                        Names = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElNames;
                        Data = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).CData;
                    end
                    
                case 3
                    if NodeData(2) > 0
                        Selected = NodeData(3);
                        Names = app.XMapToolsData.MapData.Me.Data(NodeData(2)).ElNames;
                        Data = app.XMapToolsData.MapData.Me.Data(NodeData(2)).CData;
                    end
                    
                case 4
                    if NodeData(2) > 0
                        Selected = NodeData(3);
                        Names = app.XMapToolsData.MapData.Re.Data(NodeData(2)).Labels;
                        Data = app.XMapToolsData.MapData.Re.Data(NodeData(2)).CData;
                    end
                    
            end
            
            if isempty(Data) || isempty(Names)
                close(app.WaitBar);
                return
            end
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Select maps for correlation and press OK to continue...';
            waitfor(Selector(app,Names,'Select maps in the list below','Multiple'));
            close(app.WaitBar)
            
            NamesSel = app.ExchangeSelector;
            
            % Add a detection of submasks
            SelectedPx = 1:numel(Data(1).Map(:));
            if ~isempty(app.TreeData_Additional.SelectedNodes)
                if isequal(length(app.TreeData_Additional.SelectedNodes),1)
                    NodeDataAdd = app.TreeData_Additional.SelectedNodes.NodeData;
                    if isequal(NodeDataAdd(1),11)
                        if length(NodeDataAdd) > 3
                            if NodeDataAdd(4) > 1
                                SubMask = app.XMapToolsData.MapData.MaskFile.Masks(NodeDataAdd(2)).SubMask(NodeDataAdd(3)).MaskSelMaskMap;
                                SelectedPx = find(SubMask == NodeDataAdd(4));
                            end
                        end
                    end
                end
            end
            
            CMtx = zeros(numel(SelectedPx),length(NamesSel));
            Compt = 0;
            for i = 1:length(Data)
                if ~isempty(find(ismember(NamesSel,Names{i})))
                    Compt = Compt+1;
                    CMtx(:,Compt) = Data(i).Map(SelectedPx);
                end
            end
            
            Names = NamesSel;
            
            TheSum = sum(CMtx,2);
            WhereNan = find(isnan(TheSum));
            CMtx(WhereNan,:) = [];
            WhereZeros= find(TheSum == 0);
            CMtx(WhereZeros,:) = [];
            
            C = corr(CMtx);
            Ct = tril(C);
            
            try
                switch type
                    
                    case 1
                        x = 1 : 1 : size(Ct,2); % x edges
                        y = 1 : 1 : size(Ct,1); % y edges
                        [xAll, yAll] = meshgrid(x,y);
                        xAll(Ct==0)=nan;
                        
                        clrLim = [-1,1];
                        diamLim = [0.1, 1];
                        
                        cmap = app.ColorMapValues_noMask;
                        
                        Cscaled = (C - clrLim(1))/range(clrLim); % always [0:1]
                        colIdx = discretize(Cscaled,linspace(0,1,size(cmap,1)));
                        
                        Cscaled = (abs(Ct) - 0)/1;
                        diamSize = Cscaled * range(diamLim) + diamLim(1);
                        
                        fh = figure();
                        ax = axes(fh);
                        hold(ax,'on')
                        
                        colormap(ax,cmap);
                        % colormap(CorrColormap) %Uncomment for CorrColormap
                        tickvalues = 1:length(Ct);
                        x = zeros(size(tickvalues));
                        text(x, tickvalues, Names, 'HorizontalAlignment', 'right');
                        x(:) = length(Ct)+1;
                        text(tickvalues, x, Names, 'HorizontalAlignment', 'right','Rotation',90);
                        
                        theta = linspace(0,2*pi,30); % the smaller, the less memory req'd.
                        h = arrayfun(@(i)fill(diamSize(i)/2 * cos(theta) + xAll(i), ...
                            diamSize(i)/2 * sin(theta) + yAll(i), cmap(colIdx(i),:),'LineStyle','none'),1:numel(xAll));
                        axis(ax,'equal')
                        axis(ax,'tight')
                        set(ax,'YDir','Reverse')
                        colorbar()
                        caxis(clrLim);
                        axis off
                        fh.Color = [1,1,1];
                        
                        drawnow
                        
                    case 2
                        
                        figure
                        imagesc(C); axis image
                        hcb = colorbar;
                        ylabel(hcb,'Correlation coefficient')
                        caxis([-1 1])
                        colormap(app.ColorMapValues_noMask);
                        ax = gca;
                        ax.XTick = [1:1:length(Names)];
                        ax.XTickLabel = Names;
                        %ax.XAxisLocation = 'top';
                        ax.YTick = [1:1:length(Names)];
                        ax.YTickLabel = Names;
                        ax.YTickLabelRotation = 90;
                        for i = 1:length(Names)
                            for j = 1:length(Names)
                                t = text(i,j,num2str(round(C(i,j),2)));
                                t.HorizontalAlignment = 'center';
                            end
                        end
                        
                        
                    case 3
                        
                        
                        fcnCorrMatrixPlot(CMtx, Names, '')
                        
                    case 4
                        
                        
                        
                        keyboard
                        
                end
            catch ME
                close(app.WaitBar);
            end
            
            close(app.WaitBar);
            
        end
        
        function Check4InputCode(app,varargin)
            if ~isempty(varargin{1})
                if isequal(char(varargin{1}(1)),'open') && length(varargin{1}) > 1
                    ProjectName = char(varargin{1}(2));
                    if exist(fullfile(cd,[ProjectName,'.mat']))
                        LoadProjectFile(app,cd,[ProjectName,'.mat']);
                    end
                end
            end
        end
        
        function DrawingMode(app,Value,Type,Message)
            switch Value
                
                case {'on','On'}
                    
                    DeactivatePlotZoomPanOptions(app);
                    
                    app.Assistant_TextArea.BackgroundColor = [0.66,0.62,0.91];
                    
                    switch Type
                        case 'Rectangle'
                            app.Assistant_TextArea.Value = {'> Draw a "Rectangle ROI" on the main figure',' ','> To draw the ROI, position the pointer on the image. Click on the displayed image to select the first corner, hold on and drag the mouse to the opposite corner defining a rectangle.'};
                        case 'Polygon'
                            app.Assistant_TextArea.Value = {'> Draw a "Polygon ROI" on the main figure',' ','> To draw the ROI, position the pointer on the image. Click successively on the dislayed image to draw segments of a polygon; right-clicking or clicking on the first point to validate and close automatically the shape.'};
                        case 'Circle'
                            app.Assistant_TextArea.Value = {'> Draw a "Circle ROI" on the main figure',' ','> To draw the ROI, position the pointer on the image. Click to select the first corner of a square and drag the mouse to opposite corner defining a circle.'};
                        case 'Ellipse'
                            app.Assistant_TextArea.Value = {'> Draw an "Ellipse ROI" on the main figure',' ','> To draw the ROI, position the pointer on the image. Click to select the first point and drag the mouse to a second point defining the long axis of an ellipse.'};
                        case 'Line'
                            app.Assistant_TextArea.Value = {'> Draw a "Polyline ROI" on the main figure',' ','> To draw the ROI, position the pointer on the image. Click to define the first point and the successive segments of the polyline ROI; Double-clicking at the last point position validate the selection'};
                        case 'Spot'
                            app.Assistant_TextArea.Value = {'> Select a Spot position on the main figure',' ','> To select the spot, position the pointer on the image. Click to define the point position'};
                    end
                    
                    app.DrawingModeLastTab = app.TabGroup.SelectedTab.Title;
                    app.TabGroup.SelectedTab = app.XMapToolsAssistantTab;
                    
                    app.TreeData_Main.Enable = 'off';
                    app.TreeData_Additional.Enable = 'off';
                    
                    app.TabButtonGroup.Visible = 'off';
                    app.GridLayout6.Visible = 'off';
                    
                    app.XMapToolsMenu.Enable = 'off';
                    app.FileMenu.Enable = 'off';
                    app.PlotMenu.Enable = 'off';
                    app.EditMenu.Enable = 'off';
                    app.ImageMenu.Enable = 'off';
                    app.SamplingMenu.Enable = 'off';
                    app.WorkspacesMenu.Enable = 'off';
                    app.ModulesMenu.Enable = 'off';
                    app.HelpMenu.Enable = 'off';
                    
                    app.MapSlider.Enable = 'off';
                    app.Value_MapSlider.Enable = 'off';
                    
                case {'off','Off'}
                    
                    app.Assistant_TextArea.Value = ' ';
                    app.Assistant_TextArea.BackgroundColor = [0.94,0.94,0.94];
                    
                    switch app.DrawingModeLastTab
                        case 'Information'
                            app.TabGroup.SelectedTab = app.InformationTab;
                        case 'Sampling'
                            app.TabGroup.SelectedTab = app.SamplingTab;
                        case 'Standards'
                            app.TabGroup.SelectedTab = app.StandardsTab;
                        case 'Composition'
                            app.TabGroup.SelectedTab = app.CompositionTab;
                    end
                    
                    app.TreeData_Main.Enable = 'on';
                    app.TreeData_Additional.Enable = 'on';
                    
                    app.TabButtonGroup.Visible = 'on';
                    app.GridLayout6.Visible = 'on';
                    
                    app.XMapToolsMenu.Enable = 'on';
                    app.FileMenu.Enable = 'on';
                    app.PlotMenu.Enable = 'on';
                    app.EditMenu.Enable = 'on';
                    app.ImageMenu.Enable = 'on';
                    app.SamplingMenu.Enable = 'on';
                    app.WorkspacesMenu.Enable = 'on';
                    app.ModulesMenu.Enable = 'on';
                    app.HelpMenu.Enable = 'on';
                    
                    app.MapSlider.Enable = 'on';
                    app.Value_MapSlider.Enable = 'on';
            end
            
        end
        
        
        
        
        function ApplySettingsPlotEngine(app)
            
            switch app.PlotEngineDropDown.Value
                
                case '4.4'
                    
                    axis(app.FigMain,'equal');
                    axis(app.FigMain,'image');
                    axis(app.FigMain,'equal');
                    
                    app.FigMain.Color = [0.94,0.94,0.94];
                    app.FigMain.XColor = [0.94,0.94,0.94];
                    app.FigMain.YColor = [0.94,0.94,0.94];
                    
                    app.FigMain.XTick = [];
                    app.FigMain.YTick = [];
                    
                    app.FigMain.Box = 'off';
                    
                    
                case '4.3 (legacy)'
                    
                    axis(app.FigMain,'image');
                    
                    app.FigMain.Color = 'w';
                    app.FigMain.XColor = 'k';
                    app.FigMain.YColor = 'k';
                    
                    app.FigMain.Box = 'on';
                    
                    app.FigMain.XTick = [];
                    app.FigMain.YTick = [];
                    
            end
            
            tb = axtoolbar(app.FigMain,{'pan','zoomin','zoomout','restoreview'});
            
            btn = axtoolbarbtn(tb,'push');
            btn.Icon = '078-magic wand.png';
            btn.Tooltip = 'Auto contrast';
            btn.ButtonPushedFcn = @(varargin)Button_FigMain_AutoContrastPushed(app);
            
        end
    end
    
    methods (Access = public)
        
        function ColorMap = CalculateColorMap(app,SelColorMap,Resolution)
            
            ColorData = app.ColorMaps(SelColorMap).Code;
            ColorData = flip(ColorData);
            
            Xi = 1:Resolution;
            Step = (Resolution-1)/(size(ColorData,1)-1);
            X = 1:Step:Resolution;
            
            ColorMap = zeros(length(Xi),size(ColorData,2));
            for i = 1:size(ColorData,2)
                ColorMap(:,i) = interp1(X',ColorData(:,i),Xi);
            end
            
        end
        
        
        function ResetGUI(app)
            
            % This function delete all nodes of all trees using the
            % internal delete function (a.delete)
            
            if ~isempty(app.TreeData_Main.SelectedNodes)
                app.SelectedNodes_MainTree = app.TreeData_Main.SelectedNodes.NodeData;
            end
            
            a = app.Node_It.Children;  % X-ray
            a.delete;
            
            a = app.Node_Qt.Children;  % Quanti
            a.delete;
            
            a = app.Node_Me.Children;  % Merge
            a.delete;
            
            a = app.Node_Re.Children;  % Results
            a.delete;
            
            a = app.Node_Ot.Children;  % Other
            a.delete;
            
            a = app.Node_Ct.Children;  % CT-data
            a.delete;
            
            a = app.Node_ROI.Children;  % ROI
            a.delete;
            
            a = app.Node_Im.Children;  % Im
            a.delete;
            
            a = app.Node_Masks.Children;  % MaskFiles
            a.delete;
            
            a = app.Node_TrainingSet.Children;  % Training Sets
            a.delete;
            
            a = app.Node_SegScheme.Children;  % Training Sets
            a.delete;
            
            a = app.Node_Standards.Children;  % Standards
            a.delete;
            
            a = app.Node_MapStandards.Children; % Map Standards
            a.delete;
            
        end
        
        function app = UpdateGUI(app)
            % -------------------------------------------------------------
            % This function builds the Trees from the data in memory
            %
            % Warning: use ResetGUI first, otherwise it will build on top
            % of existing trees
            %
            % Context menu are assigned when a node is selected (see the
            % callback functions)
            
            % Add MapData It
            if numel(app.XMapToolsData.MapData.It.Names) > 0
                app.Node_It.ContextMenu = app.ContextMenu_MainTree_C;
                for i = 1:numel(app.XMapToolsData.MapData.It.Names)
                    p = uitreenode(app.Node_It,'Text',char(app.XMapToolsData.MapData.It.Names{i}),'NodeData',[1,i]);
                end
            else
                app.Node_It.ContextMenu = [];
            end
            
            % Add MapData Qt
            if isfield(app.XMapToolsData.MapData,'Qt')
                if numel(app.XMapToolsData.MapData.Qt.Names) > 0
                    app.Node_Qt.ContextMenu = app.ContextMenu_MainTree_C;
                    for i = 1:numel(app.XMapToolsData.MapData.Qt.Names)
                        p = uitreenode(app.Node_Qt,'Text',char(app.XMapToolsData.MapData.Qt.Names{i}),'NodeData',[2,i,0]);
                        for j = 1:length(app.XMapToolsData.MapData.Qt.Data(i).ElNames)
                            p1 = uitreenode(p,'Text',char(app.XMapToolsData.MapData.Qt.Data(i).ElNames{j}),'NodeData',[2,i,j]);
                        end
                    end
                end
            end
            
            % Add MapData Me
            if isfield(app.XMapToolsData.MapData,'Me')
                if numel(app.XMapToolsData.MapData.Me.Names) > 0
                    app.Node_Me.ContextMenu = app.ContextMenu_MainTree_C;
                    for i = 1:numel(app.XMapToolsData.MapData.Me.Names)
                        p = uitreenode(app.Node_Me,'Text',char(app.XMapToolsData.MapData.Me.Names{i}),'NodeData',[3,i,0]);
                        for j = 1:length(app.XMapToolsData.MapData.Me.Data(i).ElNames)
                            p1 = uitreenode(p,'Text',char(app.XMapToolsData.MapData.Me.Data(i).ElNames{j}),'NodeData',[3,i,j]);
                        end
                    end
                end
            end
            
            % Add MapData Re
            if isfield(app.XMapToolsData.MapData,'Re')
                if numel(app.XMapToolsData.MapData.Re.Names) > 0
                    app.Node_Re.ContextMenu = app.ContextMenu_MainTree_C;
                    for i = 1:numel(app.XMapToolsData.MapData.Re.Names)
                        p = uitreenode(app.Node_Re,'Text',char(app.XMapToolsData.MapData.Re.Names{i}),'NodeData',[4,i,0]);
                        for j = 1:length(app.XMapToolsData.MapData.Re.Data(i).Labels)
                            p1 = uitreenode(p,'Text',char(app.XMapToolsData.MapData.Re.Data(i).Labels{j}),'NodeData',[4,i,j]);
                        end
                    end
                end
            end
            
            % Add MapData Ot
            if numel(app.XMapToolsData.MapData.Ot.Names) > 0
                app.Node_Ot.ContextMenu = app.ContextMenu_MainTree_C;
                for i = 1:numel(app.XMapToolsData.MapData.Ot.Names)
                    p = uitreenode(app.Node_Ot,'Text',char(app.XMapToolsData.MapData.Ot.Names{i}),'NodeData',[5,i]);
                end
            end
            
            % Add MapData Ct
            if numel(app.XMapToolsData.MapData.Ct.Names) > 1
                app.Node_Ct.ContextMenu = app.ContextMenu_MainTree_C;
                for i = 1:numel(app.XMapToolsData.MapData.Ct.Names)
                    p = uitreenode(app.Node_Ct,'Text',char(app.XMapToolsData.MapData.Ct.Names{i}),'NodeData',[6,i]);
                end
            end
            
            % Add ROI
            if numel(app.XMapToolsData.MapData.ROI.Names) > 0
                app.Node_ROI.ContextMenu = app.ContextMenu_MainTree_C;
                for i = 1:numel(app.XMapToolsData.MapData.ROI.Names)
                    p = uitreenode(app.Node_ROI,'Text',char(app.XMapToolsData.MapData.ROI.Names{i}),'NodeData',[7,i]);
                end
            end
            
            % Add MapData Im
            if isfield(app.XMapToolsData.MapData,'Im')
                if numel(app.XMapToolsData.MapData.Im.Names) > 0
                    %app.Node_Im.ContextMenu = app.ContextMenu_MainTree_C;
                    for i = 1:numel(app.XMapToolsData.MapData.Im.Names)
                        p = uitreenode(app.Node_Im,'Text',char(app.XMapToolsData.MapData.Im.Names{i}),'NodeData',[8,i,0]);
                        for j = 1:length(app.XMapToolsData.MapData.Im.Data(i).Labels)
                            p1 = uitreenode(p,'Text',char(app.XMapToolsData.MapData.Im.Data(i).Labels{j}),'NodeData',[8,i,j]);
                        end
                    end
                end
            end
            
            % Add MaskFiles
            for i = 1:numel(app.XMapToolsData.MapData.MaskFile.Names)
                p = uitreenode(app.Node_Masks,'Text',char(app.XMapToolsData.MapData.MaskFile.Names{i}),'NodeData',[11,i,0,0]);
                for j = 1:app.XMapToolsData.MapData.MaskFile.NbMasks(i)
                    p1 = uitreenode(p,'Text',char(app.XMapToolsData.MapData.MaskFile.Masks(i).Names{j}),'NodeData',[11,i,j,0]);
                    % disp(app.XMapToolsData.MapData.MaskFile.Masks(i).Names{j})
                    if ~isempty(app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Names)
                        for k = 1:length(app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Names)
                            p2 = uitreenode(p1,'Text',char(app.XMapToolsData.MapData.MaskFile.Masks(i).SubMask(j).Names{k}),'NodeData',[11,i,j,k]);
                        end
                    end
                end
            end
            
            % Add Training Sets
            for i = 1:length(app.XMapToolsData.TrainingSet.Names)
                p = uitreenode(app.Node_TrainingSet,'Text',char(app.XMapToolsData.TrainingSet.Names{i}),'NodeData',[12,i,0]);
                for j = 1:length(app.XMapToolsData.TrainingSet.Data(i).Names)
                    p1 = uitreenode(p,'Text',char(app.XMapToolsData.TrainingSet.Data(i).Names{j}),'NodeData',[12,i,j]);
                    for k = 1:length(app.XMapToolsData.TrainingSet.Data(i).ROI(j).Types)
                        p2 = uitreenode(p1,'Text',char(app.XMapToolsData.TrainingSet.Data(i).ROI(j).Types{k}),'NodeData',[12,i,j,k]);
                    end
                end
            end
            
            % Add Standards
            for i = 1:size(app.XMapToolsData.Standards.Coord,1)
                p = uitreenode(app.Node_Standards,'Text',app.XMapToolsData.Standards.Labels{i},'NodeData',[14,i]);
            end
            
            % Add MapStandards
            if ~isempty(app.XMapToolsData.MapStandards(1).Names)
                for iStd = 1:length(app.XMapToolsData.MapStandards)
                    p = uitreenode(app.Node_MapStandards,'Text',app.XMapToolsData.MapStandards(iStd).StandardName,'NodeData',[15,iStd,0]);
                    
                    for i = 1:length(app.XMapToolsData.MapStandards(iStd).Names)
                        p = uitreenode(app.Node_MapStandards.Children(iStd),'Text',app.XMapToolsData.MapStandards(iStd).Names{i},'NodeData',[15,iStd,i]);
                    end
                end
            end
            
            % Add Segmentation Schemes
            for i = 1:length(app.XMapToolsData.SegScheme.Names)
                p = uitreenode(app.Node_SegScheme,'Text',char(app.XMapToolsData.SegScheme.Names{i}),'NodeData',[13,1,i,0,]);
                for j = 1:length(app.XMapToolsData.SegScheme.Data(i).Names)
                    p1 = uitreenode(p,'Text',char(app.XMapToolsData.SegScheme.Data(i).Names{j}),'NodeData',[13,1,i,j]);
                    p2 = uitreenode(p1,'Text',['range: ',num2str(app.XMapToolsData.SegScheme.Data(i).ROI(j).Coordinates(1)),'->',num2str(app.XMapToolsData.SegScheme.Data(i).ROI(j).Coordinates(2))],'NodeData',[13,1,i,j,1]);
                end
            end
            
            % Add LOD data
            for i = 1:length(app.XMapToolsData.MapLOD.Names)
                p = uitreenode(app.Node_LOD,'Text',char(app.XMapToolsData.MapLOD.Names{i}),'NodeData',[16,i,0,0]);
                for j = 1:length(app.XMapToolsData.MapLOD.Data(i).ElNames)
                    p1 = uitreenode(p,'Text',char(app.XMapToolsData.MapLOD.Data(i).ElNames{j}),'NodeData',[16,i,j,0]);
                    
                    p2 = uitreenode(p1,'Text',char(app.XMapToolsData.MapLOD.Data(i).ElData(j).Label_LOD),'NodeData',[16,i,j,1]);
                    p2 = uitreenode(p1,'Text',char(app.XMapToolsData.MapLOD.Data(i).ElData(j).Label_Si_mtx),'NodeData',[16,i,j,2]);
                    p2 = uitreenode(p1,'Text',char(app.XMapToolsData.MapLOD.Data(i).ElData(j).Label_Back_mtx),'NodeData',[16,i,j,3]);
                end
            end
            
        end
        
        function ROI_DeleteROI(app)
            % This function deletes all the ROI of the main figure
            % app.FigMain
            
            app.LBC_UncCalc.Enable = 'off';
            app.LBC_ValueMC.Enable = 'off';
            app.LBC_NbSimMC.Enable = 'off';
            app.PxLabel.Enable = 'off';
            app.SimLabel.Enable = 'off';
            
            % List to be updated later on
            
            delete(findall(app.FigMain, 'Type',  'images.roi.Rectangle'));
            delete(findall(app.FigMain, 'Type',  'images.roi.Polygon'));
            delete(findall(app.FigMain, 'Type',  'images.roi.Ellipse'));
            delete(findall(app.FigMain, 'Type',  'images.roi.Circle'));
            delete(findall(app.FigMain, 'Type',  'images.roi.Point'));
            
            delete(findall(app.FigMain, 'Type',  'images.roi.Polyline'));
            
            app.SaveResultsMenu.Enable = 'off';
            
            app.EditField_LivePosition.Visible = 'on';
            app.EditField_LivePeak.Visible = 'off';
            
            app.DataCursorMode = 0;
            ApplyCursorMode(app);
        end
        
        function Standard_ApplyXYShift(app,X,Y)
            
            app.XMapToolsData.Standards.XY = [app.XMapToolsData.Standards.XY(:,1)+X,app.XMapToolsData.Standards.XY(:,2)+Y];
            
            Standards_UpdateIntensities(app);
            Standards_CalculateMapPosition(app);
            
            for i = 1:length(app.ROI_std)
                % delete exisitng ROI
                app.ROI_std(i).ROI.delete;
            end
            
            Standards_PlotStdROI(app,0);
            
        end
        
        
        function LBC_ROI_changed_shape(app, ~)
            
            WhereDensity = app.CompViewer_DensityMenu.UserData(app.CompViewer_DensityMenu.Value);
            DensityMap = app.XMapToolsData.MapData.Ot.Data(WhereDensity).Map;
            
            NodeData = app.TreeData_Main.SelectedNodes.NodeData;
            Idx = NodeData(2);
            
            Mask = zeros(size(DensityMap));
            for i = 1:length(app.ROI_LBC)
                MaskTemp = createMask(app.ROI_LBC(i).ROI);
                WhereMask = find(MaskTemp);
                Mask(WhereMask) = 1;
            end
            
            % figure, imagesc(Mask), axis image, colorbar
            
            IdxMask = find(Mask & app.XMapToolsData.MapData.Me.Data(Idx).CData(end).Map > 0);
            
            DensityDomain = sum(DensityMap(IdxMask))/numel(IdxMask);
            
            LBC_Table = cell(length(app.XMapToolsData.MapData.Me.Data(Idx).ElNames),2);
            
            for i = 1:length(app.XMapToolsData.MapData.Me.Data(Idx).ElNames)
                LBC_Table{i,1} = app.XMapToolsData.MapData.Me.Data(Idx).ElNames{i};
                
                Map = app.XMapToolsData.MapData.Me.Data(Idx).CData(i).Map;
                
                % filter for Inf and NaN (4.3) for LA-ICPMS data
                WhereInf = find(isinf(Map));
                Map(WhereInf) = 0;
                WhereNaN = find(isnan(Map));
                Map(WhereNaN) = 0;
                
                Comp = sum(DensityMap(IdxMask)/DensityDomain .* Map(IdxMask))/numel(IdxMask);              % from Lanari & Engi (2017)
                %StDev = std(DensityMap(IdxMask)/DensityDomain .* Map(IdxMask));
                
                LBC_Table{i,2} = Comp;
                %LBC_Table{i,3} = StDev;
            end
            
            app.CompViewer_UITable.Data = LBC_Table;
            app.CompViewer_UITable.ColumnName = {'Element','Composition'};
            app.CompViewer_DensityMenu.Visible = 'on';
            
            pie(app.CompViewer_PlotTable,cell2mat(LBC_Table(1:end-1,2)),LBC_Table(1:end-1,1))
            
            colormap(app.CompViewer_PlotTable,app.ColorMapValues_noMask); %turbo(length(LBC_Table(1:end-1,1))));
            
            app.CompViewer_Button_Copy.Enable = 'on';
            app.CompViewer_Button_Save.Enable = 'on';
            app.CompViewer_Button_DeleteROI.Enable = 'on';
            
            app.LBC_UncCalc.Enable = 'on';
            app.LBC_ValueMC.Enable = 'on';
            app.LBC_NbSimMC.Enable = 'on';
            app.PxLabel.Enable = 'on';
            app.SimLabel.Enable = 'on';
        end
        
        function LOD_ROI_changed_shape(app,~)
            
            % Check if selection is still good or eliminate ROI
            
            if ~isempty(app.TreeData_Additional.SelectedNodes)
                NodeData = app.TreeData_Additional.SelectedNodes.NodeData;
                if isequal(NodeData(1),16) && NodeData(2) > 0
                    SelLOD = NodeData(2);
                else
                    ROI_DeleteROI(app);
                    return
                end
            else
                ROI_DeleteROI(app);
                return
            end
            
            
            NbEl = length(app.XMapToolsData.MapLOD.Data(SelLOD).ElNames);
            
            Mask = createMask(app.ROI_LOD);
            
            LOI = zeros(NbEl,1);
            
            for i = 1:NbEl
                IdxMask = find(Mask & app.XMapToolsData.MapLOD.Data(SelLOD).ElData(i).Map_Si_mtx > 0);
                Nb = unique(app.XMapToolsData.MapLOD.Data(SelLOD).ElData(i).Map_Back_mtx(IdxMask));
                if ~isempty(Nb)
                    NbBack(i) = length(Nb);
                end
            end
            NbBackLines = max(NbBack);
            
            LBC_Table = cell(NbEl,2);
            for i = 1:NbEl
                LBC_Table{i,1} = app.XMapToolsData.MapLOD.Data(SelLOD).ElNames{i};
                
                Map_Si_mtx = app.XMapToolsData.MapLOD.Data(SelLOD).ElData(i).Map_Si_mtx;
                Map_Back_mtx = app.XMapToolsData.MapLOD.Data(SelLOD).ElData(i).Map_Back_mtx;
                DTi = app.XMapToolsData.MapLOD.Data(SelLOD).ElData(i).DTi;
                NbAnal_mtx = app.XMapToolsData.MapLOD.Data(SelLOD).ElData(i).NbAnal;
                
                IdxAllMask = find(Mask & Map_Si_mtx > 0 );
                IdxMask = find(Mask & Map_Back_mtx > 0 & ~isnan(Map_Si_mtx));
                
                if ~isempty(IdxMask)
                    Bi = mean(Map_Back_mtx(IdxMask));       % cps/µg.g-1
                    Si = mean(Map_Si_mtx(IdxMask));         % cps
                    NbAnal = sum(NbAnal_mtx(IdxMask));
                    
                    NbBack = mean(app.XMapToolsData.MapLOD.Data(SelLOD).ElData(i).NbBack(IdxMask)) * NbBackLines;
                    
                    LOD(i) = (3.29.*sqrt(Bi.*DTi.*NbAnal.*(1+NbAnal./NbBack)+2.71))./(NbAnal.*DTi.*Si);    % µg.g-1
                    
                else
                    LOD(i) = NaN;
                end
                
                
                LBC_Table{i,2} = LOD(i);
                LBC_Table{i,3} = [num2str(numel(IdxMask)),'/',num2str(numel(IdxAllMask))];
                
            end
            
            app.CompViewer_UITable.Data = LBC_Table;
            app.CompViewer_UITable.ColumnName = {'Element','LOD (µg/g)',['Nb Px ']};
            app.CompViewer_DensityMenu.Visible = 'off';
            app.CompViewer_PlotTable.Visible = 'off';
            
            app.CompViewer_Button_Copy.Enable = 'on';
            app.CompViewer_Button_Save.Enable = 'on';
            app.CompViewer_Button_DeleteROI.Enable = 'on';
            
        end
        
        
        function COMP_ROI_changed_shape(app,~)
            
            Mask = createMask(app.ROI_COMP);
            IdxMask = find(Mask);
            
            SelectedNodes = app.TreeData_Main.SelectedNodes;
            NodeData = SelectedNodes.NodeData;
            
            QtName = app.XMapToolsData.MapData.Qt.Names{NodeData(2)};
            
            %ElNames = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElNames;
            ElInd = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElInd;
            
            IndPos = find(ElInd > 0);
            
            for i = 1:length(IndPos)
                QtElNames{i} = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElNames{IndPos(i)};
                %QtElInd(i) = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElInd(IndPos(i));
                QtData(:,i) = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).CData(IndPos(i)).Map(IdxMask);
            end
            
            Sum = sum(QtData,2);
            Where = find(Sum > 5);
            
            QtData = QtData(Where,:);
            
            [MatrixSF,ElementsList,NameResult] = Call_SF_External(app,QtElNames,QtData,QtName);
            
            LBC_Table = cell(length(QtElNames)+1+length(ElementsList),4);
            
            Compt = 0;
            for i = 1:length(QtElNames)
                Compt = Compt+1;
                LBC_Table{Compt,1} = QtElNames{i};
                
                Ox(i) = mean(QtData(:,i));
                Unc(i) = std(QtData(:,i));
                Err(i) = Unc(i)/sqrt(length(Where)-1);
                
                LBC_Table{Compt,2} = Ox(i);
                LBC_Table{Compt,3} = Unc(i);
                LBC_Table{Compt,4} = Err(i);
            end
            Compt = Compt+1;
            LBC_Table{Compt,1} = 'Total';
            LBC_Table{Compt,2} = sum(Ox);
            LBC_Table{Compt,3} = sqrt(sum(Unc.^2));
            LBC_Table{Compt,4} = sqrt(sum(Err.^2));
            for j = 1:length(ElementsList)
                Compt = Compt+1;
                LBC_Table{Compt,1} = ElementsList{j};
                
                LBC_Table{Compt,2} = mean(MatrixSF(:,j));
                LBC_Table{Compt,3} = std(MatrixSF(:,j));
                LBC_Table{Compt,4} = std(MatrixSF(:,j))./sqrt(length(Where)-1);
            end
            
            app.CompViewer_UITable.Data = LBC_Table;
            
            pie(app.CompViewer_PlotTable,cell2mat(LBC_Table(1:i-1,2)),LBC_Table(1:i-1,1))
            
            app.CompViewer_UITable.ColumnName = {'',['Average (N=',num2str(length(QtData(:,1))),')'],'Stdev','Stder'};
            
            app.CompViewer_Button_Copy.Enable = 'on';
            app.CompViewer_Button_Save.Enable = 'on';
            app.CompViewer_Button_DeleteROI.Enable = 'on';
            
            app.TabGroup.SelectedTab = app.CompositionTab;
            
            % Calculate the CovarianceMatrix for structural formula
            app.CovarMatrix = [];
            app.CovarMatrix(1).Name = '';
            app.CovarMatrix(1).Means = [];
            app.CovarMatrix(1).VarCov = [];
            app.CovarMatrix(1).NbPx = [];
            
            Compt = 0;
            for i = 1:length(ElementsList)-1
                for j = i+1:length(ElementsList)
                    Compt = Compt + 1;
                    app.CovarMatrix(Compt).Name = [ElementsList{i},'-',ElementsList{j}];
                    app.CovarMatrix(Compt).Means = [mean(MatrixSF(:,i)),mean(MatrixSF(:,j))];
                    CovMtx = cov(MatrixSF(:,i),MatrixSF(:,j));
                    app.CovarMatrix(Compt).VarCov = CovMtx(:)';
                    app.CovarMatrix(Compt).NbPx = length(Where);
                end
            end
            
            
        end
        
        function EXTFCT_ROI_changed_shape(app,~)
            
            NodeData = app.TreeData_Main.SelectedNodes.NodeData;
            Idx = NodeData(2);
            
            Selection = [];
            
            for i = 1:length(app.ROI_EXTFCT)
                Mask = createMask(app.ROI_EXTFCT(i).ROI);
                Selection(i).Pixels = find(Mask);
            end
            
            ElInd = app.XMapToolsData.MapData.Qt.Data(Idx).ElInd;
            IndPos = find(ElInd > 0);
            
            for i = 1:length(IndPos)
                QtElNames{i} = app.XMapToolsData.MapData.Me.Data(Idx).ElNames{IndPos(i)};
                QtElInd(i) = app.XMapToolsData.MapData.Me.Data(Idx).ElInd(IndPos(i));
                for j = 1:length(Selection)
                    QtData(j).Data(:,i) = app.XMapToolsData.MapData.Me.Data(Idx).CData(IndPos(i)).Map(Selection(j).Pixels);
                end
            end
            
            % Calculated Mean and Std
            for i = 1:length(QtData)
                Sum = sum(QtData(i).Data,2);
                WherePos = find(Sum > 5);
                if length(WherePos) < 1
                    return
                end
                QtData(i).Mean = mean(QtData(i).Data(WherePos,:),1);
                QtData(i).Std = std(QtData(i).Data(WherePos,:),1);
                NbAn(i) = length(WherePos);
            end
            
            % Monte-Carlo (benchmark ok)
            NbSimul = 5000;
            for i = 1:length(QtData)
                QtData(i).MC = repmat(QtData(i).Mean,NbSimul,1);
                QtData(i).MC = QtData(i).MC + randn(size(QtData(i).MC)).*repmat(QtData(i).Std,NbSimul,1);
            end
            
            MinList = app.Other_MineralList.Items;
            FctList = app.Other_FunctionList.Items;
            
            WhereMin = find(ismember(MinList,app.Other_MineralList.Value));
            WhereFct = find(ismember(FctList,app.Other_FunctionList.Value));
            
            ExtFct = char(app.ExternalFunctions.Min(app.ExternalFunctions.IdxME(WhereMin)).ME.FileName{WhereFct});
            ExtFctLabel = char(app.ExternalFunctions.Min(app.ExternalFunctions.IdxME(WhereMin)).ME.Names{WhereFct});
            
            
            [MatrixResults,ElementsList] = Core_Call_ME(QtData,QtElNames,ExtFct,app.ExternalFunctions_AddParameters,app.ElOxDataDef);
            
            NbStr = '(# ';
            for i = 1:length(NbAn)
                NbStr = [NbStr,num2str(NbAn(i)),'/'];
            end
            NbStr(end) = ')';
            app.CompViewer_Label.Text = [ExtFct,' ',NbStr];
            
            RES_Table = cell(length(ElementsList),4);
            
            Compt = 0;
            
            if ~isfield(app.ExternalFunctions_AddParameters,'AddVar')
                app.ExternalFunctions_AddParameters.AddVar = {};
            end
            
            if ~isempty(app.ExternalFunctions_AddParameters.AddVar)
                for i = 1:length(app.ExternalFunctions_AddParameters.AddVar)
                    Compt = Compt+1;
                    RES_Table{Compt,1} = [app.ExternalFunctions_AddParameters.AddVar{i},'*'];
                    RES_Table{Compt,2} = app.ExternalFunctions_AddParameters.Values(i);
                end
                Compt = Compt+1;
                RES_Table{Compt,1} = '------------';
            end
            
            for i = 1:size(MatrixResults,2)
                Compt = Compt+1;
                RES_Table{Compt,1} = ElementsList{i};
                RES_Table{Compt,2} = MatrixResults(1,i);
                RES_Table{Compt,3} = std(MatrixResults(:,i));
                RES_Table{Compt,4} = std(MatrixResults(:,i))/sqrt(min(NbAn));
            end
            
            Compt = Compt+1;
            RES_Table{Compt,1} = '------------';
            
            for i = 1:length(QtData)
                Compt = Compt+1;
                RES_Table{Compt,1} = ['Mineral #',num2str(i)];
                
                for j = 1:length(QtElNames)
                    Compt = Compt+1;
                    RES_Table{Compt,1} = QtElNames{j};
                    RES_Table{Compt,2} = QtData(i).Mean(j);
                    RES_Table{Compt,3} = QtData(i).Std(j);
                    RES_Table{Compt,4} = QtData(i).Std(j)/sqrt(min(NbAn));
                end
                
                Compt = Compt+1;
                RES_Table{Compt,1} = '------------';
            end
            
            %app.CompViewer_PlotTable.Visible = 'on';
            
            if isreal(MatrixResults)
                cla(app.CompViewer_PlotTable)
                app.CompViewer_PlotTable.Visible = 'on';
                
                histogram(app.CompViewer_PlotTable,MatrixResults(:,1),'BinMethod','auto')
                axis(app.CompViewer_PlotTable,'auto')
                app.CompViewer_PlotTable.Title.String = [ElementsList{1},' (Monte-Carlo N=',num2str(NbSimul),')'];
                app.CompViewer_PlotTable.Title.Interpreter = 'none';
                app.CompViewer_PlotTable.XLabel.String = '';
                app.CompViewer_PlotTable.YLabel.String = '';
                
                drawnow
                
            else
                app.CompViewer_PlotTable.Visible = 'off';
                cla(app.CompViewer_PlotTable)
            end
            
            app.CompViewer_UITable.Data = RES_Table;
            app.CompViewer_UITable.ColumnName = {'Item','Value','Stdev','Sterr'};
            
            app.CompViewer_Button_Copy.Enable = 'on';
            app.CompViewer_Button_Save.Enable = 'on';
            app.CompViewer_Button_DeleteROI.Enable = 'on';
            app.CompViewer_DensityMenu.Visible = 'off';
            
            % Add later the composition of the two minerals (MatrixResults
            % + MatrixInput + MatrixStd
            
            app.TabGroup.SelectedTab = app.CompositionTab;
            %keyboard
            
        end
        
        function saveSamplingDataMultipleFile(app,Directory,Method,DataTable,Names,DataAll)
            
            fid = fopen(fullfile(Directory,'Data.txt'),'w');
            fprintf(fid,'%s\n',['Data exported with ',app.XMapTools_VER,' on ',datestr(now)]);
            fprintf(fid,'%s\n',['Selected map:     ',app.Info_MapType,' | ',app.Info_Phase,' | ',app.Info_MapName]);
            fprintf(fid,'%s\n',['Sampling method:  ',Method]);
            
            fprintf(fid,'\n%s\n\n','*** DATA SUMMARY ***');
            
            fprintf(fid,'%s\n','Quantity Mean Media Stdev Std_err #_non_zero');
            for i = 1:length(Names)
                fprintf(fid,'%s ',char(Names{i}));
                for j = 1:size(DataTable,1)
                    if isequal(DataTable(j,i),round(DataTable(j,i)))
                        fprintf(fid,'%.0f ',DataTable(j,i));
                    elseif DataTable(j,i) < 1
                        fprintf(fid,'%.6f ',DataTable(j,i));
                    elseif DataTable(j,i) < 10
                        fprintf(fid,'%.5f ',DataTable(j,i));
                    elseif DataTable(j,i) < 100
                        fprintf(fid,'%.4f ',DataTable(j,i));
                    elseif DataTable(j,i) < 1000
                        fprintf(fid,'%.3f ',DataTable(j,i));
                    elseif DataTable(j,i) < 10000
                        fprintf(fid,'%.2f ',DataTable(j,i));
                    else
                        fprintf(fid,'%.0f ',DataTable(j,i));
                    end
                end
                fprintf(fid,'\n');
            end
            
            fprintf(fid,'\n%s\n','-----');
            fprintf(fid,'%s\n','Note: to import this table to Excel, copy the data and paste in any cell.');
            fprintf(fid,'%s\n','      In Excel in the tab "Data", click "Text to Columns", select "delimiter" and "space".');
            
            fprintf(fid,'\n\n%s\n','*** ALL PIXEL DATA ***');
            for i = 1:length(Names)
                fprintf(fid,'\n%s ',Names{i});
                fprintf(fid,'%f ',DataAll(i).Data);
            end
            
            fprintf(fid,'\n');
            fclose(fid);
        end
        
    end
    

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, varargin)
            
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
            
            
            disp(' ')
            %disp(' ')
            %disp('XMapTools is starting...')
            %disp(' ')
            tic
            app.XMapTools_GUI.Visible = 'off';
            %disp('Interface generated and hidden'),toc
            %pause(3)
            
            %app.WaitBar.Message = 'XMapTools is getting ready';
            %drawnow
            
            if ispc
                %ScreenFraction = 0.70;
                XMapToolsWidth = 1200;
            else
                %ScreenFraction = 0.80;
                XMapToolsWidth = 1600;
            end
            WindowRatio = 0.5643;
            ScreenSize = get(0,'ScreenSize');
            
            if XMapToolsWidth>=ScreenSize(3)*0.95
                XMapToolsWidth = ScreenSize(3)*0.95;
            end
            
            app.XMapTools_GUI.Position(3) = XMapToolsWidth; %ScreenSize(3)*ScreenFraction;
            app.XMapTools_GUI.Position(4) = XMapToolsWidth*WindowRatio; %ScreenSize(3)*ScreenFraction*WindowRatio;
            
            app.XMapTools_Position.Original = [app.XMapTools_GUI.Position(3),app.XMapTools_GUI.Position(4)];
            %XMapTools_GUISizeChanged(app, event);
            app.XMapTools_Position.Live = app.XMapTools_Position.Original;
            
            app.Options_resolutionLabel.Text = ['Resolution: ',num2str(app.XMapTools_Position.Live(1)),'x',num2str(app.XMapTools_Position.Live(2)),' (',num2str(app.XMapTools_Position.Original(1)),'x',num2str(app.XMapTools_Position.Original(2)),')'];
            
            app.XMapTools_VER = 'XMapTools 4.4 build 250321';
            app.XMapTools_version.Text = app.XMapTools_VER;
            %disp('Version set'),toc
            % Check for Updates ------------------------------------------
            xmaptools_signature = app.XMapTools_VER(end-6:end);
            [release_signature,flag] = urlread('https://www.xmaptools.ch/version/release_signature.php');
            
            if flag && length(release_signature)
                
                xmaptools_signature = str2num(xmaptools_signature);
                release_signature = str2num(release_signature);
                
                if release_signature > xmaptools_signature    % must be >
                    % An update is available
                    disp(' ')
                    disp('Update is available')
                    toc
                    if isdeployed
                        if ispc
                            buttonName = questdlg('A new version of XMapTools is available!','XMapTools','Download XMapTools installer (WINDOWS)','Remind me later','Download XMapTools installer (WINDOWS)');
                        else
                            buttonName = questdlg('A new version of XMapTools is available!','XMapTools','Download XMapTools installer (macOS)','Remind me later','Download XMapTools installer (macOS)');
                        end
                    else
                        buttonName = questdlg('A new version of XMapTools is available!','XMapTools','Download XMapTools (MATLAB)','Remind me later','Download XMapTools (MATLAB)');
                    end
                    
                    WebAdress = '';
                    switch buttonName
                        case 'Remind me later'
                            
                        case 'Download XMapTools installer (WINDOWS)'
                            WebAdress = 'https://xmaptools.ch/download-last-release/XMapToolsInstaller_WIN.exe.zip';
                        case 'Download XMapTools installer (macOS)'
                            WebAdress = 'https://xmaptools.ch/download-last-release/XMapToolsInstaller_macOS.app.zip';
                            
                        otherwise
                            web('https://github.com/xmaptools/XMapTools_Public');
                            delete(app.XMapTools_GUI);
                            return
                    end
                    
                    if length(WebAdress) > 10
                        
                        directoryname = uigetdir(cd, 'Pick a directory to download the installer');
                        
                        if isequal(directoryname,0)
                            delete(app.XMapTools_GUI);
                            return
                        end
                        
                        cd(directoryname)
                        
                        unzip(WebAdress);
                        
                        if isdir('__MACOSX')
                            [status,msg,msgID] = rmdir('__MACOSX', 's');
                        end
                        
                        if ispc
                            web('https://xmaptools.ch/update-windows/');
                        else
                            web('https://xmaptools.ch/update-macos/');
                        end
                        delete(app.XMapTools_GUI);
                        return
                    end
                end
            end
            % Check for Updates ------------------------------------------
            %disp('Update checked'),toc
            app.XMapTools_LastDir = cd;
            app.XMapTools_GUI.Name = [app.XMapTools_VER,' - ',char(app.XMapTools_LastDir)];
            
            InitializeXMapToolsData(app);
            % --------------------------------------------------
            % temporary load project and create XMapToolsData
            % --------------------------------------------------
            % [filepath,name,ext] = fileparts(which('aaa4.mat'));
            % LoadProjectFile(app,filepath,[name,ext]);
            % --------------------------------------------------
            
            % Create the context menus:
            app.ContextMenu_MainTree_C = uicontextmenu(app.XMapTools_GUI);
            m1 = uimenu(app.ContextMenu_MainTree_C,'Text','Clear All','MenuSelectedFcn',@(varargin)ContextMenu_MainTree_ClearAllPushed(app));
            
            app.ContextMenu_MainTree_ID = uicontextmenu(app.XMapTools_GUI);
            m1 = uimenu(app.ContextMenu_MainTree_ID,'Text','Infos','MenuSelectedFcn',@(varargin)ContextMenu_MainTree_InfoPushed(app));
            m2 = uimenu(app.ContextMenu_MainTree_ID,'Text','Delete','MenuSelectedFcn',@(varargin)ContextMenu_MainTree_DeletePushed(app));
            
            app.ContextMenu_MainTree_IDE = uicontextmenu(app.XMapTools_GUI);
            m1 = uimenu(app.ContextMenu_MainTree_IDE,'Text','Infos','MenuSelectedFcn',@(varargin)ContextMenu_MainTree_InfoPushed(app));
            m2 = uimenu(app.ContextMenu_MainTree_IDE,'Text','Delete','MenuSelectedFcn',@(varargin)ContextMenu_MainTree_DeletePushed(app));
            m3 = uimenu(app.ContextMenu_MainTree_IDE,'Text','Export','MenuSelectedFcn',@(varargin)ContextMenu_MainTree_ExportPushed(app));
            
            app.ContextMenu_MainTree_IDD = uicontextmenu(app.XMapTools_GUI);
            m1 = uimenu(app.ContextMenu_MainTree_IDD,'Text','Infos','MenuSelectedFcn',@(varargin)ContextMenu_MainTree_InfoPushed(app));
            m2 = uimenu(app.ContextMenu_MainTree_IDD,'Text','Duplicate','MenuSelectedFcn',@(varargin)ContextMenu_MainTree_DuplicatePushed(app));
            m3 = uimenu(app.ContextMenu_MainTree_IDD,'Text','Delete','MenuSelectedFcn',@(varargin)ContextMenu_MainTree_DeletePushed(app));
            
            app.ContextMenu_MainTree_IDCD = uicontextmenu(app.XMapTools_GUI);
            m1 = uimenu(app.ContextMenu_MainTree_IDCD,'Text','Infos','MenuSelectedFcn',@(varargin)ContextMenu_MainTree_InfoPushed(app));
            m2 = uimenu(app.ContextMenu_MainTree_IDCD,'Text','Duplicate','MenuSelectedFcn',@(varargin)ContextMenu_MainTree_DuplicatePushed(app));
            m3 = uimenu(app.ContextMenu_MainTree_IDCD,'Text','Convert','MenuSelectedFcn',@(varargin)ContextMenu_MainTree_ConvertPushed(app));
            m4 = uimenu(app.ContextMenu_MainTree_IDCD,'Text','Delete','MenuSelectedFcn',@(varargin)ContextMenu_MainTree_DeletePushed(app));
            
            app.ContextMenu_MainTree_IDCED = uicontextmenu(app.XMapTools_GUI);
            m1 = uimenu(app.ContextMenu_MainTree_IDCED,'Text','Infos','MenuSelectedFcn',@(varargin)ContextMenu_MainTree_InfoPushed(app));
            m2 = uimenu(app.ContextMenu_MainTree_IDCED,'Text','Duplicate','MenuSelectedFcn',@(varargin)ContextMenu_MainTree_DuplicatePushed(app));
            m3 = uimenu(app.ContextMenu_MainTree_IDCED,'Text','Convert','MenuSelectedFcn',@(varargin)ContextMenu_MainTree_ConvertPushed(app));
            m4 = uimenu(app.ContextMenu_MainTree_IDCED,'Text','Export All','MenuSelectedFcn',@(varargin)ContextMenu_MainTree_ExportAllPushed(app));
            m5 = uimenu(app.ContextMenu_MainTree_IDCED,'Text','Delete','MenuSelectedFcn',@(varargin)ContextMenu_MainTree_DeletePushed(app));
            
            
            app.ContextMenu_MainTree_IDCSD = uicontextmenu(app.XMapTools_GUI);
            m1 = uimenu(app.ContextMenu_MainTree_IDCSD,'Text','Infos','MenuSelectedFcn',@(varargin)ContextMenu_MainTree_InfoPushed(app));
            m2 = uimenu(app.ContextMenu_MainTree_IDCSD,'Text','Duplicate','MenuSelectedFcn',@(varargin)ContextMenu_MainTree_DuplicatePushed(app));
            m3 = uimenu(app.ContextMenu_MainTree_IDCSD,'Text','Convert','MenuSelectedFcn',@(varargin)ContextMenu_MainTree_ConvertPushed(app));
            m4 = uimenu(app.ContextMenu_MainTree_IDCSD,'Text','Split (using maskfile)','MenuSelectedFcn',@(varargin)ContextMenu_MainTree_SplitPushed(app));
            m5 = uimenu(app.ContextMenu_MainTree_IDCSD,'Text','Delete','MenuSelectedFcn',@(varargin)ContextMenu_MainTree_DeletePushed(app));
            
            app.ContextMenu_MainTree_I = uicontextmenu(app.XMapTools_GUI);
            m1 = uimenu(app.ContextMenu_MainTree_I,'Text','Infos','MenuSelectedFcn',@(varargin)ContextMenu_MainTree_InfoPushed(app));
            
            app.ContextMenu_MainTree_D = uicontextmenu(app.XMapTools_GUI);
            m1 = uimenu(app.ContextMenu_MainTree_D,'Text','Delete','MenuSelectedFcn',@(varargin)ContextMenu_MainTree_DeletePushed(app));
            
            app.ContextMenu_MainTree_DG = uicontextmenu(app.XMapTools_GUI);
            m1 = uimenu(app.ContextMenu_MainTree_DG,'Text','Delete','MenuSelectedFcn',@(varargin)ContextMenu_MainTree_DeletePushed(app));
            m2 = uimenu(app.ContextMenu_MainTree_DG,'Text','Save as GIF','MenuSelectedFcn',@(varargin)ContextMenu_MainTree_SaveAsGifPushed(app));
            
            app.ContextMenu_AdditionalTree_1_D = uicontextmenu(app.XMapTools_GUI);
            m1 = uimenu(app.ContextMenu_AdditionalTree_1_D,'Text','Delete','MenuSelectedFcn',@(varargin)ContextMenu_AdditionalTree_DeletePushed(app,gco));
            
            app.ContextMenu_AdditionalTree_7_DD = uicontextmenu(app.XMapTools_GUI);
            m1 = uimenu(app.ContextMenu_AdditionalTree_7_DD,'Text','Duplicate','MenuSelectedFcn',@(varargin)ContextMenu_AdditionalTree_DuplicatePushed(app,gco));
            m2 = uimenu(app.ContextMenu_AdditionalTree_7_DD,'Text','Delete','MenuSelectedFcn',@(varargin)ContextMenu_AdditionalTree_DeletePushed(app,gco));
            
            app.ContextMenu_AdditionalTree_2_ID = uicontextmenu(app.XMapTools_GUI);
            m1 = uimenu(app.ContextMenu_AdditionalTree_2_ID,'Text','Infos','MenuSelectedFcn',@(varargin)ContextMenu_AdditionalTree_InfoPushed(app));
            m2 = uimenu(app.ContextMenu_AdditionalTree_2_ID,'Text','Delete','MenuSelectedFcn',@(varargin)ContextMenu_AdditionalTree_DeletePushed(app));
            
            app.ContextMenu_AdditionalTree_3_I = uicontextmenu(app.XMapTools_GUI);
            m1 = uimenu(app.ContextMenu_AdditionalTree_3_I,'Text','Infos','MenuSelectedFcn',@(varargin)ContextMenu_AdditionalTree_InfoPushed(app));
            
            app.ContextMenu_AdditionalTree_4_IDE = uicontextmenu(app.XMapTools_GUI);
            m1 = uimenu(app.ContextMenu_AdditionalTree_4_IDE,'Text','Infos','MenuSelectedFcn',@(varargin)ContextMenu_AdditionalTree_InfoPushed(app));
            m2 = uimenu(app.ContextMenu_AdditionalTree_4_IDE,'Text','Delete','MenuSelectedFcn',@(varargin)ContextMenu_AdditionalTree_DeletePushed(app));
            m3 = uimenu(app.ContextMenu_AdditionalTree_4_IDE,'Text','Export','MenuSelectedFcn',@(varargin)ContextMenu_AdditionalTree_ExportPushed(app));
            
            app.ContextMenu_AdditionalTree_5_DE = uicontextmenu(app.XMapTools_GUI);
            m1 = uimenu(app.ContextMenu_AdditionalTree_5_DE,'Text','Delete','MenuSelectedFcn',@(varargin)ContextMenu_AdditionalTree_DeletePushed(app,gco));
            m2 = uimenu(app.ContextMenu_AdditionalTree_5_DE,'Text','Edit','MenuSelectedFcn',@(varargin)ContextMenu_AdditionalTree_EditROIPoint(app,gco));
            
            app.ContextMenu_AdditionalTree_6_IC = uicontextmenu(app.XMapTools_GUI);
            m1 = uimenu(app.ContextMenu_AdditionalTree_6_IC,'Text','Infos','MenuSelectedFcn',@(varargin)ContextMenu_AdditionalTree_InfoPushed(app));
            m1 = uimenu(app.ContextMenu_AdditionalTree_6_IC,'Text','Create Training Set','MenuSelectedFcn',@(varargin)ContextMenu_AdditionalTree_CreateTrainingSetPushed(app));
            
            
            %disp('App initialized'),toc
            % -------------
            % Set interface
            SetInterfaceAvailability(app,'Starting');
            %disp('Interface availability checked'),toc
            % Adjust the histogram position
            app.FigHistLive.PlotBoxAspectRatioMode = 'auto';
            
            % Read setting files
            ReadColorMaps(app);
            ReadDefFiles(app);
            ReadDensityDataFile(app);
            ReadColorDataFile(app);
            
            % Load Functions
            app.ExternalFunctions = Core_Function_Indexing;
            
            app.SF_MineralList.Items = app.ExternalFunctions.ItemsSF;
            app.SF_FunctionList.Items = app.ExternalFunctions.Min(app.ExternalFunctions.IdxSF(1)).SF.Name;
            
            UpdateGUI_Function_Other(app)
            
            Developper_UnlockButtonPushed(app, 1);
            
            app.DataCursorMode = 0;
            ApplyCursorMode(app);
            
            %disp('Def files read'),toc
            % Configuration of the setup for compiled versions
            %
            %             if isdeployed && ispc
            %                 [status, result] = system('path');
            %                 realpwd = char(regexpi(result, 'Path=(.*?);', 'tokens', 'once'));
            %             else % MATLAB mode.
            %                 realpwd = pwd;
            %             end
            %
            %             if isdeployed && ismac
            %                 NameOfDeployedApp = 'XMapTools'; % do not include the '.app' extension
            %                 [~, result] = system(['top -n100 -l1 | grep ' NameOfDeployedApp ' | awk ''{print $1}''']);
            %                 result=strtrim(result);
            %                 [status, result] = system(['ps xuwww -p ' result ' | tail -n1 | awk ''{print $NF}''']);
            %                 if status==0
            %                     diridx=strfind(result,[NameOfDeployedApp '.app']);
            %                     realpwd=result(1:diridx-2);
            %                 end
            %             else
            %                 realpwd = which('XMapTools');
            %                 realpwd =  realpwd = realpwd(1:end-17);
            %             end
            
            
            %app.WaitBar.Message = 'XMapTools is getting ready';
            %app.WaitBar = uiprogressdlg(app.XMapTools_GUI,'Title','XMapTools');
            
            Check4Config(app);
            
            if isdeployed
                %app.WaitBar.Message = 'Select a working directory';
                f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
                Directory = uigetdir(app.config.xmaptools.last_path, 'Select a working directory');
                close(f);
                figure(app.XMapTools_GUI);
                if ~isempty(Directory)
                    cd(Directory)
                    app.XMapTools_LastDir = Directory;
                    app.XMapTools_GUI.Name = [app.XMapTools_VER,' - ',char(app.XMapTools_LastDir)];
                end
                %app.WaitBar.Message = 'XMapTools is almost ready';
            end
            
            if exist([cd,'/log_xmaptools.txt'])
                delete([cd,'/log_xmaptools.txt']);
            end
            diary([cd,'/log_xmaptools.txt']);
            
            if exist([cd,'/legacy.xmt'])
                app.PlotEngineDropDown.Value = '4.3 (legacy)';
            end
            
            %disp('log created'),toc
            
            %pause(0.1)
            
            %close(app.WaitBar);
            
            %disp('config file checked'),toc
            
            Text2Disp = '';
            
            Text2Disp = [Text2Disp,'Version: ',app.XMapTools_VER,'\n\n'];
            Text2Disp = [Text2Disp,'© 2021-2025, University of Lausanne, Institute of Earth Sciences, Pierre Lanari. XMapTools is licensed under the GNU General Public License v3.0. \n\n'];
            
            Text2Disp = [Text2Disp,'How to cite XMapTools in scientific publications?\n\n'];
            Text2Disp = [Text2Disp,'> XMapTools software:\n   Lanari, P., Vho, A., Bovay, T., Airaghi, L., Centrella, S., (2019). Quantitative compositional mapping of mineral phases by electron probe micro-analyser. Geological Society of London, Special Publications, 478, 39-63.\n'];
            Text2Disp = [Text2Disp,'   Lanari, P., Vidal, O., De Andrade, V., Dubacq, B., Lewin, E., Grosch, E., Schwartz, S. (2014) XMapTools: a MATLAB©-based program for electron microprobe X-ray image processing and geothermobarometry. Computers and Geosciences. 62, 227-240.\n\n'];
            
            Text2Disp = [Text2Disp,'> LA-ICPMS:\n   Markmann, T.A., Lanari, P., Piccoli, F., Pettke, T., Tamblyn, R., Tedeschi, M., Lueder, M., Kunz, B., Riel, N., and Laughton, J. (2024). Multi-phase quantitative compositional mapping by LA-ICP-MS: analytical approach and data reduction protocol implemented in XMapTools. Chemical Geology, 646, 121895.\n\n'];
            
            Text2Disp = [Text2Disp,'> CLASSIFICATION:\n   Lanari, P., Tedeschi, M., (2025). Chemical map classification in XMapTools. Applied Computing and Geosciences, 25, 100230.\n\n'];
            
            Text2Disp = [Text2Disp,'> Mapping technique:\n   Lanari, P., & Piccoli, F., (2020). New horizons in quantitative compositional mapping – Analytical conditions and data reduction using XMapTools. IOP Conf. Series: Materials Science and Engineering 891, 012016.\n\n'];
            Text2Disp = [Text2Disp,'> LA-ICPMS data visualization:\n   Raimondo, T., Payne, J., Wade, B., Lanari, P., Clark, C., Hand, M., (2017). Trace element mapping by LA-ICP-MS: assessing geochemical mobility in garnet. Contributions to Mineralogy and Petrology, 172, 17.\n\n'];
            Text2Disp = [Text2Disp,'> Local bulk composition and density correction:\n   Lanari, P., & Engi, M. (2017). Local bulk composition effects on metamorphic mineral assemblages, Reviews in Mineralogy and Geochemistry, 83, 55-102.\n\n'];
            Text2Disp = [Text2Disp,'> Bingo-Antidote (theory):\n   Duesterhoeft, E. & Lanari, P. (2020). Iterative thermodynamic modelling – Part 1: A theoretical scoring technique and a computer program (BINGO-ANTIDOTE). Journal of Metamorphic Geology, 38, 527-551.\n\n'];
            Text2Disp = [Text2Disp,'> Bingo-Antidote (application):\n   Lanari, P. & Hermann, J. (2021). Iterative thermodynamic modelling—Part 2: tracing equilibrium relationships between minerals in metamorphic rocks. Journal of Metamorphic Geology, 39, 651-674.\n\n'];
            
            app.MapInfo_TextArea.Value = sprintf(Text2Disp);
            %disp('Text updated'),toc
            
            app.Jiahui = 0;
            
            pause(0.1)   % increase the opening speed by 10-15 %
            %toc
            movegui(app.XMapTools_GUI,"center");
            %disp('GUI centered'),toc
            app.XMapTools_GUI.Visible = 'on';
            disp('End of opening function'),toc
            
            if ~isdeployed
                Check4InputCode(app,varargin);
            end
        end

        % Close request function: XMapTools_GUI
        function XMapTools_GUICloseRequest(app, event)
            
            UpdateConfig(app);
            
            pause(0.1);
            
            if app.SaveRequired
                %Answer = questdlg('Do you want to save your changes before ending this session?','XMapTools','Yes','No','Cancel (unfreeze)','Yes');
                %figure(app.XMapTools_GUI);
                
                Answer = uiconfirm(gcbf,'Do you want to save your changes before ending this session?','Confirm','Options',{'Yes','No','Cancel (unfreeze)'},'DefaultOption',2,'CancelOption',3,'Icon','question');
                
                if isequal(Answer,'Yes')
                    ProjectSaveButtonPushed(app, event);
                end
                if isequal(Answer,'Cancel (unfreeze)')
                    app.TreeData_Main.Enable = 'on';
                    app.TreeData_Additional.Enable = 'on';
                    
                    app.TabButtonGroup.Visible = 'on';
                    app.GridLayout6.Visible = 'on';
                    
                    app.XMapToolsMenu.Enable = 'on';
                    app.FileMenu.Enable = 'on';
                    app.PlotMenu.Enable = 'on';
                    app.EditMenu.Enable = 'on';
                    app.ImageMenu.Enable = 'on';
                    app.SamplingMenu.Enable = 'on';
                    app.WorkspacesMenu.Enable = 'on';
                    app.ModulesMenu.Enable = 'on';
                    app.HelpMenu.Enable = 'on';
                    
                    close(app.WaitBar)
                    return
                end
            else
                Answer = uiconfirm(gcbf,'Do you want to close XMapTools?','Confirm','Options',{'Yes','Cancel (unfreeze)'},'DefaultOption',1,'CancelOption',2,'Icon','question');
                if isequal(Answer,'Cancel (unfreeze)')
                    app.TreeData_Main.Enable = 'on';
                    app.TreeData_Additional.Enable = 'on';
                    
                    app.TabButtonGroup.Visible = 'on';
                    app.GridLayout6.Visible = 'on';
                    
                    app.XMapToolsMenu.Enable = 'on';
                    app.FileMenu.Enable = 'on';
                    app.PlotMenu.Enable = 'on';
                    app.EditMenu.Enable = 'on';
                    app.ImageMenu.Enable = 'on';
                    app.SamplingMenu.Enable = 'on';
                    app.WorkspacesMenu.Enable = 'on';
                    app.ModulesMenu.Enable = 'on';
                    app.HelpMenu.Enable = 'on';
                    
                    close(app.WaitBar)
                    return
                end
            end
            
            disp('XMapTools is closing – diary off')
            diary('off');
            
            AppId = app.Id_StandardViewer;
            if ~isempty(AppId)
                delete(AppId);
            end
            
            if ~isempty(app.Id_HelpTool)
                delete(app.Id_HelpTool);
            end
            
            delete(app);
            
        end

        % Callback function
        function XMapTools_GUISizeChanged(app, event)
            position = app.XMapTools_GUI.Position;
            
            if isfield(app.XMapTools_Position,'Original')
                app.XMapTools_Position.Live = [position(3),position(4)];
                
                app.Options_resolutionLabel.Text = ['Resolution: ',num2str(app.XMapTools_Position.Live(1)),'x',num2str(app.XMapTools_Position.Live(2)),' (',num2str(app.XMapTools_Position.Original(1)),'x',num2str(app.XMapTools_Position.Original(2)),')'];
            end
            
            %             ScreenSize = get(0,'ScreenSize');
            %             if isequal(ScreenSize(3:end),position(3:end))
            %                 keyboard
            %             end
        end

        % Changes arrangement of the app based on UIFigure width
        function updateAppLayout(app, event)
            currentFigureWidth = app.XMapTools_GUI.Position(3);
            if(currentFigureWidth <= app.onePanelWidth)
                % Change to a 3x1 grid
                app.GridLayout.RowHeight = {790, 790, 790};
                app.GridLayout.ColumnWidth = {'1x'};
                app.CenterPanel.Layout.Row = 1;
                app.CenterPanel.Layout.Column = 1;
                app.LeftPanel.Layout.Row = 2;
                app.LeftPanel.Layout.Column = 1;
                app.RightPanel.Layout.Row = 3;
                app.RightPanel.Layout.Column = 1;
            elseif (currentFigureWidth > app.onePanelWidth && currentFigureWidth <= app.twoPanelWidth)
                % Change to a 2x2 grid
                app.GridLayout.RowHeight = {790, 790};
                app.GridLayout.ColumnWidth = {'1x', '1x'};
                app.CenterPanel.Layout.Row = 1;
                app.CenterPanel.Layout.Column = [1,2];
                app.LeftPanel.Layout.Row = 2;
                app.LeftPanel.Layout.Column = 1;
                app.RightPanel.Layout.Row = 2;
                app.RightPanel.Layout.Column = 2;
            else
                % Change to a 1x3 grid
                app.GridLayout.RowHeight = {'1x'};
                app.GridLayout.ColumnWidth = {205, '1x', 275};
                app.LeftPanel.Layout.Row = 1;
                app.LeftPanel.Layout.Column = 1;
                app.CenterPanel.Layout.Row = 1;
                app.CenterPanel.Layout.Column = 2;
                app.RightPanel.Layout.Row = 1;
                app.RightPanel.Layout.Column = 3;
            end
        end

        % Selection changed function: TreeData_Main
        function TreeData_MainSelectionChanged(app, event)
            SelectedNodes = app.TreeData_Main.SelectedNodes;
            
            if isempty(SelectedNodes)
                return
            end
            
            % Deactivate first
            app.TempROI = [];
            app.SaveResultsMenu.Enable = 'off';
            
            DeactivatePlotZoomPanOptions(app);
            
            app.Classify_FilterLowProbPixels.Enable = 'off';
            app.Classify_FilterProbValue.Enable = 'off';
            app.Classify_FilterMaskFile.Enable = 'off';
            app.Classify_Modes_AddROI.Enable = 'off';
            app.Classify_PointCountingModes.Enable = 'off';
            
            app.InfosMenu.Enable = 'off';
            app.DuplicateMenu.Enable = 'off';
            app.DuplicateandAdjustMergedMenu.Enable = 'off';
            app.DeleteMenu.Enable = 'off';
            app.SelectROIMenu.Enable = 'off';
            app.EliminatePixelsMenu.Enable = 'off';
            app.ExportMapDataMenu.Enable = 'off';
            app.CopyMapDataMenu.Enable = 'off';
            app.Exportashdf5ResultsMenu.Enable = 'off';
            app.ExportMergedMenu.Enable = 'off';
            
            app.SelectAreaMenu.Enable = 'off';
            app.CropMenu.Enable = 'off';
            app.DriftCorrectionMenu.Enable = 'off';
            
            app.Calibrate_AddROIforLBC.Enable = 'off';
            app.Calibrate_Merge.Enable = 'off';
            app.Calibrate_Spider_Button.Enable = 'off';
            app.SpiderPlotMenu.Enable = 'off';
            
            app.ButtonCalibrate_LAICPMS.Enable = 'off';
            app.Calibrate_GenerateDensity.Enable = 'off';
            app.Calibrate_LOD_menu.Enable = 'off';
            app.Calibrate_LOD_CalcButton.Enable = 'off';
            app.Calibrate_ApplyLODfilter.Enable = 'off';
            
            app.SF_ExportMinCompositions.Enable = 'off';
            app.SF_ROI_menu.Enable = 'off';
            
            app.EliminateinsideMenu.Enable = 'off';
            app.EliminateoutsideMenu.Enable = 'off';
            
            app.SF_ApplyButton.Enable = 'off';
            app.Other_ApplyButton.Enable = 'off';
            app.Other_MultiEquilibriumButton.Enable = 'off';
            app.Other_ROI_menu.Enable = 'off';
            
            app.ROI_DispVertical.Visible = 'off';
            app.ROI_VerticalSlider.Visible = 'off';
            app.ROI_AngleOrientation.Visible = 'off';
            
            app.Classify_AddAllElem.Enable = 'off';
            app.Classify_RunPCAButton.Enable = 'off';
            app.Classify_Manage_Elem.Enable = 'off';
            app.Classify_Manage_Elem_DeleteAll.Enable = 'off';
            
            app.Segment_AddScheme.Enable = 'off';
            app.Segment_GradientMapButton.Enable = 'off';
            app.Segment_TrySegmentationButton.Enable = 'off';
            app.Segment_SegmentButton.Enable = 'off';
            
            app.Segment_ButtonPlot.Enable = 'off';
            app.Segment_ExportROI_TXT.Enable = 'off';
            app.Segment_SavePhaseProp.Enable = 'off';
            
            app.MapSlider.Visible = 'off';
            app.Value_MapSlider.Visible = 'off';
            
            app.Image_AddMultiPlotImageMenu.Enable = 'off';
            app.Image_AddMultiLayerImagesharedscaleMenu.Enable = 'off';
            app.Image_AddMultiLayerImageMenu.Enable = 'off';
            
            app.Image_AddCurrentImageMenu.Enable = 'on';
            
            app.ImageMenu.Enable = 'on';
            app.SamplingMenu.Enable = 'on';
            app.ModulesMenu.Enable = 'on';
            app.MaskMenu.Enable = 'off';
            app.UpdateElementOxideIndexationMenu.Enable = 'off';
            
            if ~isempty(app.ROI_LBC)
                ROI_DeleteROI(app);
                app.ROI_LBC = [];
            end
            
            if ~isempty(app.TreeData_Additional.SelectedNodes)
                for i=1:numel(app.TreeData_Additional.SelectedNodes)
                    SelectedAdditional(i,:) = app.TreeData_Additional.SelectedNodes(i).NodeData;
                end
            else
                SelectedAdditional = [];
            end
            
            % Check for multiple maps
            if length(SelectedNodes) > 1
                app.Button_FigMain_AutoContrast.Enable = 'off';
                app.Button_FigMain_MedianFilter.Enable = 'off';
                app.Button_FigMain_ResetDisplay.Enable = 'off';
                app.Button_FigMain_Pan.Enable = 'off';
                app.Button_FigMain_ZoomOut.Enable = 'off';
                app.Button_FigMain_ZoomIn.Enable = 'off';
                app.Button_FigMain_CursorInfo.Enable = 'off';
                app.Button_FigMain_PlotSurface.Enable = 'off';
                app.Button_FigMain_CopyImage.Enable = 'off';
                app.Button_FigMain_OpenNewWindow.Enable = 'off';
                app.Button_FigMain_ResetZoomPan.Enable = 'off';
                
                app.Image_AddMultiPlotImageMenu.Enable = 'on';
                app.Image_AddMultiLayerImagesharedscaleMenu.Enable = 'on';
                app.Image_AddMultiLayerImageMenu.Enable = 'on';
                
                app.Image_AddCurrentImageMenu.Enable = 'off';
                
                PlotMap_DisplayMultipleMaps(app,SelectedNodes,SelectedAdditional);
                return
            end
            
            NodeData = SelectedNodes.NodeData;
            
            % Disable options only available for a maskfile
            if ~isempty(app.TreeData_Additional.SelectedNodes)
                NodeDataAdditional = app.TreeData_Additional.SelectedNodes.NodeData;
                if length(NodeDataAdditional) > 2
                    if ~isequal(NodeDataAdditional(1),11) &&  NodeDataAdditional(2) > 0 && isequal(NodeDataAdditional(3),0)
                        app.Masks_ButtonPlotPie.Enable = 'off';
                        app.Classify_Button.Enable = 'off';
                        app.Classify_Button.Tooltip = 'Add maps and select a training set to activate the classify tool';
                    end
                end
            end
            
            % -------------------------------------------------------------
            % [1] Activate context menu
            % -------------------------------------------------------------
            % Note that it is much simpler to activate the context menu
            % only for the selected node, because we cannot in MATLAB 2020a
            % know from which node the context menu is called.
            
            switch NodeData(1)
                case 1    % It
                    % clear all context menu
                    for i = 1:length(app.Node_It.Children)
                        app.Node_It.Children(i).ContextMenu = [];
                    end
                    % Activate the selected node
                    
                    if NodeData(2) > 0
                        app.Node_It.Children(NodeData(2)).ContextMenu = app.ContextMenu_MainTree_ID;
                    end
                    
                case 2    % Qt
                    % clear all context menu
                    for i = 1:length(app.Node_Qt.Children)
                        app.Node_Qt.Children(i).ContextMenu = [];
                        for j = 1:length(app.Node_Qt.Children(i).Children)
                            app.Node_Qt.Children(i).Children(j).ContextMenu = [];
                        end
                    end
                    % Activate the selected node
                    if NodeData(2) > 0 && isequal(NodeData(3),0)
                        app.Node_Qt.Children(NodeData(2)).ContextMenu = app.ContextMenu_MainTree_IDCED;
                    end
                    if NodeData(2) > 0 && NodeData(3) > 0
                        app.Node_Qt.Children(NodeData(2)).Children(NodeData(3)).ContextMenu = app.ContextMenu_MainTree_I;
                    end
                case 3    % Me
                    % clear all context menu
                    for i = 1:length(app.Node_Me.Children)
                        app.Node_Me.Children(i).ContextMenu = [];
                        for j = 1:length(app.Node_Me.Children(i).Children)
                            app.Node_Me.Children(i).Children(j).ContextMenu = [];
                        end
                    end
                    % Activate the selected node
                    if NodeData(2) > 0 && isequal(NodeData(3),0)
                        if ~isempty(app.TreeData_Additional.SelectedNodes)
                            if isequal(app.TreeData_Additional.SelectedNodes.NodeData(1),11) && app.TreeData_Additional.SelectedNodes.NodeData(2) > 0
                                app.Node_Me.Children(NodeData(2)).ContextMenu = app.ContextMenu_MainTree_IDCSD;
                            else
                                app.Node_Me.Children(NodeData(2)).ContextMenu = app.ContextMenu_MainTree_IDCD;
                            end
                        else
                            app.Node_Me.Children(NodeData(2)).ContextMenu = app.ContextMenu_MainTree_IDCD;
                        end
                        
                        %ContextMenu_MainTree_IDCSD
                        
                    end
                    if NodeData(2) > 0 && NodeData(3) > 0
                        app.Node_Me.Children(NodeData(2)).Children(NodeData(3)).ContextMenu = app.ContextMenu_MainTree_I;
                    end
                case 4    % Re
                    % clear all context menu
                    for i = 1:length(app.Node_Re.Children)
                        app.Node_Re.Children(i).ContextMenu = [];
                        for j = 1:length(app.Node_Re.Children(i).Children)
                            app.Node_Re.Children(i).Children(j).ContextMenu = [];
                        end
                    end
                    % Activate the selected node
                    if NodeData(2) > 0 && isequal(NodeData(3),0)
                        app.Node_Re.Children(NodeData(2)).ContextMenu = app.ContextMenu_MainTree_ID;
                    end
                    if NodeData(2) > 0 && NodeData(3) > 0
                        app.Node_Re.Children(NodeData(2)).Children(NodeData(3)).ContextMenu = app.ContextMenu_MainTree_I;
                    end
                    
                case 5  % Ot
                    % clear all context menu
                    for i = 1:length(app.Node_Ot.Children)
                        app.Node_Ot.Children(i).ContextMenu = [];
                        for j = 1:length(app.Node_Ot.Children(i).Children)
                            app.Node_Ot.Children(i).Children(j).ContextMenu = [];
                        end
                    end
                    % Activate the selected node
                    if NodeData(2) > 0
                        app.Node_Ot.Children(NodeData(2)).ContextMenu = app.ContextMenu_MainTree_ID;
                    end
                    
                case 7 % ROI
                    % clear all context menu
                    for i = 1:length(app.Node_ROI.Children)
                        app.Node_ROI.Children(i).ContextMenu = [];
                    end
                    % Activate the selected node
                    if NodeData(2) > 0
                        app.Node_ROI.Children(NodeData(2)).ContextMenu = app.ContextMenu_MainTree_IDE;
                    end
                case 8 % IMAGES
                    if isempty(app.Node_Im.Children)
                        return
                    end
                    % clear all context menu
                    for i = 1:length(app.Node_Im.Children)
                        app.Node_Im.Children(i).ContextMenu = [];
                    end
                    % Activate the selected node
                    if NodeData(2) > 0
                        if isequal(NodeData(3),0)
                            if isequal(app.XMapToolsData.MapData.Im.Types(NodeData(2)),2)
                                app.Node_Im.Children(NodeData(2)).ContextMenu = app.ContextMenu_MainTree_DG;
                            else
                                app.Node_Im.Children(NodeData(2)).ContextMenu = app.ContextMenu_MainTree_D;
                            end
                        else
                            app.Node_Im.Children(NodeData(2)).Children(NodeData(3)).ContextMenu = app.ContextMenu_MainTree_D;
                        end
                        
                    end
                    
            end
            
            % -------------------------------------------------------------
            % [2] Update the menu File and Buttons
            % -------------------------------------------------------------
            
            switch NodeData(1)
                case 1    % It
                    if NodeData(2) > 0
                        app.InfosMenu.Enable = 'on';
                        app.DeleteMenu.Enable = 'on';
                        app.ExportMapDataMenu. Enable = 'on';
                        app.CopyMapDataMenu.Enable = 'on';
                        app.SelectAreaMenu.Enable = 'on';
                        
                        app.EliminatePixelsMenu.Enable = 'on';
                        
                        app.Classify_AddAllElem.Enable = 'on';
                        app.Classify_RunPCAButton.Enable = 'on';
                        app.DriftCorrectionMenu.Enable = 'on';
                        
                        % Add-remove element for classification
                        app.Classify_Manage_Elem.Enable = 'on';
                        if isempty(find(ismember(app.Classify_ElemList_ElList,char(SelectedNodes.Text))))
                            app.Classify_Manage_Elem.Icon = '056-plus.png';
                            app.Classify_Manage_Elem.Tooltip = ['Add "',char(SelectedNodes.Text),'" for classification'];
                        else
                            app.Classify_Manage_Elem.Icon = '057-minus.png';
                            app.Classify_Manage_Elem.Tooltip = ['Take "',char(SelectedNodes.Text),'" out'];
                        end
                        
                    end
                    
                case 2    % Qt
                    
                    if NodeData(2) > 0 && isequal(NodeData(3),0)
                        app.InfosMenu.Enable = 'on';
                        app.DuplicateMenu.Enable = 'on';
                        app.DeleteMenu.Enable = 'on';
                        
                        app.SF_ApplyButton.Enable = 'on';
                        
                        switch app.Other_MethodList.Value
                            case 'Map-mode'
                                app.Other_ApplyButton.Enable = 'on';
                        end
                        
                        app.Calibrate_Spider_Button.Enable = 'on';
                        app.SpiderPlotMenu.Enable = 'on';
                        
                        app.Calibrate_Merge.Enable = 'on';
                        
                        app.Classify_AddAllElem.Enable = 'on';
                        app.Classify_RunPCAButton.Enable = 'on';
                        
                        app.UpdateElementOxideIndexationMenu.Enable = 'on';
                        
                        app.Calibrate_ApplyLODfilter.Enable = 'on';
                        
                    elseif NodeData(2) > 0 && NodeData(3) > 0
                        app.InfosMenu.Enable = 'on';
                        app.ExportMapDataMenu. Enable = 'on';
                        app.CopyMapDataMenu.Enable = 'on';
                        
                        app.Calibrate_Merge.Enable = 'on';
                        
                        app.SF_ExportMinCompositions.Enable = 'on';
                        app.SF_ROI_menu.Enable = 'on';
                        app.SF_ApplyButton.Enable = 'on';
                        
                        app.Calibrate_Spider_Button.Enable = 'on';
                        app.SpiderPlotMenu.Enable = 'on';
                        
                        switch app.Other_MethodList.Value
                            case 'Map-mode'
                                app.Other_ApplyButton.Enable = 'on';
                            case 'Multi-equilibrium'
                                app.Other_MultiEquilibriumButton.Enable = 'on';
                                app.Other_ROI_menu.Enable = 'on';
                        end
                        
                        app.Classify_AddAllElem.Enable = 'on';
                        app.Classify_RunPCAButton.Enable = 'on';
                        
                        % Add-remove element for classification
                        app.Classify_Manage_Elem.Enable = 'on';
                        if isempty(find(ismember(app.Classify_ElemList_ElList,char(SelectedNodes.Text))))
                            app.Classify_Manage_Elem.Icon = '056-plus.png';
                            app.Classify_Manage_Elem.Tooltip = ['Add "',char(SelectedNodes.Text),'" for classification'];
                        else
                            app.Classify_Manage_Elem.Icon = '057-minus.png';
                            app.Classify_Manage_Elem.Tooltip = ['Take "',char(SelectedNodes.Text),'" out'];
                        end
                        
                        app.UpdateElementOxideIndexationMenu.Enable = 'on';
                        
                    end
                    
                case 3    % Me
                    
                    if NodeData(2) > 0
                        app.SelectAreaMenu.Enable = 'on';
                        app.EliminatePixelsMenu.Enable = 'on';
                        app.DuplicateandAdjustMergedMenu.Enable = 'on';
                        app.ExportMergedMenu.Enable = 'on';
                    else
                        app.SelectAreaMenu.Enable = 'off';
                    end
                    
                    if length(NodeData) < 3
                        app.Calibrate_AddROIforLBC.Enable = 'off';
                        
                    else
                        if isequal(NodeData(2),0) && isequal(NodeData(3),0)
                            app.Calibrate_AddROIforLBC.Enable = 'off';
                            
                        elseif NodeData(2) > 0 && isequal(NodeData(3),0)
                            app.InfosMenu.Enable = 'on';
                            app.DuplicateMenu.Enable = 'on';
                            app.DeleteMenu.Enable = 'on';
                            
                            app.Calibrate_AddROIforLBC.Enable = 'on';
                            
                            app.Classify_AddAllElem.Enable = 'on';
                            app.Classify_RunPCAButton.Enable = 'on';
                            
                            app.UpdateElementOxideIndexationMenu.Enable = 'on';
                        else
                            app.InfosMenu.Enable = 'on';
                            app.ExportMapDataMenu. Enable = 'on';
                            app.CopyMapDataMenu.Enable = 'on';
                            % ROI only available if a map is plotted...
                            app.EliminatePixelsMenu. Enable = 'on';
                            app.SelectROIMenu.Enable = 'on';
                            
                            app.SF_ExportMinCompositions.Enable = 'on';
                            app.SF_ROI_menu.Enable = 'on';
                            
                            app.Calibrate_AddROIforLBC.Enable = 'on';
                            
                            switch app.Other_MethodList.Value
                                case 'Multi-equilibrium'
                                    app.Other_MultiEquilibriumButton.Enable = 'on';
                                    app.Other_ROI_menu.Enable = 'on';
                            end
                            
                            app.Classify_AddAllElem.Enable = 'on';
                            app.Classify_RunPCAButton.Enable = 'on';
                            
                            % Add-remove element for classification
                            app.Classify_Manage_Elem.Enable = 'on';
                            if isempty(find(ismember(app.Classify_ElemList_ElList,char(SelectedNodes.Text))))
                                app.Classify_Manage_Elem.Icon = '056-plus.png';
                                app.Classify_Manage_Elem.Tooltip = ['Add "',char(SelectedNodes.Text),'" for classification'];
                            else
                                app.Classify_Manage_Elem.Icon = '057-minus.png';
                                app.Classify_Manage_Elem.Tooltip = ['Take "',char(SelectedNodes.Text),'" out'];
                            end
                            
                            app.UpdateElementOxideIndexationMenu.Enable = 'on';
                        end
                    end
                    
                case 4    % Re
                    
                    app.Exportashdf5ResultsMenu.Enable = 'on';
                    
                    if NodeData(2) > 0 && isequal(NodeData(3),0)
                        app.InfosMenu.Enable = 'on';
                        app.DeleteMenu.Enable = 'on';
                    else
                        app.InfosMenu.Enable = 'on';
                        app.ExportMapDataMenu. Enable = 'on';
                        app.CopyMapDataMenu.Enable = 'on';
                    end
                    
                case 5    % Other
                    % Add-remove element for classification
                    app.Classify_Manage_Elem.Enable = 'on';
                    if isempty(find(ismember(app.Classify_ElemList_ElList,char(SelectedNodes.Text))))
                        app.Classify_Manage_Elem.Icon = '056-plus.png';
                        app.Classify_Manage_Elem.Tooltip = ['Add "',char(SelectedNodes.Text),'" for classification'];
                    else
                        app.Classify_Manage_Elem.Icon = '057-minus.png';
                        app.Classify_Manage_Elem.Tooltip = ['Take "',char(SelectedNodes.Text),'" out'];
                    end
                    
                    
                case 6    % CT
                    if NodeData(2) > 0
                        app.Segment_AddScheme.Enable = 'on';
                        app.Segment_GradientMapButton.Enable = 'on';
                        
                        if ~isempty(app.TreeData_Additional.SelectedNodes)
                            if isequal(app.TreeData_Additional.SelectedNodes.NodeData(1),13) && isequal(app.TreeData_Additional.SelectedNodes.NodeData(2),1) && app.TreeData_Additional.SelectedNodes.NodeData(3) > 0
                                app.Segment_TrySegmentationButton.Enable = 'on';
                                app.Segment_SegmentButton.Enable = 'on';
                            end
                        end
                        
                    end
                    
                case 7
                    if NodeData(2) > 0
                        app.ROI_DispVertical.Visible = 'on';
                        app.ROI_VerticalSlider.Visible = 'on';
                        app.ROI_AngleOrientation.Visible = 'on';
                    end
                    
                case 8      % IMAGES
                    app.ImageMenu.Enable = 'off';
                    app.SamplingMenu.Enable = 'off';
                    app.ModulesMenu.Enable = 'off';
                    
            end
            
            
            % -------------------------------------------------------------
            % [3] Other Update
            % -------------------------------------------------------------
            if isequal(NodeData(1),1) && NodeData(2) > 0
                if ~isempty(app.TreeData_Additional.SelectedNodes)
                    NodeDataAdditional = app.TreeData_Additional.SelectedNodes.NodeData;
                    if isequal(NodeDataAdditional(1),11) && NodeDataAdditional(2) > 0 && isequal(NodeDataAdditional(3),0)
                        % We can't select a maskfile with an element
                        app.TreeData_Additional.SelectedNodes = [];
                        app.Masks_ButtonPlotPie.Enable = 'off';
                        
                        app.ButtonCalibrate_LAICPMS.Enable = 'off';
                        app.ButtonCalibrate.Enable = 'off';
                        app.ButtonCalibrate.Tooltip = 'Import standards and select a maskfile to activate the calibration tools';
                    end
                end
            end
            
            if isequal(NodeData(1),2)
                if NodeData(2) > 0 && isequal(NodeData(3),0)
                    app.SF_ApplyButton.Enable = 'on';
                    
                    switch app.Other_MethodList.Value
                        case 'Map-mode'
                            app.Other_ApplyButton.Enable = 'on';
                    end
                end
            else
                app.SF_ApplyButton.Enable = 'off';
                app.Other_ApplyButton.Enable = 'off';
            end
            
            if isequal(NodeData(1),7) && NodeData(2) > 0
                ROIsize = size(app.XMapToolsData.MapData.ROI.Data(NodeData(2)).ROI);
                if isequal(length(ROIsize),3)
                    app.Segment_ButtonPlot.Enable = 'on';
                    app.Segment_ExportROI_TXT.Enable = 'on';
                    app.Segment_SavePhaseProp.Enable = 'on';
                end
            end
            
            % Update the GUI (SPECIAL: unsupervised k-mean classification)
            app.Classify_Button.Enable = 'off';
            if isequal (app.Classification_Menu.Value,'Unsupervised k-means')
                if ~isempty(app.Classify_ElemList.Value)
                    app.Classify_Button.Enable = 'on';
                    app.Classify_Button.Tooltip = 'Unsupervised classification';
                end
            end
            
            if ~isempty(app.Classify_ElemList.Value)
                app.Classify_Manage_Elem_DeleteAll.Enable = 'on';
            end
            
            % If possible we reactivate the classify button
            if ~isempty(app.Classify_ElemList.Value)
                if ~isempty(app.TreeData_Additional.SelectedNodes)
                    NodeDataSPEC = app.TreeData_Additional.SelectedNodes(1).NodeData;
                    if isequal(NodeDataSPEC(1),12) && NodeDataSPEC(2) > 0 && isequal(NodeDataSPEC(3),0)
                        app.Classify_Button.Enable = 'on';
                        app.Classify_Button.Tooltip = 'Train a Classifier & Classify';
                    end
                end
            end
            
            % -------------------------------------------------------------
            % [4] Update the Slider
            % -------------------------------------------------------------
            if NodeData(2) > 0
                
                switch NodeData(1)
                    case 1
                        app.MapSlider.Visible = 'on';
                        if length(app.XMapToolsData.MapData.It.Names) > 1
                            app.MapSlider.Limits = [1,length(app.XMapToolsData.MapData.It.Names)];
                            app.MapSlider.Value = NodeData(2);
                            
                            app.Value_MapSlider.Visible = 'on';
                            app.Value_MapSlider.Limits = [1,length(app.XMapToolsData.MapData.It.Names)];
                            app.Value_MapSlider.Value = NodeData(2);
                        end
                        
                    case 6
                        app.MapSlider.Visible = 'on';
                        if length(app.XMapToolsData.MapData.Ct.Names) > 1
                            app.MapSlider.Limits = [1,length(app.XMapToolsData.MapData.Ct.Names)];
                            app.MapSlider.Value = NodeData(2);
                            
                            app.Value_MapSlider.Visible = 'on';
                            app.Value_MapSlider.Limits = [1,length(app.XMapToolsData.MapData.Ct.Names)];
                            app.Value_MapSlider.Value = NodeData(2);
                        end
                        
                        
                    case 7
                        SizeROI = size(app.XMapToolsData.MapData.ROI.Data(NodeData(2)).ROI);
                        if length(SizeROI) > 2
                            app.MapSlider.Visible = 'on';
                            app.Value_MapSlider.Visible = 'on';
                            app.MapSlider.Limits = [1,SizeROI(3)];
                            app.MapSlider.Value = 1;
                            
                            app.Value_MapSlider.Visible = 'on';
                            app.Value_MapSlider.Limits = [1,SizeROI(3)];
                            app.Value_MapSlider.Value = 1;
                        else
                            app.MapSlider.Visible = 'off';
                            app.MapSlider.Limits = [1,2];
                            app.MapSlider.Value = 1;
                            
                            app.Value_MapSlider.Visible = 'off';
                            app.Value_MapSlider.Limits = [1,2];
                            app.Value_MapSlider.Value = 1;
                        end
                    otherwise
                        app.MapSlider.Visible = 'off';
                        app.Value_MapSlider.Visible = 'off';
                end
            end
            
            %MapSlider
            
            if isequal(NodeData(1),8) && NodeData(2) > 0 && isequal(NodeData(3),0)
                if app.XMapToolsData.MapData.Im.Types(NodeData(2)) > 1
                    PlotMap_CreateDisplayMultiMap(app,SelectedNodes,SelectedAdditional);
                    return
                end
            end
            
            PlotMap_DisplaySelectedMap(app,SelectedNodes.NodeData,SelectedAdditional)
            
            %keyboard
        end

        % Node text changed function: TreeData_Main
        function TreeData_MainNodeTextChanged(app, event)
            node = event.Node;
            NodeData = node.NodeData;
            
            if isequal(NodeData(1),1) && isequal(NodeData(2),0)
                app.Node_It.Text = 'Intensity';
            end
            if isequal(NodeData(1),2) && isequal(NodeData(2),0)
                app.Node_Qt.Text = 'Quanti';
            end
            if isequal(NodeData(1),3) && isequal(NodeData(2),0)
                app.Node_Me.Text = 'Merged';
            end
            if isequal(NodeData(1),4) && isequal(NodeData(2),0)
                app.Node_Re.Text = 'Results';
            end
            
            if isequal(NodeData(1),1) && NodeData(2)>0
                % maybe one day we could allow the user to change the
                % element, but this would require to check that the new
                % entry is in the database
                app.Node_It.Children(NodeData(2)).Text = app.XMapToolsData.MapData.It.Names{NodeData(2)};
            end
            
            % Qt
            if isequal(NodeData(1),2) && NodeData(2)>0 && isequal(NodeData(3),0)
                % We save the new name
                app.XMapToolsData.MapData.Qt.Names{NodeData(2)} =  node.Text;
            end
            
            if isequal(NodeData(1),2) && NodeData(2)>0 && NodeData(3)>0
                app.Node_Qt.Children(NodeData(2)).Children(NodeData(3)).Text = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElNames{NodeData(3)};
            end
            
            % Me
            if isequal(NodeData(1),3) && NodeData(2)>0 && isequal(NodeData(3),0)
                % We save the new name
                app.XMapToolsData.MapData.Me.Names{NodeData(2)} =  node.Text;
            end
            
            if isequal(NodeData(1),3) && NodeData(2)>0 && NodeData(3)>0
                app.Node_Me.Children(NodeData(2)).Children(NodeData(3)).Text = app.XMapToolsData.MapData.Me.Data(NodeData(2)).ElNames{NodeData(3)};
            end
            
            % Re
            if isequal(NodeData(1),4) && NodeData(2)>0 && isequal(NodeData(3),0)
                % We save the new name
                app.XMapToolsData.MapData.Re.Names{NodeData(2)} =  node.Text;
            end
            if isequal(NodeData(1),4) && NodeData(2)>0 && NodeData(3)>0
                app.XMapToolsData.MapData.Re.Data(NodeData(2)).Labels{NodeData(3)} = node.Text;
            end
            
            
            % Im
            if isequal(NodeData(1),8) && NodeData(2)>0 && isequal(NodeData(3),0)
                % We save the new name
                app.XMapToolsData.MapData.Im.Names{NodeData(2)} =  node.Text;
            end
            
            
        end

        % Selection changed function: TreeData_Additional
        function TreeData_AdditionalSelectionChanged(app, event)
            
            app.Classify_Button.Enable = 'off';
            app.Classify_Button.Tooltip = 'Add maps and select a training set to activate the classify tool';
            app.Masks_ButtonPlotPie.Enable = 'off';
            app.Classify_FilterLowProbPixels.Enable = 'off';
            app.Classify_FilterProbValue.Enable = 'off';
            app.Classify_FilterMaskFile.Enable = 'off';
            app.ProbabilityLabel.Enable = 'off';
            app.Classify_Modes_AddROI.Enable = 'off';
            app.Classify_PointCountingModes.Enable = 'off';
            app.Calibrate_GenerateDensity.Enable = 'off';
            app.SaveResultsMenu.Enable = 'off';
            app.SF_ExportMinCompositions.Enable = 'off';
            app.SF_ROI_menu.Enable = 'off';
            app.AddStandardsManual.Enable = 'off';
            app.Calibrate_LOD_menu.Enable = 'off';
            app.Calibrate_LOD_CalcButton.Enable = 'off';
            app.MaskMenu.Enable = 'off';
            
            if isempty(app.TreeData_Additional.SelectedNodes)
                
                % Update the GUI (SPECIAL: unsupervised k-mean classification)
                app.Classify_Button.Enable = 'off';
                if isequal (app.Classification_Menu.Value,'Unsupervised k-means')
                    if ~isempty(app.Classify_ElemList.Value)
                        app.Classify_Button.Enable = 'on';
                        app.Classify_Button.Tooltip = 'Unsupervised classification';
                    end
                end
                
                % If possible we reactivate the classify button
                if ~isempty(app.Classify_ElemList.Value)
                    if ~isempty(app.TreeData_Additional.SelectedNodes)
                        NodeDataSPEC = app.TreeData_Additional.SelectedNodes(1).NodeData;
                        if isequal(NodeDataSPEC(1),12) && NodeDataSPEC(2) > 0 && isequal(NodeDataSPEC(3),0)
                            app.Classify_Button.Enable = 'on';
                            app.Classify_Button.Tooltip = 'Train a Classifier & Classify';
                        end
                    end
                end
                
                return
            end
            
            % -------------------------------------------------------------
            % Cleaning
            % -------------------------------------------------------------
            if isequal(app.TreeData_Additional.SelectedNodes.NodeData(1),16) || isequal(app.TreeData_Additional.SelectedNodes.NodeData(1),11)
                if app.HoldROIonMenu.Checked
                    % we keep the ROI in these cases
                else
                    ROI_DeleteROI(app);
                end
            else
                ROI_DeleteROI(app);
            end
            ContextMenu_AdditionalTree_Cleaning(app);
            
            DeactivatePlotZoomPanOptions(app);
            
            % -------------------------------------------------------------
            % Check for multiple selection
            % -------------------------------------------------------------
            for i=1:numel(app.TreeData_Additional.SelectedNodes)
                NodeData = app.TreeData_Additional.SelectedNodes(i).NodeData;
                SelectedAdditional(i,1:numel(NodeData)) = NodeData;
            end
            
            % Check multiple selection here first (add new constraints
            % here)
            if size(SelectedAdditional,1) > 1
                if isequal(mean(SelectedAdditional(:,1)),11)
                    % mask are selected
                    if isequal(mean(SelectedAdditional(:,2)),SelectedAdditional(1,2))
                        % only a single mask file is selected
                        if SelectedAdditional(:,3) > ones(size(SelectedAdditional(:,3)))
                            % masks are selected (and not 'none' that has
                            % an index of 1)
                        else
                            app.TreeData_Additional.SelectedNodes = [];
                            % Maybe we should reset the context menu?
                            return
                        end
                    else
                        app.TreeData_Additional.SelectedNodes = [];
                        return
                    end
                else
                    app.TreeData_Additional.SelectedNodes = [];
                    return
                end
            else
                % -------------------------------------------------------------
                % Activate the context menu (single selection)
                % -------------------------------------------------------------
                % Note that it is much simpler to activate the context menu
                % only for the selected node, because we cannot in MATLAB 2020a
                % know from which node the context menu is called.
                
                if isequal(NodeData(1),11) && NodeData(2) > 0 && isequal(NodeData(3),0)
                    % A maskfile is selected: menu 4_IDE
                    app.Node_Masks.Children(NodeData(2)).ContextMenu = app.ContextMenu_AdditionalTree_4_IDE;
                end
                if isequal(NodeData(1),11) && NodeData(2) > 0 && NodeData(3) > 1
                    % A mask is selected (> 1, means not "none): menu 3_I
                    app.Node_Masks.Children(NodeData(2)).Children(NodeData(3)).ContextMenu = app.ContextMenu_AdditionalTree_6_IC;
                end
                
                if isequal(NodeData(1),12) && NodeData(2) > 0 && isequal(NodeData(3),0)
                    % A TrainingSet is selected: menu 2_ID
                    app.Node_TrainingSet.Children(NodeData(2)).ContextMenu = app.ContextMenu_AdditionalTree_7_DD; %ContextMenu_AdditionalTree_2_ID;
                end
                if isequal(NodeData(1),12) && NodeData(2) > 0 && NodeData(3)>0
                    if length(NodeData) > 3
                        if NodeData(4) > 0
                            app.Node_TrainingSet.Children(NodeData(2)).Children(NodeData(3)).Children(NodeData(4)).ContextMenu = app.ContextMenu_AdditionalTree_2_ID;
                        else
                            app.Node_TrainingSet.Children(NodeData(2)).Children(NodeData(3)).ContextMenu = app.ContextMenu_AdditionalTree_2_ID;
                        end
                    else
                        % A PhaseDef is selected: menu 2_ID
                        app.Node_TrainingSet.Children(NodeData(2)).Children(NodeData(3)).ContextMenu = app.ContextMenu_AdditionalTree_2_ID;
                    end
                    
                end
                
                if isequal(NodeData(1),13) && isequal(NodeData(2),1) && NodeData(3)>0
                    
                    if isequal(length(NodeData),4)
                        if NodeData(4)>0
                            % A phase in a segmentation scheme is selected menu 2_ID
                            app.Node_SegScheme.Children(NodeData(3)).Children(NodeData(4)).ContextMenu = app.ContextMenu_AdditionalTree_2_ID;
                        end
                        if isequal(NodeData(4),0)
                            % A segmentation scheme is selected menu 2_ID
                            app.Node_SegScheme.Children(NodeData(3)).ContextMenu = app.ContextMenu_AdditionalTree_2_ID;
                        end
                    end
                    
                end
                
                if isequal(NodeData(1),16)
                    
                    if NodeData(2) > 0 && isequal(NodeData(3),0) && isequal(NodeData(4),0)
                        app.Node_LOD.Children(NodeData(2)).ContextMenu = app.ContextMenu_AdditionalTree_1_D;
                    end
                    
                end
                
                
                % -------------------------------------------------------------
                % Update the GUI (MaskFiles)
                % -------------------------------------------------------------
                if isequal(NodeData(1),11) && NodeData(2) > 0 && isequal(NodeData(3),0)
                    % A maskfile is selected
                    app.Masks_ButtonPlotPie.Enable = 'on';
                    app.Calibrate_GenerateDensity.Enable = 'on';
                    
                    app.Classify_FilterLowProbPixels.Enable = 'on';
                    app.Classify_FilterProbValue.Enable = 'on';
                    app.ProbabilityLabel.Enable = 'on';
                    app.Classify_FilterMaskFile.Enable = 'on';
                    
                    app.Classify_Modes_AddROI.Enable = 'on';
                    app.Classify_PointCountingModes.Enable = 'on';
                    app.Classify_FilterMaskFile.Enable = 'on';
                    
                    app.TabGroup.SelectedTab  = app.InformationTab;
                    app.MapInfo_TextArea.Value = '';
                    
                    %app.TreeData_Main.SelectedNodes = [];
                end
                
                
                % -------------------------------------------------------------
                % Update the GUI (Training Set)
                % -------------------------------------------------------------
                if ~isequal(NodeData(1),12)
                    app.Classify_AddTrainingSet.Enable = 'off';
                end
                if isequal(NodeData(1),12) && isequal(NodeData(2),0) && isequal(NodeData(3),0)
                    % Training Sets is selected
                    app.Classify_AddTrainingSet.Enable = 'on';
                    app.Classify_AddTrainingSet.Tooltip = 'Add a training set';
                    app.Classify_NameField.Value = ['PhaseDef_',num2str(numel(app.XMapToolsData.TrainingSet.Nb)+1)];
                end
                if isequal(NodeData(1),12) && NodeData(2) > 0 && isequal(NodeData(3),0)
                    % A given Training Sets is selected
                    app.Classify_AddTrainingSet.Enable = 'on';
                    app.Classify_AddTrainingSet.Tooltip = 'Add a phase';
                    app.Classify_NameField.Value = ['Phase_',num2str(app.XMapToolsData.TrainingSet.Nb(NodeData(2))+1)];
                    
                    if ~isempty(app.Classify_ElemList.Value)
                        app.Classify_Button.Enable = 'on';
                        app.Classify_Button.Tooltip = 'Train a Classifier & Classify';
                    end
                end
                if isequal(NodeData(1),12) && NodeData(2) > 0 && NodeData(3) > 0
                    % A phase is selected
                    app.Classify_AddTrainingSet.Enable = 'on';
                    app.Classify_AddTrainingSet.Tooltip = 'Add a ROI (Region Of Interest)';
                    app.Classify_NameField.Value = 'ROI';
                end
                
                % -------------------------------------------------------------
                % Update the GUI (SPECIAL: unsupervised k-mean classification)
                % -------------------------------------------------------------
                if isequal (app.Classification_Menu.Value,'Unsupervised k-means')
                    if ~isempty(app.Classify_ElemList.Value)
                        app.Classify_Button.Enable = 'on';
                        app.Classify_Button.Tooltip = 'Unsupervised classification';
                    end
                end
                
                
                
                % -------------------------------------------------------------
                % Update the GUI (Segment)
                % -------------------------------------------------------------
                if isequal(NodeData(1:2),[13,1])
                    if isequal(NodeData(3),0)
                        app.Segment_NameField.Value = ['SegmentationScheme_',num2str(numel(app.XMapToolsData.SegScheme.Names)+1)];
                    else
                        if isequal(NodeData(4),0)
                            app.Segment_NameField.Value = ['Phase_',num2str(numel(app.XMapToolsData.SegScheme.Data(NodeData(3)).Names)+1)];
                            if numel(app.XMapToolsData.SegScheme.Data(NodeData(3)).Names) > 0
                                app.Segment_SegmentButton.Enable = 'on';
                                app.Segment_TrySegmentationButton.Enable = 'on';
                            end
                        else
                            if isempty(app.XMapToolsData.SegScheme.Data(NodeData(3)).ROI(NodeData(4)).Coordinates)
                                app.Segment_NameField.Value = ['Add Range'];
                            else
                                app.Segment_NameField.Value = ['Edit Range'];
                            end
                            
                        end
                    end
                    
                    if length(NodeData) >= 4
                        
                        MainSelectedNode = app.TreeData_Main.SelectedNodes;
                        
                        if isempty(MainSelectedNode)
                            return
                        else
                            MainNodeData = MainSelectedNode.NodeData;
                        end
                        
                        if NodeData(4) > 0 && isequal(MainNodeData(1),6) && MainNodeData(2) > 0
                            
                            if ~isempty(app.XMapToolsData.SegScheme.Data(NodeData(3)).ROI(NodeData(4)).Coordinates)
                                %keyboard
                                app.EditField_LiveMin.Value = app.XMapToolsData.SegScheme.Data(NodeData(3)).ROI(NodeData(4)).Coordinates(1);
                                app.EditField_LiveMax.Value = app.XMapToolsData.SegScheme.Data(NodeData(3)).ROI(NodeData(4)).Coordinates(2);
                                
                                caxis(app.FigMain,[app.EditField_LiveMin.Value,app.EditField_LiveMax.Value])
                                app.hVerticalLines(2).Value = app.EditField_LiveMin.Value;
                                app.hVerticalLines(3).Value = app.EditField_LiveMax.Value;
                                
                                return
                            end
                            
                        end
                    end
                else
                    app.Segment_SegmentButton.Enable = 'off';
                    app.Segment_TrySegmentationButton.Enable = 'off';
                end
                
                
                % -------------------------------------------------------------
                % Update the GUI (Calibrate)
                % -------------------------------------------------------------
                if isequal(NodeData(1),11)
                    if NodeData(2) > 0 && isequal(NodeData(3),0) && size(app.XMapToolsData.Standards.Coord,1) > 1
                        app.ButtonCalibrate.Enable = 'on';
                        app.ButtonCalibrate.Tooltip = 'Open Calibration Assistant for EPMA Data';
                    elseif NodeData(2) > 0 && isequal(NodeData(3),0) && length(app.XMapToolsData.MapStandards(1).Names) > 1
                        app.ButtonCalibrate_LAICPMS.Enable = 'on';
                    else
                        app.ButtonCalibrate.Enable = 'off';
                        app.ButtonCalibrate.Tooltip = 'Import standards and select a maskfile to activate the calibration tools';
                        
                        app.ButtonCalibrate_LAICPMS.Enable = 'off';
                    end
                else
                    app.ButtonCalibrate.Enable = 'off';
                    app.ButtonCalibrate.Tooltip = 'Import standards and select a maskfile to activate the calibration tools';
                    
                    app.ButtonCalibrate_LAICPMS.Enable = 'off';
                end
                
                
                % -------------------------------------------------------------
                % Update the GUI (LOD)
                % -------------------------------------------------------------
                if isequal(NodeData(1),16)
                    if NodeData(4) >= 1
                        app.Calibrate_LOD_menu.Enable = 'off';               % Option deactiavted in 4.3 because irrelevant
                        app.Calibrate_LOD_CalcButton.Enable = 'off';         % Option deactiavted in 4.3 because irrelevant
                    end
                end
                
            end
            
            % Check for mask file to be plotted and if necessary display it
            if isequal(size(SelectedAdditional,1),1)
                if isequal(SelectedAdditional(1),11) && SelectedAdditional(2) > 0 && isequal(SelectedAdditional(3),0)
                    app.MaskMenu.Enable = 'on';
                    PlotMaskFile(app,SelectedAdditional(2));
                    ROI_DeleteROI(app);
                    return
                end
                if length(SelectedAdditional) > 3
                    if isequal(SelectedAdditional(1),11) && SelectedAdditional(2) > 0 && SelectedAdditional(3) > 0 && isequal(SelectedAdditional(4),1)
                        PlotSubMaskFile(app,SelectedAdditional);
                        return
                    end
                end
            end
            
            % Check for ROI shape to be plotted
            if isequal(SelectedAdditional(1),12) && SelectedAdditional(2) > 0 && SelectedAdditional(3) > 0
                
                if length(SelectedAdditional) > 3
                    ROI_Idx = SelectedAdditional(4);
                    EditMode = 1;
                else
                    ROI_Idx = [1:length(app.XMapToolsData.TrainingSet.Data(SelectedAdditional(2)).ROI(SelectedAdditional(3)).Types)];
                    EditMode = 0;
                end
                
                for i = 1:length(ROI_Idx)
                    Method = char(app.XMapToolsData.TrainingSet.Data(SelectedAdditional(2)).ROI(SelectedAdditional(3)).Types{ROI_Idx(i)});
                    
                    if isempty(Method)
                        return
                    end
                    
                    Coordinates = app.XMapToolsData.TrainingSet.Data(SelectedAdditional(2)).ROI(SelectedAdditional(3)).Data(ROI_Idx(i)).Coordinates;
                    
                    switch Method
                        case 'Rectangle ROI'
                            app.SelectedROI = drawrectangle(app.FigMain,'Position',Coordinates,'Color',[0.57,0.00,0.69]);
                        case 'Polygon ROI'
                            app.SelectedROI = drawpolygon(app.FigMain,'Position',Coordinates,'Color',[0.57,0.00,0.69]);
                        case 'Ellipse ROI'
                            app.SelectedROI = drawellipse(app.FigMain,'Center',Coordinates(1:2),'SemiAxes',Coordinates(3:4),'RotationAngle',Coordinates(5),'Color',[0.57,0.00,0.69]);
                        case 'Circle ROI'
                            app.SelectedROI = drawcircle(app.FigMain,'Center',Coordinates(1:2),'Radius',Coordinates(3),'Color',[0.57,0.00,0.69]);
                    end
                    
                    if EditMode
                        app.SelectedROI.InteractionsAllowed = 'all';
                        
                        app.ROIobjectListener = addlistener(app.SelectedROI, 'ROIMoved', @(varargin)ROI_MovingShape(app, app.SelectedROI));
                        app.ROIobjectListener_Delete = addlistener(app.SelectedROI, 'DeletingROI', @(varargin)ROI_DeletingShape(app, app.SelectedROI));
                        
                        app.Classify_NameField.Value = app.TreeData_Additional.SelectedNodes.Text;
                    else
                        app.SelectedROI.InteractionsAllowed = 'none';
                    end
                end
                return
            end
            
            if isequal(SelectedAdditional(1),14)
                % This plots all (0) or a given spot (value > 0)
                Standards_PlotStdROI(app,SelectedAdditional(2));
                
                if SelectedAdditional(2) > 0
                    app.Node_Standards.Children(NodeData(2)).ContextMenu = app.ContextMenu_AdditionalTree_5_DE;
                end
                
                if length(app.Node_Standards.Children) > 1
                    app.AddStandardsManual.Enable = 'on';
                end
                
                return
            end
            
            
            if isequal(SelectedAdditional(1),15)
                % First attempt to implement...
                
                if SelectedAdditional(2) > 0
                    app.TreeData_Main.SelectedNodes = [];
                    PlotMap_DisplaySelectedMap(app, -1, SelectedAdditional)
                end
                
                
            end
            
            if isequal(SelectedAdditional(1),16)
                % First attempt to implement...
                
                if SelectedAdditional(3) > 0
                    app.TreeData_Main.SelectedNodes = [];
                    PlotMap_DisplaySelectedMap(app, -2, SelectedAdditional)
                end
                
                
            end
            
            selectedNodes = app.TreeData_Main.SelectedNodes;
            
            % Temp we update the figure... BUT WE SHOULD CHECK HERE WHAT TO
            % DO...
            if ~isempty(selectedNodes)
                PlotMap_DisplaySelectedMap(app,selectedNodes.NodeData,SelectedAdditional);
            end
            
        end

        % Node text changed function: TreeData_Additional
        function TreeData_AdditionalNodeTextChanged(app, event)
            node = event.Node;
            
            NodeData = node.NodeData;
            
            if isequal(NodeData(1),11) && isequal(NodeData(2),0)
                app.Node_Masks.Text = 'Mask files';
            end
            if isequal(NodeData(1),12) && isequal(NodeData(2),0)
                app.Node_TrainingSet.Text = 'Training Sets (Classification)';
            end
            if isequal(NodeData(1),13)
                if isequal(NodeData(2),0)
                    app.Node_Corrections.Text = 'Segmentations & Corrections';
                else
                    if isequal(NodeData(3),0)
                        if isequal(NodeData(2),1)
                            app.Node_Corrections.Children(1).Text = 'Schemes (Segmentation)';
                        elseif isequal(NodeData(2),2)
                            app.Node_Corrections.Children(2).Text = 'Correction Filters';
                        end
                    end
                end
            end
            if isequal(NodeData(1),14) && isequal(NodeData(2),0)
                app.Node_Standards.Text = 'Standards (Spots)';
            end
            
            if isequal(NodeData(1),15) && isequal(NodeData(2),0)
                app.Node_MapStandards.Text = 'Standards (Maps)';
            end
            
            if isequal(NodeData(1),16)
                if isequal(NodeData(2),0)
                    app.Node_LOD.Text = 'LOD';
                    
                    % we update the mineral name
                elseif isequal(NodeData(3),0)
                    app.XMapToolsData.MapLOD.Names{NodeData(2)} = node.Text;
                end
            end
            
            
            if isequal(NodeData(1),11) && ~isequal(NodeData(2),0)    % Edit names of mask files
                if isequal(NodeData(3),0)
                    % we change the name of the "mask file"
                    app.XMapToolsData.MapData.MaskFile.Names{NodeData(2)} =  node.Text;
                else
                    % we change the "phase" name
                    app.XMapToolsData.MapData.MaskFile.Masks(NodeData(2)).Names{NodeData(3)} =  node.Text;
                end
            end
            
            if isequal(NodeData(1),12) && ~isequal(NodeData(2),0)    % Edit names of training sets
                if isequal(NodeData(3),0)
                    % we change the name of the "training set"
                    app.XMapToolsData.TrainingSet.Names{NodeData(2)} =  node.Text;
                else
                    % we change the "phase" name
                    app.XMapToolsData.TrainingSet.Data(NodeData(2)).Names{NodeData(3)} =  node.Text;
                end
            end
            
            if isequal(NodeData(1),13) && isequal(NodeData(2),1)  % Edit the Schemes
                if isequal(NodeData(4),0)
                    % We change the name of the Scheme
                    app.XMapToolsData.SegScheme.Names{NodeData(3)} =  node.Text;
                else
                    app.XMapToolsData.SegScheme.Data(NodeData(3)).Names{NodeData(4)} =  node.Text;
                end
            end
            
            
            
        end

        % Selection change function: TabButtonGroup
        function TabButtonGroupSelectionChanged(app, event)
            
            app.Workspace_ProjectImportMenu.Checked = 0;
            app.Workspace_ClassifyMenu.Checked = 0;
            app.Workspace_CalibrateMenu.Checked = 0;
            app.Workspace_FunctionsMenu.Checked = 0;
            app.Workspace_SegmentCTMenu.Checked = 0;
            app.Workspace_AddonsMenu.Checked = 0;
            app.Workspace_OptionsMenu.Checked = 0;
            app.Workspace_DeveloperMenu.Checked = 0;
            
            switch app.TabButtonGroup.SelectedTab.Title
                case 'PROJECT & IMPORT'
                    app.Workspace_ProjectImportMenu.Checked = 1;
                case 'CLASSIFY'
                    app.Workspace_ClassifyMenu.Checked = 1;
                case 'CALIBRATE'
                    app.Workspace_CalibrateMenu.Checked = 1;
                case 'FUNCTIONS'
                    app.Workspace_FunctionsMenu.Checked = 1;
                case 'SEGMENT (CT)'
                    app.Workspace_SegmentCTMenu.Checked = 1;
                case 'ADD-ONS'
                    app.Workspace_AddonsMenu.Checked = 1;
                case 'OPTIONS'
                    app.Workspace_OptionsMenu.Checked = 1;
                case 'DEVELOPER'
                    app.Workspace_DeveloperMenu.Checked = 1;
            end
            
            
            %             selectedTab = app.TabButtonGroup.SelectedTab;
            %
            %             switch selectedTab.Title
            %                 case 'CLASSIFY'
            %                     % Check if there are some maps available in Intensity
            %                     NbMaps = numel(app.XMapToolsData.MapData.It.Names);
            %                     NbMaps = numel(app.XMapToolsData.MapData.Qt.Names);
            %                     NbMaps = numel(app.XMapToolsData.MapData.Me.Names);
            %                     if NbMaps > 2
            %                         app.Classify_AddAllElem.Enable = 'on';
            %                         app.Classify_RunPCAButton.Enable = 'on';
            %                     end
            %
            %
            %             end
            
            %TreeData_MainSelectionChanged(app);
            %TreeData_AdditionalSelectionChanged(app);
            
        end

        % Callback function
        function HistLimits_ValueChanged(app, event)
            PlotMap_AdjustMinMax(app);
            
        end

        % Callback function
        function HistLimits_MinValueChanging(app, event)
            app.SliderMinHandle.Value = event.Value;
        end

        % Callback function
        function HistLimits_MaxValueChanging(app, event)
            app.SliderMaxHandle.Value = event.Value;
        end

        % Window button motion function: XMapTools_GUI
        function XMapTools_GUIWindowButtonMotion(app, event)
            if ~isempty(app.Hist_dragging)
                coords = event.IntersectionPoint;
                if coords(1) >= app.Hist_dragging_limits(1) && coords(1) <= app.Hist_dragging_limits(2)
                    set(app.Hist_dragging,'Value',coords(1));
                    if isequal(app.Hist_dragging_which,1)
                        app.EditField_LiveMin.Value = coords(1);
                    elseif isequal(app.Hist_dragging_which,2)
                        app.EditField_LiveMax.Value = coords(1);
                    end
                else
                    set(app.Hist_dragging,'Value',app.Hist_dragging_limits(1)+0.5*(app.Hist_dragging_limits(2)-app.Hist_dragging_limits(1)));
                    app.Hist_dragging = [];
                end
            end
            
            % version (2) from Nils...
            if ~isempty(app.hLineToDrag) % here we check if any line is pressed the idea is to be able to add multiple checks for other plots or lines
                %get the moue position from the UIaxis
                currentPoint   = app.FigHistLive.CurrentPoint;
                x            = currentPoint(2,1);
                y            = currentPoint(2,2);
                
                %get the axis limits
                xlim = app.FigHistLive.XLim;
                ylim = app.FigHistLive.YLim;
                
                inside = x >= xlim(1) & x <= xlim(2) & y >= ylim(1) & y <= ylim(2);
                
                if inside %if were still inside move the vertical lines
                    set(app.hLineToDrag, 'Value', x);
                    xposlines=[get(app.hVerticalLines(2),'Value') get(app.hVerticalLines(3),'Value')];
                    app.EditField_LiveMin.Value = min(xposlines);
                    app.EditField_LiveMax.Value = max(xposlines);
                    app.EditField_LivePosition.Value = app.hVerticalLines(1).Value;
                else
                    app.hLineToDrag=[];
                end
            end
            
        end

        % Window button down function: XMapTools_GUI
        function XMapTools_GUIWindowButtonDown(app, event)
            app.WindowsButtonDownValue = 1;
        end

        % Window button up function: XMapTools_GUI
        function XMapTools_GUIWindowButtonUp(app, event)
            
            app.WindowsButtonDownValue = 0;
            
            if ~isempty(app.Hist_dragging)
                coords = event.IntersectionPoint;
                set(app.Hist_dragging,'Value',coords(1));
                app.Hist_dragging = [];
                if app.hVerticalLines(2).Value < app.hVerticalLines(3).Value
                    caxis(app.FigMain,[app.hVerticalLines(2).Value,app.hVerticalLines(3).Value])
                    
                    UpdateLimitsForMultiPlot(app);
                    
                    % Sampling option
                    if ~isempty(app.ROI_sampling)
                        if isvalid(app.ROI_sampling)
                            switch app.ROI_sampling.Type
                                case 'images.roi.polyline'
                                    Sampling_ROI_changed_line(app,app.ROI_sampling);
                                case {'images.roi.circle','images.roi.polygon'}
                                    Sampling_ROI_changed_shape(app,app.ROI_sampling);
                                case 'images.roi.rectangle'
                                    Sampling_ROI_changed_strip(app,app.ROI_sampling);
                            end
                        end
                    end
                    UpdateInfoWindow(app)
                end
            end
            
            if ~isempty(app.hLineToDrag) % here i check if the line was pressed before
                app.hLineToDrag = [];
                %  here i update my xlimits and plots
                PlotMap_AdjustMinMax(app)
                
                % for me i like to keep the xlimits the same in the histogram since i
                % have the possibility to take the small steps with the buttons i
                % implemented. but here could be the point to update the xlimits
                % app.FigHistLive.XLim(1)=0.0XX*Value of the xline with smaller x
                % app.FigHistLive.XLim(2)=1.0XX*Value of the xline with larger x
            end
            
            
            
            
        end

        % Button down function: XMapTools_GUI
        function XMapTools_GUIButtonDown(app, event)
            % not used for now
            
        end

        % Value changed function: EditField_LiveMax, 
        % EditField_LiveMin
        function Plot_LiveMinMaxValueChanged(app, event)
            
            if isempty(app.hVerticalLines)
                return
            end
            
            ValueMin = app.EditField_LiveMin.Value;
            ValueMax = app.EditField_LiveMax.Value;
            
            if ValueMin < ValueMax
                
                app.hVerticalLines(2).Value = ValueMin;
                app.hVerticalLines(3).Value = ValueMax;
                
                PlotMap_AdjustMinMax(app);
                
                UpdateLimitsForMultiPlot(app);
            end
            
        end

        % Button pushed function: Button_FigMain_AutoContrast
        function Button_FigMain_AutoContrastPushed(app, event)
            %
            if isequal(app.Jiahui,0) || isequal(app.Jiahui,1)
                app.Jiahui = app.Jiahui + 1;
            else
                app.Jiahui = 0;
            end
            
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
            
            %             Q = quantile(SortedData,12);
            %             Min1 = Q(1);
            %             Max1 = Q(10);
            %             Min2 = Q(2);
            %             Max2 = Q(9);
            
            % Current values:
            [Min,Max] = caxis(app.FigMain);
            
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
            
            % Nils implementation (not used)
            % climit=[std2(ImageData)-mean(ImageData,'all','omitnan') std2(ImageData)+mean(ImageData,'all','omitnan')];
            
            % Update plots
            %             caxis(app.FigMain,[Min,Max])
            app.hVerticalLines(2).Value = double(Min);
            app.hVerticalLines(3).Value = double(Max);
            app.EditField_LiveMin.Value = double(Min);
            app.EditField_LiveMax.Value = double(Max);
            
            % Sampling option
            if ~isempty(app.ROI_sampling)
                switch app.ROI_sampling.Type
                    case 'images.roi.polyline'
                        Sampling_ROI_changed_line(app,app.ROI_sampling);
                        %case {'images.roi.circle','images.roi.polygon'}
                        %Sampling_ROI_changed_shape(app,app.ROI_sampling);
                        %case 'images.roi.rectangle'
                        %Sampling_ROI_changed_strip(app,app.ROI_sampling);
                end
            end
            
            PlotMap_AdjustMinMax(app)
            
        end

        % Callback function: Button_FigMain_MedianFilter, 
        % MedianFilterMenu
        function Button_FigMain_MedianFilterPushed(app, event)
            
            if isequal(app.Jiahui,2)
                app.Jiahui = app.Jiahui + 1;
            else
                app.Jiahui = 0;
            end
            
            app.Data2Plot = app.FigMain.Children(end).CData;
            MedianFilter = app.LastMedianFilter + 2;
            
            ColorLimits = caxis(app.FigMain);
            
            NewData2Plot = medfilt2(app.Data2Plot,[MedianFilter,MedianFilter]);
            
            app.FigMain.Children(end).CData = NewData2Plot;  % imagesc(app.FigMain,NewData2Plot)
            
            caxis(app.FigMain,ColorLimits)
            
            app.LastMedianFilter = MedianFilter;
            
        end

        % Callback function
        function Button_FigMain_ZoomInPushed(app, event)
            app.FigMain.Interactions = ZoomInteraction;
            
            
            keyboard
            %zoom(app.FigMain,'on','Direction','in');
        end

        % Callback function
        function Button_FigMain_ZoomOutPushed(app, event)
            app.FigMain.Interactions = panInteraction;
        end

        % Menu selected function: AutoContrastMenu
        function Menu_AutoContrastMenuSelected(app, event)
            Button_FigMain_AutoContrastPushed(app);
        end

        % Value changed function: Options_ColormapDropDown, 
        % Options_ColormapResEditField, Options_LogColormap, 
        % Options_LowerCheckBox, Options_LowerColor, 
        % Options_UpperCheckBox, Options_UpperColor
        function Options_UpdateColormap(app, event)
            PlotMap_UpdateColormap(app,'Map',0);
            app.SaveRequired = 1;
        end

        % Value changed function: Options_Colorbar_Inverse
        function Options_Colorbar_InverseValueChanged(app, event)
            PlotMap_UpdateColormap(app,'Map',0);
            app.SaveRequired = 1;
        end

        % Callback function: ProjectSave, SaveProjectMenu
        function ProjectSaveButtonPushed(app, event)
            if ~isempty(app.CurrentProject)
                SaveProject(app,app.CurrentProject);
            else
                SaveProjectAs(app);
            end
        end

        % Callback function: ProjectSaveAs, SaveProjectAsMenu
        function ProjectSaveAsButtonPushed(app, event)
            SaveProjectAs(app);
        end

        % Callback function: OpenProjectMenu, ProjectOpen
        function ProjectOpenButtonPushed(app, event)
            
            f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
            [filename, pathname] = uigetfile('*.mat', 'Pick a project file');
            delete(f);
            figure(app.XMapTools_GUI)
            if isequal(filename,0)
                return
            end
            
            cd(pathname);
            
            LoadProjectFile(app,pathname(1:end-1),filename);
            
            app.XMapTools_LastDir = cd;
            app.XMapTools_GUI.Name = [app.XMapTools_VER,' - ',char(app.XMapTools_LastDir)];
            
            app.SaveRequired = 0;
            
        end

        % Value changed function: ProjectName_Options
        function ProjectName_OptionsValueChanged(app, event)
            if length(app.CurrentProject) > 3
                [filepath,name,ext] = fileparts(app.CurrentProject);
                switch app.ProjectName_Options.Value
                    case 'File'
                        app.ProjectName_DisplayField.Value = [name,ext];
                        app.ProjectName_DisplayField.HorizontalAlignment = 'Center';
                        app.ProjectName_DisplayField.FontSize = 11;
                    case 'Path'
                        app.ProjectName_DisplayField.Value = [filepath];
                        app.ProjectName_DisplayField.HorizontalAlignment = 'Left';
                        app.ProjectName_DisplayField.FontSize = 10;
                end
            end
        end

        % Button pushed function: ButtonConvertLaserData
        function ButtonConvertLaserDataPushed(app, event)
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Waiting for data input from the Converter for LAICPMS';
            
            waitfor(Converter_LAICPMS(app));
            
            close(app.WaitBar);
        end

        % Button pushed function: Classify_AddTrainingSet
        function Classify_AddTrainingSetButtonPushed(app, event)
            
            NamesDB = app.DensityData.Names;
            [uniqueA] = unique(NamesDB,'first');
            AddNames = { ...
                '--- GENERIC PHASE NAMES WITH INCICES ---','Phase_1','Phase_2','Phase_3','Phase_4','Phase_5','Phase_6','Phase_7','Phase_8','Phase_9','Phase_10','Phase_11','Phase_12','Phase_13','Phase_14','Phase_15','Other', ...
                '--- FULL DATABASE IS SHOWN BELLOW ---'};
            
            PhaseList = [app.MineralColorData.Names,AddNames,uniqueA];
            
            app.SaveRequired = 1;
            
            app.Classify_AddTrainingSet.Enable = 'Off';
            
            SelectedNodes = app.TreeData_Additional.SelectedNodes;
            if isempty(SelectedNodes)
                return
            end
            NodeData = SelectedNodes.NodeData;
            
            TrainingSet = app.XMapToolsData.TrainingSet;
            Idx = numel(TrainingSet.Types);
            
            % --------------------------------------
            % ADD A NEW TRAINING SET
            if isequal(NodeData,[12,0,0])
                
                Idx = Idx+1;
                Name = app.Classify_NameField.Value;
                
                TrainingSet.Names{Idx} = Name;
                TrainingSet.Types(Idx) = 1;                 % Option for later use
                TrainingSet.MaskSignature(Idx) = 0;
                TrainingSet.MaskNode(Idx) = 0;
                TrainingSet.Nb(Idx) = 0;
                TrainingSet.Data(Idx).Names = {};
                TrainingSet.Data(Idx).ROI(1).Types = {};
                TrainingSet.Data(Idx).ROI(1).Data(1).Coordinates = [];
                %TrainingSet.Data(Idx).Density = 0;
                
                p = uitreenode(app.Node_TrainingSet,'Text',char(TrainingSet.Names(Idx)),'NodeData',[12,Idx,0]);
                
                expand(app.Node_TrainingSet);
                app.TreeData_Additional.SelectedNodes = p;
                
                if isequal(app.Classify_ActivateAssistant.Value,1)
                    app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
                    app.WaitBar.Message = 'Select phase(s) and press OK to continue...';
                    waitfor(Selector(app,PhaseList,'Select phases in the list below','Multiple'));
                    close(app.WaitBar)
                    
                    for i = 1:length(app.ExchangeSelector)
                        Name = char(app.ExchangeSelector{i});
                        IdxMask = TrainingSet.Nb(Idx)+1;        % we add one
                        TrainingSet.Data(Idx).Names{IdxMask} = Name;
                        TrainingSet.Data(Idx).ROI(IdxMask).Types{1} = '';
                        TrainingSet.Data(Idx).ROI(IdxMask).Data(1).Coordinates = [];
                        
                        p = uitreenode(app.Node_TrainingSet.Children(Idx),'Text',char(Name),'NodeData',[12,Idx,IdxMask]);
                        
                        TrainingSet.Nb(Idx) = IdxMask;
                        
                        expand(app.Node_TrainingSet.Children(Idx));
                    end
                end
            else
                Idx = NodeData(2);  % 28.04.23 solve an issue when multiple training sets are defined
            end
            
            % --------------------------------------
            % ADD A NEW PHASE
            if isequal(NodeData(1),12) && NodeData(2) > 0 && isequal(NodeData(3),0)
                
                if isequal(app.Classify_ActivateAssistant.Value,1)
                    
                    app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
                    app.WaitBar.Message = 'Select phase(s) and press OK to continue...';
                    waitfor(Selector(app,PhaseList,'Select phases in the list below','Multiple'));
                    close(app.WaitBar)
                    
                    if length(app.ExchangeSelector) > 0
                        for i = 1:length(app.ExchangeSelector)
                            Name = app.ExchangeSelector{i};
                            IdxMask = TrainingSet.Nb(NodeData(2))+1;        % we add one
                            TrainingSet.Data(NodeData(2)).Names{IdxMask} = Name;
                            TrainingSet.Data(NodeData(2)).ROI(IdxMask).Types{1} = '';
                            TrainingSet.Data(NodeData(2)).ROI(IdxMask).Data(1).Coordinates = [];
                            
                            p = uitreenode(app.Node_TrainingSet.Children(NodeData(2)),'Text',char(Name),'NodeData',[12,NodeData(2),IdxMask]);
                            
                            TrainingSet.Nb(NodeData(2)) = IdxMask;
                            
                            expand(app.Node_TrainingSet.Children(NodeData(2)));
                        end
                    else
                        Name = app.Classify_NameField.Value;
                        IdxMask = TrainingSet.Nb(NodeData(2))+1;        % we add one
                        TrainingSet.Data(NodeData(2)).Names{IdxMask} = Name;
                        TrainingSet.Data(NodeData(2)).ROI(IdxMask).Types{1} = '';
                        TrainingSet.Data(NodeData(2)).ROI(IdxMask).Data(1).Coordinates = [];
                        
                        p = uitreenode(app.Node_TrainingSet.Children(NodeData(2)),'Text',char(Name),'NodeData',[12,NodeData(2),IdxMask]);
                        
                        TrainingSet.Nb(NodeData(2)) = IdxMask;
                        
                        expand(app.Node_TrainingSet.Children(NodeData(2)));
                    end
                else
                    Name = app.Classify_NameField.Value;
                    IdxMask = TrainingSet.Nb(NodeData(2))+1;        % we add one
                    TrainingSet.Data(NodeData(2)).Names{IdxMask} = Name;
                    TrainingSet.Data(NodeData(2)).ROI(IdxMask).Types{1} = '';
                    TrainingSet.Data(NodeData(2)).ROI(IdxMask).Data(1).Coordinates = [];
                    
                    p = uitreenode(app.Node_TrainingSet.Children(NodeData(2)),'Text',char(Name),'NodeData',[12,NodeData(2),IdxMask]);
                    
                    TrainingSet.Nb(NodeData(2)) = IdxMask;
                    
                    expand(app.Node_TrainingSet.Children(NodeData(2)));
                end
                
                %                 Name = app.Classify_NameField.Value;
                %                 IdxMask = TrainingSet.Nb(NodeData(2))+1;        % we add one
                %                 TrainingSet.Data(NodeData(2)).Names{IdxMask} = Name;
                %                 TrainingSet.Data(NodeData(2)).ROI(IdxMask).Types{1} = '';
                %                 TrainingSet.Data(NodeData(2)).ROI(IdxMask).Data(1).Coordinates = [];
                %
                %                 p = uitreenode(app.Node_TrainingSet.Children(NodeData(2)),'Text',char(Name),'NodeData',[12,NodeData(2),IdxMask]);
                %
                %                 TrainingSet.Nb(NodeData(2)) = IdxMask;
                %
                %                 expand(app.Node_TrainingSet.Children(NodeData(2)));
            end
            
            % --------------------------------------
            % ADD/REPLACE A NEW ROI
            if isequal(NodeData(1),12) && NodeData(2) > 0 && NodeData(3) > 0
                
                ROI_DeleteROI(app); % update display (only)
                
                % New data structure (March 2021)
                WeReplace = 0;
                if length(NodeData) > 3
                    PositionROI = NodeData(4);
                    WeReplace = 1;
                else
                    if isempty(TrainingSet.Data(Idx).ROI(NodeData(3)).Types)
                        TrainingSet.Data(Idx).ROI(NodeData(3)).Types{1} = '';
                    end
                    
                    if isempty(TrainingSet.Data(Idx).ROI(NodeData(3)).Types{1})
                        PositionROI = 1;
                    else
                        PositionROI = length(TrainingSet.Data(Idx).ROI(NodeData(3)).Types)+1;
                    end
                end
                
                Method = app.Classify_ROI_Menu.Value;
                switch Method
                    case 'Rectangle ROI'
                        DrawingMode(app,'on','Rectangle');
                        app.SelectedROI = drawrectangle(app.FigMain,'Color',[0.57,0.00,0.69]);
                        DrawingMode(app,'off');
                        TrainingSet.Data(Idx).ROI(NodeData(3)).Data(PositionROI).Coordinates = app.SelectedROI.Position;
                    case 'Polygon ROI'
                        DrawingMode(app,'on','Polygon');
                        app.SelectedROI = drawpolygon(app.FigMain,'Color',[0.57,0.00,0.69]);
                        DrawingMode(app,'off');
                        TrainingSet.Data(Idx).ROI(NodeData(3)).Data(PositionROI).Coordinates = app.SelectedROI.Position;
                    case 'Ellipse ROI'
                        DrawingMode(app,'on','Ellipse');
                        app.SelectedROI = drawellipse(app.FigMain,'Color',[0.57,0.00,0.69]);
                        DrawingMode(app,'off');
                        TrainingSet.Data(Idx).ROI(NodeData(3)).Data(PositionROI).Coordinates = [app.SelectedROI.Center,app.SelectedROI.SemiAxes,app.SelectedROI.RotationAngle];
                    case 'Circle ROI'
                        DrawingMode(app,'on','Circle');
                        app.SelectedROI = drawcircle(app.FigMain,'Color',[0.57,0.00,0.69]);
                        DrawingMode(app,'off');
                        TrainingSet.Data(Idx).ROI(NodeData(3)).Data(PositionROI).Coordinates = [app.SelectedROI.Center,app.SelectedROI.Radius];
                end
                
                app.ROIobjectListener = addlistener(app.SelectedROI, 'ROIMoved', @(varargin)ROI_MovingShape(app, app.SelectedROI));
                app.ROIobjectListener_Delete = addlistener(app.SelectedROI, 'DeletingROI', @(varargin)ROI_DeletingShape(app, app.SelectedROI));
                
                TrainingSet.Data(Idx).ROI(NodeData(3)).Types{PositionROI} = Method;
                app.SelectedROI_Idx = NodeData;
                
                if ~WeReplace
                    p = uitreenode(app.Node_TrainingSet.Children(NodeData(2)).Children(NodeData(3)),'Text',Method,'NodeData',[12,NodeData(2),NodeData(3),PositionROI]);
                else
                    app.Node_TrainingSet.Children(NodeData(2)).Children(NodeData(3)).Children(PositionROI).Text = Method;
                end
                
            end
            %keyboard
            
            app.Classify_AddTrainingSet.Enable = 'On';
            
            app.XMapToolsData.TrainingSet = TrainingSet;
            TreeData_AdditionalSelectionChanged(app, event);
            
        end

        % Button pushed function: Classify_AddAllElem
        function Classify_AddAllElemButtonPushed(app, event)
            
            % When add all is selected, previsouly added maps are
            % eliminated.
            
            if isempty(app.TreeData_Main.SelectedNodes)
                uialert(gcbf,'You must select a map in the primary menu to add input data to the classification function!','XMapTools – Error');
            end
            
            NodeData = app.TreeData_Main.SelectedNodes.NodeData;
            
            if isequal(NodeData(1),1) && NodeData(2) > 0
                % Intensity
                app.Classify_ElemList_ElList = app.XMapToolsData.MapData.It.Names;
                app.Classify_ElemList_ElSource = NodeData(1)*ones(size(app.Classify_ElemList_ElList));
                app.Classify_ElemList_CodePos = 1:length(app.Classify_ElemList_ElList);
                app.Classify_ElemList_CodePosMap = zeros(size((app.Classify_ElemList_ElList)));
                DispCode = Classify_GenerateDispCode(app,app.Classify_ElemList_ElList);
                if NodeData(2) > 0
                    app.Classify_Manage_Elem.Icon = '057-minus.png';
                end
                app.Classify_ElemList.Value = DispCode;
                
                
                % NOT ADJUSTED YET / Same next function
                
                ElNames = app.XMapToolsData.MapData.It.Names;
                DispCode = Classify_GenerateDispCode(app,ElNames);
                app.Classify_Manage_Elem.Icon = '057-minus.png';
                app.Classify_ElemList.Value = DispCode;
            end
            
            if isequal(NodeData(1),2) || isequal(NodeData(1),3)
                % Quanti & Merged
                if isequal(NodeData(1),2)
                    app.Classify_ElemList_ElList = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElNames;
                else
                    app.Classify_ElemList_ElList = app.XMapToolsData.MapData.Me.Data(NodeData(2)).ElNames;
                end
                app.Classify_ElemList_ElSource = NodeData(1)*ones(size(app.Classify_ElemList_ElList));
                app.Classify_ElemList_CodePos = NodeData(2)*ones(size(app.Classify_ElemList_ElList));
                app.Classify_ElemList_CodePosMap = 1:length(app.Classify_ElemList_ElList);
                DispCode = Classify_GenerateDispCode(app,app.Classify_ElemList_ElList);
                if NodeData(2) > 0
                    app.Classify_Manage_Elem.Icon = '057-minus.png';
                end
                app.Classify_ElemList.Value = DispCode;
            end
            
            if ~isempty(app.Classify_ElemList.Value)
                app.Classify_Manage_Elem_DeleteAll.Enable = 'on';
            end
            
            %             if isequal(NodeData(1),3)
            %                 % Merged
            %                 app.Classify_ElemList_ElList = app.XMapToolsData.MapData.Me.Data(NodeData(2)).ElNames;
            %                 app.Classify_ElemList_ElSource = NodeData(1)*ones(size(app.Classify_ElemList_ElList));
            %                 app.Classify_ElemList_CodePos = NodeData(2)*ones(size(app.Classify_ElemList_ElList));
            %                 app.Classify_ElemList_CodePosMap = 1:length(app.Classify_ElemList_ElList);
            %                 DispCode = Classify_GenerateDispCode(app,app.Classify_ElemList_ElList);
            %                 if NodeData(2) > 0
            %                     app.Classify_Manage_Elem.Icon = '057-minus.png';
            %                 end
            %                 app.Classify_ElemList.Value = DispCode;
            %             end
            
            TreeData_AdditionalSelectionChanged(app);
        end

        % Button pushed function: Classify_Manage_Elem
        function Classify_Manage_ElemButtonPushed(app, event)
            SelectedNodes = app.TreeData_Main.SelectedNodes;
            
            NodeData = SelectedNodes.NodeData;
            
            ElemList = app.Classify_ElemList_ElList;
            [IsElem,IdxElem] = ismember(SelectedNodes.Text,ElemList);
            
            if IsElem
                % we delete
                app.Classify_ElemList_ElList(IdxElem) = [];
                app.Classify_ElemList_ElSource(IdxElem) = [];
                app.Classify_ElemList_CodePos(IdxElem) = [];
                app.Classify_ElemList_CodePosMap(IdxElem) = [];
                app.Classify_Manage_Elem.Icon = '056-plus.png';
                app.Classify_Manage_Elem.Tooltip = ['Add "',char(SelectedNodes.Text),'" for classification'];
            else
                if isequal(NodeData(1),1) && NodeData(2) > 0   % Intensity
                    app.Classify_ElemList_ElList{end+1} = char(SelectedNodes.Text);
                    app.Classify_ElemList_ElSource(end+1) = NodeData(1);
                    app.Classify_ElemList_CodePos(end+1) = NodeData(2);
                    app.Classify_ElemList_CodePosMap(end+1) = 0;
                end
                if isequal(NodeData(1),2) || isequal(NodeData(1),3) % Quanti  & Results
                    app.Classify_ElemList_ElList{end+1} = char(SelectedNodes.Text);
                    app.Classify_ElemList_ElSource(end+1) = NodeData(1);
                    app.Classify_ElemList_CodePos(end+1) = NodeData(2);
                    app.Classify_ElemList_CodePosMap(end+1) = NodeData(3);
                end
                if isequal(NodeData(1),5) && NodeData(2) > 0   % Other
                    app.Classify_ElemList_ElList{end+1} = char(SelectedNodes.Text);
                    app.Classify_ElemList_ElSource(end+1) = NodeData(1);
                    app.Classify_ElemList_CodePos(end+1) = NodeData(2);
                    app.Classify_ElemList_CodePosMap(end+1) = 0;
                end
                app.Classify_Manage_Elem.Icon = '057-minus.png';
                app.Classify_Manage_Elem.Tooltip = ['Take "',char(SelectedNodes.Text),'" out'];
            end
            
            DispCode = Classify_GenerateDispCode(app,app.Classify_ElemList_ElList);
            app.Classify_ElemList.Value = DispCode;
            
            if ~isempty(app.Classify_ElemList.Value)
                app.Classify_Manage_Elem_DeleteAll.Enable = 'on';
            end
            
            TreeData_AdditionalSelectionChanged(app);
        end

        % Button pushed function: Classify_Manage_Elem_DeleteAll
        function Classify_Manage_Elem_DeleteAllButtonPushed(app, event)
            
            SelectedNodes = app.TreeData_Main.SelectedNodes;
            NodeData = SelectedNodes.NodeData;
            
            app.Classify_ElemList_ElList = [];
            app.Classify_ElemList_ElSource = [];
            app.Classify_ElemList_CodePos= [];
            app.Classify_ElemList_CodePosMap= [];
            app.Classify_Manage_Elem.Icon = '056-plus.png';
            app.Classify_Manage_Elem.Tooltip = ['Add "',char(SelectedNodes.Text),'" for classification'];
            
            DispCode = Classify_GenerateDispCode(app,app.Classify_ElemList_ElList);
            app.Classify_ElemList.Value = DispCode;
            
            app.Classify_Manage_Elem_DeleteAll.Enable = 'off';
            
        end

        % Button pushed function: Classify_Button
        function Classify_ButtonPushed(app, event)
            
            fprintf('\n\n%s\n','**************************************************');
            fprintf('%s\n','Classification');
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'XMapTools is reading the training set';
            
            ClassificationMethod = app.Classification_Menu.Value;
            
            % -------------------------------------------------------------
            % [1.1] Generate "Data" containing all pixels of selected elements
            %           Row: Px;            Col: Elem;
            
            ElemListSel_Ori = app.Classify_ElemList_ElList;
            ElemListMap_Ori = app.Classify_ElemList_ElList;
            try
                MapSize = zeros(length(ElemListSel_Ori),2);
                for i = 1:length(ElemListSel_Ori)
                    switch app.Classify_ElemList_ElSource(i)
                        case 1
                            % Intensity data
                            Data(:,i) = app.XMapToolsData.MapData.It.Data(app.Classify_ElemList_CodePos(i)).Map(:);
                            Data2D(i).Map = app.XMapToolsData.MapData.It.Data(app.Classify_ElemList_CodePos(i)).Map;
                            Resolution(i,:) = size(app.XMapToolsData.MapData.It.Data(app.Classify_ElemList_CodePos(i)).Map);
                        case 2
                            % Quanti data
                            Data(:,i) = app.XMapToolsData.MapData.Qt.Data(app.Classify_ElemList_CodePos(i)).CData(app.Classify_ElemList_CodePosMap(i)).Map(:);
                            Data2D(i).Map = app.XMapToolsData.MapData.Qt.Data(app.Classify_ElemList_CodePos(i)).CData(app.Classify_ElemList_CodePosMap(i)).Map;
                            Resolution(i,:) = size(app.XMapToolsData.MapData.Qt.Data(app.Classify_ElemList_CodePos(i)).CData(app.Classify_ElemList_CodePosMap(i)).Map);
                        case 3
                            % Results data
                            MapSize(i,:) = size(app.XMapToolsData.MapData.Me.Data(app.Classify_ElemList_CodePos(i)).CData(app.Classify_ElemList_CodePosMap(i)).Map);
                            Data(:,i) = app.XMapToolsData.MapData.Me.Data(app.Classify_ElemList_CodePos(i)).CData(app.Classify_ElemList_CodePosMap(i)).Map(:);
                            Data2D(i).Map = app.XMapToolsData.MapData.Me.Data(app.Classify_ElemList_CodePos(i)).CData(app.Classify_ElemList_CodePosMap(i)).Map;
                            Resolution(i,:) = size(app.XMapToolsData.MapData.Me.Data(app.Classify_ElemList_CodePos(i)).CData(app.Classify_ElemList_CodePosMap(i)).Map);
                        case 5
                            % other data
                            Data(:,i) = app.XMapToolsData.MapData.Ot.Data(app.Classify_ElemList_CodePos(i)).Map(:);
                            Data2D(i).Map = app.XMapToolsData.MapData.Ot.Data(app.Classify_ElemList_CodePos(i)).Map;
                            Resolution(i,:) = size(app.XMapToolsData.MapData.Ot.Data(app.Classify_ElemList_CodePos(i)).Map);
                    end
                end
            catch ME
                MapSize = zeros(length(ElemListSel_Ori),2);
                for i = 1:length(ElemListSel_Ori)
                    switch app.Classify_ElemList_ElSource(i)
                        case 1
                            MapSize(i,:) = size(app.XMapToolsData.MapData.It.Data(app.Classify_ElemList_CodePos(i)).Map);
                        case 2
                            MapSize(i,:) = size(app.XMapToolsData.MapData.Qt.Data(app.Classify_ElemList_CodePos(i)).CData(app.Classify_ElemList_CodePosMap(i)).Map);
                        case 3
                            % Results data
                            MapSize(i,:) = size(app.XMapToolsData.MapData.Me.Data(app.Classify_ElemList_CodePos(i)).CData(app.Classify_ElemList_CodePosMap(i)).Map);
                    end
                end
                
                Text2Display = {'Classification is not possible because the maps are not the same size:',' '}
                for i = 1:length(ElemListSel_Ori)
                    Text2Display{end+1} = ['Map #',num2str(i),' ',ElemListSel_Ori{i},': ',num2str(MapSize(i,1)),' ',num2str(MapSize(i,2))]
                end
                
                uialert(app.XMapTools_GUI, Text2Display, 'XMapTools – Error')
                close(app.WaitBar);
                return
            end
            
            % Reproducibility
            if isequal(app.Classify_Reproducibility.Value,1)
                rng(app.Classify_ReproducibilityValue.Value); % For reproducibility
            else
                rng('shuffle');
            end
            
            if ~isequal(ClassificationMethod,'Unsupervised k-means')
                
                % Check training set and prepare variables
                TrainingSet = app.XMapToolsData.TrainingSet;
                NodeData = app.TreeData_Additional.SelectedNodes.NodeData;
                if ~isequal(NodeData(1),12) || isequal(NodeData(2),0) || ~isequal(NodeData(3),0)
                    uialert(app.XMapTools_GUI, 'You need to select a training set in the secondary menu', 'XMapTools – Error')
                    close(app.WaitBar);
                    return
                end
                
                % Check the integrity of the training set
                CheckROI = zeros(size(TrainingSet.Data(NodeData(2)).Names));
                for i = 1:length(CheckROI)
                    if ~isempty(TrainingSet.Data(NodeData(2)).ROI(i).Types{1})
                        CheckROI(i) = 1;
                    end
                end
                if sum(CheckROI) < length(CheckROI)
                    Cell2Disp = {'The selected training set cannot be used for classification. Missing ROI definition for:'};
                    Idx = find(CheckROI == 0);
                    for i = 1:length(Idx)
                        Cell2Disp = [Cell2Disp, TrainingSet.Data(NodeData(2)).Names{Idx(i)}];
                    end
                    uialert(app.XMapTools_GUI, Cell2Disp, 'XMapTools – Error')
                    close(app.WaitBar);
                    return
                end
                
                % Filter for Classification of a selection (10.07.22)
                SubMasking = 0;
                if isequal(TrainingSet.Types(NodeData(2)),2)
                    MaskNode = TrainingSet.MaskNode(NodeData(2));
                    
                    Signatures = extractfield(app.XMapToolsData.MapData.MaskFile.Masks,'Signature');
                    MaskInd = find(ismember(Signatures,app.XMapToolsData.TrainingSet.MaskSignature(NodeData(2))));
                    
                    if ~isempty(MaskInd)
                        SelectedMaskMap = app.XMapToolsData.MapData.MaskFile.Masks(MaskInd).MaskMap;
                        MaskMap = nan(size(SelectedMaskMap));
                        MaskMapZ = zeros(size(SelectedMaskMap));
                        PxSelected = find(SelectedMaskMap == MaskNode-1);
                        MaskMap(PxSelected) = ones(size(PxSelected));
                        MaskMapZ(PxSelected) = ones(size(PxSelected));
                        
                        Data = Data .* repmat(MaskMap(:),1,size(Data,2));
                        
                        SumData = sum(Data,2);
                        SelSubMasking.UnselectedData = find(isnan(SumData));
                        SelSubMasking.SelectedData = find(~isnan(SumData));
                        SelSubMasking.MaskMapZ = MaskMapZ;
                        SelSubMasking.MaskMapZUnselected = zeros(size(MaskMapZ));
                        IndxZeros = find(MaskMapZ == 0);
                        SelSubMasking.MaskMapZUnselected(IndxZeros) = 1;
                        
                        % Eliminate NaNs (fixed on 21.11.23)
                        for i = 1:size(Data, 2)
                            Data(SelSubMasking.UnselectedData,i) = 0;
                        end
                        
                        for i = 1:length(Data2D)
                            Data2D(i).Map = Data2D(i).Map .* MaskMap;
                        end
                        
                        %                     figure
                        %                     tiledlayout("flow")
                        %                     for i = 1:length(Data2D)
                        %                         nexttile
                        %                         imagesc(Data2D(i).Map), axis image, colorbar
                        %                     end
                        
                        SubMasking = 1;
                        
                    end
                end
                
                % check if ElIdx was used (apparently not)
                
                % -------------------------------------------------------------
                % [1.2] Dataset selection (DataTable):
                if isequal(app.Classify_MapsCheckBox.Value,0) && isequal(app.Classify_PCA1CheckBox.Value,0) && isequal(app.Classify_PCA2CheckBox.Value,0)
                    uialert(gcbf,'You must select at least one dataset (Maps or PCA)!','XMapTools – Error');
                    %waitfor(errordlg('You must select at least one dataset (Maps or PCA)!', 'XMapTools – Error'));
                    figure(app.XMapTools_GUI);
                    close(app.WaitBar);
                    return
                end
                
                ElemListMap = {};
                DataTable = [];
                
                % - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                % ADD Map Data
                if isequal(app.Classify_MapsCheckBox.Value,1)
                    % Normalization (always applied for now):
                    DataTable_REF = Data./repmat(mean(Data,1,'omitnan'),size(Data,1),1);   % as in old XMapTools version
                    
                    DataTable = Data;
                    for i = 1:size(DataTable,2)
                        
                        % Added in 3.4 for submasking classification
                        if SubMasking
                            IdxSelected = SelSubMasking.SelectedData;
                        else
                            IdxSelected = 1:length(DataTable(:,1));
                        end
                        
                        switch app.Classify_ScalingMode.Value
                            case 'Robust'
                                DataTable(IdxSelected,i) = (DataTable(IdxSelected,i) - median(DataTable(IdxSelected,i),'omitnan')) / iqr(DataTable(IdxSelected,i));  % Robust scaller
                                
                            case 'MinMax'
                                DataTable(IdxSelected,i) = rescale(DataTable(IdxSelected,i));   % MinMax scaling
                                
                            case 'Mean'
                                DataTable(IdxSelected,i) = DataTable(IdxSelected,i) / mean(DataTable(IdxSelected,i),'omitnan'); % as in old XMapTools version
                                
                            case 'NoScaling'
                                % nothing
                        end
                        % DataTable(:,i) = log10(DataTable(:,i));   % log scaling (does not work well)
                    end
                    % DataTable = (DataTable-repmat(mean(DataTable,1),size(DataTable,1),1))./repmat(std(DataTable,1),size(DataTable,1),1);
                    
                    ElemListMap = ElemListMap_Ori;
                    
                    disp(' ')
                    switch app.Classify_ScalingMode.Value
                        
                        case 'Robust'
                            disp('Data scaling: robust')
                        case 'MinMax'
                            disp('Data scaling: min-max')
                        case 'Mean'
                            disp('Data scaling: mean')
                        case 'NoScaling'
                            disp('Data scaling: no scaling')
                    end
                    disp(' ')
                end
                
                % - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                % Add PCA-M ("mean" scaling)
                if isequal(app.Classify_PCA1CheckBox.Value,1)
                    
                    DataNorm2 = Data./repmat(mean(Data,1),size(Data,1),1);
                    
                    % Filter NaN for submasks
                    WhereNaN = find(isnan(DataNorm2));
                    DataNorm2(WhereNaN) = 0;
                    
                    [coeff2,score,latent,tsquared,explained2,mu] = pca(DataNorm2');
                    
                    CumSum2 = cumsum(explained2);
                    Where2 = find(CumSum2 > 98);
                    if Where2(1) > 1
                        Cutoff2 = Where2(1)-1;
                    else
                        Cutoff2 = 1;
                    end
                    
                    for i = 1:Cutoff2
                        ElemListMap{end+1} = ['PCA-M_PC_',num2str(i),'(',num2str(explained2(i)),'%)'];
                        DataTable(:,end+1) = coeff2(:,i);
                    end
                end
                
                % - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                % PCA-N ("normalisation" scaling)
                if isequal(app.Classify_PCA2CheckBox.Value,1)
                    
                    DataNorm3 = (Data - repmat(mean(Data,1),size(Data,1),1))./repmat(std(Data),size(Data,1),1);
                    
                    % Filter NaN for submasks
                    WhereNaN = find(isnan(DataNorm3));
                    DataNorm3(WhereNaN) = 0;
                    
                    [coeff3,score,latent,tsquared,explained3,mu] = pca(DataNorm3');
                    
                    CumSum3 = cumsum(explained3);
                    Where3 = find(CumSum3 > 98);
                    if Where3(1) > 1
                        Cutoff3 = Where3(1)-1;
                    else
                        Cutoff3 = 1;
                    end
                    
                    for i = 1:Cutoff3
                        ElemListMap{end+1} = ['PCA-N_PC_',num2str(i),'(',num2str(explained3(i)),'%)'];
                        DataTable(:,end+1) = coeff3(:,i);
                    end
                    
                end
                
                % - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                % TEXTURE FILTERS
                if isequal(app.Classify_TEXFCheckBox.Value,1)
                    
                    app.WaitBar.Message = 'XMapTools is applying texture filters';
                    
                    Mtx_KF = zeros(size(Data2D(1).Map,1),size(Data2D(1).Map,2),length(Data2D));
                    Mtx_LG = Mtx_KF;
                    Mtx_LSD = Mtx_KF;
                    Mtx_LE = Mtx_KF;
                    Mtx_LR = Mtx_KF;
                    
                    h_kernel = [-1 0 1];
                    
                    for i = 1:length(Data2D)
                        Mtx_KF(:,:,i) = imfilter(rescale(Data2D(i).Map,0,1),h_kernel,'conv');
                        
                        [Filter] = imgradient(Data2D(i).Map);
                        Mtx_LG(:,:,i) = rescale(Filter);
                        
                        [Filter] = stdfilt(Data2D(i).Map,ones(3));
                        Mtx_LSD(:,:,i) = rescale(Filter);
                        
                        [Filter] = entropyfilt(Data2D(i).Map);
                        Mtx_LE(:,:,i) = rescale(Filter);
                        
                        [Filter] = rangefilt(Data2D(i).Map,ones(3));
                        Mtx_LR(:,:,i) = rescale(Filter);
                    end
                    
                    Textured_KF = sqrt(sum(Mtx_KF.^2,3));
                    Textured_LG = sqrt(sum(Mtx_LG.^2,3));
                    Textured_LSD = sqrt(sum(Mtx_LSD.^2,3));
                    Textured_LE = sqrt(sum(Mtx_LE.^2,3));
                    Textured_LR = sqrt(sum(Mtx_LR.^2,3));
                    
                    % ElemListMap{end+1} = ['Local Gradient'];
                    % DataTable(:,end+1) = Textured_LG(:);
                    
                    
                    % Composite Entropy Filter (Lanari & Tedeschi, 2024 – SP1)
                    app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
                    
                    % WinSize = [3,5,7,9,21,51,101];
                    WinSize = [3];
                    
                    Mtx_CEF = zeros(size(Data2D(1).Map,1),size(Data2D(1).Map,2),length(WinSize));
                    app.WaitBar.Value = 0;
                    
                    for i = 1:length(WinSize)
                        app.WaitBar.Message = ['Generating composite entropic image(s) (',num2str(i),'/',num2str(length(WinSize)),'; ',num2str(WinSize(i)),'-by-',num2str(WinSize(i)),')'];
                        Mtx_CEF_Temp = zeros(size(Data2D(1).Map,1),size(Data2D(1).Map,2),length(Data2D));
                        
                        for j = 1:length(Data2D)
                            Filter = entropyfilt(rescale(Data2D(j).Map,0,1),true(WinSize(i)));
                            Mtx_CEF_Temp(:,:,j) = rescale(Filter,0,1);
                        end
                        
                        Mtx_CEF(:,:,i) = sqrt(sum(Mtx_CEF_Temp.^2,3));
                        app.WaitBar.Value = i/length(WinSize);
                    end
                    
                    if 1 % Use and Plot entropic images
                        figure
                        tiledlayout('flow');
                        for i = 1:length(WinSize)
                            nexttile
                            imagesc(Mtx_CEF(:,:,i)), axis image, colorbar
                            title(['Composite entropic image ',num2str(WinSize(i)),'-by-',num2str(WinSize(i))])
                            
                            ElemListMap{end+1} = ['CEF_',num2str(WinSize(i))];
                            TexturedMap = Mtx_CEF(:,:,i);
                            DataTable(:,end+1) = TexturedMap(:);
                        end
                        colormap(gray)
                    end
                    
                    % Specific Entropy Filters
                    WinSize = [3,5,7];
                    WinSize = [3];      % A window size above 3 creates artifacts!
                    for i = 1:length(WinSize)
                        app.WaitBar.Message = ['Generating entropic images (',num2str(i),'/',num2str(length(WinSize)),'; ',num2str(WinSize(i)),'-by-',num2str(WinSize(i)),')'];
                        
                        for j = 1:length(Data2D)
                            ElNameTemp = ElemListMap_Ori{j};
                            if length(ElNameTemp) <= 2  % Exclude EDS
                                Filter = entropyfilt(rescale(Data2D(j).Map,0,1),true(WinSize(i)));
                                TheEM = rescale(Filter,0,1);
                                
                                if 0
                                    ElemListMap{end+1} = ['EF_',num2str(WinSize(i)),'_',ElemListMap_Ori{j}];
                                    DataTable(:,end+1) = TheEM(:);
                                end
                            end
                        end
                        
                        app.WaitBar.Value = i/length(WinSize);
                    end
                    
                    % Other plots
                    if 1
                        figure
                        tiledlayout('flow');
                        
                        nexttile
                        imagesc(Textured_KF), axis image, colorbar
                        title('KF') % not as sharp as the LG (sharper for detecting grain boundaries)
                        
                        nexttile
                        imagesc(Textured_LG), axis image, colorbar
                        title('LG')
                        
                        nexttile
                        imagesc(Textured_LSD), axis image, colorbar
                        title('LSD')
                        
                        nexttile
                        imagesc(Textured_LE), axis image, colorbar
                        title('LE')
                        
                        nexttile
                        imagesc(Textured_LR), axis image, colorbar
                        title('LR')
                        
                        colormap(gray)
                        
                    end
                    
                    %                 ElemListMap{end+1} = ['Local Standard Deviation'];
                    %                 DataTable(:,end+1) = Textured_LSD(:);
                    %
                    %                 ElemListMap{end+1} = ['Local Entropy'];
                    %                 DataTable(:,end+1) = Textured_LE(:);
                    %
                    %                 ElemListMap{end+1} = ['Local Range'];
                    %                 DataTable(:,end+1) = Textured_LR(:);
                    
                end
                
                app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
                app.WaitBar.Message = 'Finishing pre-classification tasks';
                
                fprintf('\n%s\t%s\n','Class','Description');
                for i = 1:length(ElemListMap)
                    fprintf('%.0f\t%s\n',i,ElemListMap{i});
                end
                
                %figure, imagesc(reshape(coeff(:,6),Resolution(1,:))), axis image, colorbar
                
                % -------------------------------------------------------------
                % [2] Generate "IdxTrain" containing the index of pixels
                % selected for each training dataset
                %           Row: Train. Px;     Col: Elem;
                %
                
                TrainSetList = TrainingSet.Data(NodeData(2)).Names;
                TrainSetIdx = [1:numel(TrainSetList)];
                NbPx = zeros(numel(TrainSetList),1);
                NbPx_Train = zeros(numel(TrainSetList),1);
                NbPx_Test = zeros(numel(TrainSetList),1);
                
                for i = 1:numel(TrainSetList)
                    IdXSelPxTable = [];
                    for j = 1:length(TrainingSet.Data(NodeData(2)).ROI(i).Types)
                        
                        Method = TrainingSet.Data(NodeData(2)).ROI(i).Types{j};
                        Coordinates = TrainingSet.Data(NodeData(2)).ROI(i).Data(j).Coordinates;
                        
                        switch Method
                            case 'Rectangle ROI'
                                ROI = drawrectangle(app.FigMain,'Position',Coordinates,'Color',[0.57,0.00,0.69]);
                            case 'Polygon ROI'
                                ROI = drawpolygon(app.FigMain,'Position',Coordinates,'Color',[0.57,0.00,0.69]);
                            case 'Ellipse ROI'
                                ROI = drawellipse(app.FigMain,'Center',Coordinates(1:2),'SemiAxes',Coordinates(3:4),'RotationAngle',Coordinates(5),'Color',[0.57,0.00,0.69]);
                            case 'Circle ROI'
                                ROI = drawcircle(app.FigMain,'Center',Coordinates(1:2),'Radius',Coordinates(3),'Color',[0.57,0.00,0.69]);
                        end
                        
                        Mask = createMask(ROI,Resolution(1,1),Resolution(1,2));
                        IdXSelPx = find(Mask);
                        %length(IdXSelPx)
                        NbPx(i) = NbPx(i)+numel(IdXSelPx);
                        IdXSelPxTable = [IdXSelPxTable;IdXSelPx];
                    end
                    
                    % Selection of 10% for testing
                    NbPx_Test(i) = floor(0.10*NbPx(i));
                    if NbPx_Test(i) < 1
                        NbPx_Test(i) = 1;
                    end
                    NbPx_Train(i) = NbPx(i)-NbPx_Test(i);
                    
                    p = randperm(NbPx(i));
                    
                    IdxTrain(1:NbPx_Train(i),i) = IdXSelPxTable(p(1:NbPx_Train(i)));
                    IdxTest(1:NbPx_Test(i),i) = IdXSelPxTable(p(NbPx_Train(i)+1:end));
                    
                end
                
                
                % -------------------------------------------------------------
                % [3] Generate "DataTrain" containing the training data
                %           Row: Composition;   Col: Elem;
                %
                SizeTrainingSet = sum(NbPx_Train);
                DataTrain_Comp = zeros(SizeTrainingSet,size(DataTable,2));
                DataTrain_Class = zeros(SizeTrainingSet,1);
                
                Prev = 0;
                for i = 1:numel(TrainSetList)
                    DataTrain_Comp(Prev+1:Prev+NbPx_Train(i),:) = DataTable(IdxTrain(1:NbPx_Train(i),i),:);
                    DataTrain_Class(Prev+1:Prev+NbPx_Train(i),1) = i*ones(NbPx_Train(i),1);
                    Prev = Prev+NbPx_Train(i);
                end
                
                % -------------------------------------------------------------
                % [4] Generate "DataTest" containing the test data
                %           Row: Composition;   Col: Elem;
                %
                SizeTestSet = sum(NbPx_Test);
                DataTest_Comp = zeros(SizeTestSet,size(DataTable,2));
                DataTest_Idx = zeros(SizeTestSet,1);
                DataTest_Class = zeros(SizeTestSet,1);
                
                Prev = 0;
                for i = 1:numel(TrainSetList)
                    DataTest_Comp(Prev+1:Prev+NbPx_Test(i),:) = DataTable(IdxTest(1:NbPx_Test(i),i),:);
                    DataTest_Idx(Prev+1:Prev+NbPx_Test(i)) = IdxTest(1:NbPx_Test(i),i);
                    DataTest_Class(Prev+1:Prev+NbPx_Test(i),1) = i*ones(NbPx_Test(i),1);
                    Prev = Prev+NbPx_Test(i);
                end
                
                % Check for NaN and replace by zeros
                WhereNaN = find(isnan(DataTrain_Comp));
                if ~isempty(WhereNaN)
                    DataTrain_Comp(WhereNaN) = zeros(size(WhereNaN));
                end
                
                WhereNaN = find(isnan(DataTest_Comp));
                if ~isempty(WhereNaN)
                    DataTest_Comp(WhereNaN) = zeros(size(WhereNaN));
                end
                
                app.WaitBar.Message = 'XMapTools is running numbers (Step 1 - Model Training)';
                
                figure
                tiledlayout('flow','tilespacing','tight')
                
                switch ClassificationMethod
                    
                    case 'Random Forest'
                        % TEST Random Forest
                        NewMaskFileName = [num2str(length(app.XMapToolsData.MapData.MaskFile.Names)+1),'_RandomForest'];
                        NewMaskFileType = 1;
                        
                        iNumBags = app.Classify_OptionValue.Value;
                        str_method = 'classification';
                        
                        disp(' '), disp(' ')
                        tic
                        BaggedEnsemble = TreeBagger(iNumBags,DataTrain_Comp,DataTrain_Class,'OOBPrediction','On','Method',str_method,'OOBPredictorImportance','on');
                        h = toc; disp(['Training (RF): ',num2str(h)])
                        
                        oobErrorBaggedEnsemble = oobError(BaggedEnsemble);
                        nexttile
                        plot(oobErrorBaggedEnsemble)
                        xlabel('Number of grown trees');
                        ylabel('Out-of-bag classification error');
                        title('Random Forest: Classification Error')
                        %print(figID, '-dpdf', sprintf('randomforest_errorplot_%s.pdf', date));
                        
                        oobPredict(BaggedEnsemble);
                        imp = BaggedEnsemble.OOBPermutedPredictorDeltaError;
                        
                        nexttile
                        bar(imp);
                        ylabel('Predictor importance estimates');
                        xlabel('Predictors');
                        title('Random Forest: Predictor Importance')
                        h = gca;
                        h.XTick = [1:length(ElemListMap)];
                        h.XTickLabel = ElemListMap;
                        h.XTickLabelRotation = 45;
                        h.TickLabelInterpreter = 'none';
                        
                        drawnow
                        %view(BaggedEnsemble.Trees{1},'mode','graph') % graphic description
                        
                        app.WaitBar.Message = 'XMapTools is running numbers (Step 2 - Classification)';
                        
                        tic
                        if SubMasking
                            [Res,Posterior,Cost] = predict(BaggedEnsemble,DataTable(SelSubMasking.SelectedData,:));
                        else
                            [Res,Posterior,Cost] = predict(BaggedEnsemble,DataTable);
                        end
                        h = toc; disp(['Classification (RF): ',num2str(h)])
                        
                        % Confusion chart
                        Res_DataTrain = predict(BaggedEnsemble,DataTrain_Comp);
                        ResNum_DataTrain = zeros(size(Res_DataTrain));
                        for i = 1:length(Res_DataTrain)
                            ResNum_DataTrain(i) = str2double(Res_DataTrain{i});
                        end
                        nexttile;
                        
                        TrainSetListConf = matlab.lang.makeUniqueStrings(TrainSetList);
                        
                        [NormValues,order] = confusionmat(DataTrain_Class,ResNum_DataTrain);
                        confusionchart(NormValues,TrainSetListConf, 'Title','Confusion Chart (Training dataset)');
                        
                        %PosteriorP = reshape(Posterior(:,6),Resolution(1,:));
                        %figure, imagesc(PosteriorP), axis image, colorbar
                        
                        ResNum = zeros(size(Res));
                        for i = 1:length(Res)
                            ResNum(i) = str2double(Res{i});
                        end
                        
                        if SubMasking
                            Posterior_Part = Posterior;
                            Posterior = zeros(length(DataTable(:,1)),size(Posterior_Part,2));
                            Posterior(SelSubMasking.SelectedData,:) = Posterior_Part;
                            
                            ResNum_Part = ResNum;
                            ResNum = zeros(length(DataTable(:,1)),1);
                            ResNum(SelSubMasking.SelectedData,:) = ResNum_Part;
                        end
                        PosteriorM = reshape(Posterior,[Resolution(1,:),size(Posterior,2)]);
                        
                        %IdxPos = find(PosteriorM(:) > 0);
                        %figure, histogram(PosteriorM(IdxPos),'Normalization','probability')
                        
                        %keyboard
                        
                        % The solution above is slow but this was not working:
                        %  ResNum = str2num(cell2mat(Res'));
                        
                        % https://ch.mathworks.com/help/stats/treebagger.predict.html
                        
                        
                    case 'Discriminant Analysis'
                        NewMaskFileName = [num2str(length(app.XMapToolsData.MapData.MaskFile.Names)+1),'_DiscriminantAnalysis'];
                        NewMaskFileType = 2;
                        
                        disp(' '), disp(' ')
                        tic
                        Mdl = fitcdiscr(DataTrain_Comp,DataTrain_Class); %,'OptimizeHyperparameters','auto');
                        h = toc; disp(['Training (DA): ',num2str(h)])
                        
                        nexttile;
                        [NormValues,order] = confusionmat(DataTrain_Class,resubPredict(Mdl));
                        
                        TrainSetListConf = matlab.lang.makeUniqueStrings(TrainSetList);
                        confusionchart(NormValues,TrainSetListConf, 'Title','Confusion Chart (Training dataset)');
                        drawnow
                        
                        app.WaitBar.Message = 'XMapTools is running numbers (Step 2 - Classification)';
                        
                        tic
                        if SubMasking
                            [ResNum,Posterior,Cost] = predict(Mdl,DataTable(SelSubMasking.SelectedData,:));  %[Res,Posterior,Cost] = predict(BaggedEnsemble,DataTable(SelSubMasking.SelectedData,:));
                        else
                            [ResNum,Posterior,Cost] = predict(Mdl,DataTable);
                        end
                        h = toc; disp(['Classification (DA): ',num2str(h)])
                        
                        if SubMasking
                            Posterior_Part = Posterior;
                            Posterior = zeros(length(DataTable(:,1)),size(Posterior_Part,2));
                            Posterior(SelSubMasking.SelectedData,:) = Posterior_Part;
                            
                            ResNum_Part = ResNum;
                            ResNum = zeros(length(DataTable(:,1)),1);
                            ResNum(SelSubMasking.SelectedData,:) = ResNum_Part;
                        end
                        
                        PosteriorM = reshape(Posterior,[Resolution(1,:),size(Posterior,2)]);
                        
                        
                    case 'Naive Bayes'
                        NewMaskFileName = [num2str(length(app.XMapToolsData.MapData.MaskFile.Names)+1),'_NaiveBayes'];
                        NewMaskFileType = 5;
                        
                        disp(' '), disp(' ')
                        tic
                        Mdl = fitcnb(DataTrain_Comp,DataTrain_Class); %,'OptimizeHyperparameters','auto');
                        h = toc; disp(['Training (NB): ',num2str(h)])
                        
                        nexttile;
                        [NormValues,order] = confusionmat(DataTrain_Class,resubPredict(Mdl));
                        
                        TrainSetListConf = matlab.lang.makeUniqueStrings(TrainSetList);
                        confusionchart(NormValues,TrainSetListConf, 'Title','Confusion Chart (Training dataset)');
                        drawnow
                        
                        app.WaitBar.Message = 'XMapTools is running numbers (Step 2 - Classification)';
                        
                        tic
                        if SubMasking
                            [ResNum,Posterior,Cost] = predict(Mdl,DataTable(SelSubMasking.SelectedData,:));  %[Res,Posterior,Cost] = predict(BaggedEnsemble,DataTable(SelSubMasking.SelectedData,:));
                        else
                            [ResNum,Posterior,Cost] = predict(Mdl,DataTable);
                        end
                        h = toc; disp(['Classification (NB): ',num2str(h)])
                        
                        if SubMasking
                            Posterior_Part = Posterior;
                            Posterior = zeros(length(DataTable(:,1)),size(Posterior_Part,2));
                            Posterior(SelSubMasking.SelectedData,:) = Posterior_Part;
                            
                            ResNum_Part = ResNum;
                            ResNum = zeros(length(DataTable(:,1)),1);
                            ResNum(SelSubMasking.SelectedData,:) = ResNum_Part;
                        end
                        
                        PosteriorM = reshape(Posterior,[Resolution(1,:),size(Posterior,2)]);
                        % Maybe we should save this matrix (though, the mixing
                        % seems not to be well catched)
                        
                        %keyboard
                        
                        
                    case 'Support Vector Machine'
                        NewMaskFileName = [num2str(length(app.XMapToolsData.MapData.MaskFile.Names)+1),'_SVM'];
                        NewMaskFileType = 3;
                        
                        disp(' '), disp(' ')
                        tic
                        Mdl = fitcecoc(DataTrain_Comp,DataTrain_Class);    % 'FitPosterior',1 (takes too long)
                        h = toc; disp(['Training (SVM): ',num2str(h)])
                        
                        nexttile;
                        [NormValues,order] = confusionmat(DataTrain_Class,resubPredict(Mdl));
                        
                        TrainSetListConf = matlab.lang.makeUniqueStrings(TrainSetList);
                        confusionchart(NormValues,TrainSetListConf, 'Title','Confusion Chart (Training dataset)');
                        drawnow
                        %                     NbCalc = size(DataTable,1);
                        %                     Posterior = zeros(NbCalc,length(Mdl.ClassNames));
                        %                     ResNum = zeros(NbCalc,1);
                        %                     Nb = 50;
                        %                     Compt = 0;
                        %                     ComptTot = numel(1:Nb:NbCalc);
                        %                     for i = 1:Nb:NbCalc
                        %                         [ResNum(i:i+Nb-1),~,~,Posterior(i:i+Nb-1,:)] = predict(Mdl,DataTable(i:i+Nb-1,:));
                        %                         Compt = Compt+1;
                        %
                        %                     end
                        
                        app.WaitBar.Message = 'XMapTools is running numbers (Step 2 - Classification)';
                        
                        tic
                        if SubMasking
                            [ResNum,Posterior,Cost] = predict(Mdl,DataTable(SelSubMasking.SelectedData,:));  %[Res,Posterior,Cost] = predict(BaggedEnsemble,DataTable(SelSubMasking.SelectedData,:));
                        else
                            [ResNum,Posterior,Cost] = predict(Mdl,DataTable);
                        end
                        h = toc; disp(['Classification (SVM): ',num2str(h)])
                        
                        if SubMasking
                            Posterior_Part = Posterior;
                            Posterior = zeros(length(DataTable(:,1)),size(Posterior_Part,2));
                            Posterior(SelSubMasking.SelectedData,:) = Posterior_Part;
                            
                            ResNum_Part = ResNum;
                            ResNum = zeros(length(DataTable(:,1)),1);
                            ResNum(SelSubMasking.SelectedData,:) = ResNum_Part;
                        end
                        
                        PosteriorM = reshape(Posterior,[Resolution(1,:),size(Posterior,2)]);
                        
                        %figure, imagesc(reshape(1+max(Posterior,[],2),Resolution(1,:))), colorbar
                        
                        
                    case 'Classification Tree'
                        NewMaskFileName = [num2str(length(app.XMapToolsData.MapData.MaskFile.Names)+1),'_ClassificationTree'];
                        NewMaskFileType = 4;
                        
                        disp(' '), disp(' ')
                        tic
                        tree = fitctree(DataTrain_Comp,DataTrain_Class); %,'OptimizeHyperparameters','auto'); % ,'AlgorithmForCategorical','PCA'
                        h = toc; disp(['Training (CT): ',num2str(h)])
                        
                        nexttile;
                        [NormValues,order] = confusionmat(DataTrain_Class,resubPredict(tree));
                        
                        TrainSetListConf = matlab.lang.makeUniqueStrings(TrainSetList);
                        confusionchart(NormValues,TrainSetListConf, 'Title','Confusion Chart (Training dataset)');
                        drawnow
                        
                        imp = predictorImportance(tree);
                        
                        nexttile;
                        bar(imp);
                        ylabel('Predictor importance estimates');
                        xlabel('Predictors');
                        title('Classification Tree: Predictor Importance')
                        h = gca;
                        h.XTick = [1:length(ElemListMap)];
                        h.XTickLabel = ElemListMap;
                        h.XTickLabelRotation = 45;
                        h.TickLabelInterpreter = 'none';
                        
                        app.WaitBar.Message = 'XMapTools is running numbers (Step 2 - Classification)';
                        
                        tic
                        if SubMasking
                            [ResNum,Posterior,Cost] = predict(tree,DataTable(SelSubMasking.SelectedData,:));
                        else
                            [ResNum,Posterior,Cost] = predict(tree,DataTable);
                        end
                        h = toc; disp(['Classification (CT): ',num2str(h)])
                        
                        if SubMasking
                            Posterior_Part = Posterior;
                            Posterior = zeros(length(DataTable(:,1)),size(Posterior_Part,2));
                            Posterior(SelSubMasking.SelectedData,:) = Posterior_Part;
                            
                            ResNum_Part = ResNum;
                            ResNum = zeros(length(DataTable(:,1)),1);
                            ResNum(SelSubMasking.SelectedData,:) = ResNum_Part;
                        end
                        
                        PosteriorM = reshape(Posterior,[Resolution(1,:),size(Posterior,2)]);
                        
                        
                    case 'k-Nearest Neighbor'
                        NewMaskFileName = [num2str(length(app.XMapToolsData.MapData.MaskFile.Names)+1),'_NearestNeighbor'];
                        NewMaskFileType = 6;
                        
                        disp(' '), disp(' ')
                        tic
                        Mdl = fitcknn(DataTrain_Comp,DataTrain_Class, 'NumNeighbors',5);   % 'KFold',10
                        h = toc; disp(['Training (KNN): ',num2str(h)])
                        
                        nexttile;
                        [NormValues,order] = confusionmat(DataTrain_Class,resubPredict(Mdl));
                        
                        TrainSetListConf = matlab.lang.makeUniqueStrings(TrainSetList);
                        confusionchart(NormValues,TrainSetListConf, 'Title','Confusion Chart (Training dataset)');
                        drawnow
                        
                        %classError = kfoldLoss(Mdl);
                        
                        app.WaitBar.Message = 'XMapTools is running numbers (Step 2 - Classification)';
                        
                        [ResNum] = predict(Mdl,DataTable);
                        
                        tic
                        if SubMasking
                            [ResNum] = predict(Mdl,DataTable(SelSubMasking.SelectedData,:));
                        else
                            [ResNum] = predict(Mdl,DataTable);
                        end
                        h = toc; disp(['Classification (KNN): ',num2str(h)])
                        
                        if SubMasking
                            ResNum_Part = ResNum;
                            ResNum = zeros(length(DataTable(:,1)),1);
                            ResNum(SelSubMasking.SelectedData,:) = ResNum_Part;
                        end
                        
                        %keyboard
                        
                    case 'k-means'
                        
                        NewMaskFileName = [num2str(length(app.XMapToolsData.MapData.MaskFile.Names)+1),'_kmeans'];
                        NewMaskFileType = 7;
                        
                        for i = 1:numel(TrainSetList)
                            IdxMin = find(DataTrain_Class == i);
                            StartingMtx(i,:) = median(DataTrain_Comp(IdxMin,:),1);
                        end
                        
                        app.WaitBar.Message = 'XMapTools is running numbers (Step 2 - Classification)';
                        
                        tic
                        if SubMasking
                            ResNum = kmeans(DataTable(SelSubMasking.SelectedData,:),size(StartingMtx,1),'Display','Iter','Start',StartingMtx);
                        else
                            ResNum = kmeans(DataTable,size(StartingMtx,1),'Display','Iter','Start',StartingMtx);
                        end
                        h = toc; disp(['Classification (KM): ',num2str(h)])
                        
                        if SubMasking
                            ResNum_Part = ResNum;
                            ResNum = zeros(length(DataTable(:,1)),1);
                            ResNum(SelSubMasking.SelectedData,:) = ResNum_Part;
                        end
                end
                
                
                app.WaitBar.Message = 'XMapTools is running numbers (Step 3 - Testing & Generating report)';
                
                
                Predicted_TestSet = ResNum(DataTest_Idx);
                
                nexttile;
                [NormValues,order] = confusionmat(DataTest_Class,Predicted_TestSet);
                
                TrainSetListConf = matlab.lang.makeUniqueStrings(TrainSetList);
                
                if SubMasking
                    confusionchart(NormValues(2:end,2:end),TrainSetListConf, 'Title','Confusion Chart (Test dataset)');
                else
                    confusionchart(NormValues,TrainSetListConf, 'Title','Confusion Chart (Test dataset)');
                end
                
                Accuracy = length(find(DataTest_Class == Predicted_TestSet))/length(DataTest_Class);
                
                for i = 1:max(DataTest_Class)
                    TP(i) = length(find(DataTest_Class == i & Predicted_TestSet == i));
                    FP(i) = length(find(DataTest_Class == i & Predicted_TestSet ~= i));
                    FN(i) = length(find(DataTest_Class ~= i & Predicted_TestSet == i));
                end
                
                Precision = TP ./ (TP + FP);
                Recall = TP ./ (TP + FN);
                
                Recall(find(isnan(Recall))) = 0;
                
                F1 = 2 ./ (1./Precision + 1./Recall);
                
                F1(find(isnan(F1))) = 0;
                F1(find(isinf(F1))) = 0;
                
                ElemListMapFormatted = '';
                for i = 1:length(ElemListMap)
                    String = char(ElemListMap{i});
                    Where = find(ismember(String,'%'));
                    if ~isempty(Where)
                        FormatedString = [String(1:Where),'%',String(Where+1:end)];
                    else
                        FormatedString = String;
                    end
                    ElemListMapFormatted{i} = FormatedString;
                end
                
                % Save data in Info
                Info.Algorithm = ClassificationMethod;
                Info.SelectedData = ElemListMapFormatted;
                Info.Scaling = app.Classify_ScalingMode.Value;
                if num2str(app.Classify_Reproducibility.Value)
                    Info.Reproducibility = ['Yes; seed = ',num2str(app.Classify_ReproducibilityValue.Value)];
                else
                    Info.Reproducibility = 'No';
                end
                Info.Classes = TrainSetList;
                Info.SelectedPx = NbPx';
                Info.TrainPx = NbPx_Train';
                Info.TestPx = NbPx_Test';
                Info.Accuracy = Accuracy;
                Info.Precision = Precision;
                Info.Recall = Recall;
                Info.F1Score = F1;
                
                NbNonZero = length(find(ResNum)>0);
                for i = 1:length(Info.Classes)
                    Info.Modes(i) = length(find(ResNum == i)) / NbNonZero;
                end
                
            else 
                % ---------------------------------------------------------
                % This is for unsupervised classification
                
                DataTable = Data;
                
                % detect sub-classification (the way to detect is specific
                % for the unsupervised classification and different than
                % for the other classifications based on a training set.
                
                SubMasking = 0;
                
                if ~isempty(app.TreeData_Additional.SelectedNodes)
                    NodeDataSM = app.TreeData_Additional.SelectedNodes.NodeData;
                    
                    if isequal(NodeDataSM(1),11) && NodeDataSM(2) > 0 && NodeDataSM(3) > 0
                        
                        MaskInd = NodeDataSM(2);
                        MaskNode = NodeDataSM(3);
                        
                        SelectedMaskMap = app.XMapToolsData.MapData.MaskFile.Masks(MaskInd).MaskMap;
                        MaskMap = nan(size(SelectedMaskMap));
                        MaskMapZ = zeros(size(SelectedMaskMap));
                        PxSelected = find(SelectedMaskMap == MaskNode-1);
                        MaskMap(PxSelected) = ones(size(PxSelected));
                        MaskMapZ(PxSelected) = ones(size(PxSelected));
                        
                        DataSM = nan(size(Data));
                        DataSM(PxSelected,:) = Data(PxSelected,:);
                        Data = DataSM;
                        
                        %DataSM = Data .* repmat(MaskMap(:),1,size(Data,2));
                        
                        SumData = sum(Data,2);
                        SelSubMasking.UnselectedData = find(isnan(SumData));
                        SelSubMasking.SelectedData = find(~isnan(SumData));
                        SelSubMasking.MaskMapZ = MaskMapZ;
                        SelSubMasking.MaskMapZUnselected = zeros(size(MaskMapZ));
                        IndxZeros = find(MaskMapZ == 0);
                        SelSubMasking.MaskMapZUnselected(IndxZeros) = 1;
                        
                        % Eliminate NaNs (fixed on 21.11.23)
                        for i = 1:size(Data, 2)
                            Data(SelSubMasking.UnselectedData,i) = 0;
                        end
                        
                        for i = 1:length(Data2D)
                            Data2D(i).Map = Data2D(i).Map .* MaskMap;
                        end
                        
                        %                     figure
                        %                     tiledlayout("flow")
                        %                     for i = 1:length(Data2D)
                        %                         nexttile
                        %                         imagesc(Data2D(i).Map), axis image, colorbar
                        %                     end
                        
                        SubMasking = 1;
                    end
                end
                
                NbClMin = 2;
                NbClMax = app.Classify_OptionValue.Value;
                
                ClVector = NbClMin:NbClMax;
                
                for i = 1:size(DataTable,2)
                    % Added in 3.4 for submasking classification
                    if SubMasking
                        IdxSelected = SelSubMasking.SelectedData;
                    else
                        IdxSelected = 1:length(DataTable(:,1));
                    end
                    
                    DataTable(IdxSelected,i) = (DataTable(IdxSelected,i) - median(DataTable(IdxSelected,i),'omitnan')) / iqr(DataTable(IdxSelected,i));  % Robust scaller
                end
                
                ResNumAll = zeros(size(DataTable,1),length(ClVector));
                SumDAll = zeros(1,length(ClVector));
                
                close(app.WaitBar)
                app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools');
                app.WaitBar.Message = 'XMapTools is running numbers (Step 2 - Unsupervised classification)';
                
                disp('  ')
                disp(['** Method: Unsupervised classification using k-means (',app.Options_KmeansAlgorithm.Value,')'])
                
                for i = 1:length(ClVector)
                    if SubMasking
                        disp('  ')
                        disp(['- Nb classes: ',num2str(i)])
                        [ResNum,C,SumD] = kmeans(DataTable(SelSubMasking.SelectedData,:),ClVector(i),'MaxIter',500,'Display','final','Replicates',5,'Distance',app.Options_KmeansAlgorithm.Value);
                        ResNumAll(SelSubMasking.SelectedData,i) = ResNum;
                        SumDAll(i) = sum(SumD);
                    else
                        [ResNum,C,SumD] = kmeans(DataTable,ClVector(i),'MaxIter',500,'Display','final','Replicates',5,'Distance',app.Options_KmeansAlgorithm.Value);
                        ResNumAll(:,i) = ResNum;
                        SumDAll(i) = sum(SumD);
                    end
                    
                    app.WaitBar.Value = i/length(ClVector);
                end
                
                close(app.WaitBar)
                app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
                app.WaitBar.Message = 'Done';
                %
                
                app.ExchangeClassification.ResNum = [];
                
                waitfor(Classification(app,ResNumAll,Resolution,SumDAll));
                
                if isempty(app.ExchangeClassification.ResNum)
                    close(app.WaitBar)
                    return
                end
                
                ResNum = app.ExchangeClassification.ResNum;
                
                if SubMasking
                    for i = 1:max(ResNum(:))
                        TrainSetList{i} = [char(app.XMapToolsData.MapData.MaskFile(MaskInd).Masks.Names{MaskNode}),'_',num2str(i)];
                    end
                else
                    for i = 1:max(ResNum(:))
                        TrainSetList{i} = ['Class_',num2str(i)];
                        NewMaskFileName = 'Unsupervised';
                        NewMaskFileType = 1;    % no idea if this is good or important.
                    end
                end
                
                %                 figure, tiledlayout('flow'),
                %                 for i = 1:length(ClVector)
                %                     t = nexttile;
                %                     TheMap = reshape(ResNumAll(:,i),Resolution(1,:));
                %                     imagesc(t,TheMap)
                %                     axis(t,'image')
                %                     colorbar(t)
                %                     colormap(t, parula(ClVector(i)))
                %                 end
                %
                
                app.WaitBar.Message = 'XMapTools is running numbers (Step 3 - Testing & Generating report)';
                
                ElemListMap = ElemListMap_Ori;
                
                ElemListMapFormatted = '';
                for i = 1:length(ElemListMap)
                    String = char(ElemListMap{i});
                    Where = find(ismember(String,'%'));
                    if ~isempty(Where)
                        FormatedString = [String(1:Where),'%',String(Where+1:end)];
                    else
                        FormatedString = String;
                    end
                    ElemListMapFormatted{i} = FormatedString;
                end
                
                % Save data in Info
                Info.Algorithm = 'Unsupervised k-means';
                Info.SelectedData = ElemListMapFormatted;
                Info.Scaling = app.Classify_ScalingMode.Value;
                if num2str(app.Classify_Reproducibility.Value)
                    Info.Reproducibility = ['Yes; seed = ',num2str(app.Classify_ReproducibilityValue.Value)];
                else
                    Info.Reproducibility = 'No';
                end
                Info.Classes = TrainSetList;
                Info.SelectedPx = 0;
                Info.TrainPx = 0;
                Info.TestPx = 0;
                Info.Accuracy = 1;
                Info.Precision = zeros(length(TrainSetList),1);
                Info.Recall = zeros(length(TrainSetList),1);
                Info.F1Score = zeros(length(TrainSetList),1);
                
                NbNonZero = length(find(ResNum)>0);
                for i = 1:length(Info.Classes)
                    Info.Modes(i) = length(find(ResNum == i)) / NbNonZero;
                end
                
            end
            
            app.WaitBar.Message = 'XMapTools is running numbers (Step 4 - Finishing)';
            
            if SubMasking
                MaskFile = app.XMapToolsData.MapData.MaskFile;
                
                SelMask = MaskNode; % Here we save the SubMask at the node position
                
                MaskFile.Masks(MaskInd).SubMask(SelMask).Names = ['None',TrainSetList];
                MaskFile.Masks(MaskInd).SubMask(SelMask).Densities = zeros(1,length(TrainSetList)+1);
                MaskFile.Masks(MaskInd).SubMask(SelMask).MaskSelMaskMap = reshape(ResNum,Resolution(1,:));
                
                % Mask info (added on 21.11.2023)
                MaskFile.Masks(MaskInd).SubMask(SelMask).Info = Info;
                
                if exist('PosteriorM','var')
                    MaskFile.Masks(MaskInd).SubMask(SelMask).MaskProbability = PosteriorM;
                else
                    MaskFile.Masks(MaskInd).SubMask(SelMask).MaskProbability = 0;
                end
                
                app.XMapToolsData.MapData.MaskFile = MaskFile;
                
                % Clear pre-existing tree
                for i = 1:length(app.Node_Masks.Children(MaskInd).Children(SelMask).Children)
                    app.Node_Masks.Children(MaskInd).Children(SelMask).Children(1).delete;
                end
                % Update the tree
                for i = 1:length(MaskFile.Masks(MaskInd).SubMask(SelMask).Names)
                    p = uitreenode(app.Node_Masks.Children(MaskInd).Children(SelMask),'Text',char(MaskFile.Masks(MaskInd).SubMask(SelMask).Names{i}),'NodeData',[11,MaskInd,SelMask,i]);
                end
                %expand(app.Node_Masks);
                app.TreeData_Additional.SelectedNodes = app.Node_Masks.Children(MaskInd).Children(SelMask).Children(1);
                expand(app.Node_Masks.Children(MaskInd).Children(SelMask));
                
            else
                % Save a new mask file
                MaskFile = app.XMapToolsData.MapData.MaskFile;
                IdxMF = length(app.XMapToolsData.MapData.MaskFile.Names)+1;
                
                if isequal(app.Classify_MapsCheckBox.Value,1)
                    NewMaskFileName = [NewMaskFileName,'_Maps'];
                end
                
                Abbrev_PCA = '';
                if isequal(app.Classify_PCA1CheckBox.Value,1) && isequal(app.Classify_PCA2CheckBox.Value,0)
                    Abbrev_PCA = '_PCA-M';
                elseif isequal(app.Classify_PCA1CheckBox.Value,0) && isequal(app.Classify_PCA2CheckBox.Value,1)
                    Abbrev_PCA = '_PCA-N';
                elseif isequal(app.Classify_PCA1CheckBox.Value,1) && isequal(app.Classify_PCA2CheckBox.Value,1)
                    Abbrev_PCA = 'PCA-M-N';
                end
                
                NewMaskFileName = [NewMaskFileName,Abbrev_PCA];
                
                if isequal(app.Classify_TEXFCheckBox.Value,1)
                    NewMaskFileName = [NewMaskFileName,'_TEXF'];
                end
                
                MaskFile.Names{IdxMF} = NewMaskFileName;
                
                MaskFile.Types(IdxMF) = NewMaskFileType;
                MaskFile.NbMasks(IdxMF) = length(TrainSetList)+1;
                
                MaskFile.Masks(IdxMF).Names = ['None',TrainSetList];
                MaskFile.Masks(IdxMF).Densities = zeros(1,length(TrainSetList)+1);
                MaskFile.Masks(IdxMF).MaskMap = reshape(ResNum,Resolution(1,:));
                
                if exist('PosteriorM','var')
                    MaskFile.Masks(IdxMF).MaskProbability = PosteriorM;
                else
                    MaskFile.Masks(IdxMF).MaskProbability = 0;
                end
                
                % Mask signature (added on 10.07.2022)
                MaskFile.Masks(IdxMF).Signature = now;
                
                % Mask colors (added on 24.02.2023)
                Colors4Mask = UpdateColorsMasks(app,MaskFile.Masks(IdxMF).Names);
                MaskFile.Masks(IdxMF).Colors = Colors4Mask;
                
                % Mask info (added on 21.11.2023)
                MaskFile.Masks(IdxMF).Info = Info;
                
                % Add submask empty variables (10.07.2022)
                for i = 1:length(TrainSetList)+1
                    MaskFile.Masks(IdxMF).SubMask(i).Names = {};
                    MaskFile.Masks(IdxMF).SubMask(i).Densities = [];
                    MaskFile.Masks(IdxMF).SubMask(i).MaskSelMaskMap = [];
                    MaskFile.Masks(IdxMF).SubMask(i).MaskProbability = [];
                end
                app.XMapToolsData.MapData.MaskFile = MaskFile;
                
                % Update the tree
                p = uitreenode(app.Node_Masks,'Text',char(app.XMapToolsData.MapData.MaskFile.Names{IdxMF}),'NodeData',[11,IdxMF,0,0]);
                for i = 1:app.XMapToolsData.MapData.MaskFile.NbMasks(IdxMF)
                    p1 = uitreenode(p,'Text',char(app.XMapToolsData.MapData.MaskFile.Masks(IdxMF).Names{i}),'NodeData',[11,IdxMF,i,0]);
                end
                %expand(app.Node_Masks);
                app.TreeData_Additional.SelectedNodes = p;
                expand(p);
                
            end
            
            if isequal(ClassificationMethod,'Classification Tree')
                view(tree,'Mode','graph');
            end
            
            %PlotMaskFile(app,IdxMF);
            TreeData_AdditionalSelectionChanged(app);
            % drawnow
            
            app.SaveRequired = 1;
            
            close(app.WaitBar);
            
            pause(0.1)
            figure(app.XMapTools_GUI)
            %keyboard
            
        end

        % Callback function
        function Classify_ButtonDeletePushed(app, event)
            ContextMenu_AdditionalTree_DeletePushed(app);
            TreeData_AdditionalSelectionChanged(app, event);
        end

        % Value changed function: Classify_FilterLowProbPixels
        function Classify_FilterLowProbPixelsValueChanged(app, event)
            TreeData_AdditionalSelectionChanged(app);
        end

        % Value changed function: Classify_FilterProbValue
        function Classify_FilterProbValueValueChanged(app, event)
            TreeData_AdditionalSelectionChanged(app);
        end

        % Button pushed function: Classify_RunPCAButton
        function Classify_RunPCAButtonPushed(app, event)
            
            NodeData = app.TreeData_Main.SelectedNodes.NodeData;
            
            switch NodeData(1)
                case 1
                    ElemListMap = app.XMapToolsData.MapData.It.Names;
                    for i = 1:length(ElemListMap)
                        Data(:,i) = app.XMapToolsData.MapData.It.Data(i).Map(:);
                        DataSize(i,:) = size(app.XMapToolsData.MapData.It.Data(i).Map);
                    end
                    
                case 2
                    ElemListMap = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElNames;
                    for i = 1:length(ElemListMap)
                        Data(:,i) = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).CData(i).Map(:);
                        DataSize(i,:) = size(app.XMapToolsData.MapData.It.Data(i).Map);
                    end
                case 3
                    ElemListMap = app.XMapToolsData.MapData.Me.Data(NodeData(2)).ElNames;
                    for i = 1:length(ElemListMap)
                        Data(:,i) = app.XMapToolsData.MapData.Me.Data(NodeData(2)).CData(i).Map(:);
                        DataSize(i,:) = size(app.XMapToolsData.MapData.It.Data(i).Map);
                    end
            end
            
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'XMapTools is running numbers, please wait';
            
            [coeff,score,latent,tsquared,explained1,mu] = pca(Data');
            
            Data2 = Data./repmat(mean(Data,1),size(Data,1),1); %repmat(mean(Data,2),1,size(Data,2));
            
            [coeff2,score,latent,tsquared,explained2,mu] = pca(Data2');
            
            Data3 = (Data - repmat(mean(Data,1),size(Data,1),1))./repmat(std(Data),size(Data,1),1);
            
            [coeff3,score,latent,tsquared,explained3,mu] = pca(Data3');
            
            %Data4 = (Data - repmat(median(Data,1),size(Data,1),1))./repmat(iqr(Data),size(Data,1),1);
            
            %[coeff4,score,latent,tsquared,explained4,mu] = pca(Data4');
            
            app.WaitBar.Message = 'finishing';
            
            CumSum1 = cumsum(explained1);
            CumSum2 = cumsum(explained2);
            CumSum3 = cumsum(explained3);
            
            Where1 = find(CumSum1 > 98);
            Where2 = find(CumSum2 > 98);
            Where3 = find(CumSum3 > 98);
            
            Cutoff1 = Where1(1)-1;
            Cutoff2 = Where2(1)-1;
            Cutoff3 = Where3(1)-1;
            
            %                         figure
            %                         subplot(3,1,1)
            %                         bar(explained1)
            %                         subplot(3,1,2)
            %                         bar(explained2)
            %                         subplot(3,1,3)
            %                         bar(explained3)
            
            %             for i = 1:size(Data,2)
            %                 figure
            %                 subplot(3,1,1)
            %                 histogram(Data(:,i))
            %                 title([ElemListMap{i},' '],'Interpreter','none')
            %                 subplot(3,1,2)
            %                 histogram(Data2(:,i))
            %                 title([ElemListMap{i},'/mean'],'Interpreter','none')
            %                 subplot(3,1,3)
            %                 histogram(Data3(:,i))
            %                 title([ElemListMap{i},'- mean / std '],'Interpreter','none')
            %             end
            
            for i = 1:size(coeff,2)
                Map = reshape(coeff(:,i),DataSize(1,:));
                
                % Save the results in Others
                Pos = length(app.XMapToolsData.MapData.Ot.Names)+1;
                
                app.XMapToolsData.MapData.Ot.Names{Pos} = ['PCA-1-',num2str(i)];
                app.XMapToolsData.MapData.Ot.Types(Pos) = 5;
                app.XMapToolsData.MapData.Ot.Data(Pos).Map = Map;
                
                % Add MapData Ot
                p = uitreenode(app.Node_Ot,'Text',char(app.XMapToolsData.MapData.Ot.Names{Pos}),'NodeData',[5,Pos]);
                
            end
            for i = 1:size(coeff2,2)
                Map = reshape(coeff2(:,i),DataSize(1,:));
                
                % Save the results in Others
                Pos = length(app.XMapToolsData.MapData.Ot.Names)+1;
                
                app.XMapToolsData.MapData.Ot.Names{Pos} = ['PCA-2-',num2str(i)];
                app.XMapToolsData.MapData.Ot.Types(Pos) = 5;
                app.XMapToolsData.MapData.Ot.Data(Pos).Map = Map;
                
                % Add MapData Ot
                p = uitreenode(app.Node_Ot,'Text',char(app.XMapToolsData.MapData.Ot.Names{Pos}),'NodeData',[5,Pos]);
            end
            for i = 1:size(coeff3,2)
                Map = reshape(coeff3(:,i),DataSize(1,:));
                
                % Save the results in Others
                Pos = length(app.XMapToolsData.MapData.Ot.Names)+1;
                
                app.XMapToolsData.MapData.Ot.Names{Pos} = ['PCA-3-',num2str(i)];
                app.XMapToolsData.MapData.Ot.Types(Pos) = 5;
                app.XMapToolsData.MapData.Ot.Data(Pos).Map = Map;
                
                % Add MapData Ot
                p = uitreenode(app.Node_Ot,'Text',char(app.XMapToolsData.MapData.Ot.Names{Pos}),'NodeData',[5,Pos]);
            end
            
            expand(app.Node_Ot);
            app.TreeData_Main.SelectedNodes = p;
            
            app.SaveRequired = 1;
            
            close(app.WaitBar);
            
        end

        % Button pushed function: Classify_FilterMaskFile
        function Classify_FilterMaskFileButtonPushed(app, event)
            
            Idx = length(app.XMapToolsData.MapData.MaskFile.Names)+1;
            
            FilterValue = app.Classify_FilterProbValue.Value;
            
            NodeData = app.TreeData_Additional.SelectedNodes.NodeData;
            Idx_Sel = NodeData(2);
            
            ValsInRange = find(app.XMapToolsData.MapData.MaskFile.Masks(Idx_Sel).MaskProbability(:) > 0.001 & app.XMapToolsData.MapData.MaskFile.Masks(Idx_Sel).MaskProbability(:) < FilterValue);
            
            if ~isempty(ValsInRange)
                app.XMapToolsData.MapData.MaskFile.Names{Idx} = [char(app.XMapToolsData.MapData.MaskFile.Names{Idx_Sel}),'_filter'];
                app.XMapToolsData.MapData.MaskFile.Types(Idx) = 100+app.XMapToolsData.MapData.MaskFile.Types(Idx_Sel);
                app.XMapToolsData.MapData.MaskFile.NbMasks(Idx) = app.XMapToolsData.MapData.MaskFile.NbMasks(Idx_Sel) + 1;
                
                MaskIdx = app.XMapToolsData.MapData.MaskFile.NbMasks(Idx) - 1; % because of "none"
                
                app.XMapToolsData.MapData.MaskFile.Masks(Idx) = app.XMapToolsData.MapData.MaskFile.Masks(Idx_Sel);
                
                app.XMapToolsData.MapData.MaskFile.Masks(Idx).Names{end+1} = 'Filtered';
                app.XMapToolsData.MapData.MaskFile.Masks(Idx).Densities(end+1) = 0;
                
                for i = 1:size(app.XMapToolsData.MapData.MaskFile.Masks(Idx).MaskProbability,3)
                    FindVals = find(app.XMapToolsData.MapData.MaskFile.Masks(Idx).MaskProbability(:,:,i) > 0.001 & app.XMapToolsData.MapData.MaskFile.Masks(Idx).MaskProbability(:,:,i) < FilterValue);
                    app.XMapToolsData.MapData.MaskFile.Masks(Idx).MaskMap(FindVals) = MaskIdx.*ones(size(FindVals));
                end
                app.XMapToolsData.MapData.MaskFile.Masks(Idx).MaskProbability(:,:,end+1) = zeros(size(app.XMapToolsData.MapData.MaskFile.Masks(Idx).MaskProbability(:,:,1)));
                
                % Update the tree
                p = uitreenode(app.Node_Masks,'Text',char(app.XMapToolsData.MapData.MaskFile.Names{Idx}),'NodeData',[11,Idx,0]);
                for i = 1:app.XMapToolsData.MapData.MaskFile.NbMasks(Idx)
                    p1 = uitreenode(p,'Text',char(app.XMapToolsData.MapData.MaskFile.Masks(Idx).Names{i}),'NodeData',[11,Idx,i]);
                end
                %expand(app.Node_Masks);
                app.TreeData_Additional.SelectedNodes = p;
                expand(p);
                
                %PlotMaskFile(app,IdxMF);
                TreeData_AdditionalSelectionChanged(app);
                % drawnow
                %close(app.WaitBar);
                
                app.SaveRequired = 1;
                
                pause(0.1)
                figure(app.XMapTools_GUI)
                %keyboard
                
            else
                warndlg('No posterior probability available for this mask file (check for the list of compatible classification routines)','XMapTools');
            end
            
            
            
            
        end

        % Button pushed function: Classify_PointCountingModes
        function Classify_PointCountingModesButtonPushed(app, event)
            MC = app.Classify_MC_PointCountingCheckBox.Value;
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = ['XMapTools is running numbers (point-counting n=',num2str(app.Classify_NbPointCounting.Value),'; MC=',num2str(MC),')'];
            
            NbPx = app.Classify_NbPointCounting.Value;
            NbSim = 1000;
            
            ROI_DeleteROI(app);
            
            NodeData = app.TreeData_Additional.SelectedNodes.NodeData;
            Idx_Sel = NodeData(2);
            
            MaskMap = app.XMapToolsData.MapData.MaskFile.Masks(Idx_Sel).MaskMap;
            Names = app.XMapToolsData.MapData.MaskFile.Masks(Idx_Sel).Names;
            
            [NbRows,NbCol] = size(MaskMap);
            
            if MC
                %app.WaitBar.Message = 'XMapTools is running numbers (point-counting + Monte-Carlo)';
                
                ProPer_All = zeros(length(Names)-1,NbSim);
                
                for iSim = 1:NbSim
                    
                    Coord = zeros(NbPx,2);
                    Min = zeros(NbPx,1);
                    
                    for i = 1:NbPx
                        Coord(i,1) = randi(NbRows,1,1);
                        Coord(i,2) = randi(NbCol,1,1);
                        
                        Where = find(Coord(:,1) == Coord(i,1) & Coord(:,2) == Coord(i,2));
                        
                        Compt = 0;
                        while length(Where) > 1
                            disp('Additional random generation...')
                            Coord(i,1) = randi(NbRows,1,1);
                            Coord(i,2) = randi(NbCol,1,1);
                            
                            Where = find(Coord(:,1) == Coord(i,1) & Coord(:,2) == Coord(i,2));
                            Compt=Compt+1;
                            if Compt > 50
                                disp('Something went wrong with the generation of unique pairs (ask Pierre Lanari)')
                                break
                            end
                        end
                        
                        Min(i) = MaskMap(Coord(i,1),Coord(i,2));
                    end
                    
                    % Calculate Modes
                    Counts = zeros(length(Names)-1,1);
                    ProPer = zeros(length(Names)-1,1);
                    for i = 2:length(Names)
                        Counts(i-1) = length(find(Min == i-1));
                        ProPer(i-1) = Counts(i-1)/NbPx*100;
                    end
                    
                    ProPer_All(:,iSim) = ProPer;
                end
                
                MeanProPer = mean(ProPer_All,2);
                StdProPer = std(ProPer_All,[],2);
                
                DataTable = {};
                for i = 1:length(Counts)
                    DataTable(i,:) = {Names{i+1},MeanProPer(i),StdProPer(i)};
                end
                
                app.CompViewer_Label.Text = 'Modes';
                
                app.CompViewer_UITable.Data = DataTable;
                app.CompViewer_UITable.ColumnName = {'Mask','Mode (Point-counting)','Unc (1-sigma)'};
                app.CompViewer_DensityMenu.Visible = 'off';
                
                pie(app.CompViewer_PlotTable,cell2mat(DataTable(:,2)),DataTable(:,1));
                
                app.CompViewer_PlotTable.Colormap = app.ColorMapValues;
                
                app.CompViewer_Button_Copy.Enable = 'on';
                app.CompViewer_Button_Save.Enable = 'on';
                app.CompViewer_Button_DeleteROI.Enable = 'on';
                
                app.TabGroup.SelectedTab = app.CompositionTab;
                
            else
                
                Coord = zeros(NbPx,2);
                Min = zeros(NbPx,1);
                
                for i = 1:NbPx
                    Coord(i,1) = randi(NbRows,1,1);
                    Coord(i,2) = randi(NbCol,1,1);
                    
                    Where = find(Coord(:,1) == Coord(i,1) & Coord(:,2) == Coord(i,2));
                    
                    Compt = 0;
                    while length(Where) > 1
                        disp('Additional random generation...')
                        Coord(i,1) = randi(NbRows,1,1);
                        Coord(i,2) = randi(NbCol,1,1);
                        
                        Where = find(Coord(:,1) == Coord(i,1) & Coord(:,2) == Coord(i,2));
                        Compt=Compt+1;
                        if Compt > 50
                            disp('Something went wrong with the generation of unique pairs (ask Pierre Lanari)')
                            break
                        end
                    end
                    
                    Min(i) = MaskMap(Coord(i,1),Coord(i,2));
                end
                
                % Calculate Modes
                Counts = zeros(length(Names)-1,1);
                ProPer = zeros(length(Names)-1,1);
                for i = 2:length(Names)
                    Counts(i-1) = length(find(Min == i-1));
                    ProPer(i-1) = Counts(i-1)/NbPx*100;
                end
                
                DataTable = {};
                for i = 1:length(Counts)
                    DataTable(i,:) = {Names{i+1},ProPer(i),Counts(i)};
                end
                
                app.CompViewer_Label.Text = 'Modes';
                
                app.CompViewer_UITable.Data = DataTable;
                app.CompViewer_UITable.ColumnName = {'Mask','Mode (Point-counting)','Nb Pixels'};
                app.CompViewer_DensityMenu.Visible = 'off';
                
                pie(app.CompViewer_PlotTable,cell2mat(DataTable(:,2)),DataTable(:,1));
                
                app.CompViewer_PlotTable.Colormap = app.ColorMapValues;
                
                app.CompViewer_Button_Copy.Enable = 'on';
                app.CompViewer_Button_Save.Enable = 'on';
                app.CompViewer_Button_DeleteROI.Enable = 'on';
                
                app.TabGroup.SelectedTab = app.CompositionTab;
                
            end
            
            close(app.WaitBar);
            
            
        end

        % Button pushed function: Classify_Modes_AddROI
        function Classify_Modes_AddROIButtonPushed(app, event)
            ROI_DeleteROI(app);
            
            DeactivatePlotZoomPanOptions(app);
            
            switch app.Classify_Modes_ROI_menu.Value
                case 'Rectangle ROI'
                    DrawingMode(app,'on','Rectangle');
                    app.ROI_Modes = drawrectangle(app.FigMain,'Color',[0.57,0.00,0.69],'InteractionsAllowed','all');
                    DrawingMode(app,'off');
                    
                case 'Polygon ROI'
                    DrawingMode(app,'on','Polygon');
                    app.ROI_Modes = drawpolygon(app.FigMain,'Color',[0.57,0.00,0.69],'InteractionsAllowed','all');
                    DrawingMode(app,'off');
            end
            
            app.ROI_Modes_Listener = addlistener(app.ROI_Modes, 'ROIMoved', @(varargin)Modes_ROI_changed_shape(app, app.ROI_Modes));
            
            % Extract data
            Modes_ROI_changed_shape(app,app.ROI_Modes);
            
            
        end

        % Value changed function: Classify_ReproducibilityValue
        function Classify_ReproducibilityValueValueChanged(app, event)
            app.SaveRequired = 1;
        end

        % Value changed function: Classify_OptionValue
        function Classify_OptionValueValueChanged(app, event)
            app.SaveRequired = 1;
        end

        % Value changed function: Classification_Menu
        function Classify_MenuValueChanged(app, event)
            value = app.Classification_Menu.Value;
            
            switch value
                case 'Random Forest'
                    app.Classify_OptionLabel.Text = 'Nb trees';
                    app.Classify_OptionValue.Value = 75;
                    
                    app.Classify_OptionLabel.Visible = 'on';
                    app.Classify_OptionValue.Visible = 'on';
                    
                    % If possible we reactivate the classify button
                    app.Classify_Button.Enable = 'off';
                    if ~isempty(app.Classify_ElemList.Value)
                        if ~isempty(app.TreeData_Additional.SelectedNodes)
                            NodeData = app.TreeData_Additional.SelectedNodes(1).NodeData;
                            if isequal(NodeData(1),12) && NodeData(2) > 0 && isequal(NodeData(3),0)
                                app.Classify_Button.Enable = 'on';
                                app.Classify_Button.Tooltip = 'Train a Classifier & Classify';
                            end
                        end
                    end
                    
                case 'Unsupervised k-means'
                    app.Classify_OptionLabel.Text = 'Max classes';
                    app.Classify_OptionValue.Value = 10;
                    
                    app.Classify_OptionLabel.Visible = 'on';
                    app.Classify_OptionValue.Visible = 'on';
                    
                    % -------------------------------------------------------------
                    % Update the GUI (SPECIAL: unsupervised k-mean classification)
                    % -------------------------------------------------------------
                    if isequal (app.Classification_Menu.Value,'Unsupervised k-means')
                        if ~isempty(app.Classify_ElemList.Value)
                            app.Classify_Button.Enable = 'on';
                            app.Classify_Button.Tooltip = 'Unsupervised classification';
                        end
                    end
                    
                otherwise
                    app.Classify_OptionLabel.Visible = 'off';
                    app.Classify_OptionValue.Visible = 'off';
                    
                    % If possible we reactivate the classify button
                    app.Classify_Button.Enable = 'off';
                    if ~isempty(app.Classify_ElemList.Value)
                        if ~isempty(app.TreeData_Additional.SelectedNodes)
                            NodeData = app.TreeData_Additional.SelectedNodes(1).NodeData;
                            if isequal(NodeData(1),12) && NodeData(2) > 0 && isequal(NodeData(3),0)
                                app.Classify_Button.Enable = 'on';
                                app.Classify_Button.Tooltip = 'Train a Classifier & Classify';
                            end
                        end
                    end
            end
            
        end

        % Button pushed function: Masks_ButtonPlotPie
        function Masks_ButtonPlotPieButtonPushed(app, event)
            %
            NodeData = app.TreeData_Additional.SelectedNodes.NodeData;
            
            if ~isequal(NodeData(1),11) || ~(NodeData(2) > 0) || ~isequal(NodeData(3),0)
                return
            end
            
            MaskFile = app.XMapToolsData.MapData.MaskFile;
            MaskMap = MaskFile.Masks(NodeData(2)).MaskMap;
            
            % Chemical data
            NodeDataMain = app.TreeData_Main.SelectedNodes.NodeData;
            
            MaskNames = MaskFile.Masks(NodeData(2)).Names(2:end);
            
            switch NodeDataMain(1)
                case 1
                    ElNames = app.XMapToolsData.MapData.It.Names;
                    Data = app.XMapToolsData.MapData.It.Data;
                case 3
                    ElNames = app.XMapToolsData.MapData.Me.Data(NodeDataMain(2)).ElNames;
                    Data = app.XMapToolsData.MapData.Me.Data(NodeDataMain(2)).CData;
                otherwise
                    return
            end
            
            NbElem = length(ElNames)-1;
            NbMasks = length(MaskNames);
            
            SumMatrix = zeros(NbMasks,NbElem);
            for i = 1:NbMasks
                PxIdx = find(MaskMap == i);
                for j = 1:NbElem
                    ElMAp = Data(j).Map;
                    IdxInf = find(isinf(ElMAp));
                    ElMAp(IdxInf) = 0;
                    SumMatrix(i,j) = sum(ElMAp(PxIdx));
                end
            end
            
            SumMatrixFrac = SumMatrix./repmat(sum(SumMatrix,1),size(SumMatrix,1),1);
            
            if NbMasks <= 8
                Grid = [3,3];
            elseif NbMasks <= 11
                Grid = [3,4];
            else
                Grid = [4,5];
            end
            
            h = figure; hold on
            Colors = MaskFile.Masks(NodeData(2)).Colors(2:end,:);
            
            for i = 1:NbElem
                Prec=0;
                for j = 1:NbMasks
                    % Left_X left_Y width height
                    if NbElem > 10
                        rectangle('Position',[i-0.5,Prec,1,SumMatrixFrac(j,i)],'FaceColor',Colors(j,:),'LineWidth',1);
                    else
                        rectangle('Position',[i-0.3,Prec,0.6,SumMatrixFrac(j,i)],'FaceColor',Colors(j,:),'LineWidth',1);
                    end
                    Prec = Prec+SumMatrixFrac(j,i);
                end
                text(i-0.1,1.05,char(ElNames{i}))
            end
            
            i = i+2;
            dY = 0.8/NbMasks;
            ddY = 0.2/(NbMasks+1);
            Prec = ddY;
            for j = 1:NbMasks
                rectangle('Position',[i-0.2,Prec,0.4,dY],'FaceColor',Colors(j,:));
                text(i+0.5,Prec+dY/2,char(MaskNames{j}))
                Prec = Prec+dY+ddY;
            end
            
            ax = gca;
            axis(ax,[0 i+3 -0.2 1.2]);
            
            ax.XTick = [];
            ax.YTick = [];
            ax.Box = 'on';
            
            pause(0.1)
            figure(app.XMapTools_GUI);
            
        end

        % Button pushed function: Command_Keyboard
        function Developper_Command_KeyboardButtonPushed(app, event)
            app.XMapToolsData.MapData
            app.XMapToolsData.TrainingSet
            app.XMapToolsData.SegScheme
            
            keyboard
        end

        % Callback function: AdminAccess, UnlockButton
        function Developper_UnlockButtonPushed(app, event)
            
            if ~admin_access(app.Password)
                app.Password  = app.AdminAccess.Value;
            end
            
            AccessGranted = admin_access(app.Password);
            
            if AccessGranted
                app.DEVELOPPERTOOLSLabel.Visible = 'on';
                app.RebuildButton.Visible = 'on';
                app.Command_Keyboard.Visible = 'on';
                app.AdminAccess.Value = '';
                
                app.UnlockButton.Visible = 'off';
                app.AdminAccess.Visible = 'off';
            else
                app.DEVELOPPERTOOLSLabel.Visible = 'off';
                app.RebuildButton.Visible = 'off';
                app.Command_Keyboard.Visible = 'off';
            end
            
            
        end

        % Button pushed function: RebuildButton
        function RebuildButtonPushed(app, event)
            RebuildGUI(app,'It');
            RebuildGUI(app,'MaskFiles');
            RebuildGUI(app,'TrainingSet');
        end

        % Menu selected function: AboutMenu
        function Menu_AboutMenuSelected(app, event)
            About_XMapTools(app);
        end

        % Menu selected function: StartNewSessionMenu
        function Menu_StartNewSessionMenuSelected(app, event)
            
            XMapTools_GUICloseRequest(app, event);
            
            XMapTools;
            
        end

        % Menu selected function: SetWorkingDirectoryMenu
        function Menu_SetWorkingDirectorySelected(app, event)
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Select a working directory';
            
            f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
            Directory = uigetdir(cd, 'Select a working directory');
            close(f);
            figure(app.XMapTools_GUI);
            
            if ~isequal(Directory,0)
                cd(Directory)
                app.XMapTools_LastDir = Directory;
                app.XMapTools_GUI.Name = [app.XMapTools_VER,' - ',char(app.XMapTools_LastDir)];
            end
            
            close(app.WaitBar);
            
        end

        % Menu selected function: QuitMenu
        function Menu_XMapToolsQuitSelected(app, event)
            close(app.XMapTools_GUI);
        end

        % Menu selected function: InfosMenu
        function Menu_FileMapInfosSelected(app, event)
            ContextMenu_MainTree_InfoPushed(app)
        end

        % Menu selected function: DuplicateMenu
        function Menu_EditMapDuplicateSelected(app, event)
            ContextMenu_MainTree_DuplicatePushed(app);
        end

        % Menu selected function: DeleteMenu
        function Menu_EditMapDeleteSelected(app, event)
            ContextMenu_MainTree_DeletePushed(app);
        end

        % Menu selected function: RectangleMenu
        function Menu_EditMapROIRectangleSelected(app, event)
            DrawingMode(app,'on','Rectangle');
            app.TempROI = drawrectangle(app.FigMain);
            DrawingMode(app,'off');
            
            app.EliminateinsideMenu.Enable = 'on';
            app.EliminateoutsideMenu.Enable = 'on';
            app.SelectROIMenu.Enable = 'off';
        end

        % Menu selected function: PolygonMenu
        function Menu_EditMapROIPolygonSelected(app, event)
            DrawingMode(app,'on','Polygon');
            app.TempROI = drawpolygon(app.FigMain);
            DrawingMode(app,'off');
            
            app.EliminateinsideMenu.Enable = 'on';
            app.EliminateoutsideMenu.Enable = 'on';
            app.SelectROIMenu.Enable = 'off';
        end

        % Menu selected function: EliminateinsideMenu
        function Menu_EditMapROIElimInsideSelected(app, event)
            if ~isempty(app.TempROI)
                EliminatePixelsFromROI(app,'in');
                ROI_DeleteROI(app);
                app.SaveRequired = 1;
            end
        end

        % Menu selected function: EliminateoutsideMenu
        function Menu_EditMapROIElimOutsideSelected(app, event)
            if ~isempty(app.TempROI)
                EliminatePixelsFromROI(app,'out');
                ROI_DeleteROI(app);
                app.SaveRequired = 1;
            end
        end

        % Menu selected function: ExportMapDataMenu
        function Menu_EditMapExportMapDataSelected(app, event)
            filter = {'*.txt'};
            f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
            [filename,filepath] = uiputfile(filter);
            close(f);
            if isequal(filename,0) && isequal(filepath,0)
                figure(app.XMapTools_GUI)
                return
            end
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Saving the data';
            ImageData = PlotMap_ExtractPlottedImage(app);
            save([filepath,filename],'ImageData','-ASCII');
            close(app.WaitBar);
        end

        % Menu selected function: CopyMapDataMenu
        function Menu_EditMapCopyMapDataSelected(app, event)
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Copying to clipboard';
            ImageData = PlotMap_ExtractPlottedImage(app);
            clipboard('copy',ImageData);
            close(app.WaitBar);
        end

        % Menu selected function: SelectAreaMenu
        function Menu_EditMapSelectAreaMenuSelected(app, event)
            
            ROI_DeleteROI(app);
            
            DrawingMode(app,'on','Rectangle');
            app.ROI_SelectionTool = drawrectangle(app.FigMain,'Color',[0.57,0.00,0.69],'InteractionsAllowed','all');
            DrawingMode(app,'off');
            
            app.CropMenu.Enable = 'on';
            
        end

        % Menu selected function: CropMenu
        function Menu_EditMapCropMenuSelected(app, event)
            
            Mask = createMask(app.ROI_SelectionTool);
            
            OriginalSize = size(Mask);
            
            Lin = sum(Mask,1);
            Col = sum(Mask,2);
            
            WhereDataX = find(Lin > 0);
            Shift_X = [WhereDataX(1)-1,WhereDataX(end)+1];
            Pos_X = Shift_X;
            
            WhereDataY = find(Col > 0);
            Shift_Y = [WhereDataY(1)-1,WhereDataY(end)+1];
            Pos_Y = Shift_Y;
            
            % Check limits
            if Shift_X(1) < 1
                Shift_X(1) = 1;
                Pos_X(1) = 1;
            end
            if Shift_X(2) > OriginalSize(2)
                Shift_X(2) = OriginalSize(2);
                Pos_X(2) = OriginalSize(2);
            end
            if Shift_Y(1) < 1
                Shift_Y(1) = 1;
                Pos_Y(1) = 1;
            end
            if Shift_Y(2) > OriginalSize(1)
                Shift_Y(2) = OriginalSize(1);
                Pos_Y(2) = OriginalSize(1);
            end
            
            app.XMapToolsData.MapSizeCheck.OriginalSize = OriginalSize;
            app.XMapToolsData.MapSizeCheck.ActualSize = [Shift_Y(2)-Shift_Y(1),Shift_X(2)-Shift_X(1)];
            app.XMapToolsData.MapSizeCheck.SizeChanged = 1;
            app.XMapToolsData.MapSizeCheck.Shift_X = Shift_X;
            app.XMapToolsData.MapSizeCheck.Shift_Y = Shift_Y;
            
            app.mapsizeLabel.Text = ['Map size: ',num2str(app.XMapToolsData.MapSizeCheck.ActualSize(2)),' x ',num2str(app.XMapToolsData.MapSizeCheck.ActualSize(1)), ' (X,Y)'];
            
            SelectedNode = app.TreeData_Main.SelectedNodes.NodeData;
            
            switch SelectedNode(1)
                case 1
                    for i = 1:length(app.XMapToolsData.MapData.It.Names)
                        app.XMapToolsData.MapData.It.Data(i).Map = app.XMapToolsData.MapData.It.Data(i).Map(Pos_Y(1):Pos_Y(2),Pos_X(1):Pos_X(2));
                    end
                    
                    ROI_DeleteROI(app);
                    TreeData_MainSelectionChanged(app);
                case 3
                    
                    for i = 1:length(app.XMapToolsData.MapData.Me.Data(SelectedNode(2)).CData)
                        app.XMapToolsData.MapData.Me.Data(SelectedNode(2)).CData(i).Map = app.XMapToolsData.MapData.Me.Data(SelectedNode(2)).CData(i).Map(Pos_Y(1):Pos_Y(2),Pos_X(1):Pos_X(2));
                    end
                    
                    Selected = app.TreeData_Main.SelectedNodes.NodeData;
                    ROI_DeleteROI(app);
                    TreeData_MainSelectionChanged(app);
            end
            
            
        end

        % Callback function: Button_FigMain_CopyImage, CopyImageMenu
        function Menu_EditCopyImageSelected(app, event)
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Copying to clipboard';
            
            if isequal(app.PanelMulti.Visible,'on')
                copygraphics(app.PanelMulti,'ContentType','vector','BackgroundColor','none')
            else
                copygraphics(app.PanelFigMain,'ContentType','vector','BackgroundColor','none');
            end
            
            close(app.WaitBar);
        end

        % Callback function: Button_FigMain_OpenNewWindow, 
        % OpenImageNewWindowMenu_2
        function Menu_EditOpenImageNewWindowSelected(app, event)
            f2 = figure;
            ax2 = gca;
            
            copyobj(app.FigMain.Children,ax2);
            axis(ax2,'image');
            colormap(ax2,app.FigMain.Colormap)
            colorbar('vertical');
            ax2.XTick = app.FigMain.XTick;
            ax2.YTick = app.FigMain.YTick;
            ax2.YDir = app.FigMain.YDir;
            ax2.CLim = app.FigMain.CLim;
            
            ax2.XLim = app.FigMain.XLim;
            ax2.YLim = app.FigMain.YLim;
            
            ax2.ColorScale = app.FigMain.ColorScale;
            
            %keyboard
            
            figure(app.XMapTools_GUI)
            pause(0.2)
            figure(f2)
        end

        % Callback function: Button_FigMain_PlotSurface, 
        % Plot3DsurfaceMenu
        function Menu_EditPlot3DsurfaceSelected(app, event)
            
            Data = PlotMap_ExtractPlottedImage(app);
            MedFilterValue = app.Options_Medfilter3DsurfaceSpinner.Value;
            
            f2 = figure;
            if MedFilterValue > 0
                Data = medfilt2(Data,[MedFilterValue,MedFilterValue]);
            end
            
            Idx = find(Data>= app.EditField_LiveMin.Value & Data <= app.EditField_LiveMax.Value);
            DataNaN = nan(size(Data));
            DataNaN(Idx) = Data(Idx);
            
            mesh(DataNaN)
            colormap([app.ColorMapValues]);
            colorbar
            caxis([app.EditField_LiveMin.Value,app.EditField_LiveMax.Value])
            title(['Median filter: ',num2str(MedFilterValue)])
            
        end

        % Menu selected function: SaveImageMenu
        function Menu_FileSaveImageSelected(app, event)
            filter = {'*.jpg';'*.png';'*.tif';'*.pdf'};
            f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
            [filename,filepath] = uiputfile(filter);
            close(f);
            if ischar(filename)
                app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
                app.WaitBar.Message = 'Saving image';
                
                if isequal(app.PanelMulti.Visible,'on')
                    exportgraphics(app.PanelMulti,[filepath filename],'Resolution',300);
                else
                    exportgraphics(app.FigMain,[filepath filename],'Resolution',300);
                end
                
                
                close(app.WaitBar);
            end
        end

        % Callback function: Button_FigMain_ResetDisplay, 
        % ResetDisplayMenu
        function Menu_Plot_ResetDisplayMenuSelected(app, event)
            if isequal(app.Jiahui,3)
                app.Jiahui = app.Jiahui + 1;
                return
            else
                app.Jiahui = 0;
            end
            
            app.TreeData_Main.BackgroundColor = [1    1    1];
            app.TreeData_Additional.BackgroundColor = [1    1    1];
            app.MapInfo_TextArea.BackgroundColor = [0.94    0.94    0.94];
            
            TreeData_MainSelectionChanged(app, event);
        end

        % Callback function: AreaPolygonMenu, 
        % Sampling_SelectAreaButton
        function Menu_Sampling_AreaMenuSelected(app, event)
            %ROI_DeleteROI(app);
            Menu_Sampling_ResetROIMenuSelected(app);
            
            DrawingMode(app,'on','Polygon');
            app.ROI_sampling = drawpolygon(app.FigMain,'Color',[0.57,0.00,0.69],'InteractionsAllowed','all');
            DrawingMode(app,'off');
            
            app.ROI_sampling_Listener = addlistener(app.ROI_sampling, 'ROIMoved', @(varargin)Sampling_ROI_changed_shape(app, app.ROI_sampling));
            
            % Extract data
            Sampling_ROI_changed_shape(app,app.ROI_sampling);
            
            app.SaveResultsMenu.Enable = 'on';
            app.Sampling_ResetButton.Enable = 'on';
        end

        % Callback function: CircleMenu, Sampling_SelectCircleButton
        function Menu_Sampling_CircleMenuSelected(app, event)
            app.TabGroup.SelectedTab = app.SamplingTab;
            
            %ROI_DeleteROI(app);
            Menu_Sampling_ResetROIMenuSelected(app);
            
            DrawingMode(app,'on','Circle');
            app.ROI_sampling = drawcircle(app.FigMain,'Color',[0.57,0.00,0.69],'InteractionsAllowed','all');
            DrawingMode(app,'off');
            
            app.ROI_sampling_Listener = addlistener(app.ROI_sampling, 'ROIMoved', @(varargin)Sampling_ROI_changed_shape(app, app.ROI_sampling));
            
            % Extract data
            Sampling_ROI_changed_shape(app,app.ROI_sampling);
            
            app.SaveResultsMenu.Enable = 'on';
            app.Sampling_ResetButton.Enable = 'on';
        end

        % Callback function: Sampling_SelectTransectButton, 
        % TransectMenu
        function Menu_Sampling_TransectMenuSelected(app, event)
            app.TabGroup.SelectedTab = app.SamplingTab;
            
            %ROI_DeleteROI(app);
            Menu_Sampling_ResetROIMenuSelected(app);
            
            DrawingMode(app,'on','Line');
            app.ROI_sampling = drawpolyline(app.FigMain,'Color',[0.57,0.00,0.69],'InteractionsAllowed','all');
            DrawingMode(app,'off');
            
            app.ROI_sampling_Listener = addlistener(app.ROI_sampling, 'ROIMoved', @(varargin)Sampling_ROI_changed_line(app, app.ROI_sampling));
            
            %app.Handle_ExtFigure = figure;
            %app.GCA_ExtFigure = gca;
            
            Sampling_ROI_changed_line(app,app.ROI_sampling);
            
            app.SaveResultsMenu.Enable = 'on';
        end

        % Callback function: Sampling_SelectStripeButton, StripMenu
        function Menu_Sampling_StripeMenuSelected(app, event)
            app.TabGroup.SelectedTab = app.SamplingTab;
            
            %ROI_DeleteROI(app);
            Menu_Sampling_ResetROIMenuSelected(app);
            
            DrawingMode(app,'on','Rectangle');
            app.ROI_sampling = drawrectangle(app.FigMain,'Color',[0.57,0.00,0.69],'Rotatable',1,'InteractionsAllowed','all','Label','>>','LabelTextColor','w');
            DrawingMode(app,'off');
            
            app.ROI_sampling_Listener = addlistener(app.ROI_sampling, 'ROIMoved', @(varargin)Sampling_ROI_changed_strip(app, app.ROI_sampling));
            
            app.ROI_sampling_Listener = addlistener(app.ROI_sampling, 'MovingROI', @(varargin)Sampling_ROI_changing_strip(app, app.ROI_sampling));
            
            %app.Handle_ExtFigure = figure;
            %app.GCA_ExtFigure = gca;
            
            Sampling_ROI_changed_strip(app,app.ROI_sampling);
            
            app.SaveResultsMenu.Enable = 'on';
            
        end

        % Callback function: ResetROIMenu, ResetROIMenu_2, 
        % Sampling_ResetButton
        function Menu_Sampling_ResetROIMenuSelected(app, event)
            ROI_DeleteROI(app);
            
            cla(app.Sampling_Plot1)
            colorbar(app.Sampling_Plot1,'hide')
            cla(app.Sampling_Plot2)
            app.Sampling_Plot1.Visible = 'off';
            app.Sampling_Plot2.Visible = 'off';
            
            app.SaveResultsMenu.Enable = 'off';
            
            app.Sampling_ResetButton.Enable = 'off';
            
            app.CropMenu.Enable = 'off';
            
        end

        % Menu selected function: SingleMapMenu
        function Menu_Sampling_Export_SingleMapMenuSelected(app, event)
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Exporting data';
            
            switch app.ROI_sampling.Type
                case 'images.roi.polyline'
                    ImageData = PlotMap_ExtractPlottedImage(app);
                    Positions = app.ROI_sampling.Position;
                    
                    X = Positions(:,1);
                    Y = Positions(:,2);
                    
                    [LesData] = Sampling_CalculateLineXY(app,X,Y,ImageData);
                    
                    Res = app.ResolutionField.Value;
                    
                    X = LesData(:,2)*Res;
                    Y = LesData(:,3);
                    
                    [Success,Message,MessageID] = mkdir('Exported-Sampling');
                    DateStr = char(datestr(now));
                    DateStr(find(DateStr == ' ')) = '_'; DateStr(find(DateStr == ':')) = '_';
                    ProjectName = ['Sampling_Transect_',DateStr];
                    Directory = fullfile(cd,'Exported-Sampling',ProjectName);
                    [Success,Message,MessageID] = mkdir(Directory);
                    
                    fid = fopen(fullfile(Directory,'Data.txt'),'w');
                    fprintf(fid,'%s\n',['Data exported with ',app.XMapTools_VER,' on ',datestr(now)]);
                    fprintf(fid,'%s\n',['Selected map:     ',app.Info_MapType,' | ',app.Info_Phase,' | ',app.Info_MapName]);
                    fprintf(fid,'%s\n','Method: Transect');
                    fprintf(fid,'%s\n\n','Columns: Distance | Value ');
                    
                    for i = 1:length(X)
                        fprintf(fid,'%12.8f\t%12.8f\n',X(i),Y(i));
                    end
                    
                    fprintf(fid,'\n');
                    fclose(fid);
                    
                    f2 = figure;
                    ax2 = gca;
                    
                    copyobj(app.Sampling_Plot1.Children,ax2);
                    colormap(ax2,app.Sampling_Plot1.Colormap)
                    colorbar('vertical');
                    ax2.XTick = app.Sampling_Plot1.XTick;
                    ax2.YTick = app.Sampling_Plot1.YTick;
                    ax2.YDir = app.Sampling_Plot1.YDir;
                    ax2.CLim = app.Sampling_Plot1.CLim;
                    ax2.XLabel = app.Sampling_Plot1.XLabel;
                    ax2.YLabel = app.Sampling_Plot1.YLabel;
                    
                    saveas(f2,fullfile(Directory,'Transect.pdf'), 'pdf');
                    close(f2);
                    figure(app.XMapTools_GUI)
                    
                    exportgraphics(app.FigMain,fullfile(Directory,'Map.pdf'));
                    
                case 'images.roi.circle'
                    ImageData = PlotMap_ExtractPlottedImage(app);
                    mask = createMask(app.ROI_sampling);
                    
                    Ind = find(mask);
                    
                    [Success,Message,MessageID] = mkdir('Exported-Sampling');
                    DateStr = char(datestr(now));
                    DateStr(find(DateStr == ' ')) = '_';
                    DateStr(find(DateStr == ':')) = '_';
                    ProjectName = ['Sampling_Circle_',DateStr];
                    Directory = fullfile(cd,'Exported-Sampling',ProjectName);
                    [Success,Message,MessageID] = mkdir(Directory);
                    
                    Nb = numel(find(ImageData(Ind) > 0));
                    
                    fid = fopen(fullfile(Directory,'Data.txt'),'w');
                    fprintf(fid,'%s\n',['Data exported with ',app.XMapTools_VER,' on ',datestr(now)]);
                    fprintf(fid,'%s\n',['Selected map:     ',app.Info_MapType,' | ',app.Info_Phase,' | ',app.Info_MapName]);
                    fprintf(fid,'%s\n','Method: Circle');
                    fprintf(fid,'%s\n\n','Columns: Mean | Median | Stdev | Std_err | #_non_zero ');
                    
                    fprintf(fid,'%12.8f\t%12.8f\t%12.8f\t%12.8f\t%12.0f\n',mean(ImageData(Ind)),median(ImageData(Ind)),std(ImageData(Ind)),std(ImageData(Ind))/Nb,Nb);
                    fprintf(fid,'\n\n');
                    
                    fprintf(fid,'%s\n\n','All pixel data:');
                    for i = 1:numel(Ind)
                        fprintf(fid,'%12.8f\n',ImageData(Ind(i)));
                    end
                    
                    fprintf(fid,'\n');
                    fclose(fid);
                    
                    exportgraphics(app.FigMain,fullfile(Directory,'Map.pdf'));
                    
                case 'images.roi.polygon'
                    ImageData = PlotMap_ExtractPlottedImage(app);
                    mask = createMask(app.ROI_sampling);
                    
                    Ind = find(mask);
                    
                    [Success,Message,MessageID] = mkdir('Exported-Sampling');
                    DateStr = char(datestr(now));
                    DateStr(find(DateStr == ' ')) = '_';
                    DateStr(find(DateStr == ':')) = '_';
                    ProjectName = ['Sampling_Polygon_',DateStr];
                    Directory = fullfile(cd,'Exported-Sampling',ProjectName);
                    [Success,Message,MessageID] = mkdir(Directory);
                    
                    fid = fopen(fullfile(Directory,'Data.txt'),'w');
                    fprintf(fid,'%s\n',['Data exported with ',app.XMapTools_VER,' on ',datestr(now)]);
                    fprintf(fid,'%s\n',['Selected map:     ',app.Info_MapType,' | ',app.Info_Phase,' | ',app.Info_MapName]);
                    fprintf(fid,'%s\n','Method: Polygon');
                    fprintf(fid,'%s\n\n','Columns: Mean | Median | Stdev | Std_err | #_non_zero ');
                    
                    Nb = numel(find(ImageData(Ind) > 0));
                    
                    fprintf(fid,'%12.8f\t%12.8f\t%12.8f\t%12.8f\t%12.0f\n',mean(ImageData(Ind)),median(ImageData(Ind)),std(ImageData(Ind)),std(ImageData(Ind))/Nb,Nb);
                    fprintf(fid,'\n\n');
                    
                    fprintf(fid,'%s\n\n','All pixel data:');
                    for i = 1:numel(Ind)
                        fprintf(fid,'%12.8f\n',ImageData(Ind(i)));
                    end
                    
                    fprintf(fid,'\n');
                    fclose(fid);
                    
                    exportgraphics(app.FigMain,fullfile(Directory,'Map.pdf'));
                    
                case 'images.roi.rectangle'
                    ImageData = PlotMap_ExtractPlottedImage(app);
                    Verticles = app.ROI_sampling.Vertices;
                    
                    % Verticle 1 --> Point 4 (in XMapTools 3)
                    % Verticle 2 --> Point 6
                    % Verticle 3 --> Point 7
                    % Verticle 4 --> Point 5
                    try
                        [MatrixProfils,Distances] = ExtractMatrixProfils(app,ImageData,Verticles(1,1),Verticles(1,2),Verticles(2,1),Verticles(2,2),Verticles(3,1),Verticles(3,2));   % previously X4,Y4,X6,Y6,X7,Y7
                    catch
                        return
                    end
                    
                    Res = app.ResolutionField.Value;
                    Distances = Distances.*Res;
                    
                    TheMean = zeros(1,size(MatrixProfils,1));
                    TheMedian = zeros(1,size(MatrixProfils,1));
                    TheStd = zeros(1,size(MatrixProfils,1));
                    Nb = zeros(1,size(MatrixProfils,1));
                    for i=1:size(MatrixProfils,1)
                        WhereValues = find(MatrixProfils(i,:) > 1e-19);
                        TheMean(i) = mean(MatrixProfils(i,WhereValues));
                        TheMedian(i) = median(MatrixProfils(i,WhereValues));
                        TheStd(i) = std(MatrixProfils(i,WhereValues));
                        Nb(i) = length(find(MatrixProfils(i,:)>1e-19));
                    end
                    FractPer = Nb/size(MatrixProfils,2)*100;
                    
                    WhereZero = find(MatrixProfils(:) < 1e-19);
                    MatrixProfils(WhereZero) = nan(size(WhereZero));
                    
                    [Success,Message,MessageID] = mkdir('Exported-Sampling');
                    DateStr = char(datestr(now));
                    DateStr(find(DateStr == ' ')) = '_';
                    DateStr(find(DateStr == ':')) = '_';
                    ProjectName = ['Sampling_Strip_',DateStr];
                    Directory = fullfile(cd,'Exported-Sampling',ProjectName);
                    [Success,Message,MessageID] = mkdir(Directory);
                    
                    fid = fopen(fullfile(Directory,'Data.txt'),'w');
                    fprintf(fid,'%s\n',['Data exported with ',app.XMapTools_VER,' on ',datestr(now)]);
                    fprintf(fid,'%s\n',['Selected map:     ',app.Info_MapType,' | ',app.Info_Phase,' | ',app.Info_MapName]);
                    fprintf(fid,'%s\n','Method: Strip');
                    fprintf(fid,'%s\n\n','Columns: Distance | Mean | Median | Stdev | Std_err | # | %_non_zero');
                    
                    for i = 1:length(TheMean)
                        fprintf(fid,'%12.8f\t%12.8f\t%12.8f\t%12.8f\t%12.8f\t%12.0f\t%12.2f\t\n',Distances(i),TheMean(i),TheMedian(i),TheStd(i),TheStd(i)/Nb(i),Nb(i),FractPer(i));
                    end
                    
                    fprintf(fid,'\n\n');
                    
                    fprintf(fid,'%s\n\n','All pixel data:');
                    fprintf(fid,[repmat('%12.8f\t', 1, size(MatrixProfils, 2)) '\n'], MatrixProfils');
                    
                    fprintf(fid,'\n');
                    fclose(fid);
                    
                    exportgraphics(app.FigMain,fullfile(Directory,'Map.pdf'));
            end
            %pause(2)
            app.WaitBar.Message = 'Successful exporting!';
            pause(2)
            close(app.WaitBar);
        end

        % Menu selected function: ExportMergedMenu
        function Menu_Sampling_Export_MergedMenuSelected(app, event)
            
            NodeData = app.TreeData_Main.SelectedNodes.NodeData;
            
            switch NodeData(1)
                case 3
                    Data = app.XMapToolsData.MapData.Me.Data(NodeData(2));
                    
                    [Success,Message,MessageID] = mkdir('Exported-Merged');
                    DateStr = char(datestr(now));
                    DateStr(find(DateStr == ' ')) = '_';
                    DateStr(find(DateStr == ':')) = '_';
                    ProjectName = [app.XMapToolsData.MapData.Me.Names{NodeData(2)},'_',DateStr];
                    Directory = fullfile(cd,'Exported-Merged',ProjectName);
                    [Success,Message,MessageID] = mkdir(Directory);
                    
                    app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
                    app.WaitBar.Message = 'Exporting data';
                    
                    for i = 1:length(Data.ElNames)
                        TheMap = Data.CData(i).Map;
                        save(fullfile(Directory,[Data.ElNames{i},'.txt']),'TheMap','-ascii');
                    end
                    
                    app.WaitBar.Message = 'Successful exporting!';
                    pause(2)
                    close(app.WaitBar);
            end
            
        end

        % Menu selected function: Exportashdf5ResultsMenu
        function Menu_Sampling_Export_hdf5MenuSelected(app, event)
            
            NodeData = app.TreeData_Main.SelectedNodes.NodeData;
            
            switch NodeData(1)
                case 4
                    Data = app.XMapToolsData.MapData.Re.Data(NodeData(2));
                    f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
                    [filename, pathname] = uiputfile([app.XMapToolsData.MapData.Re.Names{NodeData(2)},'.hdf5'], 'Save as');  % , 'HDF5 (*.hdf5)'; '*.*', 'All Files (*.*)'}
                    delete(f);
                    figure(app.XMapTools_GUI)
                    
                    hdf5write(fullfile(pathname,filename),'/Information',['Dataset generated with XMapTools ',app.XMapTools_VER]);
                    hdf5write(fullfile(pathname,filename),'/ElementList',Data.Labels,'WriteMode','append');
                    
                    for i = 1:length(Data.Labels)
                        Map = Data.CData(i).Map;
                        [WhereNaN] = find(isnan(Map));
                        Map(WhereNaN) = 0;
                        hdf5write(fullfile(pathname,filename),['/CData/',Data.Labels{i}],Map,'WriteMode','append');
                    end
            end
            
        end

        % Menu selected function: MultipleMapsMenu
        function Menu_Sampling_Export_MultipleMapsMenuSelected(app, event)
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Exporting data';
            
            NodeData = app.TreeData_Main.SelectedNodes.NodeData;
            
            ApplyMaskCorrection = 0;
            
            switch NodeData(1)
                case 1
                    Names = app.XMapToolsData.MapData.It.Names;
                    Data = app.XMapToolsData.MapData.It.Data;
                    MaskFile = app.XMapToolsData.MapData.MaskFile;
                    
                    if ~isempty(app.TreeData_Additional.SelectedNodes)
                        NodeDataAdditional = app.TreeData_Additional.SelectedNodes.NodeData;
                        if isequal(NodeDataAdditional(1),11)
                            if NodeDataAdditional(2) > 0 && NodeDataAdditional(3) > 0
                                % A mask is selected and we work with
                                % intensity data
                                Mask = zeros(size(MaskFile.Masks(NodeDataAdditional(2)).MaskMap));
                                Where = find(MaskFile.Masks(NodeDataAdditional(2)).MaskMap == NodeDataAdditional(3)-1);
                                Mask(Where) = 1;
                                ApplyMaskCorrection = 1;
                            end
                        end
                    end
                case 2
                    if NodeData(2) > 0
                        Names = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElNames;
                        Data = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).CData;
                        MaskFile = [];
                    end
                case 3
                    if NodeData(2) > 0
                        Names = app.XMapToolsData.MapData.Me.Data(NodeData(2)).ElNames;
                        Data = app.XMapToolsData.MapData.Me.Data(NodeData(2)).CData;
                        MaskFile = [];
                    end
                case 4
                    if NodeData(2) > 0
                        Names = app.XMapToolsData.MapData.Re.Data(NodeData(2)).Labels;
                        Data = app.XMapToolsData.MapData.Re.Data(NodeData(2)).CData;
                        MaskFile = [];
                    end
            end
            
            if ~exist('Data')
                close(app.WaitBar)
                return
            end
            if isempty(Data)
                close(app.WaitBar)
                return
            end
            
            switch app.ROI_sampling.Type
                
                case 'images.roi.polyline'
                    if ApplyMaskCorrection
                        ImageData = Data(1).Map .* Mask;
                    else
                        ImageData = Data(1).Map;
                    end
                    Positions = app.ROI_sampling.Position;
                    
                    X = Positions(:,1);
                    Y = Positions(:,2);
                    
                    [LesData] = Sampling_CalculateLineXY(app,X,Y,ImageData);
                    
                    DataTable = zeros(size(LesData,1),length(Names));
                    DataTable(:,1) = LesData(:,3);
                    
                    Res = app.ResolutionField.Value;
                    Distances = LesData(:,2)*Res;
                    
                    for i = 2:length(Names)
                        if ApplyMaskCorrection
                            ImageData = Data(i).Map .* Mask;
                        else
                            ImageData = Data(i).Map;
                        end
                        [LesData] = Sampling_CalculateLineXY(app,X,Y,ImageData);
                        DataTable(:,i) = LesData(:,3);
                    end
                    
                    [Success,Message,MessageID] = mkdir('Exported-Sampling');
                    DateStr = char(datestr(now));
                    DateStr(find(DateStr == ' ')) = '_';
                    DateStr(find(DateStr == ':')) = '_';
                    ProjectName = ['Sampling_Transect_',DateStr];
                    Directory = fullfile(cd,'Exported-Sampling',ProjectName);
                    [Success,Message,MessageID] = mkdir(Directory);
                    
                    f2 = figure;
                    plot(Distances,DataTable)
                    colormap(colorcube(size(DataTable,2)))
                    legend(Names)
                    
                    saveas(f2,fullfile(Directory,'Transect.pdf'), 'pdf');
                    close(f2);
                    figure(app.XMapTools_GUI)
                    
                    exportgraphics(app.FigMain,fullfile(Directory,'Map.pdf'));
                    
                    fid = fopen(fullfile(Directory,'Data.txt'),'w');
                    fprintf(fid,'%s\n',['Data exported with ',app.XMapTools_VER,' on ',datestr(now)]);
                    fprintf(fid,'%s\n',['Selected map:     ',app.Info_MapType,' | ',app.Info_Phase,' | ',app.Info_MapName]);
                    fprintf(fid,'%s\n\n',['Sampling method:  ','Transect']);
                    
                    fprintf(fid,'%s\t','Distance');
                    for i = 1:length(Names)
                        fprintf(fid,'%s\t',char(Names{i}));
                    end
                    fprintf(fid,'\n');
                    DataTable = [Distances,DataTable];
                    fprintf(fid,[repmat('%12.8f\t', 1, size(DataTable, 2)) '\n'], DataTable');
                    
                    fprintf(fid,'\n');
                    fclose(fid);
                    
                    exportgraphics(app.FigMain,fullfile(Directory,'Map.pdf'));
                    
                case 'images.roi.circle'
                    if ApplyMaskCorrection
                        ImageData = Data(1).Map .* Mask;
                    else
                        ImageData = Data(1).Map;
                    end
                    mask = createMask(app.ROI_sampling);
                    
                    Ind = find(mask);
                    
                    DataTable = zeros(2,length(Names));
                    
                    DataTable(1,1) = mean(ImageData(Ind));
                    DataTable(2,1) = median(ImageData(Ind));
                    DataTable(3,1) = std(ImageData(Ind));
                    Nb = numel(find(ImageData(Ind) > 0));
                    DataTable(4,1) = std(ImageData(Ind))/Nb;
                    DataTable(5,1) = Nb;
                    DataAll(1).Data = ImageData(Ind);
                    
                    for i = 2:length(Names)
                        if ApplyMaskCorrection
                            ImageData = Data(i).Map .* Mask;
                        else
                            ImageData = Data(i).Map;
                        end
                        DataAll(i).Data = ImageData(Ind);
                        DataTable(1,i) = mean(ImageData(Ind));
                        DataTable(2,i) = median(ImageData(Ind));
                        DataTable(3,i) = std(ImageData(Ind));
                        Nb = numel(find(ImageData(Ind) > 0));
                        DataTable(4,i) = std(ImageData(Ind))/Nb;
                        DataTable(5,i) = Nb;
                    end
                    
                    [Success,Message,MessageID] = mkdir('Exported-Sampling');
                    DateStr = char(datestr(now));
                    DateStr(find(DateStr == ' ')) = '_';
                    DateStr(find(DateStr == ':')) = '_';
                    ProjectName = ['Sampling_Circle_',DateStr];
                    Directory = fullfile(cd,'Exported-Sampling',ProjectName);
                    [Success,Message,MessageID] = mkdir(Directory);
                    
                    saveSamplingDataMultipleFile(app,Directory,'Circle',DataTable,Names,DataAll);
                    
                    exportgraphics(app.FigMain,fullfile(Directory,'Map.pdf'));
                    
                case 'images.roi.polygon'
                    if ApplyMaskCorrection
                        ImageData = Data(1).Map .* Mask;
                    else
                        ImageData = Data(1).Map;
                    end
                    mask = createMask(app.ROI_sampling);
                    
                    Ind = find(mask);
                    
                    DataTable = zeros(2,length(Names));
                    
                    DataTable(1,1) = mean(ImageData(Ind));
                    DataTable(2,1) = median(ImageData(Ind));
                    DataTable(3,1) = std(ImageData(Ind));
                    Nb = numel(find(ImageData(Ind) > 0));
                    DataTable(4,1) = std(ImageData(Ind))/Nb;
                    DataTable(5,1) = Nb;
                    DataAll(1).Data = ImageData(Ind);
                    
                    for i = 2:length(Names)
                        if ApplyMaskCorrection
                            ImageData = Data(i).Map .* Mask;
                        else
                            ImageData = Data(i).Map;
                        end
                        DataAll(i).Data = ImageData(Ind);
                        DataTable(1,i) = mean(ImageData(Ind));
                        DataTable(2,i) = median(ImageData(Ind));
                        DataTable(3,i) = std(ImageData(Ind));
                        Nb = numel(find(ImageData(Ind) > 0));
                        DataTable(4,i) = std(ImageData(Ind))/Nb;
                        DataTable(5,i) = Nb;
                    end
                    
                    [Success,Message,MessageID] = mkdir('Exported-Sampling');
                    DateStr = char(datestr(now));
                    DateStr(find(DateStr == ' ')) = '_';
                    DateStr(find(DateStr == ':')) = '_';
                    ProjectName = ['Sampling_Polygon_',DateStr];
                    Directory = fullfile(cd,'Exported-Sampling',ProjectName);
                    [Success,Message,MessageID] = mkdir(Directory);
                    
                    saveSamplingDataMultipleFile(app,Directory,'Polygon',DataTable,Names,DataAll);
                    
                    exportgraphics(app.FigMain,fullfile(Directory,'Map.pdf'));
                    
                case 'images.roi.rectangle'
                    if ApplyMaskCorrection
                        ImageData = Data(1).Map .* Mask;
                    else
                        ImageData = Data(1).Map;
                    end
                    Verticles = app.ROI_sampling.Vertices;
                    
                    try
                        [MatrixProfils,Distances] = ExtractMatrixProfils(app,ImageData,Verticles(1,1),Verticles(1,2),Verticles(2,1),Verticles(2,2),Verticles(3,1),Verticles(3,2));   % previously X4,Y4,X6,Y6,X7,Y7
                    catch
                        return
                    end
                    
                    Res = app.ResolutionField.Value;
                    Distances = Distances.*Res;
                    
                    DataTable = zeros(numel(Distances),length(Names));
                    
                    TheMean = zeros(1,size(MatrixProfils,1));
                    for i=1:size(MatrixProfils,1)
                        WhereValues = find(MatrixProfils(i,:) > 1e-19);
                        TheMean(i) = mean(MatrixProfils(i,WhereValues));
                    end
                    
                    DataTable(:,1) = TheMean;
                    
                    for i = 2:length(Names)
                        if ApplyMaskCorrection
                            ImageData = Data(i).Map .* Mask;
                        else
                            ImageData = Data(i).Map;
                        end
                        try
                            [MatrixProfils,Distances] = ExtractMatrixProfils(app,ImageData,Verticles(1,1),Verticles(1,2),Verticles(2,1),Verticles(2,2),Verticles(3,1),Verticles(3,2));   % previously X4,Y4,X6,Y6,X7,Y7
                        catch
                            return
                        end
                        TheMean = zeros(1,size(MatrixProfils,1));
                        for j=1:size(MatrixProfils,1)
                            WhereValues = find(MatrixProfils(j,:) > 1e-19);
                            TheMean(j) = mean(MatrixProfils(j,WhereValues));
                        end
                        DataTable(:,i) = TheMean;
                    end
                    
                    Distances = Distances.*Res;
                    
                    [Success,Message,MessageID] = mkdir('Exported-Sampling');
                    DateStr = char(datestr(now));
                    DateStr(find(DateStr == ' ')) = '_';
                    DateStr(find(DateStr == ':')) = '_';
                    ProjectName = ['Sampling_Strip_',DateStr];
                    Directory = fullfile(cd,'Exported-Sampling',ProjectName);
                    [Success,Message,MessageID] = mkdir(Directory);
                    
                    f2 = figure;
                    plot(Distances,DataTable)
                    colormap(colorcube(size(DataTable,2)))
                    legend(Names)
                    
                    saveas(f2,fullfile(Directory,'Strip.pdf'), 'pdf');
                    close(f2);
                    figure(app.XMapTools_GUI)
                    
                    exportgraphics(app.FigMain,fullfile(Directory,'Map.pdf'));
                    
                    fid = fopen(fullfile(Directory,'Data.txt'),'w');
                    fprintf(fid,'%s\n',['Data exported with ',app.XMapTools_VER,' on ',datestr(now)]);
                    fprintf(fid,'%s\n',['Selected map:     ',app.Info_MapType,' | ',app.Info_Phase,' | ',app.Info_MapName]);
                    fprintf(fid,'%s\n\n',['Sampling method:  ','Strip']);
                    
                    fprintf(fid,'%s\t','Distance')
                    for i = 1:length(Names)
                        fprintf(fid,'%s\t',char(Names{i}));
                    end
                    fprintf(fid,'\n');
                    fprintf(fid,[repmat('%12.8f\t', 1, size([Distances',DataTable], 2)) '\n'], [Distances',DataTable]');
                    
                    fprintf(fid,'\n');
                    fclose(fid);
                    
                    exportgraphics(app.FigMain,fullfile(Directory,'Map.pdf'));
            end
            app.WaitBar.Message = 'Export successful!';
            pause(2)
            close(app.WaitBar);
        end

        % Menu selected function: DataVisualizationMenu
        function Menu_Modules_VisualizationToolMenuSelected(app, event)
            
            NodeData = app.TreeData_Main.SelectedNodes.NodeData;
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Waiting for the Data Visualization Module (close the module to resume XMapTools) ...';
            
            switch NodeData(1)
                case 1
                    Selected = NodeData(2);
                    Names = app.XMapToolsData.MapData.It.Names;
                    Data = app.XMapToolsData.MapData.It.Data;
                    MaskFile = app.XMapToolsData.MapData.MaskFile;
                    
                    waitfor(Data_Visualization(app,Names,Data,Selected,MaskFile))
                    
                case 2
                    if NodeData(2) > 0
                        Selected = NodeData(3);
                        Names = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElNames;
                        Data = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).CData;
                        MaskFile = app.XMapToolsData.MapData.MaskFile;
                        
                        waitfor(Data_Visualization(app,Names,Data,Selected,MaskFile))
                    end
                    
                case 3
                    if NodeData(2) > 0
                        Selected = NodeData(3);
                        Names = app.XMapToolsData.MapData.Me.Data(NodeData(2)).ElNames;
                        Data = app.XMapToolsData.MapData.Me.Data(NodeData(2)).CData;
                        MaskFile = app.XMapToolsData.MapData.MaskFile;
                        
                        waitfor(Data_Visualization(app,Names,Data,Selected,MaskFile))
                    end
                    
                case 4
                    if NodeData(2) > 0
                        Selected = NodeData(3);
                        Names = app.XMapToolsData.MapData.Re.Data(NodeData(2)).Labels;
                        Data = app.XMapToolsData.MapData.Re.Data(NodeData(2)).CData;
                        MaskFile = app.XMapToolsData.MapData.MaskFile;
                        
                        waitfor(Data_Visualization(app,Names,Data,Selected,MaskFile))
                    end
                    
                case 5
                    if NodeData(2) > 0
                        Selected = NodeData(2);
                    else
                        Selected = 1;
                    end
                    
                    Names = app.XMapToolsData.MapData.Ot.Names;
                    Data = app.XMapToolsData.MapData.Ot.Data;
                    MaskFile = app.XMapToolsData.MapData.MaskFile;
                    
                    waitfor(Data_Visualization(app,Names,Data,Selected,MaskFile))
                    
            end
            
            close(app.WaitBar);
            
            
        end

        % Menu selected function: GeneratorMenu
        function Menu_Modules_GeneratorMenuSelected(app, event)
            
            
            NodeData = app.TreeData_Main.SelectedNodes.NodeData;
            
            switch NodeData(1)
                case 1
                    %Selected = NodeData(2);
                    Names = app.XMapToolsData.MapData.It.Names;
                    Data = app.XMapToolsData.MapData.It.Data;
                    
                    app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
                    app.WaitBar.Message = 'Close the Generator window to resume XMapTools';
                    waitfor(Generator(app,Names,Data));
                    close(app.WaitBar)
                    TreeData_MainSelectionChanged(app);
                    
                case 2
                    if NodeData(2) > 0
                        %Selected = NodeData(3);
                        Names = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElNames;
                        Data = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).CData;
                        
                        waitfor(Generator(app,Names,Data))
                        TreeData_MainSelectionChanged(app);
                    else
                        return
                    end
                    
                case 3
                    if NodeData(2) > 0
                        %Selected = NodeData(3);
                        Names = app.XMapToolsData.MapData.Me.Data(NodeData(2)).ElNames;
                        Data = app.XMapToolsData.MapData.Me.Data(NodeData(2)).CData;
                        
                        waitfor(Generator(app,Names,Data))
                        TreeData_MainSelectionChanged(app);
                    else
                        return
                    end
                    
                case 4
                    if NodeData(2) > 0
                        %Selected = NodeData(3);
                        Names = app.XMapToolsData.MapData.Re.Data(NodeData(2)).Labels;
                        Data = app.XMapToolsData.MapData.Re.Data(NodeData(2)).CData;
                        
                        waitfor(Generator(app,Names,Data))
                        TreeData_MainSelectionChanged(app);
                    else
                        return
                    end
                    
            end
            
            
            
            
        end

        % Menu selected function: SpiderPlotMenu
        function Menu_Modules_SpiderPlotMenuSelected(app, event)
            if isequal(app.Calibrate_Spider_Button.Enable,'on')
                
                app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
                app.WaitBar.Message = 'Waiting for the Spider Module (close the module to resume XMapTools) ...';
                
                waitfor(Spider(app));
                
                close(app.WaitBar);
            end
        end

        % Button pushed function: Sampling_ExportButton
        function Sampling_ExportButtonPushed(app, event)
            
            figure();
            ax2 = gca;
            copyobj(app.Sampling_Plot1.Children,ax2);
            
            ax2.YLim = app.Sampling_Plot1.YLim;
            ax2.YLim = app.Sampling_Plot1.YLim;
            ax2.Colormap = app.Sampling_Plot1.Colormap;
            ax2.Title = app.Sampling_Plot1.Title;
            colorbar('horizontal')
            
            if isequal(app.Sampling_Plot2.Visible,'on')
                figure();
                ax2 = gca;
                copyobj(app.Sampling_Plot2.Children,ax2);
                
                ax2.YLim = app.Sampling_Plot2.YLim;
                ax2.YLim = app.Sampling_Plot2.YLim;
                ax2.Title = app.Sampling_Plot2.Title;
            end
            
            
        end

        % Button pushed function: ButtonRotateView
        function ButtonRotateViewPushed(app, event)
            
            Position = app.FigMain.View(1);
            if Position < 269
                app.FigMain.View(1) = app.FigMain.View(1)+90;
            else
                app.FigMain.View(1) = 0;
            end
            app.FieldAngleView.Value = app.FigMain.View(1);
            
            PlotMap_AddScaleBar(app);
            
        end

        % Button pushed function: ButtonImportMaps
        function Import_ButtonImportMapsPushed(app, event)
            %
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Waiting for data input from the Import Tool';
            
            waitfor(Import_Tool(app));
            
            app.WaitBar.Message = 'Updating, please wait...';
            
            ResetGUI(app);
            UpdateGUI(app);
            
            if ~isempty(app.XMapToolsData.MapData.It.Types)
                expand(app.Node_It);
                app.TreeData_Main.SelectedNodes = app.Node_It.Children(1);
                TreeData_MainSelectionChanged(app);
                app.XMapToolsData.MapSizeCheck.OriginalSize = size(app.XMapToolsData.MapData.It.Data(1).Map);
                if isempty(app.XMapToolsData.MapSizeCheck.ActualSize)
                    app.XMapToolsData.MapSizeCheck.ActualSize = app.XMapToolsData.MapSizeCheck.OriginalSize;
                end
                app.mapsizeLabel.Text = ['Map size: ',num2str(app.XMapToolsData.MapSizeCheck.ActualSize(2)),' x ',num2str(app.XMapToolsData.MapSizeCheck.ActualSize(1)), ' (X,Y)'];
            end
            
            app.SaveRequired = 1;
            
            close(app.WaitBar)
            
        end

        % Button pushed function: Import_CTdata
        function Import_CTdataButtonPushed(app, event)
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Pick files to import as an image stack';
            
            f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
            cd(app.XMapTools_LastDir);
            [FileName,PathName,FilterIndex] = uigetfile({'*.jpg;*.png;','Image (*.jpg, *.png)';'*.*',  'All Files (*.*)'},'Pick images','MultiSelect', 'on');
            delete(f)
            figure(app.XMapTools_GUI);
            
            close(app.WaitBar);
            
            d = uiprogressdlg(gcbf,'Message','Loading selected files','Title','XMapTools');
            d.Value = 0;
            
            count = 0;
            for i = 1:length(FileName)
                
                try
                    img = imread([PathName,'/',FileName{i}]);
                    
                    count = count+1;
                    app.XMapToolsData.MapData.Ct.Names{count} = FileName{i};
                    if size(img,3) > 1
                        app.XMapToolsData.MapData.Ct.Data(count).Map = rgb2gray(img); %im2double(rgb2gray(img))*255;
                    else
                        app.XMapToolsData.MapData.Ct.Data(count).Map = img;
                    end
                    
                catch
                    
                end
                d.Value = i/length(FileName);
                
            end
            
            d.Message = 'Finishing';
            
            ResetGUI(app);
            UpdateGUI(app);
            
            app.Options_UpperCheckBox.Value = 1;
            app.SaveRequired = 1;
            
            close(d)
            
        end

        % Button pushed function: ButtonConvertProject
        function Import_ButtonConvertProjectPushed(app, event)
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Waiting for data input from the Converter for EPMA';
            
            waitfor(Converter_EPMA(app));
            app.XMapTools_LastDir = cd;
            app.XMapTools_GUI.Name = [app.XMapTools_VER,' - ',char(app.XMapTools_LastDir)];
            
            close(app.WaitBar);
            
        end

        % Button pushed function: MakeMosaic
        function Import_ButtonMakeMosaicButtonPushed(app, event)
            % Select the directory containing the maps
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Select a directory containing the maps';
            
            %MosaicDirectory = [cd,'/Mosaic'];
            
            f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
            MosaicDirectory = uigetdir(cd, 'Mosaic Directory');
            close(f);
            figure(app.XMapTools_GUI);
            
            if isempty(MosaicDirectory) || isequal(MosaicDirectory,0)
                warndlg('You must select a folder containing one folder for each map of the mosaic','XMapTools');
                close(app.WaitBar);
                return
            end
            
            app.WaitBar.Message = 'Loading maps';
            
            cd(MosaicDirectory);
            
            DIR = dir(MosaicDirectory);
            DIR_Flags = [DIR.isdir];
            
            CountMapSet = 0;
            
            ElList = [];
            MapSizes = [];
            
            for i = 1:length(DIR_Flags)
                
                if DIR_Flags(i) && ~isequal(DIR(i).name,'.') && ~isequal(DIR(i).name,'..') && ~isequal(DIR(i).name(1),'-')
                    
                    CountMapSet = CountMapSet + 1;
                    
                    Directory4Map = [MosaicDirectory,'/',DIR(i).name];
                    
                    MAPFILES = dir([Directory4Map,'/*.txt']);
                    if isempty(MAPFILES)
                        MAPFILES = dir([Directory4Map,'/*.csv']);
                    end
                    NamesTemp = {MAPFILES.name};
                    
                    Index = zeros(size(NamesTemp));
                    
                    for j = 1:length(NamesTemp)
                        [Is,Where] = ismember(ElList,NamesTemp{j});
                        
                        if isempty(Is)
                            ElList{end+1} = NamesTemp{j};
                            [Is,Where] = ismember(ElList,NamesTemp{j});
                        end
                        if ~Is
                            ElList{end+1} = NamesTemp{j};
                            [Is,Where] = ismember(ElList,NamesTemp{j});
                        end
                        
                        
                        Where = find(Where);
                        
                        MapData = load([Directory4Map,'/',NamesTemp{j}],'-ASCII');
                        Map4Mosaic(CountMapSet).MapSize = size(MapData);
                        Map4Mosaic(CountMapSet).Maps(Where).Name = NamesTemp{j};
                        Map4Mosaic(CountMapSet).Maps(Where).Data = MapData;
                    end
                    MapSizes(CountMapSet,:) = Map4Mosaic(CountMapSet).MapSize;
                    
                end
            end
            
            app.WaitBar.Message = 'Set the number of columns in the mosaic';
            NbCol = inputdlg({'Number of columns'},'XMapTools',1,{'4'});
            
            if isempty(NbCol)
                close(app.WaitBar);
                return
            else
                NbCol = str2num(char(NbCol));
            end
            
            %app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Assembling the mosaic';
            
            X = [];
            Y = [];
            MapIdx = [];
            
            CountCol = 0;
            CountRow = 1;
            for i = 1:size(MapSizes,1)
                CountCol = CountCol+1;
                if CountCol > NbCol
                    CountCol = 1;
                    CountRow = CountRow +1;
                end
                X(CountRow,CountCol) = MapSizes(i,2);
                Y(CountRow,CountCol) = MapSizes(i,1);
                MapIdx(CountRow,CountCol) = i;
            end
            
            NbLinTest = size(MapIdx,1);
            NbColTest = size(MapIdx,2);
            
            Xgrid = max(X);
            if isequal(NbLinTest,1)   % Only one colomn
                Xgrid = X;
            end
            Ygrid = max(Y');
            if isequal(NbColTest,1)   % Only one colomn
                Ygrid = Y;
            end
            
            NewMapSize = [sum(Ygrid),sum(Xgrid)];
            
            NewMaps = zeros([NewMapSize,length(ElList)]);
            
            for i = 1:size(MapSizes,1)
                
                [Lin,Col] = find(MapIdx == i);
                
                Xmin = sum(Xgrid(1:Col-1)) + 1;
                Xmax = sum(Xgrid(1:Col)) ;
                
                Ymin = sum(Ygrid(1:Lin-1)) + 1;
                Ymax = sum(Ygrid(1:Lin)) ;
                
                dX = Xmax-Xmin+1;
                dY = Ymax-Ymin+1;
                
                MapX = MapSizes(i,2);
                MapY = MapSizes(i,1);
                
                ShiftX = floor((dX-(MapX-1))/2);
                ShiftY = floor((dY-(MapY-1))/2);
                
                PosCol(1) = Xmin+ShiftX;
                PosCol(2) = PosCol(1) + MapX - 1;
                PosLin(1) = Ymin+ShiftY;
                PosLin(2) = PosLin(1) + MapY - 1;
                
                for j = 1:size(NewMaps,3)
                    NewMaps(PosLin(1):PosLin(2),PosCol(1):PosCol(2),j) = Map4Mosaic(i).Maps(j).Data;
                end
            end
            
            app.WaitBar.Message = 'Pick a directory to save the maps';
            
            f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
            Where2Save = uigetdir(cd, ['Pick a Directory']);
            delete(f)
            figure(app.XMapTools_GUI);
            
            if isempty(Where2Save) || isequal(Where2Save,0)
                warndlg('You must pick a directory to save the maps','XMapTools');
                close(app.WaitBar);
                return
            end
            
            %close(app.WaitBar);
            
            %app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Saving files';
            
            for i = 1:length(ElList)
                Map4Save = NewMaps(:,:,i);
                save([Where2Save,'/',ElList{i}],'Map4Save','-ASCII');
            end
            
            cd(Where2Save);
            app.XMapTools_LastDir = cd;
            app.XMapTools_GUI.Name = [app.XMapTools_VER,' - ',char(app.XMapTools_LastDir)];
            figure(app.XMapTools_GUI);
            
            close(app.WaitBar);
        end

        % Button pushed function: MakeMosaicStd
        function Import_MakeMosaicStdButtonPushed(app, event)
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Searching for the subfolder Mosaic';
            
            MosaicDirectory = [cd,'/Mosaic'];
            
            if ~isequal(exist(MosaicDirectory),7)
                
                app.WaitBar.Message = 'Select a directory containing the maps';
                
                f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
                MosaicDirectory = uigetdir(cd, ['Mosaic Directory']);
                delete(f)
                figure(app.XMapTools_GUI);
                
                if isempty(MosaicDirectory) || isequal(MosaicDirectory,0)
                    warndlg('You must select a folder containing one folder for each map of the mosaic','XMapTools');
                    close(app.WaitBar);
                    return
                end
                
                cd(MosaicDirectory)
                cd ..
                
            end
            
            DIR = dir(MosaicDirectory);
            DIR_Flags = [DIR.isdir];
            
            CountMapSet = 0;
            
            ElList = [];
            MapSizes = [];
            
            app.WaitBar.Message = 'Importing the maps and building the mosaic';
            
            for i = 1:length(DIR_Flags)
                if DIR_Flags(i) && ~isequal(DIR(i).name,'.') && ~isequal(DIR(i).name,'..') && ~isequal(DIR(i).name(1),'-')
                    
                    CountMapSet = CountMapSet + 1;
                    
                    Directory4Map = [MosaicDirectory,'/',DIR(i).name];
                    
                    MAPFILES = dir([Directory4Map,'/*.txt']);
                    if isempty(MAPFILES)
                        MAPFILES = dir([Directory4Map,'/*.csv']);
                    end
                    NamesTemp = {MAPFILES.name};
                    
                    Index = zeros(size(NamesTemp));
                    
                    for j = 1:length(NamesTemp)
                        
                        switch NamesTemp{j}
                            case 'Classification.txt'
                                
                                % in this version we just skip it
                                
                            case 'Standards.txt'
                                
                                % We need to read the map coordinates
                                fid = fopen([Directory4Map,'/Standards.txt'],'r');
                                
                                while 1
                                    TheLine = fgetl(fid);
                                    
                                    if isequal(TheLine,-1)
                                        break
                                    end
                                    
                                    if length(TheLine) >= 2
                                        if isequal(TheLine(1:2),'>1')
                                            TheLine = fgetl(fid);
                                            MapCoordinates(CountMapSet,:) = strread(TheLine,'%f');
                                            break
                                        end
                                    end
                                    
                                end
                                fclose(fid);
                                
                            otherwise
                                [Is,Where] = ismember(ElList,NamesTemp{j});
                                
                                if isempty(Is)
                                    ElList{end+1} = NamesTemp{j};
                                    [Is,Where] = ismember(ElList,NamesTemp{j});
                                end
                                if ~Is
                                    ElList{end+1} = NamesTemp{j};
                                    [Is,Where] = ismember(ElList,NamesTemp{j});
                                end
                                
                                
                                Where = find(Where);
                                
                                MapData = load([Directory4Map,'/',NamesTemp{j}],'-ASCII');
                                Map4Mosaic(CountMapSet).MapSize = size(MapData);
                                Map4Mosaic(CountMapSet).Maps(Where).Name = NamesTemp{j};
                                Map4Mosaic(CountMapSet).Maps(Where).Data = MapData;
                        end
                    end
                    MapSizes(CountMapSet,:) = Map4Mosaic(CountMapSet).MapSize;
                    
                end
            end
            
            SizeX = MapCoordinates(:,2)-MapCoordinates(:,1);
            SizeY = MapCoordinates(:,3)-MapCoordinates(:,4);
            
            StepX = SizeX./MapSizes(:,2);
            StepY = SizeY./MapSizes(:,1);
            
            NewRes = min([StepX;StepY]);
            
            MinX = min(MapCoordinates(:,1));
            MaxX = max(MapCoordinates(:,2));
            MinY = min(MapCoordinates(:,4));
            MaxY = max(MapCoordinates(:,3));
            
            Xsteps = [MinX:NewRes:MaxX];
            Ysteps = [MinY:NewRes:MaxY];
            
            Xmtx = repmat(Xsteps,length(Ysteps),1);
            Ymtx = repmat(Ysteps',1,length(Xsteps));
            
            NewMapSize = [length(Ysteps),length(Xsteps)];
            MapIndex = reshape([1:NewMapSize(1)*NewMapSize(2)],NewMapSize(2),NewMapSize(1));
            
            NewMaps = zeros([NewMapSize,length(ElList)]);
            
            for i = 1:size(MapSizes,1)
                
                MapRes = StepX(i);
                
                if isequal(MapRes,NewRes)
                    TheMapSizes = MapSizes(i,:);
                else
                    TheMapSizes = round(MapSizes(i,:).*(MapRes/NewRes));
                end
                
                % The Diff will always be smaller than the pixel size, which means that
                % the projection is good ? less than the pixel size!
                
                [DiffY,WhereY] = min(abs(Ymtx(:)-MapCoordinates(i,4)));
                ValY = Ymtx(WhereY);
                [DiffX,WhereX] = min(abs(Xmtx(:)-MapCoordinates(i,1)));
                ValX = Xmtx(WhereX);
                
                Therow = find(ismember(Ysteps,ValY));
                Thecol = find(ismember(Xsteps,ValX));
                
                Val1 = Therow;
                Val2 = Therow+TheMapSizes(1)-1;
                Val3 = Thecol;
                Val4 = Thecol+TheMapSizes(2)-1;
                
                for j = 1:size(NewMaps,3)
                    if isequal(MapRes,NewRes)
                        NewMaps(Val1:Val2,Val3:Val4,j) = Map4Mosaic(i).Maps(j).Data;
                    else
                        ResampledMap = ResampleMapFct(app,Map4Mosaic(i).Maps(j).Data,MapRes,NewRes);
                        
                        NewMaps(Val1:Val1+size(ResampledMap,1)-1,Val3:Val3+size(ResampledMap,2)-1,j) = ResampledMap;
                    end
                end
                
            end
            
            app.WaitBar.Message = 'Saving the new maps';
            
            fid = fopen('Standards.txt','w');
            fprintf(fid,'\n%s\n%f\t%f\t%f\t%f\n\n','>1',Xsteps(1),Xsteps(end),Ysteps(end),Ysteps(1));
            fclose(fid);
            
            for i = 1:length(ElList)
                Map4Save = NewMaps(:,:,i);
                save([cd,'/',ElList{i}],'Map4Save','-ASCII');
            end
            
            app.XMapTools_LastDir = cd;
            app.XMapTools_GUI.Name = [app.XMapTools_VER,' - ',char(app.XMapTools_LastDir)];
            
            close(app.WaitBar);
        end

        % Button pushed function: ImportStandards
        function Standards_ImportStandardsButtonPushed(app, event)
            
            app.TreeData_Additional.SelectedNodes = [];
            
            app.XMapToolsData.Standards = [];
            
            if isempty(app.TreeData_Main.SelectedNodes)
                %errordlg({'Standard data cannot be imported! Check the error message below','   ','Select and display an intensity map to import standard data'}, 'XMapTools');
                uialert(gcbf,{'Standard data cannot be imported! Check the error message below','   ','Select and display an intensity map to import standard data'},'XMapTools – Error');
                return
            else
                NodeData = app.TreeData_Main.SelectedNodes.NodeData;
                
                if ~isequal(NodeData(1),1) || isequal(NodeData(2),0)
                    %errordlg({'Standard data cannot be imported! Check the error message below','   ','Select and display an intensity map to import standard data'}, 'XMapTools');
                    uialert(gcbf,{'Standard data cannot be imported! Check the error message below','   ','Select and display an intensity map to import standard data'},'XMapTools – Error');
                    return
                end
            end
            
            [Valid,ErrorMessage] = CheckMapSizeConsistencyIntensity(app);
            
            if ~Valid
                uialert(gcbf,{'Standard data cannot be imported! Check the error message below','   ',ErrorMessage},'XMapTools – Error');
                %errordlg({'Standard data cannot be imported! Check the error message below','   ',ErrorMessage}, 'XMapTools');
                return
            end
            
            SelectFile = 1;
            Selection  = 0;
            
            if isequal(app.ImportAutoMode.Value,1)
                WhereWheAre = cd;
                PathName = [WhereWheAre,'/'];
                FileName = 'Standards.txt';
                
                if isequal(exist([PathName,FileName],'file'),2)
                    SelectFile = 0;
                    Selection = 1;
                end
            end
            
            if SelectFile
                f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
                [FileName,PathName] = uigetfile({'*.txt;*.asc;*.dat;','Maps Files (*.txt, *.asc, *.dat)';'*.*',  'All Files (*.*)'},'Pick a file');
                close(f);
                figure(app.XMapTools_GUI)
                if ~isempty(FileName)
                    Selection = 1;
                end
            end
            
            if ~Selection
                return
            end
            
            % (1) Read and Import Standard File (using XMapTools 3 format)
            
            Compt = 0;
            
            DefMap = [];
            ListPrElements = [];
            DefProfils = [];
            
            % Load
            fid = fopen(strcat(PathName,FileName));
            while 1
                tline = fgetl(fid);
                if isequal(tline,-1)
                    break
                end
                if length(tline) > 1
                    if isequal(tline(1),'>')
                        leCase = str2double(tline(2));
                        switch leCase
                            
                            case 1 % map coordinates
                                tline = fgetl(fid);
                                Read = textscan(tline,'%f');
                                DefMap = Read{1}';
                                
                            case 2 % elements order
                                tline = fgetl(fid);
                                Read = textscan(tline,'%s');
                                ListPrElements = Read{1};
                                
                            case 3 % Point mode analyses
                                while 1
                                    tline = fgetl(fid);
                                    if length(tline) < 3
                                        break
                                    end
                                    Compt = Compt+1;
                                    % changed version 2.6.1: added %f to only read
                                    % numbers and check the size of the table
                                    Read = textscan(tline,'%f');
                                    TheRowOk = Read{1}';
                                    
                                    if Compt > 1
                                        if ~isequal(length(TheRowOk),size(DefProfils,2))
                                            uialert(gcbf,{'Error while reading the third block of the Standard file (>3): ', ...
                                                'The number of columns is not constant throughout the table', ...
                                                ['Error tracking: check analysis #',num2str(Compt),' (see below)'], ...
                                                tline},'XMapTools – Error');
                                            %                                             f = errordlg({'Error while reading the third block of the Standard file (>3): ', ...
                                            %                                                 'The number of columns is not constant throughout the table', ...
                                            %                                                 ['Error tracking: check analysis #',num2str(Compt),' (see below)'], ...
                                            %                                                 tline}, 'XMapTools');
                                            
                                            return
                                        end
                                    end
                                    
                                    DefProfils(Compt,:) = TheRowOk;
                                end
                        end
                    end
                end
            end
            fclose(fid);
            
            % (2) Check for errors in the file and print
            
            ErrorText{1} = ['Cancellation, it was not possible to import the file ',FileName];
            ErrorText{2} = ['  '];
            ErrorText{3} = ['XMapTools encountered the following issue(s):'];
            ErrorDisp = 0;
            
            if isequal(length(DefMap),0)
                ErrorDisp = 1;
                ErrorText{end+1} = '   -   [>1] Missing or empty map coordinates block';
            end
            
            if ~isequal(length(DefMap),4)
                ErrorDisp = 1;
                ErrorText{end+1} = '   -   [>1] Four map coordinates are required (Xmin, Xmax, Ymin, Ymax)';
            end
            
            if isequal(length(ListPrElements),0)
                ErrorDisp = 1;
                ErrorText{end+1} = '   -   [>2] Missing or empty oxide order block';
            else
                if ~isequal(ListPrElements{end-1},'X') || ~isequal(ListPrElements{end},'Y')
                    ErrorDisp = 1;
                    ErrorText{end+1} = '   -   [>2] Issue with X and Y coordinates (note X and Y must be the two last labels and must be listed in this order)';
                end
            end
            
            if isequal(size(DefProfils,2),0)
                ErrorDisp = 1;
                ErrorText{end+1} = '   -   [>3] Missing or empty spot analyses block';
            else
                if ~isequal(size(DefProfils,2),length(ListPrElements))
                    ErrorDisp = 1;
                    ErrorText{end+1} = '   -   [>2] and [>3] have different sizes';
                end
            end
            
            if ErrorDisp
                ErrorText{end+1} = ' ';
                warndlg(ErrorText,'Error Import Function')
                return
            end
            
            % NEW CHECK DEFMAP and update if map was cropped
            if isequal(app.XMapToolsData.MapSizeCheck.SizeChanged,1)
                OriginalSize = app.XMapToolsData.MapSizeCheck.OriginalSize;
                ActualSize = app.XMapToolsData.MapSizeCheck.ActualSize;
                
                Shift_X = app.XMapToolsData.MapSizeCheck.Shift_X;
                Shift_Y = app.XMapToolsData.MapSizeCheck.Shift_Y;
                
                Xmin = DefMap(1);
                Xmax = DefMap(2);
                Ymax = DefMap(3);
                Ymin = DefMap(4);
                
                dXt = Xmax-Xmin;
                dYt = Ymax-Ymin;
                
                dX = dXt/OriginalSize(2);
                dY = dYt/OriginalSize(1);
                
                XminC = Xmin+dX*(Shift_X(1)-1);
                XmaxC = XminC+dX*ActualSize(2);
                YminC = Ymin+dY*(Shift_Y(1)-1);
                YmaxC = YminC+dY*ActualSize(1);
                
                DefMap = [XminC XmaxC YmaxC YminC];
            end
            
            % (3) Matching with database:
            
            ElemImport = ListPrElements(1:end-2);    % we filter X and Y
            RefEl = zeros(size(ElemImport));
            RefOx = zeros(size(ElemImport));
            for i = 1:length(ElemImport)
                WhereOx = find(ismember(app.ElOxDataDef.OxList,ElemImport{i}));
                if isempty(WhereOx)
                    WhereEl = find(ismember(app.ElOxDataDef.ElList,ElemImport{i}));
                    if isempty(WhereEl)
                        warndlg({['Cancellation, it was not possible to import the file ',FileName], ['   '] ...
                            ['Element: ',char(ElemImport{i}),' not recognized by XMapTools'],},'Error Import Function');
                        return
                    else
                        RefEl(i) = WhereEl;
                    end
                else
                    RefOx(i) = WhereOx;
                end
                %RefElemPro(i) = Where;
            end
            
            % At the stage the element is in the database and the index of
            % the corresponding map is saved (RefElemPro)
            
            % (4) X,Y coordinates of the standards and selection (old)
            
            Coord = DefProfils(:,end-1:end);
            
            
            % (5) Map coordinates and pixel size
            
            Xleft = DefMap(1); Xright = DefMap(2); Ybot = DefMap(3); Ytop = DefMap(4);
            SizeMap = size(app.XMapToolsData.MapData.It.Data(1).Map);
            
            SizeXPixel = abs((Xleft - Xright) / (SizeMap(2) -1 )); % x --> col
            SizeYPixel = abs((Ytop - Ybot) / (SizeMap(1) -1 )); % x --> line
            
            % based on X pixel size
            if SizeXPixel < 0.1
                % unit is probbaly mm
                app.ResolutionField.Value = SizeXPixel*1e3;
            else
                % unit is probably µm
                app.ResolutionField.Value = SizeXPixel;
            end
            
            
            if Xleft < Xright
                XCoo = Xleft : SizeXPixel : Xright;
            else
                XCoo = Xleft : -SizeXPixel : Xright;
            end
            if Ytop > Ybot
                YCoo = Ytop : -SizeYPixel : Ybot;
            else
                YCoo = Ytop : SizeYPixel : Ybot;
            end
            
            IdxAllPr = NaN * zeros(length(DefProfils(:,1)), 2);
            
            Labels = cell(length(DefProfils(:,1)),1);
            for i = 1 : length(DefProfils(:,1))
                [V(i,1), IdxAllPr(i,1)] = min(abs(XCoo-Coord(i,1))); % X
                [V(i,2), IdxAllPr(i,2)] = min(abs(YCoo-Coord(i,2))); % Y
                Labels{i} = ['Spot_',num2str(i)];
            end
            
            % Selection of the standards points inside the map
            WithinMapIdx = find(V(:,1) < SizeXPixel & V(:,2) < SizeYPixel);
            
            app.XMapToolsData.Standards.Coord = Coord(WithinMapIdx,:);
            app.XMapToolsData.Standards.Types = ones(length(WithinMapIdx),1);
            app.XMapToolsData.Standards.Labels = Labels;
            app.XMapToolsData.Standards.Selected = ones(size(app.XMapToolsData.Standards.Coord,1),1);
            app.XMapToolsData.Standards.XCoo = XCoo(WithinMapIdx);
            app.XMapToolsData.Standards.YCoo = YCoo(WithinMapIdx);
            app.XMapToolsData.Standards.XY = IdxAllPr(WithinMapIdx,:);
            app.XMapToolsData.Standards.ElemImport = ElemImport;
            app.XMapToolsData.Standards.RefEl = RefEl;
            app.XMapToolsData.Standards.RefOx = RefOx;
            app.XMapToolsData.Standards.DataPro = DefProfils(WithinMapIdx,1:end-2);
            
            Standards_UpdateIntensities(app);
            
            ResetGUI(app);
            UpdateGUI(app);
            
            % Select the node
            SelectNodeFromNodeData(app,app.SelectedNodes_MainTree)
            
            app.TreeData_Additional.SelectedNodes = app.Node_Standards;
            app.SaveRequired = 1;
            
            TreeData_AdditionalSelectionChanged(app);
            
        end

        % Button pushed function: AddStandardsManual
        function Standards_AddStandardsManualButtonPushed(app, event)
            
            
            switch app.ColorScaleBar.Value
                case 'White'
                    ColorCode = 'w';
                case 'Black'
                    ColorCode = 'k';
            end
            
            app.TabGroup.SelectedTab = app.StandardsTab;
            app.SubTabStandard.SelectedTab = app.StdDataTab;
            
            Pos = size(app.XMapToolsData.Standards.XY,1) + 1;
            StdLabel = ['ManualSpot_',num2str(Pos)];
            
            DrawingMode(app,'on','Spot')
            app.ROI_std(Pos).ROI = drawpoint(app.FigMain,'InteractionsAllowed','all','Color','b','Label',StdLabel,'LabelAlpha',0,'LabelTextColor',ColorCode);
            DrawingMode(app,'off')
            
            app.ROIobjectListener_StdSpots = addlistener(app.ROI_std(Pos).ROI, 'ROIMoved', @(varargin)Standards_MovingStdROI_Main(app, app.ROI_std(Pos).ROI));
            
            % Update
            app.XMapToolsData.Standards.Coord(Pos,:) = [0,0];
            app.XMapToolsData.Standards.Types(Pos) = 2;         % manual spot
            app.XMapToolsData.Standards.Labels{Pos} = StdLabel;
            app.XMapToolsData.Standards.Selected(Pos) = 1;      % unused
            app.XMapToolsData.Standards.XCoo(Pos) = 0;
            app.XMapToolsData.Standards.YCoo(Pos) = 0;
            app.XMapToolsData.Standards.XY(Pos,:) = round(app.ROI_std(Pos).ROI.Position);
            
            app.XMapToolsData.Standards.DataPro(Pos,:) = zeros(size(app.XMapToolsData.Standards.DataPro(Pos-1,:)));
            
            Standards_UpdateIntensities(app);
            Standards_CalculateMapPosition(app);    % thought no chemistry is available yet
            
            % add node in Tree
            p = uitreenode(app.Node_Standards,'Text',StdLabel,'NodeData',[14,Pos]);
            
            app.TreeData_Additional.SelectedNodes = app.Node_Standards.Children(end);
            
            app.SaveRequired = 1;
            TreeData_AdditionalSelectionChanged(app);
            
        end

        % Button pushed function: StdAll_Synchronize
        function Stdandards_SynchronizeButtonPushed(app, event)
            
            ROI_DeleteROI(app);
            
            Standard_ApplyXYShift(app,app.Std_Shift_X.Value,app.Std_Shift_Y.Value);
            
            app.Std_Shift_X.Value = 0;
            app.Std_Shift_Y.Value = 0;
            app.StdAll_Synchronize.Enable = 'off';
            
            app.SaveRequired = 1;
            
        end

        % Button pushed function: ButtonCalibrate
        function Standardization_ButtonCalibratePushed(app, event)
            
            
            
            if ~isempty(app.TreeData_Additional.SelectedNodes)
                NodeData = app.TreeData_Additional.SelectedNodes.NodeData;
                
                if isequal(NodeData(1), 11) && NodeData(2) > 0 && isequal(NodeData(3),0)
                    app.ButtonCalibrate.Enable = 'off';
                    app.ButtonCalibrate.Tooltip = 'Import standards and select a maskfile to activate the calibration tools';
                    
                    app.Calibrate_GenerateDensity.Enable = 'off';
                    
                    app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
                    app.WaitBar.Message = 'XMapTools is running numbers (calibration)';
                    
                    waitfor(Calibration_EPMA(app,NodeData(2)));
                    
                    app.WaitBar.Message = 'Processing, please wait...';
                    
                    ResetGUI(app);
                    UpdateGUI(app);
                    
                    close(app.WaitBar);
                    
                else
                    uialert(gcbf,{'XMapTools cannot proceed to the standardization...','   ','Select a mask file in the tree menu'},'XMapTools – Error');
                    %errordlg({'XMapTools cannot proceed to the standardization...','   ','Select a mask file in the tree menu'}, 'XMapTools');
                end
            else
                uialert(gcbf,{'XMapTools cannot proceed to the standardization...','   ','Select a mask file in the tree menu'},'XMapTools – Error');
                %errordlg({'XMapTools cannot proceed to the standardization...','   ','Select a mask file in the tree menu'}, 'XMapTools');
                return
            end
            
            app.SaveRequired = 1;
            
            %keyboard
            
            
            close(app.WaitBar);
            
            
            %keyboard
            
            
            
            
            
        end

        % Button pushed function: Calibrate_GenerateDensity
        function Calibrate_GenerateDensityButtonPushed(app, event)
            
            NodeData = app.TreeData_Additional.SelectedNodes.NodeData;
            if size(NodeData,1) > 1
                return
            end
            
            SelectedMaskFile = NodeData(2);
            MineralNames = app.XMapToolsData.MapData.MaskFile.Masks(SelectedMaskFile).Names(2:end);
            ListMinDB = app.DensityData.Names;
            
            app.XMapToolsData.MapData.MaskFile.Masks(SelectedMaskFile).Densities = zeros(size(MineralNames));
            
            for i = 1:length(MineralNames)
                [best,dist]=strnearest(MineralNames{i},ListMinDB);
                if length(best) > 1
                    best  = best(1);
                end
                if isequal(dist,0)
                    app.XMapToolsData.MapData.MaskFile.Masks(SelectedMaskFile).Densities(i) = app.DensityData.Density(best);
                end
            end
            
            for i = 1:length(app.XMapToolsData.MapData.MaskFile.Masks(SelectedMaskFile).Densities)
                input_density{i} = num2str(app.XMapToolsData.MapData.MaskFile.Masks(SelectedMaskFile).Densities(i));
            end
            
            name='Input for Phase Densities';
            numlines=1;
            
            answer=inputdlg(MineralNames,name,numlines,input_density);
            
            if isempty(answer)
                return
            end
            
            
            for i = 1:length(answer)
                Densities(i) = str2num(answer{i});
            end
            
            MaskMap = app.XMapToolsData.MapData.MaskFile.Masks(SelectedMaskFile).MaskMap;
            
            DensityMap = zeros(size(MaskMap));
            for i = 1:length(Densities)
                Where = find(MaskMap == i);
                if ~isempty(Where)
                    DensityMap(Where) = Densities(i)*ones(size(Where));
                end
            end
            
            % Add the density map
            Pos = length(app.XMapToolsData.MapData.Ot.Names)+1;
            
            app.XMapToolsData.MapData.Ot.Names{Pos} = ['Density [',char(app.XMapToolsData.MapData.MaskFile.Names{SelectedMaskFile}),']'];
            app.XMapToolsData.MapData.Ot.Types(Pos) = 10;
            app.XMapToolsData.MapData.Ot.Data(Pos).Map = DensityMap;
            
            % Add MapData Ot
            p = uitreenode(app.Node_Ot,'Text',char(app.XMapToolsData.MapData.Ot.Names{Pos}),'NodeData',[5,Pos]);
            
            expand(app.Node_Ot);
            app.TreeData_Main.SelectedNodes = p;
            
            app.SaveRequired = 1;
            
            pause(0.1)
            figure(app.XMapTools_GUI);
            
        end

        % Value changed function: Calibrate_MultiROICheckBox
        function Calibrate_MultiROICheckBoxValueChanged(app, event)
            if isequal(app.Calibrate_MultiROICheckBox.Value,0)
                app.ROI_LBC = [];
                app.ROI_LBC_Listener = [];
                ROI_DeleteROI(app);
            end
        end

        % Button pushed function: Calibrate_AddROIforLBC
        function Calibrate_AddROIforLBCButtonPushed(app, event)
            
            Types = app.XMapToolsData.MapData.Ot.Types;
            WhereDensity = find(Types == 10);
            
            if isempty(WhereDensity)
                uialert(gcbf,{'XMapTools cannot open the Composition Viewer...','   ','No density map available!'},'XMapTools – Error');
                %errordlg({'XMapTools cannot open the Composition Viewer...','   ','No density map available!'}, 'XMapTools');
                return
            end
            
            % Check for densityMaps
            Types = app.XMapToolsData.MapData.Ot.Types;
            WhereDens = find(Types == 10);
            Names = app.XMapToolsData.MapData.Ot.Names(WhereDens);
            
            app.CompViewer_DensityMenu.Items = Names;
            app.CompViewer_DensityMenu.ItemsData = [1:length(Names)];
            app.CompViewer_DensityMenu.Value = 1;
            app.CompViewer_DensityMenu.UserData = WhereDens;
            
            app.CompViewer_Label.Text = 'Local bulk composition';
            
            if isequal(app.Calibrate_MultiROICheckBox,0)
                ROI_DeleteROI(app);
                app.ROI_LBC = [];
                app.ROI_LBC_Listener = [];
                iROI = 1;
            else
                iROI = length(app.ROI_LBC) + 1;
            end
            
            switch app.Calibrate_ROI_menu.Value
                case 'Rectangle ROI'
                    DrawingMode(app,'on','Rectangle');
                    app.ROI_LBC(iROI).ROI = drawrectangle(app.FigMain,'Color',[0.57,0.00,0.69],'InteractionsAllowed','all');
                    DrawingMode(app,'off');
                    
                case 'Polygon ROI'
                    DrawingMode(app,'on','Polygon');
                    app.ROI_LBC(iROI).ROI = drawpolygon(app.FigMain,'Color',[0.57,0.00,0.69],'InteractionsAllowed','all');
                    DrawingMode(app,'off');
            end
            
            app.ROI_LBC_Listener = addlistener(app.ROI_LBC(iROI).ROI, 'ROIMoved', @(varargin)LBC_ROI_changed_shape(app, app.ROI_LBC));
            
            % Extract data
            LBC_ROI_changed_shape(app, app.ROI_LBC);
            
            app.TabGroup.SelectedTab = app.CompositionTab;
            
        end

        % Button pushed function: Calibrate_Spider_Button
        function Calibrate_Spider_ButtonPushed(app, event)
            Menu_Modules_SpiderPlotMenuSelected(app, event);
        end

        % Button pushed function: Calibrate_Merge
        function Calibrate_MergeButtonPushed(app, event)
            
            NameMerged = 'Merged_Manual';
            
            Pos = length(app.XMapToolsData.MapData.Me.Names) + 1;
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Select phase(s) and press OK to continue...';
            waitfor(Selector(app,app.XMapToolsData.MapData.Qt.Names,'Select phases in the list below','Multiple'));
            close(app.WaitBar)
            
            % s = find(ismember(app.XMapToolsData.MapData.Qt.Names,app.ExchangeSelector));
            s = app.ExchangeSelectorId;                                     % changed 4.4
            
            %[s,v] = listdlg('PromptString','Select maps:','SelectionMode','multiple','ListString',app.XMapToolsData.MapData.Qt.Names);
            
            if ~isempty(s)
                
                ElNames = app.XMapToolsData.MapData.Qt.Data(s(1)).ElNames;
                ElInd = app.XMapToolsData.MapData.Qt.Data(s(1)).ElInd;
                
                app.XMapToolsData.MapData.Me.Names{Pos} = NameMerged;
                app.XMapToolsData.MapData.Me.IsOxide(Pos) = 1;
                app.XMapToolsData.MapData.Me.MaskFile{Pos} = 'Unknown';
                app.XMapToolsData.MapData.Me.NbCalibPoints(Pos) = 0;
                
                app.XMapToolsData.MapData.Me.Data(Pos).ElNames = ElNames;
                app.XMapToolsData.MapData.Me.Data(Pos).ElInd = ElInd;
                
                for i = 1:length(ElNames)
                    
                    MergedMap = zeros(size(app.XMapToolsData.MapData.Qt.Data(s(1)).CData(1).Map));
                    for j = 1:length(s)
                        MapTemp = app.XMapToolsData.MapData.Qt.Data(s(j)).CData(i).Map;
                        WhereNan = find(isnan(MapTemp));
                        MapTemp(WhereNan) = zeros(size(WhereNan));
                        MergedMap = MergedMap + MapTemp; %app.XMapToolsData.MapData.Qt.Data(s(j)).CData(i).Map;
                    end
                    
                    app.XMapToolsData.MapData.Me.Data(Pos).CData(i).Map = MergedMap;
                end
                
                p = uitreenode(app.Node_Me,'Text',char(NameMerged),'NodeData',[3,Pos,0]);
                for i = 1:length(ElNames)
                    p1 = uitreenode(app.Node_Me.Children(Pos),'Text',char(ElNames{i}),'NodeData',[3,Pos,i]);
                end
                
                expand(app.Node_Me);
                app.TreeData_Main.SelectedNodes = p;
                
            end
            
            app.SaveRequired = 1;
            
            pause(0.1)
            figure(app.XMapTools_GUI)
            
            
        end

        % Button pushed function: ImportStandards_LAICPMS
        function Calibrate_ImportStandards_LAICPMSButtonPushed(app, event)
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Pick a standard file (MapStandards_Import.mat)';
            
            % Reset all standard data for this project
            app.XMapToolsData.MapStandards = InitializeXMapToolsData_MapStandards(app);
            
            f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
            [FileName,PathName] = uigetfile({'*.mat','Maps of Standards (*.mat)';'*.*',  'All Files (*.*)'},'Pick a file');
            close(f);
            figure(app.XMapTools_GUI)
            if isempty(FileName)
                close(app.WaitBar);
                return
            end
            
            app.WaitBar.Message = 'Loading the standard data';
            
            
            load([PathName,FileName]);
            
            if ~exist('MapStandards_Import','var')
                uialert(gcbf,'This is not a valid files (standart maps for LA-ICP-MS)','XMapTools – Error');
                %errordlg('This is not a valid files (standart maps for LA-ICP-MS)','XMapTools')
                close(app.WaitBar);
                return
            end
            
            % BABA
            if ~isfield(MapStandards_Import.StdMaps,'Int_Back')
                uialert(gcbf,'Maps and this standard file have been generated with a beta version of XMapTools and are no longer compatible. A new conversion of the LA-ICP-MS data from scratch is required!','XMapTools – Error');
                %errordlg('Maps and this standard file have been generated with a beta version of XMapTools and are no longer compatible. A new conversion of the LA-ICP-MS data from scratch is required!','XMapTools')
                close(app.WaitBar);
                return
            end
            
            % TEST new version
            if ~isfield(MapStandards_Import.StdMaps,'Std_Map')
                uialert(gcbf,'Maps and this standard file have been generated with a beta version of XMapTools and are no longer compatible. A new conversion of the LA-ICP-MS data from scratch is required!','XMapTools – Error');
                %errordlg('Maps and this standard file have been generated with a beta version of XMapTools and are no longer compatible. A new conversion of the LA-ICP-MS data from scratch is required!','XMapTools')
                
                % We can also easily create new fields:
                
                % Std_Map(1).Cps = Std_Map_Cps
                close(app.WaitBar);
                return
            end
            
            app.WaitBar.Message = 'Reading standard file';
            
            NbStandardOptions = length(MapStandards_Import.StdMaps(1).Std_Map);
            
            for iStd = 1:NbStandardOptions
                
                NbMaps = length(MapStandards_Import.ElName);
                
                app.XMapToolsData.MapStandards(iStd).StandardName = MapStandards_Import.StdMaps(1).Std_Map(iStd).StdName;
                % Data types:
                %       - Type 1        standard maps (Std_Map_Cps)
                %       - Type 2        composition of the standard (Std_Map_Conc)
                %       - Type 3        intensity of background (Int_Back)
                %       - Type 4        number of sweeps for background measurement (Sweeps_Back)
                %       - Type 5        number of sweeps for pixel measurement (Sweeps_Pixel)
                
                for i = 1:NbMaps
                    app.XMapToolsData.MapStandards(iStd).Names{i} = MapStandards_Import.ElName{i};
                    app.XMapToolsData.MapStandards(iStd).Types(i) = 1;
                    app.XMapToolsData.MapStandards(iStd).Data(i).Map = MapStandards_Import.StdMaps(i).Std_Map(iStd).Cps;
                end
                
                for i = 1:NbMaps
                    app.XMapToolsData.MapStandards(iStd).Names{NbMaps+i} = [MapStandards_Import.ElName{i},'_Int_Back'];
                    app.XMapToolsData.MapStandards(iStd).Types(NbMaps+i) = 3;
                    app.XMapToolsData.MapStandards(iStd).Data(NbMaps+i).Map = MapStandards_Import.StdMaps(i).Int_Back;
                end
                
                for i = 1:NbMaps
                    app.XMapToolsData.MapStandards(iStd).Names{2*NbMaps+i} = [MapStandards_Import.ElName{i},'_Comp'];
                    app.XMapToolsData.MapStandards(iStd).Types(2*NbMaps+i) = 2;
                    app.XMapToolsData.MapStandards(iStd).Data(2*NbMaps+i).Map = MapStandards_Import.StdMaps(i).Std_Map(iStd).Conc;
                end
                
                for i = 1:NbMaps
                    app.XMapToolsData.MapStandards(iStd).Names{3*NbMaps+i} = [MapStandards_Import.ElName{i},'_Sweeps_Back'];
                    app.XMapToolsData.MapStandards(iStd).Types(3*NbMaps+i) = 4;
                    app.XMapToolsData.MapStandards(iStd).Data(3*NbMaps+i).Map = MapStandards_Import.StdMaps(i).Sweeps_Back;
                end
                
                for i = 1:NbMaps
                    app.XMapToolsData.MapStandards(iStd).Names{4*NbMaps+i} = [MapStandards_Import.ElName{i},'_Sweeps_Pixel'];
                    app.XMapToolsData.MapStandards(iStd).Types(4*NbMaps+i) = 5;
                    app.XMapToolsData.MapStandards(iStd).Data(4*NbMaps+i).Map = MapStandards_Import.StdMaps(i).Sweeps_Pixel;
                end
                
            end
            
            % Add PxDataRaw (4.4)
            % if isfield(MapStandards_Import,'PxDataRaw')
            %    app.XMapToolsData.PxDataRaw = MapStandards_Import.PxDataRaw;
            % end
            
            app.WaitBar.Message = 'Updating the interface (may take a while)';
            
            ResetGUI(app);
            UpdateGUI(app);
            
            close(app.WaitBar);
            
            if ~isempty(app.XMapToolsData.MapStandards(1).Names)
                expand(app.Node_MapStandards);
                app.TreeData_Additional.SelectedNodes = app.Node_MapStandards.Children(1);
                app.SaveRequired = 1;
                TreeData_AdditionalSelectionChanged(app);
            end
            
        end

        % Button pushed function: ButtonCalibrate_LAICPMS
        function Calibrate_ButtonCalibrate_LAICPMSPushed(app, event)
            
            NodeData = app.TreeData_Additional.SelectedNodes.NodeData;
            SelectedMaskFile = NodeData(2);
            
            NbQuantiBefore = length(app.XMapToolsData.MapData.Qt.Names);
            
            %Tools_CalibrationModule_LAIPCMS(app,SelectedMaskFile)
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Waiting for Data from the Calibration Module (close the module to resume XMapTools) ...';
            
            waitfor(Calibration_LAICPMS(app,SelectedMaskFile));
            
            app.WaitBar.Message = 'Processing, please wait...';
            
            NbQuantiAfter = length(app.XMapToolsData.MapData.Qt.Names);
            
            if NbQuantiAfter > NbQuantiBefore
                %ResetGUI(app);
                %UpdateGUI(app);
                
                collapse(app.TreeData_Main)
                expand(app.Node_Qt.Children(end))
                app.TreeData_Main.SelectedNodes = app.Node_Qt.Children(end).Children(1);
                TreeData_MainSelectionChanged(app);
                
                app.SaveRequired = 1;
            end
            
            close(app.WaitBar);
            
        end

        % Button pushed function: LBC_UncCalc
        function LBC_UncCalcButtonPushed(app, event)
            WhereDensity = app.CompViewer_DensityMenu.UserData(app.CompViewer_DensityMenu.Value);
            DensityMap = app.XMapToolsData.MapData.Ot.Data(WhereDensity).Map;
            
            NodeData = app.TreeData_Main.SelectedNodes.NodeData;
            Idx = NodeData(2);
            
            ROI_TEMP = app.ROI_LBC;
            Positions = ROI_TEMP.Position;
            
            NbPx = app.LBC_ValueMC.Value;
            NbSim = app.LBC_NbSimMC.Value;
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools');
            app.WaitBar.Message = 'XMapTools is running numbers ...';
            
            LBC_Table = zeros(NbSim,length(app.XMapToolsData.MapData.Me.Data(Idx).ElNames));
            switch app.Calibrate_ROI_menu.Value
                case 'Rectangle ROI'
                    
                    BackPos = zeros(NbSim,4);
                    
                    for i = 1:NbSim
                        
                        app.WaitBar.Value = i/NbSim;
                        
                        while 1
                            PosTemp = Positions + NbPx.*randn(size(Positions));
                            
                            if PosTemp(1) < 0
                                PosTemp(1) = 0;
                            end
                            if PosTemp(2) < 0
                                PosTemp(2) = 0;
                            end
                            if PosTemp(3) < 0
                                PosTemp(3) = 0.1*size(DensityMap,2);
                            end
                            if PosTemp(4) < 0
                                PosTemp(4) = 0.1*size(DensityMap,1);
                            end
                            if PosTemp(1)+PosTemp(3) > size(DensityMap,2)
                                PosTemp(3) = size(DensityMap,2)-PosTemp(1);
                            end
                            if PosTemp(2)+PosTemp(4) > size(DensityMap,1)
                                PosTemp(4) = size(DensityMap,1)-PosTemp(2);
                            end
                            
                            if PosTemp > 0
                                break
                            end
                        end
                        
                        BackPos(i,:) = PosTemp;
                        
                        try
                            ROI_TEMP.Position = PosTemp;
                        catch ME
                            keyboard
                        end
                        
                        Mask = createMask(ROI_TEMP);
                        IdxMask = find(Mask & app.XMapToolsData.MapData.Me.Data(Idx).CData(end).Map > 0);
                        DensityDomain = sum(DensityMap(IdxMask))/numel(IdxMask);
                        for j = 1:length(app.XMapToolsData.MapData.Me.Data(Idx).ElNames)
                            
                            Map = app.XMapToolsData.MapData.Me.Data(Idx).CData(j).Map;
                            Comp = sum(DensityMap(IdxMask)/DensityDomain .* Map(IdxMask))/numel(IdxMask);              % from Lanari & Engi (2017)
                            
                            LBC_Table(i,j) = Comp;
                        end
                    end
                    
                    ROI_TEMP.Position = Positions;
                    
                    Menu_EditOpenImageNewWindowSelected(app);
                    hold on
                    for i = 1:NbSim
                        rectangle('Position',BackPos(i,:));
                    end
                    
                case 'Polygon ROI'
                    uialert(gcbf,'Option not yes available for polygon shapes!','XMapTools – Error');
                    %errordlg('Option not yes available for polygon shapes!','XMapTools');
                    close(app.WaitBar);
            end
            
            Sum = sum(LBC_Table,2);
            WhereNan = isnan(Sum);
            LBC_Table(WhereNan,:) = [];
            
            title(['Nb simulations valid = ',num2str(size(LBC_Table,1))])
            
            StDev = std(LBC_Table,1);
            Mean = mean(LBC_Table,1);
            
            LBC_Table_Disp = app.CompViewer_UITable.Data;
            
            for i = 1:length(app.XMapToolsData.MapData.Me.Data(Idx).ElNames)
                LBC_Table_Disp{i,3} = StDev(i);
                LBC_Table_Disp{i,4} = Mean(i);
            end
            
            fid = fopen('Last_LBCsim.txt','w');
            fprintf(fid,repmat(['%s\t'],1,length(app.XMapToolsData.MapData.Me.Data(Idx).ElNames)),app.XMapToolsData.MapData.Me.Data(Idx).ElNames{:})
            fprintf(fid,'\n');
            for i = 1:size(LBC_Table,1)
                fprintf(fid,repmat(['%f\t'],1,length(app.XMapToolsData.MapData.Me.Data(Idx).ElNames)),LBC_Table(i,:));
                fprintf(fid,'\n');
            end
            fclose(fid);
            
            app.CompViewer_UITable.Data = LBC_Table_Disp;
            app.CompViewer_UITable.ColumnName = {'Element','Composition','2std','Mean'};
            app.CompViewer_DensityMenu.Visible = 'on';
            
            %pie(app.CompViewer_PlotTable,cell2mat(LBC_Table(1:end-1,2)),LBC_Table(1:end-1,1))
            
            app.CompViewer_Button_Copy.Enable = 'on';
            app.CompViewer_Button_Save.Enable = 'on';
            app.CompViewer_Button_DeleteROI.Enable = 'on';
            
            app.LBC_UncCalc.Enable = 'off';
            app.LBC_ValueMC.Enable = 'off';
            app.LBC_NbSimMC.Enable = 'off';
            app.PxLabel.Enable = 'off';
            app.SimLabel.Enable = 'off';
            
            close(app.WaitBar)
            
        end

        % Callback function: Help_ProjectImportMenu, ImportTab_help
        function Help_ImportTab_helpButtonPushed(app, event)
            
            if isempty(app.Id_HelpTool)
                Help_Display(app,'Workspace_Project_Import.html');
            else
                app.Id_HelpTool.UpdateTextHelp('Workspace_Project_Import.html');
            end
            
            
        end

        % Callback function: ClassifyTab_help, Help_ClassifyMenu
        function Help_ClassifyTab_helpButtonPushed(app, event)
            if isempty(app.Id_HelpTool)
                Help_Display(app,'Workspace_Classify.html');
            else
                app.Id_HelpTool.UpdateTextHelp('Workspace_Classify.html');
            end
        end

        % Callback function: Help_SegmentMenu, SegmentTab_help
        function Help_SegmentTab_helpButtonPushed(app, event)
            if isempty(app.Id_HelpTool)
                Help_Display(app,'Workspace_Segment.html');
            else
                app.Id_HelpTool.UpdateTextHelp('Workspace_Segment.html');
            end
        end

        % Callback function: CalibratetTab_help, Help_CalibrateMenu
        function Help_CalibratetTab_helpButtonPushed(app, event)
            if isempty(app.Id_HelpTool)
                Help_Display(app,'Workspace_Calibrate.html');
            else
                app.Id_HelpTool.UpdateTextHelp('Workspace_Calibrate.html');
            end
        end

        % Callback function: FunctionTab_help, Help_FunctionsMenu
        function Help_FunctionTab_helpButtonPushed(app, event)
            if isempty(app.Id_HelpTool)
                Help_Display(app,'Workspace_Functions.html');
            else
                app.Id_HelpTool.UpdateTextHelp('Workspace_Functions.html');
            end
        end

        % Callback function: AddonsTab_help, Help_AddonsMenu
        function Help_AddonsTab_helpButtonPushed(app, event)
            if isempty(app.Id_HelpTool)
                Help_Display(app,'Workspace_Addons.html');
            else
                app.Id_HelpTool.UpdateTextHelp('Workspace_Addons.html');
            end
        end

        % Callback function: Help_OptionsMenu, OptionsTab_help
        function Help_OptionsTab_helpButtonPushed(app, event)
            if isempty(app.Id_HelpTool)
                Help_Display(app,'Workspace_Options.html');
            else
                app.Id_HelpTool.UpdateTextHelp('Workspace_Options.html');
            end
        end

        % Menu selected function: Help_SamplingToolsMenu
        function Help_SamplingToolsMenuSelected(app, event)
            if isempty(app.Id_HelpTool)
                Help_Display(app,'XMT_help_SamplingTools.html');
            else
                app.Id_HelpTool.UpdateTextHelp('XMT_help_SamplingTools.html');
            end
        end

        % Menu selected function: Help_DataVizualisationToolMenu
        function Help_DataVizualisationToolMenuSelected(app, event)
            if isempty(app.Id_HelpTool)
                Help_Display(app,'XMT_help_DataVizualizationTools.html');
            else
                app.Id_HelpTool.UpdateTextHelp('XMT_help_DataVizualizationTools.html');
            end
        end

        % Menu selected function: ImagesMenu
        function Help_ImagesMenuSelected(app, event)
            if isempty(app.Id_HelpTool)
                Help_Display(app,'XMT_help_Images.html');
            else
                app.Id_HelpTool.UpdateTextHelp('XMT_help_Images.html');
            end
        end

        % Value changed function: MapSlider
        function MapSliderValueChanged(app, event)
            value = round(app.MapSlider.Value);
            
            SelectedNodes = app.TreeData_Main.SelectedNodes;
            
            if isempty(SelectedNodes)
                return
            end
            NodeData = SelectedNodes.NodeData;
            
            switch NodeData(1)
                case 1
                    app.TreeData_Main.SelectedNodes = app.Node_It.Children(value);
                case 6
                    app.TreeData_Main.SelectedNodes = app.Node_Ct.Children(value);
            end
            
            app.MapSlider.Value = value;
            app.Value_MapSlider.Value = value;
        end

        % Value changing function: MapSlider
        function MapSliderValueChanging(app, event)
            changingValue = round(event.Value);
            
            SelectedNodes = app.TreeData_Main.SelectedNodes;
            
            if isempty(SelectedNodes)
                return
            end
            NodeData = SelectedNodes.NodeData;
            
            switch NodeData(1)
                case 1
                    app.TreeData_Main.SelectedNodes = app.Node_It.Children(changingValue);
                case 6
                    app.TreeData_Main.SelectedNodes = app.Node_Ct.Children(changingValue);
                case 7
                    app.LiveSliderValueForROI = changingValue;
            end
            
            if ~isempty(app.TreeData_Additional.SelectedNodes)
                for i=1:numel(app.TreeData_Additional.SelectedNodes)
                    SelectedAdditional(i,:) = app.TreeData_Additional.SelectedNodes(i).NodeData;
                end
            else
                SelectedAdditional = [];
            end
            
            app.Value_MapSlider.Value = changingValue;
            
            PlotMap_DisplaySelectedMap(app,NodeData,SelectedAdditional)
            
        end

        % Value changed function: ROI_DispVertical, 
        % ROI_VerticalSlider
        function ROI_DispVerticalValueChanged(app, event)
            
            SelectedNodes = app.TreeData_Main.SelectedNodes;
            NodeData = SelectedNodes.NodeData;
            
            SelectedAdditional = [];
            
            app.ROI_AngleOrientation.Value = app.ROI_VerticalSlider.Value;
            
            PlotMap_DisplaySelectedMap(app,NodeData,SelectedAdditional)
            
        end

        % Value changing function: ROI_VerticalSlider
        function ROI_VerticalSliderValueChanging(app, event)
            changingValue = event.Value;
            app.ROI_AngleOrientation.Value = changingValue;
            
            SelectedNodes = app.TreeData_Main.SelectedNodes;
            NodeData = SelectedNodes.NodeData;
            
            SelectedAdditional = [];
            
            app.ROI_VerticalSlider.Value = app.ROI_AngleOrientation.Value;
            
            PlotMap_DisplaySelectedMap(app,NodeData,SelectedAdditional)
            
            
        end

        % Value changed function: ROI_AngleOrientation
        function ROI_AngleOrientationValueChanged(app, event)
            SelectedNodes = app.TreeData_Main.SelectedNodes;
            NodeData = SelectedNodes.NodeData;
            
            SelectedAdditional = [];
            
            app.ROI_VerticalSlider.Value = app.ROI_AngleOrientation.Value;
            
            PlotMap_DisplaySelectedMap(app,NodeData,SelectedAdditional)
        end

        % Value changed function: Value_MapSlider
        function Value_MapSliderValueChanged(app, event)
            changingValue = round(app.Value_MapSlider.Value);
            
            SelectedNodes = app.TreeData_Main.SelectedNodes;
            NodeData = SelectedNodes.NodeData;
            
            switch NodeData(1)
                case 1
                    app.TreeData_Main.SelectedNodes = app.Node_It.Children(changingValue);
                case 6
                    app.TreeData_Main.SelectedNodes = app.Node_Ct.Children(changingValue);
                case 7
                    app.LiveSliderValueForROI = changingValue;
            end
            
            if ~isempty(app.TreeData_Additional.SelectedNodes)
                for i=1:numel(app.TreeData_Additional.SelectedNodes)
                    SelectedAdditional(i,:) = app.TreeData_Additional.SelectedNodes(i).NodeData;
                end
            else
                SelectedAdditional = [];
            end
            
            app.MapSlider.Value = changingValue;
            
            PlotMap_DisplaySelectedMap(app,NodeData,SelectedAdditional)
            
            
        end

        % Button pushed function: Segment_SegmentButton
        function Segment_SegmentButtonButtonPushed(app, event)
            
            [SegNames,Ranges] = Segmentation_ExtractDetails(app);
            
            SelectedImage = [1:length(app.XMapToolsData.MapData.Ct.Names)];
            
            Segmentation_PerformSegmentation(app,SelectedImage,SegNames,Ranges);
            
            
            
            
            
            
        end

        % Button pushed function: Segment_AddScheme
        function Segment_AddSchemeButtonPushed(app, event)
            
            SelectedNodes = app.TreeData_Additional.SelectedNodes;
            if isempty(SelectedNodes)
                return
            end
            NodeData = SelectedNodes.NodeData;
            
            SegScheme = app.XMapToolsData.SegScheme;
            Idx = numel(SegScheme.Names);
            
            if isequal(NodeData(1:2),[13,1])
                
                if isequal(NodeData(3),0)
                    % We add a new Scheme
                    
                    Idx = Idx+1;
                    
                    Name = app.Segment_NameField.Value;
                    
                    SegScheme.Names{Idx} = Name;
                    SegScheme.Types(Idx) = 1;                 % Option for later use
                    SegScheme.Nb(Idx) = 0;
                    SegScheme.Data(Idx).Names = {};
                    SegScheme.Data(Idx).Range = [];
                    
                    p = uitreenode(app.Node_SegScheme,'Text',char(SegScheme.Names(Idx)),'NodeData',[13,1,Idx,0]);
                    
                    expand(app.Node_SegScheme);
                    app.TreeData_Additional.SelectedNodes = p;
                else
                    if isequal(NodeData(4),0)
                        % we add a new Segment
                        
                        Name = app.Segment_NameField.Value;
                        
                        IdxMask = SegScheme.Nb(NodeData(3))+1;
                        SegScheme.Data(NodeData(3)).Names{IdxMask} = Name;
                        SegScheme.Data(NodeData(3)).ROI(IdxMask).Coordinates = [];
                        
                        p = uitreenode(app.Node_SegScheme.Children(NodeData(3)),'Text',char(Name),'NodeData',[13,1,NodeData(3),IdxMask]);
                        
                        SegScheme.Nb(NodeData(3)) = IdxMask;
                        
                        expand(app.Node_SegScheme.Children(NodeData(3)));
                        
                    else
                        
                        Lower = app.EditField_LiveMin.Value;
                        Higher = app.EditField_LiveMax.Value;
                        
                        SegScheme.Data(NodeData(3)).ROI(NodeData(4)).Coordinates(1,:) = [Lower,Higher];
                        
                        if isempty(app.Node_SegScheme.Children(NodeData(3)).Children(NodeData(4)).Children)
                            p = uitreenode(app.Node_SegScheme.Children(NodeData(3)).Children(NodeData(4)),'Text',['range: ',num2str(Lower),'->',num2str(Higher)],'NodeData',[13,1,NodeData(3),NodeData(4),1]);
                        else
                            app.Node_SegScheme.Children(NodeData(3)).Children(NodeData(4)).Children(1).Text = ['range: ',num2str(Lower),'->',num2str(Higher)];
                        end
                        expand(app.Node_SegScheme.Children(NodeData(3)).Children(NodeData(4)));
                    end
                end
                
            end
            
            %             SegScheme.Names = {};
            %             SegScheme.Types = [];
            %             SegScheme.Nb = [];
            %             SegScheme.Data(1).Names = {};
            %             SegScheme.Data(1).Range = [];
            app.SaveRequired = 1;
            
            app.XMapToolsData.SegScheme = SegScheme;
            TreeData_AdditionalSelectionChanged(app, event);
            %keyboard
            
        end

        % Button pushed function: Segment_TrySegmentationButton
        function Segment_TrySegmentationButtonPushed(app, event)
            [SegNames,Ranges] = Segmentation_ExtractDetails(app);
            
            SelectedImage = app.TreeData_Main.SelectedNodes.NodeData(2);
            
            Segmentation_PerformSegmentation(app,SelectedImage,SegNames,Ranges);
            
            
            
            
        end

        % Button pushed function: Segment_ButtonPlot
        function Segment_ButtonPlotPushed(app, event)
            
            SelectedNodes = app.TreeData_Main.SelectedNodes;
            NodeData = SelectedNodes.NodeData;
            
            ROI = app.XMapToolsData.MapData.ROI.Data(NodeData(2)).ROI;
            ROIsize = size(ROI);
            
            NbMask = length(app.XMapToolsData.MapData.ROI.Data(NodeData(2)).Names);
            Names = app.XMapToolsData.MapData.ROI.Data(NodeData(2)).Names;
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'XMapTools is running numbers (modes extraction)';
            
            % Vertical cut
            
            VoxTable = zeros(ROIsize(3),NbMask);
            NbVoxelsZero = zeros(ROIsize(3),1);
            
            for i = 1:ROIsize(3)
                NbVoxels = zeros(1,NbMask);
                for j=1:NbMask
                    NbVoxels(j) = numel(find(ROI(:,:,i) == j));
                end
                NbVoxelsZero(i) = numel(find(ROI(:,:,i) == 0));
                VoxTable(i,:) = NbVoxels;
            end
            
            VolTableFrac = VoxTable./repmat(sum(VoxTable,2),1,NbMask);
            
            MeanVolFrac = mean(VolTableFrac,1);
            [Val,WhereMin] = min(MeanVolFrac);
            
            fig = figure;
            left_color = [0 0 0];
            right_color = [0 0 0];
            set(fig,'defaultAxesColorOrder',[left_color; right_color]);
            subplot(2,1,1)
            yyaxis left
            area(VolTableFrac,'FaceColor','flat');
            xlabel('Image #')
            ylabel('Mode (vol%)')
            ColorMap = colormap(app.FigMain);
            colormap(gca,ColorMap(2:end,:));
            yyaxis right
            plot(VolTableFrac(:,WhereMin),'Linewidth',2);
            ylabel(['Mode of ',Names{WhereMin},' (vol%)'])
            ax = axis;
            axis([0,size(VolTableFrac,1),ax(3),ax(4)])
            legend({Names{:},Names{WhereMin}},'Location','Best')
            
            
            subplot(2,1,2)
            plot(NbVoxelsZero,'-k');
            axis([0,size(VolTableFrac,1),min(NbVoxelsZero),max(NbVoxelsZero)])
            
            
            rho = corr(VolTableFrac);
            figure,
            imagesc(rho); axis image
            colorbar
            colormap(parula(64));
            caxis([-1 1])
            ax = gca;
            ax.XTick = [1:1:NbMask];
            ax.XTickLabel = Names;
            ax.XAxisLocation = 'top';
            ax.YTick = [1:1:NbMask];
            ax.YTickLabel = Names;
            ax.YTickLabelRotation = 90;
            for i = 1:NbMask
                for j = 1:NbMask
                    t = text(i,j,num2str(round(rho(i,j),2)));
                    t.HorizontalAlignment = 'center';
                end
            end
            
            % Smoothing
            VoxTable_smooth = nan(size(VoxTable));
            Win = round(size(VoxTable_smooth,1)/app.Segment_SmoothingFact.Value);
            HalfWin = floor(Win/2);
            if HalfWin<2
                HalfWin=2;
                Win = 5;
            end
            
            for iP = HalfWin+1:1:size(VoxTable_smooth,1)-HalfWin
                VoxTable_smooth(iP,:) = sum(VoxTable(iP-(HalfWin):iP+HalfWin,:),1);
            end
            
            figure, hold on
            for i = 1:size(VoxTable,2)
                plot(VoxTable_smooth(:,i)/VoxTable_smooth(Win,i),'-','Color',ColorMap(i+1,:),'LineWidth',2)
            end
            legend({Names{:}},'Location','Best')
            ax = gca;
            ax.XLim = [0,size(VoxTable,1)];
            
            rho = corr(VoxTable_smooth(Win:size(VoxTable_smooth,1)-Win,:));
            figure,
            imagesc(rho); axis image
            colorbar
            colormap(parula(64));
            caxis([-1 1])
            ax = gca;
            ax.XTick = [1:1:NbMask];
            ax.XTickLabel = Names;
            ax.XAxisLocation = 'top';
            ax.YTick = [1:1:NbMask];
            ax.YTickLabel = Names;
            ax.YTickLabelRotation = 90;
            for i = 1:NbMask
                for j = 1:NbMask
                    t = text(i,j,num2str(round(rho(i,j),2)));
                    t.HorizontalAlignment = 'center';
                end
            end
            
            close(app.WaitBar)
            
            
            return
            
            % Temp
            Vgrt = VoxTable(:,3)*0.030^3; % in cm^3
            Count = 0;
            for iP = 50:1:length(Vgrt)-50
                Count = Count+1;
                Vgrt_smooth(Count) = sum(Vgrt(iP-49:iP+50));
            end
            
            figure, plot(Vgrt,'.')
            figure, plot([1:length(Vgrt)],[zeros(1,50),Vgrt_smooth,zeros(1,49)],'.')
            hold on
            plot([450,300,600],[242/2,132,38],'or')
            
            Vmtx = VoxTable(:,end)*0.030^3; % in cm^3
            Count = 0;
            for iP = 50:1:length(Vmtx)-50
                Count = Count+1;
                Vmtx_smooth(Count) = sum(Vmtx(iP-49:iP+50));
            end
            figure, plot(Vmtx,'.')
            figure, plot([1:length(Vmtx)],[zeros(1,50),Vmtx_smooth,zeros(1,49)],'.')
            axis([0 900 800 1100])
            % TEMP----------
            
            
            
            % Horizontal cut
            
            VoxTable = zeros(ROIsize(1),NbMask);
            NbVoxelsZero = zeros(ROIsize(1),1);
            
            for i = 1:ROIsize(1)
                NbVoxels = zeros(1,NbMask);
                for j=1:NbMask
                    NbVoxels(j) = numel(find(ROI(i,:,:) == j));
                end
                NbVoxelsZero(i) = numel(find(ROI(i,:,:) == 0));
                VoxTable(i,:) = NbVoxels;
            end
            
            VolTableFrac = VoxTable./repmat(sum(VoxTable,2),1,NbMask);
            
            MeanVolFrac = mean(VolTableFrac,1);
            [Val,WhereMin] = min(MeanVolFrac);
            
            figure
            subplot(2,1,1)
            yyaxis left
            area(VolTableFrac,'FaceColor','flat');
            xlabel('Image #')
            ylabel('Mode (vol%)')
            yyaxis right
            plot(VolTableFrac(:,WhereMin),'Linewidth',2);
            ylabel(['Mode of ',Names{WhereMin},' (vol%)'])
            ax = axis;
            axis([0,size(VolTableFrac,1),ax(3),ax(4)])
            legend({Names{:},Names{WhereMin}},'Location','Best')
            
            
            subplot(2,1,2)
            plot(NbVoxelsZero,'-k');
            axis([0,size(VolTableFrac,1),min(NbVoxelsZero),max(NbVoxelsZero)])
            
            SumVol=sum(VolTableFrac,2);
            Idx = find(~isnan(SumVol));
            rho = corr(VolTableFrac(Idx,:));
            figure,
            imagesc(rho); axis image
            colorbar
            colormap(parula(64));
            caxis([-1 1])
            ax = gca;
            ax.XTick = [1:1:NbMask];
            ax.XTickLabel = Names;
            ax.XAxisLocation = 'top';
            ax.YTick = [1:1:NbMask];
            ax.YTickLabel = Names;
            ax.YTickLabelRotation = 90;
            for i = 1:NbMask
                for j = 1:NbMask
                    t = text(i,j,num2str(round(rho(i,j),2)));
                    t.HorizontalAlignment = 'center';
                end
            end
            
            
            
        end

        % Button pushed function: Segment_SavePhaseProp
        function Segment_SavePhasePropButtonPushed(app, event)
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'XMapTools is running numbers (modes extraction)';
            
            SelectedNodes = app.TreeData_Main.SelectedNodes;
            NodeData = SelectedNodes.NodeData;
            
            ROI = app.XMapToolsData.MapData.ROI.Data(NodeData(2)).ROI;
            ROIsize = size(ROI);
            
            NbMask = length(app.XMapToolsData.MapData.ROI.Data(NodeData(2)).Names);
            Names = app.XMapToolsData.MapData.ROI.Data(NodeData(2)).Names;
            
            NbPx = zeros(size(Names));
            
            for i = 1:length(Names)
                NbPx(i) = length(find(ROI == i));
            end
            
            Prop = NbPx/sum(NbPx)*100;
            
            app.WaitBar.Message = 'Saving results ...';
            
            [Success,Message,MessageID] = mkdir('Exported-Modes');
            
            cd Exported-Modes
            f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
            [Directory, pathname] = uiputfile({'*.txt', 'TXT Files (*.txt)'}, 'Export proportions results as');
            close(f)
            cd ..
            
            figure(app.XMapTools_GUI)
            
            if ~Directory
                return
            end
            
            close(app.WaitBar)
            
            fid = fopen([pathname,Directory],'w');
            fprintf(fid,'%s\n%s\n','--','Modes exported using XMapTools');
            fprintf(fid,'%s\n\n',datestr(now));
            
            fprintf(fid,'%s\t\t%s\t%s\n','Mask','Modes (%)','Nb voxels');
            for i = 1:length(Prop)
                if length(char(Names{i})) > 7
                    fprintf(fid,'%s\t%.3f\t\t%.0f\n',Names{i},Prop(i),NbPx(i));
                else
                    fprintf(fid,'%s\t\t%.3f\t\t%.0f\n',Names{i},Prop(i),NbPx(i));
                end
                
            end
            fclose(fid);
            
        end

        % Button pushed function: Segment_GradientMapButton
        function Segment_GradientMapButtonPushed(app, event)
            %[SegNames,Ranges] = Segmentation_ExtractDetails(app);
            
            SelectedImage = app.TreeData_Main.SelectedNodes.NodeData(2);
            
            ImageTEMP = app.XMapToolsData.MapData.Ct.Data(SelectedImage).Map;
            
            Threshold = app.Segment_ErosionFilterSpinner.Value;
            Textured = CalculateTextureFilter(app,ImageTEMP);
            
            %figure, imagesc(Textured), axis image, colorbar
            
            FilterIdx = find(Textured(:)>Threshold);
            
            LowerLim = -0.05;
            
            SegTEMP = Textured;
            SegTEMP(FilterIdx) = LowerLim*ones(size(FilterIdx));
            
            figure
            tiledlayout(1,2);
            
            ax1 = nexttile;
            imagesc(Textured), axis image, colorbar
            Caxis = caxis(gca);
            caxis([LowerLim,Caxis(2)])
            title(app.SchemeDropDown.Value)
            
            ax2 = nexttile;
            imagesc(SegTEMP), axis image, colorbar, colormap([1,0,0;parula(128)])
            caxis([LowerLim,Caxis(2)])
            title(['Filter GB (value=',num2str(app.Segment_ErosionFilterSpinner.Value),')'])
            
            linkaxes([ax1 ax2],'xy')
            
            
        end

        % Value changed function: SchemeDropDown
        function Segment_SchemeDropDownValueChanged(app, event)
            switch app.SchemeDropDown.Value
                case 'Local Gradient'
                    app.Segment_ErosionFilterSpinner.Value = 0.15;
                    
                case 'Local Standard Deviation'
                    app.Segment_ErosionFilterSpinner.Value = 0.15;
                    
                case 'Local Entropy'
                    app.Segment_ErosionFilterSpinner.Value = 0.95;
                    
                case 'Local Range'
                    app.Segment_ErosionFilterSpinner.Value = 0.15;
                    
            end
            
        end

        % Button pushed function: Segment_ExportROI_TXT
        function Segment_ExportROI_TXTButtonPushed(app, event)
            
            Value = uiconfirm(app.XMapTools_GUI, 'Exporting the ROI data in a MAT file can take up to several hours. Are you sure that you want to do this?', 'XMapTools', 'Options', {'Yes','No','Cancel'});
            
            if isequal(Value,'No') || isequal(Value,'Cancel')
                return
            end
            
            NodeData = app.TreeData_Main.SelectedNodes.NodeData;
            
            if ~isequal(NodeData(1),7)
                return
            end
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Select ROI to export';
            
            MinNames = app.XMapToolsData.MapData.ROI.Data(NodeData(2)).Names;
            
            waitfor(Selector(app,MinNames,'Select ROI to export:','Single'));
            Selected = find(ismember(MinNames,app.ExchangeSelector));
            
            %Selected = listdlg('PromptString','Select ROI to export:','SelectionMode','single','ListString',MinNames);
            
            app.WaitBar.Message = 'XMapTools is running numbers';
            
            Idx = find(app.XMapToolsData.MapData.ROI.Data(NodeData(2)).ROI == Selected);
            
            SizeROI = size(app.XMapToolsData.MapData.ROI.Data(NodeData(2)).ROI);
            
            NbPixelsSlice = SizeROI(1)*SizeROI(2);
            BaseSlice = floor(Idx./NbPixelsSlice);
            
            PosSingleSlice = Idx - NbPixelsSlice.*BaseSlice;
            BasePos = floor(PosSingleSlice./SizeROI(1));
            
            X = PosSingleSlice - SizeROI(1).*BasePos;
            Y = BasePos+1;
            Z = BaseSlice+1;
            
            DataExport = [X,Y,Z];
            
            app.WaitBar.Message = 'Save the results as ...';
            
            f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
            [Directory, pathname] = uiputfile({'*.mat', 'MAT Files (*.mat)'}, 'Export ROI as');
            close(f)
            figure(app.XMapTools_GUI);
            
            % dlmwrite(fullfile(pathname,Directory), Data4Save, 'delimiter','\t');
            
            app.WaitBar.Message = 'Saving results ...';
            
            Limit = 10000;
            Compt = 1;
            
            %             try
            %                 fid = fopen(fullfile(pathname,Directory),'w');
            %                 for i = 1:size(Data4Save,1)
            %                     Compt = Compt+1;
            %                     if Compt>Limit
            %                         Compt = 0;
            %                         app.WaitBar.Message = ['XMapTools is writting data in the file (',num2str(i/size(Data4Save,1)*100),' %)'];
            %                     end
            %                     %fprintf(fid,'%f\n',Data4Save(i,:));
            %                     fprintf(fid,'%s\n',num2str(Data4Save(i,:)));
            %                 end
            %                 fclose(fid);
            %             catch ME
            %                 errordlg('Oups, something went wrong while saving the file','Error XMapTools');
            %                 close(app.WaitBar);
            %             end
            
            try
                save(fullfile(pathname,Directory),'DataExport','-v7.3');
            catch ME
                %keyboard
                uialert(gcbf,'Oups, something went wrong while saving the file','XMapTools – Error');
                %errordlg('Oups, something went wrong while saving the file','Error XMapTools');
                close(app.WaitBar);
            end
            
            close(app.WaitBar);
            
            
        end

        % Value changed function: SF_Function_CheckBox
        function Function_SF_Function_CheckBoxValueChanged(app, event)
            
            if isequal(app.SF_Function_CheckBox.Value,0)
                if isequal(app.SF_CationNorm_CheckBox.Value,0) && isequal(app.SF_OxygenNorm_CheckBox.Value,0)
                    app.SF_Function_CheckBox.Value = 1;
                end
            else
                app.SF_CationNorm_CheckBox.Value = 0;
                app.SF_OxygenNorm_CheckBox.Value = 0;
            end
            
        end

        % Value changed function: SF_OxygenNorm_CheckBox
        function Function_SF_OxygenNorm_CheckBoxValueChanged(app, event)
            
            if isequal(app.SF_OxygenNorm_CheckBox.Value,0)
                if isequal(app.SF_CationNorm_CheckBox.Value,0) && isequal(app.SF_Function_CheckBox.Value,0)
                    app.SF_OxygenNorm_CheckBox.Value = 1;
                end
            else
                app.SF_CationNorm_CheckBox.Value = 0;
                app.SF_Function_CheckBox.Value = 0;
            end
            
        end

        % Value changed function: SF_CationNorm_CheckBox
        function Function_SF_CationNorm_CheckBoxValueChanged(app, event)
            
            if isequal(app.SF_CationNorm_CheckBox.Value,0)
                if isequal(app.SF_OxygenNorm_CheckBox.Value,0) && isequal(app.SF_Function_CheckBox.Value,0)
                    app.SF_CationNorm_CheckBox.Value = 1;
                end
            else
                app.SF_OxygenNorm_CheckBox.Value = 0;
                app.SF_Function_CheckBox.Value = 0;
            end
            
        end

        % Value changed function: SF_MineralList
        function Function_SF_MineralListValueChanged(app, event)
            value = app.SF_MineralList.Value;
            Idx = find(ismember(app.SF_MineralList.Items,value));
            
            Idx = app.ExternalFunctions.IdxSF(Idx);
            
            app.SF_FunctionList.Items = app.ExternalFunctions.Min(Idx).SF.Name;
            app.SF_FunctionList.Value = app.ExternalFunctions.Min(Idx).SF.Name{1};
            
        end

        % Button pushed function: SF_ApplyButton
        function Function_SF_ApplyButtonPushed(app, event)
            
            % Data extraction (only available from Qt)
            
            SelectedNodes = app.TreeData_Main.SelectedNodes;
            NodeData = SelectedNodes.NodeData;
            
            QtName = app.XMapToolsData.MapData.Qt.Names{NodeData(2)};
            
            %ElNames = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElNames;
            ElInd = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElInd;
            
            % -------------------------------------------------------------
            % Eliminated 13.04.23 because indices are not used anymore!!!!
            %             IndPos = find(ElInd > 0);
            %
            %             for i = 1:length(IndPos)
            %                 QtElNames{i} = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElNames{IndPos(i)};
            %                 %QtElInd(i) = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElInd(IndPos(i));
            %                 QtData(:,i) = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).CData(IndPos(i)).Map(:);
            %             end
            % -------------------------------------------------------------
            
            for i = 1:length(app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElNames)
                QtElNames{i} = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElNames{i};
                QtData(:,i) = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).CData(i).Map(:);
            end
            
            SizeReshape = size(app.XMapToolsData.MapData.Qt.Data(NodeData(2)).CData(1).Map);
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'XMapTools is running numbers (structural formula)';
            
            [MatrixSF,ElementsList,NameResult] = Call_SF_External(app,QtElNames,QtData,QtName);
            
            close(app.WaitBar);
            
            IdRe = length(app.XMapToolsData.MapData.Re.Names)+1;
            
            app.XMapToolsData.MapData.Re.Names{IdRe} = NameResult;
            app.XMapToolsData.MapData.Re.Types(IdRe) = 0;
            app.XMapToolsData.MapData.Re.Coord(IdRe,1:2) = [0,0];
            
            app.XMapToolsData.MapData.Re.Data(IdRe).Labels = ElementsList;
            for i = 1:size(MatrixSF,2)
                app.XMapToolsData.MapData.Re.Data(IdRe).CData(i).Map = reshape(MatrixSF(:,i),SizeReshape);
            end
            
            app.Node_Re.ContextMenu = app.ContextMenu_MainTree_C;
            
            p = uitreenode(app.Node_Re,'Text',char(app.XMapToolsData.MapData.Re.Names{IdRe}),'NodeData',[4,IdRe,0]);
            for j = 1:length(app.XMapToolsData.MapData.Re.Data(IdRe).Labels)
                p1 = uitreenode(p,'Text',char(app.XMapToolsData.MapData.Re.Data(IdRe).Labels{j}),'NodeData',[4,IdRe,j]);
            end
            
            app.Node_Re.expand();
            app.TreeData_Main.SelectedNodes = app.Node_Re.Children(IdRe);
            
            %ResetGUI(app);
            %UpdateGUI(app);
            app.SF_ApplyButton.Enable = 'off';
            app.Other_ApplyButton.Enable = 'off';
            app.Other_MultiEquilibriumButton.Enable = 'off';
            app.Other_ROI_menu.Enable = 'off';
            
            app.SaveRequired = 1;
            
            figure(app.XMapTools_GUI)
            
            %keyboard
        end

        % Button pushed function: SF_ExportMinCompositions
        function Function_SF_ExportMinCompositionsButtonPushed(app, event)
            
            ROI_DeleteROI(app);
            
            DeactivatePlotZoomPanOptions(app);
            
            switch app.SF_ROI_menu.Value
                case 'Circle ROI'
                    DrawingMode(app,'on','Circle');
                    app.ROI_COMP = drawcircle(app.FigMain,'Color',[0.57,0.00,0.69],'InteractionsAllowed','all');
                    DrawingMode(app,'off');
                    
                case 'Rectangle ROI'
                    DrawingMode(app,'on','Rectangle');
                    app.ROI_COMP = drawrectangle(app.FigMain,'Color',[0.57,0.00,0.69],'InteractionsAllowed','all');
                    DrawingMode(app,'off');
                    
                case 'Polygon ROI'
                    DrawingMode(app,'on','Polygon');
                    app.ROI_COMP = drawpolygon(app.FigMain,'Color',[0.57,0.00,0.69],'InteractionsAllowed','all');
                    DrawingMode(app,'off');
                    
            end
            
            app.ROI_COMP_Listener = addlistener(app.ROI_COMP, 'ROIMoved', @(varargin)COMP_ROI_changed_shape(app, app.ROI_LBC));
            
            % Extract data
            COMP_ROI_changed_shape(app,app.ROI_sampling);
            
            
            
        end

        % Button pushed function: SF_HelpFile
        function Function_SF_HelpFileButtonPushed(app, event)
            
            if isdeployed
                if isempty(app.Id_HelpTool)
                    Help_Display(app,'XMT_help_External_SF.html');
                else
                    app.Id_HelpTool.UpdateTextHelp('XMT_help_External_SF.html');
                end
            else
                MinList = app.SF_MineralList.Items;
                FctList = app.SF_FunctionList.Items;
                
                WhereMin = find(ismember(MinList,app.SF_MineralList.Value));
                WhereMin = app.ExternalFunctions.IdxSF(WhereMin);
                WhereFct = find(ismember(FctList,app.SF_FunctionList.Value));
                
                ExtFct = char(app.ExternalFunctions.Min(WhereMin).SF.FileName{WhereFct});
                
                if isempty(app.Id_HelpTool)
                    Help_Display(app,ExtFct);
                else
                    app.Id_HelpTool.UpdateTextHelp(ExtFct);
                end
            end
            
        end

        % Button pushed function: Other_HelpFile
        function Function_Other_HelpFileButtonPushed(app, event)
            
            if isdeployed
                if isempty(app.Id_HelpTool)
                    Help_Display(app,'XMT_help_External_Other.html');
                else
                    app.Id_HelpTool.UpdateTextHelp('XMT_help_External_Other.html');
                end
            else
                MinList = app.ExternalFunctions.Mineral;
                FctList = app.Other_FunctionList.Items;
                
                WhereMin = find(ismember(MinList,app.Other_MineralList.Value));
                WhereFct = find(ismember(FctList,app.Other_FunctionList.Value));
                
                switch app.Other_MethodList.Value
                    case 'Map-mode'
                        ExtFct = char(app.ExternalFunctions.Min(WhereMin).TB.FileName{WhereFct});
                        
                    case 'Multi-equilibrium'
                        ExtFct = char(app.ExternalFunctions.Min(WhereMin).ME.FileName{WhereFct});
                end
                
                if isempty(app.Id_HelpTool)
                    Help_Display(app,ExtFct);
                else
                    app.Id_HelpTool.UpdateTextHelp(ExtFct);
                end
            end
            
            
            
            
        end

        % Button pushed function: Other_ApplyButton
        function Function_Other_ApplyButtonPushed(app, event)
            
            % To be implemented with the mode menu
            Mode = 1;
            
            app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'XMapTools is running numbers (thermobarometry & other methods)';
            
            % Data extraction (only available from Qt)
            
            SelectedNodes = app.TreeData_Main.SelectedNodes;
            NodeData = SelectedNodes.NodeData;
            
            QtName = app.XMapToolsData.MapData.Qt.Names{NodeData(2)};
            
            %ElNames = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElNames;
            
            % -------------------------------------------------------------
            % Eliminated 13.04.23 because indices are not used anymore!!!!
            %             ElInd = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElInd;
            %
            %             IndPos = find(ElInd > 0);
            %
            %             for i = 1:length(IndPos)
            %                 QtElNames{i} = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElNames{IndPos(i)};
            %                 QtElInd(i) = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElInd(IndPos(i));
            %                 QtData(:,i) = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).CData(IndPos(i)).Map(:);
            %             end
            % -------------------------------------------------------------
            
            for i = 1:length(app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElNames)
                QtElNames{i} = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElNames{i};
                QtData(:,i) = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).CData(i).Map(:);
            end
            
            SizeReshape = size(app.XMapToolsData.MapData.Qt.Data(NodeData(2)).CData(1).Map);
            
            %app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
            %app.WaitBar.Message = 'XMapTools is running numbers (structural formula calculation)';
            
            try
                switch Mode
                    
                    case 1
                        MinList = app.Other_MineralList.Items;
                        FctList = app.Other_FunctionList.Items;
                        
                        WhereMin = find(ismember(MinList,app.Other_MineralList.Value));
                        WhereFct = find(ismember(FctList,app.Other_FunctionList.Value));
                        
                        ExtFct = char(app.ExternalFunctions.Min(app.ExternalFunctions.IdxTB(WhereMin)).TB.FileName{WhereFct});
                        ExtFctLabel = char(app.ExternalFunctions.Min(app.ExternalFunctions.IdxTB(WhereMin)).TB.Name{WhereFct});
                        
                        AddVar = app.ExternalFunctions.Min(app.ExternalFunctions.IdxTB(WhereMin)).TB.Details(WhereFct).AddVar;
                        Def = app.ExternalFunctions.Min(app.ExternalFunctions.IdxTB(WhereMin)).TB.Details(WhereFct).Def;
                        
                        % Additional variables
                        if ~isempty(AddVar)
                            for i = 1:length(Def)
                                Def_str{i} = num2str(Def(i));
                            end
                            AddVarVal_str=inputdlg(AddVar,'Input',1,Def_str);
                            for i = 1:length(AddVarVal_str)
                                AddVarVal(i) = str2num(AddVarVal_str{i});
                            end
                            AddParameters.AddVar = AddVar;
                            AddParameters.Values = AddVarVal;
                        else
                            AddParameters.AddVar = {};
                            AddParameters.Values = [];
                        end
                        
                        [MatrixSF,ElementsList] = Core_Call_TB(QtData,QtElNames,ExtFct,AddParameters,app.ElOxDataDef);
                        
                        
                        NameResult = [QtName,' ',ExtFctLabel];
                        
                end
            catch ME
                uialert(app.XMapTools_GUI, {'Error in the computation of the external function:',ME.message}, 'XMapTools – Error')
                figure(app.XMapTools_GUI);
                close(app.WaitBar);
                return
            end
            
            
            IdRe = length(app.XMapToolsData.MapData.Re.Names)+1;
            
            app.XMapToolsData.MapData.Re.Names{IdRe} = NameResult;
            app.XMapToolsData.MapData.Re.Types(IdRe) = 0;
            app.XMapToolsData.MapData.Re.Coord(IdRe,1:2) = [0,0];
            
            app.XMapToolsData.MapData.Re.Data(IdRe).Labels = ElementsList;
            for i = 1:size(MatrixSF,2)
                app.XMapToolsData.MapData.Re.Data(IdRe).CData(i).Map = reshape(MatrixSF(:,i),SizeReshape);
            end
            
            app.Node_Re.ContextMenu = app.ContextMenu_MainTree_C;
            
            p = uitreenode(app.Node_Re,'Text',char(app.XMapToolsData.MapData.Re.Names{IdRe}),'NodeData',[4,IdRe,0]);
            for j = 1:length(app.XMapToolsData.MapData.Re.Data(IdRe).Labels)
                p1 = uitreenode(p,'Text',char(app.XMapToolsData.MapData.Re.Data(IdRe).Labels{j}),'NodeData',[4,IdRe,j]);
            end
            
            app.Node_Re.expand();
            app.TreeData_Main.SelectedNodes = app.Node_Re.Children(IdRe);
            
            %ResetGUI(app);
            %UpdateGUI(app);
            app.SF_ApplyButton.Enable = 'off';
            app.Other_ApplyButton.Enable = 'off';
            
            app.SaveRequired = 1;
            
            close(app.WaitBar);
            
            figure(app.XMapTools_GUI)
        end

        % Button pushed function: Other_MultiEquilibriumButton
        function Function_Other_MultiEquilibriumButtonPushed(app, event)
            
            MinList = app.Other_MineralList.Items;
            FctList = app.Other_FunctionList.Items;
            
            WhereMin = find(ismember(MinList,app.Other_MineralList.Value));
            WhereFct = find(ismember(FctList,app.Other_FunctionList.Value));
            
            ExtFct = char(app.ExternalFunctions.Min(app.ExternalFunctions.IdxME(WhereMin)).ME.FileName{WhereFct});
            ExtFctLabel = char(app.ExternalFunctions.Min(app.ExternalFunctions.IdxME(WhereMin)).ME.Names{WhereFct});
            
            % Read the details (Nb of minerals / additional variables)
            Min = app.ExternalFunctions.Min(app.ExternalFunctions.IdxME(WhereMin)).ME.Details(WhereFct).Min;
            AddVar = app.ExternalFunctions.Min(app.ExternalFunctions.IdxME(WhereMin)).ME.Details(WhereFct).AddVar;
            Def = app.ExternalFunctions.Min(app.ExternalFunctions.IdxME(WhereMin)).ME.Details(WhereFct).Def;
            
            % Additional variables
            if ~isempty(AddVar)
                for i = 1:length(Def)
                    Def_str{i} = num2str(Def(i));
                end
                AddVarVal_str=inputdlg(AddVar,'Input',1,Def_str);
                for i = 1:length(AddVarVal_str)
                    AddVarVal(i) = str2num(AddVarVal_str{i});
                end
                app.ExternalFunctions_AddParameters.AddVar = AddVar;
                app.ExternalFunctions_AddParameters.Values = AddVarVal;
            end
            
            % ROI
            app.ROI_EXTFCT = [];
            ROI_DeleteROI(app);
            
            DeactivatePlotZoomPanOptions(app);
            
            for i = 1:length(Min)
                switch app.Other_ROI_menu.Value
                    case 'Circle ROI'
                        DrawingMode(app,'on','Circle');
                        app.ROI_EXTFCT(i).ROI = drawcircle(app.FigMain,'Color',[0.57,0.00,0.69],'InteractionsAllowed','all','label',Min{i});
                        DrawingMode(app,'off');
                        
                    case 'Rectangle ROI'
                        DrawingMode(app,'on','Rectangle');
                        app.ROI_EXTFCT(i).ROI = drawrectangle(app.FigMain,'Color',[0.57,0.00,0.69],'InteractionsAllowed','all','label',Min{i});
                        DrawingMode(app,'off');
                        
                    case 'Polygon ROI'
                        DrawingMode(app,'on','Polygon');
                        app.ROI_EXTFCT(i).ROI = drawpolygon(app.FigMain,'Color',[0.57,0.00,0.69],'InteractionsAllowed','all','label',Min{i});
                        DrawingMode(app,'off');
                end
                app.ROI_EXTFCT_Listener = addlistener(app.ROI_EXTFCT(i).ROI, 'ROIMoved', @(varargin)EXTFCT_ROI_changed_shape(app, app.ROI_EXTFCT(i).ROI));
            end
            
            % Update the display
            EXTFCT_ROI_changed_shape(app,app.ROI_EXTFCT);
            
            app.Other_MultiEquilibriumButton.Enable = 'off';
            app.Other_ROI_menu.Enable = 'off';
            
            figure(app.XMapTools_GUI)
        end

        % Value changed function: Other_MineralList
        function Function_Other_MineralListValueChanged(app, event)
            
            
            switch app.Other_MethodList.Value
                
                case 'Map-mode'
                    
                    Idx = app.ExternalFunctions.IdxTB(find(ismember(app.ExternalFunctions.ItemsTB,app.Other_MineralList.Value)));
                    
                    app.Other_FunctionList.Items = app.ExternalFunctions.Min(Idx).TB.Name;
                    app.Other_FunctionList.Value = app.ExternalFunctions.Min(Idx).TB.Name{1};
                    
                case 'Multi-equilibrium'
                    
                    Idx = app.ExternalFunctions.IdxME(find(ismember(app.ExternalFunctions.ItemsME,app.Other_MineralList.Value)));
                    
                    app.Other_FunctionList.Items = app.ExternalFunctions.Min(Idx).ME.Names;
                    app.Other_FunctionList.Value = app.ExternalFunctions.Min(Idx).ME.Names{1};
            end
            
            
            
            
        end

        % Value changed function: Other_MethodList
        function Function_Other_MethodListValueChanged(app, event)
            UpdateGUI_Function_Other(app);
            
            Function_Other_MineralListValueChanged(app);
            
            %TreeData_MainSelectionChanged(app, event);
        end

        % Value changed function: DisplayScaleBar
        function DisplayScaleBarValueChanged(app, event)
            Menu_Plot_ResetDisplayMenuSelected(app, event);
            app.SaveRequired = 1;
        end

        % Value changed function: ResolutionField
        function ResolutionFieldValueChanged(app, event)
            Menu_Plot_ResetDisplayMenuSelected(app, event);
            app.SaveRequired = 1;
        end

        % Value changed function: ColorScaleBar
        function ColorScaleBarValueChanged(app, event)
            Menu_Plot_ResetDisplayMenuSelected(app, event);
            app.SaveRequired = 1;
        end

        % Callback function
        function Addons_BingoAntidoteOLDButtonPushed(app, event)
            
            if isdeployed
                waitfor(warndlg('Bingo-Antidote is not yet fully compatible with this version of XMapTools. We are working hard to reach full compatibility, but this will take some time! Further anouncements will be made on our website: https://xmaptools.ch','XMapTools'))
                figure(app.XMapTools_GUI);
            else
                XThermoTools_Launcher(app);
            end
            
            
            
            %keyboard
        end

        % Button pushed function: Addons_BingoAntidote_2
        function Addons_BingoAntidoteButtonPushed(app, event)
            
            % We call the bridge app (2022):
            Bridge2BA(app);
            
            
            
        end

        % Cell edit callback: Standard_UITable
        function Standard_UITableCellEdit(app, event)
            indices = event.Indices;
            newData = event.NewData;
            
            NodeData = app.TreeData_Additional.SelectedNodes.NodeData;
            
            app.XMapToolsData.Standards.DataPro(NodeData(2),indices(1)) = newData;
        end

        % Button pushed function: CompViewer_Button_Copy
        function CompViewer_Button_CopyPushed(app, event)
            
            Table2Extract = [app.CompViewer_UITable.ColumnName';app.CompViewer_UITable.Data];
            
            switch length(app.CompViewer_UITable.ColumnName)
                case 2
                    str = Table2Str_LBC(app,Table2Extract);
                case 3
                    str = Table2Str_Modes(app,Table2Extract);
                case 4
                    str = Table2Str_ME(app,Table2Extract);
            end
            
            clipboard ('copy',str);
        end

        % Button pushed function: CompViewer_Button_Save
        function CompViewer_Button_SavePushed(app, event)
            
            Table2Extract = [app.CompViewer_UITable.ColumnName';app.CompViewer_UITable.Data];
            
            
            switch app.CompViewer_Label.Text
                
                case 'Integrated LOD'
                    
                    [Success,Message,MessageID] = mkdir('Exported-LOD');
                    
                    cd Exported-LOD
                    f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
                    [Directory, pathname] = uiputfile({'*.txt', 'TXT Files (*.txt)'}, 'Export LOD results as');
                    close(f)
                    cd ..
                    
                    figure(app.XMapTools_GUI)
                    
                    if ~Directory
                        return
                    end
                    
                    str = Table2Str_LOD(app,Table2Extract);
                    
                    fid = fopen(strcat(pathname,Directory),'w');
                    fprintf(fid,'%s\n%s\n','--','Limit of detection (LOD) data exported using XMapTools');
                    fprintf(fid,'%s\n\n',datestr(now));
                    fprintf(fid,'%s\n',str);
                    fprintf(fid,'\n');
                    fclose(fid);
                    
                    
                    return
                    
                    
                    
            end
            
            switch app.TabButtonGroup.SelectedTab.Title
                
                case 'CALIBRATE'
                    
                    [Success,Message,MessageID] = mkdir('Exported-LBC');
                    
                    cd Exported-LBC
                    f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
                    [Directory, pathname] = uiputfile({'*.txt', 'TXT Files (*.txt)'}, 'Export LBC results as');
                    close(f)
                    cd ..
                    
                    figure(app.XMapTools_GUI)
                    
                    if ~Directory
                        return
                    end
                    
                    switch length(app.CompViewer_UITable.ColumnName)
                        case 2
                            str = Table2Str_LBC(app,Table2Extract);
                        case 4
                            str = Table2Str_LBC_UNC(app,Table2Extract);
                    end
                    
                    fid = fopen(strcat(pathname,Directory),'w');
                    fprintf(fid,'%s\n%s\n','--','Local bulk composition data exported using XMapTools');
                    fprintf(fid,'%s\n\n',datestr(now));
                    fprintf(fid,'%s\n',str);
                    fprintf(fid,'\n');
                    fclose(fid);
                    
                    
                    return
                    
                case 'CLASSIFY'
                    switch length(app.CompViewer_UITable.ColumnName)
                        
                        case 3
                            str = Table2Str_Modes(app,Table2Extract(1:end-1,:));
                            
                            [Success,Message,MessageID] = mkdir('Exported-Modes');
                            
                            cd Exported-Modes
                            f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
                            [Directory, pathname] = uiputfile({'*.txt', 'TXT Files (*.txt)'}, 'Export Modes results as');
                            close(f)
                            cd ..
                            
                            figure(app.XMapTools_GUI)
                            
                            if ~Directory
                                return
                            end
                            
                            fid = fopen(strcat(pathname,Directory),'w');
                            fprintf(fid,'%s\n%s\n','--','Modal abundances exported using XMapTools');
                            fprintf(fid,'%s\n\n',datestr(now));
                            
                            fprintf(fid,'%s\n',str);
                            fprintf(fid,'\n');
                            fclose(fid);
                            
                            
                    end
                    
                    return
                    
            end
            
            % Old bellow...
            
            switch length(app.CompViewer_UITable.ColumnName)
                
                
                
                case 4
                    
                    if isempty(app.CovarMatrix)
                        str = Table2Str_ME(app,Table2Extract);
                        
                        [Success,Message,MessageID] = mkdir('Exported-MultiEqui');
                        
                        cd Exported-MultiEqui
                        f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
                        [Directory, pathname] = uiputfile({'*.txt', 'TXT Files (*.txt)'}, 'Export Result as');
                        close(f)
                        cd ..
                        
                        figure(app.XMapTools_GUI)
                        
                        if ~Directory
                            return
                        end
                        
                        fid = fopen(strcat(pathname,Directory),'w');
                        fprintf(fid,'%s\n%s\n','--','Results from multi-equilibrium calculations exported using XMapTools');
                        fprintf(fid,'%s\n\n',datestr(now));
                        
                        fprintf(fid,'%s\n',str);
                        fprintf(fid,'\n');
                        fclose(fid);
                        
                    else
                        
                        str = Table2Str_ME(app,Table2Extract);
                        
                        [Success,Message,MessageID] = mkdir('Exported-StructForm');
                        
                        cd Exported-StructForm
                        f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
                        [Directory, pathname] = uiputfile({'*.txt', 'TXT Files (*.txt)'}, 'Export Result as');
                        close(f)
                        cd ..
                        
                        figure(app.XMapTools_GUI)
                        
                        if ~Directory
                            return
                        end
                        
                        fid = fopen(strcat(pathname,Directory),'w');
                        fprintf(fid,'%s\n%s\n','--','Results from strctural formula calculations exported using XMapTools');
                        fprintf(fid,'%s\n\n',datestr(now));
                        
                        fprintf(fid,'%s\n',str);
                        fprintf(fid,'\n');
                        
                        fprintf(fid,'\n%s\n','Variance-covariance data');
                        
                        for i = 1:length(app.CovarMatrix)
                            fprintf(fid,'%s\n',app.CovarMatrix(i).Name);
                            fprintf(fid,'%.10f\t%.10f\n',app.CovarMatrix(i).Means);
                            fprintf(fid,'%.10f\t%.10f\t%.10f\t%.10f\n',app.CovarMatrix(i).VarCov);
                            fprintf(fid,'%.0f\n',app.CovarMatrix(i).NbPx);
                        end
                        
                        fprintf(fid,'\n');
                        fclose(fid);
                    end
            end
            
        end

        % Value changed function: CompViewer_DensityMenu
        function CompViewer_DensityMenuValueChanged(app, event)
            LBC_ROI_changed_shape(app,app.ROI_LBC);
        end

        % Button pushed function: Button_FigMain_ZoomIn
        function Button_FigMain_ZoomInPushed2(app, event)
            
            switch app.FigMain.Toolbar.Children(3).Value
                case 'on'
                    app.FigMain.Toolbar.Children(3).Value = 'off';
                    matlab.graphics.interaction.webmodes.toggleMode(app.FigMain,'zoom',app.FigMain.Toolbar.Children(3).Value)
                case 'off'
                    app.FigMain.Toolbar.Children(3).Value = 'on';
                    matlab.graphics.interaction.webmodes.toggleMode(app.FigMain,'zoom',app.FigMain.Toolbar.Children(3).Value)
            end
            
        end

        % Button pushed function: Button_FigMain_ZoomOut
        function Button_FigMain_ZoomOutPushed2(app, event)
            
            switch app.FigMain.Toolbar.Children(4).Value
                case 'on'
                    app.FigMain.Toolbar.Children(4).Value = 'off';
                    matlab.graphics.interaction.webmodes.toggleMode(app.FigMain,'zoomout',app.FigMain.Toolbar.Children(4).Value)
                case 'off'
                    app.FigMain.Toolbar.Children(4).Value = 'on';
                    matlab.graphics.interaction.webmodes.toggleMode(app.FigMain,'zoomout',app.FigMain.Toolbar.Children(4).Value)
            end
            
        end

        % Button pushed function: Button_FigMain_Pan
        function Button_FigMain_PanPushed(app, event)
            
            switch app.FigMain.Toolbar.Children(2).Value
                case 'on'
                    app.FigMain.Toolbar.Children(2).Value = 'off';
                    matlab.graphics.interaction.webmodes.toggleMode(app.FigMain,'pan',app.FigMain.Toolbar.Children(2).Value)
                case 'off'
                    app.FigMain.Toolbar.Children(2).Value = 'on';
                    matlab.graphics.interaction.webmodes.toggleMode(app.FigMain,'pan',app.FigMain.Toolbar.Children(2).Value)
            end
            
        end

        % Button pushed function: Button_FigMain_ResetZoomPan
        function Button_FigMain_ResetZoomPanPushed(app, event)
            matlab.graphics.controls.internal.resetHelper(app.FigMain,true);
        end

        % Button pushed function: Button_FigMain_CursorInfo
        function Button_FigMain_CursorInfoPushed(app, event)
            if isequal(app.Jiahui,4)
                %warndlg('Enjoy a free sound from Jiahui','XMapTools')
                app.TreeData_Main.BackgroundColor = [0.9608    0.8549    0.8745];
                app.TreeData_Additional.BackgroundColor = [0.9608    0.8549    0.8745];
                app.MapInfo_TextArea.BackgroundColor = [1.0000    0.6510    0.7882];
                
                [y, Fs] = audioread(fullfile(app.config.xmaptools.setup_path,'Core/','sound_jiahui.mp3'));
                sound(y,Fs);
                
            else
                app.Jiahui = 0;
            end
            
            if isequal(app.DataCursorMode,1)
                app.DataCursorMode = 0;
            else
                app.DataCursorMode = 1;
            end
            
            ApplyCursorMode(app);
            
        end

        % Callback function
        function MultiplePlotMenuSelected(app, event)
            
            
            
        end

        % Callback function
        function AddMultiLayerImageMenuSelected(app, event)
            
            %keyboard
            
            
        end

        % Value changed function: Options_Medfilter3DsurfaceSpinner
        function Options_Medfilter3DsurfaceSpinnerValueChanged(app, event)
            app.SaveRequired = 1;
        end

        % Menu selected function: Image_MultiSelectionModeMenu
        function Image_MultiSelectionModeMenuSelected(app, event)
            if isequal(app.Image_MultiSelectionModeMenu.Checked,'on')
                app.Image_MultiSelectionModeMenu.Checked = 'off';
                app.TreeData_Main.Multiselect = 'off';
                app.TreeData_Main.FontColor = [0,0,0];
            else
                app.TreeData_Main.SelectedNodes = [];
                app.Image_MultiSelectionModeMenu.Checked = 'on';
                app.TreeData_Main.Multiselect = 'on';
                app.TreeData_Main.FontColor = [0,0,1];
            end
        end

        % Menu selected function: Image_AddCurrentImageMenu
        function Image_AddCurrentImageMenuSelected(app, event)
            
            ExtractData4MultiplePlot(app)
            
            app.XMapToolsData.MapData.Im.Types(end) = 1;
            app.XMapToolsData.MapData.Im.Names{end} = ['Img (single-layer) #',num2str(length(app.XMapToolsData.MapData.Im.Types == 1))];
            
            IdRe = length(app.XMapToolsData.MapData.Im.Names);
            
            p = uitreenode(app.Node_Im,'Text',char(app.XMapToolsData.MapData.Im.Names{IdRe}),'NodeData',[8,IdRe,0]);
            for j = 1:length(app.XMapToolsData.MapData.Im.Data(IdRe).Labels)
                p1 = uitreenode(p,'Text',char(app.XMapToolsData.MapData.Im.Data(IdRe).Labels{j}),'NodeData',[8,IdRe,j]);
            end
            
            app.Node_Im.expand();
            app.TreeData_Main.SelectedNodes = app.Node_Im.Children(IdRe);
            
            TreeData_MainSelectionChanged(app, event);
            
            app.SaveRequired = 1;
            
        end

        % Menu selected function: Image_AddMultiPlotImageMenu
        function Image_AddMultiPlotImageMenuSelected(app, event)
            
            ExtractData4MultiplePlot(app)
            
            app.XMapToolsData.MapData.Im.Types(end) = 2;
            app.XMapToolsData.MapData.Im.Names{end} = ['Img (multi-plot) #',num2str(length(app.XMapToolsData.MapData.Im.Types == 2))];
            
            IdRe = length(app.XMapToolsData.MapData.Im.Names);
            
            p = uitreenode(app.Node_Im,'Text',char(app.XMapToolsData.MapData.Im.Names{IdRe}),'NodeData',[8,IdRe,0]);
            for j = 1:length(app.XMapToolsData.MapData.Im.Data(IdRe).Labels)
                p1 = uitreenode(p,'Text',char(app.XMapToolsData.MapData.Im.Data(IdRe).Labels{j}),'NodeData',[8,IdRe,j]);
            end
            
            app.Node_Im.expand();
            app.TreeData_Main.SelectedNodes = app.Node_Im.Children(IdRe);
            
            Image_MultiSelectionModeMenuSelected(app, event);
            
            TreeData_MainSelectionChanged(app, event);
            
            app.SaveRequired = 1;
            
        end

        % Menu selected function: 
        % Image_AddMultiLayerImagesharedscaleMenu
        function Image_AddMultiLayerImagesharedscaleMenuSelected(app, event)
            
            ExtractData4MultiplePlot(app);
            
            app.XMapToolsData.MapData.Im.Types(end) = 3;
            app.XMapToolsData.MapData.Im.Names{end} = ['Img (multi-layer ShS) #',num2str(length(app.XMapToolsData.MapData.Im.Types == 4))];
            
            IdRe = length(app.XMapToolsData.MapData.Im.Names);
            
            p = uitreenode(app.Node_Im,'Text',char(app.XMapToolsData.MapData.Im.Names{IdRe}),'NodeData',[8,IdRe,0]);
            for j = 1:length(app.XMapToolsData.MapData.Im.Data(IdRe).Labels)
                p1 = uitreenode(p,'Text',char(app.XMapToolsData.MapData.Im.Data(IdRe).Labels{j}),'NodeData',[8,IdRe,j]);
            end
            
            app.Node_Im.expand();
            app.TreeData_Main.SelectedNodes = app.Node_Im.Children(IdRe);
            
            Image_MultiSelectionModeMenuSelected(app, event);
            
            TreeData_MainSelectionChanged(app, event);
            
            app.SaveRequired = 1;
            
        end

        % Menu selected function: Image_AddMultiLayerImageMenu
        function Image_AddMultiLayerImageMenuSelected(app, event)
            
            ExtractData4MultiplePlot(app);
            
            app.XMapToolsData.MapData.Im.Types(end) = 4;
            app.XMapToolsData.MapData.Im.Names{end} = ['Img (multi-layer MuS) #',num2str(length(app.XMapToolsData.MapData.Im.Types == 4))];
            
            IdRe = length(app.XMapToolsData.MapData.Im.Names);
            
            p = uitreenode(app.Node_Im,'Text',char(app.XMapToolsData.MapData.Im.Names{IdRe}),'NodeData',[8,IdRe,0]);
            for j = 1:length(app.XMapToolsData.MapData.Im.Data(IdRe).Labels)
                p1 = uitreenode(p,'Text',char(app.XMapToolsData.MapData.Im.Data(IdRe).Labels{j}),'NodeData',[8,IdRe,j]);
            end
            
            app.Node_Im.expand();
            app.TreeData_Main.SelectedNodes = app.Node_Im.Children(IdRe);
            
            Image_MultiSelectionModeMenuSelected(app, event)
            
            TreeData_MainSelectionChanged(app, event);
            
            app.SaveRequired = 1;
            
        end

        % Menu selected function: PlotMatrixMenu
        function PlotMatrixMenuSelected(app, event)
            CorrelationPlot(app,1);
        end

        % Menu selected function: PlotScatterplotMatrixMenu
        function PlotScatterplotMatrixMenuSelected(app, event)
            CorrelationPlot(app,3);
        end

        % Menu selected function: PlotMatrixValuesMenu
        function PlotMatrixValuesMenuSelected(app, event)
            CorrelationPlot(app,2);
        end

        % Button down function: FigHistLive
        function FigHistLiveButtonDown(app, event)
            for k=1:3
                xVertLineCoord = get(app.hVerticalLines(k),'Value');
                click_buffer=(max(app.FigHistLive.XLim)-min(app.FigHistLive.XLim))*0.02; %this is the value that makes it easier to click on the line 0.005 worked good for me
                if abs(event.IntersectionPoint(1) - xVertLineCoord(1)) < click_buffer
                    app.hLineToDrag = app.hVerticalLines(k);
                    break;
                end
            end
        end

        % Key release function: XMapTools_GUI
        function XMapTools_GUIKeyRelease(app, event)
            key = event.Key;
        end

        % Value changed function: EditField_LivePosition
        function EditField_LivePositionValueChanged(app, event)
            app.hVerticalLines(1).Value = app.EditField_LivePosition.Value;
        end

        % Menu selected function: LicenseMenu
        function LicenseMenuSelected(app, event)
            if isempty(app.Id_HelpTool)
                Help_Display(app,'License.html');
            else
                app.Id_HelpTool.UpdateTextHelp('License.html');
            end
        end

        % Menu selected function: Workspace_ProjectImportMenu
        function Workspace_ProjectImportMenuSelected(app, event)
            app.TabButtonGroup.SelectedTab = app.PROJECTIMPORTTab;
            TabButtonGroupSelectionChanged(app);
        end

        % Menu selected function: Workspace_ClassifyMenu
        function Workspace_ClassifyMenuSelected(app, event)
            app.TabButtonGroup.SelectedTab = app.CLASSIFYTab;
            TabButtonGroupSelectionChanged(app);
        end

        % Menu selected function: Workspace_CalibrateMenu
        function Workspace_CalibrateMenuSelected(app, event)
            app.TabButtonGroup.SelectedTab = app.CALIBRATETab;
            TabButtonGroupSelectionChanged(app);
        end

        % Menu selected function: Workspace_FunctionsMenu
        function Workspace_FunctionsMenuSelected(app, event)
            app.TabButtonGroup.SelectedTab = app.FUNCTIONSTab;
            TabButtonGroupSelectionChanged(app);
        end

        % Menu selected function: Workspace_SegmentCTMenu
        function Workspace_SegmentCTMenuSelected(app, event)
            app.TabButtonGroup.SelectedTab = app.SEGMENTCTTab;
            TabButtonGroupSelectionChanged(app);
        end

        % Menu selected function: Workspace_AddonsMenu
        function Workspace_AddonsMenuSelected(app, event)
            app.TabButtonGroup.SelectedTab = app.ADDONSTab;
            TabButtonGroupSelectionChanged(app);
        end

        % Menu selected function: Workspace_OptionsMenu
        function Workspace_OptionsMenuSelected(app, event)
            app.TabButtonGroup.SelectedTab = app.OPTIONSTab;
            TabButtonGroupSelectionChanged(app);
        end

        % Menu selected function: Workspace_DeveloperMenu
        function Workspace_DeveloperMenuSelected(app, event)
            app.TabButtonGroup.SelectedTab = app.DEVELOPERTab;
            TabButtonGroupSelectionChanged(app);
        end

        % Button pushed function: CompViewer_Button_DeleteROI
        function CompViewer_Button_DeleteROIPushed(app, event)
            ROI_DeleteROI(app);
        end

        % Button pushed function: Calibrate_LOD_CalcButton
        function Calibrate_LOD_CalcButtonPushed(app, event)
            
            ROI_DeleteROI(app);
            
            app.CompViewer_Label.Text = 'Integrated LOD';
            
            switch app.Calibrate_LOD_menu.Value
                case 'Rectangle ROI'
                    DrawingMode(app,'on','Rectangle');
                    app.ROI_LOD = drawrectangle(app.FigMain,'Color',[0.57,0.00,0.69],'InteractionsAllowed','all');
                    DrawingMode(app,'off');
                    
                case 'Polygon ROI'
                    DrawingMode(app,'on','Polygon');
                    app.ROI_LOD = drawpolygon(app.FigMain,'Color',[0.57,0.00,0.69],'InteractionsAllowed','all');
                    DrawingMode(app,'off');
            end
            
            app.ROI_LOD_Listener = addlistener(app.ROI_LOD, 'ROIMoved', @(varargin)LOD_ROI_changed_shape(app, app.ROI_LOD));
            
            LOD_ROI_changed_shape(app,app.ROI_LOD);
            
            app.TabGroup.SelectedTab = app.CompositionTab;
            
            app.SaveRequired = 1;
            
        end

        % Menu selected function: ApplyColorsAutoMenu
        function ApplyColorsAutoMenuSelected(app, event)
            NodeData = app.TreeData_Additional.SelectedNodes.NodeData;
            
            switch NodeData(1)
                case 11  % Export MaskFile
                    
                    MaskId = NodeData(2);
                    
                    Colors4Mask = UpdateColorsMasks(app,app.XMapToolsData.MapData.MaskFile.Masks(MaskId).Names);
                    app.XMapToolsData.MapData.MaskFile.Masks(MaskId).Colors = Colors4Mask;
                    
                    PlotMaskFile(app, MaskId);
            end
            
        end

        % Menu selected function: EditColorsManualMenu
        function EditColorsManualMenuSelected(app, event)
            NodeData = app.TreeData_Additional.SelectedNodes.NodeData;
            
            switch NodeData(1)
                case 11  % Export MaskFile
                    
                    MaskId = NodeData(2);
                    app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
                    app.WaitBar.Message = 'Close the Editor window to resume XMapTools';
                    waitfor(MaskColorEditor(app,MaskId));
                    close(app.WaitBar)
                    figure(app.XMapTools_GUI)
                    
                    PlotMaskFile(app, MaskId);
                    
            end
        end

        % Menu selected function: ExportMaskFileMenu
        function ExportMaskFileMenuSelected(app, event)
            ContextMenu_AdditionalTree_ExportPushed(app);
        end

        % Value changed function: FieldAngleView
        function FieldAngleViewValueChanged(app, event)
            value = app.FieldAngleView.Value;
            
        end

        % Menu selected function: UpdateElementOxideIndexationMenu
        function UpdateElementOxideIndexationMenuSelected(app, event)
            
            % This function offers the possibility to check and update
            % element indices of quantitative datasets (Quanti & Merged)
            
            NodeData = app.TreeData_Main.SelectedNodes.NodeData;
            
            switch NodeData(1)
                
                case 2 % Quanti
                    
                    BackElInd = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElInd;
                    if isfield(app.XMapToolsData.MapData.Qt.Data,'OxInd')
                        BackOxInd = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).OxInd;
                    else
                        BackOxInd = zeros(size(BackElInd));
                    end
                    
                    app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElInd = zeros(size(app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElNames));
                    app.XMapToolsData.MapData.Qt.Data(NodeData(2)).OxInd = zeros(size(app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElNames));
                    
                    Warning = zeros(size(length(app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElNames)));
                    
                    % Search for elements/oxides
                    for i = 1:length(app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElNames)
                        
                        NameForIndex = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElNames{i};
                        
                        Idx = find(ismember(app.ElOxDataDef.ElList,NameForIndex));
                        
                        if ~isempty(Idx)
                            app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElInd(i) = Idx;
                        else
                            Idx = find(ismember(app.ElOxDataDef.OxList,NameForIndex));
                            if ~isempty(Idx)
                                app.XMapToolsData.MapData.Qt.Data(NodeData(2)).OxInd(i) = Idx;
                                app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElInd(i) = app.ElOxDataDef.OxElIdx(Idx);   % 13.04.23 when importing oxides, the corresponding elements must be indexed
                            else
                                Warning(i) = 1;
                            end
                        end
                    end
                    
                    NewElInd = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).ElInd;
                    NewOxInd = app.XMapToolsData.MapData.Qt.Data(NodeData(2)).OxInd;
                    
                    disp(' ')
                    disp('-------------------------------------------------')
                    disp('Element Indices: (old | new)')
                    disp(BackElInd)
                    disp(NewElInd)
                    disp('Oxide Indices: (old | new)')
                    disp(BackOxInd)
                    disp(NewOxInd)
                    
                case 3 % Merged
                    
                    BackElInd = app.XMapToolsData.MapData.Me.Data(NodeData(2)).ElInd;
                    if isfield(app.XMapToolsData.MapData.Me.Data,'OxInd')
                        BackOxInd = app.XMapToolsData.MapData.Me.Data(NodeData(2)).OxInd;
                    else
                        BackOxInd = zeros(size(BackElInd));
                    end
                    
                    app.XMapToolsData.MapData.Me.Data(NodeData(2)).ElInd = zeros(size(app.XMapToolsData.MapData.Me.Data(NodeData(2)).ElNames));
                    app.XMapToolsData.MapData.Me.Data(NodeData(2)).OxInd = zeros(size(app.XMapToolsData.MapData.Me.Data(NodeData(2)).ElNames));
                    
                    Warning = zeros(size(length(app.XMapToolsData.MapData.Me.Data(NodeData(2)).ElNames)));
                    
                    % Search for elements/oxides
                    for i = 1:length(app.XMapToolsData.MapData.Me.Data(NodeData(2)).ElNames)
                        
                        NameForIndex = app.XMapToolsData.MapData.Me.Data(NodeData(2)).ElNames{i};
                        
                        Idx = find(ismember(app.ElOxDataDef.ElList,NameForIndex));
                        
                        if ~isempty(Idx)
                            app.XMapToolsData.MapData.Me.Data(NodeData(2)).ElInd(i) = Idx;
                        else
                            Idx = find(ismember(app.ElOxDataDef.OxList,NameForIndex));
                            if ~isempty(Idx)
                                app.XMapToolsData.MapData.Me.Data(NodeData(2)).OxInd(i) = Idx;
                                app.XMapToolsData.MapData.Me.Data(NodeData(2)).ElInd(i) = app.ElOxDataDef.OxElIdx(Idx);   % 13.04.23 when importing oxides, the corresponding elements must be indexed
                            else
                                Warning(i) = 1;
                            end
                        end
                    end
                    
                    NewElInd = app.XMapToolsData.MapData.Me.Data(NodeData(2)).ElInd;
                    NewOxInd = app.XMapToolsData.MapData.Me.Data(NodeData(2)).OxInd;
                    
                    disp(' ')
                    disp('-------------------------------------------------')
                    disp('Element Indices: (old | new)')
                    disp(BackElInd)
                    disp(NewElInd)
                    disp('Oxide Indices: (old | new)')
                    disp(BackOxInd)
                    disp(NewOxInd)
                    
            end
            
            app.SaveRequired = 1;
            
        end

        % Menu selected function: ReorderMenu
        function ReorderMenuSelected(app, event)
            Reorder = 0;
            
            if ~isempty(app.TreeData_Additional.SelectedNodes)
                if isequal(app.TreeData_Additional.SelectedNodes.NodeData(1),11) && app.TreeData_Additional.SelectedNodes.NodeData(2) > 0
                    
                    SelectedMaskFile = app.TreeData_Additional.SelectedNodes.NodeData(2);
                    Mode = 'MaskFile';
                    
                    app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
                    app.WaitBar.Message = 'Close the Rearranger window to resume XMapTools';
                    waitfor(Rearranger(app,Mode,SelectedMaskFile));
                    close(app.WaitBar)
                    
                    Reorder = 1;
                    app.SaveRequired = 1;
                end
            end
            
            
            
            if isequal(Reorder,0)
                uialert(app.XMapTools_GUI,{'Select a dataset to reorder. This functionality is only available for the following items: ',' - Maskfile'},'XMapTools');
            end
            
        end

        % Button pushed function: Calibrate_ApplyLODfilter
        function Calibrate_ApplyLODfilterButtonPushed(app, event)
            
            return
            
            NodeData = app.TreeData_Main.SelectedNodes.NodeData;
            
            SelQt = app.XMapToolsData.MapData.Qt.Names{NodeData(2)};
            
            LODfiles = app.XMapToolsData.MapLOD.Names;
            
            WhereLOD = find(ismember(LODfiles,SelQt));
            ElNamesLOD = app.XMapToolsData.MapLOD.Data.ElNames;
            
            if length(WhereLOD) > 1
                WhereLOD = WhereLOD(end);
            end
            
            if isequal(size(WhereLOD),[1,1])
                
                % Duplicate quanti file
                app.XMapToolsData.MapData.Qt.Names{end+1} = [char(app.XMapToolsData.MapData.Qt.Names{NodeData(2)}),'_filtered_LOD'];
                app.XMapToolsData.MapData.Qt.IsOxide(end+1) = app.XMapToolsData.MapData.Qt.IsOxide(NodeData(2));
                app.XMapToolsData.MapData.Qt.MaskFile{end+1}  = app.XMapToolsData.MapData.Qt.MaskFile{NodeData(2)};
                app.XMapToolsData.MapData.Qt.NbCalibPoints(end+1) = app.XMapToolsData.MapData.Qt.NbCalibPoints(NodeData(2));
                app.XMapToolsData.MapData.Qt.Data(end+1) = app.XMapToolsData.MapData.Qt.Data(NodeData(2));
                
                % Filter for LOD
                ElNamesMap = app.XMapToolsData.MapData.Qt.Data(end).ElNames;
                
                disp(' '), disp('LOD filtering:')
                for i = 1:length(app.XMapToolsData.MapData.Qt.Data(end).ElNames)
                    WhereEl = find(ismember(ElNamesLOD,ElNamesMap{i}));
                    if length(WhereEl) > 1
                        WhereEl = WhereEl(1);
                    end
                    NewMap = app.XMapToolsData.MapData.Qt.Data(end).CData(i).Map;
                    
                    % Here we should find everything BDL and replace by
                    % NaN or zero?
                    SelPx = find(NewMap > 0);
                    BDLidx = [];
                    if ~isempty(SelPx)
                        BDLidx = find(NewMap(SelPx) < app.XMapToolsData.MapLOD.Data.ElData(WhereEl).Map_LOD(SelPx));
                    end
                    
                    if ~isempty(BDLidx)
                        NewMap(SelPx(BDLidx)) = NaN;
                        disp([char(ElNamesMap{i}),': ',num2str(length(BDLidx)),' px below detection limit (',num2str(length(BDLidx)/length(SelPx)*100),'%)'])
                    end
                    
                    app.XMapToolsData.MapData.Qt.Data(end).CData(i).Map = NewMap;
                end
                disp('----')
                
                % Add new quanti
                Pos = length(app.XMapToolsData.MapData.Qt.Names);
                p = uitreenode(app.Node_Qt,'Text',char(app.XMapToolsData.MapData.Qt.Names{Pos}),'NodeData',[2,Pos,0]);
                
                for i = 1:length(app.XMapToolsData.MapData.Qt.Data(Pos).ElNames)
                    p1 = uitreenode(p,'Text',char(app.XMapToolsData.MapData.Qt.Data(Pos).ElNames{i}),'NodeData',[2,Pos,i]);
                end
                
            end
            
        end

        % Menu selected function: DriftCorrectionMenu
        function DriftCorrectionMenuSelected(app, event)
            
            % This feature is not fully implemented yet for 4.3 beta 1
            %
            % return
            
            NodeData = app.TreeData_Main.SelectedNodes.NodeData;
            
            NoSelectedMaskFile = 0;
            if isempty(app.TreeData_Additional.SelectedNodes)
                NoSelectedMaskFile = 1;
            else
                AddNodeData = app.TreeData_Additional.SelectedNodes.NodeData;
                if ~isequal(AddNodeData(1),11) || isequal(AddNodeData(2),0)
                    NoSelectedMaskFile = 1;
                end
            end
            
            if NoSelectedMaskFile
                uialert(app.XMapTools_GUI,'No maskfile selected, select a maskfile to calculate a drift correction','XMapTools Error')
                return
            end
            
            DriftCorrector(app,NodeData(2),AddNodeData(2));
            
            
            %keyboard
            
        end

        % Menu selected function: HoldROIonMenu
        function HoldROIonMenuSelected(app, event)
            if app.HoldROIonMenu.Checked
                app.HoldROIonMenu.Checked = 0;
            else
                app.HoldROIonMenu.Checked = 1;
            end
        end

        % Value changed function: PlotEngineDropDown
        function PlotEngineDropDownValueChanged(app, event)
            ApplySettingsPlotEngine(app);
        end

        % Menu selected function: DuplicateandAdjustMergedMenu
        function DuplicateandAdjustMergedMenuSelected(app, event)
            
            NodeData = app.TreeData_Main.SelectedNodes.NodeData;
            
            switch NodeData(1)
                
                case 3
                    
                    prompt={'Min', 'Max', 'sigma'};
                    name='Adjust Limits to:';
                    numlines=1;
                    defaultanswer={'0', '100','0.5'};
                    
                    Answer = inputdlg(prompt,name,numlines,defaultanswer);
                    
                    figure(app.XMapTools_GUI);
                    
                    MinValue = str2num(Answer{1});
                    MaxValue = str2num(Answer{2});
                    SigmaValue = str2num(Answer{3});
                    
                    app.XMapToolsData.MapData.Me.Names{end+1} = [char(app.XMapToolsData.MapData.Me.Names(NodeData(2))),'_adjusted'];
                    app.XMapToolsData.MapData.Me.IsOxide(end+1) = app.XMapToolsData.MapData.Me.IsOxide(NodeData(2));
                    app.XMapToolsData.MapData.Me.MaskFile(end+1) = app.XMapToolsData.MapData.Me.MaskFile(NodeData(2));
                    app.XMapToolsData.MapData.Me.NbCalibPoints(end+1) = app.XMapToolsData.MapData.Me.NbCalibPoints(NodeData(2));
                    app.XMapToolsData.MapData.Me.Data(end+1) = app.XMapToolsData.MapData.Me.Data(NodeData(2));
                    
                    Data = zeros(size(app.XMapToolsData.MapData.Me.Data(end).CData(1).Map,1),size(app.XMapToolsData.MapData.Me.Data(end).CData(1).Map,2),length(app.XMapToolsData.MapData.Me.Data(end).CData));
                    
                    for i = 1:size(Data,3)
                        Data(:,:,i) = app.XMapToolsData.MapData.Me.Data(end).CData(i).Map;
                    end
                    
                    if isequal(app.XMapToolsData.MapData.Me.Data(end).ElNames{end},'Total(wt%)')
                        DataSum = Data(:,:,end);
                    else
                        DataSum = sum(Data,3);
                    end
                    
                    WhereHigh = find(DataSum > MaxValue);
                    WhereLow = find(DataSum < MinValue);
                    
                    SimulMax = MaxValue + SigmaValue .* randn(length(WhereHigh),1);
                    SimulMin = MinValue + SigmaValue .* randn(length(WhereLow),1);
                    
                    NormHigh = SimulMax ./ DataSum(WhereHigh);
                    NormLow = SimulMin ./ DataSum(WhereLow);
                    
                    for i = 1:size(Data,3)
                        MapTemp = Data(:,:,i);
                        MapTemp(WhereHigh) = MapTemp(WhereHigh) .* NormHigh;
                        MapTemp(WhereLow) = MapTemp(WhereLow) .* NormLow;
                        Data(:,:,i) = MapTemp;
                    end
                    
                    for i = 1:size(Data,3)
                        app.XMapToolsData.MapData.Me.Data(end).CData(i).Map = Data(:,:,i);
                    end
                    
                    % Update:
                    i = length(app.XMapToolsData.MapData.Me.Names);
                    p = uitreenode(app.Node_Me,'Text',char(app.XMapToolsData.MapData.Me.Names{i}),'NodeData',[3,i,0]);
                    
                    for j = 1:length(app.XMapToolsData.MapData.Me.Data(i).ElNames)
                        p1 = uitreenode(p,'Text',char(app.XMapToolsData.MapData.Me.Data(i).ElNames{j}),'NodeData',[3,i,j]);
                    end
                    
            end
            
            app.SaveRequired = 1;
            
            
            
            %keyboard
            
        end

        % Button pushed function: Tool_ExportCompositions
        function Tool_ExportCompositionsButtonPushed(app, event)
            
            Data_Export(app);
            
            
            
            
            
            
            
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create XMapTools_GUI and hide until all components are created
            app.XMapTools_GUI = uifigure('Visible', 'off');
            app.XMapTools_GUI.AutoResizeChildren = 'off';
            app.XMapTools_GUI.Position = [100 100 1401 790];
            app.XMapTools_GUI.Name = 'XMapTools';
            app.XMapTools_GUI.Icon = 'xmaptools_ios_icon_HR.png';
            app.XMapTools_GUI.CloseRequestFcn = createCallbackFcn(app, @XMapTools_GUICloseRequest, true);
            app.XMapTools_GUI.SizeChangedFcn = createCallbackFcn(app, @updateAppLayout, true);
            app.XMapTools_GUI.WindowButtonDownFcn = createCallbackFcn(app, @XMapTools_GUIWindowButtonDown, true);
            app.XMapTools_GUI.WindowButtonUpFcn = createCallbackFcn(app, @XMapTools_GUIWindowButtonUp, true);
            app.XMapTools_GUI.WindowButtonMotionFcn = createCallbackFcn(app, @XMapTools_GUIWindowButtonMotion, true);
            app.XMapTools_GUI.ButtonDownFcn = createCallbackFcn(app, @XMapTools_GUIButtonDown, true);
            app.XMapTools_GUI.KeyReleaseFcn = createCallbackFcn(app, @XMapTools_GUIKeyRelease, true);

            % Create GridLayout
            app.GridLayout = uigridlayout(app.XMapTools_GUI);
            app.GridLayout.ColumnWidth = {205, '1x', 275};
            app.GridLayout.RowHeight = {'1x'};
            app.GridLayout.ColumnSpacing = 0;
            app.GridLayout.RowSpacing = 0;
            app.GridLayout.Padding = [0 0 0 0];
            app.GridLayout.Scrollable = 'on';

            % Create LeftPanel
            app.LeftPanel = uipanel(app.GridLayout);
            app.LeftPanel.Layout.Row = 1;
            app.LeftPanel.Layout.Column = 1;

            % Create GridLayout7
            app.GridLayout7 = uigridlayout(app.LeftPanel);
            app.GridLayout7.ColumnWidth = {'1x'};
            app.GridLayout7.RowHeight = {'0.04x', '1x', '0.04x', '1x'};
            app.GridLayout7.RowSpacing = 2;
            app.GridLayout7.Padding = [3 3 3 3];

            % Create TreeData_Main
            app.TreeData_Main = uitree(app.GridLayout7);
            app.TreeData_Main.SelectionChangedFcn = createCallbackFcn(app, @TreeData_MainSelectionChanged, true);
            app.TreeData_Main.NodeTextChangedFcn = createCallbackFcn(app, @TreeData_MainNodeTextChanged, true);
            app.TreeData_Main.Editable = 'on';
            app.TreeData_Main.FontSize = 10;
            app.TreeData_Main.Tooltip = {''};
            app.TreeData_Main.Layout.Row = 2;
            app.TreeData_Main.Layout.Column = 1;

            % Create Node_It
            app.Node_It = uitreenode(app.TreeData_Main);
            app.Node_It.NodeData = [1 0];
            app.Node_It.Icon = '105-photos.png';
            app.Node_It.Text = 'Intensity';

            % Create Node_Qt
            app.Node_Qt = uitreenode(app.TreeData_Main);
            app.Node_Qt.NodeData = [2 0];
            app.Node_Qt.Icon = '220-layers.png';
            app.Node_Qt.Text = 'Quanti';

            % Create Node_Me
            app.Node_Me = uitreenode(app.TreeData_Main);
            app.Node_Me.NodeData = [3 0];
            app.Node_Me.Icon = '236-folder.png';
            app.Node_Me.Text = 'Merged';

            % Create Node_Re
            app.Node_Re = uitreenode(app.TreeData_Main);
            app.Node_Re.NodeData = [4 0];
            app.Node_Re.Icon = '321-exit.png';
            app.Node_Re.Text = 'Results';

            % Create Node_Ot
            app.Node_Ot = uitreenode(app.TreeData_Main);
            app.Node_Ot.NodeData = [5 0];
            app.Node_Ot.Icon = '004-picture.png';
            app.Node_Ot.Text = 'Other';

            % Create Node_Ct
            app.Node_Ct = uitreenode(app.TreeData_Main);
            app.Node_Ct.NodeData = [6 0];
            app.Node_Ct.Icon = '119-database.png';
            app.Node_Ct.Text = 'CT-data';

            % Create Node_ROI
            app.Node_ROI = uitreenode(app.TreeData_Main);
            app.Node_ROI.NodeData = [7 0];
            app.Node_ROI.Icon = '007-television.png';
            app.Node_ROI.Text = 'ROI';

            % Create Node_Im
            app.Node_Im = uitreenode(app.TreeData_Main);
            app.Node_Im.NodeData = [8 0 0];
            app.Node_Im.Icon = '006-video player.png';
            app.Node_Im.Text = 'Images';

            % Create TreeData_Additional
            app.TreeData_Additional = uitree(app.GridLayout7);
            app.TreeData_Additional.Multiselect = 'on';
            app.TreeData_Additional.SelectionChangedFcn = createCallbackFcn(app, @TreeData_AdditionalSelectionChanged, true);
            app.TreeData_Additional.NodeTextChangedFcn = createCallbackFcn(app, @TreeData_AdditionalNodeTextChanged, true);
            app.TreeData_Additional.Editable = 'on';
            app.TreeData_Additional.FontSize = 10;
            app.TreeData_Additional.Layout.Row = 4;
            app.TreeData_Additional.Layout.Column = 1;

            % Create Node_Masks
            app.Node_Masks = uitreenode(app.TreeData_Additional);
            app.Node_Masks.NodeData = [11 0 0];
            app.Node_Masks.Icon = '165-layers.png';
            app.Node_Masks.Text = 'Mask files';

            % Create Node_TrainingSet
            app.Node_TrainingSet = uitreenode(app.TreeData_Additional);
            app.Node_TrainingSet.NodeData = [12 0 0];
            app.Node_TrainingSet.Icon = '090-share.png';
            app.Node_TrainingSet.Text = 'Training Sets (Classification)';

            % Create Node_Corrections
            app.Node_Corrections = uitreenode(app.TreeData_Additional);
            app.Node_Corrections.NodeData = [13 0 0];
            app.Node_Corrections.Icon = '116-database.png';
            app.Node_Corrections.Text = 'Segmentation & Corrections';

            % Create Node_SegScheme
            app.Node_SegScheme = uitreenode(app.Node_Corrections);
            app.Node_SegScheme.NodeData = [13 1 0];
            app.Node_SegScheme.Icon = '118-database.png';
            app.Node_SegScheme.Text = 'Schemes (Segmentation)';

            % Create Node_Filters
            app.Node_Filters = uitreenode(app.Node_Corrections);
            app.Node_Filters.NodeData = [13 2 0];
            app.Node_Filters.Icon = '079-calculator.png';
            app.Node_Filters.Text = 'Correction Filters';

            % Create Node_Standards
            app.Node_Standards = uitreenode(app.TreeData_Additional);
            app.Node_Standards.NodeData = [14 0 0];
            app.Node_Standards.Icon = '208-settings.png';
            app.Node_Standards.Text = 'Standards (Spots)';

            % Create Node_MapStandards
            app.Node_MapStandards = uitreenode(app.TreeData_Additional);
            app.Node_MapStandards.NodeData = [15 0 0];
            app.Node_MapStandards.Icon = '207-settings.png';
            app.Node_MapStandards.Text = 'Standards (Maps)';

            % Create Node_LOD
            app.Node_LOD = uitreenode(app.TreeData_Additional);
            app.Node_LOD.NodeData = [16 0 0 0];
            app.Node_LOD.Icon = '030-eject.png';
            app.Node_LOD.Text = 'LOD';

            % Create PrimaryTreeMenuLabel
            app.PrimaryTreeMenuLabel = uilabel(app.GridLayout7);
            app.PrimaryTreeMenuLabel.VerticalAlignment = 'bottom';
            app.PrimaryTreeMenuLabel.FontSize = 11;
            app.PrimaryTreeMenuLabel.FontAngle = 'italic';
            app.PrimaryTreeMenuLabel.Layout.Row = 1;
            app.PrimaryTreeMenuLabel.Layout.Column = 1;
            app.PrimaryTreeMenuLabel.Text = 'Primary Tree Menu';

            % Create SecondaryTreeMenuLabel
            app.SecondaryTreeMenuLabel = uilabel(app.GridLayout7);
            app.SecondaryTreeMenuLabel.VerticalAlignment = 'bottom';
            app.SecondaryTreeMenuLabel.FontSize = 11;
            app.SecondaryTreeMenuLabel.FontAngle = 'italic';
            app.SecondaryTreeMenuLabel.Layout.Row = 3;
            app.SecondaryTreeMenuLabel.Layout.Column = 1;
            app.SecondaryTreeMenuLabel.Text = 'Secondary Tree Menu';

            % Create CenterPanel
            app.CenterPanel = uipanel(app.GridLayout);
            app.CenterPanel.Layout.Row = 1;
            app.CenterPanel.Layout.Column = 2;

            % Create GridLayout2
            app.GridLayout2 = uigridlayout(app.CenterPanel);
            app.GridLayout2.ColumnWidth = {'0.05x', '1.1x', '0.5x'};
            app.GridLayout2.RowHeight = {'1x', '3x', '2x', '0.3x'};
            app.GridLayout2.ColumnSpacing = 2.5;
            app.GridLayout2.RowSpacing = 1.5;
            app.GridLayout2.Padding = [3 3 3 3];

            % Create TabButtonGroup
            app.TabButtonGroup = uitabgroup(app.GridLayout2);
            app.TabButtonGroup.AutoResizeChildren = 'off';
            app.TabButtonGroup.SelectionChangedFcn = createCallbackFcn(app, @TabButtonGroupSelectionChanged, true);
            app.TabButtonGroup.Layout.Row = 1;
            app.TabButtonGroup.Layout.Column = [1 3];

            % Create PROJECTIMPORTTab
            app.PROJECTIMPORTTab = uitab(app.TabButtonGroup);
            app.PROJECTIMPORTTab.AutoResizeChildren = 'off';
            app.PROJECTIMPORTTab.Title = 'PROJECT & IMPORT';
            app.PROJECTIMPORTTab.BackgroundColor = [0.9412 0.9412 0.9412];

            % Create GridLayout_ImportTab
            app.GridLayout_ImportTab = uigridlayout(app.PROJECTIMPORTTab);
            app.GridLayout_ImportTab.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.3x', '1x', '1x', '1x', '1x', '1x', '0.3x', '1x', '1x', '1x', '1x', '0.3x', '1x', '1x', '1x', '1x', '1x', '0.3x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.3x', '1x', '0.15x'};
            app.GridLayout_ImportTab.RowHeight = {'1x', '1x', '1x', '0.6x'};
            app.GridLayout_ImportTab.ColumnSpacing = 4;
            app.GridLayout_ImportTab.RowSpacing = 4;
            app.GridLayout_ImportTab.Padding = [5 5 5 5];

            % Create ProjectOpen
            app.ProjectOpen = uibutton(app.GridLayout_ImportTab, 'push');
            app.ProjectOpen.ButtonPushedFcn = createCallbackFcn(app, @ProjectOpenButtonPushed, true);
            app.ProjectOpen.Icon = '241-folder.png';
            app.ProjectOpen.IconAlignment = 'top';
            app.ProjectOpen.FontSize = 10;
            app.ProjectOpen.Tooltip = {'Open Project'};
            app.ProjectOpen.Layout.Row = [1 2];
            app.ProjectOpen.Layout.Column = [1 2];
            app.ProjectOpen.Text = 'Open';

            % Create ProjectSave
            app.ProjectSave = uibutton(app.GridLayout_ImportTab, 'push');
            app.ProjectSave.ButtonPushedFcn = createCallbackFcn(app, @ProjectSaveButtonPushed, true);
            app.ProjectSave.Icon = '022-save.png';
            app.ProjectSave.IconAlignment = 'top';
            app.ProjectSave.FontSize = 10;
            app.ProjectSave.Tooltip = {'Save Project'};
            app.ProjectSave.Layout.Row = [1 2];
            app.ProjectSave.Layout.Column = [3 4];
            app.ProjectSave.Text = 'Save';

            % Create ProjectSaveAs
            app.ProjectSaveAs = uibutton(app.GridLayout_ImportTab, 'push');
            app.ProjectSaveAs.ButtonPushedFcn = createCallbackFcn(app, @ProjectSaveAsButtonPushed, true);
            app.ProjectSaveAs.Icon = '022-save-as.png';
            app.ProjectSaveAs.IconAlignment = 'center';
            app.ProjectSaveAs.Tooltip = {'Save Project As ...'};
            app.ProjectSaveAs.Layout.Row = 1;
            app.ProjectSaveAs.Layout.Column = 5;
            app.ProjectSaveAs.Text = '';

            % Create PROJECTLabel
            app.PROJECTLabel = uilabel(app.GridLayout_ImportTab);
            app.PROJECTLabel.HorizontalAlignment = 'center';
            app.PROJECTLabel.VerticalAlignment = 'bottom';
            app.PROJECTLabel.FontSize = 9;
            app.PROJECTLabel.FontColor = [0.149 0.149 0.149];
            app.PROJECTLabel.Layout.Row = 4;
            app.PROJECTLabel.Layout.Column = [1 7];
            app.PROJECTLabel.Text = 'PROJECT';

            % Create Label
            app.Label = uilabel(app.GridLayout_ImportTab);
            app.Label.HorizontalAlignment = 'right';
            app.Label.FontSize = 10;
            app.Label.Layout.Row = 3;
            app.Label.Layout.Column = 2;
            app.Label.Text = '';

            % Create ProjectName_DisplayField
            app.ProjectName_DisplayField = uieditfield(app.GridLayout_ImportTab, 'text');
            app.ProjectName_DisplayField.Editable = 'off';
            app.ProjectName_DisplayField.HorizontalAlignment = 'center';
            app.ProjectName_DisplayField.FontSize = 10;
            app.ProjectName_DisplayField.Layout.Row = 3;
            app.ProjectName_DisplayField.Layout.Column = [1 6];
            app.ProjectName_DisplayField.Value = 'project not saved yet ...';

            % Create DATACONVERSIONLabel
            app.DATACONVERSIONLabel = uilabel(app.GridLayout_ImportTab);
            app.DATACONVERSIONLabel.HorizontalAlignment = 'center';
            app.DATACONVERSIONLabel.VerticalAlignment = 'bottom';
            app.DATACONVERSIONLabel.FontSize = 9;
            app.DATACONVERSIONLabel.FontColor = [0.149 0.149 0.149];
            app.DATACONVERSIONLabel.Layout.Row = 4;
            app.DATACONVERSIONLabel.Layout.Column = [9 13];
            app.DATACONVERSIONLabel.Text = 'DATA CONVERSION';

            % Create ProjectName_Options
            app.ProjectName_Options = uidropdown(app.GridLayout_ImportTab);
            app.ProjectName_Options.Items = {'File', 'Path'};
            app.ProjectName_Options.ValueChangedFcn = createCallbackFcn(app, @ProjectName_OptionsValueChanged, true);
            app.ProjectName_Options.Layout.Row = 3;
            app.ProjectName_Options.Layout.Column = 7;
            app.ProjectName_Options.Value = 'File';

            % Create ButtonImportMaps
            app.ButtonImportMaps = uibutton(app.GridLayout_ImportTab, 'push');
            app.ButtonImportMaps.ButtonPushedFcn = createCallbackFcn(app, @Import_ButtonImportMapsPushed, true);
            app.ButtonImportMaps.Icon = '323-add.png';
            app.ButtonImportMaps.IconAlignment = 'top';
            app.ButtonImportMaps.FontSize = 10;
            app.ButtonImportMaps.Tooltip = {'Import Maps'};
            app.ButtonImportMaps.Layout.Row = [1 2];
            app.ButtonImportMaps.Layout.Column = [15 16];
            app.ButtonImportMaps.Text = 'Import';

            % Create OPTIONSPROJECTLabel
            app.OPTIONSPROJECTLabel = uilabel(app.GridLayout_ImportTab);
            app.OPTIONSPROJECTLabel.HorizontalAlignment = 'center';
            app.OPTIONSPROJECTLabel.VerticalAlignment = 'bottom';
            app.OPTIONSPROJECTLabel.FontSize = 9;
            app.OPTIONSPROJECTLabel.FontColor = [0.149 0.149 0.149];
            app.OPTIONSPROJECTLabel.Layout.Row = 4;
            app.OPTIONSPROJECTLabel.Layout.Column = [26 35];
            app.OPTIONSPROJECTLabel.Text = 'OPTIONS (PROJECT)';

            % Create ResolutionmEditFieldLabel
            app.ResolutionmEditFieldLabel = uilabel(app.GridLayout_ImportTab);
            app.ResolutionmEditFieldLabel.HorizontalAlignment = 'right';
            app.ResolutionmEditFieldLabel.FontSize = 11;
            app.ResolutionmEditFieldLabel.Layout.Row = 1;
            app.ResolutionmEditFieldLabel.Layout.Column = [26 29];
            app.ResolutionmEditFieldLabel.Text = 'Resolution (µm)';

            % Create ResolutionField
            app.ResolutionField = uieditfield(app.GridLayout_ImportTab, 'numeric');
            app.ResolutionField.ValueDisplayFormat = '%.0f';
            app.ResolutionField.ValueChangedFcn = createCallbackFcn(app, @ResolutionFieldValueChanged, true);
            app.ResolutionField.FontSize = 10;
            app.ResolutionField.Layout.Row = 1;
            app.ResolutionField.Layout.Column = [30 31];
            app.ResolutionField.Value = 20;

            % Create DisplayScaleBar
            app.DisplayScaleBar = uicheckbox(app.GridLayout_ImportTab);
            app.DisplayScaleBar.ValueChangedFcn = createCallbackFcn(app, @DisplayScaleBarValueChanged, true);
            app.DisplayScaleBar.Text = 'Scale bar';
            app.DisplayScaleBar.FontSize = 11;
            app.DisplayScaleBar.Layout.Row = 1;
            app.DisplayScaleBar.Layout.Column = [32 34];

            % Create ColorScaleBar
            app.ColorScaleBar = uidropdown(app.GridLayout_ImportTab);
            app.ColorScaleBar.Items = {'White', 'Black'};
            app.ColorScaleBar.ValueChangedFcn = createCallbackFcn(app, @ColorScaleBarValueChanged, true);
            app.ColorScaleBar.Layout.Row = 1;
            app.ColorScaleBar.Layout.Column = 35;
            app.ColorScaleBar.Value = 'White';

            % Create ButtonRotateView
            app.ButtonRotateView = uibutton(app.GridLayout_ImportTab, 'push');
            app.ButtonRotateView.ButtonPushedFcn = createCallbackFcn(app, @ButtonRotateViewPushed, true);
            app.ButtonRotateView.Icon = '292-rotate.png';
            app.ButtonRotateView.IconAlignment = 'center';
            app.ButtonRotateView.Tooltip = {'Rotate View by 90°'};
            app.ButtonRotateView.Layout.Row = 3;
            app.ButtonRotateView.Layout.Column = 26;
            app.ButtonRotateView.Text = '';

            % Create FieldAngleView
            app.FieldAngleView = uieditfield(app.GridLayout_ImportTab, 'numeric');
            app.FieldAngleView.ValueChangedFcn = createCallbackFcn(app, @FieldAngleViewValueChanged, true);
            app.FieldAngleView.Editable = 'off';
            app.FieldAngleView.HorizontalAlignment = 'center';
            app.FieldAngleView.FontSize = 10;
            app.FieldAngleView.Layout.Row = 3;
            app.FieldAngleView.Layout.Column = [27 28];

            % Create ImportTab_help
            app.ImportTab_help = uibutton(app.GridLayout_ImportTab, 'push');
            app.ImportTab_help.ButtonPushedFcn = createCallbackFcn(app, @Help_ImportTab_helpButtonPushed, true);
            app.ImportTab_help.Icon = '061-info.png';
            app.ImportTab_help.Tooltip = {'Help & Documentation'};
            app.ImportTab_help.Layout.Row = 1;
            app.ImportTab_help.Layout.Column = 37;
            app.ImportTab_help.Text = '';

            % Create Import_CTdata
            app.Import_CTdata = uibutton(app.GridLayout_ImportTab, 'push');
            app.Import_CTdata.ButtonPushedFcn = createCallbackFcn(app, @Import_CTdataButtonPushed, true);
            app.Import_CTdata.Icon = '117-database.png';
            app.Import_CTdata.IconAlignment = 'top';
            app.Import_CTdata.VerticalAlignment = 'bottom';
            app.Import_CTdata.FontSize = 9;
            app.Import_CTdata.Tooltip = {'Import Image Stack (CT-data)'};
            app.Import_CTdata.Layout.Row = [1 2];
            app.Import_CTdata.Layout.Column = [17 18];
            app.Import_CTdata.Text = 'Import CT';

            % Create MakeMosaicStd
            app.MakeMosaicStd = uibutton(app.GridLayout_ImportTab, 'push');
            app.MakeMosaicStd.ButtonPushedFcn = createCallbackFcn(app, @Import_MakeMosaicStdButtonPushed, true);
            app.MakeMosaicStd.Icon = '209-windows.png';
            app.MakeMosaicStd.IconAlignment = 'center';
            app.MakeMosaicStd.Tooltip = {'Generate Mosaic in the Original Coordinates System'};
            app.MakeMosaicStd.Layout.Row = 2;
            app.MakeMosaicStd.Layout.Column = 11;
            app.MakeMosaicStd.Text = '';

            % Create MakeMosaic
            app.MakeMosaic = uibutton(app.GridLayout_ImportTab, 'push');
            app.MakeMosaic.ButtonPushedFcn = createCallbackFcn(app, @Import_ButtonMakeMosaicButtonPushed, true);
            app.MakeMosaic.Icon = '198-windows.png';
            app.MakeMosaic.IconAlignment = 'center';
            app.MakeMosaic.Tooltip = {'Generate Mosaic (Grid)'};
            app.MakeMosaic.Layout.Row = 1;
            app.MakeMosaic.Layout.Column = 11;
            app.MakeMosaic.Text = '';

            % Create ButtonConvertProject
            app.ButtonConvertProject = uibutton(app.GridLayout_ImportTab, 'push');
            app.ButtonConvertProject.ButtonPushedFcn = createCallbackFcn(app, @Import_ButtonConvertProjectPushed, true);
            app.ButtonConvertProject.Icon = '202-settings.png';
            app.ButtonConvertProject.IconAlignment = 'top';
            app.ButtonConvertProject.FontSize = 10;
            app.ButtonConvertProject.Tooltip = {'Open XMapTools'' EPMA Converter'};
            app.ButtonConvertProject.Layout.Row = [1 2];
            app.ButtonConvertProject.Layout.Column = [9 10];
            app.ButtonConvertProject.Text = 'EPMA';

            % Create Image
            app.Image = uiimage(app.GridLayout_ImportTab);
            app.Image.Layout.Row = [1 4];
            app.Image.Layout.Column = 8;
            app.Image.ImageSource = 'ImageDelimiter.png';

            % Create Image_7
            app.Image_7 = uiimage(app.GridLayout_ImportTab);
            app.Image_7.Layout.Row = [1 4];
            app.Image_7.Layout.Column = 19;
            app.Image_7.ImageSource = 'ImageDelimiter.png';

            % Create Image_8
            app.Image_8 = uiimage(app.GridLayout_ImportTab);
            app.Image_8.Layout.Row = [1 4];
            app.Image_8.Layout.Column = 36;
            app.Image_8.ImageSource = 'ImageDelimiter.png';

            % Create Image_9
            app.Image_9 = uiimage(app.GridLayout_ImportTab);
            app.Image_9.Layout.Row = [1 4];
            app.Image_9.Layout.Column = 25;
            app.Image_9.ImageSource = 'ImageDelimiter.png';

            % Create Image_29
            app.Image_29 = uiimage(app.GridLayout_ImportTab);
            app.Image_29.Layout.Row = [1 4];
            app.Image_29.Layout.Column = 14;
            app.Image_29.ImageSource = 'ImageDelimiter.png';

            % Create IMPORTMAPSIMAGESLabel
            app.IMPORTMAPSIMAGESLabel = uilabel(app.GridLayout_ImportTab);
            app.IMPORTMAPSIMAGESLabel.HorizontalAlignment = 'center';
            app.IMPORTMAPSIMAGESLabel.VerticalAlignment = 'bottom';
            app.IMPORTMAPSIMAGESLabel.FontSize = 9;
            app.IMPORTMAPSIMAGESLabel.FontColor = [0.149 0.149 0.149];
            app.IMPORTMAPSIMAGESLabel.Layout.Row = 4;
            app.IMPORTMAPSIMAGESLabel.Layout.Column = [15 18];
            app.IMPORTMAPSIMAGESLabel.Text = 'IMPORT MAPS & IMAGES';

            % Create ButtonConvertLaserData
            app.ButtonConvertLaserData = uibutton(app.GridLayout_ImportTab, 'push');
            app.ButtonConvertLaserData.ButtonPushedFcn = createCallbackFcn(app, @ButtonConvertLaserDataPushed, true);
            app.ButtonConvertLaserData.Icon = '201-settings.png';
            app.ButtonConvertLaserData.IconAlignment = 'top';
            app.ButtonConvertLaserData.FontSize = 9;
            app.ButtonConvertLaserData.Tooltip = {'Open XMapTools'' LA-ICPMS Converter'};
            app.ButtonConvertLaserData.Layout.Row = [1 2];
            app.ButtonConvertLaserData.Layout.Column = [12 13];
            app.ButtonConvertLaserData.Text = 'LA-ICPMS';

            % Create mapsizeLabel
            app.mapsizeLabel = uilabel(app.GridLayout_ImportTab);
            app.mapsizeLabel.HorizontalAlignment = 'center';
            app.mapsizeLabel.FontSize = 10;
            app.mapsizeLabel.Layout.Row = 3;
            app.mapsizeLabel.Layout.Column = [29 35];
            app.mapsizeLabel.Text = 'map size:';

            % Create CLASSIFYTab
            app.CLASSIFYTab = uitab(app.TabButtonGroup);
            app.CLASSIFYTab.AutoResizeChildren = 'off';
            app.CLASSIFYTab.Title = 'CLASSIFY';

            % Create GridLayout4
            app.GridLayout4 = uigridlayout(app.CLASSIFYTab);
            app.GridLayout4.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '0.3x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.3x', '1x', '1x', '1x', '1x', '0.3x', '1x', '1x', '1x', '1x', '0.3x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.3x', '1x', '0.15x'};
            app.GridLayout4.RowHeight = {'1x', '1x', '1x', '0.6x'};
            app.GridLayout4.ColumnSpacing = 4;
            app.GridLayout4.RowSpacing = 4;
            app.GridLayout4.Padding = [5 5 5 5];

            % Create Classify_AddTrainingSet
            app.Classify_AddTrainingSet = uibutton(app.GridLayout4, 'push');
            app.Classify_AddTrainingSet.ButtonPushedFcn = createCallbackFcn(app, @Classify_AddTrainingSetButtonPushed, true);
            app.Classify_AddTrainingSet.Icon = '073-add.png';
            app.Classify_AddTrainingSet.IconAlignment = 'top';
            app.Classify_AddTrainingSet.FontSize = 10;
            app.Classify_AddTrainingSet.Tooltip = {'Add Training Set / Phase Definition / ROI'};
            app.Classify_AddTrainingSet.Layout.Row = [1 2];
            app.Classify_AddTrainingSet.Layout.Column = [1 2];
            app.Classify_AddTrainingSet.Text = 'Add';

            % Create EditFieldLabel
            app.EditFieldLabel = uilabel(app.GridLayout4);
            app.EditFieldLabel.HorizontalAlignment = 'right';
            app.EditFieldLabel.FontSize = 10;
            app.EditFieldLabel.Layout.Row = 1;
            app.EditFieldLabel.Layout.Column = 4;
            app.EditFieldLabel.Text = 'Edit Field';

            % Create Classify_NameField
            app.Classify_NameField = uieditfield(app.GridLayout4, 'text');
            app.Classify_NameField.HorizontalAlignment = 'center';
            app.Classify_NameField.FontSize = 10;
            app.Classify_NameField.Layout.Row = 1;
            app.Classify_NameField.Layout.Column = [3 6];
            app.Classify_NameField.Value = 'PhaseDef_1';

            % Create Classification_Menu
            app.Classification_Menu = uidropdown(app.GridLayout4);
            app.Classification_Menu.Items = {'Random Forest', 'Discriminant Analysis', 'Naive Bayes', 'Support Vector Machine', 'Classification Tree', 'k-Nearest Neighbor', 'k-means', 'Unsupervised k-means'};
            app.Classification_Menu.ValueChangedFcn = createCallbackFcn(app, @Classify_MenuValueChanged, true);
            app.Classification_Menu.FontSize = 10;
            app.Classification_Menu.Layout.Row = 1;
            app.Classification_Menu.Layout.Column = [19 22];
            app.Classification_Menu.Value = 'Random Forest';

            % Create Classify_Button
            app.Classify_Button = uibutton(app.GridLayout4, 'push');
            app.Classify_Button.ButtonPushedFcn = createCallbackFcn(app, @Classify_ButtonPushed, true);
            app.Classify_Button.Icon = '042-shuffle.png';
            app.Classify_Button.IconAlignment = 'top';
            app.Classify_Button.FontSize = 10;
            app.Classify_Button.Tooltip = {' Train a Classifier & Classify'};
            app.Classify_Button.Layout.Row = [2 3];
            app.Classify_Button.Layout.Column = [21 22];
            app.Classify_Button.Text = 'Classify';

            % Create Classify_ElemList
            app.Classify_ElemList = uieditfield(app.GridLayout4, 'text');
            app.Classify_ElemList.Editable = 'off';
            app.Classify_ElemList.HorizontalAlignment = 'center';
            app.Classify_ElemList.FontSize = 10;
            app.Classify_ElemList.Tooltip = {'Elements selected for classification'};
            app.Classify_ElemList.Layout.Row = 1;
            app.Classify_ElemList.Layout.Column = [10 16];

            % Create Classify_Manage_Elem
            app.Classify_Manage_Elem = uibutton(app.GridLayout4, 'push');
            app.Classify_Manage_Elem.ButtonPushedFcn = createCallbackFcn(app, @Classify_Manage_ElemButtonPushed, true);
            app.Classify_Manage_Elem.Icon = '056-plus.png';
            app.Classify_Manage_Elem.IconAlignment = 'center';
            app.Classify_Manage_Elem.Tooltip = {'Edit Selected Map'};
            app.Classify_Manage_Elem.Layout.Row = 1;
            app.Classify_Manage_Elem.Layout.Column = 9;
            app.Classify_Manage_Elem.Text = '';

            % Create Classify_AddAllElem
            app.Classify_AddAllElem = uibutton(app.GridLayout4, 'push');
            app.Classify_AddAllElem.ButtonPushedFcn = createCallbackFcn(app, @Classify_AddAllElemButtonPushed, true);
            app.Classify_AddAllElem.Icon = '323-add.png';
            app.Classify_AddAllElem.IconAlignment = 'center';
            app.Classify_AddAllElem.Tooltip = {'Add Maps for Classification'};
            app.Classify_AddAllElem.Layout.Row = 1;
            app.Classify_AddAllElem.Layout.Column = 8;
            app.Classify_AddAllElem.Text = '';

            % Create Classify_ROI_Menu
            app.Classify_ROI_Menu = uidropdown(app.GridLayout4);
            app.Classify_ROI_Menu.Items = {'Rectangle ROI', 'Polygon ROI', 'Ellipse ROI', 'Circle ROI'};
            app.Classify_ROI_Menu.Tooltip = {'Method for ROI drawing'};
            app.Classify_ROI_Menu.FontSize = 10;
            app.Classify_ROI_Menu.Layout.Row = 2;
            app.Classify_ROI_Menu.Layout.Column = [3 6];
            app.Classify_ROI_Menu.Value = 'Rectangle ROI';

            % Create TRAININGSETLabel
            app.TRAININGSETLabel = uilabel(app.GridLayout4);
            app.TRAININGSETLabel.HorizontalAlignment = 'center';
            app.TRAININGSETLabel.VerticalAlignment = 'bottom';
            app.TRAININGSETLabel.FontSize = 9;
            app.TRAININGSETLabel.FontColor = [0.149 0.149 0.149];
            app.TRAININGSETLabel.Layout.Row = 4;
            app.TRAININGSETLabel.Layout.Column = [1 6];
            app.TRAININGSETLabel.Text = 'TRAINING SET';

            % Create CLASSIFICATIONPARAMETERSLabel
            app.CLASSIFICATIONPARAMETERSLabel = uilabel(app.GridLayout4);
            app.CLASSIFICATIONPARAMETERSLabel.HorizontalAlignment = 'center';
            app.CLASSIFICATIONPARAMETERSLabel.VerticalAlignment = 'bottom';
            app.CLASSIFICATIONPARAMETERSLabel.FontSize = 9;
            app.CLASSIFICATIONPARAMETERSLabel.FontColor = [0.149 0.149 0.149];
            app.CLASSIFICATIONPARAMETERSLabel.Layout.Row = 4;
            app.CLASSIFICATIONPARAMETERSLabel.Layout.Column = [8 17];
            app.CLASSIFICATIONPARAMETERSLabel.Text = 'CLASSIFICATION PARAMETERS';

            % Create APPLYLabel_2
            app.APPLYLabel_2 = uilabel(app.GridLayout4);
            app.APPLYLabel_2.HorizontalAlignment = 'center';
            app.APPLYLabel_2.VerticalAlignment = 'bottom';
            app.APPLYLabel_2.FontSize = 9;
            app.APPLYLabel_2.FontColor = [0.149 0.149 0.149];
            app.APPLYLabel_2.Layout.Row = 4;
            app.APPLYLabel_2.Layout.Column = [19 22];
            app.APPLYLabel_2.Text = 'APPLY';

            % Create Masks_ButtonPlotPie
            app.Masks_ButtonPlotPie = uibutton(app.GridLayout4, 'push');
            app.Masks_ButtonPlotPie.ButtonPushedFcn = createCallbackFcn(app, @Masks_ButtonPlotPieButtonPushed, true);
            app.Masks_ButtonPlotPie.Icon = '087-menu-rot.png';
            app.Masks_ButtonPlotPie.IconAlignment = 'top';
            app.Masks_ButtonPlotPie.FontSize = 10;
            app.Masks_ButtonPlotPie.Tooltip = {'Plot Compositions'};
            app.Masks_ButtonPlotPie.Layout.Row = [1 2];
            app.Masks_ButtonPlotPie.Layout.Column = [34 35];
            app.Masks_ButtonPlotPie.Text = 'Plot';

            % Create MASKANALYSISVISUALIZATIONLabel
            app.MASKANALYSISVISUALIZATIONLabel = uilabel(app.GridLayout4);
            app.MASKANALYSISVISUALIZATIONLabel.HorizontalAlignment = 'center';
            app.MASKANALYSISVISUALIZATIONLabel.VerticalAlignment = 'bottom';
            app.MASKANALYSISVISUALIZATIONLabel.FontSize = 9;
            app.MASKANALYSISVISUALIZATIONLabel.FontColor = [0.149 0.149 0.149];
            app.MASKANALYSISVISUALIZATIONLabel.Layout.Row = 4;
            app.MASKANALYSISVISUALIZATIONLabel.Layout.Column = [30 35];
            app.MASKANALYSISVISUALIZATIONLabel.Text = 'MASK ANALYSIS & VISUALIZATION';

            % Create ClassifyTab_help
            app.ClassifyTab_help = uibutton(app.GridLayout4, 'push');
            app.ClassifyTab_help.ButtonPushedFcn = createCallbackFcn(app, @Help_ClassifyTab_helpButtonPushed, true);
            app.ClassifyTab_help.Icon = '061-info.png';
            app.ClassifyTab_help.Tooltip = {'Help & Documentation'};
            app.ClassifyTab_help.Layout.Row = 1;
            app.ClassifyTab_help.Layout.Column = 37;
            app.ClassifyTab_help.Text = '';

            % Create FILTERINGOPTIONSLabel
            app.FILTERINGOPTIONSLabel = uilabel(app.GridLayout4);
            app.FILTERINGOPTIONSLabel.HorizontalAlignment = 'center';
            app.FILTERINGOPTIONSLabel.VerticalAlignment = 'bottom';
            app.FILTERINGOPTIONSLabel.FontSize = 9;
            app.FILTERINGOPTIONSLabel.FontColor = [0.149 0.149 0.149];
            app.FILTERINGOPTIONSLabel.Layout.Row = 4;
            app.FILTERINGOPTIONSLabel.Layout.Column = [24 27];
            app.FILTERINGOPTIONSLabel.Text = 'FILTERING OPTIONS';

            % Create Classify_FilterLowProbPixels
            app.Classify_FilterLowProbPixels = uicheckbox(app.GridLayout4);
            app.Classify_FilterLowProbPixels.ValueChangedFcn = createCallbackFcn(app, @Classify_FilterLowProbPixelsValueChanged, true);
            app.Classify_FilterLowProbPixels.Text = 'Apply Filter';
            app.Classify_FilterLowProbPixels.FontSize = 9;
            app.Classify_FilterLowProbPixels.Layout.Row = 1;
            app.Classify_FilterLowProbPixels.Layout.Column = [24 26];

            % Create ProbabilityLabel
            app.ProbabilityLabel = uilabel(app.GridLayout4);
            app.ProbabilityLabel.HorizontalAlignment = 'right';
            app.ProbabilityLabel.FontSize = 9;
            app.ProbabilityLabel.Layout.Row = 2;
            app.ProbabilityLabel.Layout.Column = [24 26];
            app.ProbabilityLabel.Text = 'Probability';

            % Create Classify_FilterProbValue
            app.Classify_FilterProbValue = uieditfield(app.GridLayout4, 'numeric');
            app.Classify_FilterProbValue.Limits = [0 1];
            app.Classify_FilterProbValue.ValueDisplayFormat = '%.2f';
            app.Classify_FilterProbValue.ValueChangedFcn = createCallbackFcn(app, @Classify_FilterProbValueValueChanged, true);
            app.Classify_FilterProbValue.HorizontalAlignment = 'center';
            app.Classify_FilterProbValue.FontSize = 9;
            app.Classify_FilterProbValue.Layout.Row = 2;
            app.Classify_FilterProbValue.Layout.Column = 27;
            app.Classify_FilterProbValue.Value = 1;

            % Create Classify_PCA1CheckBox
            app.Classify_PCA1CheckBox = uicheckbox(app.GridLayout4);
            app.Classify_PCA1CheckBox.Tooltip = {'Include maps of principal components into the classification'};
            app.Classify_PCA1CheckBox.Text = 'PCA-M';
            app.Classify_PCA1CheckBox.FontSize = 9;
            app.Classify_PCA1CheckBox.Layout.Row = 2;
            app.Classify_PCA1CheckBox.Layout.Column = [11 12];

            % Create Classify_RunPCAButton
            app.Classify_RunPCAButton = uibutton(app.GridLayout4, 'push');
            app.Classify_RunPCAButton.ButtonPushedFcn = createCallbackFcn(app, @Classify_RunPCAButtonPushed, true);
            app.Classify_RunPCAButton.Icon = '004-picture.png';
            app.Classify_RunPCAButton.IconAlignment = 'center';
            app.Classify_RunPCAButton.Tooltip = {'Generate Maps of the Principal Components (PCA)'};
            app.Classify_RunPCAButton.Layout.Row = 2;
            app.Classify_RunPCAButton.Layout.Column = 8;
            app.Classify_RunPCAButton.Text = '';

            % Create Classify_FilterMaskFile
            app.Classify_FilterMaskFile = uibutton(app.GridLayout4, 'push');
            app.Classify_FilterMaskFile.ButtonPushedFcn = createCallbackFcn(app, @Classify_FilterMaskFileButtonPushed, true);
            app.Classify_FilterMaskFile.Icon = '311-app.png';
            app.Classify_FilterMaskFile.IconAlignment = 'center';
            app.Classify_FilterMaskFile.Tooltip = {'Create new mask file with pixels filtered by probability (all pixels below threshold)'};
            app.Classify_FilterMaskFile.Layout.Row = 1;
            app.Classify_FilterMaskFile.Layout.Column = 27;
            app.Classify_FilterMaskFile.Text = '';

            % Create Classify_Modes_AddROI
            app.Classify_Modes_AddROI = uibutton(app.GridLayout4, 'push');
            app.Classify_Modes_AddROI.ButtonPushedFcn = createCallbackFcn(app, @Classify_Modes_AddROIButtonPushed, true);
            app.Classify_Modes_AddROI.Icon = '093-binoculars.png';
            app.Classify_Modes_AddROI.IconAlignment = 'top';
            app.Classify_Modes_AddROI.FontSize = 9;
            app.Classify_Modes_AddROI.Tooltip = {'Add ROI to export mask modes'};
            app.Classify_Modes_AddROI.Layout.Row = [1 2];
            app.Classify_Modes_AddROI.Layout.Column = [32 33];
            app.Classify_Modes_AddROI.Text = 'Add ROI';

            % Create Classify_Modes_ROI_menu
            app.Classify_Modes_ROI_menu = uidropdown(app.GridLayout4);
            app.Classify_Modes_ROI_menu.Items = {'Rectangle ROI', 'Polygon ROI'};
            app.Classify_Modes_ROI_menu.Tooltip = {'Method for ROI drawing'};
            app.Classify_Modes_ROI_menu.FontSize = 9;
            app.Classify_Modes_ROI_menu.Layout.Row = 1;
            app.Classify_Modes_ROI_menu.Layout.Column = [29 31];
            app.Classify_Modes_ROI_menu.Value = 'Rectangle ROI';

            % Create Image_30
            app.Image_30 = uiimage(app.GridLayout4);
            app.Image_30.Layout.Row = [1 4];
            app.Image_30.Layout.Column = 7;
            app.Image_30.ImageSource = 'ImageDelimiter.png';

            % Create Image_31
            app.Image_31 = uiimage(app.GridLayout4);
            app.Image_31.Layout.Row = [1 4];
            app.Image_31.Layout.Column = 18;
            app.Image_31.ImageSource = 'ImageDelimiter.png';

            % Create Image_4
            app.Image_4 = uiimage(app.GridLayout4);
            app.Image_4.Layout.Row = [1 4];
            app.Image_4.Layout.Column = 23;
            app.Image_4.ImageSource = 'ImageDelimiter.png';

            % Create Image_5
            app.Image_5 = uiimage(app.GridLayout4);
            app.Image_5.Layout.Row = [1 4];
            app.Image_5.Layout.Column = 28;
            app.Image_5.ImageSource = 'ImageDelimiter.png';

            % Create Image_6
            app.Image_6 = uiimage(app.GridLayout4);
            app.Image_6.Layout.Row = [1 4];
            app.Image_6.Layout.Column = 36;
            app.Image_6.ImageSource = 'ImageDelimiter.png';

            % Create ExtractmodesLabel
            app.ExtractmodesLabel = uilabel(app.GridLayout4);
            app.ExtractmodesLabel.HorizontalAlignment = 'center';
            app.ExtractmodesLabel.FontSize = 9;
            app.ExtractmodesLabel.FontAngle = 'italic';
            app.ExtractmodesLabel.Layout.Row = 2;
            app.ExtractmodesLabel.Layout.Column = [29 31];
            app.ExtractmodesLabel.Text = 'Extract modes';

            % Create Classify_PCA2CheckBox
            app.Classify_PCA2CheckBox = uicheckbox(app.GridLayout4);
            app.Classify_PCA2CheckBox.Tooltip = {'Include maps of (normnalized) principal components into the classification'};
            app.Classify_PCA2CheckBox.Text = 'PCA-N';
            app.Classify_PCA2CheckBox.FontSize = 9;
            app.Classify_PCA2CheckBox.Layout.Row = 2;
            app.Classify_PCA2CheckBox.Layout.Column = [13 14];

            % Create Classify_TEXFCheckBox
            app.Classify_TEXFCheckBox = uicheckbox(app.GridLayout4);
            app.Classify_TEXFCheckBox.Tooltip = {'Include additional calculated maps into the classification'};
            app.Classify_TEXFCheckBox.Text = 'TEXF';
            app.Classify_TEXFCheckBox.FontSize = 9;
            app.Classify_TEXFCheckBox.Layout.Row = 2;
            app.Classify_TEXFCheckBox.Layout.Column = [15 16];

            % Create Classify_MapsCheckBox
            app.Classify_MapsCheckBox = uicheckbox(app.GridLayout4);
            app.Classify_MapsCheckBox.Tooltip = {'Include (normalized) maps into the classification'};
            app.Classify_MapsCheckBox.Text = 'Maps';
            app.Classify_MapsCheckBox.FontSize = 9;
            app.Classify_MapsCheckBox.Layout.Row = 2;
            app.Classify_MapsCheckBox.Layout.Column = [9 10];
            app.Classify_MapsCheckBox.Value = true;

            % Create Classify_MixingPixelsPer
            app.Classify_MixingPixelsPer = uilabel(app.GridLayout4);
            app.Classify_MixingPixelsPer.FontSize = 9;
            app.Classify_MixingPixelsPer.FontAngle = 'italic';
            app.Classify_MixingPixelsPer.Layout.Row = 3;
            app.Classify_MixingPixelsPer.Layout.Column = [24 27];
            app.Classify_MixingPixelsPer.Text = 'Mix. pixels: ';

            % Create ScalingLabel
            app.ScalingLabel = uilabel(app.GridLayout4);
            app.ScalingLabel.HorizontalAlignment = 'right';
            app.ScalingLabel.FontSize = 9;
            app.ScalingLabel.Layout.Row = 3;
            app.ScalingLabel.Layout.Column = [8 9];
            app.ScalingLabel.Text = 'Scaling';

            % Create Classify_ScalingMode
            app.Classify_ScalingMode = uidropdown(app.GridLayout4);
            app.Classify_ScalingMode.Items = {'Robust', 'MinMax', 'Mean', 'NoScaling'};
            app.Classify_ScalingMode.FontSize = 9;
            app.Classify_ScalingMode.Layout.Row = 3;
            app.Classify_ScalingMode.Layout.Column = [10 12];
            app.Classify_ScalingMode.Value = 'Robust';

            % Create Classify_Reproducibility
            app.Classify_Reproducibility = uicheckbox(app.GridLayout4);
            app.Classify_Reproducibility.Text = 'Reproducibility';
            app.Classify_Reproducibility.FontSize = 9;
            app.Classify_Reproducibility.Layout.Row = 3;
            app.Classify_Reproducibility.Layout.Column = [13 15];
            app.Classify_Reproducibility.Value = true;

            % Create Classify_ReproducibilityValue
            app.Classify_ReproducibilityValue = uispinner(app.GridLayout4);
            app.Classify_ReproducibilityValue.Limits = [-99 99];
            app.Classify_ReproducibilityValue.ValueDisplayFormat = '%11.0g';
            app.Classify_ReproducibilityValue.ValueChangedFcn = createCallbackFcn(app, @Classify_ReproducibilityValueValueChanged, true);
            app.Classify_ReproducibilityValue.HorizontalAlignment = 'center';
            app.Classify_ReproducibilityValue.FontSize = 9;
            app.Classify_ReproducibilityValue.Layout.Row = 3;
            app.Classify_ReproducibilityValue.Layout.Column = [16 17];
            app.Classify_ReproducibilityValue.Value = 1;

            % Create Classify_OptionLabel
            app.Classify_OptionLabel = uilabel(app.GridLayout4);
            app.Classify_OptionLabel.HorizontalAlignment = 'center';
            app.Classify_OptionLabel.FontSize = 9;
            app.Classify_OptionLabel.Layout.Row = 2;
            app.Classify_OptionLabel.Layout.Column = [19 20];
            app.Classify_OptionLabel.Text = 'Option';

            % Create Classify_OptionValue
            app.Classify_OptionValue = uispinner(app.GridLayout4);
            app.Classify_OptionValue.Limits = [2 5000];
            app.Classify_OptionValue.ValueChangedFcn = createCallbackFcn(app, @Classify_OptionValueValueChanged, true);
            app.Classify_OptionValue.HorizontalAlignment = 'center';
            app.Classify_OptionValue.FontSize = 9;
            app.Classify_OptionValue.Layout.Row = 3;
            app.Classify_OptionValue.Layout.Column = [19 20];
            app.Classify_OptionValue.Value = 40;

            % Create Classify_ActivateAssistant
            app.Classify_ActivateAssistant = uicheckbox(app.GridLayout4);
            app.Classify_ActivateAssistant.Text = 'Phase Assistant';
            app.Classify_ActivateAssistant.FontSize = 9;
            app.Classify_ActivateAssistant.Layout.Row = 3;
            app.Classify_ActivateAssistant.Layout.Column = [1 6];
            app.Classify_ActivateAssistant.Value = true;

            % Create Classify_PointCountingModes
            app.Classify_PointCountingModes = uibutton(app.GridLayout4, 'push');
            app.Classify_PointCountingModes.ButtonPushedFcn = createCallbackFcn(app, @Classify_PointCountingModesButtonPushed, true);
            app.Classify_PointCountingModes.Icon = '090-share.png';
            app.Classify_PointCountingModes.IconAlignment = 'center';
            app.Classify_PointCountingModes.Tooltip = {'Calculate pseudo-modes (point counting)'};
            app.Classify_PointCountingModes.Layout.Row = 3;
            app.Classify_PointCountingModes.Layout.Column = 35;
            app.Classify_PointCountingModes.Text = '';

            % Create PointcountingLabel
            app.PointcountingLabel = uilabel(app.GridLayout4);
            app.PointcountingLabel.HorizontalAlignment = 'right';
            app.PointcountingLabel.FontSize = 9;
            app.PointcountingLabel.Layout.Row = 3;
            app.PointcountingLabel.Layout.Column = [29 31];
            app.PointcountingLabel.Text = ' Point counting (#)';

            % Create Classify_NbPointCounting
            app.Classify_NbPointCounting = uieditfield(app.GridLayout4, 'numeric');
            app.Classify_NbPointCounting.Limits = [10 100000];
            app.Classify_NbPointCounting.RoundFractionalValues = 'on';
            app.Classify_NbPointCounting.ValueDisplayFormat = '%.0f';
            app.Classify_NbPointCounting.HorizontalAlignment = 'center';
            app.Classify_NbPointCounting.FontSize = 9;
            app.Classify_NbPointCounting.Tooltip = {'Set the number of points for the point-counting method'};
            app.Classify_NbPointCounting.Layout.Row = 3;
            app.Classify_NbPointCounting.Layout.Column = [32 33];
            app.Classify_NbPointCounting.Value = 900;

            % Create Classify_MC_PointCountingCheckBox
            app.Classify_MC_PointCountingCheckBox = uicheckbox(app.GridLayout4);
            app.Classify_MC_PointCountingCheckBox.Tooltip = {'Calculate uncertainties using Monte Carlo (1000 simulations)'};
            app.Classify_MC_PointCountingCheckBox.Text = '';
            app.Classify_MC_PointCountingCheckBox.Layout.Row = 3;
            app.Classify_MC_PointCountingCheckBox.Layout.Column = 34;

            % Create Classify_Manage_Elem_DeleteAll
            app.Classify_Manage_Elem_DeleteAll = uibutton(app.GridLayout4, 'push');
            app.Classify_Manage_Elem_DeleteAll.ButtonPushedFcn = createCallbackFcn(app, @Classify_Manage_Elem_DeleteAllButtonPushed, true);
            app.Classify_Manage_Elem_DeleteAll.Icon = '058-error.png';
            app.Classify_Manage_Elem_DeleteAll.IconAlignment = 'center';
            app.Classify_Manage_Elem_DeleteAll.Tooltip = {'Edit Selected Map'};
            app.Classify_Manage_Elem_DeleteAll.Layout.Row = 1;
            app.Classify_Manage_Elem_DeleteAll.Layout.Column = 17;
            app.Classify_Manage_Elem_DeleteAll.Text = '';

            % Create CALIBRATETab
            app.CALIBRATETab = uitab(app.TabButtonGroup);
            app.CALIBRATETab.AutoResizeChildren = 'off';
            app.CALIBRATETab.Title = 'CALIBRATE';

            % Create CalibrateGridLayout
            app.CalibrateGridLayout = uigridlayout(app.CALIBRATETab);
            app.CalibrateGridLayout.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '0.3x', '1x', '1x', '1x', '1x', '0.3x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.3x', '1x', '1x', '1x', '1x', '1x', '0.3x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.3x', '1x', '0.15x'};
            app.CalibrateGridLayout.RowHeight = {'1x', '1x', '1x', '0.6x'};
            app.CalibrateGridLayout.ColumnSpacing = 4;
            app.CalibrateGridLayout.RowSpacing = 4;
            app.CalibrateGridLayout.Padding = [5 5 5 5];

            % Create EPMASTANDARDDATALabel
            app.EPMASTANDARDDATALabel = uilabel(app.CalibrateGridLayout);
            app.EPMASTANDARDDATALabel.HorizontalAlignment = 'center';
            app.EPMASTANDARDDATALabel.VerticalAlignment = 'bottom';
            app.EPMASTANDARDDATALabel.FontSize = 9;
            app.EPMASTANDARDDATALabel.FontColor = [0.149 0.149 0.149];
            app.EPMASTANDARDDATALabel.Layout.Row = 4;
            app.EPMASTANDARDDATALabel.Layout.Column = [1 5];
            app.EPMASTANDARDDATALabel.Text = 'EPMA STANDARD DATA';

            % Create ImportStandards
            app.ImportStandards = uibutton(app.CalibrateGridLayout, 'push');
            app.ImportStandards.ButtonPushedFcn = createCallbackFcn(app, @Standards_ImportStandardsButtonPushed, true);
            app.ImportStandards.Icon = '141-radar.png';
            app.ImportStandards.IconAlignment = 'top';
            app.ImportStandards.FontSize = 10;
            app.ImportStandards.Tooltip = {'Import Standard Spot Analyses (from file)'};
            app.ImportStandards.Layout.Row = [1 2];
            app.ImportStandards.Layout.Column = [1 2];
            app.ImportStandards.Text = 'Import';

            % Create AddStandardsManual
            app.AddStandardsManual = uibutton(app.CalibrateGridLayout, 'push');
            app.AddStandardsManual.ButtonPushedFcn = createCallbackFcn(app, @Standards_AddStandardsManualButtonPushed, true);
            app.AddStandardsManual.Icon = '056-plus.png';
            app.AddStandardsManual.IconAlignment = 'top';
            app.AddStandardsManual.FontSize = 10;
            app.AddStandardsManual.Tooltip = {'Add Standard Point'};
            app.AddStandardsManual.Layout.Row = [1 2];
            app.AddStandardsManual.Layout.Column = [3 4];
            app.AddStandardsManual.Text = 'Add';

            % Create ImportAutoMode
            app.ImportAutoMode = uicheckbox(app.CalibrateGridLayout);
            app.ImportAutoMode.Text = 'Import from Standards.txt';
            app.ImportAutoMode.FontSize = 9;
            app.ImportAutoMode.Layout.Row = 3;
            app.ImportAutoMode.Layout.Column = [1 5];
            app.ImportAutoMode.Value = true;

            % Create STANDARDIZATIONLabel
            app.STANDARDIZATIONLabel = uilabel(app.CalibrateGridLayout);
            app.STANDARDIZATIONLabel.HorizontalAlignment = 'center';
            app.STANDARDIZATIONLabel.VerticalAlignment = 'bottom';
            app.STANDARDIZATIONLabel.FontSize = 9;
            app.STANDARDIZATIONLabel.FontColor = [0.149 0.149 0.149];
            app.STANDARDIZATIONLabel.Layout.Row = 4;
            app.STANDARDIZATIONLabel.Layout.Column = [7 10];
            app.STANDARDIZATIONLabel.Text = 'STANDARDIZATION';

            % Create Label_2
            app.Label_2 = uilabel(app.CalibrateGridLayout);
            app.Label_2.HorizontalAlignment = 'center';
            app.Label_2.VerticalAlignment = 'bottom';
            app.Label_2.FontSize = 9;
            app.Label_2.FontColor = [0.149 0.149 0.149];
            app.Label_2.Layout.Row = 4;
            app.Label_2.Layout.Column = [21 29];
            app.Label_2.Text = ' ';

            % Create ButtonCalibrate
            app.ButtonCalibrate = uibutton(app.CalibrateGridLayout, 'push');
            app.ButtonCalibrate.ButtonPushedFcn = createCallbackFcn(app, @Standardization_ButtonCalibratePushed, true);
            app.ButtonCalibrate.Icon = '010-record.png';
            app.ButtonCalibrate.IconAlignment = 'top';
            app.ButtonCalibrate.FontSize = 10;
            app.ButtonCalibrate.Tooltip = {'Open Calibration Assistant for EPMA Data'};
            app.ButtonCalibrate.Layout.Row = [1 2];
            app.ButtonCalibrate.Layout.Column = [7 8];
            app.ButtonCalibrate.Text = 'Calibrate';

            % Create LOCALBULKCOMPOSITIONLabel
            app.LOCALBULKCOMPOSITIONLabel = uilabel(app.CalibrateGridLayout);
            app.LOCALBULKCOMPOSITIONLabel.HorizontalAlignment = 'center';
            app.LOCALBULKCOMPOSITIONLabel.VerticalAlignment = 'bottom';
            app.LOCALBULKCOMPOSITIONLabel.FontSize = 9;
            app.LOCALBULKCOMPOSITIONLabel.FontColor = [0.149 0.149 0.149];
            app.LOCALBULKCOMPOSITIONLabel.Layout.Row = 4;
            app.LOCALBULKCOMPOSITIONLabel.Layout.Column = [12 21];
            app.LOCALBULKCOMPOSITIONLabel.Text = 'LOCAL BULK COMPOSITION';

            % Create Calibrate_GenerateDensity
            app.Calibrate_GenerateDensity = uibutton(app.CalibrateGridLayout, 'push');
            app.Calibrate_GenerateDensity.ButtonPushedFcn = createCallbackFcn(app, @Calibrate_GenerateDensityButtonPushed, true);
            app.Calibrate_GenerateDensity.Icon = '331-volume control.png';
            app.Calibrate_GenerateDensity.IconAlignment = 'top';
            app.Calibrate_GenerateDensity.FontSize = 10;
            app.Calibrate_GenerateDensity.Tooltip = {'Generate Density Map (from a Mask File)'};
            app.Calibrate_GenerateDensity.Layout.Row = [1 2];
            app.Calibrate_GenerateDensity.Layout.Column = [12 13];
            app.Calibrate_GenerateDensity.Text = 'Density';

            % Create Calibrate_Merge
            app.Calibrate_Merge = uibutton(app.CalibrateGridLayout, 'push');
            app.Calibrate_Merge.ButtonPushedFcn = createCallbackFcn(app, @Calibrate_MergeButtonPushed, true);
            app.Calibrate_Merge.Icon = '176-windows.png';
            app.Calibrate_Merge.IconAlignment = 'top';
            app.Calibrate_Merge.FontSize = 10;
            app.Calibrate_Merge.Tooltip = {'Merge Quanti Data'};
            app.Calibrate_Merge.Layout.Row = [1 2];
            app.Calibrate_Merge.Layout.Column = [14 15];
            app.Calibrate_Merge.Text = 'Merge';

            % Create Calibrate_AddROIforLBC
            app.Calibrate_AddROIforLBC = uibutton(app.CalibrateGridLayout, 'push');
            app.Calibrate_AddROIforLBC.ButtonPushedFcn = createCallbackFcn(app, @Calibrate_AddROIforLBCButtonPushed, true);
            app.Calibrate_AddROIforLBC.Icon = '093-binoculars.png';
            app.Calibrate_AddROIforLBC.IconAlignment = 'top';
            app.Calibrate_AddROIforLBC.FontSize = 9;
            app.Calibrate_AddROIforLBC.Tooltip = {'Add ROI for LBC extraction'};
            app.Calibrate_AddROIforLBC.Layout.Row = [1 2];
            app.Calibrate_AddROIforLBC.Layout.Column = [20 21];
            app.Calibrate_AddROIforLBC.Text = 'Add ROI';

            % Create Calibrate_ROI_menu
            app.Calibrate_ROI_menu = uidropdown(app.CalibrateGridLayout);
            app.Calibrate_ROI_menu.Items = {'Rectangle ROI', 'Polygon ROI'};
            app.Calibrate_ROI_menu.Tooltip = {'Method for ROI drawing'};
            app.Calibrate_ROI_menu.FontSize = 10;
            app.Calibrate_ROI_menu.Layout.Row = 1;
            app.Calibrate_ROI_menu.Layout.Column = [16 19];
            app.Calibrate_ROI_menu.Value = 'Rectangle ROI';

            % Create Calibrate_MultiROICheckBox
            app.Calibrate_MultiROICheckBox = uicheckbox(app.CalibrateGridLayout);
            app.Calibrate_MultiROICheckBox.ValueChangedFcn = createCallbackFcn(app, @Calibrate_MultiROICheckBoxValueChanged, true);
            app.Calibrate_MultiROICheckBox.Tooltip = {'Activate Mutli-ROI mode'};
            app.Calibrate_MultiROICheckBox.Text = 'Multi-ROI';
            app.Calibrate_MultiROICheckBox.FontSize = 9;
            app.Calibrate_MultiROICheckBox.Layout.Row = 2;
            app.Calibrate_MultiROICheckBox.Layout.Column = [17 19];

            % Create CalibratetTab_help
            app.CalibratetTab_help = uibutton(app.CalibrateGridLayout, 'push');
            app.CalibratetTab_help.ButtonPushedFcn = createCallbackFcn(app, @Help_CalibratetTab_helpButtonPushed, true);
            app.CalibratetTab_help.Icon = '061-info.png';
            app.CalibratetTab_help.Tooltip = {'Help & Documentation'};
            app.CalibratetTab_help.Layout.Row = 1;
            app.CalibratetTab_help.Layout.Column = 38;
            app.CalibratetTab_help.Text = '';

            % Create Image_15
            app.Image_15 = uiimage(app.CalibrateGridLayout);
            app.Image_15.Layout.Row = [1 4];
            app.Image_15.Layout.Column = 6;
            app.Image_15.ImageSource = 'ImageDelimiter.png';

            % Create Image_16
            app.Image_16 = uiimage(app.CalibrateGridLayout);
            app.Image_16.Layout.Row = [1 4];
            app.Image_16.Layout.Column = 11;
            app.Image_16.ImageSource = 'ImageDelimiter.png';

            % Create Image_17
            app.Image_17 = uiimage(app.CalibrateGridLayout);
            app.Image_17.Layout.Row = [1 4];
            app.Image_17.Layout.Column = 22;
            app.Image_17.ImageSource = 'ImageDelimiter.png';

            % Create Image_18
            app.Image_18 = uiimage(app.CalibrateGridLayout);
            app.Image_18.Layout.Row = [1 4];
            app.Image_18.Layout.Column = 28;
            app.Image_18.ImageSource = 'ImageDelimiter.png';

            % Create Image_19
            app.Image_19 = uiimage(app.CalibrateGridLayout);
            app.Image_19.Layout.Row = [1 4];
            app.Image_19.Layout.Column = 37;
            app.Image_19.ImageSource = 'ImageDelimiter.png';

            % Create STANDARDIZATIONLAICPMSLabel
            app.STANDARDIZATIONLAICPMSLabel = uilabel(app.CalibrateGridLayout);
            app.STANDARDIZATIONLAICPMSLabel.HorizontalAlignment = 'center';
            app.STANDARDIZATIONLAICPMSLabel.VerticalAlignment = 'bottom';
            app.STANDARDIZATIONLAICPMSLabel.FontSize = 9;
            app.STANDARDIZATIONLAICPMSLabel.FontColor = [0.149 0.149 0.149];
            app.STANDARDIZATIONLAICPMSLabel.Layout.Row = 4;
            app.STANDARDIZATIONLAICPMSLabel.Layout.Column = [31 36];
            app.STANDARDIZATIONLAICPMSLabel.Text = 'STANDARDIZATION (LA-ICP-MS)';

            % Create ButtonCalibrate_LAICPMS
            app.ButtonCalibrate_LAICPMS = uibutton(app.CalibrateGridLayout, 'push');
            app.ButtonCalibrate_LAICPMS.ButtonPushedFcn = createCallbackFcn(app, @Calibrate_ButtonCalibrate_LAICPMSPushed, true);
            app.ButtonCalibrate_LAICPMS.Icon = '010-record.png';
            app.ButtonCalibrate_LAICPMS.IconAlignment = 'top';
            app.ButtonCalibrate_LAICPMS.FontSize = 10;
            app.ButtonCalibrate_LAICPMS.Tooltip = {'Open Calibration Assistant for LA-ICP-MS Data'};
            app.ButtonCalibrate_LAICPMS.Layout.Row = [1 2];
            app.ButtonCalibrate_LAICPMS.Layout.Column = [31 32];
            app.ButtonCalibrate_LAICPMS.Text = 'Calibrate';

            % Create ImportStandards_LAICPMS
            app.ImportStandards_LAICPMS = uibutton(app.CalibrateGridLayout, 'push');
            app.ImportStandards_LAICPMS.ButtonPushedFcn = createCallbackFcn(app, @Calibrate_ImportStandards_LAICPMSButtonPushed, true);
            app.ImportStandards_LAICPMS.Icon = '141-radar.png';
            app.ImportStandards_LAICPMS.IconAlignment = 'top';
            app.ImportStandards_LAICPMS.FontSize = 10;
            app.ImportStandards_LAICPMS.Tooltip = {'Import Standard Maps (from file)'};
            app.ImportStandards_LAICPMS.Layout.Row = [1 2];
            app.ImportStandards_LAICPMS.Layout.Column = [29 30];
            app.ImportStandards_LAICPMS.Text = 'Import';

            % Create Calibrate_Spider_Button
            app.Calibrate_Spider_Button = uibutton(app.CalibrateGridLayout, 'push');
            app.Calibrate_Spider_Button.ButtonPushedFcn = createCallbackFcn(app, @Calibrate_Spider_ButtonPushed, true);
            app.Calibrate_Spider_Button.Icon = '345-more.png';
            app.Calibrate_Spider_Button.IconAlignment = 'top';
            app.Calibrate_Spider_Button.FontSize = 10;
            app.Calibrate_Spider_Button.Tooltip = {'Open Spider Module'};
            app.Calibrate_Spider_Button.Layout.Row = [1 2];
            app.Calibrate_Spider_Button.Layout.Column = [35 36];
            app.Calibrate_Spider_Button.Text = 'Spider';

            % Create LBC_UncCalc
            app.LBC_UncCalc = uibutton(app.CalibrateGridLayout, 'push');
            app.LBC_UncCalc.ButtonPushedFcn = createCallbackFcn(app, @LBC_UncCalcButtonPushed, true);
            app.LBC_UncCalc.Icon = '080-calculator.png';
            app.LBC_UncCalc.IconAlignment = 'center';
            app.LBC_UncCalc.Tooltip = {'Calculate Uncertainties using Monte Carlo'};
            app.LBC_UncCalc.Layout.Row = 2;
            app.LBC_UncCalc.Layout.Column = 26;
            app.LBC_UncCalc.Text = '';

            % Create PxLabel
            app.PxLabel = uilabel(app.CalibrateGridLayout);
            app.PxLabel.HorizontalAlignment = 'right';
            app.PxLabel.FontSize = 9;
            app.PxLabel.Layout.Row = 2;
            app.PxLabel.Layout.Column = [23 24];
            app.PxLabel.Text = 'Px';

            % Create LBC_ValueMC
            app.LBC_ValueMC = uieditfield(app.CalibrateGridLayout, 'numeric');
            app.LBC_ValueMC.Limits = [1 1000000];
            app.LBC_ValueMC.RoundFractionalValues = 'on';
            app.LBC_ValueMC.ValueDisplayFormat = '%.0f';
            app.LBC_ValueMC.HorizontalAlignment = 'center';
            app.LBC_ValueMC.FontSize = 10;
            app.LBC_ValueMC.Layout.Row = 2;
            app.LBC_ValueMC.Layout.Column = 25;
            app.LBC_ValueMC.Value = 20;

            % Create LBC_NbSimMC
            app.LBC_NbSimMC = uieditfield(app.CalibrateGridLayout, 'numeric');
            app.LBC_NbSimMC.Limits = [0 1000000];
            app.LBC_NbSimMC.RoundFractionalValues = 'on';
            app.LBC_NbSimMC.ValueDisplayFormat = '%.0f';
            app.LBC_NbSimMC.HorizontalAlignment = 'center';
            app.LBC_NbSimMC.FontSize = 10;
            app.LBC_NbSimMC.Layout.Row = 1;
            app.LBC_NbSimMC.Layout.Column = [25 26];
            app.LBC_NbSimMC.Value = 100;

            % Create SimLabel
            app.SimLabel = uilabel(app.CalibrateGridLayout);
            app.SimLabel.HorizontalAlignment = 'right';
            app.SimLabel.FontSize = 9;
            app.SimLabel.Layout.Row = 1;
            app.SimLabel.Layout.Column = [23 24];
            app.SimLabel.Text = 'Sim';

            % Create Calibrate_LOD_CalcButton
            app.Calibrate_LOD_CalcButton = uibutton(app.CalibrateGridLayout, 'push');
            app.Calibrate_LOD_CalcButton.ButtonPushedFcn = createCallbackFcn(app, @Calibrate_LOD_CalcButtonPushed, true);
            app.Calibrate_LOD_CalcButton.Icon = '030-eject.png';
            app.Calibrate_LOD_CalcButton.IconAlignment = 'center';
            app.Calibrate_LOD_CalcButton.Tooltip = {'Draw ROI and calculate LOD for pixels '};
            app.Calibrate_LOD_CalcButton.Layout.Row = 3;
            app.Calibrate_LOD_CalcButton.Layout.Column = 36;
            app.Calibrate_LOD_CalcButton.Text = '';

            % Create Calibrate_LOD_menu
            app.Calibrate_LOD_menu = uidropdown(app.CalibrateGridLayout);
            app.Calibrate_LOD_menu.Items = {'Rectangle ROI', 'Polygon ROI'};
            app.Calibrate_LOD_menu.Tooltip = {'Method for ROI drawing'};
            app.Calibrate_LOD_menu.FontSize = 10;
            app.Calibrate_LOD_menu.Layout.Row = 3;
            app.Calibrate_LOD_menu.Layout.Column = [31 35];
            app.Calibrate_LOD_menu.Value = 'Rectangle ROI';

            % Create Calibrate_ApplyLODfilter
            app.Calibrate_ApplyLODfilter = uibutton(app.CalibrateGridLayout, 'push');
            app.Calibrate_ApplyLODfilter.ButtonPushedFcn = createCallbackFcn(app, @Calibrate_ApplyLODfilterButtonPushed, true);
            app.Calibrate_ApplyLODfilter.Icon = '062-forbidden.png';
            app.Calibrate_ApplyLODfilter.IconAlignment = 'center';
            app.Calibrate_ApplyLODfilter.Tooltip = {'Duplicate and apply LOD filter'};
            app.Calibrate_ApplyLODfilter.Layout.Row = 1;
            app.Calibrate_ApplyLODfilter.Layout.Column = 33;
            app.Calibrate_ApplyLODfilter.Text = '';

            % Create POINTCOUNTINGLabel
            app.POINTCOUNTINGLabel = uilabel(app.CalibrateGridLayout);
            app.POINTCOUNTINGLabel.HorizontalAlignment = 'center';
            app.POINTCOUNTINGLabel.VerticalAlignment = 'bottom';
            app.POINTCOUNTINGLabel.FontSize = 9;
            app.POINTCOUNTINGLabel.FontColor = [0.149 0.149 0.149];
            app.POINTCOUNTINGLabel.Layout.Row = 4;
            app.POINTCOUNTINGLabel.Layout.Column = [23 27];
            app.POINTCOUNTINGLabel.Text = 'POINT COUNTING';

            % Create FUNCTIONSTab
            app.FUNCTIONSTab = uitab(app.TabButtonGroup);
            app.FUNCTIONSTab.AutoResizeChildren = 'off';
            app.FUNCTIONSTab.Title = 'FUNCTIONS';

            % Create GridLayout_ExternalFctTab
            app.GridLayout_ExternalFctTab = uigridlayout(app.FUNCTIONSTab);
            app.GridLayout_ExternalFctTab.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.3x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.3x', '1x', '1x', '1x', '1x', '1x', '0.3x', '1x', '0.15x'};
            app.GridLayout_ExternalFctTab.RowHeight = {'1x', '1x', '1x', '0.6x'};
            app.GridLayout_ExternalFctTab.ColumnSpacing = 4;
            app.GridLayout_ExternalFctTab.RowSpacing = 4;
            app.GridLayout_ExternalFctTab.Padding = [5 5 5 5];

            % Create NORMALIZATIONSTRUCTURALFORMULALabel
            app.NORMALIZATIONSTRUCTURALFORMULALabel = uilabel(app.GridLayout_ExternalFctTab);
            app.NORMALIZATIONSTRUCTURALFORMULALabel.HorizontalAlignment = 'center';
            app.NORMALIZATIONSTRUCTURALFORMULALabel.VerticalAlignment = 'bottom';
            app.NORMALIZATIONSTRUCTURALFORMULALabel.FontSize = 9;
            app.NORMALIZATIONSTRUCTURALFORMULALabel.FontColor = [0.149 0.149 0.149];
            app.NORMALIZATIONSTRUCTURALFORMULALabel.Layout.Row = 4;
            app.NORMALIZATIONSTRUCTURALFORMULALabel.Layout.Column = [1 14];
            app.NORMALIZATIONSTRUCTURALFORMULALabel.Text = 'NORMALIZATION & STRUCTURAL FORMULA';

            % Create SF_OxygenNorm_CheckBox
            app.SF_OxygenNorm_CheckBox = uicheckbox(app.GridLayout_ExternalFctTab);
            app.SF_OxygenNorm_CheckBox.ValueChangedFcn = createCallbackFcn(app, @Function_SF_OxygenNorm_CheckBoxValueChanged, true);
            app.SF_OxygenNorm_CheckBox.Text = 'Oxygen';
            app.SF_OxygenNorm_CheckBox.FontSize = 11;
            app.SF_OxygenNorm_CheckBox.Layout.Row = 2;
            app.SF_OxygenNorm_CheckBox.Layout.Column = [1 3];

            % Create SF_CationNorm_CheckBox
            app.SF_CationNorm_CheckBox = uicheckbox(app.GridLayout_ExternalFctTab);
            app.SF_CationNorm_CheckBox.ValueChangedFcn = createCallbackFcn(app, @Function_SF_CationNorm_CheckBoxValueChanged, true);
            app.SF_CationNorm_CheckBox.Text = 'Cations';
            app.SF_CationNorm_CheckBox.FontSize = 11;
            app.SF_CationNorm_CheckBox.Layout.Row = 3;
            app.SF_CationNorm_CheckBox.Layout.Column = [1 3];

            % Create SF_NbOx
            app.SF_NbOx = uieditfield(app.GridLayout_ExternalFctTab, 'numeric');
            app.SF_NbOx.HorizontalAlignment = 'center';
            app.SF_NbOx.FontSize = 10;
            app.SF_NbOx.Layout.Row = 2;
            app.SF_NbOx.Layout.Column = [4 5];
            app.SF_NbOx.Value = 6;

            % Create SF_NbCat
            app.SF_NbCat = uieditfield(app.GridLayout_ExternalFctTab, 'numeric');
            app.SF_NbCat.HorizontalAlignment = 'center';
            app.SF_NbCat.FontSize = 10;
            app.SF_NbCat.Layout.Row = 3;
            app.SF_NbCat.Layout.Column = [4 5];
            app.SF_NbCat.Value = 1;

            % Create SF_Function_CheckBox
            app.SF_Function_CheckBox = uicheckbox(app.GridLayout_ExternalFctTab);
            app.SF_Function_CheckBox.ValueChangedFcn = createCallbackFcn(app, @Function_SF_Function_CheckBoxValueChanged, true);
            app.SF_Function_CheckBox.Text = 'Function';
            app.SF_Function_CheckBox.FontSize = 11;
            app.SF_Function_CheckBox.Layout.Row = 1;
            app.SF_Function_CheckBox.Layout.Column = [1 3];
            app.SF_Function_CheckBox.Value = true;

            % Create SF_MineralList
            app.SF_MineralList = uidropdown(app.GridLayout_ExternalFctTab);
            app.SF_MineralList.ValueChangedFcn = createCallbackFcn(app, @Function_SF_MineralListValueChanged, true);
            app.SF_MineralList.FontSize = 10;
            app.SF_MineralList.Layout.Row = 1;
            app.SF_MineralList.Layout.Column = [4 7];

            % Create SF_FunctionList
            app.SF_FunctionList = uidropdown(app.GridLayout_ExternalFctTab);
            app.SF_FunctionList.FontSize = 10;
            app.SF_FunctionList.Layout.Row = 1;
            app.SF_FunctionList.Layout.Column = [8 13];

            % Create SF_ApplyButton
            app.SF_ApplyButton = uibutton(app.GridLayout_ExternalFctTab, 'push');
            app.SF_ApplyButton.ButtonPushedFcn = createCallbackFcn(app, @Function_SF_ApplyButtonPushed, true);
            app.SF_ApplyButton.Icon = '311-app.png';
            app.SF_ApplyButton.IconAlignment = 'top';
            app.SF_ApplyButton.FontSize = 10;
            app.SF_ApplyButton.Tooltip = {'Apply the Selected Function (Structural Formula)'};
            app.SF_ApplyButton.Layout.Row = [2 3];
            app.SF_ApplyButton.Layout.Column = [13 14];
            app.SF_ApplyButton.Text = 'Apply';

            % Create SF_HelpFile
            app.SF_HelpFile = uibutton(app.GridLayout_ExternalFctTab, 'push');
            app.SF_HelpFile.ButtonPushedFcn = createCallbackFcn(app, @Function_SF_HelpFileButtonPushed, true);
            app.SF_HelpFile.Icon = '061-info_C.png';
            app.SF_HelpFile.Tooltip = {'Display Function Description'};
            app.SF_HelpFile.Layout.Row = 1;
            app.SF_HelpFile.Layout.Column = 14;
            app.SF_HelpFile.Text = '';

            % Create THERMOBAROMETRYOTHERMETHODSLabel
            app.THERMOBAROMETRYOTHERMETHODSLabel = uilabel(app.GridLayout_ExternalFctTab);
            app.THERMOBAROMETRYOTHERMETHODSLabel.HorizontalAlignment = 'center';
            app.THERMOBAROMETRYOTHERMETHODSLabel.VerticalAlignment = 'bottom';
            app.THERMOBAROMETRYOTHERMETHODSLabel.FontSize = 9;
            app.THERMOBAROMETRYOTHERMETHODSLabel.FontColor = [0.149 0.149 0.149];
            app.THERMOBAROMETRYOTHERMETHODSLabel.Layout.Row = 4;
            app.THERMOBAROMETRYOTHERMETHODSLabel.Layout.Column = [16 27];
            app.THERMOBAROMETRYOTHERMETHODSLabel.Text = 'THERMOBAROMETRY & OTHER METHODS';

            % Create Other_MethodList
            app.Other_MethodList = uidropdown(app.GridLayout_ExternalFctTab);
            app.Other_MethodList.Items = {'Map-mode', 'Multi-equilibrium'};
            app.Other_MethodList.ValueChangedFcn = createCallbackFcn(app, @Function_Other_MethodListValueChanged, true);
            app.Other_MethodList.FontSize = 10;
            app.Other_MethodList.Layout.Row = 1;
            app.Other_MethodList.Layout.Column = [16 19];
            app.Other_MethodList.Value = 'Map-mode';

            % Create Other_FunctionList
            app.Other_FunctionList = uidropdown(app.GridLayout_ExternalFctTab);
            app.Other_FunctionList.FontSize = 10;
            app.Other_FunctionList.Layout.Row = 2;
            app.Other_FunctionList.Layout.Column = [20 24];

            % Create Other_MultiEquilibriumButton
            app.Other_MultiEquilibriumButton = uibutton(app.GridLayout_ExternalFctTab, 'push');
            app.Other_MultiEquilibriumButton.ButtonPushedFcn = createCallbackFcn(app, @Function_Other_MultiEquilibriumButtonPushed, true);
            app.Other_MultiEquilibriumButton.Icon = '093-binoculars.png';
            app.Other_MultiEquilibriumButton.IconAlignment = 'top';
            app.Other_MultiEquilibriumButton.FontSize = 9;
            app.Other_MultiEquilibriumButton.Tooltip = {'Apply the Selected Function (Other)'};
            app.Other_MultiEquilibriumButton.Layout.Row = [2 3];
            app.Other_MultiEquilibriumButton.Layout.Column = [28 29];
            app.Other_MultiEquilibriumButton.Text = 'Add ROIs';

            % Create Other_ApplyButton
            app.Other_ApplyButton = uibutton(app.GridLayout_ExternalFctTab, 'push');
            app.Other_ApplyButton.ButtonPushedFcn = createCallbackFcn(app, @Function_Other_ApplyButtonPushed, true);
            app.Other_ApplyButton.Icon = '311-app.png';
            app.Other_ApplyButton.IconAlignment = 'top';
            app.Other_ApplyButton.FontSize = 10;
            app.Other_ApplyButton.Tooltip = {'Apply the Selected Function (Other)'};
            app.Other_ApplyButton.Layout.Row = [2 3];
            app.Other_ApplyButton.Layout.Column = [26 27];
            app.Other_ApplyButton.Text = 'Apply';

            % Create Other_MineralList
            app.Other_MineralList = uidropdown(app.GridLayout_ExternalFctTab);
            app.Other_MineralList.ValueChangedFcn = createCallbackFcn(app, @Function_Other_MineralListValueChanged, true);
            app.Other_MineralList.FontSize = 10;
            app.Other_MineralList.Layout.Row = 1;
            app.Other_MineralList.Layout.Column = [20 25];

            % Create Other_HelpFile
            app.Other_HelpFile = uibutton(app.GridLayout_ExternalFctTab, 'push');
            app.Other_HelpFile.ButtonPushedFcn = createCallbackFcn(app, @Function_Other_HelpFileButtonPushed, true);
            app.Other_HelpFile.Icon = '061-info_C.png';
            app.Other_HelpFile.Tooltip = {'Display Function Description'};
            app.Other_HelpFile.Layout.Row = 2;
            app.Other_HelpFile.Layout.Column = 25;
            app.Other_HelpFile.Text = '';

            % Create Other_ROI_menu
            app.Other_ROI_menu = uidropdown(app.GridLayout_ExternalFctTab);
            app.Other_ROI_menu.Items = {'Circle ROI', 'Rectangle ROI', 'Polygon ROI'};
            app.Other_ROI_menu.Tooltip = {'Method for ROI drawing'};
            app.Other_ROI_menu.FontSize = 10;
            app.Other_ROI_menu.Layout.Row = 1;
            app.Other_ROI_menu.Layout.Column = [26 29];
            app.Other_ROI_menu.Value = 'Circle ROI';

            % Create SF_ExportMinCompositions
            app.SF_ExportMinCompositions = uibutton(app.GridLayout_ExternalFctTab, 'push');
            app.SF_ExportMinCompositions.ButtonPushedFcn = createCallbackFcn(app, @Function_SF_ExportMinCompositionsButtonPushed, true);
            app.SF_ExportMinCompositions.Icon = '093-binoculars.png';
            app.SF_ExportMinCompositions.Tooltip = {'Add ROI for Exporting Compositions'};
            app.SF_ExportMinCompositions.Layout.Row = 3;
            app.SF_ExportMinCompositions.Layout.Column = 12;
            app.SF_ExportMinCompositions.Text = '';

            % Create SF_ROI_menu
            app.SF_ROI_menu = uidropdown(app.GridLayout_ExternalFctTab);
            app.SF_ROI_menu.Items = {'Circle ROI', 'Rectangle ROI', 'Polygon ROI'};
            app.SF_ROI_menu.Tooltip = {'Method for ROI drawing'};
            app.SF_ROI_menu.FontSize = 10;
            app.SF_ROI_menu.Layout.Row = 3;
            app.SF_ROI_menu.Layout.Column = [8 11];
            app.SF_ROI_menu.Value = 'Circle ROI';

            % Create FunctionTab_help
            app.FunctionTab_help = uibutton(app.GridLayout_ExternalFctTab, 'push');
            app.FunctionTab_help.ButtonPushedFcn = createCallbackFcn(app, @Help_FunctionTab_helpButtonPushed, true);
            app.FunctionTab_help.Icon = '061-info.png';
            app.FunctionTab_help.Tooltip = {'Help & Documentation'};
            app.FunctionTab_help.Layout.Row = 1;
            app.FunctionTab_help.Layout.Column = 37;
            app.FunctionTab_help.Text = '';

            % Create Image_20
            app.Image_20 = uiimage(app.GridLayout_ExternalFctTab);
            app.Image_20.Layout.Row = [1 4];
            app.Image_20.Layout.Column = 15;
            app.Image_20.ImageSource = 'ImageDelimiter.png';

            % Create Image_21
            app.Image_21 = uiimage(app.GridLayout_ExternalFctTab);
            app.Image_21.Layout.Row = [1 4];
            app.Image_21.Layout.Column = 30;
            app.Image_21.ImageSource = 'ImageDelimiter.png';

            % Create Image_22
            app.Image_22 = uiimage(app.GridLayout_ExternalFctTab);
            app.Image_22.Layout.Row = [1 4];
            app.Image_22.Layout.Column = 36;
            app.Image_22.ImageSource = 'ImageDelimiter.png';

            % Create SEGMENTCTTab
            app.SEGMENTCTTab = uitab(app.TabButtonGroup);
            app.SEGMENTCTTab.AutoResizeChildren = 'off';
            app.SEGMENTCTTab.Title = 'SEGMENT (CT)';

            % Create GridLayout_SegmentTab
            app.GridLayout_SegmentTab = uigridlayout(app.SEGMENTCTTab);
            app.GridLayout_SegmentTab.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.3x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.3x', '1x', '1x', '0.3x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.3x', '1x', '1x', '1x', '1x', '0.3x', '1x', '0.15x'};
            app.GridLayout_SegmentTab.RowHeight = {'1x', '1x', '1x', '0.6x'};
            app.GridLayout_SegmentTab.ColumnSpacing = 4;
            app.GridLayout_SegmentTab.RowSpacing = 4;
            app.GridLayout_SegmentTab.Padding = [5 5 5 5];

            % Create SEGMENTATIONSCHEMELabel
            app.SEGMENTATIONSCHEMELabel = uilabel(app.GridLayout_SegmentTab);
            app.SEGMENTATIONSCHEMELabel.HorizontalAlignment = 'center';
            app.SEGMENTATIONSCHEMELabel.VerticalAlignment = 'bottom';
            app.SEGMENTATIONSCHEMELabel.FontSize = 9;
            app.SEGMENTATIONSCHEMELabel.FontColor = [0.149 0.149 0.149];
            app.SEGMENTATIONSCHEMELabel.Layout.Row = 4;
            app.SEGMENTATIONSCHEMELabel.Layout.Column = [1 8];
            app.SEGMENTATIONSCHEMELabel.Text = 'SEGMENTATION SCHEME';

            % Create SEGMENTATIONPARAMETERSTESTLabel
            app.SEGMENTATIONPARAMETERSTESTLabel = uilabel(app.GridLayout_SegmentTab);
            app.SEGMENTATIONPARAMETERSTESTLabel.HorizontalAlignment = 'center';
            app.SEGMENTATIONPARAMETERSTESTLabel.VerticalAlignment = 'bottom';
            app.SEGMENTATIONPARAMETERSTESTLabel.FontSize = 9;
            app.SEGMENTATIONPARAMETERSTESTLabel.FontColor = [0.149 0.149 0.149];
            app.SEGMENTATIONPARAMETERSTESTLabel.Layout.Row = 4;
            app.SEGMENTATIONPARAMETERSTESTLabel.Layout.Column = [10 19];
            app.SEGMENTATIONPARAMETERSTESTLabel.Text = 'SEGMENTATION PARAMETERS & TEST';

            % Create APPLYLabel
            app.APPLYLabel = uilabel(app.GridLayout_SegmentTab);
            app.APPLYLabel.HorizontalAlignment = 'center';
            app.APPLYLabel.VerticalAlignment = 'bottom';
            app.APPLYLabel.FontSize = 9;
            app.APPLYLabel.FontColor = [0.149 0.149 0.149];
            app.APPLYLabel.Layout.Row = 4;
            app.APPLYLabel.Layout.Column = [21 22];
            app.APPLYLabel.Text = 'APPLY';

            % Create Segment_AddScheme
            app.Segment_AddScheme = uibutton(app.GridLayout_SegmentTab, 'push');
            app.Segment_AddScheme.ButtonPushedFcn = createCallbackFcn(app, @Segment_AddSchemeButtonPushed, true);
            app.Segment_AddScheme.Icon = '073-add.png';
            app.Segment_AddScheme.IconAlignment = 'top';
            app.Segment_AddScheme.FontSize = 10;
            app.Segment_AddScheme.Tooltip = {'Add Segmentation Scheme / Group / Range'};
            app.Segment_AddScheme.Layout.Row = [1 2];
            app.Segment_AddScheme.Layout.Column = [1 2];
            app.Segment_AddScheme.Text = 'Add';

            % Create Segment_NameField
            app.Segment_NameField = uieditfield(app.GridLayout_SegmentTab, 'text');
            app.Segment_NameField.HorizontalAlignment = 'center';
            app.Segment_NameField.FontSize = 10;
            app.Segment_NameField.Layout.Row = 1;
            app.Segment_NameField.Layout.Column = [3 8];
            app.Segment_NameField.Value = 'SegmentationScheme_1';

            % Create Segment_SegmentButton
            app.Segment_SegmentButton = uibutton(app.GridLayout_SegmentTab, 'push');
            app.Segment_SegmentButton.ButtonPushedFcn = createCallbackFcn(app, @Segment_SegmentButtonButtonPushed, true);
            app.Segment_SegmentButton.Icon = '091-share.png';
            app.Segment_SegmentButton.IconAlignment = 'top';
            app.Segment_SegmentButton.FontSize = 10;
            app.Segment_SegmentButton.Tooltip = {'Segment All Images'};
            app.Segment_SegmentButton.Layout.Row = [1 2];
            app.Segment_SegmentButton.Layout.Column = [21 22];
            app.Segment_SegmentButton.Text = 'Segment';

            % Create Segment_ErosionFilterCheckBox
            app.Segment_ErosionFilterCheckBox = uicheckbox(app.GridLayout_SegmentTab);
            app.Segment_ErosionFilterCheckBox.Tooltip = {'Activate grain boundary filter'};
            app.Segment_ErosionFilterCheckBox.Text = 'Filter GB';
            app.Segment_ErosionFilterCheckBox.FontSize = 11;
            app.Segment_ErosionFilterCheckBox.Layout.Row = 1;
            app.Segment_ErosionFilterCheckBox.Layout.Column = [10 13];
            app.Segment_ErosionFilterCheckBox.Value = true;

            % Create Segment_ErosionFilterSpinner
            app.Segment_ErosionFilterSpinner = uispinner(app.GridLayout_SegmentTab);
            app.Segment_ErosionFilterSpinner.Step = 0.01;
            app.Segment_ErosionFilterSpinner.Limits = [0 1];
            app.Segment_ErosionFilterSpinner.FontSize = 10;
            app.Segment_ErosionFilterSpinner.Layout.Row = 2;
            app.Segment_ErosionFilterSpinner.Layout.Column = [13 14];
            app.Segment_ErosionFilterSpinner.Value = 0.15;

            % Create Segment_TrySegmentationButton
            app.Segment_TrySegmentationButton = uibutton(app.GridLayout_SegmentTab, 'push');
            app.Segment_TrySegmentationButton.ButtonPushedFcn = createCallbackFcn(app, @Segment_TrySegmentationButtonPushed, true);
            app.Segment_TrySegmentationButton.Icon = '091-share.png';
            app.Segment_TrySegmentationButton.IconAlignment = 'top';
            app.Segment_TrySegmentationButton.FontSize = 11;
            app.Segment_TrySegmentationButton.Tooltip = {'Segment Selected Image'};
            app.Segment_TrySegmentationButton.Layout.Row = 3;
            app.Segment_TrySegmentationButton.Layout.Column = 19;
            app.Segment_TrySegmentationButton.Text = '';

            % Create Segment_ButtonPlot
            app.Segment_ButtonPlot = uibutton(app.GridLayout_SegmentTab, 'push');
            app.Segment_ButtonPlot.ButtonPushedFcn = createCallbackFcn(app, @Segment_ButtonPlotPushed, true);
            app.Segment_ButtonPlot.Icon = '087-menu-rot.png';
            app.Segment_ButtonPlot.IconAlignment = 'top';
            app.Segment_ButtonPlot.FontSize = 10;
            app.Segment_ButtonPlot.Tooltip = {'Plot Phase Proportions'};
            app.Segment_ButtonPlot.Layout.Row = [1 2];
            app.Segment_ButtonPlot.Layout.Column = [30 31];
            app.Segment_ButtonPlot.Text = 'Plot';

            % Create DATAVISUALIZATIONLabel
            app.DATAVISUALIZATIONLabel = uilabel(app.GridLayout_SegmentTab);
            app.DATAVISUALIZATIONLabel.HorizontalAlignment = 'center';
            app.DATAVISUALIZATIONLabel.VerticalAlignment = 'bottom';
            app.DATAVISUALIZATIONLabel.FontSize = 9;
            app.DATAVISUALIZATIONLabel.FontColor = [0.149 0.149 0.149];
            app.DATAVISUALIZATIONLabel.Layout.Row = 4;
            app.DATAVISUALIZATIONLabel.Layout.Column = [24 32];
            app.DATAVISUALIZATIONLabel.Text = 'DATA VISUALIZATION';

            % Create SegmentTab_help
            app.SegmentTab_help = uibutton(app.GridLayout_SegmentTab, 'push');
            app.SegmentTab_help.ButtonPushedFcn = createCallbackFcn(app, @Help_SegmentTab_helpButtonPushed, true);
            app.SegmentTab_help.Icon = '061-info.png';
            app.SegmentTab_help.Tooltip = {'Help & Documentation'};
            app.SegmentTab_help.Layout.Row = 1;
            app.SegmentTab_help.Layout.Column = 39;
            app.SegmentTab_help.Text = '';

            % Create Segment_SavePhaseProp
            app.Segment_SavePhaseProp = uibutton(app.GridLayout_SegmentTab, 'push');
            app.Segment_SavePhaseProp.ButtonPushedFcn = createCallbackFcn(app, @Segment_SavePhasePropButtonPushed, true);
            app.Segment_SavePhaseProp.Icon = '022-save.png';
            app.Segment_SavePhaseProp.IconAlignment = 'top';
            app.Segment_SavePhaseProp.FontSize = 11;
            app.Segment_SavePhaseProp.Tooltip = {'Save phase proportions'};
            app.Segment_SavePhaseProp.Layout.Row = 1;
            app.Segment_SavePhaseProp.Layout.Column = 32;
            app.Segment_SavePhaseProp.Text = '';

            % Create Image_10
            app.Image_10 = uiimage(app.GridLayout_SegmentTab);
            app.Image_10.Layout.Row = [1 4];
            app.Image_10.Layout.Column = 9;
            app.Image_10.ImageSource = 'ImageDelimiter.png';

            % Create Image_11
            app.Image_11 = uiimage(app.GridLayout_SegmentTab);
            app.Image_11.Layout.Row = [1 4];
            app.Image_11.Layout.Column = 20;
            app.Image_11.ImageSource = 'ImageDelimiter.png';

            % Create Image_12
            app.Image_12 = uiimage(app.GridLayout_SegmentTab);
            app.Image_12.Layout.Row = [1 4];
            app.Image_12.Layout.Column = 23;
            app.Image_12.ImageSource = 'ImageDelimiter.png';

            % Create Image_13
            app.Image_13 = uiimage(app.GridLayout_SegmentTab);
            app.Image_13.Layout.Row = [1 4];
            app.Image_13.Layout.Column = 33;
            app.Image_13.ImageSource = 'ImageDelimiter.png';

            % Create Image_14
            app.Image_14 = uiimage(app.GridLayout_SegmentTab);
            app.Image_14.Layout.Row = [1 4];
            app.Image_14.Layout.Column = 38;
            app.Image_14.ImageSource = 'ImageDelimiter.png';

            % Create Segment_InterpFilterCheckBox
            app.Segment_InterpFilterCheckBox = uicheckbox(app.GridLayout_SegmentTab);
            app.Segment_InterpFilterCheckBox.Tooltip = {'Activate grain boundary filter'};
            app.Segment_InterpFilterCheckBox.Text = 'Interp GB';
            app.Segment_InterpFilterCheckBox.FontSize = 11;
            app.Segment_InterpFilterCheckBox.Layout.Row = 3;
            app.Segment_InterpFilterCheckBox.Layout.Column = [10 13];
            app.Segment_InterpFilterCheckBox.Value = true;

            % Create Segment_InterpOrderSpinner
            app.Segment_InterpOrderSpinner = uispinner(app.GridLayout_SegmentTab);
            app.Segment_InterpOrderSpinner.Limits = [1 20];
            app.Segment_InterpOrderSpinner.RoundFractionalValues = 'on';
            app.Segment_InterpOrderSpinner.FontSize = 10;
            app.Segment_InterpOrderSpinner.Layout.Row = 3;
            app.Segment_InterpOrderSpinner.Layout.Column = [17 18];
            app.Segment_InterpOrderSpinner.Value = 5;

            % Create OrderLabel
            app.OrderLabel = uilabel(app.GridLayout_SegmentTab);
            app.OrderLabel.HorizontalAlignment = 'right';
            app.OrderLabel.FontSize = 9;
            app.OrderLabel.Layout.Row = 3;
            app.OrderLabel.Layout.Column = [15 16];
            app.OrderLabel.Text = 'Order';

            % Create Segment_GradientMapButton
            app.Segment_GradientMapButton = uibutton(app.GridLayout_SegmentTab, 'push');
            app.Segment_GradientMapButton.ButtonPushedFcn = createCallbackFcn(app, @Segment_GradientMapButtonPushed, true);
            app.Segment_GradientMapButton.Icon = '104-wifi.png';
            app.Segment_GradientMapButton.IconAlignment = 'top';
            app.Segment_GradientMapButton.FontSize = 11;
            app.Segment_GradientMapButton.Tooltip = {'Calculate GB Map'};
            app.Segment_GradientMapButton.Layout.Row = 2;
            app.Segment_GradientMapButton.Layout.Column = 19;
            app.Segment_GradientMapButton.Text = '';

            % Create Segment_SmoothingFact
            app.Segment_SmoothingFact = uispinner(app.GridLayout_SegmentTab);
            app.Segment_SmoothingFact.Step = 2;
            app.Segment_SmoothingFact.Limits = [1 300];
            app.Segment_SmoothingFact.RoundFractionalValues = 'on';
            app.Segment_SmoothingFact.FontSize = 10;
            app.Segment_SmoothingFact.Layout.Row = 1;
            app.Segment_SmoothingFact.Layout.Column = [28 29];
            app.Segment_SmoothingFact.Value = 30;

            % Create SmoothingFactLabel
            app.SmoothingFactLabel = uilabel(app.GridLayout_SegmentTab);
            app.SmoothingFactLabel.HorizontalAlignment = 'right';
            app.SmoothingFactLabel.FontSize = 9;
            app.SmoothingFactLabel.Layout.Row = 1;
            app.SmoothingFactLabel.Layout.Column = [24 27];
            app.SmoothingFactLabel.Text = 'Smoothing Fact.';

            % Create SchemeDropDown
            app.SchemeDropDown = uidropdown(app.GridLayout_SegmentTab);
            app.SchemeDropDown.Items = {'Local Gradient', 'Local Standard Deviation', 'Local Range', 'Local Entropy'};
            app.SchemeDropDown.ValueChangedFcn = createCallbackFcn(app, @Segment_SchemeDropDownValueChanged, true);
            app.SchemeDropDown.Layout.Row = 1;
            app.SchemeDropDown.Layout.Column = [14 19];
            app.SchemeDropDown.Value = 'Local Gradient';

            % Create ThresholdLabel
            app.ThresholdLabel = uilabel(app.GridLayout_SegmentTab);
            app.ThresholdLabel.HorizontalAlignment = 'right';
            app.ThresholdLabel.FontSize = 9;
            app.ThresholdLabel.Layout.Row = 2;
            app.ThresholdLabel.Layout.Column = [10 12];
            app.ThresholdLabel.Text = 'Threshold';

            % Create OrderLabel_2
            app.OrderLabel_2 = uilabel(app.GridLayout_SegmentTab);
            app.OrderLabel_2.HorizontalAlignment = 'right';
            app.OrderLabel_2.FontSize = 9;
            app.OrderLabel_2.Layout.Row = 2;
            app.OrderLabel_2.Layout.Column = [15 16];
            app.OrderLabel_2.Text = 'Order';

            % Create Segment_InterpOrderFilterGBSpinner
            app.Segment_InterpOrderFilterGBSpinner = uispinner(app.GridLayout_SegmentTab);
            app.Segment_InterpOrderFilterGBSpinner.Step = 2;
            app.Segment_InterpOrderFilterGBSpinner.Limits = [1 49];
            app.Segment_InterpOrderFilterGBSpinner.RoundFractionalValues = 'on';
            app.Segment_InterpOrderFilterGBSpinner.Editable = 'off';
            app.Segment_InterpOrderFilterGBSpinner.FontSize = 10;
            app.Segment_InterpOrderFilterGBSpinner.Layout.Row = 2;
            app.Segment_InterpOrderFilterGBSpinner.Layout.Column = [17 18];
            app.Segment_InterpOrderFilterGBSpinner.Value = 3;

            % Create Segment_ExportROI_TXT
            app.Segment_ExportROI_TXT = uibutton(app.GridLayout_SegmentTab, 'push');
            app.Segment_ExportROI_TXT.ButtonPushedFcn = createCallbackFcn(app, @Segment_ExportROI_TXTButtonPushed, true);
            app.Segment_ExportROI_TXT.Icon = '096-upload.png';
            app.Segment_ExportROI_TXT.IconAlignment = 'top';
            app.Segment_ExportROI_TXT.FontSize = 11;
            app.Segment_ExportROI_TXT.Tooltip = {'Export ROI as MAT file'};
            app.Segment_ExportROI_TXT.Layout.Row = 2;
            app.Segment_ExportROI_TXT.Layout.Column = 24;
            app.Segment_ExportROI_TXT.Text = '';

            % Create ADDONSTab
            app.ADDONSTab = uitab(app.TabButtonGroup);
            app.ADDONSTab.AutoResizeChildren = 'off';
            app.ADDONSTab.Title = 'ADD-ONS';

            % Create GridLayout_AddonsTab
            app.GridLayout_AddonsTab = uigridlayout(app.ADDONSTab);
            app.GridLayout_AddonsTab.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.3x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.3x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout_AddonsTab.RowHeight = {'1x', '1x', '1x', '0.6x'};
            app.GridLayout_AddonsTab.ColumnSpacing = 4;
            app.GridLayout_AddonsTab.RowSpacing = 4;
            app.GridLayout_AddonsTab.Padding = [5 5 5 5];
            app.GridLayout_AddonsTab.Tooltip = {'Open Bingo-Antidote'};

            % Create AddonsTab_help
            app.AddonsTab_help = uibutton(app.GridLayout_AddonsTab, 'push');
            app.AddonsTab_help.ButtonPushedFcn = createCallbackFcn(app, @Help_AddonsTab_helpButtonPushed, true);
            app.AddonsTab_help.Icon = '061-info.png';
            app.AddonsTab_help.Tooltip = {'Help & Documentation'};
            app.AddonsTab_help.Layout.Row = 1;
            app.AddonsTab_help.Layout.Column = 35;
            app.AddonsTab_help.Text = '';

            % Create Addons_BingoAntidote_2
            app.Addons_BingoAntidote_2 = uibutton(app.GridLayout_AddonsTab, 'push');
            app.Addons_BingoAntidote_2.ButtonPushedFcn = createCallbackFcn(app, @Addons_BingoAntidoteButtonPushed, true);
            app.Addons_BingoAntidote_2.Icon = 'logo_transparent.png';
            app.Addons_BingoAntidote_2.IconAlignment = 'top';
            app.Addons_BingoAntidote_2.FontSize = 6;
            app.Addons_BingoAntidote_2.Tooltip = {'Start Bingo-Antidote'};
            app.Addons_BingoAntidote_2.Layout.Row = [1 2];
            app.Addons_BingoAntidote_2.Layout.Column = [12 13];
            app.Addons_BingoAntidote_2.Text = 'Bingo-Antidote';

            % Create THERMODYNAMICMODELINGLabel
            app.THERMODYNAMICMODELINGLabel = uilabel(app.GridLayout_AddonsTab);
            app.THERMODYNAMICMODELINGLabel.HorizontalAlignment = 'center';
            app.THERMODYNAMICMODELINGLabel.VerticalAlignment = 'bottom';
            app.THERMODYNAMICMODELINGLabel.FontSize = 9;
            app.THERMODYNAMICMODELINGLabel.FontColor = [0.149 0.149 0.149];
            app.THERMODYNAMICMODELINGLabel.Layout.Row = 4;
            app.THERMODYNAMICMODELINGLabel.Layout.Column = [12 21];
            app.THERMODYNAMICMODELINGLabel.Text = 'THERMODYNAMIC MODELING';

            % Create Image_32
            app.Image_32 = uiimage(app.GridLayout_AddonsTab);
            app.Image_32.Layout.Row = [1 4];
            app.Image_32.Layout.Column = 11;
            app.Image_32.ImageSource = 'ImageDelimiter.png';

            % Create Image_33
            app.Image_33 = uiimage(app.GridLayout_AddonsTab);
            app.Image_33.Layout.Row = [1 4];
            app.Image_33.Layout.Column = 22;
            app.Image_33.ImageSource = 'ImageDelimiter.png';

            % Create OTHERTOOLSLabel
            app.OTHERTOOLSLabel = uilabel(app.GridLayout_AddonsTab);
            app.OTHERTOOLSLabel.HorizontalAlignment = 'center';
            app.OTHERTOOLSLabel.VerticalAlignment = 'bottom';
            app.OTHERTOOLSLabel.FontSize = 9;
            app.OTHERTOOLSLabel.FontColor = [0.149 0.149 0.149];
            app.OTHERTOOLSLabel.Layout.Row = 4;
            app.OTHERTOOLSLabel.Layout.Column = [1 10];
            app.OTHERTOOLSLabel.Text = 'OTHER TOOLS';

            % Create Tool_ExportCompositions
            app.Tool_ExportCompositions = uibutton(app.GridLayout_AddonsTab, 'push');
            app.Tool_ExportCompositions.ButtonPushedFcn = createCallbackFcn(app, @Tool_ExportCompositionsButtonPushed, true);
            app.Tool_ExportCompositions.Icon = 'xmaptools_ios_icon_HR.png';
            app.Tool_ExportCompositions.IconAlignment = 'top';
            app.Tool_ExportCompositions.FontSize = 8;
            app.Tool_ExportCompositions.Tooltip = {'Open Data Export Module'};
            app.Tool_ExportCompositions.Layout.Row = [1 2];
            app.Tool_ExportCompositions.Layout.Column = [1 2];
            app.Tool_ExportCompositions.Text = 'Export';

            % Create OPTIONSTab
            app.OPTIONSTab = uitab(app.TabButtonGroup);
            app.OPTIONSTab.AutoResizeChildren = 'off';
            app.OPTIONSTab.Title = 'OPTIONS';

            % Create OptionsGridLayout
            app.OptionsGridLayout = uigridlayout(app.OPTIONSTab);
            app.OptionsGridLayout.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.3x', '1x', '1x', '1x', '1x', '1x', '1x', '0.3x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.3x', '1x', '0.1x'};
            app.OptionsGridLayout.RowHeight = {'1x', '1x', '1x', '0.6x'};
            app.OptionsGridLayout.ColumnSpacing = 4;
            app.OptionsGridLayout.RowSpacing = 4;
            app.OptionsGridLayout.Padding = [5 5 5 5];

            % Create ColormapDropDownLabel
            app.ColormapDropDownLabel = uilabel(app.OptionsGridLayout);
            app.ColormapDropDownLabel.FontSize = 9;
            app.ColormapDropDownLabel.Layout.Row = 1;
            app.ColormapDropDownLabel.Layout.Column = [1 2];
            app.ColormapDropDownLabel.Text = 'Colormap';

            % Create Options_ColormapDropDown
            app.Options_ColormapDropDown = uidropdown(app.OptionsGridLayout);
            app.Options_ColormapDropDown.ValueChangedFcn = createCallbackFcn(app, @Options_UpdateColormap, true);
            app.Options_ColormapDropDown.Tooltip = {'Select the active colormap'};
            app.Options_ColormapDropDown.FontSize = 10;
            app.Options_ColormapDropDown.Layout.Row = 1;
            app.Options_ColormapDropDown.Layout.Column = [3 6];

            % Create Options_LowerCheckBox
            app.Options_LowerCheckBox = uicheckbox(app.OptionsGridLayout);
            app.Options_LowerCheckBox.ValueChangedFcn = createCallbackFcn(app, @Options_UpdateColormap, true);
            app.Options_LowerCheckBox.Tooltip = {'Add a lower color layer'};
            app.Options_LowerCheckBox.Text = 'Lower';
            app.Options_LowerCheckBox.FontSize = 9;
            app.Options_LowerCheckBox.Layout.Row = 2;
            app.Options_LowerCheckBox.Layout.Column = [1 2];
            app.Options_LowerCheckBox.Value = true;

            % Create Options_UpperCheckBox
            app.Options_UpperCheckBox = uicheckbox(app.OptionsGridLayout);
            app.Options_UpperCheckBox.ValueChangedFcn = createCallbackFcn(app, @Options_UpdateColormap, true);
            app.Options_UpperCheckBox.Tooltip = {'Add an upper color layer'};
            app.Options_UpperCheckBox.Text = 'Upper';
            app.Options_UpperCheckBox.FontSize = 9;
            app.Options_UpperCheckBox.Layout.Row = 2;
            app.Options_UpperCheckBox.Layout.Column = [6 7];

            % Create Options_LowerColor
            app.Options_LowerColor = uidropdown(app.OptionsGridLayout);
            app.Options_LowerColor.Items = {'Black', 'White'};
            app.Options_LowerColor.ValueChangedFcn = createCallbackFcn(app, @Options_UpdateColormap, true);
            app.Options_LowerColor.Tooltip = {'Select the color for the lower layer'};
            app.Options_LowerColor.FontSize = 10;
            app.Options_LowerColor.Layout.Row = 2;
            app.Options_LowerColor.Layout.Column = [3 5];
            app.Options_LowerColor.Value = 'Black';

            % Create Options_UpperColor
            app.Options_UpperColor = uidropdown(app.OptionsGridLayout);
            app.Options_UpperColor.Items = {'Black', 'White'};
            app.Options_UpperColor.ValueChangedFcn = createCallbackFcn(app, @Options_UpdateColormap, true);
            app.Options_UpperColor.Tooltip = {'Select the color for the upper layer'};
            app.Options_UpperColor.FontSize = 10;
            app.Options_UpperColor.Layout.Row = 2;
            app.Options_UpperColor.Layout.Column = [8 10];
            app.Options_UpperColor.Value = 'Black';

            % Create Options_LogColormap
            app.Options_LogColormap = uicheckbox(app.OptionsGridLayout);
            app.Options_LogColormap.ValueChangedFcn = createCallbackFcn(app, @Options_UpdateColormap, true);
            app.Options_LogColormap.Tooltip = {'Use a log colormap'};
            app.Options_LogColormap.Text = 'LOG';
            app.Options_LogColormap.FontSize = 11;
            app.Options_LogColormap.Layout.Row = 1;
            app.Options_LogColormap.Layout.Column = [9 10];

            % Create Options_Colorbar_Inverse
            app.Options_Colorbar_Inverse = uicheckbox(app.OptionsGridLayout);
            app.Options_Colorbar_Inverse.ValueChangedFcn = createCallbackFcn(app, @Options_Colorbar_InverseValueChanged, true);
            app.Options_Colorbar_Inverse.Tooltip = {'Use a log colormap'};
            app.Options_Colorbar_Inverse.Text = 'Reverse color palette';
            app.Options_Colorbar_Inverse.FontSize = 11;
            app.Options_Colorbar_Inverse.Layout.Row = 3;
            app.Options_Colorbar_Inverse.Layout.Column = [1 6];

            % Create Options_ColormapResEditField
            app.Options_ColormapResEditField = uieditfield(app.OptionsGridLayout, 'numeric');
            app.Options_ColormapResEditField.Limits = [3 9999];
            app.Options_ColormapResEditField.RoundFractionalValues = 'on';
            app.Options_ColormapResEditField.ValueChangedFcn = createCallbackFcn(app, @Options_UpdateColormap, true);
            app.Options_ColormapResEditField.HorizontalAlignment = 'center';
            app.Options_ColormapResEditField.FontSize = 10;
            app.Options_ColormapResEditField.Tooltip = {'Colormap resolution (def. 256)'};
            app.Options_ColormapResEditField.Layout.Row = 1;
            app.Options_ColormapResEditField.Layout.Column = [7 8];
            app.Options_ColormapResEditField.Value = 256;

            % Create COLORBARCOLORPALETTELabel
            app.COLORBARCOLORPALETTELabel = uilabel(app.OptionsGridLayout);
            app.COLORBARCOLORPALETTELabel.HorizontalAlignment = 'center';
            app.COLORBARCOLORPALETTELabel.VerticalAlignment = 'bottom';
            app.COLORBARCOLORPALETTELabel.FontSize = 9;
            app.COLORBARCOLORPALETTELabel.FontColor = [0.149 0.149 0.149];
            app.COLORBARCOLORPALETTELabel.Layout.Row = 4;
            app.COLORBARCOLORPALETTELabel.Layout.Column = [1 17];
            app.COLORBARCOLORPALETTELabel.Text = 'COLOR BAR & COLOR PALETTE';

            % Create Options_DispNegativeValues
            app.Options_DispNegativeValues = uicheckbox(app.OptionsGridLayout);
            app.Options_DispNegativeValues.Text = 'Disp. negative values';
            app.Options_DispNegativeValues.FontSize = 11;
            app.Options_DispNegativeValues.Layout.Row = 3;
            app.Options_DispNegativeValues.Layout.Column = [7 12];

            % Create Options_resolutionLabel
            app.Options_resolutionLabel = uilabel(app.OptionsGridLayout);
            app.Options_resolutionLabel.HorizontalAlignment = 'center';
            app.Options_resolutionLabel.FontSize = 10;
            app.Options_resolutionLabel.Layout.Row = 1;
            app.Options_resolutionLabel.Layout.Column = [26 35];
            app.Options_resolutionLabel.Text = 'resolution x resolution';

            % Create OTHEROPTIONSLabel
            app.OTHEROPTIONSLabel = uilabel(app.OptionsGridLayout);
            app.OTHEROPTIONSLabel.HorizontalAlignment = 'center';
            app.OTHEROPTIONSLabel.VerticalAlignment = 'bottom';
            app.OTHEROPTIONSLabel.FontSize = 9;
            app.OTHEROPTIONSLabel.FontColor = [0.149 0.149 0.149];
            app.OTHEROPTIONSLabel.Layout.Row = 4;
            app.OTHEROPTIONSLabel.Layout.Column = [26 35];
            app.OTHEROPTIONSLabel.Text = 'OTHER OPTIONS';

            % Create OptionsTab_help
            app.OptionsTab_help = uibutton(app.OptionsGridLayout, 'push');
            app.OptionsTab_help.ButtonPushedFcn = createCallbackFcn(app, @Help_OptionsTab_helpButtonPushed, true);
            app.OptionsTab_help.Icon = '061-info.png';
            app.OptionsTab_help.Tooltip = {'Help & Documentation'};
            app.OptionsTab_help.Layout.Row = 1;
            app.OptionsTab_help.Layout.Column = 37;
            app.OptionsTab_help.Text = '';

            % Create Image_24
            app.Image_24 = uiimage(app.OptionsGridLayout);
            app.Image_24.Layout.Row = [1 4];
            app.Image_24.Layout.Column = 36;
            app.Image_24.ImageSource = 'ImageDelimiter.png';

            % Create Image_26
            app.Image_26 = uiimage(app.OptionsGridLayout);
            app.Image_26.Layout.Row = [1 4];
            app.Image_26.Layout.Column = 25;
            app.Image_26.ImageSource = 'ImageDelimiter.png';

            % Create CLASSIFICATIONLabel
            app.CLASSIFICATIONLabel = uilabel(app.OptionsGridLayout);
            app.CLASSIFICATIONLabel.HorizontalAlignment = 'center';
            app.CLASSIFICATIONLabel.VerticalAlignment = 'bottom';
            app.CLASSIFICATIONLabel.FontSize = 9;
            app.CLASSIFICATIONLabel.FontColor = [0.149 0.149 0.149];
            app.CLASSIFICATIONLabel.Layout.Row = 4;
            app.CLASSIFICATIONLabel.Layout.Column = [19 24];
            app.CLASSIFICATIONLabel.Text = 'CLASSIFICATION';

            % Create Options_Medfilter3DsurfaceSpinnerLabel
            app.Options_Medfilter3DsurfaceSpinnerLabel = uilabel(app.OptionsGridLayout);
            app.Options_Medfilter3DsurfaceSpinnerLabel.HorizontalAlignment = 'right';
            app.Options_Medfilter3DsurfaceSpinnerLabel.FontSize = 11;
            app.Options_Medfilter3DsurfaceSpinnerLabel.Layout.Row = 2;
            app.Options_Medfilter3DsurfaceSpinnerLabel.Layout.Column = [26 29];
            app.Options_Medfilter3DsurfaceSpinnerLabel.Text = 'Med-filter 3D surf';

            % Create Options_Medfilter3DsurfaceSpinner
            app.Options_Medfilter3DsurfaceSpinner = uispinner(app.OptionsGridLayout);
            app.Options_Medfilter3DsurfaceSpinner.Limits = [0 50];
            app.Options_Medfilter3DsurfaceSpinner.ValueChangedFcn = createCallbackFcn(app, @Options_Medfilter3DsurfaceSpinnerValueChanged, true);
            app.Options_Medfilter3DsurfaceSpinner.HorizontalAlignment = 'center';
            app.Options_Medfilter3DsurfaceSpinner.FontSize = 10;
            app.Options_Medfilter3DsurfaceSpinner.Layout.Row = 2;
            app.Options_Medfilter3DsurfaceSpinner.Layout.Column = [30 31];
            app.Options_Medfilter3DsurfaceSpinner.Value = 3;

            % Create Options_ApplyAutoContrast
            app.Options_ApplyAutoContrast = uicheckbox(app.OptionsGridLayout);
            app.Options_ApplyAutoContrast.Text = 'Apply auto-contrast';
            app.Options_ApplyAutoContrast.FontSize = 11;
            app.Options_ApplyAutoContrast.Layout.Row = 3;
            app.Options_ApplyAutoContrast.Layout.Column = [13 17];

            % Create Options_ColormapResEditField_2
            app.Options_ColormapResEditField_2 = uieditfield(app.OptionsGridLayout, 'numeric');
            app.Options_ColormapResEditField_2.Limits = [2 10];
            app.Options_ColormapResEditField_2.RoundFractionalValues = 'on';
            app.Options_ColormapResEditField_2.HorizontalAlignment = 'center';
            app.Options_ColormapResEditField_2.FontSize = 10;
            app.Options_ColormapResEditField_2.Tooltip = {'BCR filter'};
            app.Options_ColormapResEditField_2.Layout.Row = 1;
            app.Options_ColormapResEditField_2.Layout.Column = 22;
            app.Options_ColormapResEditField_2.Value = 2;

            % Create Options_LogColormap_2
            app.Options_LogColormap_2 = uicheckbox(app.OptionsGridLayout);
            app.Options_LogColormap_2.Enable = 'off';
            app.Options_LogColormap_2.Tooltip = {'Activate BCR correction from the maskfile'};
            app.Options_LogColormap_2.Text = 'BCR filter';
            app.Options_LogColormap_2.FontSize = 11;
            app.Options_LogColormap_2.Layout.Row = 1;
            app.Options_LogColormap_2.Layout.Column = [19 21];

            % Create Options_MaskSelectionForMerged
            app.Options_MaskSelectionForMerged = uicheckbox(app.OptionsGridLayout);
            app.Options_MaskSelectionForMerged.Text = 'Mask selection (merged)';
            app.Options_MaskSelectionForMerged.FontSize = 11;
            app.Options_MaskSelectionForMerged.Layout.Row = 3;
            app.Options_MaskSelectionForMerged.Layout.Column = [19 24];

            % Create Image_34
            app.Image_34 = uiimage(app.OptionsGridLayout);
            app.Image_34.Layout.Row = [1 4];
            app.Image_34.Layout.Column = 18;
            app.Image_34.ImageSource = 'ImageDelimiter.png';

            % Create UnskmeanLabel
            app.UnskmeanLabel = uilabel(app.OptionsGridLayout);
            app.UnskmeanLabel.FontSize = 9;
            app.UnskmeanLabel.Layout.Row = 2;
            app.UnskmeanLabel.Layout.Column = [19 20];
            app.UnskmeanLabel.Text = 'Uns. kmean';

            % Create Options_KmeansAlgorithm
            app.Options_KmeansAlgorithm = uidropdown(app.OptionsGridLayout);
            app.Options_KmeansAlgorithm.Items = {'sqeuclidean', 'cityblock', 'cosine', 'correlation'};
            app.Options_KmeansAlgorithm.Tooltip = {'Select the active colormap'};
            app.Options_KmeansAlgorithm.FontSize = 10;
            app.Options_KmeansAlgorithm.Layout.Row = 2;
            app.Options_KmeansAlgorithm.Layout.Column = [21 24];
            app.Options_KmeansAlgorithm.Value = 'sqeuclidean';

            % Create ColorMapPreview
            app.ColorMapPreview = uiaxes(app.OptionsGridLayout);
            app.ColorMapPreview.PlotBoxAspectRatio = [7.67441860465116 1 1];
            app.ColorMapPreview.XColor = 'none';
            app.ColorMapPreview.XTick = [];
            app.ColorMapPreview.YColor = 'none';
            app.ColorMapPreview.YTick = [];
            app.ColorMapPreview.ZColor = 'none';
            app.ColorMapPreview.FontSize = 12;
            app.ColorMapPreview.Layout.Row = [1 2];
            app.ColorMapPreview.Layout.Column = [11 17];

            % Create DEVELOPERTab
            app.DEVELOPERTab = uitab(app.TabButtonGroup);
            app.DEVELOPERTab.AutoResizeChildren = 'off';
            app.DEVELOPERTab.Title = 'DEVELOPER';

            % Create GridLayout5
            app.GridLayout5 = uigridlayout(app.DEVELOPERTab);
            app.GridLayout5.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '0.3x', '1x', '1x', '1x', '1x', '1x', '0.3x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout5.RowHeight = {'1x', '1x', '1x', '0.6x'};
            app.GridLayout5.ColumnSpacing = 4;
            app.GridLayout5.RowSpacing = 4;
            app.GridLayout5.Padding = [5 5 5 5];

            % Create Command_Keyboard
            app.Command_Keyboard = uibutton(app.GridLayout5, 'push');
            app.Command_Keyboard.ButtonPushedFcn = createCallbackFcn(app, @Developper_Command_KeyboardButtonPushed, true);
            app.Command_Keyboard.Icon = '019-compact disc.png';
            app.Command_Keyboard.IconAlignment = 'top';
            app.Command_Keyboard.FontSize = 10;
            app.Command_Keyboard.Layout.Row = [1 2];
            app.Command_Keyboard.Layout.Column = [7 8];
            app.Command_Keyboard.Text = 'keyboard';

            % Create RebuildButton
            app.RebuildButton = uibutton(app.GridLayout5, 'push');
            app.RebuildButton.ButtonPushedFcn = createCallbackFcn(app, @RebuildButtonPushed, true);
            app.RebuildButton.Icon = '090-share.png';
            app.RebuildButton.IconAlignment = 'top';
            app.RebuildButton.FontSize = 10;
            app.RebuildButton.Layout.Row = [1 2];
            app.RebuildButton.Layout.Column = [10 11];
            app.RebuildButton.Text = 'Rebuild';

            % Create DEVELOPPERTOOLSLabel
            app.DEVELOPPERTOOLSLabel = uilabel(app.GridLayout5);
            app.DEVELOPPERTOOLSLabel.HorizontalAlignment = 'center';
            app.DEVELOPPERTOOLSLabel.VerticalAlignment = 'bottom';
            app.DEVELOPPERTOOLSLabel.FontSize = 9;
            app.DEVELOPPERTOOLSLabel.FontColor = [0.149 0.149 0.149];
            app.DEVELOPPERTOOLSLabel.Layout.Row = 4;
            app.DEVELOPPERTOOLSLabel.Layout.Column = [6 11];
            app.DEVELOPPERTOOLSLabel.Text = 'DEVELOPPER TOOLS';

            % Create DEVELOPPERMODELabel
            app.DEVELOPPERMODELabel = uilabel(app.GridLayout5);
            app.DEVELOPPERMODELabel.HorizontalAlignment = 'center';
            app.DEVELOPPERMODELabel.VerticalAlignment = 'bottom';
            app.DEVELOPPERMODELabel.FontSize = 9;
            app.DEVELOPPERMODELabel.FontColor = [0.149 0.149 0.149];
            app.DEVELOPPERMODELabel.Layout.Row = 4;
            app.DEVELOPPERMODELabel.Layout.Column = [1 5];
            app.DEVELOPPERMODELabel.Text = 'DEVELOPPER MODE';

            % Create AdminAccess
            app.AdminAccess = uieditfield(app.GridLayout5, 'text');
            app.AdminAccess.ValueChangedFcn = createCallbackFcn(app, @Developper_UnlockButtonPushed, true);
            app.AdminAccess.HorizontalAlignment = 'center';
            app.AdminAccess.FontColor = [0.502 0.502 0.502];
            app.AdminAccess.Layout.Row = 1;
            app.AdminAccess.Layout.Column = [1 5];
            app.AdminAccess.Value = 'Password';

            % Create UnlockButton
            app.UnlockButton = uibutton(app.GridLayout5, 'push');
            app.UnlockButton.ButtonPushedFcn = createCallbackFcn(app, @Developper_UnlockButtonPushed, true);
            app.UnlockButton.Layout.Row = 2;
            app.UnlockButton.Layout.Column = [3 5];
            app.UnlockButton.Text = 'Unlock';

            % Create Image_27
            app.Image_27 = uiimage(app.GridLayout5);
            app.Image_27.Layout.Row = [1 4];
            app.Image_27.Layout.Column = 6;
            app.Image_27.ImageSource = 'ImageDelimiter.png';

            % Create Image_28
            app.Image_28 = uiimage(app.GridLayout5);
            app.Image_28.Layout.Row = [1 4];
            app.Image_28.Layout.Column = 12;
            app.Image_28.ImageSource = 'ImageDelimiter.png';

            % Create PlotEngineDropDownLabel
            app.PlotEngineDropDownLabel = uilabel(app.GridLayout5);
            app.PlotEngineDropDownLabel.HorizontalAlignment = 'right';
            app.PlotEngineDropDownLabel.FontSize = 11;
            app.PlotEngineDropDownLabel.Layout.Row = 1;
            app.PlotEngineDropDownLabel.Layout.Column = [14 16];
            app.PlotEngineDropDownLabel.Text = 'Plot Engine';

            % Create PlotEngineDropDown
            app.PlotEngineDropDown = uidropdown(app.GridLayout5);
            app.PlotEngineDropDown.Items = {'4.4', '4.3 (legacy)'};
            app.PlotEngineDropDown.ValueChangedFcn = createCallbackFcn(app, @PlotEngineDropDownValueChanged, true);
            app.PlotEngineDropDown.FontSize = 11;
            app.PlotEngineDropDown.Layout.Row = 1;
            app.PlotEngineDropDown.Layout.Column = [17 19];
            app.PlotEngineDropDown.Value = '4.4';

            % Create GridLayout_Bottom
            app.GridLayout_Bottom = uigridlayout(app.GridLayout2);
            app.GridLayout_Bottom.ColumnWidth = {'1x', '1x', '0.4x', '0.7x', '1x', '0.5x', '0.5x', '0.4x'};
            app.GridLayout_Bottom.RowHeight = {'1x'};
            app.GridLayout_Bottom.Layout.Row = 4;
            app.GridLayout_Bottom.Layout.Column = [2 3];

            % Create MapSlider
            app.MapSlider = uislider(app.GridLayout_Bottom);
            app.MapSlider.MajorTicks = [];
            app.MapSlider.MajorTickLabels = {};
            app.MapSlider.ValueChangedFcn = createCallbackFcn(app, @MapSliderValueChanged, true);
            app.MapSlider.ValueChangingFcn = createCallbackFcn(app, @MapSliderValueChanging, true);
            app.MapSlider.MinorTicks = [];
            app.MapSlider.Layout.Row = 1;
            app.MapSlider.Layout.Column = [1 2];

            % Create Value_MapSlider
            app.Value_MapSlider = uieditfield(app.GridLayout_Bottom, 'numeric');
            app.Value_MapSlider.Limits = [0 100];
            app.Value_MapSlider.RoundFractionalValues = 'on';
            app.Value_MapSlider.ValueChangedFcn = createCallbackFcn(app, @Value_MapSliderValueChanged, true);
            app.Value_MapSlider.HorizontalAlignment = 'left';
            app.Value_MapSlider.Layout.Row = 1;
            app.Value_MapSlider.Layout.Column = 3;

            % Create ROI_DispVertical
            app.ROI_DispVertical = uicheckbox(app.GridLayout_Bottom);
            app.ROI_DispVertical.ValueChangedFcn = createCallbackFcn(app, @ROI_DispVerticalValueChanged, true);
            app.ROI_DispVertical.Text = 'Vertical';
            app.ROI_DispVertical.Layout.Row = 1;
            app.ROI_DispVertical.Layout.Column = 4;

            % Create ROI_VerticalSlider
            app.ROI_VerticalSlider = uislider(app.GridLayout_Bottom);
            app.ROI_VerticalSlider.Limits = [0 360];
            app.ROI_VerticalSlider.MajorTicks = [];
            app.ROI_VerticalSlider.ValueChangedFcn = createCallbackFcn(app, @ROI_DispVerticalValueChanged, true);
            app.ROI_VerticalSlider.ValueChangingFcn = createCallbackFcn(app, @ROI_VerticalSliderValueChanging, true);
            app.ROI_VerticalSlider.MinorTicks = [];
            app.ROI_VerticalSlider.Layout.Row = 1;
            app.ROI_VerticalSlider.Layout.Column = 5;

            % Create ROI_AngleOrientation
            app.ROI_AngleOrientation = uispinner(app.GridLayout_Bottom);
            app.ROI_AngleOrientation.Limits = [0 360];
            app.ROI_AngleOrientation.RoundFractionalValues = 'on';
            app.ROI_AngleOrientation.ValueChangedFcn = createCallbackFcn(app, @ROI_AngleOrientationValueChanged, true);
            app.ROI_AngleOrientation.Layout.Row = 1;
            app.ROI_AngleOrientation.Layout.Column = 6;

            % Create GridLayout_Credits
            app.GridLayout_Credits = uigridlayout(app.GridLayout2);
            app.GridLayout_Credits.ColumnWidth = {'1x', '0.1x'};
            app.GridLayout_Credits.RowHeight = {'0.5x', '1x'};
            app.GridLayout_Credits.RowSpacing = 5;
            app.GridLayout_Credits.Padding = [5 5 5 5];
            app.GridLayout_Credits.Layout.Row = 4;
            app.GridLayout_Credits.Layout.Column = 3;

            % Create GridLayout_MainFigure
            app.GridLayout_MainFigure = uigridlayout(app.GridLayout2);
            app.GridLayout_MainFigure.ColumnWidth = {'1x'};
            app.GridLayout_MainFigure.Padding = [5 10 5 5];
            app.GridLayout_MainFigure.Layout.Row = [2 3];
            app.GridLayout_MainFigure.Layout.Column = [2 3];

            % Create PanelMulti
            app.PanelMulti = uipanel(app.GridLayout_MainFigure);
            app.PanelMulti.BorderType = 'none';
            app.PanelMulti.Layout.Row = [1 2];
            app.PanelMulti.Layout.Column = 1;

            % Create PanelSingle
            app.PanelSingle = uipanel(app.GridLayout_MainFigure);
            app.PanelSingle.BorderType = 'none';
            app.PanelSingle.Layout.Row = [1 2];
            app.PanelSingle.Layout.Column = 1;

            % Create GridLayout14
            app.GridLayout14 = uigridlayout(app.PanelSingle);
            app.GridLayout14.ColumnWidth = {'1x'};
            app.GridLayout14.RowHeight = {'1x'};
            app.GridLayout14.ColumnSpacing = 1;
            app.GridLayout14.RowSpacing = 1;
            app.GridLayout14.Padding = [1 1 1 20];

            % Create PanelFigMain
            app.PanelFigMain = uipanel(app.GridLayout14);
            app.PanelFigMain.BorderType = 'none';
            app.PanelFigMain.TitlePosition = 'centertop';
            app.PanelFigMain.Layout.Row = 1;
            app.PanelFigMain.Layout.Column = 1;

            % Create GridLayout6
            app.GridLayout6 = uigridlayout(app.GridLayout2);
            app.GridLayout6.ColumnWidth = {'1x'};
            app.GridLayout6.RowHeight = {'1x', '1x', '1x', '0.2x', '1x', '0.2x', '1x', '1x', '1x', '1x', '0.2x', '1x', '1x', '1x', '0.2x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout6.ColumnSpacing = 5;
            app.GridLayout6.RowSpacing = 4;
            app.GridLayout6.Padding = [5 10 4 10];
            app.GridLayout6.Layout.Row = [2 3];
            app.GridLayout6.Layout.Column = 1;

            % Create Image2_3
            app.Image2_3 = uiimage(app.GridLayout6);
            app.Image2_3.Layout.Row = 6;
            app.Image2_3.Layout.Column = 1;
            app.Image2_3.ImageSource = 'ImageDelimiter_Horizontal.png';

            % Create Image2_2
            app.Image2_2 = uiimage(app.GridLayout6);
            app.Image2_2.Layout.Row = 11;
            app.Image2_2.Layout.Column = 1;
            app.Image2_2.ImageSource = 'ImageDelimiter_Horizontal.png';

            % Create Image2
            app.Image2 = uiimage(app.GridLayout6);
            app.Image2.Layout.Row = 4;
            app.Image2.Layout.Column = 1;
            app.Image2.ImageSource = 'ImageDelimiter_Horizontal.png';

            % Create Button_FigMain_AutoContrast
            app.Button_FigMain_AutoContrast = uibutton(app.GridLayout6, 'push');
            app.Button_FigMain_AutoContrast.ButtonPushedFcn = createCallbackFcn(app, @Button_FigMain_AutoContrastPushed, true);
            app.Button_FigMain_AutoContrast.Icon = 'XXX_magic-wand.png';
            app.Button_FigMain_AutoContrast.IconAlignment = 'center';
            app.Button_FigMain_AutoContrast.Tooltip = {'Auto Contrast'};
            app.Button_FigMain_AutoContrast.Layout.Row = 1;
            app.Button_FigMain_AutoContrast.Layout.Column = 1;
            app.Button_FigMain_AutoContrast.Text = '';

            % Create Button_FigMain_MedianFilter
            app.Button_FigMain_MedianFilter = uibutton(app.GridLayout6, 'push');
            app.Button_FigMain_MedianFilter.ButtonPushedFcn = createCallbackFcn(app, @Button_FigMain_MedianFilterPushed, true);
            app.Button_FigMain_MedianFilter.Icon = 'XXX_grid.png';
            app.Button_FigMain_MedianFilter.IconAlignment = 'center';
            app.Button_FigMain_MedianFilter.Tooltip = {'Median Filter'};
            app.Button_FigMain_MedianFilter.Layout.Row = 2;
            app.Button_FigMain_MedianFilter.Layout.Column = 1;
            app.Button_FigMain_MedianFilter.Text = '';

            % Create Button_FigMain_ResetDisplay
            app.Button_FigMain_ResetDisplay = uibutton(app.GridLayout6, 'push');
            app.Button_FigMain_ResetDisplay.ButtonPushedFcn = createCallbackFcn(app, @Menu_Plot_ResetDisplayMenuSelected, true);
            app.Button_FigMain_ResetDisplay.Icon = 'XXX_control.png';
            app.Button_FigMain_ResetDisplay.IconAlignment = 'center';
            app.Button_FigMain_ResetDisplay.Tooltip = {'Reset Colorbar & Filters'};
            app.Button_FigMain_ResetDisplay.Layout.Row = 3;
            app.Button_FigMain_ResetDisplay.Layout.Column = 1;
            app.Button_FigMain_ResetDisplay.Text = '';

            % Create Button_FigMain_ZoomIn
            app.Button_FigMain_ZoomIn = uibutton(app.GridLayout6, 'push');
            app.Button_FigMain_ZoomIn.ButtonPushedFcn = createCallbackFcn(app, @Button_FigMain_ZoomInPushed2, true);
            app.Button_FigMain_ZoomIn.Icon = 'XXX_zoom_in.png';
            app.Button_FigMain_ZoomIn.IconAlignment = 'center';
            app.Button_FigMain_ZoomIn.Tooltip = {'Zoom In'};
            app.Button_FigMain_ZoomIn.Layout.Row = 7;
            app.Button_FigMain_ZoomIn.Layout.Column = 1;
            app.Button_FigMain_ZoomIn.Text = '';

            % Create Button_FigMain_ZoomOut
            app.Button_FigMain_ZoomOut = uibutton(app.GridLayout6, 'push');
            app.Button_FigMain_ZoomOut.ButtonPushedFcn = createCallbackFcn(app, @Button_FigMain_ZoomOutPushed2, true);
            app.Button_FigMain_ZoomOut.Icon = 'XXX_zoom_out.png';
            app.Button_FigMain_ZoomOut.IconAlignment = 'center';
            app.Button_FigMain_ZoomOut.Tooltip = {'Zoom Out'};
            app.Button_FigMain_ZoomOut.Layout.Row = 8;
            app.Button_FigMain_ZoomOut.Layout.Column = 1;
            app.Button_FigMain_ZoomOut.Text = '';

            % Create Button_FigMain_Pan
            app.Button_FigMain_Pan = uibutton(app.GridLayout6, 'push');
            app.Button_FigMain_Pan.ButtonPushedFcn = createCallbackFcn(app, @Button_FigMain_PanPushed, true);
            app.Button_FigMain_Pan.Icon = 'XXX_controller.png';
            app.Button_FigMain_Pan.IconAlignment = 'center';
            app.Button_FigMain_Pan.Tooltip = {'Pan'};
            app.Button_FigMain_Pan.Layout.Row = 9;
            app.Button_FigMain_Pan.Layout.Column = 1;
            app.Button_FigMain_Pan.Text = '';

            % Create Button_FigMain_ResetZoomPan
            app.Button_FigMain_ResetZoomPan = uibutton(app.GridLayout6, 'push');
            app.Button_FigMain_ResetZoomPan.ButtonPushedFcn = createCallbackFcn(app, @Button_FigMain_ResetZoomPanPushed, true);
            app.Button_FigMain_ResetZoomPan.Icon = 'XXX_home.png';
            app.Button_FigMain_ResetZoomPan.IconAlignment = 'center';
            app.Button_FigMain_ResetZoomPan.Tooltip = {'Reset Zoom & Pan'};
            app.Button_FigMain_ResetZoomPan.Layout.Row = 10;
            app.Button_FigMain_ResetZoomPan.Layout.Column = 1;
            app.Button_FigMain_ResetZoomPan.Text = '';

            % Create Button_FigMain_OpenNewWindow
            app.Button_FigMain_OpenNewWindow = uibutton(app.GridLayout6, 'push');
            app.Button_FigMain_OpenNewWindow.ButtonPushedFcn = createCallbackFcn(app, @Menu_EditOpenImageNewWindowSelected, true);
            app.Button_FigMain_OpenNewWindow.Icon = 'XXX_ExportMap.png';
            app.Button_FigMain_OpenNewWindow.IconAlignment = 'center';
            app.Button_FigMain_OpenNewWindow.Tooltip = {'Open Image New Window'};
            app.Button_FigMain_OpenNewWindow.Layout.Row = 13;
            app.Button_FigMain_OpenNewWindow.Layout.Column = 1;
            app.Button_FigMain_OpenNewWindow.Text = '';

            % Create Button_FigMain_CopyImage
            app.Button_FigMain_CopyImage = uibutton(app.GridLayout6, 'push');
            app.Button_FigMain_CopyImage.ButtonPushedFcn = createCallbackFcn(app, @Menu_EditCopyImageSelected, true);
            app.Button_FigMain_CopyImage.Icon = 'XXX_ExportCopy.png';
            app.Button_FigMain_CopyImage.IconAlignment = 'center';
            app.Button_FigMain_CopyImage.Tooltip = {'Copy Image to Clipboard'};
            app.Button_FigMain_CopyImage.Layout.Row = 12;
            app.Button_FigMain_CopyImage.Layout.Column = 1;
            app.Button_FigMain_CopyImage.Text = '';

            % Create Button_FigMain_PlotSurface
            app.Button_FigMain_PlotSurface = uibutton(app.GridLayout6, 'push');
            app.Button_FigMain_PlotSurface.ButtonPushedFcn = createCallbackFcn(app, @Menu_EditPlot3DsurfaceSelected, true);
            app.Button_FigMain_PlotSurface.Icon = 'XXX_ExportTopo.png';
            app.Button_FigMain_PlotSurface.IconAlignment = 'center';
            app.Button_FigMain_PlotSurface.Tooltip = {'Plot 3D surface'};
            app.Button_FigMain_PlotSurface.Layout.Row = 14;
            app.Button_FigMain_PlotSurface.Layout.Column = 1;
            app.Button_FigMain_PlotSurface.Text = '';

            % Create Button_FigMain_CursorInfo
            app.Button_FigMain_CursorInfo = uibutton(app.GridLayout6, 'push');
            app.Button_FigMain_CursorInfo.ButtonPushedFcn = createCallbackFcn(app, @Button_FigMain_CursorInfoPushed, true);
            app.Button_FigMain_CursorInfo.Icon = 'XXX_CursorMode_Activate.png';
            app.Button_FigMain_CursorInfo.IconAlignment = 'center';
            app.Button_FigMain_CursorInfo.Tooltip = {'Enable data tips'};
            app.Button_FigMain_CursorInfo.Layout.Row = 5;
            app.Button_FigMain_CursorInfo.Layout.Column = 1;
            app.Button_FigMain_CursorInfo.Text = '';

            % Create RightPanel
            app.RightPanel = uipanel(app.GridLayout);
            app.RightPanel.Layout.Row = 1;
            app.RightPanel.Layout.Column = 3;

            % Create GridLayout8
            app.GridLayout8 = uigridlayout(app.RightPanel);
            app.GridLayout8.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout8.RowHeight = {'1.2x', '1x', '1x', '0.6x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout8.ColumnSpacing = 5;
            app.GridLayout8.RowSpacing = 5;
            app.GridLayout8.Padding = [3 3 3 3];

            % Create EditField_LivePeak
            app.EditField_LivePeak = uieditfield(app.GridLayout8, 'text');
            app.EditField_LivePeak.HorizontalAlignment = 'center';
            app.EditField_LivePeak.FontSize = 9;
            app.EditField_LivePeak.Layout.Row = 4;
            app.EditField_LivePeak.Layout.Column = [3 4];

            % Create EditField_LivePosition
            app.EditField_LivePosition = uieditfield(app.GridLayout8, 'numeric');
            app.EditField_LivePosition.ValueDisplayFormat = '%3.4g';
            app.EditField_LivePosition.ValueChangedFcn = createCallbackFcn(app, @EditField_LivePositionValueChanged, true);
            app.EditField_LivePosition.HorizontalAlignment = 'center';
            app.EditField_LivePosition.FontSize = 9;
            app.EditField_LivePosition.Layout.Row = 4;
            app.EditField_LivePosition.Layout.Column = [3 4];

            % Create EditField_LiveMin
            app.EditField_LiveMin = uieditfield(app.GridLayout8, 'numeric');
            app.EditField_LiveMin.ValueDisplayFormat = '%3.4g';
            app.EditField_LiveMin.ValueChangedFcn = createCallbackFcn(app, @Plot_LiveMinMaxValueChanged, true);
            app.EditField_LiveMin.HorizontalAlignment = 'center';
            app.EditField_LiveMin.FontSize = 9;
            app.EditField_LiveMin.FontColor = [1 0 0];
            app.EditField_LiveMin.Layout.Row = 4;
            app.EditField_LiveMin.Layout.Column = [1 2];

            % Create EditField_LiveMax
            app.EditField_LiveMax = uieditfield(app.GridLayout8, 'numeric');
            app.EditField_LiveMax.ValueDisplayFormat = '%3.4g';
            app.EditField_LiveMax.ValueChangedFcn = createCallbackFcn(app, @Plot_LiveMinMaxValueChanged, true);
            app.EditField_LiveMax.HorizontalAlignment = 'center';
            app.EditField_LiveMax.FontSize = 9;
            app.EditField_LiveMax.FontColor = [1 0 0];
            app.EditField_LiveMax.Layout.Row = 4;
            app.EditField_LiveMax.Layout.Column = [5 6];

            % Create TabGroup
            app.TabGroup = uitabgroup(app.GridLayout8);
            app.TabGroup.Layout.Row = [5 24];
            app.TabGroup.Layout.Column = [1 6];

            % Create InformationTab
            app.InformationTab = uitab(app.TabGroup);
            app.InformationTab.Title = 'Information';

            % Create GridLayout9
            app.GridLayout9 = uigridlayout(app.InformationTab);
            app.GridLayout9.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout9.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout9.ColumnSpacing = 3;
            app.GridLayout9.RowSpacing = 3;
            app.GridLayout9.Padding = [3 3 3 3];

            % Create XMapTools_logo
            app.XMapTools_logo = uiimage(app.GridLayout9);
            app.XMapTools_logo.Layout.Row = 19;
            app.XMapTools_logo.Layout.Column = 7;
            app.XMapTools_logo.ImageSource = 'xmaptools_ios_icon_HR.png';

            % Create XMapTools_version
            app.XMapTools_version = uilabel(app.GridLayout9);
            app.XMapTools_version.HorizontalAlignment = 'right';
            app.XMapTools_version.FontSize = 10;
            app.XMapTools_version.FontAngle = 'italic';
            app.XMapTools_version.Layout.Row = 19;
            app.XMapTools_version.Layout.Column = [2 6];
            app.XMapTools_version.Text = 'Version';

            % Create MapInfo_TextArea
            app.MapInfo_TextArea = uitextarea(app.GridLayout9);
            app.MapInfo_TextArea.Editable = 'off';
            app.MapInfo_TextArea.FontSize = 9.5;
            app.MapInfo_TextArea.FontColor = [0.2784 0.2784 0.2784];
            app.MapInfo_TextArea.BackgroundColor = [0.9412 0.9412 0.9412];
            app.MapInfo_TextArea.Layout.Row = [1 18];
            app.MapInfo_TextArea.Layout.Column = [1 7];

            % Create SamplingTab
            app.SamplingTab = uitab(app.TabGroup);
            app.SamplingTab.Title = 'Sampling';

            % Create GridLayout9_2
            app.GridLayout9_2 = uigridlayout(app.SamplingTab);
            app.GridLayout9_2.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout9_2.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout9_2.ColumnSpacing = 3;
            app.GridLayout9_2.RowSpacing = 3;
            app.GridLayout9_2.Padding = [3 3 3 10];

            % Create GridLayout13
            app.GridLayout13 = uigridlayout(app.GridLayout9_2);
            app.GridLayout13.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout13.RowHeight = {'1x'};
            app.GridLayout13.ColumnSpacing = 5;
            app.GridLayout13.Padding = [21 3 21 3];
            app.GridLayout13.Layout.Row = 1;
            app.GridLayout13.Layout.Column = [1 7];

            % Create Sampling_SelectCircleButton
            app.Sampling_SelectCircleButton = uibutton(app.GridLayout13, 'push');
            app.Sampling_SelectCircleButton.ButtonPushedFcn = createCallbackFcn(app, @Menu_Sampling_CircleMenuSelected, true);
            app.Sampling_SelectCircleButton.Icon = 'XXX_Shaded_Circle.png';
            app.Sampling_SelectCircleButton.Tooltip = {'Sampling: Circle'};
            app.Sampling_SelectCircleButton.Layout.Row = 1;
            app.Sampling_SelectCircleButton.Layout.Column = 1;
            app.Sampling_SelectCircleButton.Text = '';

            % Create Sampling_SelectAreaButton
            app.Sampling_SelectAreaButton = uibutton(app.GridLayout13, 'push');
            app.Sampling_SelectAreaButton.ButtonPushedFcn = createCallbackFcn(app, @Menu_Sampling_AreaMenuSelected, true);
            app.Sampling_SelectAreaButton.Icon = 'XXX_Shaded_Square.png';
            app.Sampling_SelectAreaButton.Tooltip = {'Sampling: Polygon Free Shape'};
            app.Sampling_SelectAreaButton.Layout.Row = 1;
            app.Sampling_SelectAreaButton.Layout.Column = 2;
            app.Sampling_SelectAreaButton.Text = '';

            % Create Sampling_SelectTransectButton
            app.Sampling_SelectTransectButton = uibutton(app.GridLayout13, 'push');
            app.Sampling_SelectTransectButton.ButtonPushedFcn = createCallbackFcn(app, @Menu_Sampling_TransectMenuSelected, true);
            app.Sampling_SelectTransectButton.Icon = 'XXX_Share.png';
            app.Sampling_SelectTransectButton.Tooltip = {'Sampling: Transect (Path)'};
            app.Sampling_SelectTransectButton.Layout.Row = 1;
            app.Sampling_SelectTransectButton.Layout.Column = 3;
            app.Sampling_SelectTransectButton.Text = '';

            % Create Sampling_SelectStripeButton
            app.Sampling_SelectStripeButton = uibutton(app.GridLayout13, 'push');
            app.Sampling_SelectStripeButton.ButtonPushedFcn = createCallbackFcn(app, @Menu_Sampling_StripeMenuSelected, true);
            app.Sampling_SelectStripeButton.Icon = 'XXX_FocusHorizontal.png';
            app.Sampling_SelectStripeButton.Tooltip = {'Sampling: Stripe (Rectangle-shape)'};
            app.Sampling_SelectStripeButton.Layout.Row = 1;
            app.Sampling_SelectStripeButton.Layout.Column = 4;
            app.Sampling_SelectStripeButton.Text = '';

            % Create Sampling_ExportButton
            app.Sampling_ExportButton = uibutton(app.GridLayout13, 'push');
            app.Sampling_ExportButton.ButtonPushedFcn = createCallbackFcn(app, @Sampling_ExportButtonPushed, true);
            app.Sampling_ExportButton.Icon = 'XXX_images.png';
            app.Sampling_ExportButton.Tooltip = {'Export/edit figure'};
            app.Sampling_ExportButton.Layout.Row = 1;
            app.Sampling_ExportButton.Layout.Column = 6;
            app.Sampling_ExportButton.Text = '';

            % Create Sampling_ResetButton
            app.Sampling_ResetButton = uibutton(app.GridLayout13, 'push');
            app.Sampling_ResetButton.ButtonPushedFcn = createCallbackFcn(app, @Menu_Sampling_ResetROIMenuSelected, true);
            app.Sampling_ResetButton.Icon = 'XXX_closeMini.png';
            app.Sampling_ResetButton.Tooltip = {'Reset ROI'};
            app.Sampling_ResetButton.Layout.Row = 1;
            app.Sampling_ResetButton.Layout.Column = 7;
            app.Sampling_ResetButton.Text = '';

            % Create Sampling_Plot1
            app.Sampling_Plot1 = uiaxes(app.GridLayout9_2);
            app.Sampling_Plot1.PlotBoxAspectRatio = [1.02534562211982 1 1];
            app.Sampling_Plot1.FontSize = 9;
            app.Sampling_Plot1.Layout.Row = [3 10];
            app.Sampling_Plot1.Layout.Column = [1 7];

            % Create Sampling_Plot2
            app.Sampling_Plot2 = uiaxes(app.GridLayout9_2);
            app.Sampling_Plot2.PlotBoxAspectRatio = [1.02534562211982 1 1];
            app.Sampling_Plot2.FontSize = 9;
            app.Sampling_Plot2.Layout.Row = [12 19];
            app.Sampling_Plot2.Layout.Column = [1 7];

            % Create StandardsTab
            app.StandardsTab = uitab(app.TabGroup);
            app.StandardsTab.Title = 'Standards';

            % Create GridLayout9_3
            app.GridLayout9_3 = uigridlayout(app.StandardsTab);
            app.GridLayout9_3.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout9_3.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout9_3.ColumnSpacing = 3;
            app.GridLayout9_3.RowSpacing = 3;
            app.GridLayout9_3.Padding = [3 3 3 3];

            % Create SubTabStandard
            app.SubTabStandard = uitabgroup(app.GridLayout9_3);
            app.SubTabStandard.TabLocation = 'bottom';
            app.SubTabStandard.Layout.Row = [1 19];
            app.SubTabStandard.Layout.Column = [1 7];

            % Create StdDataTab
            app.StdDataTab = uitab(app.SubTabStandard);
            app.StdDataTab.Title = 'Data';

            % Create GridLayout10
            app.GridLayout10 = uigridlayout(app.StdDataTab);
            app.GridLayout10.ColumnWidth = {'1x', '1x', '1x', '1x'};
            app.GridLayout10.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.7x', '0.7x', '1x'};
            app.GridLayout10.ColumnSpacing = 3;
            app.GridLayout10.RowSpacing = 3;
            app.GridLayout10.Padding = [3 3 3 3];

            % Create StandardLabel
            app.StandardLabel = uilabel(app.GridLayout10);
            app.StandardLabel.HorizontalAlignment = 'center';
            app.StandardLabel.FontSize = 14;
            app.StandardLabel.FontWeight = 'bold';
            app.StandardLabel.Layout.Row = 1;
            app.StandardLabel.Layout.Column = [1 4];
            app.StandardLabel.Text = 'Spot #';

            % Create CoordinatesSpotLabel
            app.CoordinatesSpotLabel = uilabel(app.GridLayout10);
            app.CoordinatesSpotLabel.HorizontalAlignment = 'center';
            app.CoordinatesSpotLabel.VerticalAlignment = 'bottom';
            app.CoordinatesSpotLabel.FontSize = 11;
            app.CoordinatesSpotLabel.FontWeight = 'bold';
            app.CoordinatesSpotLabel.Layout.Row = 17;
            app.CoordinatesSpotLabel.Layout.Column = [1 4];
            app.CoordinatesSpotLabel.Text = 'Coordinates (Spot)';

            % Create X_OriginalLabel
            app.X_OriginalLabel = uilabel(app.GridLayout10);
            app.X_OriginalLabel.HorizontalAlignment = 'center';
            app.X_OriginalLabel.VerticalAlignment = 'bottom';
            app.X_OriginalLabel.FontSize = 9;
            app.X_OriginalLabel.Layout.Row = 18;
            app.X_OriginalLabel.Layout.Column = 1;
            app.X_OriginalLabel.Text = 'X_Original';

            % Create Y_OriginalLabel
            app.Y_OriginalLabel = uilabel(app.GridLayout10);
            app.Y_OriginalLabel.HorizontalAlignment = 'center';
            app.Y_OriginalLabel.VerticalAlignment = 'bottom';
            app.Y_OriginalLabel.FontSize = 9;
            app.Y_OriginalLabel.Layout.Row = 18;
            app.Y_OriginalLabel.Layout.Column = 2;
            app.Y_OriginalLabel.Text = 'Y_Original';

            % Create X_MapLabel
            app.X_MapLabel = uilabel(app.GridLayout10);
            app.X_MapLabel.HorizontalAlignment = 'center';
            app.X_MapLabel.VerticalAlignment = 'bottom';
            app.X_MapLabel.FontSize = 9;
            app.X_MapLabel.Layout.Row = 18;
            app.X_MapLabel.Layout.Column = 3;
            app.X_MapLabel.Text = 'X_Map';

            % Create Y_MapLabel
            app.Y_MapLabel = uilabel(app.GridLayout10);
            app.Y_MapLabel.HorizontalAlignment = 'center';
            app.Y_MapLabel.VerticalAlignment = 'bottom';
            app.Y_MapLabel.FontSize = 9;
            app.Y_MapLabel.Layout.Row = 18;
            app.Y_MapLabel.Layout.Column = 4;
            app.Y_MapLabel.Text = 'Y_Map';

            % Create Xori
            app.Xori = uieditfield(app.GridLayout10, 'numeric');
            app.Xori.Editable = 'off';
            app.Xori.HorizontalAlignment = 'center';
            app.Xori.FontSize = 10;
            app.Xori.Layout.Row = 19;
            app.Xori.Layout.Column = 1;

            % Create Yori
            app.Yori = uieditfield(app.GridLayout10, 'numeric');
            app.Yori.Editable = 'off';
            app.Yori.HorizontalAlignment = 'center';
            app.Yori.FontSize = 10;
            app.Yori.Layout.Row = 19;
            app.Yori.Layout.Column = 2;

            % Create Xm
            app.Xm = uieditfield(app.GridLayout10, 'numeric');
            app.Xm.Editable = 'off';
            app.Xm.HorizontalAlignment = 'center';
            app.Xm.FontSize = 10;
            app.Xm.Layout.Row = 19;
            app.Xm.Layout.Column = 3;

            % Create Ym
            app.Ym = uieditfield(app.GridLayout10, 'numeric');
            app.Ym.Editable = 'off';
            app.Ym.HorizontalAlignment = 'center';
            app.Ym.FontSize = 10;
            app.Ym.Layout.Row = 19;
            app.Ym.Layout.Column = 4;

            % Create Standard_UITable
            app.Standard_UITable = uitable(app.GridLayout10);
            app.Standard_UITable.ColumnName = {'Map'; 'Int'; 'El/Ox_std'; 'wt%'; ''};
            app.Standard_UITable.ColumnWidth = {'fit'};
            app.Standard_UITable.RowName = {};
            app.Standard_UITable.CellEditCallback = createCallbackFcn(app, @Standard_UITableCellEdit, true);
            app.Standard_UITable.Layout.Row = [2 16];
            app.Standard_UITable.Layout.Column = [1 4];

            % Create StdAllPlotTab
            app.StdAllPlotTab = uitab(app.SubTabStandard);
            app.StdAllPlotTab.Title = 'Plot';

            % Create GridLayout11
            app.GridLayout11 = uigridlayout(app.StdAllPlotTab);
            app.GridLayout11.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};

            % Create GridLayout12
            app.GridLayout12 = uigridlayout(app.GridLayout11);
            app.GridLayout12.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout12.RowHeight = {'1x'};
            app.GridLayout12.ColumnSpacing = 4;
            app.GridLayout12.Padding = [3 3 3 3];
            app.GridLayout12.Layout.Row = 4;
            app.GridLayout12.Layout.Column = [1 2];

            % Create Std_Shift_X
            app.Std_Shift_X = uieditfield(app.GridLayout12, 'numeric');
            app.Std_Shift_X.Editable = 'off';
            app.Std_Shift_X.HorizontalAlignment = 'center';
            app.Std_Shift_X.FontSize = 11;
            app.Std_Shift_X.Layout.Row = 1;
            app.Std_Shift_X.Layout.Column = 2;

            % Create Std_Shift_Y
            app.Std_Shift_Y = uieditfield(app.GridLayout12, 'numeric');
            app.Std_Shift_Y.Editable = 'off';
            app.Std_Shift_Y.HorizontalAlignment = 'center';
            app.Std_Shift_Y.FontSize = 11;
            app.Std_Shift_Y.Layout.Row = 1;
            app.Std_Shift_Y.Layout.Column = 3;

            % Create StdAll_Synchronize
            app.StdAll_Synchronize = uibutton(app.GridLayout12, 'push');
            app.StdAll_Synchronize.ButtonPushedFcn = createCallbackFcn(app, @Stdandards_SynchronizeButtonPushed, true);
            app.StdAll_Synchronize.Icon = '044-repeat.png';
            app.StdAll_Synchronize.IconAlignment = 'top';
            app.StdAll_Synchronize.FontSize = 9;
            app.StdAll_Synchronize.Enable = 'off';
            app.StdAll_Synchronize.Layout.Row = 1;
            app.StdAll_Synchronize.Layout.Column = 4;
            app.StdAll_Synchronize.Text = '';

            % Create StdAll_profil
            app.StdAll_profil = uiaxes(app.GridLayout11);
            xlabel(app.StdAll_profil, 'Point (#)')
            ylabel(app.StdAll_profil, 'Intensity')
            zlabel(app.StdAll_profil, 'Z')
            app.StdAll_profil.PlotBoxAspectRatio = [2.08854166666667 1 1];
            app.StdAll_profil.FontSize = 9;
            app.StdAll_profil.Layout.Row = [1 3];
            app.StdAll_profil.Layout.Column = [1 2];

            % Create StdAll_map2
            app.StdAll_map2 = uiaxes(app.GridLayout11);
            title(app.StdAll_map2, 'sqrt(sum(corrcoef^2))')
            app.StdAll_map2.Toolbar.Visible = 'off';
            app.StdAll_map2.PlotBoxAspectRatio = [1.39236111111111 1 1];
            app.StdAll_map2.FontSize = 9;
            app.StdAll_map2.Box = 'on';
            app.StdAll_map2.Layout.Row = [9 12];
            app.StdAll_map2.Layout.Column = [1 2];

            % Create StdAll_map1
            app.StdAll_map1 = uiaxes(app.GridLayout11);
            title(app.StdAll_map1, 'Element')
            app.StdAll_map1.Toolbar.Visible = 'off';
            app.StdAll_map1.PlotBoxAspectRatio = [1.38275862068966 1 1];
            app.StdAll_map1.FontSize = 9;
            app.StdAll_map1.Box = 'on';
            app.StdAll_map1.Layout.Row = [5 8];
            app.StdAll_map1.Layout.Column = [1 2];

            % Create CompositionTab
            app.CompositionTab = uitab(app.TabGroup);
            app.CompositionTab.Title = 'Composition';

            % Create GridLayout9_4
            app.GridLayout9_4 = uigridlayout(app.CompositionTab);
            app.GridLayout9_4.ColumnWidth = {'1x', '1x', '1x', '0.4x', '1x', '1x', '1x', '0.4x'};
            app.GridLayout9_4.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout9_4.ColumnSpacing = 3;
            app.GridLayout9_4.RowSpacing = 3;
            app.GridLayout9_4.Padding = [3 3 3 10];

            % Create CompViewer_DensityMenu
            app.CompViewer_DensityMenu = uidropdown(app.GridLayout9_4);
            app.CompViewer_DensityMenu.ValueChangedFcn = createCallbackFcn(app, @CompViewer_DensityMenuValueChanged, true);
            app.CompViewer_DensityMenu.Layout.Row = 1;
            app.CompViewer_DensityMenu.Layout.Column = [1 3];

            % Create CompViewer_Button_Copy
            app.CompViewer_Button_Copy = uibutton(app.GridLayout9_4, 'push');
            app.CompViewer_Button_Copy.ButtonPushedFcn = createCallbackFcn(app, @CompViewer_Button_CopyPushed, true);
            app.CompViewer_Button_Copy.Icon = '009-photo camera.png';
            app.CompViewer_Button_Copy.IconAlignment = 'top';
            app.CompViewer_Button_Copy.FontSize = 9;
            app.CompViewer_Button_Copy.Enable = 'off';
            app.CompViewer_Button_Copy.Tooltip = {'Copy data to clipboard'};
            app.CompViewer_Button_Copy.Layout.Row = 1;
            app.CompViewer_Button_Copy.Layout.Column = 5;
            app.CompViewer_Button_Copy.Text = '';

            % Create CompViewer_Button_Save
            app.CompViewer_Button_Save = uibutton(app.GridLayout9_4, 'push');
            app.CompViewer_Button_Save.ButtonPushedFcn = createCallbackFcn(app, @CompViewer_Button_SavePushed, true);
            app.CompViewer_Button_Save.Icon = '022-save.png';
            app.CompViewer_Button_Save.IconAlignment = 'top';
            app.CompViewer_Button_Save.FontSize = 9;
            app.CompViewer_Button_Save.Enable = 'off';
            app.CompViewer_Button_Save.Tooltip = {'Save data in a text-file'};
            app.CompViewer_Button_Save.Layout.Row = 1;
            app.CompViewer_Button_Save.Layout.Column = 6;
            app.CompViewer_Button_Save.Text = '';

            % Create CompViewer_Label
            app.CompViewer_Label = uilabel(app.GridLayout9_4);
            app.CompViewer_Label.HorizontalAlignment = 'center';
            app.CompViewer_Label.VerticalAlignment = 'bottom';
            app.CompViewer_Label.FontSize = 14;
            app.CompViewer_Label.FontWeight = 'bold';
            app.CompViewer_Label.Layout.Row = 2;
            app.CompViewer_Label.Layout.Column = [1 7];

            % Create CompViewer_UITable
            app.CompViewer_UITable = uitable(app.GridLayout9_4);
            app.CompViewer_UITable.ColumnName = {'Element'; 'Composition'};
            app.CompViewer_UITable.ColumnWidth = {'fit'};
            app.CompViewer_UITable.RowName = {};
            app.CompViewer_UITable.Layout.Row = [3 14];
            app.CompViewer_UITable.Layout.Column = [1 8];

            % Create CompViewer_Button_DeleteROI
            app.CompViewer_Button_DeleteROI = uibutton(app.GridLayout9_4, 'push');
            app.CompViewer_Button_DeleteROI.ButtonPushedFcn = createCallbackFcn(app, @CompViewer_Button_DeleteROIPushed, true);
            app.CompViewer_Button_DeleteROI.Icon = '276-trash.png';
            app.CompViewer_Button_DeleteROI.IconAlignment = 'top';
            app.CompViewer_Button_DeleteROI.FontSize = 9;
            app.CompViewer_Button_DeleteROI.Enable = 'off';
            app.CompViewer_Button_DeleteROI.Tooltip = {'Reset ROI'};
            app.CompViewer_Button_DeleteROI.Layout.Row = 1;
            app.CompViewer_Button_DeleteROI.Layout.Column = 7;
            app.CompViewer_Button_DeleteROI.Text = '';

            % Create CompViewer_PlotTable
            app.CompViewer_PlotTable = uiaxes(app.GridLayout9_4);
            xlabel(app.CompViewer_PlotTable, 'X')
            ylabel(app.CompViewer_PlotTable, 'Y')
            zlabel(app.CompViewer_PlotTable, 'Z')
            app.CompViewer_PlotTable.PlotBoxAspectRatio = [2.74766355140187 1 1];
            app.CompViewer_PlotTable.XTick = [0 0.2 0.4 0.6 0.8 1];
            app.CompViewer_PlotTable.Layout.Row = [15 19];
            app.CompViewer_PlotTable.Layout.Column = [1 8];

            % Create XMapToolsAssistantTab
            app.XMapToolsAssistantTab = uitab(app.TabGroup);
            app.XMapToolsAssistantTab.Title = 'XMapTools Assistant';

            % Create GridLayout15
            app.GridLayout15 = uigridlayout(app.XMapToolsAssistantTab);
            app.GridLayout15.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout15.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout15.ColumnSpacing = 3;
            app.GridLayout15.RowSpacing = 3;
            app.GridLayout15.Padding = [3 3 3 10];

            % Create Assistant_TextArea
            app.Assistant_TextArea = uitextarea(app.GridLayout15);
            app.Assistant_TextArea.Editable = 'off';
            app.Assistant_TextArea.FontSize = 20;
            app.Assistant_TextArea.BackgroundColor = [0.902 0.902 0.902];
            app.Assistant_TextArea.Layout.Row = [2 18];
            app.Assistant_TextArea.Layout.Column = [1 7];

            % Create MessagesLabel
            app.MessagesLabel = uilabel(app.GridLayout15);
            app.MessagesLabel.HorizontalAlignment = 'center';
            app.MessagesLabel.FontSize = 15;
            app.MessagesLabel.FontWeight = 'bold';
            app.MessagesLabel.Layout.Row = 1;
            app.MessagesLabel.Layout.Column = [1 7];
            app.MessagesLabel.Text = 'Messages';

            % Create FigHistLive
            app.FigHistLive = uiaxes(app.GridLayout8);
            app.FigHistLive.XTick = [];
            app.FigHistLive.YTick = [];
            app.FigHistLive.YScale = 'log';
            app.FigHistLive.YMinorTick = 'on';
            app.FigHistLive.ColorOrder = [0 0.4471 0.7412;0.851 0.3255 0.098;0.9294 0.6941 0.1255;0.4941 0.1843 0.5569;0.4667 0.6745 0.1882;0.302 0.7451 0.9333;0.6353 0.0784 0.1843];
            app.FigHistLive.FontSize = 1;
            app.FigHistLive.Box = 'on';
            app.FigHistLive.Layout.Row = [1 3];
            app.FigHistLive.Layout.Column = [1 6];
            app.FigHistLive.ButtonDownFcn = createCallbackFcn(app, @FigHistLiveButtonDown, true);

            % Create XMapToolsMenu
            app.XMapToolsMenu = uimenu(app.XMapTools_GUI);
            app.XMapToolsMenu.Text = 'XMapTools';

            % Create AboutMenu
            app.AboutMenu = uimenu(app.XMapToolsMenu);
            app.AboutMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_AboutMenuSelected, true);
            app.AboutMenu.Separator = 'on';
            app.AboutMenu.Text = 'About ...';

            % Create LicenseMenu
            app.LicenseMenu = uimenu(app.XMapToolsMenu);
            app.LicenseMenu.MenuSelectedFcn = createCallbackFcn(app, @LicenseMenuSelected, true);
            app.LicenseMenu.Text = 'License';

            % Create SetWorkingDirectoryMenu
            app.SetWorkingDirectoryMenu = uimenu(app.XMapToolsMenu);
            app.SetWorkingDirectoryMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_SetWorkingDirectorySelected, true);
            app.SetWorkingDirectoryMenu.Separator = 'on';
            app.SetWorkingDirectoryMenu.Text = 'Set Working Directory';

            % Create StartNewSessionMenu
            app.StartNewSessionMenu = uimenu(app.XMapToolsMenu);
            app.StartNewSessionMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_StartNewSessionMenuSelected, true);
            app.StartNewSessionMenu.Text = 'Start New Session';

            % Create QuitMenu
            app.QuitMenu = uimenu(app.XMapToolsMenu);
            app.QuitMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_XMapToolsQuitSelected, true);
            app.QuitMenu.Separator = 'on';
            app.QuitMenu.Text = 'Quit';

            % Create FileMenu
            app.FileMenu = uimenu(app.XMapTools_GUI);
            app.FileMenu.Text = 'File';

            % Create InfosMenu
            app.InfosMenu = uimenu(app.FileMenu);
            app.InfosMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_FileMapInfosSelected, true);
            app.InfosMenu.Text = 'Map Infos';

            % Create SaveImageMenu
            app.SaveImageMenu = uimenu(app.FileMenu);
            app.SaveImageMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_FileSaveImageSelected, true);
            app.SaveImageMenu.Text = 'Save Image';

            % Create OpenProjectMenu
            app.OpenProjectMenu = uimenu(app.FileMenu);
            app.OpenProjectMenu.MenuSelectedFcn = createCallbackFcn(app, @ProjectOpenButtonPushed, true);
            app.OpenProjectMenu.Separator = 'on';
            app.OpenProjectMenu.Text = 'Open Project';

            % Create SaveProjectMenu
            app.SaveProjectMenu = uimenu(app.FileMenu);
            app.SaveProjectMenu.MenuSelectedFcn = createCallbackFcn(app, @ProjectSaveButtonPushed, true);
            app.SaveProjectMenu.Text = 'Save Project';

            % Create SaveProjectAsMenu
            app.SaveProjectAsMenu = uimenu(app.FileMenu);
            app.SaveProjectAsMenu.MenuSelectedFcn = createCallbackFcn(app, @ProjectSaveAsButtonPushed, true);
            app.SaveProjectAsMenu.Text = 'Save Project As ...';

            % Create EditMenu
            app.EditMenu = uimenu(app.XMapTools_GUI);
            app.EditMenu.Text = 'Edit';

            % Create MapMenu
            app.MapMenu = uimenu(app.EditMenu);
            app.MapMenu.Text = 'Map';

            % Create DuplicateMenu
            app.DuplicateMenu = uimenu(app.MapMenu);
            app.DuplicateMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_EditMapDuplicateSelected, true);
            app.DuplicateMenu.Separator = 'on';
            app.DuplicateMenu.Text = 'Duplicate';

            % Create DuplicateandAdjustMergedMenu
            app.DuplicateandAdjustMergedMenu = uimenu(app.MapMenu);
            app.DuplicateandAdjustMergedMenu.MenuSelectedFcn = createCallbackFcn(app, @DuplicateandAdjustMergedMenuSelected, true);
            app.DuplicateandAdjustMergedMenu.Text = 'Duplicate and Adjust (Merged)';

            % Create DeleteMenu
            app.DeleteMenu = uimenu(app.MapMenu);
            app.DeleteMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_EditMapDeleteSelected, true);
            app.DeleteMenu.Text = 'Delete';

            % Create EliminatePixelsMenu
            app.EliminatePixelsMenu = uimenu(app.MapMenu);
            app.EliminatePixelsMenu.Text = 'Eliminate Pixels';

            % Create SelectROIMenu
            app.SelectROIMenu = uimenu(app.EliminatePixelsMenu);
            app.SelectROIMenu.Text = 'Select ROI';

            % Create RectangleMenu
            app.RectangleMenu = uimenu(app.SelectROIMenu);
            app.RectangleMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_EditMapROIRectangleSelected, true);
            app.RectangleMenu.Text = 'Rectangle';

            % Create PolygonMenu
            app.PolygonMenu = uimenu(app.SelectROIMenu);
            app.PolygonMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_EditMapROIPolygonSelected, true);
            app.PolygonMenu.Text = 'Polygon';

            % Create EliminateinsideMenu
            app.EliminateinsideMenu = uimenu(app.EliminatePixelsMenu);
            app.EliminateinsideMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_EditMapROIElimInsideSelected, true);
            app.EliminateinsideMenu.Separator = 'on';
            app.EliminateinsideMenu.Text = 'Eliminate inside';

            % Create EliminateoutsideMenu
            app.EliminateoutsideMenu = uimenu(app.EliminatePixelsMenu);
            app.EliminateoutsideMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_EditMapROIElimOutsideSelected, true);
            app.EliminateoutsideMenu.Text = 'Eliminate outside';

            % Create ExportMapDataMenu
            app.ExportMapDataMenu = uimenu(app.MapMenu);
            app.ExportMapDataMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_EditMapExportMapDataSelected, true);
            app.ExportMapDataMenu.Text = 'Export Map Data';

            % Create CopyMapDataMenu
            app.CopyMapDataMenu = uimenu(app.MapMenu);
            app.CopyMapDataMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_EditMapCopyMapDataSelected, true);
            app.CopyMapDataMenu.Text = 'Copy Map Data';

            % Create SelectAreaMenu
            app.SelectAreaMenu = uimenu(app.MapMenu);
            app.SelectAreaMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_EditMapSelectAreaMenuSelected, true);
            app.SelectAreaMenu.Separator = 'on';
            app.SelectAreaMenu.Text = 'Select Area';

            % Create CropMenu
            app.CropMenu = uimenu(app.MapMenu);
            app.CropMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_EditMapCropMenuSelected, true);
            app.CropMenu.Text = 'Crop';

            % Create DatasetMenu
            app.DatasetMenu = uimenu(app.EditMenu);
            app.DatasetMenu.Text = 'Dataset';

            % Create ExportMergedMenu
            app.ExportMergedMenu = uimenu(app.DatasetMenu);
            app.ExportMergedMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_Sampling_Export_MergedMenuSelected, true);
            app.ExportMergedMenu.Text = 'Export (Merged)';

            % Create Exportashdf5ResultsMenu
            app.Exportashdf5ResultsMenu = uimenu(app.DatasetMenu);
            app.Exportashdf5ResultsMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_Sampling_Export_hdf5MenuSelected, true);
            app.Exportashdf5ResultsMenu.Text = 'Export as hdf5 (Results)';

            % Create UpdateElementOxideIndexationMenu
            app.UpdateElementOxideIndexationMenu = uimenu(app.DatasetMenu);
            app.UpdateElementOxideIndexationMenu.MenuSelectedFcn = createCallbackFcn(app, @UpdateElementOxideIndexationMenuSelected, true);
            app.UpdateElementOxideIndexationMenu.Text = 'Update Element/Oxide Indexation';

            % Create MaskMenu
            app.MaskMenu = uimenu(app.EditMenu);
            app.MaskMenu.Text = 'Mask';

            % Create ExportMaskFileMenu
            app.ExportMaskFileMenu = uimenu(app.MaskMenu);
            app.ExportMaskFileMenu.MenuSelectedFcn = createCallbackFcn(app, @ExportMaskFileMenuSelected, true);
            app.ExportMaskFileMenu.Text = 'Export Mask File';

            % Create ReorderMaskMenu
            app.ReorderMaskMenu = uimenu(app.MaskMenu);
            app.ReorderMaskMenu.Separator = 'on';
            app.ReorderMaskMenu.Text = 'Reorder';

            % Create ApplyColorsAutoMenu
            app.ApplyColorsAutoMenu = uimenu(app.MaskMenu);
            app.ApplyColorsAutoMenu.MenuSelectedFcn = createCallbackFcn(app, @ApplyColorsAutoMenuSelected, true);
            app.ApplyColorsAutoMenu.Separator = 'on';
            app.ApplyColorsAutoMenu.Text = 'Apply Colors (Auto)';

            % Create EditColorsManualMenu
            app.EditColorsManualMenu = uimenu(app.MaskMenu);
            app.EditColorsManualMenu.MenuSelectedFcn = createCallbackFcn(app, @EditColorsManualMenuSelected, true);
            app.EditColorsManualMenu.Text = 'Edit Colors (Manual)';

            % Create DriftCorrectionMenu
            app.DriftCorrectionMenu = uimenu(app.EditMenu);
            app.DriftCorrectionMenu.MenuSelectedFcn = createCallbackFcn(app, @DriftCorrectionMenuSelected, true);
            app.DriftCorrectionMenu.Text = 'Drift Correction';

            % Create ReorderMenu
            app.ReorderMenu = uimenu(app.EditMenu);
            app.ReorderMenu.MenuSelectedFcn = createCallbackFcn(app, @ReorderMenuSelected, true);
            app.ReorderMenu.Separator = 'on';
            app.ReorderMenu.Text = 'Reorder';

            % Create CopyImageMenu
            app.CopyImageMenu = uimenu(app.EditMenu);
            app.CopyImageMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_EditCopyImageSelected, true);
            app.CopyImageMenu.Separator = 'on';
            app.CopyImageMenu.Text = 'Copy Image';

            % Create OpenImageNewWindowMenu_2
            app.OpenImageNewWindowMenu_2 = uimenu(app.EditMenu);
            app.OpenImageNewWindowMenu_2.MenuSelectedFcn = createCallbackFcn(app, @Menu_EditOpenImageNewWindowSelected, true);
            app.OpenImageNewWindowMenu_2.Text = 'Open Image New Window';

            % Create Plot3DsurfaceMenu
            app.Plot3DsurfaceMenu = uimenu(app.EditMenu);
            app.Plot3DsurfaceMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_EditPlot3DsurfaceSelected, true);
            app.Plot3DsurfaceMenu.Text = 'Plot 3D surface';

            % Create ResetROIMenu_2
            app.ResetROIMenu_2 = uimenu(app.EditMenu);
            app.ResetROIMenu_2.MenuSelectedFcn = createCallbackFcn(app, @Menu_Sampling_ResetROIMenuSelected, true);
            app.ResetROIMenu_2.Separator = 'on';
            app.ResetROIMenu_2.Text = 'Reset ROI';

            % Create PlotMenu
            app.PlotMenu = uimenu(app.XMapTools_GUI);
            app.PlotMenu.Text = 'Plot';

            % Create CorrelationMenu
            app.CorrelationMenu = uimenu(app.PlotMenu);
            app.CorrelationMenu.Text = 'Correlation';

            % Create PlotMatrixMenu
            app.PlotMatrixMenu = uimenu(app.CorrelationMenu);
            app.PlotMatrixMenu.MenuSelectedFcn = createCallbackFcn(app, @PlotMatrixMenuSelected, true);
            app.PlotMatrixMenu.Text = 'Plot Matrix';

            % Create PlotMatrixValuesMenu
            app.PlotMatrixValuesMenu = uimenu(app.CorrelationMenu);
            app.PlotMatrixValuesMenu.MenuSelectedFcn = createCallbackFcn(app, @PlotMatrixValuesMenuSelected, true);
            app.PlotMatrixValuesMenu.Text = 'Plot Matrix Values';

            % Create PlotScatterplotMatrixMenu
            app.PlotScatterplotMatrixMenu = uimenu(app.CorrelationMenu);
            app.PlotScatterplotMatrixMenu.MenuSelectedFcn = createCallbackFcn(app, @PlotScatterplotMatrixMenuSelected, true);
            app.PlotScatterplotMatrixMenu.Text = 'Plot Scatterplot Matrix';

            % Create SaveDataMenu
            app.SaveDataMenu = uimenu(app.CorrelationMenu);
            app.SaveDataMenu.Text = 'Save Data';

            % Create AutoContrastMenu
            app.AutoContrastMenu = uimenu(app.PlotMenu);
            app.AutoContrastMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_AutoContrastMenuSelected, true);
            app.AutoContrastMenu.Separator = 'on';
            app.AutoContrastMenu.Text = 'Auto Contrast';

            % Create MedianFilterMenu
            app.MedianFilterMenu = uimenu(app.PlotMenu);
            app.MedianFilterMenu.MenuSelectedFcn = createCallbackFcn(app, @Button_FigMain_MedianFilterPushed, true);
            app.MedianFilterMenu.Text = 'Median Filter';

            % Create ResetDisplayMenu
            app.ResetDisplayMenu = uimenu(app.PlotMenu);
            app.ResetDisplayMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_Plot_ResetDisplayMenuSelected, true);
            app.ResetDisplayMenu.Separator = 'on';
            app.ResetDisplayMenu.Text = 'Reset Display';

            % Create ImageMenu
            app.ImageMenu = uimenu(app.XMapTools_GUI);
            app.ImageMenu.Text = 'Image';

            % Create Image_MultiSelectionModeMenu
            app.Image_MultiSelectionModeMenu = uimenu(app.ImageMenu);
            app.Image_MultiSelectionModeMenu.MenuSelectedFcn = createCallbackFcn(app, @Image_MultiSelectionModeMenuSelected, true);
            app.Image_MultiSelectionModeMenu.Text = 'Multi-Selection Mode';

            % Create Image_AddCurrentImageMenu
            app.Image_AddCurrentImageMenu = uimenu(app.ImageMenu);
            app.Image_AddCurrentImageMenu.MenuSelectedFcn = createCallbackFcn(app, @Image_AddCurrentImageMenuSelected, true);
            app.Image_AddCurrentImageMenu.Separator = 'on';
            app.Image_AddCurrentImageMenu.Text = 'Add Current Image';

            % Create Image_AddMultiPlotImageMenu
            app.Image_AddMultiPlotImageMenu = uimenu(app.ImageMenu);
            app.Image_AddMultiPlotImageMenu.MenuSelectedFcn = createCallbackFcn(app, @Image_AddMultiPlotImageMenuSelected, true);
            app.Image_AddMultiPlotImageMenu.Text = 'Add Multi-Plot Image';

            % Create Image_AddMultiLayerImagesharedscaleMenu
            app.Image_AddMultiLayerImagesharedscaleMenu = uimenu(app.ImageMenu);
            app.Image_AddMultiLayerImagesharedscaleMenu.MenuSelectedFcn = createCallbackFcn(app, @Image_AddMultiLayerImagesharedscaleMenuSelected, true);
            app.Image_AddMultiLayerImagesharedscaleMenu.Text = 'Add Multi-Layer Image (shared-scale)';

            % Create Image_AddMultiLayerImageMenu
            app.Image_AddMultiLayerImageMenu = uimenu(app.ImageMenu);
            app.Image_AddMultiLayerImageMenu.MenuSelectedFcn = createCallbackFcn(app, @Image_AddMultiLayerImageMenuSelected, true);
            app.Image_AddMultiLayerImageMenu.Text = 'Add Multi-Layer Image (multi-scale)';

            % Create SamplingMenu
            app.SamplingMenu = uimenu(app.XMapTools_GUI);
            app.SamplingMenu.Text = 'Sampling';

            % Create CircleMenu
            app.CircleMenu = uimenu(app.SamplingMenu);
            app.CircleMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_Sampling_CircleMenuSelected, true);
            app.CircleMenu.Text = 'Circle';

            % Create AreaPolygonMenu
            app.AreaPolygonMenu = uimenu(app.SamplingMenu);
            app.AreaPolygonMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_Sampling_AreaMenuSelected, true);
            app.AreaPolygonMenu.Text = 'Area (Polygon)';

            % Create TransectMenu
            app.TransectMenu = uimenu(app.SamplingMenu);
            app.TransectMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_Sampling_TransectMenuSelected, true);
            app.TransectMenu.Text = 'Transect';

            % Create StripMenu
            app.StripMenu = uimenu(app.SamplingMenu);
            app.StripMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_Sampling_StripeMenuSelected, true);
            app.StripMenu.Text = 'Strip';

            % Create SaveResultsMenu
            app.SaveResultsMenu = uimenu(app.SamplingMenu);
            app.SaveResultsMenu.Separator = 'on';
            app.SaveResultsMenu.Text = 'Save Results';

            % Create SingleMapMenu
            app.SingleMapMenu = uimenu(app.SaveResultsMenu);
            app.SingleMapMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_Sampling_Export_SingleMapMenuSelected, true);
            app.SingleMapMenu.Text = 'Single Map';

            % Create MultipleMapsMenu
            app.MultipleMapsMenu = uimenu(app.SaveResultsMenu);
            app.MultipleMapsMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_Sampling_Export_MultipleMapsMenuSelected, true);
            app.MultipleMapsMenu.Text = 'Multiple Maps';

            % Create ResetROIMenu
            app.ResetROIMenu = uimenu(app.SamplingMenu);
            app.ResetROIMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_Sampling_ResetROIMenuSelected, true);
            app.ResetROIMenu.Text = 'Reset ROI';

            % Create HoldROIonMenu
            app.HoldROIonMenu = uimenu(app.SamplingMenu);
            app.HoldROIonMenu.MenuSelectedFcn = createCallbackFcn(app, @HoldROIonMenuSelected, true);
            app.HoldROIonMenu.Text = 'Hold ROI on';

            % Create WorkspacesMenu
            app.WorkspacesMenu = uimenu(app.XMapTools_GUI);
            app.WorkspacesMenu.Text = 'Workspaces';

            % Create Workspace_ProjectImportMenu
            app.Workspace_ProjectImportMenu = uimenu(app.WorkspacesMenu);
            app.Workspace_ProjectImportMenu.MenuSelectedFcn = createCallbackFcn(app, @Workspace_ProjectImportMenuSelected, true);
            app.Workspace_ProjectImportMenu.Text = 'Project and Import';

            % Create Workspace_ClassifyMenu
            app.Workspace_ClassifyMenu = uimenu(app.WorkspacesMenu);
            app.Workspace_ClassifyMenu.MenuSelectedFcn = createCallbackFcn(app, @Workspace_ClassifyMenuSelected, true);
            app.Workspace_ClassifyMenu.Text = 'Classify';

            % Create Workspace_CalibrateMenu
            app.Workspace_CalibrateMenu = uimenu(app.WorkspacesMenu);
            app.Workspace_CalibrateMenu.MenuSelectedFcn = createCallbackFcn(app, @Workspace_CalibrateMenuSelected, true);
            app.Workspace_CalibrateMenu.Text = 'Calibrate';

            % Create Workspace_FunctionsMenu
            app.Workspace_FunctionsMenu = uimenu(app.WorkspacesMenu);
            app.Workspace_FunctionsMenu.MenuSelectedFcn = createCallbackFcn(app, @Workspace_FunctionsMenuSelected, true);
            app.Workspace_FunctionsMenu.Text = 'Functions';

            % Create Workspace_SegmentCTMenu
            app.Workspace_SegmentCTMenu = uimenu(app.WorkspacesMenu);
            app.Workspace_SegmentCTMenu.MenuSelectedFcn = createCallbackFcn(app, @Workspace_SegmentCTMenuSelected, true);
            app.Workspace_SegmentCTMenu.Text = 'Segment (CT)';

            % Create Workspace_AddonsMenu
            app.Workspace_AddonsMenu = uimenu(app.WorkspacesMenu);
            app.Workspace_AddonsMenu.MenuSelectedFcn = createCallbackFcn(app, @Workspace_AddonsMenuSelected, true);
            app.Workspace_AddonsMenu.Text = 'Add-ons';

            % Create Workspace_OptionsMenu
            app.Workspace_OptionsMenu = uimenu(app.WorkspacesMenu);
            app.Workspace_OptionsMenu.MenuSelectedFcn = createCallbackFcn(app, @Workspace_OptionsMenuSelected, true);
            app.Workspace_OptionsMenu.Text = 'Options';

            % Create Workspace_DeveloperMenu
            app.Workspace_DeveloperMenu = uimenu(app.WorkspacesMenu);
            app.Workspace_DeveloperMenu.MenuSelectedFcn = createCallbackFcn(app, @Workspace_DeveloperMenuSelected, true);
            app.Workspace_DeveloperMenu.Text = 'Developer';

            % Create ModulesMenu
            app.ModulesMenu = uimenu(app.XMapTools_GUI);
            app.ModulesMenu.Text = 'Modules';

            % Create DataVisualizationMenu
            app.DataVisualizationMenu = uimenu(app.ModulesMenu);
            app.DataVisualizationMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_Modules_VisualizationToolMenuSelected, true);
            app.DataVisualizationMenu.Text = 'Data Visualization';

            % Create SpiderPlotMenu
            app.SpiderPlotMenu = uimenu(app.ModulesMenu);
            app.SpiderPlotMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_Modules_SpiderPlotMenuSelected, true);
            app.SpiderPlotMenu.Text = 'Spider Plot';

            % Create GeneratorMenu
            app.GeneratorMenu = uimenu(app.ModulesMenu);
            app.GeneratorMenu.MenuSelectedFcn = createCallbackFcn(app, @Menu_Modules_GeneratorMenuSelected, true);
            app.GeneratorMenu.Separator = 'on';
            app.GeneratorMenu.Text = 'Generator';

            % Create HelpMenu
            app.HelpMenu = uimenu(app.XMapTools_GUI);
            app.HelpMenu.Text = 'Help';

            % Create Help_TabsMenu
            app.Help_TabsMenu = uimenu(app.HelpMenu);
            app.Help_TabsMenu.Text = 'Workspaces';

            % Create Help_ProjectImportMenu
            app.Help_ProjectImportMenu = uimenu(app.Help_TabsMenu);
            app.Help_ProjectImportMenu.MenuSelectedFcn = createCallbackFcn(app, @Help_ImportTab_helpButtonPushed, true);
            app.Help_ProjectImportMenu.Text = 'Project, Import';

            % Create Help_ClassifyMenu
            app.Help_ClassifyMenu = uimenu(app.Help_TabsMenu);
            app.Help_ClassifyMenu.MenuSelectedFcn = createCallbackFcn(app, @Help_ClassifyTab_helpButtonPushed, true);
            app.Help_ClassifyMenu.Text = 'Classify';

            % Create Help_CalibrateMenu
            app.Help_CalibrateMenu = uimenu(app.Help_TabsMenu);
            app.Help_CalibrateMenu.MenuSelectedFcn = createCallbackFcn(app, @Help_CalibratetTab_helpButtonPushed, true);
            app.Help_CalibrateMenu.Text = 'Calibrate';

            % Create Help_FunctionsMenu
            app.Help_FunctionsMenu = uimenu(app.Help_TabsMenu);
            app.Help_FunctionsMenu.MenuSelectedFcn = createCallbackFcn(app, @Help_FunctionTab_helpButtonPushed, true);
            app.Help_FunctionsMenu.Text = 'Functions';

            % Create Help_SegmentMenu
            app.Help_SegmentMenu = uimenu(app.Help_TabsMenu);
            app.Help_SegmentMenu.MenuSelectedFcn = createCallbackFcn(app, @Help_SegmentTab_helpButtonPushed, true);
            app.Help_SegmentMenu.Text = 'Segment (CT)';

            % Create Help_AddonsMenu
            app.Help_AddonsMenu = uimenu(app.Help_TabsMenu);
            app.Help_AddonsMenu.MenuSelectedFcn = createCallbackFcn(app, @Help_AddonsTab_helpButtonPushed, true);
            app.Help_AddonsMenu.Text = 'Add-ons';

            % Create Help_OptionsMenu
            app.Help_OptionsMenu = uimenu(app.Help_TabsMenu);
            app.Help_OptionsMenu.MenuSelectedFcn = createCallbackFcn(app, @Help_OptionsTab_helpButtonPushed, true);
            app.Help_OptionsMenu.Text = 'Options';

            % Create Help_SamplingToolsMenu
            app.Help_SamplingToolsMenu = uimenu(app.HelpMenu);
            app.Help_SamplingToolsMenu.MenuSelectedFcn = createCallbackFcn(app, @Help_SamplingToolsMenuSelected, true);
            app.Help_SamplingToolsMenu.Text = 'Sampling Tools';

            % Create Help_DataVizualisationToolMenu
            app.Help_DataVizualisationToolMenu = uimenu(app.HelpMenu);
            app.Help_DataVizualisationToolMenu.MenuSelectedFcn = createCallbackFcn(app, @Help_DataVizualisationToolMenuSelected, true);
            app.Help_DataVizualisationToolMenu.Text = 'Data Vizualisation Tool';

            % Create ImagesMenu
            app.ImagesMenu = uimenu(app.HelpMenu);
            app.ImagesMenu.MenuSelectedFcn = createCallbackFcn(app, @Help_ImagesMenuSelected, true);
            app.ImagesMenu.Text = 'Images';

            % Show the figure after all components are created
            app.XMapTools_GUI.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = XMapTools_exported(varargin)

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.XMapTools_GUI)

            % Execute the startup function
            runStartupFcn(app, @(app)startupFcn(app, varargin{:}))

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.XMapTools_GUI)
        end
    end
end