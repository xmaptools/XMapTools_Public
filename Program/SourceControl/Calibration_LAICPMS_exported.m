classdef Calibration_LAICPMS_exported < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        XMapToolsCalibrationLaICPMS     matlab.ui.Figure
        GridLayout                      matlab.ui.container.GridLayout
        Image                           matlab.ui.control.Image
        GridLayout2                     matlab.ui.container.GridLayout
        Button_Help                     matlab.ui.control.Button
        GridLayout3                     matlab.ui.container.GridLayout
        AppyCalibration                 matlab.ui.control.Button
        TabGroup                        matlab.ui.container.TabGroup
        CalibrationDefinitionTab        matlab.ui.container.Tab
        GridLayout6                     matlab.ui.container.GridLayout
        GridLayout4                     matlab.ui.container.GridLayout
        ClassDropDownLabel              matlab.ui.control.Label
        ClassDropDown                   matlab.ui.control.DropDown
        InternalElementStdDropDownLabel  matlab.ui.control.Label
        InternalElementStdDropDown      matlab.ui.control.DropDown
        ConstantcompositionCheckBox     matlab.ui.control.CheckBox
        VariablecompositionCheckBox     matlab.ui.control.CheckBox
        FixedInternalStdConc_EditFieldLabel  matlab.ui.control.Label
        FixedInternalStdConc_EditField  matlab.ui.control.NumericEditField
        InternalCompositionConverterLabel  matlab.ui.control.Label
        wtOxideDropDownLabel            matlab.ui.control.Label
        wtOxideDropDown                 matlab.ui.control.DropDown
        Oxide_EditField                 matlab.ui.control.NumericEditField
        ppm_EditField                   matlab.ui.control.NumericEditField
        toXXLabel                       matlab.ui.control.Label
        ggLabel                         matlab.ui.control.Label
        UITable                         matlab.ui.control.Table
        ElementDropDownLabel            matlab.ui.control.Label
        ElementDropDown                 matlab.ui.control.DropDown
        DisplayValue                    matlab.ui.control.NumericEditField
        ButtonTarget                    matlab.ui.control.Button
        UITable_ElemList                matlab.ui.control.Table
        GridLayout5                     matlab.ui.container.GridLayout
        Standard_DropDown               matlab.ui.control.DropDown
        StdApplytoallButton             matlab.ui.control.Button
        StdApplytoSelectionButton       matlab.ui.control.Button
        SaveunfilteredmapsCheckBox      matlab.ui.control.CheckBox
        BydefaultLODfilteredmapsaresavedLabel  matlab.ui.control.Label
        LODfilteringLabel               matlab.ui.control.Label
        Plot1                           matlab.ui.control.UIAxes
        Plot3                           matlab.ui.control.UIAxes
        Plot2                           matlab.ui.control.UIAxes
        PixelReconstructionandImprovedPrecisionPRIPTab  matlab.ui.container.Tab
        PRIP_GridLayout                 matlab.ui.container.GridLayout
        PRIP_Table_ROI                  matlab.ui.control.Table
        PRIP_Table_Analysis             matlab.ui.control.Table
        GridLayout8                     matlab.ui.container.GridLayout
        PRIP_AddROI                     matlab.ui.control.Button
        PRIP_DeleteROI                  matlab.ui.control.Button
        PRIP_MenuROI                    matlab.ui.control.DropDown
        PRIP_Export                     matlab.ui.control.Button
        GridLayout9                     matlab.ui.container.GridLayout
        PRIP_MapMenu                    matlab.ui.control.DropDown
        PRIP_Limits_Min                 matlab.ui.control.NumericEditField
        PRIP_Limits_Max                 matlab.ui.control.NumericEditField
        PRIP_LogScale                   matlab.ui.control.CheckBox
        PRIP_Plot                       matlab.ui.control.UIAxes
        PRIPAITab                       matlab.ui.container.Tab
        PRIPAI_GridLayout10             matlab.ui.container.GridLayout
        ElemPxPrec_UITable              matlab.ui.control.Table
        GridLayout10                    matlab.ui.container.GridLayout
        PRIP_AI_Grid_Min                matlab.ui.control.NumericEditField
        PRIP_AI_Grid_Step               matlab.ui.control.NumericEditField
        PRIP_AI_Grid_Max                matlab.ui.control.NumericEditField
        CalculateROIsButton             matlab.ui.control.Button
        GridsizeminstepmaxLabel         matlab.ui.control.Label
        Label                           matlab.ui.control.Label
        PRIP_AI_Grid_Nb                 matlab.ui.control.NumericEditField
        SetasPredictorButton            matlab.ui.control.Button
        SetasOutputButton               matlab.ui.control.Button
        GridLayout11                    matlab.ui.container.GridLayout
        TrainSNNButton                  matlab.ui.control.Button
        GridResolutionDropDown          matlab.ui.control.DropDown
        ResolutionDropDownLabel         matlab.ui.control.Label
        PRIP_AI_MapMenu                 matlab.ui.control.DropDown
        ParametersPanel                 matlab.ui.container.Panel
        GridLayout12                    matlab.ui.container.GridLayout
        NormalisationCheckBox           matlab.ui.control.CheckBox
        NbNeuronsInput                  matlab.ui.control.NumericEditField
        NbLayersInput                   matlab.ui.control.NumericEditField
        NeuronsLabel                    matlab.ui.control.Label
        LayersLabel                     matlab.ui.control.Label
        MultipleSNNCheckBox             matlab.ui.control.CheckBox
        PRIP_AI_Plot                    matlab.ui.control.UIAxes
        ContextMenu                     matlab.ui.container.ContextMenu
        CopyMenu                        matlab.ui.container.Menu
    end

    
    properties (Access = private)
        XMapToolsApp        % Description
        MaskFile
        
        ElNamesStd
        NbElStd
        
        % MapStandardsData    % eliminated and replaced by MapStandards
        MapStandards
        MapCpsData
        
        MapCpsData_PRIP_AI
        
        PxDataRaw       % For PRIP
        
        Map_k
        
        ROI_sampling
        ROI_sampling_Listener
        ROI_sampling_Listener2
        
        DataInter
        NbPoints
        PointsValid
        
        CalibrationOptions
        
        SelectedCell
        
        PRIP_Activated
        
        WaitBar
        
        UpdatePlotROI
        
        PRIP_ROIs
        PRIP_ROIs_Listener
        PRIP_ROIs_Data
        
        PRIP_AI_GridData
        PRIP_AI_SelectedPredictors
        
        PRIP_AI_SelectedRowsinTable
        
        GeneratedDataCellFormat
        GeneratedLabelCellFormat
        GeneratedLODCellFormat
        
    end
    
    methods (Access = private)
        
        function PlotAllMaps(app)
            
            % Mask
            MaskId = app.ClassDropDown.Value;
            
            MaskMap = zeros(size(app.MaskFile.MaskMap));
            MaskInd = find(app.MaskFile.MaskMap == MaskId);
            MaskMap(MaskInd) = ones(size(MaskInd));
            
            % Intensity
            ElId = app.ElementDropDown.Value;
            CpsMap = app.MapCpsData(ElId).Cps;
            
            imagesc(app.Plot1,CpsMap.*MaskMap)
            axis(app.Plot1,'image')
            if size(MaskMap,2) > 1.2*size(MaskMap,1)
                colorbar(app.Plot1,'horizontal');
            else
                colorbar(app.Plot1,'vertical');
            end
            colormap(app.Plot1,app.XMapToolsApp.ColorMapValues_noMask);
            title(app.Plot1,[char(app.ElNamesStd{ElId}),' intensity (in cps)'])
            
            tb = axtoolbar(app.Plot1,{'pan','zoomin','zoomout','restoreview'});
            disableDefaultInteractivity(app.Plot1);
            
            drawnow
            
            WePlot = 0;
            if isequal(app.ConstantcompositionCheckBox.Value,1)
                RefConc = app.FixedInternalStdConc_EditField.Value;
                if ~isequal(RefConc,0)
                    WePlot = 1;
                end
            end
            
            if isequal(app.VariablecompositionCheckBox.Value,1)
                if sum(app.PointsValid) >= 2
                    WePlot = 1;
                end
            end
            
            
            if WePlot
                
                SelStd = app.CalibrationOptions.SelStd(ElId);
                
                imagesc(app.Plot3,app.Map_k(:,:,SelStd))
                axis(app.Plot3,'image')
                if size(MaskMap,2) > 1.2*size(MaskMap,1)
                    colorbar(app.Plot3,'horizontal');
                else
                    colorbar(app.Plot3,'vertical');
                end
                colormap(app.Plot3,app.XMapToolsApp.ColorMapValues_noMask);
                title(app.Plot3,['k matrix (',app.CalibrationOptions.StdOptions{SelStd},')'],'Interpreter','none')
                
                imagesc(app.Plot2,app.MapCpsData(ElId).Conc)
                axis(app.Plot2,'image')
                if size(MaskMap,2) > 1.2*size(MaskMap,1)
                    colorbar(app.Plot2,'horizontal');
                else
                    colorbar(app.Plot2,'vertical');
                end
                colormap(app.Plot2,app.XMapToolsApp.ColorMapValues_noMask);
                title(app.Plot2,[char(app.ElNamesStd{ElId}),' (in µg/g)'])
                
                app.Plot2.Visible = 'on';
                app.Plot3.Visible = 'on';
                
                disableDefaultInteractivity(app.Plot2);
                disableDefaultInteractivity(app.Plot3);
            else
                app.AppyCalibration.Enable = 'off';
                cla(app.Plot2)
                cla(app.Plot3)
                app.Plot2.Visible = 'off';
                app.Plot3.Visible = 'off';
            end
            
            %keyboard
            
        end
        
        
        function Plot_PRIP(app)
            % Mask
            MaskId = app.ClassDropDown.Value;
            
            MaskMap = zeros(size(app.MaskFile.MaskMap));
            MaskInd = find(app.MaskFile.MaskMap == MaskId);
            MaskMap(MaskInd) = ones(size(MaskInd));
            
            % Intensity
            ElId = app.PRIP_MapMenu.Value;
            ConcMap = app.MapCpsData(ElId).Conc;
            
            cla(app.PRIP_Plot,'reset')
            
            imagesc(app.PRIP_Plot,ConcMap.*MaskMap);
            axis(app.PRIP_Plot,'image')
            if size(MaskMap,2) > 1.2*size(MaskMap,1)
                colorbar(app.PRIP_Plot,'horizontal');
            else
                colorbar(app.PRIP_Plot,'vertical');
            end
            colormap(app.PRIP_Plot,[app.XMapToolsApp.ColorMapValues_noMask;1.0000,0.0784,0.5765]);
            
            app.PRIP_Limits_Min.Value = min(ConcMap(:));
            app.PRIP_Limits_Max.Value = max(ConcMap(:));
            
            LodMap = app.MapCpsData(ElId).LOD;
            
            WhereBelowLOD = find(ConcMap > 0 & ConcMap <= LodMap);
            
            LOD_Filter = nan(size(LodMap));
            LOD_Filter(WhereBelowLOD) = 2 * app.PRIP_Limits_Max.Value;
            
            AlphaMap = zeros(size(LOD_Filter));
            AlphaMap(WhereBelowLOD) = 1;
            
            hold(app.PRIP_Plot,'on')
            imagesc(app.PRIP_Plot,LOD_Filter, "AlphaData", AlphaMap);
            
            app.PRIP_Plot.XTick = [];
            app.PRIP_Plot.YTick = [];
            
            disableDefaultInteractivity(app.PRIP_Plot)
            
            CheckLogScale(app)
        end
        
        function Plot_PRIP_AI(app)
            % Mask
            MaskId = app.ClassDropDown.Value;
            
            MaskMap = zeros(size(app.MaskFile.MaskMap));
            MaskInd = find(app.MaskFile.MaskMap == MaskId);
            MaskMap(MaskInd) = ones(size(MaskInd));
            
            % Intensity
            ElId = app.PRIP_AI_MapMenu.Value;
            ConcMap = app.MapCpsData_PRIP_AI(ElId).Conc;
            
            cla(app.PRIP_AI_Plot,'reset')
            
            imagesc(app.PRIP_AI_Plot,ConcMap.*MaskMap);
            axis(app.PRIP_AI_Plot,'image')
            if size(MaskMap,2) > 1.2*size(MaskMap,1)
                colorbar(app.PRIP_AI_Plot,'horizontal');
            else
                colorbar(app.PRIP_AI_Plot,'vertical');
            end
            colormap(app.PRIP_AI_Plot,[app.XMapToolsApp.ColorMapValues_noMask]);
            
            app.PRIP_Limits_Min.Value = min(ConcMap(:));
            app.PRIP_Limits_Max.Value = max(ConcMap(:));
            %
            %             LodMap = app.MapCpsData_PRIP_AI(ElId).LOD;
            %
            %             WhereBelowLOD = find(ConcMap > 0 & ConcMap <= LodMap);
            %
            %             LOD_Filter = nan(size(LodMap));
            %             LOD_Filter(WhereBelowLOD) = 2 * app.PRIP_Limits_Max.Value;
            %
            %             AlphaMap = zeros(size(LOD_Filter));
            %             AlphaMap(WhereBelowLOD) = 1;
            %
            %             hold(app.PRIP_Plot,'on')
            %             imagesc(app.PRIP_Plot,LOD_Filter, "AlphaData", AlphaMap);
            %
            %             app.PRIP_Plot.XTick = [];
            %             app.PRIP_Plot.YTick = [];
            %
            disableDefaultInteractivity(app.PRIP_AI_Plot)
            %
            CheckLogScale(app)
        end
        
        
        
        function CalculateConversion(app)
            
            OxIdx = app.wtOxideDropDown.Value;
            OxValue = app.Oxide_EditField.Value;
            
            app.toXXLabel.Text = ['to ',app.XMapToolsApp.ElOxDataDef.ElList{app.XMapToolsApp.ElOxDataDef.OxElIdx(OxIdx)}];
            
            app.ppm_EditField.Value = OxValue * app.XMapToolsApp.ElOxDataDef.OxFact(OxIdx) * 1e4;
            
        end
        
        function CalibrateMaps(app)
            
            % Fixed composition for the internal standard
            if isequal(app.ConstantcompositionCheckBox.Value,1)
                RefConc = app.FixedInternalStdConc_EditField.Value;
                if ~isequal(RefConc,0)
                    % Mask
                    MaskId = app.ClassDropDown.Value;
                    
                    MaskMap = zeros(size(app.MaskFile.MaskMap));
                    MaskInd = find(app.MaskFile.MaskMap == MaskId);
                    MaskMap(MaskInd) = ones(size(MaskInd));
                    
                    %
                    IntStd_Id = app.InternalElementStdDropDown.Value;
                    
                    if isequal(app.ConstantcompositionCheckBox.Value,1)
                        Con_Unk = ones(size(MaskMap)).* app.FixedInternalStdConc_EditField.Value .* MaskMap;
                    end
                    
                    k_mtx = [];
                    for i =1:length(app.MapStandards)
                        It_Std = app.MapStandards(i).Data(IntStd_Id).Cps .* MaskMap;
                        Con_Std = app.MapStandards(i).Data(IntStd_Id).Conc .* MaskMap;
                        
                        It_Unk = app.MapCpsData(IntStd_Id).Cps .* MaskMap;
                        
                        k_mtx(:,:,i) = (It_Unk.*Con_Std)./(Con_Unk.*It_Std);
                    end
                    
                    for i = 1:length(app.InternalElementStdDropDown.Items)
                        
                        SelStd = app.CalibrationOptions.SelStd(i);
                        
                        It_Unk_mtx = app.MapCpsData(i).Cps;
                        
                        It_Std_mtx = app.MapStandards(SelStd).Data(i).Cps;
                        Con_Std_mxt = app.MapStandards(SelStd).Data(i).Conc;
                        
                        app.MapCpsData(i).Conc = (It_Unk_mtx.*Con_Std_mxt)./(k_mtx(:,:,SelStd).*It_Std_mtx);
                        
                        % Limit of detection (Theoule-sur-Mer)
                        Si = k_mtx(:,:,SelStd).*It_Std_mtx./Con_Std_mxt;                % cps/µg.g-1
                        Bi = app.MapStandards(SelStd).Data(i).Int_Back;                 % cps
                        DTi = app.CalibrationOptions.SweepTime(i)/1e3;                  % seconds
                        NbAnal = app.MapStandards(SelStd).Data(i).Sweeps_Pixel;         % dimensionless
                        NbBack = app.MapStandards(SelStd).Data(i).Sweeps_Back;          % dimensionless
                        
                        app.MapCpsData(i).LOD = (3.29.*sqrt(Bi.*DTi.*NbAnal.*(1+NbAnal./NbBack)+2.71))./(NbAnal.*DTi.*Si);    % µg.g-1
                        
                        % Filter LOD for values with zero intensities for
                        % background (Cavalaire 2023):
                        
                        % FilterLOD = find(Bi == 0);
                        % app.MapCpsData(i).LOD(FilterLOD) = 0;
                        
                        % --> Deactivated after Cassis for PRIP module.
                        
                    end
                    
                    app.Map_k =  k_mtx;
                    
                    app.AppyCalibration.Enable = 'on';
                    
                    app.VariablecompositionCheckBox.Enable = 'off';
                    app.ConstantcompositionCheckBox.Enable = 'off';
                    app.ClassDropDown.Enable = 'off';
                end
            else
                if sum(app.PointsValid) >= 2
                    % Mask
                    MaskId = app.ClassDropDown.Value;
                    
                    MaskMap = zeros(size(app.MaskFile.MaskMap));
                    MaskInd = find(app.MaskFile.MaskMap == MaskId);
                    MaskMap(MaskInd) = ones(size(MaskInd));
                    
                    %
                    IntStd_Id = app.InternalElementStdDropDown.Value;
                    
                    k_mtx = [];
                    for i =1:length(app.MapStandards)
                        It_Std = app.MapStandards(i).Data(IntStd_Id).Cps .* MaskMap;
                        Con_Std = app.MapStandards(i).Data(IntStd_Id).Conc .* MaskMap;
                        
                        It_Unk = app.MapCpsData(IntStd_Id).Cps .* MaskMap;
                        
                        %
                        FitData = app.DataInter(:,find(app.PointsValid));
                        p = polyfit(FitData(1,:),FitData(2,:),1);
                        
                        Con_Unk = polyval(p,It_Unk) .* MaskMap;
                        
                        k_mtx(:,:,i) = (It_Unk.*Con_Std)./(Con_Unk.*It_Std);
                    end
                    
                    for i = 1:length(app.InternalElementStdDropDown.Items)
                        SelStd = app.CalibrationOptions.SelStd(i);
                        
                        It_Unk_mtx = app.MapCpsData(i).Cps;
                        
                        It_Std_mtx = app.MapStandards(SelStd).Data(i).Cps;
                        Con_Std_mxt = app.MapStandards(SelStd).Data(i).Conc;
                        
                        app.MapCpsData(i).Conc = (It_Unk_mtx.*Con_Std_mxt)./(k_mtx(:,:,SelStd).*It_Std_mtx);
                        
                        % Limit of detection (Theoule-sur-Mer)
                        Si = k_mtx(:,:,SelStd).*It_Std_mtx./Con_Std_mxt;                % cps/µg.g-1
                        Bi = app.MapStandards(SelStd).Data(i).Int_Back;                 % cps
                        DTi = app.CalibrationOptions.SweepTime(i)/1e3;                  % seconds
                        NbAnal = app.MapStandards(SelStd).Data(i).Sweeps_Pixel;         % dimensionless
                        NbBack = app.MapStandards(SelStd).Data(i).Sweeps_Back;          % dimensionless
                        
                        app.MapCpsData(i).LOD = (3.29.*sqrt(Bi.*DTi.*NbAnal.*(1+NbAnal./NbBack)+2.71))./(NbAnal.*DTi.*Si);    % µg.g-1
                        
                        %Con_Unk_mtx = (It_Unk_mtx.*Con_Std_mxt)./(k_mtx.*It_Std_mtx);
                    end
                    
                    app.Map_k =  k_mtx;
                    
                    app.AppyCalibration.Enable = 'on';
                    
                    app.VariablecompositionCheckBox.Enable = 'off';
                    app.ConstantcompositionCheckBox.Enable = 'off';
                end
            end
            
            
        end
        
        function ChangeMode(app,Mode)
            
            switch Mode
                
                case 'Fixed'
                    app.ConstantcompositionCheckBox.Value = 1;
                    app.VariablecompositionCheckBox.Value = 0;
                    
                    app.FixedInternalStdConc_EditField.Visible = 'on';
                    app.FixedInternalStdConc_EditFieldLabel.Visible = 'on';
                    
                    app.UITable.Visible = 'off';
                    app.DisplayValue.Visible = 'off';
                    app.ButtonTarget.Visible = 'off';
                    
                    %keyboard
                    
                case 'Variable'
                    app.ConstantcompositionCheckBox.Value = 0;
                    app.VariablecompositionCheckBox.Value = 1;
                    
                    
                    app.FixedInternalStdConc_EditField.Visible = 'off';
                    app.FixedInternalStdConc_EditFieldLabel.Visible = 'off';
                    
                    app.UITable.Visible = 'on';
                    app.DisplayValue.Visible = 'on';
                    app.ButtonTarget.Visible = 'on';
                    
                    %keyboard
                    
            end
            
        end
        
        function ROI_Value_Extractor(app, ~)
            
            ImageData = PlotMap_ExtractPlottedImage(app);
            mask = createMask(app.ROI_sampling);
            Ind = find(mask);
            app.DisplayValue.Value = mean(ImageData(Ind));
            
        end
        
        function ImageData = PlotMap_ExtractPlottedImage(app)
            lesInd = app.Plot1.Children;
            for i=1:numel(lesInd)
                if isequal(lesInd(i).Type,'image')
                    ImageData = lesInd(i).CData;
                    break
                else
                    ImageData = [];
                end
            end
        end
        
        
        function UpdateTableElementStandard(app)
            
            TableEl = {};
            
            for i = 1:length(app.CalibrationOptions.ElNames)
                TableEl{i,1} = ['#',num2str(i)];
                TableEl{i,2} = app.CalibrationOptions.ElNames{i};
                TableEl{i,3} = app.CalibrationOptions.StdOptions{app.CalibrationOptions.SelStd(i)};
                TableEl{i,4} = app.CalibrationOptions.SweepTime(i);
            end
            
            app.UITable_ElemList.Data = TableEl;
            app.UITable_ElemList.ColumnFormat = {'char','char',app.CalibrationOptions.StdOptions,'numeric'};
            app.UITable_ElemList.ColumnEditable = [false,false,true,true];
            app.UITable_ElemList.ColumnName = {'Idx','Element','Primary Standard','Sweep time (ms)'};
            app.UITable_ElemList.ColumnWidth = {'1x','1x','3x','2x'};
            
            s = uistyle('BackgroundColor','white');
            addStyle(app.UITable_ElemList,s);
            
            s2 = uistyle('BackgroundColor',[0.6863,0.8549,1]);
            addStyle(app.UITable_ElemList,s2,'row',app.InternalElementStdDropDown.Value);
            
            WePlot = 0;
            if isequal(app.ConstantcompositionCheckBox.Value,1)
                RefConc = app.FixedInternalStdConc_EditField.Value;
                if ~isequal(RefConc,0)
                    WePlot = 1;
                end
            end
            if isequal(app.VariablecompositionCheckBox.Value,1)
                if sum(app.PointsValid) >= 2
                    WePlot = 1;
                end
            end
            
            if WePlot
                CalibrateMaps(app);
                PlotAllMaps(app);
            end
            
            
        end
        
        function Overwrite_Import_DT(app)
            fid = fopen('Import_DT.txt','w');
            fprintf(fid,'%s\n','##########################################################################');
            fprintf(fid,'%s\n','#     XMapTools Last Import Settings for DT & STANDARDS (DO NOT EDIT)    #');
            fprintf(fid,'%s\n','##########################################################################');
            fprintf(fid,'%s\n','...');
            for i = 1:length(app.CalibrationOptions.ElNames)
                fprintf(fid,'%s\t%s\t%f\n',app.CalibrationOptions.ElNames{i},app.CalibrationOptions.StdOptions{app.CalibrationOptions.SelStd(i)},app.CalibrationOptions.SweepTime(i));
            end
            fclose(fid);
            %disp('Import_DT.txt saved!')
        end
        
        function DeactivatePlotZoomPanOptions(app)
            
            if ~isempty(app.Plot1)
                
                if ~isempty(app.Plot1.Toolbar.Children)
                    app.Plot1.Toolbar.Children(1).Value = 'off';
                    matlab.graphics.interaction.webmodes.toggleMode(app.Plot1,'pan',app.Plot1.Toolbar.Children(1).Value)
                    
                    app.Plot1.Toolbar.Children(2).Value = 'off';
                    matlab.graphics.interaction.webmodes.toggleMode(app.Plot1,'zoom',app.Plot1.Toolbar.Children(2).Value)
                    
                    app.Plot1.Toolbar.Children(3).Value = 'off';
                    matlab.graphics.interaction.webmodes.toggleMode(app.Plot1,'zoomout',app.Plot1.Toolbar.Children(3).Value)
                end
            end
        end
        
        function CheckLogScale(app)
            %log
            if app.PRIP_LogScale.Value
                app.PRIP_Plot.ColorScale = 'log';
                app.PRIP_AI_Plot.ColorScale = 'log';
            else
                app.PRIP_Plot.ColorScale = 'linear';
                app.PRIP_AI_Plot.ColorScale = 'linear';
            end
        end
        
        function CheckLimits_PRIP(app)
            if app.PRIP_Limits_Max.Value > app.PRIP_Limits_Min.Value
                caxis(app.PRIP_Plot,[app.PRIP_Limits_Min.Value,app.PRIP_Limits_Max.Value])
            end
        end
        
        function eliminate_ROI(app)
            delete(findall(app.PRIP_Plot, 'Type',  'images.roi.Circle'));
            delete(findall(app.PRIP_Plot, 'Type',  'images.roi.Rectangle'));
            delete(findall(app.PRIP_Plot, 'Type',  'images.roi.Polygon'));
        end
        
        function PRIP_ROIs_DataExtraction_FIRST(app, SelROI)
            
            Mask = createMask(app.PRIP_ROIs.ROI(SelROI).ROI,app.PRIP_Plot.Children(end));
            Ind = find(Mask);
            
            % Check that we are on the phase?
            Type = app.PRIP_ROIs.ROI(SelROI).ROI.Type;
            
            switch Type
                case 'images.roi.circle'
                    app.PRIP_ROIs_Data(SelROI).Type = 'Circle';
                case 'images.roi.rectangle'
                    app.PRIP_ROIs_Data(SelROI).Type = 'Rectangle';
                case 'images.roi.polygon'
                    app.PRIP_ROIs_Data(SelROI).Type = 'Polygon';
            end
            
            [X_grid,Y_grid] = meshgrid([1:size(Mask,2)],[1:size(Mask,1)]);
            
            app.PRIP_ROIs_Data(SelROI).PxIdx = Ind;
            app.PRIP_ROIs_Data(SelROI).PxCoordXY = [X_grid(Ind),Y_grid(Ind)];
            app.PRIP_ROIs_Data(SelROI).NbIdx = length(Ind);
            app.PRIP_ROIs_Data(SelROI).Mask = Mask;
            
        end
        
        function PRIP_ROIs_DataExtraction(app, ~)
            
            SelROI = app.PRIP_Table_ROI.Selection;
            
            %for i = 1:length(app.PRIP_ROIs.ROI)
            
            Data4Table = app.PRIP_Table_ROI.Data;
            
            Mask = createMask(app.PRIP_ROIs.ROI(SelROI).ROI,app.PRIP_Plot.Children(end));
            Ind = find(Mask);
            
            % Update the number of pixels
            app.PRIP_ROIs_Data(SelROI).NbIdx = length(Ind);
            Data4Table{SelROI,3} = app.PRIP_ROIs_Data(SelROI).NbIdx;
            app.PRIP_Table_ROI.Data = Data4Table;
            
            % Check that we are on the phase?
            Type = app.PRIP_ROIs.ROI(SelROI).ROI.Type;
            
            switch Type
                case 'images.roi.circle'
                    app.PRIP_ROIs_Data(SelROI).Type = 'Circle';
                    
                    app.PRIP_ROIs.ROI_Data(SelROI).Center = app.PRIP_ROIs.ROI(SelROI).ROI.Center;
                    app.PRIP_ROIs.ROI_Data(SelROI).Radius = app.PRIP_ROIs.ROI(SelROI).ROI.Radius;
                    
                case 'images.roi.rectangle'
                    app.PRIP_ROIs_Data(SelROI).Type = 'Rectangle';
                    
                    app.PRIP_ROIs.ROI_Data(SelROI).Position = app.PRIP_ROIs.ROI(SelROI).ROI.Position;
                    
                case 'images.roi.polygon'
                    app.PRIP_ROIs_Data(SelROI).Type = 'Polygon';
                    
                    app.PRIP_ROIs.ROI_Data(SelROI).Position = app.PRIP_ROIs.ROI(SelROI).ROI.Position;
            end
            
            [X_grid,Y_grid] = meshgrid([1:size(Mask,2)],[1:size(Mask,1)]);
            
            % figure, imagesc(Mask), hold on, plot(X_grid(Ind),Y_grid(Ind),'or'), axis image
            
            app.PRIP_ROIs_Data(SelROI).PxIdx = Ind;
            app.PRIP_ROIs_Data(SelROI).PxCoordXY = [X_grid(Ind),Y_grid(Ind)];
            app.PRIP_ROIs_Data(SelROI).NbIdx = length(Ind);
            app.PRIP_ROIs_Data(SelROI).Mask = Mask;
            
            %end
            
            CalculateCalibrationROIs(app);
            
            PRIP_Table_ROICellSelection(app);
            
            % PRIP_UpdateTableROI(app)
        end
        
        function PRIP_UpdateTableROI(app)
            
            Data4Table = {};
            
            for i = 1:length(app.PRIP_ROIs.ROI)
                Data4Table{i,1} = ['ROI_',num2str(i)];
                Data4Table{i,2} = app.PRIP_ROIs_Data(i).Type;
                Data4Table{i,3} = app.PRIP_ROIs_Data(i).NbIdx;
            end
            
            app.PRIP_Table_ROI.Data = Data4Table;
            app.PRIP_Table_ROI.Selection = i;
            
            if i > 0
                app.PRIP_Export.Enable = 'on';
            end
            
            %             CalculateCalibrationROIs(app);
            %
            %             app.UpdatePlotROI = 0;
            %             PRIP_Table_ROICellSelection(app);
            %             app.UpdatePlotROI = 1;
        end
        
        function CalculateCalibrationROIs(app)
            
            % This is the Casis version for ROIs. If this function is
            % changed the next function for GRIDs should also be adjusted!
            
            for iROI = 1:length(app.PRIP_ROIs_Data)
                for iStd = 1:length(app.MapStandards)
                    
                    PxIdxROI_XMap = app.PRIP_ROIs_Data(iROI).PxIdx;                       % This doesn't work for PxDataRaw, but I don't understand why!
                    
                    PxCoordXY = app.PRIP_ROIs_Data(iROI).PxCoordXY;
                    PxCoordXY_data = app.PxDataRaw.PixelCoordXY;
                    
                    PxIdxROI = find(ismember(PxCoordXY_data,PxCoordXY,'rows'));     % This works
                    
                    %figure, imagesc(app.PRIP_ROIs_Data(iROI).Mask), hold on, plot(app.PRIP_ROIs_Data(iROI).PxCoordXY(:,1),app.PRIP_ROIs_Data(iROI).PxCoordXY(:,2),'or'), axis image
                    
                    % Calculate It_Unk for the ROI
                    DataSweeps = zeros(1,length(app.PxDataRaw.ElNames));
                    Pos = 1;
                    
                    for iPx = 1:length(PxIdxROI)
                        if iPx > 1
                            Pos = size(DataSweeps,1)+1;
                        end
                        for iElem = 1:length(app.PxDataRaw.ElNames)
                            IntData = app.PxDataRaw.ElData(iElem).PxData(PxIdxROI(iPx)).Intensity;
                            DataSweeps(Pos:Pos+length(IntData)-1,iElem) = IntData;
                        end
                    end
                    
                    IdxNaN = find(isnan(DataSweeps));
                    DataSweeps(IdxNaN) = 0;
                    
                    MeanDataSweeps = mean(DataSweeps,1); %
                    
                    for i = 1:size(DataSweeps,2)
                        IdxPos = find(DataSweeps(:,i) > 0);
                        MeanDataSweeps_ExclZeros(i) = mean(DataSweeps(IdxPos,i));
                    end
                    
                    IntStd_Id = app.InternalElementStdDropDown.Value;
                    
                    It_Std = mean(app.MapStandards(iStd).Data(IntStd_Id).Cps(PxIdxROI_XMap));
                    Con_Std = mean(app.MapStandards(iStd).Data(IntStd_Id).Conc(PxIdxROI_XMap));
                    
                    It_Unk = MeanDataSweeps(IntStd_Id);
                    Con_Unk = app.FixedInternalStdConc_EditField.Value;
                    
                    k_mtx(iStd) = (It_Unk.*Con_Std)./(Con_Unk.*It_Std);
                    
                    
                end
                
                Conc = zeros(length(app.PxDataRaw.ElNames),1);
                ConcMeanPixel = zeros(length(app.PxDataRaw.ElNames),1);
                LOD_Px = zeros(length(app.PxDataRaw.ElNames),1);
                ConcStdPixel = zeros(length(app.PxDataRaw.ElNames),1);
                LOD_ROI = zeros(length(app.PxDataRaw.ElNames),1);
                
                for i = 1:length(app.PxDataRaw.ElNames)
                    
                    SelStd = app.CalibrationOptions.SelStd(i);
                    
                    It_Unk = MeanDataSweeps(i);
                    It_Std = mean(app.MapStandards(SelStd).Data(i).Cps(PxIdxROI_XMap));
                    Con_Std = mean(app.MapStandards(SelStd).Data(i).Conc(PxIdxROI_XMap));
                    
                    Conc(i) = (It_Unk.*Con_Std)./(k_mtx(SelStd).*It_Std);
                    
                    CalibratedPx = app.MapCpsData(i).Conc(find(app.PRIP_ROIs_Data(iROI).Mask));
                    ConcMeanPixel(i) = median(CalibratedPx);
                    ConcStdPixel(i) = mad(CalibratedPx,1);
                    
                    PxLOD = app.MapCpsData(i).LOD(find(app.PRIP_ROIs_Data(iROI).Mask));
                    LOD_Px(i) = median(PxLOD);
                    
                    
                    % Limit of detection (Theoule-sur-Mer)
                    Si = k_mtx(iStd).*It_Std./Con_Std;                                                   % cps/µg.g-1        probably ok
                    Bi = mean(app.MapStandards(iStd).Data(i).Int_Back(PxIdxROI_XMap));                   % cps               mean of background int (sum would be too much)
                    DTi = app.CalibrationOptions.SweepTime(i)/1e3 * length(size(DataSweeps,1));          % seconds           probably ok (* nb_sweeps)
                    NbAnal = sum(app.MapStandards(SelStd).Data(i).Sweeps_Pixel(PxIdxROI_XMap));          % dimensionless     nb_sweeps
                    NbBack = sum(app.MapStandards(SelStd).Data(i).Sweeps_Back(PxIdxROI_XMap));           % dimensionless     wrong, too high!
                    
                    if Bi > 0
                        LOD_ROI(i) = (3.29.*sqrt(Bi.*DTi.*NbAnal.*(1+NbAnal./NbBack)+2.71))./(NbAnal.*DTi.*Si);    % µg.g-1
                    end
                    % including filter LOD for values with zero intensities for background (Cavalaire 2023)
                    
                end
                
                app.PRIP_ROIs_Data(iROI).Conc = Conc;
                app.PRIP_ROIs_Data(iROI).LOD_ROI = LOD_ROI;
                app.PRIP_ROIs_Data(iROI).ConcMeanPixel = ConcMeanPixel;
                app.PRIP_ROIs_Data(iROI).ConcStdPixel = ConcStdPixel;
                app.PRIP_ROIs_Data(iROI).LOD_Px = LOD_Px;
            end
        end
        
        function CalculateCalibrationGRIDs(app)
            
            % Warning this function is similar to the function CalculateCalibrationROIs
            % but was kept separate for PRIP-AI to avoid breaking the Casis version of PRIP.
            
            % Sainte-Maxime, 08.02.2025
            
            for iGrid = 1:length(app.PRIP_AI_GridData.Grid)
                
                for iROI = 1:length(app.PRIP_AI_GridData.Grid(iGrid).Data)
                    
                    for iStd = 1:length(app.MapStandards)
                        
                        PxIdxROI_XMap = app.PRIP_AI_GridData.Grid(iGrid).Data(iROI).PxIdx;
                        
                        PxCoordXY = app.PRIP_AI_GridData.Grid(iGrid).Data(iROI).PxCoordXY;
                        PxCoordXY_data = app.PxDataRaw.PixelCoordXY;
                        
                        PxIdxROI = find(ismember(PxCoordXY_data,PxCoordXY,'rows'));     % This works
                        
                        % Calculate It_Unk for the ROI
                        DataSweeps = zeros(1,length(app.PxDataRaw.ElNames));
                        Pos = 1;
                        
                        for iPx = 1:length(PxIdxROI)
                            if iPx > 1
                                Pos = size(DataSweeps,1)+1;
                            end
                            for iElem = 1:length(app.PxDataRaw.ElNames)
                                IntData = app.PxDataRaw.ElData(iElem).PxData(PxIdxROI(iPx)).Intensity;
                                DataSweeps(Pos:Pos+length(IntData)-1,iElem) = IntData;
                            end
                        end
                        
                        IdxNaN = find(isnan(DataSweeps));
                        DataSweeps(IdxNaN) = 0;
                        
                        MeanDataSweeps = mean(DataSweeps,1);
                        
                        for i = 1:size(DataSweeps,2)
                            IdxPos = find(DataSweeps(:,i) > 0);
                            MeanDataSweeps_ExclZeros(i) = mean(DataSweeps(IdxPos,i));   % unused as in the other function
                        end
                        
                        IntStd_Id = app.InternalElementStdDropDown.Value;
                        
                        It_Std = mean(app.MapStandards(iStd).Data(IntStd_Id).Cps(PxIdxROI_XMap));
                        Con_Std = mean(app.MapStandards(iStd).Data(IntStd_Id).Conc(PxIdxROI_XMap));
                        
                        It_Unk = MeanDataSweeps(IntStd_Id);
                        Con_Unk = app.FixedInternalStdConc_EditField.Value;
                        
                        k_mtx(iStd) = (It_Unk.*Con_Std)./(Con_Unk.*It_Std);
                        
                        % Save single pixels composition filted by LOD (for
                        % training data later):
                        
                        % (El,Px) – same format as the NN input
                        for i = 1:length(app.MapCpsData)
                            app.PRIP_AI_GridData.Grid(iGrid).Data(iROI).ConcAllPx(:,i) = app.MapCpsData(i).Conc(PxIdxROI_XMap);
                            app.PRIP_AI_GridData.Grid(iGrid).Data(iROI).LODAllPx(:,i) = app.MapCpsData(i).LOD(PxIdxROI_XMap);
                        end
                        
                    end
                    
                    Conc = zeros(length(app.PxDataRaw.ElNames),1);
                    ConcMeanPixel = zeros(length(app.PxDataRaw.ElNames),1);
                    LOD_Px = zeros(length(app.PxDataRaw.ElNames),1);
                    ConcStdPixel = zeros(length(app.PxDataRaw.ElNames),1);
                    LOD_ROI = zeros(length(app.PxDataRaw.ElNames),1);
                    
                    for i = 1:length(app.PxDataRaw.ElNames)
                        
                        SelStd = app.CalibrationOptions.SelStd(i);
                        
                        It_Unk = MeanDataSweeps(i);
                        It_Std = mean(app.MapStandards(SelStd).Data(i).Cps(PxIdxROI_XMap));
                        Con_Std = mean(app.MapStandards(SelStd).Data(i).Conc(PxIdxROI_XMap));
                        
                        Conc(i) = (It_Unk.*Con_Std)./(k_mtx(SelStd).*It_Std);
                        
                        CalibratedPx = app.MapCpsData(i).Conc(find(app.PRIP_AI_GridData.Grid(iGrid).Data(iROI).Mask));
                        ConcMeanPixel(i) = median(CalibratedPx);
                        ConcStdPixel(i) = mad(CalibratedPx,1);
                        
                        PxLOD = app.MapCpsData(i).LOD(find(app.PRIP_AI_GridData.Grid(iGrid).Data(iROI).Mask));
                        LOD_Px(i) = median(PxLOD);
                        
                        
                        % Limit of detection (Theoule-sur-Mer)
                        Si = k_mtx(iStd).*It_Std./Con_Std;                                                   % cps/µg.g-1        probably ok
                        Bi = mean(app.MapStandards(iStd).Data(i).Int_Back(PxIdxROI_XMap));                   % cps               mean of background int (sum would be too much)
                        DTi = app.CalibrationOptions.SweepTime(i)/1e3 * length(size(DataSweeps,1));          % seconds           probably ok (* nb_sweeps)
                        NbAnal = sum(app.MapStandards(SelStd).Data(i).Sweeps_Pixel(PxIdxROI_XMap));          % dimensionless     nb_sweeps
                        NbBack = sum(app.MapStandards(SelStd).Data(i).Sweeps_Back(PxIdxROI_XMap));           % dimensionless     wrong, too high!
                        
                        if Bi > 0
                            LOD_ROI(i) = (3.29.*sqrt(Bi.*DTi.*NbAnal.*(1+NbAnal./NbBack)+2.71))./(NbAnal.*DTi.*Si);    % µg.g-1
                        end
                        
                    end
                    
                    app.PRIP_AI_GridData.Grid(iGrid).Data(iROI).Conc = Conc;
                    app.PRIP_AI_GridData.Grid(iGrid).Data(iROI).LOD_ROI = LOD_ROI;
                    app.PRIP_AI_GridData.Grid(iGrid).Data(iROI).ConcMeanPixel = ConcMeanPixel;
                    app.PRIP_AI_GridData.Grid(iGrid).Data(iROI).ConcStdPixel = ConcStdPixel;
                    app.PRIP_AI_GridData.Grid(iGrid).Data(iROI).LOD_Px = LOD_Px;
                    
                end
                
            end
            
        end
        
        function Count = CountROI(app)
            
            Count = sum(length(findall(app.PRIP_Plot, 'Type',  'images.roi.Circle'))+length(findall(app.PRIP_Plot, 'Type',  'images.roi.Rectangle'))+length(findall(app.PRIP_Plot, 'Type',  'images.roi.Polygon')));
            
        end
        
        function FillTableForPRIP_AI_Step1(app)
            
            ElNames = app.InternalElementStdDropDown.Items;
            
            for i = 1:length(ElNames)
                Map = app.MapCpsData(i).Conc;
                MapLOD = app.MapCpsData(i).LOD;
                
                NbPx(i) = numel(Map);
                Nb_BDL(i) = numel(find(Map(:) <= MapLOD(:)));
            end
            
            Per_BDL = Nb_BDL./NbPx*100;
            
            Cell4Table = cell(numel(ElNames),2);
            
            Cell4Table(:,1) = ElNames;
            Cell4Table(:,2) = num2cell(Nb_BDL);
            
            app.ElemPxPrec_UITable.Data = Cell4Table;
            app.ElemPxPrec_UITable.ColumnName = {'El','Px<=LOD'};
            
            app.PRIP_AI_SelectedPredictors = zeros(size(ElNames));
            
            for i = 1:length(ElNames)
                if isequal(Nb_BDL(i),0)
                    app.PRIP_AI_SelectedPredictors(i) = 1;
                end
            end
            
            update_color_uitable(app);
            
        end
        
        function FillTableForPRIP_AI_Step2(app)
            
            ElNames = app.InternalElementStdDropDown.Items;
            
            Cell4Table = app.ElemPxPrec_UITable.Data;
            
            Cell4Table = Cell4Table(:,1:2); % eliminate the extra entries
            
            Nb_BDL = [];
            
            for iGrid = 1:length(app.PRIP_AI_GridData.Grid)
                
                for iEl = 1:length(ElNames)
                    NbROIs = length(app.PRIP_AI_GridData.Grid(iGrid).Data);
                    
                    Ok = 0;
                    Bad = 0;
                    for iROI = 1:NbROIs
                        if app.PRIP_AI_GridData.Grid(iGrid).Data(iROI).Conc(iEl) > app.PRIP_AI_GridData.Grid(iGrid).Data(iROI).LOD_ROI(iEl)
                            Ok = Ok + 1;
                        else
                            Bad = Bad + 1;
                        end
                    end
                    Nb_BDL(iEl,iGrid) = Bad;
                end
            end
            
            Cell4Table(:,3:size(Nb_BDL,2)+2) = num2cell(Nb_BDL);
            
            ColumnName = app.ElemPxPrec_UITable.ColumnName(1:2);
            for i = 1:length(app.PRIP_AI_GridData.Grid)
                ColumnName{end+1} = [num2str(app.PRIP_AI_GridData.Res(i)),'x',num2str(app.PRIP_AI_GridData.Res(i))];
            end
            
            app.ElemPxPrec_UITable.Data = Cell4Table;
            app.ElemPxPrec_UITable.ColumnName = ColumnName;
            
            app.GridResolutionDropDown.Items = ColumnName(3:end);
            app.GridResolutionDropDown.ItemsData = [1:length(ColumnName(3:end))];
            
        end
        
        function CalculateNumberGrids(app)
            app.PRIP_AI_Grid_Nb.Value = numel([app.PRIP_AI_Grid_Min.Value:app.PRIP_AI_Grid_Step.Value:app.PRIP_AI_Grid_Max.Value]);
        end
        
        function update_color_uitable(app)
            for i = 1:length(app.PRIP_AI_SelectedPredictors)
                if app.PRIP_AI_SelectedPredictors(i)
                    s = uistyle('FontColor',[0,0,1],'FontWeight','bold');
                    addStyle(app.ElemPxPrec_UITable,s,'cell',[i,1]);
                else
                    s = uistyle('FontColor',[1,0,1],'FontWeight','bold');
                    addStyle(app.ElemPxPrec_UITable,s,'cell',[i,1]);
                end
            end
        end
        
        function GenerateCellFromSelectedData(app)
            
            app.GeneratedLabelCellFormat = app.InternalElementStdDropDown.Items; 
            
            Conc = cell(length(app.PRIP_ROIs_Data), length(app.GeneratedLabelCellFormat));
            LOD =  cell(length(app.PRIP_ROIs_Data), length(app.GeneratedLabelCellFormat));
            
            for i = 1:length(app.PRIP_ROIs_Data)
                
                Conc(i,:) = num2cell(app.PRIP_ROIs_Data(i).Conc');
                LOD(i,:) = num2cell(app.PRIP_ROIs_Data(i).LOD_ROI');
            end
            
            app.GeneratedDataCellFormat = Conc;
            app.GeneratedLODCellFormat = LOD;
            
        end
    end
    

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, XMapToolsApp, SelectedMaskFile)
            
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
            
            app.XMapToolsCalibrationLaICPMS.Visible = 'off';
            
            movegui(app.XMapToolsCalibrationLaICPMS,'center');
            
            app.PRIP_GridLayout.Visible = 'off';
            
            app.XMapToolsApp = XMapToolsApp;
            
            % Take data that were previously loaded (otherwise empty):
            app.PxDataRaw = app.XMapToolsApp.XMapToolsData.PxDataRaw;
            
            app.MaskFile.Name = app.XMapToolsApp.XMapToolsData.MapData.MaskFile.Names{SelectedMaskFile};
            app.MaskFile.NbMasks = app.XMapToolsApp.XMapToolsData.MapData.MaskFile.NbMasks(SelectedMaskFile);
            app.MaskFile.Names = app.XMapToolsApp.XMapToolsData.MapData.MaskFile.Masks(SelectedMaskFile).Names;
            app.MaskFile.MaskMap = app.XMapToolsApp.XMapToolsData.MapData.MaskFile.Masks(SelectedMaskFile).MaskMap;
            
            app.ClassDropDown.Items = app.MaskFile.Names(2:end);
            app.ClassDropDown.ItemsData = 1:app.MaskFile.NbMasks;
            
            for iStd = 1:length(app.XMapToolsApp.XMapToolsData.MapStandards)
                
                % Standard data
                Names = app.XMapToolsApp.XMapToolsData.MapStandards(iStd).Names;
                Types = app.XMapToolsApp.XMapToolsData.MapStandards(iStd).Types;
                
                app.ElNamesStd = Names(find(Types == 1));
                app.NbElStd = length(app.ElNamesStd);
                
                % Data types:
                %       - Type 1        standard maps (Std_Map_Cps)
                %       - Type 2        composition of the standard (Std_Map_Conc)
                %       - Type 3        intensity of background (Int_Back)
                %       - Type 4        number of sweeps for background measurement (Sweeps_Back)
                %       - Type 5        number of sweeps for pixel measurement (Sweeps_Pixel)
                
                for i = 1:app.NbElStd
                    app.MapStandards(iStd).Data(i).Cps = app.XMapToolsApp.XMapToolsData.MapStandards(iStd).Data(i).Map;
                    app.MapStandards(iStd).Data(i).Int_Back = app.XMapToolsApp.XMapToolsData.MapStandards(iStd).Data(app.NbElStd+i).Map;
                    app.MapStandards(iStd).Data(i).Conc = app.XMapToolsApp.XMapToolsData.MapStandards(iStd).Data(app.NbElStd*2+i).Map;
                    app.MapStandards(iStd).Data(i).Sweeps_Back = app.XMapToolsApp.XMapToolsData.MapStandards(iStd).Data(app.NbElStd*3+i).Map;
                    app.MapStandards(iStd).Data(i).Sweeps_Pixel = app.XMapToolsApp.XMapToolsData.MapStandards(iStd).Data(app.NbElStd*4+i).Map;
                end
                
                StdOptions{iStd} = app.XMapToolsApp.XMapToolsData.MapStandards(iStd).StandardName;
                
            end
            
            app.Standard_DropDown.Items = StdOptions;
            app.Standard_DropDown.ItemsData = [1:length(StdOptions)];
            
            app.InternalElementStdDropDown.Items = app.ElNamesStd;
            app.InternalElementStdDropDown.ItemsData = 1:app.NbElStd;
            
            WhereSi = find(ismember(app.ElNamesStd,'Si'));
            if ~isempty(WhereSi)
                app.InternalElementStdDropDown.Value = WhereSi;
            end
            
            app.ElementDropDown.Items = app.ElNamesStd;
            app.ElementDropDown.ItemsData = 1:app.NbElStd;
            
            % MapData
            ElNamesCps = app.XMapToolsApp.XMapToolsData.MapData.It.Names;
            [ElIsInStd,ElIndex] = ismember(app.ElNamesStd,ElNamesCps);
            
            if ~isequal(sum(ElIsInStd),length(ElIsInStd))
                
                Where0 = find(ElIsInStd == 0);
                Str = 'Following map(s) not available:';
                for i = 1:length(Where0)
                    Str = [Str,' ',char(app.ElNamesStd(Where0(i)))];
                end
                
                errordlg({'Mismatch between intensity maps and maps of standards',Str},'Error')
                close(app.XMapToolsCalibrationLaICPMS)
                return
            end
            
            for i = 1:app.NbElStd
                app.MapCpsData(i).Cps = app.XMapToolsApp.XMapToolsData.MapData.It.Data(ElIndex(i)).Map;
                app.MapCpsData(i).Conc = zeros(size(app.MapCpsData(i).Cps));
                app.MapCpsData(i).LOD = zeros(size(app.MapCpsData(i).Cps));
            end
            
            app.UITable.Data = {0,0,0,0,0,0;0,0,0,0,0,0};
            
            app.ConstantcompositionCheckBox.Value = 1;
            app.UITable.Visible = 'Off';
            app.DisplayValue.Visible = 'off';
            app.ButtonTarget.Visible = 'off';
            app.PRIP_Export.Enable = 'Off';
            
            app.wtOxideDropDown.Items = app.XMapToolsApp.ElOxDataDef.OxList;
            app.wtOxideDropDown.ItemsData = 1:length(app.wtOxideDropDown.Items);
            CalculateConversion(app);
            
            app.CalibrationOptions.ElNames = app.InternalElementStdDropDown.Items;
            app.CalibrationOptions.StdOptions = StdOptions;
            app.CalibrationOptions.SelStd = ones(size(ElNamesCps));
            app.CalibrationOptions.SweepTime = 10*ones(size(ElNamesCps));
            
            if exist('Import_DT.txt')
                EN = {};
                SN = {};
                DT = [];
                
                fid = fopen('Import_DT.txt','r');
                TheL = fgetl(fid); TheL = fgetl(fid); TheL = fgetl(fid); TheL = fgetl(fid);
                while 1
                    TheL = fgetl(fid);
                    if isequal(TheL,-1)
                        break
                    end
                    TheStr = textscan(TheL,'%s','Delimiter','\t');
                    TheStr = TheStr{1};
                    if isequal(length(TheStr),3)
                        EN{end+1} = TheStr{1};
                        SN{end+1} = TheStr{2};
                        DT(end+1) = str2num(TheStr{3});
                    end
                end
                
                fclose(fid);
                
                
                for i = 1:length(app.InternalElementStdDropDown.Items)
                    Idx = find(ismember(EN,app.InternalElementStdDropDown.Items{i}));
                    if ~isempty(Idx)
                        WhereStd = find(ismember(StdOptions,SN{i}));
                        if ~isempty(WhereStd)
                            app.CalibrationOptions.SelStd(i) = WhereStd;
                        end
                        app.CalibrationOptions.SweepTime(i) = DT(i);
                    end
                end
            end
            
            CalculateNumberGrids(app)
            
            app.XMapToolsCalibrationLaICPMS.Visible = 'on';
            
            app.WaitBar = uiprogressdlg(app.XMapToolsCalibrationLaICPMS,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Updating the interface';
            
            UpdateTableElementStandard(app)
            
            PlotAllMaps(app);
            
            app.PRIP_Activated = 0;
            app.PRIP_ROIs.Nb = 0;
            app.PRIP_Table_ROI.SelectionType = 'row';
            
            app.SetasPredictorButton.Enable = 'off';
            app.SetasOutputButton.Enable = 'off';
            
            app.GridResolutionDropDown.Visible = 'off';
            app.TrainSNNButton.Visible = 'off';
            app.PRIP_AI_Plot.Visible = 'off';
            app.PRIP_AI_MapMenu.Visible = 'off';
            app.ParametersPanel.Visible = 'off';
            
            app.PRIPAI_GridLayout10.Visible = 'off';
            
            InternalElementStdDropDownValueChanged(app);
            
            close(app.WaitBar)
        end

        % Value changed function: InternalElementStdDropDown
        function InternalElementStdDropDownValueChanged(app, event)
            app.WaitBar = uiprogressdlg(app.XMapToolsCalibrationLaICPMS,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Updating the interface';
            
            app.ElementDropDown.Value = app.InternalElementStdDropDown.Value;
            
            % Update oxides
            ElList_DB = app.XMapToolsApp.ElOxDataDef.ElList;
            [Ok,ElIdx] = ismember(char(app.ElementDropDown.Items(app.ElementDropDown.Value)),ElList_DB);
            OxId = find(app.XMapToolsApp.ElOxDataDef.OxElIdx == ElIdx);
            if ~isempty(OxId)
                app.wtOxideDropDown.Value = OxId(1);
            end
            
            CalculateConversion(app);
            
            UpdateTableElementStandard(app);
            
            CalibrateMaps(app);
            PlotAllMaps(app);
            
            close(app.WaitBar);
        end

        % Value changed function: ClassDropDown
        function ClassDropDownValueChanged(app, event)
            delete(findall(app.Plot1, 'Type',  'images.roi.Circle'));
            CalibrateMaps(app);
            PlotAllMaps(app);
        end

        % Value changed function: ElementDropDown
        function ElementDropDownValueChanged(app, event)
            delete(findall(app.Plot1, 'Type',  'images.roi.Circle'));
            PlotAllMaps(app);
        end

        % Value changed function: FixedInternalStdConc_EditField
        function FixedInternalStdConc_EditFieldValueChanged(app, event)
            
            app.WaitBar = uiprogressdlg(app.XMapToolsCalibrationLaICPMS,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'The calibrated maps will be available very soon, so please hang on!';
            
            CalibrateMaps(app);
            PlotAllMaps(app);
            if isequal(app.PRIP_Activated,0)
                
                % Module initialization (PRIP)
                app.PRIP_MapMenu.Items = app.ElementDropDown.Items;
                app.PRIP_MapMenu.ItemsData = app.ElementDropDown.ItemsData;
                
                Plot_PRIP(app)
                
                app.PRIP_Activated = 1;
                
                % Module initialization (PRIP-AI)
                FillTableForPRIP_AI_Step1(app);
                
                app.PRIP_AI_MapMenu.Items = app.ElementDropDown.Items;
                app.PRIP_AI_MapMenu.ItemsData = app.ElementDropDown.ItemsData;
                
                
                if ~isempty(app.PxDataRaw.ElNames)
                    app.PRIP_GridLayout.Visible = 'on';
                end
                
                close(app.WaitBar);
            end
        end

        % Value changed function: wtOxideDropDown
        function wtOxideDropDownValueChanged(app, event)
            CalculateConversion(app);
        end

        % Value changed function: Oxide_EditField
        function Oxide_EditFieldValueChanged(app, event)
            CalculateConversion(app);
            if app.ConstantcompositionCheckBox.Value
                app.FixedInternalStdConc_EditField.Value = app.ppm_EditField.Value;
                FixedInternalStdConc_EditFieldValueChanged(app, event)
            end
        end

        % Button pushed function: AppyCalibration
        function AppyCalibrationButtonPushed(app, event)
            
            Pos = length(app.XMapToolsApp.XMapToolsData.MapData.Qt.Names) + 1;
            
            % ------------------------------------------------------------
            % Save maps filtered for LOD:
            app.XMapToolsApp.XMapToolsData.MapData.Qt.Names{Pos} = [char(app.ClassDropDown.Items{app.ClassDropDown.Value}),' µg/g'];
            app.XMapToolsApp.XMapToolsData.MapData.Qt.IsOxide(Pos) = 0;
            app.XMapToolsApp.XMapToolsData.MapData.Qt.MaskFile{Pos} = app.MaskFile.Name;
            app.XMapToolsApp.XMapToolsData.MapData.Qt.NbCalibPoints(Pos) = 0;
            
            p = uitreenode(app.XMapToolsApp.Node_Qt,'Text',char(app.XMapToolsApp.XMapToolsData.MapData.Qt.Names{Pos}),'NodeData',[2,Pos,0]);
            
            for i = 1:length(app.ElNamesStd)
                app.XMapToolsApp.XMapToolsData.MapData.Qt.Data(Pos).ElNames{i} = app.ElNamesStd{i};
                app.XMapToolsApp.XMapToolsData.MapData.Qt.Data(Pos).ElInd(i) = find(ismember(app.XMapToolsApp.ElOxDataDef.ElList,app.ElNamesStd{i}));
                
                MAP = app.MapCpsData(i).Conc;
                LOD = app.MapCpsData(i).LOD;
                
                IdxBelowLOD = find(MAP(:) <= LOD(:));
                
                MAP(IdxBelowLOD) = 0;
                
                app.XMapToolsApp.XMapToolsData.MapData.Qt.Data(Pos).CData(i).Map = MAP;
                
                p1 = uitreenode(p,'Text',char(app.XMapToolsApp.XMapToolsData.MapData.Qt.Data(Pos).ElNames{i}),'NodeData',[2,Pos,i]);
            end
            
            % ------------------------------------------------------------
            % Save maps not filtered for LOD:
            if app.SaveunfilteredmapsCheckBox.Value
                
                Pos = length(app.XMapToolsApp.XMapToolsData.MapData.Qt.Names) + 1;
                
                app.XMapToolsApp.XMapToolsData.MapData.Qt.Names{Pos} = [char(app.ClassDropDown.Items{app.ClassDropDown.Value}),' µg/g (no LOD filter)'];
                app.XMapToolsApp.XMapToolsData.MapData.Qt.IsOxide(Pos) = 0;
                app.XMapToolsApp.XMapToolsData.MapData.Qt.MaskFile{Pos} = app.MaskFile.Name;
                app.XMapToolsApp.XMapToolsData.MapData.Qt.NbCalibPoints(Pos) = 0;
                
                p = uitreenode(app.XMapToolsApp.Node_Qt,'Text',char(app.XMapToolsApp.XMapToolsData.MapData.Qt.Names{Pos}),'NodeData',[2,Pos,0]);
                
                for i = 1:length(app.ElNamesStd)
                    app.XMapToolsApp.XMapToolsData.MapData.Qt.Data(Pos).ElNames{i} = app.ElNamesStd{i};
                    app.XMapToolsApp.XMapToolsData.MapData.Qt.Data(Pos).ElInd(i) = find(ismember(app.XMapToolsApp.ElOxDataDef.ElList,app.ElNamesStd{i}));
                    app.XMapToolsApp.XMapToolsData.MapData.Qt.Data(Pos).CData(i).Map = app.MapCpsData(i).Conc;
                    
                    p1 = uitreenode(p,'Text',char(app.XMapToolsApp.XMapToolsData.MapData.Qt.Data(Pos).ElNames{i}),'NodeData',[2,Pos,i]);
                end
            end
            
            % ------------------------------------------------------------
            % Limit of detection (version 4.2)
            Pos = length(app.XMapToolsApp.XMapToolsData.MapLOD.Names);
            
            app.XMapToolsApp.XMapToolsData.MapLOD.Names{Pos+1} = [char(app.ClassDropDown.Items{app.ClassDropDown.Value}),' µg/g'];
            app.XMapToolsApp.XMapToolsData.MapLOD.Types(Pos+1) = 1;     % not used for now
            
            p = uitreenode(app.XMapToolsApp.Node_LOD,'Text',char(app.XMapToolsApp.XMapToolsData.MapLOD.Names{Pos+1}),'NodeData',[16,Pos+1,0,0]);
            
            for i = 1:length(app.ElNamesStd)
                app.XMapToolsApp.XMapToolsData.MapLOD.Data(Pos+1).ElNames{i} = app.ElNamesStd{i};
                
                p = uitreenode(app.XMapToolsApp.Node_LOD.Children(Pos+1),'Text',char(app.XMapToolsApp.XMapToolsData.MapLOD.Data(Pos+1).ElNames{i}),'NodeData',[16,Pos+1,i,0]);
                
                app.XMapToolsApp.XMapToolsData.MapLOD.Data(Pos+1).ElData(i).Label_LOD = ['LOD ',app.ElNamesStd{i},' µg/g'];
                app.XMapToolsApp.XMapToolsData.MapLOD.Data(Pos+1).ElData(i).Map_LOD = app.MapCpsData(i).LOD;
                
                p = uitreenode(app.XMapToolsApp.Node_LOD.Children(Pos+1).Children(i),'Text',char(app.XMapToolsApp.XMapToolsData.MapLOD.Data(Pos+1).ElData(i).Label_LOD),'NodeData',[16,Pos+1,i,1]);
                
                if length(app.CalibrationOptions.StdOptions) > 1
                    IdStd = app.CalibrationOptions.SelStd(i);
                    It_Std_mtx = app.MapStandards(IdStd).Data(i).Cps;
                    Con_Std_mxt = app.MapStandards(IdStd).Data(i).Conc;
                    
                    app.XMapToolsApp.XMapToolsData.MapLOD.Data(Pos+1).ElData(i).Label_Si_mtx = ['Si_mtx (',app.CalibrationOptions.StdOptions{IdStd},')'];
                    app.XMapToolsApp.XMapToolsData.MapLOD.Data(Pos+1).ElData(i).Map_Si_mtx = app.Map_k(:,:,IdStd).*It_Std_mtx./Con_Std_mxt; % cps/µg.g-1
                    
                    app.XMapToolsApp.XMapToolsData.MapLOD.Data(Pos+1).ElData(i).Label_Back_mtx = ['Back_mtx'];
                    app.XMapToolsApp.XMapToolsData.MapLOD.Data(Pos+1).ElData(i).Map_Back_mtx = app.MapStandards(IdStd).Data(i).Int_Back; % cps
                    
                    app.XMapToolsApp.XMapToolsData.MapLOD.Data(Pos+1).ElData(i).NbAnal = app.MapStandards(IdStd).Data(i).Sweeps_Pixel; % dimensionless
                    app.XMapToolsApp.XMapToolsData.MapLOD.Data(Pos+1).ElData(i).NbBack = app.MapStandards(IdStd).Data(i).Sweeps_Back; % dimensionless
                else
                    It_Std_mtx = app.MapStandards(1).Data(i).Cps;
                    Con_Std_mxt = app.MapStandards(1).Data(i).Conc;
                    
                    app.XMapToolsApp.XMapToolsData.MapLOD.Data(Pos+1).ElData(i).Label_Si_mtx = ['Si_mtx (',char(app.CalibrationOptions.StdOptions),')'];
                    app.XMapToolsApp.XMapToolsData.MapLOD.Data(Pos+1).ElData(i).Map_Si_mtx = app.Map_k.*It_Std_mtx./Con_Std_mxt; % cps/µg.g-1
                    
                    app.XMapToolsApp.XMapToolsData.MapLOD.Data(Pos+1).ElData(i).Label_Back_mtx = ['Back_mtx'];
                    app.XMapToolsApp.XMapToolsData.MapLOD.Data(Pos+1).ElData(i).Map_Back_mtx = app.MapStandards(1).Data(i).Int_Back;  % cps
                    
                    app.XMapToolsApp.XMapToolsData.MapLOD.Data(Pos+1).ElData(i).NbAnal = app.MapStandards(1).Data(i).Sweeps_Pixel; % dimensionless
                    app.XMapToolsApp.XMapToolsData.MapLOD.Data(Pos+1).ElData(i).NbBack = app.MapStandards(1).Data(i).Sweeps_Back; % dimensionless
                end
                
                p = uitreenode(app.XMapToolsApp.Node_LOD.Children(Pos+1).Children(i),'Text',char(app.XMapToolsApp.XMapToolsData.MapLOD.Data(Pos+1).ElData(i).Label_Si_mtx),'NodeData',[16,Pos+1,i,2]);
                
                p = uitreenode(app.XMapToolsApp.Node_LOD.Children(Pos+1).Children(i),'Text',char(app.XMapToolsApp.XMapToolsData.MapLOD.Data(Pos+1).ElData(i).Label_Back_mtx),'NodeData',[16,Pos+1,i,3]);
                
                app.XMapToolsApp.XMapToolsData.MapLOD.Data(Pos+1).ElData(i).DTi = app.CalibrationOptions.SweepTime(i)/1e3; % seconds
                
            end
            
            %             % Limit of detection:
            %             Pos = length(app.XMapToolsApp.XMapToolsData.MapData.Ot.Names);
            %
            %             for i = 1:length(app.ElNamesStd)
            %                 app.XMapToolsApp.XMapToolsData.MapData.Ot.Names{Pos+i} = [char(app.ElNamesStd{i}),'_',char(app.ClassDropDown.Items{app.ClassDropDown.Value}),' µg/g'];
            %                 app.XMapToolsApp.XMapToolsData.MapData.Ot.Types(Pos+i) = 1;
            %                 app.XMapToolsApp.XMapToolsData.MapData.Ot.Data(Pos+i).Map = app.MapCpsData(i).LOD;
            %
            %                 p = uitreenode(app.XMapToolsApp.Node_Ot,'Text',char(app.XMapToolsApp.XMapToolsData.MapData.Ot.Names{Pos+i}),'NodeData',[5,Pos+i]);
            %             end
            
            close(app.XMapToolsCalibrationLaICPMS);
            
        end

        % Value changed function: ConstantcompositionCheckBox
        function ConstantcompositionCheckBoxValueChanged(app, event)
            value = app.ConstantcompositionCheckBox.Value;
            if isequal(value,1) && isequal(app.VariablecompositionCheckBox.Value,1)
                % We go to this mode
                ChangeMode(app,'Fixed');
            else
                ChangeMode(app,'Variable');
            end
        end

        % Value changed function: VariablecompositionCheckBox
        function VariablecompositionCheckBoxValueChanged(app, event)
            value = app.VariablecompositionCheckBox.Value;
            if isequal(value,1) && isequal(app.ConstantcompositionCheckBox.Value,1)
                % We go to this mode
                ChangeMode(app,'Variable');
            else
                ChangeMode(app,'Fixed');
            end
        end

        % Button pushed function: ButtonTarget
        function ButtonTargetPushed(app, event)
            
            delete(findall(app.Plot1, 'Type',  'images.roi.Circle'));
            
            DeactivatePlotZoomPanOptions(app);
            
            app.ROI_sampling = drawcircle(app.Plot1,'Color',[0.47,0.67,0.19],'InteractionsAllowed','all');
            
            app.ROI_sampling_Listener = addlistener(app.ROI_sampling, 'ROIMoved', @(varargin)ROI_Value_Extractor(app, app.ROI_sampling));
            app.ROI_sampling_Listener2 = addlistener(app.ROI_sampling, 'MovingROI', @(varargin)DeactivatePlotZoomPanOptions(app));
            
            ROI_Value_Extractor(app, app.ROI_sampling);
            
        end

        % Display data changed function: UITable
        function UITableDisplayDataChanged(app, event)
            NewDisplayData = app.UITable.DisplayData;
            
            app.DataInter = cell2mat(NewDisplayData);
            
            app.NbPoints = 0;
            app.PointsValid = zeros(1,size(app.DataInter,2));
            for i = 1:size(app.DataInter,2)
                if app.DataInter(1,i) > 0 && app.DataInter(2,i) > 0
                    app.NbPoints = app.NbPoints+1;
                    app.PointsValid(i) = 1;
                end
            end
            
            if sum(app.PointsValid) >= 2
                CalibrateMaps(app);
                PlotAllMaps(app);
            end
            
            
        end

        % Cell edit callback: UITable
        function UITableCellEdit(app, event)
            indices = event.Indices;
            newData = event.NewData;
            
        end

        % Cell selection callback: UITable
        function UITableCellSelection(app, event)
            indices = event.Indices;
            
            if isequal(indices(1),1)
                app.UITable.Data(indices(1),indices(2)) = {app.DisplayValue.Value};
            end
            
            if isequal(indices(1),2)
                app.UITable.Data(indices(1),indices(2)) = {app.ppm_EditField.Value};
            end
            
            UITableDisplayDataChanged(app, event)
        end

        % Button pushed function: Button_Help
        function Button_HelpPushed(app, event)
            
            
            if isempty(app.XMapToolsApp.Id_HelpTool)
                Help_Display(app.XMapToolsApp,'XMT_help_Calibration_LAICPMS.html');
            else
                app.XMapToolsApp.Id_HelpTool.UpdateTextHelp('XMT_help_Calibration_LAICPMS.html');
            end
            
            
            
            
        end

        % Cell edit callback: UITable_ElemList
        function UITable_ElemListCellEdit(app, event)
            
            Column = event.Indices(2);
            Row = event.Indices(1);
            NewData = event.NewData;
            
            switch Column
                case 4
                    app.CalibrationOptions.SweepTime(Row) = NewData;
                case 2
                    Value = find(ismember(app.CalibrationOptions.StdOptions,event.NewData));
                    app.CalibrationOptions.SelStd(Row) = Value;
            end
            
            UpdateTableElementStandard(app);
            Overwrite_Import_DT(app);
        end

        % Button pushed function: StdApplytoallButton
        function StdApplytoallButtonPushed(app, event)
            
            app.CalibrationOptions.SelStd(:) = app.Standard_DropDown.Value;
            
            UpdateTableElementStandard(app);
            Overwrite_Import_DT(app);
            
        end

        % Button pushed function: StdApplytoSelectionButton
        function StdApplytoSelectionButtonPushed(app, event)
            
            Selection = app.SelectedCell(:,1);
            
            app.CalibrationOptions.SelStd(Selection) = app.Standard_DropDown.Value;
            
            UpdateTableElementStandard(app);
            Overwrite_Import_DT(app);
            
        end

        % Cell selection callback: UITable_ElemList
        function UITable_ElemListCellSelection(app, event)
            app.SelectedCell = event.Indices;
        end

        % Value changed function: PRIP_MapMenu
        function PRIP_MapMenuValueChanged(app, event)
            Plot_PRIP(app);
        end

        % Value changed function: PRIP_LogScale
        function PRIP_LogScaleValueChanged(app, event)
            CheckLogScale(app)
        end

        % Value changed function: PRIP_Limits_Min
        function PRIP_Limits_MinValueChanged(app, event)
            CheckLimits_PRIP(app)
        end

        % Value changed function: PRIP_Limits_Max
        function PRIP_Limits_MaxValueChanged(app, event)
            CheckLimits_PRIP(app)
        end

        % Button pushed function: PRIP_AddROI
        function PRIP_AddROIButtonPushed(app, event)
            
            eliminate_ROI(app);
            
            DeactivatePlotZoomPanOptions(app);
            
            PosROI = app.PRIP_ROIs.Nb + 1;
            
            switch app.PRIP_MenuROI.Value
                
                case 'Circle'
                    app.PRIP_ROIs.ROI(PosROI).ROI = drawcircle(app.PRIP_Plot,'Color',[0.47,0.67,0.19],'InteractionsAllowed','all');
                    
                    app.PRIP_ROIs.ROI_Data(PosROI).Type = 'Circle';
                    app.PRIP_ROIs.ROI_Data(PosROI).Center = app.PRIP_ROIs.ROI(PosROI).ROI.Center;
                    app.PRIP_ROIs.ROI_Data(PosROI).Radius = app.PRIP_ROIs.ROI(PosROI).ROI.Radius;
                    
                case 'Rectangle'
                    app.PRIP_ROIs.ROI(PosROI).ROI = drawrectangle(app.PRIP_Plot,'Color',[0.47,0.67,0.19],'InteractionsAllowed','all');
                    
                    app.PRIP_ROIs.ROI_Data(PosROI).Type = 'Rectangle';
                    app.PRIP_ROIs.ROI_Data(PosROI).Position = app.PRIP_ROIs.ROI(PosROI).ROI.Position;
                    
                case 'Polygon'
                    app.PRIP_ROIs.ROI(PosROI).ROI = drawpolygon(app.PRIP_Plot,'Color',[0.47,0.67,0.19],'InteractionsAllowed','all');
                    
                    app.PRIP_ROIs.ROI_Data(PosROI).Type = 'Polygon';
                    app.PRIP_ROIs.ROI_Data(PosROI).Position = app.PRIP_ROIs.ROI(PosROI).ROI.Position;
            end
            
            app.PRIP_ROIs_Listener = addlistener(app.PRIP_ROIs.ROI(PosROI).ROI, 'ROIMoved', @(varargin)PRIP_ROIs_DataExtraction(app, app.PRIP_ROIs));
            
            app.PRIP_ROIs.Nb = PosROI;
            
            PRIP_ROIs_DataExtraction_FIRST(app, PosROI);
            
            PRIP_UpdateTableROI(app);
            
            PRIP_ROIs_DataExtraction(app, app.PRIP_ROIs);
            
        end

        % Button pushed function: PRIP_DeleteROI
        function PRIP_DeleteROIButtonPushed(app, event)
            
            SelROI = app.PRIP_Table_ROI.Selection;
            
            if ~isempty(SelROI)
                eliminate_ROI(app);
                
                app.PRIP_ROIs.Nb = app.PRIP_ROIs.Nb - 1;
                app.PRIP_ROIs.ROI(SelROI) = [];
                app.PRIP_ROIs.ROI_Data(SelROI) = [];
                
                PRIP_UpdateTableROI(app);
                
                PRIP_Table_ROICellSelection(app);
            end
            
        end

        % Button pushed function: PRIP_Export
        function PRIP_ExportButtonPushed(app, event)
            
            % Simple export function
            
            GenerateCellFromSelectedData(app)
            
            [Success,Message,MessageID] = mkdir('Exported-PRIP');
            DateStr = char(datestr(now));
            DateStr(find(DateStr == ' ')) = '_'; DateStr(find(DateStr == ':')) = '_';
            ProjectName = ['Export_',DateStr];
            Directory = fullfile(cd,'Exported-PRIP',ProjectName);
            [Success,Message,MessageID] = mkdir(Directory);
            
            T1 = cell2table(app.GeneratedDataCellFormat,'VariableNames',app.GeneratedLabelCellFormat);
            writetable(T1,fullfile(Directory,'Data.csv'));
            
            T2 = cell2table(app.GeneratedLODCellFormat,'VariableNames',app.GeneratedLabelCellFormat);
            writetable(T2,fullfile(Directory,'LOD.csv'));
            
            T3 = cell2table(app.PRIP_Table_ROI.Data,'VariableNames',app.PRIP_Table_ROI.ColumnName);
            writetable(T3,fullfile(Directory,'ROIs.csv'));
            
            f2 = figure;
            ax2 = gca;
            
            copyobj(app.PRIP_Plot.Children(end),ax2);
            axis(ax2,'image');
            colormap(ax2,app.PRIP_Plot.Colormap)
            colorbar('vertical');
            ax2.XTick = app.PRIP_Plot.XTick;
            ax2.YTick = app.PRIP_Plot.YTick;
            ax2.YDir = app.PRIP_Plot.YDir;
            ax2.CLim = app.PRIP_Plot.CLim;
            
            ax2.XLim = app.PRIP_Plot.XLim;
            ax2.YLim = app.PRIP_Plot.YLim;
            
            ax2.ColorScale = app.PRIP_Plot.ColorScale;
            
            for i = 1:length(app.PRIP_ROIs_Data)
                switch app.PRIP_ROIs.ROI_Data(i).Type
                    
                    case 'Circle'
                        drawcircle(ax2,'Color',[0.47,0.67,0.19],'InteractionsAllowed','none','Center',app.PRIP_ROIs.ROI_Data(i).Center,'Radius',app.PRIP_ROIs.ROI_Data(i).Radius,'Label',num2str(i));
                        
                    case 'Rectangle'
                        drawrectangle(ax2,'Color',[0.47,0.67,0.19],'InteractionsAllowed','none','Position',app.PRIP_ROIs.ROI_Data(i).Position,'Label',num2str(i));
                        
                    case 'Polygon'
                        drawpolygon(ax2,'Color',[0.47,0.67,0.19],'InteractionsAllowed','none','Position',app.PRIP_ROIs.ROI_Data(i).Position,'Label',num2str(i));
                end
            end
            
            exportgraphics(ax2, fullfile(Directory,'Map.pdf'));
            
            close(f2);
            
        end

        % Cell selection callback: PRIP_Table_ROI
        function PRIP_Table_ROICellSelection(app, event)
            
            SelROI = app.PRIP_Table_ROI.Selection;
            
            %if app.UpdatePlotROI
            
            eliminate_ROI(app);
            
            switch app.PRIP_ROIs.ROI_Data(SelROI).Type
                
                case 'Circle'
                    app.PRIP_ROIs.ROI(SelROI).ROI = drawcircle(app.PRIP_Plot,'Color',[0.47,0.67,0.19],'InteractionsAllowed','all','Center',app.PRIP_ROIs.ROI_Data(SelROI).Center,'Radius',app.PRIP_ROIs.ROI_Data(SelROI).Radius);
                    
                case 'Rectangle'
                    app.PRIP_ROIs.ROI(SelROI).ROI = drawrectangle(app.PRIP_Plot,'Color',[0.47,0.67,0.19],'InteractionsAllowed','all','Position',app.PRIP_ROIs.ROI_Data(SelROI).Position);
                    
                case 'Polygon'
                    app.PRIP_ROIs.ROI(SelROI).ROI = drawpolygon(app.PRIP_Plot,'Color',[0.47,0.67,0.19],'InteractionsAllowed','all','Position',app.PRIP_ROIs.ROI_Data(SelROI).Position);
            end
            
            app.PRIP_ROIs_Listener = addlistener(app.PRIP_ROIs.ROI(SelROI).ROI, 'ROIMoved', @(varargin)PRIP_ROIs_DataExtraction(app, app.PRIP_ROIs));
            %end
            
            ElNames = app.PxDataRaw.ElNames;
            
            Data4Table = {};
            BackgroundTable = ones(length(ElNames),3);
            
            for i = 1:length(ElNames)
                Data4Table{i,1} = ElNames{i};
                Data4Table{i,2} = app.PRIP_ROIs_Data(SelROI).Conc(i);
                Data4Table{i,3} = app.PRIP_ROIs_Data(SelROI).LOD_ROI(i);
                
                Data4Table{i,4} = app.PRIP_ROIs_Data(SelROI).ConcMeanPixel(i);
                Data4Table{i,5} = app.PRIP_ROIs_Data(SelROI).ConcStdPixel(i);
                
                Data4Table{i,6} = app.PRIP_ROIs_Data(SelROI).LOD_Px(i);
                
                if Data4Table{i,4} < Data4Table{i,6} && Data4Table{i,2} < Data4Table{i,3}
                    BackgroundTable(i,:) = [1,0.76,1];
                end
                
                if Data4Table{i,4} < Data4Table{i,6} && Data4Table{i,2} > Data4Table{i,3}
                    BackgroundTable(i,:) = [0.5608,0.7843,0.5373];
                end
                
            end
            
            app.PRIP_Table_Analysis.ColumnName = {'Element','Weight(ROI)','LOD(ROI)','Weight(Px)_med','Weight(Px)_mad','LOD(Px)_med'};
            
            app.PRIP_Table_Analysis.Data = Data4Table;
            
            app.PRIP_Table_Analysis.BackgroundColor = BackgroundTable;
            
            
            
            % keyboard
        end

        % Selection change function: TabGroup
        function TabGroupSelectionChanged(app, event)
            
            selectedTab = app.TabGroup.SelectedTab;
            
            
            if isempty(app.PxDataRaw.ElNames)
                
                switch selectedTab.Title
                    
                    case 'Pixel Reconstruction and Improved Precision (PRIP)'
                        
                        if isdeployed
                            Result = uiconfirm(app.XMapToolsCalibrationLaICPMS, {'Warning, you are entering a grey area! ',' ', 'The PRIP tools are under development and only a small fraction is currently available in the public version of XMapTools. The results obtained with the current feature are not fully tested and should not be considered fully reliable. If you have any questions or doubts, please contact us.'}, 'XMapTools', 'Options', {'I understand and wish to proceed (uncertain)','I understand and will wait for the next release (Cancel)'});
                        
                            switch Result
                                case 'I understand and will wait for the next release (Cancel)'
                                    return
                            end
                        end
                        
                        
                        Result = uiconfirm(app.XMapToolsCalibrationLaICPMS, 'Missing sweep data. Do you want to load a file?', 'XMapTools', 'Options', {'Yes','No','Cancel'});
                        
                        switch Result
                            case 'Yes'
                                
                                app.WaitBar = uiprogressdlg(app.XMapToolsCalibrationLaICPMS,'Title','XMapTools','Indeterminate','on');
                                app.WaitBar.Message = 'Pick a sweep data file (SweepData_Import.mat)';
                                
                                f=figure('Position',[1,1,5,5],'Unit','Pixel'); drawnow; f.Visible = 'off';
                                [FileName,PathName] = uigetfile({'*.mat','Sweep data (*.mat)';'*.*',  'All Files (*.*)'},'Pick a file');
                                close(f);
                                figure(app.XMapToolsCalibrationLaICPMS)
                                if isempty(FileName)
                                    close(app.WaitBar);
                                    return
                                end
                                
                                app.WaitBar.Message = 'Loading the sweep data';
                                
                                load([PathName,FileName]);
                                
                                if exist('SweepData_Import','var')
                                
                                    app.PxDataRaw = [];
                                    
                                    app.PxDataRaw = SweepData_Import.PxDataRaw;
                                    
                                    % Save back the data to XMapTools
                                    app.XMapToolsApp.XMapToolsData.PxDataRaw = app.PxDataRaw;
                                    
                                    app.WaitBar.Message = 'Activating the PRIP Module';
                                    
                                    app.PRIP_GridLayout.Visible = 'on';
                                    
                                else
                                    uialert(app.XMapToolsCalibrationLaICPMS,'This file is not a valid file with sweep data.','XMapTools – Error');
                                    app.TabGroup.SelectedTab = app.TabGroup.Children(1);
                                end
                                
                                close(app.WaitBar);
                                
                                
                            case 'No'
                                app.TabGroup.SelectedTab = app.TabGroup.Children(1);
                                
                                
                            case 'Cancel'
                                app.TabGroup.SelectedTab = app.TabGroup.Children(1);
                        end
                        
                        %keyboard
                end
                
            end
            
            switch selectedTab.Title
                
                case 'PRIP AI'
                    
                    if isdeployed
                        Results = uiconfirm(app.XMapToolsCalibrationLaICPMS, {'This module is not available in the current release of XMapTools. Stay tuned!'}, 'XMapTools', 'Options', {'Cancel'});
                        app.TabGroup.SelectedTab = app.TabGroup.Children(1);
                        return
                    else
                        app.PRIPAI_GridLayout10.Visible = 'on';
                    end
                    
                    
            end
            
        end

        % Menu selected function: CopyMenu
        function CopyMenuSelected(app, event)
            
            
            
            d = [app.PRIP_Table_Analysis.Data];
            
            str = 'Element';
            str = sprintf('%s\t%s',str,'LOD(ROI)');
            str = sprintf('%s\t%s',str,'Weight(ROI)');
            str = sprintf('%s\t%s',str,'Weight(Px_med)');
            str = sprintf('%s\t%s',str,'Weight(Px_mad)');
            str = sprintf('%s\t%s',str,'LOD(Px)_med');
            
            for i = 1:size(d,1)
                str = sprintf('%s\n%s\t%f\t%f\t%f\t%f\t%f',str,char(d{i,1}),d{i,2},d{i,3},d{i,4},d{i,5},d{i,6});
            end
            
            clipboard ('copy',str);
            
        end

        % Value changed function: PRIP_AI_Grid_Min
        function PRIP_AI_Grid_MinValueChanged(app, event)
            CalculateNumberGrids(app)
        end

        % Value changed function: PRIP_AI_Grid_Step
        function PRIP_AI_Grid_StepValueChanged(app, event)
            CalculateNumberGrids(app)
        end

        % Value changed function: PRIP_AI_Grid_Max
        function PRIP_AI_Grid_MaxValueChanged(app, event)
            CalculateNumberGrids(app)
        end

        % Button pushed function: CalculateROIsButton
        function CalculateROIsButtonPushed(app, event)
            
            app.WaitBar = uiprogressdlg(app.XMapToolsCalibrationLaICPMS,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Generating the ROIs for all grids and calculating LODs using PRIP';
            
            % Prepare the mask
            MaskId = app.ClassDropDown.Value;
            
            MaskMap = zeros(size(app.MaskFile.MaskMap));
            MaskInd = find(app.MaskFile.MaskMap == MaskId);
            MaskMap(MaskInd) = ones(size(MaskInd));
            
            app.PRIP_AI_GridData.Nb = app.PRIP_AI_Grid_Nb.Value;
            app.PRIP_AI_GridData.Res = [app.PRIP_AI_Grid_Min.Value:app.PRIP_AI_Grid_Step.Value:app.PRIP_AI_Grid_Max.Value];
            
            Map4Size = app.PRIP_Plot.Children(end).CData;
            
            [X_grid,Y_grid] = meshgrid([1:size(MaskMap,2)],[1:size(MaskMap,1)]);
            
            for iRes = 1 : length(app.PRIP_AI_GridData.Res)
                
                % Generate the grid:
                StepSizePx = app.PRIP_AI_GridData.Res(iRes);
                Xi = 0:StepSizePx:size(Map4Size,2);
                Yi = 0:StepSizePx:size(Map4Size,1);
                
                Compt = 0;
                
                for iX = 1:length(Xi)-1
                    for iY = 1:length(Yi)-1
                        
                        bw = poly2mask([Xi(iX)-1,Xi(iX+1)-1,Xi(iX+1)-1,Xi(iX)-1],[Yi(iY+1)-1,Yi(iY+1)-1,Yi(iY)-1,Yi(iY)-1],size(MaskMap,1),size(MaskMap,2));
                        PxIdx = find(bw);
                        SelMask = MaskMap(PxIdx);
                        
                        if isequal(sum(SelMask),numel(SelMask))
                            Compt = Compt + 1;
                            app.PRIP_AI_GridData.Grid(iRes).Data(Compt).PxIdx = PxIdx;
                            app.PRIP_AI_GridData.Grid(iRes).Data(Compt).PxCoordXY = [X_grid(PxIdx),Y_grid(PxIdx)];
                            app.PRIP_AI_GridData.Grid(iRes).Data(Compt).NbIdx = length(PxIdx);
                            app.PRIP_AI_GridData.Grid(iRes).Data(Compt).Mask = bw;
                        end
                        
                        
                    end
                end
                
                %figure, hold on
                %Colors4ColorMap = turbo(length(app.PRIP_AI_GridData.Grid(iRes).Data));
                %for i = 1:length(app.PRIP_AI_GridData.Grid(iRes).Data)
                %    plot(app.PRIP_AI_GridData.Grid(iRes).Data(i).PxCoordXY(:,1),app.PRIP_AI_GridData.Grid(iRes).Data(i).PxCoordXY(:,2),'o','Color',Colors4ColorMap(i,:))
                %end
                
            end
            
            CalculateCalibrationGRIDs(app);
            
            % Update the table
            FillTableForPRIP_AI_Step2(app);
            
            
            app.SetasPredictorButton.Enable = 'on';
            app.SetasOutputButton.Enable = 'on';
            
            app.GridResolutionDropDown.Visible = 'on';
            app.TrainSNNButton.Visible = 'on';
            
            app.ParametersPanel.Visible = 'on';
            
            close(app.WaitBar)
            
            
        end

        % Button pushed function: SetasPredictorButton
        function SetasPredictorButtonPushed(app, event)
            
            app.PRIP_AI_SelectedPredictors(app.PRIP_AI_SelectedRowsinTable) = 1;
            update_color_uitable(app);
            
        end

        % Button pushed function: SetasOutputButton
        function SetasOutputButtonPushed(app, event)
            
            app.PRIP_AI_SelectedPredictors(app.PRIP_AI_SelectedRowsinTable) = 0;
            update_color_uitable(app);
            
        end

        % Cell selection callback: ElemPxPrec_UITable
        function ElemPxPrec_UITableCellSelection(app, event)
            app.PRIP_AI_SelectedRowsinTable = event.Indices(:,1);
        end

        % Button pushed function: TrainSNNButton
        function TrainSNNButtonPushed(app, event)
            
            SelectedPredictors = find(app.PRIP_AI_SelectedPredictors == 1);
            SelectedOutputs = find(app.PRIP_AI_SelectedPredictors == 0);
            
            iGrid = app.GridResolutionDropDown.Value;
            
            Compt = 0;
            for iROI = 1:length(app.PRIP_AI_GridData.Grid(iGrid).Data)
                
                Conc_Predictors = app.PRIP_AI_GridData.Grid(iGrid).Data(iROI).Conc(SelectedPredictors);
                LOD_ROI_Predictors = app.PRIP_AI_GridData.Grid(iGrid).Data(iROI).LOD_ROI(SelectedPredictors);
                
                Conc_Outputs = app.PRIP_AI_GridData.Grid(iGrid).Data(iROI).Conc(SelectedOutputs);
                LOD_ROI_Outputs = app.PRIP_AI_GridData.Grid(iGrid).Data(iROI).LOD_ROI(SelectedOutputs);
                
                % We need all elements of the ROI to be above LOD:
                
                if isempty(find(Conc_Predictors < LOD_ROI_Predictors)) && isempty(find(Conc_Outputs < LOD_ROI_Outputs))
                    
                    % We loop over the pixels of the ROI to add them one by one and 
                    % to  check if their values are > LOD. Othwerwise we take the ROI value.  
                    
                    for i = 1:size(app.PRIP_AI_GridData.Grid(iGrid).Data(iROI).ConcAllPx,1)
                        
                        ConcSelPx = app.PRIP_AI_GridData.Grid(iGrid).Data(iROI).ConcAllPx(i,:);
                        LODSelPx = app.PRIP_AI_GridData.Grid(iGrid).Data(iROI).LODAllPx(i,:);
                        
                        Conc_Predictors_Px = zeros(size(Conc_Predictors));
                        
                        for j = 1:length(SelectedPredictors)
                            if ConcSelPx(SelectedPredictors(j)) > LODSelPx(SelectedPredictors(j))
                                Conc_Predictors_Px(j) = ConcSelPx(SelectedPredictors(j));
                            else
                                Conc_Predictors_Px(j) = Conc_Predictors(j);
                            end
                        end
                        
                        Conc_Outputs_Px = zeros(size(Conc_Outputs));
                        
                        for j = 1:length(SelectedOutputs)
                            if ConcSelPx(SelectedOutputs(j)) > LODSelPx(SelectedOutputs(j))
                                Conc_Outputs_Px(j) = ConcSelPx(SelectedOutputs(j));
                            else
                                Conc_Outputs_Px(j) = Conc_Outputs(j);
                            end
                        end
                        
                        Compt = Compt+1;
                        Predictors(:,Compt) = Conc_Predictors_Px;
                        Outputs(:,Compt) = Conc_Outputs_Px;
                        
                    end
                    
                    % This was the first implementation with only the ROI
                    % values. Tested in St-Maxime, Feb. 2025.  
                    
                    % Compt = Compt+1;
                    % Predictors(:,Compt) = Conc_Predictors;
                    % Outputs(:,Compt) = Conc_Outputs;
                end
            end
            
            x = Predictors;     % network input
            
            figure, tiledlayout('flow');
            
            for iDim = 1:size(Outputs,1)
                
                if app.MultipleSNNCheckBox.Value
                    t = Outputs(iDim,:);        
                else
                    t = Outputs;
                end
                
                disp(' ')
                disp('________ New job PRIP AI ________')
                disp(['Nb constraints: ',num2str(numel(x))]);
                disp(['Nb predictors: ',num2str(size(x,1))]);
                disp(['Nb outputs: ',num2str(size(t,1))]);
                
                trainFcn = 'trainlm';  % Levenberg-Marquardt backpropagation.
                
                % Create a Fitting Network
                hiddenLayerSize = app.NbLayersInput.Value .* ones(1,app.NbNeuronsInput.Value);
                net = fitnet(hiddenLayerSize,trainFcn);
                
                disp(['Nb neurons: ',num2str(numel(hiddenLayerSize))]);
                disp(['Nb hiden layers: ',num2str(mean(hiddenLayerSize))]);
                
                net.layers{1}.transferFcn = 'poslin';
                net.layers{2}.transferFcn = 'poslin';
                
                % net.layers{3}.transferFcn = 'poslin';     % this doesn't
                % work at all. 
                
                % view(net)
                
                %rng("default") % For reproducibility
                
                if app.NormalisationCheckBox.Value
                    [x,settings] = mapminmax(Predictors);
                    disp(['Normalisasion: on']);
                end
                
                net.divideParam.trainRatio = 70/100;
                net.divideParam.valRatio = 15/100;
                net.divideParam.testRatio = 15/100;
                
                [net,tr] = train(net,x,t);
                
                % Print some results
                nexttile; hold on
                plot(tr.epoch,tr.perf,'-b','LineWidth',2)
                plot(tr.epoch,tr.vperf,'-g','LineWidth',2)
                plot(tr.epoch,tr.tperf,'-r','LineWidth',2)
                xline(tr.best_epoch,'-k') 
                a = gca;
                a.YScale = 'log';
                title([app.PRIP_AI_MapMenu.Items{SelectedOutputs(iDim)}])
                
                MaskId = app.ClassDropDown.Value;
                MaskMap = zeros(size(app.MaskFile.MaskMap));
                MaskInd = find(app.MaskFile.MaskMap == MaskId);
                MaskMap(MaskInd) = ones(size(MaskInd));
                
                InputMtx = zeros(numel(SelectedPredictors),numel(MaskInd));
                
                for iEl = 1:length(SelectedPredictors)
                    for i = 1:length(MaskInd)
                        InputMtx(iEl,i) = app.MapCpsData(SelectedPredictors(iEl)).Conc(MaskInd(i));
                    end
                end
                
                if app.NormalisationCheckBox.Value
                    InputMtx = mapminmax.apply(InputMtx,settings);
                end
                
                Predictions = net(InputMtx);
                
                if app.MultipleSNNCheckBox.Value
                    PredictionsMtx(iDim,:) = Predictions;
                else
                    PredictionsMtx = Predictions;
                    break
                end
                
            end
            
            NegValues = find(PredictionsMtx < 0);
            if ~isempty(NegValues)
                disp(['** ',num2str(numel(NegValues)),' negative values replaced by 0']);
                PredictionsMtx(NegValues) = 0;
            end
            
            app.MapCpsData_PRIP_AI = app.MapCpsData;
            
            for iEl = 1:length(SelectedOutputs)
                Where = SelectedOutputs(iEl);
                WhereBDL = find(app.MapCpsData_PRIP_AI(Where).Conc < app.MapCpsData_PRIP_AI(Where).LOD);
                Selection = find(ismember(MaskInd,WhereBDL));
                app.MapCpsData_PRIP_AI(Where).Conc(MaskInd(Selection)) = PredictionsMtx(iEl,Selection);
            end
            
            app.PRIP_AI_Plot.Visible = 'On';
            app.PRIP_AI_MapMenu.Visible = 'on';
            
            Plot_PRIP_AI(app)
            
            caxis(app.PRIP_AI_Plot,[1e-2 1e0])
            
        end

        % Value changed function: PRIP_AI_MapMenu
        function PRIP_AI_MapMenuValueChanged(app, event)
            Plot_PRIP_AI(app);
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create XMapToolsCalibrationLaICPMS and hide until all components are created
            app.XMapToolsCalibrationLaICPMS = uifigure('Visible', 'off');
            app.XMapToolsCalibrationLaICPMS.Position = [100 100 1309 771];
            app.XMapToolsCalibrationLaICPMS.Name = 'Calibration Assistant For LA-ICP-MS Data – XMapTools';

            % Create GridLayout
            app.GridLayout = uigridlayout(app.XMapToolsCalibrationLaICPMS);
            app.GridLayout.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};

            % Create Image
            app.Image = uiimage(app.GridLayout);
            app.Image.Layout.Row = 1;
            app.Image.Layout.Column = [1 2];
            app.Image.HorizontalAlignment = 'left';
            app.Image.ImageSource = 'logo_xmap_final.png';

            % Create GridLayout2
            app.GridLayout2 = uigridlayout(app.GridLayout);
            app.GridLayout2.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout2.RowHeight = {'1x'};
            app.GridLayout2.Padding = [10 8 10 8];
            app.GridLayout2.Layout.Row = 1;
            app.GridLayout2.Layout.Column = [3 5];

            % Create Button_Help
            app.Button_Help = uibutton(app.GridLayout2, 'push');
            app.Button_Help.ButtonPushedFcn = createCallbackFcn(app, @Button_HelpPushed, true);
            app.Button_Help.Icon = '061-info.png';
            app.Button_Help.Layout.Row = 1;
            app.Button_Help.Layout.Column = 5;
            app.Button_Help.Text = '';

            % Create GridLayout3
            app.GridLayout3 = uigridlayout(app.GridLayout);
            app.GridLayout3.ColumnWidth = {'1x'};
            app.GridLayout3.RowHeight = {'1x'};
            app.GridLayout3.Padding = [90 5 90 5];
            app.GridLayout3.Layout.Row = 1;
            app.GridLayout3.Layout.Column = [6 9];

            % Create AppyCalibration
            app.AppyCalibration = uibutton(app.GridLayout3, 'push');
            app.AppyCalibration.ButtonPushedFcn = createCallbackFcn(app, @AppyCalibrationButtonPushed, true);
            app.AppyCalibration.Icon = '044-repeat.png';
            app.AppyCalibration.Enable = 'off';
            app.AppyCalibration.Layout.Row = 1;
            app.AppyCalibration.Layout.Column = 1;
            app.AppyCalibration.Text = 'Apply standardization';

            % Create TabGroup
            app.TabGroup = uitabgroup(app.GridLayout);
            app.TabGroup.SelectionChangedFcn = createCallbackFcn(app, @TabGroupSelectionChanged, true);
            app.TabGroup.Layout.Row = [2 13];
            app.TabGroup.Layout.Column = [1 9];

            % Create CalibrationDefinitionTab
            app.CalibrationDefinitionTab = uitab(app.TabGroup);
            app.CalibrationDefinitionTab.Title = 'Calibration Definition';

            % Create GridLayout6
            app.GridLayout6 = uigridlayout(app.CalibrationDefinitionTab);
            app.GridLayout6.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout6.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};

            % Create GridLayout4
            app.GridLayout4 = uigridlayout(app.GridLayout6);
            app.GridLayout4.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout4.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.6x', '1x'};
            app.GridLayout4.RowSpacing = 4;
            app.GridLayout4.Layout.Row = [1 6];
            app.GridLayout4.Layout.Column = [1 5];

            % Create ClassDropDownLabel
            app.ClassDropDownLabel = uilabel(app.GridLayout4);
            app.ClassDropDownLabel.HorizontalAlignment = 'right';
            app.ClassDropDownLabel.Layout.Row = 1;
            app.ClassDropDownLabel.Layout.Column = [1 2];
            app.ClassDropDownLabel.Text = 'Class';

            % Create ClassDropDown
            app.ClassDropDown = uidropdown(app.GridLayout4);
            app.ClassDropDown.ValueChangedFcn = createCallbackFcn(app, @ClassDropDownValueChanged, true);
            app.ClassDropDown.Layout.Row = 1;
            app.ClassDropDown.Layout.Column = [3 6];

            % Create InternalElementStdDropDownLabel
            app.InternalElementStdDropDownLabel = uilabel(app.GridLayout4);
            app.InternalElementStdDropDownLabel.HorizontalAlignment = 'right';
            app.InternalElementStdDropDownLabel.Layout.Row = 2;
            app.InternalElementStdDropDownLabel.Layout.Column = [1 3];
            app.InternalElementStdDropDownLabel.Text = 'Internal Element Std';

            % Create InternalElementStdDropDown
            app.InternalElementStdDropDown = uidropdown(app.GridLayout4);
            app.InternalElementStdDropDown.ValueChangedFcn = createCallbackFcn(app, @InternalElementStdDropDownValueChanged, true);
            app.InternalElementStdDropDown.Layout.Row = 2;
            app.InternalElementStdDropDown.Layout.Column = [4 6];

            % Create ConstantcompositionCheckBox
            app.ConstantcompositionCheckBox = uicheckbox(app.GridLayout4);
            app.ConstantcompositionCheckBox.ValueChangedFcn = createCallbackFcn(app, @ConstantcompositionCheckBoxValueChanged, true);
            app.ConstantcompositionCheckBox.Text = 'Constant composition';
            app.ConstantcompositionCheckBox.Layout.Row = 2;
            app.ConstantcompositionCheckBox.Layout.Column = [7 11];

            % Create VariablecompositionCheckBox
            app.VariablecompositionCheckBox = uicheckbox(app.GridLayout4);
            app.VariablecompositionCheckBox.ValueChangedFcn = createCallbackFcn(app, @VariablecompositionCheckBoxValueChanged, true);
            app.VariablecompositionCheckBox.Text = 'Variable composition';
            app.VariablecompositionCheckBox.Layout.Row = 3;
            app.VariablecompositionCheckBox.Layout.Column = [7 11];

            % Create FixedInternalStdConc_EditFieldLabel
            app.FixedInternalStdConc_EditFieldLabel = uilabel(app.GridLayout4);
            app.FixedInternalStdConc_EditFieldLabel.HorizontalAlignment = 'right';
            app.FixedInternalStdConc_EditFieldLabel.Layout.Row = 2;
            app.FixedInternalStdConc_EditFieldLabel.Layout.Column = [12 13];
            app.FixedInternalStdConc_EditFieldLabel.Text = 'µg/g';

            % Create FixedInternalStdConc_EditField
            app.FixedInternalStdConc_EditField = uieditfield(app.GridLayout4, 'numeric');
            app.FixedInternalStdConc_EditField.ValueChangedFcn = createCallbackFcn(app, @FixedInternalStdConc_EditFieldValueChanged, true);
            app.FixedInternalStdConc_EditField.Layout.Row = 2;
            app.FixedInternalStdConc_EditField.Layout.Column = [14 15];

            % Create InternalCompositionConverterLabel
            app.InternalCompositionConverterLabel = uilabel(app.GridLayout4);
            app.InternalCompositionConverterLabel.VerticalAlignment = 'bottom';
            app.InternalCompositionConverterLabel.FontWeight = 'bold';
            app.InternalCompositionConverterLabel.Layout.Row = 8;
            app.InternalCompositionConverterLabel.Layout.Column = [1 8];
            app.InternalCompositionConverterLabel.Text = 'Internal Composition Converter';

            % Create wtOxideDropDownLabel
            app.wtOxideDropDownLabel = uilabel(app.GridLayout4);
            app.wtOxideDropDownLabel.HorizontalAlignment = 'right';
            app.wtOxideDropDownLabel.Layout.Row = 9;
            app.wtOxideDropDownLabel.Layout.Column = [1 3];
            app.wtOxideDropDownLabel.Text = 'wt% Oxide';

            % Create wtOxideDropDown
            app.wtOxideDropDown = uidropdown(app.GridLayout4);
            app.wtOxideDropDown.ValueChangedFcn = createCallbackFcn(app, @wtOxideDropDownValueChanged, true);
            app.wtOxideDropDown.Layout.Row = 9;
            app.wtOxideDropDown.Layout.Column = [4 6];

            % Create Oxide_EditField
            app.Oxide_EditField = uieditfield(app.GridLayout4, 'numeric');
            app.Oxide_EditField.ValueChangedFcn = createCallbackFcn(app, @Oxide_EditFieldValueChanged, true);
            app.Oxide_EditField.Layout.Row = 9;
            app.Oxide_EditField.Layout.Column = [7 8];
            app.Oxide_EditField.Value = 1;

            % Create ppm_EditField
            app.ppm_EditField = uieditfield(app.GridLayout4, 'numeric');
            app.ppm_EditField.Editable = 'off';
            app.ppm_EditField.Layout.Row = 9;
            app.ppm_EditField.Layout.Column = [11 12];

            % Create toXXLabel
            app.toXXLabel = uilabel(app.GridLayout4);
            app.toXXLabel.HorizontalAlignment = 'center';
            app.toXXLabel.Layout.Row = 9;
            app.toXXLabel.Layout.Column = [9 10];
            app.toXXLabel.Text = 'to XX';

            % Create ggLabel
            app.ggLabel = uilabel(app.GridLayout4);
            app.ggLabel.Layout.Row = 9;
            app.ggLabel.Layout.Column = [13 14];
            app.ggLabel.Text = 'µg/g';

            % Create UITable
            app.UITable = uitable(app.GridLayout4);
            app.UITable.ColumnName = {'Pt-1'; 'Pt-2'; 'Pt-3'; 'Pt-4'; 'Pt-5'; 'Pt-6'};
            app.UITable.RowName = {'Intensity (cps)'; 'Composition (µg/g)'; ''};
            app.UITable.ColumnEditable = true;
            app.UITable.CellEditCallback = createCallbackFcn(app, @UITableCellEdit, true);
            app.UITable.CellSelectionCallback = createCallbackFcn(app, @UITableCellSelection, true);
            app.UITable.DisplayDataChangedFcn = createCallbackFcn(app, @UITableDisplayDataChanged, true);
            app.UITable.Layout.Row = [4 7];
            app.UITable.Layout.Column = [1 15];

            % Create ElementDropDownLabel
            app.ElementDropDownLabel = uilabel(app.GridLayout4);
            app.ElementDropDownLabel.HorizontalAlignment = 'right';
            app.ElementDropDownLabel.Layout.Row = 1;
            app.ElementDropDownLabel.Layout.Column = [11 12];
            app.ElementDropDownLabel.Text = 'Element';

            % Create ElementDropDown
            app.ElementDropDown = uidropdown(app.GridLayout4);
            app.ElementDropDown.ValueChangedFcn = createCallbackFcn(app, @ElementDropDownValueChanged, true);
            app.ElementDropDown.Layout.Row = 1;
            app.ElementDropDown.Layout.Column = [13 15];

            % Create DisplayValue
            app.DisplayValue = uieditfield(app.GridLayout4, 'numeric');
            app.DisplayValue.Editable = 'off';
            app.DisplayValue.Layout.Row = 3;
            app.DisplayValue.Layout.Column = [14 15];

            % Create ButtonTarget
            app.ButtonTarget = uibutton(app.GridLayout4, 'push');
            app.ButtonTarget.ButtonPushedFcn = createCallbackFcn(app, @ButtonTargetPushed, true);
            app.ButtonTarget.Icon = '142-target.png';
            app.ButtonTarget.Tooltip = {'Pick a ROI (circle)'};
            app.ButtonTarget.Layout.Row = 3;
            app.ButtonTarget.Layout.Column = 13;
            app.ButtonTarget.Text = '';

            % Create UITable_ElemList
            app.UITable_ElemList = uitable(app.GridLayout6);
            app.UITable_ElemList.ColumnName = {'Column 1'; 'Column 2'; 'Column 3'; 'Column 4'};
            app.UITable_ElemList.RowName = {};
            app.UITable_ElemList.CellEditCallback = createCallbackFcn(app, @UITable_ElemListCellEdit, true);
            app.UITable_ElemList.CellSelectionCallback = createCallbackFcn(app, @UITable_ElemListCellSelection, true);
            app.UITable_ElemList.Layout.Row = [1 6];
            app.UITable_ElemList.Layout.Column = [6 8];

            % Create GridLayout5
            app.GridLayout5 = uigridlayout(app.GridLayout6);
            app.GridLayout5.ColumnWidth = {'1x', '1x', '1x', '1x'};
            app.GridLayout5.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout5.ColumnSpacing = 4;
            app.GridLayout5.RowSpacing = 4;
            app.GridLayout5.Padding = [4 4 4 4];
            app.GridLayout5.Layout.Row = [1 6];
            app.GridLayout5.Layout.Column = 9;

            % Create Standard_DropDown
            app.Standard_DropDown = uidropdown(app.GridLayout5);
            app.Standard_DropDown.Layout.Row = 1;
            app.Standard_DropDown.Layout.Column = [1 4];

            % Create StdApplytoallButton
            app.StdApplytoallButton = uibutton(app.GridLayout5, 'push');
            app.StdApplytoallButton.ButtonPushedFcn = createCallbackFcn(app, @StdApplytoallButtonPushed, true);
            app.StdApplytoallButton.Layout.Row = 2;
            app.StdApplytoallButton.Layout.Column = [1 4];
            app.StdApplytoallButton.Text = 'Apply to all';

            % Create StdApplytoSelectionButton
            app.StdApplytoSelectionButton = uibutton(app.GridLayout5, 'push');
            app.StdApplytoSelectionButton.ButtonPushedFcn = createCallbackFcn(app, @StdApplytoSelectionButtonPushed, true);
            app.StdApplytoSelectionButton.Layout.Row = 3;
            app.StdApplytoSelectionButton.Layout.Column = [1 4];
            app.StdApplytoSelectionButton.Text = 'Apply to Selection';

            % Create SaveunfilteredmapsCheckBox
            app.SaveunfilteredmapsCheckBox = uicheckbox(app.GridLayout5);
            app.SaveunfilteredmapsCheckBox.Text = 'Save unfiltered maps';
            app.SaveunfilteredmapsCheckBox.FontSize = 9;
            app.SaveunfilteredmapsCheckBox.Layout.Row = 10;
            app.SaveunfilteredmapsCheckBox.Layout.Column = [1 4];

            % Create BydefaultLODfilteredmapsaresavedLabel
            app.BydefaultLODfilteredmapsaresavedLabel = uilabel(app.GridLayout5);
            app.BydefaultLODfilteredmapsaresavedLabel.FontSize = 9;
            app.BydefaultLODfilteredmapsaresavedLabel.Layout.Row = 9;
            app.BydefaultLODfilteredmapsaresavedLabel.Layout.Column = [1 4];
            app.BydefaultLODfilteredmapsaresavedLabel.Text = {'By default, LOD-filtered'; 'maps are saved.'};

            % Create LODfilteringLabel
            app.LODfilteringLabel = uilabel(app.GridLayout5);
            app.LODfilteringLabel.HorizontalAlignment = 'center';
            app.LODfilteringLabel.VerticalAlignment = 'bottom';
            app.LODfilteringLabel.FontWeight = 'bold';
            app.LODfilteringLabel.Layout.Row = 8;
            app.LODfilteringLabel.Layout.Column = [1 4];
            app.LODfilteringLabel.Text = 'LOD filtering';

            % Create Plot1
            app.Plot1 = uiaxes(app.GridLayout6);
            title(app.Plot1, 'Title')
            app.Plot1.PlotBoxAspectRatio = [1.40467625899281 1 1];
            app.Plot1.XTick = [];
            app.Plot1.YTick = [];
            app.Plot1.Layout.Row = [7 12];
            app.Plot1.Layout.Column = [1 3];

            % Create Plot3
            app.Plot3 = uiaxes(app.GridLayout6);
            title(app.Plot3, 'Title')
            app.Plot3.PlotBoxAspectRatio = [1.40467625899281 1 1];
            app.Plot3.XTick = [];
            app.Plot3.YTick = [];
            app.Plot3.Layout.Row = [7 12];
            app.Plot3.Layout.Column = [4 6];

            % Create Plot2
            app.Plot2 = uiaxes(app.GridLayout6);
            title(app.Plot2, 'Title')
            app.Plot2.PlotBoxAspectRatio = [1.40467625899281 1 1];
            app.Plot2.XTick = [];
            app.Plot2.YTick = [];
            app.Plot2.Layout.Row = [7 12];
            app.Plot2.Layout.Column = [7 9];

            % Create PixelReconstructionandImprovedPrecisionPRIPTab
            app.PixelReconstructionandImprovedPrecisionPRIPTab = uitab(app.TabGroup);
            app.PixelReconstructionandImprovedPrecisionPRIPTab.Title = 'Pixel Reconstruction and Improved Precision (PRIP)';

            % Create PRIP_GridLayout
            app.PRIP_GridLayout = uigridlayout(app.PixelReconstructionandImprovedPrecisionPRIPTab);
            app.PRIP_GridLayout.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.PRIP_GridLayout.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};

            % Create PRIP_Table_ROI
            app.PRIP_Table_ROI = uitable(app.PRIP_GridLayout);
            app.PRIP_Table_ROI.ColumnName = {'ROI'; 'Type'; 'Pixels'};
            app.PRIP_Table_ROI.RowName = {};
            app.PRIP_Table_ROI.CellSelectionCallback = createCallbackFcn(app, @PRIP_Table_ROICellSelection, true);
            app.PRIP_Table_ROI.Layout.Row = [2 5];
            app.PRIP_Table_ROI.Layout.Column = [7 11];

            % Create PRIP_Table_Analysis
            app.PRIP_Table_Analysis = uitable(app.PRIP_GridLayout);
            app.PRIP_Table_Analysis.ColumnName = {'Element'; 'Weight'; 'LOD(ROI)'; 'LOD(Pixel)'};
            app.PRIP_Table_Analysis.RowName = {};
            app.PRIP_Table_Analysis.Layout.Row = [6 14];
            app.PRIP_Table_Analysis.Layout.Column = [7 11];

            % Create GridLayout8
            app.GridLayout8 = uigridlayout(app.PRIP_GridLayout);
            app.GridLayout8.ColumnWidth = {'1x', '0.5x', '1x', '1x', '0.5x', '1x'};
            app.GridLayout8.RowHeight = {'1x'};
            app.GridLayout8.Padding = [0 2 0 2];
            app.GridLayout8.Layout.Row = 1;
            app.GridLayout8.Layout.Column = [7 11];

            % Create PRIP_AddROI
            app.PRIP_AddROI = uibutton(app.GridLayout8, 'push');
            app.PRIP_AddROI.ButtonPushedFcn = createCallbackFcn(app, @PRIP_AddROIButtonPushed, true);
            app.PRIP_AddROI.Icon = '139-placeholder.png';
            app.PRIP_AddROI.Layout.Row = 1;
            app.PRIP_AddROI.Layout.Column = 3;
            app.PRIP_AddROI.Text = 'Add ROI';

            % Create PRIP_DeleteROI
            app.PRIP_DeleteROI = uibutton(app.GridLayout8, 'push');
            app.PRIP_DeleteROI.ButtonPushedFcn = createCallbackFcn(app, @PRIP_DeleteROIButtonPushed, true);
            app.PRIP_DeleteROI.Icon = '058-error.png';
            app.PRIP_DeleteROI.Layout.Row = 1;
            app.PRIP_DeleteROI.Layout.Column = 4;
            app.PRIP_DeleteROI.Text = 'Delete ROI';

            % Create PRIP_MenuROI
            app.PRIP_MenuROI = uidropdown(app.GridLayout8);
            app.PRIP_MenuROI.Items = {'Circle', 'Rectangle', 'Polygon'};
            app.PRIP_MenuROI.Layout.Row = 1;
            app.PRIP_MenuROI.Layout.Column = [1 2];
            app.PRIP_MenuROI.Value = 'Circle';

            % Create PRIP_Export
            app.PRIP_Export = uibutton(app.GridLayout8, 'push');
            app.PRIP_Export.ButtonPushedFcn = createCallbackFcn(app, @PRIP_ExportButtonPushed, true);
            app.PRIP_Export.Icon = '096-upload.png';
            app.PRIP_Export.Layout.Row = 1;
            app.PRIP_Export.Layout.Column = 6;
            app.PRIP_Export.Text = 'Export';

            % Create GridLayout9
            app.GridLayout9 = uigridlayout(app.PRIP_GridLayout);
            app.GridLayout9.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '0.4x', '1x', '1x', '1x'};
            app.GridLayout9.RowHeight = {'1x'};
            app.GridLayout9.Padding = [0 2 0 2];
            app.GridLayout9.Layout.Row = 1;
            app.GridLayout9.Layout.Column = [1 6];

            % Create PRIP_MapMenu
            app.PRIP_MapMenu = uidropdown(app.GridLayout9);
            app.PRIP_MapMenu.ValueChangedFcn = createCallbackFcn(app, @PRIP_MapMenuValueChanged, true);
            app.PRIP_MapMenu.Layout.Row = 1;
            app.PRIP_MapMenu.Layout.Column = [1 2];

            % Create PRIP_Limits_Min
            app.PRIP_Limits_Min = uieditfield(app.GridLayout9, 'numeric');
            app.PRIP_Limits_Min.ValueChangedFcn = createCallbackFcn(app, @PRIP_Limits_MinValueChanged, true);
            app.PRIP_Limits_Min.Enable = 'off';
            app.PRIP_Limits_Min.Layout.Row = 1;
            app.PRIP_Limits_Min.Layout.Column = 4;

            % Create PRIP_Limits_Max
            app.PRIP_Limits_Max = uieditfield(app.GridLayout9, 'numeric');
            app.PRIP_Limits_Max.ValueChangedFcn = createCallbackFcn(app, @PRIP_Limits_MaxValueChanged, true);
            app.PRIP_Limits_Max.Enable = 'off';
            app.PRIP_Limits_Max.Layout.Row = 1;
            app.PRIP_Limits_Max.Layout.Column = 5;

            % Create PRIP_LogScale
            app.PRIP_LogScale = uicheckbox(app.GridLayout9);
            app.PRIP_LogScale.ValueChangedFcn = createCallbackFcn(app, @PRIP_LogScaleValueChanged, true);
            app.PRIP_LogScale.Text = 'Log color scale';
            app.PRIP_LogScale.Layout.Row = 1;
            app.PRIP_LogScale.Layout.Column = [8 9];
            app.PRIP_LogScale.Value = true;

            % Create PRIP_Plot
            app.PRIP_Plot = uiaxes(app.PRIP_GridLayout);
            title(app.PRIP_Plot, 'Title')
            app.PRIP_Plot.PlotBoxAspectRatio = [1.18336314847943 1 1];
            app.PRIP_Plot.XTick = [];
            app.PRIP_Plot.YTick = [];
            app.PRIP_Plot.Layout.Row = [2 14];
            app.PRIP_Plot.Layout.Column = [1 6];

            % Create PRIPAITab
            app.PRIPAITab = uitab(app.TabGroup);
            app.PRIPAITab.Title = 'PRIP AI';

            % Create PRIPAI_GridLayout10
            app.PRIPAI_GridLayout10 = uigridlayout(app.PRIPAITab);
            app.PRIPAI_GridLayout10.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.PRIPAI_GridLayout10.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};

            % Create ElemPxPrec_UITable
            app.ElemPxPrec_UITable = uitable(app.PRIPAI_GridLayout10);
            app.ElemPxPrec_UITable.ColumnName = {'Column 1'; 'Column 2'; 'Column 3'; 'Column 4'};
            app.ElemPxPrec_UITable.RowName = {};
            app.ElemPxPrec_UITable.CellSelectionCallback = createCallbackFcn(app, @ElemPxPrec_UITableCellSelection, true);
            app.ElemPxPrec_UITable.Layout.Row = [2 14];
            app.ElemPxPrec_UITable.Layout.Column = [1 4];

            % Create GridLayout10
            app.GridLayout10 = uigridlayout(app.PRIPAI_GridLayout10);
            app.GridLayout10.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout10.RowHeight = {'1x'};
            app.GridLayout10.ColumnSpacing = 5;
            app.GridLayout10.RowSpacing = 5;
            app.GridLayout10.Padding = [2 0 2 0];
            app.GridLayout10.Layout.Row = 1;
            app.GridLayout10.Layout.Column = [1 5];

            % Create PRIP_AI_Grid_Min
            app.PRIP_AI_Grid_Min = uieditfield(app.GridLayout10, 'numeric');
            app.PRIP_AI_Grid_Min.Limits = [2 100];
            app.PRIP_AI_Grid_Min.ValueChangedFcn = createCallbackFcn(app, @PRIP_AI_Grid_MinValueChanged, true);
            app.PRIP_AI_Grid_Min.Layout.Row = 1;
            app.PRIP_AI_Grid_Min.Layout.Column = 5;
            app.PRIP_AI_Grid_Min.Value = 4;

            % Create PRIP_AI_Grid_Step
            app.PRIP_AI_Grid_Step = uieditfield(app.GridLayout10, 'numeric');
            app.PRIP_AI_Grid_Step.Limits = [1 100];
            app.PRIP_AI_Grid_Step.ValueChangedFcn = createCallbackFcn(app, @PRIP_AI_Grid_StepValueChanged, true);
            app.PRIP_AI_Grid_Step.Layout.Row = 1;
            app.PRIP_AI_Grid_Step.Layout.Column = 6;
            app.PRIP_AI_Grid_Step.Value = 2;

            % Create PRIP_AI_Grid_Max
            app.PRIP_AI_Grid_Max = uieditfield(app.GridLayout10, 'numeric');
            app.PRIP_AI_Grid_Max.Limits = [4 100];
            app.PRIP_AI_Grid_Max.ValueChangedFcn = createCallbackFcn(app, @PRIP_AI_Grid_MaxValueChanged, true);
            app.PRIP_AI_Grid_Max.Layout.Row = 1;
            app.PRIP_AI_Grid_Max.Layout.Column = 7;
            app.PRIP_AI_Grid_Max.Value = 10;

            % Create CalculateROIsButton
            app.CalculateROIsButton = uibutton(app.GridLayout10, 'push');
            app.CalculateROIsButton.ButtonPushedFcn = createCallbackFcn(app, @CalculateROIsButtonPushed, true);
            app.CalculateROIsButton.Icon = '044-repeat.png';
            app.CalculateROIsButton.Layout.Row = 1;
            app.CalculateROIsButton.Layout.Column = [10 13];
            app.CalculateROIsButton.Text = 'Calculate ROIs';

            % Create GridsizeminstepmaxLabel
            app.GridsizeminstepmaxLabel = uilabel(app.GridLayout10);
            app.GridsizeminstepmaxLabel.HorizontalAlignment = 'right';
            app.GridsizeminstepmaxLabel.Layout.Row = 1;
            app.GridsizeminstepmaxLabel.Layout.Column = [1 4];
            app.GridsizeminstepmaxLabel.Text = 'Grid size (min:step:max)';

            % Create Label
            app.Label = uilabel(app.GridLayout10);
            app.Label.HorizontalAlignment = 'center';
            app.Label.Layout.Row = 1;
            app.Label.Layout.Column = 8;
            app.Label.Text = '=';

            % Create PRIP_AI_Grid_Nb
            app.PRIP_AI_Grid_Nb = uieditfield(app.GridLayout10, 'numeric');
            app.PRIP_AI_Grid_Nb.Editable = 'off';
            app.PRIP_AI_Grid_Nb.HorizontalAlignment = 'center';
            app.PRIP_AI_Grid_Nb.Enable = 'off';
            app.PRIP_AI_Grid_Nb.Layout.Row = 1;
            app.PRIP_AI_Grid_Nb.Layout.Column = 9;

            % Create SetasPredictorButton
            app.SetasPredictorButton = uibutton(app.PRIPAI_GridLayout10, 'push');
            app.SetasPredictorButton.ButtonPushedFcn = createCallbackFcn(app, @SetasPredictorButtonPushed, true);
            app.SetasPredictorButton.FontWeight = 'bold';
            app.SetasPredictorButton.FontColor = [0 0 1];
            app.SetasPredictorButton.Layout.Row = 3;
            app.SetasPredictorButton.Layout.Column = 5;
            app.SetasPredictorButton.Text = 'Set as Predictor';

            % Create SetasOutputButton
            app.SetasOutputButton = uibutton(app.PRIPAI_GridLayout10, 'push');
            app.SetasOutputButton.ButtonPushedFcn = createCallbackFcn(app, @SetasOutputButtonPushed, true);
            app.SetasOutputButton.FontWeight = 'bold';
            app.SetasOutputButton.FontColor = [1 0 1];
            app.SetasOutputButton.Layout.Row = 4;
            app.SetasOutputButton.Layout.Column = 5;
            app.SetasOutputButton.Text = 'Set as Output';

            % Create GridLayout11
            app.GridLayout11 = uigridlayout(app.PRIPAI_GridLayout10);
            app.GridLayout11.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout11.RowHeight = {'1x'};
            app.GridLayout11.ColumnSpacing = 5;
            app.GridLayout11.RowSpacing = 5;
            app.GridLayout11.Padding = [2 0 2 0];
            app.GridLayout11.Layout.Row = 1;
            app.GridLayout11.Layout.Column = [6 11];

            % Create TrainSNNButton
            app.TrainSNNButton = uibutton(app.GridLayout11, 'push');
            app.TrainSNNButton.ButtonPushedFcn = createCallbackFcn(app, @TrainSNNButtonPushed, true);
            app.TrainSNNButton.Icon = '042-shuffle.png';
            app.TrainSNNButton.Layout.Row = 1;
            app.TrainSNNButton.Layout.Column = [6 8];
            app.TrainSNNButton.Text = 'Train SNN';

            % Create GridResolutionDropDown
            app.GridResolutionDropDown = uidropdown(app.GridLayout11);
            app.GridResolutionDropDown.Layout.Row = 1;
            app.GridResolutionDropDown.Layout.Column = [4 5];

            % Create ResolutionDropDownLabel
            app.ResolutionDropDownLabel = uilabel(app.GridLayout11);
            app.ResolutionDropDownLabel.HorizontalAlignment = 'right';
            app.ResolutionDropDownLabel.Layout.Row = 1;
            app.ResolutionDropDownLabel.Layout.Column = [2 3];
            app.ResolutionDropDownLabel.Text = 'Resolution';

            % Create PRIP_AI_MapMenu
            app.PRIP_AI_MapMenu = uidropdown(app.GridLayout11);
            app.PRIP_AI_MapMenu.ValueChangedFcn = createCallbackFcn(app, @PRIP_AI_MapMenuValueChanged, true);
            app.PRIP_AI_MapMenu.Layout.Row = 1;
            app.PRIP_AI_MapMenu.Layout.Column = [12 13];

            % Create ParametersPanel
            app.ParametersPanel = uipanel(app.PRIPAI_GridLayout10);
            app.ParametersPanel.Title = 'Parameters';
            app.ParametersPanel.Layout.Row = [6 14];
            app.ParametersPanel.Layout.Column = 5;

            % Create GridLayout12
            app.GridLayout12 = uigridlayout(app.ParametersPanel);
            app.GridLayout12.ColumnWidth = {'1x', '1x', '1x', '1x'};
            app.GridLayout12.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout12.ColumnSpacing = 5;
            app.GridLayout12.RowSpacing = 5;
            app.GridLayout12.Padding = [5 5 5 5];

            % Create NormalisationCheckBox
            app.NormalisationCheckBox = uicheckbox(app.GridLayout12);
            app.NormalisationCheckBox.Text = 'Normalisation';
            app.NormalisationCheckBox.FontSize = 10;
            app.NormalisationCheckBox.Layout.Row = 1;
            app.NormalisationCheckBox.Layout.Column = [1 4];

            % Create NbNeuronsInput
            app.NbNeuronsInput = uieditfield(app.GridLayout12, 'numeric');
            app.NbNeuronsInput.Limits = [2 5];
            app.NbNeuronsInput.HorizontalAlignment = 'center';
            app.NbNeuronsInput.Layout.Row = 3;
            app.NbNeuronsInput.Layout.Column = [1 2];
            app.NbNeuronsInput.Value = 2;

            % Create NbLayersInput
            app.NbLayersInput = uieditfield(app.GridLayout12, 'numeric');
            app.NbLayersInput.Limits = [5 50];
            app.NbLayersInput.HorizontalAlignment = 'center';
            app.NbLayersInput.Layout.Row = 3;
            app.NbLayersInput.Layout.Column = [3 4];
            app.NbLayersInput.Value = 5;

            % Create NeuronsLabel
            app.NeuronsLabel = uilabel(app.GridLayout12);
            app.NeuronsLabel.HorizontalAlignment = 'center';
            app.NeuronsLabel.VerticalAlignment = 'bottom';
            app.NeuronsLabel.FontSize = 10;
            app.NeuronsLabel.FontWeight = 'bold';
            app.NeuronsLabel.Layout.Row = 2;
            app.NeuronsLabel.Layout.Column = [1 2];
            app.NeuronsLabel.Text = 'Neurons';

            % Create LayersLabel
            app.LayersLabel = uilabel(app.GridLayout12);
            app.LayersLabel.HorizontalAlignment = 'center';
            app.LayersLabel.VerticalAlignment = 'bottom';
            app.LayersLabel.FontSize = 10;
            app.LayersLabel.FontWeight = 'bold';
            app.LayersLabel.Layout.Row = 2;
            app.LayersLabel.Layout.Column = [3 4];
            app.LayersLabel.Text = 'Layers';

            % Create MultipleSNNCheckBox
            app.MultipleSNNCheckBox = uicheckbox(app.GridLayout12);
            app.MultipleSNNCheckBox.Text = 'Multiple-SNN';
            app.MultipleSNNCheckBox.FontSize = 10;
            app.MultipleSNNCheckBox.Layout.Row = 6;
            app.MultipleSNNCheckBox.Layout.Column = [1 4];
            app.MultipleSNNCheckBox.Value = true;

            % Create PRIP_AI_Plot
            app.PRIP_AI_Plot = uiaxes(app.PRIPAI_GridLayout10);
            title(app.PRIP_AI_Plot, 'Title')
            app.PRIP_AI_Plot.PlotBoxAspectRatio = [1.18336314847943 1 1];
            app.PRIP_AI_Plot.XTick = [];
            app.PRIP_AI_Plot.YTick = [];
            app.PRIP_AI_Plot.Layout.Row = [2 14];
            app.PRIP_AI_Plot.Layout.Column = [6 11];

            % Create ContextMenu
            app.ContextMenu = uicontextmenu(app.XMapToolsCalibrationLaICPMS);
            
            % Assign app.ContextMenu
            app.PRIP_Table_Analysis.ContextMenu = app.ContextMenu;

            % Create CopyMenu
            app.CopyMenu = uimenu(app.ContextMenu);
            app.CopyMenu.MenuSelectedFcn = createCallbackFcn(app, @CopyMenuSelected, true);
            app.CopyMenu.Text = 'Copy';

            % Show the figure after all components are created
            app.XMapToolsCalibrationLaICPMS.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = Calibration_LAICPMS_exported(varargin)

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.XMapToolsCalibrationLaICPMS)

            % Execute the startup function
            runStartupFcn(app, @(app)startupFcn(app, varargin{:}))

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.XMapToolsCalibrationLaICPMS)
        end
    end
end