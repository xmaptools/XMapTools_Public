classdef Data_Export_exported < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        DataExport                   matlab.ui.Figure
        GridLayout                   matlab.ui.container.GridLayout
        DataSourcePanel              matlab.ui.container.Panel
        GridLayout2                  matlab.ui.container.GridLayout
        Mode_Merged_Button           matlab.ui.control.StateButton
        Mode_Quanti_Button           matlab.ui.control.StateButton
        Mode_Results_Button          matlab.ui.control.StateButton
        GridLayout3                  matlab.ui.container.GridLayout
        Image                        matlab.ui.control.Image
        MaskFileOutputFormatPanel    matlab.ui.container.Panel
        GridLayout4                  matlab.ui.container.GridLayout
        MaskFileDropDown             matlab.ui.control.DropDown
        MaskFileDropDownLabel        matlab.ui.control.Label
        SecondaryDropDownLabel       matlab.ui.control.Label
        SecondaryDropDown            matlab.ui.control.DropDown
        DataPanel                    matlab.ui.container.Panel
        GridLayout5                  matlab.ui.container.GridLayout
        MeanCheckBox                 matlab.ui.control.CheckBox
        MedianCheckBox               matlab.ui.control.CheckBox
        ModeCheckBox                 matlab.ui.control.CheckBox
        ExportedDataLabel            matlab.ui.control.Label
        UncertaintiesLabel           matlab.ui.control.Label
        StandarddeviationCheckBox    matlab.ui.control.CheckBox
        BRCCheckBox                  matlab.ui.control.CheckBox
        SizeEditFieldLabel           matlab.ui.control.Label
        SizeEditField                matlab.ui.control.NumericEditField
        ThresholdEditFieldLabel      matlab.ui.control.Label
        ThresholdEditField           matlab.ui.control.NumericEditField
        PlotButton                   matlab.ui.control.Button
        MasksSubmasksLabel           matlab.ui.control.Label
        IncludebothMaskSubMasksCheckBox  matlab.ui.control.CheckBox
        IncludeSubMasksCheckBox      matlab.ui.control.CheckBox
        ExcludezerosCheckBox         matlab.ui.control.CheckBox
        MetadataPanel                matlab.ui.container.Panel
        GridLayout5_2                matlab.ui.container.GridLayout
        SampleNameEditField          matlab.ui.control.EditField
        SampleNameEditFieldLabel     matlab.ui.control.Label
        PressureGPaEditField         matlab.ui.control.NumericEditField
        PressureGPaEditFieldLabel    matlab.ui.control.Label
        TemperatureCEditField        matlab.ui.control.NumericEditField
        TemperatureCEditFieldLabel   matlab.ui.control.Label
        PrintAssemblageCheckBox      matlab.ui.control.CheckBox
        Include_SampleCheckBox       matlab.ui.control.CheckBox
        Include_PressureCheckBox     matlab.ui.control.CheckBox
        Include_TemperatureCheckBox  matlab.ui.control.CheckBox
        GenerateSaveButton           matlab.ui.control.Button
        ExportFormatDropDown         matlab.ui.control.DropDown
        ExportFormatDropDownLabel    matlab.ui.control.Label
    end

    
    properties (Access = private)
        XMapToolsApp 
        
        GeneratedDataCellFormat 
        GeneratedLabelCellFormat
        
        BRC
        
        WaitBar 
        
        
    end
    
    methods (Access = private)
        
        function update_export_source(app)
            
            % turn off the other buttons when they will be there. 
            
            %if isequal(app.Mode_Merged_Button.Value,0)
                %app.Mode_Merged_Button.Value = 1;
            %end
            
            if isequal(app.Mode_Merged_Button.Value,1)
                app.SecondaryDropDown.Items = app.XMapToolsApp.XMapToolsData.MapData.Me.Names;
                app.SecondaryDropDown.ItemsData = [1:length(app.SecondaryDropDown.Items)];
                app.SecondaryDropDownLabel.Text = "Merged";
            end
            
            if isequal(app.Mode_Quanti_Button.Value,1)
                app.SecondaryDropDown.Items = app.XMapToolsApp.XMapToolsData.MapData.Qt.Names;
                app.SecondaryDropDown.ItemsData = [1:length(app.SecondaryDropDown.Items)];
                app.SecondaryDropDownLabel.Text = "Quanti";
            end
            
            if isequal(app.Mode_Results_Button.Value,1)
                app.SecondaryDropDown.Items = app.XMapToolsApp.XMapToolsData.MapData.Re.Names;
                app.SecondaryDropDown.ItemsData = [1:length(app.SecondaryDropDown.Items)];
                app.SecondaryDropDownLabel.Text = "Results";
            end
            
        end
        
        function update_gui_method(app)
            
            switch app.ExportFormatDropDown.Value
                case 'ThermoFit'
                    app.ModeCheckBox.Value = 1;
                    app.MedianCheckBox.Value = 0;
                    app.MeanCheckBox.Value = 0;
                    
                    app.StandarddeviationCheckBox.Value = 1;
                    
                    app.PrintAssemblageCheckBox.Value = 1;
                    app.Include_SampleCheckBox.Value = 1;
                    app.Include_PressureCheckBox.Value = 1;
                    app.Include_TemperatureCheckBox.Value = 1;
                    
                case 'MinPlot'
                    app.ModeCheckBox.Value = 0;
                    app.MedianCheckBox.Value = 1;
                    app.MeanCheckBox.Value = 0;
                    
                    app.StandarddeviationCheckBox.Value = 0;
                    
                    app.PrintAssemblageCheckBox.Value = 0;
                    app.Include_SampleCheckBox.Value = 0;
                    app.Include_PressureCheckBox.Value = 0;
                    app.Include_TemperatureCheckBox.Value = 0;
            end
        end
        
        function GenerateCellFromSelectedDataIncludingSubmasks(app)
            
            MaskFile = app.XMapToolsApp.XMapToolsData.MapData.MaskFile;
            
            if isequal(app.Mode_Merged_Button.Value,1)
                Data_Sel = app.XMapToolsApp.XMapToolsData.MapData.Me;
                ElemList = Data_Sel.Data(app.SecondaryDropDown.Value).ElNames;
            end
            
            if isequal(app.Mode_Quanti_Button.Value,1)
                Data_Sel = app.XMapToolsApp.XMapToolsData.MapData.Qt;
                ElemList = Data_Sel.Data(app.SecondaryDropDown.Value).ElNames;
            end
            
            if isequal(app.Mode_Results_Button.Value,1) 
                Data_Sel = app.XMapToolsApp.XMapToolsData.MapData.Re;
                ElemList = Data_Sel.Data(app.SecondaryDropDown.Value).Labels;
            end
            
            PhaseList = MaskFile.Masks(app.MaskFileDropDown.Value).Names(2:end);

            MaskMap = MaskFile.Masks(app.MaskFileDropDown.Value).MaskMap;
            
            PhaseList = {};
            IsSubMask = [];     % 1 is a submask, 0 is not.
            MaskIdx = [];
            SubMaskIdx = [];
            Compt = 0;
            
            app.WaitBar = uiprogressdlg(app.DataExport,'Title','XMapTools','Value',0);
            app.WaitBar.Message = 'XMapTools is pulling out numbers for you, please wait';
            
            for i = 2:length(MaskFile.Masks(app.MaskFileDropDown.Value).Names)
                if isempty(MaskFile.Masks(app.MaskFileDropDown.Value).SubMask(i).Names)
                    Compt = Compt+1;
                    PhaseList{Compt} = MaskFile.Masks(app.MaskFileDropDown.Value).Names{i};
                    IsSubMask(Compt) = 0;
                    MaskIdx(Compt) = i;
                    SubMaskIdx(Compt) = 0;
                else
                    if isequal(app.IncludeSubMasksCheckBox.Value,1)
                        if isequal(app.IncludebothMaskSubMasksCheckBox.Value,1)
                            % we include both
                            Compt = Compt+1;
                            PhaseList{Compt} = MaskFile.Masks(app.MaskFileDropDown.Value).Names{i};
                            IsSubMask(Compt) = 0;
                            MaskIdx(Compt) = i;
                            SubMaskIdx(Compt) = 0;
                        end
                        for j = 2:length(MaskFile.Masks(app.MaskFileDropDown.Value).SubMask(i).Names)
                            Compt = Compt+1;
                            PhaseList{Compt} = MaskFile.Masks(app.MaskFileDropDown.Value).SubMask(i).Names{j};
                            IsSubMask(Compt) = 1;
                            MaskIdx(Compt) = i;
                            SubMaskIdx(Compt) = j-1;
                        end
                    else
                        Compt = Compt+1;
                        PhaseList{Compt} = MaskFile.Masks(app.MaskFileDropDown.Value).Names{i};
                        IsSubMask(Compt) = 0;
                        MaskIdx(Compt) = i;
                        SubMaskIdx(Compt) = 0;
                    end
                end
            end
            
            % Export of compositions:
            NbOutput = 0;
            if app.MedianCheckBox.Value
                NbOutput = NbOutput + 1;
                Posmedian = NbOutput;
            end
            if app.MeanCheckBox.Value
                NbOutput = NbOutput + 1;
                Posmean = NbOutput;
            end
            if app.ModeCheckBox.Value
                NbOutput = NbOutput + 1;
                PosModes = NbOutput;
            end
            
            CellData = cell(NbOutput * length(PhaseList),length(ElemList) + 1);
            
            if isequal(app.ExcludezerosCheckBox.Value,1)
                NumberPixels = zeros(size(CellData));
                NumberPixelsExcluded = zeros(size(CellData));
            end
            
            for i = 1:length(PhaseList)
                
                app.WaitBar.Value = i/(length(PhaseList));
                
                PhaseMap = zeros(size(MaskMap));
                
                if IsSubMask(i)
                    SubMaskMap = MaskFile.Masks(app.MaskFileDropDown.Value).SubMask(MaskIdx(i)).MaskSelMaskMap;
                    Idx = find(SubMaskMap == SubMaskIdx(i));
                    PhaseMap(Idx) = 1;
                else
                    Idx = find(MaskMap == MaskIdx(i)-1);
                    PhaseMap(Idx) = 1;
                end
                
                % Apply BRC filter
                PhaseMap = PhaseMap .* app.BRC;
                
                Idx = find(PhaseMap);
                
                count = 0;
                for j = 1:length(ElemList) 
                    
                    PxData = Data_Sel.Data(app.SecondaryDropDown.Value).CData(j).Map(Idx);
                    
                    if isequal(app.ExcludezerosCheckBox.Value,1)
                        NbPxTemp = length(PxData);
                        IdxSelected = find(PxData <= 1e-12);
                        PxData(IdxSelected) = [];
                        NbPxExTemp = length(IdxSelected);
                    end
                    
                    if app.ModeCheckBox.Value
                        if isequal(j,1)
                            CellData{NbOutput*(i-1)+PosModes+count,1} = PhaseList{i};
                            CellData{NbOutput*(i-1)+PosModes,2} = 'mode(px)';
                        end
                        ModeVal = calc_mode(app,PxData);
                        CellData{NbOutput*(i-1)+PosModes,j+2} = ModeVal;
                        
                        if isequal(app.ExcludezerosCheckBox.Value,1)
                            NumberPixels(NbOutput*(i-1)+PosModes,j) = NbPxTemp;
                            NumberPixelsExcluded(NbOutput*(i-1)+PosModes,j) = NbPxExTemp;
                        end
                    end
                    
                    if app.MedianCheckBox.Value
                        if isequal(j,1)
                            CellData{NbOutput*(i-1)+Posmedian,1} = PhaseList{i};
                            CellData{NbOutput*(i-1)+Posmedian,2} = 'median(px)';
                        end
                        CellData{NbOutput*(i-1)+Posmedian,j+2} = median(PxData);
                        
                        if isequal(app.ExcludezerosCheckBox.Value,1)
                            NumberPixels(NbOutput*(i-1)+Posmedian,j) = NbPxTemp;
                            NumberPixelsExcluded(NbOutput*(i-1)+Posmedian,j) = NbPxExTemp;
                        end
                    end
                    
                    if app.MeanCheckBox.Value
                        if isequal(j,1)
                            CellData{NbOutput*(i-1)+Posmean,1} = PhaseList{i};
                            CellData{NbOutput*(i-1)+Posmean,2} = 'mean(px)';
                        end
                        CellData{NbOutput*(i-1)+Posmean,j+2} = mean(PxData);
                        
                        if isequal(app.ExcludezerosCheckBox.Value,1)
                            NumberPixels(NbOutput*(i-1)+Posmean,j) = NbPxTemp;
                            NumberPixelsExcluded(NbOutput*(i-1)+Posmean,j) = NbPxExTemp;
                        end
                    end
                end
            end
            
            Labels = ['mineral','analysis_type',ElemList];
            
            % Add columns for other selections
            if app.Include_SampleCheckBox.Value
                SampleNames = cell(size(CellData,1),1);
                [SampleNames{:}] = deal(app.SampleNameEditField.Value);
                CellData = [SampleNames,CellData(:,1:end)];
                Labels = ['sample',Labels];
            end
            
            if app.Include_PressureCheckBox.Value
                PressureValues = cell(size(CellData,1),1);
                [PressureValues{:}] = deal(app.PressureGPaEditField.Value);
                CellData = [CellData(:,1:end),PressureValues];
                Labels = [Labels,'pressure_gpa'];
            end
            
            if app.Include_TemperatureCheckBox.Value
                TemperatureValues = cell(size(CellData,1),1);
                [TemperatureValues{:}] = deal(app.TemperatureCEditField.Value);
                CellData = [CellData(:,1:end),TemperatureValues];
                Labels = [Labels,'temperature_degreeC'];
            end
            
            if isequal(app.ExcludezerosCheckBox.Value,1)
                TotalPixelsExcluded = sum(NumberPixelsExcluded,2);
                TotalPixels = sum(NumberPixels,2);
                FractionExcluded = zeros(size(TotalPixels));
                for i = 1:length(TotalPixelsExcluded)
                    if TotalPixels(i) > 0
                        FractionExcluded(i) = TotalPixelsExcluded(i)./TotalPixels(i) .* 100;
                    end
                end
                CellData = [CellData(:,1:end),num2cell(FractionExcluded)];
                Labels = [Labels,'fraction_nul_px_percent'];
            end
            
            app.GeneratedDataCellFormat = CellData;
            app.GeneratedLabelCellFormat = Labels;
            
            close(app.WaitBar)
            
        end
        
        
        
        
        function CalculateBRC(app)
            
            TheNbPx = app.SizeEditField.Value;
            TheNbPxOnGarde = app.ThresholdEditField.Value;
            
            MaskFile = app.XMapToolsApp.XMapToolsData.MapData.MaskFile;
            
            MaskMap = MaskFile.Masks(app.MaskFileDropDown.Value).MaskMap;
            MaskNames = MaskFile.Masks(app.MaskFileDropDown.Value).Names;
            
            % Proceed to the correction
            TheLin = size(MaskMap,1);
            TheCol = size(MaskMap,2);
            %CoordMatrice = reshape([1:TheLin*TheCol],TheLin,TheCol);
            
            TheMaskFinal = zeros(size(MaskMap));
            
            Position = round(TheNbPx/2);
            TheNbPxInSel = TheNbPx^2;
            TheCriterion = TheNbPxInSel*TheNbPxOnGarde/100;
            
            for i=1:length(MaskNames) - 1                % for each phase
                
                TheMask = zeros(size(MaskMap));
                VectorOk = find(MaskMap == i);
                
                TheMask(VectorOk) = ones(size(VectorOk));
                
                TheWorkingMat = zeros(size(TheMask,1)*size(TheMask,2),TheNbPxInSel+1);
                
                VectMask = TheMask(:);
                TheWorkingMat(find(VectMask)) = 1000*ones(size(find(VectMask)));
                
                Compt = 1;
                for iLin = 1:TheNbPx
                    
                    
                    for iCol = 1:TheNbPx
                        
                        % SCAN
                        TheTempMat = zeros(size(TheMask));
                        TheTempMat(Position:end-(Position-1),Position:end-(Position-1)) = TheMask(iLin:end-(TheNbPx-iLin),iCol:end-(TheNbPx-iCol));
                        Compt = Compt+1;
                        TheWorkingMat(:,Compt) = TheTempMat(:);
                        
                    end
                end
                
                TheSum = sum(TheWorkingMat,2);
                OnVire1 = find(TheSum < 1000+TheCriterion & TheSum > 1000);
                TheMaskFinal(OnVire1) = ones(size(OnVire1));
                
            end
            
            app.BRC = ones(size(TheMaskFinal));
            app.BRC(find(TheMaskFinal(:) == 1)) = zeros(length(find(TheMaskFinal(:) == 1)),1);
            
        end
        
        function ModeVal = calc_mode(app,PxData)
            
            % This is the XMapTools version (not working for some LAICPMS
            % data?)
            [N,EDGES] = histcounts(PxData);
            if length(EDGES) < 300 
                [N,EDGES] = histcounts(PxData,300);
            end
            [Val,PosMax] = max(N);
            dPos = (EDGES(2)-EDGES(1)); %/2;
            ModeVal = EDGES(PosMax)+dPos;
            
            
            return
            % Version with kde, slow and unstable. 
            
            [bandwidth,density,xmesh,cdf] = kde(PxData);
            % figure, plot(xmesh,density)
            
            [MaxX,PosMaxX] = max(density);
            ModeVal = xmesh(PosMaxX);
            
            return
            % previous version with mode() below: 
            
            ModeVal = mode(PxData);
            
            if abs((ModeVal - mean(PxData))/ModeVal) > 0.10  % 10% deviation to mean = problem in the mode determination?
%                 
%                 [X,edges] = histcounts(PxData);
%                 [MaxX,PosMaxX] = max(X);
%                 ModeVal = edges(PosMaxX);
%                 
%                 if length(edges) < 500 && length(PxData) > 10000
%                     [X,edges] = histcounts(PxData,500);
%                     [MaxX,PosMaxX] = max(X);
%                     ModeVal = edges(PosMaxX);
%                     disp('Mode: ** histogram method (500)')
%                 
%                 elseif length(edges) < 200 && length(PxData) > 1000
%                     [X,edges] = histcounts(PxData,200);
%                     [MaxX,PosMaxX] = max(X);
%                     ModeVal = edges(PosMaxX);
%                     disp('Mode: ** histogram method (200)')
%                 else
%                     [X,edges] = histcounts(PxData,80);
%                     [MaxX,PosMaxX] = max(X);
%                     ModeVal = edges(PosMaxX);
%                     disp('Mode: ** histogram method (80)')
%                 end
                
                OriginalMode = ModeVal;

                [bandwidth,density,xmesh,cdf] = kde(PxData);
                % figure, plot(xmesh,density)
                
                [MaxX,PosMaxX] = max(density);
                ModeVal = xmesh(PosMaxX);
                
                if ModeVal < 0 
                    ModeVal = 0;
                end
                
                disp(['Mode was ',num2str(OriginalMode),' changed to ',num2str(ModeVal)])
            end
            
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
            
            app.DataExport.Visible = 'off';
            
            movegui(app.DataExport,'center');
            
            app.XMapToolsApp = XMapToolsApp;
            
            app.MaskFileDropDown.Items = app.XMapToolsApp.XMapToolsData.MapData.MaskFile.Names;
            app.MaskFileDropDown.ItemsData = [1:length(app.MaskFileDropDown.Items)];
            
            app.Mode_Merged_Button.Value = 1;
            update_export_source(app);
            
            update_gui_method(app);
            
            CalculateBRC(app);
            
            app.DataExport.Visible = 'on';
            
        end

        % Close request function: DataExport
        function DataExportCloseRequest(app, event)
            
            if ~isempty(app.WaitBar)
                Answer = uiconfirm(gcbf,'Do you want to close the Data Export Module?','Confirm','Options',{'Yes','Cancel (unfreeze)'},'DefaultOption',1,'CancelOption',2,'Icon','question');
                if isequal(Answer,'Cancel (unfreeze)')
                    close(app.WaitBar)
                    return
                end
            end
            delete(app)
        end

        % Value changed function: MaskFileDropDown
        function MaskFileDropDownValueChanged(app, event)
            CalculateBRC(app)
        end

        % Value changed function: ExportFormatDropDown
        function ExportFormatDropDownValueChanged(app, event)
            update_gui_method(app) 
        end

        % Value changed function: Mode_Merged_Button
        function Mode_Merged_ButtonValueChanged(app, event)
            if isequal(app.Mode_Merged_Button.Value,1)
                app.Mode_Quanti_Button.Value = 0;
                app.Mode_Results_Button.Value = 0;
            else
                app.Mode_Quanti_Button.Value = 1;
                app.Mode_Results_Button.Value = 0;
            end
            update_export_source(app);
        end

        % Value changed function: Mode_Quanti_Button
        function Mode_Quanti_ButtonValueChanged(app, event)
            if isequal(app.Mode_Quanti_Button.Value,1)
                app.Mode_Merged_Button.Value = 0;
                app.Mode_Results_Button.Value = 0;
            else
                app.Mode_Merged_Button.Value = 1;
                app.Mode_Results_Button.Value = 0;
            end
            update_export_source(app) 
        end

        % Value changed function: Mode_Results_Button
        function Mode_Results_ButtonValueChanged(app, event)
            if isequal(app.Mode_Results_Button.Value,1)
                app.Mode_Merged_Button.Value = 0;
                app.Mode_Quanti_Button.Value = 0;
            else
                app.Mode_Quanti_Button.Value = 1;
                app.Mode_Merged_Button.Value = 0;
            end
            update_export_source(app) 
        end

        % Button pushed function: GenerateSaveButton
        function GenerateSaveButtonPushed(app, event)
            
            switch app.ExportFormatDropDown.Value
                case 'ThermoFit'
                    uialert(app.DataExport,'The ThermoFit format is not yet available, stay tuned!','XMapTools');
                    app.ExportFormatDropDown.Value = 'MinPlot';
                    return
            end
            
            if isempty(app.SecondaryDropDown.Value)
                uialert(app.DataExport,'No data selected in the drop-down menu','XMapTools');
                return
            end
            
            GenerateCellFromSelectedDataIncludingSubmasks(app);
            
            T = cell2table(app.GeneratedDataCellFormat,'VariableNames',app.GeneratedLabelCellFormat);
            
            
            [Success,Message,MessageID] = mkdir('Exported-MinComp');
            DateStr = char(datestr(now));
            DateStr(find(DateStr == ' ')) = '_'; DateStr(find(DateStr == ':')) = '_';
            ProjectName = ['MinComp_',app.ExportFormatDropDown.Value,'_',DateStr];
            Directory = fullfile(cd,'Exported-MinComp');
            [Success,Message,MessageID] = mkdir(Directory);
            
            
            switch app.ExportFormatDropDown.Value
                
                case 'MinPlot'
                    
                    writetable(T,fullfile(Directory,[ProjectName,'.csv']));
                    
                    uialert(app.DataExport,'The file has been saved in the folder /Exported-MinComp','XMapTools','Icon','success');
                
                case 'Thermofit'
                    %fid = fopen(fullfile(Directory,[ProjectName,'.txt']),'w');
                    %fclose(fid);
            end
            
            
        end

        % Value changed function: SizeEditField
        function SizeEditFieldValueChanged(app, event)
            value = app.SizeEditField.Value;
            isodd = rem(value,2) == 1;
            if ~isodd
                app.SizeEditField.Value = 3;
                uialert(app.DataExport,'This parameter must be an odd value (3, 5, 7, etc.)','XMapTools');
            end
            
        end

        % Button pushed function: PlotButton
        function PlotButtonPushed(app, event)
            
            app.WaitBar = uiprogressdlg(app.DataExport,'Title','XMapTools','Indeterminate','on');
            app.WaitBar.Message = 'Generating plots (can take some time)';
            
            MaskFile = app.XMapToolsApp.XMapToolsData.MapData.MaskFile;
            
            MaskMap = MaskFile.Masks(app.MaskFileDropDown.Value).MaskMap;
            MaskNames = MaskFile.Masks(app.MaskFileDropDown.Value).Names(2:end);
            
            figure
            tiledlayout('flow')
            
            BRC = app.BRC;
            
            for i = 1:length(MaskNames)
                PhaseMap = zeros(size(MaskMap));
                Idx = find(MaskMap == i);
                PhaseMap(Idx) = 1;
                Idx2 = find(MaskMap == i & BRC == 0);
                PhaseMap(Idx2) = 2;
                
                nexttile
                imagesc(PhaseMap), axis image
                title(MaskNames{i})
            end
            
            colormap([1,1,1;0,0,0;1,0,0])
            
            close(app.WaitBar)
            
        end

        % Value changed function: IncludebothMaskSubMasksCheckBox
        function IncludebothMaskSubMasksCheckBoxValueChanged(app, event)
            if isequal(app.IncludebothMaskSubMasksCheckBox.Value,1)
                app.IncludeSubMasksCheckBox.Value = 1;
            end
        end

        % Value changed function: IncludeSubMasksCheckBox
        function IncludeSubMasksCheckBoxValueChanged(app, event)
            if isequal(app.IncludeSubMasksCheckBox.Value,0)
                app.IncludebothMaskSubMasksCheckBox.Value = 0;
            end
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create DataExport and hide until all components are created
            app.DataExport = uifigure('Visible', 'off');
            app.DataExport.Position = [100 100 837 599];
            app.DataExport.Name = 'XMapTools Export Module';
            app.DataExport.CloseRequestFcn = createCallbackFcn(app, @DataExportCloseRequest, true);

            % Create GridLayout
            app.GridLayout = uigridlayout(app.DataExport);
            app.GridLayout.ColumnWidth = {'0.5x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.5x'};
            app.GridLayout.RowHeight = {'0.1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '0.2x', '1.2x', '0.2x'};

            % Create DataSourcePanel
            app.DataSourcePanel = uipanel(app.GridLayout);
            app.DataSourcePanel.TitlePosition = 'centertop';
            app.DataSourcePanel.Title = 'Data Source';
            app.DataSourcePanel.Layout.Row = [2 4];
            app.DataSourcePanel.Layout.Column = [11 21];
            app.DataSourcePanel.FontWeight = 'bold';
            app.DataSourcePanel.FontSize = 11;

            % Create GridLayout2
            app.GridLayout2 = uigridlayout(app.DataSourcePanel);
            app.GridLayout2.ColumnWidth = {'1x', '1x', '1x', '1x', '1x'};
            app.GridLayout2.RowHeight = {'1x'};
            app.GridLayout2.ColumnSpacing = 14;
            app.GridLayout2.RowSpacing = 14;
            app.GridLayout2.Padding = [15 10 15 10];

            % Create Mode_Merged_Button
            app.Mode_Merged_Button = uibutton(app.GridLayout2, 'state');
            app.Mode_Merged_Button.ValueChangedFcn = createCallbackFcn(app, @Mode_Merged_ButtonValueChanged, true);
            app.Mode_Merged_Button.Icon = '236-folder.png';
            app.Mode_Merged_Button.IconAlignment = 'top';
            app.Mode_Merged_Button.Text = 'Merged';
            app.Mode_Merged_Button.FontSize = 10;
            app.Mode_Merged_Button.Layout.Row = 1;
            app.Mode_Merged_Button.Layout.Column = 1;

            % Create Mode_Quanti_Button
            app.Mode_Quanti_Button = uibutton(app.GridLayout2, 'state');
            app.Mode_Quanti_Button.ValueChangedFcn = createCallbackFcn(app, @Mode_Quanti_ButtonValueChanged, true);
            app.Mode_Quanti_Button.Icon = '220-layers.png';
            app.Mode_Quanti_Button.IconAlignment = 'top';
            app.Mode_Quanti_Button.Text = 'Quanti';
            app.Mode_Quanti_Button.FontSize = 10;
            app.Mode_Quanti_Button.Layout.Row = 1;
            app.Mode_Quanti_Button.Layout.Column = 2;

            % Create Mode_Results_Button
            app.Mode_Results_Button = uibutton(app.GridLayout2, 'state');
            app.Mode_Results_Button.ValueChangedFcn = createCallbackFcn(app, @Mode_Results_ButtonValueChanged, true);
            app.Mode_Results_Button.Icon = '321-exit.png';
            app.Mode_Results_Button.IconAlignment = 'top';
            app.Mode_Results_Button.Text = 'Results';
            app.Mode_Results_Button.FontSize = 10;
            app.Mode_Results_Button.Layout.Row = 1;
            app.Mode_Results_Button.Layout.Column = 3;

            % Create GridLayout3
            app.GridLayout3 = uigridlayout(app.GridLayout);
            app.GridLayout3.ColumnWidth = {'1x'};
            app.GridLayout3.RowHeight = {'1x'};
            app.GridLayout3.Padding = [0 15 0 20];
            app.GridLayout3.Layout.Row = [2 4];
            app.GridLayout3.Layout.Column = [2 10];

            % Create Image
            app.Image = uiimage(app.GridLayout3);
            app.Image.Layout.Row = 1;
            app.Image.Layout.Column = 1;
            app.Image.HorizontalAlignment = 'left';
            app.Image.ImageSource = 'logo_xmap_final.png';

            % Create MaskFileOutputFormatPanel
            app.MaskFileOutputFormatPanel = uipanel(app.GridLayout);
            app.MaskFileOutputFormatPanel.TitlePosition = 'centertop';
            app.MaskFileOutputFormatPanel.Title = 'Mask File & Output Format';
            app.MaskFileOutputFormatPanel.Layout.Row = [5 6];
            app.MaskFileOutputFormatPanel.Layout.Column = [2 21];
            app.MaskFileOutputFormatPanel.FontWeight = 'bold';
            app.MaskFileOutputFormatPanel.FontSize = 11;

            % Create GridLayout4
            app.GridLayout4 = uigridlayout(app.MaskFileOutputFormatPanel);
            app.GridLayout4.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout4.RowHeight = {'1x'};
            app.GridLayout4.Padding = [5 5 5 5];

            % Create MaskFileDropDown
            app.MaskFileDropDown = uidropdown(app.GridLayout4);
            app.MaskFileDropDown.ValueChangedFcn = createCallbackFcn(app, @MaskFileDropDownValueChanged, true);
            app.MaskFileDropDown.Layout.Row = 1;
            app.MaskFileDropDown.Layout.Column = [3 7];

            % Create MaskFileDropDownLabel
            app.MaskFileDropDownLabel = uilabel(app.GridLayout4);
            app.MaskFileDropDownLabel.HorizontalAlignment = 'right';
            app.MaskFileDropDownLabel.Layout.Row = 1;
            app.MaskFileDropDownLabel.Layout.Column = [1 2];
            app.MaskFileDropDownLabel.Text = 'Mask File';

            % Create SecondaryDropDownLabel
            app.SecondaryDropDownLabel = uilabel(app.GridLayout4);
            app.SecondaryDropDownLabel.HorizontalAlignment = 'right';
            app.SecondaryDropDownLabel.Layout.Row = 1;
            app.SecondaryDropDownLabel.Layout.Column = [8 9];
            app.SecondaryDropDownLabel.Text = 'Merged';

            % Create SecondaryDropDown
            app.SecondaryDropDown = uidropdown(app.GridLayout4);
            app.SecondaryDropDown.Layout.Row = 1;
            app.SecondaryDropDown.Layout.Column = [10 14];

            % Create DataPanel
            app.DataPanel = uipanel(app.GridLayout);
            app.DataPanel.TitlePosition = 'centertop';
            app.DataPanel.Title = 'Data';
            app.DataPanel.Layout.Row = [7 13];
            app.DataPanel.Layout.Column = [2 11];
            app.DataPanel.FontWeight = 'bold';
            app.DataPanel.FontSize = 11;

            % Create GridLayout5
            app.GridLayout5 = uigridlayout(app.DataPanel);
            app.GridLayout5.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout5.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout5.ColumnSpacing = 5;
            app.GridLayout5.RowSpacing = 5;
            app.GridLayout5.Padding = [5 5 5 5];

            % Create MeanCheckBox
            app.MeanCheckBox = uicheckbox(app.GridLayout5);
            app.MeanCheckBox.Text = 'Mean';
            app.MeanCheckBox.FontSize = 11;
            app.MeanCheckBox.Layout.Row = 3;
            app.MeanCheckBox.Layout.Column = [2 4];

            % Create MedianCheckBox
            app.MedianCheckBox = uicheckbox(app.GridLayout5);
            app.MedianCheckBox.Text = 'Median';
            app.MedianCheckBox.FontSize = 11;
            app.MedianCheckBox.Layout.Row = 2;
            app.MedianCheckBox.Layout.Column = [2 4];

            % Create ModeCheckBox
            app.ModeCheckBox = uicheckbox(app.GridLayout5);
            app.ModeCheckBox.Enable = 'off';
            app.ModeCheckBox.Text = 'Mode';
            app.ModeCheckBox.FontSize = 11;
            app.ModeCheckBox.Layout.Row = 4;
            app.ModeCheckBox.Layout.Column = [2 4];

            % Create ExportedDataLabel
            app.ExportedDataLabel = uilabel(app.GridLayout5);
            app.ExportedDataLabel.Layout.Row = 1;
            app.ExportedDataLabel.Layout.Column = [1 3];
            app.ExportedDataLabel.Text = 'Exported Data:';

            % Create UncertaintiesLabel
            app.UncertaintiesLabel = uilabel(app.GridLayout5);
            app.UncertaintiesLabel.Layout.Row = 4;
            app.UncertaintiesLabel.Layout.Column = [6 9];
            app.UncertaintiesLabel.Text = 'Uncertainties:';

            % Create StandarddeviationCheckBox
            app.StandarddeviationCheckBox = uicheckbox(app.GridLayout5);
            app.StandarddeviationCheckBox.Text = 'Standard deviation';
            app.StandarddeviationCheckBox.FontSize = 11;
            app.StandarddeviationCheckBox.Layout.Row = 5;
            app.StandarddeviationCheckBox.Layout.Column = [7 11];

            % Create BRCCheckBox
            app.BRCCheckBox = uicheckbox(app.GridLayout5);
            app.BRCCheckBox.Text = 'BRC';
            app.BRCCheckBox.FontSize = 11;
            app.BRCCheckBox.Layout.Row = 7;
            app.BRCCheckBox.Layout.Column = [1 2];
            app.BRCCheckBox.Value = true;

            % Create SizeEditFieldLabel
            app.SizeEditFieldLabel = uilabel(app.GridLayout5);
            app.SizeEditFieldLabel.HorizontalAlignment = 'right';
            app.SizeEditFieldLabel.Layout.Row = 7;
            app.SizeEditFieldLabel.Layout.Column = 3;
            app.SizeEditFieldLabel.Text = 'Size';

            % Create SizeEditField
            app.SizeEditField = uieditfield(app.GridLayout5, 'numeric');
            app.SizeEditField.Limits = [1 100];
            app.SizeEditField.ValueChangedFcn = createCallbackFcn(app, @SizeEditFieldValueChanged, true);
            app.SizeEditField.HorizontalAlignment = 'center';
            app.SizeEditField.Layout.Row = 7;
            app.SizeEditField.Layout.Column = 4;
            app.SizeEditField.Value = 3;

            % Create ThresholdEditFieldLabel
            app.ThresholdEditFieldLabel = uilabel(app.GridLayout5);
            app.ThresholdEditFieldLabel.HorizontalAlignment = 'right';
            app.ThresholdEditFieldLabel.Layout.Row = 7;
            app.ThresholdEditFieldLabel.Layout.Column = [5 6];
            app.ThresholdEditFieldLabel.Text = 'Threshold';

            % Create ThresholdEditField
            app.ThresholdEditField = uieditfield(app.GridLayout5, 'numeric');
            app.ThresholdEditField.Limits = [1 100];
            app.ThresholdEditField.HorizontalAlignment = 'center';
            app.ThresholdEditField.Layout.Row = 7;
            app.ThresholdEditField.Layout.Column = [7 8];
            app.ThresholdEditField.Value = 80;

            % Create PlotButton
            app.PlotButton = uibutton(app.GridLayout5, 'push');
            app.PlotButton.ButtonPushedFcn = createCallbackFcn(app, @PlotButtonPushed, true);
            app.PlotButton.Icon = '165-layers.png';
            app.PlotButton.Layout.Row = 7;
            app.PlotButton.Layout.Column = [9 11];
            app.PlotButton.Text = 'Plot';

            % Create MasksSubmasksLabel
            app.MasksSubmasksLabel = uilabel(app.GridLayout5);
            app.MasksSubmasksLabel.Layout.Row = 1;
            app.MasksSubmasksLabel.Layout.Column = [6 9];
            app.MasksSubmasksLabel.Text = 'Masks & Submasks';

            % Create IncludebothMaskSubMasksCheckBox
            app.IncludebothMaskSubMasksCheckBox = uicheckbox(app.GridLayout5);
            app.IncludebothMaskSubMasksCheckBox.ValueChangedFcn = createCallbackFcn(app, @IncludebothMaskSubMasksCheckBoxValueChanged, true);
            app.IncludebothMaskSubMasksCheckBox.Text = 'Include both';
            app.IncludebothMaskSubMasksCheckBox.FontSize = 11;
            app.IncludebothMaskSubMasksCheckBox.Layout.Row = 3;
            app.IncludebothMaskSubMasksCheckBox.Layout.Column = [7 11];

            % Create IncludeSubMasksCheckBox
            app.IncludeSubMasksCheckBox = uicheckbox(app.GridLayout5);
            app.IncludeSubMasksCheckBox.ValueChangedFcn = createCallbackFcn(app, @IncludeSubMasksCheckBoxValueChanged, true);
            app.IncludeSubMasksCheckBox.Text = 'Include submasks';
            app.IncludeSubMasksCheckBox.FontSize = 11;
            app.IncludeSubMasksCheckBox.Layout.Row = 2;
            app.IncludeSubMasksCheckBox.Layout.Column = [7 11];
            app.IncludeSubMasksCheckBox.Value = true;

            % Create ExcludezerosCheckBox
            app.ExcludezerosCheckBox = uicheckbox(app.GridLayout5);
            app.ExcludezerosCheckBox.Text = 'Exclude zeros';
            app.ExcludezerosCheckBox.FontSize = 11;
            app.ExcludezerosCheckBox.Layout.Row = 6;
            app.ExcludezerosCheckBox.Layout.Column = [1 5];

            % Create MetadataPanel
            app.MetadataPanel = uipanel(app.GridLayout);
            app.MetadataPanel.TitlePosition = 'centertop';
            app.MetadataPanel.Title = 'Metadata';
            app.MetadataPanel.Layout.Row = [7 13];
            app.MetadataPanel.Layout.Column = [12 21];
            app.MetadataPanel.FontWeight = 'bold';
            app.MetadataPanel.FontSize = 11;

            % Create GridLayout5_2
            app.GridLayout5_2 = uigridlayout(app.MetadataPanel);
            app.GridLayout5_2.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout5_2.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout5_2.ColumnSpacing = 5;
            app.GridLayout5_2.RowSpacing = 5;
            app.GridLayout5_2.Padding = [5 5 5 5];

            % Create SampleNameEditField
            app.SampleNameEditField = uieditfield(app.GridLayout5_2, 'text');
            app.SampleNameEditField.HorizontalAlignment = 'right';
            app.SampleNameEditField.Layout.Row = 1;
            app.SampleNameEditField.Layout.Column = [4 8];
            app.SampleNameEditField.Value = 'none';

            % Create SampleNameEditFieldLabel
            app.SampleNameEditFieldLabel = uilabel(app.GridLayout5_2);
            app.SampleNameEditFieldLabel.HorizontalAlignment = 'right';
            app.SampleNameEditFieldLabel.Layout.Row = 1;
            app.SampleNameEditFieldLabel.Layout.Column = [1 3];
            app.SampleNameEditFieldLabel.Text = 'Sample Name';

            % Create PressureGPaEditField
            app.PressureGPaEditField = uieditfield(app.GridLayout5_2, 'numeric');
            app.PressureGPaEditField.Limits = [0 50];
            app.PressureGPaEditField.Layout.Row = 2;
            app.PressureGPaEditField.Layout.Column = [5 8];
            app.PressureGPaEditField.Value = 0.5;

            % Create PressureGPaEditFieldLabel
            app.PressureGPaEditFieldLabel = uilabel(app.GridLayout5_2);
            app.PressureGPaEditFieldLabel.HorizontalAlignment = 'right';
            app.PressureGPaEditFieldLabel.Layout.Row = 2;
            app.PressureGPaEditFieldLabel.Layout.Column = [1 4];
            app.PressureGPaEditFieldLabel.Text = 'Pressure (GPa)';

            % Create TemperatureCEditField
            app.TemperatureCEditField = uieditfield(app.GridLayout5_2, 'numeric');
            app.TemperatureCEditField.Limits = [0 2000];
            app.TemperatureCEditField.Layout.Row = 3;
            app.TemperatureCEditField.Layout.Column = [5 8];
            app.TemperatureCEditField.Value = 600;

            % Create TemperatureCEditFieldLabel
            app.TemperatureCEditFieldLabel = uilabel(app.GridLayout5_2);
            app.TemperatureCEditFieldLabel.HorizontalAlignment = 'right';
            app.TemperatureCEditFieldLabel.Layout.Row = 3;
            app.TemperatureCEditFieldLabel.Layout.Column = [1 4];
            app.TemperatureCEditFieldLabel.Text = 'Temperature (°C)';

            % Create PrintAssemblageCheckBox
            app.PrintAssemblageCheckBox = uicheckbox(app.GridLayout5_2);
            app.PrintAssemblageCheckBox.Text = 'Print Assemblage';
            app.PrintAssemblageCheckBox.FontSize = 11;
            app.PrintAssemblageCheckBox.Layout.Row = 5;
            app.PrintAssemblageCheckBox.Layout.Column = [3 7];

            % Create Include_SampleCheckBox
            app.Include_SampleCheckBox = uicheckbox(app.GridLayout5_2);
            app.Include_SampleCheckBox.Text = '';
            app.Include_SampleCheckBox.Layout.Row = 1;
            app.Include_SampleCheckBox.Layout.Column = 9;

            % Create Include_PressureCheckBox
            app.Include_PressureCheckBox = uicheckbox(app.GridLayout5_2);
            app.Include_PressureCheckBox.Text = '';
            app.Include_PressureCheckBox.Layout.Row = 2;
            app.Include_PressureCheckBox.Layout.Column = 9;

            % Create Include_TemperatureCheckBox
            app.Include_TemperatureCheckBox = uicheckbox(app.GridLayout5_2);
            app.Include_TemperatureCheckBox.Text = '';
            app.Include_TemperatureCheckBox.Layout.Row = 3;
            app.Include_TemperatureCheckBox.Layout.Column = 9;

            % Create GenerateSaveButton
            app.GenerateSaveButton = uibutton(app.GridLayout, 'push');
            app.GenerateSaveButton.ButtonPushedFcn = createCallbackFcn(app, @GenerateSaveButtonPushed, true);
            app.GenerateSaveButton.Icon = '022-save-as.png';
            app.GenerateSaveButton.FontSize = 14;
            app.GenerateSaveButton.FontWeight = 'bold';
            app.GenerateSaveButton.Layout.Row = 15;
            app.GenerateSaveButton.Layout.Column = [13 18];
            app.GenerateSaveButton.Text = 'Generate & Save ';

            % Create ExportFormatDropDown
            app.ExportFormatDropDown = uidropdown(app.GridLayout);
            app.ExportFormatDropDown.Items = {'MinPlot', 'ThermoFit'};
            app.ExportFormatDropDown.ValueChangedFcn = createCallbackFcn(app, @ExportFormatDropDownValueChanged, true);
            app.ExportFormatDropDown.Layout.Row = 15;
            app.ExportFormatDropDown.Layout.Column = [6 10];
            app.ExportFormatDropDown.Value = 'MinPlot';

            % Create ExportFormatDropDownLabel
            app.ExportFormatDropDownLabel = uilabel(app.GridLayout);
            app.ExportFormatDropDownLabel.HorizontalAlignment = 'right';
            app.ExportFormatDropDownLabel.Layout.Row = 15;
            app.ExportFormatDropDownLabel.Layout.Column = [3 5];
            app.ExportFormatDropDownLabel.Text = 'Export Format';

            % Show the figure after all components are created
            app.DataExport.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = Data_Export_exported(varargin)

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.DataExport)

            % Execute the startup function
            runStartupFcn(app, @(app)startupFcn(app, varargin{:}))

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.DataExport)
        end
    end
end