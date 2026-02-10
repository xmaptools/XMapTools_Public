classdef Signal_Selector_exported < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        SignalSelectorGUI         matlab.ui.Figure
        GridLayout                matlab.ui.container.GridLayout
        Tree                      matlab.ui.container.Tree
        Image                     matlab.ui.control.Image
        ManualselectionPanel      matlab.ui.container.Panel
        GridLayout2               matlab.ui.container.GridLayout
        SelecttimeintervalButton  matlab.ui.control.Button
        NameEditFieldLabel        matlab.ui.control.Label
        NameEditField             matlab.ui.control.EditField
        StartEditFieldLabel       matlab.ui.control.Label
        StartEditField            matlab.ui.control.NumericEditField
        EndEditFieldLabel         matlab.ui.control.Label
        EndEditField              matlab.ui.control.NumericEditField
        AutomatedselectionPanel   matlab.ui.container.Panel
        GridLayout3               matlab.ui.container.GridLayout
        FindAutomaticallyButton   matlab.ui.control.Button
        NbSweepEditFieldLabel     matlab.ui.control.Label
        NbSweepEditField          matlab.ui.control.NumericEditField
        Label                     matlab.ui.control.Label
        PlotButton                matlab.ui.control.Button
        SaveROIsButton            matlab.ui.control.Button
        SigmaEditFieldLabel       matlab.ui.control.Label
        SigmaEditField            matlab.ui.control.NumericEditField
        DeleteButton              matlab.ui.control.Button
        PlotMenuDropDownLabel     matlab.ui.control.Label
        PlotMenuDropDown          matlab.ui.control.DropDown
        ApplyCloseButton          matlab.ui.control.Button
        UIAxes                    matlab.ui.control.UIAxes
    end

    
    properties (Access = private)
        XMapToolsApp
        CallingApp
        Data
        
        ROI
        ROI_Listener
        
        SeqAuto
        ROI_auto
        
        Integrations
        MaxSweep
    end
    
    methods (Access = private)
        
        function DeactivatePlotZoomPanOptions(app)
            if ~isempty(app.UIAxes)
                if ~isempty(app.UIAxes.Toolbar.Children)
                    app.UIAxes.Toolbar.Children(2).Value = 'off';
                    matlab.graphics.interaction.webmodes.toggleMode(app.UIAxes,'pan',app.UIAxes.Toolbar.Children(2).Value)
                    
                    app.UIAxes.Toolbar.Children(3).Value = 'off';
                    matlab.graphics.interaction.webmodes.toggleMode(app.UIAxes,'zoom',app.UIAxes.Toolbar.Children(3).Value)
                    
                    app.UIAxes.Toolbar.Children(4).Value = 'off';
                    matlab.graphics.interaction.webmodes.toggleMode(app.UIAxes,'zoomout',app.UIAxes.Toolbar.Children(4).Value)
                end
            end
        end
        
        function ROI_DeleteROI(app)
            delete(findall(app.UIAxes, 'Type',  'images.roi.Rectangle'));
            delete(findall(app.UIAxes, 'Type',  'images.roi.Polygon'));
            delete(findall(app.UIAxes, 'Type',  'images.roi.Ellipse'));
            delete(findall(app.UIAxes, 'Type',  'images.roi.Circle'));
            delete(findall(app.UIAxes, 'Type',  'images.roi.Point'));
            delete(findall(app.UIAxes, 'Type',  'images.roi.Polyline'));
        end
        
        
        function ROI_Position_Changed(app, ~)
            
            SelectedROI = app.Tree.SelectedNodes.NodeData(1);
            
            [StartSweep, EndSweep] = CheckLimitsROI(app);
            
            app.Integrations.Data(SelectedROI).Interval = [StartSweep,EndSweep];
            app.Integrations.Data(SelectedROI).Position = app.ROI.Position;
            app.Integrations.Data(SelectedROI).XLim = app.UIAxes.XLim;
            app.Integrations.Data(SelectedROI).YLim = app.UIAxes.YLim;
            
            app.StartEditField.Value = StartSweep;
            app.EndEditField.Value = EndSweep;
            
            app.Tree.Children(SelectedROI).Children(1).Text = ['start = ',num2str(StartSweep),' end = ',num2str(EndSweep)];
            
        end
        
        function PlotSelectedData(app)
            if isequal(app.PlotMenuDropDown.Value,0)
                plot(app.UIAxes,app.Data.SumData,'.-','MarkerSize',5);
            else
                plot(app.UIAxes,app.Data.Cps(:,app.PlotMenuDropDown.Value),'.-','MarkerSize',5);
            end
        end
        
        function [StartSweep, EndSweep] = CheckLimitsROI(app)
            
            StartSweep = round(app.ROI.Position(1));
            
            if StartSweep < 1
                Shift = 1-StartSweep;
                StartSweep = 1;
                app.ROI.Position(1) = StartSweep;
                app.ROI.Position(3) = app.ROI.Position(3) - Shift;
            end
            
            EndSweep = round(app.ROI.Position(1) + app.ROI.Position(3));
            
            if EndSweep > app.MaxSweep
                Shift = EndSweep - app.MaxSweep;
                EndSweep = app.MaxSweep;
                app.ROI.Position(3) = app.ROI.Position(3) - Shift;
            end
            
        end
        
        function CheckIntegrationSequence(app)
            
            IntegrationsCheck = app.Integrations;
            
            XPos = zeros(size(IntegrationsCheck.Data));
            
            for i = 1:length(IntegrationsCheck.Data)
                XPos(i) = IntegrationsCheck.Data(i).Position(1);
            end
            
            [Vals,Idx] = sort(XPos);
            
            IntegrationsNew = app.Integrations;
            
            for i = 1:length(Idx)
                IntegrationsNew.Names{i} = IntegrationsCheck.Names{Idx(i)};
                IntegrationsNew.Data(i) = IntegrationsCheck.Data(Idx(i));
            end
            
            app.Integrations = IntegrationsNew;
        end
    end
    

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, CallingApp, XMapToolsApp, Data, Mode, DefNameText)
            
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
            
            app.SignalSelectorGUI.Visible = 'off';
            
            movegui(app.SignalSelectorGUI,"center");
            
            app.XMapToolsApp = XMapToolsApp;
            app.CallingApp = CallingApp;
            app.Data = Data;
            
            switch Mode
                case 'Auto' % not implemented
                    app.ManualselectionPanel.Visible = 'off';
                    app.AutomatedselectionPanel.Visible = 'off';
                case 'Manual'
                    app.ManualselectionPanel.Visible = 'on';
                    app.AutomatedselectionPanel.Visible = 'off';
            end
            
            app.MaxSweep = size(app.Data.Cps,1);
            
            app.ApplyCloseButton.Visible = 'off';
            app.DeleteButton.Visible = 'off';
            app.PlotButton.Visible = 'off';
            app.SaveROIsButton.Visible = 'off';
            app.Label.Visible = 'off';
            
            app.NameEditField.Value = DefNameText;
            
            app.PlotMenuDropDown.Items =  ['SumData',Data.ElName];
            app.PlotMenuDropDown.ItemsData = [0:length(Data.ElName)];
            app.PlotMenuDropDown.Value = 0;
            
            PlotSelectedData(app)
            
            app.UIAxes.YScale = 'log';
            rp = rulerPanInteraction('Dimensions','x');
            app.UIAxes.Interactions = [rp];
            tb = axtoolbar(app.UIAxes,{'export','pan','zoomin','zoomout','restoreview'});
            
            app.Integrations.Names = {};
            app.Integrations.Data(1).Interval = [];
            app.Integrations.Data(1).Position = [];
            app.Integrations.Data(1).XLim = [];
            app.Integrations.Data(1).YLim = [];
            
            app.SignalSelectorGUI.Visible = 'on';

        end

        % Button pushed function: SelecttimeintervalButton
        function SelecttimeintervalButtonPushed(app, event)
            
            ROI_DeleteROI(app);
            
            app.PlotButton.Visible = 'off';
            app.SaveROIsButton.Visible = 'off';
            app.Label.Visible = 'off';
            
            DeactivatePlotZoomPanOptions(app);
            
            PosROI = length(app.Integrations.Names) + 1;
            
            app.ROI = drawrectangle(app.UIAxes,'Color',app.XMapToolsApp.GetROIColor,'InteractionsAllowed','all');
            
            app.Integrations.Names{PosROI} = [app.NameEditField.Value,'_',num2str(PosROI)];
            
            [StartSweep, EndSweep] = CheckLimitsROI(app);
            
            app.Integrations.Data(PosROI).Interval = [StartSweep,EndSweep];
            app.Integrations.Data(PosROI).Position = app.ROI.Position;
            app.Integrations.Data(PosROI).XLim = app.UIAxes.XLim;
            app.Integrations.Data(PosROI).YLim = app.UIAxes.YLim;
            
            app.StartEditField.Value = StartSweep;
            app.EndEditField.Value = EndSweep;
            
            app.ROI_Listener = addlistener(app.ROI, 'ROIMoved', @(varargin)ROI_Position_Changed(app, app.ROI));
            
            p = uitreenode(app.Tree,'Text',app.Integrations.Names{PosROI},'NodeData',[PosROI,0]);
            
            p1 = uitreenode(p,'Text',['start = ',num2str(StartSweep),' end = ',num2str(EndSweep)],'NodeData',[PosROI,1]);
            
            expand(p);
            
            app.Tree.SelectedNodes = p;
            
            app.ROI.Label = app.Integrations.Names{PosROI};
            
            if PosROI > 1
                app.ApplyCloseButton.Visible = 'on';
                app.DeleteButton.Visible = 'on';
            else
                app.ApplyCloseButton.Visible = 'off';
                app.DeleteButton.Visible = 'off';
            end
            
            if isequal(PosROI,1)
                app.AutomatedselectionPanel.Visible = 'on';
            else
                app.AutomatedselectionPanel.Visible = 'off';
            end
            
        end

        % Button pushed function: DeleteButton
        function DeleteButtonPushed(app, event)
            
            ROI_DeleteROI(app);
            
            SelectedNode = app.Tree.SelectedNodes.NodeData;
            
            app.Tree.Children(SelectedNode(1)).delete;
            
            app.Integrations.Names(SelectedNode(1)) = [];
            app.Integrations.Data(SelectedNode(1)) = [];
            
            if isequal(length(app.Integrations.Names),1)
                app.AutomatedselectionPanel.Visible = 'on';
                app.ApplyCloseButton.Visible = 'off';
                app.DeleteButton.Visible = 'off';
            else
                app.AutomatedselectionPanel.Visible = 'off';
                app.DeleteButton.Visible = 'on';
            end
            
            % Adjust NodeData:            
            for i = 1:length(app.Tree.Children)
                app.Tree.Children(i).NodeData = [i,0];
                app.Tree.Children(i).Children(1).NodeData = [i,1];                
            end
            
            app.Tree.SelectedNodes = app.Tree.Children(1);
            
            TreeSelectionChanged(app);
            
        end

        % Value changed function: PlotMenuDropDown
        function PlotMenuDropDownValueChanged(app, event)
            PlotSelectedData(app);
        end

        % Selection changed function: Tree
        function TreeSelectionChanged(app, event)
            
            ROI_DeleteROI(app);
            
            app.PlotButton.Visible = 'off';
            app.SaveROIsButton.Visible = 'off';
            app.Label.Visible = 'off';
            
            DeactivatePlotZoomPanOptions(app);
            
            SelectedROI = app.Tree.SelectedNodes.NodeData(1);
            
            app.ROI = drawrectangle(app.UIAxes,'Color',app.XMapToolsApp.GetROIColor,'InteractionsAllowed','all','Position',app.Integrations.Data(SelectedROI).Position);
            
            app.UIAxes.XLim = app.Integrations.Data(SelectedROI).XLim;
            app.UIAxes.YLim = app.Integrations.Data(SelectedROI).YLim;
            
            app.ROI_Listener = addlistener(app.ROI, 'ROIMoved', @(varargin)ROI_Position_Changed(app, app.ROI));
            
            app.ROI.Label = app.Integrations.Names{SelectedROI};
            
            if length(app.Tree.Children) > 1
                app.DeleteButton.Visible = 'on';
            end
            
        end

        % Button pushed function: ApplyCloseButton
        function ApplyCloseButtonPushed(app, event)
            
            CheckIntegrationSequence(app);
            
            Background.Names = app.Integrations.Names;
            
            for i = 1:length(app.Integrations.Data)
                Background.PositionsOri(i,:) = app.Integrations.Data(i).Interval;
                Background.Times(i,:) = [app.CallingApp.Data.time_DT(app.Integrations.Data(i).Interval(1)),app.CallingApp.Data.time_DT(app.Integrations.Data(i).Interval(2))];
            end
            
            Background.Positions = Background.PositionsOri;
            
            for i = 1:length(app.Integrations.Data)
                Background.Times(i,:) = [app.CallingApp.Data.time_DT(app.Integrations.Data(i).Interval(1)),app.CallingApp.Data.time_DT(app.Integrations.Data(i).Interval(2))];
            end
            
            app.CallingApp.ExchangeSelector = Background;
            
            SignalSelectorGUICloseRequest(app, 1);
            
        end

        % Close request function: SignalSelectorGUI
        function SignalSelectorGUICloseRequest(app, event)
            delete(app)
        end

        % Button pushed function: FindAutomaticallyButton
        function FindAutomaticallyButtonPushed(app, event)
            ROI_DeleteROI(app);
            
            Signal = app.Data.SumData;
            SelectedSignal = Signal(app.Integrations.Data(1).Interval(1):app.Integrations.Data(1).Interval(2));
            
            MeanSignal = mean(SelectedSignal);
            StdSignal = std(SelectedSignal);
            
            Sigma = app.SigmaEditField.Value;
            
            WhereSignal = find(Signal >= MeanSignal-Sigma*StdSignal & Signal <= MeanSignal+Sigma*StdSignal);
            
            Count = 1;
            SeqAuto = [1,0,0];
            
            for i = 1:length(WhereSignal)-1
                
                if isequal(WhereSignal(i+1) - WhereSignal(i),1)
                    SeqAuto(Count,3) = SeqAuto(Count,3) + 1;
                else
                    if SeqAuto(Count,3) >= app.NbSweepEditField.Value
                        SeqAuto(Count,2) = WhereSignal(i);
                        Count = Count + 1;
                        SeqAuto(Count,1) = WhereSignal(i+1);
                    else
                        SeqAuto(Count,1) = WhereSignal(i+1);
                        SeqAuto(Count,3) = 0;
                    end
                end
            end
            
            if isequal(SeqAuto(end,2),0)
                SeqAuto = SeqAuto(1:end-1,:);
            end
            
            app.Label.Text = [num2str(size(SeqAuto,1)),' potential ROI found'];
            
            if size(SeqAuto,1) > 1 
                app.PlotButton.Visible = 'on';
                app.SaveROIsButton.Visible = 'on';
                app.Label.Visible = 'on';
                
                app.UIAxes.XLimMode = 'auto';
                app.UIAxes.YLimMode = 'auto';
                
                app.SeqAuto = SeqAuto;
            end
            
        end

        % Button pushed function: PlotButton
        function PlotButtonPushed(app, event)
            ROI_DeleteROI(app);
            
            Signal = app.Data.SumData;
            
            for i = 1: size(app.SeqAuto,1)
                SelectedSignal = Signal(app.SeqAuto(i,1):app.SeqAuto(i,2));
                SelectedSignalPos = SelectedSignal(find(SelectedSignal));
                if ~isempty(SelectedSignalPos)
                    MinValue = min(SelectedSignalPos);
                else
                    MinValue = 0.1;
                end
                MaxValue = max(SelectedSignalPos);
                Position = [app.SeqAuto(i,1),MinValue,app.SeqAuto(i,2)-app.SeqAuto(i,1),MaxValue-MinValue];
                app.ROI_auto(i).ROI = drawrectangle(app.UIAxes,'Color',app.XMapToolsApp.GetROIColor,'InteractionsAllowed','none','Position',Position);
            end
            
            
        end

        % Button pushed function: SaveROIsButton
        function SaveROIsButtonPushed(app, event)
            
            ROI_DeleteROI(app);
            
            app.PlotButton.Visible = 'off';
            app.SaveROIsButton.Visible = 'off';
            app.Label.Visible = 'off';
            
            for i = 1:length(app.Tree.Children)
                app.Tree.Children(i).delete;
            end
            
            Signal = app.Data.SumData;
            
            for i = 1: size(app.SeqAuto,1)
                
                PosROI = i;
                
                StartSweep = app.SeqAuto(i,1);
                EndSweep = app.SeqAuto(i,2);
                
                SelectedSignal = Signal(app.SeqAuto(i,1):app.SeqAuto(i,2));
                SelectedSignalPos = SelectedSignal(find(SelectedSignal));
                if ~isempty(SelectedSignalPos)
                    MinValue = min(SelectedSignalPos);
                else
                    MinValue = 0.1;
                end
                MaxValue = max(SelectedSignalPos);
                Position = [app.SeqAuto(i,1),MinValue,app.SeqAuto(i,2)-app.SeqAuto(i,1),MaxValue-MinValue];
                
                app.Integrations.Names{PosROI} = ['Auto_',num2str(PosROI)];
                
                app.Integrations.Data(PosROI).Interval = [StartSweep,EndSweep];
                app.Integrations.Data(PosROI).Position = Position;
                app.Integrations.Data(PosROI).XLim = app.UIAxes.XLim;
                app.Integrations.Data(PosROI).YLim = app.UIAxes.YLim;
                
                p = uitreenode(app.Tree,'Text',app.Integrations.Names{PosROI},'NodeData',[PosROI,0]);
                
                p1 = uitreenode(p,'Text',['start = ',num2str(StartSweep),' end = ',num2str(EndSweep)],'NodeData',[PosROI,1]);
                
                expand(p);
                
            end
            
            app.Tree.SelectedNodes = app.Tree.Children(1);
            
            TreeSelectionChanged(app);
            
            if PosROI > 1
                app.ApplyCloseButton.Visible = 'on';
                app.DeleteButton.Visible = 'on';
            else
                app.ApplyCloseButton.Visible = 'off';
                app.DeleteButton.Visible = 'off';
            end
            
            if isequal(PosROI,1)
                app.AutomatedselectionPanel.Visible = 'on';
            else
                app.AutomatedselectionPanel.Visible = 'off';
            end
            
        end

        % Value changed function: NbSweepEditField
        function NbSweepEditFieldValueChanged(app, event)
            if isequal(app.Label.Visible,'on') || isequal(app.Label.Visible,'On')
                FindAutomaticallyButtonPushed(app, 1);
            end
        end

        % Value changed function: SigmaEditField
        function SigmaEditFieldValueChanged(app, event)
            if isequal(app.Label.Visible,'on') || isequal(app.Label.Visible,'On')
                FindAutomaticallyButtonPushed(app, 1);
            end
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create SignalSelectorGUI and hide until all components are created
            app.SignalSelectorGUI = uifigure('Visible', 'off');
            app.SignalSelectorGUI.Position = [100 100 1258 616];
            app.SignalSelectorGUI.Name = 'Signal Selector – XMapTools';
            app.SignalSelectorGUI.CloseRequestFcn = createCallbackFcn(app, @SignalSelectorGUICloseRequest, true);

            % Create GridLayout
            app.GridLayout = uigridlayout(app.SignalSelectorGUI);
            app.GridLayout.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout.ColumnSpacing = 5;
            app.GridLayout.RowSpacing = 5;

            % Create Tree
            app.Tree = uitree(app.GridLayout);
            app.Tree.SelectionChangedFcn = createCallbackFcn(app, @TreeSelectionChanged, true);
            app.Tree.Layout.Row = [5 18];
            app.Tree.Layout.Column = [1 5];

            % Create Image
            app.Image = uiimage(app.GridLayout);
            app.Image.Layout.Row = [1 2];
            app.Image.Layout.Column = [1 9];
            app.Image.ImageSource = 'logo_xmap_final.png';

            % Create ManualselectionPanel
            app.ManualselectionPanel = uipanel(app.GridLayout);
            app.ManualselectionPanel.TitlePosition = 'centertop';
            app.ManualselectionPanel.Title = 'Manual selection';
            app.ManualselectionPanel.Layout.Row = [1 4];
            app.ManualselectionPanel.Layout.Column = [11 17];

            % Create GridLayout2
            app.GridLayout2 = uigridlayout(app.ManualselectionPanel);
            app.GridLayout2.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout2.RowHeight = {'1x', '1x', '1x'};
            app.GridLayout2.ColumnSpacing = 5;
            app.GridLayout2.RowSpacing = 5;
            app.GridLayout2.Padding = [5 5 5 5];

            % Create SelecttimeintervalButton
            app.SelecttimeintervalButton = uibutton(app.GridLayout2, 'push');
            app.SelecttimeintervalButton.ButtonPushedFcn = createCallbackFcn(app, @SelecttimeintervalButtonPushed, true);
            app.SelecttimeintervalButton.Icon = 'XXX_ExportMap.png';
            app.SelecttimeintervalButton.Layout.Row = 2;
            app.SelecttimeintervalButton.Layout.Column = [1 6];
            app.SelecttimeintervalButton.Text = 'Select time interval';

            % Create NameEditFieldLabel
            app.NameEditFieldLabel = uilabel(app.GridLayout2);
            app.NameEditFieldLabel.HorizontalAlignment = 'right';
            app.NameEditFieldLabel.FontSize = 11;
            app.NameEditFieldLabel.Layout.Row = 1;
            app.NameEditFieldLabel.Layout.Column = [1 2];
            app.NameEditFieldLabel.Text = 'Name';

            % Create NameEditField
            app.NameEditField = uieditfield(app.GridLayout2, 'text');
            app.NameEditField.HorizontalAlignment = 'center';
            app.NameEditField.Layout.Row = 1;
            app.NameEditField.Layout.Column = [3 6];
            app.NameEditField.Value = 'Background';

            % Create StartEditFieldLabel
            app.StartEditFieldLabel = uilabel(app.GridLayout2);
            app.StartEditFieldLabel.HorizontalAlignment = 'right';
            app.StartEditFieldLabel.FontSize = 11;
            app.StartEditFieldLabel.Layout.Row = 3;
            app.StartEditFieldLabel.Layout.Column = 1;
            app.StartEditFieldLabel.Text = 'Start';

            % Create StartEditField
            app.StartEditField = uieditfield(app.GridLayout2, 'numeric');
            app.StartEditField.HorizontalAlignment = 'center';
            app.StartEditField.FontSize = 11;
            app.StartEditField.Layout.Row = 3;
            app.StartEditField.Layout.Column = [2 3];

            % Create EndEditFieldLabel
            app.EndEditFieldLabel = uilabel(app.GridLayout2);
            app.EndEditFieldLabel.HorizontalAlignment = 'right';
            app.EndEditFieldLabel.FontSize = 11;
            app.EndEditFieldLabel.Layout.Row = 3;
            app.EndEditFieldLabel.Layout.Column = 4;
            app.EndEditFieldLabel.Text = 'End';

            % Create EndEditField
            app.EndEditField = uieditfield(app.GridLayout2, 'numeric');
            app.EndEditField.HorizontalAlignment = 'center';
            app.EndEditField.Layout.Row = 3;
            app.EndEditField.Layout.Column = [5 6];

            % Create AutomatedselectionPanel
            app.AutomatedselectionPanel = uipanel(app.GridLayout);
            app.AutomatedselectionPanel.TitlePosition = 'centertop';
            app.AutomatedselectionPanel.Title = 'Automated selection';
            app.AutomatedselectionPanel.Layout.Row = [1 4];
            app.AutomatedselectionPanel.Layout.Column = [24 36];

            % Create GridLayout3
            app.GridLayout3 = uigridlayout(app.AutomatedselectionPanel);
            app.GridLayout3.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout3.RowHeight = {'1x', '1x', '1x'};
            app.GridLayout3.ColumnSpacing = 5;
            app.GridLayout3.RowSpacing = 5;
            app.GridLayout3.Padding = [5 5 5 5];

            % Create FindAutomaticallyButton
            app.FindAutomaticallyButton = uibutton(app.GridLayout3, 'push');
            app.FindAutomaticallyButton.ButtonPushedFcn = createCallbackFcn(app, @FindAutomaticallyButtonPushed, true);
            app.FindAutomaticallyButton.Icon = 'XXX_center_round.png';
            app.FindAutomaticallyButton.Layout.Row = 2;
            app.FindAutomaticallyButton.Layout.Column = [1 6];
            app.FindAutomaticallyButton.Text = 'Find Automatically';

            % Create NbSweepEditFieldLabel
            app.NbSweepEditFieldLabel = uilabel(app.GridLayout3);
            app.NbSweepEditFieldLabel.HorizontalAlignment = 'right';
            app.NbSweepEditFieldLabel.FontSize = 11;
            app.NbSweepEditFieldLabel.Layout.Row = 2;
            app.NbSweepEditFieldLabel.Layout.Column = [7 8];
            app.NbSweepEditFieldLabel.Text = 'Nb Sweep';

            % Create NbSweepEditField
            app.NbSweepEditField = uieditfield(app.GridLayout3, 'numeric');
            app.NbSweepEditField.ValueChangedFcn = createCallbackFcn(app, @NbSweepEditFieldValueChanged, true);
            app.NbSweepEditField.HorizontalAlignment = 'center';
            app.NbSweepEditField.FontSize = 11;
            app.NbSweepEditField.Layout.Row = 2;
            app.NbSweepEditField.Layout.Column = 9;
            app.NbSweepEditField.Value = 100;

            % Create Label
            app.Label = uilabel(app.GridLayout3);
            app.Label.HorizontalAlignment = 'center';
            app.Label.Layout.Row = 3;
            app.Label.Layout.Column = [1 7];

            % Create PlotButton
            app.PlotButton = uibutton(app.GridLayout3, 'push');
            app.PlotButton.ButtonPushedFcn = createCallbackFcn(app, @PlotButtonPushed, true);
            app.PlotButton.Layout.Row = 3;
            app.PlotButton.Layout.Column = [8 9];
            app.PlotButton.Text = 'Plot';

            % Create SaveROIsButton
            app.SaveROIsButton = uibutton(app.GridLayout3, 'push');
            app.SaveROIsButton.ButtonPushedFcn = createCallbackFcn(app, @SaveROIsButtonPushed, true);
            app.SaveROIsButton.Layout.Row = 3;
            app.SaveROIsButton.Layout.Column = [10 12];
            app.SaveROIsButton.Text = 'Save ROIs';

            % Create SigmaEditFieldLabel
            app.SigmaEditFieldLabel = uilabel(app.GridLayout3);
            app.SigmaEditFieldLabel.HorizontalAlignment = 'right';
            app.SigmaEditFieldLabel.FontSize = 11;
            app.SigmaEditFieldLabel.Layout.Row = 2;
            app.SigmaEditFieldLabel.Layout.Column = [10 11];
            app.SigmaEditFieldLabel.Text = 'Sigma';

            % Create SigmaEditField
            app.SigmaEditField = uieditfield(app.GridLayout3, 'numeric');
            app.SigmaEditField.ValueChangedFcn = createCallbackFcn(app, @SigmaEditFieldValueChanged, true);
            app.SigmaEditField.HorizontalAlignment = 'center';
            app.SigmaEditField.FontSize = 11;
            app.SigmaEditField.Layout.Row = 2;
            app.SigmaEditField.Layout.Column = 12;
            app.SigmaEditField.Value = 5;

            % Create DeleteButton
            app.DeleteButton = uibutton(app.GridLayout, 'push');
            app.DeleteButton.ButtonPushedFcn = createCallbackFcn(app, @DeleteButtonPushed, true);
            app.DeleteButton.Icon = '057-minus.png';
            app.DeleteButton.Layout.Row = 4;
            app.DeleteButton.Layout.Column = [1 3];
            app.DeleteButton.Text = 'Delete';

            % Create PlotMenuDropDownLabel
            app.PlotMenuDropDownLabel = uilabel(app.GridLayout);
            app.PlotMenuDropDownLabel.HorizontalAlignment = 'right';
            app.PlotMenuDropDownLabel.Layout.Row = 5;
            app.PlotMenuDropDownLabel.Layout.Column = [28 30];
            app.PlotMenuDropDownLabel.Text = 'Plot Menu';

            % Create PlotMenuDropDown
            app.PlotMenuDropDown = uidropdown(app.GridLayout);
            app.PlotMenuDropDown.ValueChangedFcn = createCallbackFcn(app, @PlotMenuDropDownValueChanged, true);
            app.PlotMenuDropDown.Layout.Row = 5;
            app.PlotMenuDropDown.Layout.Column = [31 36];

            % Create ApplyCloseButton
            app.ApplyCloseButton = uibutton(app.GridLayout, 'push');
            app.ApplyCloseButton.ButtonPushedFcn = createCallbackFcn(app, @ApplyCloseButtonPushed, true);
            app.ApplyCloseButton.Icon = '044-repeat.png';
            app.ApplyCloseButton.IconAlignment = 'top';
            app.ApplyCloseButton.FontWeight = 'bold';
            app.ApplyCloseButton.Layout.Row = [2 3];
            app.ApplyCloseButton.Layout.Column = [19 22];
            app.ApplyCloseButton.Text = 'Apply & Close';

            % Create UIAxes
            app.UIAxes = uiaxes(app.GridLayout);
            xlabel(app.UIAxes, 'Sweep')
            ylabel(app.UIAxes, 'Intensity')
            app.UIAxes.PlotBoxAspectRatio = [2.67268041237113 1 1];
            app.UIAxes.FontSize = 9;
            app.UIAxes.Layout.Row = [6 18];
            app.UIAxes.Layout.Column = [6 36];

            % Show the figure after all components are created
            app.SignalSelectorGUI.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = Signal_Selector_exported(varargin)

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.SignalSelectorGUI)

            % Execute the startup function
            runStartupFcn(app, @(app)startupFcn(app, varargin{:}))

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.SignalSelectorGUI)
        end
    end
end