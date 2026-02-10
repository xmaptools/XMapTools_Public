classdef Classification_exported < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        Classify                      matlab.ui.Figure
        GridLayout                    matlab.ui.container.GridLayout
        Image                         matlab.ui.control.Image
        AppySaveButton                matlab.ui.control.Button
        ClassesSpinnerLabel           matlab.ui.control.Label
        ClassesSpinner                matlab.ui.control.Spinner
        ClassProportionsPanel         matlab.ui.container.Panel
        GridLayout2                   matlab.ui.container.GridLayout
        UIAxes_Pie                    matlab.ui.control.UIAxes
        ClassificationResidualsPanel  matlab.ui.container.Panel
        GridLayout2_2                 matlab.ui.container.GridLayout
        UIAxes_SumD                   matlab.ui.control.UIAxes
        Panel_3                       matlab.ui.container.Panel
        GridLayout2_3                 matlab.ui.container.GridLayout
        UIAxes                        matlab.ui.control.UIAxes
    end

    
    properties (Access = private)
        XMapToolsApp
        
        ResNumAll
        Resolution
        SumDAll
        Data

    end
    
    methods (Access = private)
        
        function PlotSelectedClassification(app)
            
            Idx = app.ClassesSpinner.Value;
            
            TheMap = reshape(app.ResNumAll(:,Idx-1),app.Resolution(1,:));
            
            imagesc(app.UIAxes,TheMap)
            axis(app.UIAxes,'image')
            colorbar(app.UIAxes)
            colormap(app.UIAxes,[0,0,0;hsv(Idx)])
            caxis(app.UIAxes,[-0.5,Idx+0.5])
            
            app.UIAxes_SumD.Children(1).XData = Idx;
            app.UIAxes_SumD.Children(1).YData = app.SumDAll(Idx-1);
            
            pie(app.UIAxes_Pie,app.Data(Idx-1).Prop)
            colormap(app.UIAxes_Pie,hsv(Idx))
            
        end
        
        function checkorder(app)
            
            ResNumAll = app.ResNumAll;
            
            ResNumAllNew = zeros(size(ResNumAll));
            
            for i = 1:size(ResNumAll,2)
                ColVect = ResNumAll(:,i);
                NewVect = zeros(size(ColVect));
                
                NbClasses = max(ColVect);
                               
                NbPx = zeros(NbClasses,1);
                for j = 1:NbClasses
                    NbPx(j) = length(find(ColVect(:) == j));
                end
                
                [B, Idx] = sort(NbPx,'descend');
                
                %NbPx
                %NbPx(Idx)
                
                for j = 1:NbClasses
                    Where = find(ColVect(:) == j);
                    NewPosition = find(Idx == j);
                    NewVect(Where) = NewPosition;
                    %disp([num2str(j),' was replaced by ',num2str(NewPosition)])
                end
                
                ResNumAllNew(:,i) = NewVect;
                
                app.Data(i).Prop = B;
                
            end
            
            app.ResNumAll = ResNumAllNew;
            
        end
    end
    

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, XMapToolsApp, ResNumAll, Resolution, SumDAll)
                        %
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
            
            app.Classify.Visible = 'off';
            
            app.XMapToolsApp = XMapToolsApp;
            app.ResNumAll = ResNumAll;
            app.Resolution = Resolution;
            app.SumDAll = SumDAll;
            
            checkorder(app);
            
            plot(app.UIAxes_SumD,[2:size(ResNumAll,2)+1],app.SumDAll,'-k','LineWidth',2)
            hold(app.UIAxes_SumD,'on')
            plot(app.UIAxes_SumD,2,app.SumDAll(2),'or','MarkerEdgeColor','r','MarkerFaceColor','w','MarkerSize',8,'LineWidth',3)
            
            app.ClassesSpinner.Value = 2;
            app.ClassesSpinner.Limits = [2,size(ResNumAll,2)+1];
            
            PlotSelectedClassification(app);
            
            movegui(app.Classify,"center");
            app.Classify.Visible = 'on';
            
        end

        % Callback function
        function NbClassesSliderValueChanged(app, event)
            PlotSelectedClassification(app)
        end

        % Value changed function: ClassesSpinner
        function ClassesSpinnerValueChanged(app, event)
            PlotSelectedClassification(app)
        end

        % Button pushed function: AppySaveButton
        function AppySaveButtonPushed(app, event)

            app.XMapToolsApp.ExchangeClassification.ResNum = app.ResNumAll(:,app.ClassesSpinner.Value-1);
            
            close(app.Classify)
            
            %app.XMapToolsApp.ExchangeClassification
            
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create Classify and hide until all components are created
            app.Classify = uifigure('Visible', 'off');
            app.Classify.Position = [100 100 1152 796];
            app.Classify.Name = 'Classification App';

            % Create GridLayout
            app.GridLayout = uigridlayout(app.Classify);
            app.GridLayout.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout.RowHeight = {'1x', '1x', '1x', '0.5x', '1x', '0.5x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout.ColumnSpacing = 5;
            app.GridLayout.RowSpacing = 5;

            % Create Image
            app.Image = uiimage(app.GridLayout);
            app.Image.Layout.Row = [1 3];
            app.Image.Layout.Column = [1 11];
            app.Image.ImageSource = 'logo_xmap_final.png';

            % Create AppySaveButton
            app.AppySaveButton = uibutton(app.GridLayout, 'push');
            app.AppySaveButton.ButtonPushedFcn = createCallbackFcn(app, @AppySaveButtonPushed, true);
            app.AppySaveButton.Icon = '044-repeat.png';
            app.AppySaveButton.FontSize = 14;
            app.AppySaveButton.FontWeight = 'bold';
            app.AppySaveButton.Layout.Row = 7;
            app.AppySaveButton.Layout.Column = [3 9];
            app.AppySaveButton.Text = 'Apply & Save';

            % Create ClassesSpinnerLabel
            app.ClassesSpinnerLabel = uilabel(app.GridLayout);
            app.ClassesSpinnerLabel.HorizontalAlignment = 'right';
            app.ClassesSpinnerLabel.Layout.Row = 5;
            app.ClassesSpinnerLabel.Layout.Column = [3 4];
            app.ClassesSpinnerLabel.Text = 'Classes';

            % Create ClassesSpinner
            app.ClassesSpinner = uispinner(app.GridLayout);
            app.ClassesSpinner.Limits = [1 10];
            app.ClassesSpinner.ValueChangedFcn = createCallbackFcn(app, @ClassesSpinnerValueChanged, true);
            app.ClassesSpinner.Layout.Row = 5;
            app.ClassesSpinner.Layout.Column = [5 9];
            app.ClassesSpinner.Value = 1;

            % Create ClassProportionsPanel
            app.ClassProportionsPanel = uipanel(app.GridLayout);
            app.ClassProportionsPanel.TitlePosition = 'centertop';
            app.ClassProportionsPanel.Title = 'Class Proportions';
            app.ClassProportionsPanel.Layout.Row = [1 8];
            app.ClassProportionsPanel.Layout.Column = [12 23];

            % Create GridLayout2
            app.GridLayout2 = uigridlayout(app.ClassProportionsPanel);
            app.GridLayout2.ColumnWidth = {'1x'};
            app.GridLayout2.RowHeight = {'1x'};
            app.GridLayout2.Padding = [0 0 0 0];

            % Create UIAxes_Pie
            app.UIAxes_Pie = uiaxes(app.GridLayout2);
            app.UIAxes_Pie.PlotBoxAspectRatio = [2.06837606837607 1 1];
            app.UIAxes_Pie.XTick = [];
            app.UIAxes_Pie.YTick = [];
            app.UIAxes_Pie.FontSize = 9;
            app.UIAxes_Pie.Layout.Row = 1;
            app.UIAxes_Pie.Layout.Column = 1;

            % Create ClassificationResidualsPanel
            app.ClassificationResidualsPanel = uipanel(app.GridLayout);
            app.ClassificationResidualsPanel.TitlePosition = 'centertop';
            app.ClassificationResidualsPanel.Title = 'Classification Residuals';
            app.ClassificationResidualsPanel.Layout.Row = [1 8];
            app.ClassificationResidualsPanel.Layout.Column = [24 35];

            % Create GridLayout2_2
            app.GridLayout2_2 = uigridlayout(app.ClassificationResidualsPanel);
            app.GridLayout2_2.ColumnWidth = {'1x'};
            app.GridLayout2_2.RowHeight = {'1x'};
            app.GridLayout2_2.Padding = [0 0 0 0];

            % Create UIAxes_SumD
            app.UIAxes_SumD = uiaxes(app.GridLayout2_2);
            xlabel(app.UIAxes_SumD, 'Nb Classes')
            ylabel(app.UIAxes_SumD, 'Residual')
            app.UIAxes_SumD.PlotBoxAspectRatio = [2.24161073825503 1 1];
            app.UIAxes_SumD.Layout.Row = 1;
            app.UIAxes_SumD.Layout.Column = 1;

            % Create Panel_3
            app.Panel_3 = uipanel(app.GridLayout);
            app.Panel_3.TitlePosition = 'centertop';
            app.Panel_3.Layout.Row = [9 25];
            app.Panel_3.Layout.Column = [1 35];

            % Create GridLayout2_3
            app.GridLayout2_3 = uigridlayout(app.Panel_3);
            app.GridLayout2_3.ColumnWidth = {'1x'};
            app.GridLayout2_3.RowHeight = {'1x'};

            % Create UIAxes
            app.UIAxes = uiaxes(app.GridLayout2_3);
            app.UIAxes.XTick = [];
            app.UIAxes.YTick = [];
            app.UIAxes.Layout.Row = 1;
            app.UIAxes.Layout.Column = 1;

            % Show the figure after all components are created
            app.Classify.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = Classification_exported(varargin)

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.Classify)

            % Execute the startup function
            runStartupFcn(app, @(app)startupFcn(app, varargin{:}))

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.Classify)
        end
    end
end