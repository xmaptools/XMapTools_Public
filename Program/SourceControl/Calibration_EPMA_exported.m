classdef Calibration_EPMA_exported < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        XMapToolsGlobalStandardization  matlab.ui.Figure
        GridLayout                      matlab.ui.container.GridLayout
        Image                           matlab.ui.control.Image
        GridLayout2                     matlab.ui.container.GridLayout
        Button_help                     matlab.ui.control.Button
        Button_CopyTable                matlab.ui.control.Button
        GridLayout3                     matlab.ui.container.GridLayout
        AppyStandardization             matlab.ui.control.Button
        ApplyToAllMaks                  matlab.ui.control.CheckBox
        UITable                         matlab.ui.control.Table
        Tree                            matlab.ui.container.Tree
        GridLayout5                     matlab.ui.container.GridLayout
        AIparametersDropDown            matlab.ui.control.DropDown
        AIparametersDropDownLabel       matlab.ui.control.Label
        AdjustManualButton              matlab.ui.control.Button
        SlopeManualLabel                matlab.ui.control.Label
        SlopeManual                     matlab.ui.control.NumericEditField
        BackgroundManualLabel           matlab.ui.control.Label
        BackgroundManual                matlab.ui.control.NumericEditField
        LegendElistheelementLabel       matlab.ui.control.Label
        TextArea                        matlab.ui.control.TextArea
        Plot                            matlab.ui.control.UIAxes
        Map_Total                       matlab.ui.control.UIAxes
        Map_Spec                        matlab.ui.control.UIAxes
    end

    
    properties (Access = private)
        XMapToolsID            % to access the data
        MaskFile
        Standards
        Fit
        Calib
        
        PlotData 
        
        Debug
        XMapToolsApp
        
        WaitBar
        
        CatchError
    end
    
    properties (Access = public)
        ExchangeSelector
        
    end
    
    methods (Access = private)
        
        function FitAll(app)
            
            if app.Debug
                disp('***************************************************************************')
                disp('***************************************************************************')
                disp(' ')
                disp('>> FitAll GENERAL FIT started...')
            end
            
            % (1) Spots and Mask file
            XY = app.Standards.XY;
            Masks = zeros(size(XY,1),1);
            MapSizeForCheck = size(app.MaskFile.MaskMap);
            
            for i = 1:size(XY,1)
                if XY(i,2) > MapSizeForCheck(1) || XY(i,1) > MapSizeForCheck(2)
                    uialert(app.XMapToolsID.XMapTools_GUI,[app.Standards.Labels{i},' is outside the map boundaries and must be eliminated'],'Error')
                    app.CatchError = 1;
                    return
                end
                Masks(i) = app.MaskFile.MaskMap(XY(i,2),XY(i,1));
            end
            
            % (2) General Background and Slope fit
            DataIt = app.Standards.DataIt;
            Err2s = 2./sqrt(DataIt).*DataIt;
            DataPro = app.Standards.DataPro;
            ElMap = app.Standards.ElMap;
            ElQuanti = app.Standards.ElemImport;
            
            % We directly consider all matching elements
            DataStdIdx = find(~ismember(ElMap,'none'));
            
            Calibrate = zeros(size(DataStdIdx,2),1);
            Gen_back  = zeros(size(DataStdIdx,2),1);
            Gen_slope = zeros(size(DataStdIdx,2),1);
            
            app.PlotData.GenCalib(1).Map = {};
            app.PlotData.GenCalib(1).p = [];
            app.PlotData.GenCalib(1).s = [];
            
            for i = 1:length(DataStdIdx)
                
                Pos = DataStdIdx(i);
                
                if app.Debug
                    disp(' '),disp(' ')
                    disp(['  #####################################################'])
                    disp(['  Element: ',ElMap{Pos}])
                end
                
                %app.PlotData.GenCalib(i).Map = ElMap{Pos};
                
                Calibrate(i) = 1;
                
                X = DataPro(:,Pos);
                Y = DataIt(:,Pos);
                E = Err2s(:,Pos);
                
                app.PlotData.GenData(i).Xori = X;
                app.PlotData.GenData(i).Yori = Y;
                app.PlotData.GenData(i).Eori = E;
                
                Xori = X;
                Yori = Y;
                Eori = E;
                
                %figure(1), errorbar(X,Y,E,'o');
                
                %Ax = gca;
                
                %hold on
                
                % first fit all data
                [p,s] = polyfit(X,Y,1);
                
                % xi = linspace(0,max(X));
                % plot(xi,polyval(p,xi),'-k')
                
                if app.Debug
                    disp(['  Nb std: ',num2str(length(X))])
                    disp(['  Polyfit p1: ',num2str(p(1)),' | ',num2str(p(2))])
                end
                
                % Outlier Rejection ---------------------------------------
                % Temporary for testing
                % max 20 % otherwise one phase can be excluded
                %
                % Maybe in the futur implement rejection based on the k
                % values... (same range than afterward).
                %
                if 1
                    Max4NormX = max(X);
                    Max4NormY = max(Y+E);
                    
                    deltaY = (Y-(p(1)*X+p(2)))/Max4NormY;
                    deltaX = (X-((Y-p(2))/p(1)))/Max4NormX;
                    DistXY = sqrt(1./(1./deltaX.^2 + 1./deltaY.^2));
                    
                    Out1 = isoutlier(DistXY,'median');
                    
                    PosOutlier = find(Out1);
                    
                    if ~isempty(PosOutlier) && length(PosOutlier)<0.2*numel(X)  % otherwise one group can be out
                        %hold on, errorbar(X(PosOutlier),Y(PosOutlier),E(PosOutlier),'or'), hold off
                        PosSel = find(Out1 == 0);
                        X = X(PosSel);
                        Y = Y(PosSel);
                        E = E(PosSel);
                        
                        [p,s] = polyfit(X,Y,1);
                    end
                    
                    %keyboard
                end
                
                if app.Debug
                    disp(['  First outlier rejection test passed (',num2str(length(PosOutlier)),' outliers found)'])
                    disp(['  Polyfit p1: ',num2str(p(1)),' | ',num2str(p(2))])
                    disp(['   -----------------------------------------'])
                    disp(['       X         Y         E       Outlier'])
                    disp([Xori,Yori,Eori,Out1])
                    disp(['   -----------------------------------------'])
                end
                
                %plot([0,Ax.XLim(2)],polyval(p,[0,Ax.XLim(2)]),'-r')
                
                if p(1) > 0 && p(2) > 0
                    Gen_back(i) = p(2);
                    Gen_slope(i) = p(1);
                    
                    app.PlotData.GenCalib(i).p = p;
                    app.PlotData.GenCalib(i).s = s;
                    
                    app.PlotData.GenData(i).X = X;
                    app.PlotData.GenData(i).Y = Y;
                    app.PlotData.GenData(i).E = E;
                else
                    
                    if p(1) < 0
                        % In this case the slope is negative because the
                        % element is below detection limit... 
                        Count = 0;
                        BackTemp = median(Y)-0.1*median(Y);
                        [p2,s2] = polyfix(app,X,Y,1,0,BackTemp);
                        Count = Count+1;
                        if p2(1) < 0
                            BackTemp = median(Y)-0.3*median(Y);
                            [p2,s2] = polyfix(app,X,Y,1,0,BackTemp);
                            Count = Count+1;
                            if p2(1) < 0
                                BackTemp = median(Y)-0.5*median(Y);
                                [p2,s2] = polyfix(app,X,Y,1,0,1);
                                Count = Count+1;
                            end
                        end
                        if app.Debug
                            disp(['  Warning: negative slope was found!'])
                            disp(['  ** ',num2str(Count),' backround reduction stage (median - 10/30/50%)'])
                            disp(['  Polyfit p2: ',num2str(p2(1)),' | ',num2str(p2(2))])
                        end
                        
                    else    
                        [p2,s2] = polyfix(app,X,Y,1,0,1);  % this is the proper way
                        
                        if app.Debug
                            disp(['  Warning: negative intercept was found'])
                            disp(['  ** new anchored fit (background = 1)'])
                            disp(['  Polyfit p2: ',num2str(p2(1)),' | ',num2str(p2(2))])
                        end
                    end
                    
                    %[p2,s2] = polyfit([X;zeros(100*numel(X),1)],[Y;zeros(100*numel(X),1)],1);
                    %plot([0,Ax.XLim(2)],polyval(p2,[0,Ax.XLim(2)]),'-b')
                    
                    Gen_back(i) = p2(2);
                    Gen_slope(i) = p2(1);
                    
                    app.PlotData.GenCalib(i).p = p2;
                    app.PlotData.GenCalib(i).s = s2;
                    
                    app.PlotData.GenData(i).X = X;
                    app.PlotData.GenData(i).Y = Y;
                    app.PlotData.GenData(i).E = E;
                    
                    if app.Debug
                        disp(' ')
                        disp(['  General fit for element ',ElMap{Pos},' (slope: ',num2str(p2(1)),' | background: ',num2str(p2(2))])
                        
                    end
                end
                %title([ElMap{i},' Back = ',num2str(Gen_back(i))])
                
                
            end
            
            app.Fit.DataStdIdx = DataStdIdx;
            app.Fit.Calibrate = Calibrate';
            app.Fit.Idx = find(Calibrate)';
            app.Fit.Gen_back = Gen_back';
            app.Fit.Gen_slope = Gen_slope';
            
            
            if app.Debug
                disp('***************************************************************************')
                disp('***************************************************************************')
                disp(' ')
                disp('>> FitAll SPECIFIC FIT started...')
            end
            
            % Specific fit
            
            app.Fit.Spec_back = zeros(app.MaskFile.NbMasks-1,length(DataStdIdx));
            app.Fit.Spec_slope = zeros(app.MaskFile.NbMasks-1,length(DataStdIdx));
            
            app.Fit.Spec_minC = zeros(app.MaskFile.NbMasks-1,length(DataStdIdx));
            app.Fit.Spec_maxC = zeros(app.MaskFile.NbMasks-1,length(DataStdIdx));
            app.Fit.Spec_numNonZero= zeros(app.MaskFile.NbMasks-1,length(DataStdIdx));
            
            app.PlotData.MinNames = app.MaskFile.Names(2:end);
            app.PlotData.ElNames = app.Standards.ElMap(DataStdIdx);
            app.PlotData.QElnames = ElQuanti(DataStdIdx);
            
            app.PlotData.Mineral(1).Name = '';
            app.PlotData.Mineral(1).El(1).Map = '';
            app.PlotData.Mineral(1).El(1).MapCheck = '';
            app.PlotData.Mineral(1).El(1).X = [];
            app.PlotData.Mineral(1).El(1).Y = [];
            app.PlotData.Mineral(1).El(1).E = [];
            app.PlotData.Mineral(1).El(1).Xout = [];
            app.PlotData.Mineral(1).El(1).Yout = [];
            app.PlotData.Mineral(1).El(1).Eout = [];
            app.PlotData.Mineral(1).El(1).p = [];
            app.PlotData.Mineral(1).El(1).s = [];
            app.PlotData.Mineral(1).El(1).p_final = [];
            
            for i = 1:app.MaskFile.NbMasks-1
                
                StdIdx = find(Masks == i);
                app.PlotData.Mineral(i).Name = app.PlotData.MinNames{i};
                
                if app.Debug
                    disp(' '),disp(' ')
                    disp(['  -----------------------------------------------------'])
                    disp(['  Mineral: ',app.PlotData.Mineral(i).Name])
                    disp(['  Nb std: ',num2str(length(StdIdx))])
                    
                    disp(' ')
                end
                
                if ~isempty(StdIdx)
                    if app.Debug
                        disp(['  >> Specific fit started...'])
                    end
                    
                    for j = 1:length(DataStdIdx)
                        
                        Pos = DataStdIdx(j);
                        
                        X = DataPro(StdIdx,Pos);
                        
                        % Check to avoid zero which return p = [inf, nan];
                        WhereZeros = find(X == 0);
                        if ~isempty(WhereZeros)
                            X(WhereZeros) = 1e-5*ones(size(WhereZeros));
                        end
                        
                        if app.Debug
                            disp(['    ##############'])
                            disp(['     Element: ',char(app.PlotData.ElNames{j})])
                            disp(['     ** zeros check passed: ',num2str(length(WhereZeros)),' std values of zeros replaced by 1e-5'])
                        end
                        
                        Y = DataIt(StdIdx,Pos);
                        E = Err2s(StdIdx,Pos);
                        
                        
                        % First test of potential outlier rejection
                        % ideally at the end implement up to 20 % rejection
                        TFx = isoutlier(X);
                        TFy = isoutlier(Y);
                        WhereTFxy = find(TFx > 0 & TFy > 0);
                        WhereTFok = find(TFx == 0 & TFy == 0);
                        
                        if length(WhereTFxy) < length(X)*0.2  % max 20 % of the standards
                            X_Temp = X(WhereTFok);
                            Y_Temp = Y(WhereTFok);
                        else
                            X_Temp = X;
                            Y_Temp = Y;
                        end
                        
                        if app.Debug
                            disp(['     ** First outlier rejection level (applied to X and Y) passed: ',num2str(length(WhereTFxy)),'/',num2str(length(X)),' rejected'])
                        end
                        
                        
                        % FIRST POLYFIT
                        warning('off')
                        [pf,sf] = polyfit(X_Temp,Y_Temp,1);
                        warning('on')
                        [p,s] = polyfix(app,X_Temp,Y_Temp,1,0,Gen_back(j));
                        
                        % Check if polyfit is better
                        k_pf = pf(1)/Gen_slope(j);
                        k_p = p(1)/Gen_slope(j);
                        
                        if pf(2) < Gen_back(j) && pf(2) > 0 && abs(k_pf-1) < abs(k_p-1) && sf.normr < s.normr
                            % Then the polyfit is better from all
                            % perspectives
                            
                            p = pf;
                            s = sf;
                            
                            Back_j = p(2);
                            
                            if app.Debug
                                disp(['     ** First polyfit passed: ',num2str(p(1)),' | ',num2str(p(2)),' (Gen_back was ',num2str(Gen_back(j)),')'])
                            end
                        else
                            Back_j = Gen_back(j);
                            if app.Debug
                                disp(['     ** First polyfix passed: ',num2str(p(1)),' | ',num2str(p(2))])
                            end
                        end
                        
                        
                        
                        
                        % Outlier Rejection --------------------------------------
                        % Temporary for testing
                        Xout = [];
                        Yout = [];
                        Eout = [];
                        if length(X) > 2
                            %figure(1), hold off, errorbar(X,Y,E,'ok'), axis([0 max(X),0,max(Y+E)]), hold on
                            
                            Max4NormX = max(X);
                            Max4NormY = max(Y+E); 
                            
                            deltaY = (Y-(p(1)*X+p(2)))/Max4NormY;
                            deltaX = (X-((Y-p(2))/p(1)))/Max4NormX;
                            DistXY = sqrt(1./(1./deltaX.^2 + 1./deltaY.^2));
                            
                            %Out1 = isoutlier(DistXY,'median');
                            
                            % version PL (4 sigma)
                            Out1 = zeros(size(DistXY));
                            WhereOut = find(DistXY > 2*E/Max4NormY);
                            if ~isempty(WhereOut) && length(WhereOut)/length(DistXY) < 0.33 % We can exclude more than 1/3
                                Out1(WhereOut) = ones(size(WhereOut));  
                            end
                            
                            % this below works for the two versions above 
                            PosOutlier = find(Out1); 
                            
                            if ~isempty(PosOutlier)
                                %hold on, errorbar(X(PosOutlier),Y(PosOutlier),E(PosOutlier),'or')
                                Xout = X(PosOutlier);
                                Yout = Y(PosOutlier);
                                Eout = E(PosOutlier);
                                
                                PosSel = find(Out1 == 0);
                                X = X(PosSel);
                                Y = Y(PosSel);
                                E = E(PosSel);
                                
                                [p,s] = polyfix(app,X,Y,1,0,Back_j);  % here we use Back_j instead of Gen_back as it can be different
                            end
                            if app.Debug
                                disp(['     ** Second outlier rejection level (distance) passed: ',num2str(length(PosOutlier)),'/',num2str(length(X)),' rejected'])
                            end
                            %keyboard
                        end
                        
                        
                        %figure,
                        %errorbar(X,Y,E,'o'), hold on
                        %Ax = gca;
                        %plot([0,Ax.XLim(2)],polyval(p,[0,Ax.XLim(2)]),'-r')
                        %plot([0,Ax.XLim(2)],polyval([app.Fit.Gen_slope(j),app.Fit.Gen_back(j)],[0,Ax.XLim(2)]),'-b')
                        %title([ElMap{j},' in ',app.MaskFile.Names{i+1}])
                        
                        app.Fit.Spec_back(i,j)  = p(2);
                        app.Fit.Spec_slope(i,j)  = p(1);
                        
                        %if isempty(X)
                            %keyboard
                        %end
                        
                        app.Fit.Spec_minC(i,j)  = min(X);
                        app.Fit.Spec_maxC(i,j)  = max(X);
                        app.Fit.Spec_numNonZero(i,j) = numel(find(X ~= 0 & Y ~= 0));
                        
                        app.PlotData.Mineral(i).El(j).Map = app.PlotData.ElNames{j};
                        app.PlotData.Mineral(i).El(j).MapCheck = ElMap{Pos};
                        app.PlotData.Mineral(i).El(j).X = X;
                        app.PlotData.Mineral(i).El(j).Y = Y;
                        app.PlotData.Mineral(i).El(j).E = E;
                        app.PlotData.Mineral(i).El(j).Xout = Xout;
                        app.PlotData.Mineral(i).El(j).Yout = Yout;
                        app.PlotData.Mineral(i).El(j).Eout = Eout;
                        app.PlotData.Mineral(i).El(j).p = p;
                        app.PlotData.Mineral(i).El(j).s = s;
                    end
                else
                    if app.Debug
                        disp(['  ** Specific fit impossible (no std)'])
                    end
                    for j = 1:length(DataStdIdx)
                        Pos = DataStdIdx(j);
                        app.PlotData.Mineral(i).El(j).Map = app.PlotData.ElNames{j};
                        app.PlotData.Mineral(i).El(j).MapCheck = ElMap{Pos};
                        app.PlotData.Mineral(i).El(j).X = [];
                        app.PlotData.Mineral(i).El(j).Y = [];
                        app.PlotData.Mineral(i).El(j).E = [];
                        app.PlotData.Mineral(i).El(j).Xout = [];
                        app.PlotData.Mineral(i).El(j).Yout = [];
                        app.PlotData.Mineral(i).El(j).Eout = [];
                        app.PlotData.Mineral(i).El(j).p = [];
                        app.PlotData.Mineral(i).El(j).s = [];
                    end
                end
                
                
            end
            
            app.Fit.Spec_k = app.Fit.Spec_slope./repmat(app.Fit.Gen_slope,size(app.Fit.Spec_slope,1),1);    
                
            WhereZeros = find(app.Fit.Spec_k == 0);
            app.Fit.Spec_k(WhereZeros) = zeros(size(WhereZeros));
            
            SmartCalibration(app);
            
        end
        
        
        function SmartCalibration(app)
            
            %  ---------------------------------------------------
            %   Type    Description
            %  ---------------------------------------------------
            %   1       Spec (slope)       
            %   2       Spec (slope + background)
            %
            %  ---------------------------------------------------
            
            
            if app.Debug
                disp(' ')
                disp(' ')
                disp('***************************************************************************')
                disp('***************************************************************************')
                disp(' ')
                disp('>> SmartCalibration started...')
            end
            
            app.Calib.Back = zeros(size(app.Fit.Spec_k));
            app.Calib.Slope = zeros(size(app.Fit.Spec_k));
            app.Calib.k_final = zeros(size(app.Fit.Spec_k));
            
            for i = 1:size(app.Calib.Back,1)
            
                if app.Debug
                    disp(' '),disp(' ')
                    disp(['  -----------------------------------------------------'])
                    disp(['  Mineral: ',app.PlotData.Mineral(i).Name])
                end
                
                for j = 1:size(app.Calib.Back,2)
                    
                    Spec_slope = app.Fit.Spec_slope(i,j);
                    Spec_back = app.Fit.Spec_back(i,j);
                    
                    Gen_slope = app.Fit.Gen_slope(1,j);
                    Gen_back = app.Fit.Gen_back(1,j);
                    
                    k = app.Fit.Spec_k(i,j);
                    
                    NbSpots = numel(app.PlotData.Mineral(i).El(j).X);
                    if NbSpots >= 1
                        X_mean = mean(app.PlotData.Mineral(i).El(j).X);
                        X_med = median(app.PlotData.Mineral(i).El(j).X);
                        Y_mean = mean(app.PlotData.Mineral(i).El(j).Y);
                        Y_med = median(app.PlotData.Mineral(i).El(j).Y);
                    else
                        X_mean = 0;  % otherwise this fails when no standard is available for the first phase in the list...
                        X_med = 0;
                        Y_mean = 0;
                        Y_med = 0;
                    end
                    
                    if app.Debug
                            disp(['    ##############'])
                            disp(['     Element: ',char(app.PlotData.ElNames{j})])
                            disp([' '])
                            disp(['     GENERAL fit:  ',num2str(Gen_slope),' | ',num2str(Gen_back)])
                            disp(['     SPECIFIC fit: ',num2str(Spec_slope),' | ',num2str(Spec_back)])
                            disp(['     k:            ',num2str(k)])
                            disp(['     mean(X):      ',num2str(X_mean)])
                            disp(['     median(X):    ',num2str(X_med)])
                            disp(['     mean(Y):      ',num2str(Y_mean)])
                            disp(['     median(Y):    ',num2str(Y_med)])
                            disp([' '])
                    end
                    
                    % ------------------------
                    % General check (non-zero)
                    
                    if isequal(Spec_slope,0)
                        app.Calib.Slope(i,j) = Gen_slope;
                        if app.Debug
                            disp(['     ** Slope value initially taken from GENERAL (',num2str(Gen_slope),')'])
                        end
                    else
                        app.Calib.Slope(i,j) = Spec_slope;
                        if app.Debug
                            disp(['     ** Slope value initially taken from SPECIFIC (',num2str(Spec_slope),')'])
                        end
                    end
                    
                    if isequal(Spec_back,0)
                        app.Calib.Back(i,j) = Gen_back;
                        if app.Debug
                            disp(['     ** Background value initially taken from GENERAL (',num2str(Gen_back),')'])
                        end
                    else
                        app.Calib.Back(i,j) = Spec_back;
                        if app.Debug
                            disp(['     ** Background value initially taken from SPECIFIC (',num2str(Spec_back),')'])
                        end
                    end
                    
                    
                    % ------------
                    % Check: SLOPE
                    
                    % (1) Negative slope in Spec we take Gen
                    if Spec_slope < 0
                        app.Calib.Slope(i,j) = Gen_slope;
                        if app.Debug
                            disp(['     ** Warning: negative slope value for SPECIFIC => value from GENERAL taken (',num2str(Gen_slope),')'])
                        end
                    end
                    
                    % (2) k factor below & above threshold (contamination in It)
                    if k > 4 || k < 0.25
                        app.Calib.Slope(i,j) = Gen_slope;
                        if app.Debug
                            disp(['     ** Warning: k outside [0.5,3] range => value from GENERAL taken  (',num2str(Gen_slope),')'])
                        end
                    end
                    
                    
                    
                    % -----------
                    % Check: BACK
                    
                    % (1) BACK lower that BACK_spec (low-concentrated elements)
                    if NbSpots > 5
                        if Y_med < Gen_back    % && Y_mean < Gen_back  % not sure one outlier can fuck up the check
                            % We do not check the value! Compatible with
                            % low and high count rates
                            if Y_med < app.Calib.Back(i,j)  % Background can be from SPEC
                                
                                app.Calib.Back(i,j) = Y_med;
                                
                                if app.Debug
                                    disp(['     ** Warning: Background values are too high => median(Y) taken  (',num2str(app.Calib.Back(i,j)),'; median(Y) < SPEC & GEN)'])
                                end
                            end
                        end
                        
                        TdX = 0.1;
                        dYforTdX = app.Calib.Slope(i,j)*TdX; 
                        if X_med < TdX && Y_med > Gen_back && dYforTdX < 10
                            app.Calib.Back(i,j) = Y_med; 
                            if app.Debug
                                disp(['     ** Warning: Background values are too high => median(Y) taken  (',num2str(app.Calib.Back(i,j)),'; TdX = 0.1 filter for low element content)'])
                            end
                        end
                        
                        TdX = 0.02;
                        dYforTdX = app.Calib.Slope(i,j)*TdX; 
                        if X_med < TdX && Y_med > Gen_back && dYforTdX < 10
                            app.Calib.Back(i,j) = Y_med;
                            if app.Debug
                                disp(['     ** Warning: Background values are too high => median(Y) taken  (',num2str(app.Calib.Back(i,j)),'; TdX = 0.2 filter for low element content)'])
                            end
                        end
                        
                    end
                    
                    
                    % -----
                    % FINAL
                    
                    app.PlotData.Mineral(i).El(j).p_final = [app.Calib.Slope(i,j),app.Calib.Back(i,j)];
                    app.Calib.k_final(i,j) = app.Calib.Slope(i,j)/Gen_slope;
                    
                    if app.Debug
                        disp(' ')
                        disp(['     Final calib: ',num2str(app.PlotData.Mineral(i).El(j).p_final(1)),' | ',num2str(app.PlotData.Mineral(i).El(j).p_final(2))])
                        disp(['     k:           ',num2str(app.Calib.k_final(i,j))])
                        disp(' ')
                    end
                end
                
            end
            
        end
        
        
        function PlotAll(app)
            
            NodeData = app.Tree.SelectedNodes.NodeData;
            
            DataStdIdx = app.Fit.DataStdIdx;
            
            app.AdjustManualButton.Visible = 'off';
            
            
            if isequal(NodeData(2),0) % Mineral
                
                cla(app.Plot,'reset');
                hold(app.Plot,'on');
                MinIdx = NodeData(1);
                
                % Check for GeneralFit ----------------
                if MinIdx > length(app.PlotData.Mineral)
                    for i = 1:length(app.PlotData.GenCalib)
                        if ~isempty(app.PlotData.GenData(i).Xori)
                            errorbar(app.Plot,app.PlotData.GenData(i).Xori,app.PlotData.GenData(i).Yori,app.PlotData.GenData(i).Eori,'or','HandleVisibility','off');
                        end
                        if ~isempty(app.PlotData.GenData(i).X)
                            errorbar(app.Plot,app.PlotData.GenData(i).X,app.PlotData.GenData(i).Y,app.PlotData.GenData(i).E,'ok','HandleVisibility','off');
                        end
                    end
                    
                    for i = 1:length(app.PlotData.GenCalib)
                        plot(app.Plot,[0,app.Plot.XLim(2)],polyval(app.PlotData.GenCalib(i).p,[0,app.Plot.XLim(2)]),'--b');
                    end
                    
                    cla(app.Map_Spec,'reset')
                    app.Map_Spec.Visible = 'off';
                    
                    cla(app.Map_Total,'reset')
                    app.Map_Total.Visible = 'off';
                    
                    app.UITable.Visible = 'off';
                    return
                end
                
                PlotSpot = 0;
                for i = 1:length(app.PlotData.Mineral(MinIdx).El)
                    if ~isempty(app.PlotData.Mineral(MinIdx).El(i).X)
                        errorbar(app.Plot,app.PlotData.Mineral(MinIdx).El(i).X,app.PlotData.Mineral(MinIdx).El(i).Y,app.PlotData.Mineral(MinIdx).El(i).E,'ok','HandleVisibility','off');
                        PlotSpot = 1;
                    end
                end
                if PlotSpot
                    axis(app.Plot,'auto');
                    YlimT = app.Plot.YLim;
                else
                    app.Plot.XLim(2) = 100;
                end
                
                for i = 1:length(app.PlotData.Mineral(MinIdx).El)
                    plot(app.Plot,[0,app.Plot.XLim(2)],polyval(app.PlotData.Mineral(MinIdx).El(i).p_final,[0,app.Plot.XLim(2)]),'-k');
                    plot(app.Plot,[0,app.Plot.XLim(2)],polyval(app.PlotData.GenCalib(i).p,[0,app.Plot.XLim(2)]),'--b');
                end
                if PlotSpot
                    app.Plot.YLim = YlimT;
                end
                
                legend(app.Plot,{'final','General'},'Location','northwest');
                
                hold(app.Plot,'off')
                
                % MAP
                cla(app.Map_Spec,'reset')
                app.Map_Spec.Visible = 'off';
                
                cla(app.Map_Total,'reset')
                app.Map_Total.Visible = 'on';
                imagesc(app.Map_Total,app.PlotData.Mineral(NodeData(1)).SumOx);
                axis(app.Map_Total,'image');
                h = colorbar(app.Map_Total);
                colormap(app.Map_Total,[0,0,0;parula(64)])
                app.Map_Total.XTick = [];
                app.Map_Total.YTick = [];
                
                ylabel(h, 'Total (wt%)')
                
                PosIndex = find(app.PlotData.Mineral(NodeData(1)).SumOx);
                Data = app.PlotData.Mineral(NodeData(1)).SumOx(PosIndex);
                
%                 try
%                     caxis(app.Map_Total,[min(Data(:)),max(Data(:))])
%                 catch ME
%                     %keyboard
%                 end
                %caxis(app.Map_Total,[95 102])
                
                tb = axtoolbar(app.Map_Total,{'pan','zoomin','zoomout','restoreview'});
                
                btn = axtoolbarbtn(tb,'push');
                btn.Icon = '078-magic wand.png';
                btn.Tooltip = 'Auto contrast';
                btn.ButtonPushedFcn = @(varargin)Button_Map_Total_AutoContrastPushed(app);
                
                
            else  % Mineral + Element
                
                MinIdx = NodeData(1);
                ElIdx = NodeData(2);
                
                cla(app.Plot,'reset');
                hold(app.Plot,'on');
                
                % Check for GeneralFit ----------------
                if MinIdx > length(app.PlotData.Mineral)
                    
                    if ~isempty(app.PlotData.GenData(ElIdx).Xori)
                        errorbar(app.Plot,app.PlotData.GenData(ElIdx).Xori,app.PlotData.GenData(ElIdx).Yori,app.PlotData.GenData(ElIdx).Eori,'or','HandleVisibility','off');
                    end
                    if ~isempty(app.PlotData.GenData(ElIdx).X)
                        errorbar(app.Plot,app.PlotData.GenData(ElIdx).X,app.PlotData.GenData(ElIdx).Y,app.PlotData.GenData(ElIdx).E,'ok','HandleVisibility','off');
                    end
                    
                    plot(app.Plot,[0,app.Plot.XLim(2)],polyval(app.PlotData.GenCalib(ElIdx).p,[0,app.Plot.XLim(2)]),'--b');
                                        
                    cla(app.Map_Spec,'reset')
                    app.Map_Spec.Visible = 'off';
                    
                    cla(app.Map_Total,'reset')
                    app.Map_Total.Visible = 'off';
                    
                    app.UITable.Visible = 'off';
                    return
                end
                
                app.AdjustManualButton.Visible = 'on';
                
                if ~isempty(app.PlotData.Mineral(MinIdx).El(ElIdx).X)
                    errorbar(app.Plot,app.PlotData.Mineral(MinIdx).El(ElIdx).X,app.PlotData.Mineral(MinIdx).El(ElIdx).Y,app.PlotData.Mineral(MinIdx).El(ElIdx).E,'ok','HandleVisibility','off');
                    axis(app.Plot,'auto');
                    
                    if ~isempty(app.PlotData.Mineral(MinIdx).El(ElIdx).Xout)
                        errorbar(app.Plot,app.PlotData.Mineral(MinIdx).El(ElIdx).Xout,app.PlotData.Mineral(MinIdx).El(ElIdx).Yout,app.PlotData.Mineral(MinIdx).El(ElIdx).Eout,'or','HandleVisibility','off');
                        axis(app.Plot,'auto');
                    end
                    
                    Xmax = app.Plot.XLim(2);
                else
                    Xmax = 100;
                end
                
                Xi = [0,Xmax];
                [Yi] = polyval(app.PlotData.Mineral(MinIdx).El(ElIdx).p_final,Xi); % app.PlotData.Mineral(MinIdx).El(ElIdx).s
                
                plot(app.Plot,Xi,Yi,'-k');
                %plot(app.Plot,Xi,Yi+delta,'-k');
                %plot(app.Plot,Xi,Yi-delta,'-k');
                plot(app.Plot,[0,Xmax],polyval(app.PlotData.GenCalib(ElIdx).p,[0,Xmax]),'--b');
                
                legend(app.Plot,{'final','General'},'Location','northwest');

                %app.Plot.XLim(1) = 0;
                %keyboard
                
                hold(app.Plot,'off')
                
                % MAPS
                app.Map_Spec.Visible = 'on';
                cla(app.Map_Spec)
                imagesc(app.Map_Spec,app.PlotData.Mineral(MinIdx).El(ElIdx).MapOx)
                axis(app.Map_Spec,'image');
                h = colorbar(app.Map_Spec);
                colormap(app.Map_Spec,[0,0,0;parula(64)])
                caxis(app.Map_Spec,[0,max(app.PlotData.Mineral(MinIdx).El(ElIdx).MapOx(:))])
                app.Map_Spec.XTick = [];
                app.Map_Spec.YTick = [];
                
                ylabel(h, app.PlotData.QElnames(ElIdx));
                
                tb = axtoolbar(app.Map_Spec,{'pan','zoomin','zoomout','restoreview'});
                
                btn = axtoolbarbtn(tb,'push');
                btn.Icon = '078-magic wand.png';
                btn.Tooltip = 'Auto contrast';
                btn.ButtonPushedFcn = @(varargin)Button_Map_Spec_AutoContrastPushed(app);
                
                
                cla(app.Map_Total)
                imagesc(app.Map_Total,app.PlotData.Mineral(MinIdx).SumOx);
                axis(app.Map_Total,'image');
                h = colorbar(app.Map_Total);
                colormap(app.Map_Total,[0,0,0;parula(64)])
                caxis(app.Map_Total,[0,max(app.PlotData.Mineral(MinIdx).SumOx(:))])
                
                app.Map_Total.XTick = [];
                app.Map_Total.YTick = [];
                
                ylabel(h, 'Total (wt%)')
                
                tb2 = axtoolbar(app.Map_Total,{'pan','zoomin','zoomout','restoreview'});
                
                btn2 = axtoolbarbtn(tb2,'push');
                btn2.Icon = '078-magic wand.png';
                btn2.Tooltip = 'Auto contrast';
                btn2.ButtonPushedFcn = @(varargin)Button_Map_Total_AutoContrastPushed(app);
                
                PosIndex = find(app.PlotData.Mineral(MinIdx).SumOx);
                Data = app.PlotData.Mineral(MinIdx).SumOx(PosIndex);
                caxis(app.Map_Total,[min(Data(:)),max(Data(:))])
                
            end
            
            updateTable(app)
            
        end
        
        function CalculateMaps(app)
            
            El2Find = app.Standards.ElMap(app.Fit.DataStdIdx);
            MapNames = app.XMapToolsID.XMapToolsData.MapData.It.Names;
            
            [Yes,IdxMaps] = ismember(El2Find,MapNames);
            
            QElnames = app.PlotData.QElnames; % Checked this is the right order
            Ox4MaN = QElnames(find(Yes))';
            MineralsNames4MaN = app.PlotData.MinNames;
            PeakWtPer = zeros(size(app.Calib.Back));
            
            WhereZero = find(app.Calib.Slope == 0);
            app.Calib.Slope(WhereZero) = 1e-11;
            
            El4MaN = El2Find(find(Yes));
            
            for i = 1:size(app.Calib.Back,1)
                
                SumOx = zeros(size(app.XMapToolsID.XMapToolsData.MapData.It.Data(IdxMaps(1)).Map));
                
                for j =  1:size(app.Calib.Back,2)
                    
                    MapIt = app.XMapToolsID.XMapToolsData.MapData.It.Data(IdxMaps(j)).Map;
                    MapOx = zeros(size(MapIt));
                    
                    IdxMask = find(app.MaskFile.MaskMap == i);
                    
                    MapOx(IdxMask) = (MapIt(IdxMask)-app.Calib.Back(i,j))/app.Calib.Slope(i,j);
                    
                    % FILTER negative values
                    WhereNeg = find(MapOx < 0);
                    if ~isempty(WhereNeg)
                        MapOx(WhereNeg) = zeros(size(WhereNeg));
                    end
                    
                    app.PlotData.Mineral(i).El(j).MapOx = MapOx;
                    
                    IdxDef = find(MapOx(:) > 0);
                    if length(IdxDef) > 1
                        PeakPosition = FindPeakPosition(app,MapOx(IdxDef));
                    else
                        PeakPosition = 0;
                    end
                    PeakWtPer(i,j) = PeakPosition;
                    
                    SumOx = SumOx+MapOx;
                end
                %keyboard
                app.PlotData.Mineral(i).SumOx = SumOx;
            end
            
            %PeakWtPer
            
            [MaN_all,Z_Cat] = CalculateMaN(app,PeakWtPer,Ox4MaN,El4MaN);
            
            % Save
            fid = fopen('last_k_data.txt','w');
            
            fprintf(fid,'%s\n','>1 (Elements)');
            for i = 1:length(El4MaN)-1
                fprintf(fid,'%s\t',El4MaN{i});
            end
            fprintf(fid,'%s\n',El4MaN{end});
            
            fprintf(fid,'%s\n','>2 (Oxides)');
            for i = 1:length(Ox4MaN)-1
                fprintf(fid,'%s\t',Ox4MaN{i});
            end
            fprintf(fid,'%s\n',Ox4MaN{end});
            
            fprintf(fid,'%s\n','>3 (element Z)');
            for i = 1:length(Z_Cat)-1
                fprintf(fid,'%f\t',Z_Cat(i));
            end
            fprintf(fid,'%f\n',Z_Cat(end));
            
            fprintf(fid,'%s\n','>4 (Mineral Names)');
            for i = 1:length(MineralsNames4MaN)-1
                fprintf(fid,'%s\t',MineralsNames4MaN{i});
            end
            fprintf(fid,'%s\n',MineralsNames4MaN{end});
            
            fprintf(fid,'%s\n','>5 (Mean Atomic Numbers)');
            for i = 1:length(MaN_all)-1
                fprintf(fid,'%f\t',MaN_all(i));
            end
            fprintf(fid,'%f\n',MaN_all(end));
            
            fprintf(fid,'%s\n','>6 (k table: mineral/element)');
            for i = 1:size(app.Calib.k_final,1)
                for j = 1:size(app.Calib.k_final,2)-1
                    fprintf(fid,'%f\t',app.Calib.k_final(i,j));
                end
                fprintf(fid,'%f\n',app.Calib.k_final(i,end));
            end
            fclose(fid);
        end
        
        function [MaN_all,Z_Cat] = CalculateMaN(app,PeakWtPer,Ox4MaN,El4MaN)
            
            ElOxDataRef = app.XMapToolsID.ElOxDataDef;
            
            [Yes_Oxide,Position] = ismember(Ox4MaN,ElOxDataRef.OxList);
            
            ElInd = Position;
            
            WhereNonOxide = find(Yes_Oxide==0);
            if ~isempty(WhereNonOxide)                
                [Yes_Elem,Position_Elem] = ismember(Ox4MaN,ElOxDataRef.ElList);
                
                for i = 1:length(Yes_Oxide)
                    if Yes_Elem(i) > 0
                        Nb_Cat(i) = 1;
                        Nb_O(i) = 0;
                        % this is the atomic number
                        ElInd(i) = Position_Elem(i);
                    else
                        Nb_Cat(i) = ElOxDataRef.OxNbCat(Position(i));
                        Nb_O(i) = ElOxDataRef.OxNbOx(Position(i));
                        % this is the atomic number
                        ElInd(i) = ElOxDataRef.OxElIdx(Position(i));
                    end
                end
                
            else
                Nb_Cat = ElOxDataRef.OxNbCat(Position);
                Nb_O = ElOxDataRef.OxNbOx(Position);
            end
            
             
            Z_Cat = ElInd;
            Z_O = 8*ones(1,length(Nb_Cat));
            
            Mm_Cat = ElOxDataRef.ElMass(ElInd);
            Mm_O = 15.999*ones(1,length(Nb_O));
            
            Mm_Ox = Mm_Cat.*Nb_Cat+Mm_O.*Nb_O;
            
            MaN_all = zeros(size(PeakWtPer,1),1);
            
            for i = 1:size(PeakWtPer,1)
                Z_Ox = PeakWtPer(i,:).*(Z_Cat.*Mm_Cat.*Nb_Cat./Mm_Ox+Z_O.*Mm_O.*Nb_O./Mm_Ox);
                MaN_all(i)=sum(Z_Ox)/sum(PeakWtPer(i,:));
            end
            
        end
        
        function Button_Map_Total_AutoContrastPushed(app)
            
            NodeData = app.Tree.SelectedNodes.NodeData;
            ImageData = app.PlotData.Mineral(NodeData(1)).SumOx;
            ImageData = ImageData(find(ImageData > 0));
            
            SortedData = sort(ImageData(:));
            SelCrit = round(numel(SortedData) * 0.065);
            
            Min1 = SortedData(SelCrit);
            Max1 = SortedData(end-SelCrit);
            
            SortedData2 = SortedData(SelCrit:end-SelCrit);
            SelCrit2 = round(numel(SortedData2) * 0.065);
            
            Min2 = SortedData2(SelCrit2);
            Max2 = SortedData2(end-SelCrit2);
            
            % Current values:
            [Min,Max] = caxis(app.Map_Total);
            
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
            
            % Update plots
            caxis(app.Map_Total,[Min,Max]);
            
        end
        
        function Button_Map_Spec_AutoContrastPushed(app)
            
            NodeData = app.Tree.SelectedNodes.NodeData;
            
            MinIdx = NodeData(1);
            ElIdx = NodeData(2);
            
            ImageData = app.PlotData.Mineral(MinIdx).El(ElIdx).MapOx;
            ImageData = ImageData(find(ImageData > 0));
            
            SortedData = sort(ImageData(:));
            SelCrit = round(numel(SortedData) * 0.065);
            
            Min1 = SortedData(SelCrit);
            Max1 = SortedData(end-SelCrit);
            
            SortedData2 = SortedData(SelCrit:end-SelCrit);
            SelCrit2 = round(numel(SortedData2) * 0.065);
            
            Min2 = SortedData2(SelCrit2);
            Max2 = SortedData2(end-SelCrit2);
            
            % Current values:
            [Min,Max] = caxis(app.Map_Spec);
            
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
            
            % Update plots
            caxis(app.Map_Spec,[Min,Max]);
            
        end
        
        function updateTable(app)
            
            %El2Find = app.Standards.ElMap(find(~ismember(app.Standards.ElMap,'none')));
            %MapNames = app.XMapToolsID.XMapToolsData.MapData.It.Names;
            
            k = app.Fit.Spec_k; %(:,find(~ismember(app.Standards.ElMap,'none')));
            k_final = app.Calib.k_final;
            
            %[Yes,IdxMaps] = ismember(El2Find,MapNames);
            
            NodeData = app.Tree.SelectedNodes.NodeData;
            MinIdx = NodeData(1);
            ElIdx = NodeData(2);
            
            Cell2Disp = cell(length(app.PlotData.Mineral(MinIdx).El)+2,8);
            SumOx = zeros(size(app.PlotData.Mineral(MinIdx).El(1).MapOx));
            
            % We display only for a mineral
            for i = 1:length(app.PlotData.Mineral(MinIdx).El)
                
                Cell2Disp{i,1} = app.PlotData.Mineral(MinIdx).El(i).Map;        % El name
                Cell2Disp{i,2} = length(app.PlotData.Mineral(MinIdx).El(i).Y);  % Nb standards
                Cell2Disp{i,3} = median(app.PlotData.Mineral(MinIdx).El(i).Y);  % Median(It)
                Cell2Disp{i,4} = median(app.PlotData.Mineral(MinIdx).El(i).X);  % Median(Wt)
                
                Cell2Disp{i,5} = FindPeakPosition(app,app.PlotData.Mineral(MinIdx).El(i).MapOx(find(app.PlotData.Mineral(MinIdx).El(i).MapOx>0)));
                Cell2Disp{i,5} = sum(app.PlotData.Mineral(MinIdx).El(i).MapOx(find(app.PlotData.Mineral(MinIdx).El(i).MapOx>0)))/numel(find(app.PlotData.Mineral(MinIdx).SumOx > 0));
                if isequal(k(MinIdx,i),k_final(MinIdx,i))
                    Cell2Disp{i,6} = k(MinIdx,i);   % k value
                else
                    Cell2Disp{i,6} = [num2str(k(MinIdx,i)),' (',num2str(k_final(MinIdx,i)),')']; % k value
                end
                if ~isempty(app.PlotData.Mineral(MinIdx).El(i).p)
                    Cell2Disp{i,7} = app.PlotData.Mineral(MinIdx).El(i).p(1);       % Slope
                    Cell2Disp{i,8} = app.PlotData.Mineral(MinIdx).El(i).p(2);       % Back
                else
                    Cell2Disp{i,7} = [num2str(app.Fit.Gen_slope(i)),' (*)'];
                    Cell2Disp{i,8} = [num2str(app.Fit.Gen_back(i)),' (*)'];
                end
                SumOx = SumOx+app.PlotData.Mineral(MinIdx).El(i).MapOx;
            end
            
            Cell2Disp{end-1,1} = 'sum(wt)'; 
            Cell2Disp{end-1,4} = sum(cell2mat(Cell2Disp(:,4)));
            Cell2Disp{end-1,5} = sum(cell2mat(Cell2Disp(:,5)));
            
            PeakSumOx = FindPeakPosition(app,SumOx(find(SumOx>0)));
            
            Cell2Disp{end,1} = 'mode(SumOx)';
            Cell2Disp{end,5} = PeakSumOx;
            
            app.UITable.Data = Cell2Disp;
            app.UITable.Visible = 'on';
            
            drawnow
            
            Width = app.UITable.Position(3);
            app.UITable.ColumnWidth = {0.10*Width,0.10*Width,0.145*Width,0.145*Width,0.135*Width,0.125*Width,0.125*Width,0.125*Width};

        end
        
        function [p,S,mu] = polyfix(app,x,y,n,xfix,yfix,xder,dydx)
            %POLYFIX Fit polynomial p to data, but specify value at specific points
            %
            %   p = polyfix(x,y,n,xfix,yfix)
            % finds the coefficients of the polynomial of degree n that fits the data
            % in a least-squares sense, with the constraint that polyval(p,xfix) = yfix
            %
            %   p = polyfix(x,y,n,xfix,yfix,xder,dydx)
            % uses the additional constraint that the derivative at xder = dydx
            %
            %   [p,S] = polyfix(...) or [p,S,mu] is also possible
            %   See the documentation for polyfit for details
            %
            % The polynomial order n must be high enough to match all specified values
            % NOTE: For the lowest order allowed, p will fit the constraints, but
            %       may disregard x and y.
            %
            % Example 1:
            % x = linspace(0,2,100)';y = sin(pi*x)+ 0.1*randn(100,1);
            % p = polyfix(x,y,3,[0,2],[0,0]);plot(x,y,'.',x,polyval(p,x));
            %
            % Example 2:
            % x = linspace(0,1,100)';y = sin(x*pi/2) + 0.1*randn(100,1);
            % p = polyfix(x,y,4,[],[],[0 1],[1 0]);plot(x,y,'.',x,polyval(p,x))
            % See also: polyfit, polyval
            % Are Mjaavatten, Telemark University College, Norway, November 2015
            % Revision history
            % 2015-11-28: Version 1.0
            % 2015-12-07: Version 1.1:
            %             Added option for specifying derivatives
            %             The output is now a row vector, for consistency with polyfit
            %             Added test for polynomial degree
            % 2017-08-23: Version 1.2
            %             Fixed trivial errors in the help section
            % 2019-10-30: Version 1.3
            %             Added outputs S and mu, in line with polyfit
            %% Make sure all input arrays are column vectors of compatible sizes:
            x = x(:);
            y = y(:);
            % Center and scale if the user specifies mu as an output
            if nargout > 2
                mu = [mean(x); std(x)];
                x = (x - mu(1))/mu(2);
                xfix = (xfix - mu(1))/mu(2);
            end
            
            nfit = length(x);
            if ~(length(y)== nfit)
                error('x and y must have the same size');
            end
            xfix = xfix(:);
            yfix = yfix(:);
            nfix = length(xfix);
            if ~(length(yfix)== nfix)
                error('xfit and yfit must have the same size');
            end
            if nargin > 6 % Derivatives specified
                xder = xder(:);
                dydx = dydx(:);
                if nargout > 2
                    xder = (xder-mu(1))/mu(2);
                    dydx = dydx*mu(2);
                end
            else
                xder = [];
                dydx = [];
            end
            nder = length(xder);
            if ~(length(dydx) == nder)
                error('xder and dydx must have the same size');
            end
            nspec = nfix + nder;
            specval = [yfix;dydx];
            %% First find A and pc such that A*pc = specval
            A = zeros(nspec,n+1);
            % Specified y values
            for i = 1:n+1
                A(1:nfix,i) = ones(nfix,1).*xfix.^(n+1-i);
            end
            % Specified values of dydx
            if nder > 0
                for i = 1:n
                    A(nfix +(1:nder),i) = (n-i+1)*ones(nder,1).*xder.^(n-i);
                end
            end
            if nfix > 0
                lastcol = n+1;
                nmin = nspec - 1;
            else
                lastcol = n;   % If only derivatives, p(n+1) is arbitrary
                nmin = nspec;
            end
            if n < nmin
                error('Polynomial degree too low. Cannot match all constraints');
            end
            %% Find the unique polynomial of degree nmin that fits the constraints.
            firstcol = n-nmin+1;   % A(:,firstcol_lastcol) detrmines p0
            pc0 = A(:,firstcol:lastcol)\specval;  % Satifies A*pc = specval
            % Now extend to degree n and pad with zeros:
            pc = zeros(n+1,1);
            pc(firstcol:lastcol) = pc0;    % Satisfies A*pcfull = yfix
            % Construct Vandermonde matrix.
            V(:,n+1) = ones(length(x),1,class(x));
            for j = n:-1:1
                V(:,j) = x.*V(:,j+1);
            end
            % Subtract constraints polynomial values from y.
            yfit = y-polyval(pc,x);
            %% We now find the p0 that minimises (V*p0-yfit)'*(V*p0-yfit)
            %  given that A*p0 = 0
            B = null(A);    % For any (n+1-nspc by 1) vector z, A*B*z = 0
            z = V*B\yfit;   % Least squares solution of V*B*z = yfit
            p0 = B*z;       % Satisfies A*p0 = 0;
            p = p0'+pc';    % Satisfies A*p = b;
            %% Add error etimation struct in the same way as polyfit
            if nargout > 1
                [~,R] = qr(V,0);
                r = y - V*p';
                % S is a structure containing three elements: the triangular factor from a
                % QR decomposition of the Vandermonde matrix, the degrees of freedom and
                % the norm of the residuals.
                S.R = R;
                S.df = max(0,length(y) - (n+1));
                S.normr = norm(r);
            end
        end
        
        function PeakPosition = FindPeakPosition(app,Values)
            
            [N,EDGES] = histcounts(Values,'BinMethod','auto');
            [Val,PosMax] = max(N);
            dPos = (EDGES(2)-EDGES(1)); %/2;
            PeakPosition = EDGES(PosMax)+dPos;
            
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
            
            app.XMapToolsApp = XMapToolsApp;
            
            app.XMapToolsGlobalStandardization.Visible = 'off';
            
            app.SlopeManual.Visible = 'off';
            app.SlopeManualLabel.Visible = 'off';
            app.BackgroundManual.Visible = 'off';
            app.BackgroundManualLabel.Visible = 'off';
            
            % Debug mode
            app.Debug = 1;
            if app.Debug
                if exist('last_calibration.txt')
                    delete('last_calibration.txt');
                end
                diary last_calibration.txt
                disp(['Calibration started (',char(datetime('now')),')'])
                disp(' ')
            end
            
            movegui(app.XMapToolsGlobalStandardization,'center'); 
            
            app.XMapToolsID = XMapToolsApp;
            
            % (1) Import mask file
            app.MaskFile.Name = app.XMapToolsID.XMapToolsData.MapData.MaskFile.Names{SelectedMaskFile};
            app.MaskFile.NbMasks = app.XMapToolsID.XMapToolsData.MapData.MaskFile.NbMasks(SelectedMaskFile);
            app.MaskFile.Names = app.XMapToolsID.XMapToolsData.MapData.MaskFile.Masks(SelectedMaskFile).Names;
            app.MaskFile.MaskMap = app.XMapToolsID.XMapToolsData.MapData.MaskFile.Masks(SelectedMaskFile).MaskMap;
            
            % (2) Import spot analyses
            app.Standards = app.XMapToolsID.XMapToolsData.Standards;
            
            FitAll(app);
            
            if isequal(app.CatchError,1)
                XMapToolsGlobalStandardizationCloseRequest(app, 1)
                return
            end
            
            % Generate Tree
            for i = 2:length(app.MaskFile.Names)
                p = uitreenode(app.Tree,'Text',char(app.MaskFile.Names{i}),'NodeData',[i-1,0]);
                count = 0;
                for j = 1:length(app.Standards.ElMap)
                    if ~isequal(app.Standards.ElMap{j},'none')
                        count = count+1;
                        p1 = uitreenode(p,'Text',char(app.Standards.ElMap{j}),'NodeData',[i-1,count]);
                    end
                end
            end
            
            % Add General Calibration
            p = uitreenode(app.Tree,'Text',char('General fit'),'NodeData',[i,0]);
            count = 0;
            for j = 1:length(app.Standards.ElMap)
                if ~isequal(app.Standards.ElMap{j},'none')
                    count = count+1;
                    p1 = uitreenode(p,'Text',char(app.Standards.ElMap{j}),'NodeData',[i,count]);
                end
            end
            
            
            app.Tree.SelectedNodes = app.Tree.Children(1);
            
            CalculateMaps(app);
            
            PlotAll(app);
            
            app.AppyStandardization.Enable = 'on';
            app.ApplyToAllMaks.Enable = 'on';
            
            app.XMapToolsGlobalStandardization.Visible = 'on';
            
            
        end

        % Selection changed function: Tree
        function TreeSelectionChanged(app, event)
            %selectedNodes = app.Tree.SelectedNodes;
            
            app.SlopeManual.Visible = 'off';
            app.SlopeManualLabel.Visible = 'off';
            app.BackgroundManual.Visible = 'off';
            app.BackgroundManualLabel.Visible = 'off';
            
            PlotAll(app);
            
        end

        % Button pushed function: AppyStandardization
        function AppyStandardizationButtonPushed(app, event)
            
            AllSelected = 1;
            
            if isequal(app.ApplyToAllMaks.Value,0)
                
                app.WaitBar = uiprogressdlg(gcbf,'Title','XMapTools','Indeterminate','on');
                app.WaitBar.Message = 'Select masks to calibrate and press OK to continue...';
                waitfor(Selector(app,app.PlotData.MinNames,'Select masks in the list below','Multiple'));
                close(app.WaitBar)
                
                SelectedMasks = ismember(app.PlotData.MinNames,app.ExchangeSelector);
                
                if sum(SelectedMasks) < length(SelectedMasks)
                    AllSelected = 0;
                end
                
            else
                SelectedMasks = ones(size(app.PlotData.MinNames));
            end
            
            
            % ADD data in XMapTools
            ElNames = app.PlotData.ElNames; 
            
            % Check for EDS maps
            for i = 1:length(ElNames)
                Eli = ElNames{i};
                EDS = find(ismember(Eli,'_'));
                if ~isempty(EDS)
                    ElNames{i} = Eli(1:EDS-1);
                end
            end
            
            QElnames = app.PlotData.QElnames;
            
            [Yes,Idx] = ismember(ElNames,app.XMapToolsID.ElOxDataDef.ElList);
            ElIdx = app.XMapToolsID.ElOxDataDef.ElIdx(Idx);
            
            %app.XMapToolsID.XMapToolsData.MapData.Qt
            
            % (1) Quanti
            
            PosQt = length(app.XMapToolsID.XMapToolsData.MapData.Qt.Names);
            Compt = 0;
            for i = 1:length(app.PlotData.Mineral)
                if SelectedMasks(i)
                    Compt = Compt + 1;
                    
                    app.XMapToolsID.XMapToolsData.MapData.Qt.Names{PosQt+Compt} = app.PlotData.MinNames{i};
                    app.XMapToolsID.XMapToolsData.MapData.Qt.IsOxide(PosQt+Compt) = 1;
                    app.XMapToolsID.XMapToolsData.MapData.Qt.MaskFile{PosQt+Compt} = app.MaskFile.Name;
                    app.XMapToolsID.XMapToolsData.MapData.Qt.NbCalibPoints(PosQt+Compt) = max(app.Fit.Spec_numNonZero(i,:));
                    
                    MapTemp = zeros(size(app.PlotData.Mineral(i).El(1).MapOx));
                    
                    for j = 1:length(ElNames)
                        
                        app.XMapToolsID.XMapToolsData.MapData.Qt.Data(PosQt+Compt).ElNames{j} = QElnames{j};
                        app.XMapToolsID.XMapToolsData.MapData.Qt.Data(PosQt+Compt).ElInd(j) = ElIdx(j);
                        
                        % ADD StdData IMPORTANT or not?
                        
                        app.XMapToolsID.XMapToolsData.MapData.Qt.Data(PosQt+Compt).CData(j).Map = app.PlotData.Mineral(i).El(j).MapOx;
                        
                        MapTemp = MapTemp + app.PlotData.Mineral(i).El(j).MapOx;
                    end
                    
                    app.XMapToolsID.XMapToolsData.MapData.Qt.Data(PosQt+Compt).ElNames{j+1} = 'Total(wt%)';
                    app.XMapToolsID.XMapToolsData.MapData.Qt.Data(PosQt+Compt).ElInd(j+1) = 0;
                    app.XMapToolsID.XMapToolsData.MapData.Qt.Data(PosQt+Compt).CData(j+1).Map = MapTemp;
                end
            end
            
            
            % (2) Merged
            if AllSelected
                PosMe = length(app.XMapToolsID.XMapToolsData.MapData.Me.Names)+1;
                
                app.XMapToolsID.XMapToolsData.MapData.Me.Names{PosMe} = ['Merged_',app.MaskFile.Name];
                app.XMapToolsID.XMapToolsData.MapData.Me.IsOxide(PosMe) = 1;
                app.XMapToolsID.XMapToolsData.MapData.Me.MaskFile{PosMe} = app.MaskFile.Name;
                app.XMapToolsID.XMapToolsData.MapData.Me.NbCalibPoints(PosMe) = 0;
                
                MapTempSum = zeros(size(app.PlotData.Mineral(1).El(1).MapOx));
                
                for j = 1:length(ElNames)
                    
                    app.XMapToolsID.XMapToolsData.MapData.Me.Data(PosMe).ElNames{j} = QElnames{j};
                    app.XMapToolsID.XMapToolsData.MapData.Me.Data(PosMe).ElInd(j) = ElIdx(j);
                    
                    MapTemp = zeros(size(app.PlotData.Mineral(1).El(1).MapOx));
                    for i = 1:length(app.PlotData.Mineral)
                        MapTemp = MapTemp + app.PlotData.Mineral(i).El(j).MapOx;
                    end
                    
                    app.XMapToolsID.XMapToolsData.MapData.Me.Data(PosMe).CData(j).Map = MapTemp;
                    
                    MapTempSum = MapTempSum+MapTemp;
                end
                
                app.XMapToolsID.XMapToolsData.MapData.Me.Data(PosMe).ElNames{j+1} = 'Total(wt%)';
                app.XMapToolsID.XMapToolsData.MapData.Me.Data(PosMe).ElInd(j+1) = 0;
                
                app.XMapToolsID.XMapToolsData.MapData.Me.Data(PosMe).CData(j+1).Map = MapTempSum;
            end
            
            XMapToolsGlobalStandardizationCloseRequest(app, event);
        end

        % Close request function: XMapToolsGlobalStandardization
        function XMapToolsGlobalStandardizationCloseRequest(app, event)
            diary off
            delete(app)
            return
        end

        % Button pushed function: Button_CopyTable
        function Button_CopyTablePushed(app, event)
            
            TableData  = app.UITable.Data;
            
            str = '';
            str = sprintf('%s%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n',str,'Elem','#(std)','med(it)','med(wt)_s','peak(wt)_m','k-factor','slope','back');
            for i = 1:size(TableData,1)
                for j = 1:size(TableData,2)
                    if ischar(TableData{i,j})
                        str = sprintf('%s%s\t',str,TableData{i,j});
                    else
                        str = sprintf('%s%f\t',str,TableData{i,j});
                    end
                end
                str = sprintf('%s\n',str);
            end
            
            clipboard ('copy',str);
        end

        % Button pushed function: AdjustManualButton
        function AdjustManualButtonPushed(app, event)
            app.SlopeManual.Visible = 'on';
            app.SlopeManualLabel.Visible = 'on';
            app.BackgroundManual.Visible = 'on';
            app.BackgroundManualLabel.Visible = 'on';
            
            NodeData = app.Tree.SelectedNodes.NodeData;
            MinIdx = NodeData(1);
            ElIdx = NodeData(2); 
            
            app.SlopeManual.Value = app.PlotData.Mineral(MinIdx).El(ElIdx).p_final(1);
            app.BackgroundManual.Value = app.PlotData.Mineral(MinIdx).El(ElIdx).p_final(2);
            
        end

        % Value changed function: BackgroundManual, SlopeManual
        function ManualValueChanged(app, event)
            NodeData = app.Tree.SelectedNodes.NodeData;
            MinIdx = NodeData(1);
            ElIdx = NodeData(2);
            
            app.PlotData.Mineral(MinIdx).El(ElIdx).p_final(1) = app.SlopeManual.Value;
            app.PlotData.Mineral(MinIdx).El(ElIdx).p_final(2) = app.BackgroundManual.Value;
            
            app.Calib.Slope(MinIdx,ElIdx) = app.SlopeManual.Value;
            app.Calib.Back(MinIdx,ElIdx) = app.BackgroundManual.Value;
            
            CalculateMaps(app);
            PlotAll(app);
            
        end

        % Button pushed function: Button_help
        function Button_helpPushed(app, event)
            
            if isempty(app.XMapToolsApp.Id_HelpTool)
                Help_Display(app.XMapToolsApp,'XMT_help_Calibration_EPMA.html');
            else
                app.XMapToolsApp.Id_HelpTool.UpdateTextHelp('XMT_help_Calibration_EPMA.html');
            end
            
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create XMapToolsGlobalStandardization and hide until all components are created
            app.XMapToolsGlobalStandardization = uifigure('Visible', 'off');
            app.XMapToolsGlobalStandardization.Position = [100 100 1237 763];
            app.XMapToolsGlobalStandardization.Name = 'Calibration Assistant For EPMA Data – XMapTools';
            app.XMapToolsGlobalStandardization.CloseRequestFcn = createCallbackFcn(app, @XMapToolsGlobalStandardizationCloseRequest, true);

            % Create GridLayout
            app.GridLayout = uigridlayout(app.XMapToolsGlobalStandardization);
            app.GridLayout.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '0.5x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout.RowHeight = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};

            % Create Image
            app.Image = uiimage(app.GridLayout);
            app.Image.Layout.Row = 1;
            app.Image.Layout.Column = [1 2];
            app.Image.ImageSource = 'logo_xmap_final.png';

            % Create GridLayout2
            app.GridLayout2 = uigridlayout(app.GridLayout);
            app.GridLayout2.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout2.RowHeight = {'1x'};
            app.GridLayout2.ColumnSpacing = 8;
            app.GridLayout2.Padding = [10 9 10 9];
            app.GridLayout2.Layout.Row = 1;
            app.GridLayout2.Layout.Column = [3 7];

            % Create Button_help
            app.Button_help = uibutton(app.GridLayout2, 'push');
            app.Button_help.ButtonPushedFcn = createCallbackFcn(app, @Button_helpPushed, true);
            app.Button_help.Icon = '061-info.png';
            app.Button_help.Layout.Row = 1;
            app.Button_help.Layout.Column = 6;
            app.Button_help.Text = '';

            % Create Button_CopyTable
            app.Button_CopyTable = uibutton(app.GridLayout2, 'push');
            app.Button_CopyTable.ButtonPushedFcn = createCallbackFcn(app, @Button_CopyTablePushed, true);
            app.Button_CopyTable.Icon = '009-photo camera.png';
            app.Button_CopyTable.Layout.Row = 1;
            app.Button_CopyTable.Layout.Column = 3;
            app.Button_CopyTable.Text = '';

            % Create GridLayout3
            app.GridLayout3 = uigridlayout(app.GridLayout);
            app.GridLayout3.ColumnWidth = {'1x', '1x', '1x', '1x', '1x'};
            app.GridLayout3.RowHeight = {'1x'};
            app.GridLayout3.Padding = [10 5 10 5];
            app.GridLayout3.Layout.Row = 1;
            app.GridLayout3.Layout.Column = [10 13];

            % Create AppyStandardization
            app.AppyStandardization = uibutton(app.GridLayout3, 'push');
            app.AppyStandardization.ButtonPushedFcn = createCallbackFcn(app, @AppyStandardizationButtonPushed, true);
            app.AppyStandardization.Icon = '044-repeat.png';
            app.AppyStandardization.Enable = 'off';
            app.AppyStandardization.Layout.Row = 1;
            app.AppyStandardization.Layout.Column = [1 3];
            app.AppyStandardization.Text = 'Apply standardization';

            % Create ApplyToAllMaks
            app.ApplyToAllMaks = uicheckbox(app.GridLayout3);
            app.ApplyToAllMaks.Enable = 'off';
            app.ApplyToAllMaks.Text = 'Apply to all maks';
            app.ApplyToAllMaks.Layout.Row = 1;
            app.ApplyToAllMaks.Layout.Column = [4 5];
            app.ApplyToAllMaks.Value = true;

            % Create UITable
            app.UITable = uitable(app.GridLayout);
            app.UITable.ColumnName = {'El.'; '#(std)'; 'med(it)'; 'med(wt)_s'; 'mode(wt)_m'; 'k-factor'; 'slope'; 'back'};
            app.UITable.RowName = {};
            app.UITable.Layout.Row = [3 9];
            app.UITable.Layout.Column = [8 13];

            % Create Tree
            app.Tree = uitree(app.GridLayout);
            app.Tree.SelectionChangedFcn = createCallbackFcn(app, @TreeSelectionChanged, true);
            app.Tree.Layout.Row = [3 8];
            app.Tree.Layout.Column = [1 2];

            % Create GridLayout5
            app.GridLayout5 = uigridlayout(app.GridLayout);
            app.GridLayout5.ColumnWidth = {'1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x', '1x'};
            app.GridLayout5.RowHeight = {'1x'};
            app.GridLayout5.Padding = [3 2 3 20];
            app.GridLayout5.Layout.Row = 2;
            app.GridLayout5.Layout.Column = [1 13];

            % Create AIparametersDropDown
            app.AIparametersDropDown = uidropdown(app.GridLayout5);
            app.AIparametersDropDown.Items = {'Auto', 'Auto (machine learning)', 'Manual'};
            app.AIparametersDropDown.Enable = 'off';
            app.AIparametersDropDown.Layout.Row = 1;
            app.AIparametersDropDown.Layout.Column = 15;
            app.AIparametersDropDown.Value = 'Auto';

            % Create AIparametersDropDownLabel
            app.AIparametersDropDownLabel = uilabel(app.GridLayout5);
            app.AIparametersDropDownLabel.HorizontalAlignment = 'right';
            app.AIparametersDropDownLabel.Enable = 'off';
            app.AIparametersDropDownLabel.Layout.Row = 1;
            app.AIparametersDropDownLabel.Layout.Column = [13 14];
            app.AIparametersDropDownLabel.Text = 'AI parameters';

            % Create AdjustManualButton
            app.AdjustManualButton = uibutton(app.GridLayout5, 'push');
            app.AdjustManualButton.ButtonPushedFcn = createCallbackFcn(app, @AdjustManualButtonPushed, true);
            app.AdjustManualButton.Layout.Row = 1;
            app.AdjustManualButton.Layout.Column = [1 2];
            app.AdjustManualButton.Text = 'Adjust (Manual)';

            % Create SlopeManualLabel
            app.SlopeManualLabel = uilabel(app.GridLayout5);
            app.SlopeManualLabel.HorizontalAlignment = 'right';
            app.SlopeManualLabel.Layout.Row = 1;
            app.SlopeManualLabel.Layout.Column = 6;
            app.SlopeManualLabel.Text = 'Slope';

            % Create SlopeManual
            app.SlopeManual = uieditfield(app.GridLayout5, 'numeric');
            app.SlopeManual.Limits = [0 Inf];
            app.SlopeManual.ValueDisplayFormat = '%0.2f';
            app.SlopeManual.ValueChangedFcn = createCallbackFcn(app, @ManualValueChanged, true);
            app.SlopeManual.Layout.Row = 1;
            app.SlopeManual.Layout.Column = 7;

            % Create BackgroundManualLabel
            app.BackgroundManualLabel = uilabel(app.GridLayout5);
            app.BackgroundManualLabel.HorizontalAlignment = 'right';
            app.BackgroundManualLabel.Layout.Row = 1;
            app.BackgroundManualLabel.Layout.Column = [3 4];
            app.BackgroundManualLabel.Text = 'Background';

            % Create BackgroundManual
            app.BackgroundManual = uieditfield(app.GridLayout5, 'numeric');
            app.BackgroundManual.Limits = [0 Inf];
            app.BackgroundManual.ValueDisplayFormat = '%0.4g';
            app.BackgroundManual.ValueChangedFcn = createCallbackFcn(app, @ManualValueChanged, true);
            app.BackgroundManual.Layout.Row = 1;
            app.BackgroundManual.Layout.Column = 5;

            % Create LegendElistheelementLabel
            app.LegendElistheelementLabel = uilabel(app.GridLayout);
            app.LegendElistheelementLabel.VerticalAlignment = 'top';
            app.LegendElistheelementLabel.Layout.Row = [11 12];
            app.LegendElistheelementLabel.Layout.Column = [7 12];
            app.LegendElistheelementLabel.Text = '';

            % Create TextArea
            app.TextArea = uitextarea(app.GridLayout);
            app.TextArea.Editable = 'off';
            app.TextArea.FontSize = 9;
            app.TextArea.Layout.Row = [10 12];
            app.TextArea.Layout.Column = [8 13];
            app.TextArea.Value = {'-----------------------------------------------------------------------------'; '                       Check calibration table description'; '-----------------------------------------------------------------------------'; '  - El.                      element'; '  - #(std)                 number of internal standards (spot analyses)'; '  - med(it)               median value of intensity for all pixels'; '  - med(wt)_s         median composition all internal standards'; '  - mode(wt)_m      most frequent composition in calibrated pixels'; '  - k-factor             value of the k factor'; '  - slope                 slope of the calibration curve'; '  - back                  intercept of the calibration curve = background'; '  - sum(wt)             sum of the column'; '  - mode(SumOx)   most frequent sum of the calibrated pixels'; ''};

            % Create Plot
            app.Plot = uiaxes(app.GridLayout);
            xlabel(app.Plot, 'wt.%')
            ylabel(app.Plot, 'Intensity')
            app.Plot.PlotBoxAspectRatio = [1.39208633093525 1 1];
            app.Plot.Box = 'on';
            app.Plot.Layout.Row = [3 8];
            app.Plot.Layout.Column = [3 7];

            % Create Map_Total
            app.Map_Total = uiaxes(app.GridLayout);
            app.Map_Total.XTick = [];
            app.Map_Total.YTick = [];
            app.Map_Total.Box = 'on';
            app.Map_Total.Layout.Row = [9 12];
            app.Map_Total.Layout.Column = [4 6];

            % Create Map_Spec
            app.Map_Spec = uiaxes(app.GridLayout);
            app.Map_Spec.XTick = [];
            app.Map_Spec.YTick = [];
            app.Map_Spec.Box = 'on';
            app.Map_Spec.Layout.Row = [9 12];
            app.Map_Spec.Layout.Column = [1 3];

            % Show the figure after all components are created
            app.XMapToolsGlobalStandardization.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = Calibration_EPMA_exported(varargin)

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.XMapToolsGlobalStandardization)

            % Execute the startup function
            runStartupFcn(app, @(app)startupFcn(app, varargin{:}))

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.XMapToolsGlobalStandardization)
        end
    end
end