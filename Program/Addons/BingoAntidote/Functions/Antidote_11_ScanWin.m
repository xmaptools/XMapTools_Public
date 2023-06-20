function [Output,Antidote_VARIABLES] = Antidote_11_ScanWin(WorkVariXMap,MinimOptions,app)
%
%
%

% -------------
% The behaviour of this function has been modified in Bingo-Antidote 2 
% to follow the chemical potential mapping strategy (PL 14.05.23) 


ROI_DeleteROI(app);

% Load MinimOptions to enable TEST mode
load('MinimOptions.mat');

Tmin = app.TminEditField.Value;
Tmax = app.TmaxEditField.Value;
Pmin = app.PminEditField.Value;
Pmax = app.PmaxEditField.Value;
Res = app.AntidoteGridresolutionEditField.Value;

Ti = [Tmin:(Tmax-Tmin)/(Res-1):Tmax];
Pi = [Pmin:(Pmax-Pmin)/(Res-1):Pmax];

LIMS = [Ti(1),Ti(end),Pi(1),Pi(end)];

dT4Plot = (Ti(2) - Ti(1))/2;
dP4Plot = (Pi(2) - Pi(1))/2;

Axis_Maps = [LIMS(1)-dT4Plot LIMS(2)+dT4Plot (LIMS(3)-dP4Plot) (LIMS(4)+dP4Plot)];

[BinSet] = SetBin(app);

SelectedBinBulk = app.ROITree.SelectedNodes.NodeData;
BinBulkOri = app.BinBulk(SelectedBinBulk);
TempBinBulk = BinBulkOri;

ProgPath = BinSet.Path;
TheSelDatabase = BinSet.Database;
iDB = find(ismember(app.DatabaseListBox.Items,TheSelDatabase));
DefMin = app.BingoDefault.Theriak.Database(iDB).DefMin;

app.Report_Antidote{end+1} = ['Antidote: Recipe [11] - Scanning window (find optimal P–T, variable bulk)'];
app.Report_Antidote{end+1} = '';
% app.Report_Antidote{end+1} = ['Bulk: ',BinSet.Bulk2Display,''];
app.Report_Antidote{end+1} = ['Database: ',BinSet.Database,''];
app.Report_Antidote{end+1} = '';
app.Text_Report_Antidote.Value = app.Report_Antidote;

% Generate the grid
MapSize = size(app.MapData.CData(1).Map);

dX = app.AntidoteROIparamNbStepsEditField.Value;
dY = dX;

Step = dX;

Xi = [floor((dX-1)/2):Step:MapSize(2) - floor((dX-1)/2)];
Yi = [floor((dY-1)/2):Step:MapSize(1) - floor((dY-1)/2)];

ROI = drawrectangle(app.UIAxes,'Position',[0,1,1,1],'Color',[1,0,0.7137],'InteractionsAllowed','none');

cla(app.UIAxes2,'reset');
axis(app.UIAxes2,[app.TminEditField.Value,app.TmaxEditField.Value,app.PminEditField.Value,app.PmaxEditField.Value]);
hold(app.UIAxes2,'on')
xlabel(app.UIAxes2,'Temperature (°C)'), ylabel(app.UIAxes2,'Pressure (GPa)')


DataScanWin.Idx = [];
DataScanWin.Position = [];
DataScanWin.TCf = [];
DataScanWin.Pf = [];
DataScanWin.Qasm = [];
DataScanWin.Qvol = [];
DataScanWin.Qcmp = [];
DataScanWin.Qtot = [];

tic
Compt = 0;
for iX = 1:length(Xi)
    for iY = 1:length(Yi)
        
        app.TabAntidote.SelectedTab = app.ROITab;
        
        Compt = Compt+1;
        if Compt > 1 
            app.Report_Antidote{end+1} = '';
            app.Report_Antidote{end+1} = '';
        end
        app.Report_Antidote{end+1} = [' -->  LOOP: ',num2str(Compt),'/',num2str(length(Xi)*length(Yi))];
        app.Report_Antidote{end+1} = '';
        
        XVals = [Xi(iX)-floor((dX-1)/2),Xi(iX)+floor((dX-1)/2),Xi(iX)+floor((dX-1)/2),Xi(iX)-floor((dX-1)/2)];
        YVals = [Yi(iY)+floor((dY-1)/2),Yi(iY)+floor((dY-1)/2),Yi(iY)-floor((dY-1)/2),Yi(iY)-floor((dY-1)/2)];
        
        ROI.Position = [XVals(end),YVals(end),dX,dX];
        TempBinBulk.DomainXrefYref = ROI.Vertices;
        drawnow
        
        [TempBinBulk] = BinUpdateBulkComposition(app,TempBinBulk,SelectedBinBulk);
        
        [Bulk,TempBinBulk] = SuperFastBulkUpdate(app.BinGfDef,TempBinBulk,1);
        
        [BinSet] = SetBinFAST(ProgPath,DefMin,TheSelDatabase,Bulk,app.BinGfDef);
        app.Report_Antidote{end+1} = ['      BULK: ',BinSet.Bulk2Display];
        
        %
        [BinPhaseDef] = BinButtonCalcPhaseFAST(TempBinBulk,app);
        
%         disp('After BinButtonCalcPhaseFAST:')
%         extractfield(BinPhaseDef,'PhaseVolProp')
        
        app.BinPhaseDef = BinPhaseDef;
        
        [WorkVariXMap] = GenerateResXMap(BinPhaseDef);
        
        app.Report_Antidote{end+1} = '';
        app.Text_Report_Antidote.Value = app.Report_Antidote;
        pause(0.1)
        scroll(app.Text_Report_Antidote,'bottom');
        
        if isequal(MinimOptions.Search.Symplex.FirstOpt,1)

            tic
            % -------------------------------------------------------------
            %                 ** P-T scanning ** starts here...
            % -------------------------------------------------------------
            
            app.Report_Antidote{end+1} = ['      ##### Exploratory P-T scanning (',num2str(Res),' x ',num2str(Res),') #####'];
            app.Report_Antidote{end+1} = '';
            app.Text_Report_Antidote.Value = app.Report_Antidote;
            
            E1 = nan(length(Pi),length(Ti));
            E2 = nan(length(Pi),length(Ti));
            E3 = nan(length(Pi),length(Ti));
            E4 = nan(length(Pi),length(Ti));
            
            IsHSat = nan(length(Pi),length(Ti));
            
            cla(app.UIAxes_LiveAntidote1,'reset');
            
            app.TabGroup2.SelectedTab = app.LiveTab;
            
            app.LiveUpdate = 0;
            app.LIVE_Qtot_Gauge.Value = 0;
            app.EditField_BestQtot.Value = 0;
            
            pause(0.1)
            scroll(app.Text_Report_Antidote,'bottom');
            
            for iTC = 1:length(Ti)
                for iP = 1:length(Pi)
                    
                    [Residual,Evaluation,WorkVariMod] = OptiBingoPT([1,1],[Ti(iTC),Pi(iP)],LIMS,BinSet,WorkVariXMap,MinimOptions,app);
                    E4(iP,iTC) = Residual;
                    
                    if isequal(app.LiveDisplaySwitch.Value,'On')
                        app.LIVE_Qtot_Gauge.Value = abs(min(E4(:)));
                        app.EditField_BestQtot.Value = app.LIVE_Qtot_Gauge.Value;
                    else
                        app.LIVE_Qtot_Gauge.Value = 0;
                        app.EditField_BestQtot.Value = 0;
                    end
                    
                    E1(iP,iTC) = Evaluation.assemblage;
                    E2(iP,iTC) = Evaluation.Volume;
                    E3(iP,iTC) = Evaluation.Compositions;
                    
                    IsHSat(iP,iTC) = SaturationCheck4H(WorkVariMod);
                    
                end
                
                imagesc(app.UIAxes_LiveAntidote1,Ti,Pi,E4);
                app.UIAxes_LiveAntidote1.YDir = 'normal';
                axis(app.UIAxes_LiveAntidote1,Axis_Maps)
                colormap(app.UIAxes_LiveAntidote1,[0,0,0;RdYlBu(64)]);
                colorbar(app.UIAxes_LiveAntidote1)
                xlabel(app.UIAxes_LiveAntidote1,'Temperature (°C)');
                ylabel(app.UIAxes_LiveAntidote1,'Pressure (GPa)');
                title(app.UIAxes_LiveAntidote1,'Objective function (-Q_{tot})');
                
                drawnow
            end
            
            % Find pixels with H saturation...
            WhereSaturation  = find(IsHSat);
            if length(WhereSaturation)
                
                app.Report_Antidote{end+1} = ['       ** WARNING **'];
                app.Report_Antidote{end+1} = ['       The component H2O is "saturated" for ',num2str(length(WhereSaturation)),'/',num2str(length(IsHSat(:))),' P-T couples'];
                app.Report_Antidote{end+1} = ['       This may cause convergence to local minima if H is part of the optimization'];
                app.Report_Antidote{end+1} = ['       Further warning messages related to this issue will be displayed below'];
                app.Report_Antidote{end+1} = '';
                app.Text_Report_Antidote.Value = app.Report_Antidote;
                
                TGrid4Plot = repmat(Ti,length(Pi),1);
                PGrid4Plot = repmat(Pi',1,length(Ti));
                
                hold(app.UIAxes_LiveAntidote1,'on');
                
                plot(app.UIAxes_LiveAntidote1,TGrid4Plot(WhereSaturation),PGrid4Plot(WhereSaturation),'xw');
                drawnow
            end
            
            [sP,sTC] = find(E4==min(E4(:)));
            
            if length(sP) > 1
                sP = sP(1);
                sTC = sTC(1);
            end
            
            X0 = [Ti(sTC),Pi(sP)];
            
            app.Report_Antidote{end+1} = ['      RESULTS (Exploratory P-T scanning)'];
            app.Report_Antidote{end+1} = ['      X0(1) = ',num2str(Pi(sP)),' (P,GPa)'];
            app.Report_Antidote{end+1} = ['      X0(2) = ',num2str(Ti(sTC)),' (T,°C)'];
            app.Report_Antidote{end+1} = '';
            
            ht1 = toc;
            app.Report_Antidote{end+1} = ['      CPU time ',num2str(ht1),' s'];
            app.Report_Antidote{end+1} = '';
            app.Text_Report_Antidote.Value = app.Report_Antidote;
            
            plot(app.UIAxes_LiveAntidote1,Ti(sTC),Pi(sP),'pw','markerfacecolor','w','markersize',10);
            
            pause(0.1)
            scroll(app.Text_Report_Antidote,'bottom');
            drawnow
            
        else
            % From bingo P-T input
            X0 = [app.BingoTemperatureEditField.Value,app.BingoPressureEditField.Value];
        end
        
        options = optimset('fminsearch'); options=optimset(options,'TolX',0.0001,'TolFun',0.0001,'display','iter','MaxFunEvals',300,'MaxIter',100);
        
        NORM = X0;
        X0 = X0./NORM;
        
        %[E4] = OptiBingoPT(X0,NORM,LIMS,InvMet,WorkVariXMap,MinimOptions,handles);
        
        app.Report_Antidote{end+1} = ['      ##### Final P-T-X Optimization (from: T = ',num2str(NORM(1)),' degree C; P = ',num2str(NORM(2)),' bar) #####'];
        app.Report_Antidote{end+1} = [''];
        app.Report_Antidote{end+1} = ['      Method: Simplex'];
        if MinimOptions.Weights.Use
            app.Report_Antidote{end+1} = ['      Equation: Other [E4 = -(',num2str(MinimOptions.Weights.Values(MinimOptions.Weights.Selected,1)),'*E1 + ',num2str(MinimOptions.Weights.Values(MinimOptions.Weights.Selected,2)),'*E2 + ',num2str(MinimOptions.Weights.Values(MinimOptions.Weights.Selected,3)),'*E3)]'];
            app.Report_Antidote{end+1} = '';
        else
            app.Report_Antidote{end+1} = ['      Equation: Classic','[E4 = -1/3*(E1 + (E1/100)*E2 + (E1/100)*(E2/100)*E3)]'];
            app.Report_Antidote{end+1} = '';
        end
        app.Text_Report_Antidote.Value = app.Report_Antidote;
        
        pause(0.1)
        scroll(app.Text_Report_Antidote,'bottom');
        
        app.LiveUpdate = 1;
        
        tic
        f = @OptiBingoPT;
        [Result,Residual] = fminsearch(f, X0,options,NORM,LIMS,BinSet,WorkVariXMap,MinimOptions,app);
        ht2 = toc;
        
        TCf = Result(1)*NORM(1);
        Pf = Result(2)*NORM(2);
        
        app.Report_Antidote{end+1} = ['      RESULTS (Final P-T optimization)'];
        app.Report_Antidote{end+1} = ['      X(1) = ',num2str(Pf),' (P,GPa)'];
        app.Report_Antidote{end+1} = ['      X(2) = ',num2str(TCf),' (T,°C)'];
        app.Report_Antidote{end+1} = '';
        
        ht1 = toc;
        app.Report_Antidote{end+1} = ['      CPU time ',num2str(ht2),' s'];
        app.Report_Antidote{end+1} = '';
        app.Text_Report_Antidote.Value = app.Report_Antidote;
        
        pause(0.1)
        scroll(app.Text_Report_Antidote,'bottom');
        
        
        % BINGO CALL -----------------------------------------------------
        app.Report_Antidote{end+1} = ['      -----------------------------------------------------'];
        app.Report_Antidote{end+1} = ['      >>> New BINGO Run: ',datestr(now),'  <<<'];
        app.Report_Antidote{end+1} = [' '];
        app.Text_Report_Antidote.Value = app.Report_Antidote;
        
        D_Temp = num2str(TCf);
        D_Press = num2str(Pf * 1e4);
        
        %plot(app.UIAxes2,app.BingoTemperatureEditField.Value,app.BingoPressureEditField.Value,'ok')
        %axis(app.UIAxes2,[app.TminEditField.Value,app.TmaxEditField.Value,app.PminEditField.Value,app.PmaxEditField.Value])
        
        app.Report_Antidote{end+1} = ['      Bulk: ',BinSet.Bulk2Display];
        app.Report_Antidote{end+1} = ['      Database: ',BinSet.Database];
        app.Report_Antidote{end+1} = ['      P(GPa): ',num2str(Pf)];
        app.Report_Antidote{end+1} = ['      P(bar): ',D_Press];
        app.Report_Antidote{end+1} = ['      T(C): ',num2str(TCf)];
        app.Report_Antidote{end+1} = [' '];
        app.Text_Report_Antidote.Value = app.Report_Antidote;
        
        pause(0.1)
        scroll(app.Text_Report_Antidote,'bottom');
        
        % -------------------------------------------------------------------------
        % (-) Call Theriak
        WorkVariMod = TheriakCall(BinSet,D_Temp,D_Press);
        
        % -------------------------------------------------------------------------
        % (-) Call Bingo
        DoWePrint = 1;
        
        Report.Mode = 'BINGO';
        Report.Tdeg = D_Temp;
        Report.Pbar = D_Press;
        
        plot(app.UIAxes2,TCf,Pf,'ok')
        text(app.UIAxes2,TCf,Pf,['LBC_',num2str(Compt)],'Interpreter','none');
        drawnow
        
        UpdateText2Disp = 1;
        
        % Reset Bingo's report
        app.Report_Bingo = {};
        
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
        
        for i = 1:length(app.Text_Report.Value)
            app.Report_Antidote{end+1} = ['      ',char(app.Text_Report.Value{i})];
        end
        
        app.Report_Antidote{end+1} = ['      >>> End BINGO Run: ',TheDate,'  <<<'];
        app.Report_Antidote{end+1} = ['      -----------------------------------------------------'];
        app.Text_Report_Antidote.Value = app.Report_Antidote;
        
        pause(0.1)
        scroll(app.Text_Report_Antidote,'bottom');
        
        hp1 = pie(app.Result_Plot1,Report.VolXMap+0.000000000001,Report.TheNamesPlotMap);
        set(hp1(2:2:end),'FontSize',9);
        colormap(app.Result_Plot1,RdYlBu(length(Report.VolXMap)))
        title(app.Result_Plot1,'Observations (map)','FontSize',13);
        
        hp2 = pie(app.Result_Plot2,Report.VolTher+0.000000000001,Report.TheNamesPlotTher);
        set(hp2(2:2:end),'FontSize',9);
        colormap(app.Result_Plot2,RdYlBu(length(Report.VolXMap)))
        title(app.Result_Plot2,'Model (Theriak)','FontSize',13);
        
        app.TabGroup2.SelectedTab = app.ResultsTab;
        
        DataScanWin.Idx(Compt) = Compt;
        DataScanWin.Position(Compt,:) = ROI.Position;
        DataScanWin.TCf(Compt) = TCf;
        DataScanWin.Pf(Compt) = Pf;
        DataScanWin.Qasm(Compt) = A1;
        DataScanWin.Qvol(Compt) = V1;
        DataScanWin.Qcmp(Compt) = C1;
        DataScanWin.Qtot(Compt) = TOTAL2;
        
        % keyboard
    end
end

app.Report_Antidote{end+1} = [' '];
app.Report_Antidote{end+1} = [' '];

app.Report_Antidote{end+1} = ['               SUMMARY (Scanning Window):'];
app.Report_Antidote{end+1} = [' -----------------------------------------------------'];
app.Report_Antidote{end+1} = 'IDX | POSITION (X,Y,W,H) | T(°C) | P(GPa) | Qasm(%) | Qvol(%) | Qcmp(%) | Qtot(%) ';
for i = 1:length(DataScanWin.Idx)
    app.Report_Antidote{end+1} = ['LEB_',num2str(DataScanWin.Idx(i)),' | ',num2str(DataScanWin.Position(i,1)),',',num2str(DataScanWin.Position(i,2)),',',num2str(DataScanWin.Position(i,3)),',',num2str(DataScanWin.Position(i,4)),' | ',num2str(DataScanWin.TCf(i)),' | ',num2str(DataScanWin.Pf(i)),' | ',num2str(DataScanWin.Qasm(i)),' | ',num2str(DataScanWin.Qvol(i)),' | ',num2str(DataScanWin.Qcmp(i)),' | ',num2str(DataScanWin.Qtot(i))];
end
app.Report_Antidote{end+1} = [' -----------------------------------------------------'];

app.Report_Antidote{end+1} = [' '];
app.Report_Antidote{end+1} = [' '];

app.Report_Antidote{end+1} = ['End Antidote Job'];

app.Report_Antidote{end+1} = [' '];
app.Report_Antidote{end+1} = [' '];

app.Text_Report_Antidote.Value = app.Report_Antidote;

scroll(app.Text_Report_Antidote,'bottom');

Output.WeCallBingo = 0;
Output.WeSaveWorkspace = 0;
Output.Message = 'Success';

Antidote_VARIABLES = [];

return





