function [Output,Antidote_VARIABLES,handles] = Antidote_1_OptPTX(AntidoteRecipes,AntidoteMode,WorkVariXMap,MinimOptions,handles)
%
%
%

% Load MinimOptions to enable TEST mode
load('MinimOptions.mat');

% General loading for P-T as variables
eval(['Ti = [',get(handles.BinTextBinTCminmax,'String'),'];']);
eval(['Pi = [',get(handles.BinTextBinPminmax,'String'),'];']);

LIMS = [Ti(1),Ti(end),Pi(1),Pi(end)];

dT4Plot = (Ti(2) - Ti(1))/2;
dP4Plot = (Pi(2) - Pi(1))/2;

Axis_Maps = [LIMS(1)-dT4Plot LIMS(2)+dT4Plot (LIMS(3)-dP4Plot)/1000 (LIMS(4)+dP4Plot)/1000];

BinSet = SetBin(handles);

fprintf('%s\t%s\n','Antidote',char(AntidoteRecipes(AntidoteMode)))
fprintf('%s\t\t%s\n','Bulk',BinSet.Bulk2Display)
fprintf('%s\t%s\n','Database',BinSet.Database);
disp(' ')

%set(handles.Live_Disp2_Text2,'Visible','On','String','Optimization');

if isequal(MinimOptions.Search.Symplex.FirstOpt,1) % Preliminary P-T input
    
    set(handles.Live_Disp2_Text1,'Visible','Off')
    
    % Update the display in the LIVE PANNEL
    set(handles.axes_Live_PLOT1,'Visible','On');
    axes(handles.axes_Live_PLOT1), cla, colorbar,
    xlabel('Temperature (C)'), ylabel('Pressure (Kbar)')
    title('Objective function (-Q_t_o_t)')
    axis(Axis_Maps);
    
    set(handles.axes_Live_PLOT2,'Visible','On');
    axes(handles.axes_Live_PLOT2), cla, colorbar,
    xlabel('Temperature (C)'), ylabel('Pressure (Kbar)')
    title('Q_a_s_m')
    axis(Axis_Maps)
    
    set(handles.axes_Live_PLOT3,'Visible','On');
    axes(handles.axes_Live_PLOT3), cla, colorbar,
    xlabel('Temperature (C)'), ylabel('Pressure (Kbar)')
    title('Q_v_o_l')
    axis(Axis_Maps)
    
    set(handles.axes_Live_PLOT4,'Visible','On');
    axes(handles.axes_Live_PLOT4), cla, colorbar,
    xlabel('Temperature (C)'), ylabel('Pressure (Kbar)')
    title('Q_c_m_p')
    axis(Axis_Maps)
    
    UpdateLIVEWaitBar(1,0,handles);
    
    set(handles.Live_Disp1_Text1,'Visible','On','String','>>');
    set(handles.Live_Disp1_Text2,'Visible','On','String','Exploratory P-T scanning');
    set(handles.Live_Disp1_Text3,'Visible','On','String','Computing ...','ForegroundColor',[1,0,0]);
    
    set(handles.Live_Disp2_Text1,'Visible','Off');
    set(handles.Live_Disp2_Text2,'Visible','Off');
    set(handles.Live_Disp2_Text3,'Visible','Off');
    
    set(handles.Live_Disp3_Text1,'Visible','Off');
    set(handles.Live_Disp3_Text2,'Visible','Off');
    set(handles.Live_Disp3_Text3,'Visible','Off');
    
    drawnow
    
    % -------------------------------------------------------------
    %                 ** P-T scanning ** starts here...
    % -------------------------------------------------------------
    
    NbSteps = str2num(MinimOptions.Search.Symplex.NbSteps);
    
    % Temporary model
    TCstep = (Ti(end)-Ti(1))/(NbSteps-1);
    Pstep = (Pi(end)-Pi(1))/(NbSteps-1);
    
    TC = Ti(1):TCstep:Ti(end);
    P = Pi(1):Pstep:Pi(end);
    
    dT4Plot = (TC(2) - TC(1))/2;
    dP4Plot = (P(2) - P(1))/2;
    Axis_Maps = [TC(1)-dT4Plot TC(end)+dT4Plot (P(1)-dP4Plot)/1000 (P(end)+dP4Plot)/1000];
    
    disp(' ')
    disp(['##### Exploratory P-T scanning (',num2str(NbSteps),' x ',num2str(NbSteps),') #####']);
    disp(' ');
    tic
    Compt = 0;
    Compt2 = 0; LimDisp = 5;
    
    E1 = nan(length(P),length(TC));
    E2 = nan(length(P),length(TC));
    E3 = nan(length(P),length(TC));
    E4 = nan(length(P),length(TC));
    
    IsHSat = nan(length(P),length(TC));
    
    for iTC = 1:length(TC)
        for iP = 1:length(P)
            
            [Res,Evaluation,WorkVariMod] = OptiBingoPT([1,1],[TC(iTC),P(iP)],LIMS,BinSet,WorkVariXMap,MinimOptions,handles);
            E4(iP,iTC) = Res;
            
            E1(iP,iTC) = Evaluation.assemblage;
            E2(iP,iTC) = Evaluation.Volume;
            E3(iP,iTC) = Evaluation.Compositions;
            
            IsHSat(iP,iTC) = SaturationCheck4H(WorkVariMod);
            
            Compt = Compt+1;
            Compt2 = Compt2+1;
            if isequal(Compt2,LimDisp)
                Compt2 = 0;
                UpdateLIVEWaitBar(1,Compt/(length(TC)*length(P)),handles);
            end
        end
        
        % Update display
        axes(handles.axes_Live_PLOT1);
        imagesc(TC,P/1000,E4), colorbar, set(gca,'ydir','norm')
        caxis([min(E4(:))-1 max(E4(:))+1])
        xlabel('Temperature (C)'), ylabel('Pressure (kbar)')
        title('Objective function (-Q_t_o_t)')
        axis(Axis_Maps)
        
        axes(handles.axes_Live_PLOT2);
        imagesc(TC,P/1000,E1), colorbar, set(gca,'ydir','norm')
        caxis([0 100])
        xlabel('Temperature (C)'), ylabel('Pressure (kbar)')
        title('Q_a_s_m')
        axis(Axis_Maps)
        
        axes(handles.axes_Live_PLOT3);
        imagesc(TC,P/1000,E2), colorbar, set(gca,'ydir','norm')
        caxis([min(E2(:))-1 max(E2(:))+1])
        xlabel('Temperature (C)'), ylabel('Pressure (kbar)')
        title('Q_v_o_l')
        axis(Axis_Maps)
        
        axes(handles.axes_Live_PLOT4);
        imagesc(TC,P/1000,E3), colorbar, set(gca,'ydir','norm')
        caxis([min(E3(:))-2 max(E3(:))+2])
        xlabel('Temperature (C)'), ylabel('Pressure (kbar)')
        title('Q_c_m_p')
        axis(Axis_Maps)
        
        drawnow
    end
    
    UpdateLIVEWaitBar(1,1,handles);
    
    % Find pixels with H saturation...
    WhereSaturation  = find(IsHSat);
    if length(WhereSaturation)
        disp(' ')
        disp([' ** WARNING **'])
        disp(['    The component H2O is "saturated" for ',num2str(length(WhereSaturation)),'/',num2str(length(IsHSat(:))),' P-T couples'])
        disp(['    -> This may cause convergence to local minima if H is part of the optimization']);
        disp(['       Further warning messages related to this issue will be displayed below']);
        disp(' ')
        
        TGrid4Plot = repmat(TC,length(P),1);
        PGrid4Plot = repmat(P',1,length(TC));
        
        axes(handles.axes_Live_PLOT1); hold on
        plot(TGrid4Plot(WhereSaturation),PGrid4Plot(WhereSaturation)/1000,'xw');
        drawnow
    end
    
    [sP,sTC] = find(E4==min(E4(:)));
    
    if length(sP) > 1
        sP = sP(1);
        sTC = sTC(1);
    end
    
    X0 = [TC(sTC),P(sP)];
    
    disp('RESULTS (Exploratory P-T scanning)')
    fprintf('%s\t%s\t%.0f\n','X0(1)','P(bar)',P(sP));
    fprintf('%s\t%s\t%.0f\n\n','X0(2)','T(C)',TC(sTC));
    
    ht1 = toc;
    fprintf('%s\t%.4f\n','CPU time',ht1);
    
    axes(handles.axes_Live_PLOT1);
    hold on,
    plot(TC(sTC),P(sP)/1000,'pw','markerfacecolor','w','markersize',10);
    
    set(handles.Live_Disp1_Text1,'Visible','Off');
    set(handles.Live_Disp1_Text3,'Visible','On','String','Done','ForegroundColor',[0,0,0]);
    
    drawnow
    
    disp(' '), disp(' ')
else
    % From bingo P-T input
    X0 = [str2num(get(handles.BinTextBinTC,'String')),str2num(get(handles.BinTextBinP,'String'))];
end


% Get started with possible X variables:  (Syros - March 2019)
BinGfDef = handles.BinGfDef;

%AdditionalVariables = zeros(3,1);
IsHOptimized = 0;
ElementB = {};
Min= [];
Max = [];
AdditionalVariables = 0;
Compt = 0;
if BinGfDef.Fluids.Optimize
    for i = 1:length(BinGfDef.Fluids.Spec)
        if BinGfDef.Fluids.Spec(i).IsActive
            Compt = Compt+1;
            ElementB{Compt} = char(BinGfDef.Fluids.Spec(i).ListVariAntidote);
            Min(Compt) = BinGfDef.Fluids.Spec(i).Lower;
            Max(Compt) = BinGfDef.Fluids.Spec(i).Upper;
            AdditionalVariables(Compt) = 1;
            
            if isequal(ElementB(Compt),{'H'})
                IsHOptimized = 1;
            end
        end
    end
    %AdditionalVariables(1) = 1;
end

% Add Oxygen here...
if BinGfDef.Oxygen.ActivateExtraO
    if BinGfDef.Oxygen.OptimizeExtraO
        Compt = Compt+1;
        ElementB{Compt} = 'O';
        Min(Compt) = BinGfDef.Oxygen.ExtraO.Lower;
        Max(Compt) = BinGfDef.Oxygen.ExtraO.Upper;
        AdditionalVariables(Compt) = 1;
    end
end


if sum(AdditionalVariables) > 0  && isequal(MinimOptions.Search.Symplex.FirstOpt,1)
    
    MaxQ1 = max(E1(:));
    WhereMaxQ1 = find(E1 == MaxQ1);
    NbMaxQ1 = length(WhereMaxQ1);
    
    fprintf('%s\n',' ++ Diagnostic before Exploratory P-T-X scanning ++')
    fprintf('\t%s\t%.2f\n','Min(-Qass)',-MaxQ1);
    fprintf('\t%s\t%.f\n','Nb.      ',NbMaxQ1);
    
    if isequal(MinimOptions.TestMode,1)     % TEST mode 
        LimitMax = 1e5;                     % i.e. no limit
        disp(' ')
        disp(' ')
        disp(' <> TEST MODE   ##   No selection if Nb > 50')
        disp(' ')
    else
        LimitMax = 50;
    end
    
    
    if NbMaxQ1 > LimitMax
        % Random selection: 
        
        Perm = randperm(NbMaxQ1);
        Perm = Perm(1:LimitMax);
        
        WhereMaxQ1 = WhereMaxQ1(Perm);
        NbMaxQ1 = length(WhereMaxQ1);
        disp(' ')
        disp([' ** WARNING **'])
        disp([' the number of P-T initial guesses for stage 2 exceeded the limit of ',num2str(LimitMax)])
        disp(['    -> a random selection has been made to reduce the size of the problem ']);
        disp(' ')
        
        fprintf('\t%s\t%.2f\n','Min(-Qass)',-MaxQ1);
        fprintf('\t%s\t%.f','Nb.      ',NbMaxQ1);
    end
    
    TGrid = repmat(TC,length(P),1);
    PGrid = repmat(P',1,length(TC));
    
    ScanT = TGrid(WhereMaxQ1);
    ScanP = PGrid(WhereMaxQ1);
    
    IsSatScanPT = IsHSat(WhereMaxQ1);
    
    axes(handles.axes_Live_PLOT1);
    hold on,
    plot(ScanT,ScanP/1000,'sw','markerfacecolor','w','markersize',3);
    
    % -------------------------------------------------------------
    %              ** P-T-X scanning ** starts here...
    % -------------------------------------------------------------
    % This stage is critical, otherwise the P-T-X optimization
    % fails to find the global minimum if the initial guess is far
    % from the optimal solution.
    %
    % Here we assume that the shape of the P-T objective function
    % does not change too much with X (as in Lanari et al. 2017).
    
    
    ScanDim = length(Max);
    switch ScanDim
        case 1
            if isequal(MinimOptions.TestMode,1)  % TEST mode 
                Answ = inputdlg('X scan resolution','TEST mode',1,{'100'});
                ScanRes = str2num(Answ{1});
                %ScanRes = 10;
                disp([' <> TEST MODE   ##   Scan resolution (X) increased to: ',num2str(10)])
                disp(' ')
            else
                ScanRes = 10;
            end
        case 2
            ScanRes = 5;
        case 3
            ScanRes = 3;
    end
    
    ScanNb = NbMaxQ1*ScanRes^ScanDim;
    
    fprintf('\n\n')
    disp(' ')
    disp(['##### Exploratory P-T-X scanning (',num2str(ScanNb),') #####']);
    fprintf('\t%s\t%s\t%.0f\n','* X(1)','P-T',length(ScanT));
    if IsHOptimized
        fprintf('\t%s\t%.0f%s\n','  ... Warning',sum(IsSatScanPT)/length(IsSatScanPT)*100,' % of initial P-T couples are saturated (H)');
    end
    
    
    tic
    
    % set(handles.Live_Disp2_Text3,'Visible','On','String','Computing ...','ForegroundColor',[1,0,0]);
    % set(handles.Live_Disp2_Text1,'Visible','On')
    % set(handles.Live_Disp2_Text2,'String',['Prelimirary P-T-X scanning (',num2str(ScanNb),')']);
    
    set(handles.Live_Disp2_Text1,'Visible','On','String','>>');
    set(handles.Live_Disp2_Text2,'Visible','On','String',['Exploratory P-T-X scanning (',num2str(ScanNb),')']);
    set(handles.Live_Disp2_Text3,'Visible','On','String','Computing ...','ForegroundColor',[1,0,0]);
    
    drawnow
    
    % We need plenty of variables for the optimization of H
    SelectedBinBulk = get(handles.BinPopUpBulkCompoList,'Value');
    BinBulkOri = handles.BinBulk(SelectedBinBulk);
    TempBinBulk = BinBulkOri;
    
    %Min = handles.BinGfDef.Fluids.Spec(1).Lower;
    %Max = handles.BinGfDef.Fluids.Spec(1).Upper;
    
    LIMSx = [LIMS(1:2);LIMS(3:4)];
    
    for i = 1:length(Min)
        X0(end+1) = Min(i)+(Max(i)-Min(i))/2;
        LIMSx(end+1,:) = [Min(i);Max(i)];
        fprintf('\t%s\t%s\t%.0f\n',['* X(',num2str(i+1),')'],ElementB{i},ScanRes);
    end
    
    ScanX = X0(3:end);    % size: ScanDim
    ScandX = (Max-Min)/(ScanRes+1);
    
    switch length(ScanX)
        case 1
            Xteps1 = [Min+ScandX:ScandX:Max-ScandX]';
            
            BinMtx1 = repmat(Xteps1,1,length(ScanT));
            
            TMtx = repmat(ScanT',length(Xteps1),1);
            PMtx = repmat(ScanP',length(Xteps1),1);
            
            ScanXsteps = BinMtx1(:);
            ScanTsteps = TMtx(:);
            ScanPsteps = PMtx(:);
            
        case 2
            Xteps1 = [Min(1)+ScandX(1):ScandX(1):Max(1)-ScandX(1)];
            Xteps2 = [Min(2)+ScandX(2):ScandX(2):Max(2)-ScandX(2)];
            
            BinMtL1x1 = repmat(Xteps1,ScanRes,1);
            BinMtL1x2 = repmat(Xteps2',1,ScanRes);
            
            ScanXteps1 = [BinMtL1x1(:),BinMtL1x2(:)];
            
            BinMtL2x1 = repmat(ScanXteps1(:,1),1,length(ScanT));
            BinMtL2x2 = repmat(ScanXteps1(:,2),1,length(ScanT));
            
            TMtx = repmat(ScanT',size(BinMtL2x1,1),1);
            PMtx = repmat(ScanP',size(BinMtL2x1,1),1);
            
            ScanXsteps = [BinMtL2x1(:),BinMtL2x2(:)];
            ScanTsteps = TMtx(:);
            ScanPsteps = PMtx(:);
            
            
        case 3
            
            disp(' NOT YET AVAILABLE... Because it is tricky in this case')
            Output.WeCallBingo = 0;
            Output.WeSaveWorkspace = 0;
            Output.Message = 'Error';
            
            Antidote_VARIABLES = [];
            return
            keyboard
            
            Xteps1 = [Min(1)+ScandX(1):ScandX(1):Max(1)-ScandX(1)];
            Xteps2 = [Min(2)+ScandX(2):ScandX(2):Max(2)-ScandX(2)];
            Xteps3 = [Min(3)+ScandX(3):ScandX(3):Max(3)-ScandX(3)];
            
            BinMtx1 = repmat(Xteps1,ScanRes,1,1);
            BinMtx2 = repmat(Xteps2',1,ScanRes,1);
            BinMtx3 = repmat(Xteps3',1,1);
            
            ScanXteps = [BinMtx1(:),BinMtx2(:),BinMtx3(:)];
            
            % TO BE TESTED !!!!!!!!!!!!!!!!!!
            keyboard
    end
    
    E4 = nan(size(ScanXsteps,1),1);
    IsHSat2 = nan(size(ScanXsteps,1),1);
    
    disp(' ')
    Compt = 0;
    LimDisp = 5;
    UpdateLIVEWaitBar(2,0.0001,handles);
    for i = 1:length(ScanXsteps)
        
        % update the bulk
        [Bulk,TempBinBulk] = SuperFast_X_Update(TempBinBulk,ElementB,ScanXsteps(i,:));
        Bulk(1) = '1';
        BinSet.Bulk2Display = Bulk;
        
        %disp(' ')
        %fprintf('%s\t%.0f\n','STEP',i);
        %disp(Bulk)
        %for j = 1:length(ElementB)
        %    fprintf('%s\t%.5f\n',ElementB{j},ScanXteps(i,j));
        %end
        
        % Method 1:
        [Res,Evaluation,WorkVariMod] = OptiBingoPT([1,1],[ScanTsteps(i),ScanPsteps(i)],LIMS,BinSet,WorkVariXMap,MinimOptions,handles);
        
        IsHSat2(i) = SaturationCheck4H(WorkVariMod);
        
        
        % Method 2 (for testing - done 09.03.2019 - 2 variables)
        % D_Temp = num2str(X0(1));
        % D_Press = num2str(X0(2));
        % [WorkVariMod] = TheriakCall(BinSet,D_Temp,D_Press);
        % DoWePrint = 0;
        % [Emin_T,Evaluation_T] = Opti_EpsiMinimCalc(WorkVariMod,WorkVariXMap,MinimOptions,DoWePrint,handles);
        
        E4(i) = Res;
        
        %fprintf('%s\t%.3f\n','Res.',Res);
        
        Compt = Compt+1;
        if isequal(Compt,LimDisp)
            Compt = 0;
            UpdateLIVEWaitBar(2,i/length(ScanXsteps),handles);
            drawnow
        end
        
    end
    
    PerSat = sum(IsHSat2)/length(IsHSat2)*100;
    %fprintf('\t%s\t%.0f%s\n','  ... Warning',PerSat,' % of them saturated (H)');
    if IsHOptimized && PerSat > 25
        disp(' ')
        disp([' ** WARNING **'])
        disp([' There are too many P-T-X points with H2O saturation (',num2str(PerSat),' %)'])
        disp(['    -> It is strongly recommended to restrict the range of possible H values for better efficency of the exploratory scans']);
        disp(['       Further warning messages related to this issue will be displayed below']);
        disp(' ')
    end
    
    UpdateLIVEWaitBar(2,1,handles);
    
    
    if isequal(MinimOptions.TestMode,1)  % TEST mode
        disp([' <> TEST MODE   ##   Results of P-T-X explanatory ']);
        disp(' ');
        
        E4Map = reshape(E4,size(TMtx));
        for i = 1:size(E4Map,2)
            [E4MinCol(i),IdxMin] = min(E4Map(:,i));
            XOptiCol(i) = Xteps1(IdxMin);
        end
        
        MapE4Min = zeros(numel(P),numel(TC));
        MapXOpti = zeros(numel(P),numel(TC));
        [Is,IdxTC] = ismember(ScanT,TC);
        [Is,IdxP] = ismember(ScanP,P);
        
        for i = 1:length(IdxP)
            MapE4Min(IdxP(i),IdxTC(i)) = E4MinCol(i);
            MapXOpti(IdxP(i),IdxTC(i)) = XOptiCol(i);
        end
        
        figure, 
        imagesc(TC,P,MapE4Min); set(gca,'Ydir','normal');
        TheMinE4 = min(MapE4Min(find(MapE4Min)));
        TheMaxE4 = max(MapE4Min(find(MapE4Min)));
        AdjustE4ColorBar = (TheMaxE4-TheMinE4)*0.05;
        caxis([TheMinE4-AdjustE4ColorBar,TheMaxE4+AdjustE4ColorBar])
        colormap([0,0,0;RdYlBu(256);1,1,1])
        colorbar;
        
        figure, 
        imagesc(TC,P,MapXOpti); set(gca,'Ydir','normal');
        TheMinE4 = min(MapXOpti(find(MapXOpti)));
        TheMaxE4 = max(MapXOpti(find(MapXOpti)));
        AdjustE4ColorBar = (TheMaxE4-TheMinE4)*0.01;
        caxis([TheMinE4-AdjustE4ColorBar,TheMaxE4+AdjustE4ColorBar])
        colormap([1,1,1;RdYlBu(256);0,0,0])
        colorbar;
        
        if 0  % turn to 1 to activate this extra plot (3D bar diagram)
            figure,
            hh = bar3(MapXOpti);

            figure,
            h = bar3(MapE4Min.*-1);
            h2 = get(h(3),'parent');
            set(h2,'yLim', [0.5 20.5]);
            gtick=get(h2,'ytick');
            set(h2,'yticklabel',P(gtick));
            set(h2,'ytickmode','manual');
            set(h2,'xLim', [0.5 20.5]);
            gtick=get(h2,'xtick');
            set(h2,'xticklabel',TC(gtick));
            set(h2,'xtickmode','manual');
            TC(gtick);
            zlim([ 80 100])
            colorbar
            for k = 1:length(h)
                zdata = hh(k).ZData;
                h(k).CData = zdata;
                h(k).FaceColor = 'texturemap';
            end
        end
        
        %keyboard
        
        VariablesStruc = whos;       % this does not save the figures in MATLAB 2014b on
        Compt = 0;
        for i = 1:length(VariablesStruc)
            if ~isequal(VariablesStruc(i).name,'eventdata') && ~isequal(VariablesStruc(i).name,'hObject') && ~isequal(VariablesStruc(i).name,'handles')
                Compt = Compt+1;
                NewStruc(Compt).name = VariablesStruc(i).name;
            end
        end
        save('Last_Optimal-PTX-map.mat',NewStruc.name);
        
        
%         figure,
%         scatter(ScanT,ScanP,100*ones(size(E4MinCol)),E4MinCol,'filled');
%         colormap([0,0,0;RdYlBu(64)])
%         colorbar;
    end
    
    
    % Ok, here we have to exclude all the points that were
    % saturated in H2O...
    
    WhereBestStep2 = find(E4 == min(E4));
    if length(WhereBestStep2 > 1)
        WhereBestStep2 = WhereBestStep2(1);
    end
    
    if IsHOptimized
        WhereUnsat = find(IsHSat2 == 0);
        
        if ~length(WhereUnsat)
            
            disp(' ')
            disp(' ** FATAL ERROR **')
            disp(' Sorry guys but the objective function seems to be flat in H')
            disp(' It is not possible to optimize H further...')
            disp('  -> You can try to reduce the upper limit of H values')
            disp('     Scanning H at fixed PT might help to fix this issue (see recipe 13)')
            disp(' ** FATAL ERROR **')
            disp(' ')
            disp(' '),disp(' ')
            disp(['   >>> End ANTIDOTE job: ',datestr(now),'  <<<'])
            disp('- - - - - - - - - - - - - - - - - - - - - - - - - - -')
            disp(' ')
            
            Output.WeCallBingo = 0;
            Output.WeSaveWorkspace = 0;
            Output.Message = 'Error';
            
            Antidote_VARIABLES = [];
            return
        end
        
        WhereMinUnsat = find(E4(WhereUnsat) == min(E4(WhereUnsat)));
        WhereBestStep2_UnSat = WhereUnsat(WhereMinUnsat);
        if length(WhereBestStep2_UnSat > 1)
            WhereBestStep2_UnSat = WhereBestStep2_UnSat(1);
        end
        
        if ~isequal(WhereBestStep2,WhereBestStep2_UnSat)
            
            disp(' ')
            disp([' ** WARNING **'])
            disp([' The following minimum was skipped because of H saturation:'])
            fprintf('\t%s\t%.5f\n','T',ScanTsteps(WhereBestStep2));
            fprintf('\t%s\t%.5f\n','P',ScanPsteps(WhereBestStep2));
            fprintf('\t%s\t%.5f\n','X',ScanXsteps(WhereBestStep2));
            fprintf('\t%s\t%.5f\n','-Qtot',E4(WhereBestStep2));
            disp([' ... and replaced by: ']);
            fprintf('\t%s\t%.5f\n','T',ScanTsteps(WhereBestStep2_UnSat));
            fprintf('\t%s\t%.5f\n','P',ScanPsteps(WhereBestStep2_UnSat));
            fprintf('\t%s\t%.5f\n','X',ScanXsteps(WhereBestStep2_UnSat));
            fprintf('\t%s\t%.5f\n','-Qtot',E4(WhereBestStep2_UnSat));
            disp(' Note: If P and T are different, the result of the optimization ')
            disp(' is probably not robust...')
            disp(['  -> You can try to set the upper H limit to the saturation point '])
            disp(['     at ',num2str(ScanTsteps(WhereBestStep2)),' - ',num2str(ScanPsteps(WhereBestStep2))])
            disp([' ** WARNING **'])
            disp(' ')
            
            WhereBestStep2 = WhereBestStep2_UnSat;
        end
    end
    
    X0(1) = ScanTsteps(WhereBestStep2);
    X0(2) = ScanPsteps(WhereBestStep2);
    
    axes(handles.axes_Live_PLOT1);
    hold on,
    plot(X0(1),X0(2)/1000,'o','MarkerEdgeColor','k','MarkerFaceColor','r');
    
    %disp(' '), disp(' ')
    disp('RESULTS (Exploratory P-T-X scanning)')
    fprintf('\t%s\t%.5f\n','T',ScanTsteps(WhereBestStep2));
    fprintf('\t%s\t%.5f\n','P',ScanPsteps(WhereBestStep2));
    for i = 1:length(ElementB)
        fprintf('\t%s\t%.5f\n',ElementB{i},ScanXsteps(WhereBestStep2,i));
        X0(2+i) = ScanXsteps(WhereBestStep2,i);
    end
    fprintf('\t%s\t%.5f\t%s\n','Minimum',E4(WhereBestStep2),['(real minimum: ',num2str(min(E4)),')']);
    disp(' ')
    
    ht1 = toc;
    fprintf('%s\t%.4f\n','CPU time',ht1);
    disp(' ')
    
    set(handles.Live_Disp2_Text1,'Visible','Off')
    set(handles.Live_Disp2_Text3,'Visible','On','String','Done','ForegroundColor',[0,0,0]);
    
    
    % -------------------------------------------------------------
    %  ** P-T-X Optimization ** starts here...
    % -------------------------------------------------------------
    
    %  set(handles.Live_Disp3_Text3,'Visible','On','String','Computing ...','ForegroundColor',[1,0,0]);
    %  set(handles.Live_Disp3_Text1,'Visible','On')
    %  set(handles.Live_Disp3_Text2,'String','P-T-X optimization');
    %
    set(handles.Live_Disp3_Text1,'Visible','On','String','>>');
    set(handles.Live_Disp3_Text2,'Visible','On','String',['Global P-T-X optimization']);
    set(handles.Live_Disp3_Text3,'Visible','On','String','Computing ...','ForegroundColor',[1,0,0]);
    
    drawnow
    
    options = optimset('fminsearch'); options=optimset(options,'TolX',0.0001,'TolFun',0.0001,'display','iter','MaxFunEvals',1000,'MaxIter',300);
    
    NORM = X0;
    %NORM(3) = NORM(3)/100;
    X0 = X0./NORM;
    
    disp(['##### Final P-T-X Optimization (from: T = ',num2str(NORM(1)),' degree C; P = ',num2str(NORM(2)),') #####']);
    for i = 1:length(Min)
        disp(['      * Additional variable:   X',num2str(i),'(',char(ElementB{i}),') = ',num2str(NORM(2+i)),'   [',num2str(Min(i)),' - ',num2str(Max(i)),']']);
    end
    
    disp(' ')
    fprintf('%s\t\t%s\n','Method:','Simplex');
    if MinimOptions.Weights.Use
        fprintf('%s\t%s\t%s\n','Equation:','Other',['[E4 = -(',num2str(MinimOptions.Weights.Values(MinimOptions.Weights.Selected,1)),'*E1 + ',num2str(MinimOptions.Weights.Values(MinimOptions.Weights.Selected,2)),'*E2 + ',num2str(MinimOptions.Weights.Values(MinimOptions.Weights.Selected,3)),'*E3)]']);
    else
        fprintf('%s\t%s\t%s\n','Equation:','Classic','[E4 = -1/3*(E1 + (E1/100)*E2 + (E1/100)*(E2/100)*E3)]');
    end
    
    % goto bypass
    
    % *** BYPASS MODE *** (PL 09.03.2019)
    % temporary to avoid optimizing PT (only X for testing
    % purposes only);
    
    Bypass = [];
    %Bypass = NORM(1:2);
    
    if length(Bypass)
        disp(' ')
        disp('*** WARNING *** the Bypass mode is activated (P-T are not optimized)')
        %disp(' ')
    end
    
    
    
    tic
    f = @OptiBingoPTX;
    [Result,Res] = fminsearch(f, X0,options,NORM,LIMSx,BinSet,WorkVariXMap,MinimOptions,TempBinBulk,ElementB,Bypass,IsHOptimized,handles);
    ht2 = toc;
    
    
    
    if length(Bypass)
        AddComment = '** bypass mode is on (this variable was skipped)';
        TCf = Bypass(1);
        Pf = Bypass(2);
    else
        AddComment = '';
        TCf = round(Result(1)*NORM(1));   % round is important for convergence if X is involved (tested agin june 2019)
        Pf = round(Result(2)*NORM(2));
    end
    %Hf = Result(3)*NORM(3);
    X_Vari = Result(3:end).*NORM(3:end);
    
    disp('RESULTS (Final P-T-X optimization)')
    fprintf('%s\t%s\t%.0f\t%s\n','X(1)','P(bar)',Pf,AddComment);
    fprintf('%s\t%s\t%.0f\t%s\n','X(2)','T(C)',TCf,AddComment);
    for i = 1:length(X_Vari)
        fprintf('%s\t%s\t%.6f\n',['X(',num2str(i),')'],ElementB{i},X_Vari(i));
    end
    
    % Here we need to update the bulk
    %[Bulk,TempBinBulk] = SuperFast_H_Update(TempBinBulk,Hf);
    [Bulk,TempBinBulk] = SuperFast_X_Update(TempBinBulk,ElementB,X_Vari);
    
    handles.BinBulk(SelectedBinBulk) = TempBinBulk;
    
    SelectedCompo = handles.BinBulk(SelectedBinBulk).CompoDisp;
    switch SelectedCompo % handles.BinBulk(SelectedBinBulk).CompoDisp
        case 1         % original
            set(handles.BinTextBulkCompo,'String',TempBinBulk.CompositionOriginal);
            set(handles.BinTextBulkCompo,'ForegroundColor',[0,0,0]);
            
        case 2         % modified
            set(handles.BinTextBulkCompo,'String',TempBinBulk.CompositionModified);
            set(handles.BinTextBulkCompo,'ForegroundColor',[1,0,0]);
            
        case 3         % iterative bulk
            set(handles.BinTextBulkCompo,'String',TempBinBulk.CompositionIterative);
            set(handles.BinTextBulkCompo,'ForegroundColor',[0,0,1]);
    end
    
    fprintf('\n%s\t\t%s\n\n','Bulk',Bulk);
    
else
    
    set(handles.Live_Disp2_Text3,'Visible','On','String','Computing ...','ForegroundColor',[1,0,0]);
    set(handles.Live_Disp2_Text1,'Visible','On')
    set(handles.Live_Disp2_Text2,'String','P-T optimization');
    
    drawnow
    
    options = optimset('fminsearch'); options=optimset(options,'TolX',0.0001,'TolFun',0.0001,'display','iter','MaxFunEvals',300,'MaxIter',100);
    
    NORM = X0;
    X0 = X0./NORM;
    
    %[E4] = OptiBingoPT(X0,NORM,LIMS,InvMet,WorkVariXMap,MinimOptions,handles);
    
    disp(['##### Final P-T Optimization (from: ',num2str(NORM(1)),' bar; ',num2str(NORM(2)),' C) #####']);
    
    disp(' ')
    fprintf('%s\t\t%s\n','Method:','Simplex');
    if MinimOptions.Weights.Use
        fprintf('%s\t%s\t%s\n','Equation:','Other',['[E4 = -(',num2str(MinimOptions.Weights.Values(MinimOptions.Weights.Selected,1)),'*E1 + ',num2str(MinimOptions.Weights.Values(MinimOptions.Weights.Selected,2)),'*E2 + ',num2str(MinimOptions.Weights.Values(MinimOptions.Weights.Selected,3)),'*E3)]']);
    else
        %fprintf('%s\t%s\t%s\n','Equation:','Classic','[E4 = -1/3*(E1 + (E1/100)*E2 + (E1/100)*(E2/100)*E3)]');
        fprintf('%s\t%s\t%s\n','Equation:','Classic','[E4 = -1/3*(E1 + (E1/100)*E2 + (E1/100)*E3)]');
    end
    
    tic
    f = @OptiBingoPT;
    [Result,Res] = fminsearch(f, X0,options,NORM,LIMS,BinSet,WorkVariXMap,MinimOptions,handles);
    ht2 = toc;
    
    TCf = Result(1)*NORM(1);
    Pf = Result(2)*NORM(2);
    
    disp(' ')
    disp('RESULTS (Final P-T optimization)')
    fprintf('%s\t%s\t%.0f\n','X(1)','P(bar)',Pf);
    fprintf('%s\t%s\t%.0f\n\n','X(2)','T(C)',TCf);
    
    
    %fprintf('%s\t\t%.0f\n','Temp. P',Pf);
    %fprintf('%s\t%.0f\n','Temp. TC',TCf);
    
end

fprintf('%s\t%.4f\n','CPU time',ht2);

E4 = Res;

set(handles.BinTextBinTC,'String',num2str(TCf));
set(handles.BinTextBinP,'String',num2str(Pf));

set(handles.Live_Disp3_Text1,'Visible','Off')
set(handles.Live_Disp3_Text3,'Visible','On','String','Done','ForegroundColor',[0,0,0]);

%         set(handles.Live_Disp1_Text1,'Visible','Off','String','');
%         set(handles.Live_Disp1_Text2,'Visible','Off','String','');
%         set(handles.Live_Disp1_Text3,'Visible','Off','String','');
%
%         set(handles.Live_Disp2_Text1,'Visible','Off','String','');
%         set(handles.Live_Disp2_Text2,'Visible','Off','String','');
%         set(handles.Live_Disp2_Text3,'Visible','Off','String','');
%
%         set(handles.Live_Disp3_Text1,'Visible','Off','String','');
%         set(handles.Live_Disp3_Text2,'Visible','Off','String','');
%         set(handles.Live_Disp3_Text3,'Visible','Off','String','');





Output.WeCallBingo = 1;
Output.WeSaveWorkspace = 1;
Output.Message = 'Success';


w = whos;
for a = 1:length(w)
    if ~isequal(w(a).name,'eventdata') && ~isequal(w(a).name,'hObject') && ~isequal(w(a).name,'handles')
        Antidote_VARIABLES.(w(a).name) = eval(w(a).name); 
    end
end


return



