function [Output,Antidote_VARIABLES,handles] = Antidote_11_ScanW(AntidoteRecipes,AntidoteMode,WorkVariXMap,MinimOptions,handles)
%
%

% Possible improvements (PL): 
% - Minor issues for the plot + save table in /Antidote...
% - Use four figures (map+domain, PT, FirstStep Qtot, FirstStp Qfinal)
% - Save all the data not only the table...



% General loading for P-T as variables
set(handles.uitable_sliding,'Visible','off');


%cla(handles.axes2);

eval(['Ti = [',get(handles.BinTextBinTCminmax,'String'),'];']);
eval(['Pi = [',get(handles.BinTextBinPminmax,'String'),'];']);

LIMS = [Ti(1),Ti(end),Pi(1),Pi(end)];

%eduester
% BinSet = SetBin(handles);
% -------------------------------------------------------------------------
% (1) SET initial variables for Theriak (BinSet)

switch handles.BingoDefault.SelectedProgram
    case 1
        ProgPath = handles.BingoDefault.Theriak.Path;
        DefMin = handles.BingoDefault.Theriak.Database(get(handles.BinPopUpDatabase,'Value')).DefMin;
    case 2
        ProgPath = handles.BingoDefault.PerpleX.Path;
        DefMin = handles.BingoDefault.PerpleX.Database(get(handles.BinPopUpDatabase,'Value')).DefMin;
end

Databases = get(handles.BinPopUpDatabase,'String');
TheSelDatabase = Databases{get(handles.BinPopUpDatabase,'Value')};

% -------------------------------------------------------------------------

SelectedBinBulk = get(handles.BinPopUpBulkCompoList,'Value');
BinBulkOri = handles.BinBulk(SelectedBinBulk) ;

TempBinBulk = BinBulkOri;

laSize = size(XimrotateX(handles.MapWindow.Maps.Data(1).ValuesOx,handles.rotateFig));
lesX = [0 laSize(2)];
lesY = [0 laSize(1)];
xLength = lesX(2)-lesX(1);
yLength = lesY(2)-lesY(1);

x_stepsize=str2num(get(handles.BinTextAnt1,'String'));
y_stepsize=str2num(get(handles.BinTextAnt2,'String'));
nwindows=0; %number of windows
x_wsize=2*floor(xLength/(x_stepsize+1));
y_wsize=2*floor(yLength/(y_stepsize+1));
xul=0;
yul=0;
xur=x_wsize;
yur=0;
xbr=x_wsize;
ybr=y_wsize;
xbl=0;
ybl=y_wsize;

% Update the display in the LIVE PANNEL
set(handles.axes_Live_PLOT5,'Visible','On');
axes(handles.axes_Live_PLOT5), cla, %colorbar,
xlabel('Temperature (C)'), ylabel('Pressure (Kbar)')
axis(LIMS)

drawnow

for j=0:(y_stepsize-1)
    for i=0:(x_stepsize-1)
        nwindows=nwindows+1;
        CoorOri=[(xul+i*x_wsize/2) (yul+j*y_wsize/2); (xur+i*x_wsize/2) (yur+j*y_wsize/2); (xbr+i*x_wsize/2) (ybr+j*y_wsize/2); (xbl+i*x_wsize/2) (ybl+j*y_wsize/2)];
        
        
        CoorSim = CoorOri;
        TempBinBulk.DomainXrefYref = CoorSim;
        [TempBinBulk] = BinUpdateBulkComposition(TempBinBulk,1,handles);
        Bulk = TempBinBulk.CompositionOriginal;
        handles.BinBulk(SelectedBinBulk)=TempBinBulk;
        %new
        [ModPhaseDef] = BinButtonCalcPhaseFAST(handles);
        handles.BinPhaseDef = ModPhaseDef;
        
        %disp('VOR GENERATERES')
        %for i=1:length(handles.BinPhaseDef)
        %   i
        %   handles.BinPhaseDef(i).PhaseVolProp
        %   handles.BinPhaseDef(i).IsPhaseIn
        %end
        
        [WorkVariXMap] = GenerateResXMap(handles.BinPhaseDef);
        % -------------------------------------------------------------------------
        % Options (to be integrated to the interface later)
        [BinSet] = SetBinFAST(ProgPath,DefMin,TheSelDatabase,Bulk,handles.BinGfDef);
        
        fprintf('%s\t%s\n','Antidote',char(AntidoteRecipes(AntidoteMode)))
        fprintf('%s\t\t%s\n','Bulk',BinSet.Bulk2Display)
        fprintf('%s\t%s\n','Database',BinSet.Database);
        disp(' ')
        
        if isequal(MinimOptions.Search.Symplex.FirstOpt,1) % Preliminary P-T input
            
            NbSteps = str2num(MinimOptions.Search.Symplex.NbSteps);
            
            % Temporary model
            
            TCstep = (Ti(end)-Ti(1))/(NbSteps-1);
            Pstep = (Pi(end)-Pi(1))/(NbSteps-1);
            
            TC = Ti(1):TCstep:Ti(end);
            P = Pi(1):Pstep:Pi(end);
            
            disp(' ')
            disp(['##### Prelimirary P-T gridding (',num2str(NbSteps),' x ',num2str(NbSteps),') #####']);
            disp(' ');
            tic
            
            E1 = nan(length(P),length(TC));
            E2 = nan(length(P),length(TC));
            E3 = nan(length(P),length(TC));
            E4 = nan(length(P),length(TC));
            
            for iTC = 1:length(TC)
                for iP = 1:length(P)
                    
                    [Res,Evaluation] = OptiBingoPT([1,1],[TC(iTC),P(iP)],LIMS,BinSet,WorkVariXMap,MinimOptions,handles);
                    E4(iP,iTC) = Res;
                    
                    E1(iP,iTC) = Evaluation.assemblage;
                    E2(iP,iTC) = Evaluation.Volume;
                    E3(iP,iTC) = Evaluation.Compositions;
                    
                end
                
                
            end
            ht1 = toc;
            
            [sP,sTC] = find(E4==min(E4(:)));
            
            if length(sP) > 1
                sP = sP(1);
                sTC = sTC(1);
            end
            
            X0 = [TC(sTC),P(sP)];
            
            fprintf('%s\t\t%.0f\n','Temp. P',P(sP));
            fprintf('%s\t%.0f\n','Temp. TC',TC(sTC));
            
            fprintf('%s\t%.4f\n','CPU time',ht1);
            
            
            disp(' '), disp(' ')
        else
            
            % From Bingo...
            X0 = [str2num(get(handles.BinTextBinTC,'String')),str2num(get(handles.BinTextBinP,'String'))];
            
            %disp(['##### MINIMIZATION from Bingo input (',get(handles.BinTextBinTC,'String'),'?C and  ',get(handles.BinTextBinP,'String'),'bar) #####']);
            %disp(' ');
            
        end
        
        options = optimset('fminsearch'); options=optimset(options,'TolX',0.0001,'TolFun',0.0001,'display','off','MaxFunEvals',300,'MaxIter',100);
        
        NORM = X0;
        X0 = X0./NORM;
        
        %[E4] = OptiBingoPT(X0,NORM,LIMS,InvMet,WorkVariXMap,MinimOptions,handles);
        
        disp(['##### P-T Optimization (from: ',num2str(NORM(1)),' C; ',num2str(NORM(2)),' bar) #####']);
        
        disp(' ')
        fprintf('%s\t\t%s\n','Method:','Simplex');
        if MinimOptions.Weights.Use
            fprintf('%s\t%s\t%s\n','Equation:','Other',['[E4 = -(',num2str(MinimOptions.Weights.Values(MinimOptions.Weights.Selected,1)),'*E1 + ',num2str(MinimOptions.Weights.Values(MinimOptions.Weights.Selected,2)),'*E2 + ',num2str(MinimOptions.Weights.Values(MinimOptions.Weights.Selected,3)),'*E3)]']);
        else
            fprintf('%s\t%s\t%s\n','Equation:','Classic','[E4 = -1/3*(E1 + (E1/100)*E2 + (E1/100)*(E2/100)*E3)]');
        end
        
        tic
        f = @OptiBingoPT;
        [Result,Res] = fminsearch(f, X0,options,NORM,LIMS,BinSet,WorkVariXMap,MinimOptions,handles);
        ht2 = toc;
        
        TCf = Result(1)*NORM(1);
        Pf = Result(2)*NORM(2);
        
        disp(' ')
        fprintf('%s\t\t%.0f\n','Temp. P',Pf);
        fprintf('%s\t%.0f\n','Temp. TC',TCf);
        
        fprintf('%s\t%.4f\n','CPU time',ht2);
        
        E4 = Res;
        fprintf('%s\t\t%.0f\n','E4 = ',(-E4));
        set(handles.BinTextBinRes4,'String',num2str(-E4));
        set(handles.BinTextBinTC,'String',num2str(TCf));
        set(handles.BinTextBinP,'String',num2str(Pf));
        
        disp(' '), disp(' ')
        disp('>> ANTIDOTE calls BINGO ---->')
        %eduester: without pie chart
        %BinButtonRunBingo_Callback(hObject, eventdata, handles);
        
        
        D_Temp = get(handles.BinTextBinTC,'String');
        D_Press = get(handles.BinTextBinP,'String');
        
        % -------------------------------------------------------------------------
        % (3) Call Theriak
        WorkVariMod = TheriakCall(BinSet,D_Temp,D_Press);
        
        % -------------------------------------------------------------------------
        % (4) Call Bingo
        DoWePrint = 0;
        Report.Mode = 'BINGO';
        Report.Tdeg = D_Temp;
        Report.Pbar = D_Press;
        
        [A1,V1,C1,TOTAL2] = BingoCall(WorkVariMod,WorkVariXMap,Report,DoWePrint,handles);
        SW_data(nwindows,:)= [nwindows -E4 A1 V1 C1 TCf Pf CoorSim(1,:) CoorSim(3,:)];
        
        set(handles.BinTextBinRes1,'String',[num2str(A1,'%.0f'),' %']);
        set(handles.BinTextBinRes2,'String',[num2str(V1,'%.2f'),' %']);
        set(handles.BinTextBinRes3,'String',[num2str(C1,'%.2f'),' %']);
        set(handles.BinTextBinRes4,'String',[num2str(TOTAL2,'%.2f'),' %']);
        fprintf('%s\t\t%.0f\n','Q_ass = ',(A1));
        fprintf('%s\t\t%.2f\n','Q_vol = ',(V1));
        fprintf('%s\t\t%.2f\n','Q_comp = ',(C1));
        fprintf('%s\t\t%.2f\n','Q_total = ',(TOTAL2));
        disp(' '), disp(' ')
        
        % -------------------------------------------------------------------------
        % eduester
        %CleanAxes2(handles);
        
        % Update display
        axes(handles.axes_Live_PLOT5);
        plot(str2num(D_Temp),str2num(D_Press),'o','MarkerFaceColor','r','MarkerEdgeColor','k');
        txt=['   #',num2str(nwindows),' - ',num2str(-1*E4),'%'];
        text(str2num(D_Temp),str2num(D_Press),txt);
        eval(['TCi = [',get(handles.BinTextBinTCminmax,'String'),'];']);
        eval(['Pi = [',get(handles.BinTextBinPminmax,'String'),'];']);
        xlabel('Temperature (C)'), ylabel('Pressure (Kbar)')
        axis(LIMS);
        
        drawnow
        
        axes(handles.axes1), hold on
        plot(xul+((i+1)*x_wsize/2),yul+((j+1)*y_wsize/2),'-ok','MarkerFaceColor','w','MarkerEdgeColor','k')
        txt=['  #',num2str(nwindows)];
        text(xul+((i+1)*x_wsize/2),yul+((j+1)*y_wsize/2),txt, 'Color', 'k');
        drawnow
        
        fprintf('%s\t\t%s\n','Bulk',BinSet.Bulk2Display)
        fprintf('%s\t%s\n','Database',BinSet.Database);
        fprintf('%s\t\t%.0f\n','P(bar)',str2num(D_Press));
        fprintf('%s\t\t%.0f\n','T(C)',str2num(D_Temp));
        disp(' ')
        
        set(handles.uitable_sliding,'Data',SW_data,'Visible','on');
        
    end %for i=0:(x_stepsize-1)
end %for j=0:(y_stepsize-1)

datum=clock;
filename=['SW_data_' num2str(datum(1)) '-' num2str(datum(2)) '-' num2str(datum(3)) '_' num2str(datum(4)) '-' num2str(datum(5)) '.txt'];
save (filename, 'SW_data', '-ascii');
set(handles.uitable_sliding,'Data',SW_data);
set(handles.uitable_sliding,'Visible','on');


ht1=toc;
disp(' ')
fprintf('%s\t%.4f\n','CPU time',ht1);




Output.WeCallBingo = 0;
Output.WeSaveWorkspace = 0;
Output.Message = 'Success';

Antidote_VARIABLES = [];

return
