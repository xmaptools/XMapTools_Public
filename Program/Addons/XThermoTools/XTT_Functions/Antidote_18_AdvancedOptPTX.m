function [Output,Antidote_VARIABLES,handles] = Antidote_18_AdvancedOptPTX(AntidoteRecipes,AntidoteMode,WorkVariXMap,MinimOptions,handles)
%
% Subroutine developed during SWOT 2020 largely motivated by Rob's input
%   - Limited to 8 phases / 8 elements in the selection module
%   - Not fully tested
%   - Not yet compatible with multiple groups!
%   - The addition of other variables could be controlled by a subroutine
%
%

WorkVariXMap = AddXMg(WorkVariXMap);
BinPhaseDef = handles.BinPhaseDef;

% Add XMg to BinPhaseDef
for i = 1:length(BinPhaseDef)
    
    if ~isempty(find(ismember(BinPhaseDef(i).ListVariAntidote,'MG'))) || ~isempty(find(ismember(BinPhaseDef(i).ListVariAntidote,'FE')))
        BinPhaseDef(i).ListVariAntidote{end+1} = 'XMG';
    end
end
 
[BinPhaseDef] = XTTselectCompInAntidote(WorkVariXMap,BinPhaseDef);

[WorkVariXMap] = GenerateResXMap(BinPhaseDef); 

WorkVariXMap = AddXMg(WorkVariXMap);   % Add again XMg...


eval(['Ti = [',get(handles.BinTextBinTCminmax,'String'),'];']);
eval(['Pi = [',get(handles.BinTextBinPminmax,'String'),'];']);

LIMS = [Ti(1),Ti(end),Pi(1),Pi(end)];

BinSet = SetBin(handles);

fprintf('%s\t%s\n','Antidote',char(AntidoteRecipes(AntidoteMode)))
fprintf('%s\t\t%s\n','Bulk',BinSet.Bulk2Display)
fprintf('%s\t%s\n','Database',BinSet.Database);
disp(' ')

set(handles.Live_Disp2_Text2,'Visible','On','String','P-T optimization (multi-phase)');

if isequal(MinimOptions.Search.Symplex.FirstOpt,1) % Preliminary P-T input
    
    set(handles.axes_Live_PLOT5,'Visible','On');
    axes(handles.axes_Live_PLOT5), cla, colorbar,
    xlabel('Temperature (C)'), ylabel('Pressure (Kbar)')
    title('Residuals')
    axis(LIMS)
    
    UpdateLIVEWaitBar(1,0,handles);
    
    set(handles.Live_Disp1_Text1,'Visible','On','String','>>');
    set(handles.Live_Disp1_Text2,'Visible','On','String','Preliminary P-T gridding (multi-phase)');
    set(handles.Live_Disp1_Text3,'Visible','On','String','Computing ...','ForegroundColor',[1,0,0]);
    
    drawnow
    
    % Temporary model
    NbSteps = str2num(MinimOptions.Search.Symplex.NbSteps);
    
    TCstep = (Ti(end)-Ti(1))/(NbSteps-1);
    Pstep = (Pi(end)-Pi(1))/(NbSteps-1);
    
    TC = Ti(1):TCstep:Ti(end);
    P = Pi(1):Pstep:Pi(end);
    
    disp(' ')
    disp(['##### Prelimirary P-T gridding (',num2str(NbSteps),' x ',num2str(NbSteps),') #####']);
    disp(' ');
    tic
    Compt = 0;
    Compt2 = 0; LimDisp = 5;
    
    ESP = nan(length(P),length(TC));
    
    for iTC = 1:length(TC)
        for iP = 1:length(P)
            
            [Res] = OptiBingoPTMultiPhase([1,1],[TC(iTC),P(iP)],LIMS,BinSet,WorkVariXMap,BinPhaseDef);
            ESP(iP,iTC) = Res;
            
            Compt = Compt+1;
            Compt2 = Compt2+1;
            if isequal(Compt2,LimDisp)
                Compt2 = 0;
                UpdateLIVEWaitBar(1,Compt/(length(TC)*length(P)),handles);
            end
            
        end
        
        ShiftVal = 1;  % (in % of Qcmp)
        
        axes(handles.axes_Live_PLOT5)
        imagesc(TC,P,ESP), colorbar, set(gca,'ydir','norm')
        if max(ESP(:))+ShiftVal > 0
            MaxValuePlot = 0;
        else
            MaxValuePlot = max(ESP(:))+ShiftVal;
        end
        caxis([min(ESP(:))-ShiftVal MaxValuePlot])
        xlabel('Temperature (C)'), ylabel('Pressure (Kbar)')
        title('Residuals')
        
        drawnow
    end
    ht1 = toc;
    
    ValueMin = min(ESP(:));
    [sP,sTC] = find(ESP==ValueMin);
    
    set(handles.Live_Disp1_Text1,'Visible','Off');
    set(handles.Live_Disp1_Text3,'Visible','On','String','Done','ForegroundColor',[0,0,0]);
    
    drawnow
    
    if length(sP) > 1 && length(sTC)>1 %&& isequal(ValueMin,-100)
        
        disp(' ')
        disp([' ** WARNING **'])
        disp([' The minimum is NON-UNIQUE as the bottom of the objective function is flat '])
        disp([' with ',num2str(length(sP)),' pixels having a value of ',num2str(ValueMin)])
        disp(' ')
        disp(['    -> a random selection has been made and Antidote will not converge ']);
        disp(['       within this region (non-unique minimum)']);
        disp(' '),disp(' ')
        
        WeTake = floor(1+rand(1)*length(sTC));
        
        axes(handles.axes_Live_PLOT5), hold on,
        plot(TC(sTC),P(sP),'om')
        plot(TC(sTC(WeTake)),P(sP(WeTake)),'*m')
        drawnow
        
        X0 = [round(TC(sTC(WeTake))),round(P(sP(WeTake)))];
        
        fprintf('%s\t%.4f\n','CPU time',ht1);
        
    elseif length(sP) > 1 && length(sTC)>1
        % Starting point from Bingo...
        
        fprintf('%s\t%.4f\n','CPU time',ht1);
        
        disp(' ')
        disp('  THIS PHASE IS NOT STABLE --> Let''s try with the P-T from Bingo ')
        
        X0 = [str2num(get(handles.BinTextBinTC,'String')),str2num(get(handles.BinTextBinP,'String'))];
        
    else
        
        fprintf('%s\t\t%.0f\n','Temp. P',P(sP));
        fprintf('%s\t%.0f\n','Temp. TC',TC(sTC));
        
        fprintf('%s\t%.4f\n','CPU time',ht1);
        
        axes(handles.axes_Live_PLOT5), hold on,
        plot(TC(sTC),P(sP),'*m')
        drawnow
        
        X0 = [TC(sTC),P(sP)];
    end
else
    % From Bingo...
    X0 = [str2num(get(handles.BinTextBinTC,'String')),str2num(get(handles.BinTextBinP,'String'))];
    
end

set(handles.Live_Disp2_Text3,'Visible','On','String','Computing ...','ForegroundColor',[1,0,0]);
set(handles.Live_Disp2_Text1,'Visible','On')
drawnow

options = optimset('fminsearch'); options=optimset(options,'TolX',0.0001,'TolFun',0.0001,'display','iter','MaxFunEvals',300,'MaxIter',100);

NORM = X0;
X0 = X0./NORM;

%[E4] = OptiBingoPT(X0,NORM,LIMS,InvMet,WorkVariXMap,handles);
disp(' '),
disp(['##### P-T Optimization (from: ',num2str(NORM(2)),' bar; ',num2str(NORM(1)),' C) #####']);
tic
f = @OptiBingoPTMultiPhase;
[Result,Res] = fminsearch(f, X0,options,NORM,LIMS,BinSet,WorkVariXMap,BinPhaseDef);
ht2 = toc;

TCf = Result(1)*NORM(1);
Pf = Result(2)*NORM(2);

disp(' ')
fprintf('%s\t\t%.0f\n','Temp. P',Pf);
fprintf('%s\t%.0f\n','Temp. TC',TCf);

fprintf('%s\t%.4f\n','CPU time',ht2);

ESP = Res;

set(handles.BinTextBinTC,'String',num2str(TCf));
set(handles.BinTextBinP,'String',num2str(Pf));

set(handles.Live_Disp2_Text1,'Visible','Off')
set(handles.Live_Disp2_Text3,'Visible','On','String','Done','ForegroundColor',[0,0,0]);



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


function WorkVariXMap = AddXMg(WorkVariXMap)
%
% Add XMg to WorkVariXMap
WorkVariXMap.NbEl = WorkVariXMap.NbEl+1;
WorkVariXMap.Els{end+1} = 'XMG';
WorkVariXMap.COMP(:,end+1) = zeros(size(WorkVariXMap.COMP,1),1);
WorkVariXMap.UNC(:,end+1) = zeros(size(WorkVariXMap.UNC,1),1);
IdxFe = find(ismember(WorkVariXMap.Els,'FE'));
IdxMg = find(ismember(WorkVariXMap.Els,'MG'));
for i = 1:WorkVariXMap.NbPhases
    Fe = WorkVariXMap.COMP(i,IdxFe);
    Mg = WorkVariXMap.COMP(i,IdxMg);
    sFe = WorkVariXMap.UNC(i,IdxFe);
    sMg = WorkVariXMap.UNC(i,IdxMg);
    if Fe > 0 || Mg > 0
        WorkVariXMap.COMP(i,end) = Mg/(Mg+Fe);
        WorkVariXMap.UNC(i,end) = sqrt((sMg/Mg)^2+(sqrt(sMg^2+sFe^2)/(Mg+Fe))^2);
    end
end
return

