function [out1,out2,out3] = XTTginputXTT(arg1,handles)
%XTTginputXTT Graphical input from mouse for VER_XThermoTools_804 (modified from ginput)
%
%   [Xref,Yref] = XTTginputXTT(N) gets N points from the current axes and returns 
%   the X- and Y-coordinates in the REF system (without rotate). 
%
%   P. Lanari (31.07.2013)

out1 = []; out2 = []; out3 = []; y = [];
c = computer;
if ~strcmp(c(1:2),'PC')
    tp = get(0,'TerminalProtocol');
else
    tp = 'micro';
end

if ~strcmp(tp,'none') && ~strcmp(tp,'x') && ~strcmp(tp,'micro'),
    if nargout == 1,
        if nargin == 1,
            out1 = trmginput(arg1);
        else
            out1 = trmginput;
        end
    elseif nargout == 2 || nargout == 0,
        if nargin == 1,
            [out1,out2] = trmginput(arg1);
        else
            [out1,out2] = trmginput;
        end
        if  nargout == 0
            out1 = [ out1 out2 ];
        end
    elseif nargout == 3,
        if nargin == 1,
            [out1,out2,out3] = trmginput(arg1);
        else
            [out1,out2,out3] = trmginput;
        end
    end
else
    
    fig = gcf;
    figure(gcf);
    
    if nargin == 0
        how_many = -1;
        b = [];
    else
        how_many = arg1;
        b = [];
        if  ischar(how_many) ...
                || size(how_many,1) ~= 1 || size(how_many,2) ~= 1 ...
                || ~(fix(how_many) == how_many) ...
                || how_many < 0
            error(message('MATLAB:ginput:NeedPositiveInt'))
        end
        if how_many == 0
            % If input argument is equal to zero points,
            % give a warning and return empty for the outputs.
            
            warning (message('MATLAB:ginput:InputArgumentZero'));
        end
    end
    
    % Setup the figure to disable interactive modes and activate pointers. 
    %initialState = setupFcn(fig);
    
    % onCleanup object to restore everything to original state in event of
    % completion, closing of figure errors or ctrl+c. 
    %c = onCleanup(@() restoreFcn(initialState));
    
    % Suspend figure functions
   state = uisuspend(fig);
   
   
   % Display coordinates new 1.5.4 (11/2012)
   axes(handles.axes1);
   set(gcf, 'WindowButtonMotionFcn', @mouseMOVEginput);
   
   
   toolbar = findobj(allchild(fig),'flat','Type','uitoolbar');
   if ~isempty(toolbar)
        ptButtons = [uigettool(toolbar,'Plottools.PlottoolsOff'), ...
                     uigettool(toolbar,'Plottools.PlottoolsOn')];
        ptState = get (ptButtons,'Enable');
        set (ptButtons,'Enable','off');
   end

   %axes(handles.axes1);
   set(fig,'pointer','cross');
   fig_units = get(fig,'units');
   char = 0; 
    
    
    % We need to pump the event queue on unix
    % before calling WAITFORBUTTONPRESS
    drawnow
    char = 0;
    
    while how_many ~= 0
        % Use no-side effect WAITFORBUTTONPRESS
        waserr = 0;
        try
            keydown = wfbp;
        catch %#ok<CTCH>
            waserr = 1;
        end
        if(waserr == 1)
            if(ishghandle(fig))
                cleanup(c);
                error(message('MATLAB:ginput:Interrupted'));
            else
                cleanup(c);
                error(message('MATLAB:ginput:FigureDeletionPause'));
            end
        end
        % g467403 - ginput failed to discern clicks/keypresses on the figure it was
        % registered to operate on and any other open figures whose handle
        % visibility were set to off
        figchildren = allchild(0);
        if ~isempty(figchildren)
            ptr_fig = figchildren(1);
        else
            error(message('MATLAB:ginput:FigureUnavailable'));
        end
        %         old code -> ptr_fig = get(0,'CurrentFigure'); Fails when the
        %         clicked figure has handlevisibility set to callback
        if(ptr_fig == fig)
            if keydown
                char = get(fig, 'CurrentCharacter');
                button = abs(get(fig, 'CurrentCharacter'));
            else
                button = get(fig, 'SelectionType');
                if strcmp(button,'open')
                    button = 1;
                elseif strcmp(button,'normal')
                    button = 1;
                elseif strcmp(button,'extend')
                    button = 2;
                elseif strcmp(button,'alt')
                    button = 3;
                else
                    error(message('MATLAB:ginput:InvalidSelection'))
                end
            end
            axes_handle = gca;
            drawnow;
            pt = get(axes_handle, 'CurrentPoint');
            
            how_many = how_many - 1;
            
            if(char == 13) % & how_many ~= 0)
                % if the return key was pressed, char will == 13,
                % and that's our signal to break out of here whether
                % or not we have collected all the requested data
                % points.
                % If this was an early breakout, don't include
                % the <Return> key info in the return arrays.
                % We will no longer count it if it's the last input.
                break;
            end
            
            out1 = [out1;pt(1,1)]; %#ok<AGROW>
            y = [y;pt(1,2)]; %#ok<AGROW>
            b = [b;button]; %#ok<AGROW>
        end
    end
    
    % Cleanup and Restore 
    %cleanup(c);
    uirestore(state);
    if ~isempty(toolbar) && ~isempty(ptButtons)
         set (ptButtons(1),'Enable',ptState{1});
         set (ptButtons(2),'Enable',ptState{2});
    end
    set(fig,'units',fig_units);
    
    if nargout > 1
        out2 = y;
        if nargout > 2
            out3 = b;
        end
    else
        out1 = [out1 y];
    end
    
    
   % The folowing seems to work
   
   laSize = size(XimrotateX(handles.MapWindow.Maps.Data(1).ValuesOx,handles.rotateFig));

   lesX = [0 laSize(2)]; %get(handles.axes1,'XLim');
   lesY = [0 laSize(1)]; %get(handles.axes1,'YLim');

   xLength = lesX(2)-lesX(1);
   yLength = lesY(2)-lesY(1);

     
   switch handles.rotateFig
        
        case 0
            xFor = out1;
            yFor = out2;
            
            out1 = xFor;
            out2 = yFor;

            %set(handles.text47, 'string', round(C(1,1))); %abscisse
            %set(handles.text48, 'string', round(C(1,2))); %ordonne
            
        case 90
            xFor = out1;
            yFor = out2;
            
            out1 = yLength - yFor;
            out2 = xFor;
            
            %set(handles.text47, 'string', round(yLength - C(1,2))); %abscisse
            %set(handles.text48, 'string', round(C(1,1))); %ordonne
            
        case 180
            xFor = out1;
            yFor = out2;
            
            out1 = xLength - xFor;
            out2 = yLength - yFor;
            
            %set(handles.text47, 'string', round(xLength - C(1,1))); %abscisse
            %set(handles.text48, 'string', round(yLength - C(1,2))); %ordonne
            
            
        case 270
            xFor = out1;
            yFor = out2;
            
            out1 = yFor;
            out2 = xLength - xFor;
            
            %set(handles.text47, 'string', round(C(1,2))); %abscisse
            %set(handles.text48, 'string', round(xLength - C(1,1))); %ordonne   
            
   end 
end
return


% XTTGINPUTXTT SUB-FUNCTION
function key = wfbp
%WFBP   Replacement for WAITFORBUTTONPRESS that has no side effects.

fig = gcf;
current_char = []; %#ok<NASGU>

% Now wait for that buttonpress, and check for error conditions
waserr = 0;
try
    h=findall(fig,'Type','uimenu','Accelerator','C');   % Disabling ^C for edit menu so the only ^C is for
    set(h,'Accelerator','');                            % interrupting the function.
    keydown = waitforbuttonpress;
    current_char = double(get(fig,'CurrentCharacter')); % Capturing the character.
    if~isempty(current_char) && (keydown == 1)          % If the character was generated by the
        if(current_char == 3)                           % current keypress AND is ^C, set 'waserr'to 1
            waserr = 1;                                 % so that it errors out.
        end
    end
    
    set(h,'Accelerator','C');                           % Set back the accelerator for edit menu.
catch %#ok<CTCH>
    waserr = 1;
end
drawnow;
if(waserr == 1)
    set(h,'Accelerator','C');                          % Set back the accelerator if it errored out.
    error(message('MATLAB:ginput:Interrupted'));
end

if nargout>0, key = keydown; end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
return

% XTTGINPUTXTT SUB-FUNCTION
function initialState = setupFcn(fig)

% Store Figure Handle. 
initialState.figureHandle = fig; 

% Suspend figure functions
initialState.uisuspendState = uisuspend(fig);

% Disable Plottools Buttons
initialState.toolbar = findobj(allchild(fig),'flat','Type','uitoolbar');
if ~isempty(initialState.toolbar)
    initialState.ptButtons = [uigettool(initialState.toolbar,'Plottools.PlottoolsOff'), ...
        uigettool(initialState.toolbar,'Plottools.PlottoolsOn')];
    initialState.ptState = get (initialState.ptButtons,'Enable');
    set (initialState.ptButtons,'Enable','off');
end

% Setup FullCrosshair Pointer without warning. 
oldwarnstate = warning('off', 'MATLAB:hg:Figure:Pointer');
set(fig,'Pointer','fullcrosshair');
warning(oldwarnstate);

% Adding this to enable automatic updating of currentpoint on the figure 
set(fig,'WindowButtonMotionFcn',@(o,e) dummy());

% Get the initial Figure Units
initialState.fig_units = get(fig,'Units');
return

% XTTGINPUTXTT SUB-FUNCTION
function restoreFcn(initialState)
if ishghandle(initialState.figureHandle)
    % Figure Units
    set(initialState.figureHandle,'Units',initialState.fig_units);
    set(initialState.figureHandle,'WindowButtonMotionFcn','');
    
    % Plottools Icons
    if ~isempty(initialState.toolbar) && ~isempty(initialState.ptButtons)
        set (initialState.ptButtons(1),'Enable',initialState.ptState{1});
        set (initialState.ptButtons(2),'Enable',initialState.ptState{2});
    end
    
    % UISUSPEND
    uirestore(initialState.uisuspendState);
end
return

% XTTGINPUTXTT SUB-FUNCTION
function dummy()
% do nothing, this is there to update the GINPUT WindowButtonMotionFcn. 
return

% XTTGINPUTXTT SUB-FUNCTION      % NOT USED
function cleanup(c)
if isvalid(c)
    delete(c);
end
return

% Adapted from the XMapTools function (30.01.2016)
function mouseMOVEginput(hObject, eventdata) 

handles = guidata(hObject);

lesX = get(handles.axes1,'XLim');
lesY = get(handles.axes1,'YLim');

xLength = lesX(2)-lesX(1);
yLength = lesY(2)-lesY(1);

C = get(gca,'CurrentPoint');  

if C(1,1) >= 0 && C(1,1) <= lesX(2) && C(1,2) >= 0 && C(1,2) <= lesY(2)

     switch handles.rotateFig
        
        case 0
            %set(gcf,'pointer','crosshair');
            set(handles.LiveCoordX, 'string', round(C(1,1))); %abscisse
            set(handles.LiveCoordY, 'string', round(C(1,2))); %ordonn?e
            
        case 90
            %set(gcf,'pointer','crosshair');
            set(handles.LiveCoordX, 'string', round(yLength - C(1,2))); %abscisse
            set(handles.LiveCoordY, 'string', round(C(1,1))); %ordonn?e
            
        case 180
            %set(gcf,'pointer','crosshair');
            set(handles.LiveCoordX, 'string', round(xLength - C(1,1))); %abscisse
            set(handles.LiveCoordY, 'string', round(yLength - C(1,2))); %ordonn?e
             
        case 270
            %set(gcf,'pointer','crosshair');
            set(handles.LiveCoordX, 'string', round(C(1,2))); %abscisse
            set(handles.LiveCoordY, 'string', round(xLength - C(1,1))); %ordonn?e 
    
     end
    
else
    
    set(handles.LiveCoordX, 'string', '...'); %abscisse
    set(handles.LiveCoordY, 'string', '...'); %ordonn?e  
end 
return

