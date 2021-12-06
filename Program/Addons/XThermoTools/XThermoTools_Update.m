function [UpdateState] = XThermoTools_Update(varargin)
%
% This is the update for the program (not the database)...
%
% Output varibale "UpdateState" (from XMapTools ...):
%   [-1]        No update system                        Continue
%   [0]         Server cannot be reached                Continue
%   [1]         This add-on is up-to-date               Continue
%   [2]         An update has been installed            Continue
%   [3]         An update is available but skiped       Stop
%   [4]         An update is available but skiped       Continue
%   [5]         Abord the call                          Stop
%


% Step 1 - Get the path
LocBase = varargin{1}(1:end-1);


% Step 2 - Read XMapTools' version
disp(' '), disp(' ')
disp('XThermoTools update ... (connecting to the update server) ...')

fid = fopen('XTT_Local_version.txt','r'); 
localVersion =fgetl(fid); 
fclose(fid);

localStr = strread(localVersion,'%s','delimiter','-');

Version = localStr{2};
ReleaseDate = localStr{3};
ProgramState = localStr{4}; 

[onlineVersion,flag] = urlread('http://www.xmaptools.com/FTP_addons/XThermoTools/Program_version.php');
[onlinePackage,flag] = urlread('http://www.xmaptools.com/FTP_addons/XThermoTools/Program_package.php');


if flag && length(onlineVersion) 
    % internet connection, we check for updates ...
    onlineStr = strread(onlineVersion,'%s','delimiter','-');
    OLVersion = onlineStr{2};
    LOVersion = Version;
    
    % Check if update is required
    lesOLversions = strread(OLVersion,'%f','delimiter','.');
    lesLOversions = strread(LOVersion,'%f','delimiter','.');
    
    ServerVersion = (lesOLversions(1)+lesOLversions(2)*0.01);
    LocalVersion = (lesLOversions(1)+lesLOversions(2)*0.01);

    disp(['XThermoTools update ... (connected to the update server) ...'])
    disp(['XThermoTools update ... (Local version: ',num2str(LOVersion),')'])
    disp(['XThermoTools update ... (Online version: ',num2str(OLVersion),')'])
    
    
    
    if LocalVersion <  ServerVersion    % "<" means "update available"
        
        textAfficher = { ...
            'A mandatory update of XThermoTools is available. ',...
            ' ', ...
            'Note that the files stored in XMapTools/Addons/XThermoTools/ will be deted and replaced by new files. Save a copy of the files you modified before updating XThermoTools or your changes will be lost. ',...
            ' ', ...
            'Press [Update] to upgrade XThermoTools or [Cancel] to end the procedure and save your files', ...
            ' ', ...
            ['Version: ',char(onlineVersion)],...
            ['File: ',char(onlinePackage)], ...
            ' '};
    
        buttonName = questdlg(textAfficher,'XThermoTools Update','Update','Cancel','Update');
          
        
        switch buttonName
            case 'Cancel'
                UpdateState = 3;
                disp(['XThermoTools update ... (Update skipped by user) ... Aborted']);
                return
            
            case 'Update'
                
                WhereWeAre = cd;
                h = waitbar(0,'XThermoTools is updating - Please wait...');
                waitbar(0.2,h);
                
                rmpath(LocBase);
                
                % [1] Go to the add-on directory
                cd(LocBase);
                waitbar(0.4,h);
                        
                cd ..
                
                % We remove the olf package:
                rmdir('XThermoTools','s')
                
                waitbar(0.6,h);
                
                % Download of the new package: 
                WebAdress = ['http://www.xmaptools.com/FTP_addons/XThermoTools/',char(onlinePackage)];
                unzip(WebAdress);
                
                if isdir('__MACOSX')
                    [status,msg,msgID] = rmdir('__MACOSX', 's');
                end
                
                waitbar(0.9,h);
                
                waitbar(1,h);
                
                cd(WhereWeAre);
                
                % Update is done:
                close(h);
                
                UpdateState = 2;
                disp(['XThermoTools update ... (XThermoTools has been updated) ... Done']);
                return
                
                
                
                
        end

    elseif LocalVersion >  ServerVersion
        UpdateState = 1;
        disp(['XThermoTools update ... (This is an unofficial pre-release) ... Done']); 
        return
    else
        UpdateState = 1;
        disp(['XThermoTools update ... (XThermoTools is up-to-date) ... Done']);
        return
    end
    keyboard 

else
    UpdateState = 0;
    disp(['XThermoTools update ... (The update server cannot be reached) ... Aborted']);
    return
end

return

