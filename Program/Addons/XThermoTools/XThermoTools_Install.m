function [OKrun] = XThermoTools_Install(Path)
%
OKrun = 0;

fid = fopen('XTT_Config.txt');

localPath =fgetl(fid);
TheriakPath =fgetl(fid);

fclose(fid); 

if ispc
    TestProg = [TheriakPath,'\theriak.exe'];
    TestPath = [localPath,'\'];
else
    TestProg = [TheriakPath,'/theriak'];
    TestPath = [localPath,'/'];
end

if isequal(Path,TestPath) && exist(TestProg)
    OKrun = 1;
else
    WhereWeAreBeforeInstal = cd;
    cd(Path);
    Valid = Install_XThermoTools;
    cd(WhereWeAreBeforeInstal);
    
    if Valid
        OKrun = 1;
    end
end
return



function [Valid] = Install_XThermoTools()
% Install_XThermoTools is a function to install the add-on XThermoTools 
%
%

Valid = 0;

Files4TestM = {'XTHermoTools'};

if exist('XThermoTools_Install')
    WhereIsSetup = which('XThermoTools_Install');
    WhereIsSetup = WhereIsSetup(1:end-(length('XThermoTools_Install')+3));
else
    f = warndlg({'XThermoTools cannot be installed', ...
        '- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -', ...
        '###  Critical error [ES0666]  ###', ...
        '- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -', ...
        'Please repport the error to pierre.lanari@geo.unibe.ch'});
    
    disp(' ')
    disp('---------------------------------------------------------------')
    disp(' ')
    disp('### Critical error [ES0666] ###')
    disp('Problem to find where is the program XThermoTools_Install.p')
    cd
    ls
    disp('---------------------------------------------------------------')
    disp(' ')
    
    return
end

disp(' ')
disp('TEST of XThermoTools setup path -> DONE')


WhereWeAre = cd;
if ~isequal(WhereWeAre,WhereIsSetup)
    f = warndlg({'XTHermoTools cannot be installed', ...
        '- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -', ...
        '###  Error [ES0601]  ###', ...
        '- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -', ...
        'You are not running XTHermoTools.p from the right directory' ...
        '   (1) go to the right directory using the MATLAB Current Folder tool' ...
        '   (2) run again the setup' ...
        ' ' ...
        'Details are printed out in the MATLAB Command Window'});
    
    disp(' ')
    disp('---------------------------------------------------------------')
    disp(' ')
    disp('### Critical error [ES0601] ###')
    disp('Where you are:')
    WhereWeAre
    disp('Where is Install_GRTMOD.p:')
    WhereIsSetup
    disp(' ')
    disp('---------------------------------------------------------------')
    disp(' ')
    
    return
else
    disp(' ')
    disp(['Directory: ',char(WhereWeAre)]);
    disp(' ')
end

% TEST 3
for i=1:length(Files4TestM)
    FileForTest = Files4TestM{i};
    
    if exist([WhereWeAre,'/',FileForTest,'.m']) || exist([WhereWeAre,'/',FileForTest,'.p'])
        disp(['TEST: ',FileForTest,'.p -> DONE'])
    else
        f = warndlg({'The setup package seems to be corrupted', ...
            '- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -', ...
            '###  Error [ES0604]  ###', ...
            '- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -', ...
            'At least one critical file is missing. Try getting a new copy' ...
            ' ' ...
            'Details are printed out in the MATLAB Command Window'});
        
        disp(' ')
        disp('---------------------------------------------------------------')
        disp(' ')
        disp('### Critical error [ES0604] ###')
        disp(['Setup package corrupted: ',char(FileForTest),'.p is missing ...'])
        cd
        ls
        disp('---------------------------------------------------------------')
        disp(' ')
        
        return
    end
end
disp(' ')
disp('TEST of the package -> DONE')



ButtonName = questdlg({['This program will install the add-on XThermoTools in your computer'],'Would you like to continue?'}, ...
                         'Install_XTHermoTools', ...
                         'Yes', 'No', 'Cancel', 'Yes');


if isequal(ButtonName,'Yes')
    
    ButtonName = questdlg({'XTHermoTools needs to know the THERIAK directory','What do we do?'}, ...
                         'Install_XTHermoTools', ...
                         'Select THERIAK directory','Cancel', 'Select THERIAK directory');
    
    
    if ~isequal(ButtonName,'Select THERIAK directory')
        disp(' ')
        disp('CANCELLATION: XThermoTools IS NOT INSTALLED ON YOUR MACHINE ...');
        disp(' ')
        disp(' ')
        return
    end
    
    DirectoryName = uigetdir(cd,'THERIAK directory');
    
    if ~length(DirectoryName)
        disp(' ')
        disp('CANCELLATION: XThermoTools IS NOT INSTALLED ON YOUR MACHINE ...');
        disp(' ')
        disp(' ')
        return
    end
    
    disp(' ')
    disp(['THERIAK directory: ',DirectoryName]);
                     

    
    fid =fopen([WhereIsSetup,'/XTT_Config.txt'],'w');
    fprintf(fid,'%s\n',WhereIsSetup);
    fprintf(fid,'%s',DirectoryName);
    fclose(fid);

    
    Valid = 1;

else
    
    Valid = 0;
    
end

disp(' ');
disp(' ');
disp(' ');