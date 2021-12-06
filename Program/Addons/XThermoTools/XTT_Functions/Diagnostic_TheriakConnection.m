function [] = Diagnostic_TheriakConnection()
% 
% This function checks the connection to theriak
% 
% Pierre Lanari 10.01.2017
%

clc

DateReport = datestr(now,'dd-mmm-yyyy_HH:MM:SS');
DatePrint = datestr(now,'dd-mmm-yyyy HH:MM:SS');

diary(['Report_test_XThermoTools'])

% Test of the files
disp(' ');
disp(' ');
disp(['New Test of *XThermoTools*']);
disp(['Date: ',DatePrint]);
disp(' ');
disp(' ');
disp('###############################################')
disp('Part 1: Configuration file and Paths')
disp('###############################################')
disp(' ');

fid = fopen('XTT_Config.txt');
XThermoToolsDir = fgetl(fid);
TheriakDir = fgetl(fid);
fclose(fid);

disp(['XThermoToolsDir: ',XThermoToolsDir]);
disp(['TheriakDir: ',TheriakDir]);
disp(' ');
disp(['>> which(''XThermoTools.m''):']);
disp(which('XThermoTools.m'))

disp(' ');
disp(' ');

disp('###############################################')
disp('Part 2: theriak.ini (WINDOWS-users)')
disp('###############################################')
disp(' ');
if ispc
    Check = exist([TheriakDir,'/theriak.ini']);
    disp(['>> exist([TheriakDir,''/theriak.ini'']): ',num2str(Check)])
    if isequal(Check,2)
        [SUCCESS,MESSAGE,MESSAGEID] = copyfile([TheriakDir,'/theriak.ini'],[cd,'/theriak.ini'],'f');
    end
    if isequal(SUCCESS,1)
        disp(['>> copying theriak.ini to working directory: Done']);
    else
        disp(['>> copying theriak.ini to working directory: Aborded (see error messages below)']);
        SUCCESS
        MESSAGE
        MESSAGEID
    end
else
    disp('not tested (Mac or Linux)');
end
disp(' ');
disp(' ');


disp('###############################################')
disp('Part 3: Working directory')
disp('###############################################')
disp(' ');

disp(['working directory: ',cd]);
disp(['>> ls:']);
disp('- - - - ')
disp(ls)
disp('- - - - ')
disp(' ');
disp(' ');


disp('###############################################')
disp('Part 4: Connection to theriak')
disp('###############################################')

disp(' ');

% - - - Initialization (DO NOT EDIT)  - - - -
setenv('DYLD_LIBRARY_PATH', '/usr/local/bin')
setenv('GFORTRAN_STDIN_UNIT', '5') 
setenv('GFORTRAN_STDOUT_UNIT', '6') 
setenv('GFORTRAN_STDERR_UNIT', '0')
% - - - - - - - - - - - - - - - - - - - - - -

INFILE=char( ['500   10000'],['1   SI(1)AL(1)FE(0.3)MG(0.3)H(1)O(?)   *']);
dlmwrite('THERIN' , INFILE,'delimiter','');
disp(' -> The test function created a file THERIN')

dlmwrite('THERCOM',char('JUN92_test.bs','no'),'delimiter','');
disp(' -> The test function uses JUN92_test.bs')

% copy database:
Path4DB = [XThermoToolsDir,'/XTT_Databases/JUN92.bs']
[SUCCESS,MESSAGE,MESSAGEID] = copyfile(Path4DB,[cd,'/','JUN92_test.bs'],'f');
if isequal(SUCCESS,1)
    disp(['>> copying JUN92.bs from the database directory: Done (JUN92_test.bs was generated)']);
else
    disp(['>> copying JUN92.bs from the database directory: Aborded (see error messages below)']);
    SUCCESS
    MESSAGE
    MESSAGEID
end

[wum,yum]=system([[TheriakDir,'/theriak'],'   THERCOM   THERIN']);

disp(yum)

disp(' ');
disp(' ');

if exist('THERCOM','file')
    delete THERCOM
    disp(' ')
    disp('File THERCOM has been deleted')
    
end

if exist('JUN92_test.bs','file')
    delete JUN92_test.bs
    disp(' ')
    disp('File JUN92_test.bs has been deleted')
end

disp(' ');
disp(' ');

disp('Test completed!')
disp('For more information, please contact pierre.lanari@geo.unibe.ch')

diary off
return




