function [EN,DT,MZ] = ReadXML_Method_LAICPMS(FileName)
% 
% XMapTools is a free software solution for the analysis of chemical maps
% Copyright © 2022-2024 University of Bern, Institute of Geological Sciences, Pierre Lanari

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
%
% Created by P. Lanari (02.11.2022)

DT = [];
EN = {};
MZ = [];

fid = fopen(FileName);

while 1
    NextLine = fgetl(fid);
    
    if isequal(NextLine,-1)
        break
    end
    
    Str = textscan(NextLine,'%s','delimiter',{'>','<'});
    Str = Str{1};
    
    if length(Str) > 2
        if isequal(Str{2},'IntegrationTime')
            DT(end+1) = str2num(Str{3});
        end
        if isequal(Str{2},'MZ')
            MZ(end+1) = str2num(Str{3});
        end
        if isequal(Str{2},'ElementName')
            EN{end+1} = Str{3};
        end
        if isequal(Str{2},'OptimizeID')
            break
        end
    end
    
    if length(Str) > 1 
        
    end
end

fclose(fid);

% convert DT in ms
DT = DT * 1e3;

end